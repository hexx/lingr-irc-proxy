package com.github.hexx

import akka.actor._
import akka.util.ByteString
import _root_.dispatch._
import com.github.hexx.dispatch.lingr._

object Irc {
  val serverName = "lingr-irc-proxy"

  sealed trait Command
  case class Nick(name: String) extends Command
  case class User(name: String, host: String, server: String, real: String) extends Command
  case class PrivMsg(room: String, msg: String) extends Command
  case class Notice(room: String, msg: String) extends Command
  case class Join(channel: String) extends Command
  case class Part(channel: String) extends Command
  case class UserHost(name: String) extends Command
  case class Mode(channel: String) extends Command
  case object Quit extends Command

  class Server(port: Int) extends Actor with ActorLogging {
    val state = IO.IterateeRef.Map.async[IO.Handle]()(context.dispatcher)
    var _actors: Map[IO.Handle, ActorRef] = Map()

    override def preStart {
      IOManager(context.system).listen("localhost", port)
    }

    def receive = {
      case IO.NewClient(server) =>
        val socket = server.accept()
        val connection = context.actorOf(Props(new Connection(socket)))
        _actors += socket -> connection
        state(socket) flatMap (_ => sendCommand(connection))
      case IO.Read(socket, bytes) =>
        state(socket)(IO Chunk bytes)
      case IO.Closed(socket, cause) =>
        state(socket)(IO EOF None)
        _actors.get(socket).foreach(context.stop(_))
        _actors -= socket
        state -= socket
    }

    def readCommand: IO.Iteratee[Command] =
      for {
        c <- IO takeUntil ByteString(" ") map (_.utf8String)
        p <- IO takeUntil ByteString("\r\n") map (_.utf8String)
      } yield {
        c match {
          case "NICK" => Nick(p)
          case "USER" =>
            val r = """(\S*) (\S*) (\S*) :?(.*)""".r
            val r(name, host, server, real) = p
            User(name, host, server, real)
          case "PRIVMSG" =>
            val r = """#(.*) :(.*)""".r
            val r(room, msg) = p
            PrivMsg(room, msg)
          case "NOTICE" =>
            val r = """#(.*) :(.*)""".r
            val r(room, msg) = p
            Notice(room, msg)
          case "JOIN" =>
            val r = """#(.*)""".r
            val r(channel) = p
            Join(channel)
          case "PART" =>
            val r = """#(.*) :.*""".r
            val r(channel) = p
            Part(channel)
          case "QUIT" => Quit
          case "USERHOST" => UserHost(p)
          case "MODE" => Mode(p)
        }
      }

    def sendCommand(actor: ActorRef): IO.Iteratee[Unit] = {
      IO.repeat {
        readCommand map { c =>
          log.debug("receiveFromClient: " + c)
          actor ! c
        }
      }
    }
  }

  class Connection(socket: IO.SocketHandle) extends Actor with ActorLogging {
    var _nick: String = Lingr.user
    var _name: String = _
    var _host: String = _
    var _session: Promise[String] = _
    var _lingrConnection: Promise[ActorRef] = _

    val motd = "*****\n" + "https://github.com/hexx/lingr-irc-proxy" + "\n" + "*****"

    def receive = {
      case Nick(nick) => // ignore
      case User(name, host, server, real) =>
        _name = name
        _host = host
        replyWelcome
        replyMotd
        _session = Lingr.createSession map (_.session)
        _lingrConnection = _session map { s =>
          val c = context.actorOf(Props(new Lingr.Connection(s)))
          c ! Lingr.GetRooms
          c
        }
      case PrivMsg(channel, msg) =>
        _lingrConnection.foreach(_ ! Lingr.Say(channel, msg))
      case Notice(channel, msg) => {}
        _lingrConnection.foreach(_ ! Lingr.Say(channel, msg))
      case Join(channel) =>
        join(channel)
      case Part(channel) =>
        sendPart(channel, _nick)
        _lingrConnection.foreach(_ ! Lingr.Unsubscribe(channel))
      case Quit =>
        _lingrConnection.foreach(_ ! Lingr.Quit)
        _session.foreach(Lingr.destroySession(_))
        _lingrConnection.foreach(context.stop(_))
      case UserHost(name) => // ignore
      case Room(channel, _, _, _, _, _, roster) =>
        join(channel)
        replyNames(channel, roster.members.filter(_.is_online) map { m =>
          if (m.is_owner) {
            "@" + m.username
          } else {
            m.username
          }
        })
      case Message(_, channel, _, _, _, name, nick, msg, _, _) =>
        sendPrivMsg(channel, name, msg)
      case Presence(channel, _, _, name, nick, _, status, _) =>
        if (status == "online") {
          sendJoin(channel, name)
        } else {
          sendPart(channel, name)
        }
    }

    def join(channel: String) = {
      sendJoin(channel, _nick)
      _lingrConnection.foreach(_ ! Lingr.Subscribe(channel))
    }

    def reply(number: Int, line: String) {
      send(":%s %03d %s %s".format(Irc.serverName, number, _nick, line))
    }

    def replyWelcome = reply(1, "Welcome to Lingr-IRC proxy!")

    def replyMotd {
      reply(375, "- " + Irc.serverName + " message of the day")
      motd.lines.foreach(s => reply(372, "- " + s))
      reply(376, "End of MOTD command")
    }

    def replyNames(channel: String, nicks: List[String]) {
      log.debug("replyNames: " + nicks.mkString(" "))
      reply(353, "#" + channel + " :" + nicks.mkString(" "))
      reply(366, "#" + channel + " :End of NAMES list")
    }

    def replyUserHost(name: String, host: String) {
      reply(302, "%s :%s=+%s".format(name, name, host))
    }

    def sendJoin(channel: String, nick: String) {
      send(":%s JOIN #%s".format(nick, channel))
    }

    def sendPart(channel: String, nick: String) {
      send(":%s PART #%s".format(nick, channel))
    }

    def sendPrivMsg(channel: String, nick: String, msg: String) {
      send(":%s PRIVMSG #%s :%s".format(nick, channel, msg))
    }

    def send(msg: String) {
      log.debug("sendToClient: " + msg)
      socket.write(ByteString(msg + "\r\n"))
    }
  }
}

object Lingr {
  val secret = "JRvZKmqqll20gjM2PaTBor0Q4qK"
  lazy val apiKey = Option(System.getProperty("lingr.appkey")) getOrElse "8ilYP0"
  lazy val user = Option(System.getProperty("lingr.user")) getOrElse "pab"
  lazy val password = Option(System.getProperty("lingr.password")) getOrElse "pablingr"
  var _rooms: Set[String] = Set()

  sealed trait Command
  case class Say(room: String, msg: String) extends Command
  case object GetRooms extends Command
  case class Subscribe(room: String) extends Command
  case class Unsubscribe(room: String) extends Command
  case object Quit extends Command

  def createSession = Http(Session.create(apiKey, user, password))

  def destroySession(session: String) = Http(Session.destroy(apiKey, session))

  case class Connection(session: String) extends Actor {
    var startObserve = false

    def observe(counter: Int): Promise[Unit] = {
      Http(Event.observe(apiKey, session, counter)) flatMap { r =>
        val c = r.fold(
          _.fold(
            _ => counter,
            p => {
              p.events foreach (context.parent ! _.presence)
              p.counter
            }
          ),
          m => {
            m.events foreach (context.parent ! _.message)
            m.counter
          }
        )
        observe(c)
      }
    }

    def receive = {
      case Say(room: String, msg: String) =>
        Http(Room.say(apiKey, session, room, msg))
      case GetRooms =>
        for {
          res <- Http(User.getRooms(apiKey, session))
          room <- res.rooms
          res2 <- Http(Room.show(apiKey, session, room))
          room2 <- res2.rooms
        } context.parent ! room2
      case Subscribe(room: String) =>
        _rooms += room
        Http(Room.subscribe(apiKey, session, room)) foreach { r =>
          if (!startObserve) {
            startObserve = true
            observe(r.counter)
          }
        }
      case Unsubscribe(room: String) =>
        _rooms -= room
        unsubscribe(room)
      case Quit =>
        _rooms foreach (unsubscribe(_)())
    }

    def unsubscribe(room: String) = Http(Room.unsubscribe(apiKey, session, room))
  }
}

object LingrIrcProxy extends App {
  ActorSystem().actorOf(Props(new Irc.Server(6667)))
}
