package com.github.hexx.dispatch.lingr

import dispatch._
import net.liftweb.json._
import com.ning.http.client.Request

case class User(username: String, name: String)

case class Message(
  id: String,
  room: String,
  public_session_id: String,
  icon_url: String,
  `type`: String,
  speaker_id: String,
  nickname: String,
  text: String,
  timestamp: String,
  local_id: String
)

case class Member(
  icon_url: String,
  username: String,
  name: String,
  is_owner: Boolean,
  is_online: Boolean,
  pokeable: Boolean,
  timestamp: String
)

case class Bot(
  id: String,
  name: String,
  icon_url: String,
  status: String
)

case class Roster(members: List[Member], bots: List[Bot])

case class Room(
  id: String,
  name: String,
  blurb: String,
  is_public: Boolean,
  messages: List[Message],
  faved_message_ids: List[String],
  roster: Roster
)

case class Presence(
  room: String,
  public_session_id: String,
  icon_url: String,
  username: String,
  nickname: String,
  timestamp: String,
  status: String, // "online" or "offline"
  text: String
)

object Lingr {
  def api = :/("lingr.com") / "api"
  def eventApi = :/("lingr.com:8080") / "api"
}

object Session {
  case class Response(status: String, session: String, public_id: String, nickname: String, user: User)

  def sessionApi = Lingr.api / "session"

  def create(apiKey: String, user: String, password: String)(implicit formats: Formats = DefaultFormats) =
    sessionApi / "create" <<
      Map("app_key" -> apiKey, "user" -> user, "password" -> password) OK
      (as.lift.Json andThen (_.extract[Response]))

  def verify(apiKey: String, session: String)(implicit formats: Formats = DefaultFormats) =
    sessionApi / "verify" <<
      Map("app_key" -> apiKey, "session" -> session) OK
      (as.lift.Json andThen (_.extract[Response]))

  def destroy(apiKey: String, session: String) =
    sessionApi / "destroy" <<
      Map("app_key" -> apiKey, "session" -> session) OK
      as.lift.Json
}

object User {
  case class GetRoomsResponse(status: String, rooms: List[String])

  def getRooms(apiKey: String, session: String)(implicit formats: Formats = DefaultFormats) =
    Lingr.api / "user" / "get_rooms" <<
      Map("app_key" -> apiKey, "session" -> session) OK
      (as.lift.Json andThen (_.extract[GetRoomsResponse]))
}

object Room {
  case class ShowResponse(status: String, rooms: List[Room])

  case class GetArchivesResponse(status: String, messages: List[Message])

  case class SayResponse(status: String, messages: List[Message])

  case class SubscribeResponse(status: String, counter: Int)

  def roomApi = Lingr.api / "room"

  def show(apiKey: String, session: String, room: String)(implicit formats: Formats = DefaultFormats) =
    roomApi / "show" <<
      Map("app_key" -> apiKey, "session" -> session, "room" -> room) OK
      (as.lift.Json andThen (_.extract[ShowResponse]))

  def getArchives(apiKey: String, session: String, room: String, before: Int = 30)(implicit formats: Formats = DefaultFormats) =
    roomApi / "get_archives" <<
      Map("app_key" -> apiKey, "session" -> session, "room" -> room, "before" -> before.toString) OK
      (as.lift.Json andThen (_.extract[GetArchivesResponse]))

  def say(apiKey: String, session: String, room: String, text: String)(implicit formats: Formats = DefaultFormats) =
    roomApi / "say" <<
      Map("app_key" -> apiKey, "session" -> session, "room" -> room, "text" -> text) OK
      (as.lift.Json andThen (_.extract[SayResponse]))

  def subscribe(apiKey: String, session: String, room: String)(implicit formats: Formats = DefaultFormats) =
    roomApi / "subscribe" <<
      Map("app_key" -> apiKey, "session" -> session, "room" -> room) OK
      (as.lift.Json andThen (_.extract[SubscribeResponse]))

  def unsubscribe(apiKey: String, session: String, room: String)(implicit formats: Formats = DefaultFormats) =
    roomApi / "unsubscribe" <<
      Map("app_key" -> apiKey, "session" -> session, "room" -> room) OK
      (as.lift.Json andThen (_.extract[SubscribeResponse]))
}

object Favorite {
  case class Response(status: String, message: Message)

  def favoriteApi = Lingr.api / "favorite"

  def add(apiKey: String, session: String, message: String)(implicit formats: Formats = DefaultFormats) =
    favoriteApi / "add" <<
      Map("app_key" -> apiKey, "session" -> session, "message" -> message) OK
      (as.lift.Json andThen (_.extract[Response]))

  def remove(apiKey: String, session: String, message: String)(implicit formats: Formats = DefaultFormats) =
    favoriteApi / "remove" <<
      Map("app_key" -> apiKey, "session" -> session, "message" -> message) OK
      (as.lift.Json andThen (_.extract[Response]))
}

object Event {
  def eventApi = Lingr.eventApi / "event"

  case class MessageAndEventId(message: Message, event_id: Int)
  case class PresenceAndEventId(presence: Presence, event_id: Int)

  case class MessageResponse(status: String, counter: Int, events: List[MessageAndEventId])
  case class PresenceResponse(status: String, counter: Int, events: List[PresenceAndEventId])
  case class EmptyResponse(status: String)

  def observe(apiKey: String, session: String, counter: Int)
    (implicit formats: Formats = DefaultFormats):
      (Request, OkFunctionHandler[Either[Either[EmptyResponse, PresenceResponse], MessageResponse]]) =
    eventApi / "observe" <<?
      Map("app_key" -> apiKey, "session" -> session, "counter" -> counter.toString) OK
      (as.lift.Json andThen { json =>
        try {
          Right(json.extract[MessageResponse])
        } catch {
          case _ => try {
            Left(Right(json.extract[PresenceResponse]))
          } catch {
            case _ => Left(Left(json.extract[EmptyResponse]))
          }
        }
      })
}
