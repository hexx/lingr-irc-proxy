An IRC proxy for Lingr using Akka
=================================

This is an entry for Typesafe Developer Contest.

What's this?
------------

Lingr_ is a web chat service.  This server enables you to connect Lingr by IRC protocol.

How to run
----------

Write your API key, username and password of Lingr to bin/lingr-irc-proxy.

::

  % sbt assembly
  % cd bin
  % ./lingr-irc-proxy

Then, connect your favorite IRC client to the localhost server.

.. _Lingr: http://lingr.com
