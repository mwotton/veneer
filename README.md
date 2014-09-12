veneer
======

veneer is an intermediary that presents a blocking RPC interface to a
webservice. It handles dropped connections and will try forever - if
this isn't what you want, just wrap it in a timeout.

Importantly, it only keeps one connection to the underlying service
open. This is useful when you have many thousands of threads and don't
want to bogart the port space.

The actual implementation is utterly horrible, and I am posting it
largely in order to start a conversation about how to do this better.
