# WebSockets #

[WebSockets](http://www.websocket.org/) are a construct that permits duplex communication between client and Web servers. Technically, the specification is compatible with the HTTP protocol during the hand-shaking procedure, while it relies on a length-constrained framing protocol for further communication.

Combined with JSON, the advantage of WebSockets within wireless sensor networks is the size of the packets that actually transit on the network. The minimal payload introduced by both the framing and the concise textual representation of data allows most packet to fit within the size of one radio packet. Consequently, even though WebSockets still require a statefull protocol such as TCP, they minimise the size of the state to hold when transferring data from a source to its destination.

Based on these ideas, the Context Manager supports [streaming](ContextManager#Streaming.md) of data between clients and the web server that it uses.  Once a web socket connection has been establish, data is transmitted between the ends in JSON format. Every time an object is updated and the update matches a series of criteria, the JSON representation of the object is sent as a WebSocket frame. Similarly, clients are able to send a(n incomplete) JSON representation to update one or several of its fields as a WebSocket frame.

While WebSockets have especially been designed to fit well with WSN, they can also be used in other scenarios, see for example, how PlugWise plugs are integrated to the ContextManager.