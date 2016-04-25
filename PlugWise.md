# Introduction #

[PlugWise](http://www.plugwise.com/) are Smart plugs that both can measure the electricity consumption of the device(s) connected to the plug, but also turn them on and off. These plugs are an effort to better, sustainable homes, by allowing both an understanding of your electricity requirements, but also by allowing to switch on and off devices according to some given rules.

The PlugWise bridge implements a duplex connection between a (set of) PlugWise plug(s) and the ContextManager. It will allow you to simply turn on and off your physical devices by simply switching the boolean state of an object in the context.


# Details #

The PlugWise bridge is implemented on top of the WebSockets streaming interface to best reaction times. It bridges completely the state of one (or several) physical plug(s) and of (an) object(s) within the ContextManager. Consequently, changes to the state of the physical plug will automatically be reflected into the object in the context; and (relevant) changes to the context object will be propagated to the plug, turning on and off the electrical relay that it carries.

The implementation uses a library that itself is a wrapper around the command line interface of the unofficial [PlugWise python library](https://bitbucket.org/hadara/python-plugwise/).  The library requires the command line program and library to be properly installed.  Unless you change permissions yourself, you will have to elevate your privileges (`sudo`!) to be able to run the Tcl script with success, since it forks `plugwise_util`, which itself requires access to the serial port (over USB).


# Command-Line #

The program is implemented by the `plugwise.tcl` program and accepts the following arguments:

  * `-context` is the URL of the ContextManager containing the objects that will be bound to physical plugs.
  * `-serial` is the serial port to which the plugwise USB stick is connected. Generally, it will be something like `/dev/ttyUSB0`.
  * `-links` contains a list of triplets with the following meaning:
    * The MAC address of the PlugWise, as printed on the sticker that comes with each PlugWise.
    * The UUID of the object to bind the PlugWise to
    * A list of field bindings, binding internal names (see below) to fields in the object. Each binding should contain the name of an internal field and the name of a field, separated by a colon sign. If no colon sign is present, both names will be deemed equivalent.  The implemented internal names are the following:
      * power is the instant power, in Watts.
      * energy is the latest consumption from the log, changed once every hour (values are also stored in the past using the automatic storage options that are available in the ContextManager.
      * status is the status of the relay in the PlugWise. Note that changes made externally to the device rely on a polling of its status, which means that they might be delayed.