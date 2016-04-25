# Introduction #

While the ContikiBridge is tuned to sensor networks and how to extract data from these networks at regular intervals, it is tightened to a given (more-or-less standardised) interface and a bit complex to get started with.  The SimpleSensorBridge is, instead, meant as a prototyping tool to quickly start getting data from sensors.


# Details #

The SimpleSensorBridge is implemented using the application called `devpoll.tcl`. As its name implies, the bridge will poll for the value of one or several sensors at regular intervals and publish some of these value as the fields of an object in the ContextManager. The bridge makes the assumption that one JSON expression is available at a given URL, a JSON expression that contains the values of one or more sensors. The bridge is then able to regularly copy one or several values from the JSON expression into the fields of an object of the context manager.

`devpoll.tcl` takes the following arguments:

  * `-context` is a string and points at the URL of the context manager to publish data to.
  * `-sampling` is an integer (expressed in milliseconds) and contains the polling frequency.
  * `-report` contains a list of triplets with the following meaning:
    * The URL of the JSON expression containing data from the sensor. This can (should?) contain IPv6 addresses.
    * The UUID of the object in the context that will receive the data.
    * A list of matching between the fields of the JSON expression and the fields of the object. This list should contain pairs containing the name of the field in the JSON and the name of the field in the object.


# Caveats #

HTTP usually represents a big overhead to simply get data in the form of a JSON expression from sensor networks. In meshing networks, most of the network will be carrying meta-information, as formed by the HTTP protocol headers, rather than the data itself.