# Introduction #

The [Contiki operating system](http://www.contiki-os.org/) is an operating system aimed at the deployment of large-scale sensor networks. The [Contiki bridge](http://code.google.com/p/efr-tools/source/browse/trunk/apps/cxManager/contiki.tcl) is an application designed to receive data from sensors and forward them to the context manager. The bridge talks **directly** to the sensors, on IPv6 and receives their data directly.

# Sensor Protocol #

Sensors host a tiny web server, talking REST/JSON. The root of the web server contains a full description of the sensor, e.g.
```
{
  "node":{
    "node-type":"Tyndall Mote",
    "time":82220
  },
  "rsc":{
    "temperature":{
      "unit":"Celcius",
      "value":29.30
    }
  },
  "cfg":{
    "host":"2001:5c0:1400:b::d623",
    "port":9090,
    "path":"/receive!2",
    "appdata":"",
    "proto":"http",
    "interval":120
  }
}
```

The `node` block describes the node, or mote. The `rsc` block describes the resources that are available on the mote, i.e. the sensors and actuators that it contains.  In the example above, the mote only contains a temperature sensor, and it is currently `29.30` degrees. The `cfg` block contains the configuration of the mote, i.e. where it should send its data, which protocol to use and how often.  Data is sent in the same format as the `rsc` block.


# Usage #

The bridge has partial support for UDP and complete support for HTTP and WebSockets. The bridge takes a number of port and protocol specifications on the command line and will use the first one to subscribe itself as the receiver of sensor data within the sensor.

The command-line option `report` controls both which sensors to listen to, what sort of data translation to perform, and what object of the context manager should receive the data. It contains a series of 4 values, which are understood as follows:
  1. IPv6 address of the sensor to communicate with. You will have to ensure that you have full routing to the sensor. We recommend services such as [gogo6](http://www.gogo6.com/) to cross IPv4 boundaries whenever necessary.
  1. The name of the sensor within its resource description definition. In the example above, that would be `temperature`.
  1. The UUID of the receiving object in the context manager.
  1. The matching between the variables of the sensor (including implicit such) and the variables of the object in the context manager. This mapping is a list of pairs, as described below:
    1. The first item is the name of the field in the object in the context manager.
    1. The second item is either the name of the field in the resource description of the sensor or an implicit value. More specifically:
      * `value` as seen in the example above is the value of the sensor.
      * `sampling` is the **measured** sampling rate within the bridge.  The bridge keeps track of the time at which it receives data and provide an instantaneous sampling rate. This can be useful in applications where you want to see if you might have missed measures from your sensors, which might occur on top of UDP.
      * `timestamp` is the timestamp at which the value was received from the sensor. This is expressed in [RFC3339](http://www.ietf.org/rfc/rfc3339.txt) format.

# Initialisation and Data Flow #

## Initialisation ##

The initialisation procedure performs the following steps:
  1. It connects to each declared sensor in `report` to retrieve their full description. It uses this initialisation phase to actually detect its own (IPv6) address if that was not specified on the command line.
  1. While waiting for the sensors to answers, the bridge starts receiving for data on the UDP and HTTP ports that were specified on the command line.
  1. On reception of the sensor description data, the bridge controls the validity of the arguments provided in `report` and, if valid, register itself at the sensor for reception of data.  This is achieved by POSTing valid values for the `cfg` block of the sensor description. The bridge picks up the first protocol/port specification from the `ports` command line option.

## Data Flow ##

Once registration has been successful, the mote will start sending data to the bridge at regular intervals. On reception, timing is computed (sampling frequency, data and time of reception) and data is further sent to the context manager which root URL is pointed at by the `context` command-line option.

# Tcl Caveats #

The latest version of the UDP library, i.e. the one which is tagged as 1.0.9 from the teapot, does not seem to support IPv6 properly. So, while the code provided in the bridge is working on IPv4, it will not work properly on IPv6. This is the reason why the bridge also supports HTTP and actually favours HTTP. HTTP, being on top of TCP, does not play nicely with sensor networks: its payload is far bigger than the size of a packet in the air and it is statefull. To reduce payload, the bridge also supports WebSockets even though this is not supported by the Contiki implementation in the motes yet.

Also it is essential to let the TCP listening routines to properly pick up an IPv6 interface. I had to switch off all self-validation of the internal httpd to be able to receive data on the TCP socket (HTTP formatted data!).

For an easier (but with caveats) bridge to other sensors, have a look at SimpleSensorBridge.