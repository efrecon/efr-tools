# Introduction #

The aim of the Twitter conduit within the [ContextManager](ContextManager.md) is to send tweets to reflect the state of your sensors. In general, you probably do not want to tweet for each update on the sensor you get, but more a summary of what happens. This is all up to you though...

# Details #

As all other conduits, the Twitter conduit is available interface through a REST/JSON API.  You initialise it by giving it an `object` field, which should contain the UUID of an object in your context.  The conduit supposes that your object contains a field called `status`, a field that is a string and that will contain the text of the tweets that are sent to your account (see below). In the status, any occurrence of strings enclosed by `%` will be replaced by the value of that field (sans the `%`) in the object, if present. So, if the object that is bound contains a field called `temperature`, any occurrence of the string `%temperature%` will be replaced by the content of the field, which will lead to a new tweet every time the content of the status changes (thus indirectly every time the temperature changes in our example).

For the time being, you will have to provide your secret details to the twitter conduit since it does not have complete support for [OAuth](http://oauth.net/).  These details are shared by all instances of the twitter conduit objects, you can directly edit the [twitter.tcl](http://code.google.com/p/efr-tools/source/browse/trunk/apps/cxManager/lib/conduits/twitter.tcl). But it is probably best to create/edit a file called contextManager.cfg in the same directory as the main application file, a file that will contains your personal details for access to your twitter account. My configuration file contains something in the style with (obfuscated on purpose!):

```
# Twitter account details, create an app at https://dev.twitter.com/apps
twitter.consumer_key    "7UJf9qmYhRyyPD1zl6kjg"
twitter.consumer_secret "XXXXXXXXXXXXXXXX"
twitter.access_token    "550404598-OXMimlgOXzrTun3VEcvxRiS0UIVkDQ00N5o3q3hM"
twitter.access_secret   "XXXXXXXXXXXXXXX"
```

# Pairing #

The current implementation and file format of the pairing mechanisms do not support twitter directly. However, you can use the main [twitter.tcl](http://code.google.com/p/efr-tools/source/browse/trunk/apps/cxManager/twitter.tcl) application as a base for how to initiate the twitter conduit to satisfy your needs. The application uses two objects within the context.  One contains a temperature value, got from a sensor. The second object contains a "textual" temperature, providing a rough approximation of the exact floating point value of the sensor. The idea is to express whether it is cold or warm, rather than saying that it is -10 degrees, or 35 degrees. That textual value is used in a `status` field that is used by the twitter conduit to push out a human-readable of the temperature in my house. An example of the resulting twitter feed is [@me3gas\_dev](https://twitter.com/#!/me3gas_dev). The textual approximation is attained by using the rule capabilities of the triggers.