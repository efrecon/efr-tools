

# Introduction #

The context manager (sometimes called the context engine) is aimed at
being the hub of the house, i.e. the place where all relevant sensors
report (directly or indirectly) their data, sometimes in aggregated
form, but also where all applications will dig for information that is
relevant to them, i.e. both values from some sensors, but also their
location or information about their surroundings. In addition, the
manager is able to integrate external cloud services that are relevant
for applications running in the house (or for the house).

The context engine provides dynamic ways to model the context, e.g. a
house and all its online devices, be they sensors or actuators, but
also its inhabitants and their coarse or exact location for
example. The dynamism of the context is essential so as to be able to
adapt to new types and sorts of devices as they are introduced, but
also to adapt to cultural differences when it comes to understanding
(and thus modelling) the space. All houses have rooms, but not all
households use these rooms in the same way, especially when looking
from an international perspective.

# Summary of Functionalities #

## Design ##

The context engine provides the following set of functionality:
  * It takes a schema and a model as input to provide a logical context of a building or a house. This context can be accessed and modified using REST/json calls for maximised flexibility and integration.
  * The context engine provides a number of ground operations to:
    * Modify values of objects that already are instantiated, which will be an operation that is often used when the value of a sensor changes.
    * Provide means to search for objects by the content of their field, the name of their class, etc.
    * Provide means to understand arrays as a technique to organise (part of) the model in a hierarchy and to find specific objects within such a hierarchy.
    * Get the content of the whole or part of the context, including the values of the fields of the instantiated objects and including values from the past, whenever they are accessible.
    * Provide means to trigger external web services whenever (part of) an object has changed.
  * The context engine provides hooks to handle historical and planned data. Historical data is essential for data-mining activities so as to analyse (groups of) household behaviour. Planned data can be used to store the result of data-mining activities, but also to store data from external services, such as weather forecasts.
    * A hook is able to automatically save versions of objects to a database for later retrieval. Given the unstructured nature of the context, noSQL databases are a perfect match, especially since they form the base of a number of data-mining techniques.
    * Another is able to update the actual context using data that has been time stamped in the future and placed in the noSQL cluster.
    * Finally, a last hook automatically saves the values of objects that are modified to CSV files in a directory (see [History](History.md)). These files facilitates import in other tools for statistics, graphing, etc.
  * The context engine should be easily extendible and provides the ability to be extended by plugins. Plugins will improve or complement the behaviour of the context engine based on its internal API.
  * On top of the plugin system, the context engine provides the concept of “conduits” that are logical entities connected to external web services and will direct data into or from the model depending on a number of conditions. Typically, conduits will perform some transformation on the data to or from the external web service, while also retaining data that is specific to the remote service, e.g. login credentials, authorisation details, session information, etc.

The figure below describes a schematic description of the context engine. Note that the figure shows both conduits and hooks to help understanding the general design. However, access to external services and database services are described in more details in the remainder of this document.

![http://efr-tools.googlecode.com/svn/wiki/contextManager.png](http://efr-tools.googlecode.com/svn/wiki/contextManager.png)

## Startup ##
On start up, the context engine performs the following operations in sequence:
  * The context engine starts a (preferably secured) web server with the proper credentials and proper encryption settings.
  * The web server exposes the schema and model that will define the context of the building or the house that the engine is controlling and modelling.
  * It reads the schema that will describe what classes of objects are allowed to appear in the context. This includes possible access to remote schemas that might be included from the main schema. Reading of the main schema might be through accessing the internal web service if necessary.
  * It will then read the model that describes the particular building that it is modelling and controlling. All constraints implied by the schema that has just been read will be applied as the model is being read.
  * All objects instantiated as part of the model are bound to the noSQL engine so that further write operations will automatically lead to new versions of the object being stored and so that later get operations will be able to get older data, whenever  possible.
  * It will initialise all conduits that are accessible to this context engine. Conduits are conceptually separated from the remaining of the code and are plugins communicating with the remaining of the context engine through a tiny and well-defined (internal) API.
  * It will read an initial “pairing” state that is used to initialise a number of conduits and to bind a number of objects to remote services. Pairing is explained later and mostly a helper functionality that aims at reinitialising the context engine every time that it starts and reaching a similar functioning state.


# Formats #
## Schema ##
The context engine should provide techniques to specify the schema that will be used to describe the context itself. A key requirement to the provision of this schema is that it should be easily approachable not only by IT specialists, but also by less-knowledgeable people. To this end the schema brings in object-orientation concepts but simplifies them to their outermost. For example, provides simple inheritance and mixes both object field specifications and inheritance. The schema does not provide concepts such as private variables or similar, once again for the sake of simplification.

Below is a cut-down example of a schema, providing a flavour of how a schema would look and feel like. A longer example is part of the [code](http://code.google.com/p/efr-tools/source/browse/trunk/apps/cxManager/models/model.sha). Roughly, this example schema divides the space into a number of possible floors and rooms within a building, and enables each part of the space to carry a number of devices. The example sports a single type of device, namely a thermometer, which demonstrates the (definition and) use of constraints to provide for a richer expression of units and properties of the physical world.

```
Space {
  name String
  contains Space[]
  devices Device[]
  Outside {
  }
  Building {
    address Address
    pos Coordinate
  }
  Apartment {
    number Integer
  }
  Floor {
    above Floor
    below Floor
  }
  Room {
    Kitchen {
    }
    Bedroom {
    }
    Office {
    }
    Bathroom {
    }
  }
}

Address {
  street String
  streetNumber Integer
  areaCode Integer
  city String
  country String
}

Coordinate {
  latitude Float
  longitude Float
}

Temperature:Float {
  intervals {[-273.15,[}
  unit "celsius"
}

Device {
  name String
  PhysicalDevice {
    SensorDevice {
      Weather {
        Thermometer {
          value Temperature
        }
      }
    }
  }
}
```


Six ground types were isolated from which all other types (constraints and class definitions) will be defined. The ground types are:
  * Integer
  * Float
  * Boolean. Allowed values for a Boolean are false, true, on, off, 0 (off) and any other integer (on).
  * String
  * Timestamp as specified in [RFC 3339](http://datatracker.ietf.org/doc/rfc3339/).
  * UUID (Universally Unique Identifiers) as specified in [RFC 4122](http://datatracker.ietf.org/doc/rfc4122/)

Constraints offer the ability to extend ground types and constrain their values. Embedded in constraints is also the unit of the value that will be stored in the context, which allows to quickly describe constraints that are more “talkative” and easier to read when instantiating object. For example, “Temperature” or “Longitude” carries more semantics than just a “Float. The unit itself is described according to the naming conventions of the International System of Units as specified by the BIPM.

Below is BNF representation of the complete grammar that schema specification should support. The grammar is defined according to the syntax specified in [RFC 2234](http://datatracker.ietf.org/doc/rfc2234/).

```
sign = "+" / "-"
float = [sign] 1*DIGIT ["." 1*DIGIT]
integer = [sign] 1*DIGIT
timestamp = <A timestamp as defined in RFC3339>
boolean = "on" / "off" / "true" / "false" / integer
string = *VCHAR
ground-unit = "meter" / "m"
ground-unit /= "metre"
ground-unit /= "gramme" / "g"
ground-unit /= "second" / "s"
ground-unit /= "ampere" / "A"
ground-unit /= "kelvin" / "K"
ground-unit /= "mole" / "mol"
ground-unit /= "candle" / "cd"
ground-unit /= "radian" / "rad"
ground-unit /= "steradian" / "sr"
ground-unit /= "hertz" / "Hz"
ground-unit /= "newton" / "N"
ground-unit /= "pascal" / "Pa"
ground-unit /= "joule" / "J"
ground-unit /= "watt" / "W"
ground-unit /= "celsius" / "°C"
ground-unit /= "coulomb" / "C"
ground-unit /= "volt" / "V"
ground-unit /= "ohm" / "Ω"
ground-unit /= "siemens" / "S"
ground-unit /= "farad" / "F"
ground-unit /= "henry" / "H"
ground-unit /= "weber" / "Wb"
ground-unit /= "tesla" / "T"
ground-unit /= "lumen" / "lm"
ground-unit /= "lux" / "lx"
ground-unit /= "dioptry"
ground-unit /= "becquerel" / "Bq"
ground-unit /= "gray" / "Gy"
ground-unit /= "sievert" / "Sv"
ground-unit /= "katal" / "kat"
ground-unit /= "minute" / "min"
ground-unit /= "hour" / "h"
ground-unit /= "day" / "d"
ground-unit /= "liter" / "L" / "l" / "litre"
prefix-unit = "yotta" / "Y"
prefix-unit /= "zetta" / "Z"
prefix-unit /= "exa" / "E"
prefix-unit /= "peta" / "P"
prefix-unit /= "tera" / "T"
prefix-unit /= "giga" / "G"
prefix-unit /= "mega" / "M"
prefix-unit /= "kilo" / "k"
prefix-unit /= "hecto" / "h"
prefix-unit /= "deca" / "da"
prefix-unit /= "deci" / "d"
prefix-unit /= "centi" / "c"
prefix-unit /= "milli" / "m"
prefix-unit /= "micro" / "μ"
prefix-unit /= "nano" / "n"
prefix-unit /= "pico" / "p"
prefix-unit /= "femto" / "f"
prefix-unit /= "atto" / "a"
prefix-unit /= "zepto" / "z"
prefix-unit /= "yocto" / "y"
unit-name = prefix-unit ground-unit
system-type = "Float"
system-type /= "Integer"
system-type /= "Timestamp"
system-type /= "Boolean"
system-type /= "String"
uppercase = "A"-"Z"
constraint-name = uppercase 1*ALPHA
constraint-def = constraint-name ":" system-type LWSP
                  "{" LWSP *constraint-spec LWSP "}"
constraint-switch = "oneOf" / "switch"
constraint-interval = "intervals"
constraint-value = float / integer / timestamp / boolean / string
constraint-switch-spec = constraint-switch LWSP
                            "{" LWSP constraint-value
                                *(LWSP constraint-value) 1*LWSP "}"
constraint-unit-spec = "unit" LWSP DQUOTE unit-name DQUOTE
constraint-interval = ("[" / "]") *1constraint-value
                        , *1constraint-value ("[" / "]")
constraint-interval-spec = "intervals" LWSP
                           "{" LWSP constraint-interval
                           *(LWSP constraint-interval) 1*LWSP "}"
constraint-spec = constraint-switch-spec
constraint-spec /= constraint-interval-spec
constraint-spec /= constraint-unit-spec
lowercase = "a"-"z"
class-name = uppercase 1*ALPHA
field-name = lowercase 1*ALPHA
field-spec = field-name LWSP (system-type / class-name) *1("[]")
class-spec = class-name LWSP "{" *(class-spec / field-spec) "}"
uri = <a URI as described in RFC 39864>
inclusion-spec = “INCLUDE” LWSP DQUOTE uri DQUOTE
comment = “#” *VCHAR (CR/CRLF/LF)
empty-line = WSP (CR/CRLF/LF)
schema = *(class-spec / constraint-spec / inclusion-spec / comment /
           empty-line)
```

To simplify the approach by non-technical experts, no forward declaration of classes or constraints is necessary. All new “types” that are discovered will be understood as (empty) classes as a start and converted when their real definition occurs. While this has the drawback of more complicated parsing and the possibility of duplicates or of unknown state (what to do when a class with a given name is then specified as a constraint under the same name), these problems are considered minor compared to the necessity to think about forward declaring classes or constraints to be able to use them.

Implementation of the schema is found in the [schema package](http://code.google.com/p/efr-tools/source/browse/#svn%2Ftrunk%2Fapps%2FcxManager%2Flib%2Fschema).

## Model ##
The schema only specifies and constraints the types of the objects that should be placed in a model. While the schema is essential to the context engine since it provides guidelines to what can be instantiated within the model, achieving a conceptual model of a house (building) and all its online devices is the ultimate goal of the context engine. To this end, the context engine will provide a file format that is easily approachable allowing people to quickly model their own house. At later stages, and depending on the success of the approach, graphical tools could certainly provide help in specifying the final model, perhaps based on existing drawings (blueprints or CAD).

Below is an extract of a model, based on the example schema above. The purpose of this example is to set the scene and provide a flavour for how model files could look like. Complete models will tend to be extensive, so the example below is not complete.

```
Outside pHataren1 {
  name "PositivHataren1"
  contains {myHouse}
  devices {outsideTemp}
}
Address aSoderman10 {
  street "August Södermansväg"
  streetNumber 10
  areaCode 12938
  city "Hägersten"
  country "Sweden"
}
# Approximate center of our lot.
Coordinate myPosition {
  latitude 59.299428
  longitude 17.970209
}
# The house contains three floors, which will contain the rooms.
Building myHouse {  
  name "House Frecon-Waller"
  address aSoderman10
  pos myPosition
  contains {ground cellar top}
}
# The different floors in the house, here only one for the sake of
# concision.
Floor ground {
  name "Ground Floor"
  contains {hall kitchen diningRoom livingRoom bath vilma}
  above cellar
  below top
}
#
# Devices
#
Thermometer outsideTemp {
  name "Heat Pump temperature sensor outside"
}
```

The model should use the schema to control the content of objects that are created within the model. Every instance of a class should be referenced using an identifier. Using techniques similar to those used for the schema, objects can be referenced before they are actually used, but the model should provide enough feedback whenever the data that is specified does not correspond to the schema that controls what can be specified.

In the resulting model, both instantiated objects within the model, but also classes should be identified by a UUID. The UUID should be of type 3 or 5 and build using a concatenation of the URL to the model (or to the schema), the class name and (when relevant) the reference to the object. This ensures the uniqueness of UUID even upon restarts of the context engine.

Below is a BNF representation of the complete grammar that the model specification should support. The grammar is defined according to the syntax specified in RFC 2234.

```
sign            = "+" / "-"
float           = [sign] 1*DIGIT ["." 1*DIGIT]
integer         = [sign] 1*DIGIT
timestamp       = <A timestamp as defined in RFC3339>
boolean         = "on" / "off" / "true" / "false" / integer
string          = *VCHAR

uppercase       = "A"-"Z"
class-name      = uppercase 1*ALPHA

lowercase       = "a"-"z"
object-ref      = lowercase 1*ALPHA

field-name      = lowercase 1*ALPHA
field-value     = DQUOTE string DQUOTE
field-value     /= (boolean / DQUOTE boolean DQUOTE)
field-value     /= integer
field-value     /= DQUOTE timestamp DQUOTE
field-value     /= float
field-value     /= object-ref
field-list      = "{" *(1*LWSP field-value) "}"
field-def       = field-name 1*LWSP (field-list / field-value)

object-def      = class-name 1*LWSP object-ref 1*LWSP
                    "{" *(1*LWSP field-def) *LWSP "}" (CR/CRLF/LF)

comment         = “#” *VCHAR (CR/CRLF/LF)
empty-line      = WSP (CR/CRLF/LF)

schema          = *(object-def / comment / empty-line)
```

There are some extra checks that are performed during the parsing. However, these are based on the schema specification and fall outside of the BNF description of the model. These checks should be as follows:
  * Within an instance definition, only fields which are directly specified for the class in the schema or inherited by the class should appear
  * The rule `field-list` above can only appear for fields that have been specified as arrays in the schema.
  * It is not possible to mix types in an array.
  * Values that fall off the constraint definition will not be allowed.

It is acceptable not to specify the value of a given field, in which case it will default to the following values, which are type dependent:
  * The default Integer is 0
  * The default Float is 0.0
  * The default String is the empty string
  * The default Boolean is false.
  * The default timestamp is the current date and time at instantiation.
  * The default UUID is an automatically generated UUID of type 4.
  * The default array is empty.

# Web Service API #

The web service API supported by the context engine accepts REST calls and respond to these calls solely using JSON constructs. The “tree” of the calls is organised as follows:
  * `/context` provides access to the context itself, i.e. getting information about the classes, the objects of the objects, but also setting their value or registering actions to be performed whenever values are modified (see trigger below). Individual items are encapsulated under `/context/UUID`, where UUID should be replaced by the UUID of a class or an object.
  * `/trigger` is the sub-tree that supports long-lived connections to generic external web-services. This sub-tree is for querying, destroying and triggering manually existing triggers.
  * `/find`, `/locate` and `/topmost` are root level trees for find objects within the context and traversing the hierarchy that it forms.
  * `/conduits` is the tree that encapsulates all plug in conduits that are made available to talk to external well-known web-services, thus encapsulating logic specific to these services.

## Accessing Objects ##

### /context ###

This operation does not take any REST arguments and return a JSON array containing a full description of the content of the context and of its current values. Each object in the array represents an object of the context and is returned as a JSON array. The JSON array contains the following fields:
  * reference is the name of the reference to the object as from the model file that was read to initialize the context. This can never be used for queries and only exists for enabling a matching against the content of the initial file when debugging or looking for possible problems.
  * uuid is the UUID of the object.
  * class is the UUID of the class that describes the object.
  * value is a JSON object itself where each field has the same name as the field of the instance of the object and each JSON value contains the value of that field. In other words, value provides the full current state of the object.

An example output is provided below; actual context descriptions are usually much longer:

```
[
   {
      "reference":"heating",
      "uuid":"5c931263-5a6c-5096-d40f-c290492fbca7",
      "class":"75478d31-c6c5-5e89-010a-03d995528f14",
      "value":{
         "name":"heating and workshop",
         "contains":[

         ],
         "devices":[
            "55851044-b290-56a5-3c88-d64ffbfa75e9"
         ]
      }
   },
   {
      "reference":"greenline",
      "uuid":"55851044-b290-56a5-3c88-d64ffbfa75e9",
      "class":"14b36c3a-3b70-5d3f-c64a-ad2e82f260c8",
      "value":{
         "coldFluidIn":0,
         "coldFluidOut":0,
         "heatFluidOut":0,
         "compressing":false,
         "resistance":0,
         "valve":false,
         "radiatorIn":0,
         "radiatorOut":0,
         "hotWater":0,
         "compressor":0,
         "room":0,
         "outside":0,
         "heatFluidIn":0,
         "model":"Greenline HT+",
         "manufacturer":"IVT",
         "details":"http://www.ivt.se/IVT_Greenline_HT_Plus",
         "name":"Heat Pump"
   }
]
```

### /context/UUID ###

In this call, the UUID of the class or object is taken implicitly from the path. This operation accepts the following REST arguments:
  * `uuid`, the UUID of the object or the class to request information for, in which case the UUID part of the path can be omitted.
  * `when` specifies a timestamp in the past for which we want to request the data for. This only works for objects and is explained in further details below.

The call returns a JSON array containing the current value of the object requested or of the class. For objects, this is the same as the value from the operation above. For classes, it is a JSON array with the following fields:
  * name is the name of the class
  * uuid is the UUID of the class
  * supers is an array containing all the super classes of the class in the hierarchy, starting from the one closest to the class in the hierarchy and ending with the topmost class in the class hierarchy.
  * fields is an array of objects where each object contains the following fields:
    * origin is the UUID of the class that "owns" the field, i.e. where the field was declared. This is to be cross-matched to both the UUID of the class itself, but also of its super classes in the hierarchy.
    * name is the name of the field.
    * type is the class name of the field, which can include constraints or ground types.
    * multiple is a Boolean telling if the field is an array (true) or not (false).

In most cases, applications will request for the current value of an object, thus leaving away or blank the when argument to the REST call. When accessing values from the past, this argument can contain one of the following:
  * An integer, which is expressed in the number of seconds since the epoch.
  * A string formatted in the RFC 3339 format
  * A string formatted according to the former UNIX conventions, in which case the context engine will try to do its best to parse the date. This format accepts “friendly” specifications such as "today" or "yesterday".

Assuming data is being versioned and saved as time goes by to the noSQL cluster, searching will look for a match within a span of +/- 24h around the timestamp that is specified by when and will return the data at the time that is closest to the requested timestamp, i.e. more specifically the first data available strictly before the timestamp that was given if no data was stored at that exact timestamp.

Following the example above, calling `/context/55851044-b290-56a5-3c88-d64ffbfa75e9` would return the following JSON array:

```
   {
         "coldFluidIn":0,
         "coldFluidOut":0,
         "heatFluidOut":0,
         "compressing":false,
         "resistance":0,
         "valve":false,
         "radiatorIn":0,
         "radiatorOut":0,
         "hotWater":0,
         "compressor":0,
         "room":0,
         "outside":0,
         "heatFluidIn":0,
         "model":"Greenline HT+",
         "manufacturer":"IVT",
         "details":"http://www.ivt.se/IVT_Greenline_HT_Plus",
         "name":"Heat Pump"
   }
```

## Modifying Objects ##

### /context/UUID/set ###

This operation permits the user to modify the content of an object of
the context. Once again, the UUID of the object to modify is to be
taken from the path (the `UUID` above) or from the REST argument
uuid, in which case the UUID should be omitted from the path. The
operations accepts any number of REST arguments, which name should
match the name of a field in the object and which value is the new
value for that field. The value should be an (encoded and valid) JSON
representation of the value at that time. The operation returns a JSON
representation of the new value of the object after the modification
of the field(s).

Continuing on the example above, calling
`/context/55851044-b290-56a5-3c88-d64ffbfa75e9?hotWater=56.6`
would return the following JSON array:

```
{
         "coldFluidIn":0,
         "coldFluidOut":0,
         "heatFluidOut":0,
         "compressing":false,
         "resistance":0,
         "valve":false,
         "radiatorIn":0,
         "radiatorOut":0,
         "hotWater":56.6,
         "compressor":0,
         "room":0,
         "outside":0,
         "heatFluidIn":0,
         "model":"Greenline HT+",
         "manufacturer":"IVT",
         "details":"http://www.ivt.se/IVT_Greenline_HT_Plus",
         "name":"Heat Pump"
   }
```


### /context/UUID/append ###

This operation permits the user to append value to multi-fields in an
object of the context. Once again, the UUID of the object to modify is
to be taken from the path (the `UUID` above) or from the REST
argument uuid, in which case the UUID should be omitted from the
path. The operations accepts any number of REST arguments, which name
should match the name of a field in the object and which value is the
new value that should be appended to the field. The operation returns
a JSON representation of the new value of the object after the
modification of the field(s).

### /context/

&lt;UUID&gt;

/destroy ###

This operation removes an object from the context. Once again, the
UUID of the object to modify is to be taken from the path (the 

&lt;UUID&gt;


above) or from the REST argument uuid, in which case the UUID should
be omitted from the path.


## Search ##

A number of the search operations accept string patterns for
restricting the search to parts of the context. For two strings to
match, their contents must be identical except that the following
special sequences may appear in the pattern:

  * `*` Matches any sequence of characters in the string, including a null string.
  * `?` Matches any single character in the string.
  * `[chars]` Matches any character in the set given by chars. If a sequence of the form `x-y` appears in chars, then any character between `x` and `y`, inclusive, will match.
  * `\x` Matches the single character `x`. This provides a way of avoiding the special interpretation of the characters `*?[]\` in the pattern.

### /find ###

This operation aims at finding objects within the context. It accepts the following REST arguments
  * type is a pattern that restrict the search to the classes which name matches the pattern or which inherit from that class. An empty type includes thus all the classes of the context.
  * field is a pattern that match the name of one or several particular fields in the matching classes. It defaults to all the fields.
  * filter is a pattern that match the value of the selected fields, it defaults to any value.

The format of the result is the same as for the /context operation
described above. Actually, without any arguments at all and given the
default behaviour of the REST arguments, /find called without any
arguments is conceptually similar to calling /context, even though the
order of the returned JSON array might not be the same.

### /topmost ###

This operation assumes that a topmost class is used to categorize e.g. space or devices and that it uses an array field to model the hierarchy containment of the spaces or devices. This is the encouraged way of organizing and classifying objects in models (and therefore schemas). It is then able to return the topmost object(s) of that hierarchy. The operation takes the following mandatory REST arguments
  * type is a pattern that should match the topmost container class from which all other classes in the categorisation inherit.
  * traverse is the exact name of the array field that forms the hierarchical classification.

So, for example, if a schema was using the class "Space" as the topmost class in the schema hierarchy for modelling space and was using the field "contains" (an array) in that class for modelling object hierarchy, calling `/topmost?type=Spa*&traverse=contains` would return the following JSON array, formatted as for calls to `/find`. This array contains the definition for “Sweden”, a meaningful alternative to a topmost container for all objects that inherits from Space in a model.

```
[
   {
      "reference": "sweden",
      "uuid": "f1b0d9a8-8990-50c0-ad49-0a6b4a3ec35f",
      "value": {
         "name": "Sweden",
         "contains": [
            "ead9d1f1-af80-5639-59f0-5c57c1c58569"
         ],
         "devices": [
            "684c4e19-c4ed-5861-f127-59109a41bb56",
            "929494fb-84e1-50cb-beea-c04aecda088a"
         ]
      }
   }
]
```


### /locate ###

Locate make the same hierarchical categorisation assumption as /topmost, but looks for specific objects within that hierarchy. Thus, the operation can be understood as finding within that hierarchy. It is typically used for looking specific devices within the space. The operation takes the following REST arguments:
  * traverse is the exact name of the array field that forms the hierarchical classification. This is mandatory for a successful query.
  * within specifies within which multi-field we should be looking for objects. Not specifying this parameter will be understood as looking within the same multi-field as for hierarchy traversal (see type).
  * container should be the UUID of a class or an object. If pointing at an object, this will be the top object at which we start the traversal.  If pointing at a class, the traversal will select all topmost objects of that given class, respecting the traverse specification.
  * type is a pattern that selects the type of the object that we will be looking for. If not specified, the query will be looking for the same type as the type of the within field.
  * field is a pattern matching the name of a field in the objects that we are looking for, i.e. of the type specified implicitly or explicitly by type
  * filter is a pattern matching the content of the field and only objects whose field content matches the value of the filter will be returned.

The `/locate` operation returns its results in the same format as
/find or /context. In the examples below, we assume that the schema
uses “contains” to construct the space hierarchy and that the Space
class contains an array called “devices” that might contains online
devices, a ground class that is inherited from all devices that can be
placed in the context.

`/locate?container=f1b0d9a8-8990-50c0-ad49-0a6b4a3ec35f&traverse=contains&field=name&filter=heating*` would return a room which name would start with the string heating and which would be contained under the topmost “Sweden” object from the example above.

`/locate?container=f1b0d9a8-8990-50c0-ad49-0a6b4a3ec35f&traverse=contains&within=devices` would return all the devices that are contained in the same hierarchy as in the previous example.

`/locate?container=f1b0d9a8-8990-50c0-ad49-0a6b4a3ec35f&traverse=contains&within=devices&type=HeatPump` would only return the instances of a class named HeatPump among the devices from the example above.


## Generic Web Service Integration ##

### /context/UUID/listen ###

This operation listens to modifications made to an object and is able to mediate the content of the object to remote web services. Once again, the UUID of the object to modify is to be taken from the path (the UUID above) or from the REST argument uuid, in which case the UUID should be omitted from the path. The REST call takes the following arguments:
  * receiver is the URL that will receive the callback and be notified when the object changes. In the URL, any string that is surrounded by % signs will be replaced by the current value of the field with the same name (sans % signs) in the object.
  * field is a pattern matching one of more of the field of the object. This can be used to react only on parts of the objects. Not specifying the field will lead to a callback for any modification to the fields in the object.
  * header is a list of key and values, separated by colon signs. These will be automatically be added to the HTTP request at the time of the callback, permitting to transmit API keys or the like. It defaults to an empty string, i.e. no special headers.
  * method can be one of GET, POST, PUT or DELETE and specifies which type of HTTP method will be used at the time of the callback. It defaults to GET and will automatically become a POST if query arguments are specified.
  * type can be used to specify the content type of the data that will be sent as part of the request. It will default to some sensible values, but can be used to drive the “Content-Type” part of the HTTP header better whenever necessary.
  * body can be used to put or post data via the HTTP request made to the server. In the body, which is a textual string, strings surrounded by % signs will be substituted as in receiver.
  * jitter is expressed in milliseconds and guarantees that callbacks are not made more often than the jitter period. It defaults to 100, so as to allow to easily set several fields in an object simultaneously without triggering several callbacks.
  * expression is a mathematical expression that should evaluate to true for the trigger to fire and perform the callback. In that expression, strings surrounded by % signs will be substituted as in receiver. The complete syntax for that expression is out of the scope of this text but can be found here￼.
  * always is a Boolean controlling when the callback should be considered for triggering. The default is to only trigger the callback whenever the value of the fields that the callback is bound to changes, setting always to true will make sure the callback triggers even if none of the values have changed.

### /trigger ###

This operation can be used to return the list of triggers that are currently registered within a context. It returns a JSON array of object which fields are exactly the ones as of the trigger creation (see above), with the addition of:
  * uuid, a UUID for the trigger itself.
  * object, the UUID of the object to which the trigger is associated.

### /trigger/UUID ###

This operation is exactly the same as the above, except that it
returns the information for a single trigger only, the UUID of which
is extracted from the path at UUID. Once again, the UUID of the
trigger to request is to be taken from the path (the UUID above) or
from the REST argument uuid, in which case the UUID should be omitted
from the path.

### /trigger/UUID/test ###

This operation manually triggers the logic around the trigger. In
other words, if the trigger is set to always callback the receiver
URL, it will perform the corresponding web service call as long as the
mathematical expression evaluates positively. Once again, the UUID of
the trigger to test is to be taken from the path (the UUID above) or
from the REST argument uuid, in which case the UUID should be omitted
from the path.

### /trigger/UUID/destroy ###

This operation removes a trigger. Once again, the UUID of the trigger
to remove is to be taken from the path (the UUID above) or from the
REST argument uuid, in which case the UUID should be omitted from the
path.

## Streaming ##

The context manager supports streaming of data to and from objects. Streaming is built upon a combination of WebSocket and JSON and, as such, attempts to minimise the size of packets describing updates, thus being easier to implement to and from wireless sensor networks. WebSockets also have the advantage of easily passing through most firewalls, thus making it easy to push object updates to clients behind a firewall.

### /context/UUID/stream ###

This operation provides a duplex channel to and from an object, thus being able to both mediate changes that have occurred within an object, but also to update its current value. As opposed to all other operations within the context manager, clients should access this entry point using the `ws:` or `wss:` schemes, i.e. the schemes that have been specified for WebSockets. The UUID of the object to modify is to be taken from the path (the UUID above) or from the REST argument uuid, in which case the UUID should be omitted from the path. This call takes the following arguments:
  * field is a pattern matching one of more of the field of the object. This can be used to react only on parts of the objects. Not specifying the field will lead to a callback for any modification to the fields in the object.
  * jitter is expressed in milliseconds and guarantees that callbacks are not made more often than the jitter period. It defaults to 100, so as to allow to easily set several fields in an object simultaneously without triggering several callbacks.
  * expression is a mathematical expression that should evaluate to true for the trigger to fire and perform the callback. In that expression, strings surrounded by % signs will be substituted as in receiver. The complete syntax for that expression is out of the scope of this text but can be found here￼.
  * always is a Boolean controlling when the callback should be considered for triggering. The default is to only trigger the callback whenever the value of the fields that the callback is bound to changes, setting always to true will make sure the callback triggers even if none of the values have changed.

Down to the client, every time one of the matching fields is updated and as controlled by the expression, a full JSON representation of the object is sent as a `text` packet along the WebSocket. Clients can send similar JSON representations to update the content of the object.

The operation creates a trigger which can be operated similarly to all other triggers. However, there is no way to return the UUID of the trigger at creation, usage of the `/trigger` call is recommended in that case.


# Initial Pairing #

In order to be able to restart from a similar state at all times, the
context engine is able to read from a pairing configuration file once
the schema and the model have been read. The purpose of this file is
to establish all the necessary conduit connections to well-known
services. Pairing is made at the conduit level, at the REST/JSON
level. In other words, we initialising the pairing, the context engine
behaves as it was an external client, thus being an external client to
itself. This is to be able to support new conduits in the future and
to fail nicely if some conduit initialisation did not succeed
properly. The pairing contract file is formatted as follows.

Any blank line or line starting with a hash (#) sign will be
ignored. Each pairing involve at least a line specifying the source
and destination, possibly followed by a number of lines (led by any
positive number of whitespaces) describing the mapping between the
receiver fields and the source fields.

Pairing lines are formatted according to the following grammar:

```
source1*WSP*ALPHAinteger*ALPHA[&lt;&gt;]WSPdestination
```

The integer defines a polling frequency, in seconds, whenever this is
needed for the pairing to function properly. Omitting this will
default to the polling default that is specific to each conduit and
usually is 2 minutes. The source and destination will automatically
define the type of conduit that is to be used to be able to perform
the operation. They can be one of the following:

  * A single integer will be understood as the identifier of a feed at [cosm](https://cosm.com/). While the conduit provides way to access non-public feeds via an API key, it will sometimes be enough to simply be able to access a (public) feed, polling its content at the necessary frequency.
  * A string matching a UUID will be understood as an object of the present context.
  * A URL terminated by a UUID will be understood as a remote context
  * A URL-like starting with the scheme gcal: will be understood as a Google calendar. The exact format is gcal://username:password@calendar-name/. Where username and password are the name of a valid Google account together with its password, and calendar-name is the name of a calendar accessible to that account.
  * A URL-like starting with the scheme upnp: (case independent, so you can write UPnP:) will be understand as a UPnP service on the network. The string after the colon can either be a UUID, in which case this will be the UPnP of a root device, or a string filter, in which case this will be a UPnP service which names matches the filter.

Lines directly under a pairing lines and indented by any positive
number of whitespaces will be used to match the destination fields to
the source fields. They have the following format:

```
1*WSP%field-name%*WSP=*WSPmath-expr
```

Where field-name is the name of a “field” in the destination of the pair (for a local or remote context, it would be the name of a field within the object; for a cosm feed, it would be the id of a datastream). The mathematical expression uses field names surrounded by % signs, as for the expression in the triggers and within the source. In addition, it is also possible to access prior versions of field at the remote entity by adding a pipe character followed by an index to the name of the field. So, for example, if a remote entity had a field called `value`, specifying `%value%` in the mathematical expression would mean the last received version of the field `value`. Specifying `%value|1%` would mean the value for the field received before the last one. `%value|0%` is equivalent to simply specifying `%value%`. The number of versions available cached in memory for remote object is specified as a command-line parameter to the context manager. Consequently, mathematical expressions that would be impossible, for example, because not enough versions of the object are available yet will fail gracefully until all necessary conditions are met.

For the special case of UPnP services, the fields will be the
variables that are declared as part of the service. The implementation
will look for a SOAP action that is changes the variable and contains
the keyword "set" or the keyword "get". When setting the variable, the
action should only have one argument, and when getting, no argument at
all.

Conduits are exhibited to external applications under the /conduits
virtual tree of the web server API. Being plug ins, and thus not
entirely part of the architecture itself, the description of their API
has been kept out of this document for brevity. Most conduits provide
an API to establish pairing, to list the existing pairs, modify
existing pairs and destroy them. In most cases, the conduit allow the
usage of the values of an object's field when their names are enclosed
by percentage signs, but also some simple mathematical transformation
on the way to or from the cloud service. For example, the [Twitter](Twitter.md)
conduit is bound to an object and is able to use the values of the
fields in the objects in the tweets that are sent to the service.