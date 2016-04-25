# History #

The context manager is able to save all changes being made to objects to a set of CSV files.  An separate application is able to replay these updates at a later time, possibly at a quicker pace.

# Saving #

By default, the context manager will automatically save all changes made to objects to CSV files in a directory which is pointed at by the `-history` option to the main program. These files are organised so that the first column contains a timestamp for the change to the object, and the last column contains a description of the changes that were made to the object. All columns in between contain a complete description of the object, one field for one column. The format of the last column, containing the description of the changes that were made at the time of the update is of the list of fields changed, separated by the `|` (pipe) sign.

# Replaying #

Replaying updates for one or several objects is made possible by the `replay` application, which is able to use both the regular web API and the streaming facilities based on the WebSockets implementation. The application is able to replay at a quicker pace and also to map from source (save) UUIDs to destination UUIDs if necessary.

The application is able to use a "controller" object in the context manager, i.e. an object that will be able to describe the current state of the replaying operations, but also able to send back "commands" to the replay application.  The object should have the following fields:

  * a field called `timestamp` (an `Integer`) that will contain the current time and date of the replaying.
  * a field called `state` (a `String`) that will contain the state of the replaying, but also can be used to modify the state of the replaying. The state is initially at `INIT` and will change to `PLAY` once replaying has started.  Changing it to `PAUSE` will pause the replaying, while changing it back to `PLAY` will continue.