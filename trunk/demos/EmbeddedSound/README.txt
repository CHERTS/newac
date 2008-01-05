This demo shows how to play a sound file compiled into an application as a resource (*.res).
If you don't know how to embed binaries as resources into your application,
there is a good tutorial for you:
http://delphi.about.com/od/objectpascalide/a/embed_resources.htm
We embed file kalimba.wma as a resource. We get access to this embedded resource using
TResourceStream class which is a TStream descendant. All we need now is to assign a
TResourceStream instance to the WMIn1.Stream property. Now our application (sort of alarm clock)
can play sound from its internal data, not from an external file (when you compile the app you will see
that it doesn't need kalimba.wma to play sound).
