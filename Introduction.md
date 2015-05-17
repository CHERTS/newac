# Introduction #

> An introduction to NewAC, Delphi components for manipulating audio data.

# Author #
> Andrei Borovsky, anb@symmetrica.net

Contributors:
> Several people have made contributions to the NewAC code.

  * Lev Amelin: improved and extended TMP3Out component.
  * Thomas la Cour (http://www.top-house.dk/~nr161/delphi/): providing Monkey Audio input/output components.
  * Jan Henrik Ejme: adding ACM-playing support.
  * Thomas Grelle: improving CDRip support.
  * Jan Bartels (ATR Industrie-Elektronik GmbH & Co. KG, Viersen, Germany): some useful corrections and suggestions.
  * Sergei Borisov: quite a few contributions, including TWVIn and TWVOut components.
  * Eriks Aleksans: many useful suggestions.
  * Wayne Thursby: documentation and not breaking things

# On the Web #

> ## Project Homepage ##
> > You can find the official homepage of NewAC at http://www.symmetrica.net/newac.


> ## Project Discussion ##
> > Discussion and support can be found on the Google Group newac-users at http://groups.google.com/group/newac-users.


> ## Project Source ##
> > The source code for NewAC is now hosted on Google Code http://code.google.com/p/newac/.

# Version History #

NewAC 1.3.2
  * Improved WMA encoder
  * Object Model has changed: SuspendWhenIdle property and WaitForStop method are now absolete.

NewAC v1.3.1
  * MP3 decoding is now done with Windows decoder (additional DLL (mpadec.dll) no longer needed)

NewAC v1.3
  * Windows Media files input support (TWMIn component), Windows Media Audio (wma) files output support (TWMAOut component).
  * Event handling improved

NewAC v1.2
  * WavPack format support (TWVIn, TWVOut components).
  * 24-bit sound support.
  * Unicode file names support (via the WideFileName property).
  * Large (larger than 2 GB) files support.

NewAC v1.1
  * Base class interfaces is changed to make data flow vore efficient.
  * FLAC and Monkey's Audio codecs are updated.
  * New TAudioConverter and TResampler components are added.
  * New MP3 decoder support is added to make MP3 input seekable.

NewAC v1.0.1
  * Ogg Vorbis bugs are fixed.
  * DirectSound API is implemented in Delphi, so there is no more need in dswrapper DLL.
  * Base classes are renamed from TACS**to TAu**.
  * Some minor bugs are fixed.

NewAC v1.0
  * Some bugs are fixed.
  * TMP3In component is added for mp3 playback.
  * TCDPlayer can now work with several drives.

ACS v2.3
  * Changes in the object model: Event and thread handling improved.
  * DirectX input/output has been added (TDXAudiIn and TDXAudiOut components).
  * TMP3Out component has been improved.

# Introduction to NewAC Programming #

## Basic Operation ##

The New Audio Components package is a set of components designed to handle different sound processing tasks, such as reading and storing data in different audio formats, working with sound hardware, audio streams mixing and so on.

Most of the components in the New Audio Components package belong to one of three categories: input components, output components, converter components. There are also two components that do not fit into this scheme. These arfe TCDPlayer component and TMixer component.

The input components acquire audio input from some source (device, disk file, etc.) and convert it into a digital audio stream which is the internal audio representation in NewAC. The output components take in the digital audio produced by input components and send it to some audio output device or store it on disk in a specified audio file format. Converter components perform different operations on audio streams. These components take in a digital audio stream and output a stream which is modified according to the function of the component. The following diagram illustrates the flow of audio data in the NewAC processing chain.

(see diagram.png)

All components are linked via their Input properties. Arrows show connections between components (from WaveOut1 to AudioIn1). Audio data flows through the chain in the opposite direction, from AudioIn1 to WaveOut1. In this example we read audio data from the soundcard input component AudioIn1 (input component), pass it through the RateConverter1 component (converter component) and store in a WAV file via WaveOut1 component (output component).

The converter component is not a necessary element in this chain. In many cases you will just connect input and output components together to convert data from one format to the other or to perform I/O with some particular device. As you can see NewAC isn't just a collection of components implementing interfaces to some hardware or file formats. It is rather a set of building blocks with which you can construct a complete sound-processing application.

The driving force of this sound processing chain is an output component. Output components use internal thread objects that call data-retrieving functions of the input/converter components attached to the output component. Note that NewAC provides you with several ways to set the amount of CPU time consumed by these threads: TheradPriority, Delay, and SuspendWhenIdle properties. You can use this features to set the best performance/CPU usage ratio in your NewAC applications.

## Ripping a CD to OGG ##

Suppose you want your application to convert data from audio CD tracks to Ogg Vorbis audio compression format. First, you put into your form a TCDIn input component that reads data from CD tracks (don't confuse this component with TCDPlayer component which is designed to play CD tracks). Now you need an output component, and in our case it is VorbisOut. In order to chain two components together you have to assign an instance of the TCDIn component to the VorbisOut's Input property.

```
VorbisOut1.Input := CDIn1;
```

Now set the CD track you want to be converted.

```
CDIn1.StartTrack := 1;
CDIn1.EndTrack := 1;
```

This tells the component to read only one track (the first track on the CD).

Now you have to set up a file name for an output file.

```
VorbisOut1.FileName := 'Track1.ogg';
```

And desired compression ratio.

```
VorbisOut1.Compression := 0.1;
```

Believe it or not, you have now set up everything for the data conversion. In order to perform the conversion, call Run method is used.

```
VorbisOut1.Run; 
```

The actual compression process runs in its own thread, while your main thread can do something else. You can control how conversion goes on by checking VorbisOut1's Progress property. When the conversion is finished, the VorbisOut1.OnDone property will be called.

If you disable some application's controls while NewAC components perform their action and enable them back in the OnDone event handler (like it is done in most of the demos), make sure you disable the controls before calling the Run method. Otherwise these controls may be not automatically reenabled if an error occurs during NewAC operation.

You get access to an input component's data directly in a non-threaded way. The following example shows how data can be obtained from an audio CD with the TCDIn component.

```

var
  Len, DataSize : Integer;
  Data : Pointer;
begin
  ...
  CDIn1.StartTrack := 1;
  CDIn1.EndTrack := 1;
  CDIn1.Init;
  Len := CDIn1.CopyData(Data, DataSize);
  while Len <> 0 do
  begin
    // Process CD data
    Len := CDIn1.CopyData(Data, DataSize);
  end;
  CDIn1.Flush;
  ...
end;

```

## Converting Stereo to Mono ##

To get stereo audio data from a WAV file, convert it to mono and save to another WAV file, we will need an instance of the TWaveIn component. It reads data from a WAV file, an instance of the TMSConverter component then converts the stereo stream to mono, and an instance of the TWaveOut component that writes data to a WAV file. Now we chain these three components.

```

WaveIn1.FileName := 'input.wav';
MSConverter1.Input := WaveIn1;
WaveOut1.Input := MSConverter1;
WaveOut1.FileName := 'output.wav';
WaveOut1.Run;

```

Converter components take input from input components or from other converters and output data to output components or other converters. It is possible to chain any number of converters this way. There is a special converter in NewAC called AudioMixer. This component takes in two audio streams and mixes them into a single output stream. In the new version of NewAC AudioMixer can also concatenate to incoming audio streams, i.e. make one stream where two input streams go consecutively.

## Using Streams ##

Almost all NewAC components that read data from audio files or store data in audio file format can work with both disk files and memory streams (currently only TMACIn amd TMACOut components do not support general streams). You can use FileName property to specify the name of the file on disk (or the WideFileName property if the file name is in Unicode) or the Stream property to assign some other type of stream (including a memory stream TMemoryStream) to an input or an output component.

## Stopping and Pausing ##

You can stop a playing chain by calling an output component's Stop method. If you want to pause playback, call the output component's Pause method, and call Resume to resume the paused playback. Input components that read data from disc files and memory streams allow seeking. Use an input component's Seek method to set the current input position (the position is measured from the beginning of data in frames, not in bytes).

## Terminating the Playback ##

You can call the output component’s Stop method to terminate an ongoing playback at any time, but because of the multithreaded nature of NewAC the playback doesn’t stop at once when requested. There are two modes of behavior when you call a Stop method. The first mode is asynchronous. In this mode the called Stop returns at once (may be before the output component is stopped). When the output components is actually stopped your application gets an OnDone event. In this mode you can use OnDone event handler to get notified when your App’s audio chain finishes playing. Sometimes you don’t want to wait for OnDone event. For example it is a good practice to stop any current output in the Application’s Form OnClose Event. In that case you can call Stop in synchronous mode. In this mode Stop blocks tha calling thread untill the output is actually stopped. No OnDone event is raised in this mode.. To call Stop in asynchronous mode pass True as a value of its only argument (this is the default value, so calling Stop without an argument stops asynchronously). To make synchronous Stop call pass the False value as the method's arguemt. There are two ways of calling Stop.

```
WaveOut1.Stop;  // Asynchronous call 
...	// Wait for an OnDone event to know when the output is done 
```
or
```
WaveOut1.Stop(False); // Synchronous call. When it returns the output is actually done
```

# Chaining Audio Tasks #

In some cases you may want to start the next audio task automatically right after the previous task has been finished. Suppose you build an audio player with a kind of playlist. You want the files in the playlist to be played one after another. When the current file playback or other output operation is finished output component generates OnDone event indicating it has finished current output operation. Starting from the NewAC version 1.0 you can set new input source component (or reset current input properties) from OnDone event handler. See AudioPlayer demo program for an example.

# Editing Sound #

Several sound-editing operations can be performed with NewAC. Converter components allow you to change the number of channels, bits per sample, and the sample rate of an audio stream. With the TAudioMixer component you can mix together two audio streams, selecting volume level values for each stream, and concatenate two streams into a single styream. See the AudioMixer demo for streams mixing and concatenation example.

TWaveOut and TVorbisOut components allow you to append audio data to the already existing files. Set the component's FileMode property to foAppend to append data to an existing file rather than rewriting it.


All input components that descend from TAuFileIn allow you to specify StartSample and EndSample properties. If you set these properties' values, the actual data reading will start from the StartSample and will stop at EndSample value (the total number of samples in the input stream is returned via TotalSamples property, and since it is read from the file, you should check the Valid property value before reading TotalSamples). This way you can select a fragment of an input audio file, rather than a whole file, for further processing. AudioCutter demo shows how to paly back and save selected fragments of audio files. The StartSample and EndSample properties aloow to select audio file fragments with the maximum possible precision. If you don't need that precision you can use SetStartTime and SetEndTime methods that specify start and end time for the selection in minutes:seconds format.

# Handling Thread Exceptions #

NewAC does its best to recover from any error that might occur. If an exception is raised within NewAC after you have called an output component's Run method it is not propagated. The OnThreadException event is generated insteead and the exception's text is stored in the output component's ExceptionMessage property. After that an OnDone event is generated as usual. You can determine if an error has haappened during the NewAC operation by assigning a handler to the OnThreadException event or by checking the ExceptionMessage value in an OnDone event handler (it should be an empty string if no error took place). See CDRipper and Wave2Ogg-2 demos for how error-handling is done.

# Input Switching #

Starting from the ACS version 2.2 most of the output components allow assigning new input on the fly (i.e. while the component is doing playback). It is important to remember that when changing input on the fly, new input audio stream parameters must be thhe same as the old ones. Note that TAudioProcessor Component doesn't allow modifing Input property while it is playing. InputList component allows modifing all input items at anytime except the item, currently being played. You can switch to other InputList's item on the fly by changing the components CurrentInput property.


# Avoiding Common Errors #

This section explains how to avoid some errors that are most often encountered by the programmers beginning to use NewAC.

  * Never call an ouput component's Run method if the component is busy. You can check if it is by inspecting the value of the Status property. Ouput component goes to the busy state after its Run method is called. When component becomes idle, its OnDone event is triggered.
  * With the TCDIn component it is a good practice to check if the right kind of media is in the drive before calling any disc-concerning method. You can check the CD in the drive by means of DiscInfo and Status properties.
  * When using file input components always check if the assigned file is valid with Valid property before reading any file description properties or calling Run method.

# Legacy Components #

Some NewAC components are now considered legaacy. They are kept for backward compatability and their development is stopped. For all these components there are analogous newer components that should be used instead. Here is the list of the current legacy components and their replacements.

  * TAudioIn (should be replaced with TDXAudioIn in new programs).
  * TAudioOut (should be replaced with TDXAudioOut in new programs).
  * TSampleConverter (TSampleConverter's functionality is duplicated in TAudioConverter, which additionally supports 24 bit sound conversion).
  * TRateConverter (should be replaced with TResampler in new programs).
  * TMSConverter (TMSConverter's functionality is duplicated in TAudioConverter, which additionally supports 24 bit sound conversion).

# The Third-party Libraries #

Some of NewAC components require certain shared libraries, which are not included in the package but are publicly available and may be found on many sites. In order to use VorbisIn and VorbisOut components you will need Ogg Vorbis shared libraries. The original libraries are available at www.xiph.org Important note: the alternative versions of Ogg Vorbis libraries specially adapted for use in Delphi applications are available. It is recommended to use the adapted libraries with NewAC. You can download these libraries either in binary or in the source code form from the NewAC Site.


> Binaries - http://symmetrica.net/uploads/newac/windlls.zip
> Sources - http://symmetrica.net/index.php?page=libsources

It is important to note that since all libraries required by NewAC components are loaded dynamically at run-time, you can use NewAC as a whole without these libraries. Of course, those components requiring certain library will only work if the library is found.

  * TFLACIn and TFLACOut components depend on libFLAC.dll library under Windows and libFLAC.so library under Linux.
  * TMP3ToWav component requires MADLib.dll library.
  * Monkey Audio components require MacDLL.dll library. This library is included into windlls.zip archive (see below). You can also download it with Monkey Audio SDK available on the Net.
  * Under Windows TCDIn component requires CDRip.dll library distibuted under GPL (I use the variant of the library distributed with NeoAudio).
  * TResampler component requires the libsamplerate library.
  * TMP3Out component requires lame\_enc.dll (under Windows) or libmp3lame.so (under Linux).
Note:
> LAME is an encoder created for educational purposes only. It may be illegal to use LAME in your Country. Check this out.

# Installation #

Since NewAc is an ordinary Delphi package its installation procedure should be straightforward for most of the Delphi users.

If you have installed previous version of ACS or NewAC, uninstall it before installing the new version. Click Component->Install Packages menu item. In the opened dialog box select the Audio Components Suite package and click "Remove" button. Make sure to remove all the previous version files from the IDE search path.

To install New Audio Components package, first compile the package. Create or select directory where you want the components to be installed (you can use your Delphi lib directory). We will call this directory NewAC directory. Now go to the Source directory of the NewAC distribution, and copy all the files to the NewAC directory. Go to the NewAC directory, open NewAC.dpk file in your IDE. Press "Compile" button in the package manager window. Press Install button.. After this the two new tabs: "Audio I/O" and "Audio Processing" will appear on the components palette. Unless you have selected your Delphi lib directory as NewAC directory, you will have to add the NewAC installation directory to the Delphi libraries search path. This can be done on the "Library" page of the "Environment Options" dialog box (Tools->Environment Options... menu item).

# Writing Your Own Components #

Eventually you may want to extend NewAC with your own components. It will be great if you make these components available for the entire community (although you are not required to do so). How to write a new component for NewAC? You have the sources of all the components, and they all can be your guides. But most component's sources contain lots of component-specific stuff. It is not easy to tell from the first site, what is required, and what is arbitrary. Undestanding this I have provided a special input/output components' writer guide. You can find it in the file called cwguide.htm. Two demo components, TDemoWaveIn and TDemoWaveOut, are written specially for this tutrorial. These two components are not the part of the main NewAC package, you can find them in the file ComponentsDemo.pas in the Demos\ComponentsDemo folder. These demo components handle simple input and output tasks, and the sources are extensively commented (unlike other components' sources, I must admit).

# Licenses #

The NewAC code itself is distributed free of charge, under a simple license that allows an unrestricted use (both non-commercial and commercial). See the license.txt for more detail.The third-party libraries required by different NewAC components are distributed under different licences.

  * The ogg.dll, vorbis.dll, vorbisfile.dll, and vorbisenc.dll libraries are distributed under the GPLv2.
  * The libFLAC.dll library is distributed under the GPLv2.
  * The libsamplerate.dll library is distributed under the GPLv2.
  * The MACDll.dll library is distributed under the custom license (http://www.monkeysaudio.com for more details).
  * The CDRip.dll library is distributed under the GPLv2.