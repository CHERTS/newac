
ID3v2 Library - Copyright (c) 2007-2009 3delite. All rights reserved.
=====================================================================

NOTE: To run the EXEs, first you'll have to copy id3v2Library.dll (and ID3v2LibraryUnicodeWrappper.dll if using Delphi 2009) into the same directory as the EXEs.

NOTE: Old ID3v2.2 Tags (with 3 character frame names) are not supported!


What's the point?
=================
ID3v2 Library is a component (.dll) for use in Win32 (9x/ME/2K/XP/Vista) software.

You should also see the included example program ID3Tester's source-code for example of how to use ID3v2 Library in your own programs.


Requirements:
=============
Any dev. environment that supports the stdcall calling convention (some ID3v2 functions require Delphi).

Error codes:
============

all return a WORD containing two 1-byte codes. The
1st byte represents an integer (0-255) and are error
codes if the tag was not properly loaded/saved.

0   = ok
1   = file couldn't be opened
2   = file couldn't be opened for writing
3   = Couldn't read from file
4   = Couldn't write to file
5   = Unspecified I/O Error
8   = Specified tag location past EOF
16  = ID3v2 header not found
17  = ID3v2 MAJOR version not supported
18  = ID3v2 MINOR version not supported
32  = Bad specified tag size
33  = Bad Frame ID found
34  = Tag exceeds size limits
64  = Unknown encryption method
    This library doesn't support:
192 = Encryption
193 = Grouping
194 = Data length could not be calculated because the data is encrypted
    General Errors:
252 = Tag is still in the process of loading
253 = No frames in the tag
254 = Bad ID3v2 tag
255 = Unknown Error Encountered


The 2nd byte is for warning flags. If set, the tag
loaded/saved fine, but some unknown/unsupported
elements were found.

%abcdef00

a = unknown flags set in header
b = unknown flags set in extended header
c = unknown flags set in frame header(s)
d = ID3v2 Minor version higher than latest supported
e = Unsupported ID3v2 feature bypassed
f = Extended header padding size and actual padding size do not match


Latest Version
==============
The latest version of ID3v2 Library can always be found at 3delite's website:

http://www.3delite.hu/Object Pascal Developer Resources/id3v2library.html


Copyright, Disclaimer, and all that other jazz
==============================================
This software is provided "as is", without warranty of ANY KIND, either expressed or implied, including but not limited to the implied warranties of merchantability and/or fitness for a particular purpose. The author shall NOT be held liable for ANY damage to you, your computer, or to anyone or anything else, that may result from its use, or misuse. Basically, you use it at YOUR OWN RISK.

Usage of ID3v2 Library indicates that you agree to the above conditions.

You may freely distribute the ID3v2 Library package as long as NO FEE is charged and all the files remain INTACT AND UNMODIFIED.

All trademarks and other registered names contained in the ID3v2 Library package are the property of their respective owners.


ID3v2 Library in shareware and commercial software?
======================================================
You can use this component in your free programs for free. If like it and use it for shareware and commercial you must register.

Shareware License: 50 Euros, for usage of the component in an unlimited number of your shareware software.

http://www.shareit.com/product.html?productid=300184607

Commercial License: 250 Euros, for usage of the component in a single commercial product.

http://www.shareit.com/product.html?productid=300184612

In all cases there are no royalties to pay, and you can use all future updates without further cost, all you need to do is just obtain the newest version.
If none of these licenses match your requirements, or if you have any questions, get in touch (3delite@3delite.hu).


Installation:
=============

Add the directory to the search path, and to Uses add: ID3v2LibraryDefs and ID3v2LibraryUnicodeWrappperDefs if using Delphi 2009.


Credits
=======
* James Webb @ audioxl.com for the great id3 units Copyright (c) 1998-2001 audioxl.com


Bug reports, Suggestions, Comments, Enquiries, etc...
=====================================================
If you have any of the aforementioned please email:

3delite@3delite.hu


History
=======
1.0 - 30/10/2007
----------------
First release.

1.1.2.42 - 13/01/2008
---------------------
*ID3v2_GetAlbumPicture() and ID3v2_GetAlbumPictureStream() supports wider range of APIC headers

1.1.2.44 - 24/09/2008
---------------------
*Fixed saving the tags into files that do not exists (the file is automatically created)

1.1.2.47 - 10/12/2008
---------------------
*Fixed a major bug in ID3v2_SetFrameData() that caused crashes

1.1.2.49 - 28/12/2008
---------------------
+Added unicode wrapper for loading/saving unicode file names

