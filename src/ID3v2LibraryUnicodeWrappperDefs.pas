//********************************************************************************************************************************
//*                                                                                                                              *
//*     ID3v2 Library V1.1 © 3delite 2007-2009                                                                                   *
//*     See ID3v2 Library Readme.txt for details                                                                                 *
//*                                                                                                                              *
//* Two licenses are available if you like and use this library:                                                                 *
//* Shareware License: 50 Euros                                                                                                  *
//* Commercial License: 250 Euros                                                                                                *
//*                                                                                                                              *
//*     http://www.shareit.com/product.html?productid=300184607                                                                  *
//*                                                                                                                              *
//*     http://www.3delite.hu/Object%20Pascal%20Developer%20Resources/id3v2library.html                                          *
//*                                                                                                                              *
//*     Credits:                                                                                                                 *
//* James Webb @ audioxl.com for the great id3 units Copyright (c) 1998-2001 audioxl.com                                         *
//*                                                                                                                              *
//* If you have any questions or enquiries please mail: 3delite@3delite.hu                                                       *
//*                                                                                                                              *
//* Good coding! :)                                                                                                              *
//* 3delite                                                                                                                      *
//********************************************************************************************************************************

unit ID3v2LibraryUnicodeWrappperDefs;

interface

uses
    Windows,
    ID3v2LibraryDefs;

    function InitID3v2LibraryUnicodeWrappper: Boolean;
    function FreeID3v2LibraryUnicodeWrappper: Boolean;

const
    NAME_ID3v2_LoadUnicode                  = 'ID3v2_LoadUnicode';
    NAME_ID3v2_SaveUnicode                  = 'ID3v2_SaveUnicode';
    NAME_ID3v2_GetUnicodeText               = 'ID3v2_GetUnicodeText';
    NAME_ID3v2_SetUnicodeText               = 'ID3v2_SetUnicodeText';
    NAME_ID3v2_GetUnicodeCOMM               = 'ID3v2_GetUnicodeCOMM';
    NAME_ID3v2_SetUnicodeCOMM               = 'ID3v2_SetUnicodeCOMM';
    NAME_ID3v1_KillUnicode                  = 'ID3v1_KillUnicode';
    NAME_ID3v2_KillUnicode                  = 'ID3v2_KillUnicode';


const
    ID3v2LibraryUnicodeWrappperDLL  = 'ID3v2LibraryUnicodeWrappper.dll';

type
    t_ID3v2_LoadUnicode             = function (Tag: Pointer; FileName: PWideChar): Integer; stdcall;
    t_ID3v2_SaveUnicode             = function (Tag: Pointer; FileName: PWideChar): Integer; stdcall;
    t_ID3v2_GetUnicodeText          = function (Tag: Pointer; FrameName: PANSIChar): PWideChar; stdcall;
    t_ID3v2_SetUnicodeText          = function (Tag: Pointer; FrameName: PANSIChar; Text: PWideChar): Bool; stdcall;
    t_ID3v2_GetUnicodeCOMM          = function (Tag: Pointer): PWideChar; stdcall;
    t_ID3v2_SetUnicodeCOMM          = function (Tag: Pointer; Text: PWideChar): Bool; stdcall;
    t_ID3v1_KillUnicode             = function (FileName: PWideChar): Integer; stdcall;
    t_ID3v2_KillUnicode             = function (FileName: PWideChar): Integer; stdcall;

var
    ID3v2_LoadUnicode: t_ID3v2_LoadUnicode;
    ID3v2_SaveUnicode: t_ID3v2_SaveUnicode;
    ID3v2_GetUnicodeText: t_ID3v2_GetUnicodeText;
    ID3v2_SetUnicodeText: t_ID3v2_SetUnicodeText;
    ID3v2_GetUnicodeCOMM: t_ID3v2_GetUnicodeCOMM;
    ID3v2_SetUnicodeCOMM: t_ID3v2_SetUnicodeCOMM;
    ID3v1_KillUnicode: t_ID3v1_KillUnicode;
    ID3v2_KillUnicode: t_ID3v2_KillUnicode;

var
    ID3v2LibraryUnicodeWrappperDLLHandle: THandle = 0;
    ID3v2LibraryUnicodeWrappperDLLLoaded: Boolean = False;

    WID3v2DLLHandle: THandle = 0;

implementation

Uses
    SysUtils;

function InitID3v2LibraryUnicodeWrappper: Boolean;
begin
    ID3v2LibraryUnicodeWrappperDLLHandle := SafeLoadLibrary(PChar(ID3v2LibraryUnicodeWrappperDLL));
    Result := ID3v2LibraryUnicodeWrappperDLLHandle <> 0;
    if Result then begin
        ID3v2_LoadUnicode       := GetProcAddress(ID3v2LibraryUnicodeWrappperDLLHandle, PChar(NAME_ID3v2_LoadUnicode));
        ID3v2_SaveUnicode       := GetProcAddress(ID3v2LibraryUnicodeWrappperDLLHandle, PChar(NAME_ID3v2_SaveUnicode));
        ID3v2_GetUnicodeText    := GetProcAddress(ID3v2LibraryUnicodeWrappperDLLHandle, PChar(NAME_ID3v2_GetUnicodeText));
        ID3v2_SetUnicodeText    := GetProcAddress(ID3v2LibraryUnicodeWrappperDLLHandle, PChar(NAME_ID3v2_SetUnicodeText));
        ID3v2_GetUnicodeCOMM    := GetProcAddress(ID3v2LibraryUnicodeWrappperDLLHandle, PChar(NAME_ID3v2_GetUnicodeCOMM));
        ID3v2_SetUnicodeCOMM    := GetProcAddress(ID3v2LibraryUnicodeWrappperDLLHandle, PChar(NAME_ID3v2_SetUnicodeCOMM));
        ID3v1_KillUnicode  := GetProcAddress(ID3v2LibraryUnicodeWrappperDLLHandle, PChar(NAME_ID3v1_KillUnicode));
        ID3v2_KillUnicode  := GetProcAddress(ID3v2LibraryUnicodeWrappperDLLHandle, PChar(NAME_ID3v2_KillUnicode));
    end;
    if (@ID3v2_LoadUnicode = nil)
    OR (@ID3v2_SaveUnicode = nil)
    OR (@ID3v2_GetUnicodeText = nil)
    OR (@ID3v2_SetUnicodeText = nil)
    OR (@ID3v2_GetUnicodeCOMM = nil)
    OR (@ID3v2_SetUnicodeCOMM = nil)
    OR (@ID3v1_KillUnicode = nil)
    OR (@ID3v2_KillUnicode = nil)
        then Result := False;

    if Result
        then ID3v2LibraryUnicodeWrappperDLLLoaded := True;
end;

function FreeID3v2LibraryUnicodeWrappper: Boolean;
begin
    Result := False;
    if ID3v2LibraryUnicodeWrappperDLLHandle <> 0 then begin
        Result := FreeLibrary(ID3v2LibraryUnicodeWrappperDLLHandle);
        if Result
            then ID3v2LibraryUnicodeWrappperDLLLoaded := False;
    end;
    if WID3v2DLLHandle <> 0 then begin
        Result := FreeLibrary(WID3v2DLLHandle);
    end;
end;

Initialization
    InitID3v2LibraryUnicodeWrappper;
Finalization
    FreeID3v2LibraryUnicodeWrappper;
end.
