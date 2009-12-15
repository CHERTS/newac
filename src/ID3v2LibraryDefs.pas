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

unit ID3v2LibraryDefs;

interface

uses
  Windows, SysUtils, Classes;

    function InitID3v2Library: Boolean;
    function FreeID3v2Library: Boolean;

const
    ID3v2DLL = 'ID3v2Library.dll';

const
    NAME_ID3v1_Create           = 'ID3v1_Create';
    NAME_ID3v1_Free             = 'ID3v1_Free';
    NAME_ID3v1_Load             = 'ID3v1_Load';
    NAME_ID3v1_Save             = 'ID3v1_Save';
    NAME_ID3v1_Kill             = 'ID3v1_Kill';
    NAME_ID3v1_GetTitle         = 'ID3v1_GetTitle';
    NAME_ID3v1_SetTitle         = 'ID3v1_SetTitle';
    NAME_ID3v1_GetArtist        = 'ID3v1_GetArtist';
    NAME_ID3v1_SetArtist        = 'ID3v1_SetArtist';
    NAME_ID3v1_GetAlbum         = 'ID3v1_GetAlbum';
    NAME_ID3v1_SetAlbum         = 'ID3v1_SetAlbum';
    NAME_ID3v1_GetYear          = 'ID3v1_GetYear';
    NAME_ID3v1_SetYear          = 'ID3v1_SetYear';
    NAME_ID3v1_GetComment       = 'ID3v1_GetComment';
    NAME_ID3v1_SetComment       = 'ID3v1_SetComment';
    NAME_ID3v1_GetTrack         = 'ID3v1_GetTrack';
    NAME_ID3v1_SetTrack         = 'ID3v1_SetTrack';
    NAME_ID3v1_GetGenre         = 'ID3v1_GetGenre';
    NAME_ID3v1_SetGenre         = 'ID3v1_SetGenre';
    NAME_ID3v1_GenreByte2Text   = 'ID3v1_GenreByte2Text';
    NAME_ID3v1_Rev1             = 'ID3v1_Rev1';
    NAME_ID3v1_GenreListToData  = 'ID3v1_GenreListToData';
    NAME_ID3v1_GenreDataToList  = 'ID3v1_GenreDataToList';

const
    NAME_ID3v2_Create               = 'ID3v2_Create';
    NAME_ID3v2_Free                 = 'ID3v2_Free';
    NAME_ID3v2_Kill                 = 'ID3v2_Kill';
    NAME_ID3v2_Load                 = 'ID3v2_Load';
    NAME_ID3v2_LoadFromStream       = 'ID3v2_LoadFromStream';
    NAME_ID3v2_Loaded               = 'ID3v2_Loaded';
    NAME_ID3v2_Save                 = 'ID3v2_Save';
    NAME_ID3v2_SaveToStream         = 'ID3v2_SaveToStream';
    NAME_ID3v2_GetAsciiText         = 'ID3v2_GetAsciiText';
    NAME_ID3v2_SetAsciiText         = 'ID3v2_SetAsciiText';
    NAME_ID3v2_FrameCount           = 'ID3v2_FrameCount';
    NAME_ID3v2_GetFrameData         = 'ID3v2_GetFrameData';
    NAME_ID3v2_SetFrameData         = 'ID3v2_SetFrameData';
    NAME_ID3v2_CheckFrameExists     = 'ID3v2_CheckFrameExists';
    NAME_ID3v2_GetFrameID           = 'ID3v2_GetFrameID';
    NAME_ID3v2_DeleteFrame          = 'ID3v2_DeleteFrame';
    NAME_ID3v2_CreateFrame          = 'ID3v2_CreateFrame';
    NAME_ID3v2_CompressFrame        = 'ID3v2_CompressFrame';
    NAME_ID3v2_DeCompressFrame      = 'ID3v2_DeCompressFrame';
    NAME_ID3v2_AddAlbumPicture      = 'ID3v2_AddAlbumPicture';
    NAME_ID3v2_GetAlbumPicture      = 'ID3v2_GetAlbumPicture';
    NAME_ID3v2_GetAlbumPictureStream  = 'ID3v2_GetAlbumPictureStream';
    NAME_ID3v2_SyncFrame            = 'ID3v2_SyncFrame';
    NAME_ID3v2_UnSyncFrame          = 'ID3v2_UnSyncFrame';
    NAME_ID3v2_SetCOMM              = 'ID3v2_SetCOMM';
    NAME_ID3v2_GetCOMM              = 'ID3v2_GetCOMM';
    NAME_ID3v2_SetWXXX              = 'ID3v2_SetWXXX';
    NAME_ID3v2_GetWXXX              = 'ID3v2_GetWXXX';
    NAME_ID3v2_GetFrameDataP        = 'ID3v2_GetFrameDataP';
    NAME_ID3v2_SetFrameDataP        = 'ID3v2_SetFrameDataP';
    NAME_ID3v2_GetFrameSize         = 'ID3v2_GetFrameSize';
    NAME_ID3v2_FrameTypeCount       = 'ID3v2_FrameTypeCount';
    NAME_ID3v2_TagSize              = 'ID3v2_TagSize';
    NAME_ID3v2_TagMajorVer          = 'ID3v2_TagMajorVer';
    NAME_ID3v2_TagMinorVer          = 'ID3v2_TagMinorVer';
    NAME_ID3v2_FrameIsCompressed    = 'ID3v2_FrameIsCompressed';
    NAME_ID3v2_GetGenre             = 'ID3v2_GetGenre';

type
    t_ID3v1_Create              = function: Pointer; stdcall;
    t_ID3v1_Free                = function (Tag: Pointer): Boolean; stdcall;
    t_ID3v1_Load                = function (Tag: Pointer; FileName: PANSIChar): Boolean; stdcall;
    t_ID3v1_Save                = function (Tag: Pointer; FileName: PANSIChar): Boolean; stdcall;
    t_ID3v1_Kill                = function (Tag: Pointer): Integer; stdcall;
    t_ID3v1_GetTitle            = function (Tag: Pointer): PANSIChar; stdcall;
    t_ID3v1_SetTitle            = function (Tag: Pointer; Title: PANSIChar): Boolean; stdcall;
    t_ID3v1_GetArtist           = function (Tag: Pointer): PANSIChar; stdcall;
    t_ID3v1_SetArtist           = function (Tag: Pointer; Artist: PANSIChar): Boolean; stdcall;
    t_ID3v1_GetAlbum            = function (Tag: Pointer): PANSIChar; stdcall;
    t_ID3v1_SetAlbum            = function (Tag: Pointer; Album: PANSIChar): Boolean; stdcall;
    t_ID3v1_GetYear             = function (Tag: Pointer): PANSIChar; stdcall;
    t_ID3v1_SetYear             = function (Tag: Pointer; Year: PANSIChar): Boolean; stdcall;
    t_ID3v1_GetComment          = function (Tag: Pointer): PANSIChar; stdcall;
    t_ID3v1_SetComment          = function (Tag: Pointer; Comment: PANSIChar): Boolean; stdcall;
    t_ID3v1_GetTrack            = function (Tag: Pointer): Integer; stdcall;
    t_ID3v1_SetTrack            = function (Tag: Pointer; Track: Byte): Boolean; stdcall;
    t_ID3v1_GetGenre            = function (Tag: Pointer): Integer; stdcall;
    t_ID3v1_SetGenre            = function (Tag: Pointer; Genre: Integer): Boolean; stdcall;
    t_ID3v1_GenreByte2Text      = function (Number: Byte): PANSIChar; stdcall;
    t_ID3v1_Rev1                = function (Tag: Pointer): Bool; stdcall;
    t_ID3v1_GenreListToData     = function (Genre: Integer): Integer; stdcall;
    t_ID3v1_GenreDataToList     = function (Genre: Integer): Integer; stdcall;

type
    t_ID3v2_Create              = function: Pointer; stdcall;
    t_ID3v2_Free                = function (Tag: Pointer): Boolean; stdcall;
    t_ID3v2_Kill                = function (Tag: Pointer): Integer; stdcall;
    t_ID3v2_Load                = function (Tag: Pointer; FileName: PANSIChar): Integer; stdcall;
    t_ID3v2_LoadFromStream      = function (Tag: Pointer; var TagStream: TStream; Location: Int64 = 0): Integer; stdcall;
    t_ID3v2_Loaded              = function (Tag: Pointer): Boolean; stdcall;
    t_ID3v2_Save                = function (Tag: Pointer; FileName: PANSIChar): Integer; stdcall;
    t_ID3v2_SaveToStream        = function (Tag: Pointer; var TagStream: TStream): Integer; stdcall;
    t_ID3v2_GetAsciiText        = function (Tag: Pointer; FrameName: PANSIChar): PANSIChar; stdcall;
    t_ID3v2_SetAsciiText        = function (Tag: Pointer; FrameName, Text: PANSIChar): Boolean; stdcall;
    t_ID3v2_FrameCount          = function (Tag: Pointer): Cardinal; stdcall;
    t_ID3v2_GetFrameData        = function (Tag: Pointer; FrameIndex: Cardinal; var Data: TStream): Integer; stdcall;
    t_ID3v2_SetFrameData        = function (Tag: Pointer; FrameIndex: Cardinal; var Data: TStream): Integer; stdcall;
    t_ID3v2_CheckFrameExists    = function (Tag: Pointer; FrameID: PANSIChar): Integer; stdcall;
    t_ID3v2_GetFrameID          = function (Tag: Pointer; FrameIndex: Cardinal): PANSIChar; stdcall;
    t_ID3v2_DeleteFrame         = function (Tag: Pointer; FrameIndex: Cardinal): Boolean; stdcall;
    t_ID3v2_CreateFrame         = function (Tag: Pointer; FrameID: PANSIChar): Cardinal; stdcall;
    t_ID3v2_CompressFrame       = function (Tag: Pointer; FrameIndex: Cardinal): Boolean; stdcall;
    t_ID3v2_DeCompressFrame     = function (Tag: Pointer; FrameIndex: Cardinal): Boolean; stdcall;
    t_ID3v2_AddAlbumPicture     = function (Tag: Pointer; PictureFileName: PANSIChar; PictureFileFormat: Cardinal = 0): Boolean; stdcall;
    t_ID3v2_GetAlbumPicture     = function (Tag: Pointer; FrameIndex: Cardinal; PictureFileName: PANSIChar): Cardinal; stdcall;
        //* FrameIndex := 0 means first available
        //*               1 means first frame, 2 second frame, etc.
        //* PictureFormat := 1 means JPG, 2 PNG, 3 BMP, 4 GIF
    t_ID3v2_GetAlbumPictureStream = function (Tag: Pointer; FrameIndex: Cardinal; PictureStream: TStream): Cardinal; stdcall;
    t_ID3v2_SyncFrame           = function (Tag: Pointer; FrameIndex: Cardinal): Integer; stdcall;
    t_ID3v2_UnSyncFrame         = function (Tag: Pointer; FrameIndex: Cardinal): Integer; stdcall;
    t_ID3v2_SetCOMM             = function (Tag: Pointer; Comment, Description: PANSIChar): Integer; stdcall;
    t_ID3v2_GetCOMM             = function (Tag: Pointer): PANSIChar; stdcall;
    t_ID3v2_SetWXXX             = function (Tag: Pointer; URL: PANSIChar): Integer; stdcall;
    t_ID3v2_GetWXXX             = function (Tag: Pointer): PANSIChar; stdcall;
    t_ID3v2_GetFrameDataP       = function (Tag: Pointer; FrameIndex: Cardinal; var Data: Pointer; var Length: Cardinal): Integer; stdcall;
    t_ID3v2_SetFrameDataP       = function (Tag: Pointer; FrameIndex: Cardinal; Data: Pointer; Length: Cardinal): Integer; stdcall;
    t_ID3v2_GetFrameSize        = function (Tag: Pointer; FrameIndex: Cardinal): Cardinal; stdcall;
    t_ID3v2_FrameTypeCount      = function (Tag: Pointer; FrameName: PANSIChar): Cardinal; stdcall;
    t_ID3v2_TagSize             = function (Tag: Pointer): Int64; stdcall;
    t_ID3v2_TagMajorVer         = function (Tag: Pointer): Integer; stdcall;
    t_ID3v2_TagMinorVer         = function (Tag: Pointer): Integer; stdcall;
    t_ID3v2_FrameIsCompressed   = function (Tag: Pointer; FrameIndex: Integer): Bool; stdcall;
    t_ID3v2_GetGenre            = function (Tag: Pointer): PANSIChar; stdcall;

//type
//    Tid3Tag = Pointer;
//    Tid3v2Tag = Pointer;

var
    ID3v1_Create: t_ID3v1_Create;
    ID3v1_Free: t_ID3v1_Free;
    ID3v1_Load: t_ID3v1_Load;
    ID3v1_Save: t_ID3v1_Save;
    ID3v1_Kill: t_ID3v1_Kill;
    ID3v1_GetTitle: t_ID3v1_GetTitle;
    ID3v1_SetTitle: t_ID3v1_SetTitle;
    ID3v1_GetArtist: t_ID3v1_GetArtist;
    ID3v1_SetArtist: t_ID3v1_SetArtist;
    ID3v1_GetAlbum: t_ID3v1_GetAlbum;
    ID3v1_SetAlbum: t_ID3v1_SetAlbum;
    ID3v1_GetYear: t_ID3v1_GetYear;
    ID3v1_SetYear: t_ID3v1_SetYear;
    ID3v1_GetComment: t_ID3v1_GetComment;
    ID3v1_SetComment: t_ID3v1_SetComment;
    ID3v1_GetTrack: t_ID3v1_GetTrack;
    ID3v1_SetTrack: t_ID3v1_SetTrack;
    ID3v1_GetGenre: t_ID3v1_GetGenre;
    ID3v1_SetGenre: t_ID3v1_SetGenre;
    ID3v1_GenreByte2Text: t_ID3v1_GenreByte2Text;
    ID3v1_Rev1: t_ID3v1_Rev1;
    ID3v1_GenreListToData: t_ID3v1_GenreListToData;
    ID3v1_GenreDataToList: t_ID3v1_GenreDataToList;

var
    ID3v2_Create: t_ID3v2_Create;
    ID3v2_Free: t_ID3v2_Free;
    ID3v2_Kill: t_ID3v2_Kill;
    ID3v2_Load: t_ID3v2_Load;
    ID3v2_LoadFromStream: t_ID3v2_LoadFromStream;
    ID3v2_Loaded: t_ID3v2_Loaded;
    ID3v2_Save: t_ID3v2_Save;
    ID3v2_SaveToStream: t_ID3v2_SaveToStream;
    ID3v2_GetAsciiText: t_ID3v2_GetAsciiText;
    ID3v2_SetAsciiText: t_ID3v2_SetAsciiText;
    ID3v2_FrameCount: t_ID3v2_FrameCount;
    ID3v2_GetFrameData: t_ID3v2_GetFrameData;
    ID3v2_SetFrameData:t_ID3v2_SetFrameData;
    ID3v2_CheckFrameExists: t_ID3v2_CheckFrameExists;
    ID3v2_GetFrameID: t_ID3v2_GetFrameID;
    ID3v2_DeleteFrame: t_ID3v2_DeleteFrame;
    ID3v2_CreateFrame: t_ID3v2_CreateFrame;
    ID3v2_CompressFrame: t_ID3v2_CompressFrame;
    ID3v2_DeCompressFrame: t_ID3v2_DeCompressFrame;
    ID3v2_AddAlbumPicture: t_ID3v2_AddAlbumPicture;
    ID3v2_GetAlbumPicture: t_ID3v2_GetAlbumPicture;
    ID3v2_GetAlbumPictureStream: t_ID3v2_GetAlbumPictureStream;
    ID3v2_SyncFrame: t_ID3v2_SyncFrame;
    ID3v2_UnSyncFrame: t_ID3v2_UnSyncFrame;
    ID3v2_SetCOMM: t_ID3v2_SetCOMM;
    ID3v2_GetCOMM: t_ID3v2_GetCOMM;
    ID3v2_SetWXXX: t_ID3v2_SetWXXX;
    ID3v2_GetWXXX: t_ID3v2_GetWXXX;
    ID3v2_GetFrameDataP: t_ID3v2_GetFrameDataP;
    ID3v2_SetFrameDataP: t_ID3v2_SetFrameDataP;
    ID3v2_GetFrameSize: t_ID3v2_GetFrameSize;
    ID3v2_FrameTypeCount: t_ID3v2_FrameTypeCount;
    ID3v2_TagSize: t_ID3v2_TagSize;
    ID3v2_TagMajorVer: t_ID3v2_TagMajorVer;
    ID3v2_TagMinorVer: t_ID3v2_TagMinorVer;
    ID3v2_FrameIsCompressed: t_ID3v2_FrameIsCompressed;
    ID3v2_GetGenre: t_ID3v2_GetGenre;

var
  ID3v2DLLHandle: THandle = 0;
  ID3v2DLLLoaded: Boolean = False;

implementation

//uses ;

function InitID3v2Library: Boolean;
begin
    //ID3v2DLLLoaded := False;
    ID3v2DLLHandle := SafeLoadLibrary(ID3v2DLL);
    Result := ID3v2DLLHandle <> 0;
    if Result then begin
        ID3v1_Create            := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_Create));
        ID3v1_Free              := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_Free));
        ID3v1_Load              := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_Load));
        ID3v1_Save              := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_Save));
        ID3v1_Kill              := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_Kill));
        ID3v1_GetTitle          := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_GetTitle));
        ID3v1_SetTitle          := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_SetTitle));
        ID3v1_GetArtist         := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_GetArtist));
        ID3v1_SetArtist         := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_SetArtist));
        ID3v1_GetAlbum          := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_GetAlbum));
        ID3v1_SetAlbum          := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_SetAlbum));
        ID3v1_GetYear           := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_GetYear));
        ID3v1_SetYear           := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_SetYear));
        ID3v1_GetComment        := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_GetComment));
        ID3v1_SetComment        := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_SetComment));
        ID3v1_GetTrack          := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_GetTrack));
        ID3v1_SetTrack          := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_SetTrack));
        ID3v1_GetGenre          := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_GetGenre));
        ID3v1_SetGenre          := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_SetGenre));
        ID3v1_GenreByte2Text    := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_GenreByte2Text));
        ID3v1_Rev1              := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_Rev1));
        ID3v1_GenreListToData   := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_GenreListToData));
        ID3v1_GenreDataToList   := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v1_GenreDataToList));

        ID3v2_Create            := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_Create));
        ID3v2_Free              := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_Free));
        ID3v2_Kill              := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_Kill));
        ID3v2_Load              := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_Load));
        ID3v2_LoadFromStream    := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_LoadFromStream));
        ID3v2_Loaded            := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_Loaded));
        ID3v2_Save              := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_Save));
        ID3v2_SaveToStream      := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_SaveToStream));
        ID3v2_GetAsciiText      := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_GetAsciiText));
        ID3v2_SetAsciiText      := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_SetAsciiText));
        ID3v2_FrameCount        := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_FrameCount));
        ID3v2_GetFrameData      := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_GetFrameData));
        ID3v2_SetFrameData      := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_SetFrameData));
        ID3v2_CheckFrameExists  := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_CheckFrameExists));
        ID3v2_GetFrameID        := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_GetFrameID));
        ID3v2_DeleteFrame       := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_DeleteFrame));
        ID3v2_CreateFrame       := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_CreateFrame));
        ID3v2_CompressFrame     := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_CompressFrame));
        ID3v2_DeCompressFrame   := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_DeCompressFrame));
        ID3v2_AddAlbumPicture   := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_AddAlbumPicture));
        ID3v2_GetAlbumPicture   := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_GetAlbumPicture));
        ID3v2_GetAlbumPictureStream := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_GetAlbumPictureStream));
        ID3v2_SyncFrame         := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_SyncFrame));
        ID3v2_UnSyncFrame       := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_UnSyncFrame));
        ID3v2_SetCOMM           := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_SetCOMM));
        ID3v2_GetCOMM           := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_GetCOMM));
        ID3v2_SetWXXX           := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_SetWXXX));
        ID3v2_GetWXXX           := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_GetWXXX));
        ID3v2_GetFrameDataP     := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_GetFrameDataP));
        ID3v2_SetFrameDataP     := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_SetFrameDataP));
        ID3v2_GetFrameSize      := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_GetFrameSize));
        ID3v2_FrameTypeCount    := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_FrameTypeCount));
        ID3v2_TagSize           := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_TagSize));
        ID3v2_TagMajorVer       := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_TagMajorVer));
        ID3v2_TagMinorVer       := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_TagMinorVer));
        ID3v2_FrameIsCompressed := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_FrameIsCompressed));
        ID3v2_GetGenre          := GetProcAddress(ID3v2DLLHandle, PChar(NAME_ID3v2_GetGenre));
    end;
    if (@ID3v1_Create = nil)
    OR (@ID3v1_Free = nil)
    OR (@ID3v1_Load = nil)
    OR (@ID3v1_Save = nil)
    OR (@ID3v1_Kill = nil)
    OR (@ID3v1_GetTitle = nil)
    OR (@ID3v1_SetTitle = nil)
    OR (@ID3v1_GetArtist = nil)
    OR (@ID3v1_SetArtist = nil)
    OR (@ID3v1_GetAlbum = nil)
    OR (@ID3v1_SetAlbum = nil)
    OR (@ID3v1_GetYear = nil)
    OR (@ID3v1_SetYear = nil)
    OR (@ID3v1_GetComment = nil)
    OR (@ID3v1_SetComment = nil)
    OR (@ID3v1_GetTrack = nil)
    OR (@ID3v1_SetTrack = nil)
    OR (@ID3v1_GenreByte2Text = nil)
    OR (@ID3v1_Rev1 = nil)
    OR (@ID3v1_GenreListToData = nil)
    OR (@ID3v1_GenreDataToList = nil)

    OR (@ID3v2_Create = nil)
    OR (@ID3v2_Free = nil)
    OR (@ID3v2_Kill = nil)
    OR (@ID3v2_Load = nil)
    OR (@ID3v2_LoadFromStream = nil)
    OR (@ID3v2_Loaded = nil)
    OR (@ID3v2_Save = nil)
    OR (@ID3v2_SaveToStream = nil)
    OR (@ID3v2_GetAsciiText = nil)
    OR (@ID3v2_SetAsciiText = nil)
    OR (@ID3v2_FrameCount = nil)
    OR (@ID3v2_GetFrameData = nil)
    OR (@ID3v2_SetFrameData = nil)
    OR (@ID3v2_CheckFrameExists = nil)
    OR (@ID3v2_GetFrameID = nil)
    OR (@ID3v2_DeleteFrame = nil)
    OR (@ID3v2_CreateFrame = nil)
    OR (@ID3v2_CompressFrame = nil)
    OR (@ID3v2_DeCompressFrame = nil)
    OR (@ID3v2_AddAlbumPicture = nil)
    OR (@ID3v2_GetAlbumPicture = nil)
    OR (@ID3v2_GetAlbumPictureStream = nil)
    OR (@ID3v2_SyncFrame = nil)
    OR (@ID3v2_UnSyncFrame = nil)
    OR (@ID3v2_SetCOMM = nil)
    OR (@ID3v2_GetCOMM = nil)
    OR (@ID3v2_SetWXXX = nil)
    OR (@ID3v2_GetWXXX = nil)
    OR (@ID3v2_GetFrameDataP = nil)
    OR (@ID3v2_SetFrameDataP = nil)
    OR (@ID3v2_GetFrameSize = nil)
    OR (@ID3v2_FrameTypeCount = nil)
    OR (@ID3v2_TagSize = nil)
    OR (@ID3v2_TagMajorVer = nil)
    OR (@ID3v2_TagMinorVer = nil)
    OR (@ID3v2_FrameIsCompressed = nil)
    OR (@ID3v2_GetGenre = nil)
        then Result := False;

    if Result
        then ID3v2DLLLoaded := True;
end;

function FreeID3v2Library: Boolean;
begin
    Result := False;
    if ID3v2DLLHandle <> 0 then begin
        Result := FreeLibrary(ID3v2DLLHandle);
        if Result
            then ID3v2DLLLoaded := False;
    end;
end;

Initialization
    InitID3v2Library;
Finalization
    FreeID3v2Library;

end.
