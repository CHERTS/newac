(*
  This file is a part of New Audio Components package 1.8
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit NewAC_AVI;

interface

uses
  Windows, Classes, SysUtils, ACS_Classes, ACS_Procs, ActiveX, MMSystem, _DXTypes;

type
  TStreamType = packed array[0..3] of Char;

const

  streamtypeAUDIO : TStreamType = ('a', 'u', 'd', 's');

  AVIERR_OK = $00000000;
  AVIERR_UNSUPPORTED = $80044065;
  AVIERR_BADFORMAT = $80044066;
  AVIERR_MEMORY = $80044067;
  AVIERR_INTERNAL = $80044068;
  AVIERR_BADFLAGS = $80044069;
  AVIERR_BADPARAM = $8004406A;
  AVIERR_BADSIZE = $8004406B;
  AVIERR_BADHANDLE = $8004406C;
  AVIERR_FILEREAD = $8004406D;
  AVIERR_FILEWRITE = $8004406E;
  AVIERR_FILEOPEN = $8004406F;
  AVIERR_COMPRESSOR = $80044070;
  AVIERR_NOCOMPRESSOR = $80044071;
  AVIERR_READONLY = $80044072;
  AVIERR_NODATA = $80044073;

type

  LONG = Longint;
  PVOID = Pointer;

  TAVIFileInfoW = record
    dwMaxBytesPerSec,           // max. transfer rate
    dwFlags,                     // the ever-present flags
    dwCaps,
    dwStreams,
    dwSuggestedBufferSize,

    dwWidth,
    dwHeight,

    dwScale,
    dwRate,           // dwRate / dwScale == samples/second
    dwLength,

    dwEditCount: DWORD;

    szFileType: array[0..63] of WideChar;
  end;
  PAVIFileInfoW = ^TAVIFileInfoW;

  TAVIStreamInfoW = record
    fccType,
    fccHandler,
    dwFlags,        // Contains AVITF_* flags
    dwCaps: DWORD;
    wPriority,
    wLanguage: WORD;
    dwScale,
    dwRate, // dwRate / dwScale == samples/second
    dwStart,
    dwLength, // In units above...
    dwInitialFrames,
    dwSuggestedBufferSize,
    dwQuality,
    dwSampleSize: DWORD;
    rcFrame: TRect;
    dwEditCount : DWORD;
    dwFormatChangeCount : DWORD;
    szName:  array[0..63] of WideChar;
  end;

  IAVIStream = interface
  ['{00020021-0000-0000-C000-000000000046}']
    function Create(lParam1, lParam2: LPARAM): HResult; stdcall;
    function Info(var psi: TAVIStreamInfoW; lSize: LONG): HResult; stdcall;
    function FindSample(lPos, lFlags: LongWord): LongWord; stdcall;
    function ReadFormat(lPos: LONG; lpFormat: PVOID; var lpcbFormat: LONG): HResult; stdcall;
    function SetFormat(lPos: LONG; lpFormat: PVOID; lpcbFormat: LONG): HResult; stdcall;
    function Read(lStart, lSamples: LONG; lpBuffer: PVOID; cbBuffer: LONG; var plBytes: LONG; var plSamples: LONG): HResult; stdcall;
    function Write(lStart, lSamples: LONG; lpBuffer: PVOID; cbBuffer: LONG; dwFlags: DWORD; var plSampWritten: LONG; var plBytesWritten: LONG): HResult; stdcall;
    function Delete(lStart, lSamples: LONG): HResult; stdcall;
    function ReadData(fcc: DWORD; lp: PVOID; var lpcb: LONG): HResult; stdcall;
    function WriteData(fcc: DWORD; lp: PVOID; cb:  LONG): HResult; stdcall;
    function SetInfo(var lpInfo: TAVIStreamInfoW; cbInfo: LONG): HResult; stdcall;
  end;
  PAVIStream = ^IAVIStream;

  IAVIFile = interface(IUnknown)
  ['{00020020-0000-0000-C000-000000000046}']
    function Info(var pfi: TAVIFileInfoW; lSize: LONG): HResult; stdcall;
    function GetStream(var ppStream: IAVIStream; const fccType: TStreamType; lParam: LONG): HResult; stdcall;
    function CreateStream(var ppStream: PAVIStream; var pfi: TAVIFileInfoW): HResult; stdcall;
    function WriteData(ckid: DWORD; lpData: PVOID; cbData: LONG): HResult; stdcall;
    function ReadData(ckid: DWORD; lpData: PVOID; var lpcbData: LONG): HResult; stdcall;
    function EndRecord: HResult; stdcall;
    function DeleteStream(fccType: DWORD; lParam: LONG): HResult; stdcall;
  end;
  PAVIFile = ^IAVIFile;

  TAVIIn = class(TAuFileIn)
  private
    AVIFile : IAVIFile;
    AVIStream : IAVIStream;
    _Buf : Pointer;
    _BufSize : Integer;
    _SampleSize : Word;
    _StartSample, _StartFrom : LongWord;
    FHasAudio : Boolean;
    function GetHasAudio : Boolean;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    function SeekInternal(var SampleNum : Int64) : Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* Property: HasAudio
       Read this property to determine if the input file has an audio stream.
       The False value indicates that either an audio stream is missing (in
       WMV file) or the input file is corrupt.

       Note:
       Windows Media files may contain several audio streams. In the current
       version TWMIn reads data only from the first audio stream it finds. *)
    property HasAudio : Boolean read GetHasAudio;
  published
    property EndSample;
    property StartSample;
  end;


implementation

  procedure AVIFileInit; stdcall; external 'avifil32.dll' name 'AVIFileInit';
  procedure AVIFileExit; stdcall; external 'avifil32.dll' name 'AVIFileExit';
  function AVIFileOpen(var ppfile: IAVIFILE; szFile: PWideChar; uMode: LongWord; lpHandler: PCLSID): HResult; stdcall; external 'avifil32.dll' name 'AVIFileOpenW';
  function AVIFileRelease(pfile: IAVIFile): LongWord; stdcall external 'avifil32.dll' name 'AVIFileRelease';

  constructor TAVIIn.Create;
  begin
    inherited Create(AOwner);
  end;

  destructor TAVIIn.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TAVIIn.OpenFile;
  var
    wf : TWaveFormatEx;
    wfl : Integer;
    StreamInfo : TAVIStreamInfoW;
  begin
    OpenCS.Enter;
    try
    if FOpened = 0 then
    begin
      if FWideFileName = '' then
        raise EAuException.Create('File name is not assigned');
      AVIFileInit;
      if AVIFileOpen(AVIFile, PWideChar(FWideFileName), OF_READ or OF_SHARE_DENY_WRITE, nil) <> AVIERR_OK then
        raise EAuException.Create('Failed to open input file');
      FValid := True;
      if AVIFile.GetStream(AVIStream, streamtypeAUDIO, 0) = AVIERR_OK then
        FHasAudio := True
      else
      begin
        FHasAudio := False;
        Exit;
      end;
      AVIStream.Info(StreamInfo, SizeOf(StreamInfo));
      FTotalSamples := StreamInfo.dwLength;
      _StartSample := StreamInfo.dwStart;
      _StartFrom := _StartSample;
      wfl := SizeOf(wf);
      AVIStream.ReadFormat(0, @wf, wfl);
      _SampleSize := (wf.wBitsPerSample div 8) * wf.nChannels;
      FSize := _SampleSize * FTotalSamples;
      FChan := wf.nChannels;
      FBPS := wf.wBitsPerSample;
      FSR := wf.nSamplesPerSec;
      FSeekable := True;
      Inc(FOpened);
      _Buf := nil;
      _BufSize := 0;
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TAVIIn.CloseFile;
  begin
    OpenCS.Enter;
    try
    if FOpened > 0 then
    begin
      AVIStream := nil;
      AVIFileRelease(AVIFile);
      AVIFileExit;
      if _Buf <> nil then FreeMem(_Buf);
      FOpened := 0;
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TAVIIn.GetDataInternal(var Buffer : Pointer; var Bytes : LongWord);
  var
    br, sr : Integer;
  begin
    if (FSize > 0) and (FSize - FPosition < Bytes) then
      Bytes := FSize - FPosition;
    Bytes := Bytes - (Bytes mod _SampleSize);
    if Bytes > _BufSize then
    begin
      if _Buf <> nil then FreeMem(_Buf);
      GetMem(_Buf, Bytes);
      _BufSize := Bytes;
    end;
    AVIStream.Read(_StartSample, Bytes div _SampleSize, _Buf, Integer(Bytes), br, sr);
    Inc(_StartSample, Bytes div _SampleSize);
    Buffer := _Buf;
  end;

  function TAVIIn.SeekInternal(var SampleNum : Int64) : Boolean;
  var
    Offset : LongWord;
  begin
    Result := False;
    if Busy then
    begin
      _StartSample := SampleNum + _StartFrom;
      Result := True;
    end;
  end;

  function TAVIIn.GetHasAudio;
  begin
    OpenFile;
    Result := Self.FHasAudio;
  end;

end.
