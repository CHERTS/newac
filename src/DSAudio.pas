(*
  This file is a part of New Audio Components package v. 2.6
  Copyright (c) 2002-2010, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit DSAudio;

interface

uses
  SysUtils, FastMove, Windows, MMSystem, _DirectSound, ACS_Types;

type

   DSOut = record
    DirectSound : IDirectSound8;
    DirectSoundBuffer : IDirectSoundBuffer8;
    PrimaryBuffer : IDirectSoundBuffer;
    events : array [0..1] of THandle;
    IsPlaying : Boolean;
    BufferSize : LongWord;
    Underflows : LongWord;
    Offset : LongWord;
  end;

  DSW_DeviceInfo = record
    guid : TGUID;
    {$IFDEF UNICODE}
    name : array [0..127] of WideChar;
    {$ENDIF}
    {$IFNDEF UNICODE}
    name : array [0..127] of Char;
    {$ENDIF}
  end;

  PDSW_DeviceInfo = ^DSW_DeviceInfo;

  DSW_Devices = record
    devcount : Integer;
    dinfo : array [0..15] of DSW_DeviceInfo;
  end;

  PDSW_Devices = ^DSW_Devices;

  function DSEnumerateOutputDevices(devices : PDSW_Devices) : HRESULT;
  function DSInitOutputDevice(var ds : DSOut; lpGUID : PGUID) : HRESULT;
  function DSInitOutputBuffer(var ds  : DSOut; Wnd : HWND; bps : LongWord;
        nFrameRate : LongWord; nChannels, bytesPerBuffer : LongWord) : HRESULT;
  function DSInitOutputBufferEx(var ds : DSOut; Wnd : HWND; var WaveFormat : TWaveFormatExtensible; bytesPerBuffer : LongWord) : HRESULT;
  function DSStartOutput(var ds : DSOut) : HRESULT;
  function DSStopOutput(var ds : DSOut): HRESULT;
  function DSRestartOutput(var ds : DSOut) : HRESULT;
  function DSQueryOutputSpace(var ds : DSOut; var bytesEmpty : Integer) : HRESULT;
  function DSFillEmptySpace(var ds : DSOut; Fill : Byte) : HRESULT;
  function DSWriteBlock(var ds : DSOut; buf : PByte; numBytes : LongWord) : HRESULT;
  procedure DSTerminateOutput(var ds : DSOut);
  function WaitForCursor(var ds: DSOut; evnt : LongWord) : Boolean;
  procedure UnlockEvents(var ds: DSOut);
  function DSGetVolume(var ds : DSOut; out Volume: Longint) : HRESULT;
  function DSSetVolume(var ds : DSOut; Volume: Longint) : HRESULT;

implementation

function DSEnumOutputCallback(lpGuid : PGUID;
                              {$IFDEF UNICODE}
                              lpcstrDescription : PWideChar;
                              {$ENDIF}
                              {$IFNDEF UNICODE}
                              lpcstrDescription : PChar;
                              {$ENDIF}
                              lpcstrModule : PChar; lpContext : Pointer) : BOOL; stdcall;
var
  l : Integer;
  devices : PDSW_Devices; // = (DSW_Devices *) lpContext;
begin
  devices := lpContext;
  if devices.devcount > 15 then
  begin
    Result := FALSE;
    Exit;
  end;
  if lpGuid <> nil then
    Move(lpGuid^, devices.dinfo[devices.devcount].guid, sizeof(TGUID))
  else
    FillChar(devices.dinfo[devices.devcount].guid, sizeof(TGUID), 0);
  {$IFDEF UNICODE}
  l := strlen(lpcstrDescription)*2;
  if l > 255 then l := 255;
  {$ENDIF}
  {$IFNDEF UNICODE}
  l := strlen(lpcstrDescription);
  if l > 127 then l := 127;
  {$ENDIF}
  Move(lpcstrDescription[0], devices.dinfo[devices.devcount].name, l);
  devices.dinfo[devices.devcount].name[l] := Char(0);
  Inc(devices.devcount);
  Result := TRUE;
end;

function DSEnumerateOutputDevices(devices : PDSW_Devices) : HRESULT;
begin
  devices.devcount := 0;
  Result := DirectSoundEnumerate(DSEnumOutputCallback, devices);
end;

function DSInitOutputDevice(var ds : DSOut; lpGUID : PGUID) : HRESULT;
begin
  Result := DirectSoundCreate8(lpGUID, ds.DirectSound, nil);
end;

function DSInitOutputBuffer(var ds  : DSOut; Wnd : HWND; bps : LongWord;
        nFrameRate : LongWord; nChannels, bytesPerBuffer : LongWord) : HRESULT;
var
  dwDataLen : DWORD;
  _hWnd : HWND;
  hr : HRESULT;
  wfFormat : TWaveFormatEx;
  primaryDesc : TDSBufferDesc;
  secondaryDesc : TDSBufferDesc;
  pDSBuffData : PByte;
  DirectSoundBuffer : IDirectSoundBuffer;
  DSN : IDirectSoundNotify8;
  PN : array[0..1] of TDSBPositionNotify;
begin
  ds.BufferSize := bytesPerBuffer;
  ds.IsPlaying := FALSE;
  ds.Underflows := 0;
  _hWnd := Wnd;
  if _hWnd = 0 then
    _hWnd := GetDesktopWindow();
  hr := ds.DirectSound.SetCooperativeLevel(_hWnd, DSSCL_EXCLUSIVE);
  if hr <> DS_OK then
  begin
    Result := hr;
    Exit;
  end;
//
  // -----------------------------------------------------------------------
  // Create primary buffer and set format just so we can specify our custom format.
  // Otherwise we would be stuck with the default which might be 8 bit or 22050 Hz.
  // Setup the primary buffer description
  FillChar(primaryDesc, sizeof(TDSBUFFERDESC), 0);
  primaryDesc.dwSize := sizeof(DSBUFFERDESC);
  primaryDesc.dwFlags := DSBCAPS_PRIMARYBUFFER; // all panning, mixing, etc done by synth
  primaryDesc.dwBufferBytes := 0;
  primaryDesc.lpwfxFormat := nil;
  // Create the buffer
  Result := ds.DirectSound.CreateSoundBuffer(
        primaryDesc, ds.PrimaryBuffer, nil);  // ATTENTION
  if (Result <> DS_OK) then Exit;
//3
    // Define the buffer format
  wfFormat.wFormatTag := WAVE_FORMAT_PCM;
  wfFormat.nChannels := nChannels;
  wfFormat.nSamplesPerSec := nFrameRate;
  wfFormat.wBitsPerSample := bps;
  wfFormat.nBlockAlign := wfFormat.nChannels * wfFormat.wBitsPerSample div 8;
  wfFormat.nAvgBytesPerSec := wfFormat.nSamplesPerSec * wfFormat.nBlockAlign;
  wfFormat.cbSize := 0;  // No extended format info.
  // Set the primary buffer's format
  Result := ds.PrimaryBuffer.SetFormat(@wfFormat);
//2
  if Result <> DS_OK then Exit;
    // ----------------------------------------------------------------------
    // Setup the secondary buffer description
  ZeroMemory(@secondaryDesc, sizeof(TDSBUFFERDESC));
  secondaryDesc.dwSize := sizeof(TDSBUFFERDESC);
  secondaryDesc.dwFlags :=  DSBCAPS_GLOBALFOCUS or DSBCAPS_GETCURRENTPOSITION2 or DSBCAPS_CTRLVOLUME or DSBCAPS_CTRLPOSITIONNOTIFY;
  secondaryDesc.dwBufferBytes := bytesPerBuffer;
  secondaryDesc.lpwfxFormat := @wfFormat;
    // Create the secondary buffer
  Result := ds.DirectSound.CreateSoundBuffer(secondaryDesc, DirectSoundBuffer, nil);
  ds.DirectSoundBuffer := DirectSoundBuffer as IDirectSoundBuffer8;
  if Result <> DS_OK then Exit;
  DirectSoundBuffer := nil;
//1
  ds.events[0] := CreateEvent(nil, True, False, nil);
  ds.events[1] := CreateEvent(nil, True, False, nil);
  DSN := ds.DirectSoundBuffer as IDirectSoundNotify8;
  PN[0].dwOffset := 0;
  PN[0].hEventNotify := ds.events[0];
  PN[1].dwOffset := ds.BufferSize div 2;
  PN[1].hEventNotify := ds.events[1];
  Result := DSN.SetNotificationPositions(2, @PN);
  if Result = DSERR_INVALIDPARAM then
     raise Exception.Create(IntToHex(Result, 8));
  DSN := nil;
    // Lock the DS buffer
   Result := ds.DirectSoundBuffer.Lock(0, ds.BufferSize, @pDSBuffData,
            @dwDataLen, nil, nil, 0);
  if Result <> DS_OK then Exit;
    // Zero the DS buffer
  ZeroMemory(pDSBuffData, dwDataLen);
    // Unlock the DS buffer
  Result := ds.DirectSoundBuffer.Unlock(pDSBuffData, dwDataLen, nil, 0);
  if Result <> DS_OK then Exit;
  Result := DS_OK;
end;

function DSStartOutput(var ds : DSOut) : HRESULT;
begin
  Result := ds.DirectSoundBuffer.SetCurrentPosition(0);
  if Result <> DS_OK then Exit;
    // Start the buffer playback in a loop.
  if Assigned(ds.DirectSoundBuffer) then
  begin
    Result := ds.DirectSoundBuffer.Play(0, 0, DSBPLAY_LOOPING );
    if Result <> DS_OK then Exit;
  end;
  Result := 0;
end;

function DSStopOutput(var ds : DSOut): HRESULT;
begin
    // Stop the buffer playback
  if ds.DirectSoundBuffer <> nil then
  begin
    Result := ds.DirectSoundBuffer.Stop;
  end
  else Result := 0;
end;

function DSRestartOutput(var ds : DSOut) : HRESULT;
begin
  if ds.DirectSoundBuffer <> nil then
  begin
    Result := ds.DirectSoundBuffer.Play(0, 0, DSBPLAY_LOOPING);
    if Result = DS_OK then
    begin
      Result := 0;
      Exit;
    end;
    Exit;
  end;
  Result := -1;
end;

function DSQueryOutputSpace(var ds : DSOut; var bytesEmpty : Integer) : HRESULT;
var
  //hr : HRESULT;
  playCursor :  DWORD;
  writeCursor : DWORD;
  numBytesEmpty : Integer;
//  bytesExpected : long;
//  buffersWrapped : long;

begin
  DS.Underflows := 0;
  Result := ds.DirectSoundBuffer.GetCurrentPosition(@playCursor, @writeCursor);
  if Result <> DS_OK then Exit;
  if writeCursor > ds.Offset then
  begin
   ds.Offset := writeCursor;
   DS.Underflows := 1;
  end;
  numBytesEmpty := playCursor - ds.Offset;
  if numBytesEmpty < 0 then
    numBytesEmpty := numBytesEmpty + Integer(ds.BufferSize); // unwrap offset
(*  if numBytesEmpty > (dsw.dsw_OutputSize - playWriteGap) then
  begin
    if dsw.dsw_OutputRunning then
    begin
      Inc(dsw.dsw_OutputUnderflows);
                //            AddTraceMessage("underflow detected! numBytesEmpty", numBytesEmpty );
    end;
    dsw.dsw_WriteOffset := writeCursor;
    numBytesEmpty := dsw.dsw_OutputSize - playWriteGap;
  end;                        *)
  bytesEmpty := numBytesEmpty;
end;


function DSFillEmptySpace(var ds : DSOut; Fill : Byte) : HRESULT;
var
//   hr : HRESULT;
  lpbuf1 : PBYTE;
  lpbuf2 : PBYTE;
  dwsize1 : DWORD;
  dwsize2 : DWORD;
  bytesEmpty : Integer;
begin
  Result := DSQueryOutputSpace(ds, bytesEmpty); // updates dsw_FramesPlayed
  if Result <> DS_OK then Exit;
  if bytesEmpty = 0 then
  begin
    Result := DS_OK;
    Exit;
  end;
    // Lock free space in the DS
  Result := ds.DirectSoundBuffer.Lock(ds.Offset, bytesEmpty, @lpbuf1,
            @dwsize1, @lpbuf2, @dwsize2, 0);
  if Result = DS_OK then
  begin
     // Copy the buffer into the DS
    FillMemory(lpbuf1, dwsize1, Fill);
    if lpbuf2 <> nil then
      FillMemory(lpbuf2, dwsize2, Fill);
      // Update our buffer offset and unlock sound buffer
    ds.Offset := (ds.Offset + dwsize1 + dwsize2) mod ds.BufferSize;
    ds.DirectSoundBuffer.Unlock(lpbuf1, dwsize1, lpbuf2, dwsize2);
  end;
end;

function DSWriteBlock(var ds : DSOut; buf : PByte; numBytes : LongWord) : HRESULT;
var
  lpbuf1 : PBYTE;
  lpbuf2 : PBYTE;
  dwsize1 : DWORD;
  dwsize2 : DWORD;
  tmpbuf : PByte;
begin
  // Lock free space in the DS
  Result := ds.DirectSoundBuffer.Lock(ds.Offset, numBytes, @lpbuf1,
                @dwsize1, @lpbuf2, @dwsize2, 0);
  if Result = DS_OK then
  begin
    // Copy the buffer into the DS
    Move(buf^, lpbuf1^, dwsize1);
    if lpbuf2 <> nil then
    begin
      tmpbuf := buf;
      Inc(tmpbuf, dwsize1);
      Move(tmpbuf^, lpbuf2^, dwsize2);
    end;
        // Update our buffer offset and unlock sound buffer
    ds.Offset := (ds.Offset + dwsize1 + dwsize2) mod ds.BufferSize;
    ds.DirectSoundBuffer.Unlock(lpbuf1, dwsize1, lpbuf2, dwsize2);
  end;
end;

procedure DSTerminateOutput(var ds : DSOut);
begin
  if Assigned(ds.DirectSoundBuffer) then
  begin
    ds.DirectSoundBuffer.Stop;
    ds.DirectSoundBuffer := nil;
  end;
  if Assigned(ds.PrimaryBuffer) then
         ds.PrimaryBuffer := nil;
  if Assigned(ds.DirectSound) then
  begin
    ds.DirectSound := nil;
  end;
  CloseHandle(ds.events[0]);
  CloseHandle(ds.events[1]);
end;

function WaitForCursor(var ds: DSOut; evnt : LongWord) : Boolean;
begin
  WaitForSingleObject(ds.events[evnt], INFINITE);
  ResetEvent(ds.events[evnt]);
  Result := WaitForSingleObject(ds.events[1-evnt], 0) = WAIT_OBJECT_0;
end;

procedure UnlockEvents(var ds: DSOut);
begin
  SetEvent(ds.events[0]);
  SetEvent(ds.events[1]);
end;

function DSInitOutputBufferEx(var ds : DSOut; Wnd : HWND; var WaveFormat : TWaveFormatExtensible; bytesPerBuffer : LongWord) : HRESULT;
var
  dwDataLen : DWORD;
  playCursor : DWORD;
  _hWnd : HWND;
  hr : HRESULT;
  primaryDesc : TDSBufferDesc;
  secondaryDesc : TDSBufferDesc;
  pDSBuffData : PByte;
  DirectSoundBuffer : IDirectSoundBuffer;
  DSN : IDirectSoundNotify8;
  PN : array[0..1] of TDSBPositionNotify;
begin
  ds.BufferSize := bytesPerBuffer;
  ds.Underflows := 0;
  // We were using getForegroundWindow() but sometimes the ForegroundWindow may not be the
  // applications's window. Also if that window is closed before the Buffer is closed
  // then DirectSound can crash. (Thanks for Scott Patterson for reporting this.)
  // So we will use GetDesktopWindow() which was suggested by Miller Puckette.
  // hWnd = GetForegroundWindow();
  _hWnd := Wnd;
  if _hWnd = 0 then
    _hWnd := GetDesktopWindow();
  // Set cooperative level to DSSCL_EXCLUSIVE so that we can get 16 bit output, 44.1 KHz.
  // Exclusize also prevents unexpected sounds from other apps during a performance.
  hr := ds.DirectSound.SetCooperativeLevel(_hWnd, DSSCL_EXCLUSIVE);
  if hr <> DS_OK then
  begin
    Result := hr;
    Exit;
  end;
  // -----------------------------------------------------------------------
  // Create primary buffer and set format just so we can specify our custom format.
  // Otherwise we would be stuck with the default which might be 8 bit or 22050 Hz.
  // Setup the primary buffer description
  FillChar(primaryDesc, sizeof(TDSBUFFERDESC), 0);
  primaryDesc.dwSize := sizeof(DSBUFFERDESC);
  primaryDesc.dwFlags := DSBCAPS_PRIMARYBUFFER; // all panning, mixing, etc done by synth
  primaryDesc.dwBufferBytes := 0;
  primaryDesc.lpwfxFormat := nil; //PWaveFormatEx(@WaveFormat);
  // Create the buffer
  Result := ds.DirectSound.CreateSoundBuffer(
        primaryDesc, ds.PrimaryBuffer, nil);  // ATTENTION
  if (Result <> DS_OK) then Exit;
//  Set the primary buffer's format
Result := ds.PrimaryBuffer.SetFormat(PWaveFormatEx(@WaveFormat));
  if (Result <> DS_OK) then Exit;
    // ----------------------------------------------------------------------
    // Setup the secondary buffer description
  ZeroMemory(@secondaryDesc, sizeof(TDSBUFFERDESC));
  secondaryDesc.dwSize := sizeof(TDSBUFFERDESC);
  secondaryDesc.dwFlags :=  DSBCAPS_GLOBALFOCUS or DSBCAPS_GETCURRENTPOSITION2 or DSBCAPS_CTRLVOLUME or DSBCAPS_CTRLPOSITIONNOTIFY;
  secondaryDesc.dwBufferBytes := bytesPerBuffer;
  secondaryDesc.lpwfxFormat := PWaveFormatEx(@WaveFormat);
    // Create the secondary buffer
  Result := ds.DirectSound.CreateSoundBuffer(secondaryDesc, DirectSoundBuffer, nil);
  ds.DirectSoundBuffer := DirectSoundBuffer as IDirectSoundBuffer8;
  if Result <> DS_OK then Exit;
  DirectSoundBuffer := nil;
//1
  ds.events[0] := CreateEvent(nil, True, False, nil);
  ds.events[1] := CreateEvent(nil, True, False, nil);
  DSN := ds.DirectSoundBuffer as IDirectSoundNotify8;
  PN[0].dwOffset := 0;
  PN[0].hEventNotify := ds.events[0];
  PN[1].dwOffset := ds.BufferSize div 2;
  PN[1].hEventNotify := ds.events[1];
  Result := DSN.SetNotificationPositions(2, @PN);
  if Result = DSERR_INVALIDPARAM then
     raise Exception.Create(IntToHex(Result, 8));
  DSN := nil;
    // Lock the DS buffer

  Result := ds.DirectSoundBuffer.Lock(0, ds.BufferSize, @pDSBuffData,
            @dwDataLen, nil, nil, 0);
  if Result <> DS_OK then Exit;
    // Zero the DS buffer
  ZeroMemory(pDSBuffData, dwDataLen);
    // Unlock the DS buffer
  Result := ds.DirectSoundBuffer.Unlock(pDSBuffData, dwDataLen, nil, 0);
  if Result <> DS_OK then Exit;
  hr := ds.DirectSoundBuffer.GetCurrentPosition(@playCursor, @(ds.Offset));
  if( hr <> DS_OK ) then
  begin
    Result := hr;
    Exit;
  end;
    ///* printf("DSW_InitOutputBuffer: playCursor = %d, writeCursor = %d\n", playCursor, dsw->dsw_WriteOffset ); */
  Result := DS_OK;
end;

function DSGetVolume(var ds : DSOut; out Volume: Longint) : HRESULT;
begin
  if Assigned(ds.DirectSoundBuffer) then
    Result := ds.DirectSoundBuffer.GetVolume(Volume)
  else
  begin
    Volume := 0;
    Result := S_OK;
  end;
end;

function DSSetVolume(var ds : DSOut; Volume: Longint) : HRESULT;
begin
  if Assigned(ds.DirectSoundBuffer) then
    Result := ds.DirectSoundBuffer.SetVolume(Volume)
  else
    Result := S_OK;
  if Result = DSERR_CONTROLUNAVAIL then
    raise Exception.Create('Control not available');
end;


end.
