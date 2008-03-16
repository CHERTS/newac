(*
  This file is a part of New Audio Components package 1.7
  Copyright (c) 2002-2008 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit AudioDMO;

interface

uses
  SysUtils, Classes, Windows, ACS_Classes, ACS_Types, msdmo;

const
  RESAMPLER_INPUT_BUF_SIZE = $300000;

type

  TMSResampler = class(TAuConverter)
  private
    FBuffer : PBuffer8;
    Offset : LongWord;
    FBufSize : LongWord;
    FEndOfInput : Boolean;
    HasMoreOutput : Boolean;
    FOutSampleRate : LongWord;
    resampler : dmo_resampler;
  protected
    function GetSR : LongWord; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OutSampleRate : LongWord read FOutSampleRate write FOutSampleRate;
  end;

implementation

  constructor TMSResampler.Create(AOwner: TComponent);
  var
    VersionInfo : TOSVersionInfo;
  begin
    inherited Create(AOwner);
    if not (csDesigning in ComponentState) then
    begin
      VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
      GetVersionEx(VersionInfo);
      if VersionInfo.dwMajorVersion < 6 then
        raise EAuException.Create('TMSResampler component requires Windows Vista or later version');
    end;
  end;

  destructor TMSResampler.Destroy;
  begin
    inherited Destroy;
  end;

  function TMSResampler.GetSR;
  begin
    Result := 0;
    if not Assigned(FInput) then Exit;
    if FOutSampleRate = 0 then Result := FInput.SampleRate
    else Result := FOutSampleRate;
  end;

  procedure TMSResampler.InitInternal;
  begin
    if not Assigned(FInput) then
      raise EAuException.Create('Input not assigned');
    FInput.Init;
    Busy := True;
    FSize := Round(FInput.Size*GetSR/FInput.SampleRate);
    FPosition := 0;
    if GetSR <> FInput.SampleRate then
    begin
      dmo_resampler_init(resampler);
      resampler.input_spec.sample_rate := FInput.SampleRate;
      resampler.input_spec.channels := FInput.Channels;
      resampler.input_spec.bps := FInput.BitsPerSample;
      resampler.output_sr := GetSR;
      dmo_resampler_set_formats(resampler);
    end;
    FEndOfInput := False;
    Offset := 0;
    FBufSize := 0;
    HasMoreOutput := False;
  end;

  procedure TMSResampler.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
  var
    InBuf : Pointer;
    l : LongWord;
  begin
    if GetSR = FInput.SampleRate then
    begin
      FInput.GetData(Buffer, Bytes);
      Exit;
    end;
    if Offset = FBufSize then
    begin
      if not HasMoreOutput then
      begin
        if not FEndOfInput then
        begin
//          if dmo_resampler_accepts_data(resampler) then
//          begin
            l := RESAMPLER_INPUT_BUF_SIZE -
              (RESAMPLER_INPUT_BUF_SIZE mod (resampler.input_spec.channels*resampler.input_spec.bps div 8));
            dmo_resampler_prepare_input(resampler, InBuf, l);
            l := FInput.FillBuffer(InBuf, l, FEndOfInput);
            if FEndOfInput then
              dmo_resampler_reset_input(resampler, l);
            dmo_resampler_write_input(resampler);
//          end;
        end else //if not FEndOfInput then
        begin
          Buffer := nil;
          Bytes := 0;
          Exit;
        end; // if not FEndOfInput then... else
      end; // if not HasMoreOutput then
      dmo_resampler_free_output(resampler);
      HasMoreOutput := not dmo_resampler_get_output(resampler, Pointer(FBuffer), FBufSize);
      HasMoreOutput := HasMoreOutput or (not dmo_resampler_accepts_data(resampler));
      Offset := 0;
    end; // if Offset = FBufSize
    if Bytes > FBufSize - Offset then
      Bytes := FBufSize - Offset;
    Buffer := @FBuffer[Offset];
    Inc(Offset, Bytes);
  end;

  procedure TMSResampler.FlushInternal;
  begin
    FInput.Flush;
    if FOutSampleRate <> FInput.SampleRate then
    begin
      dmo_resampler_free_output(resampler);
      dmo_resampler_free(resampler);
    end;
  end;

end.
