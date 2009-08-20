(*
  This file is a part of New Audio Components package v 1.3.1
  Copyright (c) 2002-2007, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit ACS_smpeg;

(* Title: ACS_SMPEG
    Delphi interface for mp3 playback. Uses Windows Media Audio decoder (the <TWMin> component). *)

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Classes, ACS_Tags, ACS_WinMedia;


type

// Yeah, folks that's all there is to it.

(* Class: TMP3In
   The mp3 file/stream decoder that uses the Windows built-in decoder,
   descends from <TWMIn> *)

  TMP3In = class (TWMIn)
  protected
    procedure OpenFile; override;
  end;

implementation

  procedure TMP3In.OpenFile;
  begin
    if FOpened = 0 then
    begin
      inherited;
      FSize := FSize + (FSR div 3)*(FChan*(FBPS div 8));
      FTotalSamples := FTotalSamples + (FSR div 3);
    end else
      Inherited;
  end;
end.
