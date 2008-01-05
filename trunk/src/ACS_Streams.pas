(*
  This file is a part of New Audio Components package v 1.2
  Copyright (c) 2002-2007, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Revision: 1.3 $ $Date: 2007/08/15 10:00:23 $ *)

unit ACS_Streams;

{ Title: ACS_Streams
    Utility classes for streams. }

interface

uses
  Classes, SysUtils, ACS_Classes;

const

  OUTBUF_SIZE = $4000;
  INBUF_SIZE = $8000;

type

  TStreamOut = class(TAuStreamedOutput)
  private
    function GetSR : Integer;
    function GetBPS : Integer;
    function GetCh : Integer;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OutSampleRate : Integer read GetSR;
    property OutBitsPerSample : Integer read GetBPS;
    property OutChannles : Integer read GetCh;
  end;

  TStreamIn = class(TAuStreamedInput)
  private
    FBPS, FChan, FFreq : Integer;
    _Buffer : Pointer;
    CurrentBufferSize : Integer;
  protected
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : Integer); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  published
    property InBitsPerSample : Integer read FBPS write FBPS;
    property InChannels : Integer read FChan write FChan;
    property InSampleRate : Integer read FFreq write FFreq;
  end;


implementation

procedure TStreamOut.Prepare;
begin
  if not FStreamAssigned then
  raise EAuException.Create('Stream is not assigned.');
  FInput.Init;
end;

procedure TStreamOut.Done;
begin
  FInput.Flush;
end;

function TStreamOut.DoOutput;
var
  Len : Integer;
  P : Pointer;
begin
  // No exceptions Here
  Result := True;
  if not Busy then Exit;
  if Abort or (not CanOutput) then
  begin
    Result := False;
    Exit;
  end;
  GetMem(P, OUTBUF_SIZE);
  Len := OUTBUF_SIZE;
  Finput.GetData(P, Len);
  if Len > 0 then
  begin
    Result := True;
    FStream.WriteBuffer(P^, Len);
  end
  else Result := False;
  FreeMem(P);
end;

constructor TStreamOut.Create;
begin
  inherited Create(AOwner);
end;

destructor TStreamOut.Destroy;
begin
  inherited Destroy;
end;

constructor TStreamIn.Create;
begin
  inherited Create(AOwner);
  FBPS := 8;
  FChan := 1;
  FFreq := 8000;
  FSize := -1;
  if not (csDesigning	in ComponentState) then
  begin
    CurrentBufferSize := INBUF_SIZE;
    GetMem(_Buffer, CurrentBufferSize);
  end;
end;

destructor TStreamIn.Destroy;
begin
  if not (csDesigning	in ComponentState) then
  begin
    FreeMem(_Buffer);
  end;  
  inherited Destroy;
end;

procedure TStreamIn.InitInternal;
begin
  if Busy then raise EAuException.Create('The component is busy');
  if not Assigned(FStream) then raise EAuException.Create('Stream object not assigned');
  FPosition := FStream.Position;
  Busy := True;
  FSize := FStream.Size;
end;

procedure TStreamIn.FlushInternal;
begin
//  FStream.Position := 0;
  Busy := False;
end;

procedure TStreamIn.GetDataInternal;
begin
  if Bytes > CurrentBufferSize then
  begin
    CurrentBufferSize := Bytes;
    FreeMem(_Buffer);
    GetMem(_Buffer, CurrentBufferSize);
  end;
  Bytes := FStream.Read(_Buffer^, Bytes);
  Buffer := _Buffer;
  FPosition := FStream.Position;
  //  Inc(FPosition, Result);
  if FPosition >= FSize then
  begin
    Bytes := 0;
    Buffer := nil;
  end;  
end;

function TStreamOut.GetSR;
begin
  if not Assigned(Input) then
  raise EAuException.Create('Input is not assigned.');
  Result := FInput.SampleRate;
end;

function TStreamOut.GetBPS;
begin
  if not Assigned(Input) then
  raise EAuException.Create('Input is not assigned.');
  Result := FInput.BitsPerSample;
end;

function TStreamOut.GetCh;
begin
  if not Assigned(Input) then
  raise EAuException.Create('Input is not assigned.');
  Result := FInput.Channels;
end;

function TStreamIn.GetBPS;
begin
  Result := FBPS
end;

function TStreamIn.GetCh;
begin
  Result := FChan;
end;

function TStreamIn.GetSR;
begin
  Result := Self.FFreq;
end;


end.
