(* TAudioProcessor component demo. This demo uses TAudioProcessor for swapping input channels. *)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ACS_Classes, ACS_DXAudio, ACS_Wave, ACS_Misc, ACS_Types, StdCtrls;

type
  TForm10 = class(TForm)
    AudioProcessor1: TAudioProcessor;
    WaveIn1: TWaveIn;
    DXAudioOut1: TDXAudioOut;
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    Button2: TButton;
    procedure AudioProcessor1GetData(Sender: TComponent; var Buffer: Pointer;
      var Bytes: Cardinal);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure DXAudioOut1Done(Sender: TComponent);
    procedure AudioProcessor1Init(Sender: TComponent; var TotalSize: Int64);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form10: TForm10;

implementation

{$R *.dfm}

procedure TForm10.AudioProcessor1GetData(Sender: TComponent;
  var Buffer: Pointer; var Bytes: Cardinal);
var
 B32 : PBuffer32;
 B16 : PBuffer16;
 Tmp : Integer;
 i : Integer;
begin
  AudioProcessor1.Input.GetData(Buffer, Bytes);
  if Buffer = nil then
    Exit;
  case AudioProcessor1.Input.BitsPerSample of
    16 :
    begin
      B16 := Buffer;
      for i := 0 to (Bytes div 4) - 1 do
      begin
        Tmp := B16[i*2];
        B16[i*2] := B16[i*2 + 1];
        B16[i*2 + 1] := Tmp;
      end;
    end;
    32 :
    begin
      B32 := Buffer;
      for i := 0 to (Bytes div 8) - 1 do
      begin
        Tmp := B32[i*2];
        B32[i*2] := B32[i*2 + 1];
        B32[i*2 + 1] := Tmp;
      end;
    end;
  end;
end;

procedure TForm10.AudioProcessor1Init(Sender: TComponent; var TotalSize: Int64);
begin
  TAudioProcessor(Sender).Input.Init;
  TotalSize := TAudioProcessor(Sender).Input.Size
end;

procedure TForm10.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    Button1.Enabled := False;
    WaveIn1.FileName := OpenDialog1.FileName;
    DXAudioOut1.Run;
  end;
end;

procedure TForm10.Button2Click(Sender: TObject);
begin
  DXAudioOut1.Stop;
end;


procedure TForm10.DXAudioOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
end;

end.
