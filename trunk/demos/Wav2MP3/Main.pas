(*
 NewAC Wav to MP3 file converter.
 Copyright (c) Andrei Borovsky
 You can contact me at anb@symmetrica.net
 You will need LAME encoder to run this demo.
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ACS_Classes, ACS_Wave, Spin,
  ACS_LAME;

type
  TForm1 = class(TForm)
    WaveIn1: TWaveIn;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MP3Out1: TMP3Out;
    StatusBar1: TStatusBar;
    ComboBox1: TComboBox;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure MP3Out1Done(Sender: TComponent);
    procedure MP3Out1Progress(Sender: TComponent);
    procedure MP3Out1ThreadException(Sender: TComponent;
      const Msg: String);
  private
    { Private declarations }
    function StrToBitRate(const S : String) : TMP3Bitrate;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  S : String;
begin
  MP3Out1.BitRate := Self.StrToBitRate(ComboBOx1.Text);
  If OpenDialog1.Execute then
  begin
    WaveIn1.FileName := OpenDialog1.FileName;
    S := OpenDialog1.FileName;
    SetLength(S, Length(S) - 4);
    SaveDialog1.FileName := S + '.mp3';
    if SaveDialog1.Execute then
    begin
      Self.StatusBar1.Panels[0].Text := 'Converting...';
      MP3Out1.FileName := SaveDialog1.FileName;
      MP3Out1.Run;
      Button1.Enabled := False;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MP3Out1.Delay := 16 - TrackBar1.Position*5;
//  MP3Out1.SampleRate := sr22khz;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  MP3Out1.Delay := 16 - TrackBar1.Position*5;
end;

procedure TForm1.MP3Out1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
  if MP3Out1.ExceptionMessage = '' then
    StatusBar1.Panels[0].Text := 'Success'
  else
    StatusBar1.Panels[0].Text := MP3Out1.ExceptionMessage;
end;

procedure TForm1.MP3Out1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := MP3Out1.Progress;
end;

procedure TForm1.MP3Out1ThreadException(Sender: TComponent;
  const Msg: String);
begin
  Self.StatusBar1.Panels[0].Text := Msg;
end;

function TForm1.StrToBitRate;
begin
  if S = '48' then
    Result := br48;
  if S = '56' then
    Result := br56;
  if S = '64' then
    Result := br64;
  if S = '80' then
    Result := br80;
  if S = '96' then
    Result := br96;
  if S = '112' then
    Result := br112;
  if S = '128' then
    Result := br128;
  if S = '192' then
    Result := br192;
  if S = '256' then
    Result := br256;
  if S = '320' then
    Result := br320;
end;

end.
