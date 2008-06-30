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
    StatusBar1: TStatusBar;
    ComboBox1: TComboBox;
    Label4: TLabel;
    MP3Out1: TMP3Out;
    Button2: TButton;
    Edit1: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    Edit2: TEdit;
    Edit3: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    Edit4: TEdit;
    Label9: TLabel;
    Edit5: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure MP3Out1Done(Sender: TComponent);
    procedure MP3Out1Progress(Sender: TComponent);
    procedure MP3Out1ThreadException(Sender: TComponent;
      const Msg: String);
    procedure Button2Click(Sender: TObject);
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
begin
  If OpenDialog1.Execute then
  begin
    WaveIn1.FileName := OpenDialog1.FileName;
    StatusBar1.Panels.Items[0].Text := 'File to convert: ' + ExtractFileName(WaveIn1.FileName);
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
    Result := mbr48;
  if S = '56' then
    Result := mbr56;
  if S = '64' then
    Result := mbr64;
  if S = '80' then
    Result := mbr80;
  if S = '96' then
    Result := mbr96;
  if S = '112' then
    Result := mbr112;
  if S = '128' then
    Result := mbr128;
  if S = '192' then
    Result := mbr192;
  if S = '256' then
    Result := mbr256;
  if S = '320' then
    Result := mbr320;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  MP3Out1.BitRate := StrToBitRate(ComboBOx1.Text);
  if WaveIn1.FileName <> '' then
  begin
    SaveDialog1.FileName := ChangeFileExt(WaveIn1.FileName, '.mp3');
    if SaveDialog1.Execute then
    begin
      Self.StatusBar1.Panels[0].Text := 'Converting...';
      MP3Out1.Id3v1Tags.Clear;
      if Edit1.Text <> '' then
         MP3Out1.Id3v1Tags.Title := Edit1.Text;
      if Edit2.Text <> '' then
         MP3Out1.Id3v1Tags.Artist := Edit2.Text;
      if Edit3.Text <> '' then
         MP3Out1.Id3v1Tags.Album := Edit3.Text;
      if Edit4.Text <> '' then
         MP3Out1.Id3v1Tags.Genre := Edit4.Text;
      if Edit5.Text <> '' then
         MP3Out1.Id3v1Tags.Year := StrToInt(Edit5.Text);
      MP3Out1.FileName := SaveDialog1.FileName;
      Button1.Enabled := False;
      MP3Out1.Run;
    end;
  end;

end;

end.
