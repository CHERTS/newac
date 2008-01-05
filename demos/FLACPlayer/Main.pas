(*
 NewAC Ogg Vorbis audio player demo main unit.
 To run this demo you will need Ogg Vorbis ibraries.
 See the ACS documentation on where to find these libraries.
 (c) Andrei Borovsky. You can contact me at anb@symmetrica.net
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ACS_Classes,
  ACS_Audio, ACS_Vorbis, Buttons, ACS_Converters, ACS_FLAC;

type
  TForm1 = class(TForm)
    AudioOut1: TAudioOut;
    OpenDialog1: TOpenDialog;
    Label1: TLabel;
    Label2: TLabel;
    Timer2: TTimer;
    Label3: TLabel;
    ScrollBar1: TScrollBar;
    Label5: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Memo1: TMemo;
    Label9: TLabel;
    StereoBalance1: TStereoBalance;
    TrackBar1: TTrackBar;
    Label10: TLabel;
    FLACIn1: TFLACIn;
    procedure BitBtn1Click(Sender: TObject);
    procedure AudioOut1Progress(Sender: TComponent);
    procedure AudioOut1Done(Sender: TComponent);
    procedure Timer2Timer(Sender: TObject);
    procedure ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  Secs : String;
begin
  if OpenDialog1.Execute then
  begin
    FLACIn1.FileName := OpenDialog1.FileName;
    if not FLACIn1.Valid then
    begin
      Label3.Caption := 'Ogg file is not valid.';
      Exit;
    end;
    BitBtn1.Enabled := False;
    Label3.Caption := FLACIn1.FileName;
    AudioOut1.Run;
    Label4.Caption := IntToStr(FLACIn1.SampleRate);
    if FLACIn1.Channels = 1 then Label8.Caption := 'Mono'
    else Label8.Caption := 'Stereo';
    Secs := IntToStr(FLACIn1.TotalTime mod 60);
    if FLACIn1.TotalTime mod 60 < 10 then Secs := '0'+Secs;
    Label7.Caption := Format('%d:%s', [FLACIn1.TotalTime div 60, Secs]);
    ScrollBar1.Enabled := True;
    Memo1.Clear;
    if FLACIn1.VorbisComments.Artist <> '' then
      Memo1.Lines.Add('Artist: ' + FLACIn1.VorbisComments.Artist);
    if FLACIn1.VorbisComments.Album <> '' then
      Memo1.Lines.Add('Album: ' + FLACIn1.VorbisComments.Album);
    if FLACIn1.VorbisComments.Title <> '' then
      Memo1.Lines.Add('Title: ' + FLACIn1.VorbisComments.Title);
    if FLACIn1.VorbisComments.Date <> '' then
      Memo1.Lines.Add('Date: ' + FLACIn1.VorbisComments.Date);
    if FLACIn1.VorbisComments.Genre <> '' then
      Memo1.Lines.Add('Genre: ' + FLACIn1.VorbisComments.Genre);
    if FLACIn1.VorbisComments.Track <> 0 then
      Memo1.Lines.Add('Track: ' + IntToStr(FLACIn1.VorbisComments.Track));
  end;
end;

procedure TForm1.AudioOut1Progress(Sender: TComponent);
begin
  ScrollBar1.Position := AudioOut1.Progress;
end;

procedure TForm1.AudioOut1Done(Sender: TComponent);
begin
   BitBtn1.Enabled := True;
   ScrollBar1.Enabled := False;
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
//  Label1.Caption := IntToStr(In1.InstantBitRate);
end;

procedure TForm1.ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  FLACIn1.Jump(ScrollPos-AudioOut1.Progress);
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  if AudioOut1.Status = tosPlaying then
  AudioOut1.Pause
  else
  AudioOut1.Resume;
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
  AudioOut1.Stop;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  StereoBalance1.Balance := TrackBar1.Position/10;
end;

end.
