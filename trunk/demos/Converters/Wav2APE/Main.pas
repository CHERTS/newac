(*
 NewAC Wav to Ogg file converter.
 Copyright (c) Andrei Borovsky
 You can contact me at anb@symmetrica.net
 You will need Ogg Vorbis codec libraries to
 run this demo.
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ACS_Classes, ACS_Wave, Spin, ACS_MAC;

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
    Label4: TLabel;
    SpinEdit1: TSpinEdit;
    MACOut1: TMACOut;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure MP3Out1ThreadException(Sender: TComponent;
      const Msg: String);
    procedure MACOut1Done(Sender: TComponent);
    procedure MACOut1Progress(Sender: TComponent);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
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
  MACOut1.CompressionLevel := SpinEdit1.Value;
  If OpenDialog1.Execute then
  begin
    WaveIn1.FileName := OpenDialog1.FileName;
    S := OpenDialog1.FileName;
    SetLength(S, Length(S) - 4);
    SaveDialog1.FileName := S + '.ape';
    if SaveDialog1.Execute then
    begin
      Button1.Enabled := False;
      Self.StatusBar1.Panels[0].Text := 'Converting...';
      MACOut1.FileName := SaveDialog1.FileName;
      MACOut1.Run;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MACOut1.Delay := 16 - TrackBar1.Position*5;
//  MP3Out1.SampleRate := sr22khz;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  MACOut1.Delay := 16 - TrackBar1.Position*5;
end;

procedure TForm1.MP3Out1ThreadException(Sender: TComponent;
  const Msg: String);
begin
  Self.StatusBar1.Panels[0].Text := Msg;
end;

procedure TForm1.MACOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
  if MACOut1.ExceptionMessage = '' then
    StatusBar1.Panels[0].Text := 'Success'
  else
    StatusBar1.Panels[0].Text := MACOut1.ExceptionMessage;
end;

procedure TForm1.MACOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := MACOut1.Progress;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MACOut1.Stop(False);
end;

end.
