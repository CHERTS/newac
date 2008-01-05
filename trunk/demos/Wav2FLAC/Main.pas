(*
 NewAC Wav to FLAC file converter.
 Copyright (c) Andrei Borovsky,
 You can contact me at anb@symmetrica.net
 You will need the FLAC codec library to
 run this demo.
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ACS_Classes, ACS_Wave, Spin,
  ACS_FLAC;

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
    FLACOut1: TFLACOut;
    StatusBar1: TStatusBar;
    SpinEdit1: TSpinEdit;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FLACOut1Progress(Sender: TComponent);
    procedure FLACOut1Done(Sender: TComponent);
    procedure FLACOut1ThreadException(Sender: TComponent;
      const Msg: String);
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
  If OpenDialog1.Execute then
  begin
    WaveIn1.FileName := OpenDialog1.FileName;
    S := WaveIn1.FileName;
    SaveDialog1.FileName := ChangeFileExt(S, '.flac');
    if SaveDialog1.Execute then
    begin
      StatusBar1.Panels[0].Text := 'Converting...';
      FLACOut1.FileName := SaveDialog1.FileName;
      FLACOut1.CompressionLevel := SpinEdit1.Value;
      Button1.Enabled := False;
      FLACOut1.Run;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FLACOut1.Delay := 16 - TrackBar1.Position*5;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  FLACOut1.Delay := 16 - TrackBar1.Position*5;
end;

procedure TForm1.FLACOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := FLACOut1.Progress;
end;

procedure TForm1.FLACOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
  StatusBar1.Panels[0].Text := 'Success';
end;

procedure TForm1.FLACOut1ThreadException(Sender: TComponent;
  const Msg: String);
begin
  StatusBar1.Panels[0].Text := Msg;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FLACOut1.Stop(False);
  FLACOut1.WaitForStop;
end;

end.
