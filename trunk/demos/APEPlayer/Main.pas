(*
 NewAC APE (Monkey's Auido) player demo.
 To run this demo you will need the MACDll.dll library.
 See ACS documentation on where to find the library.
 (c) Andrei Borovsky, anb@symmetrica.net
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ACS_Classes,
  ACS_Audio, Buttons, ACS_MAC, ACS_DXAudio;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    ForwardButton: TButton;
    BackwardButton: TButton;
    CheckBox1: TCheckBox;
    MACIn1: TMACIn;
    DXAudioOut1: TDXAudioOut;
    procedure BitBtn1Click(Sender: TObject);
    procedure AudioOut1Progress(Sender: TComponent);
    procedure AudioOut1Done(Sender: TComponent);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AudioOut1ThreadException(Sender: TComponent;
      const Msg: String);
    procedure ForwardButtonClick(Sender: TObject);
    procedure BackwardButtonClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
    FS : TFileStream;
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
  FN : String;
begin
  if OpenDialog1.Execute then
  begin
    MACIn1.FileName := OpenDialog1.FileName;
//    if not MP3In1.Valid then
//    begin
//      StatusBar1.Panels[0].Text := 'MP3 file is not valid.';
//      Exit;
//    end;
    BitBtn1.Enabled := False;
    StatusBar1.Panels[0].Text := MACIn1.FileName;
    DXAudioOut1.Run;
    Label4.Caption := IntToStr(MACIn1.SampleRate) + ' Hz';
    if MACIn1.Channels = 1 then Label8.Caption := 'Mono'
    else Label8.Caption := 'Stereo';
    Secs := IntToStr(MACIn1.TotalTime mod 60);
    if MACIn1.TotalTime mod 60 < 10 then Secs := '0'+Secs;
    Label7.Caption := Format('%d:%s', [MACIn1.TotalTime div 60, Secs]);
    MACIn1.Loop := CheckBox1.Checked;
  end;
end;

procedure TForm1.AudioOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := DXAudioOut1.Progress;
end;

procedure TForm1.AudioOut1Done(Sender: TComponent);
begin
   BitBtn1.Enabled := True;
   ProgressBar1.Position := 0;
   StatusBar1.Panels[0].Text := DXAudioOut1.ExceptionMessage;
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  if DXAudioOut1.Status = tosPlaying then
  DXAudioOut1.Pause
  else
  DXAudioOut1.Resume;
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
  DXAudioOut1.Stop;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DXAudioOut1.Stop(False);
  DXAudioOut1.WaitForStop;
  MACIn1.Free;
end;

procedure TForm1.AudioOut1ThreadException(Sender: TComponent;
  const Msg: String);
begin
  StatusBar1.Panels[0].Text := Msg;
end;

procedure TForm1.ForwardButtonClick(Sender: TObject);
begin
  MACIn1.Jump(10);
end;

procedure TForm1.BackwardButtonClick(Sender: TObject);
begin
  MACIn1.Jump(-10);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  MACIn1.Loop := CheckBox1.Checked;
end;

end.
