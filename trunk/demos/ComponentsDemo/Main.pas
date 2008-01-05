(*
 NewAC Wave player demo (shows how the TDemoWaveIn component works).
 See ACS documentation on where to find the library.
 (c) Andrei Borovsky, anb@symmetrica.net
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ACS_Classes,
  ACS_Audio, Buttons, ComponentsDemo;

type
  TForm1 = class(TForm)
    AudioOut1: TAudioOut;
    OpenDialog1: TOpenDialog;
    Label6: TLabel;
    Label7: TLabel;
    BitBtn1: TBitBtn;
    BitBtn3: TBitBtn;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    ForwardButton: TButton;
    BackwardButton: TButton;
    CheckBox1: TCheckBox;
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
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FS : TFileStream;
    WaveIn : TDemoWaveIn;
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
    WaveIn.FileName := OpenDialog1.FileName;
//    if not MP3In1.Valid then
//    begin
//      StatusBar1.Panels[0].Text := 'MP3 file is not valid.';
//      Exit;
//    end;
    BitBtn1.Enabled := False;
    StatusBar1.Panels[0].Text := WaveIn.FileName;
    AudioOut1.Run;
    Secs := IntToStr(WaveIn.TotalTime mod 60);
    if WaveIn.TotalTime mod 60 < 10 then Secs := '0'+Secs;
    Label7.Caption := Format('%d:%s', [WaveIn.TotalTime div 60, Secs]);
    WaveIn.Loop := CheckBox1.Checked;
  end;
end;

procedure TForm1.AudioOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := AudioOut1.Progress;
end;

procedure TForm1.AudioOut1Done(Sender: TComponent);
begin
   BitBtn1.Enabled := True;
   ProgressBar1.Position := 0;
   StatusBar1.Panels[0].Text := AudioOut1.ExceptionMessage;
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

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AudioOut1.Stop(False);
  AudioOut1.WaitForStop;
  WaveIn.Free;
end;

procedure TForm1.AudioOut1ThreadException(Sender: TComponent;
  const Msg: String);
begin
  StatusBar1.Panels[0].Text := Msg;
end;

procedure TForm1.ForwardButtonClick(Sender: TObject);
begin
  WaveIn.Jump(10);
end;

procedure TForm1.BackwardButtonClick(Sender: TObject);
begin
  WaveIn.Jump(-10);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  WaveIn.Loop := CheckBox1.Checked;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  WaveIn := TDemoWaveIn.Create(Self);
  AudioOut1.Input := WaveIn;
end;

end.
