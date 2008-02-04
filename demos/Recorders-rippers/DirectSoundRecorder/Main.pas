(*
 NewAC sound recorder based on DirectSound API.
 Copyright (c) 2007, Andrei Borovsky, anb@symmetrica.net.
 Stores recorded data in Wave and Ogg formats.
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ACS_Classes, ACS_Vorbis, StdCtrls, ComCtrls, 
  ACS_DXAudio, Spin, ExtCtrls, ACS_Wave, ACS_FLAC;

type
  TForm1 = class(TForm)
    SelectFileButton: TButton;
    RecordButton: TButton;
    SaveDialog1: TSaveDialog;
    Label1: TLabel;
    StatusBar1: TStatusBar;
    StopButton: TButton;
    Timer1: TTimer;
    SpinEdit1: TSpinEdit;
    Label2: TLabel;
    SpinEdit2: TSpinEdit;
    Label3: TLabel;
    Label4: TLabel;
    SREdit: TEdit;
    StereoCheckBox: TCheckBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    RadioGroup1: TRadioGroup;
    PauseButton: TButton;
    VorbisOut1: TVorbisOut;
    WaveOut1: TWaveOut;
    DXAudioIn1: TDXAudioIn;
    FLACOut1: TFLACOut;
    procedure RecordButtonClick(Sender: TObject);
    procedure SaveDialog1TypeChange(Sender: TObject);
    procedure OutputDone(Sender: TComponent);
    procedure StopButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure SelectFileButtonClick(Sender: TObject);
    procedure PauseButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    Output : TAuFileOut;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.RecordButtonClick(Sender: TObject);
var
  Ext : String;
begin
  Ext := ExtractFileExt(SaveDialog1.FileName);
  Ext := AnsiLowerCase(Ext);
  if Ext = '' then
    raise EAuException.Create('Cannot determine the input file format');
  if Ext = '.ogg' then
  begin
    Output := VorbisOut1;
    VorbisOut1.Compression := SpinEdit1.Value/10;
  end else
  if Ext = '.flac' then
  begin
    Output := FLACOut1;
  end else
  if Ext = '.wav' then
    Output := WaveOut1;
  Output.FileName := SaveDialog1.FileName;
  DXAudioIn1.InSampleRate := StrToInt(SREdit.Text);
  if StereoCheckBox.Checked then
    DXAudioIn1.InChannels := 2
  else
    DXAudioIn1.InChannels := 1;
  if RadioGroup1.ItemIndex = 0 then
    DXAudioIn1.InBitsPerSample := 16
  else
    DXAudioIn1.InBitsPerSample := 24;
  RecordButton.Enabled := False;
  SelectFileButton.Enabled := False;
  SpinEdit2.Enabled := False;
  Output.Run;
  StatusBar1.Panels.Items[0].Text := Format('Recording to "%s"', [SaveDialog1.FileName]);
  Timer1.Interval := 1000;
end;


procedure TForm1.SaveDialog1TypeChange(Sender: TObject);
begin
  if (SaveDialog1.FilterIndex = 1) then
  SaveDialog1.DefaultExt := '.wav'
  else
  SaveDialog1.DefaultExt := '.ogg'
end;

procedure TForm1.OutputDone(Sender: TComponent);
begin
  SelectFileButton.Enabled := True;
  RecordButton.Enabled := True;
  SpinEdit2.Enabled := True;
  if Output <> nil then
  begin
    Output.Stop(False);
  end;  
end;

procedure TForm1.StopButtonClick(Sender: TObject);
begin
  if Output <> nil then
    Output.Stop;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if Output <> nil then
  begin
    Label2.Caption := Format('%d seconds elapsed', [Output.TimeElapsed]);
    Label7.Caption := IntToStr(DXAudioIn1.Overruns);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SpinEdit2.MaxValue := DXAudioIn1.DeviceCount - 1;
  SpinEdit2.MinValue := 0;
  SpinEdit2.Value := 0;
  Label4.Caption := DXAudioIn1.DeviceName[SpinEdit2.Value];
end;

procedure TForm1.SpinEdit2Change(Sender: TObject);
begin
  Label4.Caption := DXAudioIn1.DeviceName[SpinEdit2.Value];
  DXAudioIn1.DeviceNumber := SpinEdit2.Value;
end;

procedure TForm1.SelectFileButtonClick(Sender: TObject);
begin
  SaveDialog1.Execute;
end;

procedure TForm1.PauseButtonClick(Sender: TObject);
begin
  if Output <> nil then
  begin
    if Output.Status = tosPlaying then Output.Pause
    else
    if Output.Status = tosPaused then Output.Resume;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Output <> nil then
  begin
    Output.Stop(False);
  end;  
end;

end.
