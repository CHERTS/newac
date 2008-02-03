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
  ACS_FLAC, ExtCtrls;

type
  TForm1 = class(TForm)
    WaveIn1: TWaveIn;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    StatusBar1: TStatusBar;
    SpinEdit1: TSpinEdit;
    Label4: TLabel;
    Panel1: TPanel;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    SpinEdit2: TSpinEdit;
    FLACOut1: TFLACOut;
    procedure Button1Click(Sender: TObject);
    procedure FLACOut1Progress(Sender: TComponent);
    procedure FLACOut1Done(Sender: TComponent);
    procedure FLACOut1ThreadException(Sender: TComponent;
      const Msg: String);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
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
begin
  if WaveIn1.FileName = '' then Exit;
  SaveDialog1.FileName := ChangeFileExt(WaveIn1.FileName, '.flac');
  if SaveDialog1.Execute then
  begin
    StatusBar1.Panels[0].Text := 'Converting...';
    FLACOut1.FileName := SaveDialog1.FileName;
    FLACOut1.CompressionLevel := SpinEdit1.Value;
    FLACOut1.Tags.Title := Edit1.Text;
    FLACOut1.Tags.Album := Edit2.Text;
    FLACOut1.Tags.Artist := Edit3.Text;
    FLACOut1.Tags.Date := Edit4.Text;
    FLACOut1.Tags.Genre := Edit5.Text;
    FLACOut1.Tags.Track := SpinEdit2.Value;        
    Button1.Enabled := False;
    FLACOut1.Run;
  end;
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
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  If OpenDialog1.Execute then
  begin
    WaveIn1.FileName := OpenDialog1.FileName;
    StatusBar1.Panels[0].Text := ExtractFileName(WaveIn1.FileName);
  end;
end;

end.
