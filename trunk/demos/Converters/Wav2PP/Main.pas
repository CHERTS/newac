(*
 ACS Wav to PasPack file converter.
 Copyright (c) Andrei Borovsky
 You can contact me at anb@symmetrica.net
 You will need Ogg Vorbis codec libraries to
 run this demo.
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ACS_Classes, ACS_Wave, Spin, ExtCtrls, NewACPack;

type
  TForm1 = class(TForm)
    WaveIn1: TWaveIn;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    Label4: TLabel;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    Button2: TButton;
    PasPackOut1: TPasPackOut;
    ComboBox1: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure PasPackOut1Done(Sender: TComponent);
    procedure PasPackOut1Progress(Sender: TComponent);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure PasPackOut1ThreadException(Sender: TComponent);
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
  S : WideString;
begin
  If WaveIn1.FileName <> '' then
  begin
    S := WaveIn1.FileName;
    SaveDialog1.FileName := ChangeFileExt(S, '.ppk');
    if SaveDialog1.Execute then
    begin
      case ComboBox1.ItemIndex of
        0 : PasPackOut1.CodecType := ctAdaptive;
        1 : PasPackOut1.CodecType := ctFast;
        2 : PasPackOut1.CodecType := ctNLMS;
      end;  
      PasPackOut1.FileName := SaveDialog1.FileName;
      PasPackOut1.Run;
      StatusBar1.Panels[0].Text := 'Converting to ' + ExtractFileName(PasPackOut1.FileName);
    end;
  end;
end;

procedure TForm1.PasPackOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
  ProgressBar1.Position := 0;
  if PasPackOut1.ExceptionMessage = '' then
    StatusBar1.Panels[0].Text := 'Converted ' + ExtractFileName(PasPackOut1.FileName)
  else
    StatusBar1.Panels[0].Text := PasPackOut1.ExceptionMessage;
end;

procedure TForm1.PasPackOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := PasPackOut1.Progress;
end;



procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  PasPackOut1.Stop(False);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  If OpenDialog1.Execute then
  begin
    WaveIn1.FileName := OpenDialog1.FileName;
    Self.StatusBar1.Panels[0].Text := 'File to convert: ' + WaveIn1.FileName;
  end;  
end;

procedure TForm1.PasPackOut1ThreadException(Sender: TComponent);
begin
  StatusBar1.Panels[0].Text := PasPackOut1.ExceptionMessage;
end;

end.
