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
  Dialogs, ComCtrls, StdCtrls, ACS_Classes, ACS_Vorbis, ACS_Wave, Spin,
  ACS_Converters;

type
  TForm1 = class(TForm)
    WaveIn1: TWaveIn;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    ComboBox1: TComboBox;
    Label4: TLabel;
    CheckBox1: TCheckBox;
    StatusBar1: TStatusBar;
    VorbisOut1: TVorbisOut;
    MSConverter1: TMSConverter;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure VorbisOut1Done(Sender: TComponent);
    procedure VorbisOut1Progress(Sender: TComponent);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
  If WaveIn1.WideFileName <> '' then
  begin
    S := OpenDialog1.FileName;
    SetLength(S, Length(S) - 4);
    SaveDialog1.FileName := S + '.ogg';
    if SaveDialog1.Execute then
    begin
      StatusBar1.Panels[0].Text := 'Converting...';
      VorbisOut1.FileName := SaveDialog1.FileName;
      Button1.Enabled := False;
      VorbisOut1.Run;
    end;
  end;
end;

procedure TForm1.VorbisOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
  if VorbisOut1.ExceptionMessage = '' then
    StatusBar1.Panels[0].Text := 'Success'
  else
    StatusBar1.Panels[0].Text := 'ERROR: ' +VorbisOut1.ExceptionMessage;
end;

procedure TForm1.VorbisOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := VorbisOut1.Progress;
end;



procedure TForm1.FormCreate(Sender: TObject);
begin
  VorbisOut1.DesiredNominalBitrate := br192;
  VorbisOut1.DesiredMaximumBitrate := br192;
  VorbisOut1.MinimumBitrate := br192;
end;

procedure TForm1.ComboBox1Select(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
      0 :
      begin
        VorbisOut1.DesiredNominalBitrate := br24;
        VorbisOut1.DesiredMaximumBitrate := br24;
        VorbisOut1.MinimumBitrate := br24;
      end;
      
      1 :
      begin
        VorbisOut1.DesiredNominalBitrate := br32;
        VorbisOut1.DesiredMaximumBitrate := br32;
        VorbisOut1.MinimumBitrate := br32;
      end;

      2 :
      begin
        VorbisOut1.DesiredNominalBitrate := br64;
        VorbisOut1.DesiredMaximumBitrate := br64;
        VorbisOut1.MinimumBitrate := br64;
      end;

      3 :
      begin
        VorbisOut1.DesiredNominalBitrate := br128;
        VorbisOut1.DesiredMaximumBitrate := br128;
        VorbisOut1.MinimumBitrate := br128;
      end;

      4 :
      begin
        VorbisOut1.DesiredNominalBitrate := br192;
        VorbisOut1.DesiredMaximumBitrate := br192;
        VorbisOut1.MinimumBitrate := br192;
      end;

      5 :
      begin
        VorbisOut1.DesiredNominalBitrate := br256;
        VorbisOut1.DesiredMaximumBitrate := br256;
        VorbisOut1.MinimumBitrate := br256;
      end;

      6 :
      begin
        VorbisOut1.DesiredNominalBitrate := br320;
        VorbisOut1.DesiredMaximumBitrate := br320;
        VorbisOut1.MinimumBitrate := br256;
      end;

      7 :
      begin
        VorbisOut1.DesiredNominalBitrate := br499;
        VorbisOut1.DesiredMaximumBitrate := br499;
        VorbisOut1.MinimumBitrate := br499;
      end;
   end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  VorbisOut1.Stop(False);
  if CheckBox1.Checked then
  begin
    VorbisOut1.Input := MSConverter1;
    MSConverter1.Input := WaveIn1;
  end else
  begin
    VorbisOut1.Input := WaveIn1;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Self.StatusBar1.Panels[0].Text := '';
  If OpenDialog1.Execute then
  begin
    WaveIn1.FileName := OpenDialog1.FileName;
    StatusBar1.Panels[0].Text := 'File to convert: ' + WaveIn1.FileName;
  end;  
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  VorbisOut1.Stop(False);
end;

end.
