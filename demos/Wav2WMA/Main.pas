(*
 ACS Wav to WMA file converter.
 Copyright (c) Andrei Borovsky
 You can contact me at anb@symmetrica.net
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ACS_Classes, ACS_Wave, Spin, ExtCtrls, ACS_WinMedia;

type
  TForm1 = class(TForm)
    WaveIn1: TWaveIn;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    AlbumEdit: TEdit;
    ArtistEdit: TEdit;
    DateEdit: TEdit;
    GenreEdit: TEdit;
    TitleEdit: TEdit;
    TrackSpinEdit: TSpinEdit;
    RadioGroup1: TRadioGroup;
    WMAOut1: TWMAOut;
    procedure Button1Click(Sender: TObject);
    procedure WMAOut1Done(Sender: TComponent);
    procedure WMAOut1Progress(Sender: TComponent);
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
  S : WideString;
  i : Integer;
begin
  If OpenDialog1.Execute then
  begin
    WaveIn1.FileName := OpenDialog1.FileName;
    S := OpenDialog1.FileName;
    SaveDialog1.FileName := ChangeFileExt(S, '.wma');
    if SaveDialog1.Execute then
    begin
      WMAOut1.FileName := SaveDialog1.FileName;
      WMAOut1.Quality := TWMAQuality(RadioGroup1.ItemIndex + 1);
      WMAOut1.Id3v2Tags.Clear;
      if Self.AlbumEdit.Text <> '' then
        WMAOut1.Id3v2Tags.Album := AlbumEdit.Text;
      if Self.ArtistEdit.Text <> '' then
        WMAOut1.Id3v2Tags.Artist := ArtistEdit.Text;
      if Self.DateEdit.Text <> '' then
        WMAOut1.Id3v2Tags.Year := DateEdit.Text;
      if Self.GenreEdit.Text <> '' then
        WMAOut1.Id3v2Tags.Genre := GenreEdit.Text;
      if Self.TitleEdit.Text <> '' then
        WMAOut1.Id3v2Tags.Title := TitleEdit.Text;
      if Self.TrackSpinEdit.Value <> 0 then
        WMAOut1.Id3v2Tags.Track := IntToStr(TrackSpinEdit.Value);
      Button1.Enabled := False;
      WMAOut1.Run;
      StatusBar1.Panels[0].Text := 'Converting to ' + ExtractFileName(WMAOut1.FileName);
    end;
  end;
end;

procedure TForm1.WMAOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
  ProgressBar1.Position := 0;
  if WMAOut1.ExceptionMessage = '' then
    StatusBar1.Panels[0].Text := 'Converted ' + ExtractFileName(WMAOut1.FileName)
  else
    StatusBar1.Panels[0].Text := WMAOut1.ExceptionMessage;
end;

procedure TForm1.WMAOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := WMAOut1.Progress;
end;



procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WMAOut1.Stop(False);
  WMAOut1.WaitForStop;
end;

end.
