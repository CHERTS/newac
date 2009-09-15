unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ACS_Classes, ACS_WinMedia, ACS_Misc, NewACDTS, ComCtrls;

type
  TForm6 = class(TForm)
    ListBox1: TListBox;
    OpenDialog1: TOpenDialog;
    DTSIn1: TDTSIn;
    AudioPlayList1: TAudioPlayList;
    WMAOut1: TWMAOut;
    Button1: TButton;
    Button2: TButton;
    SaveDialog1: TSaveDialog;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Button3: TButton;
    StatusBar1: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure WMAOut1Done(Sender: TComponent);
    procedure WMAOut1ThreadException(Sender: TComponent);
    procedure AudioPlayList1PlayItemChanged(Sender: TComponent);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}

procedure TForm6.AudioPlayList1PlayItemChanged(Sender: TComponent);
begin
  StatusBar1.Panels[0].Text := Format('Converting %s', [ListBox1.Items[AudioPlayList1.CurrentItem]]);
end;

procedure TForm6.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    ListBox1.Items.AddStrings(OpenDialog1.Files);
  end;
end;

procedure TForm6.Button2Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    WMAOut1.FileName := SaveDialog1.FileName;
    WMAOut1.Id3v2Tags.Artist := Edit1.Text;
    WMAOut1.Id3v2Tags.Album := Edit2.Text;
    AudioPlayList1.Files.AddStrings(ListBox1.Items);
    Button1.Enabled := False;
    Button2.Enabled := False;
    Button3.Enabled := False;
    WMAOut1.Run;
  end;
end;

procedure TForm6.Button3Click(Sender: TObject);
begin
  ListBox1.Clear;
end;

procedure TForm6.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WMAOut1.Stop(False);
end;

procedure TForm6.WMAOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
  Button2.Enabled := True;
  Button3.Enabled := True;
  if WMAOut1.ExceptionMessage = '' then
    StatusBar1.Panels[0].Text := 'Done.';
end;

procedure TForm6.WMAOut1ThreadException(Sender: TComponent);
begin
  StatusBar1.Panels[0].Text := WMAOut1.ExceptionMessage;
end;

end.
