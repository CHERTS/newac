(*
  This demo shows how to play a sound file compiled into an application as a resource (*.res).
  If you don't know how to embed binaries as resources into your application,
  there is a good tutorial for you:
  http://delphi.about.com/od/objectpascalide/a/embed_resources.htm
  We embed file kalimba.wma as a resource. We get access to this embedded resource using
  TResourceStream class which is a TStream descendant. All we need now is to assign a
  TResourceStream instance to the WMIn1.Stream property. Now our application (sort of alarm clock)
  can play sound from its internal data, not from an external file (when you compile the app you will see
  that it doesn't need kalimba.wma to play sound).
 *)

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ACS_Classes, ACS_DXAudio, ACS_WinMedia, StrUtils;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Timer1: TTimer;
    Edit1: TEdit;
    Label2: TLabel;
    WMIn1: TWMIn;
    DXAudioOut1: TDXAudioOut;
    Button1: TButton;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    AlarmSet : Boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Label1.Caption := TimeToStr(Time);
  AnsiReplaceStr(Edit1.Text, ' ', '');
  if StrLComp(PChar(Label1.Caption), PChar(Edit1.Text), Length(Edit1.Text)) = 0 then
    if AlarmSet then
    begin
      if DXAudioOut1.Status = tosIdle then
        DXAudioOut1.Run;
      AlarmSet := False;
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Res : TResourceStream;
begin
  Res := TResourceStream.Create(HInstance, 'Alarm', RT_RCDATA);
  WMIn1.Stream := Res;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  AlarmSet := True;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DXAudioOut1.Stop(False);
end;

end.
