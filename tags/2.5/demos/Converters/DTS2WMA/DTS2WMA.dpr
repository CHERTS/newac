program DTS2WMA;

uses
  Forms,
  Main in '..\Wav2WMA-2\Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
