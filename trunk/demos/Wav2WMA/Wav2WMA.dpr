program Wav2WMA;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  libwma1 in '..\..\Source\libwma1.pas',
  wmfintf in '..\..\Source\wmfintf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
