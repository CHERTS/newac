program newac_test;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  ACS_Classes in '..\src\ACS_Classes.pas',
  ACS_WavPack in '..\src\ACS_WavPack.pas',
  Test_ACS_WavPack in 'Test_ACS_WavPack.pas',
  ACS_FLAC in '..\src\ACS_FLAC.pas',
  Test_ACS_FLAC in 'Test_ACS_FLAC.pas',
  uTestBase in 'uTestBase.pas',
  Test_ACS_MAC in 'Test_ACS_MAC.pas';

{$R *.RES}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

