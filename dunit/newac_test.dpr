program newac_test;

(*
  This file is a part of New Audio Components package v 1.6
  Copyright (c) 2002-2007, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net

  ****************************************************************************
*)

(* $Id:$ *)

(* Title: DUnit Test Project
    This project is a collection of test code, called Test Cases, which are
    intended to ensure code security through revisions and refactorings.
    Currently the only focus of the tests are to compare WAVs before and after
    transcoding with lossless formats. The sources are of various sample rates
    and bit depths. The project should be expanded to include tests for all of
    the various aspects of each component.

    For more information on DUnit, check out http://dunit.sourceforge.net. *)


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

