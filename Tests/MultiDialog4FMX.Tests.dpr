program MultiDialog4FMX.Tests;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF }
  DUnitX.TestFramework,
  MultiDialog4FMX.Interfaces in '..\src\MultiDialog4FMX.Interfaces.pas',
  MultiDialog4FMX.Base in '..\src\MultiDialog4FMX.Base.pas',
  MultiDialog4FMX.Factory in '..\src\MultiDialog4FMX.Factory.pas',
  MultiDialog4FMX.Android in '..\src\MultiDialog4FMX.Android.pas',
  MultiDialog4FMX.Tests.Mocks in 'MultiDialog4FMX.Tests.Mocks.pas',
  MultiDialog4FMX.Tests.Builder in 'MultiDialog4FMX.Tests.Builder.pas',
  MultiDialog4FMX.Tests.Buttons in 'MultiDialog4FMX.Tests.Buttons.pas',
  MultiDialog4FMX.Tests.Factory in 'MultiDialog4FMX.Tests.Factory.pas',
  MultiDialog4FMX.Tests.Android in 'MultiDialog4FMX.Tests.Android.pas',
  MultiDialog4FMX.Tests.MemoryLeaks in 'MultiDialog4FMX.Tests.MemoryLeaks.pas';

{$IFNDEF TESTINSIGHT}
var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger : ITestLogger;
{$ENDIF}

begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    // Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    
    // Create the test runner
    runner := TDUnitX.CreateRunner;
    
    // Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    
    // When true, Assertions must be made during tests
    runner.FailsOnNoAsserts := False;
    
    // Tell the runner how we will log things
    // Log to the console window if desired
    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      runner.AddLogger(logger);
    end;
    
    // Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    
    // Run tests
    results := runner.Execute;
    
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    // We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
    begin
      System.Writeln(E.ClassName, ': ', E.Message);
      System.ExitCode := EXIT_ERRORS;
    end;
  end;
{$ENDIF}
end.
