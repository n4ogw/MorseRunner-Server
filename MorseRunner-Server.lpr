program MorseRunnerServer;

{$MODE Delphi}

uses
  {$IFDEF Linux}
  cthreads,
  {$ENDIF}
  Forms, Interfaces, 
  Main in 'Main.pas' {MainForm},
  Setup in 'Setup.pas',
  Contest in 'Contest.pas',
  RndFunc in 'RndFunc.pas',
  Ini in 'Ini.pas',
  Station in 'Station.pas',
  MorseKey in 'VCL\MorseKey.pas',
  StnColl in 'StnColl.pas',
  DxStn in 'DxStn.pas',
  MyStn in 'MyStn.pas',
  CallLst in 'CallLst.pas',
  QrmStn in 'QrmStn.pas',
  Log in 'Log.pas',
  Qsb in 'Qsb.pas',
  DxOper in 'DxOper.pas',
  QrnStn in 'QrnStn.pas',
  SndTypes in 'VCL\SndTypes.pas',
  MorseTbl in 'VCL\MorseTbl.pas',
  QuickAvg in 'VCL\QuickAvg.pas',
  MovAvg in 'VCL\MovAvg.pas',
  Mixers in 'VCL\Mixers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Morse Runner Server';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

