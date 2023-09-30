//------------------------------------------------------------------------------
//This Source Code Form is subject to the terms of the Mozilla Public
//License, v. 2.0. If a copy of the MPL was not distributed with this
//file, You can obtain one at http://mozilla.org/MPL/2.0/.
//------------------------------------------------------------------------------
unit Ini;

{$MODE Delphi}

interface

uses
  SysUtils, IniFiles, Math;

const
  SEC_STN = 'Station';
  SEC_BND = 'Band';
  SEC_TST = 'Contest';
  SEC_SYS = 'System';

  DEFAULTBUFCOUNT = 1;
  DEFAULTPOLLTIME = 25;  // time to poll serial ports (ms)
  DEFAULTBUFSIZE = 1024; // audio buffer size
  DEFAULTRATE = 22050;   // audio sample rate (Hz)

type
  TRunMode = (rmStop, rmRun);
  TOpMode = (opWpx, opCab);
  TSerialMode = (modeWinkey, modeSo2rmini);
  TBackgroundMode = (none, randomNoise, randomQrn, wavNoise);

var
  Wpm: integer = 30;
  BandWidth: integer = 500;
  BackgroundMode: TBackgroundMode = randomNoise;
  wavVolume: integer = 60;
  wavVol: single;
  monVol: single;
  Pitch: integer = 500;
  Qsk: boolean = False;
  Rit: integer = 0;
  BufSize: integer = DEFAULTBUFSIZE;
  Activity: integer = 2;
  SerialPollTime: integer = DEFAULTPOLLTIME;
  Qrm: boolean = True;
  Qsb: boolean = True;
  Flutter: boolean = True;
  Lids: boolean = True;
  OpMode: TOpMode = opWpx;
  RunMode: TRunMode = rmStop;
  serialMode: TSerialMode = modeWinkey;
  {$IFDEF Linux}
  serialPortWinkey  : string = '/tmp/pty1a';
  serialPortOtrsp  : string = '/tmp/pty2a';
  serialPortSo2rmini  : string = '/tmp/pty1a';
  {$ENDIF}
  {$IFDEF Windows}
  serialPortWinkey  : string = 'COM12';
  serialPortOtrsp  : string = 'COM14';
  serialPortSo2rmini  : string = 'COM16';
  {$ENDIF}
  cabFile: string = '';
  wavFile: string = '';

procedure FromIni;
procedure ToIni;

implementation

uses
  Main, Setup, Contest, Serial;

procedure FromIni;
var
  V: integer;
  configFile: string;
begin
   {$IFDEF Windows}
   configFile := GetAppConfigFile(False);
   {$ENDIF}
   {$IFDEF Linux}
   configFile := GetUserDir+'.local/share/morserunner-server/morserunner-server.ini';
   {$ENDIF}

  with TIniFile.Create(configFile) do
    try
      MainForm.SetPitch(ReadInteger(SEC_STN, 'Pitch', 4));
      MainForm.SetBw(ReadInteger(SEC_STN, 'BandWidth', 9));
      Wpm := ReadInteger(SEC_STN, 'Wpm', Wpm);
      Wpm := Max(10, Min(120, Wpm));
      MainForm.SpinEdit1.Value := Wpm;
      Tst[1].Me.Wpm := Wpm;
      Tst[2].Me.Wpm := Wpm;
      MainForm.SetQsk(ReadBool(SEC_STN, 'Qsk', Qsk));
      V := ReadInteger(SEC_STN, 'SelfMonVolume', -74);
      MainForm.TrackBar.position := Round((V + 80) * 2.5);
      MonVol := Power(10, V / 20.0);
      case ReadInteger(SEC_TST, 'SerialMode', 0) of
        0: serialMode := modeWinkey;
        1: serialMode := modeSo2rmini;
      end;
   {$IFDEF Linux}
      serialPortWinkey := ReadString(SEC_STN, 'serialPortWinkey', serialPortWinkey);
      serialPortOTRSP := ReadString(SEC_STN, 'serialPortOtrsp', serialPortOTRSP);
      serialPortSo2rmini := ReadString(SEC_STN, 'serialPortSo2rmini', serialPortSo2rMini);
   {$ENDIF}
   {$IFDEF Windows}
      serialPortWInkey := ReadString(SEC_STN, 'serialPortWinkey', serialPortWinkey);
      serialPortOTRSP := ReadString(SEC_STN, 'serialPortOtrsp', serialPortOTRSP);
      serialPortSo2rmini := ReadString(SEC_STN, 'serialPortSo2rmini', serialPortSo2rMini);
   {$ENDIF}

      Ser.serialInit;

      Activity := ReadInteger(SEC_BND, 'Activity', Activity);
      MainForm.SpinEdit3.Value := Activity;
      V := ReadInteger(SEC_BND, 'BackgroundMode', 1);
      MainForm.SetBackground(V);
      wavVolume := ReadInteger(SEC_BND, 'WavVolume', wavVolume);
      WavVol := 1000 + Power(7, wavVolume / 10);
      MainForm.TrackBar1.position := wavVolume;
      MainForm.CheckBox3.Checked := ReadBool(SEC_BND, 'Qrm', Qrm);
      MainForm.CheckBox2.Checked := ReadBool(SEC_BND, 'Qsb', Qsb);
      MainForm.CheckBox5.Checked := ReadBool(SEC_BND, 'Flutter', Flutter);
      MainForm.CheckBox6.Checked := ReadBool(SEC_BND, 'Lids', Lids);
      MainForm.ReadCheckBoxes;

      // buffer size, serial poll time. These are read-only from cfg file
      V := ReadInteger(SEC_SYS, 'BufSize', 0);
      if V = 0 then
      begin
        V := 4;
        WriteInteger(SEC_SYS, 'BufSize', V);
      end;
      V := Max(1, Min(5, V));
      BufSize := 64 shl V;
      Tst[1].Filt.SamplesInInput := BufSize;
      Tst[1].Filt2.SamplesInInput := BufSize;
      Tst[2].Filt.SamplesInInput := BufSize;
      Tst[2].Filt2.SamplesInInput := BufSize;
      SerialPollTime := ReadInteger(SEC_SYS, 'SerialPollTime', DEFAULTPOLLTIME);

      case ReadInteger(SEC_TST, 'Mode', 0) of
        0:
        begin
          opMode := opWPX;
        end;
        1:
        begin
          opMode := opCab;
        end;
      end;
      cabFile := ReadString(SEC_TST, 'CabrilloFile', '');
      if cabFile <> '' then
	 MainForm.setCabrillo(cabFile, False);
      wavFile := ReadString(SEC_TST, 'WavFile', '');
      if wavFile <> '' then
	 MainForm.setWav(wavFile, False);
    finally
      Free;
    end;
end;


procedure ToIni;
var
  configFile: string;
begin
   {$IFDEF Windows}
   configFile := GetAppConfigFile(False);
   {$ENDIF}
   {$IFDEF Linux}
   configFile := GetUserDir+'.local/share/morserunner-server/morserunner-server.ini';
   ForceDirectories(GetUserDir+'.local/share/morserunner-server');
   {$ENDIF}

  with TIniFile.Create(configFile) do
    try
      WriteInteger(SEC_SYS, 'SerialPollTime', SerialPollTime);
      WriteInteger(SEC_STN, 'Pitch', MainForm.ComboBox1.ItemIndex);
      WriteInteger(SEC_STN, 'BandWidth', MainForm.ComboBox2.ItemIndex);
      WriteInteger(SEC_STN, 'Wpm', Wpm);
      WriteBool(SEC_STN, 'Qsk', Qsk);
      WriteInteger(SEC_STN, 'SelfMonVolume', Round(-80 + MainForm.TrackBar.position / 2.5));
      case serialMode of
        modeWinkey: WriteInteger(SEC_TST, 'serialMode', 0);
        modeSo2rmini: WriteInteger(SEC_TST, 'serialMode', 1);
      end;
      WriteString(SEC_STN, 'serialPortWinkey', serialPortWinkey);
      WriteString(SEC_STN, 'serialPortOtrsp', serialPortOtrsp);
      WriteString(SEC_STN, 'serialPortSo2rmini', serialPortSo2rmini);
      WriteInteger(SEC_BND, 'Activity', Activity);
      case BackgroundMode of
        none: WriteInteger(SEC_BND, 'BackgroundMode', 0);
        randomNoise: WriteInteger(SEC_BND, 'BackgroundMode', 1);
        randomQrn: WriteInteger(SEC_BND, 'BackgroundMode', 2);
        wavNoise: WriteInteger(SEC_BND, 'BackgroundMode', 3);
      end;
      WriteInteger(SEC_BND, 'WavVolume', wavVolume);

      WriteBool(SEC_BND, 'Qrm', Qrm);
      WriteBool(SEC_BND, 'Qsb', Qsb);
      WriteBool(SEC_BND, 'Flutter', Flutter);
      WriteBool(SEC_BND, 'Lids', Lids);
      case opMode of
        opWPX: WriteInteger(SEC_TST, 'mode', 0);
        opCab: WriteInteger(SEC_TST, 'mode', 1);
      end;
      WriteString(SEC_TST, 'CabrilloFile', CabFile);
      WriteString(SEC_TST, 'WavFile', wavFile);

    finally
      Free;
    end;
end;

end.
