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
  TRunMode    = (rmStop, rmRun);
  TOpMode     = (opWpx, opCab);
  TSerialMode =  (modeWinkey, modeSo2rmini);
var
  Wpm		  : integer = 30;
  BandWidth	  : integer = 500;
  Pitch		  : integer = 500;
  Qsk		  : boolean = False;
  Rit		  : integer = 0;
  BufSize	  : integer = DEFAULTBUFSIZE;
  Activity	  : integer = 2;
  SerialPollTime : integer = DEFAULTPOLLTIME;
  Qrn		  : boolean = True;
  Qrm		  : boolean = True;
  Qsb		  : boolean = True;
  Flutter	  : boolean = True;
  Lids		  : boolean = True;
  OpMode	  : TOpMode = opWpx;
  RunMode	  : TRunMode = rmStop;
   serialMode	  : TSerialMode = modeWinkey;
  {$IFDEF Linux}
  serialPortWinkey  : string = '/tmp/pty1a';
  serialPortOtrsp  : string = '/tmp/pty2a';
  serialPortSo2rmini  : string = '/tmp/pty3a';
  {$ENDIF} 
  {$IFDEF Windows}
  serialPortWinkey  : string = 'COM12';
  serialPortOtrsp  : string = 'COM14';
  serialPortSo2rmini  : string = 'COM16';
  {$ENDIF} 
  cabFile     : string = '';

procedure FromIni;
procedure ToIni;

implementation

uses
  Main, Setup, Contest;

procedure FromIni;
var
  V	      : integer;
  configFile  :  string;
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
      V := ReadInteger(SEC_STN, 'SelfMonVolume', -44);
      MainForm.TrackBar.position := Round((V + 70) / 8);
      case ReadInteger(SEC_TST, 'SerialMode', 0) of
	0 : serialMode := modeWinkey;
	1 : serialMode := modeSo2rmini;
      end;
   {$IFDEF Linux}
      serialPortWinkey := ReadString(SEC_STN, 'serialPortWinkey', '/tmp/pty1a');
      serialPortOTRSP := ReadString(SEC_STN, 'serialPortOtrsp', '/tmp/pty2a');
      serialPortSo2rmini := ReadString(SEC_STN, 'serialPortSo2rmini', '/tmp/pty3a');
   {$ENDIF}
   {$IFDEF Windows}
      serialPortWInkey := ReadString(SEC_STN, 'serialPortWinkey', 'COM12');
      serialPortOTRSP := ReadString(SEC_STN, 'serialPortOtrsp', 'COM14');
      serialPortSo2rmini := ReadString(SEC_STN, 'serialPortSo2rmini', 'COM16');
   {$ENDIF}

      MainForm.serialInit;
   
      Activity := ReadInteger(SEC_BND, 'Activity', Activity);
      MainForm.SpinEdit3.Value := Activity;

      MainForm.CheckBox4.Checked := ReadBool(SEC_BND, 'Qrn', Qrn);
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
	0 :
        begin
          opMode := opWPX;
        end;
        1 :
        begin
          opMode := opCab;
        end;
      end;
      cabFile := ReadString(SEC_TST, 'CabrilloFile', '');
      MainForm.setCabrillo(cabFile);
    finally
      Free;
    end;
end;


procedure ToIni;
var
   configFile :  string;
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
      WriteInteger(SEC_STN, 'SelfMonVolume', Round(-70 + MainForm.TrackBar.position * 8));
      case serialMode of
	modeWinkey : WriteInteger(SEC_TST, 'serialMode', 0);
	modeSo2rmini : WriteInteger(SEC_TST, 'serialMode', 1);
      end;
      WriteString(SEC_STN, 'serialPortWinkey', serialPortWinkey);
      WriteString(SEC_STN, 'serialPortOtrsp', serialPortOtrsp);
      WriteString(SEC_STN, 'serialPortSo2rmini', serialPortSo2rmini);
      WriteInteger(SEC_BND, 'Activity', Activity);
      WriteBool(SEC_BND, 'Qrn', Qrn);
      WriteBool(SEC_BND, 'Qrm', Qrm);
      WriteBool(SEC_BND, 'Qsb', Qsb);
      WriteBool(SEC_BND, 'Flutter', Flutter);
      WriteBool(SEC_BND, 'Lids', Lids);
      case opMode of
	opWPX : WriteInteger(SEC_TST, 'mode', 0);
	opCab : WriteInteger(SEC_TST, 'mode', 1);
      end;
      WriteString(SEC_TST,'CabrilloFile', CabFile);

    finally
      Free;
    end;
end;

end.
