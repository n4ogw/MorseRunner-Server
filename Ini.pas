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
  DEFAULTBUFSIZE = 512;
  DEFAULTRATE = 11025;


type
  TRunMode = (rmStop, rmRun);
  TOpMode  = (opWpx, opCab);
   
var
  Wpm	     : integer = 30;
  BandWidth  : integer = 500;
  Pitch	     : integer = 500;
  Qsk	     : boolean = False;
  Rit	     : integer = 0;
  BufSize    : integer = DEFAULTBUFSIZE;
  Activity   : integer = 2;
  Qrn	     : boolean = True;
  Qrm	     : boolean = True;
  Qsb	     : boolean = True;
  Flutter    : boolean = True;
  Lids	     : boolean = True;
  OpMode     : TOpMode = opWpx;
  RunMode    : TRunMode = rmStop;
  serialPort : string = '/tmp/pty1';
  cabFile    : string = '';

procedure FromIni;
procedure ToIni;

implementation

uses
  Main, Contest;

procedure FromIni;
var
  V: integer;
begin
   with TIniFile.Create(GetUserDir+'.local/share/morserunner-server/morserunner-server.ini') do
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
      MainForm.setPort(ReadString(SEC_STN, 'serialPort', '/tmp/pty1'));
   
      Activity := ReadInteger(SEC_BND, 'Activity', Activity);
      MainForm.SpinEdit3.Value := Activity;

      MainForm.CheckBox4.Checked := ReadBool(SEC_BND, 'Qrn', Qrn);
      MainForm.CheckBox3.Checked := ReadBool(SEC_BND, 'Qrm', Qrm);
      MainForm.CheckBox2.Checked := ReadBool(SEC_BND, 'Qsb', Qsb);
      MainForm.CheckBox5.Checked := ReadBool(SEC_BND, 'Flutter', Flutter);
      MainForm.CheckBox6.Checked := ReadBool(SEC_BND, 'Lids', Lids);
      MainForm.ReadCheckBoxes;

      //buffer size
      V := ReadInteger(SEC_SYS, 'BufSize', 0);
      if V = 0 then
      begin
        V := 3;
        WriteInteger(SEC_SYS, 'BufSize', V);
      end;
      V := Max(1, Min(5, V));
      BufSize := 64 shl V;
      Tst[1].Filt.SamplesInInput := BufSize;
      Tst[1].Filt2.SamplesInInput := BufSize;
      Tst[2].Filt.SamplesInInput := BufSize;
      Tst[2].Filt2.SamplesInInput := BufSize;

      cabFile := ReadString(SEC_TST, 'CabrilloFile', '');
      MainForm.setCabrillo(cabFile);
      case ReadInteger(SEC_TST, 'Mode', 0) of
	0 : opMode := opWPX;
	1 : opMode := opCab;
      end;
      MainForm.SetMode(opMode);

    finally
      Free;
    end;
end;


procedure ToIni;
var
  V: integer;
begin
   ForceDirectories(GetUserDir+'.local/share/morserunner-server');
   with TIniFile.Create(GetUserDir+'.local/share/morserunner-server/morserunner-server.ini') do
    try
      WriteInteger(SEC_STN, 'Pitch', MainForm.ComboBox1.ItemIndex);
      WriteInteger(SEC_STN, 'BandWidth', MainForm.ComboBox2.ItemIndex);
      WriteInteger(SEC_STN, 'Wpm', Wpm);
      WriteBool(SEC_STN, 'Qsk', Qsk);
      WriteInteger(SEC_STN, 'SelfMonVolume', Round(-70 + MainForm.TrackBar.position * 8));
      WriteString(SEC_STN, 'serialPort', serialPort);
      WriteInteger(SEC_BND, 'Activity', Activity);
      WriteBool(SEC_BND, 'Qrn', Qrn);
      WriteBool(SEC_BND, 'Qrm', Qrm);
      WriteBool(SEC_BND, 'Qsb', Qsb);
      WriteBool(SEC_BND, 'Flutter', Flutter);
      WriteBool(SEC_BND, 'Lids', Lids);
      WriteString(SEC_TST,'CabrilloFile', CabFile);
      case opMode of
	opWPX : WriteInteger(SEC_TST, 'mode', 0);
	opCab : WriteInteger(SEC_TST, 'mode', 1);
      end;

    finally
      Free;
    end;
end;

end.
