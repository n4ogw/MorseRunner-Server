//------------------------------------------------------------------------------
//This Source Code Form is subject to the terms of the Mozilla Public
//License, v. 2.0. If a copy of the MPL was not distributed with this
//file, You can obtain one at http://mozilla.org/MPL/2.0/.
//------------------------------------------------------------------------------
unit Main;

{$MODE Delphi}

interface

uses
  {$IFDEF Windows}
  LazFileUtils,
  {$ENDIF}
  // LazLogger,  // add to use DebugLn on windows
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Buttons,
  Contest, Ini, MorseKey, CallLst, StdCtrls, Station, Menus, ExtCtrls, Math, ComCtrls,
  Spin, Cabrillo, Setup, SndOut, SndCustm, WavFile, Serial;

type

  { TMainForm }

  TMainForm = class(TForm)
    AlSoundOut1: TAlSoundOut;
    BackgroundMode1: TMenuItem;
    ComboBox3: TComboBox;
    Devices1: TMenuItem;
    OpenDialog1: TOpenDialog;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    MenuCabrillo: TMenuItem;
    MenuWAV: TMenuItem;
    qrnMode0: TMenuItem;
    qrnMode1: TMenuItem;
    qrnMode2: TMenuItem;
    qrnMode3: TMenuItem;
    startButton: TToggleBox;
    TrackBar: TTrackBar;
    TrackBar1: TTrackBar;
    N1: TMenuItem;
    Bevel1: TBevel;
    Help1: TMenuItem;
    Readme1: TMenuItem;
    About1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Panel6: TPanel;
    N4: TMenuItem;
    N5: TMenuItem;
    Panel9: TPanel;
    GroupBox3: TGroupBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    SpinEdit1: TSpinEdit;
    SpinEdit3: TSpinEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    Label17: TLabel;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Settings1: TMenuItem;
    QSK1: TMenuItem;
    CWSpeed1: TMenuItem;
    N10WPM1: TMenuItem;
    N15WPM1: TMenuItem;
    N20WPM1: TMenuItem;
    N25WPM1: TMenuItem;
    N30WPM1: TMenuItem;
    N35WPM1: TMenuItem;
    N40WPM1: TMenuItem;
    N45WPM1: TMenuItem;
    N50WPM1: TMenuItem;
    N55WPM1: TMenuItem;
    N60WPM1: TMenuItem;
    CWBandwidth1: TMenuItem;
    CWBandwidth2: TMenuItem;
    N300Hz1: TMenuItem;
    N350Hz1: TMenuItem;
    N400Hz1: TMenuItem;
    N450Hz1: TMenuItem;
    N500Hz1: TMenuItem;
    N550Hz1: TMenuItem;
    N600Hz1: TMenuItem;
    N650Hz1: TMenuItem;
    N700Hz1: TMenuItem;
    N750Hz1: TMenuItem;
    N800Hz1: TMenuItem;
    N850Hz1: TMenuItem;
    N900Hz1: TMenuItem;
    N100Hz1: TMenuItem;
    N150Hz1: TMenuItem;
    N200Hz1: TMenuItem;
    N250Hz1: TMenuItem;
    N300Hz2: TMenuItem;
    N350Hz2: TMenuItem;
    N400Hz2: TMenuItem;
    N450Hz2: TMenuItem;
    N500Hz2: TMenuItem;
    N550Hz2: TMenuItem;
    N600Hz2: TMenuItem;
    MonLevel1: TMenuItem;
    N30dB1: TMenuItem;
    N20dB1: TMenuItem;
    N10dB1: TMenuItem;
    N0dB1: TMenuItem;
    N10dB2: TMenuItem;
    N6: TMenuItem;
    QRM1: TMenuItem;
    QSB1: TMenuItem;
    Flutter1: TMenuItem;
    LIDS1: TMenuItem;
    Activity1: TMenuItem;
    N11: TMenuItem;
    N21: TMenuItem;
    N31: TMenuItem;
    N41: TMenuItem;
    N51: TMenuItem;
    N61: TMenuItem;
    N71: TMenuItem;
    N81: TMenuItem;
    N91: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    Panel11: TPanel;

    procedure ComboBox3Change(Sender: TObject);
    procedure ShowSetup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AlSoundOut1BufAvailable(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure CabrilloClick(Sender: TObject);
    procedure WAVClick(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpinEdit1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
    procedure SpinEdit3Change(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Readme1Click(Sender: TObject);
    procedure startButtonChange(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure QSK1Click(Sender: TObject);
    procedure NWPMClick(Sender: TObject);
    procedure Pitch1Click(Sender: TObject);
    procedure Bw1Click(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure SelfMonClick(Sender: TObject);
    procedure backgroundClick(Sender: TObject);
    procedure WavLevelClick(Sender: TObject);
    procedure SetBackground(n: integer);
    procedure Settings1Click(Sender: TObject);
    procedure LIDS1Click(Sender: TObject);
    procedure Activity1Click(Sender: TObject);
    procedure setCabrillo(fileName : string; showMsg: boolean);
    procedure setWav(fileName : string; showMsg: boolean);
    procedure SetSerialMode(mode: TSerialMode);

  private
    procedure EnableCtl(Ctl: TWinControl; AEnable: boolean);
    procedure IncRit(dF: integer);
    procedure UpdateRitIndicator;
    procedure DecSpeed;
    procedure IncSpeed;

  public
    procedure Run(Value: TRunMode);
    procedure SetQsk(Value: boolean);
    procedure SetPitch(PitchNo: integer);
    procedure SetBw(BwNo: integer);
    procedure ReadCheckboxes;

{$IFDEF Windows}
    procedure CustomExceptionHandler(Sender: TObject; E: Exception);
{$endif}
  end;

var
  MainForm: TMainForm;
  timer1: TTimer;

implementation

{$R *.lfm}

{$ifdef Windows}
procedure TMainForm.CustomExceptionHandler(Sender: TObject; E: Exception);
begin
     // override default exception handler, since error messages cause problem
     // in win32 app with no console
end;

{$endif}

procedure TMainForm.FormCreate(Sender: TObject);
begin
{$ifdef Windows}   
  Application.OnException := CustomExceptionHandler;
{$endif}
  Randomize;
  Tst[1] := TContest.Create(1);
{$ifdef Linux}   
  Tst[1].Filt.SamplesInInput := AlSoundOut1.Samples;
  Tst[1].Filt2.SamplesInInput := AlSoundOut1.Samples;
{$endif}
  Tst[2] := TContest.Create(2);
{$ifdef Linux}   
  Tst[2].Filt.SamplesInInput := AlSoundOut1.Samples;
  Tst[2].Filt2.SamplesInInput := AlSoundOut1.Samples;
{$endif}
  Cab := TCabrillo.Create;
  Cab.SetNExch(2);
  LoadCallList;
  AlSoundOut1.BufCount := 4;
  AlSoundOut1.SetChannel(0);
  MakeKeyer;
  Keyer.Rate := DEFAULTRATE;
  Keyer.BufSize := Ini.BufSize;
  Wav := TWav.Create();
  Ser := TSerial.Create();
  FromIni;
  timer1 := TTimer.Create(Self);
  timer1.Interval := Ini.SerialPollTime;
  timer1.OnTimer := Ser.checkSerial;
  timer1.Enabled := True;
end;

procedure TMainForm.ShowSetup(Sender: TObject);
begin
  Setup.serialModeSave := Ini.serialMode;
  Setup.serialPortWinkeySave := Ini.serialPortWinkey;
  Setup.serialPortOtrspSave := Ini.serialPortOtrsp;
  Setup.serialPortSo2rminiSave := Ini.serialPortSo2rmini;
  Form1.Edit1.Text := Ini.serialPortWinkey;
  Form1.Edit2.Text := Ini.serialPortOtrsp;
  Form1.Edit3.Text := Ini.serialPortSo2rmini;
  Form1.position := poMainFormCenter;
  SetSerialMode(serialMode);

  Setup.modeSave := Ini.OpMode;
  if Ini.OpMode = opWpx then
  begin
    Form1.CheckGroup2.Checked[0] := True;
    Form1.CheckGroup2.Checked[1] := False;
  end
  else
  begin
    Form1.CheckGroup2.Checked[0] := False;
    Form1.CheckGroup2.Checked[1] := True;
  end;
  Form1.ShowModal;
end;

procedure TMainForm.ComboBox3Change(Sender: TObject);
begin
  case ComboBox3.ItemIndex of
    0: Ini.BackgroundMode := none;
    1: Ini.BackgroundMode := randomNoise;
    2: Ini.BackgroundMode := randomQrn;
    3: Ini.BackgroundMode := wavNoise;
  end;
  if Ini.BackgroundMode = wavNoise then
  begin
    ComboBox2.Enabled := False;
    CheckBox1.Enabled := False;
  end
  else
  begin
    ComboBox2.Enabled := True;
    CheckBox1.Enabled := True;
  end;
end;

procedure TMainForm.SetSerialMode(mode: TSerialMode);
begin
  if mode = modeWinkey then
  begin
    Form1.Edit1.Enabled := True;
    Form1.Edit2.Enabled := True;
    Form1.Edit3.Enabled := False;
    Form1.CheckGroup1.Checked[0] := True;
    Form1.CheckGroup1.Checked[1] := False;
    Form1.uELED3.Color := clSilver;
    if Ser.serialStatus then
      Form1.uELED1.Color := clGreen
    else
      Form1.uELED1.Color := clRed;
    if Ser.serialStatusOtrsp then
      Form1.uELED2.Color := clGreen
    else
      Form1.uELED2.Color := clRed;
  end
  else
  begin
    Form1.Edit1.Enabled := False;
    Form1.Edit2.Enabled := False;
    Form1.Edit3.Enabled := True;
    Form1.CheckGroup1.Checked[0] := False;
    Form1.CheckGroup1.Checked[1] := True;
    Form1.uELED1.Color := clSilver;
    Form1.uELED2.Color := clSilver;
    if Ser.serialStatus then
      Form1.uELED3.Color := clGreen
    else
      Form1.uELED3.Color := clRed;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Wav.Free;
  Cab.Free;
  Ser.Free;
  ToIni;
  Tst[1].Free;
  Tst[2].Free;
  DestroyKeyer;
end;


procedure TMainForm.AlSoundOut1BufAvailable(Sender: TObject);
begin
  if AlSoundOut1.Enabled then
  begin
    try
      AlSoundOut1.PutData(Tst[1].GetAudio(1), Tst[2].GetAudio(2));
    except
    end;
  end;
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  // key handling disabled
  { 
  case Key of
    #27: //Esc = Abort send
    begin
      Tst[1].Me.AbortSend;
      Tst[2].Me.AbortSend;
    end;

    else
      Exit;
  end;
  }
  Key := #0;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  // key handling disabled
  { 
  case Key of
    VK_UP: ;
    // RIT disabled
    //if GetKeyState(VK_CONTROL) >= 0 then IncRit(1);

    VK_DOWN: ;
    // RIT disabled
    //if GetKeyState(VK_CONTROL) >= 0 then IncRit(-1);

    VK_PRIOR: //PgUp
      IncSpeed;

    VK_NEXT: //PgDn
      DecSpeed;
    else
      Exit;
  end;
  }
  Key := 0;
end;


procedure TMainForm.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_INSERT, VK_RETURN: Key := 0;
  end;
end;

procedure TMainForm.setCabrillo(fileName : string; showMsg: boolean);
begin
  Cab.LoadFile(fileName, showMsg);
end;

procedure TMainForm.setWav(fileName : string; showMsg: boolean);
begin
  Wav.LoadFile(fileName, showMsg);
end;

procedure TMainForm.WAVClick(Sender: TObject);
begin
  OpenDialog1.Filter := '*.wav; *.WAV';
  OpenDialog1.Title := 'Select WAV file';
  if OpenDialog1.Execute then
  begin
    if fileExists(OpenDialog1.Filename) then
    begin
      Ini.wavFile := OpenDialog1.Filename;
    end;
  end
  else
  begin
    {$ifdef Linux}
    WriteLn('No file selected');
    {$endif}
    Exit;
  end;
  setWav(Ini.wavFile, True);
end;

procedure TMainForm.CabrilloClick(Sender: TObject);
begin
  OpenDialog1.Filter := '*; *.*';
  OpenDialog1.Title := 'Select Cabrillo File';
  if OpenDialog1.Execute then
  begin
    if fileExists(OpenDialog1.Filename) then
    begin
      Ini.cabFile := OpenDialog1.Filename;
    end;
  end
  else
  begin
    {$ifdef Linux}
     WriteLn('No file selected');
    {$endif}
    Exit;
  end;
  Cab.LoadFile(Ini.cabFile, True);
end;

procedure TMainForm.IncSpeed;
begin
  Wpm := Trunc(Wpm / 5) * 5 + 5;
  Wpm := Max(10, Min(120, Wpm));
  SpinEdit1.Value := Wpm;
  Tst[1].Me.Wpm := Wpm;
  Tst[2].Me.Wpm := Wpm;
end;


procedure TMainForm.DecSpeed;
begin
  Wpm := Ceil(Wpm / 5) * 5 - 5;
  Wpm := Max(10, Min(120, Wpm));
  SpinEdit1.Value := Wpm;
  Tst[1].Me.Wpm := Wpm;
  Tst[2].Me.Wpm := Wpm;
end;


procedure TMainForm.SetPitch(PitchNo: integer);
begin
  Ini.Pitch := 300 + PitchNo * 50;
  ComboBox1.ItemIndex := PitchNo;
  Tst[1].Modul.CarrierFreq := Ini.Pitch;
  Tst[2].Modul.CarrierFreq := Ini.Pitch;
end;


procedure TMainForm.SetBw(BwNo: integer);
var
  i: integer;
begin
  if (BwNo < 0) or (BwNo >= ComboBox2.Items.Count) then Exit;

  Ini.Bandwidth := 100 + BwNo * 50;
  ComboBox2.ItemIndex := BwNo;

  for i := 1 to 2 do
  begin
    Tst[i].Filt.Points := Round(0.7 * DEFAULTRATE / Ini.BandWidth);
    Tst[i].Filt.GainDb := 10 * Log10(500 / Ini.Bandwidth);
    Tst[i].Filt2.Points := Tst[i].Filt.Points;
    Tst[i].Filt2.GainDb := Tst[i].Filt.GainDb;
  end;

  UpdateRitIndicator;
end;

procedure TMainForm.ComboBox2Change(Sender: TObject);
begin
  SetBw(ComboBox2.ItemIndex);
end;

procedure TMainForm.ComboBox1Change(Sender: TObject);
begin
  SetPitch(ComboBox1.ItemIndex);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AlSoundOut1.Enabled := False;
end;

procedure TMainForm.SpinEdit1Change(Sender: TObject);
begin
  Ini.Wpm := SpinEdit1.Value;
  Tst[1].Me.Wpm := Ini.Wpm;
  Tst[2].Me.Wpm := Ini.Wpm;
end;

procedure TMainForm.CheckBox1Click(Sender: TObject);
begin
  SetQsk(CheckBox1.Checked);
end;

procedure TMainForm.CheckBoxClick(Sender: TObject);
begin
  ReadCheckboxes;
end;


procedure TMainForm.ReadCheckboxes;
begin
  Ini.Qrm := CheckBox3.Checked;
  Ini.Qsb := CheckBox2.Checked;
  Ini.Flutter := CheckBox5.Checked;
  Ini.Lids := CheckBox6.Checked;
end;

procedure TMainForm.SpinEdit3Change(Sender: TObject);
begin
  Ini.Activity := SpinEdit3.Value;
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.About1Click(Sender: TObject);
const
  Msg = 'CW CONTEST SIMULATOR'#13#13 +
    'Copyright (c) 2023 Torsten Clay, N4OGW rt_clay@bellsouth.net'#13#13 +
    'Portions Copyright (c) 2004-2006 Alex Shovkoplyas, VE3NEA'#13#13 +
    'Portions Copyright (c) 2022 Zach Metzinger, N0ZGO'#13;
begin
  Application.MessageBox(Msg, 'Morse Runner Server 1.12', MB_OK or MB_ICONINFORMATION);
end;


procedure TMainForm.Readme1Click(Sender: TObject);
var
  FileName: string;
begin
   {$IFDEF Linux}
   FileName := '/usr/local/share/morserunner-server/Readme.txt';
   {$Endif}
   {$IFDEF Windows}
   FileName := AppendPathDelim(ExtractFileDir(Application.ExeName))+'Readme.txt';
   {$Endif}
  OpenDocument(PChar(FileName));
end;

procedure TMainForm.EnableCtl(Ctl: TWinControl; AEnable: boolean);
const
  Clr: array[boolean] of TColor = (clBtnFace, clWindow);
begin
  Ctl.Enabled := AEnable;
  if Ctl is TSpinEdit then (Ctl as TSpinEdit).Color := Clr[AEnable]
  else if Ctl is TEdit then (Ctl as TEdit).Color := Clr[AEnable];
end;


procedure TMainForm.Run(Value: TRunMode);
var
  BStop	: boolean;
  i	: integer;
   s	: TStation;
begin
  if Value = RunMode then Exit;

  BStop := Value = rmStop;
  RunMode := Value;

  if not BStop then
  begin
    for i := 1 to 2 do
    begin
      Tst[i].Me.AbortSend;
      Tst[i].BlockNumber := 0;
      Tst[i].Me.Nr := 1;
    end;
  end;

  if not BStop then IncRit(0);

  AlSoundOut1.Enabled := not BStop;
  // remove all callers. Required in case contest is changed
  // when restarted
  if BStop then
     begin
	Tst[1].Stations.Clear;
	Tst[2].Stations.Clear;
     end;
end;


procedure TMainForm.startButtonChange(Sender: TObject);
begin
  if startButton.Checked = True then
  begin
    if RunMode = rmStop then
    begin
       // if cabrillo or wav modes, check to see if files loaded
       if ( (Ini.OpMode = opCab) and (not Cab.cabrilloLoaded)) then
       begin
	  showMessage('ERROR: Cabrillo file not loaded. See' + LineEnding +
		      'File->Load Cabrillo');
	  startButton.Checked := False;
	  exit;
       end;
       if ( (Ini.BackgroundMode = wavNoise) and (not Wav.wavLoaded)) then
       begin
	  showMessage('ERROR: WAV file not loaded. See' + LineEnding +
		      'File->Load WAV');
	  startButton.Checked := False;
	  exit;
       end;
       Run(rmRun);
    end;
  end
  else
  begin
    Tst[1].FStopPressed := True;
    Tst[2].FStopPressed := True;
  end;
end;

procedure TMainForm.TrackBarChange(Sender: TObject);
var
  Db: double;
begin
  with Trackbar do
  begin
    Ini.MonVol := Power(10, (-80 + position / 2.5) / 20.0);
    //-80..-40 dB
    Db := -80 + position / 2.5;
    if dB > 0 then Hint := Format('+%.0f dB', [dB])
    else
      Hint := Format('%.0f dB', [dB]);
  end;
end;

procedure TMainForm.TrackBar1Change(Sender: TObject);
begin
  Ini.wavVolume := TrackBar1.position;
  WavVol := 1000 + Power(7, Ini.wavVolume / 10);
end;

procedure TMainForm.IncRit(dF: integer);
begin
  case dF of
    -1: Inc(Ini.Rit, -50);
    0: Ini.Rit := 0;
    1: Inc(Ini.Rit, 50);
  end;

  Ini.Rit := Min(500, Max(-500, Ini.Rit));
  UpdateRitIndicator;
end;


procedure TMainForm.UpdateRitIndicator;
begin

end;

procedure TMainForm.SetQsk(Value: boolean);
begin
  Qsk := Value;
  CheckBox1.Checked := Qsk;
end;


procedure TMainForm.QSK1Click(Sender: TObject);
begin
  SetQsk(not QSK1.Checked);
end;


procedure TMainForm.NWPMClick(Sender: TObject);
begin
  Wpm := (Sender as TMenuItem).Tag;
  Wpm := Max(10, Min(120, Wpm));
  SpinEdit1.Value := Wpm;
  Tst[1].Me.Wpm := Wpm;
  Tst[2].Me.Wpm := Wpm;
end;



procedure TMainForm.Pitch1Click(Sender: TObject);
begin
  SetPitch((Sender as TMenuItem).Tag);

end;

procedure TMainForm.Bw1Click(Sender: TObject);
begin
  SetBw((Sender as TMenuItem).Tag);
end;

procedure TMainForm.File1Click(Sender: TObject);
var
  Stp: boolean;
begin
  Stp := RunMode = rmStop;
end;

procedure TMainForm.SelfMonClick(Sender: TObject);
begin
  MainForm.TrackBar.position := Round(((Sender as TMenuItem).Tag + 60) / 8);
  TrackBarChange(Sender);
end;

procedure TMainForm.backgroundClick(Sender: TObject);
begin
  SetBackground((Sender as TMenuItem).Tag);
end;

procedure TMainForm.WavLevelClick(Sender: TObject);
begin
  Ini.wavVolume := (Sender as TMenuItem).Tag;
  TrackBar1.position := Ini.wavVolume;
  WavVol := 1000 + Power(7, Ini.wavVolume / 10);
end;


procedure TMainForm.SetBackground(n: integer);
begin
  if (n < 0) or (n > 3) then Exit;
  case n of
    0: Ini.BackgroundMode := none;
    1: Ini.BackgroundMode := randomNoise;
    2: Ini.BackgroundMode := randomQrn;
    3: Ini.BackgroundMode := wavNoise;
  end;
  Combobox3.ItemIndex := n;
  if Ini.BackgroundMode = wavNoise then
  begin
    ComboBox2.Enabled := False;
    CheckBox1.Enabled := False;
  end
  else
  begin
    ComboBox2.Enabled := True;
    CheckBox1.Enabled := True;
  end;
end;


procedure TMainForm.Settings1Click(Sender: TObject);
begin
  QSK1.Checked := Ini.Qsk;
  case Ini.BackgroundMode of
    none: ComboBox3.ItemIndex := 0;
    randomNoise: ComboBox3.ItemIndex := 1;
    randomQrn: ComboBox3.ItemIndex := 2;
    wavNoise: ComboBox3.ItemIndex := 3;
  end;
  QRM1.Checked := Ini.Qrm;
  QSB1.Checked := Ini.Qsb;
  Flutter1.Checked := Ini.Flutter;
  LIDS1.Checked := Ini.Lids;
end;

//ALL checkboxes
procedure TMainForm.LIDS1Click(Sender: TObject);
begin
  with Sender as TMenuItem do Checked := not Checked;

  CheckBox3.Checked := QRM1.Checked;
  CheckBox2.Checked := QSB1.Checked;
  CheckBox5.Checked := Flutter1.Checked;
  CheckBox6.Checked := LIDS1.Checked;

  ReadCheckboxes;
end;

procedure TMainForm.Activity1Click(Sender: TObject);
begin
  Ini.Activity := (Sender as TMenuItem).Tag;
  SpinEdit3.Value := Ini.Activity;
end;


end.
