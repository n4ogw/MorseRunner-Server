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
//  LazLogger,  // add to use DebugLn on windows
  LCLIntf, LCLType, LMessages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, Contest, Ini, MorseKey, CallLst,
  StdCtrls, Station, Menus, ExtCtrls, Log, Math, ComCtrls, Spin, ImgList,
  synaser, Cabrillo, StrUtils, Setup, SndOut, SndCustm;

const
  WM_TBDOWN = WM_USER + 1;

type

  { TMainForm }

  TMainForm = class(TForm)
    AlSoundOut1: TAlSoundOut;
    Label4: TLabel;
    OpenDialog1: TOpenDialog;
    Label17: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    MenuCabrillo: TMenuItem;
    startButton: TToggleBox;
    TrackBar: TTrackBar;
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
    Label11: TLabel;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    SpinEdit3: TSpinEdit;
    GroupBox1: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    SpinEdit1: TSpinEdit;
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
    QRN1: TMenuItem;
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

    procedure CheckBox7Change(Sender: TObject);
    procedure ShowSetup(Sender :  TObject);
    procedure FormCreate(Sender: TObject);
    procedure AlSoundOut1BufAvailable(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure CabrilloClick(Sender: TObject);
    procedure GroupBox1ChangeBounds(Sender: TObject);
    procedure GroupBox1Click(Sender: TObject);
    procedure GroupBox1DragOver(Sender, Source: TObject; X, Y: integer;
      State: TDragState; var Accept: boolean);
    procedure GroupBox1EndDrag(Sender, Target: TObject; X, Y: integer);
    procedure GroupBox1Enter(Sender: TObject);
    procedure GroupBox1Exit(Sender: TObject);
    procedure GroupBox2Click(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure PortClick(Sender: TObject);
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
    procedure TrackBarClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure QSK1Click(Sender: TObject);
    procedure NWPMClick(Sender: TObject);
    procedure Pitch1Click(Sender: TObject);
    procedure Bw1Click(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure SelfMonClick(Sender: TObject);
    procedure Settings1Click(Sender: TObject);
    procedure LIDS1Click(Sender: TObject);
    procedure Activity1Click(Sender: TObject);
    procedure setCabrillo(fileName : string);
    procedure serialInit;
    procedure SetSerialMode(mode :  TSerialMode);

  private
    procedure EnableCtl(Ctl: TWinControl; AEnable: boolean);
    procedure IncRit(dF: integer);
    procedure UpdateRitIndicator;
    procedure DecSpeed;
    procedure IncSpeed;
    procedure otrsp;
    procedure so2rmini();
    procedure winkey;


  public
    CompetitionMode: boolean;
    procedure Run(Value: TRunMode);

    procedure SetQsk(Value: boolean);
    procedure SetPitch(PitchNo: integer);
    procedure SetBw(BwNo: integer);
    procedure ReadCheckboxes;
    procedure checkSerial(sender : TObject) ;
    procedure sendCW;
{$IFDEF Windows}
    procedure CustomExceptionHandler(Sender: TObject; E: Exception);
{$endif}
  end;

var
        MainForm	 : TMainForm;
	serialBuffer	 : array [0..127] of byte;
   serialOutBuffer	 : array [0..127] of byte;
   otrspCmd		 :  string;
        serialPtr	 : integer;
        serialOutPtr	 : integer;
	devicePtr	 : integer;
        deviceOutPtr	 : integer;
	so2rMiniCmd	 : byte;
        winkeyCmd	 : byte;
   winkeyCmdCnt		 : integer;
	serialStatus	 : boolean;
   serialStatusOtrsp	 : boolean;
	cwBuffer	 : array [1..2] of string;
	serialInput	 : TBlockSerial;
	serialInputOtrsp : TBlockSerial;
        stopSent	 : array [1..2] of boolean;
	radioNrTx	 : integer;
        timer1		 : TTimer;

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
  radioNrTx := 1;
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
  OpenDialog1.Title := 'Select Cabrillo file';
  Cab := TCabrillo.Create;
  Cab.SetNExch(2);
  LoadCallList;
  AlSoundOut1.BufCount := 4;
  AlSoundOut1.SetChannel(0);
  MakeKeyer;
  Keyer.Rate := DEFAULTRATE;
  Keyer.BufSize := Ini.BufSize;
 
  if serialInput = Nil then 
     serialInput := TBlockSerial.Create;
  serialInput.LinuxLock := false;
  if serialInputOtrsp = Nil then 
     serialInputOtrsp := TBlockSerial.Create;
  serialInputOtrsp.LinuxLock := false;
  Sleep(100);
  FromIni;
  timer1 := TTimer.create( Self );
  timer1.Interval := Ini.SerialPollTime;
  timer1.OnTimer := checkSerial;
  timer1.enabled := true;
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

procedure TMainForm.CheckBox7Change(Sender: TObject);
begin

end;

procedure TMainForm.SetSerialMode(mode :  TSerialMode);
begin
   if mode = modeWinkey then
      begin
	 Form1.Edit1.Enabled := True;
	 Form1.Edit2.Enabled := True;
	 Form1.Edit3.Enabled := False;
	 Form1.CheckGroup1.Checked[0] := True;
	 Form1.CheckGroup1.Checked[1] := False;
	 Form1.uELED3.Color := clSilver;
	 if serialStatus then
	    Form1.uELED1.Color := clGreen
	 else
	    Form1.uELED1.Color := clRed;
	 if serialStatusOtrsp then
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
	 if serialStatus then
	    Form1.uELED3.Color := clGreen
	 else
	    Form1.uELED3.Color := clRed;
      end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  serialInput.Free;
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
     AlSoundOut1.PutData(Tst[1].GetAudio,Tst[2].GetAudio);
    except
    end;
  end;
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
    #27: //Esc = Abort send
    begin
      if msgHisCall in Tst[1].Me.Msg then CallSent[1] := False;
      if msgHisCall in Tst[2].Me.Msg then CallSent[2] := False;
      if msgNR in Tst[1].Me.Msg then NrSent[1] := False;
      if msgNR in Tst[2].Me.Msg then NrSent[2] := False;
      Tst[1].Me.AbortSend;
      Tst[2].Me.AbortSend;
    end;

    else
      Exit;
  end;

  Key := #0;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_UP:;
      // RIT disabled
      //if GetKeyState(VK_CONTROL) >= 0 then IncRit(1);

    VK_DOWN:;
      // RIT disabled
      //if GetKeyState(VK_CONTROL) >= 0 then IncRit(-1);

    VK_PRIOR: //PgUp
      IncSpeed;

    VK_NEXT: //PgDn
      DecSpeed;
    else
      Exit;
  end;

  Key := 0;
end;


procedure TMainForm.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_INSERT, VK_RETURN: Key := 0;
  end;
end;

procedure TMainForm.GroupBox1ChangeBounds(Sender: TObject);
begin

end;

procedure TMainForm.setCabrillo(fileName :  string);
begin
   Cab.LoadFile(fileName);
end;

procedure TMainForm.CabrilloClick(Sender: TObject);
begin
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
   setCabrillo(Ini.cabFile);
end;

procedure TMainForm.GroupBox1Click(Sender: TObject);
begin

end;

procedure TMainForm.GroupBox1DragOver(Sender, Source: TObject;
  X, Y: integer; State: TDragState; var Accept: boolean);
begin

end;

procedure TMainForm.GroupBox1EndDrag(Sender, Target: TObject; X, Y: integer);
begin

end;

procedure TMainForm.GroupBox1Enter(Sender: TObject);
begin

end;

procedure TMainForm.GroupBox1Exit(Sender: TObject);
begin

end;

procedure TMainForm.GroupBox2Click(Sender: TObject);
begin

end;

procedure TMainForm.Label1Click(Sender: TObject);
begin

end;

procedure TMainForm.PortClick(Sender: TObject);
begin

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
   i :  integer;
begin
  if (BwNo < 0) or (BwNo >= ComboBox2.Items.Count) then Exit;

  Ini.Bandwidth := 100 + BwNo * 50;
  ComboBox2.ItemIndex := BwNo;

   for i:= 1 to 2 do
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
  Ini.Qrn := CheckBox4.Checked;
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
  Application.MessageBox(Msg, 'Morse Runner Server 1.1', MB_OK or MB_ICONINFORMATION);
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
   BStop : boolean;
   i     :  integer;
begin
  if Value = RunMode then Exit;

  BStop := Value = rmStop;
  RunMode := Value;

  if not BStop then
  begin
     for i:=1 to 2 do
     begin
	Tst[i].Me.AbortSend;
	Tst[i].BlockNumber := 0;
	Tst[i].Me.Nr := 1;
	CallSent[i] := false;
	NrSent[i] := false;
     end;
    Log.Clear;
  end;

  if not BStop then IncRit(0);
   
  AlSoundOut1.Enabled := not BStop;
end;


procedure TMainForm.startButtonChange(Sender: TObject);
begin
   if startButton.Checked = true then
      begin
	 if RunMode = rmStop then Run(rmRun)
      end
  else
     begin
	Tst[1].FStopPressed := true;
	Tst[2].FStopPressed := true;
     end;
end;

procedure TMainForm.TrackBarChange(Sender: TObject);
var
  Db: double;
begin
  with Trackbar do
  begin
    //-70..+10 dB
    Db := -70 + position * 8;
    if dB > 0 then Hint := Format('+%.0f dB', [dB])
    else
      Hint := Format('%.0f dB', [dB]);
  end;
end;

procedure TMainForm.TrackBarClick(Sender: TObject);
begin

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

procedure TMainForm.Settings1Click(Sender: TObject);
begin
  QSK1.Checked := Ini.Qsk;
  QRN1.Checked := Ini.Qrn;
  QRM1.Checked := Ini.Qrm;
  QSB1.Checked := Ini.Qsb;
  Flutter1.Checked := Ini.Flutter;
  LIDS1.Checked := Ini.Lids;
end;

//ALL checkboxes
procedure TMainForm.LIDS1Click(Sender: TObject);
begin
  with Sender as TMenuItem do Checked := not Checked;

  CheckBox4.Checked := QRN1.Checked;
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

// initialize serial devices. Two modes
// 1. winkey and otrsp (2 ports)
// 2. sor2mini (1 port)
procedure TMainForm.serialInit;
begin
  if serialInput.InstanceActive = True then
      serialInput.closeSocket;
  if serialInputOtrsp.InstanceActive = True then
      serialInputOtrsp.closeSocket;
   
   serialStatus := False;
   serialStatusOtrsp := False;
   
  if Ini.serialMode = modeSo2rmini then
     begin
	// So2rMini
	serialInput.LinuxLock := False;
        {$ifdef Linux}
	writeln('so2rmini connect to ',serialPortSo2rmini);
        {$endif}
	try
           serialInput.Connect(Ini.serialPortSo2rmini);
        except
              On E :Exception do begin
                 serialStatus := False;
                 ShowMessage(E.Message);
              end;
        end;
	if serialInput.lastError = 0 then
	begin
           serialInput.config(19200, 8, 'N', SB1, False, False);
           {$ifdef Linux}
	   WriteLn('so2rmini connected to ', Ini.serialPortSo2rmini);
           {$endif}
	   serialStatus := True;
	   serialInput.LinuxLock := False;
	end
        else
	begin
	   serialStatus := False;
	end;
     end
   else
     begin
	// winkey
	serialInput.LinuxLock := False;
        try
           serialInput.Connect(Ini.serialPortWinkey);
        except
              On E :Exception do begin
                 serialStatus := False;
		 ShowMessage(E.Message);
              end;
        end;
        if serialInput.lastError = 0 then
	begin
           serialInput.config(1200, 8, 'N', SB1, False, False);
           {$ifdef Linux}
           Writeln('winkey connected to ', Ini.serialPortWinkey);
           {$endif}
	   serialStatus := True;
	   serialInput.LinuxLock := False;
	end
        else
	begin
	   serialStatus := False;
	end;
	// otrsp
	serialInputOtrsp.LinuxLock := False;
        try
           serialInputOtrsp.Connect(Ini.serialPortOtrsp);
        except
              On E :Exception do begin
                 serialStatusOtrsp := False;
                 ShowMessage(E.Message);
              end;
        end;
        if serialInputOtrsp.lastError = 0 then
	begin
           serialInputOtrsp.config(9600, 8, 'N', SB1, False, False);
           {$ifdef Linux}
           Writeln('otrsp connected to ', Ini.serialPortOtrsp);
           {$endif}
	   serialStatusOtrsp := True;
	   serialInputOtrsp.LinuxLock := False;
	end
        else
	begin
	   serialStatusOtrsp := False;
	end;
     end;

  serialPtr := 0;
  serialOutPtr := 0;
  devicePtr := 0;
  deviceOutPtr := 0;
  cwBuffer[1] := '';
  cwBuffer[2] := '';
  so2rMiniCmd := 0;
  winkeyCmd := 0;
  winkeyCmdCnt := 0;
  stopSent[1] := True;
  stopSent[2] := True;
  otrspCmd := '';
end;

// 1. check for characters on received on device serial port (winkey or so2rmini)
// 2. check for characters to transmit on device serial port
// 3. process commands
procedure TMainForm.checkSerial(sender : TObject) ;
var
   b :  byte;
begin
  // read from winkey or so2rmini serial port into circular buffer
  while serialInput.CanReadEx(0) = True do
  begin
    serialBuffer[serialPtr] := serialInput.RecvByte(0);
    //debugln(char(serialBuffer[serialPtr]),' ',serialBuffer[serialPtr].toHexString);
    serialPtr := serialPtr + 1;
    serialPtr := serialPtr mod 128;
  end;

  // append chars to otrsp command until return is received
  while serialInputOtrsp.CanReadEx(0) = True do
  begin
    b :=  serialInputOtrsp.RecvByte(0);
    if b <> $0d then
       otrspCmd := otrspCmd + char(b)
    else
       otrsp;
  end;

  // send winkey or so2rmini output 
  while ( (serialOutPtr <> deviceOutPtr) and (serialInput.CanWrite(0) = true) ) do
  begin
      serialInput.SendByte(serialOutBuffer[deviceOutPtr]);
      deviceOutPtr := deviceOutPtr + 1;
      deviceOutPtr := deviceOutPtr mod 128;
  end;
   
  if Ini.serialMode = modeSo2rmini then
       so2rmini
     else
       winkey;

  if runMode = rmRun then
      sendCW;
end;

// send cw messages
procedure TMainForm.sendCW; 
var
  i,l		    : integer;
  i0, i1, i2, cmdNr : integer;
  firstPart, work   : string;
  const cmdSep : char = '+'; // character to mark commands
  const cmdEnd : char = '='; // character to mark message as complete
begin
   for i:=1 to 2 do
      begin
         l := Length(cwBuffer[i]);

	 if l = 0 then
	    Continue;

         // buffer overrun, kill it
         if l > 32 then
         begin
           cwBuffer[i] := '';
           Continue;
         end;

         // check to make sure message is complete (ends with =)
         if pos('=',cwBuffer[i]) = 0 then
            Continue;

         // copy part up to = into work
         i1 := Pos(cmdEnd, cwBuffer[i]);
         work := Copy(cwBuffer[i],1,i1-1);
         Delete(cwBuffer[i],1,i1);

	 cmdNr := 0;
	 i0 := 1;
	 i2 := 0;
	 while true do
	 begin
	    // find any complete +xx+ commands and remove them
	    i1 := Pos(cmdSep, work, i0);
	    i2 := Pos(cmdSep, work, i1 + 1);
	    if ((i1 <> 0) and (i2 <> 0) and ((i2 - i1) = 3)) then
	    begin
	       cmdNr := (byte(work[i1 + 2]) - 48) + (byte(work[i1 + 1]) - 48) * 10;
	       // save text before, needed to ID callsign
	       firstPart := Trim(Copy(work,1,i1-1));
               delete(work,i1,4);
	       // act on cmdNr
	       case cmdNr of
		 0: Tst[i].Me.Msg := [msgNone];
		 1:
		   begin
		      Tst[i].Me.Msg := [msgCQ];
                      NRSent[i] := False;
		      CallSent[i] := False;
		   end;
		 2:
		   begin
		      Include(Tst[i].Me.Msg, msgNR);
                      NrSent[i] := True;
		   end;
		 3: 
		    begin
		       Tst[i].Me.Msg := [msgCQ, msgTU];
                    end;
		 4: Tst[i].Me.Msg := [msgMyCall];
		 5:
		   begin
		      Exclude(Tst[i].Me.Msg, msgCQ);
		      Exclude(Tst[i].Me.Msg, msgTU);
		      Include(Tst[i].Me.Msg, msgHisCall);
		      CallSent[i] := True;
		      Tst[i].Me.HisCall := firstPart;
                   end;
		 6: Tst[i].Me.Msg := [msgB4];
		 7: Tst[i].Me.Msg := [msgQm];
		 8: Tst[i].Me.Msg := [msgNil];
		 9: Tst[i].Me.Msg := [msgGarbage];
		 10: Tst[i].Me.Msg := [msgR_NR];
		 11: Tst[i].Me.Msg := [msgR_NR2];
		 12: Tst[i].Me.Msg := [msgDeMyCall1];
		 13: Tst[i].Me.Msg := [msgDeMyCall2];
		 14: Tst[i].Me.Msg := [msgDeMyCallNr1];
		 15: Tst[i].Me.Msg := [msgDeMyCallNr2];
		 16: Tst[i].Me.Msg := [msgNrQm];
		 17: Tst[i].Me.Msg := [msgLongCQ];
		 18: Tst[i].Me.Msg := [msgMyCallNr2];
		 19: Tst[i].Me.Msg := [msgQrl];
		 20: Tst[i].Me.Msg := [msgQrl2];
		 21: Tst[i].Me.Msg := [msgQsy];
		 22: Tst[i].Me.Msg := [msgAgn];
	       end;
	       i0 := 1;
	       i2 := 0;
	    end
	    else if ( (i1 <> 0) and (i2 = 0) ) then
	    begin
	       // cw message not fully loaded, exit
	       exit;
	    end
	    else
	    begin
	       // found all +xx+ in this text, can send the rest
	       break;
	    end;
	 end;
	 if serialStatus = True then
	 begin
	    serialOutBuffer[serialOutPtr] := i;
	    serialOutPtr := serialOutPtr + 1;
	    serialOutPtr := serialOutPtr mod 128;
	    stopSent[i] := False;
	 end;
	 Tst[i].Me.SendText(work);
	 work := '';
      end;
end;

// process so2rmini-N6TR commands
procedure TMainForm.so2rmini;
var
  i	    :  integer;
  b,b2	    : byte;
  const ver : Pchar = 'TRCW V4';
begin
  // check if cw stopped sending and send so2rmini byte
  if ((stopSent[1] = False) and ((Tst[1].Me.State = stListening) or
    (Tst[1].Me.State = stCopying))) then
  begin
    serialOutBuffer[serialOutPtr] := $00;
    serialOutPtr := serialOutPtr + 1;
    serialOutPtr := serialOutPtr mod 128;
    stopSent[1] := True;
  end;
  if ((stopSent[2] = False) and ((Tst[2].Me.State = stListening) or
    (Tst[2].Me.State = stCopying))) then
  begin
     serialOutBuffer[serialOutPtr] := $00;
     serialOutPtr := serialOutPtr + 1;
     serialOutPtr := serialOutPtr mod 128;
     stopSent[2] := True;
  end;

  while devicePtr <> serialPtr do
  begin
    b := serialBuffer[devicePtr];
    devicePtr := devicePtr + 1;
    devicePtr := devicePtr mod 128;

    // already a command, byte b is argument of command
    if so2rMiniCmd <> 0 then
    begin
      case so2rMiniCmd of
        $02:
	begin
	   // audio control: lower two bits
	   // bit0   bit1
	   //    0    x    : both channels to radio 1
	   //    1    0    : both channels to radio 2
	   //    1    1    : split audio
	   b2 := (b and 3);
	   if b2 = 1 then
	      AlSoundOut1.SetChannel(1)
	   else if b2 = 3 then
	      AlSoundOut1.SetChannel(2)
	   else
	      AlSoundOut1.SetChannel(0);
	end;
        $03:
        begin
          // set CW pitch : some issues, so disabled for now
	  // if ( (b>= 30) and (b <= 90)) then
	  //   SetPitch(b * 10);
        end;
	$04:; //WriteLn('Paddle tone byte');
	$05:; //WriteLn('Paddle pin byte');
	$06:; //WriteLn('Keyer weight byte');
	$07:; //WriteLn('CW char offset byte');
	$08:  
        begin
	   //  WriteLn('CW speed byte'); 
	  if ( (b >= 10) and (b <= 60) ) then
	     begin
		SpinEdit1.Value := b;
		Tst[1].Me.Wpm := b;
		Tst[2].Me.Wpm := b;
	     end;
        end;
	$09:; //WriteLn('Paddle speed byte');
	$0A:; //WriteLn('PTT on byte');
        $0B: 
	begin
	   //  WriteLn('Radio select byte');
	   if b = $01 then
	      radioNrTx := 1
	   else
	      radioNrTx := 2;
	end;   
	$0E:; //WriteLn('PTT assert time byte');
	$0F:; //WriteLn('PTT hold time byte');
	$10:; //WriteLn('PTT hold paddle time  byte');
	$14:; //WriteLn('Curtis mode byte');
	$15:; //WriteLn('Paddle bug mode byte');
	$16:; //WriteLn('PTT enable byte');
	$17:; //WriteLn('Tune with dits byte');
	$18:; //WriteLn('Farnsworth speed byte');
	$19:; //WriteLn('Footswitch byte');
      end;
      so2rMiniCmd := 0;
      b := 0;
    end;

    // cw characters: add to send buffer if on this radio
    // 59 is ; which can also be commands
    if ( (RunMode = rmRun) and (b >= 32)) then 
    begin
      cwBuffer[radioNrTx] := cwBuffer[radioNrTx] + chr(b);
      continue;
    end;

     
    // commands
    case b of
      // send version
      $01			   : 
      begin
	 for i:= 0 to 6 do
	 begin
	   serialOutBuffer[serialOutPtr] := Byte(ver[i]);
	   serialOutPtr := serialOutPtr + 1;
	   serialOutPtr := serialOutPtr mod 128;
	 end;
      end;
      // cancel CW
      $12, $13			   : 
      begin
        if msgHisCall in Tst[1].Me.Msg then CallSent[1] := False;
        if msgHisCall in Tst[2].Me.Msg then CallSent[2] := False;
	if msgNR in Tst[1].Me.Msg then NrSent[1] := False;
	if msgNR in Tst[2].Me.Msg then NrSent[2] := False;
	Tst[1].Me.AbortSend;
        Tst[2].Me.AbortSend;
      end;
      $11			   : // query footswitch
      begin
	 serialOutBuffer[serialOutPtr] := 0;
	 serialOutPtr := serialOutPtr + 1;
	 serialOutPtr := serialOutPtr mod 128;
      end;
      $0c, $0d			   : // CW being sent query, number chars in buffer
      begin
	 serialOutBuffer[serialOutPtr] := 0;
	 serialOutPtr := serialOutPtr + 1;
	 serialOutPtr := serialOutPtr mod 128;
      end;
      $1a, $1b			   : // delete last char
      begin
	 serialOutBuffer[serialOutPtr] := 0;
	 serialOutPtr := serialOutPtr + 1;
	 serialOutPtr := serialOutPtr mod 128;
      end;
      // these commands have an argument; set cmd to this byte, next byte
      // will be interpreted as argument. $3b is ';'
      $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0E, $0F, $10,
      $14, $15, $16, $17, $18, $19 :
      begin
        so2rMiniCmd := b;
      end;
    end;
  end;
end;

// process otrsp commands
procedure TMainForm.otrsp;
begin
   otrspCmd := UpperCase(otrspCmd);
   if LeftStr(otrspCmd,1) = '?' then
      // OTRSP query - not implemented yet
   else if LeftStr(otrspCmd,2) = 'RX' then
      begin
	 if Length(otrspCmd) = 3 then
	    if RightStr(otrspCmd,1) = '1' then
	       begin
		  AlSoundOut1.SetChannel(0);
	       end  
	    else if RightStr(otrspCmd,1) = '2' then
	       begin
		  AlSoundOut1.SetChannel(1);
	       end;
	 if Length(otrspCmd) = 4 then
	    if ((RightStr(otrspCmd,2)='1S') or (RightStr(otrspCmd,2)='2S')
		  or (RightStr(otrspCmd,2)='1R') or (RightStr(otrspCmd,2)='2R')) then
	       begin
		  AlSoundOut1.SetChannel(2);
	       end;
      end
   else if LeftStr(otrspCmd,2) = 'TX' then
      begin
	 if RightStr(otrspCmd,1)='1' then
	    radioNrTx := 1
         else  if RightStr(otrspCmd,1)='2' then
            radioNrTx := 2;
      end;
   otrspCmd := '';
end;

// process winkey commands
procedure TMainForm.winkey;
var
  b	  : byte;
  haveCmd : boolean;
   echo	  : boolean;
   pointerExtra : boolean;
   const ver : byte=23;   // winkey version reported
begin
  haveCmd := False;
  echo := False;
  pointerExtra := False;
   
  // check if cw stopped sending and send status byte
  if ((stopSent[1] = False) and ((Tst[1].Me.State = stListening) or
    (Tst[1].Me.State = stCopying))) then
  begin
    // 0b11000000 = 0xc0
    serialOutBuffer[serialOutPtr] := $C0;
    serialOutPtr := serialOutPtr + 1;
    serialOutPtr := serialOutPtr mod 128;
    stopSent[1] := True;
  end;
  if ((stopSent[2] = False) and ((Tst[2].Me.State = stListening) or
     (Tst[2].Me.State = stCopying))) then
  begin
     serialOutBuffer[serialOutPtr] := $C0;
     serialOutPtr := serialOutPtr + 1;
     serialOutPtr := serialOutPtr mod 128;
     stopSent[2] := True;
  end;

  while devicePtr <> serialPtr do
  begin
    b := serialBuffer[devicePtr];
    devicePtr := devicePtr + 1;
    devicePtr := devicePtr mod 128;

    if echo then
    begin
       serialOutBuffer[serialOutPtr] := b;
       serialOutPtr := serialOutPtr + 1;
       serialOutPtr := serialOutPtr mod 128;
       echo := False;
       continue;
    end;
     
    // already a command, byte b is argument of command
    if haveCmd then
    begin
      case winkeyCmd of
	$00 : // admin command
        begin
	   if (b = $02) then
	   begin
	      // send winkey version
	      serialOutBuffer[serialOutPtr] := ver;
	      serialOutPtr := serialOutPtr + 1;
	      serialOutPtr := serialOutPtr mod 128;
	   end
	   else if (b = $04) then
	      begin
		 echo := true;
	      end;
	end;
	$01:;  // sidetone control
	$02:  // set speed
        begin
	  if ( (b >= 10) and (b <= 60) ) then
	     begin
		SpinEdit1.Value := b;
		Tst[1].Me.Wpm := b;
		Tst[2].Me.Wpm := b;
	     end;
        end;
	$04 : // PTT lead/tail
	begin

	end; 
	$06:; // pause
	$07:; // get speed pot
	$09:; // set pin config
	$0B:; // key immediate
	$0C:; // set HSCW
	$0D:; // set Farnsworth
	$0E:; // set winkey2 mode
	$0F:; // load defaults
	$10:; // set 1st extension
	$11:; // set key comp
	$12:; // set paddle switchpoint
	$13:; // null
	$14:; // software paddle
	$16:
        begin
             // winkey pointer command. No docs available on these,
             // will ignore them for now
             // b=01, 02, 03 have a following 1-byte argument
             if pointerExtra then
                begin
                  // extra arg would be handled here
                  pointerExtra := False;
                  haveCmd := False;
                  winkeyCmdCnt :=0;
                  winkeyCmd := 0;
                  Continue;
                end;
             if ((b <> 0) and not(pointerExtra)) then
                begin
                     pointerExtra := True;
                     Continue;
                end;
        end;
	$17:; // dit/dah ratio
	$18:; // buffered PTT on/off
	$19:; // buffered key
	$1A:; // wait
	$1B:; // merge letters
	$1C:; // buffered speed change
        $1D:; // HSCW speed change
	$1E:; // cancel buffered speed change
	$1F:; // buffered nop
      end;

      // read extra command arguments
      if winkeyCmdCnt = 0 then
	 begin
	    haveCmd := False;
	    winkeyCmd := 0;
	    b := 0;
            pointerExtra := false;
	 end
      else
	 begin
	    winkeyCmdCnt := winkeyCmdCnt - 1;
	 end;

      continue;
    end;

    // cw characters: add to send buffer if on this radio
    if ( (RunMode = rmRun) and (b >= 32)) then 
    begin
      cwBuffer[radioNrTx] := cwBuffer[radioNrTx] + chr(b);
      continue;
    end;

    case b of
      $08 : // backspace
      begin
	 haveCmd := False;
  	 winkeyCmdCnt := 0;
      end;
      $0A : // clear input buffer
      begin
	 haveCmd := False;
	 winkeyCmdCnt := 0;
         cwBuffer[radioNrTx] := '';
         Continue;
      end;
      $13 : // null command
      begin
	 haveCmd := False;
 	 winkeyCmdCnt := 0;
      end;
      // these commands have arguments; set cmd to this byte, next byte
      // will be interpreted as argument
      // commands with multi-byte arguments not supported!
      $00, // admin command
      $01, // sidetone control
      $02, // WPM speed
      $03, // weighting
      $06, // set pause
      $07, // get speed pot
      $09, // pin config
      $0B, // key immediate
      $0C, // HSCW
      $0D, // Farnsworth
      $0E, // Winkey2 mode
      $0F, // load defaults
      $10, // set extension
      $11, // key compensation
      $12, // paddle switchpoint
      $14, // software paddle
      $15, // req status
      $16, // pointer command
      $17, // dit/dah ratio
      $18, // ptt on/off
      $19, // key buffered
      $1A, // wait
      $1B, // merge letters
      $1C, // buffered speed change
      $1D, // HSCW speed
      $1E, // cancel buffered speed change
      $1F : // buffered nop
      begin
         winkeyCmdCnt := 0;
         winkeyCmd := b;
	 haveCmd := True;
      end;
      $04 : // PTT lead/tail (not implemented)
            // takes two bytes as arguments
      begin
	 winkeyCmdCnt := 1;
         winkeyCmd := b;
	 haveCmd := True;
      end;
      $05 : // setup speed pot (not implemented)
            // takes three bytes as arguments
      begin
	 winkeyCmdCnt := 2;
         winkeyCmd := b;
	 haveCmd := True;
      end;
    end;
  end;
end;


end.
