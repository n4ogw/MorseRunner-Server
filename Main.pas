//------------------------------------------------------------------------------
//This Source Code Form is subject to the terms of the Mozilla Public
//License, v. 2.0. If a copy of the MPL was not distributed with this
//file, You can obtain one at http://mozilla.org/MPL/2.0/.
//------------------------------------------------------------------------------
unit Main;

{$MODE Delphi}
{$UNITPATH VCL/sdl2}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, SndCustm, SndOut, Contest, Ini, MorseKey, CallLst,
  StdCtrls, Station, Menus, ExtCtrls, Log, Math, ComCtrls, Spin, ImgList, Crc32,
  synaser, custApp, Cabrillo;

const
  WM_TBDOWN = WM_USER + 1;

type

  { TMainForm }

  TMainForm = class(TForm)
    AlSoundOut1: TAlSoundOut;
    ComboBox3: TComboBox;
    Edit1: TEdit;
    OpenDialog1: TOpenDialog;
    Port: TLabel;
    Label17: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    MenuCabrillo: TMenuItem;
    Port1: TLabel;
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

    procedure ComboBox3Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
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
    procedure setPort(serialPort :  string);
    procedure setMode(mode :  TOpMode);
    procedure setCabrillo(fileName : string);
  private
    procedure EnableCtl(Ctl: TWinControl; AEnable: boolean);
    procedure IncRit(dF: integer);
    procedure UpdateRitIndicator;
    procedure DecSpeed;
    procedure IncSpeed;
    procedure so2rmini();
    procedure so2rminiInit();

  public
    CompetitionMode: boolean;
    procedure Run(Value: TRunMode);

    procedure SetQsk(Value: boolean);
    procedure SetPitch(PitchNo: integer);
    procedure SetBw(BwNo: integer);
    procedure ReadCheckboxes;
    procedure checkSerial(sender : TObject) ;
    procedure so2rminiCW;
  end;

var
        MainForm	: TMainForm;
	serialBuffer	: array [0..127] of byte;
	serialOutBuffer	: array [0..127] of byte;
        serialPtr	: integer;
        serialOutPtr	: integer;
	so2rMiniPtr	: integer;
        so2rMiniOutPtr	: integer;
	so2rMiniCmd	: byte;
	so2rMiniStatus	: boolean;
	cwBuffer	: array [1..2] of string;
	serialInput	: TBlockSerial;
	stopSent	: array [1..2] of boolean;
	radioNrTx	: integer;
        timer1		: TTimer;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  radioNrTx := 1;

  Randomize;
  Tst[1] := TContest.Create(1);
  Tst[2] := TContest.Create(2);
  OpenDialog1.Title := 'Select Cabrillo file';
  Cab := TCabrillo.Create;
  Cab.SetNExch(2);

  LoadCallList;

  AlSoundOut1.BufCount := 4;
  AlSoundOut1.SetChannel(0);
  FromIni;

  MakeKeyer;
  Keyer.Rate := DEFAULTRATE;
  Keyer.BufSize := Ini.BufSize;

  if serialInput = Nil then 
     serialInput := TBlockSerial.Create;
  serialInput.LinuxLock := false;
  Sleep(100);

  so2rminiInit;

  timer1 := TTimer.create( Self );
  timer1.Interval := 10;
  timer1.OnTimer := checkSerial;
  timer1.enabled := true;

end;

procedure TMainForm.Edit1Change(Sender: TObject);
begin
  setPort(Trim(Edit1.Text));
  if serialInput <> Nil then so2rminiInit;
end;

procedure TMainForm.ComboBox3Change(Sender: TObject);
begin
   if ComboBox3.ItemIndex = 0 then
      setMode(opWPX)
      else
      setMode(opCab);
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

procedure TMainForm.setPort(serialPort : string);
begin
   Edit1.Text := serialPort;
   Ini.serialPort := serialPort;
end;

procedure TMainForm.setMode(mode : TOpMode);
begin
   if mode = opWPX then
      begin
	 Ini.opMode := opWPX;
	 ComboBox3.ItemIndex := 0;
       end
      else
      begin
	  Ini.opMode := opCab;
	  ComboBox3.ItemIndex := 1;
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
	  WriteLn('No file selected');
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
  Application.MessageBox(Msg, 'Morse Runner Server 1.00', MB_OK or MB_ICONINFORMATION);
end;


procedure TMainForm.Readme1Click(Sender: TObject);
var
  FileName: string;
begin
   FileName := '/usr/local/share/morserunner-server/Readme.txt';
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
  BCompet, BStop : boolean;
   i		 :  integer;
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


//------------------------------------------------------------------------------
//                              accessibility
//------------------------------------------------------------------------------

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

procedure TMainForm.so2rminiInit();
begin
  if serialInput.InstanceActive = True then
      serialInput.closeSocket;
   
  serialInput.LinuxLock := False;
  serialInput.Connect(Ini.serialPort);
  serialInput.config(19200, 8, 'N', SB1, False, False);
  if serialInput.lastError <> 0 then
  begin
    WriteLn('error opening ', serialPort);
    WriteLn(serialInput.GetErrorDesc(serialInput.LastError));
    so2rMiniStatus := False;
  end     
  else
  begin
    WriteLn('connected to ', Ini.serialPort);
    so2rMiniStatus := True;
    serialInput.LinuxLock := False;
  end;

  serialPtr := 0;
  serialOutPtr := 0;
  so2rMiniPtr := 0;
  so2rMiniOutPtr := 0;
  cwBuffer[1] := '';
  cwBuffer[2] := '';
  so2rMiniCmd := 0;
  stopSent[1] := True;
  stopSent[2] := True;
end;

// check for characters on serial port
procedure TMainForm.checkSerial(sender : TObject) ; 
begin
  if so2rMiniStatus = false then
     exit;
   
  while serialInput.CanReadEx(0) = True do
  begin
    serialBuffer[serialPtr] := serialInput.RecvByte(0);
    serialPtr := serialPtr + 1;
    serialPtr := serialPtr mod 128;
  end;
   
  while ( (so2rminiOutPtr <> serialOutPtr) and (serialInput.CanWrite(0) = true) ) do
  begin
      serialInput.SendByte(serialOutBuffer[so2rminiOutPtr]);
      so2rminiOutPtr := so2rminiOutPtr + 1;
      so2rminiOutPtr := so2rminiOutPtr mod 128;
  end;   
  so2rmini;
  if runMode = rmRun then
      so2rminiCW;
end;

// send cw messages
procedure TMainForm.so2rminiCW; 
var
   i		    : integer;
  i0, i1, i2, cmdNr : integer;
  firstPart	    : string;
begin
   for i:=1 to 2 do
      begin
	 if Length(cwBuffer[i]) = 0 then
	    Continue;

	 cmdNr := 0;
	 i0 := 1;
	 i2 := 0;
	 while true do
	 begin
	    // find any complete -xx- commands and remove them 
	    i1 := Pos('-', cwBuffer[i], i0);
	    i2 := Pos('-', cwBuffer[i], i1 + 1);
	    if ((i1 <> 0) and (i2 <> 0) and ((i2 - i1) = 3)) then
	    begin
	       cmdNr := (byte(cwBuffer[i][i1 + 2]) - 48) + (byte(cwBuffer[i][i1 + 1]) - 48) * 10;
	       // save text before, needed to ID callsign
	       firstPart := Copy(cwBuffer[i],1,i1-1);
	       delete(cwBuffer[i],i1,4);
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
	       // found all -xx- in this text, can send the rest 
	       break;
	    end;
	 end;
	 if so2rMiniStatus = True then
	 begin
	    serialOutBuffer[serialOutPtr] := i;
	    serialOutPtr := serialOutPtr + 1;
	    serialOutPtr := serialOutPtr mod 128;
	    stopSent[i] := False;
	 end;
	 Tst[i].Me.SendText(cwBuffer[i]);
	 cwBuffer[i] := '';
      end;
end;

// process so2rmini-N6TR commands
procedure TMainForm.so2rmini;
var
  i	    :  integer;
  b,b2	    : byte;
  cmd	    : byte;
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

  while so2rMiniPtr <> serialPtr do
  begin
    b := serialBuffer[so2rMiniPtr];
    so2rMiniPtr := so2rMiniPtr + 1;
    so2rMiniPtr := so2rMiniPtr mod 128;

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
          // set CW pitch : gives some problems
	  //if ( (b>= 30) and (b <= 90)) then
	  //   SetPitch(b * 10);
        end;
	$04:; //WriteLn('Paddle tone byte');
	$05:; //WriteLn('Paddle pin byte');
	$06:; //WriteLn('Keyer weight byte');
	$07:; //WriteLn('CW char offset byte');
	$08:  //WriteLn('CW speed byte');
        begin
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
	   //WriteLn('Radio select byte');
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

end.
