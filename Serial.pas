//------------------------------------------------------------------------------
//This Source Code Form is subject to the terms of the Mozilla Public
//License, v. 2.0. If a copy of the MPL was not distributed with this
//file, You can obtain one at http://mozilla.org/MPL/2.0/.
//------------------------------------------------------------------------------

//
// This unit emulates serial devices: winkey, OTRSP, and sor2Mini
//
unit Serial;

{$MODE Delphi}

interface

uses
  Dialogs, SysUtils, Station, MyStn, synaser, TypInfo;

const
  BUFF_SIZE = 256;

type
  TSerial = class

  private
    cwBuffer: array [1..2] of string;
    devicePtr: integer;
    deviceOutPtr: integer;
    otrspCmd: string;
    radioNrTx: integer;
    serialBuffer: array [0..(BUFF_SIZE - 1)] of byte;
    serialInput: TBlockSerial;
    serialInputOtrsp: TBlockSerial;
    serialOutBuffer: array [0..(BUFF_SIZE - 1)] of byte;
    serialPtr: integer;
    serialOutPtr: integer;
    so2rMiniCmd: byte;
    stopSent: array [1..2] of boolean;
    winkeyCmd: byte;
    winkeyCmdCnt: integer;
    winkeyEcho: boolean;
    winkeyHaveCmd: boolean;
    winkeyPointerExtra: boolean;

    procedure addToOut(b :  byte);
    procedure otrsp;
    procedure sendCW;
    procedure so2rmini;
    procedure winkey;

  public
    serialStatus: boolean;
    serialStatusOtrsp: boolean;

    procedure checkSerial(Sender: TObject);
    procedure serialInit;
    constructor Create;
    destructor Destroy; override;
  end;

var
  Ser: TSerial;

implementation

uses
  Main, Ini, Contest;

{ TSerial }

constructor TSerial.Create;
begin
  radioNrTx := 1;
   
  if serialInput = nil then
    serialInput := TBlockSerial.Create;
   
  serialInput.LinuxLock := False;
   
  if serialInputOtrsp = nil then
    serialInputOtrsp := TBlockSerial.Create;
   
  serialInputOtrsp.LinuxLock := False;
end;

destructor TSerial.Destroy;
begin
  if serialInput <> nil then
  begin
    serialInput.closeSocket;
    serialInput.Free;
  end;
  if serialInputOtrsp <> nil then
  begin
    serialInputOtrsp.closeSocket;
    serialInputOtrsp.Free;
  end;
end;

// add a byte to the outgoing serial buffer
procedure TSerial.addToOut(b :  byte);
begin
   serialOutBuffer[serialOutPtr] := b;
   serialOutPtr := serialOutPtr + 1;
   serialOutPtr := serialOutPtr mod BUFF_SIZE;
end;
    

// initialize serial devices. Two modes
// 1. winkey and otrsp (2 ports)
// 2. sor2mini (1 port)
procedure TSerial.serialInit;
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
      On E: Exception do
      begin
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
      On E: Exception do
      begin
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
      On E: Exception do
      begin
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
  winkeyHaveCmd := False;
  winkeyEcho := False;
  winkeyPointerExtra := False;
  stopSent[1] := True;
  stopSent[2] := True;
  otrspCmd := '';
end;

// 1. check for characters on received on device serial port (winkey or so2rmini)
// 2. check for characters to transmit on device serial port
// 3. process commands
procedure TSerial.checkSerial(Sender: TObject);
var
  b: byte;
begin
  // read from winkey or so2rmini serial port into circular buffer
  while serialInput.CanReadEx(0) = True do
  begin
    serialBuffer[serialPtr] := serialInput.RecvByte(0);
    serialPtr := serialPtr + 1;
    serialPtr := serialPtr mod BUFF_SIZE;
  end;

  // append chars to otrsp command until return is received
  while serialInputOtrsp.CanReadEx(0) = True do
  begin
    b := serialInputOtrsp.RecvByte(0);
    if b <> $0d then
      otrspCmd := otrspCmd + char(b)
    else
      otrsp;
  end;

  // send winkey or so2rmini output 
  while ((serialOutPtr <> deviceOutPtr) and (serialInput.CanWrite(0) = True)) do
  begin
    serialInput.SendByte(serialOutBuffer[deviceOutPtr]);
    deviceOutPtr := deviceOutPtr + 1;
    deviceOutPtr := deviceOutPtr mod BUFF_SIZE;
  end;

  if Ini.serialMode = modeSo2rmini then
    so2rmini
  else
    winkey;

  if runMode = rmRun then
    sendCW;
end;

// send cw messages
procedure TSerial.sendCW;
var
  i, l: integer;
  i0, i1, i2, cmdNr: integer;
  firstPart, work: string;
const
  cmdSep: char = '+'; // character to mark commands
const
  cmdEnd: char = '='; // character to mark message as complete
begin
  for i := 1 to 2 do
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
    if pos('=', cwBuffer[i]) = 0 then
      Continue;

    // copy part up to = into work
    i1 := Pos(cmdEnd, cwBuffer[i]);
    work := Copy(cwBuffer[i], 1, i1 - 1);
    Delete(cwBuffer[i], 1, i1);

    cmdNr := 0;
    i0 := 1;
    i2 := 0;
    while True do
    begin
      // find any complete +xx+ commands and remove them
      i1 := Pos(cmdSep, work, i0);
      i2 := Pos(cmdSep, work, i1 + 1);
      if ((i1 <> 0) and (i2 <> 0) and ((i2 - i1) = 3)) then
      begin
        cmdNr := (byte(work[i1 + 2]) - 48) + (byte(work[i1 + 1]) - 48) * 10;
        // save text before, needed to ID callsign
        firstPart := Trim(Copy(work, 1, i1 - 1));
        Delete(work, i1, 4);
        // act on cmdNr
        case cmdNr of
          0: Tst[i].Me.Msg := [msgNone];
          1:
          begin
            Tst[i].Me.Msg := [msgCQ];
          end;
          2:
          begin
            Include(Tst[i].Me.Msg, msgNR);
            Exclude(Tst[i].Me.Msg, msgCQ);
            Exclude(Tst[i].Me.Msg, msgTU);
          end;
          3:
          begin
            Exclude(Tst[i].Me.Msg, msgNR);
            Include(Tst[i].Me.Msg, msgCQ);
            Include(Tst[i].Me.Msg, msgTU);
          end;
          4: Tst[i].Me.Msg := [msgMyCall];
          5:
          begin
            if firstPart <> '' then
            begin
              Include(Tst[i].Me.Msg, msgHisCall);
              Exclude(Tst[i].Me.Msg, msgCQ);
              Tst[i].Me.HisCall := firstPart;
            end;
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
      else if ((i1 <> 0) and (i2 = 0)) then
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
      addToOut(i); 
      stopSent[i] := False;
    end;
    // if in winkey mode, send status byte to signal starting
    if serialMode = modeWinkey then
    begin
      addToOut($C0 + 4); 
    end;

    Tst[i].Me.SendText(work);
    work := '';
  end;
end;

// process so2rmini-N6TR commands
procedure TSerial.so2rmini;
var
  i: integer;
  b, b2: byte;
const
  ver: PChar = 'TRCW V4';
begin
  // check if cw stopped sending and send so2rmini byte
  if ((stopSent[1] = False) and ((Tst[1].Me.State = stListening) or
    (Tst[1].Me.State = stCopying))) then
  begin
    addToOut(0); 
    stopSent[1] := True;
  end;
  if ((stopSent[2] = False) and ((Tst[2].Me.State = stListening) or
    (Tst[2].Me.State = stCopying))) then
  begin
    addToOut(0);
    stopSent[2] := True;
  end;

  while devicePtr <> serialPtr do
  begin
    b := serialBuffer[devicePtr];
    devicePtr := devicePtr + 1;
    devicePtr := devicePtr mod BUFF_SIZE;

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
            MainForm.AlSoundOut1.SetChannel(1)
          else if b2 = 3 then
            MainForm.AlSoundOut1.SetChannel(2)
          else
            MainForm.AlSoundOut1.SetChannel(0);
        end;
        $03:
        begin
          // set CW pitch : some issues, so disabled for now
          // if ( (b>= 30) and (b <= 90)) then
          //   SetPitch(b * 10);
        end;
        $04: ; //WriteLn('Paddle tone byte');
        $05: ; //WriteLn('Paddle pin byte');
        $06: ; //WriteLn('Keyer weight byte');
        $07: ; //WriteLn('CW char offset byte');
        $08:
        begin
          //  WriteLn('CW speed byte');
          if ((b >= 10) and (b <= 60)) then
          begin
            MainForm.SpinEdit1.Value := b;
            Tst[1].Me.Wpm := b;
            Tst[2].Me.Wpm := b;
          end;
        end;
        $09: ; //WriteLn('Paddle speed byte');
        $0A: ; //WriteLn('PTT on byte');
        $0B:
        begin
          //  WriteLn('Radio select byte');
          if b = $01 then
            radioNrTx := 1
          else
            radioNrTx := 2;
        end;
        $0E: ; //WriteLn('PTT assert time byte');
        $0F: ; //WriteLn('PTT hold time byte');
        $10: ; //WriteLn('PTT hold paddle time  byte');
        $14: ; //WriteLn('Curtis mode byte');
        $15: ; //WriteLn('Paddle bug mode byte');
        $16: ; //WriteLn('PTT enable byte');
        $17: ; //WriteLn('Tune with dits byte');
        $18: ; //WriteLn('Farnsworth speed byte');
        $19: ; //WriteLn('Footswitch byte');
      end;
      so2rMiniCmd := 0;
      b := 0;
    end;

    // cw characters: add to send buffer if on this radio
    // 59 is ; which can also be commands
    if ((RunMode = rmRun) and (b >= 32)) then
    begin
      cwBuffer[radioNrTx] := cwBuffer[radioNrTx] + chr(b);
      continue;
    end;


    // commands
    case b of
      // send version
      $01:
      begin
        for i := 0 to (length(ver)-1) do
        begin
	  addToOut( byte(ver[i]) );
        end;
      end;
      // cancel CW
      $12, $13:
      begin
        cwBuffer[1] := '';
        cwBuffer[2] := '';
        Tst[1].Me.AbortSend;
        Tst[2].Me.AbortSend;
      end;
      $11: // query footswitch
      begin
	addToOut(0); 
      end;
      $0c, $0d: // CW being sent query, number chars in buffer
      begin
	addToOut(0);
      end;
      $1a, $1b: // delete last char
      begin
        addToOut(0);
      end;
      // these commands have an argument; set cmd to this byte, next byte
      // will be interpreted as argument. $3b is ';'
      $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0E, $0F, $10,
      $14, $15, $16, $17, $18, $19:
      begin
        so2rMiniCmd := b;
      end;
    end;
  end;
end;

// process otrsp commands
procedure TSerial.otrsp;
begin
  otrspCmd := UpperCase(otrspCmd);
  if LeftStr(otrspCmd, 1) = '?' then
  // OTRSP query - not implemented yet
  else if LeftStr(otrspCmd, 2) = 'RX' then
  begin
    if Length(otrspCmd) = 3 then
      if RightStr(otrspCmd, 1) = '1' then
      begin
        MainForm.AlSoundOut1.SetChannel(0);
      end
      else if RightStr(otrspCmd, 1) = '2' then
      begin
        MainForm.AlSoundOut1.SetChannel(1);
      end;
    if Length(otrspCmd) = 4 then
      if ((RightStr(otrspCmd, 2) = '1S') or (RightStr(otrspCmd, 2) = '2S') or
        (RightStr(otrspCmd, 2) = '1R') or (RightStr(otrspCmd, 2) = '2R')) then
      begin
        MainForm.AlSoundOut1.SetChannel(2);
      end;
  end
  else if LeftStr(otrspCmd, 2) = 'TX' then
  begin
    if RightStr(otrspCmd, 1) = '1' then
      radioNrTx := 1
    else if RightStr(otrspCmd, 1) = '2' then
      radioNrTx := 2;
  end;
  otrspCmd := '';
end;

// process winkey commands
procedure TSerial.winkey;
var
  b: byte;
const
  ver: byte = 23;   // winkey version reported
begin
  // check if cw stopped sending and send status byte
  if ((stopSent[1] = False) and ((Tst[1].Me.State = stListening) or
    (Tst[1].Me.State = stCopying))) then
  begin
    // 0b11000000 = 0xc0
    addToOut( $C0 );
    stopSent[1] := True;
  end;
  if ((stopSent[2] = False) and ((Tst[2].Me.State = stListening) or
    (Tst[2].Me.State = stCopying))) then
  begin
    addToOut( $C0 );
    stopSent[2] := True;
  end;

  while devicePtr <> serialPtr do
  begin
    // get next byte from circular buffer
    b := serialBuffer[devicePtr];
    devicePtr := devicePtr + 1;
    devicePtr := devicePtr mod BUFF_SIZE;

    // echo received byte to serial output
    if winkeyEcho then
    begin
      addToOut( b );
      winkeyEcho := False;
      continue;
    end;

    // already a command, so byte b is argument of command
    if winkeyHaveCmd then
    begin
      case winkeyCmd of
        $00: // admin command
        begin
          if (b = $02) then
          begin
            // send winkey version
	    addToOut ( ver );
          end
          else if (b = $04) then
          begin
            winkeyEcho := True;
          end;
        end;
        $01: ;  // sidetone control
        $02:  // set speed
        begin
          if ((b >= 10) and (b <= 60)) then
          begin
            MainForm.SpinEdit1.Value := b;
            Tst[1].Me.Wpm := b;
            Tst[2].Me.Wpm := b;
          end;
        end;
        $04: // PTT lead/tail
        begin

        end;
        $06: ; // pause
        $07: ; // get speed pot
        $09: ; // set pin config
        $0B: ; // key immediate
        $0C: ; // set HSCW
        $0D: ; // set Farnsworth
        $0E: ; // set winkey2 mode
        $0F: ; // load defaults
        $10: ; // set 1st extension
        $11: ; // set key comp
        $12: ; // set paddle switchpoint
        $13: ; // null
        $14: ; // software paddle
        $16:
        begin
          // winkey pointer command. No docs available on these,
          // will ignore them for now
          // b=01, 02, 03 have a following 1-byte argument
          if winkeyPointerExtra then
          begin
            // extra arg would be handled here
            winkeyPointerExtra := False;
            winkeyHaveCmd := False;
            winkeyCmdCnt := 0;
            winkeyCmd := 0;
            Continue;
          end;
          if ((b <> 0) and not (winkeyPointerExtra)) then
          begin
            winkeyPointerExtra := True;
            Continue;
          end;
        end;
        $17: ; // dit/dah ratio
        $18: ; // buffered PTT on/off
        $19: ; // buffered key
        $1A: ; // wait
        $1B: ; // merge letters
        $1C: ; // buffered speed change
        $1D: ; // HSCW speed change
        $1E: ; // cancel buffered speed change
        $1F: ; // buffered nop
      end;

      // read extra command arguments
      if winkeyCmdCnt = 0 then
      begin
        winkeyHaveCmd := False;
        winkeyCmd := 0;
        b := 0;
        winkeyPointerExtra := False;
      end
      else
      begin
        winkeyCmdCnt := winkeyCmdCnt - 1;
      end;

      continue;
    end;

    // cw characters: add to send buffer if on this radio
    if ((RunMode = rmRun) and (b >= 32)) then
    begin
      cwBuffer[radioNrTx] := cwBuffer[radioNrTx] + chr(b);
      continue;
    end;

    case b of
      // these have no parameter byte, and are ignored
      $08,  // backspace
      $13,  // null command
      $1E,  // cancel buffered speed change
      $1F: // buffered nop
      begin
        winkeyHaveCmd := False;
        winkeyCmdCnt := 0;
      end;
      $0A: // clear input buffer
      begin
        winkeyHaveCmd := False;
        winkeyCmdCnt := 0;
        cwBuffer[1] := '';
        cwBuffer[2] := '';
        Tst[1].Me.AbortSend;
        Tst[2].Me.AbortSend;
        Continue;
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
      $1D: // HSCW speed
      begin
        winkeyCmdCnt := 0;
        winkeyCmd := b;
        winkeyHaveCmd := True;
      end;
      $04: // PTT lead/tail (not implemented)
        // takes two bytes as arguments
      begin
        winkeyCmdCnt := 1;
        winkeyCmd := b;
        winkeyHaveCmd := True;
      end;
      $05: // setup speed pot (not implemented)
        // takes three bytes as arguments
      begin
        winkeyCmdCnt := 2;
        winkeyCmd := b;
        winkeyHaveCmd := True;
      end;
    end;
  end;
end;

end.
