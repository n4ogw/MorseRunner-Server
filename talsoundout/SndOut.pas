//------------------------------------------------------------------------------
//This Source Code Form is subject to the terms of the Mozilla Public
//License, v. 2.0. If a copy of the MPL was not distributed with this
//file, You can obtain one at http://mozilla.org/MPL/2.0/.
//------------------------------------------------------------------------------
unit SndOut;

{$MODE Delphi}

{$IFDEF Linux}
{$UNITPATH sdl2}
{$Endif}

interface

uses
{$ifdef Linux}
  LCLIntf, LCLType, LMessages, sdl2,
{$endif}
{$ifdef Windows}
  Windows, MMSystem,
{$endif}
  Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs,BaseComp, SndTypes, SndCustm, Math;

type
  TAlSoundOut = class(TCustomSoundInOut)
  private
    FOnBufAvailable: TNotifyEvent;
    FCloseWhenDone: boolean;

    procedure CheckErr;
    function NextEmptyBuffer: PWaveBuffer;
    procedure Unprepare(Buf: PWaveBuffer);
  protected
    {$ifdef Linux}
    procedure BufferDone(Buf: PWaveBuffer); override;
    {$endif}
    {$ifdef Windows}
    procedure BufferDone(AHdr: PWaveHdr); override;
    {$endif}
    procedure Start; override;
    procedure Stop; override;
  public
{$ifdef Linux}
    function PutData(Data1, Data2: TSingleArray): boolean;
{$endif}
{$ifdef Windows}
    function PutData(Data1, Data2: TSingleArray): boolean;
{$endif}
    procedure Purge;
  published
    property Enabled;
    property DeviceID;
    property SamplesPerSec;
    property BufsAdded;
    property BufsDone;
    property BufCount;
    property Samples;
    property CloseWhenDone: boolean
      read FCloseWhenDone write FCloseWhenDone default False;
    property OnBufAvailable: TNotifyEvent read FOnBufAvailable write FOnBufAvailable;
  end;

procedure Register;

implementation

procedure Register;
begin
   RegisterNoIcon([TAlSoundOut]);
end;



{ TAlSoundOut }

//------------------------------------------------------------------------------
//                              Err handling
//------------------------------------------------------------------------------
{$ifdef Linux}
procedure TAlSoundOut.CheckErr;
//var
//  Buf: array [0..MAXERRORLENGTH-1] of Char;
begin
  //  if rc = MMSYSERR_NOERROR then Exit;

  //  if waveOutGetErrorText(rc, Buf, MAXERRORLENGTH) = MMSYSERR_NOERROR
  //    then Err(Buf)
  //    else Err('Unknown error: ' + IntToStr(rc));
end;
{$endif}
{$ifdef Windows}
procedure TAlSoundOut.CheckErr;
var
   Buf: array [0..MAXERRORLENGTH-1] of Char;
begin
   if rc = MMSYSERR_NOERROR then Exit;

   if waveOutGetErrorText(rc, Buf, MAXERRORLENGTH) = MMSYSERR_NOERROR
    then Err(Buf)
     else Err('Unknown error: ' + IntToStr(rc));
end;
{$endif}

procedure TAlSoundOut.Purge;
begin
  Stop;
  Start;
end;

{$ifdef Linux}

//------------------------------------------------------------------------------
//                               start/stop
//------------------------------------------------------------------------------
procedure TAlSoundOut.Start;
var
  i: integer;
begin
  //open device
  CheckErr;

  SDL_PauseAudio(0);

  //send all buffers to the player
  if Assigned(FOnBufAvailable) then
    for i := 0 to Length(Buffers) - 1 do
    begin
      Buffers[i].used := 0;
      FOnBufAvailable(Self);
    end;
end;


procedure TAlSoundOut.Stop;
var
  i: integer;
begin
  //stop playback
  CheckErr;

  SDL_PauseAudio(1);

  for i := 0 to Length(Buffers) - 1 do Unprepare(@Buffers[i]);
  //close device
  CheckErr;
end;

//------------------------------------------------------------------------------
//                                Buffers
//------------------------------------------------------------------------------
function TAlSoundOut.NextEmptyBuffer: PWaveBuffer;
var
  i: integer;
begin
  for i := 0 to Length(Buffers) - 1 do
    if (Buffers[i].used = 0) then
    begin
      Result := @Buffers[i];
      Exit;
    end;

  Err('Output buffers full');

end;

{$endif}
{$ifdef Windows}


//------------------------------------------------------------------------------
//                               start/stop
//------------------------------------------------------------------------------
procedure TAlSoundOut.Start;
var
  i: integer;
begin
  //open device
  rc := waveOutOpen(@DeviceHandle, DeviceID, @WaveFmt, GetThreadID, 0, CALLBACK_THREAD);
  CheckErr;

  //send all buffers to the player
  if Assigned(FOnBufAvailable) then
    for i:=0 to High(Buffers) do
      FOnBufAvailable(Self);
end;


procedure TAlSoundOut.Stop;
var
  i: integer;
begin
  //stop playback
  rc := waveOutReset(DeviceHandle);
  CheckErr;
  for i:=0 to High(Buffers) do Unprepare(@Buffers[i]);
  //close device
  rc := waveOutClose(DeviceHandle);
  CheckErr;
end;

//------------------------------------------------------------------------------
//                                Buffers
//------------------------------------------------------------------------------
function  TAlSoundOut.NextEmptyBuffer: PWaveBuffer;
var
  i: integer;
begin
  for i:=0 to High(Buffers) do
    if (Buffers[i].Hdr.dwFlags and (WHDR_INQUEUE or WHDR_PREPARED)) = 0 then
      begin Result := @Buffers[i]; Exit; end;

  Result := nil;
  //Err('Output buffers full');
end;

procedure TAlSoundOut.Unprepare(Buf: PWaveBuffer);
begin
    if (Buf.Hdr.dwFlags and WHDR_PREPARED) <> 0 then
      begin
      rc := WaveOutUnprepareHeader(DeviceHandle, @Buf.Hdr, SizeOf(TWaveHdr));
      Buf.Data := nil;
      Inc(FBufsDone);
      //CheckErr;
      end;
end;


function TAlSoundOut.PutData(Data1, Data2: TSingleArray): boolean;
var
  Buf: PWaveBuffer;
  i, len: integer;
begin
  Result := false;
  if not Enabled then Exit;

  Buf := NextEmptyBuffer;
  Result := Buf <> nil;
  if not Result then Exit;
  len := Length(Data1);

  //data to buffer  (Single -> SmallInt)
  Buf.Data := nil;
  SetLength(Buf.Data, 2 * len); // twice length for 2 stereo channels
  // channel = 0 :  radio 1 to left and right
    // channel = 1 :  radio 2 to left and right
    // channel = 2 :  radio 1 to left, radio 2 to right
   if Channel = 0 then
      for i := 0 to High(Data1) do
      begin
 	Buf.Data[2 * i] := Max(-32767, Min(32767, Round(Data1[i])));
 	Buf.Data[2 * i + 1] := Max(-32767, Min(32767, Round(Data1[i])));
      end
    else if Channel = 1 then
      for i := 0 to High(Data1) do
      begin
 	Buf.Data[2 * i] := Max(-32767, Min(32767, Round(Data2[i])));
 	Buf.Data[2 * i + 1] := Max(-32767, Min(32767, Round(Data2[i])));
      end
    else
      for i := 0 to High(Data1) do
      begin
 	Buf.Data[2 * i] := Max(-32767, Min(32767, Round(Data1[i])));
 	Buf.Data[2 * i + 1] := Max(-32767, Min(32767, Round(Data2[i])));
      end;

  //for i:=0 to High(Data) do
  //  Buf.Data[i] := Max(-32767, Min(32767, Round(Data[i])));

  //fill header
  FillChar(Buf.Hdr, SizeOf(TWaveHdr), 0);
  with Buf.Hdr do
    begin
    lpData := @Buf.Data[0];
    dwBufferLength := Length(Buf.Data) * SizeOf(SmallInt);
    dwUser := DWORD(Buf);
    end;

  //send buffer
  rc := waveOutPrepareHeader(DeviceHandle, @Buf.Hdr, SizeOf(TWaveHdr));
  CheckErr;
  rc := waveOutWrite(DeviceHandle, @Buf.Hdr, SizeOf(TWaveHdr));
  CheckErr;

  Inc(FBufsAdded);
end;





//------------------------------------------------------------------------------
//                              events
//------------------------------------------------------------------------------
procedure TAlSoundOut.BufferDone(AHdr: PWaveHdr);
begin
  Unprepare(PWaveBuffer(AHdr.dwUser));

  if FCloseWhenDone and (FBufsDone = FBufsAdded)
    then Enabled := false
    else if Assigned(FOnBufAvailable) then FOnBufAvailable(Self);
end;

{$endif}

{$ifdef Linux}

procedure TAlSoundOut.Unprepare(Buf: PWaveBuffer);
begin
  Inc(FBufsDone);
end;

function TAlSoundOut.PutData(Data1, Data2: TSingleArray): boolean;
var
  Buf: PWaveBuffer;
  i, len: integer;
begin
  if not Enabled then Exit;

  Result := False;
  Buf := NextEmptyBuffer;
  Result := Buf <> nil;
  if not Result then
  begin
    Exit;
  end;
  Buf.Data := nil;
  len := Length(Data1);

  SetLength(Buf.Data, 2 * len); // twice length for 2 stereo channels
   // channel = 0 :  radio 1 to left and right
   // channel = 1 :  radio 2 to left and right
   // channel = 2 :  radio 1 to left, radio 2 to right
  if Channel = 0 then
     for i := 0 to High(Data1) do
     begin
	Buf.Data[2 * i] := Max(-32767, Min(32767, Round(Data1[i])));
	Buf.Data[2 * i + 1] := Max(-32767, Min(32767, Round(Data1[i])));
     end
   else if Channel = 1 then
     for i := 0 to High(Data1) do
     begin
	Buf.Data[2 * i] := Max(-32767, Min(32767, Round(Data2[i])));
	Buf.Data[2 * i + 1] := Max(-32767, Min(32767, Round(Data2[i])));
     end 
   else 
     for i := 0 to High(Data1) do
     begin
	Buf.Data[2 * i] := Max(-32767, Min(32767, Round(Data1[i])));
	Buf.Data[2 * i + 1] := Max(-32767, Min(32767, Round(Data2[i])));
     end;

  Buf.len := 2 * len;
  Buf.used := 1;

  Inc(FBufsAdded);

end;




//------------------------------------------------------------------------------
//                              events
//------------------------------------------------------------------------------
procedure TAlSoundOut.BufferDone(Buf: PWaveBuffer);
begin
  Unprepare(Buf);

  if FCloseWhenDone and (FBufsDone = FBufsAdded) then Enabled := False
  else if Assigned(FOnBufAvailable) then FOnBufAvailable(Self);

end;


{$ENDIF}


end.
