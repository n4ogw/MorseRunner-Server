//------------------------------------------------------------------------------
//This Source Code Form is subject to the terms of the Mozilla Public
//License, v. 2.0. If a copy of the MPL was not distributed with this
//file, You can obtain one at http://mozilla.org/MPL/2.0/.
//------------------------------------------------------------------------------
unit SndOut;

{$MODE Delphi}
{$UNITPATH sdl2}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs,
  BaseComp, SndTypes, SndCustm, Math, sdl2;

type
  TAlSoundOut = class(TCustomSoundInOut)
  private
    FOnBufAvailable: TNotifyEvent;
    FCloseWhenDone: boolean;

    procedure CheckErr;
    function NextEmptyBuffer: PWaveBuffer;
    procedure Unprepare(Buf: PWaveBuffer);
  protected
    procedure BufferDone(Buf: PWaveBuffer); override;
    procedure Start; override;
    procedure Stop; override;
  public
    function PutData(Data1, Data2: TSingleArray): boolean;
    procedure Purge;
  published
    property Enabled;
    property DeviceID;
    property SamplesPerSec;
    property BufsAdded;
    property BufsDone;
    property BufCount;
    property CloseWhenDone: boolean
      read FCloseWhenDone write FCloseWhenDone default False;
    property OnBufAvailable: TNotifyEvent read FOnBufAvailable write FOnBufAvailable;
  end;

procedure Register;



implementation

procedure Register;
begin
  RegisterComponents('Al', [TAlSoundOut]);
end;

{ TAlSoundOut }

//------------------------------------------------------------------------------
//                              Err handling
//------------------------------------------------------------------------------
procedure TAlSoundOut.CheckErr;
//var
//  Buf: array [0..MAXERRORLENGTH-1] of Char;
begin
  //  if rc = MMSYSERR_NOERROR then Exit;

  //  if waveOutGetErrorText(rc, Buf, MAXERRORLENGTH) = MMSYSERR_NOERROR
  //    then Err(Buf)
  //    else Err('Unknown error: ' + IntToStr(rc));
end;




//------------------------------------------------------------------------------
//                               start/stop
//------------------------------------------------------------------------------
procedure TAlSoundOut.Start;
var
  i: integer;
begin
  //open device
  //Writeln('SoundOut.Start');
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
  //Writeln('SoundOut.Stop');
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



procedure TAlSoundOut.Purge;
begin
  Stop;
  Start;
end;



end.
