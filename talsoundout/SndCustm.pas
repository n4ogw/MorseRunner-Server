//------------------------------------------------------------------------------
//This Source Code Form is subject to the terms of the Mozilla Public
//License, v. 2.0. If a copy of the MPL was not distributed with this
//file, You can obtain one at http://mozilla.org/MPL/2.0/.
//------------------------------------------------------------------------------
unit SndCustm;

{$MODE Delphi}

{$IFDEF Linux}
{$UNITPATH sdl2}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Forms, SyncObjs, SndTypes, sdl2;

const
  DEFAULTBUFCOUNT = 1;
  DEFAULTRATE = 22050;

type
  TCustomSoundInOut = class;

  TWaitThread = class(TThread)
  private
    Owner: TCustomSoundInOut;
    Msg: TMsg;
    procedure ProcessEvent;
  protected
    procedure Execute; override;
  public
  end;


  TCustomSoundInOut = class(TComponent)
  public
    procedure SetChannel(const Value: integer);
    function GetChannel: integer;

  private
    FDeviceID: UINT;
    FEnabled: boolean;
    channelNr: integer;

    procedure SetDeviceID(const Value: UINT);
    procedure SetSamplesPerSec(const Value: longword);
    function GetSamplesPerSec: longword;
    procedure SetEnabled(AEnabled: boolean);
    procedure DoSetEnabled(AEnabled: boolean);
    function GetBufCount: longword;
    procedure SetBufCount(const Value: longword);
  protected
    FThread: TWaitThread;
    rc: UINT;
    DeviceHandle: UINT;
    WaveFmt: UINT;
    Buffers: array of TWaveBuffer;
    FBufsAdded: longword;
    FBufsDone: longword;
    nSamplesPerSec: longword;
    FSamples: UINT;

    procedure Loaded; override;
    procedure Err(Txt: string);
    function GetThreadID: THandle;

    //override these
    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;
    procedure BufferDone(Buf: PWaveBuffer); virtual; abstract;

    property Enabled: boolean read FEnabled write SetEnabled default False;
    property DeviceID: UINT read FDeviceID write SetDeviceID default 0;
    property SamplesPerSec: longword read GetSamplesPerSec
      write SetSamplesPerSec default DEFAULTRATE;
    property BufsAdded: longword read FBufsAdded;
    property BufsDone: longword read FBufsDone;
    property BufCount: longword read GetBufCount write SetBufCount;
    property Samples: UINT read FSamples;
    
    // channel left=0; right=1; both=2
    property Channel: integer read GetChannel write SetChannel default 0;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SndObj: TCustomSoundInOut;

  procedure Register;

implementation


procedure Register;
begin
  RegisterNoIcon( [TCustomSoundInOut]);
end;

procedure BufferDoneSDL(userdata: Pointer; stream: PUInt8; len: longint); cdecl;
var
  i: integer;
  p: PUInt8Array;
begin
  // WARNING: This is executed in the audio thread kicked off by SDL_OpenAudio
  // Copy the buffer out, mark it as empty, and let the fill thread (FThread)
  // trigger the refilling in the main thread. Otherwise .. random death (SEGV).

  p := PUInt8Array(stream);

  // The generator code sometimes gives us wrongly-sized buffers, also check for valid buffer
  if (SndObj.Buffers[0].used = 1) and (len = (2 * SndObj.Buffers[0].len)) then
  begin
    for i := 0 to (len div 2) - 1 do
    begin
      p[2 * i] := SndObj.Buffers[0].Data[i] and $ff;
      p[(2 * i) + 1] := SndObj.Buffers[0].Data[i] shr 8;
    end;
  end
  else
  begin
//    Writeln('BufferDone used ', SndObj.Buffers[0].used, ', len ',
//      len, ', 2 * Buffers[0].len = ', 2 * SndObj.Buffers[0].len);
    for i := 0 to len do
    begin
      p[i] := 0; // Silence
    end;
  end;

  // Mark buffer ready for re-fill
  SndObj.Buffers[0].used := 0;

end;


{ TWaitThread }

//------------------------------------------------------------------------------
//                               TWaitThread
//------------------------------------------------------------------------------

procedure TWaitThread.Execute;
begin
  while not Terminated do
  begin
    Synchronize(ProcessEvent);
    Sleep(10);
  end;
end;


procedure TWaitThread.ProcessEvent;
begin
  if (Owner.Buffers[0].used = 0) then
  begin
    Owner.BufferDone(@Owner.Buffers[0]);
  end;
end;

{ TCustomSoundInOut }

//------------------------------------------------------------------------------
//                               system
//------------------------------------------------------------------------------
constructor TCustomSoundInOut.Create(AOwner: TComponent);
{$ifdef Linux}
var
   des, got: PSDL_AudioSpec;
  err: string;
{$endif}   
begin
  inherited Create(AOwner);

  SetBufCount(DEFAULTBUFCOUNT);

  if SDL_Init(SDL_INIT_AUDIO) < 0 then
  begin
    Writeln('SDL_Init failed.');
    Exit;
  end;
  SamplesPerSec := DEFAULTRATE;
  channelNr := 2;
{$ifdef Linux}
    // start up temporarily to get buffer size
    des := New(PSDL_AudioSpec);
    got := New(PSDL_AudioSpec);
    with des^ do
    begin
      freq := nSamplesPerSec;
      format := AUDIO_S16LSB;
      channels := 2;
      samples := 1024;
      callback := @BufferDoneSDL;
      userdata := nil;
    end;
    if SDL_OpenAudio(des, got) < 0 then
    begin
      err := SDL_GetError();
      WriteLn('OpenAudio test failed: ', err);
      Exit;
    end;
    FSamples := got^.samples;
    SDL_CloseAudio();
{$endif}   
end;


destructor TCustomSoundInOut.Destroy;
begin
  Enabled := False;
  inherited;
end;


procedure TCustomSoundInOut.Err(Txt: string);
begin
  raise ESoundError.Create(Txt);
end;

procedure TCustomSoundInOut.SetChannel(const Value: integer);
begin
  channelNr := Value;
end;

function TCustomSoundInOut.GetChannel: integer;
begin
  Result := channelNr;
end;




//------------------------------------------------------------------------------
//                            enable/disable
//------------------------------------------------------------------------------
//do not enable component at design or load time
procedure TCustomSoundInOut.SetEnabled(AEnabled: boolean);
begin
  if (not (csDesigning in ComponentState)) and
    (not (csLoading in ComponentState)) and (AEnabled <> FEnabled) then
    DoSetEnabled(AEnabled);
  FEnabled := AEnabled;
end;


//enable component after all properties have been loaded
procedure TCustomSoundInOut.Loaded;
begin
  inherited Loaded;

  if FEnabled and not (csDesigning in ComponentState) then
  begin
    FEnabled := False;
    SetEnabled(True);
  end;
end;


procedure TCustomSoundInOut.DoSetEnabled(AEnabled: boolean);
var
  des, got: PSDL_AudioSpec;
  err: string;
begin
  if AEnabled then
  begin

    SDL_CloseAudio();

    des := New(PSDL_AudioSpec);
    got := New(PSDL_AudioSpec);
    with des^ do
    begin
      freq := nSamplesPerSec;
      format := AUDIO_S16LSB;
      channels := 2;
      samples := 1024;
      callback := @BufferDoneSDL;
      userdata := nil;
    end;
    if SDL_OpenAudio(des, got) < 0 then
    begin
      err := SDL_GetError();
      WriteLn('OpenAudio failed: ', err);
      Exit;
    end;

    //reset counts
    FBufsAdded := 0;
    FBufsDone := 0;
    //create waiting thread
    FThread := TWaitThread.Create(True);
    FThread.FreeOnTerminate := True;
    FThread.Owner := Self;
    SndObj := Self;
    FThread.Priority := tpTimeCritical;
    //start
    FEnabled := True;
    try
      Start;
    except
      FreeAndNil(FThread);
      raise;
    end;
    //device started ok, wait for events
    FThread.Start;
  end
  else
  begin
    FThread.Terminate;
    Stop;
  end;
end;


//------------------------------------------------------------------------------
//                              get/set
//------------------------------------------------------------------------------

procedure TCustomSoundInOut.SetSamplesPerSec(const Value: longword);
begin
  Enabled := False;
  nSamplesPerSec := Value;
end;


function TCustomSoundInOut.GetSamplesPerSec: longword;
begin
  Result := nSamplesPerSec;
end;



procedure TCustomSoundInOut.SetDeviceID(const Value: UINT);
begin
  Enabled := False;
  FDeviceID := Value;
end;



function TCustomSoundInOut.GetThreadID: THandle;
begin
  Result := THandle(FThread.ThreadID);
end;


function TCustomSoundInOut.GetBufCount: longword;
begin
  Result := Length(Buffers);
end;

procedure TCustomSoundInOut.SetBufCount(const Value: longword);
begin
  if Enabled then
    raise Exception.Create('Cannot change the number of buffers for an open audio device');
  SetLength(Buffers, Value);
end;

{$ENDIF}
{$IFDEF Windows}


interface

uses
  Windows, Messages, SysUtils, Classes, Forms, SyncObjs, MMSystem, SndTypes;

const
  DEFAULTBUFCOUNT = 1;
  DEFAULTRATE = 22050;

type
  TCustomSoundInOut = class;

  TWaitThread = class(TThread)
    private
      Owner: TCustomSoundInOut;
      Msg: TMsg;
      procedure ProcessEvent;
    protected
      procedure Execute; override;
    public
    end;


  TCustomSoundInOut = class(TComponent)
  public
    procedure SetChannel(const Value: integer);
    function GetChannel: integer;
  private
    FDeviceID: UINT;
    FEnabled : boolean;
    channelNr: integer;

    procedure SetDeviceID(const Value: UINT);
    procedure SetSamplesPerSec(const Value: LongWord);
    function  GetSamplesPerSec: LongWord;
    procedure SetEnabled(AEnabled: boolean);
    procedure DoSetEnabled(AEnabled: boolean);
    function GetBufCount: LongWord;
    procedure SetBufCount(const Value: LongWord);
  protected
    FThread: TWaitThread;
    rc: MMRESULT;
    DeviceHandle: HWAVEOUT;
    WaveFmt: TPCMWaveFormat;
    Buffers: array of TWaveBuffer;
    FBufsAdded: LongWord;
    FBufsDone: LongWord;
    FSamples: UINT;

    procedure Loaded; override;
    procedure Err(Txt: string);
    function GetThreadID: THandle;

    //override these
    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;
    procedure BufferDone(AHdr: PWaveHdr); virtual; abstract;

    property Enabled: boolean read FEnabled write SetEnabled default false;
    property DeviceID: UINT read FDeviceID write SetDeviceID default WAVE_MAPPER;
    property SamplesPerSec: LongWord read GetSamplesPerSec write SetSamplesPerSec default DEFAULTRATE;
    property BufsAdded: LongWord read FBufsAdded;
    property BufsDone: LongWord read FBufsDone;
    property BufCount: LongWord read GetBufCount write SetBufCount;
    property Samples: UINT read FSamples;

    // channel left=0; right=1; both=2
    property Channel: integer read GetChannel write SetChannel default 0;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SndObj: TCustomSoundInOut;

  procedure Register;

implementation

procedure Register;
begin
  RegisterNoIcon( [TCustomSoundInOut]);
end;

{ TWaitThread }

//------------------------------------------------------------------------------
//                               TWaitThread
//------------------------------------------------------------------------------

procedure TWaitThread.Execute;
begin
  Priority := tpTimeCritical;

  while GetMessage(Msg, 0, 0, 0) do
    if Terminated then Exit
    else if Msg.hwnd <> 0 then Continue
    else
      case Msg.Message of
        MM_WIM_DATA, MM_WOM_DONE: Synchronize(ProcessEvent);
        MM_WIM_CLOSE: Terminate;
        end;
end;


procedure TWaitThread.ProcessEvent;
begin
  try
    if Msg.wParam = Owner.DeviceHandle then
      Owner.BufferDone(PWaveHdr(Msg.lParam));
  except on E: Exception do
    begin
    Application.ShowException(E);
    Terminate;
    end;
  end;
end;






{ TCustomSoundInOut }

//------------------------------------------------------------------------------
//                               system
//------------------------------------------------------------------------------
constructor TCustomSoundInOut.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SetBufCount(DEFAULTBUFCOUNT);

  FDeviceID := WAVE_MAPPER;

  //init WaveFmt
  with WaveFmt do
    begin
    wf.wFormatTag := WAVE_FORMAT_PCM;
    wf.nChannels := 2;             //stereo
    wf.nBlockAlign := 4;           //SizeOf(SmallInt) * nChannels;
    wBitsPerSample := 16;          //SizeOf(SmallInt) * 8;
    end;

  //fill nSamplesPerSec, nAvgBytesPerSec in WaveFmt
  SamplesPerSec := DEFAULTRATE;
  channelNr := 2;
end;


destructor TCustomSoundInOut.Destroy;
begin
  Enabled := false;
  inherited;
end;


procedure TCustomSoundInOut.Err(Txt: string);
begin
  raise ESoundError.Create(Txt);
end;


procedure TCustomSoundInOut.SetChannel(const Value: integer);
begin
     channelNr := Value;
end;

function TCustomSoundInOut.GetChannel: integer;
begin
     Result := channelNr;
end;



//------------------------------------------------------------------------------
//                            enable/disable
//------------------------------------------------------------------------------
//do not enable component at design or load time
procedure TCustomSoundInOut.SetEnabled(AEnabled: boolean);
begin
  if (not (csDesigning in ComponentState)) and
     (not (csLoading in ComponentState)) and
     (AEnabled <> FEnabled)
    then DoSetEnabled(AEnabled);
  FEnabled := AEnabled;
end;


//enable component after all properties have been loaded
procedure TCustomSoundInOut.Loaded;
begin
  inherited Loaded;

  if FEnabled and not (csDesigning in ComponentState) then
    begin
    FEnabled := false;
    SetEnabled(true);
    end;
end;


procedure TCustomSoundInOut.DoSetEnabled(AEnabled: boolean);
begin
  if AEnabled
    then
      begin
      //reset counts
      FBufsAdded := 0;
      FBufsDone := 0;
      //create waiting thread
      FThread := TWaitThread.Create(true);
      FThread.FreeOnTerminate := true;
      FThread.Owner := Self;
      //FThread.Priority := tpTimeCritical;
      //start
      FEnabled := true;
      try Start; except FreeAndNil(FThread); raise; end;
      //device started ok, wait for events
      FThread.Resume;
      end
    else
      begin
      FThread.Terminate;
      Stop;
      end;
end;





//------------------------------------------------------------------------------
//                              get/set
//------------------------------------------------------------------------------

procedure TCustomSoundInOut.SetSamplesPerSec(const Value: LongWord);
begin
  Enabled := false;

  with WaveFmt.wf do
    begin
    nSamplesPerSec := Value;
    nAvgBytesPerSec := nSamplesPerSec * nBlockAlign;
    end;
end;


function TCustomSoundInOut.GetSamplesPerSec: LongWord;
begin
  Result := WaveFmt.wf.nSamplesPerSec;
end;



procedure TCustomSoundInOut.SetDeviceID(const Value: UINT);
begin
  Enabled := false;
  FDeviceID := Value;
end;



function TCustomSoundInOut.GetThreadID: THandle;
begin
  Result := FThread.ThreadID;
end;


function TCustomSoundInOut.GetBufCount: LongWord;
begin
  Result := Length(Buffers);
end;

procedure TCustomSoundInOut.SetBufCount(const Value: LongWord);
begin
  if Enabled then
    raise Exception.Create('Cannot change the number of buffers for an open audio device');
  SetLength(Buffers, Value);
end;




{$ENDIF}

end.
