//------------------------------------------------------------------------------
//This Source Code Form is subject to the terms of the Mozilla Public
//License, v. 2.0. If a copy of the MPL was not distributed with this
//file, You can obtain one at http://mozilla.org/MPL/2.0/.
//------------------------------------------------------------------------------
unit Contest;

{$MODE Delphi}

interface

uses
  SysUtils, SndTypes, Station, StnColl, MyStn, Math, Ini,
  MovAvg, Mixers, VolumCtl, RndFunc, TypInfo, DxStn, DxOper, Log;

type
  TContest = class
  private
    function DxCount: integer;
    procedure SwapFilters;
  public
    BlockNumber: integer;
    Me: TMyStation;
    Stations: TStations;
    Agc: TVolumeControl;
    Filt, Filt2: TMovingAverage;
    Modul: TModulator;
    RitPhase: single;
    FStopPressed: boolean;

    constructor Create(radioNr : integer) ;
    destructor Destroy; override;
    procedure Init;
    function Minute: single;
    function GetAudio: TSingleArray;
    procedure OnMeFinishedSending;
    procedure OnMeStartedSending;
  end;

var
   Tst: array [1..2] of TContest;

implementation

uses
  Main;

{ TContest }

constructor TContest.Create(radioNr: integer);
begin
  Me := TMyStation.CreateStation(radioNr);
  Stations := TStations.Create(radioNr);
  Filt := TMovingAverage.Create(nil);
  Modul := TModulator.Create;
  Agc := TVolumeControl.Create(nil);

  Filt.Points := Round(0.7 * DEFAULTRATE / Ini.BandWidth);
  Filt.Passes := 3;
  Filt.SamplesInInput := Ini.BufSize;
  Filt.GainDb := 10 * Log10(500 / Ini.Bandwidth);

  Filt2 := TMovingAverage.Create(nil);
  Filt2.Passes := Filt.Passes;
  Filt2.SamplesInInput := Filt.SamplesInInput;
  Filt2.GainDb := Filt.GainDb;

  Modul.SamplesPerSec := DEFAULTRATE;
  Modul.CarrierFreq := Ini.Pitch;

  Agc.NoiseInDb := 76;
  Agc.NoiseOutDb := 76;
  Agc.AttackSamples := 155;   //AGC attack 5 ms
  Agc.HoldSamples := 155;
  Agc.AgcEnabled := True;

  Init;
end;


destructor TContest.Destroy;
begin
  Me.Free;
  Filt.Free;
  Filt2.Free;
  Modul.Free;
  inherited;
end;


procedure TContest.Init;
begin
  Me.Init;
  Stations.Clear;
  BlockNumber := 0;
end;


function TContest.GetAudio: TSingleArray;
const
  NOISEAMP = 6000;
var
  ReIm: TReImArrays;
  Blk: TSingleArray;
  i, Stn: integer;
  Bfo: single;
  Smg, Rfg: single;
begin
  //minimize audio output delay
  SetLength(Result, 1);
  Inc(BlockNumber);
  if BlockNumber < 6 then Exit;

  //complex noise
  SetLengthReIm(ReIm, Ini.BufSize);
  for i := 0 to High(ReIm.Re) do
  begin
    ReIm.Re[i] := 3 * NOISEAMP * (Random - 0.5);
    ReIm.Im[i] := 3 * NOISEAMP * (Random - 0.5);
  end;

  //QRN
  if Ini.Qrn then
  begin
    //background
    for i := 0 to High(ReIm.Re) do
      if Random < 0.01 then ReIm.Re[i] := 60 * NOISEAMP * (Random - 0.5);
    //burst
    if Random < 0.01 then Stations.AddQrn;
  end;

  //QRM
  if Ini.Qrm and (Random < 0.0002) then Stations.AddQrm;


  //audio from stations
  Blk := nil;
  for Stn := 0 to Stations.Count - 1 do
    if Stations[Stn].State = stSending then
    begin
      Blk := Stations[Stn].GetBlock;
      for i := 0 to High(Blk) do
      begin
	 Bfo := Stations[Stn].Bfo;// - RitPhase - i * TWO_PI * Ini.Rit / DEFAULTRATE;
        ReIm.Re[i] := ReIm.Re[i] + Blk[i] * Cos(Bfo);
        ReIm.Im[i] := ReIm.Im[i] - Blk[i] * Sin(Bfo);
      end;
    end;

  //Rit
  //RitPhase := RitPhase + Ini.BufSize * TWO_PI * Ini.Rit / DEFAULTRATE;
  //while RitPhase > TWO_PI do RitPhase := RitPhase - TWO_PI;
  //while RitPhase < -TWO_PI do RitPhase := RitPhase + TWO_PI;

  //my audio
  if Me.State = stSending then
  begin
    Blk := Me.GetBlock;
    //self-mon. gain
    Smg := Power(10, (-70 + MainForm.TrackBar.position * 8) / 20.0);
    Rfg := 1;
    for i := 0 to High(Blk) do
      if Ini.Qsk then
      begin
        if Rfg > (1 - Blk[i] / Me.Amplitude) then
          Rfg := (1 - Blk[i] / Me.Amplitude)
        else
          Rfg := Rfg * 0.997 + 0.003;
        ReIm.Re[i] := Smg * Blk[i] + Rfg * ReIm.Re[i];
        ReIm.Im[i] := Smg * Blk[i] + Rfg * ReIm.Im[i];
      end
      else
      begin
        ReIm.Re[i] := Smg * (Blk[i]);
        ReIm.Im[i] := Smg * (Blk[i]);
      end;
  end;


  //LPF
  Filt2.Filter(ReIm);
  ReIm := Filt.Filter(ReIm);
  if (BlockNumber mod 10) = 0 then SwapFilters;

  //mix up to Pitch frequency
  Result := Modul.Modulate(ReIm);
  //AGC
  Result := Agc.Process(Result);

  //timer tick
  Me.Tick;
  for Stn := Stations.Count - 1 downto 0 do Stations[Stn].Tick;

  //if DX is done, write to log and kill
  for i := Stations.Count - 1 downto 0 do
    if Stations[i] is TDxStation then
      with Stations[i] as TDxStation do
      begin
        if (Oper.State = osDone) and (QsoList <> nil) and
          (MyCall = QsoList[High(QsoList)].Call) then
        begin
          DataToLastQso;
        end;
      end;

  if FStopPressed then
  begin
    if (RunMode = rmRun) and not FStopPressed then
    begin
      MainForm.Run(rmStop);
      FStopPressed := False;
    end
    else
    begin
      MainForm.Run(rmStop);
      FStopPressed := False;
    end;
    end;
end;


function TContest.DxCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := Stations.Count - 1 downto 0 do
    if (Stations[i] is TDxStation) and (TDxStation(Stations[i]).Oper.State <>
      osDone) then Inc(Result);
end;


function TContest.Minute: single;
begin
  Result := BlocksToSeconds(BlockNumber) / 60;
end;


procedure TContest.OnMeFinishedSending;
var
  i: integer;
begin
  //the stations heard my CQ and want to call
   if (msgCQ in Me.Msg) or ((QsoList <> nil) and (msgTU in Me.Msg) and
    (msgMyCall in Me.Msg)) then
      for i := 1 to RndPoisson(Activity / 2) do Stations.AddCaller;

  //tell callers that I finished sending
  for i := Stations.Count - 1 downto 0 do
    Stations[i].ProcessEvent(evMeFinished);
end;


procedure TContest.OnMeStartedSending;
var
  i: integer;
begin
  //tell callers that I started sending
  for i := Stations.Count - 1 downto 0 do
    Stations[i].ProcessEvent(evMeStarted);
end;


procedure TContest.SwapFilters;
var
  F: TMovingAverage;
begin
  F := Filt;
  Filt := Filt2;
  Filt2 := F;
  Filt2.Reset;
end;



end.
