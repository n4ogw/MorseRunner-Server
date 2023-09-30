//------------------------------------------------------------------------------
//This Source Code Form is subject to the terms of the Mozilla Public
//License, v. 2.0. If a copy of the MPL was not distributed with this
//file, You can obtain one at http://mozilla.org/MPL/2.0/.
//------------------------------------------------------------------------------
unit Mixers;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, SndTypes;

type


  TModulator = class
  private
    FSamplesPerSec: integer;
    FCarrierFreq: single;
    FSampleNo: integer;
    Sn, Cs: TSingleArray;
    FGain: single;
    procedure SetSamplesPerSec(const Value: integer);
    procedure SetCarrierFreq(const Value: single);
    procedure CalcSinCos;
    procedure SetGain(const Value: single);
  public
    constructor Create;
    function Modulate(Data: TSingleArray): TSingleArray; overload;
    function Modulate(Data: TReImArrays): TSingleArray; overload;
    function Modulate(Data: TComplexArray): TSingleArray; overload;
    property SamplesPerSec: integer read FSamplesPerSec write SetSamplesPerSec;
    property CarrierFreq: single read FCarrierFreq write SetCarrierFreq;
    property Gain: single read FGain write SetGain;
  end;



implementation





//------------------------------------------------------------------------------
//                              TModulator
//------------------------------------------------------------------------------

{ TModulator }

constructor TModulator.Create;
begin
  FCarrierFreq := 600;
  FSamplesPerSec := 5512;
  FGain := 1;
  CalcSinCos;
  FSampleNo := 0;
end;


procedure TModulator.SetCarrierFreq(const Value: single);
begin
  FCarrierFreq := Value;
  CalcSinCos;
  FSampleNo := 0;
end;


procedure TModulator.SetGain(const Value: single);
begin
  CalcSinCos;
  FGain := Value;
end;

procedure TModulator.SetSamplesPerSec(const Value: integer);
begin
  FSamplesPerSec := Value;
  CalcSinCos;
  FSampleNo := 0;
end;


procedure TModulator.CalcSinCos;
var
  Cnt: integer;
  dFi: single;
  i: integer;
begin
  Cnt := Round(FSamplesPerSec / FCarrierFreq);
  FCarrierFreq := FSamplesPerSec / Cnt;
  dFi := TWO_PI / Cnt;

  SetLength(Sn, Cnt);
  SetLength(Cs, Cnt);

  Sn[0] := 0;
  Sn[1] := Sin(dFi);
  Cs[0] := 1;
  Cs[1] := Cos(dFi);

  //phase
  for i := 2 to Cnt - 1 do
  begin
    Cs[i] := Cs[1] * Cs[i - 1] - Sn[1] * Sn[i - 1];
    Sn[i] := Cs[1] * Sn[i - 1] + Sn[1] * Cs[i - 1];
  end;

  //gain
  for i := 0 to Cnt - 1 do
  begin
    Cs[i] := Cs[i] * FGain;
    Sn[i] := Sn[i] * FGain;
  end;
end;


function TModulator.Modulate(Data: TReImArrays): TSingleArray;
var
  i: integer;
begin
  //Integer(Result) := 0;
  SetLength(Result, Length(Data.Re));
  for i := 0 to High(Result) do
  begin
    Result[i] := Data.Re[i] * Sn[FSampleNo] - Data.Im[i] * Cs[FSampleNo];
    FSampleNo := (FSampleNo + 1) mod Length(Cs);
  end;
end;


function TModulator.Modulate(Data: TSingleArray): TSingleArray;
var
  i: integer;
begin
  //Integer(Result) := 0;
  SetLength(Result, Length(Data));
  for i := 0 to High(Result) do
  begin
    Result[i] := Data[i] * Cs[FSampleNo];
    FSampleNo := (FSampleNo + 1) mod Length(Cs);
  end;
end;


function TModulator.Modulate(Data: TComplexArray): TSingleArray;
var
  i: integer;
begin
  //Integer(Result) := 0;
  SetLength(Result, Length(Data));
  for i := 0 to High(Result) do
  begin
    Result[i] := Data[i].Re * Sn[FSampleNo] - Data[i].Im * Cs[FSampleNo];
    FSampleNo := (FSampleNo + 1) mod Length(Cs);
  end;
end;



end.
