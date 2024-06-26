//------------------------------------------------------------------------------
//This Source Code Form is subject to the terms of the Mozilla Public
//License, v. 2.0. If a copy of the MPL was not distributed with this
//file, You can obtain one at http://mozilla.org/MPL/2.0/.
//------------------------------------------------------------------------------
unit Qsb;

{$MODE Delphi}

interface

uses
  SysUtils, QuickAvg, SndTypes, RndFunc, Math, Ini;

type
  TQsb = class
  private
    Filt: TQuickAverage;
    FGain: single;
    FBandwidth: single;
    function NewGain: single;
    procedure SetBandwidth(const Value: single);
  public
    QsbLevel: single;
    constructor Create;
    procedure ApplyTo(Arr: TSingleArray);
    property Bandwidth: single read FBandwidth write SetBandwidth;
  end;


implementation


constructor TQsb.Create;
begin
  Filt := TQuickAverage.Create(nil);
  Filt.Passes := 3;
  QsbLevel := 1;//0.5 + 0.5 * RndUShaped;
  Bandwidth := 0.1;
end;

function TQsb.NewGain: single;
begin
  with Filt.Filter(RndUniform, RndUniform) do
    Result := Sqrt((Sqr(Re) + Sqr(Im)) * 3 * Filt.Points);
  Result := Result * QsbLevel + (1 - QsbLevel);
end;


procedure TQsb.SetBandwidth(const Value: single);
var
  i: integer;
begin
  FBandwidth := Value;
  Filt.Points := Ceil(0.37 * DEFAULTRATE / ((Ini.BufSize div 4) * Value));
  for i := 0 to Filt.Points * 3 do FGain := NewGain;
end;


procedure TQsb.ApplyTo(Arr: TSingleArray);
var
  b, i: integer;
  dG: single;
  BlkCnt: integer;
begin
  BlkCnt := Length(Arr) div (Ini.BufSize div 4);

  for b := 0 to BlkCnt - 1 do
  begin
    dG := (NewGain - FGain) / (Ini.BufSize div 4);
    for i := 0 to (Ini.BufSize div 4) - 1 do
    begin
      Arr[b * (Ini.BufSize div 4) + i] := Arr[b * (Ini.BufSize div 4) + i] * FGain;
      FGain := FGain + dG;
    end;
  end;
end;



end.
