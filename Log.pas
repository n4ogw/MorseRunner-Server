//------------------------------------------------------------------------------
//This Source Code Form is subject to the terms of the Mozilla Public
//License, v. 2.0. If a copy of the MPL was not distributed with this
//file, You can obtain one at http://mozilla.org/MPL/2.0/.
//------------------------------------------------------------------------------
unit Log;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics;

procedure Clear;

type
  TQso = record
    T: TDateTime;
    Call, TrueCall: string;
    Rst, TrueRst: integer;
    Nr, TrueNr: integer;
    Pfx: string;
    Dupe: boolean;
    Err: string;
  end;


var
  QsoList  : array of TQso;
  CallSent : array [1..2] of boolean;
  NrSent   : array [1..2] of boolean;

implementation

uses
  Contest, Main;

procedure Clear;
begin
  QsoList := nil;
  Tst[1].Stations.Clear;
  Tst[2].Stations.Clear;
end;


end.
