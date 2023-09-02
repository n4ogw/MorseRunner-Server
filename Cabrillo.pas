//------------------------------------------------------------------------------
//This Source Code Form is subject to the terms of the Mozilla Public
//License, v. 2.0. If a copy of the MPL was not distributed with this
//file, You can obtain one at http://mozilla.org/MPL/2.0/.
//------------------------------------------------------------------------------
unit Cabrillo;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, StrUtils, CallLst;

type
  TCabData  = record
		 Call : string;
		 exch : array [1..4] of string;
	      end;    
  PTCabData = ^TCabData;

TCabrillo = class
  private
   FnExch : integer;
   nCab   : integer;
   currentIndx : integer;
   CabData :array of TCabData;
   
  public
   currentExch : string;
   procedure LoadFile(fileName :  string);
   procedure SetNExch(const Value: integer);
   function GetNExch: integer;
   function PickCall: string;
   property nExch: integer read GetNExch write SetNExch;
   constructor Create;

  end;

var
   Cab	   : TCabrillo;
   CabData :  array of TCabData;
   
implementation

uses
   Main;


{ TCabrillo }

function CompareCalls(Item1, Item2: Pointer): integer;
var
   p1,p2 : PTCabData;
begin
   p1 := Item1;
   P2 := Item2;
   Result := CompareStr(p1^.call, p2^.call);
end;

  
function TCabrillo.GetNExch: integer;
begin
   Result := FnExch;
end;

procedure TCabrillo.SetNExch(const Value: integer);
begin
   FnExch := Value;
end;

procedure TCabrillo.LoadFile(fileName :  string);
var
   F	  : text;
   s	  : String;
   fields : TStringArray;
   i,j	  : integer;
   L	  : TList;
   P1     : ^TCabData;
   rec	  : ^TCabData;
begin
   nCab := 0;
   FnExch := 0;
   currentIndx := 0;
   if FileExists(fileName) = False then
      Exit;

   assign(F, fileName);
   reset(F);
   L:= TList.Create;
   L.capacity := 5000;
   while not EOF(F) do
      begin
	 readln(F,s);
	 s := Delspace1(trim(s));
	 fields:=s.Split(' ');
	 if fields[0]='QSO:' then
	    begin
	       nCab := nCab + 1;
	       // number of exchange fields
	       if FnExch = 0 then
		  FnExch := (length(fields)-7) div 2;
	       
	       rec := New(PTCabData);
	       rec^.call :=  fields[length(fields) - FnExch - 1];
	       j := 1;
	       for i := (length(fields) - FnExch) to (length(fields)-1) do
		  begin
		     rec^.exch[j] := fields[i];
		     j := j + 1;
		  end;
	       P1 := rec;
	       L.Add(TObject(P1));
	    end;
      end;
   {$IFDEF Linux}
   writeln(nCab,' qsos read. ',FnExch,' exchange fields');
   {$ENDIF}
   close(F);
   //delete dupes
   L.Sort(CompareCalls);
   for i := L.Count - 1 downto 1 do
      if CompareCalls(L[i],L[i-1]) = 0 then
	 L[i] := nil;
   nCab :=0;
   for i := 0 to L.Count - 1  do
      if L[i] <> nil then
	 nCab := nCab + 1;
   {$IFDEF Linux}
   writeln(nCab,' non-dupe calls');
   {$ENDIF}
   // copy into array
   SetLength(CabData,nCab);
   j := 0;
   for i:=0 to L.Count -1 do
      begin
	 if (L[i] <> nil) then
	    begin
	       p1 := L[i];
	       CabData[j].call := p1^.call;
	       CabData[j].exch[1] := p1^.exch[1];
	       CabData[j].exch[2] := p1^.exch[2];
	       j := j+1;
	    end;
      end;
   L.free;
end;  

function TCabrillo.PickCall: string;
var
   i :  integer;
begin
  if nCab = 0 then
  begin
     currentIndx := 0;
     Result := 'N4OGW';
     currentExch := '599 TT1';
     Exit;
  end;
   
  currentIndx := Random(nCab);
  Result := CabData[currentIndx].call;
  currentExch := '';
  for i:=1 to FnExch do
  begin
     currentExch := currentExch + CabData[currentIndx].exch[i];
     if i < FnExch then
	currentExch := currentExch + ' ';
  end;
end;


constructor TCabrillo.Create;
begin
   FnExch := 0;
   nCab := 0;
   currentIndx := 0;
end;

end.


