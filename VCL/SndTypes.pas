unit SndTypes;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Math{, ComObj};

const
  FOUR_PI = 4 * Pi;
  TWO_PI = 2 * Pi;
  HALF_PI = 0.5 * Pi;
  RinD = Pi / 180;
  SMALL_FLOAT = 1e-12;


type
  TByteArray = array of byte;
  TSmallIntArray = array of smallint;
  TIntegerArray = array of integer;
  TSingleArray = array of single;
  TSingleArray2D = array of TSingleArray;
  TDoubleArray = array of double;
  TBooleanArray = array of boolean;
  TExtendedArray = array of extended;
  PSingleArray = array of PSingle;

  TDataBufferF = array of TSingleArray;
  TDataBufferI = array of TIntegerArray;

  PHugeSingleArray = ^THugeSingleArray;
  THugeSingleArray = array[0..(MAXINT div SizeOf(single)) - 1] of single;

  PComplex = ^TComplex;

  TComplex = record
    Re, Im: single;
  end;

  TComplexArray = array of TComplex;

  TCplArr = array[0..MAXINT shr 8] of TComplex;
  PComplexArray = ^TCplArr;

  TReImArrays = record
    Re, Im: TSingleArray;
  end;

  ESoundError = class(Exception)
  end;



  PWaveBuffer = ^TWaveBuffer;

  TWaveBuffer = record
    used: integer;
    len: integer;
    Data: TSmallIntArray;
  end;

procedure SetLengthReIm(var Arr: TReImArrays; Len: integer);
procedure ClearReIm(var Arr: TReImArrays);

implementation

procedure SetLengthReIm(var Arr: TReImArrays; Len: integer);
begin
  SetLength(Arr.Re, Len);
  SetLength(Arr.Im, Len);
end;


procedure ClearReIm(var Arr: TReImArrays);
begin
  Arr.Re := nil;
  Arr.Im := nil;
end;



end.
