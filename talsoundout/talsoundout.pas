{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit talsoundout;

{$warn 5023 off : no warning about unused units}
interface

uses
  SndCustm, SndOut, SndTypes, sdl2, sdl2_gfx, sdl2_image, sdl2_mixer, 
  sdl2_net, sdl2_ttf, BaseComp, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('SndCustm', @SndCustm.Register);
  RegisterUnit('SndOut', @SndOut.Register);
end;

initialization
  RegisterPackage('talsoundout', @Register);
end.
