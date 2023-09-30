//------------------------------------------------------------------------------
//This Source Code Form is subject to the terms of the Mozilla Public
//License, v. 2.0. If a copy of the MPL was not distributed with this
//file, You can obtain one at http://mozilla.org/MPL/2.0/.
//------------------------------------------------------------------------------
unit WavFile;

{$MODE Delphi}

interface

uses
  Classes, Dialogs, SysUtils, CallLst, fpwavreader;

type
  TWav = class
  public
    Data: array of byte;
    len: integer;
    lenMax: integer;
    aptr1: integer;
    aptr2: integer;
    rms: double;
    wavLoaded : boolean;
    procedure LoadFile(fileName : string; showMsg: boolean);
    constructor Create();
  end;

var
  Wav: TWav;

implementation

{ TWav }

constructor TWav.Create;
begin
  len := 0;
  aptr1 := 0;
  aptr2 := 0;
   wavLoaded := False;
end;

// load a WAV file for background noise. Must be
// mono 22050 Hz sampling rate
procedure TWav.LoadFile(fileName : string; showMsg: boolean);
var
  i	    : integer;
  WavReader : TWavReader;
   msg	    :  string;
begin
  if FileExists(fileName) = False then
  begin
    len := 0;
    aptr1 := 0;
    aptr2 := 0;
    showMessage('ERROR:' + LineEnding + 'WAV file ' + fileName + ' not found');
    wavLoaded := False;
    Exit;
  end;

  WavReader := TWavReader.Create;
  try
    if not WavReader.LoadFromFile(fileName) then
    begin
       {$ifdef Linux}
       writeln('Error loading wav file');
       {$endif}
       showMessage('ERROR:' + LineEnding + ' WAV file ' + fileName + ' invalid');
       wavLoaded := False;      
       exit;
    end;

    if WavReader.fmt.SampleRate <> 22050 then
       begin
	  showMessage('ERROR:' + LineEnding + ' Incorrect sample rate, must be 22050 Hz');
	  FreeAndNil(WavReader);
          wavLoaded := False;
	  exit;
       end;
    if WavReader.fmt.Channels <> 1 then
       begin
	  showMessage('ERROR:' + LineEnding + ' Incorrect number of channels, must be mono');
	  FreeAndNil(WavReader);
          wavLoaded := False;
	  exit;
       end;
    if WavReader.fmt.BitsPerSample <> 16 then
       begin
	  showMessage('ERROR:' + LineEnding + ' Incorrect bits/sample, must be 16');
	  FreeAndNil(WavReader);
	  wavLoaded := False;
	  exit;
       end;

    // read up to 30 seconds of data
    // 30 sec * 2 bytes/sample * samples/sec
    // (must be mono wav file, not stereo)
    lenMax := WavReader.fmt.SampleRate * 30 * 2;

    SetLength(Data, lenMax);
    len := WavReader.ReadBuf(Data[0], length(Data));

    if showMsg then
       begin
	  msg := 'WAV File' + LineEnding;
	  msg := msg + ' Read ' + IntToStr(len div (WavReader.fmt.SampleRate*2)) + ' sec audio' + LineEnding;
	  msg := msg + ' sample rate = ' + IntToStr(WavReader.fmt.SampleRate) + ' Hz' + LineEnding;
	  msg := msg + ' channels = ' + IntToStr(WavReader.fmt.Channels) + LineEnding;
	  showMessage(msg);
       end;

    // calculate rms amplitude
    rms := 0.0;
    for i := 0 to len do
    begin
      rms := rms + Data[i] * Data[i];
    end;
    rms := rms / len;
    rms := sqrt(rms);

    {$ifdef Linux}
    writeln('File ',fileName);
    writeln('  sample rate=',WavReader.fmt.SampleRate);
    writeln('  channels=',WavReader.fmt.Channels);
    writeln('  bits per sample=',WavReader.fmt.BitsPerSample);
    writeln('  data max len=',lenMax);
    writeln('  data length read=',len);
    writeln('  rms amplitude=',rms);
    {$endif}

    // reset pointers in audio for each radio
    // offset pointer for 2nd radio to give different noise
    aptr1 := 0;
    aptr2 := len div 2;
    wavLoaded := True;

   finally
    FreeAndNil(WavReader);
  end;

end;

end.
