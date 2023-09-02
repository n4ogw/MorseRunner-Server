//------------------------------------------------------------------------------
//This Source Code Form is subject to the terms of the Mozilla Public
//License, v. 2.0. If a copy of the MPL was not distributed with this
//file, You can obtain one at http://mozilla.org/MPL/2.0/.
//------------------------------------------------------------------------------
unit Setup;

{$mode Delphi}

interface

uses
  {$ifdef Windows}
  //LazLogger,
  {$endif}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ueled, Ini;

var
   serialModeSave	  : TSerialMode;
   serialPortWinkeySave	  : string;
   serialPortOtrspSave	  : string;
   serialPortSo2rminiSave : string;
   modeSave		  : TOpMode;
   
type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckGroup1: TCheckGroup;
    CheckGroup2: TCheckGroup;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    uELED1: TuELED;
    uELED2: TuELED;
    uELED3: TuELED;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
    procedure CheckGroup2ItemClick(Sender: TObject; Index: integer);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Label1Click(Sender: TObject);
  private

  public

  end;

var
  Form1	: TForm1;

implementation

uses Main;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
   if key = #13 then Button3Click(Sender); // enter -> OK
   if key = #27 then Button1Click(Sender); // esc -> cancel
end;

procedure TForm1.Label1Click(Sender: TObject);
begin

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

// cancel button
procedure TForm1.Button1Click(Sender: TObject);
begin
   // reset to previous settings
   Ini.serialMode := serialModeSave;
   Ini.serialPortWinkey := serialPortWinkeySave;
   Ini.serialPortOtrsp := serialPortOtrspSave;
   Ini.serialPortSo2rmini := serialPortSo2rminiSave;
   MainForm.serialInit;
   MainForm.SetSerialMode(Ini.serialMode);
   Close;
end;

// accept button
procedure TForm1.Button2Click(Sender: TObject);
begin
   Ini.serialPortWinkey := Trim(Edit1.Text);
   Ini.serialPortOtrsp := Trim(Edit2.Text);
   Ini.serialPortSo2rMini := Trim(Edit3.Text);
   MainForm.serialInit;
   MainForm.SetSerialMode(Ini.serialMode);
end;

// OK button
procedure TForm1.Button3Click(Sender: TObject);
begin
   Ini.serialPortWinkey := Trim(Edit1.Text);
   Ini.serialPortOtrsp := Trim(Edit2.Text);
   Ini.serialPortSo2rMini := Trim(Edit3.Text);
   MainForm.serialInit;
   MainForm.SetSerialMode(Ini.serialMode);
   Close;
end;

procedure TForm1.CheckGroup1ItemClick(Sender: TObject; Index: integer);
begin
   if Index = 0 then
      begin
	 // prevent unchecking if other is unchecked
	 if ((CheckGroup1.Checked[0] = False) and (CheckGroup1.Checked[1] = False)) then
	    CheckGroup1.Checked[0] := True;
	 Edit1.Enabled := True;
	 Edit2.Enabled := True;
	 Edit3.Enabled := False;
	 CheckGroup1.Checked[1] := False;
	 Ini.serialMode := modeWinkey;
      end
   else
      begin
 	 // prevent unchecking if other is unchecked
	 if ((CheckGroup1.Checked[0] = False) and (CheckGroup1.Checked[1] = False)) then
	    CheckGroup1.Checked[1] := True;
	 Edit1.Enabled := False;
	 Edit2.Enabled := False;
	 Edit3.Enabled := True;
	 CheckGroup1.Checked[0] := False;
	 Ini.serialMode := modeSo2rmini;
      end;
   MainForm.serialInit;
   MainForm.SetSerialMode(Ini.serialMode);
end;

procedure TForm1.CheckGroup2ItemClick(Sender: TObject; Index: integer);
begin
   if Index = 0 then
      begin
	 // prevent unchecking if other is unchecked
	 if ((CheckGroup2.Checked[0] = False) and (CheckGroup2.Checked[1] = False)) then
	    CheckGroup2.Checked[0] := True;
	 CheckGroup2.Checked[1] := False;
	 Ini.opMode := opWPX;
      end
   else
      begin
 	 // prevent unchecking if other is unchecked
	 if ((CheckGroup2.Checked[0] = False) and (CheckGroup2.Checked[1] = False)) then
	    CheckGroup2.Checked[1] := True;
	 CheckGroup2.Checked[0] := False;
	 Ini.opMode := opCab;
      end;
end;


end.

