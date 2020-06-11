(*
    Locker, v. 1.0 - small app to keep some text away from prying eyes =)
    ---------------------------------------------------------------------
    Copyright (c) 2020 Adrian Petrila, YO3GFH
    Built with Lazarus IDE
    Uses DCPCrypt package for Lazarus, (c) David Barton

    http://www.cityinthesky.co.uk/opensource/dcpcrypt/

    Go to Package->Online Package Manager and install DCPCrypt
    TNX to all good ppl to ported it to Lazarus!

    ----------------------------------------------------------------------
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

    Features
    --------
    * when you close it, it simply encrypts to a file whatever text you type in
    the editor, making also a backup.

    * uses Rijndael and SHA512 for encryption. It's done based on a password
    of your choice.

    It's taylored to my own needs, modify it to suit your own. I'm not a professional programmer,
    so this isn't the best code you'll find on the web, you have been warned :-))

    All the bugs are guaranteed to be genuine, and are exclusively mine =)
*)

// password processing dialog and code
unit passwd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { Tdlg_pass }

  Tdlg_pass = class ( TForm )
    Bevel1: TBevel;
    bn_ok: TButton;
    bn_cancel: TButton;
    eold: TEdit;
    enew: TEdit;
    ecfm: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    warning: TLabel;

    procedure enewKeyUp  ( Sender: TObject; var Key: Word; Shift: TShiftState );
    procedure FormCreate ( Sender: TObject );

  private
    { private declarations }
  public
    { public declarations }
    function is_input_ok : boolean;
  end;



var
  dlg_pass: Tdlg_pass;

implementation

uses misc;

{$R *.lfm}

{$hints off}
// check valid input at every keystroke, disable OK button until correct data
procedure Tdlg_pass.enewKeyUp ( Sender: TObject; var Key: Word; Shift: TShiftState );
begin
  bn_ok.Enabled := is_input_ok;
end;
{$hints on}

procedure Tdlg_pass.FormCreate(Sender: TObject);
begin
  caption := pass_dlg_title;
end;

// check various stuff depending of what edit controls are enabled
function Tdlg_pass.is_input_ok : boolean;
begin
  result := false;
  case Tag of
    DLGTAG_CHANGE : result := ( length ( enew.Text ) >= 8 ) and
                              ( length ( ecfm.Text ) >= 8 ) and
                              ( enew.Text = ecfm.Text ) and
                              ( enew.Text <> eold.Text );

    DLGTAG_NEW    : result := ( length ( enew.Text ) >= 8 ) and
                              ( length ( ecfm.Text ) >= 8 ) and
                              ( enew.Text = ecfm.Text );

    DLGTAG_VERIFY : result := ( length ( enew.Text ) >= 8 );
  end;
end;


end.

