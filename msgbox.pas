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

// 3D accelerated message box
unit msgbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { Tdlg_mbox }

  Tdlg_mbox = class ( TForm )
    Bevel1: TBevel;
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    procedure FormCreate  (Sender: TObject );
  private
    { private declarations }
  public
    { public declarations }
  end;

  procedure my_msgbox ( const msg : string );

var
  dlg_mbox: Tdlg_mbox;

implementation

uses misc;

{$R *.lfm}

{ Tdlg_mbox }

// custom messagebox
procedure my_msgbox ( const msg : string );
var
  dlg : Tdlg_mbox;
begin
  dlg := Tdlg_mbox.create ( nil );
  try
    dlg.Label1.Caption := msg;
    dlg.ShowModal;
  finally
    dlg.free;
  end;
end;

procedure Tdlg_mbox.FormCreate ( Sender: TObject );
begin
  Caption := app_name;
end;

end.

