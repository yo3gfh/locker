(*
    Locker, v. 1.0 - small app to keep some text away from prying eyes =)
    ---------------------------------------------------------------------
    Copyright (c) 2017-2020 Adrian Petrila, YO3GFH
    Built with Lazarus IDE
    Uses DCPCrypt package for Lazarus, (c) David Barton

    http://www.cityinthesky.co.uk/opensource/dcpcrypt/

    Go to Package->Online Package Manager and install DCPCrypt
    TNX to all good ppl who ported it to Lazarus!

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
    the editor, also making a backup.

    * uses Rijndael and SHA512 for encryption. It's done based on a password
    of your choice.

    It's taylored to my own needs, modify it to suit your own. I'm not a professional programmer,
    so this isn't the best code you'll find on the web, you have been warned :-))

    All the bugs are guaranteed to be genuine, and are exclusively mine =)
*)

// main program and app window
unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DCPsha512, DCPrijndael, Forms, Controls,
  Graphics, Dialogs, ActnList, ComCtrls, Menus, StdCtrls, inifiles, strutils;

const
  //sha512 is 512, sha256 is 256, sha1 is 160
  KEYBITS               = 512;
  KEYSIZE               = KEYBITS div 8;
  SHALEN                = KEYSIZE * 2;

type

  { TKey }
  TKey = array[0..KEYSIZE-1] of byte; //to hold password digest

  { Tmainform }
  Tmainform = class ( TForm )
    a_about: TAction;
    a_quit: TAction;
    a_change_pass: TAction;
    al: TActionList;
    memo: TMemo;
    mmenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    sb: TStatusBar;
    procedure a_aboutExecute       ( Sender: TObject );
    procedure a_change_passExecute ( Sender: TObject );
    procedure a_quitExecute        ( Sender: TObject );
    procedure FormClose            ( Sender: TObject; var CloseAction: TCloseAction );
    procedure FormCreate           ( Sender: TObject );
  private
    { private declarations }
  public
    function hash_pass             ( const pass: string ) : string;
    function is_password_blank : boolean;
    function load_password : string;
    procedure save_password        ( const pass : string );
    procedure check_for_config;
    function new_password          ( const msg : string ) : boolean;
    function change_password       ( const msg : string ) : boolean;
    function verify_password       ( const msg : string ) : integer;
    function hash_to_key           ( const hash: string; out kkey: TKey ) : boolean;
    function encrypt_data : integer;
    function decrypt_data : integer;
    function worth_encrypting : boolean;
    function backup_data : boolean;
  end;

var
  mainform: Tmainform;

implementation

uses passwd, misc, msgbox;

{$R *.lfm}

{ Tmainform }

// make a copy of our .dat file
function Tmainform.backup_data : boolean;
var
  dt : TDateTime;
  crt_date_str : string;
begin
  result := false;
  if not fileexists ( app_data ) then exit;

  // try and make a filename from current date
  dt := now;
  crt_date_str := '';
  crt_date_str := FormatDateTime ( date_fmt, dt );
  if crt_date_str = '' then exit;

  // backup file
  result := CopyFile ( app_data, crt_date_str + app_data_bak );
end;

// must we really work?
function Tmainform.worth_encrypting : boolean;
begin
  result := ( memo.Lines.Count > 0 ) ;
end;

{$hints off}
// data decryption; returns used keysize or '0' on error
function Tmainform.decrypt_data : integer;
var
  in_fs: TFileStream;            // encrypted data from disk
  out_ms: TMemoryStream;         // will hold decrypted data
  kkey: TKey;                    // vector to hold pass digest
  dcp_rij: TDCP_rijndael;        // cypher object
  key_size: integer;
begin
  result := 0;

  // zero vector
  FillChar ( kkey, KEYSIZE, 0 );

  // convert sha512 hex digest saved to inifile to vector
  if not hash_to_key ( load_password, kkey ) then exit;

  in_fs := TFileStream.Create ( app_data, fmOpenRead );
  out_ms := TMemoryStream.Create;
  dcp_rij := TDCP_rijndael.Create ( self );

  try
    // choose maximum possible key size (256 in current implementation)
    key_size := dcp_rij.GetMaxKeySize;
    if key_size > KEYBITS then key_size := KEYBITS;

    // init, decrypt and clean up
    dcp_rij.Init ( kkey, key_size, nil );
    dcp_rij.DecryptStream ( in_fs, out_ms, in_fs.Size );
    dcp_rij.Reset;
    dcp_rij.Burn;

    // important, set pos. to the start of the decrypted stream :-)
    // ...and load data to memo
    out_ms.Seek ( 0, soFromBeginning );
    memo.Lines.Clear;
    memo.Lines.LoadFromStream ( out_ms );
    result := key_size;
  except
    on e : exception do
    begin
      my_msgbox ( e.Message );
    end;
  end;

  in_fs.free;
  out_ms.free;
  dcp_rij.free;

end;
{$hints on}

{$hints off}
// data encryption; returns used keysize or '0' on error
function Tmainform.encrypt_data : integer;
var
  in_ms: TMemoryStream;           // data to fuckup
  out_fs: TFilestream;            // file to write
  dcp_rij: TDCP_rijndael;         // cypher object
  kkey: TKey;                     // vector
  key_size: integer;
begin
  result := 0;
  if worth_encrypting then
  begin
    FillChar ( kkey, KEYSIZE, 0 );

    // convert sha512 hex digest saved to inifile to vector
    if not hash_to_key ( load_password, kkey ) then exit;

    in_ms := TMemoryStream.Create;
    out_fs := TFileStream.Create ( app_data, fmCreate );
    dcp_rij := TDCP_rijndael.Create ( self );

    try
      // save data to stream for encryption
      memo.Lines.SaveToStream ( in_ms );

      // start from the beginning, or else...
      in_ms.Seek ( 0, soFromBeginning );

      // use max. possible key size
      key_size := dcp_rij.GetMaxKeySize;
      if key_size > KEYBITS then key_size := KEYBITS;

      // init, encrypt and clean up
      dcp_rij.Init ( kkey, key_size, nil );
      dcp_rij.EncryptStream ( in_ms, out_fs, in_ms.Size );
      dcp_rij.Reset;
      dcp_rij.Burn;
      result := key_size;
    except
      on e : exception do
      begin
        my_msgbox ( e.Message );
      end;
    end;

    in_ms.free;
    out_fs.free;
    dcp_rij.free;

  end;
end;
{$hints on}

// take a 128 byte sha512 hex string and make it back into a vector
function Tmainform.hash_to_key ( const hash: string; out kkey: TKey ) : boolean;
var
  i, j: integer;
  t: string;
begin
  result := false;
  if length ( hash ) <> SHALEN then exit;
  j := 1;
  for i := 0 to KEYSIZE-1 do
    begin
      t := midstr ( hash, j, 2 );
      kkey[i] := Hex2Dec ( t );
      j := j + 2;
    end;
  result := true;
end;

// incredibly complex function for reading pass from config file
function Tmainform.load_password : string;
var
  ini: TIniFile;
begin
  result := '';
  ini := TIniFile.Create ( app_ini );
  try
    result := ini.ReadString ( ini_pass_section, ini_pass_value, '' );
  finally
    ini.free;
  end;
end;

// incredibly complex function for saving pass to config file
procedure Tmainform.save_password ( const pass : string );
var
  ini: TIniFile;
begin
  ini := TIniFile.Create ( app_ini );
  try
    ini.WriteString ( ini_pass_section, ini_pass_value, pass );
  finally
    ini.free;
  end;
end;

// lol
function Tmainform.is_password_blank : boolean;
var
  s : string;
begin
  s := load_password;
  result := ( s = '' );
end;

// create a new sha512 from a password; true on success, false otherwise
function Tmainform.new_password ( const msg : string ) : boolean;
var
  dlg: Tdlg_pass;
  n, p_new: string;
begin
  dlg := Tdlg_pass.create ( self );
  result := false;
  try
    // tell onkeyup handler that we don't need oldpass field
    dlg.tag := DLGTAG_NEW;

    // various init
    dlg.warning.Caption := msg;
    dlg.eold.Enabled := false;
    dlg.ShowModal;
    if dlg.ModalResult = mrok then
    begin
      // hash password and save it to ini
      p_new := dlg.enew.Text;
      n := hash_pass ( p_new );
      if n <> '' then
      begin
        save_password ( n );
        result := true;
      end;
    end;
  finally
    dlg.free;
  end;
end;

// change password; verifies old password and make sha512 from new one
// true on success, false otherwise
function Tmainform.change_password ( const msg : string ) : boolean;
var
  dlg: Tdlg_pass;
  o, n, s, p_old, p_new: string;
begin
  dlg := Tdlg_pass.create ( self );
  result := false;
  try
    // tell onkeyup handler we're using all fields
    dlg.Tag := DLGTAG_CHANGE;

    // init stuff
    dlg.warning.Caption := msg;

    dlg.ShowModal;

    if dlg.ModalResult = mrok then
    begin
      // make hash-hash from user inputs
      p_old := dlg.eold.Text;
      p_new := dlg.enew.Text;
      o := hash_pass ( p_old );
      n := hash_pass ( p_new );

      // load old pass from ini
      s := load_password;

      // if it's somehow empty, don't bother, save new one and be done with it
      if s = '' then
      begin
        if n <> '' then save_password(n);
        result := true;
      end
      else
      begin
        if o <> '' then
        begin  // check if old=new
          if o = s then
          begin
            save_password ( n );
            result := true;
          end
            else my_msgbox ( err_bad_old_pass );
        end;
      end;
    end;
  finally
    dlg.free;
  end;
end;

// verfy existing password;
// possible return status: VP_OK, VP_CANCELED, VP_BADPASS
function Tmainform.verify_password ( const msg : string ) : integer;
var
  dlg: Tdlg_pass;
  n, s, p_new: string;
begin
  dlg := Tdlg_pass.create ( self );
  result := VP_CANCELED;
  try
    // tell we're using just the verify pass field
    dlg.Tag := DLGTAG_VERIFY;

    // init
    dlg.warning.Caption := msg;

    dlg.eold.Enabled := false;  // disable and hide unused fields
    dlg.eold.Visible := false;
    dlg.ecfm.Enabled := false;
    dlg.ecfm.Visible := false;

    dlg.Label1.Caption := '';
    dlg.Label3.Caption := '';
    dlg.Label2.Caption := pass_dlg_pwd_text;

    dlg.ShowModal;

    if dlg.ModalResult = mrok then
    begin
      p_new := dlg.enew.Text;
      result := VP_BADPASS;

      // hash pass
      n := hash_pass ( p_new );

      // ..and check against existing
      s := load_password;

      if s <> '' then
      begin
        if s = n then result := VP_OK;
      end;
    end;
  finally
    dlg.free;
  end;
end;

// see if we have ini, create one if don't
procedure Tmainform.check_for_config;
var
  ini: TIniFile;
begin
  if not fileexists ( app_ini ) then
  begin
    ini := TIniFile.Create ( app_ini );
    try
      ini.WriteString ( ini_pass_section, ini_pass_value, '' );
    finally
      ini.free;
    end;
  end;
end;

{$hints off}
// make sha512 hex string from a password
function Tmainform.hash_pass ( const pass: string ) : string;
var
  digest: TKey;           // digest vector
  i: integer;
  s: string;
  sha512: TDCP_sha512;    // SHA object
begin
  result := '';

  // check for crap
  if pass = '' then exit;

  // zero vector
  FillChar ( digest, KEYSIZE, 0 );

  sha512 := TDCP_sha512.Create ( self );

  try
    // make digest
    sha512.Init;
    sha512.UpdateStr ( pass );
    sha512.Final ( digest );

    s := '';

    // make nice hex
    for i := 0 to KEYSIZE-1 do
      s := s + IntToHex ( digest[i], 2 );

    result := s;

  finally
    sha512.Free;
  end;
end;
{$hints on}

// quit
procedure Tmainform.a_quitExecute ( Sender: TObject );
begin
  Close;
end;

{$hints off}
procedure Tmainform.FormClose ( Sender: TObject; var CloseAction: TCloseAction );
begin
  if worth_encrypting then
  begin
    check_for_config;
    backup_data;
    if is_password_blank then        // did we erase ini while app running?
    begin
      if new_password ( warn_no_def_encr_pass ) then encrypt_data;
    end
    else
      encrypt_data;
  end;
end;
{$hints on}

procedure Tmainform.FormCreate ( Sender: TObject );
var
  vp_result: Integer;

begin
  Caption := app_name;
  // make empty ini if none found
  check_for_config;

  // is there something to decrypt?
  if FileExists ( app_data ) then
  begin
    // don't have old pass anymore?
    if is_password_blank then
    begin
      // write one back, hope it is the same
      if new_password ( warn_no_def_pass ) then
      begin
        decrypt_data;
      end;
    end
    else
    begin
      // we have a password, ask for it
      vp_result := verify_password ( msg_ver_pass );

      if vp_result = VP_OK then
      begin
         decrypt_data;
         Exit;
      end;

      if vp_result = VP_BADPASS then
      begin
        my_msgbox ( err_bad_pass );
        Application.Terminate;
        Exit;
      end;

      if vp_result = VP_CANCELED then
      begin
        Application.Terminate;
        Exit;
      end;
    end;
  end;
end;

procedure Tmainform.a_change_passExecute ( Sender: TObject );
begin
  change_password ( msg_change_pass );
end;

procedure Tmainform.a_aboutExecute ( Sender: TObject );
begin
  my_msgbox ( about_txt );
end;

end.

