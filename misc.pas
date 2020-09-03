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

// various stuff
unit misc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


const
  // for telling what editbox to check
  DLGTAG_CHANGE         = 1; // password dlg. used for pass change
  DLGTAG_NEW            = 2; // password dlg. used for first password set
  DLGTAG_VERIFY         = 3; // password dlg. used for pass check at startup

  // status (return) values for verify_password function
  VP_CANCELED           = 4; // user clicked Cancel
  VP_BADPASS            = 5; // passwords do not match
  VP_OK                 = 6; // all is well

resourcestring
  app_name              = 'Locker v1.0';
  pass_dlg_title        = 'Password management';
  about_txt             = 'Locker v1.0 - amazing personal encryption (in)utility'+#10#13+'Copyright (c) 2017-2020 by Adrian Petrila, YO3GFH';
  pass_dlg_pwd_text     = 'Password:';
  app_ini               = 'locker.ini';
  app_data              = 'locker.dat';
  app_data_bak          = '.bak';
  date_fmt              = 'YYYY_MM_DD';
  ini_pass_section      = 'Settings';
  ini_pass_value        = 'Password';
  ini_salt_value        = 'Salt';
  err_old_pass          = 'Old and new password are identical :-)';
  err_bad_old_pass      = 'Old password is wrong...';
  err_bad_pass          = 'Password is wrong...';
  warn_no_def_pass      = 'Data file found, but no default password is set. Please create one identical with the one used for encryption.';
  warn_no_def_encr_pass = 'No encryption password found, please create one.';
  msg_change_pass       = 'Password must be between 8 and 32 characters long and cannot be identical with the previous one.';
  msg_ver_pass          = 'Please enter the password to decrypt your data:';

implementation

end.

