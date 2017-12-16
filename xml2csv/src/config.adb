
with Ada.Environment_Variables;
use Ada.Environment_Variables;
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Float_Text_IO;
use Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
--with Ada.IO_Exceptions;
--use Ada.IO_Exceptions;

package body config is

config_file_extension : String := ""; -- under linux (could be: .rc or .conf)
                                           -- inf under Win, etc...
-- parse CSV: create or not csv file(s)
function parse_csv( Line : String ) return Boolean
is
begin
 if ( Line(Line'First..Line'First+2) = "csv" ) then
   CreateCSV := true;
--   Put_Line("parse_csv CreateCSV: "&Boolean'Image(config.CreateCSV));
   return True;
  else
   return False;
 end if;
end parse_csv;

-- parse Oxygen EnergyEquivivalent
function parse_OxygenEq( Line : String ) return Boolean
is
begin
 if (Line(Line'First .. Line'First+7) = "OxygenEq") then
   OxygenEq := Float'Value(Line(Line'First+9 .. Line'Last));
  -- Put(OxygenEq);
   return True;
  else
   return False;
 end if;
end parse_OxygenEq;

-- parse 2 points for HR-VO2 line
function parse_CalibOxygen( Line : String ) return Boolean
is
begin
 if (Line(Line'First .. Line'First+4) = "HRLow") then
   OxygenHR.hrLow := Integer'Value(Line(Line'First+6 .. Line'Last));
--    Put("HRLow parsed");Put(OxygenHR.hrLow);
   return True;
 elsif (Line(Line'First .. Line'First+5) = "HRHigh") then
   OxygenHR.hrHigh := Integer'Value(Line(Line'First+7 .. Line'Last));
--    Put("HRHigh parsed");Put(OxygenHR.hrHigh);
   return True;
 elsif (Line(Line'First .. Line'First+8) = "OxygenLow") then
   OxygenHR.OxLow := Float'Value(Line(Line'First+10 .. Line'Last));
--    Put("OxLow  parsed");Put(OxygenHR.OxLow);
   return True;
 elsif (Line(Line'First .. Line'First+9) = "OxygenHigh") then
   OxygenHR.OxHigh := Float'Value(Line(Line'First+11 .. Line'Last));
--    Put("OxHigh parsed");Put(OxygenHR.OxHigh);
   return True;
  else
   return False;
 end if;
end parse_CalibOxygen;


-----------------------------------------------------
-- read config file(s) in datavalues priority order
-- command line switches
-- ./fwdxml2csv.conf  TODO read conf file in workdir; NOT implemented
-- ~/.fwdxml2csv
-- /etc/fwdxml2csv.conf <-- This NOT NECESSARY,
--                          CONF VALUES ARE ALWAYS(?) PERSONAL
-- <if none of previous existed, hardcoded values will be used>
procedure load_config
is
 ConfFile : File_Type;
 Parsed : Boolean;   -- succesfully parsed or Keyword not in this line or syntax error
 ConfFileName : String := Value("HOME")&"/.fwdxml2csv"&config_file_extension;
begin
 Null;
 -- redo this with file name string as parameter
begin
 -- Put_Line("#Configuration file is: "&ConfFIleName);
 Open( ConfFile, In_File, ConfFileName );
exception
   when Name_Error =>
      Put(File => Standard_Error, Item => "No configuration file found. Using defaults.");
      New_Line;
      return;
   when Status_Error =>
      Put(File => Standard_Error, Item => "File already open.");
      New_Line;
   when Use_Error =>
      Put(File => Standard_Error,
          Item => "You do not have permission to open configuration file. Using defaults.");
      New_Line;
      return;
end;

 while not End_Of_File(ConfFile)
 loop
 	declare
         	Line : String := Get_Line(ConfFile);
        begin
               -- first skip comment lines
               if ( Line(Line'First) = '#' ) then
                Null;
               else
                Parsed := parse_csv(Line);
                Parsed := parse_OxygenEq(Line);
                Parsed := parse_CalibOxygen(Line);
--                New_Line;
               end if;
        end;
 end loop;

 Close( ConfFile );
 -- add error handling...

end load_config;

end config;
