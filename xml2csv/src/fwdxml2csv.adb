-- do command line param handling, switches, help text etc...
-- and return Success or error codes
-- then call parser if needed

-- TODO we should parse encoding: octave does not like finnish characters (UTF-8) in csv
-- (if not explicitely present in <?xml ?> line
-- assume UTF-8 see book italian on XML)
-- and write out a csv file in some
-- more classical enoding than UTF-8, for example octave seems not to like
-- lines with special finnish characters
-- what to about wide character ? skip it?
-- Ada.Strings.Map should handle encoding conversions --> Also SaxXML parser has Map functions!!

with Input_Sources.File;

with SaxFwdReader;
with Ada.Command_Line;
with Ada.Directories; --    use Ada.Directories;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Exceptions;  use Ada.Exceptions;

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with totals;
with config; -- handles programs config data in file


procedure fwdxml2csv is

 gCSV  : Boolean := False;
 gDir  : Boolean := False;

procedure parse_file ( filename : string ) is
 fwdxml		: Input_Sources.File.File_Input;
 csvname 	: Unbounded_String;
begin

        -- below 2 properties of the Input_Sources.File are used by XML/Ada:
        -- public_id is used by XML/Ada in its error message to reference locations in that file
        -- system_id is the location of the file on the system (it is used to resolve relative
        --      paths found in teh XML document)
	Input_Sources.File.Set_Public_Id(fwdxml, "Preferences File");
	Input_Sources.File.Set_System_Id(fwdxml, filename);

        begin -- try open xml in-file
		Input_Sources.File.Open( filename , fwdxml );
	exception
	when Name_Error =>
        	Put_Line ( Standard_Error ,
                Item => "Cannot open file: " & filename );
 		Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
        	return;
 	when Status_Error =>
        	Put_Line ( Standard_Error,
                Item => "File is already open?" & filename);
 		--Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
        	--return;
                -- just continue
        when Use_Error =>
        	Put_Line ( Standard_Error,
                Item => "You have no permission?" & filename);
 		Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
        	return;
        when others =>
        	Put_Line ( Standard_Error,
                Item => "Other unexpected error on file: " & filename);
 		Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
        	return;
        end;

	csvname := To_Unbounded_String(
         		Ada.Directories.Compose("",
                        Ada.Directories.Base_Name(filename), "csv") );

	SaxFwdReader.fwdxmlparser(fwdxml,To_String(csvname));

	Input_Sources.File.Close(fwdxml);

        exception
        when E : others =>
           New_Line(Standard_Error);
           Put_Line(Standard_Error, "Program error.");
           Put_Line(Standard_Error, "Send bug-report with arguments and the following information:");
           New_Line(Standard_Error);
           Put_Line(Standard_Error, Exception_Information(E));
           New_Line(Standard_Error);
end parse_file;

procedure print_usage is
begin
  Put_Line ( "Version: 0.9.0");
  Put_Line ( "Usage:");
  New_Line;
  Put_Line ( "   fwdxml2csv filename(s).fwd.xml");
  New_Line;
  Put_Line ( "Creates filename(s).fwd.csv and for each file prints summary values (date distance calories etc...)");
  New_Line;
  Put_Line ( "Note: a csv-file can be loaded to octave with -ascii option:");
  Put_Line ( "      dat = load('-ascii' ,'datafile.csv' );");
end print_usage;

begin -- parse command line argumentsand switches

 Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Success);

 if(Ada.Command_Line.Argument_Count = 0) then
         print_usage;
         New_Line;
         return;
 end if;

 -- load config file (settings in conf file(s) have
 -- lower priority then command line options
 config.load_config;
 gCSV := config.CreateCSV;

 Put_Line(Totals.ExTotHeader);
 Put_Line(Totals.ExTotHeaderUnits);

 -- support wildcard expanded argc filenames as parameters
 -- no options in this implemenation
 for p in 1..Ada.Command_Line.Argument_Count loop

       parse_file (Ada.Command_Line.argument(p));

 end loop;

end fwdxml2csv;

