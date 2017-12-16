
with Ada.Text_IO;         use Ada.Text_IO;
--with Unicode.Encodings;
with GNAT.Calendar.Time_IO;

with MeasPoint;
with Totals;
with Ada.Calendar;


PACKAGE BODY SaxFwdReader IS

CurMeasPointStr : MeasPoint.MeasPointStrings := MeasPoint.Null_MeasPointStrings;
CurMeasPoint    : MeasPoint.MeasPoint;

CSV_File : Ada.Text_IO.File_Type;

 --
 -- here collect global data from header/footer (location, date, weight...)
 -- and attributes of rawData
 --
 procedure Start_Element
  (Handler              : in out Reader;
   Namespace_URI        : Unicode.CES.Byte_Sequence := "";
   Local_Name           : Unicode.CES.Byte_Sequence := "";
   Qname                : Unicode.CES.Byte_Sequence := "";
   Atts                 : Sax.Attributes.Attributes'Class)
   is
   begin
    Handler.Current_Pref  := Null_Unbounded_String;
    Handler.Current_Value := Null_Unbounded_String;

    -- TODO how to get encoding out ??
    if Local_Name = "xml" then
       Handler.Current_Pref  := To_Unbounded_String ( Local_Name );

        Put_Line(CSV_File, "# " & To_String (Handler.Current_Pref) &
                 " version " & To_String ( To_Unbounded_String (
                                    Get_Value (Atts , "version" ))) &
                 " encoding " & To_String ( To_Unbounded_String (
                                    Get_Value (Atts , "encoding" ))) &
                 " standalone " & To_String (To_Unbounded_String (
                                    Get_Value (Atts , "standalone" ))) );
    end if;


    -- pick the attributes of rawData
    -- FIXME this solution ASSUMES rawData has always two attrs of certain names
    -- and there is only one rawData per meas data array
    -- MORE GENERAL: query num of attrs and cycle throu
    --               them and print name-value pairs
    if Local_Name = "rawData" then

       Handler.Current_Pref  := To_Unbounded_String ( Local_Name );

       ExTot.sampleRate := Positive'Value( Get_Value (Atts , "sampleRate" ));

       Put_Line(CSV_File, "# " & To_String (Handler.Current_Pref) &
                 " sampleRate " & To_String ( To_Unbounded_String (
                                    ExTot.sampleRate )) &
                 " startAndGoalSame " & To_String (To_Unbounded_String (
                                    Get_Value (Atts , "startAndGoalSame" ))) );
       Put_Line(CSV_File, "# Pressure Temperature HeartRate LapTimeMark FigureOfMerit Latitude Longitude GPSAltitude GPSWeek TimeOfWeek HorizontalVelocity MotionDirection VerticalVelocity SatellitesInView FixInfo HDOP RR1 RR2 RR3 RR4");
       Put_Line(CSV_File, "#  [bar?]     [degC]  [beats/min]    [-]         [0/1]      [deg]     [deg]        [m]      [-]      [sec]         [m/s]             [deg]            [m/s]            [-]       [1=valid] [-] [-] [-] [-] [-]");

    end if;

    -- pick a meas data record
    Handler.Current_Pref := To_Unbounded_String ( Local_Name );

   end Start_Element;

 --
 -- end element found, allcharacters copied into local buffer by Characters()
 --
 procedure End_Element
  (Handler              : in out Reader;
   Namespace_URI        : Unicode.CES.Byte_Sequence := "";
   Local_Name           : Unicode.CES.Byte_Sequence := "";
   Qname                : Unicode.CES.Byte_Sequence := "")
   is
 begin

    MeasPoint.Fill_In( CurMeasPointStr, Local_Name,
                       To_String (Handler.Current_Value) );

    if Local_Name = "rawMeasurementPoint" then

       -- to csv file
       Put_Line(CSV_File, MeasPoint.To_CSVString(CurMeasPointStr));

       -- accumulate for totals
       CurMeasPoint := MeasPoint.To_MeasPoint( CurMeasPointStr );
       Totals.AddMeasurement( ExTot, CurMeasPoint );

    end if;

    -- header (location...date) and
    -- footer (name,type,value) of the xml file
    if 
       Local_Name = "location" or
-- TODO [this might have new-line chars which breaks CSV file if written. Replace NewLine with Space.]  Local_Name = "freeText" or
       Local_Name = "weight" or
       Local_Name = "name" or
       Local_Name = "stringEvent" or
       Local_Name = "country" or
       Local_Name = "date" or
       Local_Name = "type" or
       Local_Name = "value"
    then 
       Put_Line(CSV_File, "# " & To_String (Handler.Current_Pref) &
                " "  & To_String(Handler.Current_Value));

    end if;

 end End_Element;

 --
 -- Copy to local buffer each character within element limits
 --
 procedure Characters
  (Handler              : in out Reader;
   Ch                   : Unicode.CES.Byte_Sequence)
   is
   begin

    if Handler.Current_Pref  /= Null_Unbounded_String then
       Handler.Current_Value := Handler.Current_Value & Ch;
    end if;

   end Characters;


 -- setup the Sax Parser  and
 --    ADD control flag for print out parsed strings or not by func param
 --    that should stop all Put()'s but continue
 --    supply converted data to calc
 procedure fwdxmlparser ( Input     : in out File_Input;
                          csvname : String ) is

  My_Reader : SaxFwdReader.Reader;

  dd : String(1..8);
  df : String(1..8);

 begin

  Set_Feature(My_Reader,Namespace_Prefixes_Feature,False);
  Set_Feature(My_Reader,Namespace_Feature,False);
  Set_Feature(My_Reader,Validation_Feature,False);


  ExTot := Totals.Null_ExerciseTotals;

-- FIXME add error handling
--   begin
    Ada.Text_IO.Create( File => CSV_File,
                        Mode => Ada.Text_IO.Out_File,
                        Name => csvname);
--   exception
--         when	Name_Error =>
--           	Put_Line ( Standard_Error ,
--                   Item => "Cannot create file: " & To_Unbounded_String(csvname));
--         	Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
--           	return;
--         when	Status_Error =>
--           	Put_Line ( Standard_Error,
--                   Item => "File already exist: " & To_String(csvname));
--         	-- Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
--           	-- return;
--         when	Use_Error =>
--           	Put_Line ( Standard_Error,
--                   Item => "You have no permission to create file: " & To_String(csvname));
--         	Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
--           	return;
--         when others =>
--   		Put_Line ( Standard_Error,
--          	     Item => "Other unexpected error on file: " & To_String(csvname));
--    	     	Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
--   		return;
--   FIXME if exited with Exception close the open fwd.xml file !
--   end;

    Parse(My_Reader, Input);

    Put_Line(Totals.To_String(ExTot));
    Ada.Text_IO.Close(CSV_File);

    -- compare date in filename first 8-chars with the one by GPS in file
    dd := GNAT.Calendar.Time_IO.Image(ExTot.StartDate,"%Y%m%d");
    df := csvname(csvname'First..(csvname'First+7));
    -- Put_Line("DEBUG: " & dd & " vs " & df );
    if dd /= df
    then
     Put_Line(Standard_Error, "Date in data (" & dd &") differs from date in filename: " & csvname );
    end if;

 end fwdxmlparser;

END SaxFwdReader;
