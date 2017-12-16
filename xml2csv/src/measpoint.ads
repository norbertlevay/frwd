
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Interfaces; -- Integer_16

package MeasPoint is

type MeasPointStrings is record
 pressure 	    : Unbounded_String;
 temperature 	    : Unbounded_String;
 heartRate 	    : Unbounded_String;
 lapTimeMark	    : Unbounded_String;
 figureOfMerit 	    : Unbounded_String;
 latitude 	    : Unbounded_String;
 longitude          : Unbounded_String;
 GPSAltitude 	    : Unbounded_String;
 GPSWeek 	    : Unbounded_String;
 timeOfWeek 	    : Unbounded_String;
 HorizontalVelocity : Unbounded_String;
 MotionDirection    : Unbounded_String;
 VerticalVelocity   : Unbounded_String;
 satellitesInView   : Unbounded_String;
 FixInfo 	    : Unbounded_String;
 HDOP 		    : Unbounded_String;
 RR1 		    : Unbounded_String;
 RR2 		    : Unbounded_String;
 RR3 		    : Unbounded_String;
 RR4 		    : Unbounded_String;
end record;
-- TODO when RRn meaning known: Ada bit handling by 'record_representation_clause': for <recordname> use record <bit-ranges> end record;

Null_MeasPointStrings : constant MeasPointStrings := (
Null_Unbounded_String,Null_Unbounded_String,
Null_Unbounded_String,Null_Unbounded_String,
Null_Unbounded_String,Null_Unbounded_String,
Null_Unbounded_String,Null_Unbounded_String,
Null_Unbounded_String,Null_Unbounded_String,
Null_Unbounded_String,Null_Unbounded_String,
Null_Unbounded_String,Null_Unbounded_String,
Null_Unbounded_String,Null_Unbounded_String,
Null_Unbounded_String,Null_Unbounded_String,
Null_Unbounded_String,Null_Unbounded_String);

type Bits16 is new Interfaces.Unsigned_16;

type MeasPoint is record
 pressure 	    : Float;
 temperature 	    : Float;
 heartRate 	    : Float;
 lapTimeMark	    : Natural; -- 0 or 1
 figureOfMerit 	    : Natural; -- 0 ,1, 2 .. only some values?
 latitude 	    : Float;
 longitude          : Float;
 GPSAltitude 	    : Integer;
 GPSWeek 	    : Duration;
 timeOfWeek 	    : Duration;
 HorizontalVelocity : Float;
 MotionDirection    : Float;
 VerticalVelocity   : Float;
 satellitesInView   : Natural; -- 1 .. 32?34
 FixInfo 	    : Integer;
 HDOP 		    : Float;
 RR1 		    : Bits16; -- 16bit bitmask
 RR2 		    : Bits16;
 RR3 		    : Bits16;
 RR4 		    : Bits16;
end record;

-- Null_MeasPoint : constant MeasPoint := (
-- 0.0, 0.0, 0, 0, 0,
-- 0.0, 0.0, 0, 0.0, 0.0,
-- 0.0, 0.0, 0.0,
-- 0, 0, 0.0,
-- 0, 0, 0, 0 );

-- conversions

function To_MeasPoint( mps : MeasPointStrings )
 return MeasPoint;

function To_CSVString( mp : MeasPointStrings; Separator : Character := ',')
 return String;
-- converts and guarantees 19 separators even if some record.item not set

procedure Fill_In ( mps  : in out MeasPointStrings ;
                    name : in String ; value : in String );
-- fill-in the record from parser's name & value
end MeasPoint;
