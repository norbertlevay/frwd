
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with GNAT.Calendar; use GNAT.Calendar;
with GNAT.Calendar.Time_IO;

with Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;

with config; -- for energy calcs

package body Totals is

function To_String ( ExTot : in ExerciseTotals )
 return String is
  TotStr : String := GNAT.Calendar.Time_IO.Image(ExTot.StartDate,
                                                "%Y-%m-%d %H:%M:%S")
                      & ASCII.HT & Natural'Image(ExTot.N_MeasPoints)         
                      & ASCII.HT & Natural'Image(Natural(ExTot.DurSec))
                      & ASCII.HT & Natural'Image(Natural(ExTot.DistMeters))  
                      & ASCII.HT & Integer'Image(Integer(ExTot.EnergyKCal)); 

 begin
  return TotStr;
 end To_String;

function To_CSV_String ( ExTot : in ExerciseTotals )
 return String is
  TotStr : String :=    " YYYY, MM, DD"                                  -- "StartDate "       &
                      & ", " & Natural'Image(ExTot.N_MeasPoints)         -- " N_MeasPoints "   &
                      & ", " & Duration'Image(ExTot.DurSec)              -- " Duration [sec] " &
                      & ", " & Natural'Image(Natural(ExTot.DistMeters))  -- " Distance [m] "   &
                      & ", " & Integer'Image(Integer(ExTot.EnergyKCal)); -- " Energy [kCal] "  &

 Year       : Ada.Calendar.Year_Number;
 Month      : Ada.Calendar.Month_Number;
 Day        : Ada.Calendar.Day_Number;
 Hour       : GNAT.Calendar.Hour_Number;
 Minute     : GNAT.Calendar.Minute_Number;
 Second     : GNAT.Calendar.Second_Number;
 Sub_Second : GNAT.Calendar.Second_Duration;

 begin

  GNAT.Calendar.Split(ExTot.StartDate,-- : Ada.Calendar.Time;
  		Year, --       : out Ada.Calendar.Year_Number;
      		Month, --      : out Ada.Calendar.Month_Number;
    		Day, --        : out Ada.Calendar.Day_Number;
  		Hour, --       : out Hour_Number;
		Minute, --     : out Minute_Number;
      	        Second, --     : out Second_Number;
    	        Sub_Second);-- : out Second_Duration);

  -- " YYYY, MM, DD,"
  -- Img attrib prepends one space before the number
  -- Ada.Text_IO.Put_Line("DBG Year " & Year'Img & Month'Img & Day'Img);
  TotStr( 1..5) := Year'Img;

  if Month < 10 then
   TotStr(7..9) := " " & Month'Img;
  else
   TotStr(7..9) := Month'Img;
  end if;

  if Day < 10 then
   TotStr(11..13) := " " & Day'Img;
  else
   TotStr(11..13) := Day'Img;
  end if;

  return TotStr;
 end To_CSV_String;


-- convert GPS time to Ada Calendar Time
function To_AdaTime( GPSWeek	: Duration;
 	             TOW 	: Duration )
 return Ada.Calendar.Time
 is
  GPSWeek_sec : Duration;
  TOW_sec     : Duration;
  ATime       : Ada.Calendar.Time;
 begin
  -- FIXME STILL MISSING LEAP-SECONDS
  GPSWeek_sec := 7.0*86400.0*(1024.0 + GPSWeek);
  TOW_sec     := TOW;

  ATime := Ada.Calendar.Time_Of(1980,1,6,0.0) + (GPSWeek_sec + TOW_sec);

  return ATime;
 end To_AdaTime;


-- Ref1:
-- W.D. McArdle, F.I. Katch, V.L. Katch:
-- "Exercise Physiology - Nutrition, Energy, and Human Performance"
--  Seventh Edition, 2010
--  Walters Kluver | Lippincott Wiliams & Wilkins
-- Ref 2:
-- [Albanesi]p164 Energy [KCal] of run P kg person d [km] distance : E = k * P * d ~ P * d
--                k = 0.8 .. 1.2 depnding on efficiency of run. Most pesons  k ~ 1
-- [Albanesi]p184  VO2 units: ml/kg/min  V-wth dot: (first sentence top of page 184):
--                 derivative e.g. change, e.g. in this case sonume
-- example middle of the line (Fig 8.1) : 80kg * 50 [ml/kg/min] = 4000 = 4 liters/min
function Energy_kCAL( sum_HR : Float;   -- [beats/min] integrated HR
                      N      : Natural; -- number of valid HR samples
                      sampleRate_sec : Positive ) -- [sec] sampling period of HR
 return Float
 is
  Ts : constant Float := Float(sampleRate_sec);
  -- [sec] sampling period for HR integration

  e  : constant Float := config.OxygenEq;
  -- [kCal/liter] energy equivalent of oxygen volume

  -- consumption of Oxygen [liter/min] is linearly related to HR [beats/min]
  -- VO2 = a * HR + b    [see Ref. p244 Fig11.14]
  a : constant Float := (config.OxygenHR.OxHigh - config.OxygenHR.OxLow)
                 / Float(config.OxygenHR.hrHigh - config.OxygenHR.hrLow);

  b : constant Float := Float(config.OxygenHR.OxLow) -
                    a * Float(config.OxygenHR.hrLow) ;

  Etot : Float; -- [kCal] estimated total energy-consupmtion of the exercise

 begin

  Etot := e*(Ts/60.0)*( a * sum_HR + b * Float(N) );
  -- HR is [beats/min], b is [liter/min] ->
  -- (Ts/60.0) is sampling period in min

  return Etot;
end Energy_kCAL;


-- calc approximante dist between 2 points on Earth
function Distance( lon1_deg : Float; lat1_deg : Float;
                   lon2_deg : Float; lat2_deg : Float)
 return Float
 is
  Dist : Float;
  dlat : Float;
  dlon : Float;
 begin
  dlat := lat2_deg - lat1_deg;
  dlon := lon2_deg - lon1_deg;
   -- Earth radius: 6353 ... 6384 km
   -- lat lon is in xml file expressed with 9 digits 12.3456789
   -- mean radius of Earth = 6371.088km, diff in poles is about 30km
   -- (6.28*6371000.0/360.0) = 111km

   -- why /1.75 = ~57% --> noisy GPS position tends the over-estimate distance between two (noisy) points
   -- in ols version I added a simple filter-bank which reduced ( but not eliminated ) the error compared to FRWD_Replayer value.
   -- before filtering: ddist := Sqrt(dlat*dlat + dlon*dlon)*(2.0*3.1415926*6371088.0/360.0) /1.75;
   Dist := Ada.Numerics.Elementary_Functions.Sqrt( dlat*dlat + dlon*dlon )
                     * (2.0*Ada.Numerics.Pi*6371088.0/360.0);
 return Dist;
 end Distance;


-- accumulate received MeasPoint
procedure AddMeasurement( ExTot : in out ExerciseTotals;
			  mp    : in MeasPoint.MeasPoint ) is
 begin
  -- Ada.Text_IO.Put_Line("DBG: Fix: " & mp.FixInfo'Img & "  SIV: " &  mp.satellitesInView'Img);

  -- count all meas points in log
  ExTot.N_MeasPoints := ExTot.N_MeasPoints + 1;

  if ( (mp.FixInfo = 1) and (mp.satellitesInView > 0) ) then

    -- store start time and end time (by valid fix e.g. GPS-time)
    if ExTot.StartDate = Null_Time then
      ExTot.StartDate := To_AdaTime(mp.GPSWeek,mp.timeOFWeek);
      ExTot.PrevLon := mp.longitude;
      ExTot.PrevLat := mp.latitude;
    end if;

    ExTot.CurDate := To_AdaTime(mp.GPSWeek,mp.timeOFWeek);
    ExTot.DurSec  := ExTot.CurDate - ExTot.StartDate;

    -- distance
    ExTot.DistMeters :=  ExTot.DistMeters
                       + Distance( mp.longitude, mp.latitude,
                                   ExTot.PrevLon,  ExTot.PrevLat);
    ExTot.PrevLon := mp.longitude;
    ExTot.PrevLat := mp.latitude;
  end if;

  -- Energy estimate from HR:
  if mp.heartRate > 0.0 then
    ExTot.N_HRmeas   := ExTot.N_HRmeas + 1;
    ExTot.sum_hr     := ExTot.sum_hr + mp.heartRate;
    ExTot.EnergyKCal := Energy_kCAL( ExTot.sum_hr,  ExTot.N_HRmeas, ExTot.sampleRate );
  end if;

 end AddMeasurement;

end Totals;

