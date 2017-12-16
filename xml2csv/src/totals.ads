
with Ada.Text_IO;
with Ada.Calendar;
with MeasPoint;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

package Totals is

-- FIXME format print into aligned columns header/units & data
-- Construct 80-char long strings with fixed line-pos for header/values ?
-- Or use tabulator ASCII.HT  <-- PREFERABLE can use settab in xterm etc...
-- See also To_String()
 ExTotHeader :      constant String := "   Start Date-Time  Points   Duration  Distance Energy";
 ExTotHeaderUnits : constant String := "      [UTC]           [-]      [sec]     [m]    [kCal]";

type ExerciseTotals is record
 -- accumulated results
 StartDate    : Ada.Calendar.Time; -- starting Date & Time (UTC) of the exercise
 N_MeasPoints : Natural;           -- number of MeasPoints in log (valid or invalid)
 DurSec       : Duration;          -- duration of exercise in [sec]
 DistMeters   : Float;             -- distance in [m]
 EnergyKCal   : Float;             -- estimate of consumed energy in [kCal]
 -- support variables
 sampleRate   : Positive;             -- sampling Rate of recorder (actually sample period 1sec or 4 sec)
 CurDate      : Ada.Calendar.Time; -- current time of MeasPoint read
 prevLon      : Float;             -- longitude from previous valid fix
 prevLat      : Float;             -- latitude from previous valid fix
 sum_hr       : Float;             -- current sum of HR
 e_zero       : Float;             -- Energy = EpHR * sum(HRi) + e_zero
 EpHR         : Float;             -- Energy per HR
 N_HRmeas     : Natural;           -- number of valid (HR>0) HR-meas
 filename     : Unbounded_String;  -- filename to check date agaist GPS time
end record;

-- Set default value to start of GPS time counting
Null_Time : constant Ada.Calendar.Time := Ada.Calendar.Time_Of(1980,1,6,0.0);
Null_ExerciseTotals : constant ExerciseTotals :=
   (Null_Time, 0, 0.0, 0.0, 0.0,
    1,Null_Time,0.0,0.0,0.0, 0.0, 0.0,0,Null_Unbounded_String);-- support variables
-- FIXME sampleRate defaults to 1, but if log with 4sec and not in xml yields incorrect result without warning

function To_String ( ExTot : in ExerciseTotals )
 return String;

procedure AddMeasurement( ExTot : in out ExerciseTotals;
			  mp    : in MeasPoint.MeasPoint );

end Totals;

