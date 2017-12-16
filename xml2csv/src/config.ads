
package config is

 type CalibOxygen is -- data to calibrate VO2-HR line
  record
  	hrLow  : Integer := Integer(( 94.0 + 125.0) / 2.0); -- HeartRates [beats/min]
        hrHigh : Integer := Integer((163.0 + 168.0) / 2.0);
        OxLow  : Float   := ( 0.5 +  0.75)  / 2.0;  -- Oxygen consumption [liters/min]
        OxHigh : Float   := ( 3.25 + 2.33)  / 2.0 ;
  end record;

 createCSV : Boolean := False; -- create or not a CSV file

 OxygenEq : Float:= 5.070; --[kCal/1Liter of O2]
 OxygenHR : CalibOxygen;


 procedure load_config;

end config;
