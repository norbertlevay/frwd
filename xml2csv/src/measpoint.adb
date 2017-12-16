
package body MeasPoint is

-- conversions

function To_MeasPoint( mps : MeasPointStrings )
 return MeasPoint is
 mp : MeasPoint;
 begin
  mp.pressure           := Float'Value(	   To_String( mps.pressure           ));
  mp.temperature        := Float'Value(	   To_String( mps.temperature        ));
  mp.heartRate          := Float'Value(    To_String( mps.heartRate          ));
  mp.figureOfMerit      := Natural'Value(  To_String( mps.figureOfMerit      ));
  mp.lapTimeMark        := Natural'Value(  To_String( mps.lapTimeMark        ));
  mp.latitude 	        := Float'Value(	   To_String( mps.latitude           ));
  mp.longitude          := Float'Value(	   To_String( mps.longitude          ));
  mp.GPSAltitude        := Integer'Value(  To_String( mps.GPSAltitude        ));
  mp.GPSWeek 	        := Duration'Value( To_String( mps.GPSWeek            ));
  mp.timeOfWeek         := Duration'Value( To_String( mps.timeOfWeek         ));
  mp.HorizontalVelocity := Float'Value(	   To_String( mps.HorizontalVelocity ));
  mp.MotionDirection    := Float'Value(	   To_String( mps.MotionDirection    ));
  mp.VerticalVelocity   := Float'Value(	   To_String( mps.VerticalVelocity   ));
  mp.satellitesInView   := Natural'Value(  To_String( mps.satellitesInView   ));
  mp.FixInfo 	        := Integer'Value(  To_String( mps.FixInfo            ));
  mp.HDOP 	        := Float'Value(	   To_String( mps.HDOP               ));
  mp.RR1 	        := Bits16'Value(   To_String( mps.RR1                ));
  mp.RR2 	        := Bits16'Value(   To_String( mps.RR2                ));
  mp.RR3 	        := Bits16'Value(   To_String( mps.RR3                ));
  mp.RR4 	        := Bits16'Value(   To_String( mps.RR4                ));

  return mp;
 end To_MeasPoint;

function To_CSVString( mp : MeasPointStrings; Separator : Character := ',')
 return String is
 Line : String := 
	To_String(mp.pressure          ) & Separator &
	To_String(mp.temperature       ) & Separator &
	To_String(mp.heartRate         ) & Separator &
	To_String(mp.lapTimeMark       ) & Separator &
	To_String(mp.figureOfMerit     ) & Separator &
	To_String(mp.latitude 	       ) & Separator &
	To_String(mp.longitude         ) & Separator &
	To_String(mp.GPSAltitude       ) & Separator &
	To_String(mp.GPSWeek 	       ) & Separator &
	To_String(mp.timeOfWeek        ) & Separator &
	To_String(mp.HorizontalVelocity) & Separator &
	To_String(mp.MotionDirection   ) & Separator &
	To_String(mp.VerticalVelocity  ) & Separator &
	To_String(mp.satellitesInView  ) & Separator &
	To_String(mp.FixInfo 	       ) & Separator &
	To_String(mp.HDOP 	       ) & Separator &
	To_String(mp.RR1 	       ) & Separator &
	To_String(mp.RR2 	       ) & Separator &
	To_String(mp.RR3 	       ) & Separator &
	To_String(mp.RR4 	       ) & Separator;
        -- Q:What happens if some UnboundedString undefined? :
        -- A:converts to empty String >><< of length 0 (I simply tried)
 begin
  return Line;
 end To_CSVString;

procedure Fill_In ( mps : in out MeasPointStrings ;
                    name : in String ; value : in String ) is
begin

    if    name = "latitude"
    then
      mps.latitude := To_Unbounded_String(value);

    elsif name = "longitude"
    then
      mps.longitude := To_Unbounded_String(value);

    elsif name = "HDOP"
    then
      mps.HDOP := To_Unbounded_String(value);

    elsif name = "GPSAltitude"
    then
      mps.GPSAltitude := To_Unbounded_String(value);

    elsif name = "timeOfWeek"
    then
      mps.timeOfWeek := To_Unbounded_String(value);

    elsif name = "GPSWeek"
    then
      mps.GPSWeek := To_Unbounded_String(value);

    elsif name = "heartRate"
    then
      mps.heartRate := To_Unbounded_String(value);

    elsif name = "FixInfo"
    then
      mps.FixInfo := To_Unbounded_String(value);

    elsif name = "satellitesInView"
    then
      mps.satellitesInView := To_Unbounded_String(value);

    elsif name = "figureOfMerit"
    then
      mps.figureOfMerit := To_Unbounded_String(value);

    elsif name = "temperature"
    then
      mps.temperature := To_Unbounded_String(value);

    elsif name = "pressure"
    then
      mps.pressure := To_Unbounded_String(value);

    elsif name = "MotionDirection"
    then
      mps.MotionDirection := To_Unbounded_String(value);

    elsif name = "HorizontalVelocity"
    then
      mps.HorizontalVelocity := To_Unbounded_String(value);

    elsif name = "VerticalVelocity"
    then
      mps.VerticalVelocity := To_Unbounded_String(value);

    elsif name = "RR1"
    then
      mps.RR1 := To_Unbounded_String(value);

    elsif name = "RR2"
    then
      mps.RR2 := To_Unbounded_String(value);

    elsif name = "RR3"
    then
      mps.RR3 := To_Unbounded_String(value);

    elsif name = "RR4"
    then
      mps.RR4 := To_Unbounded_String(value);

    elsif name = "lapTimeMark"
    then
        -- convert strings true/false to numbers
        -- enables to load as numerical matrix into Octave/Matlab etc...
        if    value = "true" then
	   mps.lapTimeMark := To_Unbounded_String("1");
  	elsif value = "false" then
   	   mps.lapTimeMark := To_Unbounded_String("0");
  	else
   	   -- FIXME lapTimeMark something else then 'true' or 'false' should raise Warning
   	   mps.lapTimeMark := To_Unbounded_String("0");
        end if;

    end if;

end Fill_In;

end MeasPoint;
