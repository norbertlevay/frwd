

    * Main Page
    * Data Structures
    * Files
    * Related Pages
    *  Search for[query               ]
****** time_conversion.c ******
Go_to_the_documentation_of_this_file.
00001 /**
00002 \file    time_conversion.c
00003 \brief   GNSS core 'c' function library: converting time information.
00004 \author  Glenn D. MacGougan (GDM)
00005 \date    2007-11-29
00006 \since   2005-07-30
00007
00008 \b REFERENCES \n
00009 - Hofmann-Wellenhof, B., H. Lichtenegger, and J. Collins (1994). GPS
Theory and
00010   Practice, Third, revised edition. Springer-Verlag, Wien New York. pp.
38-42 \n
00011 - http://aa.usno.navy.mil/data/docs/JulianDate.html - Julian Date
Converter \n
00012 - http://aa.usno.navy.mil/faq/docs/UT.html \n
00013 - http://wwwmacho.mcmaster.ca/JAVA/JD.html \n
00014 - Raquet, J. F. (2002), GPS Receiver Design Lecture Notes. Geomatics
Engineering,
00015   University of Calgary Graduate Course. \n
00016
00017 \b "LICENSE INFORMATION" \n
00018 Copyright (c) 2007, refer to 'author' doxygen tags \n
00019 All rights reserved. \n
00020
00021 Redistribution and use in source and binary forms, with or without
00022 modification, are permitted provided the following conditions are met: \n
00023
00024 - Redistributions of source code must retain te above copyright
00025   notice, this list of conditions and the following disclaimer. \n
00026 - Redistributions in binary form must reproduce the above copyright
00027   notice, this list of conditions and the following disclaimer in the
00028   documentation and/or other materials provided with the distribution. \n
00029 - The name(s) of the contributor(s) may not be used to endorse or promote

00030   products derived from this software without specific prior written
00031   permission. \n
00032
00033 THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
00034 OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
00035 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
00036 DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
00037 INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
00038 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
00039 SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER
00040 CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
00041 LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY

00042 OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
00043 SUCH DAMAGE.
00044 */
00045
00046 #include <sys/timeb.h>
00047 #include <time.h>
00048 #include <math.h> // for fmod()
00049 #include "time_conversion.h"
00050 #include "constants.h"
00051
00052 #ifndef WIN32
00053 #define _CRT_SECURE_NO_DEPRECATE
00054 #endif
00055
00056
00057 #define TIMECONV_JULIAN_DATE_START_OF_GPS_TIME (2444244.5)  // [days]
00058 #define TIMECONV_JULIAN_DATE_START_OF_PC_TIME  (2440587.5)  // [days]
00059 #define TIMECONV_DAYS_IN_JAN 31
00060 #define TIMECONV_DAYS_IN_MAR 31
00061 #define TIMECONV_DAYS_IN_APR 30
00062 #define TIMECONV_DAYS_IN_MAY 31
00063 #define TIMECONV_DAYS_IN_JUN 30
00064 #define TIMECONV_DAYS_IN_JUL 31
00065 #define TIMECONV_DAYS_IN_AUG 31
00066 #define TIMECONV_DAYS_IN_SEP 30
00067 #define TIMECONV_DAYS_IN_OCT 31
00068 #define TIMECONV_DAYS_IN_NOV 30
00069 #define TIMECONV_DAYS_IN_DEC 31
00070
00071
00072 // A static function to check if the utc input values are valid.
00073 // \return TRUE if valid, FALSE otherwise.
00074 static BOOL TIMECONV_IsUTCTimeValid(
00075  const unsigned short     utc_year,      //!< Universal Time Coordinated
[year]
00076  const unsigned char      utc_month,     //!< Universal Time Coordinated
[1-12 months]
00077  const unsigned char      utc_day,       //!< Universal Time Coordinated
[1-31 days]
00078  const unsigned char      utc_hour,      //!< Universal Time Coordinated
[hours]
00079  const unsigned char      utc_minute,    //!< Universal Time Coordinated
[minutes]
00080  const float              utc_seconds    //!< Universal Time Coordinated
[s]
00081  );
00082
00083
00084 BOOL TIMECONV_IsUTCTimeValid(
00085  const unsigned short     utc_year,      //!< Universal Time Coordinated
[year]
00086  const unsigned char      utc_month,     //!< Universal Time Coordinated
[1-12 months]
00087  const unsigned char      utc_day,       //!< Universal Time Coordinated
[1-31 days]
00088  const unsigned char      utc_hour,      //!< Universal Time Coordinated
[hours]
00089  const unsigned char      utc_minute,    //!< Universal Time Coordinated
[minutes]
00090  const float              utc_seconds    //!< Universal Time Coordinated
[s]
00091  )
00092 {
00093   unsigned char daysInMonth;
00094   BOOL result;
00095   if( utc_month == 0 || utc_month > 12 )
00096     return FALSE;
00097   result = TIMECONV_GetNumberOfDaysInMonth( utc_year, utc_month,
&amp;daysInMonth );
00098   if( result == FALSE )
00099     return FALSE;
00100   if( utc_day == 0 || utc_day > daysInMonth )
00101     return FALSE;
00102   if( utc_hour > 23 )
00103     return FALSE;
00104   if( utc_minute > 59 )
00105     return FALSE;
00106   if( utc_seconds > 60 )
00107     return FALSE;
00108
00109   return TRUE;
00110 }
00111
00112
00113 BOOL TIMECONV_GetSystemTime(
00114   unsigned short*     utc_year,     //!< Universal Time Coordinated
[year]
00115   unsigned char*      utc_month,    //!< Universal Time Coordinated
[1-12 months]
00116   unsigned char*      utc_day,      //!< Universal Time Coordinated
[1-31 days]
00117   unsigned char*      utc_hour,     //!< Universal Time Coordinated
[hours]
00118   unsigned char*      utc_minute,   //!< Universal Time Coordinated
[minutes]
00119   float*              utc_seconds,  //!< Universal Time Coordinated
[s]
00120   unsigned char*      utc_offset,   //!< Integer seconds that GPS is
ahead of UTC time, always positive             [s], obtained from a look up
table
00121   double*             julian_date,  //!< Number of days since noon
Universal Time Jan 1, 4713 BCE (Julian calendar) [days]
00122   unsigned short*     gps_week,     //!< GPS week (0-1024+)
[week]
00123   double*             gps_tow       //!< GPS time of week (0-604800.0)
[s]
00124   )
00125 {
00126   BOOL result;
00127
00128 #ifdef WIN32
00129   struct _timeb timebuffer; // found in <sys/timeb.h>
00130 #else
00131   struct timeb timebuffer;
00132 #endif
00133   double timebuffer_time_in_days;
00134   double timebuffer_time_in_seconds;
00135   //char *timeline; // for debugging
00136
00137 #ifndef _CRT_SECURE_NO_DEPRECATE
00138   if( _ftime_s( &amp;timebuffer ) != 0 )
00139     return FALSE;
00140 #else
00141
00142 #ifdef WIN32
00143   _ftime( &amp;timebuffer );
00144 #else
00145   ftime( &amp;timebuffer );
00146 #endif
00147
00148 #endif
00149   //timeline = ctime( &amp; ( timebuffer.time ) ); // for debugging
00150   //printf( "%s\n", timeline ); // for debugging
00151
00152   timebuffer_time_in_seconds = timebuffer.time + timebuffer.millitm /
1000.0; // [s] with ms resolution
00153
00154   // timebuffer_time_in_seconds is the time in seconds since midnight
(00:00:00), January 1, 1970,
00155   // coordinated universal time (UTC). Julian date for (00:00:00),
January 1, 1970 is: 2440587.5 [days]
00156
00157   // convert timebuffer.time from seconds to days
00158   timebuffer_time_in_days = timebuffer_time_in_seconds/SECONDS_IN_DAY; /
/ days since julian date 2440587.5000000 [days]
00159
00160   // convert to julian date
00161   *julian_date = TIMECONV_JULIAN_DATE_START_OF_PC_TIME +
timebuffer_time_in_days;
00162
00163   result = TIMECONV_DetermineUTCOffset( *julian_date, utc_offset );
00164   if( result == FALSE )
00165     return FALSE;
00166
00167   result = TIMECONV_GetGPSTimeFromJulianDate(
00168     *julian_date,
00169     *utc_offset,
00170     gps_week,
00171     gps_tow );
00172   if( result == FALSE )
00173     return FALSE;
00174
00175   result = TIMECONV_GetUTCTimeFromJulianDate(
00176     *julian_date,
00177     utc_year,
00178     utc_month,
00179     utc_day,
00180     utc_hour,
00181     utc_minute,
00182     utc_seconds );
00183   if( result == FALSE )
00184     return FALSE;
00185
00186   return TRUE;
00187 }
00188
00189
00190 BOOL TIMECONV_GetJulianDateFromGPSTime(
00191   const unsigned short    gps_week,      //!< GPS week (0-1024+)
[week]
00192   const double            gps_tow,       //!< GPS time of week (0-
604800.0)  [s]
00193   const unsigned char     utc_offset,    //!< Integer seconds that GPS is
ahead of UTC time, always positive [s]
00194   double*                 julian_date    //!< Number of days since noon
Universal Time Jan 1, 4713 BCE (Julian calendar) [days]
00195   )
00196 {
00197   if( gps_tow < 0.0  || gps_tow > 604800.0 )
00198     return FALSE;
00199
00200   // GPS time is ahead of UTC time and Julian time by the UTC offset
00201   *julian_date = (gps_week + (gps_tow-utc_offset)/604800.0)*7.0 +
TIMECONV_JULIAN_DATE_START_OF_GPS_TIME;
00202   return TRUE;
00203 }
00204
00205
00206 BOOL TIMECONV_GetJulianDateFromUTCTime(
00207  const unsigned short     utc_year,      //!< Universal Time Coordinated
[year]
00208  const unsigned char      utc_month,     //!< Universal Time Coordinated
[1-12 months]
00209  const unsigned char      utc_day,       //!< Universal Time Coordinated
[1-31 days]
00210  const unsigned char      utc_hour,      //!< Universal Time Coordinated
[hours]
00211  const unsigned char      utc_minute,    //!< Universal Time Coordinated
[minutes]
00212  const float              utc_seconds,   //!< Universal Time Coordinated
[s]
00213  double*                  julian_date    //!< Number of days since noon
Universal Time Jan 1, 4713 BCE (Julian calendar) [days]
00214  )
00215 {
00216   double y; // temp for year
00217   double m; // temp for month
00218   BOOL result;
00219
00220   // Check the input.
00221   result = TIMECONV_IsUTCTimeValid( utc_year, utc_month, utc_day,
utc_hour, utc_minute, utc_seconds );
00222   if( result == FALSE )
00223     return FALSE;
00224
00225   if( utc_month <= 2 )
00226   {
00227     y = utc_year - 1;
00228     m = utc_month + 12;
00229   }
00230   else
00231   {
00232     y = utc_year;
00233     m = utc_month;
00234   }
00235
00236   *julian_date = (int)(365.25*y) + (int)(30.6001*(m+1.0)) + utc_day +
utc_hour/24.0 + utc_minute/1440.0 + utc_seconds/86400.0 + 1720981.5;
00237   return TRUE;
00238 }
00239
00240
00241
00242 BOOL TIMECONV_GetGPSTimeFromJulianDate(
00243   const double            julian_date, //!< Number of days since noon
Universal Time Jan 1, 4713 BCE (Julian calendar) [days]
00244   const unsigned char     utc_offset,  //!< Integer seconds that GPS is
ahead of UTC time, always positive [s]
00245   unsigned short*         gps_week,    //!< GPS week (0-1024+)
[week]
00246   double*                 gps_tow      //!< GPS time of week [s]
00247   )
00248 {
00249   // Check the input.
00250   if( julian_date < 0.0 )
00251     return FALSE;
00252
00253   *gps_week = (unsigned short)((julian_date -
TIMECONV_JULIAN_DATE_START_OF_GPS_TIME)/7.0); //
00254
00255   *gps_tow   = (julian_date -
TIMECONV_JULIAN_DATE_START_OF_GPS_TIME)*SECONDS_IN_DAY; // seconds since start
of gps time [s]
00256   *gps_tow  -= (*gps_week)*SECONDS_IN_WEEK;
// seconds into the current week [s]
00257
00258   // however, GPS time is ahead of utc time by the UTC offset (and thus
the Julian date as well)
00259   *gps_tow += utc_offset;
00260   if( *gps_tow > SECONDS_IN_WEEK )
00261   {
00262     *gps_tow  -= SECONDS_IN_WEEK;
00263     *gps_week += 1;
00264   }
00265   return TRUE;
00266 }
00267
00268
00269 BOOL TIMECONV_GetUTCTimeFromJulianDate(
00270   const double        julian_date,  //!< Number of days since noon
Universal Time Jan 1, 4713 BCE (Julian calendar) [days]
00271   unsigned short*     utc_year,     //!< Universal Time Coordinated
[year]
00272   unsigned char*      utc_month,    //!< Universal Time Coordinated
[1-12 months]
00273   unsigned char*      utc_day,      //!< Universal Time Coordinated
[1-31 days]
00274   unsigned char*      utc_hour,     //!< Universal Time Coordinated
[hours]
00275   unsigned char*      utc_minute,   //!< Universal Time Coordinated
[minutes]
00276   float*              utc_seconds   //!< Universal Time Coordinated
[s]
00277   )
00278 {
00279   int a, b, c, d, e; // temporary values
00280
00281   unsigned short year;
00282   unsigned char month;
00283   unsigned char day;
00284   unsigned char hour;
00285   unsigned char minute;
00286   unsigned char days_in_month = 0;
00287   double td; // temporary double
00288   double seconds;
00289   BOOL result;
00290
00291   // Check the input.
00292   if( julian_date < 0.0 )
00293     return FALSE;
00294
00295   a = (int)(julian_date+0.5);
00296   b = a + 1537;
00297   c = (int)( ((double)b-122.1)/365.25 );
00298   d = (int)(365.25*c);
00299   e = (int)( ((double)(b-d))/30.6001 );
00300
00301   td      = b - d - (int)(30.6001*e) + fmod( julian_date+0.5, 1.0 );   /
/ [days]
00302   day     = (unsigned char)td;
00303   td     -= day;
00304   td     *= 24.0;        // [hours]
00305   hour    = (unsigned char)td;
00306   td     -= hour;
00307   td     *= 60.0;        // [minutes]
00308   minute  = (unsigned char)td;
00309   td     -= minute;
00310   td     *= 60.0;        // [s]
00311   seconds = td;
00312   month   = (unsigned char)(e - 1 - 12*(int)(e/14));
00313   year    = (unsigned short)(c - 4715 - (int)( (7.0+(double)month) / 10.0
));
00314
00315   // check for rollover issues
00316   if( seconds >= 60.0 )
00317   {
00318     seconds -= 60.0;
00319     minute++;
00320     if( minute >= 60 )
00321     {
00322       minute -= 60;
00323       hour++;
00324       if( hour >= 24 )
00325       {
00326         hour -= 24;
00327         day++;
00328
00329         result = TIMECONV_GetNumberOfDaysInMonth( year, month,
&amp;days_in_month );
00330         if( result == FALSE )
00331           return FALSE;
00332
00333         if( day > days_in_month )
00334         {
00335           day = 1;
00336           month++;
00337           if( month > 12 )
00338           {
00339             month = 1;
00340             year++;
00341           }
00342         }
00343       }
00344     }
00345   }
00346
00347   *utc_year       = year;
00348   *utc_month      = month;
00349   *utc_day        = day;
00350   *utc_hour       = hour;
00351   *utc_minute     = minute;
00352   *utc_seconds    = (float)seconds;
00353
00354   return TRUE;
00355 }
00356
00357 BOOL TIMECONV_GetGPSTimeFromUTCTime(
00358   unsigned short     utc_year,     //!< Universal Time Coordinated
[year]
00359   unsigned char      utc_month,    //!< Universal Time Coordinated    [1-
12 months]
00360   unsigned char      utc_day,      //!< Universal Time Coordinated    [1-
31 days]
00361   unsigned char      utc_hour,     //!< Universal Time Coordinated
[hours]
00362   unsigned char      utc_minute,   //!< Universal Time Coordinated
[minutes]
00363   float              utc_seconds,  //!< Universal Time Coordinated    [s]
00364   unsigned short*    gps_week,     //!< GPS week (0-1024+)
[week]
00365   double*            gps_tow       //!< GPS time of week (0-604800.0) [s]
00366   )
00367 {
00368   double julian_date=0.0;
00369   unsigned char utc_offset=0;
00370   BOOL result;
00371
00372   // Check the input.
00373   result = TIMECONV_IsUTCTimeValid( utc_year, utc_month, utc_day,
utc_hour, utc_minute, utc_seconds );
00374   if( result == FALSE )
00375     return FALSE;
00376
00377   result = TIMECONV_GetJulianDateFromUTCTime(
00378     utc_year,
00379     utc_month,
00380     utc_day,
00381     utc_hour,
00382     utc_minute,
00383     utc_seconds,
00384     &amp;julian_date );
00385   if( result == FALSE )
00386     return FALSE;
00387
00388   result = TIMECONV_DetermineUTCOffset( julian_date, &amp;utc_offset );
00389   if( result == FALSE )
00390     return FALSE;
00391
00392   result = TIMECONV_GetGPSTimeFromJulianDate(
00393     julian_date,
00394     utc_offset,
00395     gps_week,
00396     gps_tow );
00397   if( result == FALSE )
00398     return FALSE;
00399
00400   return TRUE;
00401 }
00402
00403
00404
00405 BOOL TIMECONV_GetGPSTimeFromRinexTime(
00406   unsigned short     utc_year,     //!< Universal Time Coordinated
[year]
00407   unsigned char      utc_month,    //!< Universal Time Coordinated    [1-
12 months]
00408   unsigned char      utc_day,      //!< Universal Time Coordinated    [1-
31 days]
00409   unsigned char      utc_hour,     //!< Universal Time Coordinated
[hours]
00410   unsigned char      utc_minute,   //!< Universal Time Coordinated
[minutes]
00411   float              utc_seconds,  //!< Universal Time Coordinated    [s]
00412   unsigned short*    gps_week,     //!< GPS week (0-1024+)
[week]
00413   double*            gps_tow       //!< GPS time of week (0-604800.0) [s]
00414   )
00415 {
00416   double julian_date=0.0;
00417   unsigned char utc_offset=0;
00418   BOOL result;
00419
00420   // Check the input.
00421   result = TIMECONV_IsUTCTimeValid( utc_year, utc_month, utc_day,
utc_hour, utc_minute, utc_seconds );
00422   if( result == FALSE )
00423     return FALSE;
00424
00425   result = TIMECONV_GetJulianDateFromUTCTime(
00426     utc_year,
00427     utc_month,
00428     utc_day,
00429     utc_hour,
00430     utc_minute,
00431     utc_seconds,
00432     &amp;julian_date );
00433   if( result == FALSE )
00434     return FALSE;
00435
00436   result = TIMECONV_GetGPSTimeFromJulianDate(
00437     julian_date,
00438     utc_offset,
00439     gps_week,
00440     gps_tow );
00441   if( result == FALSE )
00442     return FALSE;
00443
00444   return TRUE;
00445 }
00446
00447
00448
00449 BOOL TIMECONV_GetUTCTimeFromGPSTime(
00450   unsigned short     gps_week,     //!< GPS week (0-1024+)
[week]
00451   double             gps_tow,      //!< GPS time of week (0-604800.0) [s]
00452   unsigned short*    utc_year,     //!< Universal Time Coordinated
[year]
00453   unsigned char*     utc_month,    //!< Universal Time Coordinated    [1-
12 months]
00454   unsigned char*     utc_day,      //!< Universal Time Coordinated    [1-
31 days]
00455   unsigned char*     utc_hour,     //!< Universal Time Coordinated
[hours]
00456   unsigned char*     utc_minute,   //!< Universal Time Coordinated
[minutes]
00457   float*             utc_seconds   //!< Universal Time Coordinated    [s]
00458   )
00459 {
00460   double julian_date = 0.0;
00461   unsigned char utc_offset = 0;
00462   int i;
00463   BOOL result;
00464
00465   if( gps_tow < 0.0  || gps_tow > 604800.0 )
00466     return FALSE;
00467
00468   // iterate to get the right utc offset
00469   for( i = 0; i < 4; i++ )
00470   {
00471     result = TIMECONV_GetJulianDateFromGPSTime(
00472       gps_week,
00473       gps_tow,
00474       utc_offset,
00475       &amp;julian_date );
00476     if( result == FALSE )
00477       return FALSE;
00478
00479     result = TIMECONV_DetermineUTCOffset( julian_date, &amp;utc_offset );
00480     if( result == FALSE )
00481       return FALSE;
00482   }
00483
00484   result = TIMECONV_GetUTCTimeFromJulianDate(
00485     julian_date,
00486     utc_year,
00487     utc_month,
00488     utc_day,
00489     utc_hour,
00490     utc_minute,
00491     utc_seconds );
00492   if( result == FALSE )
00493     return FALSE;
00494
00495   return TRUE;
00496 }
00497
00498
00499
00500 BOOL TIMECONV_DetermineUTCOffset(
00501   double julian_date,       //!< Number of days since noon Universal Time
Jan 1, 4713 BCE (Julian calendar) [days]
00502   unsigned char* utc_offset //!< Integer seconds that GPS is ahead of UTC
time, always positive             [s], obtained from a look up table
00503   )
00504 {
00505   if( julian_date < 0.0 )
00506     return FALSE;
00507
00508   if(      julian_date < 2444786.5000 ) *utc_offset = 0;
00509   else if( julian_date < 2445151.5000 ) *utc_offset = 1;
00510   else if( julian_date < 2445516.5000 ) *utc_offset = 2;
00511   else if( julian_date < 2446247.5000 ) *utc_offset = 3;
00512   else if( julian_date < 2447161.5000 ) *utc_offset = 4;
00513   else if( julian_date < 2447892.5000 ) *utc_offset = 5;
00514   else if( julian_date < 2448257.5000 ) *utc_offset = 6;
00515   else if( julian_date < 2448804.5000 ) *utc_offset = 7;
00516   else if( julian_date < 2449169.5000 ) *utc_offset = 8;
00517   else if( julian_date < 2449534.5000 ) *utc_offset = 9;
00518   else if( julian_date < 2450083.5000 ) *utc_offset = 10;
00519   else if( julian_date < 2450630.5000 ) *utc_offset = 11;
00520   else if( julian_date < 2451179.5000 ) *utc_offset = 12;
00521   else if( julian_date < 2453736.5000 ) *utc_offset = 13;
00522   else                                  *utc_offset = 14;
00523
00524   return TRUE;
00525 }
00526
00527
00528
00529
00530 BOOL TIMECONV_GetNumberOfDaysInMonth(
00531   const unsigned short year,        //!< Universal Time Coordinated
[year]
00532   const unsigned char month,        //!< Universal Time Coordinated
[1-12 months]
00533   unsigned char* days_in_month      //!< Days in the specified month
[1-28|29|30|31 days]
00534   )
00535 {
00536   BOOL is_a_leapyear;
00537   unsigned char utmp = 0;
00538
00539   is_a_leapyear = TIMECONV_IsALeapYear( year );
00540
00541   switch(month)
00542   {
00543   case  1: utmp = TIMECONV_DAYS_IN_JAN; break;
00544   case  2: if( is_a_leapyear ){ utmp = 29; }else{ utmp = 28; }break;
00545   case  3: utmp = TIMECONV_DAYS_IN_MAR; break;
00546   case  4: utmp = TIMECONV_DAYS_IN_APR; break;
00547   case  5: utmp = TIMECONV_DAYS_IN_MAY; break;
00548   case  6: utmp = TIMECONV_DAYS_IN_JUN; break;
00549   case  7: utmp = TIMECONV_DAYS_IN_JUL; break;
00550   case  8: utmp = TIMECONV_DAYS_IN_AUG; break;
00551   case  9: utmp = TIMECONV_DAYS_IN_SEP; break;
00552   case 10: utmp = TIMECONV_DAYS_IN_OCT; break;
00553   case 11: utmp = TIMECONV_DAYS_IN_NOV; break;
00554   case 12: utmp = TIMECONV_DAYS_IN_DEC; break;
00555   default: return FALSE; break;
00556   }
00557
00558   *days_in_month = utmp;
00559
00560   return TRUE;
00561 }
00562
00563
00564
00565
00566 BOOL TIMECONV_IsALeapYear( const unsigned short year )
00567 {
00568   BOOL is_a_leap_year = FALSE;
00569
00570   if( (year%4) == 0 )
00571   {
00572     is_a_leap_year = TRUE;
00573     if( (year%100) == 0 )
00574     {
00575       if( (year%400) == 0 )
00576       {
00577         is_a_leap_year = TRUE;
00578       }
00579       else
00580       {
00581         is_a_leap_year = FALSE;
00582       }
00583     }
00584   }
00585   if( is_a_leap_year )
00586   {
00587     return TRUE;
00588   }
00589   else
00590   {
00591     return FALSE;
00592   }
00593 }
00594
00595
00596
00597
00598
00599 BOOL TIMECONV_GetDayOfYear(
00600   const unsigned short utc_year,    // Universal Time Coordinated
[year]
00601   const unsigned char  utc_month,   // Universal Time Coordinated
[1-12 months]
00602   const unsigned char  utc_day,     // Universal Time Coordinated
[1-31 days]
00603   unsigned short*      dayofyear    // number of days into the year (1-
366) [days]
00604   )
00605 {
00606   unsigned char days_in_feb = 0;
00607   BOOL result;
00608   result = TIMECONV_GetNumberOfDaysInMonth( utc_year, 2, &amp;days_in_feb
);
00609   if( result == FALSE )
00610     return FALSE;
00611
00612   switch( utc_month )
00613   {
00614   case  1: *dayofyear = utc_day; break;
00615   case  2: *dayofyear = (unsigned short)(TIMECONV_DAYS_IN_JAN
+ utc_day); break;
00616   case  3: *dayofyear = (unsigned short)(TIMECONV_DAYS_IN_JAN +
days_in_feb + utc_day); break;
00617   case  4: *dayofyear = (unsigned short)(62          + days_in_feb +
utc_day); break;
00618   case  5: *dayofyear = (unsigned short)(92          + days_in_feb +
utc_day); break;
00619   case  6: *dayofyear = (unsigned short)(123         + days_in_feb +
utc_day); break;
00620   case  7: *dayofyear = (unsigned short)(153         + days_in_feb +
utc_day); break;
00621   case  8: *dayofyear = (unsigned short)(184         + days_in_feb +
utc_day); break;
00622   case  9: *dayofyear = (unsigned short)(215         + days_in_feb +
utc_day); break;
00623   case 10: *dayofyear = (unsigned short)(245         + days_in_feb +
utc_day); break;
00624   case 11: *dayofyear = (unsigned short)(276         + days_in_feb +
utc_day); break;
00625   case 12: *dayofyear = (unsigned short)(306         + days_in_feb +
utc_day); break;
00626   default: return FALSE; break;
00627   }
00628
00629   return TRUE;
00630 }
00631
00632
00633 BOOL TIMECONV_GetGPSTimeFromYearAndDayOfYear(
00634  const unsigned short year,      // The year [year]
00635  const unsigned short dayofyear, // The number of days into the year (1-
366) [days]
00636  unsigned short*      gps_week,  //!< GPS week (0-1024+)
[week]
00637  double*              gps_tow    //!< GPS time of week (0-604800.0) [s]
00638  )
00639 {
00640   BOOL result;
00641   double julian_date = 0;
00642
00643   if( gps_week == NULL )
00644     return FALSE;
00645   if( gps_tow == NULL )
00646     return FALSE;
00647   if( dayofyear > 366 )
00648     return FALSE;
00649
00650   result = TIMECONV_GetJulianDateFromUTCTime(
00651     year,
00652     1,
00653     1,
00654     0,
00655     0,
00656     0,
00657     &amp;julian_date
00658     );
00659   if( result == FALSE )
00660     return FALSE;
00661
00662   julian_date += dayofyear - 1; // at the start of the day so -1.
00663
00664   result = TIMECONV_GetGPSTimeFromJulianDate(
00665     julian_date,
00666     0,
00667     gps_week,
00668     gps_tow );
00669
00670   return result;
00671 }
