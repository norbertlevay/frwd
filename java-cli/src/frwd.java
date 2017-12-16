//package business.device;


import business.device.NativeDevice;


import java.io.PrintStream;
import java.io.*;
import java.util.*; // class Date
import java.text.*; // SimpleDateFormat

import java.net.URL;
import java.net.URLClassLoader;



public class frwd //extends NativeDevice
{
    static int COMPORT = 1;
    static String LOGPATH = ".";//"C:\\Program Files";

    private static void usage(){
        System.out.println("Version: 0.1.0");
        System.out.println("Usage:");
        System.out.println("\tfrwd <cmd> [cmd-params] [--settings <coma separated setting list>]\t print usage help");
        System.out.println("\tSettings:");
        System.out.println("\t<COMPORT>,<INSTALLPATH>,<LOGDIR>");
        System.out.println("\tCommands:");
        System.out.println("\tfrwd help\t print usage help");
        System.out.println("\tfrwd info\t print logs and their length");
        System.out.println("\tfrwd getlog i\t download i-th log");
        System.out.println("\tfrwd clean\t clean recorder's flash memory");
//        System.out.println("\tfrwd fwd2xml\t convert stored fwd-log to readbale frwd.xml  NOT IMPLEMENTED");
    }

    public static void printSettings(){
        System.out.println("COMPORT: \t" + COMPORT);
        System.out.println("LOGPATH: \t" + LOGPATH);
    }
    // settings is ordered coma-separated list
    public static void readSettings(String settings){
        String[] separated = settings.split(",");
        COMPORT  = Integer.valueOf(separated[0]);
        LOGPATH = separated[1];
    }

    public static void main(String[] args)
        throws IOException,EOFException
        {
            // print all classpath
        /*    ClassLoader cl = ClassLoader.getSystemClassLoader();

            URL[] urls = ((URLClassLoader)cl).getURLs();

            System.err.println("Classpath's:");
            for(URL url: urls){
                System.err.println(url.getFile());
            }
            ///////////////////////
            */
/* debug args
            System.out.println("args.length:\t" + args.length);
            int cnt=0;
            for(String s: args){
                System.out.println(cnt++ + ":\t" + s);
            }
*/

            if (args.length > 0) {
                try {

                    // set globals
                    if ((args.length > 2) && "--settings".equals(args[args.length - 2])) {
                        readSettings(args[args.length - 1]);
                    }
                    printSettings();

                    if ("help".equals(args[0])){
                        usage();
                    }
                    else if ("info".equals(args[0])){
                        info();
                    }
                    else if ("getlog".equals(args[0])){
                        getlog(Integer.parseInt(args[1]));
                    }
                    else if ("clean".equals(args[0])){
                        // not implemented
                        // NativeDevice :: int clearMemory();
                        clearMemory();
                    }
                    else{
                        usage();
                    }

                } catch (NumberFormatException e) {
                    System.err.println("Argument" + args[0] + " must be an integer.");
                    System.exit(1);
                }
            } else
            {
                usage();
            }

        }

    public static void clearMemory()
//        throws AceException
    {
        NativeDevice nd = new NativeDevice();

        int cp = nd.initializeComPort(COMPORT);
        nd.printErrStr(cp);
        System.out.println("comport: " + cp);
        try{Thread.sleep(100L);}catch(Exception e) {}

        try{Thread.sleep(100L);}catch(Exception e) {}
        String rr = nd.printErrStr(nd.clearMemory());

        System.err.println("clearMemory: rc = " + rr );
/*        if(rc < 0)
            throw new DeviceException(i);
        else
            return;
*/
        try{Thread.sleep(100L);}catch(Exception e) {}
        nd.closeLibrary();
    }


    private static void info() {

        NativeDevice nd = new NativeDevice();

//        System.out.println("\nstartGPS: " + nd.startGPS() );
//        try{Thread.sleep(100L);}catch(Exception e) {}
//        System.out.println("NativeDevice: " + nd );
////        try{Thread.sleep(100L);}catch(Exception e) {}
//        System.out.println("getName: GPS? " + nd.getName() );
//        try{Thread.sleep(100L);}catch(Exception e) {}
//        System.out.println("starting Navigation...");
//        nd.startNavigation(true);
////        try{Thread.sleep(100L);}catch(Exception e) {}

        System.out.println("getLibraryVersion: " + nd.getLibraryVersion() );
        try{Thread.sleep(100L);}catch(Exception e) {}
        System.out.println("getVersion: of GPS? " + nd.getVersion() );
        try{Thread.sleep(100L);}catch(Exception e) {}

        int cp = nd.initializeComPort(COMPORT);
        nd.printErrStr(cp);
        System.out.println("comport: " + cp);
        try{Thread.sleep(100L);}catch(Exception e) {}
//        try{Thread.sleep(100L);}catch(Exception e) {}
        int logcnt = 0;
        nd.printErrStr(logcnt=nd.getLogCount());
        System.out.println("logcnt : " + logcnt);

        int logivl = 0;
        nd.printErrStr(logivl=nd.getLogInterval());
        System.out.println("logivl : " + logivl);
        try{Thread.sleep(100L);}catch(Exception e) {}

        // read how many logs there are and list item count for each log
        int ii,nit;
        for(ii=1;ii<=logcnt;ii++)   {
            try{Thread.sleep(100L);}catch(Exception e) {}
            System.out.print("itmcnt[" + ii + "] : " + nd.printErrStr(nit=nd.getItemCount(ii)));

            int totalSecs = nit * logivl/1000;
            int hours = totalSecs / 3600;
            int minutes = (totalSecs % 3600) / 60;
            int seconds = totalSecs % 60;
            String timeString = String.format("%02d:%02d:%02d", hours, minutes, seconds);
            System.out.println("\t" + timeString);
        }

        System.out.println("Closing library...");
        try{Thread.sleep(100L);}catch(Exception e) {}
        nd.closeLibrary();
        // forceClose(); // needed if recorder not turned on, but Dongle plugged in (no radio communication, but Dongles serial port available)
    }

    private static void getlog(int ilog)
    throws FileNotFoundException{
        // FIXME is nd needed for calls nd.xxxx() ?
        // arent those static? NativeDevice.xxx() ??

        NativeDevice nd = new NativeDevice();

        int cp = nd.initializeComPort(COMPORT);
        nd.printErrStr(cp);
        try{Thread.sleep(100L);}catch(Exception e) {}

        int logivl = 0;
        nd.printErrStr(logivl=nd.getLogInterval());
        try{Thread.sleep(100L);}catch(Exception e) {}

        StringBuffer stringbuffer = new StringBuffer();

        // start download ... kicks off callbacks
        // readItems of i:th log to stringBuffer
        int nitems = nd.readItems(ilog, stringbuffer);
        // Note: nitemsHW = nitemsBuff - 1
        //int nit=nd.getItemCount(ilog);
        //System.err.println(nitems + "<-buff hw->"+ nit);

        // if stringbuffer empty exit :
        // to avoid creating empty files
        if(stringbuffer.length() == 0){
            System.out.println("Log stringbuffer is empty.");
            return;
        }

        // redirect System.out to file
        Date date = new Date() ;
        SimpleDateFormat dateFormat =
            new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss-SSS") ;

        String filename = LOGPATH + "\\"
            + dateFormat.format(date) + ".frwd.csv";
        System.setOut(new PrintStream(
         new BufferedOutputStream(
          new FileOutputStream(filename)), true));
        printBuffer( nitems - 1 , stringbuffer);

        try{Thread.sleep(100L);}catch(Exception e) {}
        nd.closeLibrary();
    }

    static void printBuffer(int nitems, StringBuffer stringbuffer){

        // print header

        System.out.print("# ");
        System.out.print("Pressure "           );
        System.out.print("Temperature "        );
        System.out.print("HeartRate "          );
        System.out.print("LapTimeMark "        );
        System.out.print("FigureOfMerit "      );
        System.out.print("Latitude "           );
        System.out.print("Longitude "          );
        System.out.print("GPSAltitude "        );
        System.out.print("GPSWeek "            );
        System.out.print("TimeOfWeek "         );
        System.out.print("HorizontalVelocity " );
        System.out.print("MotionDirection "    );
        System.out.print("VerticalVelocity "   );
        System.out.print("SatellitesInView "   );
        System.out.print("FixInfo "            );
        System.out.print("HDOP "               );
        System.out.print("RR1 "                );
        System.out.print("RR2 "                );
        System.out.print("RR3 "                );
        System.out.print("RR4 "                );
        System.out.println();

        // print values

        StringReader str = new StringReader(stringbuffer.toString());

        try{
            int ii;
        for(ii=0;ii<nitems;ii++)   {

  //          System.out.format("%3d:",(ii+1));

            System.out.format("%8.1f ",(double)NativeDevice.readInt(str) / 100D);
            System.out.format("%5.1f ",(double)NativeDevice.readInt(str) / 10D);
            System.out.format("%5d " , NativeDevice.readInt(str));
//            System.out.print (" " + NativeDevice.readBoolean(str));
            System.out.format ("%s" , (NativeDevice.readBoolean(str) ? " 1" : " 0") );
            System.out.format("%5d " , NativeDevice.readInt(str));
            System.out.format("%9.5f " , NativeDevice.readDouble(str));
            System.out.format("%9.5f " , NativeDevice.readDouble(str));
            System.out.format("%5d " , (int)NativeDevice.readDouble(str));
            System.out.format("%3d " , NativeDevice.readInt(str));
            System.out.format("%3d " , NativeDevice.readInt(str));
            System.out.format("%7.3f " , NativeDevice.readDouble(str));
            System.out.format("%7.3f " , NativeDevice.readDouble(str));
            System.out.format("%7.3f " , NativeDevice.readDouble(str));
            System.out.format("%5d " , NativeDevice.readInt(str));
            System.out.format("%5d " , NativeDevice.readInt(str));
            System.out.format("%6.1f " , NativeDevice.readDouble(str));
            System.out.format("%8d " , NativeDevice.readInt(str));
            System.out.format("%8d " , NativeDevice.readInt(str));
            System.out.format("%8d " , NativeDevice.readInt(str));
            System.out.format("%8d " , NativeDevice.readInt(str));

            System.out.println();

        }
        }catch(Exception e)
        {
            e.printStackTrace();
        }

    }
}
