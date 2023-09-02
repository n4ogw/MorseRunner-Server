                              MORSE RUNNER SERVER 1.1

                              Contest Simulator

               Torsten Clay N4OGW

               based on MORSE RUNNER by Alex Shovkoplyas, VE3NEA,
               http://www.dxatlas.com/MorseRunner/

	       and Linux port from Zach Metzinger, N0ZGO

DESCRIPTION
-------------

 Morse Runner Server is a CW radio contest simulator based on Morse Runner.
 Unlike the original Morse Runner, Morse Runner Server must be used in
 conjunction with a contest logging program. Morse Runner Server is controlled
 by the logging program through a virtual serial port, with Morse Runner
 Server emulating a CW device (Winkey or So2rMini-N6TR).  Morse Runner
 Server also  simulates two-radio contesting,  with two separate audio
 streams for 2BSIQ practice. In addition to the WPX contest mode from the
 original Morse Runner (exchange RST #), it is  possible to read in a
 Cabrillo file. In Cabrillo mode, callsigns and  contest exchanges are
 read from the Cabrillo file, which makes it possible  to practice for a
 wide variety of contests.


PLATFORMS
-------------

  - Linux
  - Windows


INSTALLATION
-------------

1. Linux

- Required to build program: Lazarus, fpc, SDL2 library
- Required to use: contest logging program (SO2SDR and TRlinux have
   been tested), and the utility socat
- To install: edit Makefile, adjust PREFIX (install location) and
  LAZBUILD (location of lazbuild utility from Lazarus). Then

  make
  sudo make install

  Enter virtual serial port names (see below) and choose operation mode in
  Settings->Setup...

  Linux virtual serial port creation - the utility socat can be used to create
  a virtual serial port pair to connect the logging program and Morse
  Runner Server:

  socat -d -d pty,rawer,echo=0,link=/tmp/pty1 pty,rawer,echo=0,link=/tmp/pty2

  This connects two virtual pty devices (/dev/pts/#), linked to the
  files /tmp/pty1 and /tmp/pty2. One device file is entered in the
  "Port" box in Morse Runner Server, and the other as the serial port
  for the so2rMini-N6TR setup in the logging program.  The programs
  should be started in the following sequence: 1. socat 2. Morse
  Runner Server 3. Logging program. Bash scripts
  start-morserunner-server-so2rmini and  start-morserunner-server-winkey
  are  included which automate running socat  and Morse Runner Server,
  either when using the so2rmini or winkey+OTRSP. The so2rmini requires
  only one virtual serial port pair, while winkey+OTRSP requires two pairs.



2. Windows

- Extract the zip file into a folder
- MorseRunner-Server.exe is the executable

  Enter virtual serial port names (see below) and choose operation mode in
  Settings->Setup...

  To use Morse Runner Server a virtual com port program and a contest
  logging program such as N1MM are required.  com0com
  (https://com0com.sourceforge.net/) works well to create the virtual
  ports using its default settings.

  -Create two virtual com port pairs, for example COM12+COM13 and COM14+COM15.
  In the logging program, connect winkey to COM12, and enter COM13 in the
  Morse Runner Server winkey box (Settings->Setup...)

  In the logging program, use COM14 for OTRSP radio/audio switching, and enter
  COM15 in Morse Runner Server in the OTRSP box. The default port pairs used are:

  COM12: MorseRunnerServer winkey   <-> COM13: logging program winkey
  COM14: MorseRunnerServer OTRSP    <-> COM15: logging program OTRSP
  COM16: MorseRunnerServer So2rmini-N6TR <-> COM17: logging program So2rmini-N6TR

  The winkey emulation in Morse Runner Server is very basic and does not
  emulate all winkey functions. For N1MM I unchecked all options in the "Winkey" tab
  of the device setup, and chose "Ignore Winkey speed pot".


LOGGING PROGRAM SETUP (Linux and Windows)
-----------------------------------------------

  - Besides connecting CW and audio/radio switching devices to Morse Runner Server,
  CW messages must be modified:

  - Command numbers must be added to the CW messages. These are of
  the format "+xx+", where xx is a two-digit number.
  This number controls the response of stations to the sent message.

  - An equals character (=) must be placed at the end of each message.

  Possible command numbers:

  01 : message is a CQ; stations will call
  02 : message is a contest exchange
  03 : message is a "TU" signaling qso is done
  05 : message contains the other station's callsign. Important: the call must be
       sent at the beginning of a message, and +05+ must immediately follow it.
  06 : QSO B4
  22 : Again? Message is requesting a repeat

1. Example WPX messages for SO2SDR (Linux):
  
  F1:  +01+TEST {CALL}=
  F2:  +01+TEST {CALL} {CALL}=
  F3:  +01+{CALL}=
  F9:  +22+?=
  CQ Exch: {CALL_ENTERED}+05+ +02+5NN {#}=
  QSL Msg: +03+TU {CALL}=
  Quick Qsl: +03+TU=
  Call Updated QSL: {CALL_ENTERED}+05+ +03+OK {CALL}=
  F2(Exchange): +02+5NN {#}=
  Shift-F2: +22+AGN?=

2. Example WPX config file for trlog:

  MY CALL = N4OGW                                                                 
  CONTEST = CQ WPX
  DISPLAY MODE = COLOR                                                            
  CQ MEMORY F1 = +01+TEST \=                                                       
  CQ MEMORY F2 = +01+CQ TEST \ \=
  CQ MEMORY F3 = 
  CQ MEMORY F4 =
  CQ MEMORY F5 = 
  CQ MEMORY F6 = 
  CQ MEMORY F7 = 
  CQ MEMORY F8 = +22+AGN=
  CQ MEMORY F9 = +22+?=
  CALL OK NOW MESSAGE = @+05+ +03+TU=
  QSL MESSAGE = +03+TU \=
  QSO BEFORE MESSAGE =  +06+QSO B4 \=
  QUICK QSL MESSAGE= +03+TU=
  S&P EXCHANGE= +02+5NN #=
  CQ EXCHANGE= }+05+ +02+5NN #=
  EX MEMORY F3 = 
  EX MEMORY F4 = +02+NR #=
  EX MEMORY F5 = 
  EX MEMORY F8 = 
  EX MEMORY F9 = +22+?=
  CW TONE = 500
  ardkeyer port = serial /tmp/pty2

3. Example WPX messages for N1MM logger:

###################
#   RUN Messages 
###################
F1 CQ,+01+TEST {MYCALL}=
F2 EXCH,{SENTRSTCUT} +02+{EXCH}=
F3 Tu,Tu +03+{MYCALL}=
F4 {MYCALL},+01+{MYCALL}=
F5 His Call,! +05+=
F6 Repeat,+05++02+{SENTRSTCUT} {EXCH}=
F7 Spare,
F8 Agn?,+22+Agn?=
F9 Nr?,+22+Nr?=
F10 Call?,+22+Cl?=
F11 Spare,
F12 Wipe,{WIPE}
###################
#   S&P Messages 
###################
F1 Qrl?,+22+?=
F2 Exch,{SENTRSTCUT} +02+{EXCH}=
F3 Tu,+03+Tu=
F4 {MYCALL},+03+{MYCALL}=
F5 His Call,!=
F6 Repeat,+05++02+{SENTRSTCUT} {EXCH} {EXCH}=
F7 Spare,
F8 Agn?,+22+Agn?=
F9 Nr?,+22+Nr?=
F10 Call?,+22+Cl?=
F11 Spare,=
F12 Wipe,{WIPE}=


  2BSIQ: Two radios are enabled by default and Morse Runner Server
  will respond to CQs on both radios. To switch audio between
  radios (left/right/split), use the normal audio and radio switching
  keys in your logging program (for example, ~ in SO2SDR and N1MM switches to
  split audio). Station and Band conditions are the same for both radios.

  Cabrillo option:

  - Under file, "Load Cabrillo". Select the Cabrillo file you want to use.
  - Select Cabrillo mode in Settings->Setup...

  When the file is loaded, dupe calls will be ignored. In the pileup,
  calls are simply chosen randomly from those in the file; calls may
  repeat, which will be more noticeable if it is a small file. The
  program simply sends the exchange as it is in the Cabrillo file- this
  may not be as the exchange is normally sent in some contests, for
  example if the callsign is part of the exchange (Sweepstakes, Sprint, etc).


  Call file:

  In WPX mode callsigns are taken from a Supercheck Partial file Master.dta;
  you can place an updated Master.dta in .local/share/morserunner-server (Linux),
  or in the program folder (Windows).


  Other options: these are identical to the original Morse Runner:

  Station

    QSK - simulates the semi-duplex operation of the radio. Enable it if your
      physical radio supports QSK. If it doesn't, enable QSK anyway to see
      what you are missing.

    CW Speed - select the CW speed, in WPM (PARIS system) that matches your
      skills. The calling stations will call you at about the same speed.

    CW Pitch - pitch in Hz.

    RX Bandwidth - the receiver bandwidth, in Hz.

    CW volume - volume of CW sidetone

  Band Conditions

     I tried to make the sound as realistic as possible, and included a few
     effects based on the mathematical model of the ionospheric propagation.
     Also, some of the calling stations exhibit less then perfect operating
     skills, again to make the simulation more realistic. These effects can
     be turned on and off using the checkboxes described below.


     QRM - interference form other running stations occurs from time to time.

     QRN - electrostatic interference.

     QSB - signal strength varies with time (Rayleigh fading channel).

     Flutter - some stations have "auroral" sound.

     LIDS - some stations call you when you are working another station,
       make mistakes when they send code, copy your messages incorrectly,
       and send RST other than 599.

     Activity - band activity, determines how many stations on average
       reply to your CQ.


  Audio buffer size

    You can adjust the audio buffer size by changing the BufSize value in the
    MorseRunner.ini file. Acceptable values are 1 through 5, the default is 3.
    Increase the buffer size for smooth audio without clicks and interruptions;
    decrease the size for faster response to keyboard commands.


VERSION HISTORY

1.1
	support both Linux and Windows
	add support for Winkey and OTRSP emulation
	message format changed from 1.01

1.0	Initial release. Support So2rmini-N6TR under linux


DISCLAIMER OF WARRANTY

THE SOFTWARE PRODUCT IS PROVIDED AS IS WITHOUT WARRANTY OF ANY KIND. TO THE
MAXIMUM EXTENT PERMITTED BY APPLICABLE LAW, THE AUTHOR FURTHER
DISCLAIMS ALL WARRANTIES, INCLUDING WITHOUT LIMITATION ANY IMPLIED WARRANTIES
OF MERCHANTABILITY, FITNESS  FOR A PARTICULAR PURPOSE, AND NONINFRINGEMENT.
THE ENTIRE RISK   ARISING OUT OF THE USE OR PERFORMANCE OF THE SOFTWARE PRODUCT
AND DOCUMENTATION REMAINS WITH RECIPIENT. TO THE MAXIMUM EXTENT PERMITTED BY
APPLICABLE LAW, IN NO EVENT SHALL  THE AUTHOR BE LIABLE FOR ANY
CONSEQUENTIAL, INCIDENTAL, DIRECT, INDIRECT, SPECIAL, PUNITIVE, OR OTHER DAMAGES
WHATSOEVER  (INCLUDING, WITHOUT LIMITATION, DAMAGES FOR LOSS OF PROFITS,
BUSINESS INTERRUPTION, LOSS OF INFORMATION, OR OTHER PECUNIARY LOSS) ARISING
OUT OF THIS AGREEMENT OR THE USE OF OR INABILITY TO USE THE SOFTWARE PRODUCT,
EVEN IF THE AUTHOR HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
DAMAGES.





END OF DOCUMENT
