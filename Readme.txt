                              MORSE RUNNER SERVER 1.00

                              Contest Simulator

               Torsten Clay N4OGW

               based on MORSE RUNNER by Alex Shovkoplyas, VE3NEA,
               http://www.dxatlas.com/MorseRunner/

	       and Lazarus/FPC conversions by Zach Metzinger, N0ZGO

DESCRIPTION

 Morse Runner Server is a CW radio contest simulator based on Morse Runner.
 Unlike the original Morse Runner, Morse Runner Server must be used in
 conjunction with a contest logging program. Morse Runner Server is controlled
 by the logging program through a virtual serial port, with Morse Runner
 Server emulating a CW device. The So2rMini/N6TR so2r/keyer is supported
 in version 1.00. Morse Runner Server simulates two-radio contesting,
 with two separate audio streams for 2BSIQ practice. In addition to the
 WPX contest mode from the original Morse Runner (exchange RST #), it is
 possible to read in a Cabrillo file. In Cabrillo mode, callsigns and
 contest exchanges are read from the Cabrillo file, which makes it possible
 to practice for a wide variety of contests.

PLATFORMS

  - Linux

INSTALLATION

 - Required to build program: Lazarus, fpc, SDL2 library
 - Required to use: contest logging program (SO2SDR and TRlinux have
   been tested), and the utility socat
 - To install:

 make
 sudo make install


CONFIGURATION

  Virtual serial port creation - the utility socat can be used to create
  a virtual serial port pair to connect the logging program and Morse
  Runner Server:

socat -d -d pty,rawer,echo=0,link=/tmp/pty1 pty,rawer,echo=0,link=/tmp/pty2

  This connects two virtual pty devices (/dev/pts/#), linked to the
  files /tmp/pty1 and /tmp/pty2. One device file is entered in the
  "Port" box in Morse Runner Server, and the other as the serial port
  for the so2rMini-N6TR setup in the logging program.  The programs
  should be started in the following sequence: 1. socat 2. Morse
  Runner Server 3. Logging program. A bash script
  start-morserunner-server is included which automates running socat
  and Morse Runner Server.
  
  Logging program setup

  - Assuming /tmp/pty1 is in the Port box, enter /tmp/pty2 for the
  serial port for the so2rMini
  - Emulated so2rmini-N6TR commands:

    send CW
    CW speed
    Headphone switching
    TX output switching
    cancel CW
    so2rMini version
    
  - Command numbers must be added to the CW messages. These are of
  the format "-xx-", where xx is a two-digit number.
  This number controls the response of stations to the sent message.
  Possible commands:

  01 : message is a CQ; stations will call
  02 : message is a contest exchange
  03 : message is a "TU" signaling qso is done
  05 : message contains the other station's callsign. Important: the call must be
       sent at the beginning of a message, and -05- must immediately follow it.
  06 : QSO B4
  22 : Again? Message is requesting a repeat

  Example WPX messages for SO2SDR:
  
  F1:  -01-TEST {CALL}
  F2:  -01-TEST {CALL} {CALL}
  F3:  -01-{CALL}
  F9:  -22-?
  CQ Exch: {CALL_ENTERED}-05- -02-5NN {#}
  QSL Msg: -03-TU {CALL}
  Quick Qsl: -03-TU
  Call Updated QSL: {CALL_ENTERED}-05- -03-OK {CALL}
  F2(Exchange): -02-5NN {#}
  Shift-F2: -22-AGN?

  Example WPX config file for trlog:

  MY CALL = N4OGW                                                                 
  CONTEST = CQ WPX
  DISPLAY MODE = COLOR                                                            
  CQ MEMORY F1 = -01-TEST \                                                       
  CQ MEMORY F2 = -01-CQ TEST \ \
  CQ MEMORY F3 = 
  CQ MEMORY F4 =
  CQ MEMORY F5 = 
  CQ MEMORY F6 = 
  CQ MEMORY F7 = 
  CQ MEMORY F8 = -22-AGN
  CQ MEMORY F9 = -22-?
  CALL OK NOW MESSAGE = @-05- -03-TU
  QSL MESSAGE = -03-TU \
  QSO BEFORE MESSAGE =  -06-QSO B4 \
  QUICK QSL MESSAGE= -03-TU
  S&P EXCHANGE= -02-5NN #
  CQ EXCHANGE= }-05- -02-5NN #
  EX MEMORY F3 = 
  EX MEMORY F4 = -02-NR #
  EX MEMORY F5 = 
  EX MEMORY F8 = 
  EX MEMORY F9 = -22-?
  CW TONE = 500
  ardkeyer port = serial /tmp/pty2


  2BSIQ: Two radios are enabled by default and Morse Runner Server
  will respond to CQs on both radios. To switch audio between
  radios (left/right/split), use the normal audio and radio switching
  keys in your logging program. Station and Band conditions are the
  same for both radios.

  Cabrillo option:

  - Under file, "Load Cabrillo". Select the Cabrillo file you want to use.
  - Select Cabrillo in "Mode"

  When the file is loaded, dupe calls will be ignored. In the pileup,
  calls are simply chosen randomly from those in the file; calls may
  repeat, which will be more noticeable if it is a small file. The
  program simply sends the exchange as it is in the Cabrillo file- this
  may not be as the exchange is normally sent in some contests, for
  example if the callsign is part of the exchange (Sweepstakes, Sprint, etc).


  Settings:

  The .ini file for the program is placed in .local/share/morserunner-server.
  In WPX mode callsigns are taken from a Supercheck Partial file Master.dta;
  you can place an updated Master.dta in .local/share/morserunner-server


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
