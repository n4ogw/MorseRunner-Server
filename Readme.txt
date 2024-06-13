                              MORSE RUNNER SERVER 1.13

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
   been tested) and the utility socat

- To install:
  edit Makefile, checking:
       PREFIX : install location, default is /usr/local
       LAZBUILD : location of "lazbuild" utility from Lazarus

- Then

  make
  sudo make install


- To start the program, use one of the two scripts,

  start-morserunner-server-winkey
  start-morserunner-server-so2rmini

  These scripts will create one or two virtual serial port pairs
  using socat (see below) and then start Morserunner Server.

  Enter virtual serial port names (see below) and choose operation mode in
  Settings->Setup... 

- Linux virtual serial port creation - the utility socat can be used to create
  a virtual serial port pair to connect the logging program and Morse
  Runner Server:

  socat -d -d pty,rawer,echo=0,link=/tmp/pty1 pty,rawer,echo=0,link=/tmp/pty2

  This connects two virtual pty devices (/dev/pts/#), linked to the
  files /tmp/pty1 and /tmp/pty2. One device file is entered in the
  "Port" box in Morse Runner Server, and the other as the serial port
  for the so2rMini-N6TR setup in the logging program. If using the scripts
  supplied, the default names are:

  winkey: /tmp/pty1a  (used by Morserunner Server)
          /tmp/pty1   (enter in logging program for winkey port)
  OTRSP:  /tmp/pty2a  (used by Morserunner Server)
          /tmp/pty2   (enter in logging program for OTRSP port)

  So2rMini-N6TR:  /tmp/pty1a (used by Morserunner Server)
                  /tmp/pty1  (enter in logging program)

  Note that the so2rmini handles both keying and audio switching, so
  OTRSP is not used when using the so2rmini.

- The programs should be started in the following sequence: 1. socat 2. Morse
  Runner Server 3. Logging program. The so2rmini requires  only one virtual
  serial port pair, while winkey+OTRSP requires two pairs.



2. Windows

- Required to use program:
  - virtual COM port program such as com0com, https://com0com.sourceforge.net/
  - contest logging program such as N1MM

- Extract the zip file into a folder. All files are kept in this folder.
  Morserunner Server will create an .ini file in the user's folder to
  store settings

- Set up two virtual com port pairs using com0com. The default settings
  work fine. Note the names of the ports, for example COM12+COM13 and
  COM14+COM15.

- MorseRunner-Server.exe is the executable

- Choose operation mode in Settings->Setup... Connect winkey and OTRSP
  to separate COM ports (for example COM12 and COM14).

- In the logging program, connect winkey and OTRSP to the other two COM
  ports, for example COM13 for winkey and COM15 for OTRSP.

- The winkey emulation in Morse Runner Server is very basic and does not
  emulate all winkey functions. For N1MM I unchecked all options in the
  "Winkey" tab of the device setup, and chose "Ignore Winkey speed pot".



Logging program setup (Linux and Windows)
-------------------------------------------


- Besides connecting CW and audio/radio switching devices to Morse Runner
  Server, CW messages must be modified:

- Command numbers must be added to each CW message. Commands are two-digit
  numbers, separated by "+" characters, for example "+01+"
  This numeric command controls the response of stations to the sent message.

- An equals character (=) must be placed at the end of each message.

- Morserunner Server command numbers:

  01 : message is a CQ; stations will call
  02 : message is a contest exchange
  03 : message is a "TU" signaling qso is done
  05 : message contains the other station's callsign. Important: the call must be
       sent at the beginning of a message, and +05+ must immediately follow it.
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

Note that F3 TU has +05+ at its beginning because N1MM will send
callsign corrections before the TU message if the call was copied
incorrectly and then corrected.

###################
#   RUN Messages 
###################
F1 CQ,+01+TEST {MYCALL}=
F2 EXCH,{SENTRSTCUT} +02+{EXCH}=
F3 Tu,+05+Tu +03+{MYCALL}=
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


Morserunner Server features
----------------------------


- 2BSIQ: Two radios are enabled by default and Morse Runner Server
  will respond to CQs on both radios. To switch audio between
  radios (left/right/split), use the normal audio and radio switching
  keys in your logging program (for example, ~ in SO2SDR and N1MM switches to
  split audio). Station and Band conditions are the same for both radios.

- Cabrillo option:

  In the default (WPX) mode, callsigns are selected at random from the Master.DTA
  file. In Cabrillo mode, the program will read a Cabrillo file and choose
  stations (and their exchange) randomly from this file.

  - Under file, "Load Cabrillo". Select the Cabrillo file you want to use.
  - Select Cabrillo mode in Settings->Setup...

  When the file is loaded, dupe calls will be ignored. In the pileup,
  calls are simply chosen randomly from those in the file; calls may
  repeat, which will be more noticeable if it is a small file. The
  program simply sends the exchange as it is in the Cabrillo file- this
  may not be as the exchange is normally sent in some contests, for
  example if the callsign is part of the exchange (Sweepstakes, Sprint, etc).

-  Master.dta file:

  In WPX mode callsigns are taken from a Supercheck Partial file Master.dta;
  you can place an updated Master.dta in .local/share/morserunner-server (Linux),
  or in the program folder (Windows).


- Other options: 

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

    QRN - this generates the background noise. There are several options; the first
       three are identical to the original Morserunner:
       
       1. none : no noise at all

       2. random : synthetic random noise

       3. random + QRN : synthetic random noise and QRN bursts

       4. WAV : read a WAV file containing background noise. Morserunner server
         will read up to 30 seconds from the file, which will be looped continuously.
	 The second radio uses the same WAV file, but with the audio offset by 15
	 seconds. The amplitude is set by the slider. The higher the WAV amplitude,
	 the weaker stations will be. One sample WAV file is included (in Linux it
	 is located in $PREFIX/share/morserunner-server). If you want to
	 record  your own noise, the WAV file must have these parameters: mono,
	 sample rate = 22050 Hz, 16 bit sample size. Record a file of about 30
	 seconds in length, the program will only read the first 30 seconds if the
	 file.

     QRM - interference form other running stations occurs from time to time.

     QSB - signal strength varies with time (Rayleigh fading channel).

     Flutter - some stations have "auroral" sound.

     LIDS - some stations call you when you are working another station,
       make mistakes when they send code, copy your messages incorrectly,
       and send RST other than 599.

     Activity - band activity, determines how many stations on average
       reply to your CQ.


- Audio buffer size

    You can adjust the audio buffer size by changing the BufSize value in the
    MorseRunner.ini file. Acceptable values are 1 through 5, the default is 3.
    Increase the buffer size for smooth audio without clicks and interruptions;
    decrease the size for faster response to keyboard commands.

- Serial port poll time

    This controls how often the virtual serial ports are polled. The value is in
    milliseconds and can be changed in the ini file, see SerialPollTime.

VERSION HISTORY

1.13
	-fix Linux compile bug. No change to Windows version
1.12
	-add option to read WAV file for background radio noise
1.11
	-when reading Cabrillo files, replace RST (599 or 59) in the
	first exchange slot with 5NN or randomly generated RST
	-fix Makefile for linux build
1.10
	support both Linux and Windows
	add support for Winkey and OTRSP emulation
	message format changed from 1.01

1.00	Initial release. Support So2rmini-N6TR under linux


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
