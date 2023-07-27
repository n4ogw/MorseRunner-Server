PREFIX=/usr/local

all:
	lazbuild MorseRunner-Server.lpr
	strip lib/x86_64-linux/MorseRunner-Server

clean:
	rm -rf lib
install:
	install -d $(PREFIX)/bin
	install -d $(PREFIX)/share/morserunner-server

	install -o root -m 755 ./lib/x86_64-linux/MorseRunner-Server $(PREFIX)/bin
	install -o root -m 755 start-morserunner-server $(PREFIX)/bin
	install -o root -m 644 Master.dta $(PREFIX)/share/morserunner-server
	install -o root -m 644 Readme.txt $(PREFIX)/share/morserunner-server
