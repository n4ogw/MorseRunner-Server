PREFIX=/usr/local
LAZBUILD=/usr/local/src/lazarus/lazbuild 
all:
	$(LAZBUILD) ./BGRABitmap/bgrabitmap/bgrabitmappack.lpk
	$(LAZBUILD) ./uecontrols-master/uecontrols.lpk
	$(LAZBUILD) ./talsoundout/talsoundout.lpk
	$(LAZBUILD) MorseRunner-Server.lpr
	strip lib/x86_64-linux/MorseRunner-Server

clean:
	rm -rf ./lib
	rm -fr ./BGRABitmap/bgrabitmap/lib
	rm -fr ./BGRABitmap/bglcontrols/lib
	rm -fr ./talsoundout/lib
install:
	install -d $(PREFIX)/bin
	install -d $(PREFIX)/share/morserunner-server

	install -o root -m 755 ./lib/x86_64-linux/MorseRunner-Server $(PREFIX)/bin
	install -o root -m 755 start-morserunner-server-so2rmini $(PREFIX)/bin
	install -o root -m 755 start-morserunner-server-winkey $(PREFIX)/bin
	install -o root -m 644 Master.dta $(PREFIX)/share/morserunner-server
	install -o root -m 644 Readme.txt $(PREFIX)/share/morserunner-server
	install -o root -m 644 qrn-500hz.wav $(PREFIX)/share/morserunner-server
