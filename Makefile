
HSFLAGS = -Wall

.PHONY: clean

flp21-fun: *.hs
	ghc $(HSFLAGS) Main.hs -o flp21-fun

clean:
	rm flp21-fun *.hi *.o
