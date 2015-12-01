Main: Main.hs
	ghc -O2 Main.hs

clean:
	rm *.o
	rm *.hi

distclean: clean
	rm Main

