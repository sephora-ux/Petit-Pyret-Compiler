all: pyretc.exe
	dune exec ./pyretc.exe test.arr

tests: pyretc.exe
	./test -1 ./pyretc.exe

pyretc.exe:
	dune build pyretc.exe

clean:
	dune clean

.PHONY: all clean pyretc.exe