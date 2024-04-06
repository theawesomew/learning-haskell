all:
	mkdir -p bin
	ghc index.hs -o bin/index

clean:
	rm index.o index.hi
	rm -r bin
