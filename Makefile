all:
	ghc index.hs -o index

clean:
	rm index.o index.hi index
