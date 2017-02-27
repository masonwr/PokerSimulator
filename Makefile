all:
	ghc -O2 -outputdir /tmp Main.hs
test:
	./Main ./IO_examples/poker.in
clean:
	rm Main
