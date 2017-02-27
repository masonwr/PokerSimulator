all:
	ghc -outputdir /tmp Main.hs
test:
	./Main ./IO_examples/poker.in
clean:
	rm Main
