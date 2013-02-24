HC=ghc --make -O2

all:
	for x in *hs ; do $(HC) $$x ; done

clean:
	rm -f *.hi *.o ????
