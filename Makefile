MAIN_FILES = Simulate.hs Main.hs
PROGS = ${MAIN_FILES:.hs=}

.PHONY: clean ${PROGS}

all : ${PROGS};

${PROGS} : % : Makefile
	ghc -Wall -Werror $*.hs

clean :
	rm -rf *.hi *.o ${PROGS}

