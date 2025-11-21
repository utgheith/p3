


DFY_FILES = ${wildcard *.dfy}
VERIFY_TARGETS = ${patsubst %.dfy,%.verify,${DFY_FILES}}
RUN_FILES = ${patsubst %.dfy, %, ${DFY_FILES}}

DFY_FLAGS = --allow-warnings --extract-counterexample --analyze-proofs

all : verify;

test : verify;

${VERIFY_TARGETS} : %.verify : Makefile %.dfy
	dafny verify ${DFY_FLAGS} $*.dfy > $*.verify 2>&1

verify: ${VERIFY_TARGETS};

	
clean :
	rm -rf *.verify ${RUN_FILES}

