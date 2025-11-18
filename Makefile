
G ?= generated

ECHO ?= /bin/echo

DFY_FILES = ${wildcard *.dfy}
NAMES = ${patsubst %.dfy,%,$(DFY_FILES)}
PROOF_FILES = ${addprefix proofs/,${DFY_FILES}}
OUT_FILES = ${patsubst %.dfy,%.out,${DFY_FILES}}
ERR_FILES = ${patsubst %.dfy,%.err,${DFY_FILES}}
RESULT_FILES = ${patsubst %.dfy,%.result,${DFY_FILES}}

DFY_FLAGS = --allow-warnings --extract-counterexample --analyze-proofs

all : verify;

test : verify;

verify: ${RESULT_FILES};

proofs/%.dfy:
	mkdir -p proofs
	touch $@

${RESULT_FILES} : %.result : Makefile %.dfy proofs/%.dfy
	@${ECHO} -n "verifying $*.dfy ... "
	@(dafny verify ${DFY_FLAGS} $*.dfy proofs/$*.dfy > $*.out 2> $*.err && ${ECHO} "pass" > $*.result) || ${ECHO} "fail" > $*.result
	@cat $*.result
	
clean :
	rm -rf *.err *.out *.result

