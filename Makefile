G ?= generated

ECHO ?= /bin/echo

TESTCASES_DIR ?= testcases

DFY_FILES = $(notdir $(wildcard $(TESTCASES_DIR)/*.dfy))
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

${RESULT_FILES} : %.result : Makefile $(TESTCASES_DIR)/%.dfy proofs/%.dfy
	@${ECHO} -n "verifying $(TESTCASES_DIR)/$*.dfy ... "
	@(dafny verify ${DFY_FLAGS} $(TESTCASES_DIR)/$*.dfy proofs/$*.dfy > $(TESTCASES_DIR)/$*.out 2> $(TESTCASES_DIR)/$*.err && ${ECHO} "pass" > $(TESTCASES_DIR)/$*.result) || ${ECHO} "fail" > $(TESTCASES_DIR)/$*.result
	@cat $(TESTCASES_DIR)/$*.result
	
clean :
	rm -rf $(TESTCASES_DIR)/*.err $(TESTCASES_DIR)/*.out $(TESTCASES_DIR)/*.result

