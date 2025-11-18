E2E_DIR ?= .
FUN_FILES = ${shell find ${E2E_DIR}/*.fun}
TEST_NAMES = ${sort ${subst .fun,,${notdir ${FUN_FILES}}}}
OK_FILES = ${addprefix ${E2E_DIR}/,${addsuffix .ok, ${TEST_NAMES}}}
TESTS = ${addsuffix .test, ${TEST_NAMES}}
OUTS = ${addsuffix .out, ${TEST_NAMES}}
DIFFS = ${addsuffix .diff, ${TEST_NAMES}}
RESULTS = ${addsuffix .result, ${TEST_NAMES}}
RUNS = ${addsuffix .run, ${TEST_NAMES}}

HS_FILES = ${shell find . -path './.*' -prune -o \( -name '*.hs' -print \) }

.PHONY : all test clean format

all :
	stack build

${OUTS} : %.out : Makefile ${HS_FILES}
	stack run sim < ${E2E_DIR}/$*.fun > $*.out 2> $*.err || echo "running $* failed, see $*.err"
	-cp sim.debug $*.debug

${DIFFS} : %.diff : ${E2E_DIR}/%.ok %.out Makefile
	(diff -wB $*.out ${E2E_DIR}/$*.ok > $*.diff 2>&1 || true)

${RESULTS} : %.result : %.diff Makefile
	((test -s $*.diff) && (echo "fail" > $*.result)) || (echo "pass" > $*.result)

${TESTS} : %.test : %.result Makefile
	@echo "$* ... `cat $*.result`"

${RUNS} : %.run : Makefile
	@stack run sim < ${E2E_DIR}/$*.fun

test: ${TESTS};


unit :
	(stack test 2> test.stderr) || (echo "tests failed. More data in test.stderr"; false)

format :
	stack exec -- which ormolu > /dev/null 2>&1 || stack install ormolu ; stack exec -- ormolu -i ${HS_FILES}

lint :
	stack exec -- which hlint > /dev/null 2>&1 || stack install hlint ; stack exec -- hlint ${HS_FILES}

clean :
	stack clean
	-rm -rf *.out *.diff *.result *.err *.o *.hi

help:
	@echo Targets
	@echo "    test ... run all tests"
	@echo "    <test_name>.test ... run the given test"
	@echo "    <test_name>.run  ... show the output from running the given test"
	@echo "    clean            ... remove all generated files"
	@echo "    format           ... format haskell code"
	@echo "    lint             ... run hlint"
	@echo "Known tests (sourced from E2E_DIR=${E2E_DIR}):"
	@echo "    ${TEST_NAMES}"
	@echo "Environment variables"
	@echo "    E2E_DIR = ${E2E_DIR} ... changes the source of the tests"
	@echo "Important files"
	@echo "    ${E2E_DIR}/<test_name>.fun ... test source code"
	@echo "    ${E2E_DIR}/<test_name>.ok  ... expected output"
	@echo "    <test_name>.out    ... actual output"
	@echo "    <test_name>.err    ... stderr from running"
	@echo "    <test_name>.debug  ... debug output"
	@echo "    <test_name>.diff   ... difference (empty means identical)"
	@echo "    <test_name>.result ... pass/fail/timeout"
	@echo "    <test_name>.time   ... runtime"

