HS_FILES = ${shell find . -path './.*' -prune -o \( -name '*.hs' -print \) }

.PHONY : all test clean format

all :
	stack build

test :
	(stack test 2> test.stderr) || (echo "tests failed more data in test.stderr"; false)

format :
	stack exec -- which ormolu > /dev/null 2>&1 || stack install ormolu ; stack exec -- ormolu -i ${HS_FILES}

lint :
	stack exec -- which hlint > /dev/null 2>&1 || stack install hlint ; stack exec -- hlint ${HS_FILES}

clean :
	stack clean

