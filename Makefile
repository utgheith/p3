HS_FILES = ${shell find . -path './.*' -prune -o \( -name '*.hs' -print \) }

.PHONY : all test clean format

all :
	stack build

test :
	stack test

format :
	stack install ormolu && stack exec -- ormolu -i ${HS_FILES}

lint :
	stack install hlint && stack exec -- hlint ${HS_FILES}

clean :
	stack clean

