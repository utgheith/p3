.PHONY : all test clean

all :
	stack build

test :
	stack test

clean :
	stack clean

