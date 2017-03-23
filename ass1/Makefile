# VARS

N ?= 2 # Default number of cores if not defined
FILENAME = ass1.eventlog

# RULES
all: run ts

setup:
	cabal configure
	cabal install --dependencies-only
	cabal build

ts:
	./.cabal-sandbox/bin/threadscope $(FILENAME)

run:
	./dist/build/ass1/ass1 +RTS -N$(N) -ls
