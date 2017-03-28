# VARS

# Default number of cores if not defined
N ?= 4
FILENAME = ass1.eventlog

# RULES
all: run ts

setup:
	cabal configure
	cabal install --dependencies-only

build:
	cabal build

ts:
	./.cabal-sandbox/bin/threadscope $(FILENAME)

run: Main.hs
	./dist/build/ass1/ass1 +RTS -N$(N) -ls -lf -A100M -H2G

Main.hs: build
