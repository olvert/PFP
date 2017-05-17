# Default number of cores if not defined
N ?= 4
FILENAME = stack-exe.eventlog

run:
	./.stack-work/install/x86_64-osx/lts-8.14/8.0.2/bin/stack-exe +RTS -ls -N$(N)

ts:
	/Users/ollesvensson/.local/bin/threadscope $(FILENAME)