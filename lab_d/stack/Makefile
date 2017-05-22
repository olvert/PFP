# Default number of cores if not defined
N ?= 4
FILENAME = stack-exe.eventlog

run:
	nice -n 20 .stack-work/install/x86_64-osx/lts-8.14/8.0.2/bin/stack-exe +RTS -ls -lf -N$(N) -A1G -H2G
	
out:
	nice -n 20 ./.stack-work/install/x86_64-osx/lts-8.14/8.0.2/bin/stack-exe --output out.html +RTS -ls -lf -N$(N) -A1G -H2G


ts:
	/Users/ollesvensson/.local/bin/threadscope $(FILENAME)