

default:  native


native:
	dune build 

native-release: 
	dune build --profile release 

exec: 
	dune exec hw1 
	
exec-release:
	dune exec hw1 --profile release
clean:
	dune clean

test:
	dune test -f

test-release:
	dune test --profile release -f
.PHONY: clean default native native-release exec exec-release test test-release 