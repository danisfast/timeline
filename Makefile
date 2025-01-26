SHELL = /bin/bash

watch:
	dune build example/main.exe -w

run:
	./_build/default/example/main.exe
