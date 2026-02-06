export GERBIL_LOADPATH := $(HOME)/.gerbil/lib

.PHONY: build test clean install uninstall

build:
	gerbil build

test: build
	gerbil test ./...

clean:
	gerbil clean

install: build
	cd /tmp && gerbil pkg link -g sinatra $(CURDIR)
	cd /tmp && gerbil pkg build -g sinatra

uninstall:
	cd /tmp && gerbil pkg unlink -g sinatra
