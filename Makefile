export GERBIL_LOADPATH := $(HOME)/.gerbil/lib

LOCAL_LIB   := .gerbil/lib/sinatra
INSTALL_DIR := $(HOME)/.gerbil/lib/sinatra

.PHONY: build test clean install uninstall

build:
	gerbil build

test: build
	gerbil test ./...

clean:
	gerbil clean

install: build
	@echo "Installing sinatra to $(INSTALL_DIR) ..."
	rsync -a --exclude '*-test*' --exclude 'example*' $(LOCAL_LIB)/ $(INSTALL_DIR)/
	@echo "Installed. Import with: (import :sinatra/sinatra)"

uninstall:
	@echo "Removing $(INSTALL_DIR) ..."
	rm -rf $(INSTALL_DIR)
	@echo "Uninstalled."
