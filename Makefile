HOME_DIR := $(HOME)/.emacs.d

help:
	@echo 'Makefile for @jonathanchu/dotemacs                                     '
	@echo '                                                                       '
	@echo 'Usage:                                                                 '
	@echo '   make setup        setup and install this emacs config'
	@echo '   make clean        removes all downloaded packages                   '

setup:
	ln -s $(shell pwd) $(HOME_DIR)

clean:
	rm -rf ./elpa
