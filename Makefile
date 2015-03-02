HOME_DIR := $(HOME)/.emacs.d

help:
	@echo 'Makefile for @jonathanchu/dotemacs                                     '
	@echo '                                                                       '
	@echo 'Usage:                                                                 '
	@echo '   make setup        setup and install this emacs config'
	@echo '                                                                       '

setup:
	ln -s $(shell pwd) $(HOME_DIR)
