#!/bin/sh
#
# Makefile for Leroi_c

FC	= gfortran
OS	= $(shell uname -s)
BUILD 	= RELEASE
# BUILD	= PARANOID
# BUILD 	= DEBUG
EXE	= Leroi.x
RUN	= Leroi
INSTALL_DIR = $(HOME)/bin  

ifeq ($(BUILD), RELEASE)
	FC_OPTS = -Q -fwhole-program -O3 -fdefault-real-16 
else ifeq ($(BUILD), PARANOID)
	FC_OPTS = -fcheck=all, -ffpe-trap=invalid,zero,overflow -g -Wall -Wextra -Werror -pedantic
else ifeq ($(BUILD), DEBUG)
	FC_OPTS = -g -zero -fcheck=all
endif

%.o: %.f90
	$(FC) $(FC_OPTS) -c $<

%.x: %.o
	$(FC) $(LN_OPTS) -o $(EXE) $<

all: $(EXE)

clean:
	rm -f *.o *.mod

install:
	chmod +x $(RUN)
	cp $(RUN) $(INSTALL_DIR)
	cp $(EXE) $(INSTALL_DIR)

