#!/bin/sh
#
# Makefile for MarcoAir

FC	= gfortran
OS	= $(shell uname -s)
BUILD 	= RELEASE
EXE	= MarcoAir.x
RUN	= MarcoAir

ifeq ($(BUILD), RELEASE)
	FC_OPTS = -O3 -march=native -Q -fwhole-program -fdefault-real-16 -ffpe-summary=none
else
	FC_OPTS = -g -traceback
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
	cp $(RUN) ~/bin
	cp $(EXE) ~/bin

