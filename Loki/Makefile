#!/bin/sh
#
# Makefile for Loki

FC	= gfortran
OS	= $(shell uname -s)
BUILD 	= RELEASE
# BUILD	= PARANOID
# BUILD 	= DEBUG
EXE	= Loki.x
RUN	= Loki

ifeq ($(BUILD), RELEASE)
	FC_OPTS = -O3 -march=native -Q -fwhole-program  -ffpe-summary=none
else ifeq ($(BUILD), PARANOID)
	FC_OPTS = -fcheck=all, -ffpe-trap=invalid,zero,overflow -g -Wall -Wextra -Werror -pedantic
else ifeq ($(BUILD), DEBUG)
	FC_OPTS = -fcheck=all, -g
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

