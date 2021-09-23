#!/bin/sh
#
# Makefile for Samaya

FC	= gfortran
OS	= $(shell uname -s)
BUILD 	= RELEASE
# BUILD 	= DEBUG
EXE	= Samaya.x
RUN	= Samaya

ifeq ($(BUILD), RELEASE)
	FC_OPTS = -Q -O3 -march=native -fwhole-program -fdefault-double-8 -fdefault-real-8
else ifeq ($(BUILD), PROFILE)
	FC_OPTS = -O3 -prof-gen -prof-dir=/home/ann019/Code/P223/2013_Release/Leroi/Profiles -profile-functions -profile-loops=all -xHost -traceback
else ifeq ($(BUILD), USEPROFILE)
	FC_OPTS = -O3 -ip -prof-use -xHost -traceback
else ifeq ($(BUILD), DEBUG)
	FC_OPTS = -g -ggdb
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

