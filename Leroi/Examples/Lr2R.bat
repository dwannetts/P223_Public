@echo off
IF NOT EXIST %1.cfl GOTO QUITD
IF EXIST %2.out COPY %2.out %2_1.out
IF EXIST %2.out DEL %2.out

IF EXIST %2.frq COPY %2.frq %2_1.frq
IF EXIST %2.frq DEL %2.frq

IF EXIST %2.amx COPY %2.amx %2_1.amx
IF EXIST %2.amx DEL %2.amx

IF EXIST Leroi.amx DEL Leroi.amx

COPY %1.cfl Leroi.CFL
COPY %3.frq Leroi.frq
ECHO _________________________________________________________
ECHO START PROCESSING JOB    %2.out = Leroi {%1.cfl} 
ECHO #########################################################
Leroi

DEL Leroi.CFL
REN Leroi.OUT %2.out
IF EXIST Leroi.FRQ DEL Leroi.frq
IF EXIST Leroi.amx REN Leroi.amx %2.amx
ECHO ____________________________________________________
ECHO JOB RESULT %2.out NOW COMPLETE 
ECHO ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
GOTO EXIT

:QUITD
      ECHO ______________________________________________
      ECHO %1.cfl DOESN'T EXIST IN THIS DIRECTORY
      ECHO EXECUTION ABORTED
      ECHO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      GOTO EXIT
:EXIT

