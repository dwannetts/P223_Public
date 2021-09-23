@echo off
IF NOT EXIST %1.cfl GOTO QUITC
IF NOT EXIST %3.inv GOTO QUITI
IF EXIST %2.out COPY %2.OUT %2_1.out
IF EXIST %2.out DEL %2.out

IF EXIST %2.mv1 COPY %2.OUT %2_1.mv1
IF EXIST %2.mv1 DEL %2.mv1

COPY %1.cfl Airbeo.cfl
COPY %3.inv Airbeo.inv
ECHO _________________________________________________________
ECHO START PROCESSING JOB    %2.OUT = Airbeo {%1.cfl, %3.inv} 
ECHO #########################################################

Airbeo

DEL Airbeo.cfl
DEL Airbeo.inv
IF EXIST Airbeo.out REN Airbeo.out %2.out
IF EXIST Airbeo.mv1 REN Airbeo.mv1 %2.mv1

ECHO ____________________________________________________
ECHO JOB RESULT %2.out NOW COMPLETE 
ECHO ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
GOTO EXIT

:QUITC
      ECHO ______________________________________________
      ECHO %1.cfl DOESN'T EXIST IN THIS DIRECTORY
      ECHO EXECUTION ABORTED
      ECHO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      GOTO EXIT

:QUITI
      ECHO ______________________________________________
      ECHO %3.inv DOESN'T EXIST IN THIS DIRECTORY
      ECHO EXECUTION ABORTED
      ECHO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      GOTO EXIT

:QUITX
      ECHO ______________________________________________
      ECHO %3.exe DOESN'T EXIST IN THIS DIRECTORY
      ECHO EXECUTION ABORTED
      ECHO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      GOTO EXIT
:EXIT

