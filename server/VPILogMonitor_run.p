/*------------------------------------------------------------------------
    File        : VPILogFileMonitor_run.p 
    Purpose     : 
    Description : Starter vpiLogFileMonitor.p - med gruppe id 
                  peker til systemparameter tabell 102 - og gruppe 
    Author(s)   : Curt H Oldenborg 
    Created     : Mon Apr 06 17:32:28 CEST 2009
  ----------------------------------------------------------------------*/

DEFINE VARIABLE iCount AS INTEGER   NO-UNDO.
DEFINE VARIABLE ipiParamGruppeid AS INTEGER INIT ? NO-UNDO. 
DEFINE VARIABLE cSessionParameter AS CHAR NO-UNDO. 

cSessionParameter = SESSION:PARAMETER.


IF cSessionParameter <> "" THEN 
DO iCount = 1 TO NUM-ENTRIES(cSessionParameter):
    IF ENTRY(iCount,cSessionParameter) BEGINS "SYSGRID" AND 
        NUM-ENTRIES(ENTRY(iCount,cSessionParameter),"=") = 2 THEN 
        ASSIGN 
            ipiParamGruppeId = INT(ENTRY(2,ENTRY(iCount,cSessionParameter),"=")) NO-ERROR.
END.
   
IF ipiParamGruppeId = ? THEN RETURN.
ELSE RUN prg\vpilogfilemonitor.p (INPUT ipiParamGruppeId) NO-ERROR.
QUIT.


