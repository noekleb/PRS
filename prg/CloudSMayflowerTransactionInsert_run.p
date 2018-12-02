
/* ---------------------------------------- (c) 2014 CHSO --------------- */
/* ---- Program Name : CloudSMayflowerTransactionInsert_run.p                      ---- */
/* ---- Description  :                                               ---- */
/* ----                                                              ---- */
/* ---- Author       : Curt H. Oldenborg                             ---- */
/* ---- Date Started :             
------------------------------------------------------------------------- */
  
LOG-MANAGER:LOGFILE-NAME    = SESSION:TEMP-DIR + "CloudSMayflowerTransactionInsert_run.log".
LOG-MANAGER:LOG-ENTRY-TYPES = "4gltrace:4".
LOG-MANAGER:LOGGING-LEVEL   = 3.
   
FILE-INFO:FILE-NAME = LOG-MANAGER:LOGFILE-NAME.
IF FILE-INFO:FILE-SIZE GE 10000000 THEN LOG-MANAGER:CLEAR-LOG(). 

IF NOT THIS-PROCEDURE:PERSISTENT THEN
DO: 
    LOG-MANAGER:WRITE-MESSAGE("CloudSReplicatestlinje -- Starting --","BATCH").
    RUN CloudSMayflowerTransactionInsert.p NO-ERROR. 
    LOG-MANAGER:WRITE-MESSAGE("CloudSReplicatestlinje -- End --","BATCH").
END.

LOG-MANAGER:CLOSE-LOG().

QUIT.
