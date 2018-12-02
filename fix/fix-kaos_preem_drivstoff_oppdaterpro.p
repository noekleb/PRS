DEFINE VARIABLE cc AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE tt_fsdag NO-UNDO
  FIELD butikknr AS INTE
  FIELD dato AS DATE
  INDEX bd IS PRIMARY UNIQUE butikknr dato.

INPUT FROM "c:\tmp\prfs_ejstat.d".

REPEAT:
  IMPORT UNFORMATTED cc.
    cc = TRIM(cc).
    IF cc = "" THEN
        NEXT.
    CREATE tt_fsdag.
    ASSIGN tt_fsdag.butikknr = INT(ENTRY(1,cc,";"))
           tt_fsdag.dato     = DATE(ENTRY(2,cc,";")).
END.
INPUT CLOSE.

FOR EACH tt_fsdag: 
  RUN oppdaterpro.p (butikknr,dato,FALSE,TRUE,"").
  OUTPUT TO "c:\tmp\prfs_ejstat_run.d" APPEND.
  EXPORT tt_fsdag.
  OUTPUT CLOSE.
END.
MESSAGE "SLUT"
  VIEW-AS ALERT-BOX INFO BUTTONS OK.
