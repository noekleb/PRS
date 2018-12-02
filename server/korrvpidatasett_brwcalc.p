

PROCEDURE chkAktiv:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEF VAR bAktiv AS LOG NO-UNDO. 
  DEF VAR bFound AS LOG NO-UNDO. 

  ASSIGN 
    bAktiv      = LOGICAL(ENTRY(1,icParam,'¤'))
  .
  FIND VPIDatasett WHERE ROWID(VPIDatasett) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIDatasett THEN 
  DO:
    bFound = CAN-FIND(FIRST VPIArtBas OF VPIDatasett).
    ocValue = IF bFound THEN 
               (IF bAktiv THEN "TRUE" ELSE "TRUE") 
              ELSE 
                (IF bAktiv THEN "SKIPROW" ELSE "FALSE").
  END.

END PROCEDURE.

