DEF VAR iPalleNr AS INT NO-UNDO.
DEF VAR iDummy AS INT NO-UNDO.

FOR EACH PkSdlHode NO-LOCK WHERE 
    PkSdlHode.cPalleNr > '':
    ASSIGN iDummy = INT(PkSdlHode.cPalleNr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT.
    IF iPalleNr < INT(PkSdlHode.cPalleNr) THEN
        iPalleNr = INT(PkSdlHode.cPalleNr).
END.
DO FOR Syspara TRANSACTION:
  FIND SysPara EXCLUSIVE-LOCK WHERE
      SysPara.SysHId = 22 AND
      SysPara.SysGr  = 5 AND
      SysPara.ParaNr = 4 NO-ERROR.
  IF NOT AVAILABLE sysPara THEN
  DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 22 
          SysPara.SysGr  = 5 
          SysPara.ParaNr = 4.
      ASSIGN  
          SysPara.Parameter1   = "0"
          SysPara.Parameter2   = ""
          Syspara.Beskrivelse  = "Sist benyttede palleNr"
          SysPara.Hjelpetekst1 = "Siste brukte pallenr. Brukt ved automatisk tildeling av pallenr."
          SysPara.Hjelpetekst2 = ""
          .
  END.
  ASSIGN 
      SysPara.Parameter1 = STRING(iPalleNr)
      .
  IF AVAILABLE SysPara THEN 
    RELEASE SysPara.
END. /* TRANSACTION */

