DEFINE OUTPUT PARAMETER iStrTypeId AS INTEGER NO-UNDO.
DEFINE VARIABLE iCount   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iFraNr   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTilNr   AS INTEGER    NO-UNDO.
DEFINE VARIABLE bHk      AS LOG        NO-UNDO.
DEFINE VARIABLE cTekst   AS CHARACTER  NO-UNDO.

DEFINE BUFFER bSysPara FOR SysPara.

{syspara.i 50 16 10 iStrTypeId INT}
IF iStrTypeId < 1000 THEN
  iStrTypeId = 1000.  
  
/* I butikk skal vi jobbe med 800000 og oppover. */  
{syspara.i 1 1 18 cTekst}
IF NOT CAN-DO('1,J,Ja,Y,Yes,True',cTekst) THEN
  DO:
      IF iStrTypeId < 800000 THEN 
          iStrTypeId = 800000.
  END. 
  
DO:
  LOOPEN: 
  DO iCount = iStrTypeId TO 999999:
    IF CAN-FIND(StrType WHERE
                StrType.StrTypeId = iCount)
      THEN NEXT.
    ELSE DO:
      iStrTypeId = iCount.
      LEAVE LOOPEN.
    END.
  END. /* LOOPEN */ 
END.

/* Lagrer id i sysparatabellen */
DO FOR bSysPara TRANSACTION:
  FIND bSysPara EXCLUSIVE-LOCK WHERE
    bSysPara.SysHId =  50 AND
    bSysPara.SysGr  =  16 AND
    bSysPara.ParaNr =  10 NO-ERROR.
  IF NOT AVAILABLE bSysPara THEN
    DO:
      CREATE bSysPara.
      ASSIGN
          bSysPara.SysHId =  50 
          bSysPara.SysGr  =  16 
          bSysPara.ParaNr =  10
          bSyspara.Beskrivelse = "Siste StrTypeId"
          .
  END.
  ASSIGN  
      bSysPara.Parameter1  = STRING(iStrTypeId)
      .
  RELEASE bSysPara.
END.
  