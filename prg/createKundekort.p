/*
  Denne rutinen spiller sammen med rutinen genkundenr.p i trg katalogen.
  Den krever et forslag til kortnummer, som den så arbeider ut fra.
*/


DEF INPUT  PARAMETER trgKundeNr AS DEC NO-UNDO.
DEF INPUT  PARAMETER trgKortNr  LIKE KundeKort.KortNr NO-UNDO.
DEF INPUT  PARAMETER iGyldighet AS INTE NO-UNDO.
DEF OUTPUT PARAMETER wRecid     AS RECID.

DEFINE VARIABLE iSisteNr AS INTEGER NO-UNDO.

DEFINE BUFFER bSysPara FOR SysPara.

{syspara.i 14 2 5 iSisteNr INT}

IF trgKortNr = "" THEN
    ASSIGN trgKortNr = SUBSTR(STRING(trgKundeNr),LENGTH(STRING(trgKundeNr)) - 5).
FIND Kunde WHERE Kunde.kundenr = trgKundeNr NO-LOCK NO-ERROR.
DO TRANSACTION:
  CREATE KundeKort.
  REPEAT:
      ASSIGN KundeKort.KundeNr      = trgKundeNr
             KundeKort.KortNr       = trgKortNr
             KundeKort.Innehaver    = IF AVAIL Kunde THEN Kunde.Navn ELSE ""
             KundeKort.AktivertDato = TODAY
             KundeKort.UtgarDato    = TODAY + iGyldighet NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
          IF trgKortNr = SUBSTR(STRING(trgKundeNr),LENGTH(STRING(trgKundeNr)) - 5) THEN
              ASSIGN trgKortNr = STRING(DECI(trgKortNr) + 5001).
          ELSE
              ASSIGN trgKortNr = STRING(DECI(trgKortNr) + 1).
      END.

      ELSE DO:
          wRecid = recid(KundeKort).
          LEAVE.
      END.
  END.

  IF  INTEGER(trgKortNr) > iSisteNr THEN 
  DO:
    FIND bSysPara EXCLUSIVE-LOCK WHERE
        bSysPara.SysHId =  14 AND
        bSysPara.SysGr  =  2 AND
        bSysPara.ParaNr =  5 NO-ERROR.
    IF NOT AVAILABLE bSysPara THEN
        DO:
          CREATE bSysPara.
          ASSIGN
              bSysPara.SysHId =  14 
              bSysPara.SysGr  =  2 
              bSysPara.ParaNr =  5 
              bSyspara.Beskrivelse = "Siste brukte kundekortnr"
              bSysPara.Parameter1  = "0"
              .
    END.
    ASSIGN  
          bSysPara.Parameter1  = STRING(trgKortNr)
          .
    RELEASE bSysPara.
  END.

END. /* TRANSACTION */
