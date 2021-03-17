/* Set StrKode for kundeordrelinje
   Parametere:  ROWID(KOrdreLinje)|<storl>
   Kommentar: Dersom artikkel har bare en størrelse så velges denne automatisk 
   
   Opprettet: 31.10.05 av BHa                  
   Endret:    
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cStrKode AS CHAR NO-UNDO.
DEF VAR cStorl   AS CHAR NO-UNDO.

cStorl = ENTRY(2,icParam,"|").

FIND KOrdreLinje EXCLUSIVE-LOCK
     WHERE ROWID(KOrdreLinje) = TO-ROWID(ENTRY(1,icParam,"|"))
     NO-ERROR.

IF AVAIL KOrdreLinje THEN DO:
  FIND FIRST ArtBas NO-LOCK
       WHERE ArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr)
       NO-ERROR.
  IF AVAIL ArtBas THEN DO:
    FIND FIRST StrType OF ArtBas NO-LOCK NO-ERROR.
    IF AVAIL StrType THEN DO:
      IF NUM-ENTRIES(StrType.Fordeling) > 1 THEN DO:
        IF CAN-DO(REPLACE(StrType.AlfaFordeling," ",""),cStorl) THEN DO:
          KOrdreLinje.Storl   = cStorl.
          FIND FIRST Strekkode NO-LOCK
               WHERE Strekkode.ArtikkelNr = ArtBas.ArtikkelNr
                 AND Strekkode.StrKode    = INT(ENTRY(LOOKUP(cStorl,REPLACE(StrType.AlfaFordeling," ","")),StrType.Fordeling))
               NO-ERROR.
          IF AVAIL Strekkode THEN
            KOrdreLinje.StrKode = Strekkode.StrKode.
        END.
        ELSE ocReturn = "Ugyldig størrelse for artikkel".
      END.
      ELSE 
        ASSIGN KOrdreLinje.Storl   = TRIM(StrType.AlfaFordeling)
               KOrdreLinje.StrKode = INT(StrType.Fordeling).
    END.
  END.
END.

IF ocReturn = "" THEN obOK = TRUE.

