DEF INPUT  PARAM icBuffer    AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid     AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.

DEF VAR cTekst AS CHAR NO-UNDO.
DEF VAR iX AS INT NO-UNDO.

FIND KOrdreHode WHERE ROWID(KOrdreHode) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.
IF AVAIL KOrdreHode THEN 
DO ON ERROR UNDO, LEAVE TRANSACTION:
  IF (INTEGER(KOrdreHode.levstatus) <= 30 OR 
      INTEGER(KOrdreHode.levstatus) > 50) THEN 
  DO:
    FOR EACH KOrdreLinje EXCLUSIVE-LOCK OF KOrdreHode:
      DELETE KOrdreLinje.
    END.
    DELETE KOrdreHode.      

  END.
  ELSE ocReturn = "Kundeordre er bekreftet og kan derfor ikke slettes.".
END.
IF ERROR-STATUS:ERROR THEN 
DO:
    DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
        cTekst = cTekst + 
                 (IF cTekst <> '' THEN CHR(10) ELSE '') + 
                 ERROR-STATUS:GET-MESSAGE(ix).      
    END.
    ocReturn = "Feil oppsto ved sletting av kundeordre" + CHR(10) + PROGRAM-NAME(1) + CHR(10) + cTekst.
END.
