DEF VAR iCl            AS INT NO-UNDO.

{syspara.i 5 1 1 iCl INT}.
FIND Butiker NO-LOCK WHERE
    Butiker.Butik = iCl NO-ERROR.

PROCEDURE kordre_LevFTekst:
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND KOrdreHode NO-LOCK
        WHERE ROWID(KOrdreHode) = irKOrdreHode
        NO-ERROR.
    IF AVAILABLE KORdreHode THEN 
    DO:
        FIND Leveringsform NO-LOCK WHERE 
            Leveringsform.LevFNr = KORdreHode.LevFNr NO-ERROR.
        IF AVAILABLE LeveringsForm THEN 
            ocValue = LeveringsForm.LevFormBeskrivelse.        
        ELSE ocValue = 'Ukjent'.
    END.
    ELSE ocValue = 'Ukjent'.

END PROCEDURE.


