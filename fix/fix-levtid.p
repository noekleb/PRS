
DEF VAR wListe AS CHAR NO-UNDO.
DEF VAR wDatoListe AS CHAR NO-UNDO.
DEF VAR wLoop AS INT NO-UNDO.
DEF VAR wDato AS DATE NO-UNDO.
DEF VAR wYear AS INT NO-UNDO.

FOR EACH BestHode WHERE 
    BestHode.BestNr = 9463 AND
    Bestillingsdato >= 01/01/2000:
  
   IF (
       int(BestHode.LevTid) >= 1 and
       int(BestHode.LevTid) <= 52
      )
    THEN DO:

       wDato = (DATE(01,01,2000) - 1) +
           (int(BestHode.LevTid) * 7).
       IF wDato < BestHode.Bestillingsdato THEN
           wDato = (DATE(01,01,2001) - 2) +
                      (int(BestHode.LevTid) * 7).

       BestHode.LevDato = wDato.
   END.

   MESSAGE besthode.bestnr besthode.bestillingsdato 
       besthode.levtid besthode.levdato
       VIEW-AS ALERT-BOX.

END.


