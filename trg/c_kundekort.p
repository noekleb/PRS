TRIGGER PROCEDURE FOR CREATE OF KundeKort.

def var trgInterntKKortId as DEC no-undo.

{trg\c_w_trg.i &Fil=SkoTex.KundeKort &Type="C"}

LOOPEN:
DO WHILE TRUE:
    RUN trg/genkkortnr.p (OUTPUT trgInterntKKortId).
    assign
      KundeKort.InterntKKortId = trgInterntKKortId
      NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
        LEAVE LOOPEN.
END.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "KundeKort" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(KundeKort.KundeNr) + ";" + 
                            KundeKort.KortNr NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "KundeKort"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(KundeKort.KundeNr) + ";" + 
                                  KundeKort.KortNr.
END.
/* Vi måste behandla en ändring från kreditkund till ikke som en delete */
/* I normala fall hanteras en delete genom att vi tar bort kundekort    */
ASSIGN ELogg.EndringsType = 1 /* IF Kunde.BetType = 2 THEN 1 ELSE 3 */
       ELogg.Behandlet    = FALSE.


