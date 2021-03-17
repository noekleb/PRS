TRIGGER PROCEDURE FOR WRITE OF MedlemsKort OLD BUFFER oldMedlemskort.

{trg\c_w_trg.i &Fil=SkoTex.Medlemskort &Type="W"}

IF (oldMedlemsKort.InterntKKortId = 0 AND MedlemsKort.Sperret = TRUE) OR (oldMedlemsKort.Sperret = TRUE AND MedlemsKort.Sperret = TRUE) THEN
DO:
  IF MedlemsKort.InterntKKortId > 0 THEN
      FIND KundeKort NO-LOCK WHERE
           KundeKort.InterntKKortId = Medlemskort.InterntKKortId NO-ERROR.
  IF AVAILABLE KundeKort THEN
  DO:

      FIND ELogg WHERE 
           ELogg.TabellNavn     = "Kunde" AND
           ELogg.EksterntSystem = "POS"    AND
           ELogg.Verdier        = STRING(KundeKort.KundeNr) + ";" + STRING(KundeKort.KortNr) NO-ERROR.
      IF NOT AVAIL Elogg THEN DO:
          CREATE Elogg.
          ASSIGN ELogg.TabellNavn     = "Kunde"
                 ELogg.EksterntSystem = "POS"   
                 ELogg.Verdier        = STRING(KundeKort.KundeNr) + ";" + STRING(KundeKort.KortNr).
      END.
      ASSIGN ELogg.EndringsType = 3
             ELogg.Behandlet    = FALSE.
  END.
END.

ELSE DO:
    IF MedlemsKort.InterntKKortId > 0 THEN
        FIND KundeKort NO-LOCK WHERE
             KundeKort.InterntKKortId = Medlemskort.InterntKKortId NO-ERROR.
    IF AVAILABLE KundeKort THEN
    DO:
        FIND ELogg WHERE 
             ELogg.TabellNavn     = "Kunde" AND
             ELogg.EksterntSystem = "POS"    AND
             ELogg.Verdier        = STRING(KundeKort.KundeNr) + ";" + STRING(KundeKort.KortNr) NO-ERROR.
        IF NOT AVAIL Elogg THEN DO:
            CREATE Elogg.
            ASSIGN ELogg.TabellNavn     = "Kunde"
                   ELogg.EksterntSystem = "POS"   
                   ELogg.Verdier        = STRING(KundeKort.KundeNr) + ";" + STRING(KundeKort.KortNr).
        END.
        ASSIGN ELogg.EndringsType = 1
               ELogg.Behandlet    = FALSE.
    END.
END.

/* Logger for sending av fil til Webside for initiering */
MEDLEM_TIL_WEB:
DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "Medlem" AND
         ELogg.EksterntSystem = "WEBINIT"    AND
         ELogg.Verdier        = STRING(MedlemsKort.MedlemsNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Medlem"
               ELogg.EksterntSystem = "WEBINIT"   
               ELogg.Verdier        = STRING(MedlemsKort.MedlemsNr).
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
END. /* MEDLEM_TIL_WEB */


IF AVAILABLE Elogg THEN
    RELEASE ELogg.


