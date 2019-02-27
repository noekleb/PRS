TRIGGER PROCEDURE FOR DELETE OF ArtPris.


DEFINE VARIABLE bUndertrykk AS LOG NO-UNDO.
DEFINE VARIABLE iCl         AS INTEGER NO-UNDO.
DEFINE VARIABLE ctrgTekst   AS CHARACTER NO-UNDO.

DEFINE BUFFER trgButiker FOR Butiker.
DEFINE BUFFER trgArtBas  FOR ArtBas.
DEFINE BUFFER trgELogg   FOR ELogg.
DEFINE BUFFER trgEkstEDBSystem FOR EkstEDBSystem.

{syspara.i 5 1 1 iCL INT}
IF iCl > 0 THEN 
  FIND trgButiker NO-LOCK WHERE trgButiker.Butik = iCl NO-ERROR.
  
/* Skal utlegg til kasse av artikkelendringer gjørs? */
{syspara.i 2 4 43 ctrgTekst}
IF CAN-DO('1,j,ja,y,yes,true',ctrgTekst) 
    THEN bUndertrykk = TRUE.
ELSE bUndertrykk = FALSE. 



/* Logging mot kasse gjøres pr. profil. */
IF AVAILABLE trgButiker
   AND trgButiker.ProfilNr <> ArtPris.ProfilNr 
   AND CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = Artpris.ArtikkelNr 
   AND ArtBas.iKasse     = TRUE) THEN
ELOGG_ARTPRIS:
DO FOR ELogg:
    FIND ELogg EXCLUSIVE-LOCK WHERE 
        ELogg.TabellNavn     = "ArtPris" AND
        ELogg.EksterntSystem = "POS"    AND
        ELogg.Verdier        = STRING(ArtPris.ArtikkelNr) + CHR(1) + string(ArtPris.ProfilNr) NO-ERROR NO-WAIT.
  
    IF LOCKED ELogg THEN 
        LEAVE ELOGG_ARTPRIS.
  
    IF NOT AVAIL Elogg THEN 
    DO:
        CREATE Elogg.
        ASSIGN 
            ELogg.TabellNavn     = "ArtPris"
            ELogg.EksterntSystem = "POS"   
            ELogg.Verdier        = STRING(ArtPris.ArtikkelNr) + CHR(1) + string(ArtPris.ProfilNr) 
            ELogg.EndringsType   = 1
            ELogg.Behandlet      = FALSE           
           NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
        DO:
            DELETE ELogg.
            LEAVE ELOGG_ARTPRIS.
        END.
    END.
    IF AVAILABLE ELogg THEN RELEASE ELogg.
END. /* ELOGG_ARTPRIS */

/* Utlegg til nettbutikk */
FIND FIRST trgEkstEDBSystem WHERE 
    trgEkstEDBSystem.DataType = "WEBBUT" AND 
    trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
FIND trgArtBas OF ArtPris NO-LOCK NO-ERROR.
IF AVAILABLE trgEkstEDBSystem AND AVAILABLE trgArtBas THEN
WEBBUTIKK:
DO FOR trgELogg:
    /* Endring som skal til Web */
    IF trgArtBas.WebButikkArtikkel THEN 
    WEBBUT:
    DO:
        FIND trgELogg WHERE 
            trgELogg.TabellNavn     = "ArtBas" AND
            trgELogg.EksterntSystem = "WEBBUT"    AND
            trgELogg.Verdier        = STRING(trgArtBas.ArtikkelNr) NO-ERROR NO-WAIT.
        IF NOT LOCKED trgELogg THEN 
        DO:
            IF NOT AVAIL trgELogg THEN 
            DO:
                CREATE trgELogg.
                ASSIGN 
                    trgELogg.TabellNavn     = "ArtBas"
                    trgELogg.EksterntSystem = "WEBBUT"   
                    trgELogg.Verdier        = STRING(trgArtBas.ArtikkelNr).
            END.
            ASSIGN 
                trgELogg.EndringsType = 1 
                trgELogg.Behandlet    = FALSE.
            RELEASE trgELogg.
        END.
    END. /* WEBBUT */
    IF AVAILABLE trgELogg THEN RELEASE trgELogg.
END. /* WEBBUTIKK */

