TRIGGER PROCEDURE FOR DELETE OF StrekKode.

DEF BUFFER trgEANNrListe FOR EANNrListe.
DEFINE BUFFER trgArtBas FOR ArtBas.

/* Logger utlegg for de profiler det gjelder. */
BLOKKEN1:
DO FOR ELogg:
    FOR EACH ArtPris NO-LOCK WHERE ArtPris.ArtikkelNr = Strekkode.ArtikkelNr:
      FIND ELogg EXCLUSIVE-LOCK WHERE 
           ELogg.TabellNavn     = "ArtPris" AND
           ELogg.EksterntSystem = "POS"    AND
           ELogg.Verdier        = STRING(ArtPris.ArtikkelNr) + CHR(1) + string(ArtPris.ProfilNr) + CHR(1) + Strekkode.Kode NO-ERROR NO-WAIT.
      IF LOCKED ELogg THEN LEAVE BLOKKEN1.
      IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "ArtPris"
               ELogg.EksterntSystem = "POS"   
               ELogg.Verdier        = STRING(ArtPris.ArtikkelNr) + CHR(1) + string(ArtPris.ProfilNr) + CHR(1) + Strekkode.Kode NO-ERROR.
      END.
      ASSIGN ELogg.EndringsType = 3
             ELogg.Behandlet    = FALSE NO-ERROR.
    END.     
    IF AVAILABLE ELogg THEN RELEASE ELogg.
END. /* BLOKKEN1 */

/* ERP */
BLOKKEN2:
DO FOR ELogg:
    FIND ELogg EXCLUSIVE-LOCK WHERE 
         ELogg.TabellNavn     = "ArtBas" AND
         ELogg.EksterntSystem = "ERP"    AND
         ELogg.Verdier        = STRING(Strekkode.ArtikkelNr)  + CHR(1) + Strekkode.Kode NO-ERROR NO-WAIT.
    IF LOCKED ELogg THEN LEAVE BLOKKEN2.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "ArtBas"
               ELogg.EksterntSystem = "ERP"   
               ELogg.Verdier        = STRING(Strekkode.ArtikkelNr)  + CHR(1) + Strekkode.Kode NO-ERROR.
    END.
    ASSIGN ELogg.EndringsType = 3
           ELogg.Behandlet    = FALSE NO-ERROR.
    IF AVAILABLE ELogg THEN 
      RELEASE ELogg.
END. /* BLOKKEN2 */

BLOKKEN3:
DO FOR trgArtBas:
    FIND trgArtBas NO-LOCK WHERE
      trgArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.

    /* Utlegg til ferskvarevekt */
    IF AVAILABLE trgArtBas AND 
       CAN-FIND (FIRST SysPara WHERE SysPara.SysHId = 23 AND SysPara.SysGr = 1 AND SysPara.Parameter1 > '0') THEN 
    DO FOR ELogg:
      IF trgArtBas.ArtSlag = 1 AND LENGTH(Strekkode.Kode) = 13 THEN 
      DO:
        FIND ELogg EXCLUSIVE-LOCK WHERE 
             ELogg.TabellNavn     = "ArtBas" AND
             ELogg.EksterntSystem = "FVEKT"    AND
             ELogg.Verdier        = STRING(trgArtBas.ArtikkelNr) + CHR(1) + STRING(Strekkode.Kode) NO-ERROR NO-WAIT.
        IF LOCKED ELogg THEN LEAVE BLOKKEN3.
        IF NOT AVAIL Elogg THEN DO:
            CREATE Elogg.
            ASSIGN ELogg.TabellNavn     = "ArtBas"
                   ELogg.EksterntSystem = "FVEKT"   
                   ELogg.Verdier        = STRING(trgArtBas.ArtikkelNr) + CHR(1) + STRING(Strekkode.Kode) NO-ERROR.
        END.
        ASSIGN ELogg.EndringsType = 3 
               ELogg.Behandlet    = FALSE NO-ERROR.
        RELEASE ELogg.
      END.
      IF AVAILABLE ELogg THEN 
        RELEASE ELogg.
    END.

    IF AVAILABLE trgArtBas THEN 
      RELEASE trgArtBas.
END. /* BLOKKEN3 */

