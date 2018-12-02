/* 
   Parameter:  
   Opprettet: 08.01.2008 Geir Otto
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fArtikkelNr    AS DEC  NO-UNDO.
DEF VAR iProfilNr      AS INT  NO-UNDO.
DEF VAR iClProfilNr    AS INT  NO-UNDO.
DEF VAR iCL            AS INT  NO-UNDO.
DEF VAR dAktiveresDato AS DATE NO-UNDO.
DEF VAR lKurs          AS DEC  NO-UNDO.
DEFINE VARIABLE bEtiTvang     AS LOG       NO-UNDO.
DEFINE VARIABLE bSettEtikett  AS LOG       NO-UNDO.
DEFINE VARIABLE cTekst        AS CHARACTER NO-UNDO.
DEFINE VARIABLE bKopierPrisko AS LOG       NO-UNDO.
DEFINE VARIABLE iEkstVPILevNr AS INTEGER  NO-UNDO.

DEFINE VARIABLE bOverfUtPris AS LOG NO-UNDO.

DEFINE BUFFER bufButiker FOR Butiker.
DEFINE BUFFER clButiker FOR Butiker.
DEFINE BUFFER bufVPIArtBas FOR VPIArtBas.
DEFINE BUFFER bufArtPris   FOR ArtPris.

ASSIGN
    fArtikkelNr    = DECIMAL(ihBuffer:BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE)
    iProfilNr      = INT(ihBuffer:BUFFER-FIELD('ProfilNr'):BUFFER-VALUE)
    .
        
/* Er ikke profilnr satt, skal HK's profilnr gjelde. */
IF iProfilNr = 0 THEN
DO:
  ASSIGN ocReturn = '** ProfilNr ikke angitt. Eloggpost ikke opprettet.'.
  RETURN.
END.

ELOGGBLOKK:
DO TRANSACTION:
    FIND VPIArtPris NO-LOCK WHERE
      VPIArtPris.EkstVPILevNr = INTEGER(ihBuffer:BUFFER-FIELD('EkstVPILevNr'):BUFFER-VALUE) AND
      VPIArtPris.VareNr       = STRING(fArtikkelNr) AND
      VPIArtPris.ProfilNr     = iProfilNr
      NO-ERROR.
    IF NOT AVAILABLE VPIArtPris THEN
      LEAVE ELOGGBLOKK.
    ELSE DO:
      FIND ELogg EXCLUSIVE-LOCK WHERE 
         ELogg.TabellNavn     = "ArtPris" AND
         ELogg.EksterntSystem = "POS"    AND
         ELogg.Verdier        = STRING(VPIArtPris.ArtikkelNr) + CHR(1) + STRING(VPIArtPris.ProfilNr) NO-ERROR NO-WAIT.
      IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "ArtPris"
               ELogg.EksterntSystem = "POS"   
               ELogg.Verdier        = STRING(VPIArtPris.ArtikkelNr) + CHR(1) + STRING(VPIArtPris.ProfilNr) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
        DO:
          DELETE ELogg.
        END.
      END.
      IF AVAILABLE ELogg AND (ELogg.EndringsType <> 2 OR ELogg.Behandlet = TRUE) THEN 
      DO:
        ASSIGN ELogg.EndringsType = 1
               ELogg.Behandlet    = FALSE NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
      END.  
      IF AVAILABLE ELogg THEN 
        RELEASE ELogg.
    END.   
END. /* PRISBLOKK TRANSACTION */

/*OM alt går bra settes ...*/
ASSIGN 
  ocReturn = ''.
