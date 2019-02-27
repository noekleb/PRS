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
DEFINE VARIABLE bLoggIPrisko AS LOG NO-UNDO.

DEFINE VARIABLE bOverfUtPris AS LOG NO-UNDO.

DEFINE BUFFER bufButiker FOR Butiker.
DEFINE BUFFER clButiker FOR Butiker.
DEFINE BUFFER bufVPIArtBas FOR VPIArtBas.
DEFINE BUFFER bufArtPris   FOR ArtPris.

/* Sjekker om etikettflagg skal settes på hk prisprofil. */
{syspara.i 2 4 42 cTekst}
IF CAN-DO('1,J,Ja,Y,YES,True',cTekst) THEN 
  bSettEtikett = TRUE.
ELSE
  bSettEtikett = FALSE. 
  
/* Sjekker om etikettflagg skal settes. */
{syspara.i 2 4 41 cTekst}
IF CAN-DO('1,J,Ja,Y,YES,True',cTekst) THEN 
  bEtiTvang = TRUE.
ELSE
  bEtiTvang = FALSE.
  
/* Sjekker om priskøpost skal kopieres til alle andre prisprofiler. */
{syspara.i 2 4 40 cTekst}
IF CAN-DO('1,J,Ja,Y,YES,True',cTekst) THEN 
  bKopierPrisko = TRUE.
ELSE
  bKopierPrisko = FALSE. 

{syspara.i 2 1 1 lKurs DEC}
{syspara.i 5 1 1 iCl INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE clButiker THEN 
  RETURN.
iClProfilNr = clButiker.ProfilNr.

ASSIGN
    fArtikkelNr    = DECIMAL(ihBuffer:BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE)
    iProfilNr      = INT(ihBuffer:BUFFER-FIELD('ProfilNr'):BUFFER-VALUE)
    /*dAktiveresDato = ihBuffer:BUFFER-FIELD('AktiveresDato'):BUFFER-VALUE.*/
    bOverfUtPris   = CAN-DO('1,j,ja,y,yes,true',ENTRY(4,icParam,'|')).
    
/* For IPS filer skal priskø post på alle nye artikler bli liggende i priskø. */
/* TN 7/10-13 Denne parameter settes feil. Entry 5 sier at all artikkelinfo skal oppdateres.
IF NUM-ENTRIES(icParam,'|') > 4 THEN 
  bLoggIPrisko = CAN-DO('1,j,ja,y,yes,true',ENTRY(5,icParam,'|')).
*/
    
ASSIGN
    dAktiveresDato = DATE(ENTRY(1,icParam,'|')) 
    NO-ERROR.   

IF (dAktiveresDato = ? OR dAktiveresDato < TODAY) 
  THEN dAktiveresDato = TODAY.

/* Er det fra butikkens egen VPI lomme, skal butikkens prisprofil benyttes. */
ASSIGN
  iekstVPILevNr = INTEGER(ihBuffer:BUFFER-FIELD('EkstVPILevNr'):BUFFER-VALUE).
IF iEkstVPILevNr > 1000000 THEN 
DO:
    FIND bufButiker NO-LOCK WHERE
        bufButiker.Butik = iEkstVPILevNr - 1000000 NO-ERROR.
    IF AVAILABLE bufButiker THEN
        iProfilNr = bufButiker.ProfilNr.
END. 
/* Er ikke profilnr satt, skal HK's profilnr gjelde. */
IF iProfilNr = 0 THEN
  iProfilNr = iClProfilNr.

PRISBLOKK:
DO TRANSACTION:
    FIND VPIArtPris NO-LOCK WHERE
      VPIArtPris.EkstVPILevNr = INTEGER(ihBuffer:BUFFER-FIELD('EkstVPILevNr'):BUFFER-VALUE) AND
      VPIArtPris.VareNr       = STRING(fArtikkelNr) AND
      VPIArtPris.ProfilNr     = iProfilNr
      NO-ERROR.
    IF NOT AVAILABLE VPIArtPris THEN 
      FIND VPIArtPris NO-LOCK WHERE
        VPIArtPris.EkstVPILevNr = INTEGER(ihBuffer:BUFFER-FIELD('EkstVPILevNr'):BUFFER-VALUE) AND
        VPIArtPris.VareNr       = STRING(fArtikkelNr) AND
        VPIArtPris.ProfilNr     = iCLProfilNr
        NO-ERROR.
    IF NOT AVAILABLE VPIArtPris THEN LEAVE PRISBLOKK.
    /* Henter lokal artikkel hvis den finnes. */ 
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = fArtikkelNr NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
        LEAVE PRISBLOKK.
    FIND ArtPris OF ArtBas NO-LOCK WHERE
      ArtPris.ProfilNr = VPIArtPris.ProfilNr NO-ERROR.
    /* Kopierer ArtPris til butikkens lokale profil. */
    IF NOT AVAILABLE ArtPris AND VPIArtPris.ProfilNr <> iCLProfilNr THEN
    DO: 
      FIND ArtPris OF ArtBas NO-LOCK WHERE
        ArtPris.ProfilNr = iCLProfilNr NO-ERROR.
      IF AVAILABLE ArtPris AND
        ( /* Kalkylen skal være endret for at det skal opprettes priskøpost. */
          ArtPris.InnkjopsPris[1] <> VPIArtPris.InnkjopsPris[1] OR
          ArtPris.VareKost[1]     <> VPIArtPris.VareKost[1]     OR
          ArtPris.Pris[1]         <> VPIArtPris.Pris[1] 
        ) 
      THEN
      DO: 
        CREATE bufArtPris.
        BUFFER-COPY ArtPris 
          EXCEPT ProfilNr
          TO bufArtPris
          ASSIGN
            bufArtPris.ProfilNr = VPIArtPris.ProfilNr.
        FIND ArtPris OF ArtBas NO-LOCK WHERE
          ArtPris.ProfilNr = VPIArtPris.ProfilNr NO-ERROR.
      END.
    END.
    
    IF NOT AVAILABLE ArtPris THEN 
        LEAVE PRISBLOKK.
   
    /* Kalkylen skal være endret for at det skal opprettes priskøpost. */ 
    IF (ArtPris.InnkjopsPris[1] <> VPIArtPris.InnkjopsPris[1] OR
            ArtPris.VareKost[1]     <> VPIArtPris.VareKost[1]     OR
            ArtPris.Pris[1]         <> VPIArtPris.Pris[1] 
           ) THEN. /* Gjør ingenting */ 
    ELSE LEAVE PRISBLOKK.
    
    FIND VarGr OF ArtBas NO-LOCK NO-ERROR.

    FIND PrisKo EXCLUSIVE-LOCK WHERE
        PrisKo.ArtikkelNr    = ArtBas.ArtikkelNr AND
        PrisKo.ProfilNr      = ArtPris.ProfilNr AND
        PrisKo.AktiveresDato = dAktiveresDato AND
        PrisKo.AktiveresTid  = 0 AND
        PrisKo.Tilbud        = FALSE NO-ERROR.
    IF NOT AVAILABLE PrisKo THEN
    DO:
        CREATE PrisKo.
        ASSIGN
            PrisKo.ArtikkelNr    = ArtBas.ArtikkelNr
            PrisKo.ProfilNr      = ArtPris.ProfilNr 
            PrisKo.AktiveresDato = dAktiveresDato 
            PrisKo.AktiveresTid  = 0 
            PrisKo.Tilbud        = FALSE
            .
    END.

    /* Overfører også utpris. */
    IF bOverfUtPris THEN  DO:
      ASSIGN
        Prisko.ValPris        = VPIArtPris.InnkjopsPris[1]
        Prisko.InnkjopsPris   = VPIArtPris.InnkjopsPris[1]
        Prisko.Rab1%          = VPIArtPris.Rab1%[1]
        Prisko.Rab1Kr         = (Prisko.InnkjopsPris * Prisko.Rab1%) / 100
        Prisko.Pris           = VPIArtPris.Pris[1]
        Prisko.Mva%           = VPIArtPris.Mva%[1]
        Prisko.MvaKr          = Prisko.Pris - (Prisko.Pris / (1 + (Prisko.Mva% / 100)))
        Prisko.VareKost       = VPIArtPris.Varekost[1]
        Prisko.DB%            = VPIArtPris.DB%[1]
        Prisko.DBKr           = Prisko.Pris - Prisko.MvaKr - Prisko.VareKost.
    END.
    ELSE /* Overfører bare innpris. */
      ASSIGN
        Prisko.ValPris        = VPIArtPris.InnkjopsPris[1]
        Prisko.InnkjopsPris   = VPIArtPris.InnkjopsPris[1]
        Prisko.Rab1%          = VPIArtPris.Rab1%[1]
        Prisko.Rab1Kr         = (Prisko.InnkjopsPris * Prisko.Rab1%) / 100
        Prisko.Pris           = ArtPris.Pris[1]
        Prisko.Mva%           = VPIArtPris.Mva%[1]
        Prisko.MvaKr          = Prisko.Pris - (Prisko.Pris / (1 + (Prisko.Mva% / 100)))
        Prisko.VareKost       = Prisko.InnkjopsPris - Prisko.Rab1Kr 
        Prisko.DBKr           = Prisko.Pris - Prisko.MvaKr - Prisko.VareKost
        Prisko.DB%            = (Prisko.DBKr / Prisko.InnkjopsPris) * 100
        Prisko.DB%            = IF Prisko.DB% = ? THEN 0 ELSE Prisko.DB%
        .

    ASSIGN 
        Prisko.EuroPris       = Prisko.Pris * lKurs
        Prisko.LevNr          = ArtBas.LevNr
        Prisko.Rab2Kr         = 0
        Prisko.Rab2%          = 0
        Prisko.Frakt          = 0
        Prisko.Frakt%         = 0
        Prisko.DivKostKr      = 0
        Prisko.DivKost%       = 0
        Prisko.Rab3Kr         = 0
        Prisko.Rab3%          = 0
        Prisko.EuroManuel     = FALSE
        Prisko.Timestyrt      = FALSE
        Prisko.Aktivert       = FALSE
        Prisko.Type           = 1
        Prisko.EndringsType   = 0
        Prisko.KoNummer       = 0
        PrisKo.Opphav         = (IF iClProfilNr = Prisko.ProfilNr THEN 'HK' ELSE 'IPS')
        Prisko.MomsKod        = IF AVAILABLE VarGr
                                  THEN VarGr.MomsKod
                                  ELSE 0
        obOk = TRUE
        .
     FIND bufVPIArtBas EXCLUSIVE-LOCK WHERE
         bufVPIArtBas.EkstVPILevNr = INTEGER(ihBuffer:BUFFER-FIELD('EkstVPILevNr'):BUFFER-VALUE) AND 
         bufVPIArtBas.ArtikkelNr   = DEC(ihBuffer:BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE) NO-ERROR.
     IF AVAILABLE bufVPIArtBas THEN
     DO:
         ASSIGN
             bufVPIArtBas.behStatus = 90.
     END.
    /* Regler for om posten skal bli liggende i priskø eller ikke.            */
    /* Skal den ikke bli liggende i priskø, settes etikettstatus til skrevet. */
    RUN sjekkEtikettstatus.p (Prisko.ArtikkelNr, 
                              iClProfilNr,
                              bSettEtikett,
                              bEtiTvang,
                              Prisko.ProfilNr, 
                              PrisKo.TYPE, 
                              PrisKo.Opphav, 
                              PrisKo.VareKost, 
                              PrisKo.Pris, 
                              INPUT-OUTPUT Prisko.EtikettStatus, 
                              INPUT-OUTPUT Prisko.KlargjorStatus). 
    /* Overstyring for IPS filer */
    /* TN 7/10-13 Denne parameter settes feil. 
    IF bLoggIPrisko THEN 
      ASSIGN
        Prisko.EtikettStatus  = 0 
        Prisko.KlargjorStatus = 0 
        .
    */
    IF AVAILABLE PrisKo THEN 
      FIND CURRENT PrisKo NO-LOCK.
END. /* PRISBLOKK TRANSACTION */

/* Kopierer PrisKo posten til alle andre profiler som skal ha den. */
IF AVAILABLE PrisKo THEN 
PRISKOEN:
DO:
  FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = PrisKo.ArtikkelNr NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN LEAVE PRISKOEN.
  
  IF bKopierPrisko AND PrisKo.ProfilNr = iClProfilNr THEN 
    RUN prisko_kopier.p (ROWID(PrisKo),ROWID(ArtPris)).
END. /* PRISKOEN */

/*OM alt går bra settes ...*/
ASSIGN 
  ocReturn = ''
.
