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
DEF VAR iCL            AS INT  NO-UNDO.
DEF VAR dAktiveresDato AS DATE NO-UNDO.
DEF VAR lKurs          AS DEC  NO-UNDO.

DEFINE VARIABLE bEtiTvang     AS LOG       NO-UNDO.
DEFINE VARIABLE bSettEtikett  AS LOG       NO-UNDO.
DEFINE VARIABLE cTekst        AS CHARACTER NO-UNDO.
DEFINE VARIABLE bKopierPrisko AS LOG       NO-UNDO.
DEFINE VARIABLE iClProfilNr   AS INT       NO-UNDO.

DEFINE BUFFER bufButiker FOR Butiker.
DEFINE BUFFER bufVPIMottak FOR VPIMottak.
DEFINE BUFFER clButiker FOR Butiker.

{syspara.i 2 1 1 lKurs DEC}

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
    fArtikkelNr     = ihBuffer:BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE.
    iProfilNr       = ihBuffer:BUFFER-FIELD('ProfilNr'):BUFFER-VALUE.
    dAktiveresDato  = ihBuffer:BUFFER-FIELD('AktiveresDato'):BUFFER-VALUE.
    .
IF dAktiveresDato = ? OR
    dAktiveresDato < TODAY THEN
    dAktiveresDato = TODAY.

IF iProfilNr = 0 THEN
DO:
    {syspara.i 5 1 1 iCL INT}
    FIND bufButiker NO-LOCK WHERE
        bufButiker.Butik = iCL NO-ERROR.
    IF AVAILABLE bufButiker THEN
        iProfilNr = bufButiker.ProfilNr.
END.

PRISBLOKK:
DO TRANSACTION:
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = fArtikkelNr NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
        LEAVE PRISBLOKK.
    FIND VarGr OF ArtBas NO-LOCK NO-ERROR.

    FIND PrisKo EXCLUSIVE-LOCK WHERE
        PrisKo.ArtikkelNr    = ArtBas.ArtikkelNr AND
        PrisKo.ProfilNr      = iProfilNr AND
        PrisKo.AktiveresDato = dAktiveresDato AND
        PrisKo.AktiveresTid  = 0 AND
        PrisKo.Tilbud        = FALSE NO-ERROR.
    IF NOT AVAILABLE PrisKo THEN
    DO:
        CREATE PrisKo.
        ASSIGN
            PrisKo.ArtikkelNr    = ArtBas.ArtikkelNr
            PrisKo.ProfilNr      = iProfilNr 
            PrisKo.AktiveresDato = dAktiveresDato 
            PrisKo.AktiveresTid  = 0 
            PrisKo.Tilbud        = FALSE
            .
    END.

    ASSIGN
        Prisko.Pris           = ihBuffer:BUFFER-FIELD('Pris'):BUFFER-VALUE
        Prisko.Mva%           = ihBuffer:BUFFER-FIELD('Mva%'):BUFFER-VALUE
        Prisko.MvaKr          = Prisko.Pris - (Prisko.Pris / (1 + (Prisko.Mva% / 100)))
        Prisko.VareKost       = ihBuffer:BUFFER-FIELD('Varekost'):BUFFER-VALUE
        Prisko.DB%            = ihBuffer:BUFFER-FIELD('DB%'):BUFFER-VALUE
        Prisko.DBKr           = Prisko.Pris - Prisko.MvaKr - Prisko.VareKost
        Prisko.ValPris        = ihBuffer:BUFFER-FIELD('InnkjopsPris'):BUFFER-VALUE
        Prisko.InnkjopsPris   = ihBuffer:BUFFER-FIELD('InnkjopsPris'):BUFFER-VALUE
        Prisko.Rab1%          = ihBuffer:BUFFER-FIELD('Rab1%'):BUFFER-VALUE
        Prisko.Rab1Kr         = (Prisko.InnkjopsPris * Prisko.Rab1%) / 100
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
        Prisko.MomsKod        = IF AVAILABLE VarGr
                                  THEN VarGr.MomsKod
                                  ELSE 0
        obOk = TRUE
        .
     FIND bufVPIMottak EXCLUSIVE-LOCK WHERE
         ROWID(bufVPIMottak) = ihBuffer:ROWID NO-ERROR.
     IF AVAILABLE VPIMottak THEN
     DO:
         ASSIGN
             bufVPIMottak.behStatus = 90.
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
