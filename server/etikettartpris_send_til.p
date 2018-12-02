/* Overføring av varer til etikettkø fra artpris
   Parameter: Hvis overføring fra ArtBas eller PrisKo:
              <profilnr>
              Ved scanning:
                <profilnr>;<Butikknr>;<Antall>;<AlleStr>¤<art.nr>|<Kode>;<art.nr>|<kode> 
   Opprettet: 04.02.10 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR hKode           AS HANDLE NO-UNDO.
DEF VAR hAntall         AS HANDLE NO-UNDO.
DEF VAR iProfilNr       AS INT    NO-UNDO.
DEF VAR iButikkNr       AS INT    NO-UNDO.
DEF VAR iAntall         AS INT    NO-UNDO.
DEF VAR bAlleStr        AS LOG    NO-UNDO INIT YES.
DEFINE VARIABLE iCL     AS INTEGER NO-UNDO.
DEFINE VARIABLE iCLOpt     AS INTEGER NO-UNDO.

DEF TEMP-TABLE ttArt
    FIELD ArtikkelNr AS DEC
    FIELD Kode       AS CHAR
    FIELD ButikkNr   AS INT 
    FIELD Antall     AS INT
    .

DEFINE VARIABLE cOptProfilbutik     AS CHARACTER   NO-UNDO.

DEFINE BUFFER clButiker FOR Butiker.
DEFINE BUFFER clOptButiker FOR Butiker.
DEF BUFFER bStrekkode FOR StrekKode.

{syspara.i 5 1 1 iCL INT}
{syspar2.i 5 1 1 cOptProfilbutik}
cOptProfilbutik = TRIM(cOptProfilbutik). 
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE clButiker THEN 
  RETURN.

iProfilNr = INT(ENTRY(1,ENTRY(1,icParam,"¤"),";")).

/* Scanning: */
IF NUM-ENTRIES(ENTRY(1,icParam,"¤"),";") > 1 THEN
  ASSIGN iButikkNr = INT(ENTRY(2,ENTRY(1,icParam,"¤"),";"))
         iAntall   = INT(ENTRY(3,ENTRY(1,icParam,"¤"),";"))
         bAlleStr  = LOGICAL(ENTRY(4,ENTRY(1,icParam,"¤"),";"))
         icParam   = ENTRY(2,icParam,"¤")
         .

IF NOT VALID-HANDLE(ihBuffer) THEN DO:
  ihBuffer = BUFFER ttArt:HANDLE.
  ASSIGN hKode   = ihBuffer:BUFFER-FIELD("Kode")
         hAntall = ihBuffer:BUFFER-FIELD("Antall")
         .
  DO ix = 1 TO NUM-ENTRIES(icParam,";"):
    CREATE ttArt.
    ASSIGN ttArt.Artikkelnr = DEC(ENTRY(1,ENTRY(ix,icParam,";"),"|"))
           ttArt.Kode       = ENTRY(2,ENTRY(ix,icParam,";"),"|")
           .
  END.
END. 

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  FIND ArtBas NO-LOCK
       WHERE ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
       NO-ERROR.
  IF AVAIL ArtBas THEN DO:
    IF iButikkNr = 0 AND iProfilNr NE 0 THEN
      /* Oppretter etiketter for alle butikker. */
      FOR EACH Butiker NO-LOCK
          WHERE Butiker.ProfilNr = iProfilNr:
        IF bAlleStr THEN
          FOR EACH bStrekKode NO-LOCK 
              OF ArtBas:
            RUN OpprettKo(Butiker.Butik,bStrekKode.Kode).
          END.
        ELSE IF VALID-HANDLE(hKode) THEN
          RUN OpprettKo (Butiker.Butik,hKode:BUFFER-VALUE).
      END.
    ELSE IF iButikkNr NE 0 THEN DO:
      /*Oppretter etiketter for den aktuelle butikken */
      IF bAlleStr THEN
        FOR EACH bStrekKode NO-LOCK 
            OF ArtBas:
          RUN OpprettKo(iButikkNr,bStrekKode.Kode).
        END.
      ELSE IF VALID-HANDLE(hKode) THEN
        RUN OpprettKo (iButikkNr,hKode:BUFFER-VALUE).
    END.
  END.
  /* Kode for overføring */
  hQuery:GET-NEXT().
END. 

DELETE OBJECT hQuery NO-ERROR.

obOK = ocReturn = "".


PROCEDURE OpprettKo:
  DEF INPUT PARAM iiButikkNr AS INT  NO-UNDO.
  DEF INPUT PARAM icKode     AS CHAR NO-UNDO.

  FIND FIRST StrekKode NO-LOCK 
       WHERE StrekKode.ArtikkelNr = ArtBas.ArtikkelNr
         AND StrekKode.Kode       = icKode
       NO-ERROR.
       
  IF AVAIL StrekKode THEN DO:
    /* Sjekker om det finnes en pris for profilen. */
    FIND FIRST ArtPris NO-LOCK
         WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr
           AND ArtPris.ProfilNr   = iProfilNr
         NO-ERROR.
    /* använder vi alternativ 2 opt för centrallager */
    /* så letar vi upp det cetrallagret och söker på profilnr */
    IF NOT AVAILABLE ArtPris AND cOptProfilbutik <> "" THEN DO:
        FIND FIRST clOptButiker WHERE clOptButiker.profilnr = iProfilNr NO-LOCK NO-ERROR.
        IF AVAIL clOptButiker AND clOptButiker.sentrallager = FALSE THEN DO:
            /*  */
            iCLOpt = clOptButiker.clButikknr.
            FIND FIRST clOptButiker WHERE clOptButiker.butik = iCLOpt NO-LOCK NO-ERROR.
            IF AVAIL clOptButiker THEN DO:
                FIND FIRST ArtPris WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
                                         ArtPris.ProfilNr   = clOptButiker.ProfilNr NO-LOCK NO-ERROR.
            END.
        END.
    END.

    /* Hvis ikke, brukes sentrallagerets pris. */
    IF NOT AVAILABLE ArtPris THEN 
      FIND FIRST ArtPris NO-LOCK
         WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr
           AND ArtPris.ProfilNr   = clButiker.ProfilNr
         NO-ERROR.

    IF AVAIL ArtPris THEN 
    DO:
      FIND FIRST Etikettko EXCLUSIVE-LOCK WHERE
        Etikettko.ButikkNr    = iiButikkNr AND
        EtikettKo.Kode        = Strekkode.Kode AND
        EtikettKo.StrKode     = Strekkode.StrKode AND
        EtikettKo.UtskriftsNr = 0 NO-ERROR.
      IF NOT AVAILABLE EtikettKo THEN 
      DO:
          CREATE Etikettko.
          BUFFER-COPY ArtBas TO Etikettko
          ASSIGN 
              Etikettko.ButikkNr    = iiButikkNr 
              EtikettKo.Kode        = Strekkode.Kode 
              EtikettKo.StrKode     = Strekkode.StrKode 
              EtikettKo.UtskriftsNr = 0 
              NO-ERROR.
        
      END. 

      ASSIGN 
          EtikettKo.UtskriftsNr = 0
          Etikettko.Pris[1]     = ArtPris.Pris[1]
          Etikettko.Pris[2]     = ArtPris.Pris[2]
          EtikettKo.EtikettAntHylleplasser = iAntall
          EtikettKo.JamforEnhet = ArtBas.JamforEnhet
          EtikettKo.Mengde      = ArtBas.Mengde
          .
      IF Etikettko.EtikettAntHylleplasser = 0 THEN
        Etikettko.EtikettAntHylleplasser = 1.
      IF Etikettko.Etikettekst1 = "" THEN
        Etikettko.Etikettekst1 = ArtBas.Beskr.
        
    END. /* ARTPRIS */
  END.
END PROCEDURE.
