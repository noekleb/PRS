/* artpris_kampanje.p

    cType1 = "Varetekst;Lev.artikkelnr;Lev.farkgekode;TilbudFraDato;TilbudTilDato;Pris;%Rabatt 1;Pris;Solgt%;AntSolgt;VerdiSolgt;Lager;Lager verdi;Varemerke;Produsent;Sesong;Varegruppe;Hovedgruppe;Prisprofil;Artikkelnummer"
    cType2 = ''
    cType3 = ''

-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iLagerBut AS INTEGER NO-UNDO.
DEFINE VARIABLE iSalgBut AS INTEGER NO-UNDO.
DEFINE VARIABLE iProfilNr AS INTEGER NO-UNDO.
DEFINE VARIABLE hQuery    AS HANDLE NO-UNDO.
DEFINE VARIABLE cLinje AS CHARACTER NO-UNDO.
DEFINE VARIABLE iType AS INTEGER NO-UNDO.
DEFINE VARIABLE iKampanjeId AS INTEGER NO-UNDO.
DEFINE VARIABLE lArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>9" NO-UNDO.
DEFINE VARIABLE lKamp% AS DECIMAL NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cEan AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLevKod AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLevFargKod AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRab% AS DECIMAL NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE TEMP-TABLE ttKampanjeHode LIKE KampanjeHode
  INDEX KampanjeId KampanjeId.
DEFINE TEMP-TABLE ttKampanjeLinje LIKE KampanjeLinje
    FIELD Beskr AS CHAR 
    FIELD LevNr AS INT 
    FIELD LevKod AS CHAR 
    FIELD LevFargKod AS CHAR
    FIELD Rab% AS DEC FORMAT "->>>9.99"
    FIELD Klar AS LOG 
  INDEX KampanjeId KampanjeId Vg LopNr.

DEFINE BUFFER bufArtPris FOR ArtPris.

{ cls\StdFunk\dsttImpFil.i }

ASSIGN
  bTest       = IF SEARCH('test.txt') = ? THEN FALSE ELSE TRUE
  iKampanjeId = INTEGER(ENTRY(1,icParam,'|'))
  iType       = INTEGER(ENTRY(2,icParam,'|'))
  iProfilNr   = INTEGER(ENTRY(3,icParam,'|'))
  cLogg       = ENTRY(4,icParam,'|')
  .
IF cLogg = '' THEN
  cLogg = 'kampanje_import' + REPLACE(STRING(TODAY),'/','').

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

FIND KampanjeHode NO-LOCK WHERE
  KampanjeHode.KampanjeId = iKampanjeId NO-ERROR.

IF bTest THEN
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg, '  Start kampanje_import.p').
  rStandardFunksjoner:SkrivTilLogg(cLogg, '  Parametre:').
  rStandardFunksjoner:SkrivTilLogg(cLogg, '    iKampanjeId: ' + STRING(iKampanjeId)).
  rStandardFunksjoner:SkrivTilLogg(cLogg, '    iProfilNr  : ' + STRING(iProfilNr)).
  rStandardFunksjoner:SkrivTilLogg(cLogg, '    iType      : ' + STRING(iType)).
  rStandardFunksjoner:SkrivTilLogg(cLogg, '    ihBuffer   : ' + ihBuffer:NAME).
  rStandardFunksjoner:SkrivTilLogg(cLogg, '    AVAIL Hode : ' + STRING(AVAILABLE KampanjeHode)).
END.

IF NOT AVAILABLE KampanjeHode THEN
  DO:
    ocReturn = '** Ukjent kampanjeid ' + STRING(iKampanjeId) + '.'.
    ASSIGN
      obOK = YES
      obOk = ocReturn = ""
      .
    RETURN.
  END.
ELSE
  DO:
    CREATE ttKampanjeHode.
    BUFFER-COPY KampanjeHode TO ttKampanjeHode.
  END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
BUFFER_LOOP:
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF AVAILABLE ttImpFilLinje THEN 
    RELEASE ttImpFilLinje.
    
  ASSIGN
    cLinje = ihBuffer:BUFFER-FIELD("Record"):BUFFER-VALUE
    NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
  DO:
    cLinje = ''.
    IF bTest THEN rStandardFunksjoner:SkrivTilLogg(cLogg, '  ** Feil ved assign cLinje.').
  END.
  
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg, '  Linje: ' + cLinje).

  /* Header linjer. */
  IF cLinje = '' OR cLinje BEGINS 'Varetekst' OR cLinje BEGINS '1;2;3;4' THEN
  DO:
    IF bTest THEN rStandardFunksjoner:SkrivTilLogg(cLogg, "  ** Linje ignorert: " + cLinje).
  END.
  ELSE DO:
    CASE itype:
      WHEN 1 THEN
        DO:
          ASSIGN
            lArtikkelNr = DEC(ENTRY(20,cLinje,';'))
            lKamp%      = DEC(REPLACE(ENTRY( 7,cLinje,';'),'.',',')) * - 1
            NO-ERROR.
          IF ERROR-STATUS:ERROR THEN
          DO:
            IF bTest THEN 
              rStandardFunksjoner:SkrivTilLogg(cLogg, "  ** Feil ved assign artikkelnr/kamp%").
            lArtikkelNr = 0.
          END.
        END.
        
      WHEN 2 THEN
        DO:
          ASSIGN
            lArtikkelNr = DEC(ENTRY(17,cLinje,';'))
            lKamp%      = KampanjeHode.Kamp% /*DEC(REPLACE(ENTRY( 7,cLinje,';'),'.',',')) * - 1*/
            NO-ERROR.
          IF ERROR-STATUS:ERROR THEN
          DO:
            IF bTest THEN 
              rStandardFunksjoner:SkrivTilLogg(cLogg, "  ** Feil ved assign artikkelnr/kamp%").
            lArtikkelNr = 0.
          END.
        END.

      WHEN 3 THEN
        DO:
          IF TRIM(ENTRY(1,cLinje,';')) = 'H' THEN 
          DO:
            ASSIGN 
              lRab% = IF DEC(REPLACE(ENTRY(10,cLinje,';'),'.',',')) > 0 THEN DEC(REPLACE(ENTRY(10,cLinje,';'),'.',',')) * - 1 ELSE KampanjeHode.Kamp%
              . 
            hQuery:GET-NEXT().
          END.
          ELSE IF TRIM(ENTRY(1,cLinje,';')) = 'L' THEN 
          DO:
            ASSIGN
              cLevKod     = TRIM(ENTRY( 3,cLinje,';'))
              cLevFargKod = TRIM(ENTRY( 4,cLinje,';'))
              lKamp%      = IF DEC(REPLACE(ENTRY( 6,cLinje,';'),'.',',')) > 0 THEN DEC(REPLACE(ENTRY( 6,cLinje,';'),'.',',')) * - 1 ELSE lRab%
              NO-ERROR.
            FIND FIRST ArtBas NO-LOCK WHERE 
              ArtBas.LevKod = cLevKod AND 
              ENTRY(1,ArtBas.LevFargKod,'/') = ENTRY(1,cLevFargKod,'/') NO-ERROR.
            IF AVAILABLE ArtBas THEN 
              lArtikkelNr = ArtBas.ArtikkelNr.
            ELSE 
              lArtikkelNr = 0.            
          END.
        END.

      WHEN 4 THEN
        DO:
          ASSIGN
            cLevKod     = TRIM(ENTRY( 5,cLinje,';'))
            cLevFargKod = TRIM(ENTRY( 6,cLinje,';'))
            lKamp%      = KampanjeHode.Kamp% /*DEC(REPLACE(ENTRY( 7,cLinje,';'),'.',',')) * - 1*/
            No-ERROR.
            FIND FIRST ArtBas NO-LOCK WHERE 
              ArtBas.LevKod = cLevKod AND 
              ENTRY(1,ArtBas.LevFargKod,'/') = ENTRY(1,cLevFargKod,'/') NO-ERROR.
            IF AVAILABLE ArtBas THEN 
              lArtikkelNr = ArtBas.ArtikkelNr.
            ELSE 
              lArtikkelNr = 0.            
        END.
    END CASE.

    IF lArtikkelNr > 0 THEN 
    DO: 
      FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
      DO:
        IF bTest THEN rStandardFunksjoner:SkrivTilLogg(cLogg, "  ** Ukjent artikkel " + STRING(lArtikkelNr)).
        hQuery:GET-NEXT().
      END.

      IF KampanjeHode.Kamp% = 0 THEN
        KampanjeHode.Kamp% = lKamp%.

      CREATE ttKampanjeLinje.
      ASSIGN
        iLinjeNr                   = iLinjeNr + 1
        ttKampanjeLinje.ProfilNr   = ttKampanjeHode.ProfilNr
        ttKampanjeLinje.KampanjeId = ttKampanjeHode.KampanjeId
        ttKampanjeLinje.Rab%       = lKamp%
        ttKampanjeLinje.KampanjeLinje = iLinjeNr
        .
    END.  
  
    IF AVAILABLE ttKampanjeLinje THEN 
    DO:
      ASSIGN
        ttKampanjeLinje.LevNr      = IF ttKampanjeLinje.LevNr = 0 THEN 40 ELSE ttKampanjeLinje.LevNr
        ttKampanjeLinje.LevKod     = ArtBas.LevKod
        ttKampanjeLinje.LevFargKod = ArtBas.LevFargKod
        ttKampanjeLinje.Behandlet  = FALSE
        ttKampanjeLinje.Klar       = FALSE
        ttKampanjeLinje.ArtikkelNr = ArtBas.ArtikkelNr
        ttKampanjeLinje.Beskr      = ArtBas.Beskr
        ttKampanjeLinje.Vg         = ArtBas.Vg
        ttKampanjeLinje.LopNr      = ArtBas.LopNr
        ttKampanjeLinje.Klar       = TRUE
        .
    
      /* Henter lokal pris. */
      FIND FIRST ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
          ArtPris.ProfilNr   = ttKampanjeHode.ProfilNr NO-ERROR.
    
      /* finnes ikke lokal pris, opprettes denne. */
      IF NOT AVAILABLE ArtPris THEN
      DO:
          FIND FIRST bufArtPris NO-LOCK WHERE
              bufArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
              bufArtPris.ProfilNr   = 1 NO-ERROR.
          CREATE ArtPris.
          BUFFER-COPY bufArtPris
              EXCEPT ProfilNr
              TO ArtPris
              ASSIGN
              ArtPris.ProfilNr = ttKampanjeHode.ProfilNr
              NO-ERROR.
      END.
    
      IF AVAILABLE ArtPris THEN
          ASSIGN
          ttKampanjeLinje.Pris[1]  = ArtPris.Pris[1]
          ttKampanjeLinje.Pris[2]  = ArtPris.Pris[1] +
                                     ((ArtPris.Pris[1] * (IF ttKampanjeLinje.Rab% <> 0
                                                            THEN ttKampanjeLinje.Rab%
                                                            ELSE ttKampanjeHode.Kamp%)) / 100)
          ttKampanjeLinje.VareKost = ArtPris.VareKost[1]
          .
    END.
  END.
  
  hQuery:GET-NEXT().
END. /* BUFFER_LOOP */

DELETE OBJECT hQuery.

IF CAN-FIND(FIRST ttKampanjeLinje) THEN
  DO:
    IF bTest THEN
      rStandardFunksjoner:SkrivTilLogg(cLogg, '    Kjører PosterData').
    RUN PosterData.
  END.

IF bTest THEN
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg, '  Slutt kampanje_import.p').
END.


ASSIGN
  obOK = YES
  obOk = ocReturn = ""
  .

/* **********************  Internal Procedures  *********************** */

PROCEDURE PosterData:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF bTest THEN rStandardFunksjoner:SkrivTilLogg(cLogg, "  Kjører 'PosterData'").
  
  FOR EACH ttKampanjeHode WHERE
      ttKampanjeHode.KampanjeId = iKampanjeId TRANSACTION:

    FOR EACH ttKampanjeLinje WHERE
        ttKampanjeLinje.KampanjeId = ttKampanjeHode.KampanjeId AND
        ttKampanjeLinje.Klar       = TRUE:
        CREATE KampanjeLinje.
        BUFFER-COPY ttKampanjeLinje 
          TO KampanjeLinje
          ASSIGN 
            KampanjeLinje.RegistrertDato = TODAY 
            KampanjeLinje.RegistrertTid = TIME 
            KampanjeLinje.RegistrertAv = USERID('skotex') 
            KampanjeLinje.EDato = TODAY 
            KampanjeLinje.ETid = TIME 
            KampanjeLinje.Brukerid = USERID('skotex') 
          NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            IF AVAILABLE KampanjeLinje THEN DELETE KampanjeLinje.
    END.
  END. /* TRANSACTION */
END PROCEDURE.

