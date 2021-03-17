DEFINE INPUT PARAMETER pcLogg AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER lArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
DEFINE INPUT PARAMETER iLinjeNr AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttMailLogg 
  FIELD LinjeNr AS INTEGER 
  FIELD Tekst AS CHARACTER 
  .

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttMailLogg.

DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
 
DEFINE BUFFER bufPrisKo FOR PrisKo.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

ASSIGN 
  cLogg = IF pcLogg <> '' THEN 
            pcLogg 
          ELSE 
            'artbasPriskoTilbudDeAktiver' + REPLACE(STRING(TODAY),'/','')
  .

FIND ArtBas  NO-LOCK WHERE 
  ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
IF AVAILABLE ArtBas THEN 
  cTekst = ' LinjeNr: ' + STRING(iLinjeNr)    
          + ' ArtikkelNr/Varetekst/modellnr/farge: '
          + STRING(ArtBas.ArtikkelNr) + '/' 
          + ArtBas.Beskr + '/'  
          + ArtBas.LevKod + '/'  
          + ArtBas.LevFargKod
          . 

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Start.' 
  ).

/* Slår av tilbud på prisprofilen.  */
FOR EACH ArtPris EXCLUSIVE-LOCK WHERE 
    ArtPris.ArtikkelNr = lArtikkelNr:

    IF ArtPris.tilbud THEN 
    DO:
      /* Logger resultatet */
      IF AVAILABLE ArtBas THEN
      DO:
        CREATE ttMailLogg.
        ASSIGN 
          ttMailLogg.LinjeNr = iLinjeNr
          ttMailLogg.Tekst   = '  Slår av artPris tilbud på artikkel:  for prisprofil ' + STRING(ArtPRis.ProfilNr) + cTekst
          .
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            ttMailLogg.Tekst 
            ).
      END.
       
      ASSIGN 
        ArtPris.tilbud = FALSE 
        .
    END.
END.

/* Slår av kampanje på alle aktive profiler.             */
/* Setter AV postens dato og tid til dagens dato og tid. */
/* Tilbudet deaktiveres neste gang priskøen behandles.   */
FOR EACH PrisKo NO-LOCK WHERE
    PrisKo.ArtikkelNr = lArtikkelNr AND
    PrisKo.TYPE       = 3:

    DO FOR bufPrisKo TRANSACTION:
        FIND bufPrisko EXCLUSIVE-LOCK WHERE 
            RECID(bufPrisKo) = RECID(PrisKo) NO-WAIT NO-ERROR.
        IF AVAILABLE bufPrisKo THEN 
        DO:
          ASSIGN
              bufPrisKo.AktiveresDato = TODAY
              bufPrisKo.AktiveresTid  = TIME
              NO-ERROR.
          IF AVAILABLE bufPrisKo THEN 
              RELEASE bufPrisKo.
          FIND ArtBas  NO-LOCK WHERE 
            ArtBas.ArtikkelNr = bufPrisKo.ArtikkelNr NO-ERROR.

          IF AVAILABLE ArtBas THEN
          DO: 
            CREATE ttMailLogg.
            ASSIGN 
              ttMailLogg.LinjeNr = iLinjeNr
              ttMailLogg.Tekst   = 'Slår av priskø kampanjepost på artikkel: ' + cTekst
              .
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                ttMailLogg.Tekst 
                ).
          END.
        END.
    END. /* TRANSACTION */
END.

/* Sletter 'PÅ' poster - Aktiverte, men ennå ikke behandlede PÅ kampanjeposter. */
FOR EACH bufPrisKo EXCLUSIVE-LOCK WHERE 
    bufPrisKo.ArtikkelNr = lArtikkelNr AND
    bufPrisko.Type       = 2:

    FIND ArtBas  NO-LOCK WHERE 
      ArtBas.ArtikkelNr = bufPrisKo.ArtikkelNr NO-ERROR.

    /* Logger resultatet */
    IF AVAILABLE ArtBas THEN 
    DO:
      CREATE ttMailLogg.
      ASSIGN 
        ttMailLogg.LinjeNr = iLinjeNr
        ttMailLogg.Tekst   = 'Sletter ikke startet kampanje fra priskø på artikkel: ' + cTekst
        .
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          ttMailLogg.Tekst 
          ).
    END.
     
    /* Døden */
    DELETE PrisKo.
END.

/* Fjerner artikkelen fra alle kampanjer den er med i. Også behandlede kampanjer. */
/* Årsaken er at de gjnbruker og bygger videre på gamle kampanjer.                */
FOR EACH KampanjeLinje EXCLUSIVE-LOCK WHERE 
    KampanjeLinje.ArtikkelNr = lArtikkelNr:

    FIND ArtBas  NO-LOCK WHERE 
      ArtBas.ArtikkelNr = KampanjeLinje.ArtikkelNr NO-ERROR.

    /* Logger resultatet */
    IF AVAILABLE ArtBAs THEN
    DO: 
      CREATE ttMailLogg.
      ASSIGN 
        ttMailLogg.LinjeNr = iLinjeNr
        ttMailLogg.Tekst   = 'Fjerner artikkel fra alle kampanjelister: ' + cTekst
        .
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          ttMailLogg.Tekst 
          ).
    END.
    /* Døden */
    DELETE KampanjeLinje.
END.

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Slutt.' 
  ).
