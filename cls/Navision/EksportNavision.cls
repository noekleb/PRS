 
 /*------------------------------------------------------------------------
    File        : EksportNavision
    Purpose     : 
    Syntax      : 
    Description : Eksporterer ulike datatyper til Navision
    Author(s)   : tomn
    Created     : Mon Sep 30 14:59:39 CEST 2019
    Notes       :  
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS cls.Navision.EksportNavision: 
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
  DEFINE VARIABLE bTest AS LOG NO-UNDO.
  DEFINE VARIABLE cBku AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.  
  
  DEFINE VARIABLE cSendLst            AS CHARACTER                      NO-UNDO.
  DEFINE VARIABLE iX                  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iLoop               AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE obOk AS LOG NO-UNDO.
  DEFINE VARIABLE bSettEOD            AS LOG NO-UNDO.
  DEFINE VARIABLE cOutletLst          AS CHARACTER NO-UNDO.  
  DEFINE VARIABLE cFilNavn            AS CHARACTER                      NO-UNDO.
  DEFINE VARIABLE cKatalog            AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cError              AS CHARACTER                      NO-UNDO.
  DEFINE VARIABLE iAntDager           AS INTEGER NO-UNDO.
  DEFINE VARIABLE iAvdelingsNr        AS INTEGER NO-UNDO.
  DEFINE VARIABLE iFraAvdelingsNr     AS INTEGER NO-UNDO.
  DEFINE VARIABLE cPilotLst           AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cNettbutikkLst      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cNettButLagerLst    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iGantAktiv AS INTEGER NO-UNDO.

  DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.

  DEFINE PUBLIC PROPERTY iAktiv AS INTEGER NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY cDagsoppgjFil AS CHARACTER NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY cFakturaFil AS CHARACTER NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY iEksportType AS INTEGER NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY cDagsKatalog AS CHARACTER NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY cFakturaKatalog AS CHARACTER NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY iReEksport AS INTEGER NO-UNDO GET. SET.

  DEFINE TEMP-TABLE ttButiker SERIALIZE-NAME "Butikk"
    FIELD AvdelingsNr AS INTEGER FORMAT ">>>>>9"
    FIELD ButNr AS INTEGER FORMAT ">>>>>9"
    FIELD ButNamn AS CHARACTER FORMAT "x(30)"
    INDEX idxButiker IS UNIQUE PRIMARY AvdelingsNr.
  
  DEFINE TEMP-TABLE ttBokforingsbilag SERIALIZE-NAME "Bokforingsbilag"
    FIELD AvdelingsNr AS INTEGER FORMAT ">>>>>9"
    FIELD Dato AS DATE FORMAT "99/99/9999"
    FIELD BokforingsId AS DECIMAL FORMAT ">>>>>>>>>>>>>>>9" /*SERIALIZE-HIDDEN*/   
    FIELD BokforingsNr AS INTEGER FORMAT ">>>>>9"
    FIELD ButNamn AS CHARACTER FORMAT "x(40)"
    INDEX idxButDatoLinje IS UNIQUE PRIMARY AvdelingsNr Dato
    INDEX idxBokforingsId IS UNIQUE BokforingsId 
    .
    
  DEFINE TEMP-TABLE ttBilagslinjer SERIALIZE-NAME "Bilagslinjer"
    FIELD BokforingsId AS DECIMAL FORMAT ">>>>>>>>>>>>>>>9" /*SERIALIZE-HIDDEN*/
    FIELD LinjeNr AS INTEGER FORMAT ">>>>>>9"
    FIELD AvdelingsNr AS INTEGER FORMAT ">>>>>9"
    FIELD ButNamn AS CHARACTER FORMAT "x(40)"
    FIELD Dato AS DATE FORMAT "99/99/9999"
    FIELD Tekst AS CHARACTER FORMAT "x(40)"
    FIELD KontoNr AS INTEGER FORMAT ">>9999"
    FIELD Belop AS DECIMAL DECIMALS 2 FORMAT "->>>>>>>>>>9.99"
    INDEX idxBokforingsId IS PRIMARY BokforingsId LinjeNr
    INDEX idxButDatoLinje AvdelingsNr Dato LinjeNr
    .
  
  DEFINE TEMP-TABLE ttBokforingsKorrBilag SERIALIZE-NAME "BokforingsKorrBilag"
    FIELD BokforingsId AS DECIMAL FORMAT ">>>>>>>>>>>>>>>9" /*SERIALIZE-HIDDEN*/
    FIELD KLinjeNr AS INTEGER FORMAT ">>>>>>9"
    FIELD LinjeNr AS INTEGER FORMAT ">>>>>>9"
    FIELD KontoNr AS INTEGER FORMAT ">>9999"
    FIELD Tekst AS CHARACTER FORMAT "x(40)"
    FIELD Belop AS DECIMAL DECIMALS 2 FORMAT "->>>>>>>>>>9.99"
    FIELD Dato AS DATE FORMAT "99/99/9999"
    INDEX idxBokforingsId IS PRIMARY BokforingsId KLinjeNr LinjeNr
    .
/*    INDEX idxBokforingsId IS UNIQUE PRIMARY BokforingsId KLinjeNr LinjeNr*/
      
  DEFINE TEMP-TABLE ttTilgode SERIALIZE-NAME "SpesifikasjonTilgodeFraAndre" 
    FIELD BokforingsId AS DECIMAL FORMAT ">>>>>>>>>>>>>>>9" /*SERIALIZE-HIDDEN*/
    FIELD LinjeNr AS INTEGER FORMAT ">>>>>>9"
    FIELD IdentNr AS CHARACTER FORMAT "x(30)"
    FIELD BruktDatotid AS DATETIME FORMAT "99/99/9999 HH:MM:SS"
    FIELD AvdelingsNr AS INTEGER FORMAT ">>>>>>9" 
    FIELD BongNr AS INTEGER FORMAT ">>>>>9"
    FIELD BruktKasseNr AS INTEGER FORMAT ">>>>9"
    FIELD SelgerNr AS DECIMAL FORMAT ">>>>>>>>>>>>9"
    FIELD Navn AS CHARACTER FORMAT "x(30)"
    FIELD Belop AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD FraAvdelingsNr AS INTEGER FORMAT ">>>>>9"
    FIELD RegistrertDatoTid AS DATETIME FORMAT "99/99/9999 HH:MM:SS"
    INDEX idxTilgode IS PRIMARY BokforingsId LinjeNr IdentNr
    .  
      
DEFINE DATASET dsDagsoppgjor SERIALIZE-NAME "Dagsoppgjor"
  FOR ttButiker, ttBokforingsbilag, ttBilagslinjer, ttBokforingsKorrBilag, ttTilgode
  DATA-RELATION drButBokfbilag FOR ttButiker, ttBokforingsbilag RELATION-FIELDS (AvdelingsNr, AvdelingsNr) NESTED
  DATA-RELATION drButBokfLinjer FOR ttBokforingsbilag, ttBilagslinjer RELATION-FIELDS (BokforingsId, BokforingsId) NESTED
  DATA-RELATION drButBokfLinjerKorr FOR ttBilagslinjer, ttBokforingsKorrBilag RELATION-FIELDS (BokforingsId, BokforingsId, LinjeNr, KLinjeNr) NESTED
  DATA-RELATION drButBokfTilgode FOR ttBilagslinjer, ttTilgode RELATION-FIELDS (BokforingsId, BokforingsId, LinjeNr, LinjeNr) NESTED
  .

DEFINE BUFFER bBokforingsVisning FOR BokforingsVisning.
DEFINE BUFFER bttBilagsLinjer FOR ttBilagsLinjer.

  CONSTRUCTOR PUBLIC EksportNavision ( pcLogg AS CHARACTER   ):
    SUPER ().
    {syspara.i 210 100 8 iGantAktiv INT}    
    {syspara.i 150 1 2 cNettButikkLst}
    {syspara.i 150 1 3 cNettButLagerLst}

    rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner().
    
    ASSIGN 
      cLogg = IF pcLogg <> '' THEN 
                pcLogg 
              ELSE 
                'eksporterDagsoppgjor' + REPLACE(STRING(TODAY),'/','')
      cBku    = 'bku'
      bTest = IF SEARCH('test.txt') <> ? THEN TRUE ELSE FALSE
      .
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          'EksportNavision() NEW:' 
          ).
    
    
  END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL prepDagsrapp( piType AS INTEGER, piButNr AS INTEGER, pdDato AS DATE ):
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
    DEFINE VARIABLE piAnt  AS INTEGER NO-UNDO.
    DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
    DEFINE VARIABLE piLinjeNr AS INTEGER NO-UNDO.
    DEFINE VARIABLE pBokforingsId AS DECIMAL NO-UNDO.
    DEFINE VARIABLE cButNamn AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lNegDiff AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lPosdiff AS DECIMAL NO-UNDO.
  
    DEFINE BUFFER bBokforingsBilag FOR BokforingsBilag.
    DEFINE BUFFER bButiker FOR Butiker.
    
    EMPTY TEMP-TABLE ttButiker.
    EMPTY TEMP-TABLE ttBokforingsbilag.
    EMPTY TEMP-TABLE ttBilagslinjer.
    EMPTY TEMP-TABLE ttBokforingsKorrBilag.
    EMPTY TEMP-TABLE ttTilgode.
    
    /* NB: Avrunder beløp til nærmeste hele krone nederst i rutinen. */
    
    bResult = FALSE.
    FOR EACH bButiker NO-LOCK WHERE 
      bButiker.butik = piButNr AND /* TN 15/9-20 Programmet bygget om for å behandle en butikk ad gangen. */
      bButiker.ApningsDato <> ? AND 
      bButiker.harButikksystem = TRUE AND 
      (bButiker.NedlagtDato = ? OR 
       bButiker.NedlagtDato <= TODAY) AND 
      NOT CAN-DO('848,849', STRING(bButiker.Butik)): 
         
      /* Utlegg av alle vanlige butikker. Ikke kommisjonsbutikker. */
      IF piType = 1 THEN
        DO: 
          IF CAN-DO(cOutletLst,STRING(bButiker.butik)) THEN 
            NEXT.
          /* Skipper Kommisjonsbutikkene. */
          IF CAN-FIND(FIRST ImpKonv WHERE 
                      ImpKonv.EDB-System = 'Gant Global' AND 
                      ImpKonv.Tabell     = 'KommisjonBut' AND 
                      ImpKonv.InterntID = STRING(piButNr)) THEN 
            NEXT.
        END.
      /* Utlegg av Outlet og kommisjonsbutikker. */
      ELSE IF piType = 2 THEN
        DO: 
          FIND FIRST ImpKonv NO-LOCK WHERE 
              ImpKonv.EDB-System = 'Gant Global' AND 
              ImpKonv.Tabell     = 'KommisjonBut' AND 
              ImpKonv.InterntID = STRING(piButNr) NO-ERROR.
          IF NOT AVAILABLE ImpKonv THEN 
            DO: 
              IF NOT CAN-DO(cOutletLst,STRING(bButiker.butik)) THEN 
                NEXT.
            END.
        END.
        
      /* For Gant skal ikke nettbutikkens oppgjør legges ut. */
      IF LOOKUP(STRING(bButiker.Butik),cNettButikkLst) > 0 THEN 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    Det gjøres ikke utlegg for nettbutikken: ' + STRING(bButiker.butik) + '.' 
          ).
        NEXT.
      END.

      /* For Gant skal ikke nettbutikkens lager oppgjør legges ut. */
      IF LOOKUP(STRING(bButiker.Butik),cNettButLagerLst) > 0 THEN 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    Det gjøres ikke utlegg for nettbutikkens lager: ' + STRING(bButiker.butik) + '.' 
          ).
        NEXT.
      END.
         
      cButNamn = bButiker.ButNamn.   
         
      /* Tar bare butikker i listen for pilot butikker. */
      IF cPilotLst <> '' THEN 
      DO:
        IF NOT CAN-DO(cPilotLst,STRING(bButiker.butik)) THEN 
          NEXT.
      END.
  
      iAvdelingsNr = hentAvdelingsnr( bButiker.Butik ).    
      IF iAvdelingsNr = 0 THEN 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    Butikk manglermapping i ImpKonv: ' + STRING(bButiker.butik) + '.' 
          ).
        NEXT.
      END.

      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    Prep. av butikk: (' + STRING(piType) + ') ' + STRING(bButiker.Butik) + ' ' + bButiker.ButNamn 
        ).
    
      /* Legger opp butikk recorden. */  
      IF NOT CAN-FIND(ttButiker WHERE 
                      ttButiker.AvdelingsNr = iAvdelingsNr) THEN 
      DO:
        CREATE ttButiker.
        ASSIGN 
          ttButiker.AvdelingsNr = iAvdelingsNr
          ttButiker.ButNr    = bButiker.butik
          ttButiker.butNamn  = bbutiker.butNamn
          .
      END.
      
      /* Henter denne for å kunne sjekke om det er en kommisjonsbutikk. */
      IF AVAILABLE ImpKonv THEN RELEASE ImpKonv.
      FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = 'Gant Global' AND 
        ImpKonv.Tabell     = 'KommisjonBut' AND 
        ImpKonv.InterntID = STRING(bButiker.butik) NO-ERROR.

      FOR EACH Bokforingsbilag NO-LOCK WHERE
        BokforingsBilag.ButikkNr       = bButiker.butik AND
        BokforingsBilag.OmsetningsDato = pdDato AND 
        BokforingsBilag.EODMottatt     = TRUE AND 
        BokforingsBilag.GodkjentFlagg  = TRUE:

        /* Overstyrer eksport til Navision. */
        IF iReEksport = 0 AND BokforingsBilag.SendtRegnskap  = TRUE THEN 
          NEXT.
          
        pBokforingsId = BokforingsBilag.BokforingsId.
          
        piant = 0.
        FOR EACH BongHode NO-LOCK WHERE 
          BongHode.butikkNr = BokforingsBilag.ButikkNr AND 
          BongHode.Dato = BokforingsBilag.OmsetningsDato:
          IF NOT CAN-FIND(FIRST BongLinje WHERE 
                      BongLinje.B_Id = BongHode.B_Id AND 
                      BongLinje.TTId = 92) THEN
              piAnt = piAnt + 1.
        END.

        /* Flagger bare oppgjør hvor det har kommet inn andre bonger en EOD bonger. */
        /* Og det skal opprettes bare en gang.                                      */
        IF piant > 0 AND NOT CAN-FIND( ttBokforingsbilag WHERE 
                                       ttBokforingsbilag.BokforingsId = Bokforingsbilag.BokforingsId ) THEN
        GJORNOEHER: 
        DO:   
          IF CAN-FIND(FIRST kas_rap WHERE kas_rap.dato = BokforingsBilag.OmsetningsDato AND 
                      kas_rap.butikk = BokforingsBilag.ButikkNr) THEN 
              RUN dagsrapp_utskrift.p ('99',BokforingsBilag.ButikkNr,BokforingsBilag.OmsetningsDato,BokforingsBilag.OmsetningsDato,TRUE,OUTPUT pcTekst) NO-ERROR.
          /* Vellykket generering av utskrift og derigjennom generering av BokforingsVisning. */    
          IF SEARCH(pcTekst) <> '' THEN 
          DO TRANSACTION:

            CREATE ttBokforingsbilag.
            ASSIGN 
              ttBokforingsbilag.BokforingsId = Bokforingsbilag.BokforingsId
              ttBokforingsbilag.BokforingsNr = Bokforingsbilag.BokforingsNr  
              ttBokforingsbilag.AvdelingsNr  = iAvdelingsNr
              ttBokforingsbilag.Dato         = Bokforingsbilag.OmsetningsDato
              ttBokforingsbilag.ButNamn      = bButiker.butNamn
              .            
            
            BOKFORINGSVISNINBLOKK:
            FOR EACH BokforingsVisning NO-LOCK WHERE 
              BokforingsVisning.BokforingsID = Bokforingsbilag.BokforingsId:
                
              /* Mva og netto omsetning skal ikke legges ut. */
              IF CAN-DO('3,4',STRING(BokforingsVisning.LinjeNr)) THEN 
                NEXT.
              
              /* 900/30 Pose og veksel skal ikke legges ut. */
              IF BokforingsVisning.TTId = 900 AND BokforingsVisning.TBId = 30 THEN 
                NEXT.
              /* 900/28 Opptalt morgen skal ikke legges ut. */
              IF BokforingsVisning.TTId = 900 AND BokforingsVisning.TBId = 28 THEN 
                NEXT.
              /* 900/29 Opptalt kveld. */
              IF BokforingsVisning.TTId = 900 AND BokforingsVisning.TBId = 29 THEN 
                NEXT.
              /* 800/0 PoseNr. */
              IF BokforingsVisning.TTId = 800 AND BokforingsVisning.TBId = 0 THEN 
                NEXT.
              IF BokforingsVisning.TTId = 800 AND BokforingsVisning.TBId = 1 THEN 
                NEXT.
              /* 900/26 Kasse endring. */
              IF BokforingsVisning.TTId = 900 AND BokforingsVisning.TBId = 26 THEN
                NEXT.
              /* Visningsposter skal ikke legges ut. */
              IF BokforingsVisning.TTId = 0 AND BokforingsVisning.TBId = 0 THEN
                NEXT.
              /* Kontanter skal ikke ut for kommisjonsbutikk. */
              IF iAktiv = 1 AND BokforingsVisning.TTId = 50 AND BokforingsVisning.TBId = 1 THEN 
                NEXT.

              /* 134/1 Gavekort ut. */
              IF BokforingsVisning.TTId = 134 AND BokforingsVisning.TBId = 1 THEN
              BLOKK134:
              DO:
                /* TN 6/10-20. Her skal gavekortene legges opp på bilags linje nivå, og den opprinnelige bilagslinjen undertrykkes. */
                FOR EACH BokforingsKorrbilag NO-LOCK WHERE 
                  BokforingsKorrBilag.BokforingsId = BokforingsVisning.BokforingsId AND
                  BokforingsKorrBilag.TTId         = BokforingsVisning.TTId AND 
                  BokforingsKorrBilag.TBId         = BokforingsVisning.TBId AND   
                  (BokforingsKorrBilag.KontoNr > 0 OR 
                  BokforingsKorrBilag.Belop <> 0) :
                 
                  CREATE ttBilagslinjer.
                  ASSIGN 
                    ttBilagslinjer.BokforingsId = Bokforingsbilag.BokforingsId
                    ttBilagslinjer.AvdelingsNr  = iAvdelingsNr
                    ttBilagslinjer.Dato         = BokforingsBilag.OmsetningsDato 
                    ttBilagslinjer.ButNamn      = bButiker.ButNamn
                    ttBilagslinjer.LinjeNr      = BokforingsVisning.LinjeNr
                    ttBilagslinjer.Tekst        = BokforingsKorrBilag.Merknad
                    ttBilagslinjer.KontoNr      = IF DEC(BokforingsKorrbilag.KontoNr) > 0 THEN 
                                                    DEC(BokforingsKorrbilag.KontoNr)
                                                  ELSE 
                                                    DEC(BokforingsVisning.Konto) 
                    ttBilagslinjer.Belop        = DEC(BokforingsKorrBilag.Belop)
                    .
                END.
                NEXT.
              END. /* BLOKK134 */
              
              CREATE ttBilagslinjer.
              ASSIGN 
                ttBilagslinjer.BokforingsId = BokforingsBilag.BokforingsId
                ttBilagslinjer.AvdelingsNr  = iAvdelingsNr
                ttBilagslinjer.Dato         = BokforingsBilag.OmsetningsDato 
                ttBilagslinjer.ButNamn      = bButiker.ButNamn
                ttBilagslinjer.LinjeNr      = BokforingsVisning.LinjeNr
                ttBilagslinjer.Tekst        = BokforingsVisning.Tekst
                ttBilagslinjer.KontoNr      = DEC(BokforingsVisning.Konto)
                ttBilagslinjer.Belop        = DEC(BokforingsVisning.Belop)
                                  .

              /* Omsetning legges ut negativt. */
              IF CAN-DO('2',STRING(BokforingsVisning.LinjeNr)) THEN
                OMSETNING:
                DO:
                        
                  ttBilagslinjer.Belop = ttBilagslinjer.Belop * -1.

                  /* For kommisjon skal kortgebyr adderes til ... :( GANT forlanger... */
                  IF iAktiv = 1 AND AVAILABLE ImpKonv THEN
                    KOMMISJON:
                    DO: 
                      /* Kortgebyr. */
                      FIND FIRST bBokforingsVisning NO-LOCK WHERE 
                        bBokforingsVisning.BokforingsId = BokforingsVisning.BokforingsId AND 
                        bBokforingsVisning.TTId         = 98 AND 
                        bBokforingsVisning.TBId         = 2 NO-ERROR.
                   
                      IF AVAILABLE bBokforingsVisning THEN 
                        ttBilagslinjer.Belop = ttBilagslinjer.Belop + DEC(bBokforingsVisning.Belop).
                    
                      /* Og så skal GANT ha dr...en motpostert. */
                      /* Finner først siste linjenr. */
                      FIND LAST bBokforingsVisning WHERE 
                        bBokforingsVisning.BokforingsId = BokforingsVisning.BokforingsId NO-ERROR.
                      /* Lager så dubletten for motpostering. */
                      IF AVAILABLE bBokforingsVisning THEN 
                        DO:
                          CREATE bttBilagslinjer.
                          BUFFER-COPY ttBilagsLinjer
                            EXCEPT LinjeNr Belop KontoNr
                            TO bttBilagsLinjer
                            ASSIGN
                              bttBilagsLinjer.LinjeNr = bBokforingsVisning.LinjeNr + 1  
                              bttBilagsLinjer.Belop   = ttBilagslinjer.Belop * -1  
                              bttBilagsLinjer.KontoNr = 3126  
                              .
                        END.
                    END. /* KOMMISJON */
                END. /* OMSETNING */
                
              /* Varekjøp */
              IF BokforingsVisning.TTId = 5 AND BokforingsVisning.TBId = 1 THEN 
                ttBilagslinjer.Belop = ttBilagslinjer.Belop * -1.
              /* Svinn */
              IF BokforingsVisning.TTId = 9 AND BokforingsVisning.TBId = 1 THEN 
                ttBilagslinjer.Belop = ttBilagslinjer.Belop * -1.
                
              FOR EACH BokforingsKorrbilag NO-LOCK WHERE 
                BokforingsKorrBilag.BokforingsId = BokforingsVisning.BokforingsId AND
                BokforingsKorrBilag.TTId         = BokforingsVisning.TTId AND 
                BokforingsKorrBilag.TBId         = BokforingsVisning.TBId AND   
                (BokforingsKorrBilag.KontoNr > 0 OR 
                BokforingsKorrBilag.Belop <> 0) :
                  
                /* TN 8/9-20 Øresavrundingen skal ikke legges ut. */  
                IF BokforingsKorrBilag.Merknad BEGINS 'Automatisk øres' THEN 
                  NEXT.
               
                CREATE ttBokforingsKorrbilag.
                ASSIGN 
                  ttBokforingsKorrbilag.BokforingsId = Bokforingsbilag.BokforingsId
                  ttBokforingsKorrbilag.KLinjeNr     = BokforingsVisning.LinjeNr
                  ttBokforingsKorrbilag.LinjeNr      = BokforingsKorrbilag.LinjeNr
                  ttBokforingsKorrbilag.KontoNr      = BokforingsKorrbilag.KontoNr 
                  ttBokforingsKorrbilag.Tekst        = BokforingsKorrBilag.Merknad
                  ttBokforingsKorrbilag.Belop        = BokforingsKorrBilag.Belop
                  ttBokforingsKorrbilag.Dato         = BokforingsBilag.OmsetningsDato /*DATE(BokforingsKorrBilag.DatoTid)*/
                  .  
              END.
              /* For å få med spes av tilgodelapper fra andre butikker */
              IF BokforingsVisning.TTId = 900 AND BokforingsVisning.TBId = 16 THEN 
              TILGODESPES:
              FOR EACH Tilgode NO-LOCK WHERE 
                  Tilgode.BruktDato  = BokforingsBilag.OmsetningsDato AND 
                  Tilgode.BruktTid  >= 0 AND
                  Tilgode.BruktButNr = BokforingsBilag.ButikkNr:
              
                  /* Spesifiser bare tilgodelapper mottatt fra andre butikker. */
                  IF Tilgode.BruktButNr = Tilgode.ButNr THEN 
                      NEXT.
              
                  FIND Selger NO-LOCK WHERE 
                      Selger.SelgerNr = Tilgode.BruktSelgerNr NO-ERROR.
                      
                  iAvdelingsNr = hentAvdelingsnr( Tilgode.BruktButNr ).    
                  iFraAvdelingsNr = hentAvdelingsnr( Tilgode.ButNr ).    
                      
                  CREATE ttTilgode.
                  ASSIGN 
                    ttTilgode.BokforingsId      = Bokforingsbilag.BokforingsId
                    ttTilgode.LinjeNr           = BokforingsVisning.LinjeNr
                    ttTilgode.IdentNr           = tilgode.IdentNr 
                    ttTilgode.BruktDatotid      = DATETIME(STRING(Tilgode.BruktDato,"99/99/9999") + ' ' + STRING(Tilgode.BruktTid,"HH:MM:SS"))
                    ttTilgode.AvdelingsNr       = iAvdelingsNr  
                    ttTilgode.BongNr            = Tilgode.BruktBongNr 
                    ttTilgode.BruktKasseNr      = Tilgode.BruktKasseNr 
                    ttTilgode.SelgerNr          = Tilgode.BruktSelgerNr 
                    ttTilgode.Navn              = IF AVAILABLE Selger THEN Selger.Navn ELSE ''  
                    ttTilgode.Belop             = Tilgode.Belop 
                    ttTilgode.FraAvdelingsNr    = iFraAvdelingsNr
                    ttTilgode.RegistrertDatoTid = DATETIME(STRING(Tilgode.RegistrertDato,"99/99/9999") + ' ' + STRING(Tilgode.RegistrertTid,"HH:MM:SS"))
                    .
              END. /* TILGODESPES */
            END. /* BOKFORINGSVISNINBLOKK */
            
            FIND bBokforingsBilag EXCLUSIVE-LOCK WHERE 
              ROWID(bBokforingsBilag) = ROWID(BokforingsBilag) NO-ERROR NO-WAIT.
            IF AVAILABLE bBokforingsBilag AND NOT LOCKED bBokforingsBilag THEN
            DO: 
              ASSIGN
                bBokforingsBilag.SendtRegnskap = TRUE
                bBokforingsBilag.SendtDato     = TODAY
                bBokforingsBilag.SendtTid      = TIME 
                bBokforingsBilag.SendAv        = USERID('Skotex')
                .
              RELEASE bBokforingsBilag.
              bResult = TRUE.
            END. 
          END. /* TRANSACTION */
        END. /* GJORNOEHER */
      END.
    END.

    /* TN 14/10-20 Avrunding. */
    ASSIGN 
      lNegDiff = 0
      lPosdiff = 0
      .

    FOR EACH ttBilagslinjer
      BREAK BY ttBilagslinjer.BokforingsId
            BY ttBilagslinjer.LinjeNr:
      piLinjeNr = ttBilagslinjer.LinjeNr.
      /* Samler opp avrunding */
      IF ttBilagslinjer.Belop > 0 THEN 
        lPosdiff = lPosdiff + (ttBilagslinjer.Belop - ROUND(ttBilagslinjer.Belop,0)).
      ELSE IF ttBilagsLinjer.Belop < 0 THEN 
        lNegdiff = lNegdiff + ABS((ttBilagslinjer.Belop - ROUND(ttBilagslinjer.Belop,0))).
      /* Avrunder. */
      ttBilagslinjer.Belop = ROUND(ttBilagslinjer.Belop,0).       
    END. 
    FOR EACH ttBokforingsKorrBilag:
      /* Samler opp avrunding */
      IF ttBokforingsKorrBilag.Belop > 0 THEN 
        lPosdiff = lPosdiff + (ttBokforingsKorrBilag.Belop - ROUND(ttBokforingsKorrBilag.Belop,0)).
      ELSE IF ttBokforingsKorrBilag.Belop < 0 THEN 
        lNegdiff = lNegdiff + ABS((ttBokforingsKorrBilag.Belop - ROUND(ttBokforingsKorrBilag.Belop,0))).
      /* Avrunder. */
      ttBokforingsKorrBilag.Belop = ROUND(ttBokforingsKorrBilag.Belop,0).       
    END. 
    FOR EACH ttTilgode:
      /* Samler opp avrunding */
      IF ttTilgode.Belop > 0 THEN 
        lPosdiff = lPosdiff + (ttTilgode.Belop - ROUND(ttTilgode.Belop,0)).
      ELSE IF ttTilgode.Belop < 0 THEN 
        lNegdiff = lNegdiff + ABS((ttTilgode.Belop - ROUND(ttTilgode.Belop,0))).
      /* Avrunder. */
      ttTilgode.Belop = ROUND(ttTilgode.Belop,0).       
    END.
    
    /* Posterer avrunding. */
    IF lPosdiff - lNegDiff <> 0 THEN 
    DO:
      CREATE ttBilagslinjer.
      ASSIGN 
        ttBilagslinjer.BokforingsId = pBokforingsId
        ttBilagslinjer.AvdelingsNr  = iAvdelingsNr
        ttBilagslinjer.Dato         = pdDato 
        ttBilagslinjer.ButNamn      = cButNamn
        ttBilagslinjer.LinjeNr      = piLinjeNr + 1
        ttBilagslinjer.Tekst        = 'Øresavrunding'
        ttBilagslinjer.KontoNr      = 2750
        ttBilagslinjer.Belop        = DEC(lPosdiff - lNegDiff)
        .
    END. 

    IF bTest THEN
    DO:
/*      TEMP-TABLE ttButiker:WRITE-JSON('file', cKatalog + 'ttButiker.JSon', TRUE).                        */
/*      TEMP-TABLE ttBokforingsbilag:WRITE-JSON('file', cKatalog + 'ttBokforingsbilag.JSon', TRUE).        */
/*      TEMP-TABLE ttBilagslinjer:WRITE-JSON('file', cKatalog + 'ttBilagslinjer.JSon', TRUE).              */
/*      TEMP-TABLE ttBokforingsKorrBilag:WRITE-JSON('file', cKatalog + 'ttBokforingsKorrBilag.JSon', TRUE).*/
/*      TEMP-TABLE ttTilgode:WRITE-JSON('file', cKatalog + 'ttTilgode.JSon', TRUE).                        */
    END.

    bResult = TRUE.
    RETURN bResult.
  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Ser til at filkatalogene er opprettet. 
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL prepKatalog(  ):   
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    ASSIGN 
      bResult = FALSE 
      .
    /* Oppretter kataloger hvis de mangler. */
    IF rStandardFunksjoner:prepKatalog(cDagsKatalog,cBku) THEN bResult = TRUE.
    IF rStandardFunksjoner:prepKatalog(cFakturaKatalog,cBku) THEN bResult = TRUE.

    RETURN bResult.
  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL settParametre(  ):
    DEFINE VARIABLE pbOk AS LOG NO-UNDO.
    {syspara.i 50 55 1 iAktiv INT}
    {syspara.i 50 55 2 cFilNavn}
    {syspara.i 50 55 4 iEksportType INT}
    {syspara.i 22 5 2 cOutletLst}
    
    ASSIGN
      bTest     = TRUE 
      cFilNavn  = 'DagsoppgjTTDDMMYYYY_BBB.json'
/*      cFilNavn  = REPLACE(cFilNavn,'DDMMYYYY', REPLACE(STRING(TODAY,'99/99/9999'),'/',''))*/
/*      cFilNavn  = REPLACE(cFilNavn,'HHMMSS', REPLACE(STRING(TIME,'HH:MM:SS'),':',''))     */
/*      cFilNavn  = REPLACE(cFilNavn,'DDMMYYYY', REPLACE(STRING(TODAY,'99/99/9999'),'/',''))*/
      cPilotLst = ''
      iAntDager = 400
      pbOk      = TRUE
      /* TEST TEST */    
/*      cPilotLst = '11' /* TEST */*/
/*      iAntDager = 5              */
      /* TEST TEST */    
      .

    IF bTest THEN
    DO: 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Parametre:' 
          ).
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    iAktiv.........:' + STRING(iAktiv) 
          ).
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    cDagsoppgjFil..:' + cFilNavn 
          ).
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    cPilotLst......:' + cPilotLst 
          ).
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    cOutletLst.....:' + cOutletLst 
          ).
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    iEksportType...:' + STRING(iEksportType) 
          ).
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    iAntDager......:' + STRING(iAntDager) 
          ).
    END.

    IF iAktiv = 1 AND pbOk THEN 
      pbOk = TRUE.
    ELSE DO: 
      pbOk = FALSE.
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  **Ikke aktiv eller feil ved lasting av parametre.' 
            ).
    END.

    RETURN pbOk.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL eksporterDagsrapp( INPUT piType AS INTEGER, piButNr AS INTEGER ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cUKatLst AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pcFilNavn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pcEksportFil AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pcEksBkuportFil AS CHARACTER NO-UNDO.

    {syspar2.i 50 55 2 cKatalog}
    
    ASSIGN 
      pcFilNavn = cFilNavn
      cUKatLst  = 'GR,OU'
      cKatalog  = RIGHT-TRIM(cKatalog,'\') + '\' + ENTRY(piType,cUKatLst) + '\'
      .
    FIND FIRST ImpKonv NO-LOCK WHERE 
      ImpKonv.EDB-System = 'Gant Global' AND 
      ImpKonv.Tabell     = 'Regnskapsavd' AND 
      ImpKonv.InterntID = STRING(piButNr) NO-ERROR.
    IF AVAILABLE ImpKonv THEN
      ASSIGN 
        pcFilNavn = REPLACE(
                            pcFilNavn,'BBB', ENTRY(1,ImpKonv.EksterntId)
                           )
        .
    ELSE 
      ASSIGN 
        pcFilNavn = REPLACE(pcFilNavn,'BBB', REPLACE(STRING(piButNr),':',''))
        .
    
    FIND FIRST ttBokforingsbilag NO-ERROR.
    IF AVAILABLE ttBokforingsbilag THEN 
      pcFilNavn  = REPLACE(pcFilNavn,'DDMMYYYY', REPLACE(STRING(ttBokforingsbilag.Dato,'99/99/9999'),'/','')).
    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    cDagsKatalog...:' + cKatalog 
        ).
    OS-CREATE-DIR VALUE(RIGHT-TRIM(cKatalog,'\')).    
    OS-CREATE-DIR VALUE(RIGHT-TRIM(cKatalog,'\') + '\bku').
        
    IF CAN-FIND(FIRST ttBilagslinjer) THEN
    DO: 
      /* Tar bort tomme ttButiker record. */
      FOR EACH ttButiker WHERE 
        NOT CAN-FIND(FIRST ttBilagslinjer WHERE 
                     ttBilagslinjer.AvdelingsNr = ttButiker.AvdelingsNr):
        DELETE ttButiker.
      END.
      
      /* Renser bort ttBilagslinjer som ikke skal eksporteres. */
      FOR EACH ttBilagslinjer:
        /* Tart bort overskriftslinjer i ttBilagslinjer. */
        IF ttBilagslinjer.Tekst BEGINS '.' THEN 
          DELETE ttBilagslinjer.
        /* Ingen verdi og skal ikke eksporteres. */
        ELSE IF (/*ttBilagslinjer.KontoNr > 0 AND*/ DEC(ttBilagslinjer.Belop) = 0) THEN 
          DELETE ttBilagslinjer. 
        /* Tart infolinjer i ttBilagslinjer. */
        ELSE IF (/*ttBilagslinjer.Tekst MATCHES '*Opptalt*' OR
                 ttBilagslinjer.Tekst MATCHES '*Veksel*' OR
                 ttBilagslinjer.Tekst MATCHES '*Pose*' OR*/
                 ttBilagslinjer.Tekst MATCHES '*kontantstrøm*') THEN 
          DELETE ttBilagslinjer.
      END.
      
      IF pitype = 1 THEN
        ASSIGN 
          pcEksportFil    = cKatalog + REPLACE(pcfilNavn,'TT','GR')
          pcEksBkuportFil = cKatalog + '\bku\'+ REPLACE(pcfilNavn,'TT','GR')
          .
      ELSE IF pitype = 2 THEN
        ASSIGN  
          pcEksportFil    = cKatalog + REPLACE(pcfilNavn,'TT','OU')
          pcEksBkuportFil = cKatalog + '\bku\'+ REPLACE(pcfilNavn,'TT','OU')
          .
      /* Utlegg dagsoppgjør. */
      DATASET dsDagsoppgjor:WRITE-JSON('file', pcEksportFil, TRUE,"UTF-8").
      /* Utlegg backup av dagsoppgjør. */
      DATASET dsDagsoppgjor:WRITE-JSON('file', pcEksBkuportFil, TRUE,"UTF-8").
          
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    Dagsoppgjør eksportert OK for butikk ' + STRING(piButNr) + '.'
        ).
    END.
    ELSE    
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    Ingen data å eksportere for butikk ' + STRING(piButNr) + '.' 
        ).

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC INTEGER hentAvdelingsnr( INPUT piButNr AS INTEGER ):
    
    DEFINE VARIABLE piReturn AS INTEGER NO-UNDO.

    FIND FIRST ImpKonv NO-LOCK WHERE 
      ImpKonv.EDB-System = 'Gant Global' AND 
      ImpKonv.Tabell     = 'Regnskapsavd' AND 
      ImpKonv.InterntID = STRING(piButNr) NO-ERROR.
    IF NOT AVAILABLE ImpKonv THEN
      piReturn = 0. 
    ELSE 
      piReturn = INT(ENTRY(1,ImpKonv.eksterntId)) NO-ERROR.
    
    RETURN piReturn.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID emptyTempFile(  ):

    EMPTY TEMP-TABLE ttButiker.
    EMPTY TEMP-TABLE ttBokforingsbilag.
    EMPTY TEMP-TABLE ttBilagslinjer.
    EMPTY TEMP-TABLE ttBokforingsKorrBilag.
    EMPTY TEMP-TABLE ttTilgode.
    
    RETURN.

  END METHOD.

  DESTRUCTOR PUBLIC EksportNavision ( ):

    emptyTempFile( ).

  END DESTRUCTOR.

END CLASS.