 
 /*------------------------------------------------------------------------
    File        : FakturaEksportNavision
    Purpose     : Automatisere innlesningen av disse fakturaene i Navision.
    Syntax      : 
    Description : Eksport av faktura som er et resultat av overføringer til overskuddslager (20), eller til Gant norge (1).
    Author(s)   : tomn
    Created     : Mon Mar 02 15:54:50 CET 2020
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS cls.Navision.FakturaEksportNavision: 
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLogg       AS CHARACTER NO-UNDO.
/*  DEFINE VARIABLE cRecordMal  AS CHARACTER NO-UNDO.*/
  DEFINE VARIABLE cOrgFilNavn AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFilNavn    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cKatalog    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ctmpFilNavn AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iDager      AS INTEGER NO-UNDO.
  DEFINE VARIABLE bTest       AS LOG NO-UNDO.
  DEFINE VARIABLE iAktiv AS INTEGER NO-UNDO.
  DEFINE VARIABLE iGantAktiv AS INTEGER NO-UNDO.
  DEFINE VARIABLE cButLstTillat AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cButLstKommisjon AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iOverskLager AS INTEGER NO-UNDO.
  DEFINE VARIABLE cOutletListe AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iFaktType AS INTEGER NO-UNDO.
  DEFINE VARIABLE cEksterneLev AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cButLst AS CHARACTER NO-UNDO.

  DEFINE BUFFER bFakturaHode FOR FakturaHode.  
  DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
    
  DEFINE STREAM Ut.
    
  DEFINE TEMP-TABLE ttFakturaHode LIKE FakturaHode
    FIELD TilButikkNr AS INTEGER
    FIELD LevNrLst AS CHARACTER
    FIELD FakturaType AS INTEGER 
    FIELD fRowId AS ROWID 
    .
    
  DEFINE TEMP-TABLE ttRecord 
    FIELD cCol01 AS CHARACTER
    FIELD cCol02 AS CHARACTER
    FIELD cCol03 AS CHARACTER
    FIELD cCol04 AS CHARACTER
    FIELD cCol05 AS CHARACTER
    FIELD cCol06 AS CHARACTER
    FIELD cCol07 AS CHARACTER
    FIELD cCol08 AS CHARACTER
    FIELD cCol09 AS CHARACTER
    FIELD cCol10 AS CHARACTER
    FIELD cCol11 AS CHARACTER
    FIELD cCol12 AS CHARACTER
    FIELD cCol13 AS CHARACTER
    FIELD cCol14 AS CHARACTER
    .      
    
  CONSTRUCTOR PUBLIC FakturaEksportNavision ( INPUT pcLogg AS CHARACTER ):
    SUPER ().

    rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ).

    {syspara.i 50 55 11 cOrgFilNavn}
    {syspar2.i 50 55 11 cKatalog}
    {syspara.i 50 55 10 iAktiv INT}
    {syspar2.i 50 55 10 iDager INT}
    {syspara.i 50 55 14 cButLstTillat}
    {syspara.i 50 55 15 cEksterneLev} /*'99,501,502,504'*/
    {syspara.i 22 20 1 iOverskLager INT}
    {syspara.i 22  5  2 cOutletListe}
    {syspara.i 210 100 8 iGantAktiv INT}

    /* Henter liste med kommisjonsbutikker. */    
    rStandardFunksjoner:getKommisjonsButLst(OUTPUT cButLstKommisjon).    
        
    ASSIGN 
      bTest       = TRUE
      iDager      = IF iDager = 0 THEN 10 ELSE iDager /* Antall dager som det skal sjekkes bakover i tid. */
      cLogg       = pcLogg
      cLogg       = IF cLogg = '' THEN 'FakturaEksportNavision' + REPLACE(STRING(TODAY),'/','') ELSE cLogg
/*      cRecordMal  = '"&Dato","&KrNota","&Kunde","&Sum","&FinansKto","&Netto","&MvaKto","&MvaBelop","&RabattKto","&RabattBelop","&Lager1","&Lager2","&Lager3","&Lager4"'*/
      cOrgFilNavn = IF cOrgFilNavn = '' THEN 'CREDIT&SeqNr.txt' ELSE cOrgFilNavn 
      cKatalog    = IF cKatalog = '' THEN '\\gant0047\Felles\GantGlobal\Faktura' ELSE cKatalog
/*      cKatalog    = 'konv' /* For test. Har ikke skriverettighet. */*/
      .

    ASSIGN 
      ctmpFilNavn = rStandardFunksjoner:getTempFileName(  )
      .
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Start Construct FakturaEksportNavision.cls.' 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Parametre:' 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    cOrgFilNavn: ' + cOrgFilNavn  
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    cKatalog: ' + cKatalog  
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    tmpFilNavn: ' + ctmpFilNavn  
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    iAktiv: ' + STRING(iAktiv)  
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    iDager: ' + STRING(iDager)  
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    cButLstTillat: ' + cButLstTillat  
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    cButLstKommisjon: ' + cButLstKommisjon  
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    iOverskLager: ' + STRING(iOverskLager)  
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    cOutletListe: ' + cOutletListe  
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Slutt Construct FakturaEksportNavision.cls.' 
        ).    

  END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL eksporterFakturaData(  ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Start (eksporterFakturaData).' 
        ).    

    /* Er faktura eksporten aktivert? */
    IF iAktiv <> 1 THEN 
    DO:
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Fakturaeksport til Navision er ikke aktivert (Syspara 50 55 10).' 
          ).    
      RETURN bResult.
    END.

    /* Skjer det avbrudd under eksporten, skal allt nullstilles. */
    EKSPORTTRANSBLOKK:
    DO TRANSACTION:
      /* HENTER FAKTURADATA                                       */
      /* Bygger tempfile for faktura datasett for eksport.        */
      /* Her hentes begge typer av faktura.                       */
      /*   Credit - Kreditnota for overføring til butikk 1 og 20. */
      /*   Eksterne - Faktura for varemottak fra eksterne lev.    */
      bResult = getFakturaHode (iDager, OUTPUT TABLE ttFakturaHode BY-REFERENCE).
     
      /* Ingen data - da avslutter vi */
      IF bResult = FALSE OR 
        NOT CAN-FIND(FIRST ttFakturaHode) THEN
      DO: 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Ingen data funnet å eksportere (1).' 
            ).    
        LEAVE EKSPORTTRANSBLOKK.
      END.

      /* 1=Gant, 2= Eksterne leverandører, 3-Kommisjon, 4-Overføringer. */
      LOOPEN:
      DO iFaktType = 1 TO 4:
        IF iFaktType = 1 THEN 
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  KREDITNOTA mot sentrallager (kjorFilUtlegg).' 
              ).    
        ELSE IF iFaktType = 2 THEN
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  EKSTERNE Leverandør faktura (kjorFilUtlegg).' 
              ).    
        ELSE IF iFaktType = 3 THEN
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  KOMMISJONS faktura (kjorFilUtlegg).' 
              ).    
        ELSE IF iFaktType = 4 THEN
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  OVERFØRINGS faktura Vareflyt (kjorFilUtlegg).' 
              ).    
              
        /* BYGGER DATASETT              */
        /* Bygger datasett for eksport i tabell ttRecord. */
        bResult = byggDatasettForEksport ( INPUT-OUTPUT TABLE ttFakturaHode BY-REFERENCE, INPUT iFaktType, OUTPUT TABLE ttRecord ).
    
        /* Ingen data - da avslutter vi */
        IF bResult = FALSE OR NOT CAN-FIND(FIRST ttRecord) THEN
        DO: 
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '    Ingen data funnet for fakturatype(' + STRING(iFaktType) + ').' 
              ).    
          NEXT LOOPEN.
        END.
        ELSE DO: 
          /* FILUTLEGG */
          bResult = kjorFilUtlegg ( INPUT TABLE ttRecord, iFaktType ).
          /* Feil ved filutlegg */
          IF bResult = FALSE  THEN
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    Feil ved filutlegg for fakturatype(' + STRING(iFaktType) + ').'
                ).    
        END.
      END. /* LOOPEN */
    END. /* EKSPORTTRANSBLOKK*/ 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Slutt (eksporterFakturaData).' 
        ).    

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID getSysPara( OUTPUT piSeqNr AS INTEGER ):
    
    DEFINE BUFFER bSysPara FOR SysPara.
      
    DO FOR bSysPara:
      FIND bSysPara NO-LOCK WHERE
          bSysPara.SysHId = 50 AND
          bSysPara.SysGr  = 55 AND
          bSysPara.ParaNr = 12 NO-ERROR.
      IF AVAILABLE bSysPara THEN
      DO: 
        ASSIGN piSeqNr = INT(bSysPara.Parameter1) NO-ERROR.
        /* Default verdi. */
        IF piSeqNr < 200 THEN 
          piSeqNr = 200.
        ELSE 
          piSeqNr = piSeqNr + 1.
        IF piSeqNr > 9999 THEN 
          piSeqNr = 200.
        RELEASE bSysPara.
      END.
    END. /* bSysPara */
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID setSysPara( INPUT piSeqNr AS INTEGER ):

    DEFINE BUFFER bSysPara FOR SysPara.
      
    DO FOR bSysPara TRANSACTION:
      FIND bSysPara EXCLUSIVE-LOCK WHERE
          bSysPara.SysHId = 50 AND
          bSysPara.SysGr  = 55 AND
          bSysPara.ParaNr = 12 NO-ERROR.
      IF AVAILABLE bSysPara THEN
      DO: 
        ASSIGN bSysPara.Parameter1 = STRING(piSeqNr)NO-ERROR.
        RELEASE bSysPara.
      END.
    END. /* bSysPara TRANSACTION*/
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL getFakturaHode( INPUT piDager AS INTEGER ,
    OUTPUT TABLE ttFakturaHode  ):
    
    DEFINE VARIABLE bResult           AS LOGICAL NO-UNDO.
    DEFINE VARIABLE pdForsteStartDato AS DATE NO-UNDO.
    DEFINE VARIABLE pdDato            AS DATE NO-UNDO.
    DEFINE VARIABLE piLoop            AS INTEGER NO-UNDO. 
    DEFINE VARIABLE pbFlagg AS LOG NO-UNDO.
    DEFINE VARIABLE pcLevNrLst AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cButNrLst AS CHARACTER NO-UNDO.

    /* Skal aldri ha med data før denne dato */
    ASSIGN 
      pdforsteStartDato = 01/01/2021
      pdDato            = TODAY - piDager
      pdDato            = IF pdDato < pdForsteStartDato THEN pdForsteStartDato ELSE pdDato
      cButNrLst         = cButLstTillat + ',' + cOutletListe + ',' + cButLstKommisjon 
      .
    /* TN 13/3-21 Legger nå også på øvrige butikker som fakturamottagere. */
    cButLst = ''.
    FOR EACH Butiker NO-LOCK WHERE 
      Butiker.harButikksystem = TRUE AND 
      Butiker.NedlagtDato = ?:
      
      /* Ikke ta med +/- butikkene. */
      IF iGantAktiv = 1 AND CAN-DO('848,849',STRING(Butiker.Butik)) THEN 
        NEXT.
        
      IF NOT CAN-DO(cButNrLst,STRING(Butiker.Butik)) THEN 
        cButLst = cButLst + 
                  (IF cButLst <> '' THEN ',' ELSE '') + 
                  STRING(Butiker.Butik).  
    END.
    IF cButLst <> '' THEN 
      cButNrLst = cButNrLst + ',' + cButLst. 

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Start (getFakturaHode):' 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    Butikkliste: ' + cButNrLst 
        ).    

    IF bTest THEN
    DO: 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '      cButNrLst: ' + cButNrLst  
          ).
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '      pdforsteStartDato: ' + STRING(pdforsteStartDato)  
          ).
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '      pdDato: ' + STRING(pdDato)  
          ).
    END.
    
    EMPTY TEMP-TABLE ttFakturaHode.

    /* Fakturering av oveføringer fra butikkene til overskuddslageret og Gant Norge.    */
    /* Overføring til Gant Norge gjøres fra butikkene når de skal krediteres varer      */
    /* som er skadd o.l.                                                                */
    /* Overføring til overskuddslager gjør butikkene ved utgang av sesong på varer      */
    /* som ikke har blitt solgt. Disse skal senere videre til Outlet'ene.               */
    /* Her kommer også faktura for varer som butikkene har sendt til butikk 1.          */
    /* Varemottak fra eksterne leverandører.                                            */
    /*   Butikkene skal også faktureres for  varer de mottar fra eksterne leverandører. */
    FAKTURALOOP:
    FOR EACH bFakturaHode NO-LOCK WHERE 
      bFakturaHode.FakturertDato >= pdDato AND
      bFakturaHode.EksportertDato = ?:
        
      FIND kunde NO-LOCK WHERE 
        Kunde.KundeNr = bFakturaHode.KundeNr NO-ERROR.
      IF NOT AVAILABLE Kunde THEN 
        NEXT.
        
      /* Bare faktura utstedt til butikker i en av listene skal eksporteres. */
      IF NOT CAN-DO(cButNrLst,STRING(Kunde.butikkNr)) THEN 
        NEXT.

      ASSIGN 
        pcLevNrLst = ''.
        /* Initierer variabelen hvis det ikke er gjort før. */  
        cEksterneLev = (IF cEksterneLev = '' THEN '99,501,502,504' ELSE cEksterneLev).
        . 
      /* Outlet faktura skal bare med hvis de inneholder varer fra eksterne leverandører. */
      IF CAN-DO(cOutletListe,STRING(Kunde.ButikkNr)) THEN
        EKSTERNLEV: 
        DO:
          /* Lister opp de leverandørene som har levert varene på fakturaen. */
          FOR EACH FakturaLinje OF bFakturaHode NO-LOCK, 
            FIRST ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = FakturaLinje.ArtikkelNr:

            IF NOT CAN-DO(pcLevNrLst,STRING(ArtBas.LevNr)) THEN 
              pcLevNrLst = pcLevNrLst + 
                           (IF pcLevNrLst = '' THEN '' ELSE ',') + 
                           STRING(ArtBas.LevNr).
          END. 
          
          /* Skipper faktura som ikke har varer fra eksterne leverandører. */
          IF pcLevNrLst <> '' THEN
          DO:
            LEVSJEKK: 
            DO piLoop = 1 TO NUM-ENTRIES(cEksterneLev):
              IF LOOKUP(ENTRY(piLoop,cEksterneLev),pcLevNrLst) > 0 THEN 
                LEAVE EKSTERNLEV. /* Denne skal vi ha med. */
            END. /* LEVSJEKK */
            NEXT FAKTURALOOP. /* Faktura skal ikke med. */
          END.
          ELSE 
            NEXT FAKTURALOOP. /* Faktura skal ikke med. */
        END. /* EKSTERNLEV */
       
      IF NOT CAN-FIND(FIRST ttFakturaHode WHERE 
                      ttFakturaHode.FakturaNr = bFakturaHode.FakturaNr) THEN 
      DO:
        CREATE ttFakturaHode.
        BUFFER-COPY bFakturaHode
          TO ttFakturaHode
          ASSIGN 
            ttFakturaHode.TilButikkNr = Kunde.ButikkNr
            ttFakturaHode.fROWID      = ROWID(FakturaHode)
            .
            
        IF CAN-DO(cButLstTillat,STRING(Kunde.butikkNr)) THEN
          ttFakturaHode.FakturaType = 1. 
        ELSE IF CAN-DO(cOutletListe,STRING(Kunde.butikkNr)) THEN 
          ttFakturaHode.FakturaType = 2. 
        ELSE IF CAN-DO(cButLstKommisjon,STRING(Kunde.butikkNr)) THEN 
          ttFakturaHode.FakturaType = 3. 
        ELSE IF CAN-DO(cButLst,STRING(Kunde.butikkNr)) THEN 
          ttFakturaHode.FakturaType = 4. 
          
        ASSIGN 
          ttFakturaHode.LevNrLst = pcLevNrLst
          .
        
        IF bTest THEN         
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '    Faktura: ' + STRING(ttFakturaHode.FakturaNr) + 
              ' Fakturadato: ' + STRING(ttFakturaHode.FakturertDato) + 
              ' KundeNr: ' + STRING(ttFakturahode.KundeNr) + 
              ' Kundens butikknr: ' + STRING(ttFakturahode.TilButikkNr) + 
              ' Faktura type: ' + STRING(ttFakturaHode.FakturaType) 
              ).
      END.            
    END. /* FAKTURALOOP */
    
    IF CAN-FIND(FIRST ttFakturaHode) THEN 
    DO:
      bResult = TRUE.
      TEMP-TABLE ttFakturaHode:WRITE-JSON('file',
                                          'konv\ttFakturaHode' + '_' + STRING(TODAY,'99999999') + '.Json',
                                          TRUE).
    END.

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Slutt (getFakturaHode).' 
        ).    

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL kjorFilUtlegg( INPUT TABLE ttRecord, INPUT piFaktType AS INTEGER ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE piAntFakt AS INTEGER NO-UNDO.
    DEFINE VARIABLE cBkuFil AS CHARACTER NO-UNDO.
    DEFINE VARIABLE piSeqNr AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bFakturaHode FOR FakturaHode.
    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Start (kjorFilUtlegg):' 
        ).    
    
    IF NOT CAN-FIND(FIRST ttRecord) THEN
    DO: 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Slutt (kjorFilUtlegg): Ingen data å eksportere.' 
          ).    
      RETURN bResult.
    END.
      
    /* Henter neste ledige sekvensNr. */
    getSysPara(piSeqNr).
    /* Skriver det tilbake. */
    setSysPara(piSeqNr).
    
    ASSIGN
      cFilNavn = cOrgFilNavn
      .
      
    IF piFaktType = 1 THEN
      cFilNavn = REPLACE(cFilNavn,'&SeqNr',STRING(piSeqNr,"9999")).
       
    ELSE IF piFaktType = 2 THEN
      DO:
        cFilNavn = REPLACE(cFilNavn,'&SeqNr',STRING(piSeqNr,"9999")).
        cFilNavn = REPLACE(cFilNavn,'CREDIT','FAKTURAELEV').
      END.
    ELSE IF piFaktType = 3 THEN
      DO:
        cFilNavn = REPLACE(cFilNavn,'&SeqNr',STRING(piSeqNr,"9999")).
        cFilNavn = REPLACE(cFilNavn,'CREDIT','FAKTURAKOMM').
      END.
    ELSE DO:
        cFilNavn = REPLACE(cFilNavn,'&SeqNr',STRING(piSeqNr,"9999")).
        cFilNavn = REPLACE(cFilNavn,'CREDIT','FAKTURAVFLYT').
    END.

    ASSIGN 
      cFilNavn = RIGHT-TRIM(cKatalog,'\') + '\' + cFilNavn

      cBkuFil = cOrgFilNavn
      cBkuFil = REPLACE(cBkuFil,'&SeqNr',STRING(piSeqNr,"9999"))
      cBkuFil = RIGHT-TRIM(cKatalog,'\') + '\bku\' + cBkuFil
      .

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    Filutlegg: ' 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '      Sekvensnr: ' + STRING(piSeqNr) + '.' 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '      Fil      : ' + cFilNavn + '.' 
        ).    

    IF CAN-FIND(FIRST ttRecord) THEN 
    DO FOR bFakturaHode:    
      OUTPUT STREAM Ut TO VALUE(cFilNavn).
      FOR EACH ttRecord:
        
        piAntFakt = piAntFakt + 1.
        EXPORT STREAM Ut DELIMITER ','
          ttRecord.
          
        FIND FIRST bFakturaHode EXCLUSIVE-LOCK WHERE 
          bFakturaHode.FakturaNr = DECIMAL(ttRecord.cCol02) NO-ERROR.
        IF AVAILABLE bFakturaHode THEN 
          DO:
            ASSIGN 
              bFakturaHode.SendingsNr     = STRING(piSeqNr)
              bFakturaHode.EksportertDato = TODAY
              bFakturaHode.EksportertAv   = USERID('SkoTex')
              .
            RELEASE bFakturaHode.
          END.    
      END.
      OUTPUT STREAM Ut CLOSE.
      
      /* Logger eksport av filen. */
      loggFakturaEksport( INPUT piSeqNr, INPUT piFaktType, INPUT cFilNavn).
      
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    OS-Copy: ' + cFilNavn + ' ' + cBkuFil + '.' 
          ).    
      OS-COPY VALUE(cFilNavn) VALUE(cBkuFil).
    END.
    ELSE 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    Ingen data å eksportere (kjoprFilUtlegg).' 
          ).    

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    Filutlegg ferdig. Eksportert ' + STRINg(piAntFakt) + ' poster til fil.' 
        ).    
        
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Slutt (kjorFilUtlegg):' 
        ).    
    bResult = TRUE.
    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL byggDatasettForEksport( INPUT-OUTPUT TABLE ttFakturaHode, 
    INPUT piFaktType AS INTEGER, 
    OUTPUT TABLE ttRecord ):
        
    DEFINE VARIABLE bResult  AS LOGICAL NO-UNDO.
    DEFINE VARIABLE pcListe  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE plNetto AS DECIMAL NO-UNDO.

    EMPTY TEMP-TABLE ttRecord.
    
    /* Leser fakturaene i to omganger.                                                           */
    /* iLoop = 1. Første runde leser fakturene som er kreditfaktura internt.                     */
    /* iLoop = 2. Andre runde leser fakturaene som skyldes varemottak fra eksterne leverandører. */
    /*            Dette gjelder sko og klokker.                                                  */
    /* iLoop = 3. Kommisjonssalg.                                                                */
    /* iLoop = 4. Overføringer mellom butikkene.                                                 */
    FOR EACH ttFakturaHode WHERE 
      ttFakturaHode.FakturaType = piFaktType:
        
      /* Mapper om data */
      MAPPING:
      DO:
        IF NOT getMapping( INPUT (ttFakturaHode.ButikkNr),
                          (IF piFaktType = 1 THEN 'Regnskapsavd' ELSE 'VareflytAvdeling') , 
                          OUTPUT pcListe ) THEN 
        DO:
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '    **Butikken ligger ikke i mapping tabell (' + STRING(ttFakturaHode.TilButikkNr) + ' ' + pcListe + '). Eksport avbrutt.' 
              ).   
          NEXT.
        END.
        IF NUM-ENTRIES(pcListe) < 5 THEN
        DO: 
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '    Feil i mapping record (' + pcListe + '). Eksport avbrutt.' 
              ).    
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '      ButikkNr: ' + STRING(ttFakturaHode.ButikkNr) + '.' 
              ).    
          NEXT.
        END.
      END. /* MAPPING */  
        
      CREATE ttRecord.
      ASSIGN              /* Legger på avrunding på nettobeløpet */
        plNetto         = (ttFakturaHode.Totalt - ttFakturaHode.MvaKr) + (ttFakturaHode.Totalt - ((ttFakturaHode.Totalt - ttFakturaHode.MvaKr) + ttFakturaHode.MvaKr))  
        ttRecord.cCol01 = REPLACE(STRING(ttFakturaHode.FakturertDato,"99/99/99"),'/','')
        ttRecord.cCol02 = STRING(ttFakturaHode.FakturaNr)
        ttRecord.cCol03 = ENTRY(2,pcListe) /* KundeNr i Navision. */
        ttRecord.cCol04 = TRIM(REPLACE(STRING(ttFakturaHode.Totalt * (IF ttFakturaHode.FakturaType > 1 THEN 1 ELSE -1),"->>>>>>>>9.99"),',','.'))
        ttRecord.cCol05 = ENTRY(3,pcListe) /* Finans/salgskonto. */ 
        ttRecord.cCol06 = TRIM(REPLACE(STRING((plNetto),"->>>>>>>>9.99"),',','.'))
        ttRecord.cCol07 = ENTRY(4,pcListe) /* Mva konto. */
        ttRecord.cCol08 = TRIM(REPLACE(STRING((ttFakturaHode.MvaKr),"->>>>>>>>9.99"),',','.'))
        ttRecord.cCol09 = ENTRY(5,pcListe) /* Rabatt konto. */
        ttRecord.cCol10 = TRIM(REPLACE(STRING(ttFakturaHode.TotalRabattKr,"->>>>>>>>9.99"),',','.'))
        /*
        Avvente med dette. Ref. Are 4/3-20
        ttRecord.cCol11 = 
        ttRecord.cCol12 = 
        ttRecord.cCol13 = 
        ttRecord.cCol14 =
        */ 
        .
      IF bTest THEN         
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    byggDatasettForEksport: Faktura: ' + STRING(ttFakturaHode.FakturaNr) + ' ' + STRING(ttFakturaHode.FakturertDato) + '.' 
            ).             
    END.

    IF CAN-FIND(FIRST ttRecord) THEN 
      bResult = TRUE.
      
    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL getMapping( INPUT piButNr AS INTEGER, INPUT pcTabell AS CHARACTER, OUTPUT pcReturn AS CHARACTER ):
    
    DEFINE VARIABLE bResult      AS LOGICAL NO-UNDO.
    DEFINE VARIABLE pcEDB-System AS CHARACTER NO-UNDO.

    DEFINE BUFFER bImpKonv FOR ImpKonv.
    
    ASSIGN 
      pcEDB-System = 'Gant Global'
      .
      
    MAPPINGBLOKK:
    DO FOR bImpKonv:
      FIND FIRST bImpKonv NO-LOCK WHERE 
        bImpKonv.EDB-System = pcEDB-System AND 
        bImpKonv.Tabell     = pcTabell AND 
        bImpKonv.InterntId = TRIM(STRING(piButNr)) NO-ERROR.
      IF AVAILABLE bImpKonv THEN
      DO: 
        ASSIGN 
          pcReturn = bImpKonv.EksterntId
          bResult = TRUE
          .
      END.
    END. /* MAPPINGBLOKK */

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL loggFakturaEksport( INPUT piSeqNr AS INTEGER, 
    INPUT piFaktType AS INTEGER,
    INPUT pcFilNavn AS CHARACTER):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    DEFINE BUFFER bFakturaEksport FOR FakturaEksport.

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  loggFakturaEksport Eksportfil: ' + pcFilNavn
        ).    

    /* Logger eksporten */
    IF piSeqNr > 0 THEN 
    DO FOR bFakturaEksport TRANSACTION:
      FIND bFakturaEksport EXCLUSIVE-LOCK WHERE 
        bFakturaEksport.SendingsNr = piSeqNr NO-ERROR.
      /* Nummerserie har gått runt. */
      IF AVAILABLE bFakturaeksport THEN 
        DELETE bFakturaEksport.
      CREATE bFakturaEksport.
      ASSIGN
        bFakturaEksport.SendingsNr       = piSeqNr
        bFakturaEksport.Opphav           = piFaktType
        bFakturaEksport.EksporterDatoTid = NOW
        bFakturaEksport.EksportertAv     = USERID('SkoTex') 
        bFakturaEksport.Merknad          = 'Eksportfil: ' + pcFilNavn
        .
      RELEASE bFakturaEksport.
    END.

    RETURN bResult.

  END METHOD.

  DESTRUCTOR PUBLIC FakturaEksportNavision ( ):

    EMPTY TEMP-TABLE ttFakturaHode.
    EMPTY TEMP-TABLE ttRecord.
    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Destruct FakturaEksportNavision.cls.' 
        ).    

  END DESTRUCTOR.

END CLASS.