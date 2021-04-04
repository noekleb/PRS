
/*------------------------------------------------------------------------
    File        : sjekkEOD.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tomn
    Created     : Thu apr 23 
    Notes       : Sjekker at EOD er kommet inn for alle aktive kasser.
                  NB: Kassene skal være aktive, og de skal ha kas_rapp 
                  record opprettet. 
                   
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSendLst            AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE cLogg               AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE iX                  AS INTEGER                        NO-UNDO.
DEFINE VARIABLE iLoop               AS INTEGER                        NO-UNDO.
DEFINE VARIABLE ceMailLst           AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE bTest               AS LOG                            NO-UNDO.
DEFINE VARIABLE obOk AS LOG NO-UNDO.
DEFINE VARIABLE bSettEOD            AS LOG NO-UNDO.

DEFINE VARIABLE cFilNavn            AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rSendEMail          AS cls.SendEMail.SendEMail        NO-UNDO.
DEFINE VARIABLE cError              AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE iantDager           AS INTEGER NO-UNDO.
DEFINE VARIABLE cPilotLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBrukerId           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cErorTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLinjeNr            AS INTEGER NO-UNDO.
DEFINE VARIABLE cUnntaksLst         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iKommisjonAktiv     AS INTEGER NO-UNDO.
DEFINE VARIABLE pcKommisjonsIntervall AS CHARACTER NO-UNDO.

DEFINE STREAM Ut.

ON WRITE OF BokforingsKorrBilag OVERRIDE DO:
END.
ON WRITE OF BokforingsBilag OVERRIDE DO:
END.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
ASSIGN 
  cLogg     = 'sjekkEOD' + REPLACE(STRING(TODAY),'/','')
  cUnntaksLst = '4,5,8,15'
  .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rSendEMail  = NEW cls.SendEMail.SendEMail( ) NO-ERROR.

{syspara.i 50 50 54 ceMailLst}
{syspara.i 55 10 1 iKommisjonAktiv INT}
{syspara.i 5 40 20 pcKommisjonsIntervall}

ASSIGN
  cBrukerId = 'batch'
  bTest     = TRUE 
  cFilNavn  = REPLACE(rStandardFunksjoner:getTempFileName(),'.tmp','.txt')
  iAntDager = 140 /* 10 */
/*  cPilotLst = '6,11,40' TN 25/8-20 Åpner nå for alle butikker. */
  cPilotLst = ''
  .

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Start.' 
  ).

SJEKKBLOKK:
DO:
  ASSIGN 
    .
    
  RUN sjekkEODMottatt (OUTPUT obOk).
  
  IF obOk THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Varsel om manglende EOD er sendt.' 
      ).
    RUN sendVarselSjekkEOD. 
  END.
  ELSE     
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  EOD sjekk av EOD for kassene OK.' 
      ).

  LEAVE SJEKKBLOKK.
  
END. /* SJEKKBLOKK */  

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Slutt.' 
  ).

QUIT.

/* **********************  Internal Procedures  *********************** */

PROCEDURE sendVarselSjekkEOD:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  IF ceMailLst = '' THEN 
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Det er ikke satt opp mottager for eMail varsel av manglnde EOD (Syspara 50 50 54).' 
      ).
    RETURN.
  END.
    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Mailvarsel sendt: ' + STRING(NOW,"99/99/9999 HH:MM:SS") 
    ).

  rSendEMail:parToADDRESS       = ceMailLst.
  rSendEMail:parMailType        = 'PAKKSEDDEL'.
  rSendEMail:parSUBJECT         = (IF SEARCH('tnc.txt') <> ? THEN 'TEST ' ELSE '') +  
                                  "Sjekk av EOD mottak fra kassene" +  
    ' Dato/Tid: ' + STRING(NOW,"99/99/99 hh:mm:ss") + ".".    
  rSendEMail:parMessage-Charset = ''. /* Blank eller 'UTF-8' når det går fra fil. */
  rSendEMail:parFILE            = cFilNavn.  
  rSendEMail:parMESSAGE = "EODSjekk av mottak av EOD fra kassene. ".

  rSendEMail:send( ).

END PROCEDURE.

PROCEDURE sjekkEODMottatt:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER pbSettEOD AS LOG NO-UNDO.
  
  DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.

  OUTPUT STREAM Ut TO value(cFilNavn).

  PUT STREAM Ut UNFORMATTED   
    'Butikk' FORMAT "x(6)" ' ' 
    'Butikkens navn' FORMAT "x(50)" ' '
    'Kasse' FORMAT "x(6)" ' ' 
    'Dato' FORMAT "x(10)" ' '
    SKIP.
  PUT STREAM Ut  
    '------' ' '
    '--------------------------------------------------' ' '
    '------' ' '
    '----------' ' '
    SKIP.
    
  pbSettEOD = FALSE.
  BUTIKKLOOP:
  FOR EACH Butiker NO-LOCK WHERE
/*    Butiker.butik = 2 AND /* TEST */*/
    Butiker.ApningsDato <> ? AND 
    Butiker.harButikksystem = TRUE AND 
    (Butiker.NedlagtDato = ? OR 
     Butiker.NedlagtDato <= TODAY): 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Butikk: ' + STRING(Butiker.Butik) + ' ' + Butiker.ButNamn 
      ).

    /* Butikker som ikke skal ha EOD sjekk. */
    IF LOOKUP(STRING(Butiker.Butik),cUnntaksLst) > 0 THEN 
      NEXT.
      
    DATOLOOP:
    DO piLoop = 1 TO iAntDager:
      IF iKommisjonAktiv = 1 THEN 
        RUN EODKommisjon (piLoop).
      RUN EODKasseSjekk (piLoop, INPUT-OUTPUT pbSettEOD).
      RUN AutoGodkjenning (piLoop).
    END. /* DATOLOOP */
  END. /* BUTIKKLOOP */
  OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

PROCEDURE EODKasseSjekk:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piLoop AS INTEGER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER pbSettEOD AS LOG.
  
  /* Sjekker og logger manglende EOD meldinger fra kassene. */
  EODKASSESJEKK:
  FOR EACH Kasse NO-LOCK WHERE 
    Kasse.ButikkNr = Butiker.Butik AND 
    Kasse.Aktiv    = TRUE AND 
    Kasse.KasseNr  <= 90:

/*    rStandardFunksjoner:SkrivTilLogg(cLogg,                                     */
/*      '    Kasse: ' + STRING(Kasse.KasseNr) + ' Dato: ' + STRING(TODAY - piLoop)*/
/*      ).                                                                        */
      
    IF NOT CAN-FIND(EODKasse WHERE
      EODKasse.ButikkNr = Kasse.ButikkNr AND
      EODKasse.GruppeNr = Kasse.GruppeNr AND 
      EODKasse.KasseNr  = Kasse.KasseNr AND
      EODKasse.EODDato  = TODAY - piLoop) 
      THEN DO: 
        /* Kasserapporten har kommet inn. */
        IF CAN-FIND(FIRST Kas_Rap WHERE 
          kas_rap.dato   = TODAY - piLoop AND  
          kas_rap.butikk = Kasse.butikkNr AND
          kas_rap.Kasse  = Kasse.KasseNr) THEN  
          NEXT.
        /* Bokføringsbilag er opprettet */  
        IF CAN-FIND(Bokforingsbilag WHERE
          Bokforingsbilag.OmsetningsDato = TODAY - piLoop AND
          Bokforingsbilag.ButikkNr       = Kasse.ButikkNr) THEN 
          NEXT.
        
        PUT STREAM Ut UNFORMATTED
          STRING(Kasse.butikkNr) FORMAT "x(6)" ' ' 
          (IF AVAILABLE Butiker THEN Butiker.ButNamn ELSE '*Ukjent butikk') FORMAT "x(50)" ' '
          STRING(Kasse.KasseNr) FORMAT "x(6)" ' ' 
          STRING(TODAY - piLoop) FORMAT "x(10)" ' '
          SKIP.

/*        rStandardFunksjoner:SkrivTilLogg(cLogg,*/
/*          '      EOD Mangler.'                 */
/*          ).                                   */

        pbSettEOD = TRUE.
      END.
  END. /* EODKASSESJEKK */

END PROCEDURE.

PROCEDURE AutoGodkjenning:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piLoop AS INTEGER NO-UNDO.

  DEFINE VARIABLE pcFilNavn AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pdSum AS DECIMAL NO-UNDO.
  DEFINE VARIABLE piKontoNr AS INTEGER NO-UNDO.
  
  DEFINE BUFFER bufBokforingsbilag FOR bokforingsBilag.

  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  AutoGodkjenning (' + STRING(TODAY - piLoop) + ').' 
    ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    piLoop: ' + STRING(piLoop) + '.' 
    ).
  
  /* Kjører automatisk godkjenning av oppgjør hvor EOD er mottatt og diff = 0. */
  AUTOGODKJENN:
  FOR EACH Bokforingsbilag NO-LOCK WHERE
    BokforingsBilag.ButikkNr       = Butiker.butik AND
    BokforingsBilag.OmsetningsDato = TODAY - (piLoop) AND 
    BokforingsBilag.EODMottatt     = TRUE AND 
    BokforingsBilag.GodkjentFlagg  = FALSE:  

   /* NB: Foreløpig kun pilot test. Butikkene 6, 11 og 40 */
    IF cPilotLst <> '' THEN 
    DO: 
      IF NOT CAN-DO(cPilotLst,STRING(BokforingsBilag.ButikkNr)) THEN 
      NEXT.
    END.

    /* Her genereres bare fil. Ingen utskrift på skriver. */
    IF CAN-FIND(FIRST kas_rap WHERE 
                kas_rap.dato = BokforingsBilag.OmsetningsDato AND 
                kas_rap.butikk = butiker.butik) THEN 
        RUN dagsrapp_utskrift.p ('99',BokforingsBilag.ButikkNr,BokforingsBilag.OmsetningsDato,BokforingsBilag.OmsetningsDato,TRUE,OUTPUT pcFilNavn) NO-ERROR.

        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    Kasserapport fil: ' + (IF pcFilNavn <> ? THEN pcFilNavn ELSE '') + '.' 
          ).

    /* Vellykket generering av utskrift og derigjennom generering av BokforingsVisning. */    
    IF SEARCH(pcFilNavn) <> '' THEN 
    DO:
      FIND LAST BokforingsVisning OF Bokforingsbilag NO-LOCK WHERE 
        BokforingsVisning.Tekst BEGINS 'Kasse diff' NO-ERROR.

      /* DIFFERANSESJEKK
         - Kommisjonsbutikker skal få nullet diffen.
         - Andre butikker godkjennes hvis diff < 1. 
      */
      IF AVAILABLE BokforingsVisning AND 
        ABS(DEC(BokforingsVisning.Belop)) < (IF iKommisjonAktiv = 1 THEN 999999999 ELSE 1) THEN
      DIFF_LIK_NULL_GODKJENNER:
      DO FOR bufBokforingsbilag TRANSACTION:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    DIFF_LIK_NULL_GODKJENNER: Butikk: ' + STRING(BokforingsBilag.ButikkNr) + ' BokføringsId: ' + STRING(BokforingsBilag.BokforingsID) + ' Dato: ' + STRING(BokforingsBilag.OmsetningsDato)   
          ).

        FIND bufBokforingsBilag EXCLUSIVE-LOCK WHERE 
          ROWID(bufBokForingsBilag) = ROWID(BokforingsBilag) NO-ERROR NO-WAIT.
          IF AVAILABLE bufBokForingsBilag AND NOT LOCKED bufbokforingsBilag THEN 
          DO:
          /* Automatisk avrunding og nulling av diff hvis denne er mindre enn 1 kr. */
          IF DEC(BokforingsVisning.Belop) <> 0 THEN
          AVRUNDOPPGJOR: 
          DO:
            RUN setSieKontoNr.p (BokforingsBilag.ButikkNr, 1, 1, OUTPUT piKontoNr). 
            FIND LAST BokforingsKorrBilag OF BokforingsBilag NO-LOCK USE-INDEX idxBokforingsKorrbilag NO-ERROR.
            IF AVAILABLE BokforingsKorrBilag THEN 
              iLinjeNr = BokforingsKorrBilag.LinjeNr + 1.
            ELSE 
              iLinjeNr = 1. 
            CREATE BokforingsKorrBilag.
            ASSIGN 
              BokforingsKorrBilag.BokforingsID = bufBokforingsBilag.BokforingsId
              BokforingsKorrBilag.LinjeNr      = iLinjeNr
              BokforingsKorrBilag.TTId         = 1
              BokforingsKorrBilag.TBId         = 1
              BokforingsKorrBilag.Belop        = DEC(BokforingsVisning.Belop) 
              BokforingsKorrBilag.Merknad      = 'Automatisk øresavrunding'
              BokforingsKorrBilag.KontoNr      = piKontoNr
              BokforingsKorrBilag.BrukerId     = cBrukerId
              BokforingsKorrBilag.EBrukerId    = cBrukerId
              NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
            DO:
              DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:  
                    cErorTekst = cErorTekst + (IF cErorTekst <> '' THEN CHR(10) ELSE '') + ERROR-STATUS:GET-MESSAGE(ix).     
              END.  
            END.
            /* Overstyrer dato og tid. Setter inn bokføringsbilagets dato. Tid ligger ikke på bokføringsbilaget. */
            ASSIGN 
              BokforingsKorrBilag.DatoTid = DATETIME(STRING(BokforingsBilag.OmsetningsDato,"99/99/9999") + ' 23:50:59')
              NO-ERROR.
              
              /* Ligger butikken utenfor kommisjonsbutikk intervallet. */
            IF NUM-ENTRIES(pcKommisjonsIntervall,'-') = 2 AND  
              iKommisjonAktiv = 1 AND 
              Butiker.Butik >= INT(ENTRY(1,pcKommisjonsIntervall,'-')) AND 
              Butiker.Butik <= INT(ENTRY(2,pcKommisjonsIntervall,'-')) THEN
            DO:
              ASSIGN  
                BokforingsKorrBilag.TTId    = 900
                BokforingsKorrBilag.TBId    = 29
                BokforingsKorrBilag.Merknad = 'Automatisk opptalt'
                BokforingsKorrBilag.KontoNr = 0
                BokforingsKorrBilag.Belop   = BokforingsKorrBilag.Belop * -1
                .
            END.
              
            rStandardFunksjoner:SkrivTilLogg(cLogg,
              '    Avrunding: ' + STRING(Butiker.butik) + ' ' + Butiker.butNamn + ' Dato: ' + STRING(TODAY - piLoop) + ' Beløp: ' + BokforingsVisning.Belop 
              ).
          END. /* AVRUNDOPPGJOR */

          ASSIGN
            bufBokforingsBilag.GodkjentDato  = TODAY
            bufBokforingsBilag.GodkjentTid   = TIME
            bufBokforingsBilag.GodkjentAv    = cBrukerId 
            bufBokforingsBilag.GodkjentFlagg = TRUE
            .
           
          rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Godkjent dagsoppgjør for butikk: ' + STRING(Butiker.butik) + ' ' + Butiker.butNamn + ' ' + STRING(BokforingsBilag.OmsetningsDato) 
            ).
            
          FIND LAST BokforingsKorrBilag OF BokforingsBilag NO-LOCK USE-INDEX idxBokforingsKorrbilag NO-ERROR.
          IF AVAILABLE BokforingsKorrBilag THEN 
            iLinjeNr = BokforingsKorrBilag.LinjeNr + 1.
          ELSE 
            iLinjeNr = 1. 
          CREATE BokforingsKorrBilag.
          ASSIGN 
            BokforingsKorrBilag.BokforingsID = bufBokforingsBilag.BokforingsId
            BokforingsKorrBilag.LinjeNr      = iLinjeNr
            BokforingsKorrBilag.TTId         = 144
            BokforingsKorrBilag.TBId         = 1
            BokforingsKorrBilag.Belop        = 0
            BokforingsKorrBilag.Merknad      = 'Automatisk godkjent'
            BokforingsKorrBilag.BrukerId     = cBrukerId
            BokforingsKorrBilag.EBrukerId    = cBrukerId
            NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
            DO:
              DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:  
                    cErorTekst = cErorTekst + (IF cErorTekst <> '' THEN CHR(10) ELSE '') + ERROR-STATUS:GET-MESSAGE(ix).     
              END.  
              rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  cErorTekst: ' + cErorTekst 
                ).
            END.
            /* Overstyrer dato og tid. Setter inn bokføringsbilagets dato. Tid ligger ikke på bokføringsbilaget. */
            ASSIGN 
              BokforingsKorrBilag.DatoTid = DATETIME(STRING(BokforingsBilag.OmsetningsDato,"99/99/9999") + ' 23:50:59')
              NO-ERROR.

          /* Sjekker om oppgjøret er tomt - ingen omsetning. */
          pdSum = 0.
          FOR EACH Kas_Rap NO-LOCK WHERE 
            kas_rap.butikk = BokforingsBilag.ButikkNr AND 
            kas_rap.dato = BokforingsBilag.OmsetningsDato:
            pdSum = pdSum + kas_rap.MvaGrunnlag[2].  
          END.    
        
          /* Automatisk utskrift av dagsoppgjøret. */
          IF pdSum <> 0 THEN 
            RUN dagsrapp_utskrift.p ('99',BokforingsBilag.ButikkNr,BokforingsBilag.OmsetningsDato,BokforingsBilag.OmsetningsDato,FALSE,OUTPUT pcFilNavn) NO-ERROR.
        
          RELEASE BokforingsKorrBilag.  
          IF AVAILABLE bufBokforingsBilag THEN 
            RELEASE bufBokforingsBilag.
        END.        
      END. /* DIFF_LIK_NULL_GODKJENNER */
      ELSE DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    DIFF_LIK_NULL_GODKJENNER: IKKE GODKJENNT! Butikk: ' + STRING(BokforingsBilag.ButikkNr) + ' BokføringsId: ' + STRING(BokforingsKorrBilag.BokforingsID) + ' Dato: ' + STRING(BokforingsBilag.OmsetningsDato)   
          ).        
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '      Available bokforingsvisning: ' + STRING(AVAILABLE BokforingsVisning) 
          ).
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '      Diff: ' + IF AVAILABLE BokforingsVisning THEN  STRING(BokforingsVisning.Belop) ELSE ''
          ).
      END.
      
    END.
  END. /* AUTOGODKJENN */
  
END PROCEDURE.

PROCEDURE EODKommisjon:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piLoop AS INTEGER NO-UNDO.
  
  DEFINE BUFFER bBokforingsBilag FOR BokforingsBilag.
  
  IF pcKommisjonsIntervall = '' OR NUM-ENTRIES(pcKommisjonsIntervall,'-') <> 2 THEN 
    RETURN.
    
  /* Ligger butikken utenfor kommisjonsbutikk intervallet. */
  IF Butiker.Butik < INT(ENTRY(1,pcKommisjonsIntervall,'-')) OR
    Butiker.Butik > INT(ENTRY(2,pcKommisjonsIntervall,'-')) THEN 
    RETURN.
  
  /* Sjekker og logger manglende EOD meldinger fra kassene. */
  EODKASSESJEKK:
  FOR EACH Kasse NO-LOCK WHERE 
    Kasse.ButikkNr = Butiker.Butik AND 
    Kasse.Aktiv    = TRUE AND 
    Kasse.KasseNr  <= 90:
  
    IF NOT CAN-FIND(EODKasse WHERE
      EODKasse.ButikkNr = Kasse.ButikkNr AND
      EODKasse.GruppeNr = Kasse.GruppeNr AND 
      EODKasse.KasseNr  = Kasse.KasseNr AND
      EODKasse.EODDato  = TODAY - piLoop) 
      THEN 
    OPPSTANDELSEN:
    DO: 
      IF NOT CAN-FIND(FIRST BokforingsBilag WHERE 
        BokforingsBilag.OmsetningsDato = TODAY - piLoop AND 
        BokforingsBilag.butikk = Kasse.ButikkNr) THEN 
        LEAVE OPPSTANDELSEN.
      ELSE 
      DO FOR bBokforingsBilag:
        FIND FIRST bBokforingsBilag EXCLUSIVE-LOCK WHERE 
          bBokforingsBilag.OmsetningsDato = TODAY - piLoop AND 
          bBokforingsBilag.butikk = Kasse.ButikkNr NO-ERROR. 
        IF AVAILABLE bBokforingsBilag THEN
        DO: 
          bBokforingsBilag.EODMottatt = TRUE.
          RELEASE bBokforingsBilag.
        END.
        CREATE EODKasse.
        ASSIGN 
          EODKasse.ButikkNr = Kasse.ButikkNr 
          EODKasse.GruppeNr = Kasse.GruppeNr  
          EODKasse.KasseNr  = Kasse.KasseNr 
          EODKasse.EODDato  = TODAY - piLoop 
          .
      END.
    END. /* OPPSTANDELSEN*/
  END. /* EODKASSESJEKK */


END PROCEDURE.

