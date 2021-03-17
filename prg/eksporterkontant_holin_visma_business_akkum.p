&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    
&ELSE
    
&ENDIF

DEFINE INPUT  PARAMETER dinpFraDato AS DATE NO-UNDO.
DEFINE INPUT  PARAMETER dinpTilDato AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER ocRetur     AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  iAntEksport AS INTEGER    NO-UNDO.

DEF VAR iAntLinjer   AS INT        NO-UNDO.
DEF VAR iAlle        AS INT        NO-UNDO.
DEF VAR bStream      AS LOG        NO-UNDO.
DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
DEFINE VARIABLE bManuell AS LOG NO-UNDO.
DEFINE VARIABLE dFraDato    AS DATE NO-UNDO.
DEFINE VARIABLE dTilDato    AS DATE NO-UNDO.
DEFINE VARIABLE pdLoopDato  AS DATE NO-UNDO.

/* Filhåndtering */
DEF VAR cFilNavn   AS CHAR FORMAT "x(40)"     NO-UNDO.
DEF VAR cKatalog   AS CHAR                    NO-UNDO.
DEF VAR cPrefix    AS CHAR                    NO-UNDO.
DEF VAR cEkstent   AS CHAR                    NO-UNDO.
DEF VAR iSekvens   AS INT  FORMAT ">>>>>>>9"  NO-UNDO.
DEF VAR cEDBSystem AS CHAR INITIAL "EkspKOAU" NO-UNDO.

DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst   AS CHARACTER NO-UNDO.
DEFINE VARIABLE bStreamApen  AS LOG NO-UNDO.

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_ELogg NO-UNDO LIKE ELogg
  FIELD B_Id AS DECIMAL FORMAT '>>>>>>>>>>>>9'
  FIELD ButikkNr AS INTEGER FORMAT '>>>>>9'
  INDEX EksportIdx ButikkNr B_Id.

DEFINE TEMP-TABLE ttMva NO-UNDO 
  FIELD MomsKod LIKE Moms.MomsKod
  FIELD Belop   AS DECIMAL FORMAT '->>>>>>>9.99'
  FIELD ButikkNr AS INTEGER FORMAT '>>>>>9'
  FIELD Dato AS DATE 
  INDEX MomsKod Dato ButikkNr MomsKod.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEFINE VARIABLE lTid AS INTEGER NO-UNDO.

/* IF lDirekte AND NOT CAN-DO(SESSION:GET-PRINTERS(),cPrinter) THEN */
/*     RETURN.                                                      */

{syspara.i 19 101 1 iAlle INT}

ASSIGN lTid = TIME.
RUN bibl_logg.p ('holin_visma', 'eksporterkontant_holin_visma_business_akkum.p: Starter SlettTriggerData. ' + string(TIME,"HH:MM:SS")).
RUN SlettTriggerData.

IF dinpFraDato = ? OR dinpTilDato = ? THEN 
DO: 
  RUN SettFraTilDato.
  RUN bibl_logg.p ('holin_visma', 'eksporterkontant_holin_visma_business_akkum.p: AUTO Periode. ' + string(dinpFraDato) + ' / ' + string(dinpTilDato) + ' ' + string(TIME,"HH:MM:SS")).
END.
ELSE DO:
  bManuell = TRUE. /* Flagger at eksporten kjøres manuelt og at det ikke skal sendes eMail. */
  RUN bibl_logg.p ('holin_visma', 'eksporterkontant_holin_visma_business_akkum.p: MANUELL Periode. ' + string(dinpFraDato) + ' / ' + string(dinpTilDato) + ' ' + string(TIME,"HH:MM:SS")).
END.

DAGEKSPORT:
DO pdLoopDato =  dinpFraDato TO dinpTilDato:
  ASSIGN
    dFraDato = pdLoopDato
    dTilDato = pdLoopDato
    .
    
  RUN SlettTriggerData.
  
  RUN bibl_logg.p ('holin_visma', 'eksporterkontant_holin_visma_business_akkum.p: Starter PrepBongData for dato ' + STRING(dFraDato) + '--' + STRING(dTilDato) + ' ' + STRING(TIME,"HH:MM:SS")).
  RUN PrepBongData (dFraDato, dTilDato).

  RUN bibl_logg.p ('holin_visma', 'eksporterkontant_holin_visma_business_akkum.p: Starter KopierElogg. ' + STRING(TIME,"HH:MM:SS")).
  RUN KopierElogg.

  RUN bibl_logg.p ('holin_visma', 'eksporterkontant_holin_visma_business_akkum.p: Starter Eksporter. ' + STRING(TIME,"HH:MM:SS")).
  RUN Eksporter.

  RUN bibl_logg.p ('holin_visma', 'eksporterkontant_holin_visma_business_akkum.p: Starter SlettELogg. ' + STRING(TIME,"HH:MM:SS")).
  RUN SlettELogg. /* */
    
END.


ocRetur = "OK," + String(iAntEksport) + cTekst.

lTid = TIME - lTid.
RUN bibl_logg.p ('holin_visma', 'eksporterkontant_holin_visma_business_akkum.p: ferdig. Tidsbruk: ' + string(lTid,"HH:MM:SS")).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-EDBSystem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EDBSystem Procedure 
PROCEDURE EDBSystem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO TRANSACTION:
    FIND FIRST EkstEDBSystem WHERE
        EkstEDBSystem.EDBSystem = cEDBSystem EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE EkstEDBSystem THEN DO:
        cTekst = ' ** Finner ikke EkstEDBSystem (' + cEDBSystem + ').'.
        LEAVE.
    END. 
    ELSE DO:
        ASSIGN
            cKatalog           = TRIM(EkstEDBSystem.Filkatalog,"\")
            cPrefix            = 'dirdebr' /*EkstEDBSystem.FilPrefix*/
            cEkstent           = 'edi' /*trim(EkstEDBSystem.FilEkstent,".")*/
            iSekvens           = IF (EkstEDBSystem.SeqNr + 1) > EkstEDBSystem.MaksSeq
                                    THEN 1
                                    ELSE EkstEDBSystem.SeqNr + 1
            EkstEDBSystem.SeqNr = iSekvens
            cFilNavn           = cKatalog + "\" +
                                 cPrefix  + 
                                 /*STRING(iSekvens,"99999999") + */  "." + 
                                 cEkstent
            .
        FIND CURRENT EkstEDBSystem NO-LOCK.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-Eksporter) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Eksporter Procedure
PROCEDURE Eksporter:

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDatoTime   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lSaldo      AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE iKundeNr    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntTTELogg AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntTTMva   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iLokEksport AS INTEGER    NO-UNDO.
    
    /* Nullstiller før ny summering. */
    FOR EACH ttMva: 
      DELETE ttMva. 
    END.
    
    ASSIGN
      cDatoTime = "_" + SUBSTR(STRING(YEAR(TODAY)),3) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + STRING(TIME)
      iKundeNr  = 3000.

    IF CAN-FIND(FIRST TT_ELogg) THEN       
    DO:
      /* Setter filnavn */
      RUN EDBSystem.        /* (EkstEDBSystem.EDBSystem)  */
      IF cFilnavn = "" THEN
        RETURN.
      bStream = TRUE.
    END.
    
    /* Setter datospesifikt filnavn ved manuell eksport. */
    IF bManuell THEN 
      cFilNavn = ENTRY(1,cFilNavn,'.') + '-' + REPLACE(STRING(dFraDato),'/','') + '.' + ENTRY(2,cFilNavn,'.').
    
    RUN bibl_logg.p ('holin_visma', 'eksporterkontant_holin_visma_business_akkum.p: Eksportfil. ' + cFilNavn + ' ' + STRING(TIME,"HH:MM:SS")).    
    
    ELOGG:
    FOR EACH TT_ELogg 
      BREAK BY TT_ELogg.ButikkNr
            BY TT_ELogg.B_Id:
        
        ASSIGN 
          TT_ELogg.EndringsType = 2
          iAntTTELogg           = iAntTTELogg + 1.

        FIND BongHode WHERE BongHode.B_Id = DECIMAL(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
        
        IF AVAIL BongHode THEN 
        DO:
          /* Ikke ferdigbehandlet bong. */
          /*
          IF BongHode.BongStatus < 5 THEN 
            NEXT ELOGG.
          */
          /* Bongen er eksportert fra Før */
          /*
          IF BongHode.EksportertDato <> ? THEN 
            NEXT ELOGG.
          */
          /* Kreditsalg skal ikke med her. TN 12/12-09 */
          IF CAN-FIND(FIRST BongLinje WHERE
                    BongLinje.B_Id = BongHode.B_id AND
                    CAN-DO("065",STRING(BongLinje.TTId,"999"))) THEN
            NEXT ELOGG.
          /* Summerer opp de ulike mva nivåene. */
          FOR EACH BongLinje NO-LOCK WHERE
            BongLinje.B_Id = BongHode.B_Id AND
            CAN-DO('1,3,10',STRING(BongLinje.TTId)) AND 
            BongLinje.Makulert = FALSE:            
            DO: 
              FIND FIRST ttMva WHERE
                ttMva.Dato     = BongHode.Dato AND
                ttMva.ButikkNr = BongHode.ButikkNr AND  
                ttMva.MomsKod  = BongLinje.MvaGr NO-ERROR.
              IF NOT AVAILABLE ttMva THEN DO:
                CREATE ttMva.
                ASSIGN
                  iAntTTMva      = iAntTTMva + 1
                  ttMva.Dato     = BongHode.Dato
                  ttMva.ButikkNr = BongHode.ButikkNr 
                  ttMva.MomsKod  = BongLinje.MvaGr.
              END.
              ASSIGN 
                ttMva.Belop = ttMva.Belop + 
                (BongLinje.LinjeSum - (BongLinje.LinjeRab + BongLinje.SubtotalRab))
                  * (IF CAN-DO('3,10',STRING(BongLinje.TTId)) THEN -1 ELSE 1) 
                .
            END.
          END.
 
          ASSIGN 
            iAntEksport = iAntEksport + 1.
              
          DO TRANSACTION:
            FIND CURRENT Bonghode EXCLUSIVE-LOCK.
            ASSIGN
              BongHode.EksportertDato = TODAY.
            RELEASE BongHode.            
          END.
        END.
    END. /* ELOGG */

    RUN bibl_logg.p ('holin_visma', 'eksporterkontant_holin_visma_business_akkum.p: Eksportfil - Ant TT_Elogg ' + string(iAntTTELogg) + ' ' + STRING(TIME,"HH:MM:SS")).    
    RUN bibl_logg.p ('holin_visma', 'eksporterkontant_holin_visma_business_akkum.p: Eksportfil - Ant TTMva ' + string(iAntTTMva) + ' ' + STRING(TIME,"HH:MM:SS")).    

    IF CAN-FIND(FIRST ttMva) THEN 
    MVA_EKSPORT:
    DO: 
      FOR EACH ttMva 
        BREAK BY ttMva.Dato
              BY ttMva.ButikkNr
              BY ttMva.MomsKod:
        IF FIRST-OF(ttMva.Dato) THEN 
        DO:
          /* Åpner stream til fil. */
          IF SEARCH(cFilnavn) = ? THEN 
          DO:
            IF bStreamApen = FALSE THEN 
              OUTPUT TO VALUE(cFilnavn) NO-ECHO APPEND.
          END.
          ELSE DO:
            IF bStreamApen = FALSE THEN  
              OUTPUT TO VALUE(cFilnavn) NO-ECHO APPEND.
          END.        
          PUT UNFORMATTED
              'H'
             + ';' + STRING(YEAR(ttMva.Dato),'9999') + 
                     STRING(MONTH(ttMva.Dato),"99") +
                     STRING(DAY(ttMva.Dato),'99')
             + ';5' 
            SKIP.
          iAntEksport = iAntEksport + 1.
          bStreamApen = TRUE.
        END.      
              
        ASSIGN
          iAntEksport = iAntEksport + 1
          iLokEksport = iLokEksport + 1.
        FIND Butiker NO-LOCK WHERE
          Butiker.butik = ttMva.ButikkNr NO-ERROR.
        PUT UNFORMATTED
            'L'
          + ';' + STRING('1') /* Tidligere ble bongnr lagt ut her. */
          + ';' + STRING(YEAR(ttMva.Dato),'9999') + 
                  STRING(MONTH(ttMva.Dato),"99") +
                  STRING(DAY(ttMva.Dato),'99')
          + ';' + STRING(YEAR(ttMva.Dato),'9999') + 
                  STRING(MONTH(ttMva.Dato),"99") +
                  STRING(DAY(ttMva.Dato),'99')
          + ';1900'
          + ';' + STRING(iKundeNr + (IF ttMva.MomsKod = 0 THEN 120 
                                     ELSE IF ttMva.Momskod = 1 THEN 10
                                     ELSE IF ttMva.MomsKod = 2 THEN 0
                                     ELSE 0)) 
          + ';' + TRIM(REPLACE(STRING(ttMva.Belop,'->>>>>>>>9.99'),',','.'))                            
          + ';' + (IF AVAILABLE Butiker THEN Butiker.KortNavn ELSE '')
          SKIP.
      END.
          
      /* Lukker stream. */
      OUTPUT CLOSE.
      bStreamApen = FALSE.
    END. /* MVA_EKSPORT */

    RUN bibl_logg.p ('holin_visma', 'eksporterkontant_holin_visma_business_akkum.p: Eksportfil - Ant EKSPORT ' + string(iLokEksport) + ' ' + STRING(TIME,"HH:MM:SS")).    
 
    /* Sender filen som eMail. */
    IF bManuell = FALSE THEN 
    DO:
      IF SEARCH(cFilNavn) <> ? THEN DO:
        RUN bibl_logg.p ('holin_visma', 'eksporterkontant_holin_visma_business_akkum.p: Sender eMail. ' + cFilNavn + ' ' + string(TIME,"HH:MM:SS")).      
        RUN sendEMailButikk (cFilNavn,'').
      END.
      IF SEARCH('c:\home\lindbak\sendes\ordersr.edi') <> ? THEN DO: 
        RUN bibl_logg.p ('holin_visma', 'eksporterkontant_holin_visma_business_akkum.p: Sender eMail. ' + 'c:\home\lindbak\sendes\ordersr.edi ' + string(TIME,"HH:MM:SS")).      
        RUN sendEMailButikk ('c:\home\lindbak\sendes\ordersr.edi','Eksport kreditsalg dampbakeriet ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS")).
      END.
    END.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-KopierElogg) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierElogg Procedure
PROCEDURE KopierElogg:

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    DEFINE BUFFER bElogg FOR Elogg.
    DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.
    
    iAnt = 0.
    
    KOPIER:
    FOR EACH ELogg WHERE ELogg.TabellNavn = "Bonghode" AND
                         ELogg.EksterntSystem = "KONTAUTO" NO-LOCK:
        iAnt = iAnt + 1.
                             
        FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL bElogg THEN DO:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            IF AVAILABLE TT_ELogg THEN
              DO: 
                ASSIGN TT_ELogg.EndringsType = 1.
                FIND Bonghode WHERE Bonghode.B_Id = DECIMAL(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
                IF AVAILABLE TT_ELogg THEN 
                  ASSIGN
                    TT_ELogg.B_Id     = Bonghode.B_Id
                    TT_ELogg.ButikkNr = BongHode.ButikkNr.                 
              END.
        END.
        IF AVAILABLE TT_Elogg THEN
            RELEASE TT_ELogg.
    END. /* KOPIER */

    RUN bibl_logg.p ('holin_visma', 'eksporterkontant_holin_visma_business_akkum.p: KopierELogg - antall kopiert ' + string(iAnt) + '. ' + STRING(TIME,"HH:MM:SS")).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
 
 
&IF DEFINED(EXCLUDE-PrepBongData) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrepBongData Procedure
PROCEDURE PrepBongData:

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pLokdFraDato AS DATE NO-UNDO.
DEFINE INPUT PARAMETER pLokdTilDato AS DATE NO-UNDO.

DEFINE VARIABLE piLokdLoopDato AS DATE NO-UNDO.
DEFINE VARIABLE iAnt           AS INTEGER NO-UNDO.

iAnt = 0.

FOR EACH Butiker NO-LOCK,
  EACH Kasse WHERE Kasse.ButikkNr = Butiker.Butik:
  
  DATOLOOP:
  DO piLokdLoopDato = pLokdFraDato TO pLokdTilDato:
  
    RUN bibl_logg.p ('holin_visma', 'eksporterkontant_holin_visma_business_akkum.p: Prep.bongdata FOR butikk/dato ' + string(Butiker.Butik) + '-' + string(piLokdLoopDato) + ' ' + STRING(TIME,"HH:MM:SS")).
  
    IF NOT CAN-FIND( FIRST BongHode NO-LOCK WHERE
            BongHode.ButikkNr = Butiker.Butik AND
            BongHode.GruppeNr = 1 AND
            BongHode.KasseNr  = Kasse.KasseNr AND
            BongHode.Dato     = piLokdLoopDato) THEN 
            NEXT DATOLOOP.
  
    FOR EACH BongHode EXCLUSIVE-LOCK WHERE
        BongHode.ButikkNr = Butiker.Butik AND
        BongHode.GruppeNr = 1 AND
        BongHode.KasseNr  = Kasse.KasseNr AND
        BongHode.Dato     = piLokdLoopDato:

      ASSIGN 
        BongHode.EksportertDato = ?
        iAnt                    = iAnt + 1.

      ERPUT:
      DO:
        FIND ELogg WHERE 
             ELogg.TabellNavn     = "Bonghode" AND
             ELogg.EksterntSystem = "KONTAUTO"    AND
             ELogg.Verdier        = STRING(BongHode.B_Id) NO-ERROR.
        IF NOT AVAIL Elogg THEN DO:
            CREATE Elogg.
            ASSIGN ELogg.TabellNavn     = "Bonghode"
                   ELogg.EksterntSystem = "KONTAUTO"   
                   ELogg.Verdier        = STRING(Bonghode.B_Id).
        END.
        ASSIGN ELogg.EndringsType = 1 
               ELogg.Behandlet    = FALSE.
        RELEASE ELogg.
      END. /* ERPUT */
    END.
    RUN bibl_logg.p ('holin_visma', 'eksporterkontant_holin_visma_business_akkum.p: Prep.bongdata - antall bonger ' + string(iAnt) + '. ' + STRING(TIME,"HH:MM:SS")).
  END. /* DATOLOOP */
END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
 
&IF DEFINED(EXCLUDE-sendEMailButikk) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendEMailButikk Procedure
PROCEDURE sendEMailButikk:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER icFiler   AS CHARACTER NO-UNDO.
 DEFINE INPUT PARAMETER pcSubject AS CHARACTER NO-UNDO.

 DEFINE VARIABLE cCLMailfra AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cSubject   AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE iCL        AS INTEGER NO-UNDO.

 DEFINE VARIABLE lMailOK AS logi NO-UNDO.
 DEFINE VARIABLE cMessage AS CHAR NO-UNDO.
 DEFINE VARIABLE cMailhub  AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cDoAUTH   AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cAuthType AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cUser     AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cPassword AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cMailTo   AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cEmailCC  AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cFiler    AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE ii        AS INTEGER     NO-UNDO.
 DEFINE VARIABLE cFrom     AS CHARACTER NO-UNDO.
 
 DEFINE BUFFER mailButiker FOR Butiker.

 {syspara.i 5 1 1 iCL INT}

 FIND mailButiker NO-LOCK WHERE
   mailButiker.Butik = iCL NO-ERROR.
 /* Butikken må finnes. */
 IF NOT AVAILABLE mailButiker THEN 
   RETURN.
   
 /* ePostadresse må være satt på butikken. */
 IF mailButiker.ePostAdresse = '' THEN 
   RETURN. 

 {syspara.i 50 50 1  cMailhub }
 {syspara.i 50 50 2  cDoAUTH  }
 {syspara.i 50 50 3  cAuthType}
 {syspara.i 50 50 4  cUser    }
 {syspara.i 50 50 5  cPassword}
 {syspara.i 50 50 31 cMailTo}
 {syspar2.i 50 50 31 cEmailCC }
 IF cDoAUTH = "0" THEN
     ASSIGN cDoAUTH   = "FALSE"
            cAuthType = ""
            cUser     = ""
            cPassword = "".
 ELSE
     cDoAUTH = "TRUE".
IF pcSubject <> ''
  THEN cSubject = pcSubject.
 ELSE 
   ASSIGN 
   cSubject = "Eksport kontantsalg dampbakeriet " + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS").
    
 /* Trekker ut bare filnavn */   
 DO ii = 1 TO NUM-ENTRIES(icFiler):
    cFiler = cFiler + (IF cFiler <> "" THEN "," ELSE "") + 
    ENTRY(NUM-ENTRIES(ENTRY(ii,icFiler),"\"),ENTRY(ii,icFiler),"\").
 END.

/*
MESSAGE 'Sending av eMail' SKIP
  'mailButiker.ePostAdresse' mailButiker.ePostAdresse SKIP
  'cMailTo' cMailTo SKIP
  'cFiler' cFiler SKIP
  'icFiler' icFiler
  VIEW-AS ALERT-BOX.
*/

 RUN prssmtpmailv5_7a.p (
        /*mailhub    */   cMailhub,
        /*EmailTo    */   cMailTo,
        /*EmailFrom  */   mailButiker.ePostAdresse,
        /*EmailCC    */   cEmailCC,
        /*Attachments*/   cFiler,
        /*LocalFiles */   icFiler,
        /*Subject    */   cSubject,
        /*Body       */   "",
        /*MIMEHeader */   "CharSet=iso8859-1",
        /*BodyType   */   "",
        /*Importance */   0,
        /*L_DoAUTH   */   cDoAUTH,
        /*C_AuthType */   cAuthType,
        /*C_User     */   cUser,
        /*C_Password */   cPassword,
        /*oSuccessful*/  OUTPUT lMailOK,
        /*vMessage   */  OUTPUT cMessage) NO-ERROR.
/*         IF lMailOk = FALSE THEN DO:                     */
/*             MESSAGE "Sending avbrutt med melding:" SKIP */
/*                     cMessage                            */
/*                     VIEW-AS ALERT-BOX.                  */
/*         END.                                            */
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-SettFraTilDato) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettFraTilDato Procedure
PROCEDURE SettFraTilDato:

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE pdDato AS DATE NO-UNDO.

{syspara.i 210 101 1 pcTekst}
ASSIGN
  pdDato = DATE(pcTekst) NO-ERROR.
IF ERROR-STATUS:ERROR OR pcTekst = '' THEN ASSIGN pdDato = TODAY.

/* Ved automatikk kjører vi alltid på dagens dato.                         */
/* Skal det legges ut data for tidligere dager, skal fix rutinen benyttes. */

ASSIGN
  dFraDato    = TODAY
  dTilDato    = TODAY 
  dinpFraDato = dFraDato
  dinpTilDato = dTilDato
  .

DO TRANSACTION:
  {setsyspara.i 210 101 1 STRING(dTilDato)}
END.  
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettELogg) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettELogg Procedure
PROCEDURE SlettELogg:

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.
	iAnt = 0.
    FOR EACH TT_Elogg WHERE TT_ELogg.EndringsType = 2:
        FIND ELogg WHERE ELogg.TabellNavn     = TT_ELogg.TabellNavn AND
                         ELogg.EksterntSystem = TT_ELogg.EksterntSystem AND
                         ELogg.Verdier        = TT_ELogg.Verdier
                      EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL ELogg THEN DO:
            iAnt = iAnt + 1.
            DELETE ELogg.
            DELETE TT_ELogg.
        END.
    END.

    RUN bibl_logg.p ('holin_visma', 'eksporterkontant_holin_visma_business_akkum.p: Slett ELogg - antall slettet ' + string(iAnt) + '. ' + STRING(TIME,"HH:MM:SS")).
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-SlettTriggerData) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTriggerData Procedure
PROCEDURE SlettTriggerData:

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
  DEFINE VARIABLE iant AS INTEGER NO-UNDO.
  
  iAnt = 0.
  
  FOR EACH ELogg EXCLUSIVE-LOCK WHERE 
    ELogg.TabellNavn     = "Bonghode" AND
    ELogg.EksterntSystem = "KONTAUTO":
    DELETE ELogg.
    iAnt = iAnt + 1.
  END.
    
  FOR EACH TT_Elogg:
    DELETE TT_ELogg.
  END.
  
  RUN bibl_logg.p ('holin_visma', 'eksporterkontant_holin_visma_business_akkum.p: SlettTriggerData - antall slettet ' + string(iAnt) + '. ' + STRING(TIME,"HH:MM:SS")).
  
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

