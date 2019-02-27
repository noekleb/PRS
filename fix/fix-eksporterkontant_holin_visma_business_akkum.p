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

DEFINE VARIABLE ocRetur     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE  iAntEksport AS INTEGER    NO-UNDO.

DEF VAR iAntLinjer   AS INT        NO-UNDO.
DEF VAR iAlle        AS INT        NO-UNDO.
DEF VAR bStream      AS LOG        NO-UNDO.
DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.

/* Filhåndtering */
DEF VAR cFilNavn   AS CHAR FORMAT "x(40)"     NO-UNDO.
DEF VAR cKatalog   AS CHAR                    NO-UNDO.
DEF VAR cPrefix    AS CHAR                    NO-UNDO.
DEF VAR cEkstent   AS CHAR                    NO-UNDO.
DEF VAR iSekvens   AS INT  FORMAT ">>>>>>>9"  NO-UNDO.
DEF VAR cEDBSystem AS CHAR INITIAL "EkspKOAU" NO-UNDO.

DEFINE VARIABLE dFraDato  AS DATE NO-UNDO.
DEFINE VARIABLE dTilDato  AS DATE NO-UNDO.
DEFINE VARIABLE iButikkNr AS INTEGER NO-UNDO.

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

/* IF lDirekte AND NOT CAN-DO(SESSION:GET-PRINTERS(),cPrinter) THEN */
/*     RETURN.                                                      */

{syspara.i 19 101 1 iAlle INT}

RUN SlettTriggerData.

RUN SettFraTilDato.

RUN PrepBongData (dFraDato, dTilDato, iButikkNr).

RUN KopierElogg.

RUN Eksporter.

RUN SlettELogg. /* */

ocRetur = "OK," + String(iAntEksport) + cTekst.


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
    DEFINE VARIABLE cDatoTime AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lSaldo    AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE iKundeNr  AS INTEGER    NO-UNDO.

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
    
    ELOGG:
    FOR EACH TT_ELogg 
      BREAK BY BongHode.ButikkNr
            BY BongHode.B_Id:
        
        ASSIGN 
          TT_ELogg.EndringsType = 2.

        FIND BongHode WHERE BongHode.B_Id = DECIMAL(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
        
        IF AVAIL BongHode THEN 
        DO:
          /* Ikke ferdigbehandlet bong. */
          IF BongHode.BongStatus < 5 THEN 
            NEXT ELOGG.
          /* Bongen er eksportert fra Før */
          IF BongHode.EksportertDato <> ? THEN 
            NEXT ELOGG.
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
              
        iAntEksport = iAntEksport + 1.
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
    END. /* MVA_EKSPORT */

    /* Sender filen som eMail. */
    IF SEARCH(cFilNavn) <> ? THEN 
      RUN sendEMailButikk (cFilNavn).
      
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

    KOPIER:
    FOR EACH ELogg WHERE ELogg.TabellNavn = "Bonghode" AND
                         ELogg.EksterntSystem = "KONTAUTO" NO-LOCK:
                             
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
DEFINE INPUT PARAMETER pdFraDato AS DATE NO-UNDO.
DEFINE INPUT PARAMETER pdTilDato AS DATE NO-UNDO.
DEFINE INPUT PARAMETER piButikkNr AS INTEGER NO-UNDO.

FOR EACH Butiker NO-LOCK WHERE
  Butiker.Butik = piButikkNr:
    FOR EACH BongHode EXCLUSIVE-LOCK WHERE
        BongHode.ButikkNr = butiker.butik AND
        BongHode.GruppeNr = 1 AND
        BongHode.KasseNr > 0 AND
        BongHode.Dato >= pdFraDato AND
        BongHode.Dato <= pdTilDato:

      ASSIGN 
        BongHode.EksportertDato = ?.

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
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER icFiler   AS CHARACTER NO-UNDO.

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
 DEFINE VARIABLE cMailTo AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cEmailCC  AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cFiler AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
 
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
 ASSIGN 
   cSubject = "Eksport kontantsalg dampbakeriet " + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS")
   .
    
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
IF ERROR-STATUS:ERROR OR pcTekst = '' 
THEN ASSIGN pdDato = TODAY.
ELSE ASSIGN pdDato = pdDato + 1.

ASSIGN
  dFraDato = pdDato
  dTilDato = (IF pdDato > TODAY THEN pdDato ELSE TODAY).

UPDATE dFraDato 
       dTilDato
       iButikkNr.

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
    FOR EACH TT_Elogg WHERE TT_ELogg.EndringsType = 2:
        FIND ELogg WHERE ELogg.TabellNavn     = TT_ELogg.TabellNavn AND
                         ELogg.EksterntSystem = TT_ELogg.EksterntSystem AND
                         ELogg.Verdier        = TT_ELogg.Verdier
                      EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL ELogg THEN DO:
            DELETE ELogg.
            DELETE TT_ELogg.
        END.
    END.

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

    FOR EACH ELogg EXCLUSIVE-LOCK WHERE 
      ELogg.TabellNavn     = "Bonghode" AND
      ELogg.EksterntSystem = "KONTAUTO":
      DELETE ELogg.
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

