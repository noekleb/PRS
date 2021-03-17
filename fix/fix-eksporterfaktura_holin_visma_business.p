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
DEFINE VARIABLE iAntEksport AS INTEGER    NO-UNDO.

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
DEF VAR cEDBSystem AS CHAR INITIAL "EkspFAAU" NO-UNDO.
DEFINE VARIABLE dDato AS DATE NO-UNDO.

DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk     AS CHARACTER NO-UNDO.

DEFINE BUFFER bKundeResKontr FOR KundeResKontr.
DEFINE BUFFER bFakturaHode   FOR FakturaHode.

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_ELogg NO-UNDO LIKE ELogg
  FIELD Faktura_Id LIKE FakturaHode.Faktura_Id
  FIELD ButikkNr LIKE FakturaHode.ButikkNr
  INDEX EksportIdx ButikkNr Faktura_Id.

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

/*RUN EksporterFaktura.*/

/*RUN KopierElogg.*/

RUN Eksportera.

RUN SlettELogg. /* */

ocRetur = "OK," + String(iAntEksport).


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
        LEAVE.
    END.
    ELSE DO:
        ASSIGN
            cKatalog           = TRIM(EkstEDBSystem.Filkatalog,"\")
            cPrefix            = 'ordersr' /*EkstEDBSystem.FilPrefix*/
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

 
&IF DEFINED(EXCLUDE-Eksportera) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Eksportera Procedure
PROCEDURE Eksportera:

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDatoTime AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lSaldo    AS DECIMAL    NO-UNDO.
    
    DEFINE BUFFER bFakturaHode FOR FakturaHode.
    
    cDatoTime = "_" + SUBSTR(STRING(YEAR(TODAY)),3) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + STRING(TIME).

    /*IF CAN-FIND(FIRST TT_ELogg) THEN*/       
    DO:
      /* Setter filnavn */
      RUN EDBSystem.        /* (EkstEDBSystem.EDBSystem)  */
      IF cFilnavn = "" THEN
        RETURN.
      bStream = TRUE.
      OUTPUT TO VALUE(cFilnavn) NO-ECHO APPEND.
    END.
    ELOGG:
    FOR EACH FakturaHode NO-LOCK WHERE 
      FakturaHode.RegistrertDato >= 04/30/2010 AND
      FakturaHode.RegistrertDato <= 06/01/2010 
      BREAK BY FakturaHode.ButikkNr
            BY FakturaHode.Faktura_Id:
        /*IF AVAIL FakturaHode THEN */
        DO:
          /*
          IF FakturaHode.EksportertDato <> ? OR
             FakturaHode.FakturaNr = 0 OR
             FakturaHode.FakturaNr = ? THEN
             NEXT ELOGG.
          IF FakturaHode.EDato = TODAY AND TIME - FakturaHode.etid < 5 THEN
             NEXT ELOGG.
          */
          /* Faktura, kreditnota, purring. */
          IF NOT CAN-DO('1,2,10',STRING(FakturaHode.BilagsType)) THEN 
             NEXT ELOGG.
          FIND FIRST KundeKort NO-LOCK WHERE
            KundeKort.KundeNr = FakturaHode.KundeNr NO-ERROR.      
          PUT UNFORMATTED
              'H'
            + ';' + STRING(FakturaHode.FakturaNr)
            + ';2' 
            + ';' + (IF AVAILABLE KundeKort THEN KundeKort.KortNr ELSE '')
            + ';' + REPLACE(FakturaHode.DeresRef,';',',')
            + ';1' 
            SKIP.
          iAntEksport = iAntEksport + 1.

          FAKTURALINJE:
          FOR EACH FakturaLinje OF FakturaHode NO-LOCK:
            FIND BongHode NO-LOCK WHERE
              BongHode.B_Id = FakturaLinje.B_Id NO-ERROR.
            IF AVAILABLE BongHode THEN
            BONGHODE:
            DO:
              IF BongHode.Dato >= 05/28/2010 AND
                BongHode.Dato <= 05/29/2010 THEN
                LEAVE BONGHODE.
              ELSE 
                NEXT FAKTURALINJE. 
            END. /* BONGHODE */
          
            IF NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = FakturaLinje.ArtikkelNr)
              THEN NEXT FAKTURALINJE.
            FIND BongHode NO-LOCK WHERE
              BongHode.B_Id = FakturaLinje.B_Id NO-ERROR.
            IF AVAILABLE BongHode THEN 
              dDato = BongHode.Dato.
            ELSE dDato = FakturaLinje.Leveringsdato.
            IF FakturaLinje.ArtikkelNr = 0
              THEN NEXT FAKTURALINJE.
            FIND ArtBas OF FakturaLinje NO-LOCK NO-ERROR.
            IF AVAILABLE ArtBas THEN FIND FIRST Strekkode OF ArtBas NO-LOCK NO-ERROR. 
            FIND Butiker NO-LOCK WHERE 
              Butiker.Butik = FakturaHode.ButikkNr NO-ERROR. 
            PUT UNFORMATTED
                'L'
              + ';' + (IF AVAILABLE Strekkode THEN Strekkode.Kode ELSE '')
              + ';' + FakturaLinje.EkstRefTekst
              + ';' + REPLACE(FakturaLinje.Varetekst,';',',')
              + ';' + TRIM(REPLACE(STRING(FakturaLinje.Antall,'->>>>>>>>9.999'),',','.'))
              + ';' + TRIM(REPLACE(STRING((FakturaLinje.Linjesum / FakturaLinje.Antall),'->>>>>>>>9.99'),',','.'))
              + ';' + TRIM(REPLACE(STRING((FakturaLinje.LinjeRabattKr / FakturaLinje.Antall),'->>>>>>>>9.99'),',','.'))                            
              + ';' + STRING(YEAR(dDato),'9999') + 
                      STRING(MONTH(dDato),"99") +
                      STRING(DAY(dDato),'99')
              + ';' + (IF AVAILABLE Butiker THEN Butiker.KortNavn ELSE '')
              + ';1'                       
              SKIP.
          END. /* FakturaLinje */
          
          /* Henter fakturaens reskontropost for å finne korrekt saldo. */
          FIND FIRST bKundereskontr EXCLUSIVE-LOCK WHERE
            bKundeReskontr.BilagsType = (IF FakturaHode.Bilagstype = 2 THEN 2
                                         ELSE IF FakturaHode.Bilagstype = 10 THEN 11
                                         ELSE 1) AND 
            bKundeResKontr.FakturaNr  = FakturaHode.FakturaNr AND
            bKundeResKontr.BArtNr     = 1 AND
            bKundeResKontr.FakturertDato = FakturaHode.FakturertDato AND
            bKundeReskontr.KundeNr       = FakturaHode.KundeNr AND
            bKundeReskontr.Forfallsdato  = FakturaHode.Forfallsdato NO-ERROR.
          IF AVAILABLE bKundeReskontr THEN 
          DO:
            ASSIGN
              lSaldo               = bKundeReskontr.Saldo
              bKundeReskontr.Saldo = 0.
            CREATE KundeResKontr.
            ASSIGN
              KundeReskontr.FakturaNr      = FakturaHode.FakturaNr        
              KundeReskontr.KundeNr        = FakturaHode.KundeNr
              KundeReskontr.FakturertDato  = TODAY /* FakturaHode.FakturertDato*/  
              KundeReskontr.ForfallsDato   = TODAY /* FakturaHode.Forfallsdato */   
              KundeReskontr.Belop          = lSaldo * -1  
              KundeReskontr.BArtNr         = 1
              KundeReskontr.BilagsType     = 3.
            CREATE KundeResKobling.
            DO:
              IF FakturaHode.Bilagstype = 1 THEN 
              ASSIGN
                KundeResKobling.DReskontro_Id = bKundeResKontr.Reskontro_Id 
                KundeResKobling.KReskontro_Id = KundeResKontr.Reskontro_Id.
              ELSE 
              ASSIGN
                KundeResKobling.DReskontro_Id = KundeResKontr.Reskontro_Id
                KundeResKobling.KReskontro_Id = bKundeResKontr.Reskontro_Id.
              ASSIGN 
                KundeResKobling.Belop         = lSaldo
                KundeResKobling.Dato          = TODAY NO-ERROR.
            END.
            RUN beregn_kunde_saldo.p ("idlist|" + STRING(FakturaHode.KundeNr),
                              ?,
                              "",
                              OUTPUT ocReturn,
                              OUTPUT obOk).
            FIND bFakturaHode EXCLUSIVE-LOCK WHERE
              RECID(bFakturaHode) = RECID(FakturaHode) NO-ERROR.
            IF AVAILABLE bFakturaHode THEN        
              ASSIGN
                bFakturaHode.EksportertDato = TODAY
                bFakturaHode.EksportertAv   = USERID("SkoTex").

            ASSIGN 
              /*TT_ELogg.EndringsType = 2*/
              iAntEksport = iAntEksport + 1.
          END.
        END.
    END. /* ELOGG */

    /* Lukker pr. butikk. */
    IF bStream THEN 
      OUTPUT CLOSE.

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
    FOR EACH ELogg WHERE ELogg.TabellNavn = "FakturaHode" AND
                         ELogg.EksterntSystem = "FAKTAUTO" NO-LOCK:

        FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL bElogg THEN DO:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            IF AVAILABLE TT_ELogg THEN
              DO: 
                ASSIGN TT_ELogg.EndringsType = 1.
                FIND FakturaHode WHERE FakturaHode.Faktura_Id = DECIMAL(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
                IF AVAILABLE TT_ELogg THEN 
                  ASSIGN
                    TT_ELogg.Faktura_Id = FakturaHode.Faktura_Id
                    TT_ELogg.ButikkNr   = FakturaHode.ButikkNr.                 
              END.
        END.        
        IF AVAILABLE TT_Elogg THEN
            RELEASE TT_ELogg.
    END. /* KOPIER */


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
   cSubject = "Eksport kreditsalg dampbakeriet " + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS")
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



&IF DEFINED(EXCLUDE-SlettELogg) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettELogg Procedure
PROCEDURE SlettELogg:

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    FOR EACH TT_Elogg WHERE TT_ELogg.EndringsType   = 2:
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

