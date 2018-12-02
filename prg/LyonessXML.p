&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
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
DEFINE INPUT  PARAMETER dRundate AS DATE        NO-UNDO.
DEFINE VARIABLE hDset AS HANDLE  NO-UNDO.
DEFINE VARIABLE lcData AS LONGCHAR     NO-UNDO.
DEFINE VARIABLE cOrgDateFormat AS CHARACTER   NO-UNDO.

DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DEFINE VARIABLE cDealerID  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOutputDir AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBackupDir AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBackupFileName AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOutputFileName AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFileName AS CHARACTER   NO-UNDO.


DEFINE TEMP-TABLE tt_filer NO-UNDO
    FIELD filnavn AS CHAR.

DEFINE TEMP-TABLE tt_Dealer NO-UNDO SERIALIZE-NAME "Dealer"
    FIELD DealerID AS CHAR XML-NODE-NAME  "ID" XML-NODE-TYPE  "ATTRIBUTE" SERIALIZE-NAME "ID"
    FIELD PeriodBegin AS DATETIME
    FIELD PeriodEnd   AS DATETIME
    FIELD Checksum    AS DECI
    FIELD FileCreationDate AS DATETIME
    INDEX did IS PRIMARY UNIQUE DealerID.
DEFINE TEMP-TABLE tt_transaction NO-UNDO SERIALIZE-NAME "Transaction"
    FIELD DealerID AS CHAR SERIALIZE-HIDDEN
    FIELD Strekkode       AS CHAR SERIALIZE-NAME "CardNumber"
    FIELD b_id     AS DECI DECIMALS 0 SERIALIZE-NAME "InvoiceNumber"
    FIELD InvoiceDate      AS DATETIME
    FIELD InvoiceAmount    AS DECI
    FIELD InvoiceCurrency  AS CHAR
    FIELD StoreNumber      AS CHAR
    FIELD SegmentNr        AS INTE
    INDEX bid IS PRIMARY DealerID.

/* DEFINE DATASET DSET SERIALIZE-HIDDEN FOR tt_Dealer, tt_transaction */
/*     DATA-RELATION tt_Dealer FOR tt_Dealer,                         */
/*       tt_transaction RELATION-FIELDS(DealerID,DealerID) NESTED .   */
/*                                                                    */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getDTstring) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDTstring Procedure 
FUNCTION getDTstring RETURNS CHARACTER
  ( INPUT dtVar AS DATETIME )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFileName Procedure 
FUNCTION getFileName RETURNS CHARACTER
  ( /* */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* Hämta systemparametrar */

{syspara.i 210 262 1 cDealerID }
{syspara.i 210 262 2 cOutputDir}
cOutputDir = RIGHT-TRIM(cOutputDir,"\") + "\".
cBackupDir = cOutputDir + "bku\".
/* /* 1: */   cDealerId                                                         */
/* /* 2: */   cExportDir /* Efter lyckt mailskick så sparas filen i ...\bku. */ */
/* /* 3: */   cMailHub                                                          */
/* /* 4: */   cMailTo                                                           */
/* /* 5: */   cMailFrom                                                         */
/* /* 6: */                                                                     */

/* Kontrollera vilken dag vi skall köra för */
/* Om ingen så kör vi föregående dag */

DO ii = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
    
END.

/* Hämta data till temptablar */
RUN FillData.
/* RUN createTEMPdata. */
IF CAN-FIND(FIRST tt_dealer) AND CAN-FIND(FIRST tt_transaction) THEN DO:

    cOrgDateFormat = SESSION:DATE-FORMAT.
    SESSION:DATE-FORMAT = 'ymd'.
    RUN getXMLstring(OUTPUT lcData).
    /*  */
    cFileName = "H" + cDealerID + "_" + getFileName() + ".xml".
    cOutputFileName = cOutputDir + cFileName.
    cBackupFileName = cBackupDir + cFileName.
    SESSION:DATE-FORMAT = cOrgDateFormat.
    OUTPUT TO VALUE(cOutputFileName).
    PUT UNFORMATTED STRING(lcdata) SKIP.
    OUTPUT CLOSE.
/* H1234_20100101151223.XML */
/*     RUN sendEmail(cOutputFileName,cBackupFileName). */
    /* om mailen gått bra så kan vi se efter tidigare filer som inte har mailats */

END.
/* IF cOutputFileName <> "" AND SEARCH(cOutputFileName) = ? THEN */
/*     RUN sendGamlaFiler.                                       */
/* MESSAGE cOutputDir                                  */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.              */
/* OUTPUT TO "CLIPBOARD".                              */
/* PUT UNFORMATTED cFilename SKIP STRING(lcdata) SKIP. */
/* OUTPUT CLOSE.                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-createTEMPdata) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTEMPdata Procedure 
PROCEDURE createTEMPdata :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
CREATE tt_Dealer.
ASSIGN tt_dealer.dealerid = cDealerID
    tt_dealer.PeriodBegin = NOW - 10000
    tt_dealer.PeriodEnd = NOW - 100
    tt_dealer.Checksum  = 900 * - 1
    tt_dealer.FileCreationDate = NOW.

CREATE tt_transaction.
ASSIGN
    tt_transaction.DealerID        = cDealerID
    tt_transaction.Strekkode       = "7710007007379"
    tt_transaction.b_id            = 2010031136000054
    tt_transaction.InvoiceDate     = NOW - 5000
    tt_transaction.InvoiceAmount   = 500 * -1
    tt_transaction.InvoiceCurrency = "NOK"
    tt_transaction.SegmentNr       = 1.

CREATE tt_transaction.
ASSIGN
    tt_transaction.DealerID        = cDealerID
    tt_transaction.Strekkode       = "7710007007379"
    tt_transaction.b_id            = 2010031136000055
    tt_transaction.InvoiceDate     = NOW - 4000
    tt_transaction.InvoiceAmount   = 400 * -1
    tt_transaction.InvoiceCurrency = "NOK"
    tt_transaction.SegmentNr       = 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillData Procedure 
PROCEDURE FillData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE dSum AS DECIMAL     NO-UNDO.
DEFINE VARIABLE lFunnet AS LOGICAL     NO-UNDO.
CREATE tt_Dealer.
ASSIGN tt_dealer.dealerid = cDealerID
    tt_dealer.PeriodBegin = dRundate
    tt_dealer.PeriodEnd   = dRundate + 1
    tt_dealer.PeriodEnd   = tt_dealer.PeriodEnd - 1
    tt_dealer.Checksum    = 0
    tt_dealer.FileCreationDate = NOW.
FOR EACH butiker NO-LOCK:
    FOR EACH bonghode WHERE bonghode.butikknr = butiker.butik AND bonghode.dato = dRundate NO-LOCK:
        IF CAN-FIND(FIRST bonglinje WHERE bonglinje.b_id = bonghode.b_id AND bonglinje.ttid = 160 AND makulert = FALSE) THEN DO:
            ASSIGN lFunnet = FALSE.
            FOR EACH bonglinje WHERE bonglinje.b_id = bonghode.b_id AND bonglinje.makulert = FALSE NO-LOCK:
                IF CAN-DO("1,3,10",STRING(bonglinje.ttid)) THEN DO:
                    lFunnet = TRUE.
                    LEAVE.
                END.
            END.
            IF NOT lFunnet THEN
                NEXT.
            FIND FIRST bonglinje WHERE bonglinje.b_id = bonghode.b_id AND bonglinje.ttid = 160 AND bonglinje.makulert = FALSE NO-LOCK.
            CREATE tt_transaction.
            ASSIGN
                tt_transaction.DealerID        = cDealerID
                tt_transaction.Strekkode       = bonglinje.Strekkode
                tt_transaction.b_id            = bonghode.b_id
                tt_transaction.InvoiceDate     = bonghode.dato
                tt_transaction.InvoiceDate     = tt_transaction.InvoiceDate + bonghode.tid * 1000
                tt_transaction.InvoiceAmount   = bonghode.belop
                tt_transaction.InvoiceCurrency = "NOK"
                tt_transaction.StoreNumber     = STRING(Bonglinje.butikknr)
                tt_transaction.SegmentNr       = 1.
            ASSIGN tt_dealer.Checksum = tt_dealer.Checksum + bonghode.belop.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getXMLstring) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getXMLstring Procedure 
PROCEDURE getXMLstring :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER lcXMLData AS LONGCHAR     NO-UNDO.
DEFINE VARIABLE tmpTransaction AS CHARACTER   NO-UNDO.

  FIND FIRST tt_Dealer.
  lcXMLData = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' + CHR(10) +
              '<EJournal>' + CHR(10) + 
              '    <Dealer ID="' + STRING(tt_Dealer.DealerID) + '">' + CHR(10) + 
              '    <PeriodBegin>' + getDTstring(tt_Dealer.PeriodBegin) + '</PeriodBegin>' + CHR(10) +
              '    <PeriodEnd>' + getDTstring(tt_Dealer.PeriodEnd) + '</PeriodEnd>' + CHR(10).
   FOR EACH tt_transaction OF tt_dealer:
       ASSIGN tmpTransaction = '    <Transaction>' + CHR(10) +
                               '        <CardNumber>'      +  tt_transaction.Strekkode                                           + '</CardNumber>'      + CHR(10) +
                               '        <InvoiceNumber>'   +  STRING(tt_transaction.b_id)                                        + '</InvoiceNumber>'   + CHR(10) +
                               '        <InvoiceDate>'     +  getDTstring(tt_transaction.InvoiceDate)                            + '</InvoiceDate>'     + CHR(10) +
                               '        <InvoiceAmount>'   +  TRIM(REPLACE(STRING(tt_transaction.InvoiceAmount,"->>>>>>9.99"),",",".")) + '</InvoiceAmount>'   + CHR(10) +
                               '        <InvoiceCurrency>' +  tt_transaction.InvoiceCurrency                                     + '</InvoiceCurrency>' + CHR(10) +
                               '        <StoreNumber>'     +  tt_transaction.StoreNumber                                         + '</StoreNumber>' + CHR(10) +
                               '        <SegmentNr>'       +  STRING(tt_transaction.SegmentNr)                                   + '</SegmentNr>'       + CHR(10).
       lcXMLData = lcXMLData + tmpTransaction + '    </Transaction>' + CHR(10).
   END.
   ASSIGN lcXMLData = lcXMLData + '    </Dealer>'     + CHR(10) +
                      '    <Checksum>' + TRIM(REPLACE(STRING(tt_Dealer.Checksum,"->>>>>>>>9.99"),",",".")) + '</Checksum>' + CHR(10) +
                      '    <FileCreationDate>' + getDTstring(tt_Dealer.FileCreationDate) + '</FileCreationDate>' + CHR(10) +
                      '</EJournal>'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sendEmail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendEmail Procedure 
PROCEDURE sendEmail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cFileName   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER cBackupFile AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cMailTo   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cMailhub  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cDoAUTH   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cAuthType AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cUser     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cPassword AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEmailCC  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEmailFrom AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE lMailOK    AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER   NO-UNDO.

    {syspara.i 50 50 1 cMailhub }
    {syspara.i 50 50 2 cDoAUTH  }
    {syspara.i 50 50 3 cAuthType}
    {syspara.i 50 50 4 cUser    }
    {syspara.i 50 50 5 cPassword}
    {syspara.i 210 262 3 cMailTo }
    {syspara.i 210 262 4 cEmailFrom}
        RUN prssmtpmailv5_7a.p (
        /*mailhub    */   cMailhub,
        /*EmailTo    */   cMailTo,
        /*EmailFrom  */   cEmailFrom,
        /*EmailCC    */   "",
        /*Attachments*/   ENTRY(NUM-ENTRIES(cFileName,"\"),cFileName,"\"),
        /*LocalFiles */   cFileName,
        /*Subject    */   "EJournal",
        /*Body       */   "",
        /*MIMEHeader */   "CharSet=iso8859-1",
        /*BodyType   */   "",
        /*Importance */   0,
        /*L_DoAUTH   */   0,  /* 0 i syspara funkar inte */
        /*C_AuthType */   cAuthType,
        /*C_User     */   cUser,
        /*C_Password */   cPassword,
        /*oSuccessful*/  OUTPUT lMailOK,
        /*vMessage   */  OUTPUT cMessage) NO-ERROR.
/*         IF cFileName <> "" THEN         */
/*             OS-DELETE VALUE(cFileName). */
        IF lMailOK = TRUE THEN DO:
            OS-RENAME VALUE(cFileName) value(cBackupFile).
        END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sendGamlaFiler) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendGamlaFiler Procedure 
PROCEDURE sendGamlaFiler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cFilNavn   AS CHARACTER  NO-UNDO.
/* Dessa har satts tidigare: cOutputDir cBackupDir */

INPUT FROM OS-DIR(cOutputDir) NO-ECHO. 
REPEAT:
    SET cFilNavn FORMAT "X(50)".
     IF cFilNavn BEGINS "H" AND NUM-ENTRIES(cFilNavn,".") = 2 AND entry(2,cFilNavn,".") = "xml" THEN DO:
         CREATE tt_filer.
         ASSIGN tt_filer.filnavn = cFilnavn.
      END.
END.
INPUT CLOSE.    
FOR EACH tt_filer:
    ASSIGN cOutputFileName = cOutputDir + tt_filer.FilNavn
           cBackupFileName = cBackupDir + tt_filer.FilNavn.
     RUN sendEmail(cOutputFileName,cBackupFileName).
     IF SEARCH(cOutputFileName) <> ? THEN /* då gick det inte bra */
         LEAVE.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getDTstring) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDTstring Procedure 
FUNCTION getDTstring RETURNS CHARACTER
  ( INPUT dtVar AS DATETIME ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN  REPLACE(STRING(dtVar,"99-99-9999THH:MM:SS.SSS"),",",".").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFileName Procedure 
FUNCTION getFileName RETURNS CHARACTER
  ( /* */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST tt_Dealer.

  RETURN  REPLACE(REPLACE(STRING(tt_Dealer.FileCreationDate,"99999999 HH:MM:SS")," ",""),":","").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

