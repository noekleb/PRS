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

DEFINE VARIABLE cDirName    AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cParaButiker AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cParaDato    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iDayDelay    AS INTEGER    NO-UNDO. /* antal dagar tillbaka -> syspara ex. 2 = today - 2 */

DEFINE VARIABLE dFirstDate AS DATE       NO-UNDO.
DEFINE VARIABLE clFtp       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFtphost    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFtpbruker  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFtppassord AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lFtp        AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cPffil AS CHARACTER  NO-UNDO.

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

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* Här kontrollerar vi om skotex är conectad annars kör connect */
RUN KontrollerSysPara.
RUN GetSyspara.
RUN ExporterData.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExporterData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExporterData Procedure 
PROCEDURE ExporterData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ii        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE dFGdato   AS DATE       NO-UNDO.
    DEFINE VARIABLE dDato     AS DATE       NO-UNDO.
    DEFINE VARIABLE iButikkNr AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iMinusDagar AS INTEGER     NO-UNDO.
    iMinusDagar = IF WEEKDAY(TODAY) = 7 THEN 10 ELSE 2.
    DO ii = 1 TO NUM-ENTRIES(cParaButiker):
        ASSIGN iButikkNr = INT(ENTRY(ii,cParaButiker))
               dFGdato   = DATE(ENTRY(ii,cParaDato)).
        IF TODAY - iDayDelay = dFGdato THEN
            NEXT.
        IF dFGdato > TODAY - iDayDelay - iMinusDagar THEN
            dFGdato = dFGdato - iMinusDagar.
        DO dDato = dFGdato + 1 TO TODAY - iDayDelay:

            RUN xartstat2timegrip.p (cDirName,iButikkNr,dDato,lFtp,cFtphost,cFtpbruker,cFtppassord).
            FIND Syspara WHERE SysPara.SysHId = 210 AND
                               SysPara.SysGr  = 200 AND 
                               SysPara.ParaNr = iButikkNr.
            ASSIGN Syspara.parameter1 = string(dDato).
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetSyspara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSyspara Procedure 
PROCEDURE GetSyspara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dDato AS DATE       NO-UNDO.

    FOR EACH Syspara WHERE SysPara.SysHId = 210 AND
                   SysPara.SysGr  = 200 NO-LOCK:
        dDato = DATE(Syspara.parameter1) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            NEXT.
        ASSIGN cParaButiker = cParaButiker + (IF cParaButiker <> "" THEN "," ELSE "") + STRING(SysPara.ParaNr)
               cParaDato    = cParaDato + (IF cParaDato <> "" THEN "," ELSE "") + SysPara.Parameter1.
    END.
    {syspara.i 210 201 1 cDirName}
    {syspara.i 210 201 2 iDayDelay INT}

    {syspara.i 210 201 5 clFtp}
    {syspara.i 210 201 6 cFtphost}
    {syspara.i 210 201 10 cFtpbruker}
    {syspar2.i 210 201 10 cFtppassord}
    lFtp = clFtp = "1".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KontrollerSysPara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerSysPara Procedure 
PROCEDURE KontrollerSysPara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {syspara.i 210 201 3 dFirstDate DATE}
    FOR EACH butiker NO-LOCK WHERE CAN-FIND(FIRST Kasse WHERE Kasse.butik = butiker.butik AND Kasse.aktiv = TRUE):
        FIND Syspara WHERE SysPara.SysHId = 210 AND
                           SysPara.SysGr  = 200 AND
                           SysPara.ParaNr = butiker.butik NO-LOCK NO-ERROR.
        IF NOT AVAIL SysPara THEN DO:
            CREATE SysPara.
            ASSIGN SysPara.SysHId = 210
                   SysPara.SysGr  = 200
                   SysPara.ParaNr = butiker.butik
                   SysPara.Beskrivelse = butiker.butnamn
                   SysPara.Parameter1  = STRING(dFirstDate - 1) .
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

