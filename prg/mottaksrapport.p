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

DEFINE VARIABLE cOutputDir  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iAntVentPRS AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAntVentI80 AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAntVentDTL AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAntVentN9  AS INTEGER    NO-UNDO.
DEFINE VARIABLE lVisOK      AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lSendEmail  AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cMailTo AS CHARACTER  NO-UNDO.

DEFINE VARIABLE hSource AS HANDLE     NO-UNDO.

DEFINE VARIABLE iK_ModellPRS AS INTEGER INIT  5   NO-UNDO.
DEFINE VARIABLE iK_ModellI80 AS INTEGER INIT 10   NO-UNDO.
DEFINE VARIABLE iK_ModellDTL AS INTEGER INIT 50   NO-UNDO.
DEFINE VARIABLE iK_ModellN9  AS INTEGER INIT 51   NO-UNDO.

DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

DEFINE VARIABLE lMailOK AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cMessage    AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cMailhub  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDoAUTH   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAuthType AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUser     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPassword AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEmailCC  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEmailELoggserver AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEmailFrom AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lAllaOK   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cKassaFilDir AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUnknownStr  AS CHARACTER   NO-UNDO.
DEFINE TEMP-TABLE TT_Ingen_But NO-UNDO
    FIELD cbutik AS CHAR.

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
    {syspara.i 50 50 1 cMailhub }
    {syspara.i 50 50 2 cDoAUTH  }
    {syspara.i 50 50 3 cAuthType}
    {syspara.i 50 50 4 cUser    }
    {syspara.i 50 50 5 cPassword}
    {syspar2.i 50 50 20 cEmailCC}
    {syspara.i 50 50 6 cEmailELoggserver}
    {syspara.i 50 50 40 cEmailFrom}
    {syspar2.i 1 1 56 cKassaFilDir}
    
IF cKassaFilDir = ? THEN
    cKassaFilDir = "".
cKassaFilDir = RIGHT-TRIM(cKassaFilDir,"\").
IF cDoAUTH = "0" THEN
    ASSIGN cDoAUTH   = "FALSE"
           cAuthType = ""
           cUser     = ""
           cPassword = "".
ELSE
    cDoAUTH = "TRUE".
ASSIGN cOutputDir  = ?
       iAntVentPRS = ?
       iAntVentI80 = ?
       iAntVentDTL = ?
       iAntVentN9  = ?
       lVisOK      = ?.
IF NOT THIS-PROCEDURE:PERSISTENT THEN
    RUN InitParaVariabler.
ELSE DO:
    hSource = SOURCE-PROCEDURE.
    RUN GetParaFromParent.
END.
IF cOutputDir  = ? OR
   iAntVentPRS = ? OR
   iAntVentI80 = ? OR 
   iAntVentDTL = ? OR
   iAntVentN9  = ? OR 
   lVisOK      = ? THEN DO:

    MESSAGE "Fel i parametrar" SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
ELSE DO:
    RUN UnknownBut.
    RUN Rapport.
/* per-ake.stalberg@preem.se */
    IF lSendEmail THEN DO:
        RUN prssmtpmailv5_7a.p (
        /*mailhub    */   cMailhub,
        /*EmailTo    */   cMailTo,
        /*EmailFrom  */   cEmailFrom,
        /*EmailCC    */   "",
        /*Attachments*/   ENTRY(NUM-ENTRIES(cFileName,"\"),cFileName,"\"),
        /*LocalFiles */   cFileName,
        /*Subject    */   "Mottagsrapport" + (IF lAllaOK = TRUE THEN " (Alla OK)" ELSE "") + cUnknownStr,
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
        IF cFileName <> "" THEN
            OS-DELETE VALUE(cFileName).
/*         IF lMailOK = FALSE THEN                    */
/*             MESSAGE cMessage                       */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    END.
END.
IF NOT THIS-PROCEDURE:PERSISTENT THEN
    QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-GetParaFromParent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetParaFromParent Procedure 
PROCEDURE GetParaFromParent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF CAN-DO(hSource:INTERNAL-ENTRIES,"SendMottakPara") THEN DO:
     RUN SendMottakPara IN hSource
     (OUTPUT cOutputDir,
      OUTPUT iAntVentDTL,
      OUTPUT iAntVentN9,
      OUTPUT iAntVentPRS,
      OUTPUT iAntVentI80,
      OUTPUT lVisOK,
      OUTPUT lSendEmail,
      OUTPUT cMailTo).
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitParaVariabler) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitParaVariabler Procedure 
PROCEDURE InitParaVariabler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
    FIND Syspara WHERE SysPara.SysHId         = 210 AND
                               SysPara.SysGr  = 250 AND
                               SysPara.ParaNr = 1   NO-LOCK NO-ERROR.
    IF AVAIL SysPara THEN
        ASSIGN cOutputDir = SysPara.Parameter1
               cOutputDir = RIGHT-TRIM(cOutputDir,"\") + "\".
    
    FIND Syspara WHERE SysPara.SysHId         = 210 AND
                               SysPara.SysGr  = 250 AND
                               SysPara.ParaNr = 2   NO-LOCK NO-ERROR.
    IF AVAIL SysPara THEN
        ASSIGN iAntVentDTL = INT(Syspara.parameter1).
    
    FIND Syspara WHERE SysPara.SysHId         = 210 AND
                               SysPara.SysGr  = 250 AND
                               SysPara.ParaNr = 3   NO-LOCK NO-ERROR.
    IF AVAIL SysPara THEN
        ASSIGN iAntVentN9 = INT(Syspara.parameter1).
    
    FIND Syspara WHERE SysPara.SysHId         = 210 AND
                               SysPara.SysGr  = 250 AND
                               SysPara.ParaNr = 4   NO-LOCK NO-ERROR.
    IF AVAIL SysPara THEN
        ASSIGN lVisOK     = Syspara.parameter1 = "J".
    
    FIND Syspara WHERE SysPara.SysHId         = 210 AND
                               SysPara.SysGr  = 250 AND
                               SysPara.ParaNr = 5   NO-LOCK NO-ERROR.
    IF AVAIL SysPara THEN
        ASSIGN lSendEmail = Syspara.parameter1 = "J".
    FIND Syspara WHERE SysPara.SysHId         = 210 AND
                               SysPara.SysGr  = 250 AND
                               SysPara.ParaNr = 10  NO-LOCK NO-ERROR.
    IF AVAIL SysPara THEN
        ASSIGN cMailTo    = Syspara.parameter1.


    FIND Syspara WHERE SysPara.SysHId         = 210 AND
                               SysPara.SysGr  = 250 AND
                               SysPara.ParaNr = 12   NO-LOCK NO-ERROR.
    IF AVAIL SysPara THEN
        ASSIGN iAntVentPRS = INT(Syspara.parameter1).

    FIND Syspara WHERE SysPara.SysHId         = 210 AND
                               SysPara.SysGr  = 250 AND
                               SysPara.ParaNr = 13   NO-LOCK NO-ERROR.
    IF AVAIL SysPara THEN
        ASSIGN iAntVentI80 = INT(Syspara.parameter1).

    /*
    DO ii = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
        CASE ENTRY(1,ENTRY(ii,SESSION:PARAMETER),"="):
            WHEN "ODIR" THEN
                ASSIGN cOutputDir = ENTRY(2,ENTRY(ii,SESSION:PARAMETER),"=")
                       cOutputDir = RIGHT-TRIM(cOutputDir,"\") + "\".
            WHEN "DTLDAR" THEN
                ASSIGN iAntVentDTL = INT(ENTRY(2,ENTRY(ii,SESSION:PARAMETER),"=")).
            WHEN "N9DAR" THEN
                ASSIGN iAntVentN9 = INT(ENTRY(2,ENTRY(ii,SESSION:PARAMETER),"=")).
            WHEN "VISOK" THEN
                ASSIGN lVisOK     = ENTRY(2,ENTRY(ii,SESSION:PARAMETER),"=") = "J".
        END CASE.
    END.
*/    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Rapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rapport Procedure 
PROCEDURE Rapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iVentdagar AS INTEGER    NO-UNDO.
    DEFINE VARIABLE dDatoKoll  AS DATE       NO-UNDO.
    DEFINE VARIABLE iLookup1   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iLookup2   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iSaknadDag AS INTEGER    NO-UNDO.
    DEFINE VARIABLE d31DecFgAr AS DATE       NO-UNDO.
    DEFINE VARIABLE cKassaTyp  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cAllaOK    AS CHARACTER   NO-UNDO.
    cFileName = cOutputDir + "Mottag-" + STRING(YEAR(TODAY),"9999") + "-" + STRING(MONTH(TODAY),"99") + "-" + STRING(DAY(TODAY),"99") + ".xls".
    lAllaOK = TRUE.
    OUTPUT TO VALUE(cFileName).
    PUT UNFORMATTED CHR(9) "Datamottagskontroll-" STRING(TODAY) SKIP.

    FOR EACH Butiker NO-LOCK:
        IF Butiker.NedlagtDato <> ? AND Butiker.NedlagtDato < TODAY THEN
            NEXT.
        IF CAN-FIND(FIRST Kasse WHERE kasse.butikknr = Butiker.butik AND
                                      kasse.modellnr = iK_ModellDTL  AND
                                      kasse.aktiv    = TRUE) THEN DO:
            ASSIGN dDatoKoll = TODAY - iAntVentDTL
                   cKassaTyp = "DTL".
        END.
        ELSE IF CAN-FIND(FIRST Kasse WHERE kasse.butikknr = Butiker.butik AND
                                      kasse.modellnr = iK_ModellN9        AND
                                      kasse.aktiv    = TRUE) THEN DO:
            ASSIGN dDatoKoll = TODAY - iAntVentN9
                   cKassaTyp = "XML".
        END.
        ELSE IF CAN-FIND(FIRST Kasse WHERE kasse.butikknr = Butiker.butik AND
                                      kasse.modellnr = iK_ModellPRS       AND
                                      kasse.aktiv    = TRUE) THEN DO:
            ASSIGN dDatoKoll = TODAY - iAntVentPRS
                   cKassaTyp = "PRS".
        END.
        ELSE IF CAN-FIND(FIRST Kasse WHERE kasse.butikknr = Butiker.butik AND
                                      kasse.modellnr = iK_ModellI80       AND
                                      kasse.aktiv    = TRUE) THEN DO:
            ASSIGN dDatoKoll = TODAY - iAntVentI80
                   cKassaTyp = "I80".
        END.
        ELSE DO:
            PUT UNFORMATTED Butiker.butik CHR(9) Butiker.Butnamn CHR(9) STRING(Butiker.EODRapporter,"Ja/") CHR(9) " " CHR(9) "FEL" CHR(9) "Ingen kassa" SKIP.
            lAllaOK = FALSE.
            NEXT.
        END.
        FIND ApnSkjema WHERE ApnSkjema.ButikkNr = Butiker.Butik AND
                             ApnSkjema.Ar       = YEAR(dDatokoll) NO-LOCK NO-ERROR.
        IF NOT AVAIL ApnSkjema THEN DO:
            PUT UNFORMATTED Butiker.butik CHR(9) Butiker.Butnamn CHR(9) STRING(Butiker.EODRapporter,"Ja/") CHR(9) cKassaTyp chr(9) "FEL" CHR(9) "Inget schema för år " YEAR(dDatokoll) SKIP.
            lAllaOK = FALSE.
        END.
        ELSE DO:
            IF CAN-DO(ApnSkjema.OpenClosed,"1") OR CAN-DO(ApnSkjema.OpenClosed,"2") THEN DO:
                ASSIGN iLookup1 = LOOKUP("1",ApnSkjema.OpenClosed)
                       iLookup2 = LOOKUP("2",ApnSkjema.OpenClosed).

                ASSIGN iSaknadDag = IF iLookUp1 = 0 THEN iLookup2 ELSE
                                    IF iLookup2 = 0 THEN iLookUp1 ELSE
                                    MIN(iLookup1,iLookup2).
                ASSIGN d31DecFgAr = DATE(12,31,ApnSkjema.Ar - 1).
                IF d31DecFgAr + iSaknadDag < dDatoKoll THEN DO:
                     PUT UNFORMATTED Butiker.butik CHR(9) Butiker.Butnamn CHR(9) STRING(Butiker.EODRapporter,"Ja/") CHR(9) cKassaTyp chr(9) "FEL" CHR(9) "Första saknade dag: " STRING(d31DecFgAr + iSaknadDag,"99-99-99") SKIP.
                     lAllaOK = FALSE.
                END.
                ELSE IF lVisOK = TRUE THEN DO:
                    PUT UNFORMATTED Butiker.butik CHR(9) Butiker.Butnamn CHR(9) STRING(Butiker.EODRapporter,"Ja/") CHR(9) cKassaTyp chr(9) "OK" CHR(9) "'" dDatoKoll SKIP.
                    lAllaOK = FALSE.
                END.
            END.
            ELSE IF lVisOK = TRUE THEN
                PUT UNFORMATTED Butiker.butik CHR(9) Butiker.Butnamn CHR(9) STRING(Butiker.EODRapporter,"Ja/") CHR(9) cKassaTyp chr(9) "OK" CHR(9) "'" "'" dDatoKoll SKIP.
        END.
    END.
    FIND TT_Ingen_But NO-ERROR.
    IF AVAIL TT_Ingen_But OR AMBIG TT_Ingen_But THEN DO:
        IF AVAIL TT_Ingen_But THEN
            cUnknownStr = " + Okänd butik".
        ELSE
            cUnknownStr = " + Okända butiker".
        PUT UNFORMATTED " " SKIP.
        PUT UNFORMATTED " " CHR(9) "Filer från okända butiker" SKIP.
        PUT UNFORMATTED " " CHR(9) cKassaFilDir SKIP.
        FOR EACH TT_Ingen_But:
            PUT UNFORMATTED TT_Ingen_But.cButik SKIP.
        END.
    END.
    OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UnknownBut) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UnknownBut Procedure 
PROCEDURE UnknownBut :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cfile-name AS CHARACTER FORMAT "x(30)" LABEL "File". 
DEFINE VARIABLE attr-list AS CHARACTER FORMAT "x(4)" LABEL "Attributes".
DEFINE VARIABLE iButNr AS INTEGER     NO-UNDO.
DEFINE VARIABLE cexcludelist AS CHARACTER   NO-UNDO.
INPUT FROM OS-DIR(cKassaFilDir).
REPEAT:
   IMPORT cfile-name ^ attr-list.
   IF attr-list <> "D" OR cfile-name = "." OR cfile-name = ".." THEN
       NEXT.
   iButNr = INT(cfile-name) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
       NEXT.
   IF CAN-FIND(Butiker WHERE Butiker.butik = iButNr) THEN
       NEXT.
   CREATE TT_Ingen_But.
   ASSIGN TT_Ingen_But.cbutik = "'" + cfile-name.
END.
INPUT CLOSE.
/* DEFINE VARIABLE cUnknownStr  AS CHARACTER   NO-UNDO. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

