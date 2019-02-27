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
DEFINE VARIABLE iAntVentDTL AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAntVentN9  AS INTEGER    NO-UNDO.
DEFINE VARIABLE lVisOK      AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lSendEmail  AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cMailTo     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cMailhub   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEMailFrom AS CHARACTER   NO-UNDO.

DEFINE VARIABLE hSource     AS HANDLE     NO-UNDO.

DEFINE VARIABLE iK_ModellDTL AS INTEGER INIT 50   NO-UNDO.
DEFINE VARIABLE iK_ModellN9  AS INTEGER INIT 51   NO-UNDO.

DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

DEFINE VARIABLE lMailOK AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cMessage    AS CHARACTER  NO-UNDO.

DEFINE VARIABLE dRappDato AS DATE       NO-UNDO.

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
ASSIGN cOutputDir  = ?
       dRappDato   = TODAY - 1.
IF NOT THIS-PROCEDURE:PERSISTENT THEN
    RUN InitParaVariabler.
ELSE DO:
    hSource = SOURCE-PROCEDURE.
    RUN GetParaFromParent.
END.
/*                                                             */
IF cOutputDir  = ? THEN DO:

    MESSAGE "Fel parametertabell:"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
ELSE DO:
    RUN Rapport.
/* per-ake.stalberg@preem.se,g.jansbo@polygon.se */
    IF lSendEmail THEN DO:
        RUN prssmtpmailv5_7a.p (
    /*mailhub    */   cMailhub,
    /*EmailTo    */   cMailTo,
    /*EmailFrom  */   cEMailFrom,
    /*EmailCC    */   "",
    /*Attachments*/   IF cFilename = "" THEN "" ELSE ENTRY(NUM-ENTRIES(cFileName,"\"),cFileName,"\"),
    /*LocalFiles */   IF cFilename = "" THEN "" ELSE cFileName,
    /*Subject    */   "Kommissionsdata till datalager " + STRING(dRappDato) + (IF cFilename = "" THEN "- Alla OK" ELSE ""),
    /*Body       */   "",
    /*MIMEHeader */   "",
    /*BodyType   */   "",
    /*Importance */   0,
    /*L_DoAUTH   */   FALSE,
    /*C_AuthType */   "",
    /*C_User     */   "",
    /*C_Password */   "",
    /*oSuccessful*/  OUTPUT lMailOK,
    /*vMessage   */  OUTPUT cMessage) NO-ERROR.
        IF cFileName <> "" THEN
            OS-DELETE VALUE(cFileName).
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
 IF CAN-DO(hSource:INTERNAL-ENTRIES,"SendDatalagerPara") THEN DO:
     RUN SendDatalagerPara IN hSource
     (OUTPUT cOutputDir,
      OUTPUT lSendEmail,
      OUTPUT cMailTo).
     /*  */
     FIND Syspara WHERE SysPara.SysHId         = 50 AND
                                SysPara.SysGr  = 50 AND
                                SysPara.ParaNr = 1  NO-LOCK NO-ERROR.
     IF AVAIL SysPara THEN
         ASSIGN cMailhub    = Syspara.parameter1.
     {syspara.i 50 50 40 cEmailFrom}

 END.
 ASSIGN cOutputDir = RIGHT-TRIM(cOutputDir,"\") + "\".

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
                               SysPara.SysGr  = 251 AND
                               SysPara.ParaNr = 1   NO-LOCK NO-ERROR.
    IF AVAIL SysPara THEN
        ASSIGN cOutputDir = SysPara.Parameter1
               cOutputDir = RIGHT-TRIM(cOutputDir,"\") + "\".
    
    FIND Syspara WHERE SysPara.SysHId         = 210 AND
                               SysPara.SysGr  = 251 AND
                               SysPara.ParaNr = 5   NO-LOCK NO-ERROR.
    IF AVAIL SysPara THEN
        ASSIGN lSendEmail = Syspara.parameter1 = "J".

    FIND Syspara WHERE SysPara.SysHId         = 210 AND
                               SysPara.SysGr  = 251 AND
                               SysPara.ParaNr = 10  NO-LOCK NO-ERROR.
    IF AVAIL SysPara THEN
        ASSIGN cMailTo    = Syspara.parameter1.

    FIND Syspara WHERE SysPara.SysHId         = 50 AND
                               SysPara.SysGr  = 50 AND
                               SysPara.ParaNr = 1  NO-LOCK NO-ERROR.
    IF AVAIL SysPara THEN
        ASSIGN cMailhub    = Syspara.parameter1.
    {syspara.i 50 50 40 cEmailFrom}
/*     DO ii = 1 TO NUM-ENTRIES(SESSION:PARAMETER):                             */
/*         CASE ENTRY(1,ENTRY(ii,SESSION:PARAMETER),"="):                       */
/*             WHEN "ODIR" THEN                                                 */
/*                 ASSIGN cOutputDir = ENTRY(2,ENTRY(ii,SESSION:PARAMETER),"=") */
/*                        cOutputDir = RIGHT-TRIM(cOutputDir,"\") + "\".        */
/*         END CASE.                                                            */
/*     END.                                                                     */
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
    DEFINE VARIABLE lDataUt AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE dDato AS DATE       NO-UNDO.
    DEFINE VARIABLE cKassenr AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER    NO-UNDO.

    cFileName = cOutputDir + "BFDAG-" + STRING(YEAR(dRappDato),"9999") + "-" + STRING(MONTH(dRappDato),"99") + "-" + STRING(DAY(dRappDato),"99") + ".xls".
    OUTPUT TO VALUE(cFileName).
    PUT UNFORMATTED CHR(9) "Bokföringsdagkontroll-" STRING(dRappDato) SKIP.
    FOR EACH Butiker NO-LOCK WHERE EODRapport = TRUE BY Butiker.butik:
        IF Butiker.NedlagtDato <> ? AND Butiker.NedlagtDato < TODAY THEN
            NEXT.
/*         FOR EACH kasse WHERE kasse.butikknr = butiker.butik NO-LOCK:                            */
/*             cKasseNr = cKasseNr + (IF cKassenr <> "" THEN "," ELSE "") + STRING(kasse.kassenr). */
/*         END.                                                                                    */
/*         dDato = DATE(1,1,2000).                                                 */
/*         DO ii = 1 TO NUM-ENTRIES(cKassenr):                                     */
/*             FIND LAST bonghode WHERE bonghode.butikknr = butiker.butik AND      */
/*                                      bonghode.gruppenr = 1 AND                  */
/*                                      bonghode.kassenr = INT(ENTRY(ii,cKassenr)) */
/*                                      NO-LOCK NO-ERROR.                          */
/*             IF AVAIL Bonghode AND bonghode.dato > dDato THEN                    */
/*                 dDato = bonghode.dato.                                          */
/*         END.                                                                    */
        FIND LAST Bokforingsdag WHERE bokforingsdag.ButikkNr = Butiker.Butik NO-LOCK NO-ERROR.
        IF (AVAIL Bokforingsdag AND (Bokforingsdag.dato < dRappDato OR Bokforingsdag.pfflagg <> 3)) OR NOT AVAIL Bokforingsdag THEN DO:
            IF lDataUt = FALSE THEN
                PUT UNFORMATTED "Butik" CHR(9) "Namn" CHR(9) "Sista bf-dag" CHR(9) "Uppdaterad" SKIP.
            lDataUt = TRUE.
            PUT UNFORMATTED Butiker.butik CHR(9) Butiker.Butnamn CHR(9) (IF AVAIL Bokforingsdag THEN "'" + STRING(Bokforingsdag.dato) ELSE "Data saknas") CHR(9) 
                                       (IF NOT AVAIL bokforingsdag THEN " " ELSE STRING(bokforingsdag.pfflagg = 3,"/N")) SKIP.
        END.
/*     /*     FIND LAST datasett WHERE datasett.butikknr = butiker.butik USE-INDEX Apningsskjema NO-LOCK NO-ERROR. */      */
/*         DISP Butiker.butik Butiker.Butnamn STRING(Bokforingsdag.dato <> TODAY - 1,"Ja/") LABEL "FEL" bokforingsdag.dato */
/*             /* dDato LABEL "Siste bong" */ datasett.dato LABEL "Siste datasett".                                        */
    END.
    OUTPUT CLOSE.
    IF lDataUt = FALSE THEN DO:
        OS-DELETE VALUE(cFileName).
        cFilename = "".
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

