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
DEFINE VARIABLE cPF      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ii       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTimeOut AS INTEGER     NO-UNDO.
DEFINE VARIABLE cDB      AS CHARACTER   NO-UNDO.

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

DO ii = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
    IF ENTRY(ii,SESSION:PARAMETER) BEGINS "PF" THEN
        cPF = ENTRY(2,ENTRY(ii,SESSION:PARAMETER),"=") NO-ERROR.
    ELSE IF ENTRY(ii,SESSION:PARAMETER) BEGINS "TIMEOUTSEK" THEN
        iTimeOut = INT(ENTRY(2,ENTRY(ii,SESSION:PARAMETER),"=")) NO-ERROR.
    ELSE IF ENTRY(ii,SESSION:PARAMETER) BEGINS "DB" THEN
        cDB = ENTRY(2,ENTRY(ii,SESSION:PARAMETER),"=") NO-ERROR.
END.
IF SEARCH(cPF) = ? OR iTimeOut = 0 OR iTimeout = ? THEN
    RETURN.
ETIME(TRUE).

REPEAT WHILE ETIME < iTimeOut * 1000:
    RUN connectDB.p (cPF).
    IF CONNECTED(cDB) THEN
        LEAVE.
    PAUSE 1 NO-MESSAGE.
END.
IF NOT CONNECTED(cDB) THEN
    RETURN.
RUN w-ScantechPcheck.w.
QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


