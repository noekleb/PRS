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

DEFINE OUTPUT PARAMETER cKundnamn  AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER dtAskTime  AS DATETIME    NO-UNDO.
DEFINE OUTPUT PARAMETER dtLastTime AS DATETIME    NO-UNDO.
DEFINE OUTPUT PARAMETER deMSgrens  AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER lOK        AS LOGICAL     NO-UNDO.
DEFINE OUTPUT PARAMETER cMessage   AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cMailTo AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cOrgDateFormat AS CHARACTER   NO-UNDO.

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
{syspara.i 1 1 100 cKundnamn}
{syspara.i 50 50 34 cMailTo}
    
dtAskTime = NOW.
cOrgDateFormat = SESSION:DATE-FORMAT.
SESSION:DATE-FORMAT = "ymd".
FIND SysPara WHERE
     SysPara.SysHId = 150 AND
     SysPara.SysGr  = 15 AND
     SysPara.ParaNr = 1 NO-LOCK NO-ERROR.
IF NOT AVAIL SysPara THEN DO:
    lOK = FALSE.
    cMessage = "SysPara 150 15 1 är inte skapad".
END.
ELSE DO:
    ASSIGN dtLastTime = DATETIME(SysPara.Parameter1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        lOK = FALSE.
        cMessage = "SysPara 150 15 1 Parameter1 har fel innehåll".
    END.
    ELSE DO:
        deMSgrens = DECI(SysPara.Parameter2) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            lOK = FALSE.
            cMessage = "SysPara 150 15 1 Parameter2 har fel innehåll".
        END.
        ELSE DO:
            IF cMailTo = "" THEN DO:
                lOK = FALSE.
                cMessage = "SysPara 50 50 34 Parameter1, maillista saknas".
            END.
            ELSE
                 lOK = TRUE.
        END.
    END.
END.
SESSION:DATE-FORMAT = cOrgDateFormat.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


