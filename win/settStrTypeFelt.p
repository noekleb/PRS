&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     : Assignar StrType.Intervall 
                           StrType.Fordeling
    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER iStrTypeID     LIKE StrType.StrTypeID NO-UNDO.
DEFINE        VARIABLE  cFordeling     AS CHARACTER           NO-UNDO.
DEFINE        VARIABLE  cIntervall     AS CHARACTER           NO-UNDO.
DEFINE        VARIABLE  cAlfaFordeling AS CHARACTER  NO-UNDO.

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

RUN AssignInfo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-AssignInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AssignInfo Procedure 
PROCEDURE AssignInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND StrType WHERE StrType.StrTypeID = iStrTypeID NO-LOCK NO-ERROR.
    IF NOT AVAIL StrType THEN
        RETURN.
    
    FIND FIRST StrTStr OF StrType NO-LOCK WHERE  
        StrTStr.SoStorl <> ? NO-ERROR.
    IF NOT AVAIL StrTStr THEN DO:
        FIND CURRENT StrType.
        ASSIGN StrType.Intervall     = ""
               StrType.Fordeling     = ""
               StrType.AlfaFordeling = "".
        RETURN.
    END.
    
    /* Setter Intevall */
    ASSIGN 
        cIntervall = StrTStr.SoStorl.
    FIND LAST StrTStr OF StrType WHERE 
        StrTStr.SoStorl <> ? NO-LOCK NO-ERROR.
   ASSIGN 
    cIntervall = cIntervall + " - " + StrTStr.SoStorl.
        
    FOR EACH StrTStr OF StrType WHERE 
        StrTStr.SoStorl <> ? NO-LOCK.
        
        FIND StrKonv WHERE 
            StrKonv.Storl = StrTStr.SoStorl NO-LOCK NO-ERROR.
        IF AVAIL StrKonv THEN
            ASSIGN cFordeling     = cFordeling + (IF cFordeling = "" THEN "" ELSE ",") + STRING(StrKonv.StrKode)
                   cAlfaFordeling = cAlfaFordeling + (IF cAlfaFordeling = "" THEN "" ELSE ",") + StrKonv.Storl.
    END.
    FIND CURRENT StrType.
    ASSIGN StrType.Intervall     = cIntervall
           StrType.Fordeling     = cFordeling
           StrType.AlfaFordeling = cAlfaFordeling.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

