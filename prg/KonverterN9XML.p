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

DEFINE INPUT  PARAMETER cFilNavn AS CHARACTER  NO-UNDO.

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

RUN AnalyserKonverterFil.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-AnalyserKonverterFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AnalyserKonverterFil Procedure 
PROCEDURE AnalyserKonverterFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE cc AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cTMPFilnavn AS CHARACTER  NO-UNDO.
   cTMPFilnavn = cFilnavn + "TMP".
   INPUT FROM VALUE(cFilNavn).
   IMPORT UNFORMATTED cc NO-ERROR.
   /* Gamla formatet skall ge error */
   IF LENGTH(cc) = 0 OR LENGTH(cc) > 50 THEN DO:
       INPUT CLOSE.
       RETURN.
   END.
   INPUT CLOSE.
   OS-RENAME VALUE(cFilNavn) VALUE(cTMPFilnavn).
   OUTPUT TO VALUE(cTMPFilNavn) APPEND.
   PUT UNFORMATTED " " SKIP.
   OUTPUT CLOSE.
   INPUT FROM VALUE(cTMPFilnavn).
   OUTPUT TO VALUE(cFilNavn).
   REPEAT:
       IMPORT UNFORMATTED cc.
       cc = TRIM(cc).
       IF cc = "" THEN
           NEXT.
       PUT UNFORMATTED cc.
   END.
   INPUT CLOSE.
   OUTPUT CLOSE.
   OS-DELETE VALUE(cTMPFilNavn).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

