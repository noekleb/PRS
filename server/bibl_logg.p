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

DEF INPUT PARAMETER cLoggfil AS CHAR NO-UNDO.
DEF INPUT PARAMETER cTekst   AS CHAR NO-UNDO.

DEF VAR cDatoTid AS CHAR NO-UNDO.
DEF VAR cFilNavn AS CHAR NO-UNDO.
DEFINE VARIABLE cKatalog AS CHAR NO-UNDO.

DEF STREAM Ut.

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
/*
IF cLoggfil MATCHES "*Suppleringsordre*" THEN
    RETURN.
*/

{syspara.i 1 1 59 cKatalog}
IF cKatalog <> '' THEN DO:
    /* Sikrer at katalog finnes. */
    OS-CREATE-DIR VALUE(RIGHT-TRIM(cKatalog,'\')).    
    cKatalog = RIGHT-TRIM(cKatalog,'\') + '\'.
END.

ASSIGN
    cFilNavn = cKatalog + cLoggfil + '.log'
    cDatoTid = STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS") + ' '.

OUTPUT STREAM Ut TO VALUE(cFilNavn) APPEND UNBUFFERED NO-ECHO.

PUT STREAM Ut UNFORMATTED
    cDatoTid
    cTekst SKIP.

OUTPUT STREAM Ut CLOSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


