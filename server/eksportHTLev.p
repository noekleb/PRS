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

DEFINE INPUT  PARAMETER iHTtype           AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER cEksportKatalog   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cEkspFilPrefix    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cEksFilEkstent    AS CHARACTER   NO-UNDO.
DEFINE        VARIABLE  cEksFilEkstentTmp AS CHARACTER   NO-UNDO.

DEFINE VARIABLE iLoop AS INTEGER     NO-UNDO.
DEFINE VARIABLE cFilNavn AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDir AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCL        AS INTEGER   NO-UNDO.
DEFINE VARIABLE pcTekst    AS CHARACTER NO-UNDO.
DEFINE STREAM Ut.

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

IF iHTtype = 10 THEN 
DO:
    RUN BxMobileUt. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BxMobileUt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BxMobileUt Procedure 
PROCEDURE BxMobileUt :
/*------------------------------------------------------------------------------
      Purpose:                                      
      Notes:                                      
DEFINE TEMP-TABLE tmpButiker
    /*  1 */ FIELD Butik    AS INT       FORMAT ">>>>>9"
    /*  2 */ FIELD ButNamn  AS CHARACTER FORMAT "x(30)"
    INDEX Butikk ButikkNr.
  ------------------------------------------------------------------------------*/

    ASSIGN
        cFilnavn = RIGHT-TRIM(RIGHT-TRIM(TRIM(cEksportKatalog), "\"),"/") + "\" +
                        cEkspFilPrefix + "." + ENTRY(1,cEksFilEkstent,',') + "_TMP"
        cDir = ENTRY(1,cFilnavn,"\") + "\".
    
    DO iLoop = 2 TO NUM-ENTRIES(cFilnavn,"\") - 1:
        cDir = cDir + ENTRY(iLoop,cFilnavn,"\") + "\".
        OS-CREATE-DIR VALUE(cDir).
    END.
    
    OUTPUT STREAM Ut TO VALUE(cFilnavn).

    LOOPEN:
    FOR EACH LevBas NO-LOCK WHERE
        LevBas.LevNr > 0:
        PUT STREAM Ut UNFORMATTED
            Levbas.LevNr ';'
            TRIM(REPLACE(LevBas.LevNamn,';','')) ';'
            TRIM(REPLACE(Levbas.LevAdr,';',''))
        SKIP.
    END. /* LOOPEN */

    OUTPUT STREAM Ut CLOSE.
    
    /* sist så skall ursprungligat tmp-filen byta namn */
    cFil = REPLACE(cFilnavn,"_TMP","").
    OS-DELETE VALUE(cFil). /* Tar bort eventuell gammel fil */
    OS-RENAME VALUE(cFilnavn) VALUE(cFil).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

