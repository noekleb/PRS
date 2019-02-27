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

DEFINE INPUT  PARAMETER ipArtikkelNr AS DECIMAL    NO-UNDO.

DEFINE VARIABLE cGenInterleave AS CHARACTER  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fInterleave) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fInterleave Procedure 
FUNCTION fInterleave RETURNS CHARACTER
  ( INPUT iStrKode    AS INT )  FORWARD.

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

{syspara.i 2 4 17 cGenInterleave}

IF cGenInterleave <> "1" THEN
    RETURN.

FIND artbas WHERE artbas.artikkelnr = ipArtikkelNr NO-LOCK NO-ERROR.
IF NOT AVAIL ArtBas OR ArtBas.LopNr = ? OR ArtBas.Vg > 999 OR 
                       ArtBas.LopNr > 9999 THEN
    RETURN.

RUN FixInterleave.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-FixInterleave) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixInterleave Procedure 
PROCEDURE FixInterleave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH StrekKode OF ArtBas WHERE StrekKode.StrKode > 0:
        /*IF (StrekKode.Bestillingsnummer = "" OR 
            LENGTH(Strekkode.Bestillingsnummer) = 10) THEN*/
            ASSIGN StrekKode.Bestillingsnummer = fInterleave(StrekKode.StrKode).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fInterleave) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fInterleave Procedure 
FUNCTION fInterleave RETURNS CHARACTER
  ( INPUT iStrKode    AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cStrl AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iTst  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cKode AS CHARACTER  NO-UNDO.

  KODEBLOKK:
  DO:
    FIND StrKonv WHERE 
        StrKonv.StrKode = iStrKode USE-INDEX StrKode NO-LOCK NO-ERROR.
    IF NOT AVAIL StrKonv THEN
        LEAVE KODEBLOKK.
    ASSIGN cStrl = IF NUM-ENTRIES(StrKonv.Storl,".") = 2 THEN
        TRIM(REPLACE(StrKonv.Storl,".","")) ELSE TRIM(StrKonv.Storl) + "0"
           cStrl = FILL("0",4 - LENGTH(cStrl)) + cStrl
           cKode = STRING(ArtBas.Vg,"999")     +
                   STRING(ArtBas.LopNr,"9999") +
                   "0" +
                   cStrl.
  END. /* KODEBLOKK */
  RETURN cKode.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

