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

DEF INPUT PARAMETER lArtikkelNr AS DEC FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEF INPUT PARAMETER iLopNr      AS INT FORMAT "->>>>>9"       NO-UNDO.

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

DO FOR ArtBas TRANSACTION:
    FIND ArtBas EXCLUSIVE-LOCK WHERE 
        ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
    IF AVAILABLE ArtBas AND iLopNr > 0 THEN
    ASS_BLOKK:
    DO:
        ASSIGN ArtBas.LopNr = iLopNr NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            LEAVE ASS_BLOKK.
        ELSE 
        FOR EACH ArtLag EXCLUSIVE-LOCK WHERE
            ArtLag.ArtikkelNr = ArtBas.ArtikkelNr:
            ASSIGN
                ArtLag.Vg    = ArtBas.Vg
                ArtLag.LopNr = ArtBas.LopNr
                .
        END.
    END. /* ASS_BLOKK */
    IF AVAILABLE ArtBas THEN
        RELEASE ArtBas.
END. /* TRANSACTION */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


