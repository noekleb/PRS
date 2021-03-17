&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : flytt_medlem_til_ny_kunde.p
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

DEF INPUT  PARAMETER wMedlemsNr  LIKE Medlem.MedlemsNr NO-UNDO.
DEF INPUT  PARAMETER wKundeNr    LIKE Kunde.KundeNr    NO-UNDO.
DEF INPUT  PARAMETER wOldKundeNr LIKE Kunde.KundeNr    NO-UNDO.
DEF OUTPUT PARAMETER wOk         AS LOG NO-UNDO.

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

RUN FlyttMedlemTilKunde.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-FlyttMedlemTilKunde) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlyttMedlemTilKunde Procedure 
PROCEDURE FlyttMedlemTilKunde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO TRANSACTION:
    FIND Medlem EXCLUSIVE-LOCK WHERE Medlem.MedlemsNr = wMedlemsNr NO-ERROR.
    IF NOT AVAILABLE Medlem THEN
        LEAVE.

    IF NOT CAN-FIND(Kunde WHERE
                    Kunde.KundeNr = wOldKundeNr) OR
        NOT CAN-FIND(Kunde WHERE
                     Kunde.KundeNr = wKundeNr) THEN
        LEAVE.

    ASSIGN
        Medlem.KundeNr = wKundeNr
        .
    FOR EACH MedlemsKort OF Medlem NO-LOCK WHERE
        MedlemsKort.InterntKKortId > 0:
        FIND KundeKort EXCLUSIVE-LOCK WHERE
            KundeKort.InterntKKortId = MedlemsKort.InterntKKortId NO-ERROR.
        IF AVAILABLE kundeKort THEN
            KundeKort.KundeNr = wKundeNr.
    END.
    ASSIGN
        wOk = TRUE.
END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

