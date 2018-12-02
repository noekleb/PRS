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

DEF INPUT PARAMETER gB_Id AS DEC NO-UNDO.

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

RUN BongSjekk.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BongSjekk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BongSjekk Procedure 
PROCEDURE BongSjekk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO TRANSACTION:
    FIND BongHode EXCLUSIVE-LOCK WHERE
        BongHode.B_Id = gB_Id NO-ERROR.
    IF NOT AVAILABLE BongHode THEN
        RETURN "AVBRYT".

    /* Betalt med Bankkort - Trukket direkte fra konto. */
    IF CAN-FIND(FIRST BongLinje WHERE
                BongLinje.B_Id = BongHode.B_Id AND
                BongLinje.TTID = 58) THEN
        BongHode.flBankkort = TRUE.

    /* Betalt med Betalingskort. */
    IF CAN-FIND(FIRST BongLinje WHERE
                BongLinje.B_Id = BongHode.B_Id AND
                BongLinje.TTID = 51) THEN
        BongHode.flBetalingskort = TRUE.
    
    /* Betalt med Gavekort. */
    IF CAN-FIND(FIRST BongLinje WHERE
                BongLinje.B_Id = BongHode.B_Id AND
                BongLinje.TTID = 53) THEN
        BongHode.flGavekort = TRUE.
    
    /* Betalt med Kreditkort. */
    IF CAN-FIND(FIRST BongLinje WHERE
                BongLinje.B_Id = BongHode.B_Id AND
                BongLinje.TTID = 52) THEN
        BongHode.flKreditkort = TRUE.
    
    /* Betalt med Kupong1. */
    IF CAN-FIND(FIRST BongLinje WHERE
                BongLinje.B_Id = BongHode.B_Id AND
                BongLinje.TTID = 56) THEN
        BongHode.flKupong1 = TRUE.
    
    /* Er gitt rabatt. */
    IF CAN-FIND(FIRST BongLinje WHERE
                BongLinje.B_Id = BongHode.B_Id AND
                BongLinje.TTID = 1 AND
                (BongLinje.LinjeRab + BongLinje.SubtotalRab) > 0) THEN
        BongHode.flRabatt = TRUE.
    
    /* Betalt med rekvisisjon. */
    IF CAN-FIND(FIRST BongLinje WHERE
                BongLinje.B_Id = BongHode.B_Id AND
                BongLinje.TTID = 55) THEN
        BongHode.flRekvisisasjon = TRUE.
    
    /* Betalt med sjekk. */
    IF CAN-FIND(FIRST BongLinje WHERE
                BongLinje.B_Id = BongHode.B_Id AND
                BongLinje.TTID = 54) THEN
        BongHode.flSjekk = TRUE.
    
    /* Systemkort er brukt. */
    IF CAN-FIND(FIRST BongLinje WHERE
                BongLinje.B_Id = BongHode.B_Id AND
                BongLinje.TTID = 54) THEN
        BongHode.flSjekk = TRUE.
    
    /* Bongen inneholder minst en makulert rad. */
    IF CAN-FIND(FIRST BongLinje WHERE
                BongLinje.B_Id = BongHode.B_Id AND
                BongLinje.Makulert = true) THEN
        BongHode.Makulert = 1.

    /* Hele bongen er makulert. */
    IF BongHode.Makulert = 1 and
        NOT CAN-FIND(FIRST BongLinje WHERE
                BongLinje.B_Id = BongHode.B_Id AND
                BongLinje.Makulert = false) THEN
        BongHode.Makulert = 2.

    IF AVAILABLE BongHode THEN
        RELEASE BongHode.
END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

