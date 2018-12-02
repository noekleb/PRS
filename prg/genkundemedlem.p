&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :
                  RUN genkunde.p (INT(FI-CL:SCREEN-VALUE),
                              OUTPUT plKundeNr,
                              OUTPUT bOk,
                              OUTPUT cMsgs).


    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF INPUT        PARAMETER iCL         AS INT  NO-UNDO.
DEF INPUT        PARAMETER iGruppeId   AS INT  NO-UNDO.
DEF INPUT-OUTPUT PARAMETER lKundeNr    AS DEC  NO-UNDO.
DEF OUTPUT       PARAMETER lMedlemsNr  AS DEC  NO-UNDO.
DEF OUTPUT       PARAMETER bOk         AS LOG  NO-UNDO.
DEF OUTPUT       PARAMETER cMsgs       AS CHAR NO-UNDO.

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

RUN genKundeMedlem.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-genKundeMedlem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genKundeMedlem Procedure 
PROCEDURE genKundeMedlem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO TRANSACTION:
      /* Kunde skal bare opprettes hvis kundenr = 0. */
      IF lKundeNr = 0 THEN
      GENKUNDE:
      DO:
          CREATE Kunde.
          ASSIGN
              Kunde.Navn       = "Ukjent"
              Kunde.ButikkNr   = iCL
              Kunde.GruppeId   = iGruppeId
              Kunde.TypeId     = 1
              Kunde.MaksKredit = 100
              Kunde.BetType    = 2 /* Kreditkunde */

              .

          ASSIGN
              lKundeNr = kunde.KundeNr
              .
      END. /* GENKUNDE */

      FIND Kunde NO-LOCK WHERE
          Kunde.KundeNr = lKundeNr NO-ERROR.
      IF AVAILABLE Kunde THEN
      GENMEDLEM:
      DO:
          CREATE Medlem.
          ASSIGN
              Medlem.Fornavn   = "Ukjent" /*Kunde.Navn*/
              Medlem.ButikkNr  = iCL
              Medlem.KundeNr   = Kunde.KundeNr
              Medlem.MedGruppe = 1
              Medlem.MedType   = 1
              .

          ASSIGN
              lMedlemsNr = Medlem.MedlemsNr
              .
      END. /* GENMEDLEM */

      IF AVAILABLE Kunde  THEN RELEASE Kunde.
      IF AVAILABLE Medlem THEN RELEASE Medlem.

  END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

