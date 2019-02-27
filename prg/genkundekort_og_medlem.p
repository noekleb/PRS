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
DEF INPUT  PARAMETER iCL         AS INT  NO-UNDO.
DEF INPUT  PARAMETER lKundeNr    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER lMedlemsNr  AS DEC  NO-UNDO.
DEF INPUT  PARAMETER lFraKortNr  AS INT  NO-UNDO.
DEF INPUT  PARAMETER lTilKortNr  AS INT  NO-UNDO.
DEF INPUT  PARAMETER igyldighet  AS INT  NO-UNDO.
DEF OUTPUT PARAMETER bOk         AS LOG  NO-UNDO.
DEF OUTPUT PARAMETER cMsgs       AS CHAR NO-UNDO.

DEF VAR plLoop AS DEC NO-UNDO.

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

DO plLoop = lFraKortNr TO lTilKortNr:
    RUN genKunde_og_medlemskort (plLoop,
                                 OUTPUT bOk,
                                 OUTPUT cMsgs).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-genKunde_og_medlemskort) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genKunde_og_medlemskort Procedure 
PROCEDURE genKunde_og_medlemskort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  
  RUN genKundeKort (plLoop,
                      OUTPUT pbOk).
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT  PARAMETER piKortNr  AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER pbOk     AS LOG  NO-UNDO.
  DEF OUTPUT PARAMETER pcTekst   AS CHAR NO-UNDO.

  DEF VAR wTekst AS CHAR NO-UNDO.
  DEF VAR wRecid  AS RECID NO-UNDO.
  
  /* Kommer det inn et kortnr og dette har mer enn 6 siffer, skal butikknummer stripes bort. */
  /* Og butikknummeret fra kortnummeret skal benyttes på kortet som genereres.               */
  IF piKortNr > 999999 THEN 
  ASSIGN
    wTekst   = STRING(piKortNr)
    iCL      = INTEGER(SUBSTRING(wTekst,1,LENGTH(wTekst) - 6))
    wTekst   = SUBSTRING(wTekst,LENGTH(wTekst) - 5, LENGTH(wTekst))
    piKortNr = INTEGER(wTekst).

  IF iGyldighet = 0 THEN
      ASSIGN
      igyldighet = 999 /* Gyldighet. */
      .

  IF NOT CAN-FIND(KundeKort WHERE
                  KundeKort.KortNr = STRING(piKortNr)) THEN
  DO TRANSACTION:
      RUN createKundekort.p (INPUT lKundeNr, STRING(piKortNr), INPUT iGyldighet, OUTPUT wRecid).
      FIND KundeKort NO-LOCK WHERE
          RECID(KundeKort) = wRecid NO-ERROR.
      IF AVAILABLE KundeKort THEN
          ASSIGN
          pbOk    = TRUE
          pcTekst = "".
      ELSE
          ASSIGN
          pbOk = FALSE
          pcTekst = "Klarte ikke opprette kundekort med kortnummer " + STRING(piKortNr) + ".".
      IF AVAILABLE KundeKort THEN
          FIND Kunde NO-LOCK WHERE
            Kunde.KundeNr = KundeKort.KundeNr NO-ERROR.

      /* Oppretter medlemskort */
      IF pbOk THEN
      OPPRETTMEDLEMSKORT:
      DO:
          ASSIGN
              wTekst = STRING(iCL) + string(piKortNr,"999999").

          RUN sjekkomkorterunikt.p (INPUT wTekst).

          IF RETURN-VALUE = "" THEN
          DO:
              CREATE Medlemskort.
              ASSIGN
                  Medlemskort.Medlemsnr       = lMedlemsNr
                  Medlemskort.KortNr          = wTekst
                  Medlemskort.Innehaver       = "Ukjent " + IF AVAILABLE Kunde THEN Kunde.Navn ELSE ""
                  Medlemskort.AktivertDato    = TODAY
                  Medlemskort.UtgarDato       = TODAY + iGyldighet
                  Medlemskort.Kunderabattkort = TRUE
                  Medlemskort.KundeKortNr     = STRING(piKortNr)
                  MedlemsKort.InterntKKortId  = IF AVAILABLE KundeKort
                                                  THEN KundeKort.InterntKKortId
                                                  ELSE MedlemsKort.InterntKKortId
                  .
              FIND Medlem WHERE
                  Medlem.MedlemsNr = lMedlemsNr NO-ERROR.
              IF AVAILABLE Medlem AND Medlem.EtterNavn = "" THEN
                  ASSIGN
                    Medlem.EtterNavn = "Medlemskort " + wTekst
                    .
          END.
          ELSE
              ASSIGN
              pbOk = FALSE
              pcTekst = "Klarte ikke opprette medlemskort til kundekort med kortnummer " + STRING(piKortNr) + ".".
      END. /* OPPRETTMEDLEMSKORT */

  END. /* TRANSACTION */
  ELSE DO:
    FIND KundeKort WHERE KundeKort.KortNr = STRING(piKortNr) NO-LOCK NO-ERROR.
    IF AVAILABLE KundeKort THEN 
    DO TRANSACTION:
      OPPRETTMEDLEMSKORT2:
      DO:
          ASSIGN
              wTekst = STRING(iCL) + string(piKortNr,"999999").

          RUN sjekkomkorterunikt.p (INPUT wTekst).

          IF RETURN-VALUE = "" THEN
          DO:
              CREATE Medlemskort.
              ASSIGN
                  Medlemskort.Medlemsnr       = lMedlemsNr
                  Medlemskort.KortNr          = wTekst
                  Medlemskort.Innehaver       = "Ukjent " + IF AVAILABLE Kunde THEN Kunde.Navn ELSE ""
                  Medlemskort.AktivertDato    = TODAY
                  Medlemskort.UtgarDato       = TODAY + iGyldighet
                  Medlemskort.Kunderabattkort = TRUE
                  Medlemskort.KundeKortNr     = STRING(piKortNr)
                  MedlemsKort.InterntKKortId  = IF AVAILABLE KundeKort
                                                  THEN KundeKort.InterntKKortId
                                                  ELSE MedlemsKort.InterntKKortId
                  .
              FIND Medlem WHERE
                  Medlem.MedlemsNr = lMedlemsNr NO-ERROR.
              IF AVAILABLE Medlem AND Medlem.EtterNavn = "" THEN
                  ASSIGN
                    Medlem.EtterNavn = "Medlemskort " + wTekst
                    .
          END.
      END. /* OPPRETTMEDLEMSKORT2 */
      ASSIGN pbOk = TRUE.
    END.  
  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

