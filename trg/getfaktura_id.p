&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : getfaktura_id.p
    Purpose     : Henter fakturan_id. For samlefaktura, hentes første 
                  åpne faktura, ellers opprettes ny faktura.

    Syntax      :  run getfaktura_id.p (input KundeNr, output faktura_id).

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER lKundeNr    AS DEC FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEFINE INPUT  PARAMETER iButikkNr   AS INT NO-UNDO.
DEFINE INPUT  PARAMETER iLevFNr     AS INT NO-UNDO.
DEFINE INPUT  PARAMETER bButikkSalg AS LOG NO-UNDO.
DEFINE INPUT  PARAMETER dDato       AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER lFaktura_Id AS DEC FORMAT ">>>>>>>>>>>>9" NO-UNDO.

DEF VAR bIkkeSamleFaktura AS LOG NO-UNDO.

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

/* 999 flagger mottak av bong med negativ saldo fra kassen.  */
/* Da skal det opprettes separat faktura på negative bonger. */                
IF iLevFNr = 999 THEN
    ASSIGN
    iLevFNr           = 1
    bIkkeSamleFaktura = TRUE
    .

FIND Kunde NO-LOCK WHERE
    Kunde.KundeNr = lKundeNr NO-ERROR.
IF NOT AVAILABLE Kunde THEN
    lFaktura_Id = ?.
ELSE 
HENT_Faktura_Id:
DO:
    IF Kunde.SamleFaktura AND bIkkeSamleFaktura = FALSE THEN
    SAMLEFAKTURA:
    DO:        
        IF bButikksalg THEN /* Butikksalg - Kreditsalg fra kassen. */
            FIND LAST FakturaHode NO-LOCK WHERE
                FakturaHode.ButikkNr     = iButikkNr AND
                FakturaHode.Samlefaktura = TRUE AND
                FakturaHode.FakturaNr    = ? AND
                FakturaHode.ButikkSalg   = TRUE AND
                FakturaHode.KundeNr      = lKundeNr AND
                MONTH(FakturaHode.Dato) = MONTH(dDato) AND 
                YEAR(FakturaHode.Dato)  = YEAR(dDato)
                USE-INDEX ButikkNr NO-ERROR.
        ELSE /* Siste Åpne som ikke er butikksalg. */
            FIND LAST FakturaHode NO-LOCK WHERE
                FakturaHode.ButikkNr     = iButikkNr AND
                FakturaHode.Samlefaktura = TRUE AND
                FakturaHode.FakturaNr    = ? AND
                FakturaHode.ButikkSalg   = FALSE AND
                FakturaHode.KundeNr      = lKundeNr AND
                MONTH(FakturaHode.Dato)  = MONTH(dDato) AND 
                YEAR(FakturaHode.Dato)   = YEAR(dDato)
                USE-INDEX ButikkNr NO-ERROR.

        IF NOT AVAILABLE FakturaHode THEN DO:
            RUN OpprettFaktura.
        END.
        ELSE
            lFaktura_Id = FakturaHode.Faktura_Id.
    END. /* SAMLEFAKTURA */
    ELSE IKKE-SALMEFAKTURA: DO:
        RUN OpprettFaktura.
        ASSIGN
            bIkkeSamleFaktura = FALSE
            .
    END. /* IKKE-SALMEFAKTURA */
END. /* HENT_Faktura_Id */

RETURN "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-OpprettFaktura) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettFaktura Procedure 
PROCEDURE OpprettFaktura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO TRANSACTION:
      CREATE FakturaHode.
      ASSIGN
          FakturaHode.KundeNr      = Kunde.KundeNr
          FakturaHode.Samlefaktura = Kunde.SamleFaktura
          FakturaHode.ButikkNr     = iButikkNr
          FakturaHode.LevFNr       = iLevFNr 
          FakturaHode.ButikkSalg   = bButikkSalg
          FakturaHode.Dato         = dDato
          .
      /* Negativ bong fra kassen    */
      /* Skal komme som kreditnota. */
      IF bIkkeSamleFaktura = TRUE THEN
      DO:
          FIND Bilagstype NO-LOCK WHERE
              Bilagstype.Bilagstype = 2 NO-ERROR.
          ASSIGN
          FakturaHode.SamleFaktura = FALSE
          FakturaHode.BilagsType   = 2
          FakturaHode.BTTekst      = (IF AVAILABLE Bilagstype
                                        THEN Bilagstype.BTTekst
                                      ELSE IF FakturaHode.BilagsType = 2 THEN "KreditNota"
                                       ELSE "Faktura")
          .
      END.

      /* Slipper transaksjonen */
      FIND CURRENT FakturaHode NO-LOCK.
      ASSIGN
          lFaktura_Id = FakturaHode.Faktura_Id
          .
  END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

