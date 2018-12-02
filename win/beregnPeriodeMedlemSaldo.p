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
DEF INPUT  PARAMETER lMedlemsNr  AS DEC  NO-UNDO.
DEF INPUT  PARAMETER cTTId       AS CHAR NO-UNDO.
DEF INPUT  PARAMETER dFraDato    AS DATE NO-UNDO.
DEF INPUT  PARAMETER dTilDato    AS DATE NO-UNDO.
DEF INPUT  PARAMETER iButikkNr   AS INT  NO-UNDO.
DEF OUTPUT PARAMETER plTotKjop   AS DEC NO-UNDO.

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

DEF VAR plSaldo      AS DEC  NO-UNDO.

FIND Medlem NO-LOCK WHERE
    Medlem.MedlemsNr = lMedlemsNr NO-ERROR.
IF NOT AVAILABLE Medlem THEN
    RETURN "AVBRYT".

ASSIGN
    plSaldo      = 0
    plTotKjop    = 0
    .

DO TRANSACTION:
  /* Legger opp ny saldo pr. butikk. */
  FOR EACH Butiker NO-LOCK WHERE
      (IF iButikkNr = 0
         THEN TRUE
         ELSE Butiker.Butik = iButikkNr):

      /* Summerer opp butikken salg. */
      FOR EACH MedTrans OF Medlem NO-LOCK WHERE
          MedTrans.Butik = Butiker.Butik AND
          (IF dFraDato <> ? THEN MedTrans.Dato >= dFraDato ELSE TRUE) AND
          (IF dTilDato <> ? THEN MedTrans.Dato <= dTilDato ELSE TRUE) AND
          (IF cTTId <> '' THEN MedTrans.TTId = INT(cTTID) ELSE TRUE):
          /* Setter første kjøpsdato. */
          ASSIGN
              plSaldo   = plSaldo   + (IF MedTrans.Antall < 0
                                         THEN (MedTrans.Pris - MedTrans.RabKr - MedTrans.SubTotalRab) * -1
                                         ELSE (MedTrans.Pris - MedTrans.RabKr - MedTrans.SubTotalRab))
              plTotKjop = plTotKjop + (IF MedTrans.Antall < 0
                                         THEN (MedTrans.Pris - MedTrans.RabKr - MedTrans.SubTotalRab) * -1
                                         ELSE (MedTrans.Pris - MedTrans.RabKr - MedTrans.SubTotalRab))
              .

      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


