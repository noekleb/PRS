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
DEF INPUT PARAMETER lMedlemsNr  AS DEC NO-UNDO.
DEF INPUT PARAMETER iButikkNr   AS INT NO-UNDO.

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

DEF VAR pdForsteDato AS DATE NO-UNDO.
DEF VAR piForsteTid  AS INT  NO-UNDO.
DEF VAR pdDatoSiste  AS DATE NO-UNDO.
DEF VAR piSisteTid   AS INT  NO-UNDO.
DEF VAR plSaldo      AS DEC  NO-UNDO.
DEF VAR plTotKjop    AS DEC  NO-UNDO.
DEF VAR plBetaling   AS DEC  NO-UNDO.

FIND Medlem NO-LOCK WHERE
    Medlem.MedlemsNr = lMedlemsNr NO-ERROR.
IF NOT AVAILABLE Medlem THEN
    RETURN "AVBRYT".

DO TRANSACTION:
  /* Renser gammal dritt. */
  FOR EACH MedlemSaldo EXCLUSIVE-LOCK where
      MedlemSaldo.MedlemsNr = Medlem.MedlemsNr AND
      (IF iButikkNr = 0
         THEN TRUE
         ELSE MedlemSaldo.Butik = iButikkNr):

      DELETE MedlemSaldo.
  END.
  /* Legger opp ny saldo pr. butikk. */
  FOR EACH Butiker NO-LOCK WHERE
      (IF iButikkNr = 0
         THEN TRUE
         ELSE Butiker.Butik = iButikkNr):

      ASSIGN
          pdForsteDato = ?
          piForsteTid  = 0
          pdDatoSiste  = ?
          piSisteTid   = 0
          plSaldo      = 0
          plTotKjop    = 0
          plBetaling   = 0
          .

      /* Summerer opp butikken salg. */
      FOR EACH MedTrans OF Medlem NO-LOCK WHERE
          MedTrans.Butik = Butiker.Butik:
          /* Setter første kjøpsdato. */
          IF pdForsteDato <> MedTrans.Dato THEN
          DO:
              ASSIGN
              pdForsteDato = IF pdForsteDato = ?
                               THEN MedTrans.Dato
                             ELSE IF pdForsteDato > MedTrans.Dato 
                               THEN MedTrans.Dato
                             ELSE pdForsteDato
              piForsteTid  = IF pdForsteDato = MedTrans.Dato
                               THEN MedTrans.Tid
                             ELSE 
                               piForsteTid
              .

          END.

          /* Setter siste kjøpsdato */
          IF pdDatoSiste <> MedTrans.Dato THEN
          DO:
              ASSIGN
              pdDatoSiste = IF pdDatoSiste = ?
                               THEN MedTrans.Dato
                             ELSE IF pdDatoSiste < MedTrans.Dato 
                               THEN MedTrans.Dato
                             ELSE pdDatoSiste
              piSisteTid   = IF pdDatoSiste = MedTrans.Dato
                               THEN MedTrans.Tid
                             ELSE 
                               piSisteTid
              .
          END.
          ASSIGN
              plSaldo   = plSaldo   + (IF MedTrans.Antall < 0
                                         THEN (MedTrans.Pris - MedTrans.RabKr - MedTrans.SubTotalRab) * -1
                                         ELSE (MedTrans.Pris - MedTrans.RabKr - MedTrans.SubTotalRab))
              plTotKjop = plTotKjop + (IF MedTrans.Antall < 0
                                         THEN (MedTrans.Pris - MedTrans.RabKr - MedTrans.SubTotalRab) * -1
                                         ELSE (MedTrans.Pris - MedTrans.RabKr - MedTrans.SubTotalRab))
              .

      END.

      IF (plTotKjop <> 0) THEN
      DO:
      CREATE MedlemSaldo.
      ASSIGN
          MedlemSaldo.MedlemsNr    = Medlem.MedlemsNr
          MedlemSaldo.ButikkNr   = Butiker.Butik
          MedlemSaldo.ForsteDato = pdForsteDato
          MedlemSaldo.ForsteTid  = piForsteTid
          MedlemSaldo.DatoSiste  = pdDatoSiste
          MedlemSaldo.SisteTid   = piSisteTid
          MedlemSaldo.Saldo      = plSaldo
          MedlemSaldo.TotaltKjop = plTotKjop
          .
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


