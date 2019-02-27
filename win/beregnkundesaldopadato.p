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
DEF INPUT  PARAMETER lKundeNr  AS DEC  NO-UNDO.
DEF INPUT  PARAMETER iButikkNr AS INT  NO-UNDO.
DEF INPUT  PARAMETER dDato     AS DATE NO-UNDO.
DEF OUTPUT PARAMETER lSaldo    AS DEC  NO-UNDO.
DEF OUTPUT PARAMETER lTotKjop  AS DEC  NO-UNDO.

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
DEF VAR plPris       AS DEC  NO-UNDO.

FIND kunde NO-LOCK WHERE
    Kunde.KundeNr = lKundeNr NO-ERROR.
IF NOT AVAILABLE Kunde THEN
    RETURN "AVBRYT".

SALDOBLOKK:
DO:
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
      FOR EACH KundeTrans OF Kunde NO-LOCK WHERE
          KundeTrans.Butik = Butiker.Butik AND
          KundeTrans.Dato <= dDato:
          /* Setter første kjøpsdato. */
          IF pdForsteDato <> KundeTrans.Dato THEN
          DO:
              ASSIGN
              pdForsteDato = IF pdForsteDato = ?
                               THEN KundeTrans.Dato
                             ELSE IF pdForsteDato > KundeTrans.Dato 
                               THEN KundeTrans.Dato
                             ELSE pdForsteDato
              piForsteTid  = IF pdForsteDato = KundeTrans.Dato
                               THEN KundeTrans.Tid
                             ELSE 
                               piForsteTid
              .

          END.

          /* Setter siste kjøpsdato */
          IF pdDatoSiste <> KundeTrans.Dato THEN
          DO:
              ASSIGN
              pdDatoSiste = IF pdDatoSiste = ?
                               THEN KundeTrans.Dato
                             ELSE IF pdDatoSiste < KundeTrans.Dato 
                               THEN KundeTrans.Dato
                             ELSE pdDatoSiste
              piSisteTid   = IF pdDatoSiste = KundeTrans.Dato
                               THEN KundeTrans.Tid
                             ELSE 
                               piSisteTid
              .
          END.
          ASSIGN
              plPris    = (IF KundeTrans.Antall < 0
                             THEN (KundeTrans.Pris - KundeTrans.RabKr - KundeTrans.SubTotalRab) * -1
                             ELSE (KundeTrans.Pris - KundeTrans.RabKr - KundeTrans.SubTotalRab))
              plSaldo   = plSaldo + (IF KundeTrans.MotPostert = FALSE
                                       THEN plPris
                                       ELSE 0)
              plTotKjop = plTotKjop + plPris
              .

      END.
      /* Sumerer opp salg av gavekort. */
      FOR EACH KundeBetTrans OF Kunde NO-LOCK WHERE
          KundeBetTrans.Butik      = Butiker.Butik AND
          KundeBetTrans.MotPostert = TRUE AND
          KundeBetTrans.TTId       = 134 AND
          KundeBetTrans.Dato      <= dDato:
          ASSIGN
              plPris    = plPris    + KundeBetTrans.Belop
              plSaldo   = plSaldo   + KundeBetTrans.Belop
              plTotKjop = plTotKjop + KundeBetTrans.Belop
              .
      END.

      /* Sumerer opp butikkens betalinger. */
      FOR EACH KundeBetTrans OF Kunde NO-LOCK WHERE
          KundeBetTrans.Butik      = Butiker.Butik AND
          KundeBetTrans.MotPostert = FALSE AND
          KundeBetTrans.Dato      <= dDato:

          /* Den er hensyntatt */
          IF CAN-DO("063",STRING(KundeBetTrans.TTId,"999")) THEN
              NEXT.

          /* Rekvisisjon, Utbetaling og Kredit telles ikke med.     */
          /* Veksel skal heller ikke med her. Det skal trekkes fra. */
          /* Bank og kontant skal heller ikke telles med her.       */
          IF NOT CAN-DO("055,059,062,065,070",STRING(KundeBetTrans.TTId,"999")) THEN 
              ASSIGN
                  plBetaling = plBetaling + KundeBetTrans.Belop
                  .
          /* Trekker fra veksel NB: Er negativ */
          ELSE IF CAN-DO("070",STRING(KundeBetTrans.TTId,"999")) THEN 
              ASSIGN
                  plBetaling = plBetaling + KundeBetTrans.Belop
                  .
      END.
      IF (plTotKjop <> 0 OR plBetaling <> 0) THEN
      DO:
/*       CREATE KundeSaldo.                               */
/*       ASSIGN                                           */
/*           KundeSaldo.KundeNr    = Kunde.KundeNr        */
/*           KundeSaldo.ButikkNr   = Butiker.Butik        */
/*           KundeSaldo.ForsteDato = pdForsteDato         */
/*           KundeSaldo.ForsteTid  = piForsteTid          */
/*           KundeSaldo.DatoSiste  = pdDatoSiste          */
/*           KundeSaldo.SisteTid   = piSisteTid           */
/*           KundeSaldo.Saldo      = plSaldo - plBetaling */
/*           KundeSaldo.TotaltKjop = plTotKjop            */
/*           .                                            */
          ASSIGN
              lSaldo   = plSaldo - plBetaling
              lTotKjop = plTotKjop
              .
      END.
  END.
END. /* SALDOBLOKK */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


