
/*------------------------------------------------------------------------
    File        : setSieKontoNr.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tomn
    Created     : Mon Jul 06 15:05:27 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
  DEFINE INPUT  PARAMETER piButikkNr AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER piTTId     AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER piTBId     AS INTEGER   NO-UNDO.
  DEFINE OUTPUT PARAMETER piKonto    AS INTEGER   NO-UNDO. 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

  ASSIGN
    piKonto = 0.
    
  /* Sjekker SIETransType for butikken. */
  FIND SIETransType NO-LOCK WHERE
    SIETRansType.ButikkNr = piButikkNr AND
    SIETransType.TTId     = piTTId AND
    SIETransType.TBId     = piTBId NO-ERROR.
  IF AVAILABLE SIETRansType THEN
       ASSIGN
         piKonto = SIETRansType.KontoNr.

  /* Sjekker SIETransType for standardbutikk = 0. */
  IF (piKonto = 0) THEN
  DO:
    FIND SIETransType NO-LOCK WHERE
      SIETRansType.ButikkNr = 0 AND
      SIETransType.TTId     = piTTId AND
      SIETransType.TBId     = piTBId NO-ERROR.
    IF AVAILABLE SIETRansType THEN
         ASSIGN
         piKonto = SIETRansType.KontoNr.
  END.

  /* Standard kontering. */
  IF (piKonto = 0) THEN
  DO:
      FIND TransBeskr NO-LOCK WHERE
           TransBeskr.TTId = piTTId AND
           TransBeskr.TBId = piTBId NO-ERROR.
      IF AVAILABLE TransBeskr THEN
           ASSIGN
         piKonto = TransBeskr.KontoNr.
  END.
