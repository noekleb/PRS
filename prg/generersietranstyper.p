/* Genererer SIE på bakgrunn av bongdata.

  RUN generersiemoms.p
                          (INPUT-OUTPUT iAntLest,
                           INPUT-OUTPUT iAntPostert,
                           OUTPUT cMsgs).
*/

DEF INPUT-OUTPUT PARAMETER iAntLest     AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iAntPostert  AS INT NO-UNDO.
DEF OUTPUT PARAMETER cMsgs AS CHAR NO-UNDO.

DEF VAR iCL                AS INTEGER NO-UNDO.

/* Kopierer mva grupper. */
RUN kopierTransTyper.

/* **********************  Internal Procedures  *********************** */

PROCEDURE kopierTransTyper:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
FOR EACH TransType NO-LOCK WHERE 
  TransType.Aktiv = TRUE,
  EACH TransBeskr OF TransType NO-LOCK:
  
  iAntLest = iAntLest + 1.
  IF NOT CAN-FIND(SIETransType WHERE
                  SIETransType.ButikkNr = 0 AND 
                  SIETransType.TTId     = TransType.TTId AND 
                  SIETransType.TBId     = TransBeskr.TBId) THEN 
  DO:
    iAntPostert = iAntPostert + 1.
    CREATE SIETransType.
    /* NB: Skal alltid ligge på butikknummer 0. */
    BUFFER-COPY TransType TO SIETransType
        ASSIGN
            SIETransType.TBID        = TransBeskr.TBId
            SIETransType.Beskrivelse = TransBeskr.Beskrivelse.
  END.
  ELSE DO:
      FIND SIETransType WHERE
           SIETransType.ButikkNr = 0 AND 
           SIETransType.TTId     = TransType.TTId AND 
           SIETransType.TBId     = TransBeskr.TBId NO-ERROR.
      IF AVAILABLE SIETransType THEN 
        ASSIGN 
            SIETransType.Beskrivelse = TransBeskr.Beskrivelse. 
  END.
END.

END PROCEDURE.

