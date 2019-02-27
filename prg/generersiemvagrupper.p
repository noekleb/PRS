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
RUN kopierMvaGrupper.

/* **********************  Internal Procedures  *********************** */

PROCEDURE kopierMvaGrupper:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
FOR EACH Moms NO-LOCK:
  iAntLest = iAntLest + 1.
  IF NOT CAN-FIND(SIEMoms WHERE
                  SIEMoms.ButikkNr = 0 AND 
                  SIEMoms.MomsKod  = Moms.MomsKod) THEN 
  DO:
    iAntPostert = iAntPostert + 1.
    CREATE SIEMoms.
    BUFFER-COPY Moms TO SIEMoms.
  END.
END.

END PROCEDURE.

