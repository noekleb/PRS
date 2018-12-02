
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
DEF INPUT  PARAMETER piRecType   AS INT NO-UNDO.
DEF INPUT  PARAMETER piAntRecord AS INT NO-UNDO.
DEF OUTPUT PARAMETER piAntLinjer AS INT NO-UNDO.

{tmptt_Avdeling.i &SHARED=SHARED}

DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Avdeling.
    ASSIGN
      piAntLinjer = piAntLinjer + 1
      tmpTT_Avdeling.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_Avdeling.AvdelingNr
      tmpTT_Avdeling.AvdelingNavn
      tmpTT_Avdeling.EDato
      tmpTT_Avdeling.ETid
      tmpTT_Avdeling.BrukerID
      tmpTT_Avdeling.RegistrertDato
      tmpTT_Avdeling.RegistrertTid
      tmpTT_Avdeling.RegistrertAv.
      .
      FIND TT_Avdeling WHERE
          TT_Avdeling.AvdelingNr = tmpTT_Avdeling.AvdelingNr NO-ERROR.
      IF NOT AVAILABLE TT_Avdeling THEN
          CREATE TT_Avdeling.
      BUFFER-COPY tmpTT_Avdeling TO TT_Avdeling.

      RELEASE TT_Avdeling.
      DELETE tmpTT_Avdeling.
  END. /* LOOPEN */

