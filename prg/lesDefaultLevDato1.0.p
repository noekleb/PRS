
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
DEF INPUT  PARAMETER piRecType   AS INT NO-UNDO.
DEF INPUT  PARAMETER piAntRecord AS INT NO-UNDO.
DEF OUTPUT PARAMETER piAntLinjer AS INT NO-UNDO.

{tmptt_DefaultLevDato.i &SHARED=SHARED}

DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_DefaultLevDato.
    ASSIGN
      piAntLinjer = piAntLinjer + 1
      tmpTT_DefaultLevDato.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_DefaultLevDato.ButikkNr
      tmpTT_DefaultLevDato.LevNr
      tmpTT_DefaultLevDato.UkeDag       
      tmpTT_DefaultLevDato.EDato      
      tmpTT_DefaultLevDato.ETid       
      tmpTT_DefaultLevDato.BrukerId       
      tmpTT_DefaultLevDato.RegistrertDato    
      tmpTT_DefaultLevDato.RegistrertTid
      tmpTT_DefaultLevDato.RegistrertAv
      tmpTT_DefaultLevDato.Tid
      tmpTT_DefaultLevDato.HasterTid      
      .
      FIND TT_DefaultLevDato WHERE
          TT_DefaultLevDato.ButikkNr = tmpTT_DefaultLevDato.ButikkNr AND
          TT_DefaultLevDato.LevNr    = tmpTT_DefaultLevDato.LevNr NO-ERROR.
      IF NOT AVAILABLE TT_DefaultLevDato THEN
          CREATE TT_DefaultLevDato.
      BUFFER-COPY tmpTT_DefaultLevDato TO TT_DefaultLevDato.

      RELEASE TT_DefaultLevDato.
      DELETE tmpTT_DefaultLevDato.
  END. /* LOOPEN */

