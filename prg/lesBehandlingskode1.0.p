
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
DEF INPUT  PARAMETER piRecType   AS INT NO-UNDO.
DEF INPUT  PARAMETER piAntRecord AS INT NO-UNDO.
DEF OUTPUT PARAMETER piAntLinjer AS INT NO-UNDO.

{tmptt_Behandlingskode.i &SHARED=SHARED}

DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Behandlingskode.
    ASSIGN
      piAntLinjer = piAntLinjer + 1
      tmpTT_Behandlingskode.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_Behandlingskode.BehKode
      tmpTT_Behandlingskode.EDato         
      tmpTT_Behandlingskode.ETid          
      tmpTT_Behandlingskode.BrukerID      
      tmpTT_Behandlingskode.RegistrertDato
      tmpTT_Behandlingskode.RegistrertTid 
      tmpTT_Behandlingskode.RegistrertAv  
      tmpTT_Behandlingskode.Beskrivelse
     .
    FIND TT_Behandlingskode WHERE
        TT_Behandlingskode.BehKode = tmpTT_Behandlingskode.BehKode NO-ERROR.
    IF NOT AVAILABLE TT_Behandlingskode THEN
        CREATE TT_Behandlingskode.
    BUFFER-COPY tmpTT_Behandlingskode TO TT_Behandlingskode.

    RELEASE TT_Behandlingskode.
    DELETE tmpTT_Behandlingskode.
  END. /* LOOPEN */


