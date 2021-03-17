
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
DEF INPUT  PARAMETER piRecType   AS INT NO-UNDO.
DEF INPUT  PARAMETER piAntRecord AS INT NO-UNDO.
DEF OUTPUT PARAMETER piAntLinjer AS INT NO-UNDO.

{tmptt_EkstVPILev.i &SHARED=SHARED}


DEF VAR piLoop AS INT NO-UNDO.

LOOPEN:
DO piLoop = 1 TO piAntRecord:
  CREATE tmptt_EkstVPILev.
  ASSIGN
    piAntLinjer = piAntLinjer + 1
    tmpTT_EkstVPILev.RecType = piRecType
    .
  IMPORT STREAM InnFil 
    tmpTT_EkstVPILev.EkstVPILevNr
    tmpTT_EkstVPILev.KortNavn
    tmpTT_EkstVPILev.Navn
    tmpTT_EkstVPILev.EDato
    tmpTT_EkstVPILev.ETid
    tmpTT_EkstVPILev.BrukerID
    tmpTT_EkstVPILev.RegistrertDato
    tmpTT_EkstVPILev.RegistrertTid
    tmpTT_EkstVPILev.RegistrertAv
    tmpTT_EkstVPILev.OppdatMaskeVPI
    tmpTT_EkstVPILev.AktivLev
    tmpTT_EkstVPILev.PrioNr
    tmpTT_EkstVPILev.LevNr
    .
  FIND TT_EkstVPILev WHERE
      TT_EkstVPILev.EkstVPILevNr = tmpTT_EkstVPILev.EkstVPILevNr NO-ERROR.
  IF NOT AVAILABLE TT_EkstVPILev THEN
      CREATE TT_EkstVPILev.
  BUFFER-COPY tmpTT_EkstVPILev 
      EXCEPT AktivLev
      TO TT_EkstVPILev
      .

  RELEASE TT_EkstVPILev.
  DELETE tmpTT_EkstVPILev.
END. /* LOOPEN */

