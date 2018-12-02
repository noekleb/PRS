
/* Kampanjestatistikk */
DO TRANSACTION:
  IF NOT CAN-FIND(StType WHERE
                  StType.StTypeId = "KAMPANJE") THEN
  DO:
      CREATE StType.
      ASSIGN
          StType.StTypeId    = "KAMPANJE"
          StType.Beskrivelse = "Kampanjestatistikk"
          .
      RUN opprettStDef (INPUT StType.StTypeId).
  END.
  IF NOT CAN-FIND(StType WHERE
                  StType.StTypeId = "KAMPART") THEN
  DO:
      CREATE StType.
      ASSIGN
          StType.StTypeId    = "KAMPTILB"
          StType.Beskrivelse = "Kampanjetilbudstatistikk"
          .
      RUN opprettStDef (INPUT StType.StTypeId).
  END.
  IF NOT CAN-FIND(StType WHERE
                  StType.StTypeId = "KAMPART") THEN
  DO:
      CREATE StType.
      ASSIGN
          StType.StTypeId    = "KAMPART"
          StType.Beskrivelse = "Kampanjeartikkelstatistikk"
          .
      RUN opprettStDef (INPUT StType.StTypeId).
  END.
  IF NOT CAN-FIND(StType WHERE
        StType.StTypeId = "NONSALE") THEN
    DO:
        CREATE StType.
        ASSIGN
            StType.StTypeId    = "NONSALE"
            StType.Beskrivelse = "NonSale artikkelstatistikk"
            .
        RUN opprettStDef (INPUT StType.StTypeId).
  END.
  IF NOT CAN-FIND(StType WHERE
                  StType.StTypeId = "LEVERAN-SA") THEN
  DO:
      CREATE StType.
      ASSIGN
          StType.StTypeId    = "LEVERAN-SA"
          StType.Beskrivelse = "Leverandør/sesong statistikk"
          .
      RUN opprettStDef (INPUT StType.StTypeId).
  END.
END.

PROCEDURE opprettStDef :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pStTypeId LIKE StType.StTypeId NO-UNDO.

  IF NOT CAN-FIND(StDef WHERE
                  StDef.StTypeId = StType.StTypeId AND
                  StDef.PerId    = "AAR") THEN
  DO:
      CREATE StDef.
      ASSIGN
          StDef.StTypeId    = StType.StTypeId
          StDef.PerId       = "AAR"
          StDef.Beskrivelse = "År"
          .
  END.
  IF NOT CAN-FIND(StDef WHERE
                  StDef.StTypeId = StType.StTypeId AND
                  StDef.PerId    = "MANED") THEN
  DO:
      CREATE StDef.
      ASSIGN
          StDef.StTypeId    = StType.StTypeId
          StDef.PerId       = "MANED"
          StDef.Beskrivelse = "Måned"
          .
  END.
  IF NOT CAN-FIND(StDef WHERE
                  StDef.StTypeId = StType.StTypeId AND
                  StDef.PerId    = "UKE") THEN
  DO:
      CREATE StDef.
      ASSIGN
          StDef.StTypeId    = StType.StTypeId
          StDef.PerId       = "UKE"
          StDef.Beskrivelse = "Uke"
          .
  END.
  IF NOT CAN-FIND(StDef WHERE
                  StDef.StTypeId = StType.StTypeId AND
                  StDef.PerId    = "DAG") THEN
  DO:
      CREATE StDef.
      ASSIGN
          StDef.StTypeId    = StType.StTypeId
          StDef.PerId       = "DAG"
          StDef.Beskrivelse = "Dag"
          .
  END.


END PROCEDURE.
