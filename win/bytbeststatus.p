  DEFINE INPUT PARAMETER wBestHodeRecid AS RECID NO-UNDO.
  DEFINE INPUT PARAMETER wModus         AS CHAR  NO-UNDO. /* "+" ELLER "-" */
  DEFINE INPUT PARAMETER wOrdreNr       LIKE Ordre.OrdreNr NO-UNDO.
  DEFINE VAR             wAnt           AS INTE INIT 1 NO-UNDO.
  DEFINE BUFFER bBestStr   FOR BestStr.
  DEFINE BUFFER bBestPris  FOR BestPris.
  
  IF LENGTH(wModus) = 2 THEN                   /* Detta görs för att vi skall */
      ASSIGN wAnt = INT(SUBSTR(wModus,2,1))   /* kunna gå från status 2 till */
             wModus = SUBSTR(wModus,1,1).     /* 6 vid direktinleverans      */
                                               /* Input wModus = +4           */
  
  FIND BestHode WHERE RECID(BestHode) = wBestHodeRecid EXCLUSIVE NO-ERROR.
  IF LOCKED BestHode THEN DO:
      MESSAGE "Posten uppdateras av en annan användare." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY "Avbryt".
  END.
  IF wModus = "+" THEN DO:
      FOR EACH bBestStr OF BestHode WHERE bBestStr.BestStat = BestHode.BestStat:    
          RELEASE BestStr.
          CREATE BestStr.
          BUFFER-COPY bBestStr EXCEPT BestStat TO BestStr.
          ASSIGN BestStr.BestStat = bBestStr.BestStat + wAnt.
      END.
      FOR EACH bBestPris OF BestHode WHERE bBestPris.BestStat = BestHode.BestStat:    
          RELEASE BestPris.
          CREATE BestPris.
          BUFFER-COPY bBestPris EXCEPT BestStat TO BestPris.
          ASSIGN BestPris.BestStat = bBestPris.BestStat + wAnt.
      END.
      ASSIGN BestHode.OrdreNr = IF BestHode.BestStat = 2 THEN 
                                wOrdreNr ELSE BestHode.OrdreNr
             BestHode.BestStat = BestHode.BestStat + wAnt
             BestHode.SendtDato = IF BestHode.BestStat = 4 THEN TODAY ELSE BestHode.SendtDato
             BestHode.SendtTid  = IF BestHode.BestStat = 4 THEN TIME ELSE BestHode.SendtTid
             BestHode.SendtAv   = IF BestHode.BestStat = 4 THEN USERID("dictdb") ELSE BestHode.SendtAv.
  END.
  ELSE IF wModus = "-" THEN DO:
      FOR EACH BestStr OF BestHode WHERE BestStr.BestStat = BestHode.BestStat - 1:
          DELETE BestStr.
      END.
      FOR EACH BestStr OF BestHode WHERE BestStr.BestStat = BestHode.BestStat:
          ASSIGN BestStr.BestStat = BestStr.BestStat - wAnt.
      END.
      FOR EACH BestPris OF BestHode WHERE BestPris.BestStat = BestHode.BestStat - 1:    
          DELETE BestPris.
      END.
      FOR EACH BestPris OF BestHode WHERE BestPris.BestStat = BestHode.BestStat:    
          ASSIGN BestPris.BestStat = BestPris.BestStat - wAnt.
      END.
      ASSIGN BestHode.OrdreNr =  IF BestHode.BestStat = 3 THEN 
                                wOrdreNr ELSE BestHode.OrdreNr
             BestHode.BestStat = BestHode.BestStat - wAnt.
  END.
