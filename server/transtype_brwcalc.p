/* Bibliotek for kalkulerte felter, pakklistelinje
  Opprettet: 09.08.07 av BHa
------------------------------------------------------------------------------*/  
PROCEDURE transtype_Brukt:
  DEF INPUT  PARAM irTransType AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND TransType WHERE ROWID(TransType) = irTransType NO-LOCK NO-ERROR.
    IF AVAIL TransType THEN DO:
      
      FIND FIRST TransLogg NO-LOCK 
           WHERE TransLogg.TTId = TransType.TTId NO-ERROR.
           
      IF AVAIL TransLogg THEN 
          ocValue = 'Yes'.
      ELSE ocValue = 'No'.
    END.
    ELSE ocValue = 'No'.
END PROCEDURE.





