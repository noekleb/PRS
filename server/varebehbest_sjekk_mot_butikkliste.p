/* Fjerner bestillinger dersom butikklisten er rediger 
   Parametere:  Gammel butikkliste;ny butiukkliste;Varebehnr
   
   Opprettet: 15.10.04 av BHa                  
-----------------------------------------------------------------------------------*/

DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix AS INT NO-UNDO.

DO ix = 1 TO NUM-ENTRIES(ENTRY(1,icParam,";")):
  IF NOT CAN-DO(ENTRY(2,icParam,";"),ENTRY(ix,ENTRY(1,icParam,";"))) THEN DO:
    FOR EACH VarebehBestLinje
        WHERE VarebehBestLinje.VareBehNr       = INT(ENTRY(3,icParam,";"))
          AND VarebehBestLinje.BestiltButikkNr = INT(ENTRY(ix,ENTRY(1,icParam,";")))
        EXCLUSIVE-LOCK:
      DELETE VarebehBestLinje.
    END.
    FOR EACH VarebehBestHode
        WHERE VarebehBestLinje.VareBehNr       = INT(ENTRY(3,icParam,";"))
          AND NOT CAN-FIND(FIRST VarebehBestLinje OF VarebehBestHode)
        EXCLUSIVE-LOCK:
      DELETE VarebehBestHode.
    END.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.
