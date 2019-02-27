/* Sett rekvisisjon for varelinjer på ordre
   Parametere: <RefNr>|<RefTekst>;<liste (nye) rowid'er>;<list over rowid'er som ikke lenger tilhører rekvisisjonen
   
   Opprettet: 24.10.06 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix           AS INT    NO-UNDO.
DEF VAR iRekvNr      AS INT    NO-UNDO.
DEF VAR cRekvTekst   AS CHAR   NO-UNDO.
DEF VAR cRowIdList   AS CHAR   NO-UNDO.
DEF VAR cDeSelRows   AS CHAR   NO-UNDO.

ASSIGN iRekvNr      = INT(ENTRY(1,ENTRY(1,icParam,";"),"|"))
       cRekvTekst   = ENTRY(2,ENTRY(1,icParam,";"),"|")
       cRowIdList   = ENTRY(2,icParam,";")
       cDeSelRows   = ENTRY(3,icParam,";")
       .

RekvTrans:
DO TRANSACTION ON ERROR UNDO,LEAVE:
  DO ix = 1 TO NUM-ENTRIES(cRowIdList):
    FIND KOrdreLinje EXCLUSIVE-LOCK
         WHERE ROWID(KOrdreLinje) = TO-ROWID(ENTRY(ix,cRowIdList))
         NO-ERROR.
    IF AVAIL KOrdreLinje THEN
      ASSIGN KOrdreLinje.RefNr    = iRekvNr
             KOrdreLinje.RefTekst = cRekvTekst
            .
    ELSE DO:
      ocReturn = "Mislykket oppdatering av rekvisisjon. Kontakt systemansvarlig".
      UNDO,LEAVE RekvTrans.
    END.
  END.
  
  DO ix = 1 TO NUM-ENTRIES(cDeSelRows):
    FIND KOrdreLinje EXCLUSIVE-LOCK
         WHERE ROWID(KOrdreLinje) = TO-ROWID(ENTRY(ix,cDeSelRows))
         NO-ERROR.
    IF AVAIL KOrdreLinje THEN
      ASSIGN KOrdreLinje.RefNr    = 0
             KOrdreLinje.RefTekst = ""
            .
    ELSE DO:
      ocReturn = "Mislykket oppdatering av rekvisisjon. Kontakt systemansvarlig".
      UNDO,LEAVE RekvTrans.
    END.
  END.
END.

obOk = ocReturn = "".
