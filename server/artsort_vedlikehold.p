/* Knytning av leverandørinndelinger til artikkel 
   Parametere:  <ArtikkelNr>|<Y7N>|<Liste over rowid'er for leverandørinndelinger>
            - Parameter 2 (Y/N): Skal inndelinger som ikke ligger i listen fjernes
   Opprettet: 25.07.05 av BHa         
   Endret:    22.08.08 av BHa
            - Dersom det finnes bestillinger på sortementet så blir disse slettet
              hvilke som skal slettes er funnet i artsort_i_bruk.p         
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fArtikkelNr AS DEC    NO-UNDO.
DEF VAR cRowIdList  AS CHAR   NO-UNDO.
DEF VAR cRowIdTrans AS CHAR   NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.

ASSIGN fArtikkelNr = DEC(ENTRY(1,icParam,"|"))
       cRowIdList  = ENTRY(3,icParam,"|")
       .
IF NUM-ENTRIES(icParam,"|") > 3 THEN
  cRowIdTrans = ENTRY(4,icParam,"|").

IF ENTRY(2,icParam,"|") = "Y" THEN 
  FOR EACH ArtSort EXCLUSIVE-LOCK
      WHERE ArtSort.ArtikkelNr = fArtikkelNr
     ,FIRST LevSort OF ArtSort NO-LOCK:
    IF NOT CAN-DO(cRowIdlist,STRING(ROWID(LevSort))) THEN
      DELETE ArtSort.
  END.

DO ix = 1 TO NUM-ENTRIES(cRowIdList):
  FIND LevSort 
       WHERE ROWID(LevSort) = TO-ROWID(ENTRY(ix,cRowIdlist)) 
       NO-LOCK NO-ERROR.
  IF AVAIL LevSort THEN DO:
    FIND FIRST ArtSort 
         WHERE ArtSort.ArtikkelNr = fArtikkelNr
           AND ArtSort.SortId     = LevSort.SortId
           AND ArtSort.LevNr      = LevSort.LevNr
         NO-LOCK NO-ERROR.
    IF NOT AVAIL ArtSort THEN DO:
      CREATE ArtSort.
      ASSIGN ArtSort.ArtikkelNr = fArtikkelNr
             ArtSort.SortId     = LevSort.SortId
             ArtSort.LevNr      = LevSort.LevNr
             .
    END.
  END.
END.

IF ocReturn = "" THEN DO:
  obOk = TRUE.
  DO ix = 1 TO NUM-ENTRIES(cRowIdTrans):
    FIND VareBehLinjeTrans EXCLUSIVE-LOCK 
         WHERE ROWID(VareBehLinjeTrans) = TO-ROWID(ENTRY(ix,cRowIdTrans))
         NO-ERROR.
    IF AVAIL VareBehLinjeTrans THEN DELETE VareBehLinjeTrans.
  END.
END. 
