/* Skriv ut ordrebekreftelse
   Parametere: <KOrdre_id>
   
   Opprettet: 10.07.06 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix           AS INT    NO-UNDO.
DEF VAR iLnr         AS INT    NO-UNDO.
DEF VAR cLevVareList AS CHAR   NO-UNDO.
DEF VAR fKOrdre_id   AS DEC    NO-UNDO.
DEF VAR bDelLev      AS LOG    NO-UNDO.
DEF VAR fLevAnt      AS DEC    NO-UNDO.

DEF BUFFER bKOrdreLinje FOR KOrdreLinje.

fKOrdre_id = DEC(ENTRY(1,icParam,";")).

FIND KOrdreHode EXCLUSIVE-LOCK
     WHERE KOrdreHode.KOrdre_id = fKOrdre_id
     NO-ERROR.

IF AVAIL KOrdreHode THEN DO ON ERROR UNDO, LEAVE:
  FIND FIRST SysPara NO-LOCK
       WHERE SysPara.SysHId = 19 
         AND SysPara.SysGr  = 1
         AND SysPara.ParaNr = 30
       NO-ERROR.
  IF AVAIL SysPara THEN DO:
    IF INT(KOrdreHode.LevStatus) < 30 THEN
      KOrdreHode.LevStatus = "30".
    ASSIGN ocReturn = SysPara.Parameter1
           obOk     = YES.
  END.
  ELSE ocReturn = "Status for ordrebekreftelse er ikke definert (sys.param 10,1,30)".
END.

IF NOT obOk THEN
  obOk = ocReturn = "".
