/* Registrering av kreditnota 
   Parametere:  ENTRY(1,"|"): Levnr
                ENTRY(2,"|"): sortId
   Opprettet: 05.07.05 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix          AS INT    NO-UNDO.
DEF VAR cRowIdTrans AS CHAR   NO-UNDO.

IF NUM-ENTRIES(icParam,"|") > 2 THEN
  cRowIdTrans = ENTRY(3,icParam,"|").

FIND FIRST LevSort EXCLUSIVE-LOCK
     WHERE LevSort.LevNr  = INT(ENTRY(1,icParam,"|"))
       AND LevSort.SortId = ENTRY(2,icParam,"|")
     NO-ERROR.
IF AVAIL LevSort THEN DO:
  FOR EACH LevSAnt OF LevSort:
      DELETE LevSAnt.
  END.
  FOR EACH ArtSort OF LevSort:
    DELETE ArtSort.
  END.
  DELETE LevSort.
END.
ELSE ocReturn = "Inndeling " + icParam + " ikke tilgjengelig".

IF ocReturn = "" THEN DO:
  obOk = TRUE.
  DO ix = 1 TO NUM-ENTRIES(cRowIdTrans):
    FIND VareBehLinjeTrans EXCLUSIVE-LOCK 
         WHERE ROWID(VareBehLinjeTrans) = TO-ROWID(ENTRY(ix,cRowIdTrans))
         NO-ERROR.
    IF AVAIL VareBehLinjeTrans THEN DELETE VareBehLinjeTrans.
  END.
END. 

