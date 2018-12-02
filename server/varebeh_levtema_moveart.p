 /* Flytter en artikkel opp/ned i et lev.tema
   Parameter: <rowid vareboktemalinje>,up/down
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR iVbTemaNr AS INT NO-UNDO.
DEF VAR iCurrSeq  AS INT NO-UNDO.

DEF BUFFER bVBT FOR VarebokTemaLinje.

FIND VarebokTemaLinje EXCLUSIVE-LOCK
     WHERE ROWID(VarebokTemaLinje) = TO-ROWID(ENTRY(1,icParam)) 
     NO-ERROR.
IF NOT AVAIL VarebokTemaLinje THEN DO:
  ocReturn = "Post ikke tilgjengelig for oppdatering".
  RETURN.
END.

ASSIGN iVbTemaNr = VarebokTemaLinje.VbTemaNr
       iCurrSeq  = VarebokTemaLinje.SeqNr.

IF ENTRY(2,icParam) = "up" THEN
  FOR EACH bVBT EXCLUSIVE-LOCK
      WHERE bVBT.VbTemaNr = iVbTemaNr
        AND bVBT.SeqNr    < iCurrSeq
         BY bVBT.SeqNr DESC:
    ASSIGN VarebokTemaLinje.SeqNr = bVBT.SeqNr
           bVBT.SeqNr             = iCurrSeq.
    LEAVE.
  END.
ELSE
  FOR EACH bVBT EXCLUSIVE-LOCK
      WHERE bVBT.VbTemaNr = iVbTemaNr
        AND bVBT.SeqNr    > iCurrSeq
         BY bVBT.SeqNr:
    ASSIGN VarebokTemaLinje.SeqNr = bVBT.SeqNr
           bVBT.SeqNr             = iCurrSeq.
    LEAVE.
  END.

obOK = TRUE.
