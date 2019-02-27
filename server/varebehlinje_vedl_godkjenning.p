/* Godkjenn flere bestillinger  
   Opprettet: 16.01.06 av BHa 
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO INIT YES.

DEF VAR cRowIdList AS CHAR NO-UNDO.
DEF VAR iButikkNr  AS INT  NO-UNDO.
DEF VAR bGodkjent  AS LOG  NO-UNDO.
DEF VAR ix         AS INT  NO-UNDO.

ASSIGN cRowIdList = ENTRY(1,icParam,";")
       iButikkNr  = INT(ENTRY(2,icParam,";"))
       bGodkjent  = LOGICAL(ENTRY(3,icParam,";")).

DO ix = 1 TO NUM-ENTRIES(cRowIdList):
  FIND VarebehLinje WHERE ROWID(VarebehLinje) = TO-ROWID(ENTRY(ix,cRowIdList)) NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinje THEN 
    FOR EACH VarebehLinjeTrans EXCLUSIVE-LOCK
        WHERE VarebehLinjeTrans.VarebehNr  = VarebehLinje.VarebehNr
          AND VarebehLinjeTrans.ButikkNr   = iButikkNr
          AND VarebehLinjeTrans.ArtikkelNr = VarebehLinje.ArtikkelNr
          :
      VarebehLinjeTrans.GodkjentBestilling = bGodkjent.
    END.
END.
