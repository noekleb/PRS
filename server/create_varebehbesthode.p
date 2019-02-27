DEF INPUT  PARAM hBuffer     AS HANDLE NO-UNDO.
DEF INPUT  PARAM icFields    AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues    AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.

DEF VAR iHodeLinjeId  AS INT NO-UNDO.

DEF BUFFER bVareBehBestHode FOR VareBehBestHode.
FIND LAST bVareBehBestHode
     WHERE bVareBehBestHode.VarebehNr   = DEC(ENTRY(LOOKUP("VarebehNr",icFields),icValues,"|"))
       AND bVareBehBestHode.CLButikkNr  = INT(ENTRY(LOOKUP("CLButikkNr",icFields),icValues,"|"))
       AND bVareBehBestHode.HodeLinjeId > 0
     NO-LOCK NO-ERROR.
IF AVAIL bVareBehBestHode THEN
  iHodeLinjeId = bVareBehBestHode.HodeLinjeId + 1.
ELSE iHodeLinjeId = 1.

hBuffer:BUFFER-FIELD("HodeLinjeId"):BUFFER-VALUE = iHodeLinjeId.

