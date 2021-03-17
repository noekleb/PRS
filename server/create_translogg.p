DEF INPUT  PARAM hBuffer     AS HANDLE NO-UNDO.
DEF INPUT  PARAM icFields    AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues    AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.

DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iTransNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iSeqNr AS INTEGER NO-UNDO.

{syspara.i 150 1 3 iButNr INT}

FIND LAST TransLogg NO-LOCK WHERE 
  TransLogg.Butik = iButNr AND 
  TransLogg.TransNr >= 0 AND 
  TransLogg.SeqNr = 1 USE-INDEX TransLogg NO-ERROR.
IF AVAILABLE TransLogg THEN 
  iTransNr = TransLogg.TransNr + 1.
ELSE 
  iTransNr = 1.

ASSIGN 
  iSeqNr = 1
  .
  
hBuffer:BUFFER-FIELD("Butik"):BUFFER-VALUE   = iButNr.
hBuffer:BUFFER-FIELD("TransNr"):BUFFER-VALUE = iTransNr.
hBuffer:BUFFER-FIELD("SeqNr"):BUFFER-VALUE   = iSeqNr.
hBuffer:BUFFER-FIELD("Postert"):BUFFER-VALUE = FALSE.

