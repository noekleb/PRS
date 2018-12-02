/* Nullstill bekreftet antall for plukkliste-linje
   Parametere: Buffer for query
   
   Opprettet: 21.08.09 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  FIND PlListeLinje EXCLUSIVE-LOCK 
       WHERE ROWID(PlListeLinje) = TO-ROWID(STRING(ihBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE))
       NO-WAIT NO-ERROR.
  IF AVAIL PlListeLinje THEN
    PlListeLinje.AntallPlukket = 0.
  hQuery:GET-NEXT().
END.


obOk = ocReturn = "".

