/* Logging av utskrift i Etikettko
   Opprettet: 05.02.10 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR iLastUtskrNr    AS INT    NO-UNDO INIT 1.

FOR EACH Etikettko NO-LOCK
    BY Etikettko.Utskriftsnr DESC:
  iLastUtskrNr = Etikettko.Utskriftsnr + 1.
  LEAVE.
END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  FIND Etikettko EXCLUSIVE-LOCK 
       WHERE ROWID(Etikettko) = TO-ROWID(string(ihBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE))
       NO-ERROR.

  IF AVAIL Etikettko THEN
    ASSIGN Etikettko.Utskriftsnr    = iLastUtskrNr
           Etikettko.UtskriftsDato  = TODAY
           .
  hQuery:GET-NEXT().
END. 

DELETE OBJECT hQuery NO-ERROR.

obOK = ocReturn = "".
