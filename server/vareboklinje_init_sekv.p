/* Initiering av sekvensnummer for vareboklinje
   Parametere: Input: Vareboknr|Hg|querysort + querydesc
   
   Opprettet: 06.09.05 av BHa               
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix              AS INT    NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(BUFFER VarebokLinje:HANDLE,BUFFER ArtBas:HANDLE,BUFFER Varemerke:HANDLE).
hQuery:QUERY-PREPARE("FOR EACH VarebokLinje WHERE VarebokNr = " + ENTRY(1,icParam,"|") 
                     + " AND Hg = " + ENTRY(2,icParam,"|") 
                     + ",FIRST ArtBas OF VarebokLinje NO-LOCK,FIRST Varemerke OF ArtBas NO-LOCK"
                     + " BY " + ENTRY(3,icParam,"|")
                      ).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ix = ix + 10.
  FIND CURRENT VarebokLinje EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL VarebokLinje THEN
    VarebokLinje.Sekv = ix.

  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.
IF ocReturn = "" THEN obOk = TRUE.
