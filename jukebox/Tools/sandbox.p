DEF VAR ix AS INT NO-UNDO.
DEF VAR bOK AS LOG NO-UNDO.
DEF VAR cProdList AS CHAR NO-UNDO INIT "Reise,Helse".
DEF VAR cDeknList AS CHAR NO-UNDO INIT "100.000,9.000.000".
DEF VAR cFile AS CHAR INIT "c:\temp\ttJson.txt".
DEF VAR httNewBasket AS HANDLE NO-UNDO.
DEF VAR hQuery AS HANDLE NO-UNDO.
DEF VAR hBuffer AS HANDLE NO-UNDO.

/*
DEF TEMP-TABLE ttBasket
    FIELD WebOrdreId    AS INT
    FIELD Produkt       AS CHAR
    FIELD Dekning       AS CHAR
    FIELD StartDato     AS DATE
    FIELD Belop         AS DEC
    FIELD SoneUtland    AS INT /* skal til postnr (om ikke feltet legges inn?) */
    FIELD Skole         AS CHAR
    FIELD SoktFraUtland AS LOG
    .

DO  ix = 1 TO 2:
  CREATE ttBasket.
  ASSIGN ttBasket.WebOrdreId    = ix
         ttBasket.Produkt       = ENTRY(ix,cProdList)
         ttBasket.Dekning       = IF ix = 1 THEN ? ELSE ENTRY(ix,cDeknList)
         ttBasket.StartDato     = TODAY
         ttBasket.Belop         = RANDOM(1000,5000)
         ttBasket.SoneUtland    = 1
         ttBasket.Skole         = "UCLA"
         ttBasket.SoktFraUtland = (IF ix = 1 THEN yes ELSE ?)
         .
END.

BUFFER ttBasket:WRITE-JSON ("file",cFile).
*/
CREATE TEMP-TABLE httNewBasket.
bOk = httNewBasket:READ-JSON ("file",cFile,"empty").
hBuffer = httNewBasket:DEFAULT-BUFFER-HANDLE.

OUTPUT TO VALUE(cFile) APPEND.

IF bOk THEN DO:
  PUT UNFORMATTED SKIP(2) "Felter og verdier fra generert tabell:" SKIP(1).

  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME).
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    DO ix = 1 TO hBuffer:NUM-FIELDS:
      PUT UNFORMATTED hBuffer:BUFFER-FIELD(ix):NAME " - "
                      hBuffer:BUFFER-FIELD(ix):DATA-TYPE " - "
                      hBuffer:BUFFER-FIELD(ix):BUFFER-VALUE
                      SKIP.  
    END.                 
    LEAVE.
/*    hQuery:GET-NEXT(). */
  END.
END.
OUTPUT CLOSE.

OS-COMMAND NO-WAIT VALUE("notepad " + cFile).
