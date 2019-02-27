/* get_fakturalinje.p
   Purpose: Hengte fakturalinjer for print
   Parameters: entry(1,icParam,"|"): Liste
               entry(2,icParam,"|"): WHERE betingelse
   Note: Henting av linjer görs genom query mot hode
-------------------------------------------------------------------------*/              
 
   
DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE ohTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR cIDliste   AS CHAR  NO-UNDO.
DEF VAR cDummy     AS CHAR   NO-UNDO.
DEF VAR httBuffer  AS HANDLE NO-UNDO.
DEF VAR hQuery     AS HANDLE NO-UNDO.
DEF VAR ix         AS INT    NO-UNDO.
DEF VAR bOk        AS LOG    NO-UNDO.
DEF VAR hHodeTT    AS HANDLE NO-UNDO.
DEF VAR hTTHodeBuff  AS HANDLE NO-UNDO.
DEF VAR qHode      AS HANDLE NO-UNDO.
IF icSessionId NE "validsession" THEN
  {incl/validatesession.i}

RUN jbserv_gettemptablejoin.p
   (icSessionId,
    1,
    0,
    "",
    "FakturaHode;Faktura_id",
    IF ENTRY(2,icParam,"|") = "" THEN
      "WHERE false"
    ELSE ENTRY(2,icParam,"|"),
    "",
    "",
    OUTPUT TABLE-HANDLE hHodeTT,
    OUTPUT cDummy,
    OUTPUT ocReturn)
    .

ASSIGN hTTHodeBuff = hHodeTT:DEFAULT-BUFFER-HANDLE
       cIDliste    = ENTRY(1,icParam,"|").
IF ENTRY(2,icParam,"|") = "" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cIDliste):
    FOR FIRST FakturaHode NO-LOCK
        WHERE FakturaHode.Faktura_id = DEC(ENTRY(ix,cIDliste))
        :
      hTTHodeBuff:BUFFER-CREATE().
      hTTHodeBuff:BUFFER-COPY(BUFFER FakturaHode:HANDLE).
    END.
  END.
END.

RUN jbserv_gettemptablejoin.p
   (icSessionId,
    1,
    0,
    "",
    "FakturaLinje",
      "WHERE false",
    "",
    "",
    OUTPUT TABLE-HANDLE ohTempTable,
    OUTPUT cDummy,
    OUTPUT ocReturn)
    .
ASSIGN httBuffer = ohTempTable:DEFAULT-BUFFER-HANDLE.
CREATE QUERY qHode.
qHode:SET-BUFFERS(hTTHodeBuff).
qHode:QUERY-PREPARE("FOR EACH " + hTTHodeBuff:NAME).
qHode:QUERY-OPEN().
qHode:GET-FIRST().
REPEAT WHILE NOT qHode:QUERY-OFF-END:
    FOR EACH FakturaLinje NO-LOCK
        WHERE FakturaLinje.Faktura_id = DECIMAL(hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE):
        httBuffer:BUFFER-CREATE().
        httBuffer:BUFFER-COPY(BUFFER FakturaLinje:HANDLE).
    END.
    qHode:GET-NEXT().
END.
qHode:QUERY-CLOSE().
DELETE OBJECT qHode.
DELETE OBJECT hHodeTT.
DELETE OBJECT ohTempTable.
