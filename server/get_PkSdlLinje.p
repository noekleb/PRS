/* get_PkSdlLinje.p
   Purpose: Hengte PkSdlLinjer for print
   Parameters: entry(1,icParam,"|"): Liste
               entry(2,icParam,"|"): WHERE betingelse
   Note: Henting av linjer görs genom query mot hode
-------------------------------------------------------------------------*/              
 
   
DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE ohTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR cIDliste    AS CHAR  NO-UNDO.
DEF VAR cDummy      AS CHAR   NO-UNDO.
DEF VAR httBuffer   AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR hHodeTT     AS HANDLE NO-UNDO.
DEF VAR hTTHodeBuff AS HANDLE NO-UNDO.
DEF VAR qHode       AS HANDLE NO-UNDO.
DEF VAR cFarg       AS CHARACTER  NO-UNDO.
IF icSessionId NE "validsession" THEN
  {incl/validatesession.i}

RUN jbserv_gettemptablejoin.p
   (icSessionId,
    1,
    0,
    "",
    "PkSdlHode;PkSdlId",
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
    FOR FIRST PkSdlHode NO-LOCK
        WHERE PkSdlHode.PkSdlId = DEC(ENTRY(ix,cIDliste))
        :
      hTTHodeBuff:BUFFER-CREATE().
      hTTHodeBuff:BUFFER-COPY(BUFFER PkSdlHode:HANDLE).
    END.
  END.
END.

RUN jbserv_gettemptablejoin.p
   (icSessionId,
    1,
    0,
    "",
    "PkSdlLinje,PkSdlPris;NyPris;NyInnkjopsPris;NyRab1%;NyVarekost,_File;+Farge|CHAR|X(20)||",
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
    FOR EACH PkSdlLinje NO-LOCK
        WHERE PkSdlLinje.PkSdlId = DECIMAL(hTTHodeBuff:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE):
        FIND artbas OF PkSdlLinje NO-LOCK NO-ERROR.
        cFarg = IF AVAIL artbas THEN artbas.levfargkod ELSE "".
        IF AVAIL artbas AND artbas.levfargkod = "" THEN DO:
            FIND farg OF artbas NO-LOCK NO-ERROR.
            IF AVAIL farg THEN
                cFarg = farg.farbeskr.
        END.
        FIND FIRST PkSdlPris OF PkSdlLinje NO-LOCK NO-ERROR.
        httBuffer:BUFFER-CREATE().
        httBuffer:BUFFER-COPY(BUFFER PkSdlLinje:HANDLE).
        IF AVAIL PkSdlPris THEN DO:
            ASSIGN httBuffer:BUFFER-FIELD("Nypris"):BUFFER-VALUE = PkSdlPris.Nypris
                   httBuffer:BUFFER-FIELD("NyInnkjopsPris"):BUFFER-VALUE = PkSdlPris.NyInnkjopsPris
                   httBuffer:BUFFER-FIELD("NyRab1%"):BUFFER-VALUE = PkSdlPris.NyRab1%
                   httBuffer:BUFFER-FIELD("NyVarekost"):BUFFER-VALUE = PkSdlPris.NyVarekost.
        END.
        httBuffer:BUFFER-FIELD("Farge"):BUFFER-VALUE = cFarg.
    END.
    qHode:GET-NEXT().
END.
qHode:QUERY-CLOSE().
DELETE OBJECT qHode.
DELETE OBJECT hHodeTT.
DELETE OBJECT ohTempTable.
