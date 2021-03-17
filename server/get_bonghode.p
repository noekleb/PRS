/* get_fakturahode.p
   Purpose: Hente bonghoden for print av bongkopi
   Parameters: entry(1,icParam,"|"): Liste av RowId's bonghode
               entry(2,icParam,"|"): ""
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
IF icSessionId NE "validsession" THEN
  {incl/validatesession.i}

RUN jbserv_gettemptablejoin.p
   (icSessionId,
    1,
    0,
    "",
    "BongHode",
    "WHERE false, NO-LOCK",
    "",
    "",
    OUTPUT TABLE-HANDLE ohTempTable,
    OUTPUT cDummy,
    OUTPUT ocReturn)
    .

ASSIGN httBuffer = ohTempTable:DEFAULT-BUFFER-HANDLE
       cIDliste  = ENTRY(1,icParam,"|").

DO ix = 1 TO NUM-ENTRIES(cIDliste):
  FOR BongHode NO-LOCK
      WHERE ROWID(BongHode) = TO-ROWID(ENTRY(ix,cIDliste)).
    httBuffer:BUFFER-CREATE().
    httBuffer:BUFFER-COPY(BUFFER BongHode:HANDLE).
  END.
END.

DELETE OBJECT ohTempTable.
