/* get_fakturahode.p
   Purpose: Hengte fakturaer for print
   Parameters: entry(1,icParam,"|"): Liste
               entry(2,icParam,"|"): WHERE betingelse
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
    "Kampanjebutikker,_File;+Namn|CHAR|X(30)||",
    IF ENTRY(2,icParam,"|") = "" THEN
      "WHERE false, NO-LOCK"
    ELSE ENTRY(2,icParam,"|"),
    "",
    "",
    OUTPUT TABLE-HANDLE ohTempTable,
    OUTPUT cDummy,
    OUTPUT ocReturn)
    .

IF ENTRY(2,icParam,"|") = "" THEN DO:
  ASSIGN httBuffer = ohTempTable:DEFAULT-BUFFER-HANDLE
         cIDliste  = ENTRY(1,icParam,"|").
 
  DO ix = 1 TO NUM-ENTRIES(cIDliste):
    FOR EACH Kampanjebutikker NO-LOCK
        WHERE Kampanjebutikker.Kampid = DEC(ENTRY(ix,cIDliste))
        :
        FIND Butiker WHERE Butiker.Butik = Kampanjebutikker.Butik NO-LOCK NO-ERROR.
        IF AVAIL butiker THEN DO:
            httBuffer:BUFFER-CREATE().
            httBuffer:BUFFER-COPY(BUFFER Kampanjebutikker:HANDLE).
            httBuffer:BUFFER-FIELD("namn"):BUFFER-VALUE = Butiker.Butnamn.
        END.
    END.
  END.
END.

DELETE OBJECT ohTempTable.
