/* get_PkSdlHode.p
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
    "PkSdlHode,_File;+Status|CHAR|X(20)||;+Levnamn|CHAR|X(50)||",
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
    FOR FIRST PkSdlHode NO-LOCK
        WHERE PkSdlHode.PkSdlId = DEC(ENTRY(ix,cIDliste)):
      httBuffer:BUFFER-CREATE().
      httBuffer:BUFFER-COPY(BUFFER PkSdlHode:HANDLE).
      FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK NO-ERROR.
      IF AVAIL PkSdlLinje THEN
          FIND LevBas OF PkSdlLinje NO-LOCK NO-ERROR.
      FIND syspara NO-LOCK WHERE SysPara.SysHId = 5 AND SysPara.SysGr = 25 AND SysPara.ParaNr =
          PkSdlHode.PkSdlStatus NO-ERROR.
      IF AVAIL syspara THEN
          httBuffer:BUFFER-FIELD("Status"):BUFFER-VALUE = syspara.parameter1.
      IF AVAIL LevBas THEN
          httBuffer:BUFFER-FIELD("Levnamn"):BUFFER-VALUE = LevBas.LevNamn.
    END.
  END.
END.

DELETE OBJECT ohTempTable.
