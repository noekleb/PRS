/* get_fakturalinje.p
   Purpose: Hente bonglinjer for print av bongkopi
   Parameters: entry(1,icParam,"|"): Liste RowId's Bonghode
               entry(2,icParam,"|"): ""
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
IF icSessionId NE "validsession" THEN
  {incl/validatesession.i}

RUN jbserv_gettemptablejoin.p
   (icSessionId,
    1,
    0,
    "",
    "BongLinje",
      "WHERE false",
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
        FOR EACH BongLinje NO-LOCK
            WHERE BongLinje.B_id = BongHode.B_Id:
            httBuffer:BUFFER-CREATE().
            httBuffer:BUFFER-COPY(BUFFER BongLinje:HANDLE).
/*             IF BongLinje.TTId > 12 AND httBuffer:BUFFER-FIELD("BongTekst"):BUFFER-VALUE = "" THEN DO: */
            IF BongLinje.TTId > 12 AND BongLinje.TTId <> 95 THEN DO:
                FIND Transtype OF Bonglinje NO-LOCK NO-ERROR.
                IF AVAIL TransType THEN
                    httBuffer:BUFFER-FIELD("BongTekst"):BUFFER-VALUE = TransType.Beskrivelse.
            END.
        END.
    END.
END.
DELETE OBJECT ohTempTable.
