/* get_fakturalinje.p
   Purpose: Hente bonglinjer for print av bongkopi
   Parameters: entry(1,icParam,"|"): Liste RowId's Bonghode
               entry(2,icParam,"|"): ""
   Note: Henting av linjer görs genom query mot hode
-------------------------------------------------------------------------*/              
 
   
DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
DEF INPUT  PARAM ohTempTable  AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK         AS LOG NO-UNDO.

DEF VAR httBuffer  AS HANDLE NO-UNDO.
DEF VAR hQuery     AS HANDLE NO-UNDO.
DEF VAR ix         AS INT    NO-UNDO.
DEF VAR bOk        AS LOG    NO-UNDO.
DEF VAR cEan       AS CHAR NO-UNDO.
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ohTempTable).
hQuery:SET-BUFFERS(ohTempTable).
hQuery:QUERY-PREPARE("FOR EACH " + ohTempTable:NAME).
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    cEan = "".
    FOR EACH strekkode WHERE strekkode.artikkelnr = DECI(ohTempTable:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE) and
                             strekkode.kodetype = 1 and
                             strekkode.strkode > 0 NO-LOCK.
        cEan = cEan + (IF cEan <> "" THEN "," ELSE "") + strekkode.kode.
    END.
    ohTempTable:BUFFER-FIELD("Ean"):BUFFER-VALUE = cEan.
    hQuery:GET-NEXT().
END.
hQuery:QUERY-CLOSE().
DELETE OBJECT hQuery.

/* IF icSessionId NE "validsession" THEN                                                                 */
/*   {incl/validatesession.i}                                                                            */
/*                                                                                                       */
/* RUN jbserv_gettemptablejoin.p                                                                         */
/*    (icSessionId,                                                                                      */
/*     1,                                                                                                */
/*     0,                                                                                                */
/*     "",                                                                                               */
/*     "BongLinje",                                                                                      */
/*       "WHERE false",                                                                                  */
/*     "",                                                                                               */
/*     "",                                                                                               */
/*     OUTPUT TABLE-HANDLE ohTempTable,                                                                  */
/*     OUTPUT cDummy,                                                                                    */
/*     OUTPUT ocReturn)                                                                                  */
/*     .                                                                                                 */
/*                                                                                                       */
/* ASSIGN httBuffer = ohTempTable:DEFAULT-BUFFER-HANDLE                                                  */
/*        cIDliste  = ENTRY(1,icParam,"|").                                                              */
/* DO ix = 1 TO NUM-ENTRIES(cIDliste):                                                                   */
/*     FOR BongHode NO-LOCK                                                                              */
/*         WHERE ROWID(BongHode) = TO-ROWID(ENTRY(ix,cIDliste)).                                         */
/*         FOR EACH BongLinje NO-LOCK                                                                    */
/*             WHERE BongLinje.B_id = BongHode.B_Id:                                                     */
/*             httBuffer:BUFFER-CREATE().                                                                */
/*             httBuffer:BUFFER-COPY(BUFFER BongLinje:HANDLE).                                           */
/*             IF BongLinje.TTId > 12 AND httBuffer:BUFFER-FIELD("BongTekst"):BUFFER-VALUE = "" THEN DO: */
/*                 FIND Transtype OF Bonglinje NO-LOCK NO-ERROR.                                         */
/*                 IF AVAIL TransType THEN                                                               */
/*                     httBuffer:BUFFER-FIELD("BongTekst"):BUFFER-VALUE = TransType.Beskrivelse.         */
/*             END.                                                                                      */
/*         END.                                                                                          */
/*     END.                                                                                              */
/* END.                                                                                                  */
/* DELETE OBJECT ohTempTable.                                                                            */
