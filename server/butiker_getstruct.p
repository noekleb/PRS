DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO. 
DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE hTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR ix               AS INT NO-UNDO.
DEF VAR bOK              AS LOG NO-UNDO.
DEF VAR hBuffMenu        AS HANDLE NO-UNDO.
DEF VAR httBuffer        AS HANDLE NO-UNDO.
DEF VAR hQuery           AS HANDLE NO-UNDO.
DEF VAR cQueryCrit       AS CHAR   NO-UNDO.
DEF VAR bOnlyCL          AS LOG    NO-UNDO.

DEF BUFFER bButiker      FOR Butiker.

ASSIGN cQueryCrit = ENTRY(1,icParam,";")
       bOnlyCL    = IF ENTRY(2,icParam,";") = "CL" THEN YES ELSE NO
       .

/* Sikrer at data i butikkregister alltid er konsistent mhp sentrallager og clButikkNr: */
FOR EACH Butiker
    WHERE clButikkNr = 0:
  ASSIGN clButikkNr = Butik
         Sentrallager = YES.
END.


CREATE TEMP-TABLE hTempTable.
CREATE BUFFER hBuffMenu FOR TABLE "Butiker".

DO ix = 1 TO hBuffMenu:NUM-FIELDS:
  hTempTable:ADD-LIKE-FIELD(hBuffMenu:BUFFER-FIELD(ix):NAME,hBuffMenu:BUFFER-FIELD(ix)).
END.
hTempTable:ADD-NEW-FIELD("iParentButik","INTEGER").
hTempTable:ADD-NEW-FIELD("iRootButik","INTEGER").
hTempTable:ADD-NEW-FIELD("iLevel","INTEGER").
hTempTable:ADD-NEW-FIELD("iNodeIndex","INTEGER").
hTempTable:ADD-NEW-FIELD("iColourCode","INTEGER").
hTempTable:ADD-NEW-FIELD("RowIdent1","CHARACTER").
hTempTable:TEMP-TABLE-PREPARE("Butiker").
httBuffer = hTempTable:DEFAULT-BUFFER-HANDLE.

ix = 0.
CREATE QUERY hQuery.
hQuery:ADD-BUFFER(BUFFER Butiker:HANDLE).
hQuery:QUERY-PREPARE("FOR EACH Butiker NO-LOCK " + cQueryCrit) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  ocReturn = "Failed to open the initial query: " + cQueryCrit + CHR(10) +
             ERROR-STATUS:GET-MESSAGE(1).
END.
hQuery:QUERY-OPEN() NO-ERROR.
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF bOnlyCL AND NOT CAN-FIND(FIRST Butiker 
                              WHERE Butiker.clButikkNr = INT(hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Butik"):BUFFER-VALUE)
                                AND Butiker.clButikkNr NE Butiker.Butik) THEN
    hQuery:GET-NEXT().
  ELSE DO:
    RUN CreateButik (hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Butik"):BUFFER-VALUE,0,1).
    hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hTempTable.
DELETE OBJECT hBuffMenu.
DELETE OBJECT hQuery.

PROCEDURE CreateButik:
  DEF INPUT PARAM iiButik       AS INT NO-UNDO.
  DEF INPUT PARAM iiParentButik AS INT NO-UNDO.
  DEF INPUT PARAM iiLevel        AS INT NO-UNDO.

  FIND bButiker WHERE bButiker.Butik = iiButik NO-LOCK.

  httBuffer:BUFFER-CREATE().
  httBuffer:BUFFER-COPY(BUFFER bButiker:HANDLE).
  ASSIGN ix = ix + 1
         httBuffer:BUFFER-FIELD("iParentButik"):BUFFER-VALUE  = iiParentButik
         httBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE     = ix
         httBuffer:BUFFER-FIELD("iLevel"):BUFFER-VALUE         = iiLevel
         httBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE      = STRING(ROWID(bButiker))
         .

  FOR EACH Butiker NO-LOCK
      WHERE Butiker.clButikkNr = bButiker.Butik
        AND NOT Butiker.Sentrallager
        AND Butiker.clButikkNr NE Butiker.Butik
         BY Butiker.Butik:
    RUN CreateButik (Butiker.Butik,Butiker.clButikkNr,iiLevel + 1).
  END.
END PROCEDURE.
