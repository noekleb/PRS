DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO. 
DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE hTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR ix               AS INT    NO-UNDO.
DEF VAR bOK              AS LOG    NO-UNDO.
DEF VAR hBuffMenu        AS HANDLE NO-UNDO.
DEF VAR httBuffer        AS HANDLE NO-UNDO.
DEF VAR hQuery           AS HANDLE NO-UNDO.
DEF VAR bArtLag          AS LOG    NO-UNDO.
DEF VAR fArtikkelNr      AS DEC    NO-UNDO.
DEF VAR iButikkNr        AS INT    NO-UNDO.
DEF VAR bEgenButikk      AS LOG    NO-UNDO.

CREATE TEMP-TABLE hTempTable.
hTempTable:ADD-NEW-FIELD("Kode","CHARACTER").
hTempTable:ADD-NEW-FIELD("Storl","CHARACTER").
hTempTable:ADD-NEW-FIELD("StrKode","INTEGER").
hTempTable:ADD-NEW-FIELD("ButikkNr","INTEGER").
hTempTable:ADD-NEW-FIELD("Lagant","DECIMAL").
hTempTable:ADD-NEW-FIELD("ArtikkelNr","DECIMAL").
hTempTable:ADD-NEW-FIELD("SeqNr","INTEGER").

hTempTable:TEMP-TABLE-PREPARE("ArtStr").
httBuffer = hTempTable:DEFAULT-BUFFER-HANDLE.

IF icParam NE "" THEN DO:
  ASSIGN fArtikkelNr = DEC(ENTRY(1,icParam))
         iButikkNr   = INT(ENTRY(2,icParam))
         bEgenButikk = INT(ENTRY(3,icParam)) = 1
         .
  FOR FIRST ArtBas NO-LOCK
      WHERE ArtBas.ArtikkelNr = fArtikkelNr:
    
      FOR EACH Strekkode OF ArtBas:
          httBuffer:BUFFER-CREATE().
          httBuffer:BUFFER-COPY(BUFFER strekkode:HANDLE).
        END.
  END.
END.


