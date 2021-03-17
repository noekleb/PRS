/* Oppretter størrelse i størrelsestypen på ArtikkelNrSerie. */
DEFINE INPUT PARAMETER cStorl       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iStrTypeId   AS INTEGER   NO-UNDO.

DEFINE VARIABLE iSeqNr AS INTEGER NO-UNDO.

DO TRANSACTION:
  IF NOT CAN-FIND(FIRST StrTStr WHERE
                  StrTStr.StrTypeId = iStrTypeId AND
                  StrTStr.SoStorl   = cStorl) THEN 
  DO:
    FIND LAST StrTStr WHERE
      StrTStr.StrTypeId = iStrTypeId
      USE-INDEX StrTStr NO-ERROR.
    IF AVAILABLE StrTStr 
      THEN iSeqNr = StrTStr.SeqNr + 1.
      ELSE iSeqNr = 1.
      
    CREATE StrTStr.
    ASSIGN
      StrTStr.SeqNr     = iSeqNr
      StrTStr.STrTypeId = iStrTypeId
      StrTStr.SoStorl   = cStorl
      .
    RELEASE StrTStr.
  END.
END.

IF iStrTypeId > 0 AND CAN-FIND(StrType WHERE StrTypeId = iStrTypeId) THEN 
  RUN settStrTypeFelt.p (iStrTypeID).
RETURN 'OK'.