/* Oppretter størrelsestype på ArtikkelNrSerie. */
DEFINE INPUT PARAMETER iEkstVPILevNr AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER cVareNr       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iStrTypeId   AS INTEGER   NO-UNDO.

DEFINE VARIABLE iSeqNr AS INTEGER NO-UNDO.

FIND VPIArtBas NO-LOCK WHERE
  VPIArtBas.EkstVPILevNr = iEkstVPILevNr AND
  VPIArtBas.VareNr       = cVareNr NO-ERROR.  
IF NOT AVAILABLE VPIArtBas THEN 
  RETURN.

FIND StrType NO-LOCK WHERE
  StrType.StrTypeId = iStrTypeId NO-ERROR.
IF NOT AVAILABLE StrType THEN
  RETURN.

/* Setter siste brukte seqnr for størrelsestypen */
iSeqNr = 0.
FOR EACH StrTStr NO-LOCK WHERE
  StrTSTr.StrTypeId = StrType.StrTypeId
  BY StrTypeId
  BY SeqNr:
  iSeqNr = StrTStr.SeqNr.
END.

/* Legger opp de størrelser som ikke finnes i størrelsestypen. */
STREKKODE:
FOR EACH VPIStrekkode OF VPIArtBas NO-LOCK:
  FIND StrKonv NO-LOCK WHERE
    StrKonv.StrKode = VPIStrekkode.StrKode NO-ERROR.
  IF NOT AVAILABLE StrKonv THEN 
    NEXT.
    
  IF NOT CAN-DO(StrType.Fordeling,string(VPIStrekkode.StrKode)) THEN
  DO:
    iSeqNr = iSeqNr + 1.
    CREATE StrTStr.
    ASSIGN
      StrTStr.SeqNr     = iSeqNr
      StrTStr.STrTypeId = StrType.StrTypeId
      StrTStr.SoStorl   = StrKonv.Storl
      .    
  END. 
END. /* STREKKODE */

IF iStrTypeId > 0 AND CAN-FIND(StrType WHERE StrTypeId = iStrTypeId) THEN 
  RUN settStrTypeFelt.p (iStrTypeID).
RETURN 'OK'.