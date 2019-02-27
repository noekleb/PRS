DEF INPUT  PARAM irVarebehLinjeTrans AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue             AS CHAR NO-UNDO.

DEF VAR iAntFord  AS INT NO-UNDO.

FIND VarebehLinjeTrans WHERE ROWID(VarebehLinjeTrans) = irVarebehLinjeTrans NO-LOCK NO-ERROR.
IF AVAIL VarebehLinjeTrans THEN DO:
  FOR EACH ArtSort NO-LOCK
      WHERE ArtSort.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
        AND ArtSort.SortId     = VarebehLinjeTrans.Kode
     ,FIRST LevSort OF ArtSort NO-LOCK:

    FOR EACH LevSAnt OF LevSort NO-LOCK:
      iAntFord      = iAntFord + LevSAnt.SoAnt.
    END.
  END.

  ocValue = STRING((Bestilt1 + Bestilt2 + Bestilt3 + Bestilt4) * MAX(1,iAntFord)).
END.
