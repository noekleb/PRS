DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
DEF INPUT PARAM  icParam       AS CHAR  NO-UNDO.
DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO.

DEF VAR fSumVerdi    AS DEC NO-UNDO.
DEF VAR iAntFord     AS INT NO-UNDO.

IF icParam NE "" THEN DO:

  FIND VareBehLinje WHERE ROWID(VareBehLinje) = irBuffer NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinje THEN DO:
    FOR EACH VareBehLinjeTrans OF VarebehLinje NO-LOCK
        WHERE ButikkNr = INT(ENTRY(1,icParam,"¤")) AND
        (Bestilt1 > 0 OR
         Bestilt2 > 0 OR
         Bestilt3 > 0 OR
         Bestilt4 > 0):
  
      FOR EACH ArtSort NO-LOCK
          WHERE ArtSort.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
            AND ArtSort.SortId     = VarebehLinjeTrans.Kode
         ,FIRST LevSort OF ArtSort NO-LOCK:
  
        FOR EACH LevSAnt OF LevSort NO-LOCK:
          iAntFord      = iAntFord + LevSAnt.SoAnt.
        END.
      END.
      fSumVerdi = (Bestilt1 + Bestilt2 + Bestilt3 + Bestilt4) * VarebehLinje.Varekost * MAX(1,iAntFord).
    END.

    IF NUM-ENTRIES(icParam,"¤") > 1 AND INT(ENTRY(2,icParam,"¤")) > fSumVerdi THEN ocReturn = "skiprow".
    ELSE ocReturn = STRING(fSumVerdi).
  END.
END.
