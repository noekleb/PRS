/* Samlet kalkulerte felter for registreringsbrowser, varebehandling, messe
   Opprettet 19.03.06 av BHa
--------------------------------------------------------------------------*/   

PROCEDURE ArtNavn:
  DEF INPUT  PARAM irVarebehLinjeTrans AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue             AS CHAR NO-UNDO.

  FIND VarebehLinjeTrans WHERE ROWID(VarebehLinjeTrans) = irVarebehLinjeTrans NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinjeTrans THEN DO:
    FIND FIRST VarebehLinje OF VarebehLinjeTrans NO-LOCK NO-ERROR.
    IF AVAIL VarebehLinje THEN
      ocValue = VarebehLinje.Beskr.
  END.
END PROCEDURE.

PROCEDURE SumAntStr:

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

END PROCEDURE.

PROCEDURE SumVerdiStr:

  DEF INPUT  PARAM irVarebehLinjeTrans AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue             AS CHAR NO-UNDO.

  DEF VAR iAntFord  AS INT NO-UNDO.

  FIND VarebehLinjeTrans WHERE ROWID(VarebehLinjeTrans) = irVarebehLinjeTrans NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinjeTrans THEN DO:
    FIND FIRST VarebehLinje OF VarebehLinjeTrans NO-LOCK NO-ERROR.
    IF AVAIL VarebehLinje THEN DO:
      FOR EACH ArtSort NO-LOCK
          WHERE ArtSort.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
            AND ArtSort.SortId     = VarebehLinjeTrans.Kode
         ,FIRST LevSort OF ArtSort NO-LOCK:

        FOR EACH LevSAnt OF LevSort NO-LOCK:
          iAntFord      = iAntFord + LevSAnt.SoAnt.
        END.
      END.
      ocValue = STRING((Bestilt1 + Bestilt2 + Bestilt3 + Bestilt4) * VarebehLinje.Varekost * MAX(1,iAntFord)).
    END.
  END.

END PROCEDURE.

PROCEDURE VisLevUke:

  DEF INPUT  PARAM iiLevUke       AS INT NO-UNDO.
  DEF INPUT  PARAM icSessionId    AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue        AS CHAR NO-UNDO.

  IF iiLevUke NE 0 THEN
    ocValue = SUBSTR(STRING(iiLevUke),5) + SUBSTR(STRING(iiLevUke),3,2).

END PROCEDURE.

PROCEDURE artsort_intervall:    
  DEF INPUT  PARAM irVarebehLinjeTrans   AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId           AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue               AS CHAR  NO-UNDO.
  
  DEF VAR cLastStr  AS CHAR NO-UNDO.
  
  FIND VarebehLinjeTrans WHERE ROWID(VarebehLinjeTrans) = irVarebehLinjeTrans NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinjeTrans THEN DO:
    FIND FIRST VarebehLinje OF VarebehLinjeTrans NO-LOCK NO-ERROR.
    IF AVAIL VarebehLinje THEN DO:
      FOR EACH ArtSort NO-LOCK
          WHERE ArtSort.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
            AND ArtSort.SortId     = VarebehLinjeTrans.Kode
         ,FIRST LevSort OF ArtSort NO-LOCK:
        FOR EACH LevSAnt NO-LOCK
            OF LevSort
            BY SeqNr:
          IF ocValue = "" THEN ocValue = SoStorl.
          cLastStr = SoStorl.
        END.
      END.
      ocValue = TRIM(ocValue) + " - " + TRIM(cLastStr).
    END.
  END.
END PROCEDURE.

PROCEDURE artsort_fordeling:    
  DEF INPUT  PARAM irVarebehLinjeTrans   AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId           AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue               AS CHAR  NO-UNDO.
    
  FIND VarebehLinjeTrans WHERE ROWID(VarebehLinjeTrans) = irVarebehLinjeTrans NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinjeTrans THEN DO:
    FIND FIRST VarebehLinje OF VarebehLinjeTrans NO-LOCK NO-ERROR.
    IF AVAIL VarebehLinje THEN DO:
      FOR EACH ArtSort NO-LOCK
          WHERE ArtSort.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
            AND ArtSort.SortId     = VarebehLinjeTrans.Kode
         ,FIRST LevSort OF ArtSort NO-LOCK:
        FOR EACH LevSAnt NO-LOCK
            OF LevSort
            BY SeqNr:
          ocValue = ocValue + STRING(SoAnt) + ",".
        END.
      END.
    END.
    ocValue = TRIM(ocValue,",").
  END.
END PROCEDURE.


PROCEDURE artsort_sumant:    
  DEF INPUT  PARAM irVarebehLinjeTrans   AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId           AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue               AS CHAR  NO-UNDO.
    
  DEF VAR iAnt    AS INT NO-UNDO.

  FIND VarebehLinjeTrans WHERE ROWID(VarebehLinjeTrans) = irVarebehLinjeTrans NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinjeTrans THEN DO:
    FIND FIRST VarebehLinje OF VarebehLinjeTrans NO-LOCK NO-ERROR.
    IF AVAIL VarebehLinje THEN DO:
      FOR EACH ArtSort NO-LOCK
          WHERE ArtSort.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
            AND ArtSort.SortId     = VarebehLinjeTrans.Kode
         ,FIRST LevSort OF ArtSort NO-LOCK:
        FOR EACH LevSAnt NO-LOCK
            OF LevSort
            BY SeqNr:
          iAnt = iAnt + SoAnt.
        END.
      END.
    END.
    ocValue = STRING(iAnt).
  END.
END PROCEDURE.
