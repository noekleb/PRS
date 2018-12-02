DEF INPUT  PARAM irPackage   AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

DEF VAR fTotSize   AS DEC NO-UNDO.       

FIND JboxPackage WHERE ROWID(JBoxPackage) = irPackage NO-LOCK NO-ERROR.
IF AVAIL JBoxPackage THEN DO:
  FOR EACH JBoxDocRel NO-LOCK
      WHERE JBoxDocRel.cContex   = "JBoxPackage"
        AND JBoxDocRel.cEntityId = STRING(JBoxPackage.iJBoxPackageId),
      FIRST JBoxDocument OF JBoxDocRel NO-LOCK:
    fTotSize = fTotSize + JBoxDocument.iDocSize.
  END.
  ocValue = STRING(fTotSize / 1000).
END.

