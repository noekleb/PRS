DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iSBudIdFra AS INTEGER NO-UNDO.
DEFINE VARIABLE iSBudIdTil AS INTEGER NO-UNDO.

ASSIGN
  iSBudIdFra = INT(ENTRY(2,icParam,'|'))
  iSBudIdTil = INT(ENTRY(1,icParam,'|'))
  .
  
DEFINE BUFFER bufSBudManed FOR SBudManed.
DEFINE BUFFER bufSBudDag   FOR SBudDag.
 
FOR EACH bufSBudManed NO-LOCK WHERE 
    bufSBudManed.SBudId = iSBudIdFra:
  CREATE SBudManed.
  BUFFER-COPY bufSBudManed
      EXCEPT SBudId
      TO SBudManed
      ASSIGN
        SBudManed.SBudId = iSBudIdTil
        .
END.

FOR EACH bufSBudDag NO-LOCK WHERE 
    bufSBudDag.SBudId = iSBudIdFra:
  CREATE SBudDag.
  BUFFER-COPY bufSBudDag
      EXCEPT SBudId
      TO SBudDag
      ASSIGN
        SBudDag.SBudId = iSBudIdTil
        .
END.

obOk = TRUE.