DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iAar      AS INTEGER NO-UNDO.
DEFINE VARIABLE iMalIdFra AS INTEGER NO-UNDO.
DEFINE VARIABLE iMalIdTil AS INTEGER NO-UNDO.

ASSIGN
  iMalIdFra = INT(ENTRY(2,icParam,'|'))
  iMalIdTil = INT(ENTRY(1,icParam,'|'))
  iAar      = INT(ENTRY(3,icParam,'|'))
  .

DEFINE BUFFER bufSBudMalManed FOR SBudMalManed.
DEFINE BUFFER bufSBudMalDag   FOR SBudMalDag.
 
FOR EACH bufSBudMalManed NO-LOCK WHERE 
    bufSBudMalManed.MalI = iMalIdFra:
  CREATE SBudMalManed.
  BUFFER-COPY bufSBudMalManed
      EXCEPT MalId AArMnd
      TO SBudMalManed
      ASSIGN
        SBudMalManed.MalId = iMalIdTil
        SBudMalManed.AarMnd = INT(STRING(iAar,'9999') + SUBSTRING(STRING(bufSBudMalManed.AarMnd,'999999'),5,2))
        .
END.

FOR EACH bufSBudMalDag NO-LOCK WHERE 
    bufSBudMalDag.MalI = iMalIdFra:
  CREATE SBudMalDag.
  BUFFER-COPY bufSBudMalDag
      EXCEPT MalId AarMnd AarMndDag
      TO SBudMalDag
      ASSIGN
        SBudMalDag.MalId     = iMalIdTil
        SBudMalDag.AarMnd    = INT(STRING(iAar,'9999') + SUBSTRING(STRING(bufSBudMalDag.AarMnd,'999999'),5,2))
        SBudMalDag.AarMndDag = INT(STRING(iAar,'9999') + SUBSTRING(STRING(bufSBudMalDag.AarMndDag,'99999999'),5,4))
        .
END.

obOK = TRUE.
