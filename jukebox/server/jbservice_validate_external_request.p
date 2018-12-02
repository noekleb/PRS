DEF INPUT  PARAM icPartnerId     AS CHAR NO-UNDO.
DEF INPUT  PARAM icPassPhrase    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOk            AS LOG  NO-UNDO.
DEF OUTPUT PARAM ocReturn        AS CHAR NO-UNDO.

DEF VAR ix      AS INT NO-UNDO.
DEF VAR cResult AS CHAR NO-UNDO.

DO ix = 0 TO LENGTH(icPartnerId) - 1:
  cResult = cResult + CHR(INT(SUBSTR(icPassPhrase,1 + ix * 7,5)) / MAX(INT(SUBSTR(icPassPhrase,6 + ix * 7,2)),1)).
END.

obOk = cResult = SUBSTR(icPartnerId,1,3).

IF obOk THEN DO:
  FIND FIRST JBoxPassPhraseValidation NO-LOCK
       WHERE JBoxPassPhraseValidation.cPassPhraseResolver = PROGRAM-NAME(1)
       NO-ERROR.
  IF AVAIL JBoxPassPhraseValidation AND NOT JBoxPassPhraseValidation.bUseUniqueKey THEN 
    RETURN.
  ELSE IF AVAIL JBoxPassPhraseValidation AND JBoxPassPhraseValidation.bUseUniqueKey THEN
    FIND FIRST JBoxExternalRequest NO-LOCK
         WHERE JBoxExternalRequest.cPassPhrase = icPassPhrase
           AND DATE(JBoxExternalRequest.dtRequest) > TODAY - JBoxPassPhraseValidation.iLimitUniqKeySearch
         NO-ERROR.
  ELSE /* Default: */
    FIND FIRST JBoxExternalRequest NO-LOCK
         WHERE JBoxExternalRequest.cPassPhrase = icPassPhrase
           AND DATE(JBoxExternalRequest.dtRequest) > TODAY - 100
         NO-ERROR.
  IF AVAIL JBoxExternalRequest THEN
    ASSIGN obOk     = NO 
           ocReturn = "Passphrase already used"
           .
END.
ELSE ocReturn = "Invalid passphrase".



