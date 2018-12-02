DEF INPUT  PARAM icBuffer          AS CHAR NO-UNDO.
DEF INPUT  PARAM irRowid           AS ROWID NO-UNDO.
DEF INPUT  PARAM icFields          AS CHAR NO-UNDO.
DEF INPUT  PARAM icClientOrgValues AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocFieldsChanged   AS CHAR NO-UNDO.

DEF VAR hOrgBuffer AS HANDLE NO-UNDO.
DEF VAR ix         AS INT NO-UNDO.
DEF VAR hField     AS HANDLE.

CREATE BUFFER hOrgBuffer FOR TABLE icBuffer.
hOrgBuffer:FIND-BY-ROWID(irRowid,NO-LOCK) NO-ERROR.
IF hOrgBuffer:AVAIL THEN 
  DO ix = 1 TO NUM-ENTRIES(icFields):
    hField = hOrgBuffer:BUFFER-FIELD(ENTRY(ix,icFields)) NO-ERROR.
    IF VALID-HANDLE(hField) AND STRING(hField:BUFFER-VALUE) NE ENTRY(ix,icClientOrgValues,"|") AND hField:BUFFER-VALUE NE ? THEN
      ocFieldsChanged = ocFieldsChanged + ENTRY(ix,icFields) + CHR(3) 
                        + STRING(hField:BUFFER-VALUE) + CHR(3) 
                        + (IF hField:LABEL NE ? THEN hField:LABEL ELSE hField:NAME) + CHR(3) 
                        + ENTRY(ix,icClientOrgValues,"|") + CHR(1).
  END.
ocFieldsChanged = TRIM(ocFieldsChanged,CHR(1)).

DELETE OBJECT hOrgBuffer NO-ERROR.
