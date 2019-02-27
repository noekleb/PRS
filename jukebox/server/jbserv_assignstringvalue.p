/* Assign string value for datatypes in version 10+ */
DEF INPUT  PARAM ihField         AS HANDLE NO-UNDO.
DEF INPUT  PARAM iiExtent        AS INT    NO-UNDO.
DEF INPUT  PARAM icUpdateValue   AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOk            AS LOG INIT TRUE   NO-UNDO.

CASE ihField:DATA-TYPE:
  WHEN "character" THEN
    ihField:BUFFER-VALUE[iiExtent] = icUpdateValue NO-ERROR.
  WHEN "date" THEN 
    ihField:BUFFER-VALUE[iiExtent] = DATE(icUpdateValue) NO-ERROR.
  WHEN "decimal" THEN
    ihField:BUFFER-VALUE[iiExtent] = DEC(icUpdateValue) NO-ERROR.
  WHEN "integer" THEN
    ihField:BUFFER-VALUE[iiExtent] = INT(icUpdateValue) NO-ERROR.
  WHEN "int64" THEN
    ihField:BUFFER-VALUE[iiExtent] = INT64(icUpdateValue) NO-ERROR.
  WHEN "logical" THEN 
    ihField:BUFFER-VALUE[iiExtent] = (IF icUpdateValue = "yes" OR icUpdateValue = "true" THEN TRUE 
                                      ELSE IF icUpdateValue = "?" THEN ?
                                      ELSE FALSE)  NO-ERROR.
  WHEN "datetime" THEN
    IF iiExtent > 0 THEN
      ihField:BUFFER-VALUE[iiExtent] = DATETIME(icUpdateValue) NO-ERROR.
    ELSE 
      ihField:BUFFER-VALUE = DATETIME(icUpdateValue) NO-ERROR.
  WHEN "datetime-tz" THEN
    IF iiExtent > 0 THEN
      ihField:BUFFER-VALUE[iiExtent] = DATETIME-TZ(icUpdateValue) NO-ERROR.
    ELSE
      ihField:BUFFER-VALUE = DATETIME-TZ(icUpdateValue) NO-ERROR.
END CASE.

IF ERROR-STATUS:ERROR THEN obOk = no.
