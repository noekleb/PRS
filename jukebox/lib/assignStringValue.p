/* Assign string value for datatypes in version 10+ */
DEF INPUT  PARAM ihField         AS HANDLE NO-UNDO.
DEF INPUT  PARAM icUpdateValue   AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOk            AS LOG init true   NO-UNDO.

CASE ihField:DATA-TYPE:
  WHEN "datetime" THEN
    ihField:BUFFER-VALUE = DATETIME(icUpdateValue) NO-ERROR.
  WHEN "datetime-tz" THEN
    ihField:BUFFER-VALUE = DATETIME-TZ(icUpdateValue) NO-ERROR.
  WHEN "int64" THEN
    ihField:BUFFER-VALUE = INT64(icUpdateValue) NO-ERROR.
END CASE.

IF ERROR-STATUS:ERROR THEN obOk = false.
