DEF INPUT  PARAM iiUkedag    AS INT NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

CASE iiUkedag:
    WHEN 1 THEN ocValue = "Mandag".
    WHEN 2 THEN ocValue = "Tirsdag".
    WHEN 3 THEN ocValue = "Onsdag".
    WHEN 4 THEN ocValue = "Torsdag".
    WHEN 5 THEN ocValue = "Fredag".
    WHEN 6 THEN ocValue = "Lørdag".
    WHEN 7 THEN ocValue = "Søndag".
END CASE.

