DEF INPUT  PARAM lPurretrinn AS INT  NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

IF CAN-FIND(FIRST Kundereskontr WHERE
            Kundereskontr.Purretrinn = lPurretrinn) 
    THEN ocValue = "*".
ELSE ocValue = "".


