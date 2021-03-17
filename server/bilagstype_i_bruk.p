DEF INPUT  PARAM lBilagsType AS INT  NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

IF CAN-FIND(FIRST Kundereskontr WHERE
            Kundereskontr.BilagsType = lBilagsType) OR
    CAN-FIND(FIRST FakturaHode WHERE
            FakturaHode.BilagsType = lBilagsType) 
    THEN ocValue = "*".
ELSE ocValue = "".
