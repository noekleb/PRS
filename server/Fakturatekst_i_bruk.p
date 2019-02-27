DEF INPUT  PARAM lFaktTekstNr AS INT  NO-UNDO.
DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

IF CAN-FIND(FIRST Butiker WHERE
            Butiker.FaktTekstNr = lFaktTekstNr) OR
    CAN-FIND(FIRST FakturaHode WHERE
            FakturaHode.FaktTekstNr = lFaktTekstNr)  OR
    CAN-FIND(FIRST KOrdreHode WHERE
            KOrdreHode.FaktTekstNr = lFaktTekstNr) 
    THEN ocValue = "*".
ELSE ocValue = "".
