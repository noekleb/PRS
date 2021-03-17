TRIGGER PROCEDURE FOR DELETE OF TelleHode.

DEF BUFFER trgHT-FilHode   FOR HT-FilHode.
DEF BUFFER trgHT-FilLinje  FOR HT-FilLinje.
DEF BUFFER trgTelleLinje   FOR TelleLinje.
DEF BUFFER tgtLokTelleHode FOR TelleHode.


/* Sletter linjene for tellelisten. */
/*
FOR EACH trgTelleLinje OF TelleHode:
    DELETE trgTelleLinje.
END.
*/

/* Sletter fillinjene for listen.  */
/*
IF TelleHode.FilId <> 0 THEN
FOR EACH VPIFilLinje EXCLUSIVE-LOCK WHERE
    VPIFilLinje.FilId = TelleHode.FilId:
    DELETE VPIFilLinje.
END.
/* Sletter filhode for listen. */
FIND VPIFilHode EXCLUSIVE-LOCK WHERE
    VPIFilHode.FilId = TelleHode.FilId NO-ERROR.
IF AVAILABLE VPIFilHode THEN
    DELETE VPIFilHode.
*/

/* Er tellelisten en telleliste, skal også de tilkoblede lokasjonslistene slettes */
/*
IF TelleHode.TelleType = 1 THEN
DO:
    FOR EACH tgtLokTelleHode EXCLUSIVE-LOCK WHERE
        tgtLokTelleHode.KobletTilTelleNr = TelleHode.TelleNr:
        /* Sletter linjene på lokasjonslisten. */
        FOR EACH trgTelleLinje OF tgtLokTelleHode:
            DELETE trgTelleLinje.
        END.
        /* Sletter fillinjene for listen.  */
        IF tgtLokTelleHode.FilId <> 0 THEN
        FOR EACH VPIFilLinje EXCLUSIVE-LOCK WHERE
            VPIFilLinje.FilId = tgtLokTelleHode.FilId:
            DELETE VPIFilLinje.
        END.
        /* Sletter filhode for listen. */
        FIND VPIFilHode EXCLUSIVE-LOCK WHERE
            VPIFilHode.FilId = tgtLokTelleHode.FilId NO-ERROR.
        IF AVAILABLE VPIFilHode THEN
            DELETE VPIFilHode.
        /* Sletter tellehode for lokasjonslisten. */
        DELETE tgtLokTelleHode.
    END.
END.
*/

/* Sletter gamle lister hvis det er gamle telleister som tas bort. */
/*
FOR EACH trgHT-FilHode WHERE
    trgHT-FilHode.TelleNr = TelleHode.TelleNr:
    FOR EACH trgHT-FilLinje OF trgHT-FilHode:
        DELETE trgHT-FilLinje.
    END.
    DELETE trgHT-FilHode.
END.
*/

