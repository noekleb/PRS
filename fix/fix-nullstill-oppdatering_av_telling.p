FIND Tellehode WHERE TelleNr = 1.
ASSIGN
    TelleHode.Oppdatert = ?.
FOR EACH TelleLinje OF TelleHode:
    TelleLinje.Oppdatert = FALSE.
END.
