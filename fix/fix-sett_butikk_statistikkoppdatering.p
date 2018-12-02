FOR EACH Butiker:
    Butiker.StatistikkOppdatering = FALSE.
END.

CURRENT-WINDOW:WIDTH = 250.
FOR EACH ButikkTeam NO-LOCK 
    WHERE ButikkTeam.TeamNr = 12 AND
          ButikkTeam.teamTypeId = 3
    BY TeamNr:
    DISPLAY ButikkTeam
        WITH WIDTH 250.
    FOR EACH ButikkKobling OF butikkTeam:
        DISPLAY
            ButikkKobling
            WITH WIDTH 250.

        FIND Butiker EXCLUSIVE-LOCK WHERE
            Butiker.Butik = ButikkKobling.Butik NO-ERROR.
        IF AVAILABLE Butiker THEN
            butiker.StatistikkOppdatering = TRUE.
    END.

END.
