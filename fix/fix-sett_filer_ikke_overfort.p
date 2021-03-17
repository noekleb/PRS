CURRENT-WINDOW:WIDTH = 250.

DEF VAR iButikkNr AS INT NO-UNDO.
/*
FOR EACH Butiker:
    Butiker.StatistikkOppdatering = FALSE.
END.

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
*/

FILERLOOOP:
FOR EACH Filer WHERE 
    Filer.Dato    >= TODAY:
     
  IF (Filer.Innlest  = TRUE AND
      Filer.Overfort = TRUE) THEN
  DO:
    ASSIGN
        iButikkNr = INT(ENTRY(5,Filer.Katalog,"\"))
        NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
        NEXT FILERLOOOP.

    /* Sjekker mot team for bemannede butikker */
    FIND ButikkKobling NO-LOCK WHERE
        ButikkKobling.teamNr = 12 AND
        ButikkKobling.TeamtypeId = 3 AND
        butikkKobling.Butik      = ibutikkNr NO-ERROR.

    IF AVAILABLE ButikkKobling THEN
    DO:
        DISPLAY 
          Filer.Dato
          Filer.FilNavn
          Filer.Katalog
          ENTRY(5,Filer.Katalog,"\")
          Filer.Innlest
          Filer.Oppdatert
          Filer.OVerfort
          WITH WIDTH 250.
    END.
  END.
END. /* FILERLOOOP */
