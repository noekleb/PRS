/* Setter første strekkode på artikkelen som hovednr. */
FOR EACH ArtBas NO-LOCK:
    FOR EACH Strekkode OF ArtBas:
        Strekkode.HovedNr = FALSE.
    END.
    FIND FIRST Strekkode OF ArtBas NO-ERROR.
    IF AVAILABLE Strekkode THEN
        Strekkode.HovedNr = TRUE.
END.
