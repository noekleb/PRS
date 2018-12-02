CURRENT-WINDOW:WIDTH = 300.
FOR EACH VareKo NO-LOCK WHERE
    VareKo.KoType = 9:

  /*
    DISPLAY
        VareKo.EAN
        STRING(VareKo.EAN)
        VareKo.KoType
        VareKo.Dato
        VareKo.Utpris
        Vareko.FraTil
        VareKo.FraTil2
        VareKo.Sendt
        WITH WIDTH 300.
  */

    FIND Strekkode NO-LOCK WHERE
        LEFT-TRIM(Strekkode.Kode,'0') = STRING(VareKo.EAN) NO-ERROR.
    IF AVAILABLE Strekkode THEN 
    DO:
      FIND ArtBas OF Strekkode EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE ArtBas THEN
          ASSIGN
            ArtBas.Utgatt = TRUE
            ArtBas.UtgattDato = VareKo.Dato
            .
        
    END.

END.
