DEF VAR iAnt AS INT NO-UNDO.
DEF VAR dDato AS DATE NO-UNDO.

FOR EACH Butiker  NO-LOCK WHERE Butiker.butik = 40:
DO dDato = 01/01/2020 TO 07/01/2020:
    FOR EACH BongHode NO-LOCK WHERE 
      BongHode.ButikkNr = Butiker.butik AND 
      BongHode.GruppeNr = 1 AND 
      BongHode.KasseNr = 1 AND 
      BongHode.Dato = dDato AND 
      BongHode.BongStatus = 7:
      
        /* NY PRS POS Flagger at det er en overføring og setter kundenr. for mottagende butikk. */
      IF CAN-FIND(FIRST BongLinje WHERE
        BongLinje.B_Id = BongHode.B_id AND
        BongLinje.TTId = 134) THEN
        DO:
          DISPLAY
            BongHode.ButikkNr
            BongHode.BongNr
            BongHode.Dato
            BongHode.BongStatus
            .
        END.

      
    END.
END.
END.
