DEF VAR iAnt AS INT NO-UNDO.
DEF VAR dDato AS DATE NO-UNDO.

FOR EACH Butiker  NO-LOCK:
DO dDato = 08/07/2020 TO TODAY:
    FOR EACH BongHode NO-LOCK WHERE 
      BongHode.ButikkNr = Butiker.butik AND 
      BongHode.GruppeNr = 1 AND 
      BongHode.KasseNr = 1 AND 
      BongHode.Dato = 08/07/2020 AND 
      BongHode.BongStatus = 7:
      
        /* NY PRS POS Flagger at det er en overføring og setter kundenr. for mottagende butikk. */
      IF CAN-FIND(FIRST BongLinje WHERE
        BongLinje.B_Id = BongHode.B_id AND
        BongLinje.TTId = 1 AND 
        BongLinje.Varegr = 9002 AND 
        BongLinje.LopeNr = 1) THEN
        iAnt = iAnt + 1.

      
      DISPLAY
        BongHode.ButikkNr
        BongHode.BongNr
        BongHode.Dato
        BongHode.BongStatus
      .
    END.
END.
END.
