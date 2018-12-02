FOR EACH Butiker NO-LOCK:
    FOR EACH BongHode EXCLUSIVE-LOCK WHERE
        BongHode.ButikkNr = butiker.butik AND
        BongHode.GruppeNr = 1 AND
        BongHode.KasseNr > 0 AND
        BongHode.Dato >= 12/10/2009 AND
        BongHode.Dato <= 12/12/2009:

        BongHode.EksportertDato = ?.

      ERPUT:
      DO:
        FIND ELogg WHERE 
             ELogg.TabellNavn     = "Bonghode" AND
             ELogg.EksterntSystem = "KONTAUTO"    AND
             ELogg.Verdier        = STRING(BongHode.B_Id) NO-ERROR.
        IF NOT AVAIL Elogg THEN DO:
            CREATE Elogg.
            ASSIGN ELogg.TabellNavn     = "Bonghode"
                   ELogg.EksterntSystem = "KONTAUTO"   
                   ELogg.Verdier        = STRING(Bonghode.B_Id).
        END.
        ASSIGN ELogg.EndringsType = 1 
               ELogg.Behandlet    = FALSE.
        RELEASE ELogg.
      END. /* ERPUT */

      /*
      FOR EACH BongLinje WHERE BongLinje.B_Id = BongHode.B_Id NO-LOCK:
          IF can-do('1,3,10',string(BongLinje.TTId))  THEN
              DISPLAY
              BongLinje.ArtikkelNr
              BongLinje.VareGr
              CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = DEC(BongLinje.ArtikkelNr)).
      END.
      */
    END.
END.
