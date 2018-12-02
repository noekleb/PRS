FOR EACH BongHode WHERE
    BongHode.Dato = 12/12/2009:

    bonghode.EksportertDato = ?.

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

END.
