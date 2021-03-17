CURRENT-WINDOW:WIDTH = 350.

FOR EACH KAs_Rap WHERE 
    Kas_Rap.Butikk = 2 /* AND 
    Kas_Rap.Dato = 04/28/2020*/:

    DISPLAY
        /* Idx */
        kas_rap.Butikk
        Kas_Rap.Dato
        kas_rap.Kasse
        kas_rap.KassererNr
        z_nummer
        /* Idx slutt. */
        Kas_Rap.Veksel
        Kas_Rap.Vekselbeholdning
        Kas_Rap.Kontant
        kas_rap.Kont_inn
        Kas_Rap.Kont_Ut
        kas_rap.Bank
        kas_rap.Dropp
        kas_rap.MvaGrunnlag[1]
        kas_rap.MvaGrunnlag[2]
        kas_rap.MvaGrunnlag[3]

        Kas_rap.MvaBelop[1]
        Kas_rap.MvaBelop[2]
        Kas_rap.MvaBelop[3]
    WITH WIDTH 350.
END.
