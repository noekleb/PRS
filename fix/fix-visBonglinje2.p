CURRENT-WINDOW:WIDTH = 350.
FOR EACH BongLinje:
    DISPLAY
        BongLinje.butik
        BongLinje.Dato
        BongLinje.KasseNr
        BongLinje.bongNr
        BongLinje.TTId
        BongLinje.TbId COLUMN-LABEL 'TBId'
        BongLinje.ArtikkelNr
        BongLinje.Bongtekst
        BongLinje.MvaGr
        BongLinje.Mva%
    WITH width 350.
END.
