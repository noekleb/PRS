DEF BUFFER bufKampanjeHode FOR KampanjeHode.

FIND kampanjehode NO-LOCK WHERE KampanjeHode.kampanjeid = 124.
FIND bufKampanjeHode WHERE bufKampanjeHode.KampanjeId = 128.
ASSIGN
    bufKampanjeHode.Komplett = FALSE
    bufKampanjeHode.Leverandorkampanje = TRUE.
DISPLAY 
    KampanjeHode.Aktivert
    KampanjeHode.Komplett  
    KampanjeHode.NormalPris
    KampanjeHode.AvslagType
    KampanjeHode.LeverandorKampanje
       SKIP
    bufKampanjeHode.Aktivert
    bufKampanjeHode.Komplett  
    bufKampanjeHode.NormalPris
    bufKampanjeHode.AvslagType
    bufKampanjeHode.LeverandorKampanje
    .
