/*
Initierer VVarekost til varekost fra gjeldende kalkyle på artikkelen.
*/

def buffer bLager for Lager.
DEF VAR plNettoPris  AS DEC NO-UNDO.

PUBLISH 'infoDisp' ("Korrigerer Lager tabellen (vvarekost).").

/* Korrigerer VVareKost i Lager tabellen. */
for each lager EXCLUSIVE:
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = Lager.ArtikkelNr NO-ERROR.

    /* Løse artlagsposter skal slettes */
    IF NOT AVAILABLE ArtBas THEN
    DO:
        FOR EACH ArtLag WHERE
            ArtLag.ArtikkelNr = Lager.ArtikkelNr AND
            ArtLag.Butik      = Lager.Butik:
            DELETE ArtLAg.
        END.
        DELETE LAger.
    END.
    ELSE DO:
        IF AVAILABLE ArtBas THEN
            find ArtPris NO-LOCK where
            ArtPris.ArtikkelNr = LAger.ArtikkelNr AND 
            ArtPris.ProfilNr   = 1 NO-ERROR.

        if available ArtPris AND AVAILABLE ArtBas then
        DO:
            IF ArtBas.OPris THEN
                ASSIGN
                Lager.VVAreKost = 0 /* Lagerantall er alltid null på disse. */
                Lager.SVK       = Lager.AntSolgt * ArtPris.VareKost[1]
                .
            ELSE
                ASSIGN
                    Lager.VVAreKost = ArtPris.VareKost[1]
                    Lager.SVK       = Lager.AntSolgt * ArtPris.VareKost[1]
                    .
        END.
    END.
end.

PUBLISH 'infoDisp' ("Korrigerer Artlag tabellen (vvarekost).").

FOR EACH TransLogg EXCLUSIVE-LOCK:
    FIND ArtBAs NO-LOCK WHERE
        ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
        FIND ArtBas NO-LOCK WHERE
             ArtBas.Vg = Translogg.Vg AND
             ArtBas.LopNr = Translogg.LopNr NO-ERROR.

    IF AVAILABLE ArtBas THEN
        find ArtPris NO-LOCK where
        ArtPris.ArtikkelNr = Translogg.ArtikkelNr AND 
        ArtPris.ProfilNr   = 1 NO-ERROR.

    if AVAILABLE ArtBas AND available ArtPris then
    DO:
        FIND VarGr NO-LOCK WHERE 
            VarGr.Vg = ArtBas.Vg NO-ERROR.
        IF ArtBAs.OPris THEN
        DO:
            ASSIGN
                plNettoPris         = TransLogg.Pris - TransLogg.Mva - Translogg.RabKr 
                TransLogg.VVareKost = (plNettoPris * VarGr.Kost_Proc) / 100 
                .
            /* Kobler transaksjonen rett. */
            IF TransLogg.ArtikkelNr = 0 THEN
                Translogg.ArtikkelNr = ArtBas.ArtikkelNr.
        END.
        ELSE
            Translogg.VVAreKost = ArtPris.VareKost[1].
    END.
END.

PUBLISH 'infoDisp' ("Korreksjon vvarekost ferdig.").
