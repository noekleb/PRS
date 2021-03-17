/*
Initierer VVarekost til varekost fra gjeldende kalkyle på artikkelen.
*/

def buffer bLager for Lager.
DEF VAR plNettoPris  AS DEC NO-UNDO.

PUBLISH 'infoDisp' ("Korrigerer null varekost iLager tabellen (vvarekost).").

/* Korrigerer VVareKost i Lager tabellen. */
for each lager EXCLUSIVE WHERE
    LAger.VVArekost =0:

    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = Lager.ArtikkelNr NO-ERROR.
    IF AVAILABLE ArtBas THEN
        find ArtPris NO-LOCK where
        ArtPris.ArtikkelNr = LAger.ArtikkelNr AND 
        ArtPris.ProfilNr   = 1 NO-ERROR.

    if available ArtPris then
    DO:
        IF ArtBas.OPris = FALSE THEN
            ASSIGN
                Lager.VVAreKost = ArtPris.VareKost[1]
                Lager.SVK       = Lager.AntSolgt * ArtPris.VareKost[1]
                .
    END.
end.

PUBLISH 'infoDisp' ("Korreksjon null vvarekost ferdig.").
