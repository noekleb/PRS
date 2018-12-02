CURRENT-WINDOW:WIDTH = 200.

OUTPUT TO VALUE("rensArtLag.txt").
    EXPORT DELIMITER ";"
        "ArtikkelNR"
        "Vg"
        "LopNr"
        "Storl"
        "Butik"
        "Lagant"
        .
FOR EACH ArtLag WHERE
    NOT CAN-FIND(ArtBas WHERE
                 ArtBas.ArtikkelNr = ArtLAg.ArtikkelNr):
    EXPORT DELIMITER ";" 
        ArtLag.ArtikkelNr
        ArtLag.Vg
        ArtLag.LopNr
        ArtLag.Storl
        ArtLAg.Butik
        ArtLAg.Lagant
        .
END.
OUTPUT CLOSE.

DISPLAY
    "ArtLag"
    WITH WIDTH 198.

/*

ArtLag.ArtikkelNr
ArtPris.ArtikkelNr
Lager.ArtikkelNr
LevPris.ArtikkelNr
PrisKo.ArtikkelNr
BestHode.ArtikkelNr
BestPris.ArtikkelNr
TransLogg.ArtikkelNr
TelleLinje.ArtikkelNr
HPrisKo.ArtikkelNr
OvArt.ArtikkelNr
OvLinje.ArtikkelNr
OvBuffer.ArtikkelNr
KundeTrans.ArtikkelNr
MedTrans.ArtikkelNr
*/
