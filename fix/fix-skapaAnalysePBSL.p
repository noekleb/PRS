DEFINE VARIABLE cPLUer AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
ASSIGN cPLUer = 
   "27,7393528210086,28,7393528210109,29,7393528210093,30,7393528210116,31,7393528210017,32,7393528210024,33,7393528210055,34,7393528210062,35,7393528210079,39,7393528210031,40,7393528210048,41,42,43,44,45,46,47,48,49,50,51,52,53,54,94,95,288,1496,1497,1498,1499,1500,1501,1502,1503,1504,1505,1506,1507,1508,1509,1510,1512,1514,1515,1516,1517,1518,1519,1520,2727,2929,3030,3131,3232".
/* "27,7393528210086,28,7393528210109,29,7393528210093,30,7393528210116,31,7393528210017,32,7393528210024,33,7393528210055,34,7393528210062,35,7393528210079,39,7393528210031,40,7393528210048,41,42,43,44,45,46,47,48,49,50,51,52,53,54,94,95,288,1496,1497,1498,1499,1500,1501,1502,1503,1504,1505,1506,1507,1508,1509,1510,1512,1514,1515,1516,1517,1518,1519,1520,2727,2929,3030,3131,3232" */
CREATE Analyse.
ASSIGN Analyse.Aktiv        = TRUE
       Analyse.AktivertDato = TODAY
       Analyse.AnalyseId    = 1
       Analyse.AnalyseType  = 1
       Analyse.KNavn        = "SLKV"
       Analyse.Navn         = "SL-analys"
       Analyse.StartDato    = DATE(1,1,2003)
       Analyse.SluttDato    = DATE(12,31,2003).

DO iCount = 1 TO NUM-ENTRIES(cPLUer):
    FIND StrekKode WHERE StrekKode.Kode = ENTRY(iCount,cPLUer) NO-LOCK NO-ERROR.
    IF AVAIL StrekKode AND NOT CAN-FIND(FIRST AnalyseArtikkel WHERE
                          AnalyseArtikkel.AnalyseId = Analyse.AnalyseId AND
                          AnalyseArtikkel.ArtikkelNr = StrekKode.ArtikkelNr) THEN DO:
        CREATE AnalyseArtikkel.
        ASSIGN AnalyseArtikkel.Aktiv        = Analyse.Aktiv
               AnalyseArtikkel.AktivertDato = Analyse.AktivertDato
               AnalyseArtikkel.AnalyseId    = Analyse.AnalyseId
               AnalyseArtikkel.ArtikkelNr   = StrekKode.ArtikkelNr
               AnalyseArtikkel.StartDato    = Analyse.StartDato
               AnalyseArtikkel.SluttDato    = Analyse.SluttDato.
    END.
END.


