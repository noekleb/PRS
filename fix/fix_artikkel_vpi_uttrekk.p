DEFINE VARIABLE cFieldList AS CHARACTER NO-UNDO.
DEFINE VARIABLE ihBuffer AS HANDLE NO-UNDO.
DEFINE VARIABLE icSessionId AS CHARACTER NO-UNDO.
DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk AS LOG NO-UNDO.

CURRENT-WINDOW:WIDTH = 300.


FOR EACH TransLogg WHERE Postert = FALSE:
    DISPLAY
        TransLogg.Dato
        TransLogg.ArtikkelNr
        TransLogg.Kode
        WITH WIDTH 300.


    FIND STrekkode NO-LOCK WHERE
        Strekkode.Kode = TransLogg.Kode NO-ERROR.
    FIND VPIStrekkode NO-LOCK WHERE
        VPIStrekkode.Kode = TransLogg.Kode NO-ERROR.
    DISPLAY
        Strekkode.Kode  WHEN AVAILABLE Strekkode
        Strekkode.ArtikkelNr WHEN AVAILABLE STrekkode
        VPIStrekkode.Kode WHEN available VPIStrekkode
        VPIStrekkode.VareNr WHEN AVAILABLE VPIStrekkode
        WITH WIDTH 300.
        
    IF AVAILABLE VPIStrekkode THEN 
        DO:
          cFieldList = "tbChooseAll|LevKod|LinkVareNr|KjedeVare|Beskr|EkstStrTypeNavn|Gjennomfaktureres|LevFargKod|AntIPkn" +
                       "|LokPris|LevNr|Salgsenhet|KjedeRab%|ProdNr|Etikettekst1|KjedeSupRab%|VmId|BongTekst|Sasong" +
                       "|Anonseartikkel|LevDato1|LevDato2|LevDato3|LevDato4|Vg|InnkjopsPris|VPIBildekode|supRab%|RAvdNr" +
                       "|forhRab%|Mengde|AnbefaltPris|JamforEnhet|Pris" +
                       "|NyStrekkode|KorrStrekkode|Grunnsortiment".
          RUN artbas_new.p ('1' + ';' + cFieldList + ';' + VPIStrekkode.VareNr), 
                            ihBuffer, 
                            icSessionid, 
                            OUTPUT ocReturn, 
                            OUTPUT obOk).      
        END.

END.
