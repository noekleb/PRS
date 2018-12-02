
DEFINE VARIABLE cdatum AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE cText AS CHARACTER  NO-UNDO.
DEFINE VARIABLE tBorttag AS INTEGER    NO-UNDO.

cDatum = "C:\Documents and Settings\TinneA\My Documents\Menigo-HK-EAN-" + STRING(TODAY,"99-99-99") + ".dat".
cText = "Ean;Varubeskrivning;Varugrupp;Inpris;UtPris;Moms;Levnr;Levnamn;Bestnr".
                           
OUTPUT TO value(cDatum).

EXPORT DELIMITER ";" "Posttyp" "Ean" "Varubeskrivning" "Varugrupp" "ButikNr" "Inpris" "UtPris" "Datum" "Moms" "Levnr" "Levnamn" "Best.nr" "Varunr" "ProducentNr" "Producentnamn".

FOR EACH vare NO-LOCK.
    tBorttag = 0.
    FIND mva OF vare NO-ERROR.
    FIND produsent OF vare NO-ERROR.
    FOR EACH vareko OF vare NO-LOCK.
        IF vareko.kotype = 9 AND vareko.dato > TODAY THEN tBorttag = 1.
    END.
    IF tBorttag = 0 THEN
    DO:    
        FOR EACH pris OF vare NO-LOCK.
            FIND lev OF pris NO-ERROR.
            IF AVAILABLE pris THEN EXPORT DELIMITER ";" "Artikel" vare.ean varetekst hgr pris.butnr engrosn utprisn pris.dato 
                                                        mva.mva pris.levnr lev.navn pris.bestnr vare.artnr produsent.prodnr produsent.navn.
        END.
    END.
END.

FOR EACH tandem NO-LOCK.
    tBorttag = 0.
    FIND vare OF tandem NO-ERROR.
    FIND mva OF vare NO-ERROR.
    FIND produsent OF vare NO-ERROR.
    FOR EACH vareko OF vare NO-LOCK.
        IF vareko.kotype = 9 AND vareko.dato > TODAY THEN tBorttag = 1.
    END.
    IF tBorttag = 0 THEN
    DO:    
        FIND FIRST pris OF vare NO-ERROR.
        FIND lev OF pris NO-ERROR.
        IF AVAILABLE pris THEN EXPORT DELIMITER ";" "Tandem" tandemean varetekst hgr pris.butnr engrosn utprisn pris.dato
                                                                     mva.mva pris.levnr lev.navn pris.bestnr artnr produsent.prodnr produsent.navn.
    END.
END.

QUIT.
