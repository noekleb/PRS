/*
Korreksjon av solgte varers kostpris i lager.
Summeres opp igjen ut fra transaksjonene.
*/

def buffer bLager for Lager.
DEF VAR plNettoPris  AS DEC NO-UNDO.
DEF VAR wsumSvk AS DEC.
DEF VAR wsumSolgt AS DEC.

PUBLISH 'infoDisp' ("Korrigerer SVK i Lager tabellen.").

FOR EACH LAger WHERE
    LAger.ArtikkelNr > 9999 AND
    Lager.AntSolgt <> 0
    BY LAger.ArtikkelNr DESCENDING:
    FIND ArtBas OF lager NO-LOCK.
                      
    ASSIGN
        wsumSVK = 0
        wsumsolgt = 0
        .
    TRANSLOGG:
    FOR EACH TransLogg OF Lager NO-LOCK:
        IF NOT CAN-DO("1,3,10",string(TransLogg.TTId)) THEN
            NEXT TRANSLOGG.
        ASSIGN
            wsumSvk = wsumSvk + (TransLogg.VVAreKost * TransLogg.Antall)
            wsumSolgt = wsumSolgt + TransLogg.Antall
            .
    END. /* TRANSLOGG */
    
    ASSIGN
        Lager.SVK = wsumSVK
        .
    PUBLISH 'infoDisp' ("Korr. SVK i lager artikkel(" + STRING(Lager.ArtikkelNr) + ".").
END.

PUBLISH 'infoDisp' ("Korr. SVK ferdig.").
