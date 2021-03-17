DEFINE VARIABLE iButNr AS INTEGER INITIAL 2 NO-UNDO.
DEFINE VARIABLE iAntHK AS INTEGER NO-UNDO.
DEFINE VARIABLE iAntBut AS INTEGER NO-UNDO.

{asListeNettbutikkordre.i}

CURRENT-WINDOW:WIDTH = 250.
CURRENT-WINDOW:HEIGHT = 35.

RUN asListeNettbutikkordre.p (INPUT ibutNr,
                              OUTPUT iAntHK,
                              OUTPUT iAntBut,
                              OUTPUT  TABLE ttKOrdreHode
                             ).

FOR EACH ttKOrdreHode NO-LOCK WHERE 
    ttKOrdreHode.Butik = 2 AND   
    ttKOrdreHode.LevStatus <= '50' AND 
    DATE(ttKOrdreHode.DatoTidOpprettet) >= TODAY - 3
    :
    
    DISPLAY
        ttKOrdreHode.Butik
        ttKOrdreHode.LevFNr
        ttKOrdreHode.KOrdre_Id
        ttKOrdreHode.EkstOrdreNr
        ttKOrdreHode.Navn
        ttKOrdreHode.LevStatus
        ttKOrdreHode.DatoTidOpprettet
        ttKOrdreHode.ShipmentSendt COLUMN-LABEL 'Sendt utlev.butikk'
    WITH WIDTH 350.
END. /* LESORDRE */    
