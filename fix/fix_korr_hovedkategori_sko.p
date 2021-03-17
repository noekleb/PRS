DEF VAR cVgLst AS CHAR NO-UNDO.
DEF VAR iVg AS INT FORMAT ">>>>>>9" NO-UNDO.
DEF VAR iLoop AS INT NO-UNDO.
DEF VAR cTekst AS CHAR NO-UNDO.
DEF VAR cChar AS CHAR NO-UNDO.
DEF VAR cCheckLst AS CHAR NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.
 
FORM 
WITH FRAME A WIDTH 350 DOWN.

/* Bygger liste med sjekkmønstre. */
DO iLoop = 2011 TO YEAR(TODAY):
    cCheckLst = cCheckLst + 
                (IF cCheckLst = '' THEN '' ELSE ',') + 
                SUBSTRING(STRING(iLoop,"9999"),3,2) + '5' + 
                (IF cCheckLst = '' THEN '' ELSE ',') + 
                SUBSTRING(STRING(iLoop,"9999"),3,2) + '6'.  
END.

MESSAGE cCheckLst
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
RETURN.


ASSIGN 
    cVgLst = '740050,710050'
    cCheckLst = '115' +
                ',116' +
                ',125' +
                ',126' +
                ',135' +
                ',136' +
                ',145' +
                ',146' +
                ',155' +
                ',156' +
                ',165' +
                ',166' +
                ',176' +
                ',176' +
                ',185' +
                ',186' + 
                ',195' +
                ',196' + 
                ',205' + 
                ',206' 
                .

DO iLoop = 1 TO NUM-ENTRIES(cVgLst):
    iVg = INT(ENTRY(iLoop,cVgLst)).
    ARTLOOP:
    FOR EACH ArtBas EXCLUSIVE-LOCK WHERE 
        ArtBas.Vg = iVg AND 
        LENGTH(ArtBas.LevKod) > 3 AND 
        CAN-DO(cCheckLst,SUBSTRING(ArtBas.LevKod,1,3)):

        cChar = SUBSTRING(ArtBas.LevKod,3,1).

        DISPLAY
            ArtBas.Vg
            ArtBas.ArtikkelNr
            ArtBas.Beskr
            ArtBas.LevKod
            ArtBas.LevFargKod
            ArtBas.Anv-Id
            ArtBas.HovedKatNr
            cChar
            (IF cChar = '5' THEN 'Dame' 
             ELSE IF cChar = '6' THEN 'Herre'
             ELSE '')
        WITH FRAME A WIDTH 350 DOWN.
        DOWN WITH FRAME A.

        IF cChar = '5' THEN
            ArtBas.HovedKatNr = 55.
        ELSE IF cChar = '6' THEN
            ArtBas.HovedKatNr = 56.
    
    END. /* ARTLOOP */

END.
