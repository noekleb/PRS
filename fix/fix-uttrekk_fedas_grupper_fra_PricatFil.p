DEF VAR cFil AS CHAR NO-UNDO.
DEF VAR cUtFil AS CHAR NO-UNDO.
DEF VAR cRecord AS CHAR FORMAT "x(200)" NO-UNDO.
DEF VAR cEDB-System AS CHAR NO-UNDO.
DEF VAR cEan AS CHAR NO-UNDO.
DEF VAR cEgenVg AS CHAR NO-UNDO.
DEF VAR lHarMapping AS LOG NO-UNDO.

DEF TEMP-TABLE tmpFedas
    FIELD Varemerke AS CHAR
    FIELD Gruppe AS CHAR
    FIELD Varetekst AS CHAR
    FIELD EgenVg AS CHAR 
    FIELD HarMapping AS LOG
    .

DEF STREAM Inn.
DEF STREAM Ut.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN
    cFil        = 'C:\NSoft\Polygon\PRS\kom\in\SportNorge\Pricat\Bergans01062018\AlleProdukter01062018.csv'
    cUtFil      = 'C:\NSoft\Polygon\PRS\kom\in\SportNorge\Pricat\Bergans01062018\FedasGrupper01062018.csv'
    cEDB-System = 'Sport Norge'
    .

INPUT STREAM Inn FROM VALUE(cFil).
OUTPUT STREAM Ut TO VALUE(cUtFil).
PUT STREAM Ut
    'Varemerke;FedasGrp;Varetekst;EgenVg;Har mapping'
    SKIP.

REPEAT:
    IMPORT STREAM Inn UNFORMATTED
        cRecord.

    ASSIGN 
        cEan = TRIM(ENTRY(5,cRecord,';'))
        .

    IF NOT CAN-FIND(tmpFedas WHERE 
                    tmpFedas.Gruppe = ENTRY(4,cRecord,';')) THEN
    DO:
        ASSIGN 
            cEgenVg = ''
            lHarMapping = FALSE
            .

        /* Sjekker mapping tabell */
        FIND FIRST ImpKonv NO-LOCK WHERE 
            ImpKonv.EDB-System = cEDB-System AND 
            ImpKonv.Tabell     = 'VarGr' AND 
            ImpKonv.EksterntId = ENTRY(4,cRecord,';') NO-ERROR.
        IF AVAILABLE ImpKonv 
            THEN 
                ASSIGN 
                    lHarMapping = TRUE
                    cEgenVg     = ImpKonv.InterntId
                    .

        /* Sjekker om EAN koden ligger på en artikkel med gyldig varegruppe. */
        IF cEgenVg = '' THEN
        DO:
            FIND Strekkode NO-LOCK WHERE 
                Strekkode.Kode = cEan NO-ERROR.
            IF AVAILABLE Strekkode THEN
            DO:
                FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
                IF AVAILABLE ArtBas THEN
                    cEgenVg = STRING(ArtBas.Vg).
            END.
        END.
             
        CREATE tmpFedas.
        ASSIGN 
            tmpFedas.Varemerke  = ENTRY(3,cRecord,';')
            tmpFedas.Gruppe     = ENTRY(4,cRecord,';')
            tmpFedas.Varetekst  = ENTRY(10,cRecord,';')
            tmpFedas.EgenVg     = cEgenvg 
            tmpFedas.HarMapping = lHarMapping
            .
        /*
        DISPLAY
            tmpFedas.Varemerke FORMAT "x(20)"
            tmpFedas.Gruppe    FORMAT "x(10)"
            tmpFedas.Varetekst  FORMAT "x(40)"
            tmpFedas.EgenVg   FORMAT "x(10)" 
            cRecord
        WITH WIDTH 350.
        */
        
        PUT STREAM Ut UNFORMATTED 
            tmpFedas.Varemerke ';' 
            tmpFedas.Gruppe ';'    
            tmpFedas.Varetekst ';' 
            tmpFedas.EgenVg ';'
            tmpFedas.HarMapping
        SKIP. 
    END.
END.
OUTPUT STREAM Ut CLOSE.
INPUT STREAM Inn CLOSE.



