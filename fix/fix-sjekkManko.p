DEF VAR iant AS INT NO-UNDO.
DEF VAR iButNr AS INT NO-UNDO.
DEF VAR lDec AS DEC NO-UNDO.

DEF TEMP-TABLE ttKORdreHode 
    FIELD KOrdre_Id AS DEC FORMAT ">>>>>>>>>>>>>9"
    FIELD EkstOrdreNr LIKE KORdrEHode.EkstOrdreNr
    FIELD DatotidOpprettet LIKE KORdrEHode.DatoTidOpprettet
    FIELD Navn LIKE KORdrEHode.Navn
    FIELD Manko AS LOG
    INDEX HodeMako KOrdre_Id Manko.

DEF TEMP-TABLE ttKOrdreLinje
    FIELD KOrdreLinjeNr LIKE KOrdreLinje.KOrdreLinjeNr
    FIELD ButNr AS INT FORMAT ">>>>>9"
    FIELD KOrdre_Id AS DEC FORMAT ">>>>>>>>>>>>>9"
    FIELD EkstOrdreNr LIKE KORdrEHode.EkstOrdreNr
    FIELD Manko AS LOG
    FIELD ArtikkelNr AS DEC FORMAT ">>>>>>>>>>>>9"
    FIELD Beskr LIKE ArtBas.Beskr
    FIELD LevKod LIKE ArtBas.LevKod
    FIELD LevFargKod LIKE ArtBas.LevFargKod
    FIELD StrKode AS INT FORMAT ">>>>>9"
    FIELD Storl LIKE ArtLag.Storl
    FIELD Antall LIKE Lager.Lagant
    INDEX LinjeManko KOrdre_Id KOrdreLinjeNr ArtikkelNr ButNr StrKode.

DEF TEMP-TABLE ttArtBas
    FIELD ArtikkelNr AS DEC FORMAT ">>>>>>>>>>>>9"
    FIELD Beskr LIKE ArtBas.Beskr
    FIELD LevKod LIKE ArtBas.LevKod
    FIELD LevFargKod LIKE ArtBas.LevFargKod
    FIELD ButNr AS INT FORMAT ">>>>>9"
    FIELD StrKode AS INT FORMAT ">>>>>9"
    FIELD Storl LIKE ArtLag.Storl
    FIELD Lagant LIKE Lager.Lagant
    FIELD Bestant LIKE Lager.Lagant
    FIELD Diff LIKE Lager.Lagant
    INDEX Lager ArtikkelNr ButNr StrKode.

ASSIGN 
    iButNr = 15
    .
OPPRETTTBL:
FOR EACH KOrdreHode NO-LOCK WHERE 
    KORdrEHode.KOrdre_Id =  1190000001 AND
    KOrdreHode.LevStatus >= '10' AND 
    KORdrEHode.LevStatus <=  '55',
    EACH KOrdreLinje OF KOrdrEHode
    BREAK BY KOrdrEHode.DatotidOpprettet DESCENDING:

    IF KORdreHode.LevStatus = '50' THEN
        NEXT.

    /* Betalingslinjer o.l. */
    ASSIGN 
        lDec = DEC(KORdreLinje.VareNr) NO-ERROR.
    IF ERROR-STATUS:ERROR OR lDec = 0 THEN
        NEXT.
    FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr) NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
        NEXT.

    FIND FIRST ttKOrdreHode WHERE 
               ttKOrdreHode.KOrdre_Id = KOrdreHode.KOrdre_Id NO-ERROR.
    IF NOT AVAILABLE ttKOrdreHode THEN
    DO:
        CREATE ttKOrdreHode.
        BUFFER-COPY KOrdreHode TO ttKORdreHode.
    END.

    FIND FIRST ttKORdreLinje WHERE 
               ttKOrdreLinje.KOrdre_Id  = KORdreLinje.KOrdre_Id AND
               ttKORdreLinje.KOrdreLinjeNr = KOrdreLinje.KOrdreLinjeNr AND 
               ttKORdreLinje.ArtikkelNr = DEC(KOrdreLinje.VareNr) AND
               ttKOrdreLinje.ButNr      = KOrdrEHode.ButikkNr AND
               ttKORdreLinje.StrKode    = KOrdreLinje.StrKode NO-ERROR.
    IF NOT AVAILABLE ttKORdreLinje THEN
    DO:
        CREATE ttKOrdreLinje.
        BUFFER-COPY KOrdreLinje 
            TO ttKOrdreLinje
            ASSIGN 
                ttKORdreLinje.ButNr = KOrdrEHode.ButikkNr
                ttKOrdreLinje.EkstOrdreNr = KOrdrEHode.EkstOrdreNr
                .
        BUFFER-COPY ArtBas 
            TO ttKOrdreLinje.
    END.

    FIND FIRST ttArtBas WHERE 
        ttArtBas.ArtikkelNr = ArtBas.ArtikkelNr AND 
        ttArtBas.ButNr = KORdreHode.butikkNr AND
        ttArtBas.StrKode = KORdreLinje.StrKode NO-ERROR.
    IF NOT AVAILABLE ttArtBas THEN
    DO:
        CREATE ttArtBas.
        ASSIGN 
            ttArtBas.ArtikkelNr = ArtBas.ArtikkelNr 
            ttArtBas.ButNr = KORdreHode.butikkNr 
            ttArtBas.StrKode = KORdreLinje.StrKode
            ttArtBas.Beskr   = ArtBas.Beskr
            ttArtBas.LevKod  = ArtBas.LevKod
            .
        /* Summerer opp lager for størrelsen */    
        FOR EACH ArtLag NO-LOCK WHERE 
            ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
            ArtLag.Butik     >= 15 AND 
            ArtLag.butik     <= 16 AND 
            artLag.StrKode    = KOrdreLinje.StrKode:
            
            ASSIGN 
                ttArtBas.Lagant  = ttArtBas.Lagant + (IF ArtLag.lagant > 0 THEN ArtLag.lagant ELSE 0)
                ttArtBas.Storl   = ArtLag.Storl
                .
        END.
    END.

    ASSIGN 
        ttArtBas.BestAnt    = ttArtBas.BestAnt + ttKORdreLinje.Antall
        ttArtBas.Diff       = ttArtBas.Lagant - ttartBas.BestAnt
        ttKOrdreLinje.Manko = NOT ttArtBas.Lagant >= ttArtBas.Bestant
        ttKORdreHode.Manko  = ttKOrdreLinje.Manko        
        .
IF ttArtBas.ArtikkelNr = 9841236 THEN
MESSAGE 
    'KORdre_Id:' ttKOrdrEHode.KOrdre_Id SKIP
    'Hode Manko:' ttKOrdrEHode.Manko SKIP
    'Linje Manko:' ttKOrdreLinje.Manko SKIP
    'Linje antall:' ttKOrdreLinje.Antall SKIP
    'Artikkel:' ttArtBas.ArtikkelNr SKIP
    'Bestant:' ttArtBas.BestAnt SKIP
    'Lagant:' ttArtBas.Lagant SKIP
    'Diff:' ttArtBas.diff
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

    iAnt = iant + 1.
END. /* OPPRETTTBL */

TEMP-TABLE ttArtBas:WRITE-JSON('file', 'konv\MankoArtBas.json', TRUE).
TEMP-TABLE ttKORdreHode:WRITE-JSON('file', 'konv\MankoKORdreHode.json', TRUE).
TEMP-TABLE ttKOrdreLinje:WRITE-JSON('file', 'konv\MankoKORdreLinje.json', TRUE).

OUTPUT TO VALUE('konv\TESTmankoArtBas.csv').
    EXPORT DELIMITER ';' 
        'ArtikkelNr'
        'Varetekst'
        'LevKod'
        'LevFargKod'
        'StrKode'
        'Storl'
        'Lagant'
        'Bestant'
        'Diff'
        .
    FOR EACH ttArtBas WHERE 
                ttArtBas.diff < 0:
        EXPORT DELIMITER ';' ttArtBas .
    END.
OUTPUT CLOSE.

OUTPUT TO VALUE('konv\TESTmankoKORdreHode.csv').
    EXPORT DELIMITER ';' 
        'KOrdre_Id'
        'EkstOrdreNr'
        'DatotidOpprettet'
        'Navn'
        'Manko'
        .
    FOR EACH ttKOrdreHode WHERE 
                ttKOrdreHode.Manko = TRUE:
        EXPORT DELIMITER ';' 
            ttKOrdreHode .
    END.
OUTPUT CLOSE.

OUTPUT TO VALUE('konv\TESTmankoKORdreLinje.csv').
    EXPORT DELIMITER ';' 
        'ButNr'
        'KOrdre_Id'
        'EkstOrdreNr'
        'Manko'
        'ArtikkelNr'
        'Beskr'
        'LevKod'
        'LevFargKod'
        'StrKode'
        'Storl'
        'Antall'
        .
    FOR EACH ttKOrdreLinje WHERE 
                ttKOrdreLinje.Manko = TRUE:
        EXPORT DELIMITER ';' 
            ttKOrdreLinje.ButNr
            ttKOrdreLinje.KOrdre_Id
            ttKOrdreLinje.EkstOrdreNr
            ttKOrdreLinje.Manko
            ttKOrdreLinje.ArtikkelNr
            ttKOrdreLinje.Beskr
            ttKOrdreLinje.LevKod
            ttKOrdreLinje.LevFargKod
            ttKOrdreLinje.Storl
            ttKOrdreLinje.Antall
            .
    END.
OUTPUT CLOSE.

