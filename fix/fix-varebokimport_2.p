CURRENT-WINDOW:WIDTH = 320.

DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR iLevNr AS INT NO-UNDO.
DEF VAR cLevKod AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR cBeskr  AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR cLevFargKod AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR cLinje AS CHAR NO-UNDO.
DEF VAR dVareBokNr AS DEC FORMAT '>>>>>>>>>>>>9' NO-UNDO.
DEF VAR cNyMerknad AS CHAR NO-UNDO.
DEF VAR cSortimentkoder LIKE vareboklinje.Sortimentkoder NO-UNDO.
DEF VAR cKampanjeuker   LIKE vareboklinje.Kampanjeuker   NO-UNDO.
DEF VAR cKampanjestotte LIKE vareboklinje.Kampanjestotte NO-UNDO.
DEF VAR cLagerKoder     LIKE vareboklinje.Lagerkoder     NO-UNDO.
DEF VAR bKjedeVare      LIKE vareboklinje.KjedeVare      NO-UNDO.
DEF VAR bGjennomfaktureres LIKE vareboklinje.Gjennomfaktureres NO-UNDO.
DEF VAR dPris LIKE vareboklinje.Pris NO-UNDO.
DEF VAR dAnbefaltPris LIKE vareboklinje.AnbefaltPris NO-UNDO.
DEF VAR dKampanjePris LIKE vareboklinje.KampanjePris NO-UNDO.
DEF VAR dLevDato1 LIKE vareboklinje.levdato1 NO-UNDO.
DEF VAR dKjedeRab% LIKE vareboklinje.KjedeRab% NO-UNDO.
DEF VAR dforhRab% LIKE vareboklinje.forhRab% NO-UNDO.
DEF VAR dsupRab% LIKE vareboklinje.supRab% NO-UNDO.
DEF VAR hVarebokLinje AS HANDLE NO-UNDO.

DEF STREAM Inn.

ASSIGN
    /*cFilNavn = 'C:\Appdir\Norrona_Korrektur_kjedesortiment.csv'*/
    cFilNavn = 'C:\Appdir\Endelig_Bergans_korrektur.csv'
    dVareBokNr = 9000026
    .

FIND VareBokHode NO-LOCK WHERE
    VareBokHode.VareBokNr = dVareBokNr NO-ERROR.

INPUT STREAM Inn FROM VALUE(cFilNavn) NO-ECHO.

REPEAT:
    IMPORT STREAM Inn UNFORMATTED
        cLinje.

    ASSIGN
        iLevNr      = int(ENTRY(1,cLinje,';'))
        cBeskr      = trim(ENTRY(2,cLinje,';'))
        cLevKod     = trim(ENTRY(3,cLinje,';'))
        cLevFargKod = trim(ENTRY(4,cLinje,';'))
        bKjedeVare         = IF can-do('yes',trim(ENTRY(23,cLinje,';'))) THEN TRUE ELSE FALSE
        bGjennomfaktureres = IF can-do('yes',trim(ENTRY(24,cLinje,';'))) THEN TRUE ELSE FALSE
        .
    FIND FIRST VareBokLinje OF VareBokHode EXCLUSIVE-LOCK WHERE
        VareBokLinje.LevNr      = iLevNr AND
        VareBokLinje.LevKod     = LevKod AND
        VareBokLinje.Beskr      = cBeskr AND
        VareBokLinje.LevFargKod = cLevFargKod NO-ERROR.

    ASSIGN
        hVareBokLinje = BUFFER VareBokLinje:HANDLE.

    IF AVAILABLE VareBokLinje THEN
    DISPLAY
        AVAILABLE VareBokLinje
        BUFFER vareboklinje:HANDLE
        VareBokLinje.LevNr      
        VareBokLinje.LevKod FORMAT "x(10)"    
        VareBokLinje.Beskr FORMAT "x(20)"     
        VareBokLinje.LevFargKod FORMAT "x(10)"
        VareBokLinje.KjedeVare         
        VareBokLinje.Gjennomfaktureres 
        
        bKjedeVare       
        bGjennomfaktureres
        WITH WIDTH 320.
    ELSE
        DISPLAY
        iLevNr     
        cBeskr     
        cLevKod    
        cLevFargKod
        WITH WIDTH 320.


    IF AVAILABLE VareBokLinje THEN
    ASSIGN
        VareBokLinje.KjedeVare         = bKjedeVare
        VareBokLinje.Gjennomfaktureres = bGjennomfaktureres
        .
END.

INPUT STREAM Inn CLOSE.
