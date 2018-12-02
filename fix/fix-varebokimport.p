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
    cFilNavn = 'C:\Appdir\Hestra_kjedesortiment.csv'
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

        
        dforhRab%   = DEC(ENTRY(7,cLinje,';')) /* Rab.forh */
        dsupRab%    = DEC(ENTRY(10,cLinje,';')) /* Rab.sup */

        dPris         = DEC(ENTRY(12,cLinje,';')) /* Markedspris */
        dAnbefaltPris = DEC(ENTRY(13,cLinje,';')) /* Veil.pris */
        dKampanjePris = DEC(ENTRY(14,cLinje,';')) /* Kamp.pris */


        cSortimentkoder = trim(ENTRY(16,cLinje,';'))
        cKampanjeuker   = trim(ENTRY(17,cLinje,';'))
        cKampanjestotte = trim(ENTRY(18,cLinje,';'))
        cLagerKoder     = trim(ENTRY(19,cLinje,';'))
        bKjedeVare         = IF can-do('yes',trim(ENTRY(21,cLinje,';'))) THEN TRUE ELSE FALSE
        bGjennomfaktureres = IF can-do('yes',trim(ENTRY(22,cLinje,';'))) THEN TRUE ELSE FALSE

        dKjedeRab% = DEC(ENTRY(23,cLinje,';')) /* Kj.rab% */
        .

    IF TRIM(ENTRY(15,cLinje,';')) <> '' THEN
        dLevDato1 = 09/06/2010.
    ELSE
        dLevDato1 = ?.

    cNyMerknad = (IF cSortimentkoder NE "" THEN 
                    cSortimentkoder + ","
                  ELSE "") + 
                 (IF cKampanjeuker NE "" THEN 
                    cKampanjeuker + ","
                  ELSE "") +
                 (IF cKampanjestotte NE "" THEN 
                    cKampanjestotte + ","
                  ELSE "") +
                 (IF cLagerkoder NE "" THEN 
                    cLagerkoder + ","
                  ELSE "").
     IF cNyMerknad NE "" THEN        
       cNyMerknad = SUBSTR(cNyMerknad,1,LENGTH(cNyMerknad) - 1).



    FIND FIRST VareBokLinje OF VareBokHode EXCLUSIVE-LOCK WHERE
        VareBokLinje.LevNr      = iLevNr AND
        VareBokLinje.LevKod     = LevKod AND
        VareBokLinje.Beskr      = cBeskr AND
        VareBokLinje.LevFargKod = cLevFargKod NO-ERROR.

    ASSIGN
        hVareBokLinje = BUFFER VareBokLinje:HANDLE.

    DISPLAY
        AVAILABLE VareBokLinje
        BUFFER vareboklinje:HANDLE
        VareBokLinje.LevNr      
        VareBokLinje.LevKod FORMAT "x(10)"    
        VareBokLinje.Beskr FORMAT "x(20)"     
        VareBokLinje.LevFargKod FORMAT "x(10)"

        dforhRab%
        dsupRab%

        dPris         
        dAnbefaltPris 
        dKampanjePris 
        dLevDato1
        cSortimentkoder FORMAT "x(10)"
        cKampanjeuker   FORMAT "x(10)"
        cKampanjestotte FORMAT "x(10)"
        cLagerKoder     FORMAT "x(10)"
        cNyMerknad      FORMAT "x(15)"
        
        bKjedeVare       
        bGjennomfaktureres

        dKjedeRab%

        WITH WIDTH 320.

    ASSIGN
        VareBokLinje.Pris              = dPris         
        VareBokLinje.AnbefaltPris      = dAnbefaltPris
        VareBokLinje.KampanjePris      = dKampanjePris
        VareBokLinje.LevDato1          = dLevDato1
        VareBokLinje.Sortimentkoder    = cSortimentkoder
        VareBokLinje.Kampanjeuker      = cKampanjeuker
        VareBokLinje.Kampanjestotte    = cKampanjestotte
        VareBokLinje.LagerKoder        = cLagerKoder
        VareBokLinje.LinjeMerknad      = cNyMerknad
        VareBokLinje.KjedeVare         = bKjedeVare
        VareBokLinje.Gjennomfaktureres = bGjennomfaktureres
        .


    IF dforhRab% NE 0 THEN DO:
        ASSIGN VareBokLinje.forhRab% = dforhRab%.
        RUN vareboklinje_kalkuler.p (hVareBokLinje,"forhRab%").
    END.
    IF dsupRab% NE 0 THEN DO:
        ASSIGN VareBokLinje.supRab% = dsupRab%.
        RUN vareboklinje_kalkuler.p (hVareBokLinje,"supRab%").
    END.
    IF dKjedeRab% NE 0 THEN DO:
        ASSIGN VareBokLinje.KjedeRab% = dKjedeRab%.
        RUN vareboklinje_kalkuler.p (hVareBokLinje,"KjedeRab%").
    END.

END.

INPUT STREAM Inn CLOSE.
