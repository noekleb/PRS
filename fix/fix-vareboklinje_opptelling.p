/* Teller opp markerte og alle poster i en varebok. */

DEF VAR lVarebokNr AS DEC NO-UNDO.
DEF VAR iAntsortimentskoder AS INT  NO-UNDO.
DEF VAR iAntKampanjeuker    AS INT  NO-UNDO.
DEF VAR iAntKampanjestotte  AS INT  NO-UNDO.
DEF VAR iAntLagerkoder      AS INT  NO-UNDO.
DEF VAR iAntTotalt          AS INT  NO-UNDO.
DEF VAR cSortKodeLst        AS CHAR NO-UNDO.
DEF VAR iAntKoder           AS INT  EXTENT 10 NO-UNDO.

ASSIGN
    lVareBokNr = 9000011
    cSortKodeLst = "B1,B2,G,K"
    .

FIND VarebokHode NO-LOCK WHERE
    VareBokHode.VareBokNr = lVareBokNr NO-ERROR.
IF NOT AVAILABLE VareBokHode THEN
    RETURN "* Ukjent vareboknr. " + STRING(lVareBokNr) + ".".
LINJE:
FOR EACH VareBokLinje OF VareBokHode NO-LOCK:
    IF VareBokLinje.Sortimentkoder <> '' THEN DO:
        iAntsortimentskoder = iAntsortimentskoder + 1.
        IF ENTRY(1,cSortKodeLst) = VareBokLinje.Sortimentkoder THEN
            iAntKoder[1] = iAntKoder[1] + 1.
        IF ENTRY(2,cSortKodeLst) = VareBokLinje.Sortimentkoder THEN
            iAntKoder[2] = iAntKoder[2] + 1.
        IF ENTRY(3,cSortKodeLst) = VareBokLinje.Sortimentkoder THEN
            iAntKoder[3] = iAntKoder[3] + 1.
        IF ENTRY(4,cSortKodeLst) = VareBokLinje.Sortimentkoder THEN
            iAntKoder[4] = iAntKoder[4] + 1.
    END.
    IF VareBokLinje.Kampanjeuker <> '' THEN
        iAntKampanjeuker = iAntKampanjeuker + 1.  
    IF VareBokLinje.Kampanjestotte <> '' THEN
        iAntKampanjestotte = iAntKampanjestotte + 1.
    IF VareBokLinje.Lagerkoder <> '' THEN
        iAntLagerkoder = iAntLagerkoder + 1.    
    iAntTotalt          = iAntTotalt + 1.        
END. /* LINJE */

MESSAGE 
    'Antall:' SKIP
    'Sortimentskoder:' iAntsortimentskoder SKIP
    '               ' ENTRY(1,cSortKodeLst) + ': ' iantKoder[1] SKIP
    '               ' ENTRY(2,cSortKodeLst) + ': ' iantKoder[2] SKIP
    '               ' ENTRY(3,cSortKodeLst) + ': ' iantKoder[3] SKIP
    '               ' ENTRY(4,cSortKodeLst) + ': ' iantKoder[4] SKIP
    'Kampanjeuker: ' iAntKampanjeuker      SKIP
    'Kampanjestøtte: ' iAntKampanjestotte  SKIP
    'Lagerkoder: ' iAntLagerkoder          SKIP
    'Totalt: ' iAntTotalt         
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
