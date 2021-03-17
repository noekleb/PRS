DEF VAR iAntKjop AS INT NO-UNDO.
DEF VAR lInnkjPris AS DEC FORMAT '->>>,>>>,>>9.99' NO-UNDO.
DEF VAR iGmlLagerAnt AS INT NO-UNDO.
DEF VAR lGmlLagerVerdi AS DEC FORMAT '->>>,>>>,>>9.99' NO-UNDO.
DEF VAR lVVareKost AS DEC FORMAT '->>>,>>>,>>9.99' NO-UNDO.

ASSIGN 
    iAntKjop        = 100
    lInnkjPris      = 100
    iGmlLagerAnt    = 100
    lGmlLagerVerdi  = 10000
    .

lVVarekost = ((iAntKjop * lInnkjPris) + lGmlLagerVerdi) / (iAntKjop + iGmlLagerAnt).

MESSAGE lVVarekost
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
