/* fix-sett-pf-flagg.p */
DEF VAR iAnt     AS INT  NO-UNDO.
DEF VAR cKatalog AS CHAR NO-UNDO.
DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR lFlagg   AS LOG  NO-UNDO.
DEF VAR cButLst  AS CHAR NO-UNDO.

ASSIGN
    cKatalog = "F:\Home\Pressbyran\Ankommet\05271\"
    lFlagg   = TRUE  /* Sletting.      */
    .

CURRENT-WINDOW:WIDTH = 250.

FOR EACH Filer WHERE
    Filer.FilNavn BEGINS "MD20040101" AND
    ENTRY(2,Filer.FilNavn,".") = "txt" 
    AND
    CAN-DO('05271,08271',ENTRY(5,Filer.Katalog,"\"))
    BY Filer.Katalog
    BY Filer.FilNavn:

    ASSIGN
        iAnt = iant + 1
        cFilNavn = Filer.FilNavn
        .

    FIND FIRST DataSett OF Filer NO-LOCK NO-ERROR.
    PAUSE 0.
    DISPLAY
        SEARCH(cKatalog + Filer.FilNavn) <> ?
        iAnt
        DataSett.DAto WHEN AVAILABLE Datasett           
        Filer.FilNAvn
        ENTRY(2,Filer.FilNavn,".")
        Filer.Filtype
        Filer.Katalog
        Filer.Storrelse
        ENTRY(5,Filer.Katalog,"\") COLUMN-LABEL "Gurre"
        Filer.FilId
        WITH WIDTH 248.

   IF lFlagg THEN DO:
       /* Logger data som er slettet, men hvor det ikke finnes ny fil å lese inn. */
/*        IF SEARCH(cKatalog + Filer.FilNavn) = ? THEN      */
/*        DO:                                               */
/*            OUTPUT TO VALUE("Goran-logg-pbr.txt") APPEND. */
/*            PUT UNFORMATTED                               */
/*                cKatalog + Filer.FilNavn SKIP.            */
/*            OUTPUT CLOSE.                                 */
/*        END.                                              */

       PAUSE 0. DISPLAY "Datasett m.m." FORMAT "x(15)" WITH WIDTH 248. 
       FOR EACH DataSett OF Filer:
           FOR EACH BongHode OF DataSett:
               BongHode.pfFLagg = 1.
           END.
           DataSett.pfFlagg = 1. /* Nytt datasett. */
       END.
   END.
END.

