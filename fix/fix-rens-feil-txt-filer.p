DEF VAR iAnt     AS INT  NO-UNDO.
DEF VAR cKatalog AS CHAR NO-UNDO.
DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR lSlett   AS LOG  NO-UNDO.
DEF VAR cButLst  AS CHAR NO-UNDO.

ASSIGN
    lSlett   = TRUE  /* Sletting.      */
    .

CURRENT-WINDOW:WIDTH = 250.

FOR EACH Filer WHERE
    Filer.FilNavn BEGINS "MDExport_2004-11-11" AND
    ENTRY(2,Filer.FilNavn,".") = "r1" 
    AND
    CAN-DO('75691',ENTRY(5,Filer.Katalog,"\"))
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

   IF lSlett THEN DO:
       /* Logger data som er slettet, men hvor det ikke finnes ny fil å lese inn. */
/*        IF SEARCH(cKatalog + Filer.FilNavn) = ? THEN      */
/*        DO:                                               */
/*            OUTPUT TO VALUE("Goran-logg-pbr.txt") APPEND. */
/*            PUT UNFORMATTED                               */
/*                cKatalog + Filer.FilNavn SKIP.            */
/*            OUTPUT CLOSE.                                 */
/*        END.                                              */

       PAUSE 0. DISPLAY "FilLinjer" FORMAT "x(8)" WITH WIDTH 248. 
       FOR EACH FilLinjer OF Filer:
           DELETE FilLinjer.
       END.
       PAUSE 0. DISPLAY "FilLogg" FORMAT "x(8)" WITH WIDTH 248. 
       FOR EACH FilLogg OF Filer:
           DELETE FilLogg.
       END.
       PAUSE 0. DISPLAY "Datasett m.m." FORMAT "x(15)" WITH WIDTH 248. 
       FOR EACH DataSett OF Filer:
           IF NOT CAN-DO(cButLst,STRING(DataSett.ButikkNr)) THEN
           DO:
               OUTPUT TO VALUE("ButLst.Txt") APPEND.
               PUT UNFORMATTED DataSett.ButikkNr "  " DataSEtt.Dato SKIP.
               OUTPUT CLOSE.
               ASSIGN
                   cButLst = cButLst + "," + STRING(DataSett.ButikkNr).
           END.
           FOR EACH BongHode OF DataSett:
               FOR EACH BongLinje WHERE
                   BongLinje.B_Id = BongHode.B_Id:
                   DELETE BongLinje.
               END.
               DELETE BongHode.
           END.
           DELETE Datasett.
       END.
       DELETE Filer.
   END.
END.

