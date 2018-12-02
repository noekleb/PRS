DEF VAR cBrukerLSt AS CHAR NO-UNDO.
DEF VAR iLoop AS INT NO-UNDO.
def var dDato as date no-undo.
def var cFil as char no-undo.

current-window:width = 350.

ASSIGN
    dDato = 01/01/2017
    cBrukerLst = 'rafael,giske'
    .

DEF STREAM Ut.

DO iLoop = 1 TO NUM-ENTRIES(cBrukerLst):
    cFil = 'c:\tmp\tn\translst' + ENTRY(iLoop,cBrukerLst) + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.csv'.
    FOR EACH Butiker NO-LOCK
        break by Butiker.butik:
        
        OUTPUT STREAM Ut TO VALUE(cFil) APPEND.
        IF first(Butiker.Butik) THEN
              PUT STREAM Ut unformatted
                 'Butik;'
                 'Navn;'
                 'TTID;'
                 'Dato;'
                 'ArtikkelNr;'
                 'Beskr;'
                 'LevKod;'
                 'Stor;'
                 'Antall;'
                 'VVarekost;'
                 'Pris;'
                 'brukerid'
             SKIP.

        for each Translogg no-lock where
            TransLogg.butik = Butiker.butik and
            TransLogg.ArtikkelNr >= 0 and
            Translogg.TTId = 5 AND
            TransLogg.Storl >= '' AND
            TransLogg.TTId  = 5 and
            TransLogg.Dato >= dDato and
            TransLogg.Dato <= today and
            TransLogg.RegistrertAv = ENTRY(iLoop,cBrukerLst):

            FIND ArtBas NO-LOCK WHERE
                ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
               
           /*          
           display
           TransLogg.Butik
           TransLogg.TTI
           Translogg.Dato
           ArtBas.ArtikkelNr
           ArtBas.Beskr
           ArtBas.LevKod
           TransLogg.Stor
           Translogg.Antall
           Translogg.VVarekost
           Translogg.Pris
           TransLogg.brukerid
             Translogg.RegistrertAv
          with width 350.
          */
              
          PUT STREAM Ut unformatted
             TransLogg.Butik ';'
             Butiker.ButNamn ';'
             TransLogg.TTID ';'
             Translogg.Dato ';'
             ArtBas.ArtikkelNr ';'
             ArtBas.Beskr ';'
             ArtBas.LevKod ';'
             TransLogg.Stor ';'
             Translogg.Antall ';'
             Translogg.VVarekost ';'
             Translogg.Pris ';'
             TransLogg.RegistrertAv
         SKIP.
        end.   
        OUTPUT STREAM Ut CLOSE.
    END.
END.

