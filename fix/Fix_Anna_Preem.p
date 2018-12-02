DEFINE VARIABLE cButikkliste AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cEAN AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ii AS INTEGER    NO-UNDO.

cEan = "7313613027023,7313613027122,7313613027221,7313613027320,7313613027429,7313613027528".
FIND ButikkTeam WHERE ButikkTeam.BrGrpNr = 1 and
                      ButikkTeam.TeamTypeId = 2 and
                      ButikkTeam.TeamNr = 3 NO-LOCK.

FOR EACH ButikkKobling OF butikkteam NO-LOCK.
    cButikkListe = cButikkListe + (IF cButikkListe <> "" THEN "," ELSE "") + STRING(ButikkKobling.butik).
END.
OUTPUT TO "c:\tmp\drycker.xls".
PUT UNFORMATTED "Butik"    CHR(9) 
                "Stationsnamn"  CHR(9)
                "Datum"    CHR(9)
                "EAN"      CHR(9)
                "Artikel"  CHR(9)
                "Antal"    CHR(9)
                "Brutto"   CHR(9)
                "Rabatt"   CHR(9)
                "Netto"    CHR(9)
                "Moms"     CHR(9)
                "Varukost" SKIP.
DO ii = 1 TO NUM-ENTRIES(cEAN):
    FOR EACH bonglinje WHERE bonglinje.strekkode = ENTRY(ii,cEAN) NO-LOCK.
        IF NOT CAN-DO(cButikkListe,STRING(bonglinje.butikknr)) THEN
            NEXT.
        FIND butiker WHERE butiker.butik = bonglinje.butikknr NO-LOCK NO-ERROR.
        FIND strekkode WHERE strekkode.kode = bonglinje.strekkode NO-LOCK NO-ERROR.
        RELEASE artbas.
        FIND artbas OF strekkode NO-LOCK NO-ERROR.
        PUT UNFORMATTED butikknr CHR(9) 
                        (IF AVAIL butiker THEN butnamn ELSE "**") CHR(9)
                        YEAR(bonglinje.dato) "-" MONTH(bonglinje.dato) "-" DAY(bonglinje.dato) CHR(9)
                        bonglinje.strekkode CHR(9)
                        (IF AVAIL artbas THEN artbas.beskr ELSE "**") CHR(9)
                        antall   CHR(9)
                        linjesum CHR(9)
                        linjerab CHR(9)
                        linjesum - linjerab CHR(9)
                        mvakr    CHR(9)
                        vvarekost SKIP.
    END.
END.
OUTPUT CLOSE.

