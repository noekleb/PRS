DEF VAR wCl AS INT NO-UNDO.

ASSIGN
    wCl = 20.
{syspara.i 5 1 1 wCl INT}

def var ii as inte no-undo.

FIXLOOP:
FOR EACH BestHode EXCLUSIVE-LOCK WHERE
    /*
    BestHode.BestNr = 1269 AND 
    */
    BestHode.Direktelev = FALSE:
       
    FIND Butiker WHERE Butiker.Butik = wCl NO-LOCK.
    FIND BestPris WHERE BestPris.BestNr = BestHode.BestNr AND
                  BestPris.BestStat = BestHode.BestStat AND
                  BestPris.ProfilNr = Butiker.ProfilNr NO-LOCK.

    ASSIGN BestHode.TotAntPar = 0.
    FOR EACH BestStr NO-LOCK WHERE
      bestStr.BestNr = BestHode.BestNr AND
      BestStr.BestStat = BestHode.BestStat:
      BestHode.TotAntPar = BestHode.TotAntPar + BestStr.Bestilt.
    END.
    ASSIGN BestHode.TotInnKjVerdi = BestHode.TotAntPar * BestPris.Varekost
           BestHode.TotDbKr =       BestHode.TotAntPar * BestPris.DbKr
           BestHode.TotSalgsVerdi = BestHode.TotAntPar * BestPris.Pris.
    /*
    MESSAGE 
           BestHode.TotInnKjVerdi  BestHode.TotAntPar SKIP
           BestHode.TotDbKr        BestHode.TotAntPar SKIP
           BestHode.TotSalgsVerdi  BestHode.TotAntPar SKIP 
        VIEW-AS ALERT-BOX.
    */

    find bestsort of besthode where fri NO-ERROR.
    IF NOT AVAILABLE BestSort THEN
        NEXT FIXLOOP.
    IF BestSort.Fordeling = "" THEN
        NEXT FIXLOOP.

    /* Tar bort gammel dritt */
    FOR EACH fributik OF BestHode:
      DELETE fributik.
    END.

    /* Legger opp ny fributik */
    /* Når Direktelev = false skal det kun ligge en fributikk på sentrallager. */
    create fributik.
    assign fributik.butik = wCl
           fributik.bestnr = BestHode.BestNr.
    do ii = 1 to num-entries(bestsort.fordeling," "):
        assign fributik.friantal[ii] = int(entry(ii,bestsort.fordeling," "))
               fributik.totantal = fributik.totantal + fributik.friantal[ii].
    end.

    /* Denne skal være blank */
    ASSIGN
        BestSort.Fordeling = "".

END. /* FIXLOOP */
