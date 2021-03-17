
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bufArtLAg FOR ArtLAg.
DEF VAR wMva% AS DEC NO-UNDO.
DEF VAR wDefMva% AS DEC NO-UNDO.
def var wWork            as dec  no-undo.
DEF VAR wWork1        AS DEC  NO-UNDO.
def var wWork2           as dec  no-undo.
def var wWork3           as dec  no-undo.
DEF VAR piPris           AS DEC  NO-UNDO.

def buffer bufLager     for Lager.

/* Default MVA% */
{syspara.i 2 1 4 wDefMva% DEC}

FUNCTION RabUMva RETURNS DECIMAL
  ( wMva% as dec ) :
/*------------------------------------------------------------------------------
  Purpose:  Beregner RABATT - MVA(På rabatten). Dvs nettorabatt.
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR wWork as DEC NO-UNDO.

  wWork = (TransLogg.RabKr) -
          ((TransLogg.RabKr) / (1 + (wMva% / 100))).
  if wWork = ? THEN wWork = 0.

  RETURN wWork.
END FUNCTION.

FIXARTLAG:
FOR EACH ArtBas NO-LOCK WHERE
    ArtBAs.ArtikkelNr = 11257 AND
    ArtBas.Lager = TRUE:

    /* Dette må til for at den skal ta artikkelen!! */
    PAUSE 0.
    DISPLAY ArtBas.ArtikkelNr.

    /* Rydder bort gammal dritt */
    FOR EACH Lager WHERE
        Lager.ArtikkelNr = ArtBas.ArtikkelNr:
        DELETE Lager.
    END.
    TRANSLOGGEN:
    FOR EACH TransLogg NO-LOCK WHERE
        TransLogg.ArtikkelNr = ArtBas.ArtikkelNr
        USE-INDEX OppslagDatoTid:

        /* Henter Moms% */
        if AVAILABLE ArtBas then /* Setter moms på transer med kjent artikkelnr. */
          FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
        if not available VarGr then
          FIND VarGr NO-LOCK where  /* Setter moms på transer med ukjent artikkelnr. */
            VarGr.Vg = TransLogg.Vg NO-ERROR.
        if AVAILABLE VarGr then
          DO:
            FIND Moms OF VarGr NO-LOCK NO-ERROR.
            if AVAILABLE Moms then
              wMva% = Moms.MomsProc.
            else
              wMva% = 0.
          END.
        ELSE
          wMva% = 0.
        /* Bruker default MVA hvis den ikke er satt. */
        if NOT AVAILABLE VarGr AND wMva% = 0 then
          wMva% = wDefMva%.

        FIND LAger EXCLUSIVE-LOCK WHERE
            LAger.ArtikkelNr = ArtBas.ArtikkelNR AND
            LAger.Butik = TransLogg.butik NO-ERROR.
        IF NOT AVAILABLE Lager THEN
        DO:
            CREATE LAger.
            ASSIGN
                Lager.ArtikkelNR = ArtBas.ARtikkelNr
                Lager.Butik      = TransLogg.Butik
                .
        END.

        LAGERKORR:
        DO:
            /*if (ArtBas.Lager = TRUE) then*/
            case TransLogg.TTId:
              when 1 then
              DO:
                  assign /* Varesalg */
                    Lager.Lagant     = Lager.Lagant     - TransLogg.Antall
                    Lager.AntSolgt   = Lager.AntSolgt   + TransLogg.Antall
                    Lager.VerdiSolgt = Lager.VerdiSolgt +
                                       (
                                        (TransLogg.Pris - Translogg.RabKr) -
                                        TransLogg.Mva
                                       ) * TransLogg.Antall
                    Lager.AntRab     = Lager.AntRab +
                                       (if TransLogg.RabKr <> 0
                                          then TransLogg.Antall
                                          else 0)
                    Lager.VerdiRabatt = Lager.VerdiRabatt +
                                       (if TransLogg.RabKr <> 0
                                          then TransLogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                                          else 0).
              END.
              when 2 then
                  assign  /* Brekkasje */
                    Lager.Lagant     = Lager.Lagant     - TransLogg.Antall
                    Lager.BrekkAnt   = Lager.BrekkAnt   + TransLogg.Antall
                    Lager.BrekkVerdi = Lager.BrekkVerdi +
                                       (TransLogg.VVareKost * TransLogg.Antall).
              when 3 then
                do:
                  assign  /* Kundereklamasjon */
                    Lager.ReklAnt    = Lager.ReklAnt    + TransLogg.Antall
                    Lager.ReklVerdi  = Lager.ReklVerdi  +
                                       (TransLogg.VVareKost * TransLogg.Antall)
                    /* Korrigerer rabatt */
                    Lager.AntRab     = Lager.AntRab +
                                       (if TransLogg.RabKr <> 0
                                          then TransLogg.Antall
                                          else 0)
                    Lager.VerdiRabatt = Lager.VerdiRabatt +
                                       (if TransLogg.RabKr <> 0
                                          then TransLogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                                          else 0)
                    /* Korrigerer salget ved retur - NB: Antallet er negativt i transaksjonen. */
                    Lager.AntSolgt   = Lager.AntSolgt   + TransLogg.Antall
                    Lager.VerdiSolgt = Lager.VerdiSolgt +
                                       (
                                        (TransLogg.Pris - Translogg.RabKr) -
                                        TransLogg.Mva
                                       ) * TransLogg.Antall.
                  IF "" = "St" THEN
                  DO:
                      assign
                        piPris = (TransLogg.Pris - Translogg.RabKr) - TransLogg.Mva
                        wWork1 = ABSOLUTE(Lager.AntSolgt * Lager.NedVerdi) /* Gammel solgtverdi */
                        wWork2 = ABSOLUTE(TransLogg.Antall * piPris) /* Verdi av tilf›rt lager */
                        wWork3 = (wWork1 + wWork2) / (ABSOLUTE(Lager.AntSolgt) + ABSOLUTE(TransLogg.Antall))
                        wWork3 = if wWork3 = ? then MAXIMUM(Lager.NedVerdi,piPris) else wWork3
                        Lager.NedVerdi = wWork3 /* Setter ny vektet snittpris */
                        .                                                                                            
                  END.
                end.
              when 4 then
                  assign  /* Lagerreklamasjon */
                    Lager.Lagant     = Lager.Lagant     - TransLogg.Antall
                    Lager.ReklLAnt   = Lager.ReklLAnt   + TransLogg.Antall
                    Lager.ReklLVerdi = Lager.ReklLVerdi +
                                       (TransLogg.VVareKost * TransLogg.Antall).
              when 5 then
                do: /* Varekjøp m/vektet vareverdi */
                    /* TN 5/4-00                                                    */
                    /* Ved slasking av varer blir verdien av "innleveransen" lik    */
                    /* verdien av hva som ligger på lager fra før i "Fra" butikken. */
                    /* Derfor må wWork3 settes lik Lager.VVarekost hvis den nye     */
                    /* VVareKost beregnes lik ?.                                    */
                  assign
                    wWork  = ABSOLUTE(Lager.Lagant   * Lager.VVareKost)  /* Gammel lagerverdi */
                    wWork2 = ABSOLUTE(TransLogg.Pris * TransLogg.Antall) /* Verdi av innkjøp  */
                    wWork3 = (wWork + wWork2) / (ABSOLUTE(Lager.LagAnt) + ABSOLUTE(TransLogg.Antall))
                    wWork3 = if wWork3 = ? then Lager.VVareKost else wWork3.

                  assign
                    Lager.Lagant    = Lager.Lagant     + TransLogg.Antall
                    Lager.VVareKost = wWork3 /* Setter ny vektet snittpris */
                    Lager.KjopAnt   = Lager.KjopAnt    + TransLogg.Antall
                    Lager.KjopVerdi = Lager.KjopVerdi  + wWork2.
                end.
              when 6 then
                do: /* Overføring */
                  /* Henter LAger eller StLAger for mottagende butikk. */
                  find bufLager exclusive-lock where
                     bufLager.ArtikkelNr = TransLogg.ArtikkelNr and
                     bufLager.Butik      = TransLogg.OvButik no-error.
                   if not available bufLager then
                   do:
                     create bufLager.
                     assign
                       bufLager.ArtikkelNr = TransLogg.ArtikkelNr
                       bufLager.Butik      = TransLogg.OvButik.
                   end.

                  assign  /* Trekker ned lager på fra butikk.            */
                          /* Alle posteringer skjer med vektet varekost. */
                    Lager.Lagant  = Lager.Lagant     - TransLogg.Antall
                    Lager.OvAnt   = Lager.OvAnt      - TransLogg.Antall
                    Lager.OvVerdi = Lager.OvVerdi    -
                                       (TransLogg.VVareKost * TransLogg.Antall).

                  /* Innleveranse medfører ny vekting av varekost i mottagende butikk */
                  assign
                    wWork  = ABSOLUTE(bufLager.Lagant * bufLager.VVareKost)   /* Gammel lagerverdi */
                    wWork2 = ABSOLUTE(TransLogg.VVareKost * TransLogg.Antall) /* Verdi av overføring  */
                    wWork3 = (wWork + wWork2) / (ABSOLUTE(bufLager.LagAnt) + ABSOLUTE(TransLogg.Antall))
                    wWork3 = if wWork3 = ? then bufLager.VVareKost else wWork3.

                  assign  /* Posterer i mottagende butikk.  */
                          /* Alle posteringer skjer med vektet varekost. */
                    bufLager.Lagant    = bufLager.Lagant     + TransLogg.Antall
                    bufLager.VVareKost = wWork3 /* Setter ny vektet snittpris */
                    bufLager.OvAnt     = bufLager.OvAnt      + TransLogg.Antall
                    bufLager.OvVerdi   = bufLager.OvVerdi    +
                                          (TransLogg.VVareKost * TransLogg.Antall).
                end.
              when 7 then
                  assign  /* Lagerjustering */
                    Lager.Lagant    = Lager.Lagant    - TransLogg.Antall
                    Lager.JustAnt   = Lager.JustAnt   + TransLogg.Antall
                    Lager.JustVerdi = Lager.JustVerdi +
                                       (TransLogg.VVareKost * TransLogg.Antall).
              when 8 then
                do:
                  assign  /* Nedskrivning */
                          /* Ingen endring i lagerantall. Kun VVarekost. */
                    Lager.NedVerdi  = Lager.NedVerdi  +
                                      (TransLogg.Pris * TransLogg.Antall)
                    Lager.VVareKost = Lager.VVareKost - (TransLogg.Pris * if TransLogg.Antall < 0
                                                                            then -1
                                                                            else 1)
                    Lager.NedAnt    = Lager.NedAnt    + TransLogg.Antall.
                end.
              when 9 then
                  assign  /* Svinn */
                    Lager.LagAnt     = Lager.LagAnt    - TransLogg.Antall
                    Lager.SvinnAnt   = Lager.SvinnAnt  + TransLogg.Antall
                    Lager.SvinnVerdi = Lager.SvinnVerdi  +
                                       (TransLogg.Pris * TransLogg.Antall).
              when 10 then
              DO:
                  assign  /* Gjennkjøp */
                    Lager.LagAnt        = Lager.LagAnt         + (TransLogg.Antall * -1) /* Negativt antall i TransLogg */
                    Lager.GjenkjopAnt   = Lager.GjenkjopAnt    + TransLogg.Antall
                    Lager.GjenkjopVerdi = Lager.GjenkjopVerdi  +
                                       (
                                        (TransLogg.Pris - Translogg.RabKr) -
                                        TransLogg.Mva
                                       ) * TransLogg.Antall
                    Lager.AntRab     = Lager.AntRab +
                                       (if TransLogg.RabKr <> 0
                                          then TransLogg.Antall
                                          else 0)
                    Lager.VerdiRabatt = Lager.VerdiRabatt +
                                       (if TransLogg.RabKr <> 0
                                          then TransLogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                                          else 0)
                    /* Korrigerer salget ved retur - NB: Antallet er negativt i transaksjonen. */
                    Lager.AntSolgt   = Lager.AntSolgt   + TransLogg.Antall
                    Lager.VerdiSolgt = Lager.VerdiSolgt +
                                       (
                                        (TransLogg.Pris - Translogg.RabKr) -
                                        TransLogg.Mva
                                       ) * TransLogg.Antall.
              END.
              when 11 then
                  assign  /* Internt forbruk */
                    Lager.LagAnt   = Lager.LagAnt    - TransLogg.Antall
                    Lager.IntAnt   = Lager.IntAnt    + TransLogg.Antall
                    Lager.IntVerdi = Lager.IntVerdi  +
                                       (TransLogg.VVareKost * TransLogg.Antall).
            END CASE.
        END. /* LAGERKORR */
    END. /* TRANSLOGGEN */

END. /* FIXARTLAG */




