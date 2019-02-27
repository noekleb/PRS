
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER wArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.

DEF BUFFER bufArtLAg FOR ArtLAg.
DEF VAR wMva% AS DEC NO-UNDO.
DEF VAR wDefMva% AS DEC NO-UNDO.
def var wWork            as dec  no-undo.
DEF VAR wWork1        AS DEC  NO-UNDO.
def var wWork2           as dec  no-undo.
def var wWork3           as dec  no-undo.
DEF VAR piPris           AS DEC  NO-UNDO.

def buffer bufLager     for Lager.
DEFINE BUFFER bufArtBas FOR ArtBas.

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
    ArtBAs.ArtikkelNr = wArtikkelNr AND
    ArtBas.OPris      = FALSE AND
    ArtBas.Lager      = TRUE:

    /* Rydder bort gammel morro. */
    FOR EACH Lager WHERE
        Lager.ArtikkelNr = ArtBas.ArtikkelNr:
        DELETE Lager.
    END.
    FOR EACH ArtLag WHERE
        ArtLag.ArtikkelNr = ArtBAs.ArtikkelNr:
        DELETE ArtLAg.
    END.

    /* Sjekker at VG LopNr er satt riktig. */
    FOR EACH TransLogg EXCLUSIVE-LOCK WHERE 
        TransLogg.ArtikkelNr = ArtBas.ArtikkelNr:
        ASSIGN
            TransLogg.Vg    = ArtBas.Vg 
            TransLogg.LopNr = ArtBas.LopNr.
    END.

    /* Sjekker at det ikke ligger gamle ArtLag poster som kommer i konflikt. */
    /* Det kan skje hvis de har feil Vg/LopNr satt.                          */
    /* Da setter vi riktig vg/lopnr på dem, eller setter lopnr = ?.          */
    IF ArtBas.LopNr > 0 AND ArtBas.LopNr <> ? THEN 
        FOR EACH ArtLag EXCLUSIVE-LOCK WHERE 
            ArtLag.Vg    = ArtBas.Vg AND 
            ArtLag.lopnr = ArtBas.LopNr:
            IF ArtLag.ArtikkelNr <> ArtBas.ArtikkelNr THEN 
            DO:
                FIND bufArtBas NO-LOCK WHERE 
                    bufArtBas.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
                IF NOT AVAILABLE ArtBas THEN 
                    DELETE ArtLag.
                ELSE ASSIGN 
                        ArtLag.vg    = bufArtBas.Vg 
                        ArtLag.lopnr = bufArtBas.LopNr NO-ERROR.
            END.
        END.

    TRANSLOGGEN:
    FOR EACH Translogg Exclusive-lock WHERE
        TransLogg.ArtikkelNr = ArtBas.ArtikkelNr and
        TransLogg.Postert = true
        USE-INDEX OppslagDatoTid:

        RUN PosterArtLag.
        RUN PosterLager.

    END. /* TRANSLOGGEN */
END. /* FIXARTLAG */

PROCEDURE PosterArtLag:
    DEF VAR piStrKode AS INT NO-UNDO.

    /* Tar bort paranteser hvis de finnes. */
    ASSIGN
        Translogg.Storl = TRIM(Translogg.Storl,"()")
        Translogg.TilStorl = TRIM(Translogg.TilStorl,"()")
        .

    /* Kun transaksjoner med gyldig størrelse skal posteres. */
    FIND StrKonv NO-LOCK WHERE
      StrKonv.Storl = TransLogg.Storl NO-ERROR.

    /* Ligger det transaksjoner med størrelser som ikke finnes i StrKonv, da */
    /* Oppretter vi dem i STrKonv.                                           */
    IF NOT AVAILABLE StrKonv THEN
    DO:
        FIND LAST StrKonv NO-LOCK USE-INDEX StrKode.
        IF AVAILABLE StrKonv THEN
            piStrKode = StrKonv.StrKode + 1.
        ELSE
            piStrKode = 1.
        CREATE StrKonv.
        ASSIGN
            StrKonv.Storl   = Translogg.Storl
            StrKonv.StrKode = piStrKode
            StrKonv.Merknad = "Opprettet fra fix-lager-og-artlag.p"
            .
    END.

    /* Hvis ikke ArtLag posten finnes opprettes den. Men transaksjonen */
    /* flagges med en kode for at lagerpost ble opprettet.             */
    IF AVAILABLE StrKonv THEN find ArtLag exclusive-lock where
        Artlag.ArtikkelNr = dec(Translogg.ArtikkelNr) AND
        ArtLag.Butik      = TransLogg.Butik and
        ArtLag.StrKode    = StrKonv.StrKode NO-ERROR.
    if not available ArtLag then
      do:
        create ArtLag.
        assign
          ArtLag.Butik      = TransLogg.Butik
          Artlag.ArtikkelNr = TransLogg.ArtikkelNr
          ArtLag.StrKode    = IF AVAILABLE StrKonv 
                                THEN StrKonv.StrKode
                                ELSE 0
          ArtLag.Storl      = TransLogg.Storl
          ArtLag.Vg         = ArtBas.Vg    /* TransLogg.Vg  */
          ArtLag.LopNr      = ArtBas.LopNr /* TransLogg.LopNr */
          NO-ERROR.     
        /* Blir det alikevel feil, skal ikke transen oppdateres men feilmerkes. */
        IF ERROR-STATUS:ERROR THEN
        DO:
            IF AVAILABLE ArtLag AND ArtLag.Butik = 0 THEN
                DELETE Artlag.
            return "UNDO".
        END.
      end.

    case TransLogg.TTId:
      when 1 then /* Varesalg */
      DO:
          assign
            ArtLag.LagAnt   = ArtLag.LagAnt     - TransLogg.Antall
            ArtLag.AntSolgt = ArtLag.AntSolgt + TransLogg.Antall
            ArtLag.AntRab   = ArtLAg.AntRab +
                              (if TransLogg.RabKr <> 0
                                 then TransLogg.Antall 
                                 else 0).
      END.
      when 2 then /* Brekkasje */
        assign
          ArtLag.LagAnt   = ArtLag.LagAnt   - TransLogg.Antall
          ArtLag.BrekkAnt = ArtLag.BrekkAnt + TransLogg.Antall.
      when 3 then /* Kundereklamasjon */
        assign
          /* TN 14/5-02 ArtLag.LagAnt  = ArtLag.LagAnt  + TransLogg.Antall */
          ArtLag.ReklAnt = ArtLag.ReklAnt + TransLogg.Antall
          /* Korrigerer salget ved reklamasjon */
          ArtLag.AntSolgt = ArtLag.AntSolgt + TransLogg.Antall.
      when 4 then /* Lagerreklamasjon */
        assign
          ArtLag.LagAnt   = ArtLag.LagAnt   - TransLogg.Antall
          ArtLag.ReklLAnt = ArtLag.ReklLAnt + TransLogg.Antall.
      when 5 then /* Varekjøp */
        assign
          ArtLag.LagAnt  = ArtLag.LagAnt  + TransLogg.Antall
          ArtLag.KjopAnt = ArtLag.KjopAnt + TransLogg.Antall.          
      when 6 then /* Overføring */
        do:
          assign  /*Inn i butikk. NB: Translogg.Antall er negativ postert i Translogg. */
            ArtLag.LagAnt  = ArtLag.LagAnt - TransLogg.Antall
            ArtLag.OvAnt   = ArtLag.OvAnt  - TransLogg.Antall.

          /* Justerer fra butikken. */  
          /* NB: i w-gridlager.w lagres fra størrelsen i TransLogg.TilStorl. */
          find bufArtLag exclusive-lock where
            bufArtLag.Butik      = TransLogg.OvButik and
            bufArtlag.ArtikkelNr = Translogg.ArtikkelNr AND
            bufArtLag.Storl      = TransLogg.TilStorl no-error no-wait.
          if locked bufArtLag then
            do:
              return "UNDO".
            end.
          if not available bufArtLag then
            do:
              FIND StrKonv NO-LOCK WHERE
                StrKonv.Storl = TransLogg.TilStorl NO-ERROR.
              create bufArtLag.
              assign
                bufArtLag.Butik      = TransLogg.OvButik
                bufArtLag.Vg         = ArtBas.Vg    /* TransLogg.Vg     */
                bufArtLag.LopNr      = ArtBas.LopNr /* TransLogg.LopNr  */
                bufArtLag.Storl      = TransLogg.TilStorl
                bufArtLag.ArtikkelNr = TransLogg.ArtikkelNr
                bufArtLag.StrKode    = (IF AVAILABLE StrKonv
                                         THEN StrKonv.StrKode
                                         ELSE bufArtLag.StrKode)
                .              
            end.

          assign  /*Trekke ned i fra butikken. Husk at TransLogg.Antall er negativ. */
            bufArtLag.LagAnt  = bufArtLag.LagAnt + TransLogg.Antall
            bufArtLag.OvAnt   = bufArtLag.OvAnt  + TransLogg.Antall.
        end.
      when 7 then /* Lagerjustering */
        assign
          ArtLag.LagAnt  = ArtLag.LagAnt  - TransLogg.Antall
          ArtLag.JustAnt = ArtLag.JustAnt + TransLogg.Antall.
      when 8 then. /* Nedskrivning - Påvirker ikke ArtLag. */
      when 9 then /* Svinn */        
        assign
          ArtLag.LagAnt   = ArtLag.LagAnt   - TransLogg.Antall
          ArtLag.SvinnAnt = ArtLag.SvinnAnt + TransLogg.Antall.
      when 10 then /* Gjennkjøp */        
        assign
          ArtLag.LagAnt      = ArtLag.LagAnt      + (TransLogg.Antall * -1) /* Negativt antall i TransLogg */
          ArtLag.GjenkjopAnt = ArtLag.GjenkjopAnt + TransLogg.Antall
          /* Korrigerer rabatter */
          ArtLag.AntRab   = ArtLAg.AntRab +
                            (if TransLogg.RabKr <> 0
                               then TransLogg.Antall 
                               else 0)
          /* Korrigerer salget ved gjenkjøp */
          ArtLag.AntSolgt = ArtLag.AntSolgt + TransLogg.Antall.
      when 11 then /* Internt forbruk */        
        assign
          ArtLag.LagAnt = ArtLag.LagAnt - TransLogg.Antall
          ArtLag.IntAnt = ArtLag.IntAnt + TransLogg.Antall.        
    end case.
END PROCEDURE. /* PosterArtlag */

PROCEDURE PosterLager:
    FIND Lager EXCLUSIVE-LOCK WHERE
        Lager.Butik = TransLogg.Butik AND
        Lager.ArtikkelNr = ArtBas.ArtikkelNR NO-ERROR.
    IF NOT AVAILABLE Lager THEN
    DO:
        CREATE Lager.
        ASSIGN
            Lager.Butik = Translogg.Butik
            Lager.ArtikkelNr = ArtBas.ArtikkelNr
            .
    END.
    case TransLogg.TTId:
      when 1 then
      DO:
          assign /* Varesalg */
            Lager.Lagant     = Lager.Lagant     - TransLogg.Antall
            Lager.AntSolgt   = Lager.AntSolgt   + TransLogg.Antall
            Lager.SVK        = Lager.SVK        + (TransLogg.Antall * Translogg.VVareKost)
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
                                  else 0) NO-ERROR.
          /* Flagger at transen trekker lager negativ på butikk. */
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
            Lager.SVK        = Lager.SVK        + (TransLogg.Antall * Translogg.VVareKost)
            Lager.VerdiSolgt = Lager.VerdiSolgt +
                               (
                                (TransLogg.Pris - Translogg.RabKr) -
                                TransLogg.Mva
                               ) * TransLogg.Antall.
        end.
      when 4 then
          assign  /* Lagerreklamasjon */
            Lager.Lagant     = Lager.Lagant     - TransLogg.Antall
            Lager.ReklLAnt   = Lager.ReklLAnt   + TransLogg.Antall
            Lager.ReklLVerdi = Lager.ReklLVerdi +
                               (TransLogg.VVareKost * TransLogg.Antall).
      when 5 then
        do: /* Varekjøp m/vektet vareverdi. */
          /* Vekting av varekost skal bare gjøres når det finnes noe på lager fra før. */
          IF Lager.Lagant > 0 AND TransLogg.Antall > 0 THEN
              assign
                wWork  = (Lager.Lagant   * Lager.VVareKost)  /* Gammel lagerverdi */
                wWork2 = (TransLogg.Pris * TransLogg.Antall) /* Verdi av innkjøp  */
                wWork3 = (wWork + wWork2) / ((Lager.LagAnt) + (TransLogg.Antall))
                wWork3 = if wWork3 = ? then Lager.VVareKost else wWork3.
          ELSE DO:
              IF Lager.Lagant <= 0 AND TransLogg.Antall > 0 THEN
                  wWork3 = TransLogg.VVareKost.
              ELSE
                  wWork3 = Lager.VVareKost.
          END.

          assign
            Lager.VVareKost = wWork3 /* Setter ny vektet snittpris */
            Lager.Lagant    = Lager.Lagant     + TransLogg.Antall
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
          IF bufLager.Lagant > 0 AND TransLogg.Antal > 0 THEN
              assign
                wWork  = (bufLager.Lagant * bufLager.VVareKost)   /* Gammel lagerverdi */
                wWork2 = (TransLogg.VVareKost * TransLogg.Antall) /* Verdi av overføring  */
                wWork3 = (wWork + wWork2) / ((bufLager.LagAnt) + (TransLogg.Antall))
                wWork3 = if wWork3 = ? then bufLager.VVareKost else wWork3.
          ELSE DO:
              IF bufLager.Lagant <= 0 AND TransLogg.Antall > 0 THEN
                  wWork3 = TransLogg.VVareKost.
              ELSE
                  wWork3 = bufLager.VVareKost.
          END.

          assign  /* Posterer i mottagende butikk.  */
                  /* Alle posteringer skjer med vektet varekost. */
            bufLager.VVareKost = wWork3 /* Setter ny vektet snittpris */
            bufLager.Lagant    = bufLager.Lagant     + TransLogg.Antall
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
            Lager.VVareKost = IF Lager.VVareKost <= 0 THEN ABS(TransLogg.Pris)
                                                  ELSE Lager.VVareKost - TransLogg.Pris
            Lager.NedVerdi  = Lager.NedVerdi  +
                              (TransLogg.Pris * TransLogg.Antall)
            Lager.NedAnt    = Lager.NedAnt    + TransLogg.Antall.
        end.
      when 9 then
      DO:
          assign  /* Svinn */
            Lager.LagAnt     = Lager.LagAnt    - TransLogg.Antall
            Lager.SvinnAnt   = Lager.SvinnAnt  + TransLogg.Antall
            Lager.SvinnVerdi = Lager.SvinnVerdi  +
                               (TransLogg.Pris * TransLogg.Antall).
      END.
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
             Lager.SVK       = Lager.SVK        + (TransLogg.Antall * Translogg.VVareKost)
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
    end case.

END PROCEDURE.




