
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER wArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.

DEF BUFFER bufArtLAg FOR ArtLAg.
DEF VAR wMva% AS DEC NO-UNDO.
DEF VAR wDefMva% AS DEC NO-UNDO.
DEF VAR wWork            AS DEC  NO-UNDO.
DEF VAR wWork1        AS DEC  NO-UNDO.
DEF VAR wWork2           AS DEC  NO-UNDO.
DEF VAR wWork3           AS DEC  NO-UNDO.
DEF VAR piPris           AS DEC  NO-UNDO.

DEF BUFFER bufLager     FOR Lager.
DEFINE BUFFER bufArtBas FOR ArtBas.

/* Default MVA% */
{syspara.i 2 1 4 wDefMva% DEC}

FUNCTION RabUMva RETURNS DECIMAL
  ( wMva% AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  Beregner RABATT - MVA(P� rabatten). Dvs nettorabatt.
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR wWork AS DEC NO-UNDO.

  wWork = (TransLogg.RabKr) -
          ((TransLogg.RabKr) / (1 + (wMva% / 100))).
  IF wWork = ? THEN wWork = 0.

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
    /* Da setter vi riktig vg/lopnr p� dem, eller setter lopnr = ?.          */
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
    FOR EACH Translogg EXCLUSIVE-LOCK WHERE
        TransLogg.ArtikkelNr = ArtBas.ArtikkelNr AND
        TransLogg.Postert = TRUE
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

    /* Kun transaksjoner med gyldig st�rrelse skal posteres. */
    FIND StrKonv NO-LOCK WHERE
      StrKonv.Storl = TransLogg.Storl NO-ERROR.

    /* Ligger det transaksjoner med st�rrelser som ikke finnes i StrKonv, da */
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
    IF AVAILABLE StrKonv THEN 
      FIND FIRST ArtLag EXCLUSIVE-LOCK WHERE
        Artlag.ArtikkelNr = dec(Translogg.ArtikkelNr) AND
        ArtLag.Butik      = TransLogg.Butik AND
        ArtLag.StrKode    = StrKonv.StrKode NO-ERROR.
    IF NOT AVAILABLE ArtLag THEN
      DO:
        CREATE ArtLag.
        ASSIGN
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
            RETURN "UNDO".
        END.
      END.

    CASE TransLogg.TTId:
      WHEN 1 THEN /* Varesalg */
      DO:
          ASSIGN
            ArtLag.LagAnt   = ArtLag.LagAnt     - TransLogg.Antall
            ArtLag.AntSolgt = ArtLag.AntSolgt + TransLogg.Antall
            ArtLag.AntRab   = ArtLAg.AntRab +
                              (IF TransLogg.RabKr <> 0
                                 THEN TransLogg.Antall 
                                 ELSE 0).
      END.
      WHEN 2 THEN /* Brekkasje */
        ASSIGN
          ArtLag.LagAnt   = ArtLag.LagAnt   - TransLogg.Antall
          ArtLag.BrekkAnt = ArtLag.BrekkAnt + TransLogg.Antall.
      WHEN 3 THEN /* Kundereklamasjon */
        ASSIGN
          /* TN 14/5-02 ArtLag.LagAnt  = ArtLag.LagAnt  + TransLogg.Antall */
          ArtLag.ReklAnt = ArtLag.ReklAnt + TransLogg.Antall
          /* Korrigerer salget ved reklamasjon */
          ArtLag.AntSolgt = ArtLag.AntSolgt + TransLogg.Antall.
      WHEN 4 THEN /* Lagerreklamasjon */
        ASSIGN
          ArtLag.LagAnt   = ArtLag.LagAnt   - TransLogg.Antall
          ArtLag.ReklLAnt = ArtLag.ReklLAnt + TransLogg.Antall.
      WHEN 5 THEN /* Varekj�p */
        ASSIGN
          ArtLag.LagAnt  = ArtLag.LagAnt  + TransLogg.Antall
          ArtLag.KjopAnt = ArtLag.KjopAnt + TransLogg.Antall.          
      WHEN 6 THEN /* Overf�ring */
        DO:          
          ASSIGN  /* I 'fra' butikken reduseres lageret. */
            ArtLag.LagAnt  = ArtLag.LagAnt - TransLogg.Antall
            ArtLag.OvAnt   = ArtLag.OvAnt  - TransLogg.Antall.

          /* Justerer fra butikken. */  
          /* NB: i w-gridlager.w lagres fra st�rrelsen i TransLogg.TilStorl. */
          FIND bufArtLag EXCLUSIVE-LOCK WHERE
            bufArtLag.Butik      = TransLogg.OvButik AND
            bufArtlag.ArtikkelNr = Translogg.ArtikkelNr AND
            bufArtLag.Storl      = TransLogg.TilStorl NO-ERROR NO-WAIT.
          IF LOCKED bufArtLag THEN
            DO:
              RETURN "UNDO".
            END.
          IF NOT AVAILABLE bufArtLag THEN
            DO:
              FIND StrKonv NO-LOCK WHERE
                StrKonv.Storl = TransLogg.TilStorl NO-ERROR.
              CREATE bufArtLag.
              ASSIGN
                bufArtLag.Butik      = TransLogg.OvButik
                bufArtLag.Vg         = ArtBas.Vg    /* TransLogg.Vg     */
                bufArtLag.LopNr      = ArtBas.LopNr /* TransLogg.LopNr  */
                bufArtLag.Storl      = TransLogg.TilStorl
                bufArtLag.ArtikkelNr = TransLogg.ArtikkelNr
                bufArtLag.StrKode    = (IF AVAILABLE StrKonv
                                         THEN StrKonv.StrKode
                                         ELSE bufArtLag.StrKode)
                .              
            END.

          /* TN 1/5-20 Mottagende butikk skal ikke oppdateres ved TBId = 2. Da er */
          /* mottagende butikk oppdatert via et varemottak (Via pakkseddel).      */
          IF Translogg.TBId < 2 THEN 
          DO:
            ASSIGN  /* I mottagende butikk �kes lageret. */
              bufArtLag.LagAnt  = bufArtLag.LagAnt + TransLogg.Antall
              bufArtLag.OvAnt   = bufArtLag.OvAnt  + TransLogg.Antall.
          END.
        END.
      WHEN 7 THEN /* Lagerjustering */
        ASSIGN
          ArtLag.LagAnt  = ArtLag.LagAnt  - TransLogg.Antall
          ArtLag.JustAnt = ArtLag.JustAnt + TransLogg.Antall.
      WHEN 8 THEN. /* Nedskrivning - P�virker ikke ArtLag. */
      WHEN 9 THEN /* Svinn */        
        ASSIGN
          ArtLag.LagAnt   = ArtLag.LagAnt   - TransLogg.Antall
          ArtLag.SvinnAnt = ArtLag.SvinnAnt + TransLogg.Antall.
      WHEN 10 THEN /* Gjennkj�p */        
        ASSIGN
          ArtLag.LagAnt      = ArtLag.LagAnt      + (TransLogg.Antall * -1) /* Negativt antall i TransLogg */
          ArtLag.GjenkjopAnt = ArtLag.GjenkjopAnt + TransLogg.Antall
          /* Korrigerer rabatter */
          ArtLag.AntRab   = ArtLAg.AntRab +
                            (IF TransLogg.RabKr <> 0
                               THEN TransLogg.Antall 
                               ELSE 0)
          /* Korrigerer salget ved gjenkj�p */
          ArtLag.AntSolgt = ArtLag.AntSolgt + TransLogg.Antall.
      WHEN 11 THEN /* Internt forbruk */        
        ASSIGN
          ArtLag.LagAnt = ArtLag.LagAnt - TransLogg.Antall
          ArtLag.IntAnt = ArtLag.IntAnt + TransLogg.Antall.        
    END CASE.
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
    CASE TransLogg.TTId:
      WHEN 1 THEN
      DO:
          ASSIGN /* Varesalg */
            Lager.Lagant     = Lager.Lagant     - TransLogg.Antall
            Lager.AntSolgt   = Lager.AntSolgt   + TransLogg.Antall
            Lager.SVK        = Lager.SVK        + (TransLogg.Antall * Translogg.VVareKost)
            Lager.VerdiSolgt = Lager.VerdiSolgt +
                               (
                                (TransLogg.Pris - Translogg.RabKr) -
                                TransLogg.Mva
                               ) * TransLogg.Antall
            Lager.AntRab     = Lager.AntRab +
                               (IF TransLogg.RabKr <> 0
                                  THEN TransLogg.Antall
                                  ELSE 0)
            Lager.VerdiRabatt = Lager.VerdiRabatt +
                               (IF TransLogg.RabKr <> 0
                                  THEN TransLogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                                  ELSE 0) NO-ERROR.
          /* Flagger at transen trekker lager negativ p� butikk. */
      END.
      WHEN 2 THEN
          ASSIGN  /* Brekkasje */
            Lager.Lagant     = Lager.Lagant     - TransLogg.Antall
            Lager.BrekkAnt   = Lager.BrekkAnt   + TransLogg.Antall
            Lager.BrekkVerdi = Lager.BrekkVerdi +
                               (TransLogg.VVareKost * TransLogg.Antall).
      WHEN 3 THEN
        DO:
          ASSIGN  /* Kundereklamasjon */
            Lager.ReklAnt    = Lager.ReklAnt    + TransLogg.Antall
            Lager.ReklVerdi  = Lager.ReklVerdi  +
                               (TransLogg.VVareKost * TransLogg.Antall)
            /* Korrigerer rabatt */
            Lager.AntRab     = Lager.AntRab +
                               (IF TransLogg.RabKr <> 0
                                  THEN TransLogg.Antall
                                  ELSE 0)
            Lager.VerdiRabatt = Lager.VerdiRabatt +
                               (IF TransLogg.RabKr <> 0
                                  THEN TransLogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                                  ELSE 0)
            /* Korrigerer salget ved retur - NB: Antallet er negativt i transaksjonen. */
            Lager.AntSolgt   = Lager.AntSolgt   + TransLogg.Antall
            Lager.SVK        = Lager.SVK        + (TransLogg.Antall * Translogg.VVareKost)
            Lager.VerdiSolgt = Lager.VerdiSolgt +
                               (
                                (TransLogg.Pris - Translogg.RabKr) -
                                TransLogg.Mva
                               ) * TransLogg.Antall.
        END.
      WHEN 4 THEN
          ASSIGN  /* Lagerreklamasjon */
            Lager.Lagant     = Lager.Lagant     - TransLogg.Antall
            Lager.ReklLAnt   = Lager.ReklLAnt   + TransLogg.Antall
            Lager.ReklLVerdi = Lager.ReklLVerdi +
                               (TransLogg.VVareKost * TransLogg.Antall).
      WHEN 5 THEN
        DO: /* Varekj�p m/vektet vareverdi. */
          /* Vekting av varekost skal bare gj�res n�r det finnes noe p� lager fra f�r. */
          IF Lager.Lagant > 0 AND TransLogg.Antall > 0 THEN
              ASSIGN
                wWork  = (Lager.Lagant   * Lager.VVareKost)  /* Gammel lagerverdi */
                wWork2 = (TransLogg.Pris * TransLogg.Antall) /* Verdi av innkj�p  */
                wWork3 = (wWork + wWork2) / ((Lager.LagAnt) + (TransLogg.Antall))
                wWork3 = IF wWork3 = ? THEN Lager.VVareKost ELSE wWork3.
          ELSE DO:
              IF Lager.Lagant <= 0 AND TransLogg.Antall > 0 THEN
                  wWork3 = TransLogg.VVareKost.
              ELSE
                  wWork3 = Lager.VVareKost.
          END.

          ASSIGN
            Lager.VVareKost = wWork3 /* Setter ny vektet snittpris */
            Lager.Lagant    = Lager.Lagant     + TransLogg.Antall
            Lager.KjopAnt   = Lager.KjopAnt    + TransLogg.Antall
            Lager.KjopVerdi = Lager.KjopVerdi  + wWork2.
        END.
      WHEN 6 THEN
        DO: /* Overf�ring */
          /* Henter LAger eller StLAger for mottagende butikk. */
          FIND bufLager EXCLUSIVE-LOCK WHERE
                     bufLager.ArtikkelNr = TransLogg.ArtikkelNr AND
                     bufLager.Butik      = TransLogg.OvButik NO-ERROR.
                   IF NOT AVAILABLE bufLager THEN
                   DO:
                     CREATE bufLager.
                     ASSIGN
                       bufLager.ArtikkelNr = TransLogg.ArtikkelNr
                       bufLager.Butik      = TransLogg.OvButik.
                   END.

          ASSIGN  /* Trekker ned lager p� fra butikk.            */
                  /* Alle posteringer skjer med vektet varekost. */
            Lager.Lagant  = Lager.Lagant     - TransLogg.Antall
            Lager.OvAnt   = Lager.OvAnt      - TransLogg.Antall
            Lager.OvVerdi = Lager.OvVerdi    -
                               (TransLogg.VVareKost * TransLogg.Antall).

          /* Innleveranse medf�rer ny vekting av varekost i mottagende butikk */
          IF bufLager.Lagant > 0 AND TransLogg.Antal > 0 THEN
              ASSIGN
                wWork  = (bufLager.Lagant * bufLager.VVareKost)   /* Gammel lagerverdi */
                wWork2 = (TransLogg.VVareKost * TransLogg.Antall) /* Verdi av overf�ring  */
                wWork3 = (wWork + wWork2) / ((bufLager.LagAnt) + (TransLogg.Antall))
                wWork3 = IF wWork3 = ? THEN bufLager.VVareKost ELSE wWork3.
          ELSE DO:
              IF bufLager.Lagant <= 0 AND TransLogg.Antall > 0 THEN
                  wWork3 = TransLogg.VVareKost.
              ELSE
                  wWork3 = bufLager.VVareKost.
          END.
          
          IF TransLogg.TBId <> 2 THEN 
          DO:
            ASSIGN  /* Posterer i mottagende butikk.  */
                    /* Alle posteringer skjer med vektet varekost. */
              bufLager.VVareKost = wWork3 /* Setter ny vektet snittpris */
              bufLager.Lagant    = bufLager.Lagant     + TransLogg.Antall
              bufLager.OvAnt     = bufLager.OvAnt      + TransLogg.Antall
              bufLager.OvVerdi   = bufLager.OvVerdi    +
                                    (TransLogg.VVareKost * TransLogg.Antall).
          END.
        END.
      WHEN 7 THEN
          ASSIGN  /* Lagerjustering */
            Lager.Lagant    = Lager.Lagant    - TransLogg.Antall
            Lager.JustAnt   = Lager.JustAnt   + TransLogg.Antall
            Lager.JustVerdi = Lager.JustVerdi +
                               (TransLogg.VVareKost * TransLogg.Antall).
      WHEN 8 THEN
        DO:
          ASSIGN  /* Nedskrivning */
                  /* Ingen endring i lagerantall. Kun VVarekost. */
            Lager.VVareKost = IF Lager.VVareKost <= 0 THEN ABS(TransLogg.Pris)
                                                  ELSE Lager.VVareKost - TransLogg.Pris
            Lager.NedVerdi  = Lager.NedVerdi  +
                              (TransLogg.Pris * TransLogg.Antall)
            Lager.NedAnt    = Lager.NedAnt    + TransLogg.Antall.
        END.
      WHEN 9 THEN
      DO:
          ASSIGN  /* Svinn */
            Lager.LagAnt     = Lager.LagAnt    - TransLogg.Antall
            Lager.SvinnAnt   = Lager.SvinnAnt  + TransLogg.Antall
            Lager.SvinnVerdi = Lager.SvinnVerdi  +
                               (TransLogg.Pris * TransLogg.Antall).
      END.
      WHEN 10 THEN
      DO:
          ASSIGN  /* Gjennkj�p */
            Lager.LagAnt        = Lager.LagAnt         + (TransLogg.Antall * -1) /* Negativt antall i TransLogg */
            Lager.GjenkjopAnt   = Lager.GjenkjopAnt    + TransLogg.Antall
            Lager.GjenkjopVerdi = Lager.GjenkjopVerdi  +
                               (
                                (TransLogg.Pris - Translogg.RabKr) -
                                TransLogg.Mva
                               ) * TransLogg.Antall
            Lager.AntRab     = Lager.AntRab +
                               (IF TransLogg.RabKr <> 0
                                  THEN TransLogg.Antall
                                  ELSE 0)
            Lager.VerdiRabatt = Lager.VerdiRabatt +
                               (IF TransLogg.RabKr <> 0
                                  THEN TransLogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                                  ELSE 0)
            /* Korrigerer salget ved retur - NB: Antallet er negativt i transaksjonen. */
            Lager.AntSolgt   = Lager.AntSolgt   + TransLogg.Antall
             Lager.SVK       = Lager.SVK        + (TransLogg.Antall * Translogg.VVareKost)
            Lager.VerdiSolgt = Lager.VerdiSolgt +
                               (
                                (TransLogg.Pris - Translogg.RabKr) -
                                TransLogg.Mva
                               ) * TransLogg.Antall.
      END.
      WHEN 11 THEN
          ASSIGN  /* Internt forbruk */
            Lager.LagAnt   = Lager.LagAnt    - TransLogg.Antall
            Lager.IntAnt   = Lager.IntAnt    + TransLogg.Antall
            Lager.IntVerdi = Lager.IntVerdi  +
                               (TransLogg.VVareKost * TransLogg.Antall).
    END CASE.

END PROCEDURE.




