
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT        PARAMETER fB_Id          AS DEC NO-UNDO.
DEF INPUT-OUTPUT PARAMETER cFilError      AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAMETER bLoggSaldoFeil AS LOG NO-UNDO.

DEF VAR plVareSum     AS DEC   NO-UNDO.
DEF VAR plBetSum      AS DEC   NO-UNDO.
DEF VAR plInnbetaling AS DEC   NO-UNDO.

/* Sjekker at bongens sum er = 0. */
ASSIGN plVareSum = 0
       plBetSum  = 0.


DO TRANSACTION:
    /* Henter bonghode */
    FIND BongHode EXCLUSIVE-LOCK WHERE
        BongHode.B_Id = fB_Id NO-ERROR.
    IF NOT AVAILABLE BongHode THEN
        RETURN.

    /* NB: Overføringer kontrolleres ikke. */
    FOR EACH BongLinje NO-LOCK WHERE
        BongLinje.B_Id = BongHode.B_Id AND
        BongLinje.Makulert = FALSE:

      /* Sum varetranser */
      /* TN 20/11-02 Deponering skal behandles som salg av vare. */
      /* Dropp skal summeres som vare */
      /* GAvekort ut skal behandles som vare i sumering */
      IF CAN-DO("001,002,003,004,005,006,007,008,009,010,011,012,057,072,134",string(BongLinje.TTId,"999")) THEN
      DO:
          IF can-do("134",string(BongLinje.TTID,"999")) THEN /* Solgt GAVEKORT */
            ASSIGN
            plVareSum = plVareSum + (BongLinje.LinjeSum - BongLinje.LinjeRab)
            .
          ELSE
          ASSIGN
            plVareSum = plVareSum + 
                        ((BongLinje.LinjeSum - 
                          BongLinje.LinjeRab -
                          BongLinje.SubTotalRab) * (IF BongLinje.Antall < 0
                                                                        THEN -1 
                                                                        ELSE 1))
            .
      END.

      /* Betalingstransaksjoner.                                */
      /* Subtotalrabatt er trukket fra fra før og skal ikke tas */
      /* med her.                                               */
      /* Dropp kontrolleres ikke. Kontant er der = 0.           */
      ELSE IF CAN-DO("050,051,052,053,054,055,056,058,059,061,062,064,065,066,067,069,070,071,073,078,079,089",string(BongLinje.TTId,"999")) THEN
      DO:
          /* Betaling av deponering, og veksel */
          /* Innbetaling på konto.            */
          IF CAN-DO("061,073,089",string(BongLinje.TTId,"999")) THEN
              ASSIGN
              plBetSum      = plBetSum - BongLinje.LinjeSum
              /* Spesiell håndtering av innbetalinger. */
              plInnbetaling = plInnbetaling +
                              (IF CAN-DO("061",STRING(BongLinje.TTId,"999"))
                                 THEN BongLinje.LinjeSum
                                 ELSE 0)
              .
          /* Betalingstranser. */
          ELSE
              ASSIGN
              plBetSum = plBetSum + BongLinje.LinjeSum
              .
      END.
    END.

    /* Logger hvis bongsummen ikke er lik 0. */
    IF (plVareSum - plBetSum <> 0) AND
       bLoggSaldoFeil = TRUE THEN
    DO:
        ASSIGN
        cFilError = cFilError + 
            (IF cFilError = ""
               THEN ""
               ELSE "|") + 
            " - Bongen's sum <> 0" + 
            " Diff: " + STRING(plVareSum,"->>>,>>>,>>9.99") + " - " + 
            STRING(plBetSum,"->>>,>>>,>>9.99") + " = " + 
            STRING(plVareSum - plBetSum,"->>>,>>>,>>9.99") + "." + 
            " (But/Kas/Dato/BongNr: " + 
            STRING(BongHode.ButikkNr) + "/" + 
            STRING(BongHode.KasseNr) + "/" + 
            STRING(BongHode.Dato) + "/" + 
            STRING(BongHode.BongNr) +  ")." + CHR(1) + "3"
        .
  /* MESSAGE "Gurre slapp løs" SKIP         */
  /*     cFilError SKIP                     */
  /*                                        */
  /*     plVareSum - plBetSum skip          */
  /*      bLoggSaldoFeil SKIP               */
  /*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    END.

    /* Oppdaterer bonghode med beløp. */
    ASSIGN BongHode.Belop = IF plInnbetaling <> 0
                              THEN plInnbetaling
                              ELSE plVareSum.
    RELEASE BongHode.
END. /* TRANSACTION */




