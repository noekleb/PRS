TRIGGER PROCEDURE FOR CREATE OF Kunde.

  DEF VAR trgKundeNr AS DEC NO-UNDO.
  DEF VAR trgButikkNr LIKE Butiker.butik NO-UNDO.
  DEF VAR trgKortNr LIKE KundeKort.KortNr NO-UNDO.
  DEF VAR iGyldighet AS INTE NO-UNDO.
  DEF VAR wRecid AS RECID NO-UNDO.

  {syspara.i 14 2 3 iGyldighet  INT}
  {trg/c_w_trg.i &Type="C" &Fil="SkoTex.Kunde"}

  LOOPEN:
  DO WHILE TRUE:
      RUN trg/genkundenr.p (OUTPUT trgKundeNr,OUTPUT trgButikkNr,OUTPUT trgKortNr).

/*       MESSAGE "Kundetrigger: "               */
/*               SKIP "trgKundeNr" trgKundeNr   */
/*               SKIP "trgButikkNr" trgButikkNr */
/*               SKIP "trgKortNr"               */
/*               SKIP ERROR-STATUS:ERROR        */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK. */
      ASSIGN
        Kunde.KundeNr  = trgKundeNr
        Kunde.ButikkNr = trgButikkNr
        NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN DO:
          RUN createKundekort.p (INPUT trgKundeNr, INPUT trgKortNr, INPUT iGyldighet, OUTPUT wRecid).
          LEAVE LOOPEN.
      END.

  END.

  ASSIGN
      Kunde.aktiv    = TRUE
      Kunde.GruppeId = 1
      Kunde.typeId   = 1
      Kunde.Makskredit = 10000
      Kunde.KreditSperret = FALSE
      Kunde.Opphort = ?
      .



