/*------------------------------------------------------------------------------
  Purpose:     manedsrapport_bygg_tmptabell.p     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER FI-ButListe AS CHAR NO-UNDO.
DEF INPUT PARAMETER FI-FraDato  AS DATE NO-UNDO.
DEF INPUT PARAMETER FI-TilDato  AS DATE NO-UNDO.

DEF VAR dDato             AS DATE NO-UNDO.
DEF VAR iLoop             AS INT NO-UNDO.
DEF VAR lVerdiUtbetBonger AS DEC NO-UNDO.
DEF VAR iantKunder        AS INT NO-UNDO.
DEF VAR lRabattKr         AS DEC NO-UNDO.
DEFINE VARIABLE lAntSolgt   AS DECIMAL NO-UNDO.
DEFINE VARIABLE lVerdisolgt AS DECIMAL NO-UNDO.
DEFINE VARIABLE lVarekost   AS DECIMAL NO-UNDO.
DEFINE VARIABLE lLagerstyrtVarekost AS DECIMAL NO-UNDO.

{manedsrapport_tmptabell.i &SHARED = "SHARED"}
DEF BUFFER tmptotManedsrap FOR tmpManedsrap.

EMPTY TEMP-TABLE tmpManedsrap.
EMPTY TEMP-TABLE tmptotManedsrap.

DO /* WITH FRAME Default-Frame*/:

  /* Total */
  FIND FIRST tmptotManedsrap WHERE
      tmptotManedsrap.ButikkNr = 0 AND
      tmptotManedsrap.Dato     = ? NO-ERROR.
  IF NOT AVAILABLE tmptotManedsrap THEN
  DO:
      CREATE tmptotManedsrap.
      ASSIGN
          tmptotManedsrap.ButikkNr = 0
          tmptotManedsrap.Dato     = ?.
  END.


  DATO:
  DO dDato = FI-FraDato TO FI-TilDato:
    BUTIKKER:
    DO iLoop = 1 TO NUM-ENTRIES(FI-ButListe):
      IF int(ENTRY(iLoop,FI-ButListe)) = 0 OR
          NOT CAN-FIND(Butiker WHERE
                       Butiker.Butik = INT(ENTRY(iLoop,FI-ButListe))) THEN
          NEXT BUTIKKER.
      ASSIGN 
          iAntKunder  = 0
          lRabattKr   = 0
          lAntSolgt   = 0
          lVerdiSolgt = 0
          lVarekost   = 0
          lLagerstyrtVarekost = 0.
      /* Leser antall kunder. */
      FOR EACH Akt_Rap NO-LOCK WHERE
          Akt_Rap.Dato  = dDato AND
          Akt_Rap.Butik = INT(ENTRY(iLoop,FI-ButListe)):
          ASSIGN
              iAntKunder  = iAntKunder  + Akt_Rap.Ant_Kunder
              lAntSolgt   = lAntSolgt   + Akt_Rap.Oms_Ant
              lVerdiSolgt = lVerdiSolgt + Akt_Rap.Oms_Verd
              .
      END.
      /* Henter rabattkr og varekost fra dag statistikk for butikken. */
      FIND StLinje NO-LOCK WHERE
        StLinje.StTypeId   = "BUTSTAT" AND
        StLinje.PerId      = "DAG" AND
        StLinje.DataObjekt = STRING(INT(ENTRY(iLoop,FI-ButListe)),"999999") AND
        StLinje.Diverse    = "" AND
        StLinje.Butik      = INT(ENTRY(iLoop,FI-ButListe)) AND
        StLinje.Aar        = YEAR(dDato) AND
        StLinje.PerLinNr   = (dDato + 1) - DATE(01,01,YEAR(dDato)) NO-ERROR.
      IF AVAILABLE StLinje THEN
          ASSIGN 
          lRabattKr = StLinje.VerdiRabatt
          lVarekost = StLinje.VVareKost.

      /* Leser varekost på lagerstyrte varer */
      LAGERKOST:
      FOR EACH ArtBas NO-LOCK WHERE
          ArtBas.Lager = TRUE:
          IF ArtBas.OPris THEN 
            NEXT.
          FIND StLinje NO-LOCK WHERE
            StLinje.StTypeId   = "ARTIKKEL" AND
            StLinje.PerId      = "DAG" AND
            StLinje.DataObjekt = STRING(ArtBas.ArtikkelNr,"9999999999999") AND
            StLinje.Diverse    = "" AND
            StLinje.Butik      = INT(ENTRY(iLoop,FI-ButListe)) AND
            StLinje.Aar        = YEAR(dDato) AND
            StLinje.PerLinNr   = (dDato + 1) - DATE(01,01,YEAR(dDato)) NO-ERROR.
          IF AVAILABLE StLinje THEN
              ASSIGN 
              lLagerstyrtVarekost = lLagerstyrtVarekost + StLinje.VVareKost.                
      END. /* LAGERKOST */

      /* Leser kassarapport postene */ 
      FOR EACH Kas_Rap NO-LOCK WHERE
          Kas_Rap.Dato  = dDato AND
          Kas_Rap.Butikk = int(ENTRY(iLoop,FI-ButListe))
          BREAK BY Kas_Rap.Dato
                BY Kas_Rap.Butikk
                BY Kas_Rap.Kasse:

          /* Pr. Butikk */
          FIND tmpManedsrap WHERE
              tmpManedsrap.ButikkNr = Kas_Rap.butikk AND
              tmpManedsrap.Dato     = Kas_Rap.Dato NO-ERROR.
          IF NOT AVAILABLE tmpManedsrap THEN
          DO:
              CREATE tmpManedsrap.
              ASSIGN
                  tmpManedsrap.ButikkNr     = Kas_Rap.butikk
                  tmpManedsrap.Dato         = Kas_Rap.Dato
                  tmpManedsrap.AntallKunder = iAntKunder
                  tmpManedsrap.RabattKr     = lRabattKr
                  tmpManedsRap.AntallSolgt  = lAntSolgt
                  tmpManedsRap.VerdiSolgt   = lVerdiSolgt
                  tmpManedsRap.TotVareKost  = ROUND(lVareKost,2)
                  tmpManedsRap.LagerstyrtVarekost = ROUND(lLagerstyrtVarekost,2)
                  .
          END.
              
          /* Opptalt fra kassereroppgjøret. */
          IF LAST-OF(Kas_Rap.Butikk) THEN
          DO:
              FOR EACH KassererOppgj NO-LOCK WHERE
                KassererOppgj.Dato     = Kas_Rap.Dato AND
                KassererOppgj.Butikk   = Kas_Rap.Butikk:
                ASSIGN
                    tmpManedsrap.Kontant = tmpManedsrap.Kontant 
                       + KassererOppgj.OpptaltVeksel /* Ved dagens slutt */
                       + KassererOppgj.OpptaltKontanter
                       + KassererOppgj.OpptaltSjekk
                       /*+ KassererOppgj.OpptaltReserve*/
                       + KassererOppgj.OpptaltValuta
                       + KassererOppgj.OpptaltBilag
                       - KassererOppgj.OpptaltInnVeksel.
              END.
          END.

          /* spes av bankposer */
          IF LAST-OF(Kas_Rap.Butikk) THEN
          DO:
              BANKPOSER:
              FOR EACH KassererOppgj NO-LOCK WHERE
                  KassererOppgj.Butikk    = int(ENTRY(iLoop,FI-ButListe)) AND
                  KassererOppgj.Dato      = dDato AND
                  KassererOppgj.OpptaltLevertBank   <> 0:
                ASSIGN
                  tmpManedsrap.BankPose = tmpManedsrap.BankPose + KassererOppgj.OpptaltLevertBank.
              END. /* BANKPOSER */
          END.

          KORTSPES:
          FOR EACH Kort_Spes NO-LOCK WHERE
              Kort_Spes.Dato       = dDato AND
              Kort_Spes.butik      = int(ENTRY(iLoop,FI-ButListe)) AND
              Kort_Spes.Kasse      = Kas_Rap.Kasse AND
              Kort_Spes.KassererNr = Kas_Rap.KassererNr AND
              Kort_Spes.z_Nummer   = Kas_Rap.z_Nummer:

              /* Bankkort er allerede tatt hånd om */
              IF CAN-DO("1,2,9,10,17,20",STRING(Kort_Spes.KortType)) THEN. /* gjør ingenting. */

              ELSE IF CAN-DO("3",STRING(Kort_Spes.KortType)) THEN
                  ASSIGN
                  tmpManedsrap.Visa = tmpManedsrap.Visa + Kort_Spes.Belop.

              ELSE IF CAN-DO("4",STRING(Kort_Spes.KortType)) THEN
                  ASSIGN
                  tmpManedsrap.Eurocard = tmpManedsrap.Eurocard + Kort_Spes.Belop.

              ELSE IF CAN-DO("5",STRING(Kort_Spes.KortType)) THEN
                  ASSIGN
                  tmpManedsrap.Amex = tmpManedsrap.Amex + Kort_Spes.Belop.

              ELSE IF CAN-DO("6",STRING(Kort_Spes.KortType)) THEN
                  ASSIGN
                  tmpManedsrap.Diners = tmpManedsrap.Diners + Kort_Spes.Belop.

              ELSE IF CAN-DO("23",STRING(Kort_Spes.KortType)) THEN
                  ASSIGN
                  tmpManedsrap.SenterGavekort = tmpManedsrap.SenterGavekort + Kort_Spes.Belop.

              /* Det øvrige samler vi opp her. */
              ELSE tmpManedsrap.DiverseKort = tmpManedsrap.DiverseKort + Kort_Spes.Belop.
          END. /* KORTSPES */

          /* Bonger med utbetalinger */
          ASSIGN
              lVerdiUtbetBonger  = 0.
          FOR EACH BongHode NO-LOCK WHERE
              BongHode.ButikkNr = Kas_Rap.Butikk AND
              BongHode.GruppeNr = 1 AND
              BongHode.KasseNr  = Kas_Rap.Kasse AND
              BongHode.Dato     = Kas_Rap.Dato AND
              BongHode.Belop    < 0:
              IF CAN-FIND(FIRST BongLinje WHERE
                          BongLinje.B_Id = BongHode.B_Id AND
                          BongLinje.TTId = 50) THEN
              ASSIGN
                  lVerdiUtbetBonger  = lVerdiUtbetBonger  + BongHode.Belop.
          END.
          /* Bonger med utbet. ferdig. */

          /* Omsetning inkl. mva */
          ASSIGN
            tmpManedsrap.OmsetningEksKred   = tmpManedsrap.OmsetningEksKred   + (Kas_Rap.MvaGrunnlag[ 1] + MvaBelop[ 1] +
                                                                                 Kas_Rap.MvaGrunnlag[ 2] + MvaBelop[ 2] +
                                                                                 Kas_Rap.MvaGrunnlag[ 3] + MvaBelop[ 3] +
                                                                                 Kas_Rap.MvaGrunnlag[ 4] + MvaBelop[ 4] +
                                                                                 Kas_Rap.MvaGrunnlag[ 5] + MvaBelop[ 5] +
                                                                                 Kas_Rap.MvaGrunnlag[ 6] + MvaBelop[ 6] +
                                                                                 Kas_Rap.MvaGrunnlag[ 7] + MvaBelop[ 7] +
                                                                                 Kas_Rap.MvaGrunnlag[ 8] + MvaBelop[ 8] +
                                                                                 Kas_Rap.MvaGrunnlag[ 9] + MvaBelop[ 9] +
                                                                                 Kas_Rap.MvaGrunnlag[10] + MvaBelop[10] - Kas_Rap.Kredit)
            tmpManedsrap.OmsetningMvaGrp1   = tmpManedsrap.OmsetningMvaGrp1   + (Kas_Rap.MvaGrunnlag[ 1] + MvaBelop[ 1])
            tmpManedsrap.OmsetningMvaGrp2   = tmpManedsrap.OmsetningMvaGrp2   + (Kas_Rap.MvaGrunnlag[ 2] + MvaBelop[ 2])
            tmpManedsrap.OmsetningMvaGrpDiv = tmpManedsrap.OmsetningMvaGrpdiv + (Kas_Rap.MvaGrunnlag[ 3] + MvaBelop[ 3] +
                                                                                 Kas_Rap.MvaGrunnlag[ 4] + MvaBelop[ 4] +
                                                                                 Kas_Rap.MvaGrunnlag[ 5] + MvaBelop[ 5] +
                                                                                 Kas_Rap.MvaGrunnlag[ 6] + MvaBelop[ 6] +
                                                                                 Kas_Rap.MvaGrunnlag[ 7] + MvaBelop[ 7] +
                                                                                 Kas_Rap.MvaGrunnlag[ 8] + MvaBelop[ 8] +
                                                                                 Kas_Rap.MvaGrunnlag[ 9] + MvaBelop[ 9] +
                                                                                 Kas_Rap.MvaGrunnlag[10] + MvaBelop[10])
           .

          /* Omsetning eks. mva */
          ASSIGN
            tmpManedsrap.OmsEksMvaGrp1   = tmpManedsrap.OmsEksMvaGrp1   + (Kas_Rap.MvaGrunnlag[ 1] )
            tmpManedsrap.OmsEksMvaGrp2   = tmpManedsrap.OmsEksMvaGrp2   + (Kas_Rap.MvaGrunnlag[ 2] )
            tmpManedsrap.OmsEksMvaGrpDiv = tmpManedsrap.OmsEksMvaGrpdiv + (Kas_Rap.MvaGrunnlag[ 3] +
                                                                           Kas_Rap.MvaGrunnlag[ 4] +
                                                                           Kas_Rap.MvaGrunnlag[ 5] +
                                                                           Kas_Rap.MvaGrunnlag[ 6] +
                                                                           Kas_Rap.MvaGrunnlag[ 7] +
                                                                           Kas_Rap.MvaGrunnlag[ 8] +
                                                                           Kas_Rap.MvaGrunnlag[ 9] +
                                                                           Kas_Rap.MvaGrunnlag[10])
           .

          /* Mva Kr */
          ASSIGN
              tmpManedsrap.MvaKrGrp1   = tmpManedsrap.MvaKrGrp1   + (MvaBelop[ 1])
              tmpManedsrap.MvaKrGrp2   = tmpManedsrap.MvaKrGrp2   + (MvaBelop[ 2])
              tmpManedsrap.MvaKrGrpDiv = tmpManedsrap.MvaKrGrpdiv + (MvaBelop[ 3] +
                                                                     MvaBelop[ 4] +
                                                                     MvaBelop[ 5] +
                                                                     MvaBelop[ 6] +
                                                                     MvaBelop[ 7] +
                                                                     MvaBelop[ 8] +
                                                                     MvaBelop[ 9] +
                                                                     MvaBelop[10])
           .

          /* omsetn slutt */

          ASSIGN
/*               tmpManedsrap.Kontant            = tmpManedsrap.Kontant            + Kas_Rap.Kontant */
              tmpManedsrap.BankKort           = tmpManedsrap.BankKort           + Kas_Rap.Bank + Kas_rap.Cashback + Kas_Rap.Reservelosning
              tmpManedsrap.KontKjopKasse      = tmpManedsrap.KontKjopKasse      + lVerdiUtbetBonger
/*               tmpManedsrap.Beskrivelse        = tmpManedsrap.Beskrivelse        + */              
              tmpManedsrap.TilgodeBruktEgne   = tmpManedsrap.TilgodeBruktEgne   + (Kas_Rap.TilgodeInn - Kas_Rap.TilgodeAndre)
              tmpManedsrap.TilgodeBruktAndre  = tmpManedsrap.TilgodeBruktAndre  + Kas_Rap.TilgodeAndre
              tmpManedsrap.GavekortBruktEgne  = tmpManedsrap.GavekortBruktEgne  + (Kas_Rap.GavekortInn - Kas_Rap.GavekortAndreInn)
              tmpManedsrap.GavekortBruktAndre = tmpManedsrap.GavekortBruktAndre + Kas_Rap.GavekortAndreInn 
              tmpManedsrap.TilgodeUt          = tmpManedsrap.TilgodeUt          + (Kas_Rap.TilgodeUt * -1)
              tmpManedsrap.GavekortUt         = tmpManedsrap.GavekortUt         + (Kas_Rap.GavekortUt * -1)  
              tmpManedsrap.InnbetaltKunde     = tmpManedsrap.InnbetaltKunde     + Kas_Rap.InnbetaltKunde

              tmpManedsrap.SumInnbutikk       = (tmpManedsrap.Kontant +
                                                 /*tmpManedsrap.BankPose + */
                                                 tmpManedsrap.BankKort +                                                                       
                                                 tmpManedsrap.Visa +              
                                                 tmpManedsrap.Eurocard +          
                                                 tmpManedsrap.Amex +              
                                                 tmpManedsrap.Diners +            
                                                 tmpManedsrap.SenterGavekort +    
                                                 tmpManedsrap.DiverseKort +       
                                                 tmpManedsrap.KontKjopKasse +     
                                                 tmpManedsrap.TilgodeBruktEgne +  
                                                 tmpManedsrap.TilgodeBruktAndre + 
                                                 tmpManedsrap.GavekortBruktEgne + 
                                                 tmpManedsrap.GavekortBruktAndre +
                                                 tmpManedsrap.TilgodeUt +         
                                                 tmpManedsrap.GavekortUt -
                                                 tmpManedsrap.InnbetaltKunde)                       
              tmpManedsrap.Kreditsalg         = tmpManedsrap.Kreditsalg         + Kas_Rap.Kredit
              tmpManedsRap.InnUtbetaling      = tmpManedsRap.InnUtbetaling + (kas_rap.kont_ut + kas_rap.kont_Inn)              
              .
          /* Bokføringsnr. */
          IF LAST-OF(Kas_Rap.Butikk) THEN
          DO:
              FIND FIRST Bokforingsbilag NO-LOCK WHERE
                  Bokforingsbilag.OmsetningsDato = Kas_Rap.Dato AND
                  Bokforingsbilag.ButikkNr       = Kas_Rap.Butikk NO-ERROR.
              IF AVAILABLE Bokforingsbilag THEN
                  tmpManedsrap.BokfNr = (IF Bokforingsbilag.GodkjentFlagg
                                          THEN STRING(Bokforingsbilag.BokforingsNr)
                                          ELSE "Ikke godkjent").
          END.
      END.
    END. /* BUTIKKER */

  END. /* DATO */
END.

/* Sumerer totaler */
FOR EACH tmpManedsrap EXCLUSIVE-LOCK:
    ASSIGN
        tmpManedsrap.DiffKasse = (tmpManedsrap.SumInnbutikk  /*- tmpManedsrap.BankPose*/ - tmpManedsrap.KontKjopKasse - tmpManedsrap.OmsetningEksKred)
        tmptotManedsrap.Kontant            = tmptotManedsrap.Kontant             + tmpManedsrap.Kontant               
        tmptotManedsrap.BankPose           = tmptotManedsrap.BankPose            + tmpManedsrap.BankPose               
        tmptotManedsrap.BankKort           = tmptotManedsrap.BankKort            + tmpManedsrap.BankKort               
        tmptotManedsrap.Visa               = tmptotManedsrap.Visa                + tmpManedsrap.Visa                
        tmptotManedsrap.Eurocard           = tmptotManedsrap.Eurocard            + tmpManedsrap.Eurocard               
        tmptotManedsrap.Amex               = tmptotManedsrap.Amex                + tmpManedsrap.Amex                
        tmptotManedsrap.Diners             = tmptotManedsrap.Diners              + tmpManedsrap.Diners                 
        tmptotManedsrap.SenterGavekort     = tmptotManedsrap.SenterGavekort      + tmpManedsrap.SenterGavekort         
        tmptotManedsrap.DiverseKort        = tmptotManedsrap.DiverseKort         + tmpManedsrap.DiverseKort            
        tmptotManedsrap.KontKjopKasse      = tmptotManedsrap.KontKjopKasse       + tmpManedsrap.KontKjopKasse       
        tmptotManedsrap.Beskrivelse        = tmptotManedsrap.Beskrivelse         + tmpManedsrap.Beskrivelse            
        tmptotManedsrap.TilgodeBruktEgne   = tmptotManedsrap.TilgodeBruktEgne    + tmpManedsrap.TilgodeBruktEgne       
        tmptotManedsrap.TilgodeBruktAndre  = tmptotManedsrap.TilgodeBruktAndre   + tmpManedsrap.TilgodeBruktAndre      
        tmptotManedsrap.GavekortBruktEgne  = tmptotManedsrap.GavekortBruktEgne   + tmpManedsrap.GavekortBruktEgne      
        tmptotManedsrap.GavekortBruktAndre = tmptotManedsrap.GavekortBruktAndre  + tmpManedsrap.GavekortBruktAndre     
        tmptotManedsrap.TilgodeUt          = tmptotManedsrap.TilgodeUt           + tmpManedsrap.TilgodeUt              
        tmptotManedsrap.GavekortUt         = tmptotManedsrap.GavekortUt          + tmpManedsrap.GavekortUt             
        tmptotManedsrap.SumInnbutikk       = tmptotManedsrap.SumInnbutikk        + tmpManedsrap.SumInnbutikk           
        tmptotManedsrap.OmsetningEksKred   = tmptotManedsrap.OmsetningEksKred    + tmpManedsrap.OmsetningEksKred       
        tmptotManedsrap.OmsetningMvaGrp1   = tmptotManedsrap.OmsetningMvaGrp1    + tmpManedsrap.OmsetningMvaGrp1       
        tmptotManedsrap.OmsetningMvaGrp2   = tmptotManedsrap.OmsetningMvaGrp2    + tmpManedsrap.OmsetningMvaGrp2       
        tmptotManedsrap.OmsetningMvaGrpDiv = tmptotManedsrap.OmsetningMvaGrpDiv  + tmpManedsrap.OmsetningMvaGrpDiv               
        tmptotManedsrap.DiffKasse          = tmptotManedsrap.DiffKasse           + tmpManedsrap.DiffKasse              
        tmptotManedsrap.Kreditsalg         = tmptotManedsrap.Kreditsalg          + tmpManedsrap.Kreditsalg             
        tmptotManedsrap.InnbetaltKunde     = tmptotManedsrap.InnbetaltKunde      + tmpManedsrap.InnbetaltKunde
        tmptotManedsRap.InnUtbetaling      = tmptotManedsRap.InnUtbetaling       + tmpManedsRap.InnUtbetaling      
        .
END.


