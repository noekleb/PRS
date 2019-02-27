/* get_fakturalinje.p
   Purpose: Hengte fakturalinjer for print
   Parameters: entry(1,icParam,"|"): Liste
               entry(2,icParam,"|"): WHERE betingelse
   Note: Henting av linjer görs genom query mot hode
-------------------------------------------------------------------------*/              
 
   
DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE ohTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR cIDliste   AS CHAR  NO-UNDO.
DEF VAR cDummy     AS CHAR   NO-UNDO.
DEF VAR httBuffer  AS HANDLE NO-UNDO.
DEF VAR hQuery     AS HANDLE NO-UNDO.
DEF VAR ix         AS INT    NO-UNDO.
DEF VAR bOk        AS LOG    NO-UNDO.
DEF VAR hHodeTT    AS HANDLE NO-UNDO.
DEF VAR hTTHodeBuff  AS HANDLE NO-UNDO.
DEF VAR qHode       AS HANDLE NO-UNDO.
DEF VAR cUkedagList AS CHAR   NO-UNDO.

IF icSessionId NE "validsession" THEN
  {incl/validatesession.i}


DEF TEMP-TABLE tmpManedsrap
    FIELD ButikkNr           AS INT  FORMAT ">>>>>9"
    FIELD Dato               AS DATE FORMAT "99/99/99"
    FIELD Ukedag             AS CHAR FORMAT "X(3)"
    FIELD BokfNr             AS CHAR FORMAT "x(20)"
    FIELD Kontant            AS DEC  FORMAT "->>>>>>9.99"
    FIELD BankPose           AS DEC  FORMAT "->>>>>>9.99"
    FIELD BankKort           AS DEC  FORMAT "->>>>>>9.99"
    FIELD Visa               AS DEC  FORMAT "->>>>>>9.99"
    FIELD Eurocard           AS DEC  FORMAT "->>>>>>9.99"
    FIELD Amex               AS DEC  FORMAT "->>>>>>9.99"
    FIELD Diners             AS DEC  FORMAT "->>>>>>9.99"
    FIELD SenterGavekort     AS DEC  FORMAT "->>>>>>9.99"
    FIELD DiverseKort        AS DEC  FORMAT "->>>>>>9.99"
    FIELD KontKjopKasse      AS DEC  FORMAT "->>>>>>9.99"
    FIELD Beskrivelse        AS CHAR FORMAT "x(30)"
    FIELD TilgodeBruktEgne   AS DEC  FORMAT "->>>>>>9.99"
    FIELD TilgodeBruktAndre  AS DEC  FORMAT "->>>>>>9.99"
    FIELD GavekortBruktEgne  AS DEC  FORMAT "->>>>>>9.99"
    FIELD GavekortBruktAndre AS DEC  FORMAT "->>>>>>9.99"
    FIELD TilgodeUt          AS DEC  FORMAT "->>>>>>9.99"
    FIELD GavekortUt         AS DEC  FORMAT "->>>>>>9.99"
    FIELD SumInnbutikk       AS DEC  FORMAT "->>>>>>9.99"
    FIELD OmsetningEksKred   AS DEC  FORMAT "->>>>>>9.99"
    FIELD DiffKasse          AS DEC  FORMAT "->>>>>>9.99"
    FIELD Kreditsalg         AS DEC  FORMAT "->>>>>>9.99"
    FIELD InnbetaltKunde     AS DEC  FORMAT "->>>>>>9.99"
    .

DEF BUFFER tmptotManedsrap FOR tmpManedsrap.


ASSIGN cIDliste    = ENTRY(1,icParam,"|").
ASSIGN cUkedagList = "Søn,Man,Tir,Ons,Tor,Fre,Lør".
ohTempTable = TEMP-TABLE tmpManedsrap:HANDLE.
RUN ByggTmpTabell.
/* DELETE OBJECT ohTempTable. */

PROCEDURE ByggTmpTabell:
   DEF VAR dDato             AS DATE NO-UNDO.
   DEF VAR iLoop             AS INT NO-UNDO.
   DEF VAR lVerdiUtbetBonger AS DEC NO-UNDO.
   DEFINE VARIABLE cButListe AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE dFraDato AS DATE  NO-UNDO.
   DEFINE VARIABLE dTilDato AS DATE  NO-UNDO.

   DO:
       ASSIGN cButListe = ENTRY(1,cIDliste,";")
              dFraDato  = DATE(ENTRY(2,cIDliste,";"))
              dTilDato  = DATE(ENTRY(3,cIDliste,";")). 

     /* Total */
/*      FIND FIRST tmptotManedsrap WHERE           */
/*          tmptotManedsrap.ButikkNr = 0 AND       */
/*          tmptotManedsrap.Dato     = ? NO-ERROR. */
/*      IF NOT AVAILABLE tmptotManedsrap THEN      */
/*      DO:                                        */
/*          CREATE tmptotManedsrap.                */
/*          ASSIGN                                 */
/*              tmptotManedsrap.ButikkNr = 0       */
/*              tmptotManedsrap.Dato     = ?.      */
/*      END.                                       */
     DATO:
     DO dDato = dFraDato TO dTilDato:
       BUTIKKER:
       DO iLoop = 1 TO NUM-ENTRIES(cButListe):
         IF int(ENTRY(iLoop,cButListe)) = 0 OR
             NOT CAN-FIND(Butiker WHERE
                          Butiker.Butik = INT(ENTRY(iLoop,cButListe))) THEN
             NEXT BUTIKKER.
         /* Leser kassarapport postene */
         FOR EACH Kas_Rap NO-LOCK WHERE
             Kas_Rap.Dato  = dDato AND
             Kas_Rap.Butikk = int(ENTRY(iLoop,cButListe))
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
                     tmpManedsrap.ButikkNr = Kas_Rap.butikk
                     tmpManedsrap.Dato     = Kas_Rap.Dato
                     tmpManedsrap.Ukedag   = ENTRY(WEEKDAY(Kas_Rap.Dato),cUkedagList).
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
                          + KassererOppgj.OpptaltBilag.
                 END.
             END.

             /* spes av bankposer */
             IF LAST-OF(Kas_Rap.Butikk) THEN
             DO:
                 BANKPOSER:
                 FOR EACH KassererOppgj NO-LOCK WHERE
                     KassererOppgj.Butikk    = int(ENTRY(iLoop,cButListe)) AND
                     KassererOppgj.Dato      = dDato AND
                     KassererOppgj.OpptaltLevertBank   <> 0:
                   ASSIGN
                     tmpManedsrap.BankPose = tmpManedsrap.BankPose + KassererOppgj.OpptaltLevertBank.
                 END. /* BANKPOSER */
             END.

             KORTSPES:
             FOR EACH Kort_Spes NO-LOCK WHERE
                 Kort_Spes.Dato     = dDato AND
                 Kort_Spes.butik    = int(ENTRY(iLoop,cButListe)) AND
                 Kort_Spes.Kasse    = Kas_Rap.Kasse AND
                 Kort_Spes.z_Nummer = Kas_Rap.z_Nummer:

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

             /* Omsetning */
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
                                                                                    Kas_Rap.MvaGrunnlag[10] + MvaBelop[10] - Kas_Rap.Kredit).

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
                                                    /*tmpManedsrap.BankPose +*/
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
                                                    tmpManedsrap.InnbetaltKunde +
                                                    Kas_Rap.Kredit)                       
                 tmpManedsrap.Kreditsalg         = tmpManedsrap.Kreditsalg         + Kas_Rap.Kredit              
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
/*          IF AVAIL tmpManedsrap THEN                                                                                                                        */
/*              ASSIGN                                                                                                                                        */
/*                  tmpManedsrap.DiffKasse = (tmpManedsrap.SumInnbutikk - tmpManedsrap.BankPose - tmpManedsrap.KontKjopKasse - tmpManedsrap.OmsetningEksKred) */
/*                  tmptotManedsrap.Kontant            = tmptotManedsrap.Kontant             + tmpManedsrap.Kontant                                           */
/*                  tmptotManedsrap.BankPose           = tmptotManedsrap.BankPose            + tmpManedsrap.BankPose                                          */
/*                  tmptotManedsrap.BankKort           = tmptotManedsrap.BankKort            + tmpManedsrap.BankKort                                          */
/*                  tmptotManedsrap.Visa               = tmptotManedsrap.Visa                + tmpManedsrap.Visa                                              */
/*                  tmptotManedsrap.Eurocard           = tmptotManedsrap.Eurocard            + tmpManedsrap.Eurocard                                          */
/*                  tmptotManedsrap.Amex               = tmptotManedsrap.Amex                + tmpManedsrap.Amex                                              */
/*                  tmptotManedsrap.Diners             = tmptotManedsrap.Diners              + tmpManedsrap.Diners                                            */
/*                  tmptotManedsrap.SenterGavekort     = tmptotManedsrap.SenterGavekort      + tmpManedsrap.SenterGavekort                                    */
/*                  tmptotManedsrap.DiverseKort        = tmptotManedsrap.DiverseKort         + tmpManedsrap.DiverseKort                                       */
/*                  tmptotManedsrap.KontKjopKasse      = tmptotManedsrap.KontKjopKasse       + tmpManedsrap.KontKjopKasse                                     */
/*                  tmptotManedsrap.Beskrivelse        = tmptotManedsrap.Beskrivelse         + tmpManedsrap.Beskrivelse                                       */
/*                  tmptotManedsrap.TilgodeBruktEgne   = tmptotManedsrap.TilgodeBruktEgne    + tmpManedsrap.TilgodeBruktEgne                                  */
/*                  tmptotManedsrap.TilgodeBruktAndre  = tmptotManedsrap.TilgodeBruktAndre   + tmpManedsrap.TilgodeBruktAndre                                 */
/*                  tmptotManedsrap.GavekortBruktEgne  = tmptotManedsrap.GavekortBruktEgne   + tmpManedsrap.GavekortBruktEgne                                 */
/*                  tmptotManedsrap.GavekortBruktAndre = tmptotManedsrap.GavekortBruktAndre  + tmpManedsrap.GavekortBruktAndre                                */
/*                  tmptotManedsrap.TilgodeUt          = tmptotManedsrap.TilgodeUt           + tmpManedsrap.TilgodeUt                                         */
/*                  tmptotManedsrap.GavekortUt         = tmptotManedsrap.GavekortUt          + tmpManedsrap.GavekortUt                                        */
/*                  tmptotManedsrap.SumInnbutikk       = tmptotManedsrap.SumInnbutikk        + tmpManedsrap.SumInnbutikk                                      */
/*                  tmptotManedsrap.OmsetningEksKred   = tmptotManedsrap.OmsetningEksKred    + tmpManedsrap.OmsetningEksKred                                  */
/*                  tmptotManedsrap.DiffKasse          = tmptotManedsrap.DiffKasse           + tmpManedsrap.DiffKasse                                         */
/*                  tmptotManedsrap.Kreditsalg         = tmptotManedsrap.Kreditsalg          + tmpManedsrap.Kreditsalg                                        */
/*                  tmptotManedsrap.InnbetaltKunde     = tmptotManedsrap.InnbetaltKunde      + tmpManedsrap.InnbetaltKunde                                    */
/*                  .                                                                                                                                         */
/*                                                                                                                                                            */

       END. /* BUTIKKER */

     END. /* DATO */
   END.


END PROCEDURE.
