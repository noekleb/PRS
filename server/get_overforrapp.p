/* get_fakturalinje.p
   Purpose: Oveføringer
   Parameters: entry(1,icParam,"|"): Liste
               entry(2,icParam,"|"): WHERE betingelse
               
               Liste: entry(1,,";") = butik från-til
                      entry(2,,";") = dato fra-til ","
                      entry(3,,";") = rapporttyp
               Om rapporttyp = 2 så ær butiker ointressanta, bara ikke uppdaterade øverføringar
               
   Note: Henting av linjer görs genom query mot hode
-------------------------------------------------------------------------*/              
 
   
DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE ohTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR cIDliste   AS CHAR  NO-UNDO.
DEF VAR cButFraTil AS CHAR   NO-UNDO.
DEF VAR cDatoFraTil AS CHAR NO-UNDO.
DEFINE VARIABLE dFraDato AS DATE  NO-UNDO.
DEFINE VARIABLE dTilDato AS DATE  NO-UNDO.
DEF VAR cType      AS CHAR   NO-UNDO.
DEF VAR cDummy     AS CHAR   NO-UNDO.
DEF VAR httBuffer  AS HANDLE NO-UNDO.
DEF VAR hQuery     AS HANDLE NO-UNDO.
DEF VAR ix         AS INT    NO-UNDO.
DEF VAR bOk        AS LOG    NO-UNDO.
DEF VAR hHodeTT    AS HANDLE NO-UNDO.
DEF VAR hTTHodeBuff  AS HANDLE NO-UNDO.
DEF VAR qHode      AS HANDLE NO-UNDO.
DEF VAR lTilBut    AS LOG NO-UNDO.
DEF VAR cButFnavn  AS CHAR NO-UNDO.
DEF VAR cButTnavn  AS CHAR NO-UNDO.

IF icSessionId NE "validsession" THEN
  {incl/validatesession.i}


DEF TEMP-TABLE tmpOvBunt NO-UNDO LIKE OvBunt
    FIELD ButFra AS CHAR
    FIELD ButTil AS CHAR.

DEF TEMP-TABLE tmpRapport NO-UNDO
    FIELD tekst AS CHAR
    FIELD ButFra AS INTE
    FIELD ButFnavn AS CHAR
    FIELD ButTil AS INTE
    FIELD ButTnavn AS CHAR
    FIELD avd1 AS DECI
    FIELD avd2 AS DECI
    FIELD avd3 AS DECI
    FIELD avd4 AS DECI
    FIELD avd5 AS DECI
    FIELD avd6 AS DECI
    FIELD avd7 AS DECI
    FIELD avd8 AS DECI
    FIELD avd9 AS DECI
    FIELD Ukjent AS DECI
    FIELD hSum AS DECI
    FIELD ant1 AS INT
    FIELD ant2 AS INT
    FIELD ant3 AS INT
    FIELD ant4 AS INT
    FIELD ant5 AS INT
    FIELD ant6 AS INT
    FIELD ant7 AS INT
    FIELD ant8 AS INT
    FIELD ant9 AS INT
    FIELD AntU AS INTE
    FIELD hAntSum AS INT
        INDEX but IS PRIMARY UNIQUE butfra buttil.
/*     FIELD ButikkNr           AS INT  FORMAT ">>>>>9"      */
/*     FIELD Dato               AS DATE FORMAT "99/99/99"    */
/*     FIELD BokfNr             AS CHAR FORMAT "x(20)"       */
/*     FIELD Kontant            AS DEC  FORMAT "->>>>>>9.99" */
/*     FIELD BankPose           AS DEC  FORMAT "->>>>>>9.99" */

/* DEF BUFFER tmptotManedsrap FOR tmpManedsrap. */


ASSIGN cIDliste    = ENTRY(1,icParam,"|").

   ASSIGN cButFraTil  = ENTRY(1,cIDliste,";")
          cDatoFraTil = ENTRY(2,cIDliste,";")
          cType       = ENTRY(3,cIDliste,";")
          lTilBut     = ENTRY(4,cIDliste,";") = "J".
   IF cType = "1" THEN
       ASSIGN dFraDato    = DATE(ENTRY(1,cDatoFraTil))
              dTilDato    = DATE(ENTRY(2,cDatoFraTil)). 

ohTempTable = IF cType = "2" THEN TEMP-TABLE tmpOvBunt:HANDLE ELSE TEMP-TABLE tmpRapport:HANDLE.
/* MESSAGE cButFraTil                     */
/*         cDatoFraTil                    */
/*         cType                          */
/*         dFraDato                       */
/*         dTilDato                       */
/*                                        */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */


IF cType = "1" THEN
    RUN ByggOverforte.
ELSE 
    RUN ByggIkkeOppdaterte.
/* DELETE OBJECT ohTempTable. */
PROCEDURE ByggIkkeOppdaterte:
    FOR EACH OvBunt WHERE OvBunt.DatoOppdatert = ?:
        CREATE tmpOvBunt.
        BUFFER-COPY OvBunt TO tmpOvBunt NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE tmpOvBunt.
        ELSE DO:
            FOR EACH OvBuffer OF OvBunt NO-LOCK:
                IF NOT CAN-DO(tmpOvBunt.ButFra,STRING(OvBuffer.ButikkNrFra)) THEN       
                    ASSIGN tmpOvBunt.ButFra = tmpOvBunt.ButFra + (IF tmpOvBunt.ButFra = "" THEN "" ELSE ",") + STRING(OvBuffer.ButikkNrFra).
                IF NOT CAN-DO(tmpOvBunt.ButTil,STRING(OvBuffer.ButikkNrTil)) THEN       
                    ASSIGN tmpOvBunt.ButTil = tmpOvBunt.ButTil + (IF tmpOvBunt.ButTil = "" THEN "" ELSE ",") + STRING(OvBuffer.ButikkNrTil).



            END.
        END.
    END.
/*     MESSAGE CAN-FIND(FIRST tmpOvbunt)      */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
END PROCEDURE.

PROCEDURE ByggOverforte:
   DEF VAR dDato             AS DATE NO-UNDO.
   DEF VAR iLoop             AS INT NO-UNDO.
   DEF VAR lVerdiUtbetBonger AS DEC NO-UNDO.
   DEFINE VARIABLE iButFra AS INTEGER    NO-UNDO.
   DEFINE VARIABLE iButTil AS INTEGER    NO-UNDO.
   DEFINE VARIABLE iAvdelingnr AS INTEGER    NO-UNDO.
   iButFra = INT(cButFraTil).
   FIND butiker NO-LOCK WHERE butiker.butik = iButFra NO-ERROR.
   IF lTilBut = FALSE THEN
       cButFnavn = IF AVAIL butiker THEN Butiker.butnamn ELSE "".
   ELSE
       cButTnavn = IF AVAIL butiker THEN Butiker.butnamn ELSE "".
   DO:
       DO dDato = dFraDato TO dTilDato:
           FOR EACH OvBunt WHERE OvBunt.DatoOppdatert = dDato NO-LOCK:
               FOR EACH OvBuffer OF OvBunt NO-LOCK:
                   IF lTilBut = FALSE AND OvBuffer.ButikkNrFra <> iButFra THEN
                       NEXT.
                   IF lTilBut = TRUE AND OvBuffer.ButikkNrTil <> iButFra THEN
                       NEXT.
                   FIND Artbas WHERE artbas.artikkelnr = OvBuffer.artikkelnr NO-LOCK NO-ERROR.
                   IF AVAIL artbas THEN DO:
                       FIND huvgr OF artbas NO-LOCK NO-ERROR.
                       IF AVAIL huvgr THEN 
                           iAvdelingNr = Huvgr.avdelingnr.
                       ELSE
                           iAvdelingnr = 10.
                   END.
                   ELSE
                       iAvdelingnr = 10.
                   FIND tmpRapport WHERE tmpRapport.ButFra = OvBuffer.ButikkNrFra AND
                                         tmpRapport.ButTil = OvBuffer.ButikkNrTil NO-ERROR.
                   IF NOT AVAIL tmpRapport THEN DO:
                       IF lTilBut = FALSE THEN DO:
                           FIND butiker NO-LOCK WHERE butiker.butik = OvBuffer.ButikkNrTil NO-ERROR.
                           cButTnavn = IF AVAIL butiker THEN Butiker.butnamn ELSE "".
                       END.
                       ELSE DO:
                           FIND butiker NO-LOCK WHERE butiker.butik = OvBuffer.ButikkNrFra NO-ERROR.
                           cButFnavn = IF AVAIL butiker THEN Butiker.butnamn ELSE "".
                       END.
                       CREATE tmpRapport.
                       ASSIGN tmpRapport.ButFra = OvBuffer.ButikkNrFra
                              tmpRapport.ButTil = OvBuffer.ButikkNrTil
                              tmpRapport.ButFnavn = cButFnavn
                              tmpRapport.ButTnavn = cButTnavn.
                   END.
                   CASE iAvdelingNr:
                       WHEN 1 THEN
                           ASSIGN tmpRapport.avd1 = tmpRapport.avd1 + OvBuffer.VareKost * OvBuffer.antall
                                  tmpRapport.ant1 = tmpRapport.ant1 + OvBuffer.antall.
                       WHEN 2 THEN
                           ASSIGN tmpRapport.avd2 = tmpRapport.avd2 + OvBuffer.VareKost * OvBuffer.antall
                                  tmpRapport.ant2 = tmpRapport.ant2 + OvBuffer.antall.
                       WHEN 3 THEN               
                           ASSIGN tmpRapport.avd3 = tmpRapport.avd3 + OvBuffer.VareKost * OvBuffer.antall
                                  tmpRapport.ant3 = tmpRapport.ant3 + OvBuffer.antall.
                       WHEN 4 THEN               
                           ASSIGN tmpRapport.avd4 = tmpRapport.avd4 + OvBuffer.VareKost * OvBuffer.antall
                                  tmpRapport.ant4 = tmpRapport.ant4 + OvBuffer.antall.
                       WHEN 5 THEN               
                           ASSIGN tmpRapport.avd5 = tmpRapport.avd5 + OvBuffer.VareKost * OvBuffer.antall
                                  tmpRapport.ant5 = tmpRapport.ant5 + OvBuffer.antall.
                       WHEN 6 THEN               
                           ASSIGN tmpRapport.avd6 = tmpRapport.avd6 + OvBuffer.VareKost * OvBuffer.antall
                                  tmpRapport.ant6 = tmpRapport.ant6 + OvBuffer.antall.
                       WHEN 7 THEN               
                           ASSIGN tmpRapport.avd7 = tmpRapport.avd7 + OvBuffer.VareKost * OvBuffer.antall
                                  tmpRapport.ant7 = tmpRapport.ant7 + OvBuffer.antall.
                       WHEN 8 THEN               
                           ASSIGN tmpRapport.avd8 = tmpRapport.avd8 + OvBuffer.VareKost * OvBuffer.antall
                                  tmpRapport.ant8 = tmpRapport.ant8 + OvBuffer.antall.
                       WHEN 9 THEN               
                           ASSIGN tmpRapport.avd9 = tmpRapport.avd9 + OvBuffer.VareKost * OvBuffer.antall
                                  tmpRapport.ant9 = tmpRapport.ant9 + OvBuffer.antall.
                       WHEN 10 THEN
                           ASSIGN tmpRapport.ukjent = tmpRapport.ukjent + OvBuffer.VareKost * OvBuffer.antall
                                  tmpRapport.antU = tmpRapport.antU + OvBuffer.antall.
                   END CASE.
                   ASSIGN tmpRapport.hSum = tmpRapport.hsum + OvBuffer.VareKost * OvBuffer.antall
                          tmpRapport.hAntSum = tmpRapport.hAntSum + OvBuffer.antall.
               END.
           END.
       END.
FOR EACH tmpRapport.
    DISP avd2.
END.

/*      DATO:                                                                                                                                                       */
/*      DO dDato = dFraDato TO dTilDato:                                                                                                                            */
/*        BUTIKKER:                                                                                                                                                 */
/*        DO iLoop = 1 TO NUM-ENTRIES(cButListe):                                                                                                                   */
/*          IF int(ENTRY(iLoop,cButListe)) = 0 OR                                                                                                                   */
/*              NOT CAN-FIND(Butiker WHERE                                                                                                                          */
/*                           Butiker.Butik = INT(ENTRY(iLoop,cButListe))) THEN                                                                                      */
/*              NEXT BUTIKKER.                                                                                                                                      */
/*          /* Leser kassarapport postene */                                                                                                                        */
/*          FOR EACH Kas_Rap NO-LOCK WHERE                                                                                                                          */
/*              Kas_Rap.Dato  = dDato AND                                                                                                                           */
/*              Kas_Rap.Butik = int(ENTRY(iLoop,cButListe))                                                                                                         */
/*              BREAK BY Kas_Rap.Dato                                                                                                                               */
/*                    BY Kas_Rap.Butik                                                                                                                              */
/*                    BY Kas_Rap.Kasse:                                                                                                                             */
/*                                                                                                                                                                  */
/*              /* Pr. Butikk */                                                                                                                                    */
/*              FIND tmpManedsrap WHERE                                                                                                                             */
/*                  tmpManedsrap.ButikkNr = Kas_Rap.butik AND                                                                                                       */
/*                  tmpManedsrap.Dato     = Kas_Rap.Dato NO-ERROR.                                                                                                  */
/*              IF NOT AVAILABLE tmpManedsrap THEN                                                                                                                  */
/*              DO:                                                                                                                                                 */
/*                  CREATE tmpManedsrap.                                                                                                                            */
/*                  ASSIGN                                                                                                                                          */
/*                      tmpManedsrap.ButikkNr = Kas_Rap.butik                                                                                                       */
/*                      tmpManedsrap.Dato     = Kas_Rap.Dato.                                                                                                       */
/*              END.                                                                                                                                                */
/*                                                                                                                                                                  */
/*              /* Opptalt fra kassereroppgjøret. */                                                                                                                */
/*              IF LAST-OF(Kas_Rap.Butik) THEN                                                                                                                      */
/*              DO:                                                                                                                                                 */
/*                  FOR EACH KassererOppgj NO-LOCK WHERE                                                                                                            */
/*                    KassererOppgj.Dato     = Kas_Rap.Dato AND                                                                                                     */
/*                    KassererOppgj.Butikk   = Kas_Rap.Butikk:                                                                                                      */
/*                    ASSIGN                                                                                                                                        */
/*                        tmpManedsrap.Kontant = tmpManedsrap.Kontant                                                                                               */
/*                           + KassererOppgj.OpptaltVeksel /* Ved dagens slutt */                                                                                   */
/*                           + KassererOppgj.OpptaltKontanter                                                                                                       */
/*                           + KassererOppgj.OpptaltSjekk                                                                                                           */
/*                           /*+ KassererOppgj.OpptaltReserve*/                                                                                                     */
/*                           + KassererOppgj.OpptaltValuta                                                                                                          */
/*                           + KassererOppgj.OpptaltBilag.                                                                                                          */
/*                  END.                                                                                                                                            */
/*              END.                                                                                                                                                */
/*                                                                                                                                                                  */
/*              /* spes av bankposer */                                                                                                                             */
/*              IF LAST-OF(Kas_Rap.Butik) THEN                                                                                                                      */
/*              DO:                                                                                                                                                 */
/*                  BANKPOSER:                                                                                                                                      */
/*                  FOR EACH KassererOppgj NO-LOCK WHERE                                                                                                            */
/*                      KassererOppgj.Butikk    = int(ENTRY(iLoop,cButListe)) AND                                                                                   */
/*                      KassererOppgj.Dato      = dDato AND                                                                                                         */
/*                      KassererOppgj.OpptaltLevertBank   <> 0:                                                                                                     */
/*                    ASSIGN                                                                                                                                        */
/*                      tmpManedsrap.BankPose = tmpManedsrap.BankPose + KassererOppgj.OpptaltLevertBank.                                                            */
/*                  END. /* BANKPOSER */                                                                                                                            */
/*              END.                                                                                                                                                */
/*                                                                                                                                                                  */
/*              KORTSPES:                                                                                                                                           */
/*              FOR EACH Kort_Spes NO-LOCK WHERE                                                                                                                    */
/*                  Kort_Spes.Dato     = dDato AND                                                                                                                  */
/*                  Kort_Spes.butik    = int(ENTRY(iLoop,cButListe)) AND                                                                                            */
/*                  Kort_Spes.Kasse    = Kas_Rap.Kasse AND                                                                                                          */
/*                  Kort_Spes.z_Nummer = Kas_Rap.z_Nummer:                                                                                                          */
/*                                                                                                                                                                  */
/*                  /* Bankkort er allerede tatt hånd om */                                                                                                         */
/*                  IF CAN-DO("1,2,9,10,17,20",STRING(Kort_Spes.KortType)) THEN. /* gjør ingenting. */                                                              */
/*                                                                                                                                                                  */
/*                  ELSE IF CAN-DO("3",STRING(Kort_Spes.KortType)) THEN                                                                                             */
/*                      ASSIGN                                                                                                                                      */
/*                      tmpManedsrap.Visa = tmpManedsrap.Visa + Kort_Spes.Belop.                                                                                    */
/*                                                                                                                                                                  */
/*                  ELSE IF CAN-DO("4",STRING(Kort_Spes.KortType)) THEN                                                                                             */
/*                      ASSIGN                                                                                                                                      */
/*                      tmpManedsrap.Eurocard = tmpManedsrap.Eurocard + Kort_Spes.Belop.                                                                            */
/*                                                                                                                                                                  */
/*                  ELSE IF CAN-DO("5",STRING(Kort_Spes.KortType)) THEN                                                                                             */
/*                      ASSIGN                                                                                                                                      */
/*                      tmpManedsrap.Amex = tmpManedsrap.Amex + Kort_Spes.Belop.                                                                                    */
/*                                                                                                                                                                  */
/*                  ELSE IF CAN-DO("6",STRING(Kort_Spes.KortType)) THEN                                                                                             */
/*                      ASSIGN                                                                                                                                      */
/*                      tmpManedsrap.Diners = tmpManedsrap.Diners + Kort_Spes.Belop.                                                                                */
/*                                                                                                                                                                  */
/*                  ELSE IF CAN-DO("23",STRING(Kort_Spes.KortType)) THEN                                                                                            */
/*                      ASSIGN                                                                                                                                      */
/*                      tmpManedsrap.SenterGavekort = tmpManedsrap.SenterGavekort + Kort_Spes.Belop.                                                                */
/*                                                                                                                                                                  */
/*                  /* Det øvrige samler vi opp her. */                                                                                                             */
/*                  ELSE tmpManedsrap.DiverseKort = tmpManedsrap.DiverseKort + Kort_Spes.Belop.                                                                     */
/*              END. /* KORTSPES */                                                                                                                                 */
/*                                                                                                                                                                  */
/*              /* Bonger med utbetalinger */                                                                                                                       */
/*              ASSIGN                                                                                                                                              */
/*                  lVerdiUtbetBonger  = 0.                                                                                                                         */
/*              FOR EACH BongHode NO-LOCK WHERE                                                                                                                     */
/*                  BongHode.ButikkNr = Kas_Rap.Butik AND                                                                                                           */
/*                  BongHode.GruppeNr = 1 AND                                                                                                                       */
/*                  BongHode.KasseNr  = Kas_Rap.Kasse AND                                                                                                           */
/*                  BongHode.Dato     = Kas_Rap.Dato AND                                                                                                            */
/*                  BongHode.Belop    < 0:                                                                                                                          */
/*                  IF CAN-FIND(FIRST BongLinje WHERE                                                                                                               */
/*                              BongLinje.B_Id = BongHode.B_Id AND                                                                                                  */
/*                              BongLinje.TTId = 50) THEN                                                                                                           */
/*                  ASSIGN                                                                                                                                          */
/*                      lVerdiUtbetBonger  = lVerdiUtbetBonger  + BongHode.Belop.                                                                                   */
/*              END.                                                                                                                                                */
/*              /* Bonger med utbet. ferdig. */                                                                                                                     */
/*                                                                                                                                                                  */
/*              /* Omsetning */                                                                                                                                     */
/*              ASSIGN                                                                                                                                              */
/*                tmpManedsrap.OmsetningEksKred   = tmpManedsrap.OmsetningEksKred   + (Kas_Rap.MvaGrunnlag[ 1] + MvaBelop[ 1] +                                     */
/*                                                                                     Kas_Rap.MvaGrunnlag[ 2] + MvaBelop[ 2] +                                     */
/*                                                                                     Kas_Rap.MvaGrunnlag[ 3] + MvaBelop[ 3] +                                     */
/*                                                                                     Kas_Rap.MvaGrunnlag[ 4] + MvaBelop[ 4] +                                     */
/*                                                                                     Kas_Rap.MvaGrunnlag[ 5] + MvaBelop[ 5] +                                     */
/*                                                                                     Kas_Rap.MvaGrunnlag[ 6] + MvaBelop[ 6] +                                     */
/*                                                                                     Kas_Rap.MvaGrunnlag[ 7] + MvaBelop[ 7] +                                     */
/*                                                                                     Kas_Rap.MvaGrunnlag[ 8] + MvaBelop[ 8] +                                     */
/*                                                                                     Kas_Rap.MvaGrunnlag[ 9] + MvaBelop[ 9] +                                     */
/*                                                                                     Kas_Rap.MvaGrunnlag[10] + MvaBelop[10] - Kas_Rap.Kredit).                    */
/*                                                                                                                                                                  */
/*              /* omsetn slutt */                                                                                                                                  */
/*                                                                                                                                                                  */
/*              ASSIGN                                                                                                                                              */
/*    /*               tmpManedsrap.Kontant            = tmpManedsrap.Kontant            + Kas_Rap.Kontant */                                                       */
/*                  tmpManedsrap.BankKort           = tmpManedsrap.BankKort           + Kas_Rap.Bank + Kas_rap.Cashback + Kas_Rap.Reservelosning                    */
/*                  tmpManedsrap.KontKjopKasse      = tmpManedsrap.KontKjopKasse      + lVerdiUtbetBonger                                                           */
/*    /*               tmpManedsrap.Beskrivelse        = tmpManedsrap.Beskrivelse        + */                                                                       */
/*                  tmpManedsrap.TilgodeBruktEgne   = tmpManedsrap.TilgodeBruktEgne   + (Kas_Rap.TilgodeInn - Kas_Rap.TilgodeAndre)                                 */
/*                  tmpManedsrap.TilgodeBruktAndre  = tmpManedsrap.TilgodeBruktAndre  + Kas_Rap.TilgodeAndre                                                        */
/*                  tmpManedsrap.GavekortBruktEgne  = tmpManedsrap.GavekortBruktEgne  + (Kas_Rap.GavekortInn - Kas_Rap.GavekortAndreInn)                            */
/*                  tmpManedsrap.GavekortBruktAndre = tmpManedsrap.GavekortBruktAndre + Kas_Rap.GavekortAndreInn                                                    */
/*                  tmpManedsrap.TilgodeUt          = tmpManedsrap.TilgodeUt          + (Kas_Rap.TilgodeUt * -1)                                                    */
/*                  tmpManedsrap.GavekortUt         = tmpManedsrap.GavekortUt         + (Kas_Rap.GavekortUt * -1)                                                   */
/*                  tmpManedsrap.InnbetaltKunde     = tmpManedsrap.InnbetaltKunde     + Kas_Rap.InnbetaltKunde                                                      */
/*                                                                                                                                                                  */
/*                  tmpManedsrap.SumInnbutikk       = (tmpManedsrap.Kontant +                                                                                       */
/*                                                     tmpManedsrap.BankPose +                                                                                      */
/*                                                     tmpManedsrap.BankKort +                                                                                      */
/*                                                     tmpManedsrap.Visa +                                                                                          */
/*                                                     tmpManedsrap.Eurocard +                                                                                      */
/*                                                     tmpManedsrap.Amex +                                                                                          */
/*                                                     tmpManedsrap.Diners +                                                                                        */
/*                                                     tmpManedsrap.SenterGavekort +                                                                                */
/*                                                     tmpManedsrap.DiverseKort +                                                                                   */
/*                                                     tmpManedsrap.KontKjopKasse +                                                                                 */
/*                                                     tmpManedsrap.TilgodeBruktEgne +                                                                              */
/*                                                     tmpManedsrap.TilgodeBruktAndre +                                                                             */
/*                                                     tmpManedsrap.GavekortBruktEgne +                                                                             */
/*                                                     tmpManedsrap.GavekortBruktAndre +                                                                            */
/*                                                     tmpManedsrap.TilgodeUt +                                                                                     */
/*                                                     tmpManedsrap.GavekortUt -                                                                                    */
/*                                                     tmpManedsrap.InnbetaltKunde)                                                                                 */
/*                  tmpManedsrap.Kreditsalg         = tmpManedsrap.Kreditsalg         + Kas_Rap.Kredit                                                              */
/*                  .                                                                                                                                               */
/*              /* Bokføringsnr. */                                                                                                                                 */
/*              IF LAST-OF(Kas_Rap.Butik) THEN                                                                                                                      */
/*              DO:                                                                                                                                                 */
/*                  FIND FIRST Bokforingsbilag NO-LOCK WHERE                                                                                                        */
/*                      Bokforingsbilag.OmsetningsDato = Kas_Rap.Dato AND                                                                                           */
/*                      Bokforingsbilag.ButikkNr       = Kas_Rap.Butik NO-ERROR.                                                                                    */
/*                  IF AVAILABLE Bokforingsbilag THEN                                                                                                               */
/*                      tmpManedsrap.BokfNr = (IF Bokforingsbilag.GodkjentFlagg                                                                                     */
/*                                              THEN string(Bokforingsbilag.BokforingsNr)                                                                           */
/*                                              ELSE "Ikke godkjent").                                                                                              */
/*              END.                                                                                                                                                */
/*          END.                                                                                                                                                    */
/* /*          IF AVAIL tmpManedsrap THEN                                                                                                                        */ */
/* /*              ASSIGN                                                                                                                                        */ */
/* /*                  tmpManedsrap.DiffKasse = (tmpManedsrap.SumInnbutikk - tmpManedsrap.BankPose - tmpManedsrap.KontKjopKasse - tmpManedsrap.OmsetningEksKred) */ */
/* /*                  tmptotManedsrap.Kontant            = tmptotManedsrap.Kontant             + tmpManedsrap.Kontant                                           */ */
/* /*                  tmptotManedsrap.BankPose           = tmptotManedsrap.BankPose            + tmpManedsrap.BankPose                                          */ */
/* /*                  tmptotManedsrap.BankKort           = tmptotManedsrap.BankKort            + tmpManedsrap.BankKort                                          */ */
/* /*                  tmptotManedsrap.Visa               = tmptotManedsrap.Visa                + tmpManedsrap.Visa                                              */ */
/* /*                  tmptotManedsrap.Eurocard           = tmptotManedsrap.Eurocard            + tmpManedsrap.Eurocard                                          */ */
/* /*                  tmptotManedsrap.Amex               = tmptotManedsrap.Amex                + tmpManedsrap.Amex                                              */ */
/* /*                  tmptotManedsrap.Diners             = tmptotManedsrap.Diners              + tmpManedsrap.Diners                                            */ */
/* /*                  tmptotManedsrap.SenterGavekort     = tmptotManedsrap.SenterGavekort      + tmpManedsrap.SenterGavekort                                    */ */
/* /*                  tmptotManedsrap.DiverseKort        = tmptotManedsrap.DiverseKort         + tmpManedsrap.DiverseKort                                       */ */
/* /*                  tmptotManedsrap.KontKjopKasse      = tmptotManedsrap.KontKjopKasse       + tmpManedsrap.KontKjopKasse                                     */ */
/* /*                  tmptotManedsrap.Beskrivelse        = tmptotManedsrap.Beskrivelse         + tmpManedsrap.Beskrivelse                                       */ */
/* /*                  tmptotManedsrap.TilgodeBruktEgne   = tmptotManedsrap.TilgodeBruktEgne    + tmpManedsrap.TilgodeBruktEgne                                  */ */
/* /*                  tmptotManedsrap.TilgodeBruktAndre  = tmptotManedsrap.TilgodeBruktAndre   + tmpManedsrap.TilgodeBruktAndre                                 */ */
/* /*                  tmptotManedsrap.GavekortBruktEgne  = tmptotManedsrap.GavekortBruktEgne   + tmpManedsrap.GavekortBruktEgne                                 */ */
/* /*                  tmptotManedsrap.GavekortBruktAndre = tmptotManedsrap.GavekortBruktAndre  + tmpManedsrap.GavekortBruktAndre                                */ */
/* /*                  tmptotManedsrap.TilgodeUt          = tmptotManedsrap.TilgodeUt           + tmpManedsrap.TilgodeUt                                         */ */
/* /*                  tmptotManedsrap.GavekortUt         = tmptotManedsrap.GavekortUt          + tmpManedsrap.GavekortUt                                        */ */
/* /*                  tmptotManedsrap.SumInnbutikk       = tmptotManedsrap.SumInnbutikk        + tmpManedsrap.SumInnbutikk                                      */ */
/* /*                  tmptotManedsrap.OmsetningEksKred   = tmptotManedsrap.OmsetningEksKred    + tmpManedsrap.OmsetningEksKred                                  */ */
/* /*                  tmptotManedsrap.DiffKasse          = tmptotManedsrap.DiffKasse           + tmpManedsrap.DiffKasse                                         */ */
/* /*                  tmptotManedsrap.Kreditsalg         = tmptotManedsrap.Kreditsalg          + tmpManedsrap.Kreditsalg                                        */ */
/* /*                  tmptotManedsrap.InnbetaltKunde     = tmptotManedsrap.InnbetaltKunde      + tmpManedsrap.InnbetaltKunde                                    */ */
/* /*                  .                                                                                                                                         */ */
/* /*                                                                                                                                                            */ */
/*                                                                                                                                                                  */
/*        END. /* BUTIKKER */                                                                                                                                       */
/*                                                                                                                                                                  */
/*      END. /* DATO */                                                                                                                                             */
   END.


END PROCEDURE.
