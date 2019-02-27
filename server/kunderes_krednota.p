/* Registrering av kreditnota 
   Parametere:  ENTRY(1,"|"): <kunde>;kundenr
                ENTRY(2,"|"): <debetid>;id faktura
                ENTRY(3,"|"): beløp (hvis ikke med: fullt betalt)
                ENTRY(4,"|"): Notat
   Opprettet: 27.06.05 av BHa                  
   Endret:    15.03.06 av BHa
              - Dersom et purregebyr krediteres så produseres ikke kreditnota betalingsdokument
                Siden krysserutinen forholder seg til fakturanr legges spesifikk reskontroid ut for purregebyrer (alt annet en faktura)  
                her og krysserutinen sjekker om funksjone eksisterer og returnerer verdi før den henter posten som skal krediteres
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.
                       
DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR ix            AS INT    NO-UNDO.
DEF VAR cIdDebet      AS CHAR   NO-UNDO.
DEF VAR hBuffer       AS HANDLE NO-UNDO.
DEF VAR fKundenr      AS DEC    NO-UNDO.
DEF VAR fKreditBelop  AS DEC    NO-UNDO.
DEF VAR dInnbet       AS DATE   NO-UNDO.
DEF VAR cNotat        AS CHAR   NO-UNDO.
DEF VAR fKredNotaNr   AS DEC    NO-UNDO.
DEF VAR fReskontroId  AS DEC    NO-UNDO.

FUNCTION getReskontroId RETURNS DECIMAL():
  RETURN fReskontroId.
END FUNCTION.

DEF BUFFER bKundereskontr FOR Kundereskontr.
DEF BUFFER bFakturaHode   FOR FakturaHode.

ASSIGN fKundenr     = DEC(ENTRY(1,icParam,"|"))
       cIdDebet     = ENTRY(2,icParam,"|")      
       fKreditBelop = DEC(ENTRY(3,icParam,"|"))
       cNotat       = ENTRY(4,icParam,"|")
       .
       
IF CAN-FIND(FIRST Kunde WHERE Kunde.Kundenr = fKundenr) THEN DO:
  /* Poster kreditering: */
  DO TRANSACTION ON ERROR UNDO,LEAVE:
    FIND FIRST Kundereskontr
         WHERE Kundereskontr.Reskontro_id = DEC(cIdDebet)
         NO-LOCK NO-ERROR.
    IF AVAIL Kundereskontr THEN DO:
      IF fKreditBelop > Kundereskontr.Belop THEN
        ocReturn = "Kan ikke kreditere mer enn pålydende for faktura".
      ELSE DO:
        IF Kundereskontr.BilagsType = 1 THEN
          RUN ProduserKredNota (OUTPUT obOk).
        ELSE DO:
          obOk = YES.
          fReskontroId = Kundereskontr.Reskontro_Id.
        END.
        IF obOk THEN
          RUN poster_innbet.p (fKundeNr,Kundereskontr.FakturaNr,TODAY,2,cNotat,0,0,fKredNotaNr,0,INPUT-OUTPUT fKreditBelop, OUTPUT ocReturn).
      END.
    END.
    ELSE 
      ocReturn = "Finner ikke faktura id " + ENTRY(ix,cIdDebet).
        
    IF ocReturn NE "" THEN UNDO, LEAVE.
  END.
  
  /* Hvis det nå er noe igjen av krediteringen så skal det lages en a konto innbetaling : */
  IF fKreditBelop > 0 THEN DO TRANSACTION:
    CREATE bKundereskontr.
    ASSIGN bKundereskontr.BArtNr        = 1  /* Varesalg */
           bKundereskontr.Bilagstype    = 2  /* Kreditnota */
           bKundereskontr.Belop         = fKreditBelop * -1
           bKundereskontr.Saldo         = fKreditBelop * -1
           bKundereskontr.Kundenr       = fKundenr
           bKundereskontr.FakturaNr     = fKredNotaNr
           bKundereskontr.Notat         = cNotat
           bKundereskontr.ForfallsDato  = TODAY
           bKundereskontr.FakturertDato = TODAY
           .
  END.

  RUN beregn_kunde_saldo.p ("idlist|" + STRING(fKundenr),?,icSessionId,OUTPUT ocReturn,OUTPUT obOk).
END.
ELSE ocReturn = "Finner ikke kunde: " + STRING(fKundenr).

IF ocReturn = "" THEN 
    ASSIGN
        ocReturn = STRING(fKredNotaNr) 
        obOk     = TRUE
        .



PROCEDURE ProduserKredNota:
  DEF OUTPUT PARAM obOK        AS LOG NO-UNDO INIT TRUE.

  DEF VAR ix            AS INT NO-UNDO.

  FIND FIRST FakturaHode
       WHERE FakturaHode.FakturaNr = Kundereskontr.FakturaNr
         AND FakturaHode.KundeNr   = Kundereskontr.KundeNr
         AND FakturaHode.BilagsTyp = 1
       NO-LOCK NO-ERROR.

  IF NOT AVAIL FakturaHode THEN DO:
    ASSIGN ocReturn = "Finner ikke faktura " + STRING(Kundereskontr.FakturaNr) + " for kunde " + STRING(Kundereskontr.KundeNr) + " (!!)".
           obOk     = FALSE.
    RETURN.
  END.

  FIND FIRST Butiker WHERE Butiker.butik = FakturaHode.ButikkNr NO-LOCK NO-ERROR.
  IF NOT AVAIL Butiker THEN DO:
    ASSIGN ocReturn = "Faktura " + STRING(Faktura_id) + " mangler butikkangivelse".
           obOk     = FALSE.
    RETURN.
  END.

  FIND Bilagstype NO-LOCK WHERE
      Bilagstype.Bilagstype = 2 NO-ERROR.
  IF NOT AVAIL Bilagstype THEN DO:
    ASSIGN ocReturn = "Bilagstype 2, kreditnota mangler i database".
           obOk     = FALSE.
    RETURN.
  END.

  RUN getfakturanr.p (2,OUTPUT fKredNotaNr,FakturaHode.ButikkNr). 

  CREATE bFakturaHode.
  BUFFER-COPY FakturaHode EXCEPT Faktura_id FakturaNr Totalt TO bFakturaHode.
  ASSIGN bFakturaHode.BilagsType    = 2
         bFakturaHode.FakturaNr     = fKredNotaNr
         bFakturaHode.FakturertDato = TODAY
         bFakturaHode.ForfallsDato  = TODAY
         bFakturaHode.FNotat        = cNotat
         bFakturaHode.BTTekst       = Bilagstype.BTTekst
         bFakturaHode.Totalrabatt%  = 0
         bFakturaHode.TotalrabattKr = 0
         bFakturaHode.MvaKr         = 0
         bFakturaHode.Mva           = 0
         bFakturaHode.NettoPris     = 0
         bFakturaHode.TotaltVolum   = 0
         bFakturaHode.AvgFriSalg    = 0
         bFakturaHode.AvgPlSalg     = 0
         bFakturaHode.AvrundingKr   = 0
         bFakturaHode.BetTekst      = ""
         bFakturaHode.Totalt        = fKreditBelop * -1
         .

  CREATE FakturaLinje.
  ASSIGN FakturaLinje.Faktura_Id     = bFakturaHode.Faktura_id
         FakturaLinje.Varetekst      = "Kredit faktura " + STRING(FakturaHode.FakturaNr) 
         FakturaLinje.EkstRefTekst   = "Oppr.forfallsdato: " + STRING(FakturaHode.ForfallsDato)
                                     + ". Oppr.beløp: " + STRING(FakturaHode.Totalt) + ", herav MVA: " + STRING(FakturaHode.MvaKr) 
         FakturaLinje.LeveringsDato  = FakturaHode.LeveringsDato
         FakturaLinje.NettoLinjeSum  = fKreditBelop * -1
         FakturaLinje.LinjeSum       = fKreditBelop * -1
         FakturaLinje.FakturaLinjeNr = 1
         .

END PROCEDURE.
