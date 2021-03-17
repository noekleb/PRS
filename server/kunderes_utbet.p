/* Registrering av utbetaling 
   Parametere:  ENTRY(1,"|"): <kunde>;kundenr
                ENTRY(2,"|"): <kreditid>;id innbetaling
                ENTRY(3,"|"): beløp (hvis ikke med: fullt betalt)
                ENTRY(4,"|"): Notat
   Opprettet: 27.06.05 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR ix           AS INT    NO-UNDO.
DEF VAR cIdKredit    AS CHAR   NO-UNDO.
DEF VAR hBuffer      AS HANDLE NO-UNDO.
DEF VAR fKundenr     AS DEC    NO-UNDO.
DEF VAR fDebetBelop  AS DEC    NO-UNDO.
DEF VAR cNotat       AS CHAR   NO-UNDO.
DEF VAR fUtbetNr     AS DEC    NO-UNDO.
DEF VAR iCL          AS DEC    NO-UNDO.

DEF BUFFER bKundereskontr FOR Kundereskontr.
DEF BUFFER bFakturaHode   FOR FakturaHode.

{syspara.i 5 1 1 iCL INT}

ASSIGN fKundenr     = DEC(ENTRY(1,icParam,"|"))
       cIdKredit    = ENTRY(2,icParam,"|")      
       fDebetBelop  = DEC(ENTRY(3,icParam,"|"))
       cNotat       = ENTRY(4,icParam,"|")
       .
       
IF CAN-FIND(FIRST Kunde WHERE Kunde.Kundenr = fKundenr) THEN DO:
  /* Poster kreditering: */
  DO TRANSACTION ON ERROR UNDO,LEAVE:
    FIND FIRST Kundereskontr
         WHERE Kundereskontr.Reskontro_id = DEC(cIdKredit)
         EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL Kundereskontr THEN DO:
      IF fDebetBelop * -1 < Kundereskontr.Saldo THEN
        ocReturn = "Kan ikke utbetale mer enn saldo for kreditpost".
      ELSE DO:
        RUN ProduserUtbetaling (OUTPUT obOk).
        IF obOk THEN DO:
          CREATE bKundereskontr.
          ASSIGN bKundereskontr.BArtNr        = 1  /* Varesalg */
                 bKundereskontr.Bilagstype    = 5
                 bKundereskontr.Belop         = fDebetBelop
                 bKundereskontr.Saldo         = 0
                 bKundereskontr.Kundenr       = fKundenr
                 bKundereskontr.FakturaNr     = fUtbetNr
                 bKundereskontr.Notat         = cNotat
                 bKundereskontr.ForfallsDato  = TODAY
                 bKundereskontr.FakturertDato = TODAY
                 .
          RUN Kryssing.
        END.
      END.
    END.
    ELSE 
      ocReturn = "Finner ikke / ikke tilgjengelig for oppdatering, faktura id " + ENTRY(ix,cIdKredit).
        
    IF ocReturn NE "" THEN UNDO, LEAVE.
  END.
  
  /* Hvis det nå er noe igjen av krediteringen så skal det lages en a konto innbetaling : */
/*   IF fDebetBelop > 0 THEN DO TRANSACTION:                   */
/*   END.                                                      */

  RUN beregn_kunde_saldo.p ("idlist|" + STRING(fKundenr),?,icSessionId,OUTPUT ocReturn,OUTPUT obOk).
END.
ELSE ocReturn = "Finner ikke kunde: " + STRING(fKundenr).

IF ocReturn = "" THEN obOk = TRUE.

PROCEDURE ProduserUtbetaling:
  DEF OUTPUT PARAM obOK AS LOG NO-UNDO INIT TRUE.

  DEF VAR ix         AS INT NO-UNDO.

  FIND Bilagstype NO-LOCK WHERE
      Bilagstype.Bilagstype = 5 NO-ERROR.
  IF NOT AVAIL Bilagstype THEN DO:
    ASSIGN ocReturn = "Bilagstype 5, utbetaling mangler i database".
           obOk     = FALSE.
    RETURN.
  END.


  FIND FIRST FakturaHode
       WHERE FakturaHode.FakturaNr = Kundereskontr.FakturaNr
         AND FakturaHode.FakturaNr NE ?
         AND FakturaHode.KundeNr   = Kundereskontr.KundeNr
         AND FakturaHode.BilagsTyp = 2
       NO-LOCK NO-ERROR.

  CREATE bFakturaHode.

  IF AVAIL FakturaHode THEN
    BUFFER-COPY FakturaHode EXCEPT Faktura_id TO bFakturaHode.
  ELSE DO:
    FIND FIRST Kunde NO-LOCK
         WHERE Kunde.Kundenr = fKundeNr 
         NO-ERROR.
    IF AVAIL Kunde THEN
      BUFFER-COPY Kunde TO bFakturaHode.
    ELSE DO:
      ASSIGN ocReturn = "Finner ikke kunde nr " + STRING(fKundeNr).
             obOk     = FALSE.
      RETURN.
    END.
  END.

  RUN getfakturanr.p (5,OUTPUT fUtbetNr,(IF AVAILABLE FakturaHode
                                           THEN FakturaHode.ButikkNr
                                         ELSE iCL)). 

  ASSIGN bFakturaHode.BilagsType    = 5
         bFakturaHode.Kundenr       = fKundenr
         bFakturaHode.FakturaNr     = fUtbetNr
         bFakturaHode.FakturertDato = TODAY
         bFakturaHode.Dato          = TODAY
         bFakturaHode.ForfallsDato  = TODAY
         bFakturaHode.FNotat        = cNotat
         bFakturaHode.BTTekst       = Bilagstype.BTTekst
         bFakturaHode.Totalrabatt%  = 0
         bFakturaHode.TotalrabattKr = 0
         bFakturaHode.MvaKr         = 0
         bFakturaHode.Mva           = 0
         bFakturaHode.NettoPris     = fDebetBelop
         bFakturaHode.TotaltVolum   = 0
         bFakturaHode.AvgFriSalg    = fDebetBelop
         bFakturaHode.AvgPlSalg     = 0
         bFakturaHode.AvrundingKr   = 0
         bFakturaHode.BetTekst      = ""
         bFakturaHode.Totalt        = fDebetBelop
         .

  CREATE FakturaLinje.
  ASSIGN FakturaLinje.Faktura_Id     = bFakturaHode.Faktura_id
         FakturaLinje.Varetekst      = "Utbetalt kredit" + (IF AVAIL FakturaHode THEN "nota " + STRING(FakturaHode.FakturaNr) ELSE "") 
         FakturaLinje.LeveringsDato  = IF AVAIL FakturaHode THEN FakturaHode.LeveringsDato ELSE ?
         FakturaLinje.NettoLinjeSum  = fDebetBelop
         FakturaLinje.LinjeSum       = fDebetBelop
         Fakturalinje.Nettopris      = fDebetBelop
         FakturaLinje.FakturaLinjeNr = 1
         FakturaLinje.Antall         = 1
         .

END PROCEDURE.

PROCEDURE Kryssing:
  ASSIGN Kundereskontr.Saldo  = Kundereskontr.Saldo + fDebetBelop
         .

  CREATE KundeResKobling.
  ASSIGN KundeResKobling.KReskontro_id = Kundereskontr.Reskontro_id
         KundeResKobling.DReskontro_id = bKundereskontr.Reskontro_id
         KundeResKobling.Dato          = bKundereskontr.ForfallsDato
         KundeResKobling.Belop         = fDebetBelop
         .
END PROCEDURE.
