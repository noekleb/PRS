/* Registrering av innbetaling 
   Parametere:  ENTRY(1,"|"): <kunde>;kundenr
                ENTRY(2,"|"): <debetidlist>;komma-separert list over debetposter
                ENTRY(3,"|"): <kreditid>;evt.kredipost
                ENTRY(4,"|"): innbetalt dato
                ENTRY(5,"|"): beløp (hvis ikke med: fullt betalt)
                ENTRY(6,"|"): Notat
                ENTRY(7,"|"): B_id (bong id)
                ENTRY(8,"|"): Bonglinje 
   Opprettet: 01.06.05 av BHa                  
   Endre:     14.09.05 av BHa
              - Rest kreditpost opprettes bare hvis det ikke fantes en eksisterende
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR ix           AS INT    NO-UNDO.
DEF VAR cIdListDebet AS CHAR   NO-UNDO.
DEF VAR hBuffer      AS HANDLE NO-UNDO.
DEF VAR fKundenr     AS DEC    NO-UNDO.
DEF VAR fKreditId    AS DEC    NO-UNDO.
DEF VAR fInnbetBelop AS DEC    NO-UNDO.
DEF VAR dInnbet      AS DATE   NO-UNDO.
DEF VAR cNotat       AS CHAR   NO-UNDO.
DEF VAR fBongId      AS DEC    NO-UNDO.
DEF VAR iBongLinje   AS INT    NO-UNDO.
DEF VAR fiKID        AS DEC    NO-UNDO.

DEF BUFFER bKundereskontr FOR Kundereskontr.


ASSIGN fKundenr     = DEC(ENTRY(1,icParam,"|"))
       cIdListDebet = ENTRY(2,icParam,"|")      
       fKreditId    = DEC(ENTRY(3,icParam,"|"))
       dInnbet      = DATE(ENTRY(4,icParam,"|"))
       fInnbetBelop = DEC(ENTRY(5,icParam,"|"))
       cNotat       = ENTRY(6,icParam,"|")
       fiKID        = DEC(ENTRY(7,icParam,"|"))
       fBongId      = DEC(ENTRY(8,icParam,"|"))
       iBongLinje   = INT(ENTRY(9,icParam,"|"))
       .
       
IF CAN-FIND(FIRST Kunde WHERE Kunde.Kundenr = fKundenr) THEN DO:
  /* Poster ny innbetaling: */
  IF fKreditId = 0 THEN DO TRANSACTION ON ERROR UNDO,LEAVE:
    IF NUM-ENTRIES(cIdListDebet) > 0 THEN DO ix = 1 TO NUM-ENTRIES(cIdListDebet):
      FIND FIRST Kundereskontr
           WHERE Kundereskontr.Reskontro_id = DEC(ENTRY(ix,cIdListDebet))
           NO-LOCK NO-ERROR.
      IF AVAIL Kundereskontr THEN 
        RUN poster_innbet.p (fKundeNr,Kundereskontr.FakturaNr,dInnbet,3,cNotat,fBongId,iBongLinje,0,fiKID, INPUT-OUTPUT fInnbetBelop, OUTPUT ocReturn).
      ELSE 
        ocReturn = "Finner ikke faktura id " + ENTRY(ix,cIdListDebet).
        
      IF ocReturn NE "" THEN UNDO, LEAVE.
    END.
    ELSE
      RUN poster_innbet.p (fKundeNr,0,dInnbet,3,cNotat,fBongId,iBongLinje,0,fiKID,INPUT-OUTPUT fInnbetBelop, OUTPUT ocReturn).
    IF ocReturn NE "" THEN UNDO, LEAVE.
  END.
  /* Poster mot eksisterende (a konto) innbetaling: */
  ELSE DO ON ERROR UNDO,LEAVE: 
    FIND FIRST bKundereskontr
         WHERE bKundereskontr.Reskontro_id = fKreditId
         EXCLUSIVE-LOCK NO-ERROR.
  
    IF AVAIL bKundereskontr THEN DO:
      IF NUM-ENTRIES(cIdListDebet) > 0 THEN DO ix = 1 TO NUM-ENTRIES(cIdListDebet):
        FIND FIRST Kundereskontr
             WHERE Kundereskontr.Reskontro_id = DEC(ENTRY(ix,cIdListDebet))
             EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL Kundereskontr THEN RUN Kryssing.
        ELSE ocReturn = "Faktura id ikke tilgjengelig for oppdatering: " + ENTRY(ix,cIdListDebet).
        IF ocReturn NE "" THEN UNDO, LEAVE.
      END.
    END.
  END.
  
  /* Hvis kreditposten ikke eksistere fra før og det nå er noe igjen av innbetalingen så lages en a konto innbetaling: */
  IF fInnbetBelop > 0 AND fKreditId = 0 THEN DO TRANSACTION:
    CREATE bKundereskontr.
    ASSIGN bKundereskontr.BArtNr        = 1  /* Varesalg */
           bKundereskontr.Bilagstype    = 3
           bKundereskontr.Belop         = fInnbetBelop * -1
           bKundereskontr.Saldo         = fInnbetBelop * -1
           bKundereskontr.Kundenr       = fKundenr
           bKundereskontr.ForfallsDato  = TODAY
           bKundereskontr.FakturertDato = TODAY
           bKundereskontr.Notat         = cNotat
           bKundereskontr.B_Id          = fBongId
           bKundereskontr.BongLinjeNr   = iBongLinje
           bKundereskontr.KID           = fiKID
           .
  END.

  RUN beregn_kunde_saldo.p ("idlist|" + STRING(fKundenr),?,icSessionId,OUTPUT ocReturn,OUTPUT obOk).
END.
ELSE ocReturn = "Finner ikke kunde: " + STRING(fKundenr).


IF ocReturn = "" THEN obOk = TRUE.


PROCEDURE Kryssing:
  DEF VAR fMaksKredit AS DEC NO-UNDO.

  fMaksKredit = MIN(fInnbetBelop,Kundereskontr.Saldo).
  IF fMaksKredit = 0 THEN RETURN.

  ASSIGN Kundereskontr.Saldo  = Kundereskontr.Saldo - fMaksKredit
         fInnbetBelop         = fInnbetBelop - fMaksKredit
         bKundereskontr.Saldo = bKundereskontr.Saldo + fMaksKredit
         .

  CREATE KundeResKobling.
  ASSIGN KundeResKobling.DReskontro_id = Kundereskontr.Reskontro_id
         KundeResKobling.KReskontro_id = bKundereskontr.Reskontro_id
         KundeResKobling.Dato          = bKundereskontr.ForfallsDato
         KundeResKobling.Belop         = fMaksKredit
         .
END PROCEDURE.
