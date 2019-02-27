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

DEF BUFFER bKundereskontr  FOR Kundereskontr.
DEF BUFFER bbKundereskontr FOR Kundereskontr.
DEF BUFFER bFakturaHode    FOR FakturaHode.

ASSIGN fKundenr     = DEC(ENTRY(1,icParam,"|"))
       cIdKredit    = ENTRY(2,icParam,"|")      
       fDebetBelop  = DEC(ENTRY(3,icParam,"|"))
       cNotat       = ENTRY(4,icParam,"|")
       .
       
IF CAN-FIND(FIRST Kunde WHERE Kunde.Kundenr = fKundenr) THEN DO:
  /* Poster debitering: */
  DO TRANSACTION ON ERROR UNDO,LEAVE:
    FIND FIRST Kundereskontr
         WHERE Kundereskontr.Reskontro_id = DEC(cIdKredit)
         EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL Kundereskontr THEN DO:
      IF fDebetBelop * -1 < Kundereskontr.Belop THEN
        ocReturn = "Kan ikke korrigere for mer enn pålydende for kreditpost".
      ELSE DO:
        Kundereskontr.Saldo = 0.

        CREATE bKundereskontr.
        BUFFER-COPY Kundereskontr EXCEPT Reskontro_id TO bKundereskontr.
        ASSIGN bKundereskontr.Bilagstype    = 6
               bKundereskontr.Belop         = fDebetBelop
               bKundereskontr.Saldo         = 0
               bKundereskontr.Notat         = "Reversering av kreditpost"
               bKundereskontr.ForfallsDato  = TODAY
               bKundereskontr.FakturertDato = TODAY
               .

        /* Fjern kryssinger for kreditpost og øk saldo på berørte debetposter: */
        FOR EACH KundeResKobling EXCLUSIVE-LOCK
            WHERE KundeResKobling.KReskontro_id = Kundereskontr.Reskontro_id
           ,FIRST bbKundereskontr 
                  WHERE bbKundereskontr.Reskontro_id = KundeResKobling.DReskontro_id:
          ASSIGN bbKundereskontr.Saldo  = bbKundereskontr.Saldo + MIN(KundeResKobling.Belop,fDebetBelop)
                 KundeResKobling.Belop = KundeResKobling.Belop - MIN(KundeResKobling.Belop,fDebetBelop)
                 fDebetBelop = fDebetBelop - MIN(bbKundereskontr.Belop,fDebetBelop)
                 .
        END.

        /* Kryss korrigering mot kreditpost: */
        RUN Kryssing.
      END.
    END.
    ELSE 
      ocReturn = "Finner ikke / ikke tilgjengelig for oppdatering, faktura id " + ENTRY(ix,cIdKredit).
        
    IF ocReturn NE "" THEN UNDO, LEAVE.
  END.
  
  RUN beregn_kunde_saldo.p ("idlist|" + STRING(fKundenr),?,icSessionId,OUTPUT ocReturn,OUTPUT obOk).
END.
ELSE ocReturn = "Finner ikke kunde: " + STRING(fKundenr).

IF ocReturn = "" THEN obOk = TRUE.

PROCEDURE Kryssing:
  CREATE KundeResKobling.
  ASSIGN KundeResKobling.KReskontro_id = Kundereskontr.Reskontro_id
         KundeResKobling.DReskontro_id = bKundereskontr.Reskontro_id
         KundeResKobling.Dato          = bKundereskontr.ForfallsDato
         KundeResKobling.Belop         = fDebetBelop
         .
END PROCEDURE.
