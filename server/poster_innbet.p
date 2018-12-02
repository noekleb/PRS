/* Postering av innbetaling/kreditering
   Parametere:  kundenr
                fakturanr
                dato, kredit
                bilagstype
                beløp (INPUT-OUTPUT)
                returmelding
   Opprettet: 03.06.05 av BHa        
   Endret:    22.03.06 av BHa
              - Sjekker mot kallende program om det er angitt en spesifikk debetpost (reskontroid)        
------------------------------------------------------------------------------*/
DEF INPUT PARAM ifKundenr             AS DEC  NO-UNDO.
DEF INPUT PARAM ifFakturaNr           AS DEC  NO-UNDO.
DEF INPUT PARAM idBetDato             AS DATE NO-UNDO.
DEF INPUT PARAM iiBilagsType          AS INT  NO-UNDO.
DEF INPUT PARAM icNotat               AS CHAR NO-UNDO.
DEF INPUT PARAM ifBongId              AS DEC  NO-UNDO.
DEF INPUT PARAM iiBongLinje           AS INT  NO-UNDO.
DEF INPUT PARAM ifKredNotaNr          AS DEC  NO-UNDO.
DEF INPUT PARAM lKID                  AS DEC  NO-UNDO.
DEF INPUT-OUTPUT PARAM iofKreditBelop AS DEC  NO-UNDO.
DEF OUTPUT PARAM ocReturn             AS CHAR NO-UNDO.

DEF VAR fReskontroId AS DEC NO-UNDO.

DEF BUFFER bKundereskontr FOR Kundereskontr.

IF CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"getReskontroId") THEN
  fReskontroId = DYNAMIC-FUNCTION("getReskontroId" IN SOURCE-PROCEDURE).

IF fReskontroId NE 0 THEN 
  FOR EACH Kundereskontr EXCLUSIVE-LOCK
      WHERE Kundereskontr.Reskontro_id = fReskontroId:
    RUN Kryssing.  
  END.
ELSE IF ifFakturaNr NE 0 AND ifFakturaNr NE ? THEN
DO: 
  FOR EACH Kundereskontr EXCLUSIVE-LOCK
      WHERE Kundereskontr.Kundenr    = ifKundenr
        AND Kundereskontr.FakturaNr  = ifFakturaNr
        AND Kundereskontr.Saldo      > 0
         BY Kundereskontr.ForfallsDato:
    RUN Kryssing.  
  END.
END.
ELSE DO:
  FOR EACH Kundereskontr EXCLUSIVE-LOCK
      WHERE Kundereskontr.Kundenr    = ifKundenr
        AND Kundereskontr.Saldo      > 0
        AND Kundereskontr.Bilagstype NE 11 /* Tar hovedstoler før purregebyrer */
         BY Kundereskontr.ForfallsDato:
    RUN Kryssing.  
  END.
  FOR EACH Kundereskontr EXCLUSIVE-LOCK
      WHERE Kundereskontr.Kundenr    = ifKundenr
        AND Kundereskontr.Saldo      > 0
         BY Kundereskontr.ForfallsDato:
    RUN Kryssing.  
  END.
END.

PROCEDURE Kryssing:
  DEF VAR fMaksKredit AS DEC NO-UNDO.

  fMaksKredit = MIN(iofKreditBelop,Kundereskontr.Saldo).
  IF fMaksKredit = 0 THEN RETURN.

  ASSIGN Kundereskontr.Saldo = Kundereskontr.Saldo - fMaksKredit
         iofKreditBelop      = iofKreditBelop - fMaksKredit
         .

  CREATE bKundereskontr.
  BUFFER-COPY Kundereskontr EXCEPT Reskontro_id TO bKundereskontr.
  ASSIGN bKundereskontr.Belop         = fMaksKredit * -1
         bKundereskontr.Saldo         = 0
         bKundereskontr.Bilagstype    = iiBilagsType
         bKundereskontr.FakturertDato = idBetDato
         bKundereskontr.ForfallsDato  = idBetDato
         bKundereskontr.Notat         = icNotat
         bKundereskontr.B_Id          = ifBongId
         bKundereskontr.BongLinjeNr   = iiBongLinje
         bKundeREskontr.KID           = lKID
         .
  IF ifKredNotaNr NE 0 THEN
    bKundereskontr.FakturaNr = ifKredNotaNr.

  CREATE KundeResKobling.
  ASSIGN KundeResKobling.DReskontro_id = Kundereskontr.Reskontro_id
         KundeResKobling.KReskontro_id = bKundereskontr.Reskontro_id
         KundeResKobling.Dato          = idBetDato
         KundeResKobling.Belop         = fMaksKredit
         .
END PROCEDURE.
