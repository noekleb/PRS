/* Opprette kundeordre
   Dersom feltet source_KOrdre_id har verdi betyr det at ordrelinjer skal kopieres fra denne
   
   Created: 06.10.05 by Brynjar Hasle       
   Endret:  22.04.13 by Brynjar
          - Sjekker om linjer skal rekalkuleres pga endring i mva kode for kunde  
            Hvis kunde er endret slik at mva kode er endret eller ordre kopieres så skal linjene kalkuleres       
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM hBuffer     AS HANDLE NO-UNDO.
DEF INPUT  PARAM icAction    AS CHAR   NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO. 


DEF VAR icFields         AS CHAR   NO-UNDO.
DEF VAR icValues         AS CHAR   NO-UNDO.
DEF VAR fSourceKOrdre_id AS DEC    NO-UNDO.
DEF VAR ocBestPris       AS CHAR   NO-UNDO.
DEF VAR fOldTotalRabatt% AS DEC    NO-UNDO.
DEF VAR hBufKOrdreLinje  AS HANDLE NO-UNDO.
DEF VAR iBetBet          AS INT    NO-UNDO.
DEF VAR bRecalcLinjer    AS LOG    NO-UNDO.

DEF BUFFER b1Post FOR Post.

ASSIGN icFields = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE)
       icValues = DYNAMIC-FUNCTION("getCurrentValues" IN SOURCE-PROCEDURE)
       hBufKOrdreLinje = BUFFER KOrdreLinje:HANDLE
       .

ASSIGN fOldTotalRabatt% = DEC(ENTRY(LOOKUP("OldTotalRabatt%",icFields),icValues,"|"))
       iBetBet          = INT(ENTRY(LOOKUP("BetBet",icFields),icValues,"|"))
       NO-ERROR.

FIND FIRST Betalingsbetingelser NO-LOCK
     WHERE Betalingsbetingelser.BetBet = iBetBet
     NO-ERROR.
IF AVAIL BetalingsBetingelser THEN
  hBuffer:BUFFER-FIELD("BetTekst"):BUFFER-VALUE = Betalingsbetingelser.BetTekst.

IF icAction = "create" THEN DO:
  
  DEF BUFFER bKOrdreLinje FOR KOrdreLinje.
  
  fSourceKOrdre_id = DEC(ENTRY(LOOKUP("source_KOrdre_id",icFields),icValues,"|")) NO-ERROR.
  
  IF CAN-FIND(KOrdreHode WHERE KOrdreHode.KOrdre_id = fSourceKOrdre_id) THEN DO:
    bRecalcLinjer = YES.
    FOR EACH KOrdreLinje NO-LOCK
        WHERE KOrdreLinje.KOrdre_id = fSourceKOrdre_id
        :
      CREATE bKOrdreLinje.
      BUFFER-COPY KOrdreLinje EXCEPT KOrdre_id Leveringsdato TO bKOrdreLinje.
      bKOrdreLinje.KOrdre_id = hBuffer:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE.
    END.
  
    IF hBuffer:BUFFER-FIELD("Verkstedordre"):BUFFER-VALUE THEN
      hBuffer:BUFFER-FIELD("ProdStatus"):BUFFER-VALUE = "1".

    ASSIGN hBuffer:BUFFER-FIELD("LevStatus"):BUFFER-VALUE       = 10
           hBuffer:BUFFER-FIELD("Utsendelsesdato"):BUFFER-VALUE = ?
           hBuffer:BUFFER-FIELD("FakturertDato"):BUFFER-VALUE   = ?
           .

  END.

  hBuffer:BUFFER-FIELD("LevStatus"):BUFFER-VALUE = "10".

  FIND Butiker WHERE
      Butiker.Butik = INT(ENTRY(LOOKUP("ButikkNr",icFields),icValues,"|")) EXCLUSIVE NO-ERROR.
  IF AVAILABLE Butiker THEN DO:
      FIND CURRENT Butiker NO-LOCK.
      FIND b1Post NO-LOCK WHERE
          b1Post.PostNr = Butiker.BuPonr NO-ERROR.
  END.
  IF AVAILABLE Butiker THEN
      ASSIGN
      hBuffer:BUFFER-FIELD("FirmaNavn"):BUFFER-VALUE            = Butiker.ButNamn
      hBuffer:BUFFER-FIELD("FirmaAdresse1"):BUFFER-VALUE        = Butiker.BuAdr
      hBuffer:BUFFER-FIELD("FirmaAdresse2"):BUFFER-VALUE        = ""
      hBuffer:BUFFER-FIELD("FirmaPostNr"):BUFFER-VALUE          = Butiker.BuPonr
      hBuffer:BUFFER-FIELD("FirmaPostSted"):BUFFER-VALUE        = (IF AVAILABLE b1Post
                                                                     THEN b1Post.Beskrivelse
                                                                   ELSE "")
      hBuffer:BUFFER-FIELD("VaarRef"):BUFFER-VALUE              = (IF (Butiker.VaarRef <> "" AND hBuffer:BUFFER-FIELD("VaarRef"):BUFFER-VALUE = "")
                                                                     THEN Butiker.VaarRef
                                                                   ELSE Butiker.BuKon)
      hBuffer:BUFFER-FIELD("FirmaTelefon"):BUFFER-VALUE         = Butiker.BuTel
      hBuffer:BUFFER-FIELD("FirmaOrganisasjonsNr"):BUFFER-VALUE = Butiker.OrganisasjonsNr
      hBuffer:BUFFER-FIELD("FirmaBankKonto"):BUFFER-VALUE       = Butiker.BankKonto
      hBuffer:BUFFER-FIELD("FirmaPostgiro"):BUFFER-VALUE        = Butiker.PostGiro  
      hBuffer:BUFFER-FIELD("FirmaURLAdresse"):BUFFER-VALUE      = Butiker.URLAdresse
      hBuffer:BUFFER-FIELD("FirmaEPost"):BUFFER-VALUE           = Butiker.ePostAdresse
      hBuffer:BUFFER-FIELD("FirmaLand"):BUFFER-VALUE            = Butiker.ButLand
      hBuffer:BUFFER-FIELD("FirmaTelefaks"):BUFFER-VALUE        = Butiker.Telefaks
      .
END.

ELSE bRecalcLinjer = hBuffer:BUFFER-FIELD("TotalRabatt%"):BUFFER-VALUE NE fOldTotalRabatt% OR DYNAMIC-FUNCTION("getContext" IN SOURCE-PROCEDURE) = "recalc_linjer".
  
IF bRecalcLinjer THEN
  FOR EACH KOrdreLinje 
      WHERE KOrdreLinje.KOrdre_id = DEC(hBuffer:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE)
      :
    
/*     RUN kordrelinje_bestpris.p (ROWID(KOrdrelinje),icSessionId,OUTPUT ocBestPris).  */

    ASSIGN KOrdreLinje.NettoPris     = KOrdreLinje.BruttoPris - KOrdreLinje.BruttoPris * hBuffer:BUFFER-FIELD("TotalRabatt%"):BUFFER-VALUE / 100
           KOrdreLinje.NettoPris     = KOrdreLinje.NettoPris  - KOrdreLinje.NettoPris * KOrdreLinje.LinjeRab% / 100
           .

    RUN kordrelinje_post_update.p (hBufKOrdreLinje,"",icSessionId,OUTPUT ocReturn).
  END.

