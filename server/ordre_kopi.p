/* Kopiering av ordre. Betingelse for kopiering er at ordre henger mot en varehåndteringsbok
   Parametere: ordrenr(kilde);lev.dato;brukerid
      
   Opprettet: 22.11.05 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR iKildeOrdre  AS INT    NO-UNDO.
DEF VAR cUserId      AS CHAR   NO-UNDO.
DEF VAR dLevDato     AS DATE   NO-UNDO.
DEF VAR ix           AS INT    NO-UNDO.
DEF VAR iSuppType    AS INT    NO-UNDO.
DEF VAR iLevUke      AS INT    NO-UNDO.
DEF VAR iMinBestStat AS INT    NO-UNDO.
DEF VAR hVareBehBestHode AS HANDLE NO-UNDO.
{syspara.i 5 5 2 iSuppType INT}

ASSIGN iKildeOrdre  = INT(ENTRY(1,icParam,";"))
       dLevDato     = DATE(ENTRY(2,icParam,";"))
       cUserId      = ENTRY(3,icParam,";")
       .

RUN weeknum.p (dLevDato,OUTPUT iLevUke).

DEF BUFFER bOrdre     FOR Ordre.
DEF BUFFER bBestHode  FOR BestHode.
DEF BUFFER bBestLinje FOR BestLinje.
DEF BUFFER bBestSort  FOR BestSort.
DEF BUFFER bBestKasse FOR BestKasse.
DEF BUFFER bBestStr   FOR BestStr.
DEF BUFFER bBestPris  FOR BestPris.
DEF BUFFER bFriButik  FOR FriButik.
DEF BUFFER bVarebehBestHode FOR VarebehBestHode.
hVareBehBestHode = BUFFER VarebehBestHode:HANDLE.

FIND bOrdre NO-LOCK
     WHERE bOrdre.Ordrenr = iKildeOrdre 
     NO-ERROR.
IF NOT AVAIL bOrdre THEN DO:
  ocReturn = "Finner ikke ordre nr " + ENTRY(1,icParam,";").
  RETURN.
END.
FIND FIRST VarebehHode NO-LOCK
     WHERE VarebehHode.VarebehNr = bOrdre.VarebehNr
     NO-ERROR.
IF NOT AVAIL VarebehHode THEN DO:
  ocReturn = "Ordre må være knyttet til varehåndteringbok for å kunne kopieres".
  RETURN.
END.

TRANSBLOKK:
DO TRANSACTION ON ERROR UNDO, LEAVE:
  CREATE Ordre.
  BUFFER-COPY bOrdre 
         EXCEPT Ordrenr 
                BekreftetAv 
                BekreftetDato 
                BekreftetOrdre 
                EDato 
                EkstId 
                ETid 
                fraERP 
                OrdreStatus 
                SendtDato
         TO Ordre.
  Ordre.LeveringsDato = dLevDato.

  FOR EACH bBestHode OF bOrdre NO-LOCK:

    FIND ArtBas 
         WHERE ArtBas.ArtikkelNr = bBestHode.ArtikkelNr 
         NO-LOCK NO-ERROR.
    IF NOT AVAIL ArtBas THEN DO:
      ocReturn = "Finner ikke artikkel " + STRING(bBestHode.artikkelnr) + " angitt på bestilling " + STRING(bBestHode.BestNr).
      UNDO, LEAVE TRANSBLOKK.
    END.

    FIND FIRST ArtPris OF ArtBas
         WHERE ArtPris.ProfilNr = VarebehHode.ProfilNr
         NO-LOCK NO-ERROR.
    IF NOT AVAIL ArtPris THEN DO:
      ocReturn = "Finner ikke pris for artikkel " + STRING(ArtBas.artikkelnr) + " angitt på bestilling " + STRING(bBestHode.BestNr).
      UNDO, LEAVE TRANSBLOKK.
    END.

    CREATE BestHode.
    BUFFER-COPY bBestHode
           EXCEPT BekreftetAv 
                  BekreftetDato 
                  BekreftetOrdre 
                  BestNr 
                  BestStat 
                  BestType 
                  BrukerID 
                  EDato 
                  EkstId 
                  EkstOrdreNr 
                  ETid 
                  fraERP 
                  OrdreNr 
                  RegistrertAv 
                  RegistrertDato 
                  RegistrertTid 
                  SendtAv 
                  SendtDato 
                  SendtTid 
                  TeamNr 
                  TotAntPar 
                  TotDbKr 
                  TotInnkjVerdi 
                  TotInnLev 
                  TotMakulert 
                  TotOverLev 
                  TotSalgsVerdi
           TO BestHode.
    ASSIGN BestHode.BestType        = iSuppType
           BestHode.BestillingsDato = TODAY
           BestHode.BestStat        = 3
           BestHode.LevDato         = dLevDato
           BestHode.LevTid          = STRING(iLevUke)
           BestHode.OrdreNr         = Ordre.Ordrenr
           .

    FIND FIRST bVarebehBestHode NO-LOCK
         WHERE bVarebehBestHode.VarebehNr = VarebehHode.VarebehNr
           AND bVarebehBestHode.BestNr    = bBestHode.BestNr
         NO-ERROR.
    IF NOT AVAIL bVarebehBestHode THEN DO:
      ocReturn = "Feil i sammenheng mellom bestilling og vareh.bok for bestilling " + STRING(bBestHode.BestNr).
      UNDO, LEAVE TRANSBLOKK.
    END.
    CREATE VarebehBestHode.
    BUFFER-COPY bVarebehBestHode 
           EXCEPT BestNr 
                  BrukerID 
                  EDato 
                  ETid 
                  godkjent 
                  HodeLinjeId 
                  LevDato 
                  Levuke 
                  OrdreNr 
                  RegistrertAv 
                  RegistrertDato 
                  RegistrertTid 
                  VerdiLevert 
                  AntLevert
           TO VarebehBestHode.
    RUN create_varebehbesthode.p (hVarebehBestHode,
                                  "VarebehNr,CLButikkNr",
                                  STRING(VarebehHode.VarebehNr) + "|" + STRING(bVarebehBestHode.CLButikkNr),
                                  icSessionId,
                                  OUTPUT ocReturn).
    IF ocReturn NE "" THEN UNDO, LEAVE TRANSBLOKK.
    ASSIGN VarebehBestHode.BestNr   = BestHode.BestNr
           VarebehBestHode.BrukerId = cUserId
           VarebehBestHode.OrdreNr  = Ordre.OrdreNr
           VarebehBestHode.LevUke   = iLevUke
           VarebehBestHode.LevDato  = dLevDato
           .

    CREATE BestPris.
    ASSIGN BestPris.ArtikkelNr    = BestHode.ArtikkelNr
           BestPris.BestNr        = BestHode.BestNr
           BestPris.BestStat      = BestHode.BestStat
           BestPris.Pris          = ArtPris.Pris[1]         
           BestPris.MvaKr         = ArtPris.MvaKr[1]        
           BestPris.Mva%          = ArtPris.Mva%[1]         
           BestPris.VareKost      = ArtPris.VareKost[1]     
           BestPris.InnkjopsPris  = ArtPris.InnkjopsPris[1] 
           BestPris.ValPris       = ArtPris.ValPris[1]      
           BestPris.Frakt         = ArtPris.Frakt[1]        
           BestPris.Frakt%        = ArtPris.Frakt%[1]       
           BestPris.Rab1Kr        = ArtPris.Rab1Kr[1]       
           BestPris.Rab1%         = ArtPris.Rab1%[1]        
           BestPris.Rab2Kr        = ArtPris.Rab2Kr[1]       
           BestPris.Rab2%         = ArtPris.Rab2%[1]        
           BestPris.Rab3Kr        = ArtPris.Rab3Kr[1]       
           BestPris.Rab3%         = ArtPris.Rab3%[1]        
           BestPris.DB%           = ArtPris.DB%[1]          
           BestPris.DBKr          = ArtPris.DBKr[1]         
           BestPris.DivKost%      = ArtPris.DivKost%[1]     
           BestPris.DivKostKr     = ArtPris.DivKostKr[1]    
           BestPris.ProfilNr      = ArtPris.ProfilNr        
           .

    FOR EACH bBestLinje OF bBestHode NO-LOCK:
      CREATE BestLinje.
      BUFFER-COPY bBestLinje EXCEPT BestNr TO BestLinje.
      BestLinje.BestNr = BestHode.BestNr.
    END.
    FOR EACH bBestSort OF bBestHode NO-LOCK:
      CREATE BestSort.
      BUFFER-COPY bBestSort EXCEPT BestNr TO BestSort.
      BestSort.BestNr = BestHode.BestNr.
    END.
    FOR EACH bBestKasse OF bBestHode NO-LOCK:
      CREATE BestKasse.
      BUFFER-COPY bBestKasse EXCEPT BestNr TO BestKasse.
      BestKasse.BestNr = BestHode.BestNr.
    END.
    FOR EACH bFriButik OF bBestHode NO-LOCK:
      CREATE FriButik.
      BUFFER-COPY bFriButik EXCEPT BestNr TO FriButik.
      FriButik.BestNr = BestHode.BestNr.
    END.
    FOR EACH bBestStr OF bBestHode NO-LOCK
        BREAK BY Butik BY Storl BY BestStat:
      IF FIRST-OF(BestStat) AND iMinBestStat = 0 OR bBestStr.BestStat = iMinBestStat THEN DO:
        CREATE BestStr.
        BUFFER-COPY bBestStr EXCEPT BestNr TO BestStr.
        ASSIGN BestStr.BestNr = BestHode.BestNr

               BestHode.TotAntPar      = BestHode.TotAntPar     + BestStr.Bestilt
               BestHode.TotDbKr        = BestHode.TotDbKr       + BestStr.Bestilt * BestPris.DBKr
               BestHode.TotInnkjVerdi  = BestHode.TotInnkjVerdi + BestStr.Bestilt * BestPris.Varekost 
               BestHode.TotSalgsVerdi  = BestHode.TotSalgsVerdi + BestStr.Bestilt * BestPris.Pris

               iMinBestStat            = bBestStr.BestStat
               .
      END.
    END.
  END.
END.


IF ocReturn = "" THEN 
  obOk = TRUE.

