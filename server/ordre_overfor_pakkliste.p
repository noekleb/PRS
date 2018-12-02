/* Overfør data fra ordre til pakkliste 
   Parametere: 
               - liste over ordrenr
   
   Opprettet: 08.08.08 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cOrdreNrList  AS CHAR NO-UNDO.
DEF VAR ix            AS INT  NO-UNDO.
DEF VAR fPkSdlId      AS DEC  NO-UNDO.
DEF VAR iCl           AS INT  NO-UNDO.
DEF VAR bMottak       AS LOG  NO-UNDO.
DEF VAR iLnr          AS INT  NO-UNDO.
DEF VAR bProceed      AS LOG  NO-UNDO.
DEF VAR bStdPrisOverf AS LOG  NO-UNDO.
DEF VAR iProfil       AS INT  NO-UNDO.
DEFINE VARIABLE cStorl AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOk    AS LOG       NO-UNDO.

cOrdreNrList = REPLACE(icParam,"|",",").

{syspara.i 5 26 1 bStdPrisOverf LOGICAL}

OverforPakkliste:
DO TRANSACTION ON ERROR UNDO,LEAVE:
  DO ix = 1 TO NUM-ENTRIES(cOrdreNrList):
    FIND Ordre EXCLUSIVE-LOCK
         WHERE Ordre.OrdreNr = INT(ENTRY(ix,cOrdreNrList)) 
         NO-ERROR.
    IF AVAIL Ordre THEN 
    ORDRE_FUNNET:
    DO:
      IF iCl = 0 THEN DO:
        iCl = Ordre.CL.
        FIND FIRST Butiker NO-LOCK
             WHERE Butiker.Butik = iCl
             NO-ERROR.
        IF NOT AVAIL Butiker THEN DO:
          ocReturn = "Sentrallager for ordre finnes ikke i butikkregister. Programfeil: " + PROGRAM-NAME(1).
          UNDO, LEAVE OverforPakkliste.
        END.
        iProfil = Butiker.ProfilNr.
      END. 
      ELSE IF Ordre.CL NE iCl THEN DO:
        ocReturn = "Ordre for flere sentrallagre kan ikke overføres til samme pakkliste".
        UNDO, LEAVE OverforPakkliste.
      END.
      FOR EACH BestHode EXCLUSIVE-LOCK 
          WHERE BestHode.OrdreNr  = Ordre.OrdreNr
            AND BestHode.BestStat < 6:
        IF BestHode.BekreftetDato = ? THEN DO:
          ocReturn = "Ordre " + ENTRY(ix,cOrdreNrList) + " er ikke bekreftet. Kan ikke settes overføres varemottak. Hele overføringen kanselleres".
          UNDO, LEAVE OverforPakkliste.
        END.
/*         IF BestHode.BekreftetDato = ? AND BestHode.BestStat < 5 THEN  */
/*           RUN bytbeststatus.p (RECID(BestHode),"-",Ordre.OrdreNr).    */
        RUN OverforPakkliste (OUTPUT ocReturn).
        bProceed = RETURN-VALUE = "proceed".
        IF ocReturn NE "" THEN DO:
          ocReturn = ocReturn + ". Ordrenr: " + ENTRY(ix,cOrdreNrList).
          UNDO, LEAVE OverforPakkliste.
        END.
      END.
    END. /* ORDRE_FUNNET */
    ELSE DO:
      ocReturn = "Ordre ikke tilgjengelig for oppdatering".
      UNDO, LEAVE OverforPakkliste.
    END.
  END.
  IF NOT bMottak THEN DO:
    ocReturn = "Alle varer er allerede registrert med fullt antall i pakkliste".  
    UNDO, LEAVE OverforPakkliste.
  END.
END.

obOk = ocReturn = "" OR bProceed.
IF obOk THEN 
  ocReturn = ocReturn + "|" + STRING(fPkSdlId).

PROCEDURE OverforPakkliste:
  DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO.

  DEF VAR fPkSdlLinjeId AS DEC NO-UNDO.
  DEF VAR fLevAntall    AS DEC NO-UNDO.
  DEF VAR iMaxStatus    AS INT NO-UNDO.

  DEF BUFFER bPkSdlLinje FOR PkSdlLinje.

  FIND FIRST ArtBas OF BestHode
       NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN DO:
    ocReturn = "Artikkel er ikke tilgjengelig for bestilling " + STRING(BestHode.BestNr).
    RETURN.
  END.

  IF fPkSdlId = 0 THEN DO:
    FIND LAST PkSdlHode NO-LOCK NO-ERROR.
    fPkSdlId = IF AVAIL PkSdlHode THEN PkSdlHode.PkSdlId + 1 ELSE 1.
    CREATE PkSdlHode.
    ASSIGN PkSdlHode.PkSdlStatus    = 10
           PkSdlHode.SendtDato      = TODAY
           fPkSdlId                 = PkSdlHode.PkSdlId
           PkSdlHode.Merknad        = "Manuell fra ordre"
           PkSdlHode.CL             = iCl
           PkSdlHode.PkSdlNr        = ENTRY(1,cOrdreNrList) + (IF NUM-ENTRIES(cOrdreNrList) > 1 THEN "-" + ENTRY(NUM-ENTRIES(cOrdreNrList),cOrdreNrList) ELSE "")
           .
  END.

  FOR EACH BestStr NO-LOCK OF BestHode:
    iMaxStatus = MAX(iMaxStatus,BestStr.BestStat).
    /*
    FIND FIRST StrKonv NO-LOCK
         WHERE TRIM(StrKonv.Storl) = TRIM(BestStr.Storl)
         NO-ERROR.
    IF NOT AVAIL StrKonv THEN DO:
      ocReturn = "En størrelse for bestilling " + STRING(BestHode.BestNr) + " fins ikke (lenger) i størrelsestabell. Overføring kansellert".
      RETURN.
    END.
    */
  END.
  
  FOR EACH BestStr NO-LOCK OF BestHode
      WHERE BestStr.BestStat = iMaxStatus
     ,FIRST StrKonv NO-LOCK
            WHERE TRIM(StrKonv.Storl) = TRIM(BestStr.Storl)
      BY StrKonv.StrKode:
        
    FOR EACH PkSdlLinje NO-LOCK
        WHERE PkSdlLinje.BestNr  = BestStr.BestNr
          AND PkSdlLinje.StrKode = StrKonv.StrKode
          AND PkSdlLinje.PkSdlId NE fPkSdlId
       ,FIRST PkSdlHode OF PkSdlLinje NO-LOCK:
      IF PkSdlHode.PkSdlStatus LE 10 THEN DO:
        ASSIGN ocReturn = "Bestilling " + STRING(BestHode.BestNr) + " er allerede registrert på en åpen pakkliste. Denne må oppdateres (eller slettes) før ny overføring kan gjøres"
               fPkSdlId = PkSdlHode.PkSdlId.
        RETURN "proceed".
      END.
    END.

    fLevAntall = 0.
    FOR EACH BestLevert NO-LOCK
        WHERE BestLevert.BestNr = BestStr.BestNr
          AND BestLevert.Butik  = BestStr.Butik
          AND TRIM(BestLevert.Storl) = TRIM(BestStr.Storl):
      fLevAntall = fLevAntall + BestLevert.Levert.
    END.
    IF fLevAntall GE BestStr.Bestilt THEN NEXT.
    ELSE bMottak = YES.

    FIND LAST PkSdlLinje NO-LOCK
         WHERE PkSdlLinje.PkSdlId = fPkSdlId
         NO-ERROR.
    fPkSdlLinjeId = IF AVAIL PkSdlLinje THEN PkSdlLinje.PkSdlLinjeId + 1 ELSE 1.

    FIND LAST Strekkode NO-LOCK WHERE
      Strekkode.ArtikkelNr = BestHode.ArtikkelNr AND
      Strekkode.StrKode    = StrKonv.StrKode AND  
      NOT Strekkode.Kode BEGINS '02' AND 
      LENGTH(Strekkode.Kode) = 13
      NO-ERROR.
    IF NOT AVAILABLE Strekkode THEN 
    FIND LAST Strekkode NO-LOCK WHERE
      Strekkode.ArtikkelNr = BestHode.ArtikkelNr AND
      Strekkode.StrKode    = StrKonv.StrKode NO-ERROR.  

    CREATE PkSdlLinje.
    ASSIGN PkSdlLinje.ArtikkelNr    = BestHode.ArtikkelNr
           PkSdlLinje.BestNr        = BestHode.BestNr
           PkSdlLinje.PkSdlId       = fPkSdlId
           PkSdlLinje.OrdreNr       = BestHode.OrdreNr
           PkSdlLinje.Beskr         = ArtBas.Beskr
           PkSdlLinje.LevFargKod    = ArtBas.LevFargKod
           PkSdlLinje.Antall        = BestStr.Bestilt - fLevAntall
           PkSdlLinje.AntLevert     = IF ArtBas.Pakke THEN 0 ELSE PkSdlLinje.Antall
           PkSdlLinje.LevKod        = ArtBas.LevKod
           PkSdlLinje.LevNr         = ArtBas.LevNr
           PkSdlLinje.PkSdlLinjeId  = fPkSdlLinjeId
           PkSdlLinje.StrKode       = StrKonv.StrKode
           PkSdlLinje.Kode          = (IF AVAILABLE Strekkode THEN Strekkode.Kode ELSE '')
           PkSdlLinje.Salgsenhet    = ArtBas.SalgsEnhet
           iLnr                     = iLnr + 1 
           PkSdlLinje.Linjenr       = iLnr
           PkSdlLinje.ButikkNr      = BestStr.Butik
           PkSdlLinje.Pakke         = ArtBas.Pakke
           PkSdlLinje.PakkeNr       = ArtBas.PakkeNr
           .
  END.

  FIND FIRST ArtPris NO-LOCK
       WHERE ArtPris.ArtikkelNr = BestHode.ArtikkelNr
         AND ArtPris.ProfilNr   = iProfil
       NO-ERROR.

  FOR EACH BestPris NO-LOCK OF BestHode
      WHERE NOT CAN-FIND(FIRST PkSdlPris
                         WHERE PkSdlPris.PkSdlId    = fPkSdlId
                           AND PkSdlPris.ArtikkelNr = BestHode.ArtikkelNr):
    FIND PkSdlPris EXCLUSIVE-LOCK WHERE
      PkSdlPris.PkSdlId    = fPkSdlId AND
      PkSdlPris.ArtikkelNr = BestHode.ArtikkelNr NO-ERROR.
    IF NOT AVAILABLE PkSdlPris THEN 
    DO:
        CREATE PkSdlPris.
        ASSIGN
            PkSdlPris.PkSdlId    = fPkSdlId
            PkSdlPris.ArtikkelNr = BestHode.ArtikkelNr.        
        BUFFER-COPY ArtBas   EXCEPT ArtikkelNr BrukerId EDato Etid RegistrertDato RegistrertTid RegistrertAv TO PkSdlPris.
        BUFFER-COPY BestHode EXCEPT ArtikkelNr BrukerId EDato Etid RegistrertDato RegistrertTid RegistrertAv TO PkSdlPris.
        /* Bestpris.ArtikkelNr står til 0. Dette feltet oppdateres ikke når bestpris opprettes. */
        BUFFER-COPY BestPris EXCEPT ArtikkelNr BrukerId EDato Etid RegistrertDato RegistrertTid RegistrertAv TO PkSdlPris.
    END.
    ASSIGN 
           PkSdlPris.OverstyrPris   = bStdPrisOverf
           PkSdlPris.NyPris         = BestPris.Pris
           PkSdlPris.NyVarekost     = BestPris.VareKost
           PkSdlPris.NyDB%          = BestPris.DB%
           PkSdlPris.NyFrakt        = BestPris.Frakt
           PkSdlPris.NyInnkjopsPris = BestPris.InnkjopsPris
           PkSdlPris.Rab1%          = BestPris.Rab1%
           PkSdlPris.NyRab1%        = BestPris.Rab1%
           .
    IF AVAIL ArtPris 
       AND ArtPris.Pris[1] = BestPris.Pris 
       AND ArtPris.InnkjopsPris[1] = BestPris.InnkjopsPris 
      THEN
      PkSdlPris.OverstyrPris = NO.
  END.

  /* Pakker ut pakke */
  IF ArtBas.Pakke THEN
      RUN pakkUtPakke (INPUT-OUTPUT fPkSdlId,
                       INPUT-OUTPUT fPkSdlLinjeId,
                       INPUT BestHode.EkstId,
                       INPUT iMaxStatus,
                       INPUT iProfil).
END PROCEDURE.

/* Lagt inn håndtering av pakke */
PROCEDURE pakkUtPakke:
    DEF INPUT-OUTPUT PARAMETER fPkSdlId        AS DEC  NO-UNDO.
    DEF INPUT-OUTPUT PARAMETER fpkPkSdlLinjeId AS DEC  NO-UNDO.
    DEF INPUT        PARAMETER cEkstId         AS CHAR NO-UNDO.
    DEF INPUT        PARAMETER iMaxStatus      AS INT  NO-UNDO.
    DEF INPUT        PARAMETER iProfil         AS INT  NO-UNDO.

    DEF VAR fMvaKr          AS DEC  NO-UNDO.
    DEF VAR fDbKr           AS DEC  NO-UNDO.

    DEF BUFFER pakkeArtBas  FOR ArtBas.
    DEF BUFFER pakkeArtPris FOR ArtPris.
    DEF BUFFER pkPksdlLinje FOR PkSdlLinje.
    DEF BUFFER pkPkSdlPris  FOR PkSdlPris.

    /* Det er alltid bare en størrelse på pakker. */
    FIND FIRST BestStr NO-LOCK OF BestHode
        WHERE BestStr.BestStat = iMaxStatus NO-ERROR.
    IF AVAILABLE BestStr THEN
       FIND FIRST StrKonv NO-LOCK
              WHERE TRIM(StrKonv.Storl) = TRIM(BestStr.Storl) NO-ERROR.
    IF NOT AVAILABLE StrKonv THEN
        RETURN. 

    UTPAKKING:
    FOR EACH PakkeLinje NO-LOCK WHERE
        PakkeLinje.ArtikkelNr = BestHode.ArtikkelNr:

        FIND pakkeArtBas NO-LOCK WHERE
            pakkeArtBas.ArtikkelNr = PakkeLinje.PkArtikkelNr NO-ERROR.
        IF NOT AVAILABLE pakkeArtBas THEN
            NEXT UTPAKKING.

        fpkPkSdlLinjeId = fpkPkSdlLinjeId + 1.

        CREATE pkPkSdlLinje.
        ASSIGN
            pkPkSdlLinje.PkSdlId          = fPkSdlId
            pkPkSdlLinje.PkSdlLinjeId     = fpkPkSdlLinjeId
            .
        ASSIGN pkPkSdlLinje.ArtikkelNr    = PakkeLinje.PkArtikkelNr
               pkPkSdlLinje.BestNr        = BestHode.BestNr 
               pkPkSdlLinje.OrdreNr       = BestHode.OrdreNr
               pkPkSdlLinje.Beskr         = pakkeArtBas.Beskr
               pkPkSdlLinje.LevFargKod    = IF AVAILABLE pakkeArtBas
                                            THEN pakkeArtBas.LevFargKod
                                            ELSE ""
               pkPkSdlLinje.Antall        = BestStr.Bestilt * PakkeLinje.Antall
               pkPkSdlLinje.AntLevert     = pkPkSdlLinje.Antall
               pkPkSdlLinje.LevKod        = IF AVAILABLE pakkeArtBas
                                            THEN pakkeArtBas.LevKod
                                            ELSE ""
               pkPkSdlLinje.LevNr         = IF AVAILABLE pakkeArtbas
                                            THEN pakkeArtBas.LevNr
                                            ELSE 0
               pkPkSdlLinje.StrKode       = PakkeLinje.StrKode
               pkPkSdlLinje.Salgsenhet    = IF AVAILABLE pakkeArtBas
                                            THEN pakkeArtBas.SalgsEnhet
                                            ELSE ""
               pkPkSdlLinje.Linjenr       = PkSdlLinje.LinjeNr
               pkPkSdlLinje.ButikkNr      = BestStr.Butik
               pkPkSdlLinje.Pakke         = pakkeArtBas.Pakke
               pkPkSdlLinje.PakkeNr       = pakkeArtBas.PakkeNr
               pkPkSdlLinje.BestNr        = BestHode.BestNr 
               pkPkSdlLinje.OrdreNr       = BestHode.OrdreNr
               .

        FIND FIRST pakkeArtPris NO-LOCK
             WHERE pakkeArtPris.ArtikkelNr = PakkeLinje.PkArtikkelNr
               AND pakkeArtPris.ProfilNr   = iProfil
             NO-ERROR.
        IF NOT AVAILABLE pakkeArtPris THEN
            FIND FIRST pakkeArtPris NO-LOCK
                 WHERE pakkeArtPris.ArtikkelNr = PakkeLinje.PkArtikkelNr NO-ERROR.

        PRIS:
        DO:
          FIND pkPkSdlPris EXCLUSIVE-LOCK WHERE
              pkPkSdlPris.PkSdlId    = fPkSdlId AND
              pkPkSdlPris.ArtikkelNr = PakkeLinje.PkArtikkelNr NO-ERROR.
          IF NOT AVAILABLE pkPkSdlPris THEN
          DO:
              CREATE pkPkSdlPris.
              ASSIGN
                  pkPkSdlPris.PkSdlId        = fPkSdlId
                  pkPkSdlPris.ArtikkelNr     = PakkeLinje.PkArtikkelNr
                  .
              ASSIGN
                  /*pkPkSdlPris.OverstyrPris   = bStdPrisOverf*/
                  pkPkSdlPris.Beskr          = pakkeArtBas.Beskr
                  pkPkSdlPris.LevKod         = pakkeArtBas.LevKod
                  pkPkSdlPris.LevFargKod     = pakkeArtBas.LevFargKod

                  /* Den nye prisen som kommer inn og bruker kan velge å overstyre med. */
                  pkPkSdlPris.NyInnkjopsPris = pakkeArtPris.VareKost[1]
                  pkPkSdlPris.NyRab1%        = 0
                  pkPkSdlPris.NyFrakt        = 0
                  pkPkSdlPris.NyVarekost     = pakkeArtPris.VareKost[1]
                  pkPkSdlPris.NyPris         = pakkeArtPris.Pris[1]
                  /* Beregner ny DB% */
                  fMvaKr                   = pakkeArtPris.Pris[1] - (pakkeArtPris.Pris[1] / (1 + (ArtPris.Mva%[1] / 100)))
                  fDbKr                    = pakkeArtPris.Pris[1] - fMvaKr - pakkeArtPris.VareKost[1]
                  pkPkSdlPris.NyDB%          = ROUND((fDbKr * 100) / (pakkeArtPris.VareKost[1] + fDbKr),2)
                  pkPkSdlPris.NyDB%          = IF pkPkSdlPris.NyDB% = ? THEN 0 ELSE pkPkSdlPris.NyDB%

                  /* Gjeldende pris som er hentet inn fra Artikkelkortet. */
                  pkPkSdlPris.InnkjopsPris   = pakkeArtPris.InnkjopsPris[1]
                  pkPkSdlPris.Rab1%          = 0
                  pkPkSdlPris.Frakt          = 0
                  pkPkSdlPris.VareKost       = pakkeArtPris.VareKost[1]
                  pkPkSdlPris.Pris           = pakkeArtPris.Pris[1]
                  pkPkSdlPris.DB%            = pakkeArtPris.DB%[1]
                  pkPkSdlPris.OverstyrPris   = NO
                  .


          END.

        END. /* PRIS */

        /* Her slipper vi recorden. Lik at DB trigger får slippe til. */
        IF AVAILABLE pkPkSdlLinje THEN RELEASE pkPkSdlLinje.
        IF AVAILABLE pkPkSdlPris  THEN RELEASE pkPkSdlPris.
    END. /* UTPAKKING */

END. /* pakkUtPakke */
