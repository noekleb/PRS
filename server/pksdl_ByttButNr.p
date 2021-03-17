/* Hent merkelapper for pakkseddel
   Parameter:  <PkSdlId>
   Opprettet: 07.09.07 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fPkSdlId        AS DEC  NO-UNDO.
DEF VAR cArtNrList      AS CHAR NO-UNDO.
DEF VAR cEANlist        AS CHAR NO-UNDO.
DEF VAR cAntallList     AS CHAR NO-UNDO.
DEF VAR cIndividList    AS CHAR NO-UNDO.
DEF VAR cPrisList       AS CHAR NO-UNDO.
DEF VAR cStrListe       AS CHAR NO-UNDO.
DEFINE VARIABLE lArtikkelNr AS DECIMAL NO-UNDO.
DEFINE VARIABLE iStrKode    AS INTEGER NO-UNDO.
DEF VAR iButNr AS INT NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.
DEFINE VARIABLE cOutletLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCl AS INTEGER NO-UNDO.
DEFINE VARIABLE rRowId AS ROWID NO-UNDO.
DEFINE VARIABLE iFrabutNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iNetButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iFlyttbut AS INTEGER NO-UNDO.

DEFINE VARIABLE lPrisRab%   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lforhRab%   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cEDB-System AS CHARACTER INITIAL 'Gant Global' NO-UNDO.
DEFINE VARIABLE fMvaKr      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE fDbKr       AS DECIMAL   NO-UNDO.
                          
DEFINE BUFFER clButiker FOR Butiker.
DEFINE BUFFER bufArtPris FOR ArtPris.

DEF VAR hQuery          AS HANDLE NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rArtPrisKalkyle AS cls.Artikkel.ArtPrisKalkyle NO-UNDO.

{syspara.i 22 5 2 cOutletLst}
{syspara.i  5 1 1 iCl INT}
{syspara.i  150 1 3 iNetButNr INT}

ASSIGN 
  cLogg       = 'pksdl_ByttButNr' + REPLACE(STRING(TODAY),'/','')
  .
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rArtPrisKalkyle  = NEW cls.Artikkel.ArtPrisKalkyle( cLogg ) NO-ERROR.

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start.' 
    ).

FIND clButiker WHERE 
    clButiker.butik = iCl NO-ERROR.

ASSIGN 
    ocReturn  = ""
    iButNr    = INT(ENTRY(1,icParam,'|'))
    rRowId    = ?
    iFrabutNr = 0
    .

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  icParam: ' + icParam 
    ).

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE TRUE").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  starter bytting av butikknr til butikk: ' + STRING(iButNr) 
    ).

BLOKKEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
  iAnt = iAnt + 1.

  FIND LAST PkSdlHode NO-LOCK WHERE
    PkSdlHode.PkSdlId = DECIMAL(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) AND 
    PkSdlHode.PkSdlStatus = 10 AND 
    CAN-FIND(FIRST PkSdlLinje OF PkSdlHode)
    USE-INDEX SendtDato NO-ERROR.
  IF rRowId = ? AND AVAILABLE PkSdlHode THEN 
  DO:
      ASSIGN 
          rRowId = ROWID(PkSdlHode)
          .
      FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK NO-ERROR.
      IF AVAILABLE PkSdlLinje THEN 
            iFraButNr = PksdlLinje.ButikkNr.
  END.

    rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Variabler: '
        + ' Fra ButikkNr: ' + STRING(iFraButNr)
        + ' Til ButikkNr: ' + STRING(iButNr)
        + ' PakkseddelId: ' + STRING(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE)
        + ' PakkseddelNr: ' + STRING(ihBuffer:BUFFER-FIELD("PkSdlNr"):BUFFER-VALUE)
        + ' Resultat: ' + STRING(AVAILABLE PkSdlHode)
        ).

  /* Tar bort ordre og bestillinger slik at disse må opprettes på nytt ved varemottak. */
  IF AVAILABLE PkSdlHode THEN
  DO:
      LINJEBLOKK:
      FOR EACH PkSdlLinje OF PkSdlHode
        BREAK BY PkSdlLinje.PkSdlId
              BY PkSdlLinje.ArtikkelNr:
          /* Sletter bestilling. Rører ikke ordren. */
          FOR EACH BestHode EXCLUSIVE-LOCK WHERE 
              BestHode.BestNr = PkSdlLinje.BestNr:
              DELETE BestHode.    
          END.
          
          /* Logger fra butikken. */
          IF iFlyttbut <> PkSdlLinje.butikkNr THEN 
            iFlyttBut = PkSdlLinje.butikkNr.
          
          IF FIRST-OF(PkSdLLinje.ArtikkelNr) THEN
          FIRST-BLOKKEN: 
          DO:
            /* Flyttes pakkseddelen fra en outlet til en butikk som ikke er outlet (Gjelder også eCom), skal */
            /* utprisen skal ikke ha rabatt. Den skal settes tilbake til den pris som ligger på profil 1.    */
            IF CAN-DO(cOutletLst,STRING(PkSdlLinje.ButikkNr)) THEN 
            MOTTAGER_IKKE_OUTLET:
            DO:
                /* Ny butikk er ikke outlet, derfor skal rabatt på utpris fjernes. */
                /* Rabatten på innkjøps prisen skal også reduseres til 10%.        */
                IF NOT CAN-DO(cOutletLst,STRING(iButNr)) THEN 
                DO:
                    FIND Butiker NO-LOCK WHERE 
                      Butiker.butik = PkSdlLinje.Butik NO-ERROR.
                    IF AVAILABLE clButiker THEN
                    BUTIKKBLOKK: 
                    DO:
                        /* Henter CL pris. */
                        FIND ArtPris NO-LOCK WHERE 
                            ArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr AND 
                            ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
                        /* CL mangler kalkyle. Kalylen kopieres fra pakskeddelens profil til CL's profil. */
                        IF NOT AVAILABLE ArtPris THEN 
                        DO:
                          /* Henter Pris fra butikken som pakkseddelen ligger på. Her er det alltid en kalkyle. */
                          FIND FIRST ArtPris NO-LOCK WHERE 
                              ArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr AND 
                              ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
                          /* Kopierer kalkylen fra pakkseddelens butikk til CL's butikk. */
                          IF AVAILABLE ArtPris THEN 
                            rArtPrisKalkyle:NyArtPris(ArtPris.ArtikkelNr, Butiker.ProfilNr, clButiker.ProfilNr).
                        END.
                        /* Her må kalkylen kopieres fra pakkseddelens butikk til CL's profil. */
                        ELSE IF (ArtPris.Pris[1] = 0 OR ArtPris.InnkjopsPris[1] = 0) THEN 
                            rArtPrisKalkyle:KopierArtPris(ArtPris.ArtikkelNr, Butiker.ProfilNr, clButiker.ProfilNr).
                        /* Henter CL pris etter ny/kopier sjekking - nå SKAL prisen finnes. */
                        FIND ArtPris NO-LOCK WHERE 
                            ArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr AND 
                            ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
                        
                        FIND PkSdlPris EXCLUSIVE-LOCK WHERE 
                            PkSdlPris.PkSdlId = PkSdlLinje.PkSdlId AND 
                            PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
                        IF AVAILABLE ArtPris AND 
                           AVAILABLE PkSdlPris  AND 
                           (ArtPris.Pris[1] > 0 AND ArtPris.InnkjopsPris[1] > 0 ) THEN
                        PRISKORR:
                        DO: 
                            /* ---------------- Her legges mottager butikk's rabatter på ----------------------*/
                            FIND FIRST ImpKonv NO-LOCK WHERE 
                                  ImpKonv.EDB-System = cEDB-System AND 
                                  ImpKonv.Tabell     = 'Def.Rab%' AND 
                                  ImpKonv.EksterntId = STRING(iButNr) NO-ERROR.
                            IF AVAILABLE ImpKonv 
                                  THEN ASSIGN 
                                      lforhRab%      = DEC(ImpKonv.Merknad)
                                      . 
  
                            /* Tar bort rabatten på UT pris. */
                            ASSIGN
                                PkSdlPris.NyPris = ArtPris.Pris[1].
                            /* Endrer rabatten på innkjøpsprisen. */ 
                            ASSIGN 
                                PkSdlPris.NyInnkjopsPris = ArtPris.InnkjopsPris[1]
                                PkSdlPris.NyRab1%        = lforhRab%
                                PkSdlPris.NyVarekost     = ROUND(ArtPris.InnkjopsPris[1] - (ArtPris.InnkjopsPris[1] * lforhRab% / 100),2)
                                fMvaKr                   = PkSdlPris.NyPris - (PkSdlPris.NyPris / (1 + (ArtPris.Mva%[1] / 100)))
                                fDbKr                    = PkSdlPris.NyPris - fMvaKr - PkSdlPris.NyVarekost                   
                                PkSdlPris.NyDb%          = ROUND((fDbKr * 100) / (PkSdlPris.NyPris - fMvaKr),2)
                                PkSdlPris.NyDb%          = IF PkSdlPris.NyDb% = ? THEN 0 ELSE PkSdlPris.NyDb%
                                .
                        END. /* PRISKORR */   
                    END. /* BUTIKKBLOKK */
                END.
            END. /* MOTTAGER_IKKE_OUTLET */
                
            /* Er mottager Outlet og den det flyttes fra IKKE outlet, skal fulle rabatter legges på. */
            /* Utgangspunktet blir da sentrallager pris uten rabatter.                               */
            ELSE IF CAN-DO(cOutletLst,STRING(iButNr)) THEN 
            MOTTAGER_ER_OUTLET:
            DO:
                /* Fra butikken er ikke outlet. */
                IF NOT CAN-DO(cOutletLst,STRING(PkSdlLinje.ButikkNr)) THEN
                DO:
                    /* Henter Outlet butikken */
                    FIND Butiker NO-LOCK WHERE 
                        Butiker.Butik = iButNr NO-ERROR.
                    IF AVAILABLE Butiker THEN 
                    DO:
                        /* Sjekker om Outlet allerede har en kalkyle. Hvis Ja, brukes denne. */
                        FIND ArtPris EXCLUSIVE-LOCK WHERE 
                            ArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr AND 
                            ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
                        /* Oppretter lokal kalkyle hvis det ikke finnes noen. Bruker HK som utgangspunkt. */
                        IF NOT AVAILABLE ArtPris THEN 
                        NY_KALKYLE:
                        DO:
                            /* Henter HK kalkyle */
                            FIND bufArtPris NO-LOCK WHERE 
                                bufArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr AND 
                                bufArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
                            CREATE ArtPris.
                            BUFFER-COPY 
                              bufArtPris 
                              EXCEPT ProfilNr
                              TO ArtPris
                              ASSIGN 
                                  ArtPris.ProfilNr = Butiker.ProfilNr.  
                        END. /* NY_KALKYLE */
                        /* Her må kalkylen kopieres fra pakkseddelens butikk til Outlet's profil. */
                        ELSE IF (ArtPris.Pris[1] = 0 OR ArtPris.InnkjopsPris[1] = 0) THEN 
                            rArtPrisKalkyle:KopierArtPris(ArtPris.ArtikkelNr, clButiker.ProfilNr, Butiker.ProfilNr).
                                  
                        /* ---------------- Her hentes Outlet'ens rabatter ----------------------*/
                        FIND FIRST ImpKonv NO-LOCK WHERE 
                              ImpKonv.EDB-System = cEDB-System AND 
                              ImpKonv.Tabell     = 'Def.Rab%' AND 
                              ImpKonv.EksterntId = STRING(iButNr) NO-ERROR.
                        IF AVAILABLE ImpKonv 
                              THEN ASSIGN 
                                  lforhRab%      = DEC(ImpKonv.Merknad)
                                  lPrisRab%      = DEC(ImpKonv.InterntId)
                                  . 
                        /* Regner om kalkylen hvis priser er angitt. */
                        IF (ArtPris.Pris[1] > 0 AND ArtPris.InnkjopsPris[1] > 0) THEN
                        DO: 
                          ASSIGN 
                            ArtPris.Pris[1]         = ROUND(ArtPris.Pris[1] - (ArtPris.Pris[1] * lPrisRab% / 100),2) 
                            ArtPris.InnkjopsPris[1] = ArtPris.InnkjopsPris[1]
                            ArtPris.Rab1%[1]        = lforhRab%
                            ArtPris.Varekost[1]     = ROUND(ArtPris.InnkjopsPris[1] - (ArtPris.InnkjopsPris[1] * lforhRab% / 100),2)
                            fMvaKr                  = ArtPris.Pris[1] - (ArtPris.Pris[1] / (1 + (ArtPris.Mva%[1] / 100)))
                            fDbKr                   = ArtPris.Pris[1] - fMvaKr - ArtPris.Varekost[1]                   
                            ArtPris.Db%[1]          = ROUND((fDbKr * 100) / (ArtPris.Pris[1] - fMvaKr),2)
                            ArtPris.Db%[1]          = IF ArtPris.Db%[1] = ? THEN 0 ELSE ArtPris.Db%[1]
                            .
                          rStandardFunksjoner:SkrivTilLogg(cLogg,
                              '  Kalkyle er regnet om ' + 
                              ' Artikkel: ' + STRING(PkSdlLinje.ArtikkelNr) + 
                              ' ProfilNr: ' + STRING(ArtPris.ProfilNr) +
                              ' lforhRab%:' + STRING(lforhRab%) +  
                              ' lPrisRab%:' + STRING(lPrisRab%) + 
                              ' Pris: ' + STRING(ArtPris.Pris[1]) +
                              ' Varekost: ' + STRING(ArtPris.varekost[1]) 
                              ).
                        END.
                        ELSE 
                          rStandardFunksjoner:SkrivTilLogg(cLogg,
                              '  Pris eller innkjøpspris er 0. Kalkylen kan ikke regnes om...' + 
                              ' Artikkel: ' + STRING(PkSdlLinje.ArtikkelNr) + 
                              ' ProfilNr: ' + STRING(ArtPris.ProfilNr) +
                              ' lforhRab%:' + STRING(lforhRab%) +  
                              ' lPrisRab%:' + STRING(lPrisRab%)
                              ).
                        /* ---------- Rabattmix ferdig --------------------------------------------- */  
                    END.
                    
                    /* Her er ArtPris tilgjengelig og korrekt. Verdier fra denne settes inn i pakkseddelen. */
                    FIND PkSdlPris EXCLUSIVE-LOCK WHERE 
                        PkSdlPris.PkSdlId = PkSdlLinje.PkSdlId AND 
                        PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
                    IF AVAILABLE PkSdlPris AND AVAILABLE ArtPris THEN 
                      ASSIGN
                         PkSdlPris.NyPris         = ArtPris.Pris[1] 
                         PkSdlPris.NyInnkjopsPris = ArtPris.InnkjopsPris[1]
                         PkSdlPris.NyRab1%        = ArtPris.Rab1%[1]
                         PkSdlPris.NyVarekost     = ArtPris.Varekost[1]
                         PkSdlPris.NyDB%          = ArtPris.Db%[1]
                         .                        
                          
                    IF AVAILABLE ArtPris THEN 
                      FIND CURRENT ArtPris NO-LOCK.
                END.
            END. /* MOTTAGER_ER_OUTLET */
          END. /* FIRST-BLOKKEN */

          /* Linjen flyttes til nytt butikknr. */
          ASSIGN 
            PkSdlLinje.ButikkNr = iButNr
            .
      END. /* LINJEBLOKK */
      
      /* Setter opphav etter endring hvis pakkseddelen er flyttet fra en outlet til en vanlig butikk. Normalt til eCom. */
      /* TN 10/1-20 Endrer ikke lenger opphav. */
      IF CAN-DO(cOutletLst,STRING(iFraButNr)) AND /* Fra butikken skal være en Outlet. */
          NOT CAN-DO(cOutletLst,STRING(iButNr)) /* Til butikken skal IKKE være en Outlet. */
      THEN 
      DO:
          FIND CURRENT PkSdlHode EXCLUSIVE-LOCK NO-ERROR.
          IF AVAILABLE PkSdlHode THEN 
          DO:
              ASSIGN 
                PkSdlHode.Merknad     = 'Byttet fra butikk ' + STRING(iFlyttBut) + ' ' + STRING(USERID('skotex')) + ' ' + STRING(NOW,"99/99/99 HH:MM:SS") + CHR(10) + 
                                        PkSdlHode.Merknad
                PkSdlHode.butikkNr    = iButNr 
                .
              FIND CURRENT PkSdlHode NO-LOCK.
          END.
      END.
      /* Uansett når det byttes butikknr, skal pakkseddelen merkes. */
      ELSE DO:
          FIND CURRENT PkSdlHode EXCLUSIVE-LOCK NO-ERROR.
          IF AVAILABLE PkSdlHode THEN 
          DO:
              rStandardFunksjoner:SkrivTilLogg(cLogg,
                  '  Pakkseddel flyttes til butikk ' + STRING(iButNr) + ' fra ' + string(iFlyttBut) 
                  ).
              ASSIGN 
                PkSdlHode.Merknad     = 'Flyttet fra butikk ' + STRING(iFlyttBut) + ' ' + STRING(USERID('skotex')) + ' ' + STRING(NOW,"99/99/99 HH:MM:SS") + CHR(10) + 
                                        PkSdlHode.Merknad
                PkSdlHode.butikkNr    = iButNr 
                .
              FIND CURRENT PkSdlHode NO-LOCK.
          END.
      END.    
            
  END. /* HODEBLOKK */

  hQuery:GET-NEXT().
END. /* BLOKKEN */

DELETE OBJECT hQuery NO-ERROR.

obOk = TRUE.
ocReturn = 'Endret butikknr.'.

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt.' 
    ).
