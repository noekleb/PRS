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
DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.
DEFINE VARIABLE cOutletLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCl AS INTEGER NO-UNDO.
DEFINE VARIABLE rRowId AS ROWID NO-UNDO.
DEFINE VARIABLE iFrabutNr AS INTEGER NO-UNDO.

DEFINE VARIABLE lPrisRab%   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lforhRab%   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cEDB-System AS CHARACTER INITIAL 'Gant Global' NO-UNDO.
DEFINE VARIABLE fMvaKr      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE fDbKr       AS DECIMAL   NO-UNDO.
                          
DEFINE BUFFER clButiker FOR Butiker.
DEFINE BUFFER bufArtPris FOR ArtPris.

DEF VAR hQuery          AS HANDLE NO-UNDO.

{syspara.i 22 5 2 cOutletLst}
{syspara.i  5 1 1 iCl INT}
FIND clButiker WHERE 
    clButiker.butik = iCl NO-ERROR.

ASSIGN 
    ocReturn  = ""
    iButNr    = INT(ENTRY(1,icParam,'|'))
    rRowId    = ?
    iFrabutNr = 0
    .

RUN bibl_loggDbFri.p ('PakkseddelInnlevFraKasse', 'asPakkseddel.p (pksdl_ByttbutNr).' 
    ).

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE TRUE").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

RUN bibl_loggDbFri.p ('PakkseddelInnlevFraKasse', 'asPakkseddel.p (pksdl_ByttbutNr): starter bytting av butikknr ' 
    + ' Butikk: '     + STRING(iButNr)
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

    RUN bibl_loggDbFri.p ('PakkseddelInnlevFraKasse', 'asPakkseddel.p LINJE (pksdl_ByttbutNr): starter bytting av butikknr ' 
        + ' Fra ButikkNr: ' + STRING(iFraButNr)
        + ' Til ButikkNr: ' + STRING(iButNr)
        + ' Pakkseddel: ' + STRING(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE)
        + ' Resultat: ' + STRING(AVAILABLE PkSdlHode)
        ).

  /* Tar bort ordre og bestillinger slik at disse må opprettes på nytt ved varemottak. */
  IF AVAILABLE PkSdlHode THEN
  DO:
      LINJEBLOKK:
      FOR EACH PkSdlLinje OF PkSdlHode:
          /* Sletter bestilling. Rører ikke ordren. */
          FOR EACH BestHode EXCLUSIVE-LOCK WHERE 
              BestHode.BestNr = PkSdlLinje.BestNr:
              DELETE BestHode.    
          END.
          
          /* Flyttes pakkseddelen fra en outlet til en butikk som ikke er outlet (Gjelder også eCom), skal */
          /* utprisen skal ikke ha rabaatt. Den skal settes tilbake til den pris som ligger på profil 1.   */
          IF CAN-DO(cOutletLst,STRING(PkSdlLinje.ButikkNr)) THEN 
          MOTTAGER_IKKE_OUTLET:
          DO:
              /* Ny butikk er ikke outlet, derfor skal rabatt på utpris fjernes. */
              /* Rabatten på innkjøps prisen skal også reduseres til 10%.        */
              IF NOT CAN-DO(cOutletLst,STRING(iButNr)) THEN 
              DO:
                  /* Henter HK pris. */
                  FIND Butiker NO-LOCK WHERE 
                      Butiker.Butik = iCl NO-ERROR.
                  IF AVAILABLE Butiker THEN
                  BUTIKKBLOKK: 
                  DO:
                      FIND ArtPris NO-LOCK WHERE 
                          ArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr AND 
                          ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
                      IF NOT AVAILABLE ArtPris THEN 
                        FIND FIRST ArtPris NO-LOCK WHERE 
                            ArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
                      FIND PkSdlPris EXCLUSIVE-LOCK WHERE 
                          PkSdlPris.PkSdlId = PkSdlLinje.PkSdlId AND 
                          PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
                      IF AVAILABLE PkSdlPris AND AVAILABLE PkSdlPris THEN
                      PRISKORR:
                      DO: 
                          /* Tar bort rabatten på UT pris. */
                          ASSIGN
                              PkSdlPris.NyPris = ArtPris.Pris[1].
                          /* Endrer rabatten på innkjøpsprisen. */ 
                          ASSIGN 
                              lforhRab%                = 10
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
          IF CAN-DO(cOutletLst,STRING(iButNr)) THEN 
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
                      FIND ArtPris NO-LOCK WHERE 
                          ArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr AND 
                          ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
                      /* Oppretter lokal kalkyle hvis det ikke finnes noen. Bruker HK som utgangspunkt. */
                      IF NOT AVAILABLE ArtPris THEN 
                      NY_KALKYLE:
                      DO:
                          /* Henter HK kalkyle */
                          FIND ArtPris NO-LOCK WHERE 
                              ArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr AND 
                              ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
                          CREATE bufArtPris.
                          BUFFER-COPY 
                            ArtPris 
                            EXCEPT ProfilNr
                            TO bufArtPris
                            ASSIGN 
                                bufArtPris.ProfilNr = Butiker.ProfilNr.  
                                
                          /* ---------------- Her legges Outlet'ens rabatter på ----------------------*/
                          FIND FIRST ImpKonv NO-LOCK WHERE 
                                ImpKonv.EDB-System = cEDB-System AND 
                                ImpKonv.Tabell     = 'Def.Rab%' AND 
                                ImpKonv.EksterntId = STRING(iButNr) NO-ERROR.
                          IF AVAILABLE ImpKonv 
                                THEN ASSIGN 
                                    lforhRab%      = DEC(ImpKonv.Merknad)
                                    lPrisRab%      = DEC(ImpKonv.InterntId)
                                    . 
                          
                          ASSIGN 
                            bufArtPris.Pris[1]         = ROUND(ArtPris.Pris[1] - (ArtPris.Pris[1] * lPrisRab% / 100),2) 
                            bufArtPris.InnkjopsPris[1] = ArtPris.InnkjopsPris[1]
                            bufArtPris.Rab1%[1]        = lforhRab%
                            bufArtPris.Varekost[1]     = ROUND(ArtPris.InnkjopsPris[1] - (ArtPris.InnkjopsPris[1] * lforhRab% / 100),2)
                            fMvaKr                     = bufArtPris.Pris[1] - (bufArtPris.Pris[1] / (1 + (bufArtPris.Mva%[1] / 100)))
                            fDbKr                      = bufArtPris.Pris[1] - fMvaKr - bufArtPris.Varekost[1]                   
                            bufArtPris.Db%[1]          = ROUND((fDbKr * 100) / (bufArtPris.Pris[1] - fMvaKr),2)
                            bufArtPris.Db%[1]          = IF bufArtPris.Db%[1] = ? THEN 0 ELSE bufArtPris.Db%[1]
                            .
                          /* ---------- Rabattmix ferdig --------------------------------------------- */  
                            
                          FIND ArtPris NO-LOCK WHERE 
                              ROWID(ArtPris) = ROWID(bufArtPris).
                          RELEASE bufArtPris.
                      END. /* NY_KALKYLE */
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
              END.
          END. /* MOTTAGER_ER_OUTLET */

          /* Linjen flyttes til nytt butikknr. */
          ASSIGN 
            PkSdlLinje.ButikkNr = iButNr
            .
      END. /* LINJEBLOKK */
      
      /* Setter opphav etter endring hvis pakkseddelen er flyttet fra en outlet til en vanlig butikk. Normalt til eCom. */
      IF CAN-DO(cOutletLst,STRING(iFraButNr)) AND /* Fra butikken skal være en Outlet. */
          NOT CAN-DO(cOutletLst,STRING(iButNr)) /* Til butikken skal IKKE være en Outlet. */
      THEN 
      DO:
          FIND CURRENT PkSdlHode EXCLUSIVE-LOCK NO-ERROR.
          IF AVAILABLE PkSdlHode THEN 
          DO:
              RUN bibl_loggDbFri.p ('PakkseddelInnlevFraKasse', 'asPakkseddel.p (pksdl_ByttbutNr): Opphav satt til 7. Pga. fra outlet til vanlig butikk. ' 
                  + ' Fra Butikk: '     + STRING(iFraButNr)
                  + ' Til Butikk: '     + STRING(iButNr)
                  + ' OutletLst : '     + cOutletLst
                  ).
              PkSdlHode.PkSdlOpphav = 7.
              FIND CURRENT PkSdlHode NO-LOCK.
          END.
      END.    
            
  END. /* HODEBLOKK */

  hQuery:GET-NEXT().
END. /* BLOKKEN */


DELETE OBJECT hQuery NO-ERROR.

obOk = TRUE.
ocReturn = 'Endret butikknr.'.
