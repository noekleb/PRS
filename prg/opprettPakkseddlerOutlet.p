/* opprettPakkseddlerOutlet.p */
DEFINE TEMP-TABLE TT_OvBuffer NO-UNDO LIKE OvBuffer.

/*DEFINE INPUT PARAMETER iBuntNr          AS INTEGER NO-UNDO.*/
DEFINE INPUT  PARAMETER TABLE FOR tt_OvBuffer.
DEFINE INPUT  PARAMETER iFraButNr AS INTEGER NO-UNDO. 
DEFINE INPUT  PARAMETER iTilbutNr          AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER cPkSdlNr         AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER plFaktura_Id     AS DECIMAL NO-UNDO.
DEFINE INPUT  PARAMETER iPkSdlOpphav     AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER fPkSdlId         AS DECIMAL FORMAT ">>>>>>>>>>>>9" NO-UNDO.

DEFINE VARIABLE iCL         AS INTEGER NO-UNDO.
DEFINE VARIABLE iCLProfilNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iLevNr      AS INTEGER NO-UNDO.
DEFINE VARIABLE iAntLinjer  AS INTEGER NO-UNDO.
DEFINE VARIABLE bStdPrisOverf AS LOG  NO-UNDO.
DEFINE VARIABLE lRab%       AS DECIMAL NO-UNDO.
DEFINE VARIABLE lPrisRab% AS DECIMAL NO-UNDO.
DEFINE VARIABLE cEDB-System AS CHARACTER INITIAL 'Gant Global' NO-UNDO.
DEFINE VARIABLE cOutletLst  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNettButLager AS INTEGER NO-UNDO.
DEFINE VARIABLE pkhBuffer AS HANDLE  NO-UNDO.     
DEFINE VARIABLE bOk       AS LOG     NO-UNDO.
DEFINE VARIABLE cReturn   AS CHARACTER NO-UNDO.
DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk AS LOG NO-UNDO.
DEFINE VARIABLE cPlussMinusButLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.

DEFINE TEMP-TABLE ttPkSdlLinje NO-UNDO LIKE PkSdlLinje.

DEFINE BUFFER clButiker FOR Butiker.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

FUNCTION FixChk RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER )  FORWARD.

{syspar2.i 22 20 2 iNettButLager INT}  

ASSIGN 
  cPlussMinusButLst = '848,849'
  cLogg             = 'opprettPakkseddlerOutlet' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','')
  bTest             = TRUE
  .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

FIND FIRST tt_OvBuffer NO-LOCK NO-ERROR.
IF AVAILABLE tt_OvBuffer THEN 
  FIND OvBunt NO-LOCK WHERE 
    Ovbunt.buntNr = tt_Ovbuffer.BuntNr NO-ERROR.
IF (plFaktura_Id <> ? AND plFaktura_Id <> 0) THEN 
  FIND FakturaHode NO-LOCK WHERE
      FakturaHode.Faktura_Id = plFaktura_Id NO-ERROR.

/* Henter butikk det overføres fra. */
FIND Butiker NO-LOCK WHERE
  Butiker.Butik = iFraButNr NO-ERROR.

{syspara.i 5 1 1 iCl INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE clButiker THEN 
  RETURN.
iClProfilNr = clButiker.ProfilNr. 

/* Setter leverandørnnumer som skal benyttes. */
{syspara.i 210 100 1 iLevNr INT}
IF iLevNr = 0 THEN
    iLevNr = 40.

{syspara.i 5 26 1 bStdPrisOverf LOGICAL}

{syspara.i 22 5 2 cOutletLst}

/* Rabattene skal hentes for den butikk som pakkseddelen er stilet til. */
FIND FIRST ImpKonv NO-LOCK WHERE 
    ImpKonv.EDB-System = cEDB-System AND 
    ImpKonv.Tabell     = 'Def.Rab%' AND 
    ImpKonv.EksterntId = STRING(iTilButNr) NO-ERROR.
IF AVAILABLE ImpKonv 
    THEN ASSIGN 
        lRab%     = DEC(ImpKonv.Merknad)
        lPrisRab% = DEC(ImpKonv.InterntId).

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start' 
    ).    
IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Parametre m.m.: ' 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    cPlussMinusButLst: ' + cPlussMinusButLst 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    iNettButLager    : ' + STRING(iNettButLager) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    iFraButNr        : ' + STRING(iFraButNr) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    iTilButNr        : ' + STRING(iTilButNr) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    iClProfilNr      : ' + STRING(iClProfilNr) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    iLevNr           : ' + STRING(iLevNr) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    bStdPrisOverf    : ' + STRING(bStdPrisOverf) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    lRab%            : ' + STRING(lRab%) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    lPrisRab%        : ' + STRING(lPrisRab%) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    cOutletLst       : ' + STRING(cOutletLst) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Avail tt_OvBuffer: ' + STRING(AVAILABLE tt_OvBuffer) + ' ' + IF AVAILABLE tt_OvBuffer THEN STRING(tt_OvBuffer.buntNr) ELSE '' 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Avail OvBunt     : ' + STRING(AVAILABLE OvBunt) + ' ' + IF AVAILABLE OvBunt THEN STRING(OvBunt.buntNr) ELSE '' 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    plFaktura_Id     : ' + STRING(plFaktura_Id) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Avail FakturaHode: ' + STRING(AVAILABLE FakturaHode) + ' ' + IF AVAILABLE FakturaHode THEN STRING(FakturaHode.FakturaNr) ELSE '' 
      ).    
END.

IF (NOT AVAILABLE tt_OvBuffer  OR 
    NOT AVAILABLE Butiker) THEN
    DO: 
      IF NOT AVAILABLE tt_Ovbuffer THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Finner ikke tt_OvBuffer. Avslutter.' 
            ).    
      IF NOT AVAILABLE Butiker THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Finner ikke Butiker. Avslutter.' 
            ).    
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          'Slutt' 
          ).    
      RETURN.
    END.

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Oppretter pakkseddler.' 
    ).    
RUN OpprettPakksedler.
     
/* Oppretter bestilling for pakkseddelen. */     
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Oppretter linjer for ordre og bestilling(' + STRING(fPkSdlId) + ')' 
    ).    
FOR EACH pkSdlLinje NO-LOCK WHERE
    PkSdlLinje.PkSdlId = fPkSdlId:
    CREATE ttPkSdlLinje.
    BUFFER-COPY PkSdlLinje TO ttPkSdlLinje.
END.

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Oppretter ordre og bestilling.' 
    ).    
pkhBuffer = TEMP-TABLE ttPkSdlLinje:DEFAULT-BUFFER-HANDLE.
RUN pksdl_opprett_ordre.p ('', pkhBuffer, '', OUTPUT cReturn, OUTPUT bOk).

/* Rydder opp før programmet avsluttes. */
EMPTY TEMP-TABLE TT_OvBuffer.
EMPTY TEMP-TABLE ttPkSdlLinje.

IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt' 
      ).    
END.
/* **********************  Internal Procedures  *********************** */

PROCEDURE OpprettPakksedler:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
DEF VAR cButikkLst AS CHAR   NO-UNDO.

DEF VAR cPakkseddelLst  AS CHAR   NO-UNDO.
DEF VAR cOrdreLst       AS CHAR   NO-UNDO.
DEF VAR piLoop     AS INT    NO-UNDO.
DEF VAR pi2Loop    AS INT    NO-UNDO.
DEF VAR cOVreFil   AS CHAR   NO-UNDO.
DEF VAR iTelleNr   AS INT    NO-UNDO.
DEF VAR iHtFilId   AS INT    NO-UNDO.
DEF VAR iParaNr    AS INT    NO-UNDO.
DEFINE VARIABLE fMvaKr AS DECIMAL NO-UNDO.
DEFINE VARIABLE fDbKr  AS DECIMAL NO-UNDO.

DEFINE VARIABLE iLnr          AS INTEGER NO-UNDO.
DEFINE VARIABLE fPkSdlLinjeId AS DECIMAL.

DEF BUFFER bSysPara FOR SysPara.

BUTIKKLOOP:
DO:
    ASSIGN
      fPkSdlId      = 0
      iLnr          = 0
      fPkSdlLinjeId = 0.

    /* Oppretter Pakkseddel for varemottak */
    PKSDLHODE:
    DO:
        FIND LAST PkSdlHode NO-LOCK NO-ERROR.
        CREATE PkSdlHode.
        ASSIGN PkSdlHode.PkSdlStatus    = 10
               PkSdlHode.SendtDato      = TODAY
               fPkSdlId                 = PkSdlHode.PkSdlId
               PkSdlHode.Merknad        = "Via fakturamodul. " + CHR(13) + 
                                          'Overført fra butikk ' + STRING(TT_OvBuffer.ButikkNrFra) + '.' + 
                                          ' til ' + STRING(TT_OvBuffer.ButikkNrTil) + 
                                           (IF AVAILABLE FakturaHode THEN ' (Faktura: ' + LEFT-TRIM(STRING(FakturaHode.FakturaNr),'0') + ')' ELSE '') +
                                           '.' 
               PkSdlHode.CL             = iCl
               PkSdlHode.PkSdlNr        = IF (AVAILABLE OvBunt AND OvBunt.Opphav = 10) THEN 
                                            LEFT-TRIM(STRING(OvBunt.BuntNr),'0')
                                          ELSE IF AVAILABLE FakturaHode THEN 
                                            LEFT-TRIM(STRING(FakturaHode.FakturaNr),'0')
                                          ELSE IF cPkSdlNr <> '' THEN 
                                            cPkSdlNr
                                          ELSE 
                                            LEFT-TRIM(REPLACE(STRING(TODAY,"99/99/99"),'/',''),'0') + REPLACE(STRING(TIME,"HH:MM:SS"),':','')
               PkSdlHode.EkstId         = IF AVAILABLE FakturaHode THEN 
                                            STRING(FakturaHode.FakturaNr) 
                                          ELSE 
                                            cPkSdlNr /*STRING(plFaktura_Id)*/
               PkSdlHode.FakturaNr      = (IF AVAILABLE FakturaHode THEN FakturaHode.FakturaNr ELSE PkSdlHode.FakturaNr)
               PkSdlHode.LevNr          = iLevNr
               PkSdlHode.PkSdlOpphav    = iPkSdlOpphav
               /* Opphav 4, 5 og 6 kommer alle fra xoverforbong. Opphav 7 er ikke lenger i bruk. Bytte av butikk. */
               /* Opphav 4 og 5 er overskuddsvarer. Opphav 6 er ventelager eCom.                                  */
               PkSdlHode.Overskuddsvarer = IF CAN-DO('4,5',STRING(PkSdlHode.PkSdlOpphav)) THEN TRUE ELSE FALSE 
               PksdlHode.ButikkNr       = itilButNr
               .
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Pakkseddel PakkseddelNr satt til: ' + PkSdlHode.PkSdlNr  
            ).    
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Pakkseddel FakturaNr satt til: ' + STRING(PkSdlHode.FakturaNr)  
            ).    

        IF AVAILABLE FakturaHode THEN 
        DO:
            FIND CURRENT FakturaHode EXCLUSIVE-LOCK.
            ASSIGN 
                FakturaHode.PkSdlNr = PkSdlHode.PkSdlNr.
            FIND CURRENT FakturaHode NO-LOCK.
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  Faktura PakkseddelNr satt til: ' + PkSdlHode.PkSdlNr  
              ).    
        END.
    END. /* PKSDLHODE */

    /* Setter linjeId. */
    FIND LAST PkSdlLinje NO-LOCK
         WHERE PkSdlLinje.PkSdlId = fPkSdlId
         NO-ERROR.
    fPkSdlLinjeId = IF AVAIL PkSdlLinje THEN 
      PkSdlLinje.PkSdlLinjeId + 1 ELSE 1.

    OPPRETT_LINJER:                    
    FOR EACH tt_OvBuffer NO-LOCK WHERE:

        FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = tt_OvBuffer.ArtikkelNr NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN NEXT.
        FIND FIRST StrKonv NO-LOCK WHERE
            StrKonv.Storl = tt_OvBuffer.Storl NO-ERROR.
        IF NOT AVAILABLE StrKonv THEN NEXT.

        FIND FIRST Strekkode NO-LOCK WHERE
            Strekkode.ArtikkelNr = ArtBas.ArtikkelNr AND
            Strekkode.StrKode    = StrKonv.StrKode AND 
            LENGTH(Strekkode.Kode) = 13 AND 
            NOT Strekkode.Kode BEGINS '02' 
            NO-ERROR.
        IF NOT AVAILABLE Strekkode THEN 
            FIND FIRST Strekkode NO-LOCK WHERE
                Strekkode.ArtikkelNr = ArtBas.ArtikkelNr AND
                Strekkode.StrKode    = StrKonv.StrKode 
                NO-ERROR.
        IF NOT AVAILABLE Strekkode THEN
        DO: 
            RUN genEAN (ArtBas.ArtikkelNr,StrKonv.Storl). 
            FIND FIRST Strekkode NO-LOCK WHERE
                Strekkode.ArtikkelNr = ArtBas.ArtikkelNr AND
                Strekkode.StrKode    = StrKonv.StrKode 
                NO-ERROR.
        END.  
        CREATE PkSdlLinje.
        ASSIGN iLnr                     = iLnr + 1 
               PkSdlLinje.Linjenr       = iLnr
               PkSdlLinje.PkSdlLinjeId  = fPkSdlLinjeId
               PkSdlLinje.PkSdlId       = fPkSdlId
               PkSdlLinje.ArtikkelNr    = (IF AVAILABLE ArtBas THEN ArtBas.ArtikkelNr ELSE 0)
               PkSdlLinje.BestNr        = 0
               PkSdlLinje.OrdreNr       = 0
               PkSdlLinje.Beskr         = (IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE '')
               PkSdlLinje.LevFargKod    = (IF AVAILABLE ArtBas THEN ArtBas.LevFargKod ELSE '')
               PkSdlLinje.Antall        = tt_OvBuffer.Antall
               PkSdlLinje.AntLevert     = tt_OvBuffer.Antall
               PkSdlLinje.LevKod        = ArtBas.LevKod
               PkSdlLinje.LevNr         = ArtBas.LevNr
               PkSdlLinje.StrKode       = (IF AVAILABLE StrKonv THEN StrKonv.StrKode ELSE 0)
               PkSdlLinje.Kode          = (IF AVAILABLE Strekkode THEN Strekkode.Kode ELSE '')
               PkSdlLinje.Salgsenhet    = ArtBas.SalgsEnhet
               PkSdlLinje.ButikkNr      = iTilbutNr
               PkSdlLinje.Pakke         = FALSE 
               PkSdlLinje.PakkeNr       = 0
               fPkSdlLinjeId            = fPkSdlLinjeId + 1
               .
                
        FIND PkSdlPris EXCLUSIVE-LOCK WHERE
          PkSdlPris.PkSdlId    = fPkSdlId AND
          PkSdlPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
        IF NOT AVAILABLE PkSdlPris THEN 
        DO:
            CREATE PkSdlPris.
            ASSIGN
                PkSdlPris.PkSdlId    = fPkSdlId
                PkSdlPris.ArtikkelNr = ArtBas.ArtikkelNr.        
            BUFFER-COPY ArtBas   
              EXCEPT    ArtikkelNr BrukerId EDato Etid RegistrertDato RegistrertTid RegistrertAv 
              TO        PkSdlPris.
        END.

        /* Henter kalkylen fra butikken det overføres fra. */
        FIND FIRST ArtPris NO-LOCK
             WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr
               AND ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
            FIND FIRST ArtPris NO-LOCK
                 WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr
                 AND ArtPris.ProfilNr     = iClProfilNr NO-ERROR.
        
        /* GENERELT - Alle tar utgangspunkt i innkjøpspris fra kalkylen.      */
        /*            Denne er lik for alle profiler. Rabatt% avviker.        */
        /*          - Mva beregnes hvis ikke kunden er seatt opp som mva fri. */
        
        /* 1. Overføring fra vanlig butikk til sentrallager.      */
        /*    Butikken skal ha 10% rabatt og MVA på faktura.      */
        /*    Pakkseddel som legges opp på Outlet, skal ha 50%.   */
        
        /* 2. Overføring mellom vanlige butikker.            */
        /*    Vanlige butikker får ikke overføre til Outlet. */
        /*    Butikken skal ha 10% rabatt. Mva beregnes.     */
        
        /* 3. Overføring mellom Outlet butikker.           */
        /*    Butikken skal ha 50% rabatt og være mva fri. */
        
        /* 4. Pakkseddel fra overskuddslager til Outlet. Oerskuddsvarer. */
        /*    Butikken skal ha 50%. Mva fritt.                           */
        
        /* 5. Pakkseddel fra ventelager til nettbutikkens lager                  */
        /*    Butikken skal ha 10% rabatt.                                       */
        /*    NB: Her er det ikke utstedt faktura når varene ble                 */
        /*        overført til ventelageret.                                     */
        /*    Pakkseddel som legges opp på Nettbutikk's lager har samme rabatt%. */
        
        /* 6. Ved overføring til en +/- butikk, benyttes kalkylen i fra butikken. */

        /* Ref. regel 6. */
        IF CAN-DO(cPlussMinusButLst,STRING(iTilButNr)) THEN
        PLUSSMINUSBUTIKKER: 
        DO:
            ASSIGN 
               PkSdlPris.VareKost       = ArtPris.VareKost[1]
               PkSdlPris.Rab1%          = ArtPris.Rab1%[1]
               PkSdlPris.Pris           = ArtPris.Pris[1]
               PkSdlPris.Frakt          = ArtPris.Frakt[1]
               PkSdlPris.Db%            = ArtPris.Db%[1]
               PkSdlPris.InnkjopsPris   = ArtPris.InnkjopsPris[1]
               PkSdlPris.OverstyrPris   = YES
               .
    
            ASSIGN 
                /* Gammel pris skal gjelde i butikken */
                PkSdlPris.NyPris         = ArtPris.Pris[1] 
                PkSdlPris.NyVarekost     = ArtPris.VareKost[1]
                PkSdlPris.NyRab1%        = ArtPris.Rab1%[1]
                PkSdlPris.NyInnkjopsPris = ArtPris.InnkjopsPris[1]
                PkSdlPris.NyFrakt        = ArtPris.Frakt[1]
                PkSdlPris.NyDB%          = ArtPris.Db%[1]
               .
        END. /* PLUSSMINUSBUTIKKER */
        ELSE 
        ALLEANDREBUTIKKER: 
        DO:
            ASSIGN 
               PkSdlPris.VareKost       = ArtPris.VareKost[1]
               PkSdlPris.Rab1%          = ArtPris.Rab1%[1]
               PkSdlPris.Pris           = ArtPris.Pris[1]
               PkSdlPris.Frakt          = ArtPris.Frakt[1]
               PkSdlPris.Db%            = ArtPris.Db%[1]
               PkSdlPris.InnkjopsPris   = ArtPris.InnkjopsPris[1]
               PkSdlPris.OverstyrPris   = YES
               .

            ASSIGN 
                /* Ny pris som skal gjelde i butikken */
                PkSdlPris.NyPris         = ArtPris.Pris[1] - ((ArtPris.Pris[1] * lPrisRab%) / 100) 
                PkSdlPris.NyVarekost     = tt_OvBuffer.VareKost - ((tt_OvBuffer.VareKost * lRab%) / 100)
                PkSdlPris.NyRab1%        = lRab%
                PkSdlPris.NyInnkjopsPris = tt_OvBuffer.VareKost
                PkSdlPris.NyFrakt        = 0
                fMvaKr                   = PkSdlPris.NyPris - (PkSdlPris.NyPris / (1 + (ArtPris.Mva%[1] / 100)))
                fDbKr                    = PkSdlPris.NyPris - fMvaKr - PkSdlPris.NyVarekost                   
                PkSdlPris.NyDB%          = ROUND((fDbKr * 100) / (PkSdlPris.NyPris - fMvaKr),2)
                PkSdlPris.NyDB%          = IF PkSdlPris.NyDB% = ? THEN 0 ELSE PkSdlPris.NyDB%
                .
        END. /* ALLEANDREBUTIKKER */
    END. /* OPPRETT_LINJER */
    
    IF CAN-FIND(PkSdlHode WHERE 
                PkSdlHode.PkSdlId = fPkSdlId) THEN 
        RUN PkSdlSetLandedCost.p (STRING(fPkSdlId), ?, '', OUTPUT ocReturn, OUTPUT obOk).
END. /* BUTIKKLOOP */

END PROCEDURE.

PROCEDURE genEAN :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plArtikkelNr AS DEC  NO-UNDO.
  DEF INPUT PARAMETER cStorl       AS CHAR NO-UNDO.

  DEF VAR cKode AS CHAR NO-UNDO.
  
  DEFINE BUFFER bufStrKonv FOR StrKonv.
  
  FIND bufStrKonv WHERE bufStrKonv.Storl = cStorl USE-INDEX Storl NO-LOCK NO-ERROR.
  IF NOT AVAIL bufStrKonv THEN
      RETURN.
  /* Finnes det strekkode pï¿½ stï¿½rrrelsen fra fï¿½r, skal vi ikke legge opp ny. */
  IF CAN-FIND(FIRST StrekKode WHERE StrekKode.ArtikkelNr = ArtBas.ArtikkelNr AND
                              StrekKode.KodeType = 1 AND
                              StrekKode.StrKode  = bufStrKonv.StrKode
                          /*  AND StrekKode.Kode BEGINS "02" */
                              ) THEN RETURN.

  ASSIGN cKode = "02" + STRING(ArtBas.ArtikkelNr,"9999999") + STRING(bufStrKonv.StrKode,"999")
         cKode = FixChk(cKode).
  DO TRANSACTION:
    CREATE StrekKode.
    ASSIGN StrekKode.ArtikkelNr = ArtBas.ArtikkelNr
           StrekKode.Kode       = cKode
           StrekKode.KodeType   = 1 /* anvï¿½nd inte iKodeType, vi kan ha 0 */
           StrekKode.StrKode    = bufStrKonv.StrKode 
           StrekKode.VareId     = ArtBas.ArtikkelNr
        NO-ERROR.
    /* TN Koden kan finnes fra fï¿½r - 02 koder gav feilmelding. */
    IF ERROR-STATUS:ERROR THEN
    DO:
        IF AVAILABLE StrekKode THEN
            DELETE StrekKode.
    END.
    ELSE FIND CURRENT Strekkode NO-LOCK. 
  END.
  IF AVAILABLE Strekkode THEN 
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Strekkode opprettet: ' + Strekkode.Kode  
        ).    
    RELEASE Strekkode.
  END. 
END PROCEDURE.

FUNCTION FixChk RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
      DEF VAR iCount1 AS INTE NO-UNDO.
      DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
      DEF VAR iSum AS INTE NO-UNDO.
        DO iCount1 = LENGTH(cKode) TO 1 BY -1:  
            ASSIGN iMulti = IF iMulti = 1 THEN 3 ELSE 1
                   iSum = iSum + INT(SUBSTR(cKode,iCount1,1)) * iMulti.
        END.
        RETURN cKode + string((10 - iSum MODULO 10) MODULO 10).

END FUNCTION.
