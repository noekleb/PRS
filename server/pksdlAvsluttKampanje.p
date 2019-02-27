
/*------------------------------------------------------------------------
    File        : pksdlAvsluttKampanje.p
    Purpose     : Ved varemottak av pakksedler fra Gant Global, skal kampanje avsluttes. Årsak er at varemottak initierer at artikkelen går inn i en ny sesong, og skal ikke lenger selges på tilbud. Gjelder for varemottak av pakksedler for Gant global, men ikke fo rpakksedler som kommer fra overføringer mellom butikker.

    Syntax      :

    Description : På Outlet - avslutter kampanje som er aktive på varer som ligger på pakkseddelen. Sletter også artikkelen fra de kampanjene den er med på i kampanjeregisteret.

    Author(s)   : 
    Created     : Fri Aug 25 13:17:52 CEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER lPkSdlId LIKE PkSdlHode.PkSdlId NO-UNDO.

DEFINE VARIABLE cOutletLst    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iButNr        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iGantAktiv    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLogg         AS CHARACTER NO-UNDO.
DEFINE VARIABLE h_PrisKo      AS HANDLE NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
{syspara.i 22 5 2 cOutletLst}
{syspara.i 210 100 8 iGantAktiv INT}

ASSIGN 
    cLogg = 'pksdlAvsluttKampanje' + REPLACE(STRING(TODAY),'/','')
    .

/* SJekker at aktuell kunde er aktiv. */
IF iGantaktiv <> 1 THEN 
    RETURN '** Gant ikke aktiv.'.

FIND PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlId = lPkSdlId  NO-ERROR.

/* Ukjent pakkseddel */
IF NOT AVAILABLE pkSdlHode THEN 
    RETURN '** Ukjent pakkseddel'.
    
/* Bare Stock og FORWARD order fra Gant GLOBAL skal behandles. */
IF PkSdlHode.PkSdlOpphav <> 1 THEN 
    RETURN '** Pakksedel har feil opphav.'. 

/* Bare pakksedler til Outlet skal ha denne behandllingen. */
FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK  NO-ERROR.
IF NOT AVAILABLE PkSdlLinje THEN 
    RETURN '** Pakkseddel uten varelinjer.'.
IF NOT CAN-DO(cOutLetLst, STRING(PkSdlLinje.butikkNr)) THEN 
    RETURN '** Bare pakksedler fra Outlet kan behandles.'. 

/* Henter butikken for å finne prisprofilen */
FIND Butiker NO-LOCK WHERE 
    butiker.butik = PkSdlLinje.butik NO-ERROR.
IF NOT AVAILABLE butiker THEN 
    RETURN '** Ukjent butikk på pakkseddel.'.
ibutNr = Butiker.butik.

RUN bibl_loggDbFri.p (cLogg, 
    'Behandler pakkseddel - PakkseddelId: ' + STRING(PkSdlHode.PkSdlId)   
    + ' PkSdlNr: ' + PkSdlHode.PkSdlNr 
    + ' Butikk: ' + STRING(PkSdlLinje.butikkNr) 
    + '.' 
    ).
    
IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN prisko.p PERSISTENT SET h_PrisKo.
        
/* Slår av kampanje på alle artikler på pakkseddelen. */
FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK WHERE
    CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr):
    FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
    IF AVAILABLE ArtBas THEN 
    REMOVE_KAMPANJE:
    DO:
        /* Sletter 'PÅ' poster. */
        FOR EACH PrisKo EXCLUSIVE-LOCK WHERE 
            PrisKo.ArtikkelNr = PkSdlLinje.ArtikkelNr AND
            PrisKo.ProfilNr   = Butiker.ProfilNr:
                
            /* Døden */
            IF Prisko.Type = 2 THEN
            DO: 
                DELETE PrisKo.

                /* Logger resultatet */
                RUN bibl_loggDbFri.p (cLogg, 
                    'Slettet PÅ kampanjeposter : '   
                    + '   Butikk: '     + STRING(iButNr)
                    + ' ArtikkelNr/Varetekst/modellnr/farge: '
                    + STRING(PkSdlLinje.ArtikkelNr) + '/' 
                    + ArtBas.Beskr + '/'  
                    + ArtBas.LevKod + '/'  
                    + ArtBas.LevFargKod 
                    ).
            END.    
        END.
        
        /* Endrer og aktiverer 'AV' poster. */
        FOR EACH PrisKo EXCLUSIVE-LOCK WHERE 
            PrisKo.ArtikkelNr = PkSdlLinje.ArtikkelNr AND
            PrisKo.ProfilNr   = Butiker.ProfilNr:
                
            /* Aktiverer AV poster */
            IF Prisko.Type = 3 THEN
            DO: 
                ASSIGN
                    PrisKo.AktiveresDato = TODAY 
                    PrisKo.AktiveresTid  = 0
                    PrisKo.GyldigTilDato = TODAY
                    PrisKo.GyldigTilTid  = 0 
                    .
                RUN KlargjorPriskoEn IN h_PrisKo (ROWID(ArtBas)).

                /* Logger resultatet */
                RUN bibl_loggDbFri.p (cLogg, 
                    'Aktivert AV kampanjeposter: '   
                    + '   Butikk: '     + STRING(iButNr)
                    + ' ArtikkelNr/Varetekst/modellnr/farge: '
                    + STRING(PkSdlLinje.ArtikkelNr) + '/' 
                    + ArtBas.Beskr + '/'  
                    + ArtBas.LevKod + '/'  
                    + ArtBas.LevFargKod 
                    ).
            END.    
        END.
        
        /* Fjerner artikkelen fra alle kampanjer den er med i. Også behandlede kampanjer. */
        /* Årsaken er at de gjnbruker og bygger videre på gamle kampanjer.                */
        FOR EACH KampanjeLinje EXCLUSIVE-LOCK WHERE 
            KampanjeLinje.ArtikkelNr = PkSdlLinje.ArtikkelNr:

            /* Logger resultatet */
            RUN bibl_loggDbFri.p (cLogg, 
                'Slettet fra kampanje      : '   
                + ' Kampanje: ' 
                + STRING(KampanjeLinje.KampanjeId) + '  '
                + ' ArtikkelNr/Varetekst/modellnr/farge: '
                + STRING(PkSdlLinje.ArtikkelNr) + '/' 
                + ArtBas.Beskr + '/'  
                + ArtBas.LevKod + '/'  
                + ArtBas.LevFargKod 
                ).

            DELETE KampanjeLinje.
        END.
        
    END. /* REMOVE_KAMPANJE */   
END.      

IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_PrisKo.
       
