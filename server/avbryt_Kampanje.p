&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* vid kampanje skall icParam vara ",KAMPANJE,kampanjeid" */
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.
 
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR ix          AS INT NO-UNDO.
DEF VAR httTable    AS HANDLE NO-UNDO.
DEF VAR iKampanjeId LIKE KampanjeHode.KampanjeId    NO-UNDO.
DEFINE VARIABLE iTime    AS INTEGER    NO-UNDO.
DEFINE VARIABLE dIdag    AS DATE       NO-UNDO.
DEFINE VARIABLE h_PrisKo AS HANDLE NO-UNDO.
DEFINE VARIABLE lRab% AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE lKPris AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.

DEFINE VARIABLE cOptProfilbutik     AS CHARACTER   NO-UNDO.

DEFINE BUFFER bufButik         FOR butiker.
DEFINE BUFFER bufKampanjeHode  FOR KampanjeHode.
DEFINE BUFFER bufKampanjeLinje FOR KampanjeLinje.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

IF ENTRY(1,icParam) = "?" THEN
    RETURN.
{syspar2.i 5 1 1 cOptProfilbutik}
cOptProfilbutik = TRIM(cOptProfilbutik).   
RUN prisko.p PERSISTENT SET h_prisko.  
    
ASSIGN iTime  = TIME - 10
       dIdag  = TODAY.
IF NUM-ENTRIES(icParam) > 2 AND ENTRY(2,icParam) = "KAMPANJE" AND 
    CAN-FIND(KampanjeHode WHERE KampanjeHode.KampanjeId = INT(ENTRY(3,icParam))) THEN DO:
    ASSIGN iKampanjeId = INT(ENTRY(3,icParam)).

    RUN StopKampanje.
END.
ELSE DO:
    IF ENTRY(2,icParam) = "ARTNUM" THEN
        RUN StopTilbud2.
    ELSE IF ENTRY(2,icParam) = "GJENBRUK" THEN
    DO:
        ASSIGN iKampanjeId = INT(ENTRY(3,icParam)).
        RUN Gjenbruk.
    END.    
    ELSE IF ENTRY(2,icParam) = "RAB%" THEN
    DO:
        ASSIGN 
            iKampanjeId = INT(ENTRY(3,icParam))
            lRab%       = DEC(REPLACE(ENTRY(4,icParam),'|',','))
            .
        RUN EndreRab%.
    END.    
    ELSE IF ENTRY(2,icParam) = "KPRIS" THEN
    DO:
        ASSIGN 
            iKampanjeId = INT(ENTRY(3,icParam))
            lKPris      = DEC(REPLACE(ENTRY(4,icParam),'|',','))
            .
        RUN EndreKampPris.
    END.
    ELSE
        RUN StopTilbud.
END.

IF VALID-HANDLE(h_Prisko) THEN 
  DELETE PROCEDURE h_prisko.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-"EndreRab%") = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE "EndreRab%" Procedure
PROCEDURE "EndreRab%":
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cAction AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cProfilLista AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER     NO-UNDO.

    FIND KampanjeHode NO-LOCK WHERE KampanjeHode.KampanjeId = iKampanjeId NO-ERROR.    
    IF NOT AVAIL KampanjeHode OR KampanjeHode.Aktivert = TRUE THEN
        RETURN "AVBRYT".

    /* detta kanske skall göras vid normalpris as well */
    IF KampanjeHode.NormalPris = FALSE AND cOptProfilbutik <> "" THEN DO:
        ASSIGN cProfilLista = STRING(Kampanjehode.profilnr).
        FIND FIRST butiker WHERE butiker.profilnr = KampanjeHode.ProfilNr NO-LOCK NO-ERROR.
        IF butiker.sentrallager = TRUE THEN DO:
            FOR EACH bufButik WHERE bufButik.clbutik = butiker.butik NO-LOCK.
                IF NOT CAN-DO(cProfilLista,STRING(bufButik.Profilnr)) THEN
                    cProfilLista + "," + STRING(bufButik.Profilnr).
            END.
        END.
    END.
    ELSE
        ASSIGN cProfilLista = STRING(Kampanjehode.profilnr).

    FOR EACH Kampanjelinje OF kampanjehode EXCLUSIVE-LOCK TRANSACTION.
        FIND ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = KampanjeLinje.ArtikkelNr AND 
            ArtPris.ProfilNr   = KampanjeLinje.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
            FIND FIRST ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = KampanjeLinje.ArtikkelNr NO-ERROR.
        IF AVAILABLE ArtPris THEN 
        DO:
            ASSIGN
                KampanjeLinje.VareKost = ArtPris.Varekost[1] 
                KampanjeLinje.Pris[2]  = ArtPris.Pris[1] + ROUND((ArtPris.Pris[1] * lRab%) / 100,2)
                KampanjeLinje.MvaKr    = ArtPris.Pris[1] - (ArtPris.Pris[1] / (1 + (ArtPris.Mva%[1] / 100)))
                .
        END.
    END. /* TRANSACTION */

    DO TRANSACTION:
        FIND CURRENT KampanjeHode EXCLUSIVE-LOCK.
        IF AVAILABLE KampanjeHode THEN 
        DO:
            ASSIGN 
                KampanjeHode.Kamp%        = lRab%
                KampanjeHode.AvslagType   = 1
                KampanjeHode.KampanjePris = 0
                .
            FIND CURRENT KampanjeHode NO-LOCK.
        END.
    END. /* TRANSACTION */

    obOK = TRUE.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-EndreKampPris) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndreKampPris Procedure
PROCEDURE EndreKampPris:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cAction AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cProfilLista AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER     NO-UNDO.

    FIND KampanjeHode NO-LOCK WHERE KampanjeHode.KampanjeId = iKampanjeId NO-ERROR.    
    IF NOT AVAIL KampanjeHode OR KampanjeHode.Aktivert = TRUE THEN
        RETURN "AVBRYT".

    /* detta kanske skall göras vid normalpris as well */
    IF KampanjeHode.NormalPris = FALSE AND cOptProfilbutik <> "" THEN DO:
        ASSIGN cProfilLista = STRING(Kampanjehode.profilnr).
        FIND FIRST butiker WHERE butiker.profilnr = KampanjeHode.ProfilNr NO-LOCK NO-ERROR.
        IF butiker.sentrallager = TRUE THEN DO:
            FOR EACH bufButik WHERE bufButik.clbutik = butiker.butik NO-LOCK.
                IF NOT CAN-DO(cProfilLista,STRING(bufButik.Profilnr)) THEN
                    cProfilLista + "," + STRING(bufButik.Profilnr).
            END.
        END.
    END.
    ELSE
        ASSIGN cProfilLista = STRING(Kampanjehode.profilnr).

    FOR EACH Kampanjelinje OF kampanjehode EXCLUSIVE-LOCK TRANSACTION.
        FIND ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = KampanjeLinje.ArtikkelNr AND 
            ArtPris.ProfilNr   = KampanjeLinje.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
            FIND FIRST ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = KampanjeLinje.ArtikkelNr NO-ERROR.
        IF AVAILABLE ArtPris THEN 
        DO:
            ASSIGN
                KampanjeLinje.VareKost = ArtPris.Varekost[1] 
                KampanjeLinje.Pris[2]  = lKPris
                KampanjeLinje.MvaKr    = ArtPris.Pris[1] - (ArtPris.Pris[1] / (1 + (ArtPris.Mva%[1] / 100)))
                .
        END.
    END. /* TRANSACTION */

    DO TRANSACTION:
        FIND CURRENT KampanjeHode EXCLUSIVE-LOCK.
        IF AVAILABLE KampanjeHode THEN 
        DO:
            ASSIGN 
                KampanjeHode.KampanjePris = lKPris
                KampanjeHode.AvslagType   = 2
                KampanjeHode.Kamp%        = 0
                .
            FIND CURRENT KampanjeHode NO-LOCK.
        END.
    END. /* TRANSACTION */
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-Gjenbruk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gjenbruk Procedure
PROCEDURE Gjenbruk:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cAction AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cProfilLista AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
    DEFINE VARIABLE lokKampanjeId LIKE KampanjeHode.KampanjeId NO-UNDO.
    
    FIND LAST bufKampanjeHode NO-LOCK USE-INDEX KampanjeId NO-ERROR.
    IF AVAILABLE bufKampanjeHode
        THEN lokKampanjeId = bufKampanjeHode.KampanjeId + 1.
    ELSE 
        lokKampanjeId = 1.
    

    FIND KampanjeHode NO-LOCK WHERE KampanjeHode.KampanjeId = iKampanjeId NO-ERROR.    
    IF NOT AVAIL KampanjeHode OR KampanjeHode.Aktivert = FALSE THEN
        RETURN "AVBRYT".

    /* detta kanske skall göras vid normalpris as well */
    IF KampanjeHode.NormalPris = FALSE AND cOptProfilbutik <> "" THEN DO:
        ASSIGN cProfilLista = STRING(Kampanjehode.profilnr).
        FIND FIRST butiker WHERE butiker.profilnr = KampanjeHode.ProfilNr NO-LOCK NO-ERROR.
        IF butiker.sentrallager = TRUE THEN DO:
            FOR EACH bufButik WHERE bufButik.clbutik = butiker.butik NO-LOCK.
                IF NOT CAN-DO(cProfilLista,STRING(bufButik.Profilnr)) THEN
                    cProfilLista + "," + STRING(bufButik.Profilnr).
            END.
        END.
    END.
    ELSE
        ASSIGN cProfilLista = STRING(Kampanjehode.profilnr).

    DO TRANSACTION:
        CREATE bufKampanjeHode.
        BUFFER-COPY KampanjeHode 
            EXCEPT KampanjeId
            TO bufKampanjeHode
            ASSIGN 
                bufKampanjeHode.KampanjeId = lokKampanjeId
                bufKampanjeHode.Aktivert     = FALSE
                bufKampanjeHode.Komplett     = FALSE
                bufKampanjeHode.StartDato    = TODAY 
                bufKampanjeHode.SluttDato    = TODAY
                bufKampanjeHode.AktiveresTid = 0 
                bufKampanjeHode.GyldigTilTid = (24 * 60 * 60) - 1 
                bufKampanjeHode.Notat        = ''
                .                
        FIND CURRENT bufKampanjeHode NO-LOCK.
    END.

    FOR EACH Kampanjelinje OF kampanjehode EXCLUSIVE-LOCK TRANSACTION.
        CREATE bufKampanjeLinje.
        BUFFER-COPY KampanjeLinje
            EXCEPT KampanjeId Behandlet
            TO bufKampanjeLinje
            ASSIGN 
                bufKampanjeLinje.KampanjeId = bufKampanjeHode.KampanjeId
                bufKampanjeLinje.Behandlet  = FALSE
                .
    END. /* TRANSACTION */

    obOK = TRUE.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-StopKampanje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StopKampanje Procedure 
PROCEDURE StopKampanje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cAction AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cProfilLista AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER     NO-UNDO.

    FIND KampanjeHode NO-LOCK WHERE KampanjeHode.KampanjeId = iKampanjeId NO-ERROR.    
    IF NOT AVAIL KampanjeHode OR KampanjeHode.Aktivert = FALSE THEN
        RETURN "AVBRYT".
    /* detta kanske skall göras vid normalpris as well */
    IF KampanjeHode.NormalPris = FALSE AND cOptProfilbutik <> "" THEN DO:
        ASSIGN cProfilLista = STRING(Kampanjehode.profilnr).
        FIND FIRST butiker WHERE butiker.profilnr = KampanjeHode.ProfilNr NO-LOCK NO-ERROR.
        IF butiker.sentrallager = TRUE THEN DO:
            FOR EACH bufButik WHERE bufButik.clbutik = butiker.butik NO-LOCK.
                IF NOT CAN-DO(cProfilLista,STRING(bufButik.Profilnr)) THEN
                    cProfilLista + "," + STRING(bufButik.Profilnr).
            END.
        END.
    END.
    ELSE
        ASSIGN cProfilLista = STRING(Kampanjehode.profilnr).
    

    FOR EACH Kampanjelinje OF kampanjehode EXCLUSIVE-LOCK TRANSACTION.
        /* Avslutte TILBUD */
        IF KampanjeHode.NormalPris = FALSE THEN
        DO ii = 1 TO NUM-ENTRIES(cProfilLista):
            FIND FIRST Prisko EXCLUSIVE-LOCK WHERE 
                PrisKo.Artikkelnr    = KampanjeLinje.ArtikkelNr AND
/*                 PrisKo.ProfilNr      = KampanjeHode.ProfilNr AND */
                PrisKo.ProfilNr      = INT(ENTRY(ii,cProfilLista)) AND
                PrisKo.AktiveresDato = KampanjeHode.StartDato AND
                PrisKo.AktiveresTid  = KampanjeHode.AktiveresTid AND                
                Prisko.Tilbud        = TRUE   AND
                PrisKo.TYPE          = 2
                NO-ERROR.
            IF AVAILABLE PrisKo THEN
              RUN SlettPrisKo IN h_PrisKo (ROWID(PrisKo)).

            ELSE DO:
                FIND FIRST Prisko EXCLUSIVE-LOCK WHERE 
                    PrisKo.Artikkelnr    = Kampanjelinje.Artikkelnr AND
/*                 PrisKo.ProfilNr      = KampanjeHode.ProfilNr AND */
                    PrisKo.ProfilNr      = INT(ENTRY(ii,cProfilLista)) AND
                    PrisKo.AktiveresTid  = KampanjeHode.GyldigTilTid AND                
                    Prisko.Tilbud        = TRUE AND
                    PrisKo.TYPE          = 3
                    NO-ERROR.
                IF AVAILABLE PrisKo THEN
                DO:
                    ASSIGN
                        PrisKo.AktiveresDato = TODAY
                        PrisKo.aktiveresTid  = TIME.
                    FIND CURRENT Prisko NO-LOCK.
                    /* Klargjør priskø for artikkelen. */
                    RUN KlargjorPrisKoEn IN h_PrisKo (ROWID(ArtBas)).                        
                END.
            END.
        END.
        /* NORMALPRIS */
        ELSE DO:
            FIND FIRST Prisko EXCLUSIVE-LOCK WHERE 
                PrisKo.Artikkelnr    = KampanjeLinje.ArtikkelNr AND
                PrisKo.ProfilNr      = KampanjeHode.ProfilNr AND
                PrisKo.AktiveresDato = KampanjeHode.StartDato AND
                PrisKo.AktiveresTid  = KampanjeHode.AktiveresTid AND                
                Prisko.Tilbud        = FALSE AND
                PrisKo.TYPE          = 1
                NO-ERROR.
            IF AVAILABLE PrisKo THEN
              RUN SlettPrisKo IN h_PrisKo (ROWID(PrisKo)).
        END.

        IF KampanjeHode.leverandorkampanje = TRUE THEN
        DO:
            FIND FIRST Prisko EXCLUSIVE-LOCK WHERE 
                PrisKo.Artikkelnr    = KampanjeLinje.ArtikkelNr AND
                PrisKo.ProfilNr      = KampanjeHode.ProfilNr AND
                PrisKo.AktiveresDato = KampanjeHode.StartDato AND
                PrisKo.AktiveresTid  = KampanjeHode.AktiveresTid AND                
                Prisko.Tilbud        = TRUE   AND
                PrisKo.TYPE          = 5
                NO-ERROR.
            IF AVAILABLE PrisKo THEN
              RUN SlettPrisKo IN h_PrisKo (ROWID(PrisKo)).

            ELSE DO:
                FIND FIRST Prisko EXCLUSIVE-LOCK WHERE 
                    PrisKo.Artikkelnr    = Kampanjelinje.Artikkelnr AND
                    PrisKo.ProfilNr      = KampanjeLinje.ProfilNr   AND
                    PrisKo.AktiveresDato = KampanjeHode.SluttDato  AND
                    PrisKo.AktiveresTid  = KampanjeHode.GyldigTilTid AND                
                    Prisko.Tilbud        = TRUE AND
                    PrisKo.TYPE          = 6
                    NO-ERROR.
                IF AVAILABLE PrisKo THEN
                DO:
                    ASSIGN
                        PrisKo.AktiveresDato = TODAY
                        PrisKo.aktiveresTid  = TIME.
                    FIND CURRENT Prisko NO-LOCK.
                    /* Klargjør priskø for artikkelen. */
                    RUN KlargjorPrisKoEn IN h_PrisKo (ROWID(ArtBas)).                        
                END.
            END.
        END.
        ASSIGN
            KampanjeLinje.Behandlet = FALSE.
    END. /* TRANSACTION */

    DO TRANSACTION:
        FIND CURRENT KampanjeHode EXCLUSIVE-LOCK.
        ASSIGN
            KampanjeHode.Aktivert = FALSE
            KampanjeHode.Komplett = FALSE
            .
        FIND CURRENT KampanjeHode NO-LOCK.
    END. /* TRANSACTION */

    obOK = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StopTilbud) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StopTilbud Procedure 
PROCEDURE StopTilbud :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER bPrisKo  FOR PrisKo.
DEFINE BUFFER bArtPris FOR ArtPris.
IF NOT VALID-HANDLE(ihBuffer) AND NUM-ENTRIES(icParam) > 1 THEN DO:
  CREATE TEMP-TABLE httTable.
  httTable:ADD-LIKE-FIELD("ArtikkelNr","ArtBas.ArtikkelNr").
  httTable:TEMP-TABLE-PREPARE("ttArtBas").
  ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.
  IF ENTRY(2,icParam) = "ROWID" THEN
    DO ix = 3 TO NUM-ENTRIES(icParam):
      FIND ArtBas WHERE ROWID(ArtBas) = TO-ROWID(ENTRY(ix,icParam)) NO-LOCK NO-ERROR.
      IF AVAIL ArtBas THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER ArtBas:HANDLE).
      END.
    END.
  ELSE
    DO ix = 3 TO NUM-ENTRIES(icParam):
      FIND ArtBas WHERE ArtBas.ArtikkelNr = DEC(ENTRY(ix,icParam)) NO-LOCK NO-ERROR.
      IF AVAIL ArtBas THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER ArtBas:HANDLE).
      END.
    END.
END.
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

DO:
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
      FOR EACH ArtPris WHERE ArtPris.ArtikkelNr = DECI(STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE)) AND
                             ArtPris.TilBud = TRUE NO-LOCK.
          FIND FIRST bPrisko EXCLUSIVE-LOCK WHERE 
              bPrisKo.Artikkelnr    = ArtPris.Artikkelnr AND
              bPrisKo.ProfilNr      = ArtPris.ProfilNr   AND
              bPrisKo.AktiveresDato = ArtPris.TilbudTilDato  AND
              bPrisKo.AktiveresTid  = ArtPris.TilbudTilTid AND                
              bPrisko.Tilbud        = TRUE AND
              bPrisKo.TYPE          = 3
              NO-ERROR.
          IF AVAILABLE bPrisKo THEN
          DO:
              ASSIGN
                  bPrisKo.AktiveresDato = TODAY
                  bPrisKo.aktiveresTid  = TIME.
          END.
          ELSE DO:
              FIND bArtPris WHERE ROWID(bArtPris) = ROWID(ArtPris).
              ASSIGN bArtPris.Tilbud = FALSE.
              RELEASE bArtPris.
          END.
      END.
      hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hQuery.

IF ocReturn = "" THEN obOk = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StopTilbud2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StopTilbud2 Procedure 
PROCEDURE StopTilbud2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER bPrisKo  FOR PrisKo.
DEFINE BUFFER bArtPris FOR ArtPris.
DO ix = 3 TO NUM-ENTRIES(icParam):
    FIND ArtBas WHERE ArtBas.ArtikkelNr = DEC(ENTRY(ix,icParam)) NO-LOCK NO-ERROR.
    FOR EACH ArtPris WHERE ArtPris.ArtikkelNr = DEC(ENTRY(ix,icParam)) AND ArtPris.TilBud = TRUE NO-LOCK.
        FIND FIRST bPrisko EXCLUSIVE-LOCK WHERE 
            bPrisKo.Artikkelnr    = ArtPris.Artikkelnr AND
            bPrisKo.ProfilNr      = ArtPris.ProfilNr   AND
            bPrisKo.AktiveresDato = ArtPris.TilbudTilDato  AND
            bPrisKo.AktiveresTid  = ArtPris.TilbudTilTid AND                
            bPrisko.Tilbud        = TRUE AND
            bPrisKo.TYPE          = 3
            NO-ERROR.
        IF AVAILABLE bPrisKo THEN
        DO:
            ASSIGN
                bPrisKo.AktiveresDato = TODAY
                bPrisKo.aktiveresTid  = TIME.
        END.
        ELSE DO:
            FIND bArtPris WHERE ROWID(bArtPris) = ROWID(ArtPris).
            ASSIGN bArtPris.Tilbud = FALSE.
            RELEASE bArtPris.
        END.
    END.
END.

IF ocReturn = "" THEN obOk = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

