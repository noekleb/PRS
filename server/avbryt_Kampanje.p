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
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cBeskrivelse        AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE lRab%               AS DECIMAL                        FORMAT "->>9.9" NO-UNDO.
DEFINE VARIABLE dStartDato          AS DATE                           NO-UNDO.
DEFINE VARIABLE iAktiveresTid       AS INTEGER                        NO-UNDO.
DEFINE VARIABLE dSluttDato          AS DATE                           NO-UNDO.
DEFINE VARIABLE iGyldigTiltid       AS INTEGER                        NO-UNDO.
 
DEFINE VARIABLE hQuery              AS HANDLE                         NO-UNDO.
DEFINE VARIABLE ix                  AS INTEGER                        NO-UNDO.
DEFINE VARIABLE httTable            AS HANDLE                         NO-UNDO.
DEFINE VARIABLE iKampanjeId         LIKE KampanjeHode.KampanjeId NO-UNDO.
DEFINE VARIABLE iTime               AS INTEGER                        NO-UNDO.
DEFINE VARIABLE dIdag               AS DATE                           NO-UNDO.
DEFINE VARIABLE h_PrisKo            AS HANDLE                         NO-UNDO.
DEFINE VARIABLE lKPris              AS DECIMAL                        FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE bTest               AS LOG                            NO-UNDO.
DEFINE VARIABLE cLogg               AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE lPris        AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.

DEFINE VARIABLE cOptProfilbutik     AS CHARACTER                      NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

ASSIGN 
  bTest = IF SEARCH('tnc.txt') <> ? THEN TRUE ELSE FALSE 
  cLogg = 'avbryt_Kampanje.p' + REPLACE(STRING(TODAY),'/','') 
  NO-ERROR.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start' 
    ).    

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

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start' 
    ).    

IF ENTRY(1,icParam) = "?" THEN
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Entry 1 i icParam er ? - avbryter.' 
    ).    
  RETURN.
END.
{syspar2.i 5 1 1 cOptProfilbutik}
cOptProfilbutik = TRIM(cOptProfilbutik).   
RUN prisko.p PERSISTENT SET h_prisko.  
    
ASSIGN 
  iTime = TIME - 10
  dIdag = TODAY.
       
IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  cOptProfilbutik: ' + cOptProfilbutik 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  iTime: ' + STRING(iTime,"HH:MM:SS") 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  dIdag: ' + STRING(dIdag) 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  icParam: ' + icParam 
    ).    
END.       
       
IF NUM-ENTRIES(icParam) > 2 AND ENTRY(2,icParam) = "KAMPANJE" AND 
  CAN-FIND(KampanjeHode WHERE KampanjeHode.KampanjeId = INT(ENTRY(3,icParam))) THEN 
DO:
  ASSIGN 
    iKampanjeId = INT(ENTRY(3,icParam)).

  RUN StopKampanje.
END.
ELSE 
DO:
  IF ENTRY(2,icParam) = "ARTNUM" THEN
    RUN StopTilbud2.
  ELSE IF ENTRY(2,icParam) = "GJENBRUK" THEN
    DO:
      ASSIGN 
        iKampanjeId = INT(ENTRY(3,icParam)).
          
      RUN Gjenbruk.
    END.    
  ELSE IF ENTRY(2,icParam) = "KOPIOGENDRING" THEN
    DO:
      ASSIGN 
        iKampanjeId = INT(ENTRY(3,icParam)).
      IF NUM-ENTRIES(icParam) >= 4 THEN 
        ASSIGN 
          lRab%         = DEC(REPLACE(ENTRY(4,icParam),'|',','))
          cBeskrivelse  = TRIM(REPLACE(ENTRY(5,icParam),'|',','))
          dStartDato    = DATE(REPLACE(ENTRY(6,icParam),'|',','))
          iAktiveresTid = INT(REPLACE(ENTRY(7,icParam),'|',','))
          dSluttDato    = DATE(REPLACE(ENTRY(8,icParam),'|',','))
          iGyldigTilTid = INT(REPLACE(ENTRY(9,icParam),'|',','))
          .
          
      RUN KopiOgEndring.
    END.    
    ELSE IF ENTRY(2,icParam) = "RAB%" THEN
      DO:
        ASSIGN 
          iKampanjeId = INT(ENTRY(3,icParam))
          lRab%       = DEC(REPLACE(ENTRY(4,icParam),'|',','))
          .
        RUN EndreKampRab%.
      END.                         
      ELSE IF ENTRY(2,icParam) = "RABKAMP%" THEN
        DO:
          ASSIGN 
            iKampanjeId = INT(ENTRY(3,icParam))
            lRab%       = DEC(REPLACE(ENTRY(4,icParam),'|',','))
            .
          RUN EndreAktRab%.
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

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt' 
    ).    

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-EndreAktRab%) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndreAktRab% Procedure
PROCEDURE EndreAktRab%:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cAction      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cProfilLista AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ii           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lPris        AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.

  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Start EndreAktRab%' 
      ).    

  FIND KampanjeHode NO-LOCK WHERE KampanjeHode.KampanjeId = iKampanjeId NO-ERROR.    
  IF NOT AVAILABLE KampanjeHode OR KampanjeHode.Aktivert = FALSE THEN
  DO:
    ASSIGN 
      obOK     = FALSE
      ocReturn = 'Ukjent kampanje eller kampanje ikke aktivert (KampanjeId: ' + STRING(iKampanjeId) + ').'
      .
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    Error: ' + ocReturn  
        ).    
    RETURN "AVBRYT".
  END.

  /* detta kanske skall göras vid normalpris as well */
  IF KampanjeHode.NormalPris = FALSE AND cOptProfilbutik <> "" THEN 
  DO:
    ASSIGN 
      cProfilLista = STRING(Kampanjehode.profilnr).
    FIND FIRST butiker WHERE butiker.profilnr = KampanjeHode.ProfilNr NO-LOCK NO-ERROR.
    IF butiker.sentrallager = TRUE THEN 
    DO:
      FOR EACH bufButik WHERE bufButik.clbutik = butiker.butik NO-LOCK.
        IF NOT CAN-DO(cProfilLista,STRING(bufButik.Profilnr)) THEN
          cProfilLista + "," + STRING(bufButik.Profilnr).
      END.
    END.
  END.
  ELSE
    ASSIGN cProfilLista = STRING(Kampanjehode.profilnr).

  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    cProfilLista: ' + cProfilLista 
      ).    

  FOR EACH Kampanjelinje OF kampanjehode EXCLUSIVE-LOCK TRANSACTION.
    DO ii = 1 TO NUM-ENTRIES(cProfilLista):
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = KampanjeLinje.ArtikkelNr NO-ERROR.
      FIND ArtPris EXCLUSIVE-LOCK WHERE 
        ArtPris.ArtikkelNr = KampanjeLinje.ArtikkelNr AND 
        ArtPris.ProfilNr   = KampanjeLinje.ProfilNr NO-ERROR.
      IF NOT AVAILABLE ArtPris THEN 
        FIND FIRST ArtPris EXCLUSIVE-LOCK WHERE 
          ArtPris.ArtikkelNr = KampanjeLinje.ArtikkelNr NO-ERROR.
      IF AVAILABLE ArtPris THEN 
      DO:     
        /* KampanjeLinje */  
        ASSIGN
          lPris                  = KampanjeLinje.Pris[2]
          KampanjeLinje.VareKost = ArtPris.Varekost[1] 
          KampanjeLinje.Pris[2]  = ArtPris.Pris[1] + ROUND((ArtPris.Pris[1] * lRab%) / 100,2)
          KampanjeLinje.MvaKr    = ArtPris.Pris[2] - (ArtPris.Pris[2] / (1 + (ArtPris.Mva%[2] / 100)))
          Kampanjelinje.Varekost = IF Kampanjelinje.Varekost = ? THEN 0 ELSE Kampanjelinje.Varekost     
          Kampanjelinje.Pris[2]  = IF Kampanjelinje.Pris[2] = ? THEN 0 ELSE Kampanjelinje.Pris[2]     
          Kampanjelinje.MvaKr    = IF Kampanjelinje.MvaKr = ? THEN 0 ELSE Kampanjelinje.MvaKr     
          .
        /* Artpris. */
        ASSIGN 
          ArtPris.Pris[2]  = ArtPris.Pris[1] + ROUND((ArtPris.Pris[1] * lRab%) / 100,2)
          ArtPris.MvaKr[2] = ArtPris.Pris[2] - (ArtPris.Pris[2] / (1 + (ArtPris.Mva%[2] / 100)))
          ArtPris.DbKr[2]  = ArtPris.Pris[2] - ArtPris.MvaKr[2] - ArtPris.VareKost[2]
          ArtPris.Db%[2]   = ROUND((ArtPris.DbKr[2] / (ArtPris.Pris[2] - ArtPris.MvaKr[2])) * 100,2)
          ArtPris.Pris[2]  = IF ArtPris.Pris[2] = ? THEN 0 ELSE Artpris.Pris[2]     
          ArtPris.MvaKr[2] = IF ArtPris.MvaKr[2] = ? THEN 0 ELSE Artpris.MvaKr[2]     
          ArtPris.DbKr[2]  = IF ArtPris.DbKr[2] = ? THEN 0 ELSE Artpris.DbKr[2]     
          ArtPris.Db%[2]   = IF ArtPris.Db%[2] = ? THEN 0 ELSE Artpris.Db%[2]     
          .
        IF bTest THEN 
          rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    Rabatten endret på kampanjelinje: Artikkel: ' + STRING(kampanjelinje.ArtikkelNr) + 
            ' ProfilNr: ' + STRING(KampanjeLinje.ProfilNr) + 
            ' fra ' + STRING(KampanjeHode.Kamp% * -1) + 
            ' til ' + STRING(lRab% * -1) + '.' + 
            ' Ny pris: ' + STRING(KampanjeLinje.Pris[2]) + 
            ' gmlPris: ' + STRING(lPris) + '.'    
            ).    

        FIND FIRST Prisko EXCLUSIVE-LOCK WHERE 
          PrisKo.Artikkelnr    = KampanjeLinje.ArtikkelNr AND
          PrisKo.ProfilNr      = INT(ENTRY(ii,cProfilLista)) AND
          PrisKo.AktiveresDato = KampanjeHode.StartDato AND
          PrisKo.AktiveresTid  >= KampanjeHode.AktiveresTid AND                
          Prisko.Tilbud        = TRUE   AND
          PrisKo.TYPE          = 2
          NO-ERROR.
        
        IF bTest THEN 
          rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    FIND PÅ Prisko: Available' + STRING(AVAILABLE PrisKo) +  
            ' ArtikkelNr: ' + STRING(KampanjeLinje.ArtikkelNr) + 
            ' ProfilNr: ' + ENTRY(ii,cProfilLista) + 
            ' StartDato: ' + STRING(KampanjeHode.StartDato) + 
            ' AktiveresTid: ' + STRING(KampanjeHode.AktiveresTid) + 
            ' Tilbud: ' + STRING(TRUE) +
            ' Type: 2'     
            ).    
        
        IF AVAILABLE PrisKo THEN
        ENDRERAB%1:
        DO:
          ASSIGN 
            PrisKo.Pris  = ArtPris.Pris[2]
            PrisKo.MvaKr = ArtPris.MvaKr[2]
            PrisKo.DbKr  = ArtPris.DbKr[2]
            PrisKo.Db%   = ArtPris.Db%[2]
            .     
          IF bTest THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
              '    Priskøens PÅ post: ' + STRING(Prisko.ArtikkelNr) + 
              ' Levkod: ' + ArtBas.LevKod +  
              ' LevFargkod: ' + ArtBas.LevFargKod +  
              ' ProfilNr: ' + STRING(PrisKo.ProfilNr) + 
              ' Pris: ' + STRING(PrisKo.Pris) + 
              ' MvaKr: ' + STRING(PrisKo.MvaKr) + 
              ' Db%: ' + STRING(PrisKo.Db%)    
              ).    
          FIND CURRENT Prisko NO-LOCK.
        END. /* ENDRERAB%1 */
        ELSE 
        DO:
          FIND FIRST Prisko EXCLUSIVE-LOCK WHERE 
            PrisKo.Artikkelnr    = Kampanjelinje.Artikkelnr AND
            PrisKo.ProfilNr      = INT(ENTRY(ii,cProfilLista)) AND
            PrisKo.aktiveresDato = KampanjeHode.SluttDato AND 
            PrisKo.AktiveresTid  >= KampanjeHode.GyldigTilTid AND                
            Prisko.Tilbud        = TRUE AND
            PrisKo.TYPE          = 3
            NO-ERROR.

          IF bTest THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
              '    FIND PÅ Prisko: Available' + STRING(AVAILABLE PrisKo) +  
              ' ArtikkelNr: ' + STRING(KampanjeLinje.ArtikkelNr) + 
              ' ProfilNr: ' + ENTRY(ii,cProfilLista) + 
              ' StartDato: ' + STRING(KampanjeHode.SluttDato) + 
              ' AktiveresTid: ' + STRING(KampanjeHode.GyldigTilTid) + 
              ' Tilbud: ' + STRING(TRUE) +
              ' Type: 3'     
              ).    

          IF AVAILABLE PrisKo THEN
          ENDRERAB%2:
          DO:
            ASSIGN 
              PrisKo.Pris  = ArtPris.Pris[2]
              PrisKo.MvaKr = ArtPris.MvaKr[2]
              PrisKo.DbKr  = ArtPris.DbKr[2]
              PrisKo.Db%   = ArtPris.Db%[2]
              .     
            IF bTest THEN 
              rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    Priskøens AV post: ' + STRING(Prisko.ArtikkelNr) +
                ' Levkod: ' + ArtBas.LevKod +  
                ' LevFargkod: ' + ArtBas.LevFargKod +  
                ' ProfilNr: ' + STRING(PrisKo.ProfilNr) + 
                ' Pris: ' + STRING(PrisKo.Pris) + 
                ' MvaKr: ' + STRING(PrisKo.MvaKr) + 
                ' Db%: ' + STRING(PrisKo.Db%)    
                ).    
            FIND CURRENT Prisko NO-LOCK.
          END. /* ENDRERAB%2 */
        END.
      END.
    END.
  END. /* TRANSACTION */

  DO TRANSACTION:
    FIND CURRENT KampanjeHode EXCLUSIVE-LOCK.
    IF AVAILABLE KampanjeHode THEN 
    DO:
      ASSIGN 
        KampanjeHode.Notat        = 'AKTIV kampanjerabatt endret fra ' + STRING(KampanjeHode.Kamp% * -1) + ' til ' + STRING(lRab% * -1) +
                                            ' ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS") + ' av ' + USERID('Skotex') + '.' +
                                            (IF KampanjeHode.Notat = '' THEN '' ELSE CHR(10)) +
                                            KampanjeHode.Notat
        KampanjeHode.Kamp%        = lRab%
        KampanjeHode.AvslagType   = 1
        KampanjeHode.KampanjePris = 0
        .
      FIND CURRENT KampanjeHode NO-LOCK.
    END.
  END. /* TRANSACTION */

  ASSIGN 
    obOK     = TRUE
    ocReturn = 'Rabatt endret på aktivt tilbud.'
    .
      
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Slutt EndreAktRab%' 
      ).    
END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-EndreKampRab%) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndreKampRab% Procedure
PROCEDURE EndreKampRab%:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cAction      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cProfilLista AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ii           AS INTEGER   NO-UNDO.

  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Start EndreRab%' 
      ).    

  FIND KampanjeHode NO-LOCK WHERE KampanjeHode.KampanjeId = iKampanjeId NO-ERROR.    
  IF NOT AVAILABLE KampanjeHode OR KampanjeHode.Aktivert = TRUE THEN
  DO:
    ASSIGN 
      obOK     = FALSE
      ocReturn = 'Ukjent kampanje eller kampanje er aktivert (KampanjeId: ' + STRING(iKampanjeId) + ').'
      .
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    Error: ' + ocReturn  
        ).    
    RETURN "AVBRYT".
  END.

  /* detta kanske skall göras vid normalpris as well */
  IF KampanjeHode.NormalPris = FALSE AND cOptProfilbutik <> "" THEN 
  DO:
    ASSIGN 
      cProfilLista = STRING(Kampanjehode.profilnr).
    FIND FIRST butiker WHERE butiker.profilnr = KampanjeHode.ProfilNr NO-LOCK NO-ERROR.
    IF butiker.sentrallager = TRUE THEN 
    DO:
      FOR EACH bufButik WHERE bufButik.clbutik = butiker.butik NO-LOCK.
        IF NOT CAN-DO(cProfilLista,STRING(bufButik.Profilnr)) THEN
          cProfilLista + "," + STRING(bufButik.Profilnr).
      END.
    END.
  END.
  ELSE
    ASSIGN cProfilLista = STRING(Kampanjehode.profilnr).

  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    cProfilLista: ' + cProfilLista 
      ).    

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
        lPris                  = KampanjeLinje.Pris[2]
        KampanjeLinje.VareKost = ArtPris.Varekost[1] 
        KampanjeLinje.Pris[2]  = ArtPris.Pris[1] + ROUND((ArtPris.Pris[1] * lRab%) / 100,2)
        KampanjeLinje.MvaKr    = ArtPris.Pris[1] - (ArtPris.Pris[1] / (1 + (ArtPris.Mva%[1] / 100)))
        Kampanjelinje.Varekost = IF Kampanjelinje.Varekost = ? THEN 0 ELSE Kampanjelinje.Varekost     
        Kampanjelinje.Pris[2]  = IF Kampanjelinje.Pris[2] = ? THEN 0 ELSE Kampanjelinje.Pris[2]     
        Kampanjelinje.MvaKr    = IF Kampanjelinje.MvaKr = ? THEN 0 ELSE Kampanjelinje.MvaKr     
        .
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    Rabatt endret på kampanjelinje: Artikkel: ' + STRING(kampanjelinje.ArtikkelNr) + ' ProfilNr: ' + STRING(KampanjeLinje.ProfilNr) + 
          ' fra ' + STRING(KampanjeHode.Kamp% * -1) + ' til ' + STRING(lRab% * -1) + '.' + ' Ny pris: ' + STRING(KampanjeLinje.Pris[2]) + ' gmlPris: ' + STRING(lPris) + '.'    
          ).    
    END.
  END. /* TRANSACTION */

  DO TRANSACTION:
    FIND CURRENT KampanjeHode EXCLUSIVE-LOCK.
    IF AVAILABLE KampanjeHode THEN 
    DO:
      ASSIGN 
        KampanjeHode.Notat        = 'Kampanjerabatt endret fra ' + STRING(KampanjeHode.Kamp% * -1) + ' til ' + STRING(lRab% * -1) +
                                            ' ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS") + ' av ' + USERID('Skotex') + '.' +
                                            (IF KampanjeHode.Notat = '' THEN '' ELSE CHR(10)) +
                                            KampanjeHode.Notat
        KampanjeHode.Kamp%        = lRab%
        KampanjeHode.AvslagType   = 1
        KampanjeHode.KampanjePris = 0
        .
      FIND CURRENT KampanjeHode NO-LOCK.
    END.
  END. /* TRANSACTION */

  ASSIGN 
    obOK     = TRUE
    ocReturn = 'Rabatt endret på tilbud.'
    .
      
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Slutt EndreRab%' 
      ).    
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
  DEFINE VARIABLE cAction      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cProfilLista AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ii           AS INTEGER   NO-UNDO.

  FIND KampanjeHode NO-LOCK WHERE KampanjeHode.KampanjeId = iKampanjeId NO-ERROR.    
  IF NOT AVAILABLE KampanjeHode OR KampanjeHode.Aktivert = TRUE THEN
    RETURN "AVBRYT".

  /* detta kanske skall göras vid normalpris as well */
  IF KampanjeHode.NormalPris = FALSE AND cOptProfilbutik <> "" THEN 
  DO:
    ASSIGN 
      cProfilLista = STRING(Kampanjehode.profilnr).
    FIND FIRST butiker WHERE butiker.profilnr = KampanjeHode.ProfilNr NO-LOCK NO-ERROR.
    IF butiker.sentrallager = TRUE THEN 
    DO:
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
  DEFINE VARIABLE cAction       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cProfilLista  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ii            AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lokKampanjeId LIKE KampanjeHode.KampanjeId NO-UNDO.
    
  FIND LAST bufKampanjeHode NO-LOCK USE-INDEX KampanjeId NO-ERROR.
  IF AVAILABLE bufKampanjeHode
    THEN lokKampanjeId = bufKampanjeHode.KampanjeId + 1.
  ELSE 
    lokKampanjeId = 1.

  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Start Gjennbruk' 
      ).    
    
  FIND KampanjeHode NO-LOCK WHERE KampanjeHode.KampanjeId = iKampanjeId NO-ERROR.    
  IF NOT AVAILABLE KampanjeHode OR KampanjeHode.Aktivert = FALSE THEN
    RETURN "AVBRYT".

  /* detta kanske skall göras vid normalpris as well */
  IF KampanjeHode.NormalPris = FALSE AND cOptProfilbutik <> "" THEN 
  DO:
    ASSIGN 
      cProfilLista = STRING(Kampanjehode.profilnr).
    FIND FIRST butiker WHERE butiker.profilnr = KampanjeHode.ProfilNr NO-LOCK NO-ERROR.
    IF butiker.sentrallager = TRUE THEN 
    DO:
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
      bufKampanjeHode.KampanjeId   = lokKampanjeId
      bufKampanjeHode.Beskrivelse  = cBeskrivelse
      bufKampanjeHode.Aktivert     = FALSE
      bufKampanjeHode.Komplett     = FALSE
      bufKampanjeHode.StartDato    = dStartDato 
      bufKampanjeHode.SluttDato    = dSluttDato
      bufKampanjeHode.AktiveresTid = iAktiveresTid 
      bufKampanjeHode.Kamp%        = lRab%
      bufKampanjeHode.GyldigTilTid = iGyldigTilTid 
      bufKampanjeHode.Notat        = 'Kopiert fra kampanje ' + STRING(KampanjeHode.KampanjeId) + ' ' + KampanjeHode.Beskrivelse + ' ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS") 
      .   
    FIND CURRENT bufKampanjeHode NO-LOCK.
    ASSIGN 
      ocReturn = 'Ny kampanje ' +  STRING(bufKampanjeHode.KampanjeId) + 
                     ' kopiert fra kampanje ' + STRING(KampanjeHode.KampanjeId) + ' ' + KampanjeHode.Beskrivelse + '.'.
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
                
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    Kampanjelinje kopiert: Artikkel: ' + STRING(kampanjelinje.ArtikkelNr) + ' ProfilNr: ' + STRING(KampanjeLinje.ProfilNr)  
        ).    
                
  END. /* TRANSACTION */

  obOK = TRUE.

  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Slutt Gjennbruk' 
      ).    
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-KopiOgEndring) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopiOgEndring Procedure
PROCEDURE KopiOgEndring:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cAction       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cProfilLista  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ii            AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lokKampanjeId LIKE KampanjeHode.KampanjeId NO-UNDO.
    
  FIND LAST bufKampanjeHode NO-LOCK USE-INDEX KampanjeId NO-ERROR.
  IF AVAILABLE bufKampanjeHode
    THEN lokKampanjeId = bufKampanjeHode.KampanjeId + 1.
  ELSE 
    lokKampanjeId = 1.

  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Start KopiOgEndring' 
      ).    
    
  FIND KampanjeHode NO-LOCK WHERE KampanjeHode.KampanjeId = iKampanjeId NO-ERROR.    
  IF NOT AVAILABLE KampanjeHode OR KampanjeHode.Aktivert = FALSE THEN
    RETURN "AVBRYT".

  /* detta kanske skall göras vid normalpris as well */
  IF KampanjeHode.NormalPris = FALSE AND cOptProfilbutik <> "" THEN 
  DO:
    ASSIGN 
      cProfilLista = STRING(Kampanjehode.profilnr).
    FIND FIRST butiker WHERE butiker.profilnr = KampanjeHode.ProfilNr NO-LOCK NO-ERROR.
    IF butiker.sentrallager = TRUE THEN 
    DO:
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
      bufKampanjeHode.KampanjeId   = lokKampanjeId
      bufKampanjeHode.Beskrivelse  = cBeskrivelse
      bufKampanjeHode.Aktivert     = FALSE
      bufKampanjeHode.Komplett     = FALSE
      bufKampanjeHode.StartDato    = dStartDato 
      bufKampanjeHode.SluttDato    = dSluttDato
      bufKampanjeHode.AktiveresTid = iAktiveresTid 
      bufKampanjeHode.Kamp%        = lRab%
      bufKampanjeHode.GyldigTilTid = iGyldigTilTid 
      bufKampanjeHode.Notat        = 'Kopiert fra kampanje ' + STRING(KampanjeHode.KampanjeId) + ' ' + KampanjeHode.Beskrivelse + ' ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS") 
      .   
    FIND CURRENT bufKampanjeHode NO-LOCK.
    ASSIGN 
      ocReturn = 'Ny kampanje ' +  STRING(bufKampanjeHode.KampanjeId) + 
                     ' kopiert fra kampanje ' + STRING(KampanjeHode.KampanjeId) + ' ' + KampanjeHode.Beskrivelse + '.'.
  END.

  FOR EACH Kampanjelinje OF kampanjehode EXCLUSIVE-LOCK TRANSACTION.
    FIND ArtPris NO-LOCK WHERE 
      ArtPris.ArtikkelNr = KampanjeLinje.ArtikkelNr AND 
      ArtPris.ProfilNr   = KampanjeLinje.ProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
      FIND FIRST ArtPris NO-LOCK WHERE 
        ArtPris.ArtikkelNr = KampanjeLinje.ArtikkelNr NO-ERROR.
    CREATE bufKampanjeLinje.
    BUFFER-COPY KampanjeLinje
      EXCEPT KampanjeId Behandlet
      TO bufKampanjeLinje
      ASSIGN 
      bufKampanjeLinje.KampanjeId = bufKampanjeHode.KampanjeId
      bufKampanjeLinje.Behandlet  = FALSE
      .
                
      ASSIGN
        lPris                     = KampanjeLinje.Pris[2]
        bufKampanjeLinje.VareKost = ArtPris.Varekost[1] 
        bufKampanjeLinje.Pris[2]  = ArtPris.Pris[1] + ROUND((ArtPris.Pris[1] * lRab%) / 100,2)
        bufKampanjeLinje.MvaKr    = ArtPris.Pris[1] - (ArtPris.Pris[1] / (1 + (ArtPris.Mva%[1] / 100)))
        bufKampanjelinje.Varekost = IF bufKampanjelinje.Varekost = ? THEN 0 ELSE bufKampanjelinje.Varekost     
        bufKampanjelinje.Pris[2]  = IF bufKampanjelinje.Pris[2] = ? THEN 0 ELSE bufKampanjelinje.Pris[2]     
        bufKampanjelinje.MvaKr    = IF bufKampanjelinje.MvaKr = ? THEN 0 ELSE bufKampanjelinje.MvaKr     
        .
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    Linje kopiert og rabatt endret på kampanjelinje: Artikkel: ' + STRING(bufkampanjelinje.ArtikkelNr) + ' ProfilNr: ' + STRING(bufKampanjeLinje.ProfilNr) + 
          ' fra ' + STRING(KampanjeHode.Kamp% * -1) + ' til ' + STRING(lRab% * -1) + '.' + ' Ny pris: ' + STRING(bufKampanjeLinje.Pris[2]) + ' gmlPris: ' + STRING(lPris) + '.'    
          ).    
  END. /* TRANSACTION */

  ASSIGN 
    obOK = TRUE
    .
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Slutt KopiOgEndring' 
      ).    

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
  DEFINE VARIABLE cAction      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cProfilLista AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ii           AS INTEGER   NO-UNDO.

  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Start StoppKampanje' 
      ).    

  FIND KampanjeHode NO-LOCK WHERE KampanjeHode.KampanjeId = iKampanjeId NO-ERROR.    
  IF NOT AVAILABLE KampanjeHode OR KampanjeHode.Aktivert = FALSE THEN
    RETURN "AVBRYT".
  /* Detta kanske skall göras vid normalpris as well */
  IF KampanjeHode.NormalPris = FALSE AND cOptProfilbutik <> "" THEN 
  DO:
    ASSIGN 
      cProfilLista = STRING(Kampanjehode.profilnr).
    FIND FIRST butiker WHERE butiker.profilnr = KampanjeHode.ProfilNr NO-LOCK NO-ERROR.
    IF butiker.sentrallager = TRUE THEN 
    DO:
      FOR EACH bufButik WHERE bufButik.clbutik = butiker.butik NO-LOCK.
        IF NOT CAN-DO(cProfilLista,STRING(bufButik.Profilnr)) THEN
          cProfilLista + "," + STRING(bufButik.Profilnr).
      END.
    END.
  END.
  ELSE
    ASSIGN cProfilLista = STRING(Kampanjehode.profilnr).
    
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    cProfilLista: ' + cProfilLista 
      ).    

  FOR EACH Kampanjelinje OF kampanjehode EXCLUSIVE-LOCK TRANSACTION.
    /* Avslutte TILBUD */
    IF KampanjeHode.NormalPris = FALSE THEN
    DO ii = 1 TO NUM-ENTRIES(cProfilLista):
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    Normalpris - ProfilNr: ' + ENTRY(ii,cProfilLista) 
          ).    
          
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

      ELSE 
      DO:
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
            PrisKo.AktiveresDato = diDag
            PrisKo.aktiveresTid  = iTime.
          FIND CURRENT Prisko NO-LOCK.

          IF bTest THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
              '    Priskø deaktiveres: Artikkel: ' + STRING(PrisKo.ArtikkelNr) + ' ProfilNr: ' + STRING(PrisKo.ProfilNr) + ' ' + STRING(PrisKo.AktiveresDato) + ' ' + STRING(PrisKo.aktiveresTid,"HH:MM:SS")  
              ).    

          /* Klargjør priskø for artikkelen. */
          RUN KlargjorPrisKoEn IN h_PrisKo (ROWID(ArtBas)).                        
        END.
      END.
    END.
    /* NORMALPRIS */
    ELSE 
    DO:
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    Normalpris'  
          ).    
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
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    Leverandørkampanje'  
          ).    
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

      ELSE 
      DO:
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
            PrisKo.AktiveresDato = diDag
            PrisKo.aktiveresTid  = iTime.
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
      KampanjeHode.Notat    = 'DeAktivert ' + STRING(NOW,"99/99/99 HH:MM:SS") + ' av ' + USERID('SkoTex') + 
                                    (IF KampanjeHode.Notat <> '' THEN CHR(10) ELSE '') + 
                                    KampanjeHode.Notat
      .

    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    Stempler kampanjehode: ' + STRING(KampanjeHode.Aktivert) + ' ' + KampanjeHode.Notat  
        ).    
        
    FIND CURRENT KampanjeHode NO-LOCK.
  END. /* TRANSACTION */

  obOK = TRUE.
    
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Slutt StoppKampanje' 
      ).    
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
  IF NOT VALID-HANDLE(ihBuffer) AND NUM-ENTRIES(icParam) > 1 THEN 
  DO:
    CREATE TEMP-TABLE httTable.
    httTable:ADD-LIKE-FIELD("ArtikkelNr","ArtBas.ArtikkelNr").
    httTable:TEMP-TABLE-PREPARE("ttArtBas").
    ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.
    IF ENTRY(2,icParam) = "ROWID" THEN
    DO ix = 3 TO NUM-ENTRIES(icParam):
      FIND ArtBas WHERE ROWID(ArtBas) = TO-ROWID(ENTRY(ix,icParam)) NO-LOCK NO-ERROR.
      IF AVAILABLE ArtBas THEN 
      DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER ArtBas:HANDLE).
      END.
    END.
    ELSE
    DO ix = 3 TO NUM-ENTRIES(icParam):
      FIND ArtBas WHERE ArtBas.ArtikkelNr = DEC(ENTRY(ix,icParam)) NO-LOCK NO-ERROR.
      IF AVAILABLE ArtBas THEN 
      DO:
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
        ELSE 
        DO:
          FIND bArtPris WHERE ROWID(bArtPris) = ROWID(ArtPris).
          ASSIGN 
            bArtPris.Tilbud = FALSE.
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
      ELSE 
      DO:
        FIND bArtPris WHERE ROWID(bArtPris) = ROWID(ArtPris).
        ASSIGN 
          bArtPris.Tilbud = FALSE.
        RELEASE bArtPris.
      END.
    END.
  END.

  IF ocReturn = "" THEN obOk = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

