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

DEFINE INPUT  PARAMETER iArtikkelNr LIKE ArtBas.ArtikkelNr  NO-UNDO.
DEFINE INPUT  PARAMETER iKodeType   LIKE StrekKode.KodeType NO-UNDO.
DEFINE INPUT  PARAMETER cStorrelser AS   CHARACTER          NO-UNDO.
/* iKodeType 1=EAN,2=Interleave øvrigt ær den batchnummer i translogg !dirty! */
DEFINE VARIABLE iBatchNr AS INTEGER    NO-UNDO.
DEFINE VARIABLE cReturVerdi AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lNotNullNull AS LOGICAL    NO-UNDO.

DEF VAR cGenEan000     AS CHARACTER  NO-UNDO.
DEF VAR cGenInterleave AS CHAR       NO-UNDO.
DEF VAR iLandKode      AS INT        NO-UNDO.
DEF VAR iEANType       AS INT        NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fInterleave) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fInterleave Procedure 
FUNCTION fInterleave RETURNS CHARACTER
  ( INPUT dArtikkelNr AS DEC,
    INPUT iStrKode    AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixChk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FixChk Procedure 
FUNCTION FixChk RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNyttVareId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNyttVareId Procedure 
FUNCTION getNyttVareId RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-hentEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD hentEAN Procedure 
FUNCTION hentEAN RETURNS CHARACTER
  ( INPUT piEANType AS INT, 
    INPUT piLandKode AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
{syspar2.i 2 4 8 cGenEan000}
{syspara.i 2 4 17 cGenInterleave}

IF cStorrelser = "000" THEN /* om vi skall tillåta manuellt så kommer det '000' */
    cGenEan000 = "yes".

ASSIGN
    iEANType = iKodeType /* Landkoden styrer hvilken EAN nr tabell som skal benyttes. */
    .

/* Bedriftsinterne EAN13 */
IF iEANType = 1 THEN
    ASSIGN
    iKodeType = 1.
ELSE IF iEANType = 10 THEN
    ASSIGN
    iKodeType = 1.
ELSE IF iEANType = 11 THEN
    ASSIGN
    iKodeType = 1.
ELSE IF iEANType = 12 THEN
    ASSIGN
    iKodeType = 1.

IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
    IF cStorrelser = "" THEN
        ASSIGN cStorrelser = "*".
    IF cStorrelser = "TRANSLOGG" THEN DO:
        /* quick & dirty */
        ASSIGN iBatchNr  = iKodeType /* Här använder vi kodetypen som batchnr */
               iKodeType = 1.
        RUN GenFraTransLogg (iArtikkelNr).
        RETURN.
    END.
    IF cStorrelser = "HKVPI" OR cStorrelser = "HKVPI_HKINST" THEN DO:
        /* quick & dirty */
        IF cStorrelser = "HKVPI" THEN
            RUN HentFraHKVpi (iArtikkelNr).
        ELSE
            RUN HentFraHKVpiALLE (iArtikkelNr).
        RETURN.
    END.
    IF iKodeType = 0 OR iKodeType = 1 OR iKodeType = 5 THEN DO:
        IF iKodeType = 5 THEN /* Vi skall inte registrera '000' */
/*             ASSIGN lNotNullNull = TRUE. */
            ASSIGN lNotNullNull = cStorrelser <> "000".
        RUN GenEAN (iArtikkelNr).
    END.
    ELSE IF iKodeType = 2 THEN
        RUN GenInterleave (iArtikkelNr).
    ELSE IF iKodeType = 3 THEN /* Använder BestLevert */
        RUN GenFraBestLevert (iArtikkelNr). /* här använder vi parametern till BestHode.BestNr */
    ELSE IF iKodeType = 4 THEN /* Använder BestStr */
        RUN GenFraBestStr (iArtikkelNr). /* här använder vi parametern till BestHode.BestNr */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-GenEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenEAN Procedure 
PROCEDURE GenEAN :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
    DEFINE VARIABLE cStrl AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iTst  AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cKode AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cStrekUtenStrl AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cStrekMedStrl AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cStrekNoStrl AS CHARACTER  NO-UNDO. /* 000 i störrelse */

    /* Är varan HK-styrt, har vi en kod ?  */
    /* Har streckkod med iKodeType = 1 (EAN) lagts upp tidigare */
    FIND ArtBas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
    IF /*artbas.artikkelnr > 9999999 OR*/  Artbas.Sanertdato <> ? THEN
        RETURN.

    FIND Strtype OF ArtBas NO-LOCK NO-ERROR.
    IF NOT AVAIL StrType THEN RETURN "".
    /*
    ASSIGN cStrekUtenStrl = "02" + STRING(ArtBas.ArtikkelNr,"9999999")
           cStrekNoStrl   = FixChk(cStrekUtenStrl + "000").
    */
    IF ArtBas.StrTypeId <> 2 AND ArtBas.IndividType = 0 AND lNotNullNull = FALSE THEN DO:
        IF cGenEAN000 = "yes" THEN DO:
            ASSIGN cStrekNoStrl = hentEAN(iEANType,iLandKode).
            IF cStrekNoStrl = '' THEN RETURN.
            FIND StrekKode OF ArtBas WHERE StrekKode.Kode = cStrekNoStrl NO-LOCK NO-ERROR.
            IF NOT AVAIL StrekKode THEN DO:
                CREATE StrekKode.
                ASSIGN StrekKode.ArtikkelNr = ArtBas.ArtikkelNr
                       StrekKode.Kode       = cStrekNoStrl
                       StrekKode.KodeType   = 1 /* använd inte iKodeType, vi kan ha 0 */
                       StrekKode.StrKode    = 0
                       /*StrekKode.VareId     = ArtBas.ArtikkelNr*/
                    .
                /* Denne skal ALDRI ha Interleave kode TN 6/2-07 */
            END.
        END.
    END.
    IF ArtBas.StrTypeId > 2 AND iKodeType = 0 THEN
        RETURN.
    IF StrType.StrtypeId > 1 THEN
    FOR EACH StrTStr OF StrType NO-LOCK:
        DO:
        
            FIND StrKonv WHERE StrKonv.Storl = StrTStr.SoStorl USE-INDEX Storl NO-LOCK NO-ERROR.
            IF NOT AVAIL StrKonv THEN
                NEXT.
            IF CAN-FIND(FIRST StrekKode WHERE StrekKode.ArtikkelNr = ArtBas.ArtikkelNr AND
                                        StrekKode.KodeType = 1 AND
                                        StrekKode.StrKode  = StrKonv.StrKode
/*                                         AND StrekKode.Kode BEGINS "02" */
                                        ) THEN
                NEXT.
            IF NOT CAN-DO(cStorrelser,STRING(StrKonv.StrKode)) THEN
                NEXT.
            ASSIGN cStrl = STRING(StrKonv.StrKode,">999")
                   cStrekMedStrl = cStrekUtenStrl + cStrl
                   /*cKode = FixChk(cStrekMedStrl)*/
                   cKode = hentEAN(iEANType,iLandKode).
            IF cKode = '' THEN RETURN.
            IF CAN-FIND(strekkode WHERE strekkode.kode = cKode) THEN
                NEXT.
            CREATE StrekKode.
            ASSIGN StrekKode.ArtikkelNr        = ArtBas.ArtikkelNr
                   StrekKode.Kode              = cKode
                   StrekKode.KodeType          = 1 /* använd inte iKodeType, vi kan ha 0 */
                   StrekKode.StrKode           = StrKonv.StrKode 
                   /*StrekKode.VareId            = IF ArtBas.ArtikkelNr <= 9999999 THEN ArtBas.ArtikkelNr ELSE 0*/
                NO-ERROR.
            IF Strekkode.BestillingsNummer = "" THEN
                StrekKode.BestillingsNummer = fInterleave(ArtBas.ArtikkelNr,StrKonv.StrKode)
                .
            /* TN Koden kan finnes fra før - 02 koder gav feilmelding. */
            IF ERROR-STATUS:ERROR THEN
            DO:
                IF AVAILABLE StrekKode THEN
                    DELETE StrekKode.
                NEXT.
            END.
        END.
    END.
    IF NOT CAN-FIND(FIRST StrekKode OF ArtBas WHERE StrekKode.HovedNr = TRUE) THEN 
    DO:
        FIND FIRST StrekKode OF ArtBas EXCLUSIVE-LOCK WHERE
            StrekKode.StrKode = 0 AND StrekKode.KodeType = 1 NO-ERROR.
        IF NOT AVAILABLE StrekKode THEN
            FIND FIRST StrekKode OF ArtBas WHERE StrekKode.KodeType = 1 EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE Strekkode THEN
            ASSIGN StrekKode.HovedNr = TRUE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GenFraBestLevert) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenFraBestLevert Procedure 
PROCEDURE GenFraBestLevert :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER dBestNr      LIKE ArtBas.ArtikkelNr NO-UNDO.
    DEFINE VARIABLE         iLeveringsNr AS INTEGER             NO-UNDO.
    DEFINE VARIABLE         cStorl       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cStrl                AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cKode                AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cStrekUtenStrl       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cStrekMedStrl        AS CHARACTER   NO-UNDO.
    /* egentligen är Bestnr INTEGER men programmet tar artikkelnr eg decimal som parameter */
    DEFINE VARIABLE         iBestNr LIKE BestHode.bestnr   NO-UNDO.
    ASSIGN iBestNr = dBestNr NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Feil i 'GenFraBestHode'."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    FIND BestHode WHERE BestHode.BestNr = iBestNr NO-LOCK NO-ERROR.
    FIND ArtBas OF BestHode NO-LOCK NO-ERROR.
    IF /*artbas.artikkelnr > 9999999 OR*/ Artbas.Sanertdato <> ? THEN
        RETURN.
    FIND LAST BestLevert OF BestHode NO-LOCK NO-ERROR.
    IF AVAIL BestLevert THEN DO:

        ASSIGN iLeveringsNr = BestLevert.LeveringsNr
               /*cStrekUtenStrl = "02" + STRING(ArtBas.ArtikkelNr,"9999999")*/.

        FOR EACH BestLevert NO-LOCK WHERE BestLevert.BestNr        = BestHode.BestNr
                                      AND BestLevert.Leveringsnr   = iLeveringsNr
                                      AND skotex.BestLevert.Levert > 0 BREAK BY BestLevert.Storl:
            IF FIRST-OF(BestLevert.Storl) THEN DO:
                ASSIGN cStorl = BestLevert.Storl.
                RUN FixStorl IN h_dproclib (INPUT-OUTPUT cStorl).
                FIND StrKonv WHERE StrKonv.Storl = cStorl NO-LOCK NO-ERROR.
                IF AVAIL StrKonv THEN DO:
                    IF CAN-FIND(FIRST StrekKode WHERE StrekKode.ArtikkelNr = ArtBas.ArtikkelNr AND
                                                StrekKode.KodeType = 1 AND
                                                StrekKode.StrKode  = StrKonv.StrKode) THEN
                        NEXT.
                    
                    ASSIGN cStrl = STRING(StrKonv.StrKode,">999")
                           /*cStrekMedStrl = cStrekUtenStrl + cStrl*/
                           /*cKode = FixChk(cStrekMedStrl)*/
                           iEANType = 1
                           cKode = hentEAN(iEANType,iLandKode).
                    IF cKode = '' THEN RETURN.
                    IF CAN-FIND(strekkode WHERE strekkode.kode = cKode) THEN
                        NEXT.

                    CREATE StrekKode.
                    ASSIGN StrekKode.ArtikkelNr = ArtBas.ArtikkelNr
                           StrekKode.Kode       = cKode
                           StrekKode.KodeType   = 1
                           StrekKode.StrKode    = StrKonv.StrKode 
                           /*StrekKode.VareId     = ArtBas.ArtikkelNr*/
                        .
                    IF Strekkode.BestillingsNummer = "" THEN
                        StrekKode.BestillingsNummer = fInterleave(ArtBas.ArtikkelNr,StrKonv.StrKode).
                END.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GenFraBestStr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenFraBestStr Procedure 
PROCEDURE GenFraBestStr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER dBestNr      LIKE ArtBas.ArtikkelNr NO-UNDO.
    DEFINE VARIABLE         cStorl       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cStrl                AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cKode                AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cStrekUtenStrl       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cStrekMedStrl        AS CHARACTER   NO-UNDO.
    /* egentligen är Bestnr INTEGER men programmet tar artikkelnr eg decimal som parameter */
    DEFINE VARIABLE         iBestNr LIKE BestHode.bestnr   NO-UNDO.
    ASSIGN iBestNr = dBestNr NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Feil i 'GenFraBestHode'."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    FIND BestHode WHERE BestHode.BestNr = iBestNr NO-LOCK NO-ERROR.
    FIND ArtBas OF BestHode NO-LOCK NO-ERROR.
    IF /*artbas.artikkelnr > 9999999 OR*/ Artbas.Sanertdato <> ? THEN
        RETURN.
    FIND LAST BestStr OF BestHode NO-LOCK NO-ERROR.
    /*ASSIGN cStrekUtenStrl = "02" + STRING(ArtBas.ArtikkelNr,"9999999").*/
    FOR EACH BestStr NO-LOCK WHERE BestStr.BestNr        = BestHode.BestNr
                               AND BestStr.BestStat      = BestHode.BestStat
                               AND BestStr.Bestilt       > 0 BREAK BY BestStr.Storl:
        IF FIRST-OF(BestStr.Storl) THEN DO:
            ASSIGN cStorl = BestStr.Storl.
            RUN FixStorl IN h_dproclib (INPUT-OUTPUT cStorl).
            FIND StrKonv WHERE StrKonv.Storl = cStorl NO-LOCK NO-ERROR.
            IF AVAIL StrKonv THEN DO:
                IF CAN-FIND(FIRST StrekKode WHERE StrekKode.ArtikkelNr = ArtBas.ArtikkelNr
                                              AND StrekKode.KodeType   = 1
                                              AND StrekKode.StrKode    = StrKonv.StrKode) THEN
                    NEXT.
                ASSIGN cStrl         = STRING(StrKonv.StrKode,">999")
                       /*cStrekMedStrl = cStrekUtenStrl + cStrl*/
                       /*cKode = FixChk(cStrekMedStrl)*/
                       iEANType      = 1
                       cKode         = hentEAN(iEANType,iLandKode).
                IF cKode = '' THEN RETURN.
                IF CAN-FIND(strekkode WHERE strekkode.kode = cKode) THEN
                    NEXT.
                CREATE StrekKode.
                ASSIGN StrekKode.ArtikkelNr = ArtBas.ArtikkelNr
                       StrekKode.Kode       = cKode
                       StrekKode.KodeType   = 1
                       StrekKode.StrKode    = StrKonv.StrKode 
                       /*StrekKode.VareId     = ArtBas.ArtikkelNr*/
                    .
                IF Strekkode.BestillingsNummer = "" THEN
                    StrekKode.BestillingsNummer = fInterleave(ArtBas.ArtikkelNr,StrKonv.StrKode).
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GenFraTransLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenFraTransLogg Procedure 
PROCEDURE GenFraTransLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
    DEFINE VARIABLE cStrl AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cKode AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cStrekUtenStrl AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cStrekMedStrl AS CHARACTER   NO-UNDO.
    /* Har streckkod med iKodeType = 1 (EAN) lagts upp tidigare */

    FIND ArtBas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
    IF /*artbas.artikkelnr > 9999999 OR */ Artbas.Sanertdato <> ? THEN
        RETURN.

    /*ASSIGN cStrekUtenStrl = "02" + STRING(ArtBas.ArtikkelNr,"9999999").*/
    FOR EACH Translogg WHERE TransLogg.BatchNr = iBatchNr NO-LOCK:
            FIND StrKonv WHERE StrKonv.Storl = TransLogg.Storl USE-INDEX Storl NO-LOCK NO-ERROR.
            IF NOT AVAIL StrKonv THEN
                NEXT.
            IF CAN-FIND(FIRST StrekKode WHERE StrekKode.ArtikkelNr = ArtBas.ArtikkelNr AND
                                        StrekKode.KodeType = iKodeType AND
                                        StrekKode.StrKode  = StrKonv.StrKode) THEN
                NEXT.
            ASSIGN /*cStrl = STRING(StrKonv.StrKode,">999")
                   cStrekMedStrl = cStrekUtenStrl + cStrl
                   cKode = FixChk(cStrekMedStrl)*/
                   iEANType = 1
                   cKode    = hentEAN(iEANType,iLandKode).
            IF cKode = '' THEN RETURN.
            IF CAN-FIND(strekkode WHERE strekkode.kode = cKode) THEN
                NEXT.
            CREATE StrekKode.
            ASSIGN StrekKode.ArtikkelNr = ArtBas.ArtikkelNr
                   StrekKode.Kode       = cKode
                   StrekKode.KodeType   = 1
                   StrekKode.StrKode    = StrKonv.StrKode 
                   /*StrekKode.VareId     = ArtBas.ArtikkelNr*/
                .
            IF Strekkode.BestillingsNummer = "" THEN
                StrekKode.BestillingsNummer = fInterleave(ArtBas.ArtikkelNr,StrKonv.StrKode).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GenInterleave) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenInterleave Procedure 
PROCEDURE GenInterleave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
    DEFINE VARIABLE cStrl AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iTst  AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cKode AS CHARACTER  NO-UNDO.

    FIND ArtBas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
    IF Artbas.Sanertdato <> ? THEN
        RETURN.
    IF ArtBas.LopNr = ? THEN
        RETURN.
    IF Artbas.lopnr > 9999 OR ArtBas.Vg > 999 THEN
        RETURN.
    FIND Strtype OF ArtBas NO-LOCK NO-ERROR.
    FOR EACH StrTStr OF StrType NO-LOCK:
        /* Alfanumeriske karrakterer tillates ikke i størrelsen her. */
        ASSIGN iTst = INT(StrTStr.SoStorl) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            NEXT.
        ELSE DO:
            FIND StrKonv WHERE StrKonv.Storl = StrTStr.SoStorl USE-INDEX Storl NO-LOCK NO-ERROR.
            IF NOT AVAIL StrKonv THEN
                NEXT.
            IF CAN-FIND(StrekKode WHERE StrekKode.ArtikkelNr = ArtBas.ArtikkelNr AND
                                        StrekKode.KodeType = iKodeType AND
                                        StrekKode.StrKode  = StrKonv.StrKode) THEN
                NEXT.
            ASSIGN cStrl = IF NUM-ENTRIES(StrTStr.SoStorl,".") = 2 THEN
                TRIM(REPLACE(StrTStr.SoStorl,".","")) ELSE TRIM(StrTStr.SoStorl) + "0"
                   cStrl = FILL("0",4 - LENGTH(cStrl)) + cStrl
                   cKode = STRING(ArtBas.Vg,"999")     +
                           STRING(ArtBas.LopNr,"9999") +
                           "0" +
                           cStrl.
            IF CAN-FIND(strekkode WHERE strekkode.kode = cKode) THEN
                NEXT.
            CREATE StrekKode.
            ASSIGN StrekKode.ArtikkelNr = ArtBas.ArtikkelNr
                   StrekKode.Kode       = cKode
                   StrekKode.KodeType   = 2
                   StrekKode.StrKode    = StrKonv.StrKode 
/*                    StrekKode.VareId */
                .
        END.
    END.
    IF NOT CAN-FIND(FIRST StrekKode OF ArtBas WHERE StrekKode.HovedNr = TRUE) THEN DO:
        FIND FIRST StrekKode OF ArtBas.
        ASSIGN StrekKode.HovedNr = TRUE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-HentFraHKVpi) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentFraHKVpi Procedure 
PROCEDURE HentFraHKVpi :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipArtikkelNr AS DECIMAL  NO-UNDO.
    FIND ArtBas WHERE ArtBas.Artikkelnr = ipArtikkelNr NO-LOCK NO-ERROR.
    IF NOT AVAIL ArtBas THEN
        NEXT.
    IF Artbas.Sanertdato <> ? THEN
        RETURN.
    FIND VpiArtBas WHERE VPIArtBas.EkstVPILevNr = 1 AND VpiArtBas.ArtikkelNr = ipArtikkelNr NO-LOCK NO-ERROR.
    IF NOT AVAIL vpiArtBas THEN
        RETURN.
    FOR EACH vpiStrekKode OF VpiArtBas WHERE NOT CAN-FIND(StrekKode WHERE StrekKode.Kode = vpiStrekKode.kode) NO-LOCK:
        IF LENGTH(vpiStrekKode.Kode) <> 13 THEN
            NEXT.
        IF vpiStrekKode.Kode BEGINS "02" THEN
            NEXT.
        IF NOT vpiStrekKode.Kode = FixChk(SUBSTR(vpiStrekKode.Kode,1,12)) THEN
            NEXT.
        RELEASE StrekKode.
        BUFFER-COPY vpiStrekKode TO StrekKode
            ASSIGN StrekKode.Artikkelnr = ipArtikkelNr NO-ERROR.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-HentFraHKVpiALLE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentFraHKVpiALLE Procedure 
PROCEDURE HentFraHKVpiALLE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipArtikkelNr AS DECIMAL  NO-UNDO.
    FIND ArtBas WHERE ArtBas.Artikkelnr = ipArtikkelNr NO-LOCK NO-ERROR.
    IF NOT AVAIL ArtBas THEN
        NEXT.
    IF Artbas.Sanertdato <> ? THEN
        RETURN.
    FOR EACH EkstVPILev WHERE EkstVPILev.AktivLev = TRUE:
        FIND VpiArtBas WHERE VPIArtBas.EkstVPILevNr = EkstVPILev.EkstVPILevNr AND VpiArtBas.ArtikkelNr = ipArtikkelNr NO-LOCK NO-ERROR.
        IF NOT AVAIL vpiArtBas THEN
            NEXT.
        FOR EACH vpiStrekKode OF VpiArtBas WHERE NOT CAN-FIND(StrekKode WHERE StrekKode.Kode = vpiStrekKode.kode) NO-LOCK:
            IF LENGTH(vpiStrekKode.Kode) <> 13 THEN
                NEXT.
            IF vpiStrekKode.Kode BEGINS "02" THEN
                NEXT.
            IF NOT vpiStrekKode.Kode = FixChk(SUBSTR(vpiStrekKode.Kode,1,12)) THEN
                NEXT.
            RELEASE StrekKode.
            BUFFER-COPY vpiStrekKode TO StrekKode
                ASSIGN StrekKode.Artikkelnr = ipArtikkelNr NO-ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fInterleave) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fInterleave Procedure 
FUNCTION fInterleave RETURNS CHARACTER
  ( INPUT dArtikkelNr AS DEC,
    INPUT iStrKode    AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cStrl AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iTst  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cKode AS CHARACTER  NO-UNDO.

  IF cGenInterleave = "1" THEN
  KODEBLOKK:
  DO:
    FIND ArtBas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
    IF ArtBas.LopNr = ? THEN
        LEAVE KODEBLOKK.
    FIND StrKonv WHERE 
        StrKonv.StrKode = iStrKode USE-INDEX StrKode NO-LOCK NO-ERROR.
    IF NOT AVAIL StrKonv THEN
        LEAVE KODEBLOKK.
    IF ArtBas.Vg > 999 OR ArtBas.LopNr > 9999 THEN
        LEAVE KODEBLOKK.

    ASSIGN cStrl = IF NUM-ENTRIES(StrKonv.Storl,".") = 2 THEN
        TRIM(REPLACE(StrKonv.Storl,".","")) ELSE TRIM(StrKonv.Storl) + "0"
           cStrl = FILL("0",4 - LENGTH(cStrl)) + cStrl
           cKode = STRING(ArtBas.Vg,"999")     +
                   STRING(ArtBas.LopNr,"9999") +
                   "0" +
                   cStrl.
  END. /* KODEBLOKK */

  RETURN cKode.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixChk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FixChk Procedure 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNyttVareId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNyttVareId Procedure 
FUNCTION getNyttVareId RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iVareId LIKE StrekKode.VareId    NO-UNDO.
  FIND LAST StrekKode WHERE StrekKode.Kodetype = 1 USE-INDEX VareId NO-LOCK NO-ERROR.
  ASSIGN iVareId = IF AVAIL Strekkode THEN StrekKode.VareId + 1 ELSE 100000.
  RETURN iVareId.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-hentEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION hentEAN Procedure 
FUNCTION hentEAN RETURNS CHARACTER
  ( INPUT piEANType AS INT, 
    INPUT piLandKode AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cEAN AS CHAR NO-UNDO.
  
  RUN hentEAN.p (piEANType,piLandKode,OUTPUT cEAN). 
  
  RETURN cEAN.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

