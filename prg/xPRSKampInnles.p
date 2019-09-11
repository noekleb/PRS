&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xPRSKampInnles.p
    Purpose     : Innlesning av data til kampanjemodulen

    Syntax      : xPRSKampInnles.p (lFilId, h_Parent, output iantLinjer).

    Description : Leser inn data i kampanjeregisteret. Setter opp standardverdier. 

    Author(s)   : Tom Nøkleby
    Created     : 29/12/2014
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.

DEF VAR piLinjeNr AS INT  NO-UNDO.
DEF VAR pcLinje   AS CHAR NO-UNDO.
DEF VAR piAntFeil AS INT  NO-UNDO. 
DEF VAR iKampanjeId AS INT NO-UNDO.
DEF VAR iProfilNr AS INT NO-UNDO.
DEF VAR lRab%     AS DEC NO-UNDO.
DEF VAR cLogg AS CHAR NO-UNDO.

DEFINE VARIABLE lDec  AS DECIMAL NO-UNDO.
DEFINE VARIABLE cStr  AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE bOk   AS LOG NO-UNDO.

DEF STREAM InnFil.

DEFINE BUFFER bKampanjeHode  FOR KampanjeHode.
DEFINE BUFFER bKampanjeLinje FOR KampanjeLinje.
DEFINE BUFFER bufArtPris FOR ArtPris.

DEFINE TEMP-TABLE ttKampanjeHode LIKE KampanjeHode
  INDEX KampanjeId KampanjeId.
DEFINE TEMP-TABLE ttKampanjeLinje LIKE KampanjeLinje
    FIELD Beskr AS CHAR 
    FIELD LevNr AS INT 
    FIELD LevKod AS CHAR 
    FIELD LevFargKod AS CHAR
    FIELD Rab% AS DEC FORMAT "->>>9.99"
    FIELD Klar AS LOG 
  INDEX KampanjeId KampanjeId Vg LopNr.

DEFINE TEMP-TABLE ttError
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .
{windows.i}
{incl/devmode.i}
{incl/custdevmode.i}
{AssignRutiner.i}

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
ASSIGN
    cLogg = 'PRSKampanjeImport' + REPLACE(STRING(TODAY),'/','').

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.


RUN bibl_loggDbFri.p (cLogg, 
    'Start av Kampanje import'
    ). 
RUN bibl_loggDbFri.p (cLogg, 
    'Leser fil: ' + VPIFilHode.Katalog + '\' + VPIFilHode.FilNavn  
    ). 

RUN LesInnFil.
RUN PosterData.

/* Stempler posten som innlest. */
DO TRANSACTION:
    FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
    ASSIGN
        VPIFilHode.VPIFilStatus = 5
        .
END.
IF AVAILABLE VPIFilHode THEN
    FIND CURRENT VPIFilHode    NO-LOCK.

IF CAN-FIND(FIRST ttError) THEN
  RUN ErrorLogg.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE b2Ok AS LOG NO-UNDO.

  /* Tømmer feillogg. */
  FOR EACH ttError:
    DELETE ttError.
  END.

  RUN TellOppLinjer.
  RUN bibl_logg.p (cLogg, 'xPRSKampInnles.p: Leser inn fil: ' + VPIFilHode.FilNavn + ' Antall linjer: ' + STRING(iTotAntLinjer) + ' ' + string(TIME,"HH:MM:SS")).

  ASSIGN
      piLinjeNr  = 0
      pcLinje    = ''
      piAntFeil  = 0
      iAntLinjer = 0
      b2Ok       = TRUE 
      .
      
  FIND LAST KampanjeHode NO-LOCK NO-ERROR.
  IF AVAILABLE KampanjeHode THEN
  DO:
      iKampanjeId = KampanjeHode.KampanjeId + 1.
      RELEASE KampanjeHode.
  END.
  ELSE 
      iKampanjeId = 1.

  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.

    ASSIGN
      iAntLinjer = iAntLinjer + 1
      .
    /* Skipper tomme linjer */
    IF pcLinje = "" THEN
        NEXT LESERLINJER.
        
    /* Skipper overskriftslinje fra mal. */
    IF pcLinje BEGINS "1;2;3;4;5" THEN
        NEXT LESERLINJER.
    
    /* Skipper overskriftslinjer */
    IF NOT CAN-DO('H,L',TRIM(ENTRY(1,pcLinje,';'))) THEN
        NEXT LESERLINJER.
    
    IF TRIM(ENTRY(1,pcLinje,';')) = 'H' THEN
    KAMPANJEHODE:
    DO:
        /* Skipper linjer med for få entries. */
        IF NUM-ENTRIES(pcLinje,";") < 10 THEN
        DO:
          ASSIGN
            piAntFeil = piAntFeil + 1
            .
          CREATE ttError.
          ASSIGN
            ttError.LinjeNr = piAntFeil
            ttError.Tekst   = "** Feil på header linje (Skal være minst 10 entries) " + STRING(iAntLinjer) + ": " + pcLinje
            .
            NEXT LESERLINJER.
        END.
        
        CREATE ttKampanjeHode.
    
        RUN AssignDate(4,OUTPUT ttKampanjeHode.StartDato, OUTPUT bOk).
        IF bOk = FALSE THEN b2Ok = FALSE.
        RUN AssignDate(6,OUTPUT ttKampanjeHode.SluttDato, OUTPUT bOk).
        IF bOk = FALSE THEN b2Ok = FALSE.
        RUN AssignInt(9,OUTPUT ttKampanjeHode.ProfilNr, OUTPUT bOk).
        IF bOk = FALSE THEN b2Ok = FALSE.
        RUN AssignDec(10,OUTPUT ttKampanjeHode.Kamp%, OUTPUT bOk).
        IF bOk = FALSE THEN b2Ok = FALSE.

        ASSIGN 
            ttKampanjeHode.KampanjeId  = iKampanjeId
            ttKampanjeHode.Beskrivelse = ENTRY(3,pcLinje,';')
            ttKampanjeHode.NormalPris  = IF ENTRY(8,pcLinje,';') = 'N' THEN TRUE ELSE FALSE
            ttKampanjeHode.Aktivert    = FALSE
            ttKampanjeHode.Komplett    = FALSE
            ttKampanjeHode.Notat       = 'Importert fil ' + cFilNavn + ' ' + STRING(TODAY) + ' ' + 
                                         STRING(TIME,"HH:MM:SS") + '.'
            iProfilNr = ttKampanjeHode.ProfilNr
            lRab%                      = ttKampanjeHode.Kamp%
            .
        ASSIGN 
            ttKampanjeHode.AktiveresTid = INT(ENTRY(1,ENTRY(5,pcLinje,';'),':')) * 3600 +
                                          INT(ENTRY(2,ENTRY(5,pcLinje,';'),':')) * 60
            ttKampanjeHode.GyldigtilTid = INT(ENTRY(1,ENTRY(7,pcLinje,';'),':')) * 3600 +
                                          INT(ENTRY(2,ENTRY(7,pcLinje,';'),':')) * 60            
        NO-ERROR.                

        /* Det har vært feil i en eller flere kolonner */
        IF b2Ok = FALSE THEN 
          NEXT LESERLINJER.

    END. /* KAMPANJEHODE */
    ELSE IF TRIM(ENTRY(1,pcLinje,';')) = 'L' THEN
    KAMPANJELINJE:
    DO:
        /* Skipper linjer med for få entries. */
        IF NUM-ENTRIES(pcLinje,";") < 6 THEN
        DO:
          ASSIGN
            piAntFeil = piAntFeil + 1
            .
          CREATE ttError.
          ASSIGN
            ttError.LinjeNr = piAntFeil
            ttError.Tekst   = "** Feil på linje (Skal være minst 6 entries) " + STRING(iAntLinjer) + ": " + pcLinje
            .
            NEXT LESERLINJER.
        END.
        
        LEGG_OPP_MODELLENS_FARGER:
        FOR EACH ArtBas NO-LOCK WHERE
            ArtBas.LevNr      = INT(ENTRY(2,pcLinje,';')) AND
            ArtBas.LevKod     = TRIM(ENTRY(3,pcLinje,';')) AND
            ArtBas.LevFargKod = TRIM(ENTRY(4,pcLinje,';')):

            CREATE ttKampanjeLinje.
    
            RUN AssignInt(2,OUTPUT ttKampanjeLinje.LevNr, OUTPUT bOk).
            IF bOk = FALSE THEN b2Ok = FALSE.
    
            ASSIGN 
                ttKampanjeLinje.ProfilNr   = ttKampanjeHode.ProfilNr
                ttKampanjeLinje.KampanjeId = iKampanjeId
                ttKampanjeLinje.LevNr      = IF ttKampanjeLinje.LevNr = 0 THEN 40 ELSE ttKampanjeLinje.LevNr
                ttKampanjeLinje.LevKod     = ENTRY(3,pcLinje,';')
                ttKampanjeLinje.LevFargKod = ENTRY(4,pcLinje,';')
                ttKampanjeLinje.Beskr      = ENTRY(5,pcLinje,';')
                ttKampanjeLinje.Rab%       = DEC(ENTRY(6,pcLinje,';'))
                ttKampanjeLinje.Behandlet  = FALSE
                ttKampanjeLinje.Klar       = FALSE
                ttKampanjeLinje.ArtikkelNr = ArtBas.ArtikkelNr
                ttKampanjeLinje.Beskr      = ArtBas.Beskr
                ttKampanjeLinje.Vg         = ArtBas.Vg
                ttKampanjeLinje.LopNr      = ArtBas.LopNr
                ttKampanjeLinje.Klar       = TRUE
                .
                
            /* Henter lokal pris. */    
            FIND FIRST ArtPris NO-LOCK WHERE 
                ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                ArtPris.ProfilNr   = iProfilNr NO-ERROR.
                
            /* finnes ikke lokal pris, opprettes denne. */    
            IF NOT AVAILABLE ArtPris THEN
            DO:
                FIND FIRST bufArtPris NO-LOCK WHERE 
                    bufArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                    bufArtPris.ProfilNr   = 1 NO-ERROR.
                CREATE ArtPris.
                BUFFER-COPY bufArtPris 
                    EXCEPT ProfilNr
                    TO ArtPris 
                    ASSIGN
                    ArtPris.ProfilNr = iProfilNr
                    NO-ERROR.
            END.

            IF AVAILABLE ArtPris THEN
                ASSIGN 
                ttKampanjeLinje.Pris[2]  = ArtPris.Pris[1] - 
                                           ((ArtPris.Pris[1] * (IF ttKampanjeLinje.Rab% <> 0 
                                                                  THEN ttKampanjeLinje.Rab%
                                                                  ELSE lRab%)) / 100)
                ttKampanjeLinje.VareKost = ArtPris.VareKost[1]
                .

            RUN bibl_loggDbFri.p (cLogg, 
                '    Modell LevNr: ' + STRING(ttKampanjeLinje.LevNr) + 
                     ' LevKod: ' + ttKampanjeLinje.LevKod +
                     ' FargeKode: ' + ttKampanjeLinje.LevFargKod
                ). 
        

        END. /* LEGG_OPP_MODELLENS_FARGER */
        
    END. /* KAMPANJELINJE */

    STATUS DEFAULT "Lese linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PosterData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PosterData Procedure 
PROCEDURE PosterData :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  
  FOR EACH ttKampanjeHode WHERE 
      ttKampanjeHode.KampanjeId = iKampanjeId TRANSACTION:
          
    FIND KampanjeHode EXCLUSIVE-LOCK WHERE 
      KampanjeHode.KampanjeId = ttKampanjeHode.KampanjeId NO-ERROR.
    IF NOT AVAILABLE KampanjeHode THEN 
    DO:
      CREATE KampanjeHode.
      BUFFER-COPY ttKampanjeHode TO KampanjeHode 
          ASSIGN
          KampanjeHode.Kamp% = ttKampanjeHode.Kamp% * -1
          NO-ERROR.
    END.

    RELEASE KampanjeHode.

    FOR EACH ttKampanjeLinje WHERE
        ttKampanjeLinje.KampanjeId = ttKampanjeHode.KampanjeId AND
        ttKampanjeLinje.Klar       = TRUE:
        CREATE KampanjeLinje.
        BUFFER-COPY ttKampanjeLinje TO KampanjeLinje NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
        DO:
            IF AVAILABLE KampanjeLinje THEN DELETE KampanjeLinje.
            RUN bibl_loggDbFri.p (cLogg, 
                '** Feil ved buffer-copy artikkell: LevNr: ' + STRING(ttKampanjeLinje.LevNr) + ' ' + 
                                  ' LevArtNr: ' + ttKampanjeLinje.LevKod + ' ' + 
                                  ' LevFargKod: ' + ttKampanjeLinje.LevFargKod
                ). 
            
        END.
        
    END.
    
  END. /* TRANSACTION */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

