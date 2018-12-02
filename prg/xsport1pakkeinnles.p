&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xsport1pakkeinnles.p
    Purpose     :

    Syntax      :

    Description : Leser inn fil med pakkestruktur og bygger opp pakkene.
                  Alle artikller og strekkoder må finnes fra før.

    Author(s)   : Tom Nøkleby
    Created     : 23/06-09
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
DEF VAR iInt          AS INT  NO-UNDO.
DEF VAR ctmpKatalog   AS CHAR NO-UNDO.
DEF VAR pcLinje       AS CHAR NO-UNDO.
DEF VAR piAntFeil AS INT  NO-UNDO.
DEF VAR pcBkuFil  AS CHAR NO-UNDO.
DEFINE VARIABLE iPakkeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE lDec     AS DECIMAL NO-UNDO.
DEFINE VARIABLE cTekst   AS CHARACTER NO-UNDO.

DEF STREAM InnFil.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .
DEFINE TEMP-TABLE ttPakke
  FIELD RecType  AS CHARACTER 
  FIELD LinjeNr  AS INTEGER
  FIELD LevNr    AS INTEGER 
  FIELD PakkeEAN AS CHARACTER FORMAT "x(15)"
  FIELD LinjeEAN AS CHARACTER FORMAT "x(15)"
  FIELD AntIPkn  AS DECIMAL FORMAT "->>,>>9.999"
  INDEX PakkeLinje PakkeEAN LinjeEAN.

DEFINE BUFFER bArtBAs FOR ArtBas.

{windows.i}
{incl\DevMode.i}
{incl\CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

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
         HEIGHT             = 19.05
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

ASSIGN
    ctmpKatalog = SESSION:TEMP-DIRECTORY
    .

RUN bibl_logg.p ('VPIImport', 'xsport1pakkeinnles.p: Automatisk VPI import av pakkefil: ' + STRING(VPIFilHode.FilNavn) + ' ' + string(TIME,"HH:MM:SS")).

RUN LesInnFil.
RUN OpprettPakker.
/*
IF NOT CAN-FIND(FIRST tt_Error) THEN
  RUN OpprettPakker.
ELSE DO:
  RUN bibl_logg.p ('VPIImport', 'xsport1pakkeinnles.p: Innlesning av pakkefil avbrutt. Feil i fil. ' + STRING(TIME,"HH:MM:SS")).
END.
*/
IF VPIFilHode.FilNavn BEGINS 'APK' THEN 
    RUN sendVPI.

/* Stempler posten som innlest. */
DO TRANSACTION:
    FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
    ASSIGN
        VPIFilHode.VPIFilStatus = 5
        .
END.
IF AVAILABLE VPIFilHode THEN
    FIND CURRENT VPIFilHode    NO-LOCK.

IF CAN-FIND(FIRST tt_Error) THEN
    RUN ErrorLogg.

ASSIGN
  pcBkuFil = VPIFilHode.Katalog + "~\bku" + "\" + 
             VPIFilHode.FilNavn
  .

/* Sikrer at backup katalog finnes. */
OS-CREATE-DIR value(VPIFilHode.Katalog + "~\bku").
/* Flytter filen til backup katalog. */
OS-COPY value(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) 
        value(pcBkuFil).
/* Renser bort fil */
IF SEARCH(pcBkuFil) <> ? THEN
DO:
    /* Filen tas bort fra katalogen. */
    IF SEARCH(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) <> ? THEN
        OS-DELETE VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn).
END.

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-OpprettPakker) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettPakker Procedure 
PROCEDURE OpprettPakker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR plFilId      AS DEC NO-UNDO.
DEF VAR pbOk         AS LOG NO-UNDO.
DEF VAR piAntLinjer  AS LOG NO-UNDO.
DEF VAR plArtikkelNr AS DEC NO-UNDO.

DEFINE BUFFER pkStrekkode FOR Strekkode.

IF iAntLinjer > 0 THEN
DO:
    PAKKE:                    
    FOR EACH ttPakke WHERE
        ttPakke.AntIPkn > 0
        BREAK BY ttPakke.PakkeEAN
              BY ttPakke.LinjeEAN:

        FIND pkStrekkode NO-LOCK WHERE
          pkStrekkode.Kode = ttPakke.PakkeEAN NO-ERROR.

        /* Pakke hode markeres som pakke */      
        IF FIRST-OF(ttPakke.PakkeEAN) THEN
        DO TRANSACTION:
          IF NOT AVAILABLE pkStrekkode THEN
            DO:
              CREATE tt_Error.
              ASSIGN
                tt_error.LinjeNr = ttPakke.LinjeNr
                tt_Error.Tekst   = "** Ukjent strekkode på pakke. " + ttPakke.PakkeEAN + '.'. 
              LEAVE PAKKE.
            END.
          FIND ArtBas OF pkStrekkode NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ArtBas THEN
            DO:
              CREATE tt_Error.
              ASSIGN
                tt_error.LinjeNr = ttPakke.LinjeNr
                tt_Error.Tekst   = "** Ukjent artikkel på pakkelinje. " + STRING(pkStrekkode.ArtikkelNr) + '.'. 
              LEAVE PAKKE.
            END.
          IF ArtBas.OPris OR ArtBas.Pant OR ArtBas.SanertDato <> ? THEN 
            DO: 
              CREATE tt_Error.
              ASSIGN
                tt_error.LinjeNr = ttPakke.LinjeNr.
              IF ArtBas.Pakke THEN tt_Error.Tekst   = "** Pakkeartikkel på pakkelinje. " + STRING(artbas.artikkelNr) + '.'.
              ELSE IF ArtBas.Pant THEN tt_Error.Tekst   = "** Pantvare på pakkelinje. " + STRING(artbas.artikkelNr) + '.'. 
              ELSE IF ArtBas.OPris THEN tt_Error.Tekst   = "** Åpen pris på pakkelinje. " + STRING(artbas.artikkelNr) + '.'. 
              ELSE IF ArtBas.SanertDato <> ? THEN tt_Error.Tekst = "** Sanert artikkel på pakkelinje. " + STRING(artbas.artikkelNr) + '.'. 
              LEAVE PAKKE.
            END.
          /*IF ArtBas.Pakke = FALSE THEN*/
            DO:
              IF ArtBas.PakkeNr = 0 THEN DO:
                FIND LAST bArtBas NO-LOCK USE-INDEX Pakke.
                IF AVAILABLE bArtBas THEN
                  iPakkeNr = iPakkeNr + 1.
                ELSE
                  iPakkeNr = 1.
              END.
              ELSE iPakkeNr = ArtBas.PakkeNr.
              FIND CURRENT ArtBas EXCLUSIVE-LOCK.
              ASSIGN
                ArtBas.Pakke      = TRUE
                ArtBas.Lager      = FALSE
                ArtBas.PakkeNr    = iPakkeNr
                ArtBas.Storrelser = FALSE
                ArtBas.StrTypeId  = 2.
              FIND CURRENT ArtBas NO-LOCK.
            END.
          /* Rydder bort alle pakkelinjene til pakkevaren før ny import. */
          FOR EACH Pakkelinje OF ArtBas EXCLUSIVE-LOCK:
            DELETE Pakkelinje.
          END.
        END. /* FIRST-OF PakkeEAN TRANSACTION */
        
        /* Oppretter pakkelinjer. */
        IF AVAILABLE pkStrekkode THEN 
        PAKKELINJE:
        DO:
          FIND Strekkode NO-LOCK WHERE
            Strekkode.Kode = ttPakke.LinjeEAN NO-ERROR.
          IF NOT AVAILABLE Strekkode THEN
            DO:
              CREATE tt_Error.
              ASSIGN
                tt_error.LinjeNr = ttPakke.LinjeNr
                tt_Error.Tekst   = "** Ukjent strekkode på pakkelinje. " + ttPakke.LinjeEAN + '.'. 
              LEAVE PAKKE.
            END.
          FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ArtBas THEN
            DO:
              CREATE tt_Error.
              ASSIGN
                tt_error.LinjeNr = ttPakke.LinjeNr
                tt_Error.Tekst   = "** Ukjent artikkel på pakkelinje. " + STRING(Strekkode.ArtikkelNr) + '.'. 
              LEAVE PAKKE.
            END.
          IF ArtBas.Pakke OR ArtBas.OPris OR ArtBas.Pant OR ArtBas.SanertDato <> ? THEN
            DO: 
              CREATE tt_Error.
              ASSIGN
                tt_error.LinjeNr = ttPakke.LinjeNr.
              IF ArtBas.Pakke THEN tt_Error.Tekst   = "** Pakkeartikkel på pakkelinje. " + STRING(artbas.artikkelNr) + '.'.
              ELSE IF ArtBas.OPris THEN tt_Error.Tekst   = "** Åpen pris på pakkelinje. " + STRING(artbas.artikkelNr) + '.'. 
              ELSE IF ArtBas.SanertDato <> ? THEN tt_Error.Tekst = "** Sanert artikkel på pakkelinje. " + STRING(artbas.artikkelNr) + '.'. 
              ELSE IF ArtBas.Pant THEN tt_Error.Tekst   = "** Pantvare på pakkelinje. " + STRING(artbas.artikkelNr) + '.'. 
              LEAVE PAKKE.
            END.
          IF ttPakke.AntIPkn = 0 THEN
            DO:
              CREATE tt_Error.
              ASSIGN
                tt_error.LinjeNr = ttPakke.LinjeNr
                tt_Error.Tekst   = "** 0 i antall på pakkelinje. " + STRING(Strekkode.ArtikkelNr) + '.'. 
              LEAVE PAKKE.
            END.
          DO TRANSACTION:
            FIND PakkeLinje EXCLUSIVE-LOCK WHERE
              PakkeLinje.ArtikkelNr = pkStrekkode.ArtikkelNr AND
              PakkeLinje.PkArtikkelNr = Strekkode.ArtikkelNr AND
              PakkeLinje.StrKode      = Strekkode.StrKode NO-ERROR.
            IF NOT AVAILABLE PakkeLinje THEN
              DO:
                CREATE PakkeLinje.
                ASSIGN
                  PakkeLinje.ArtikkelNr = pkStrekkode.ArtikkelNr
                  PakkeLinje.PkArtikkelNr = Strekkode.ArtikkelNr
                  PakkeLinje.StrKode      = Strekkode.StrKode.
              END.
            ASSIGN
              PakkeLinje.Antall = ttPakke.AntIPkn.
            RELEASE PakkeLinje.
          END. /* TRANSACTION */
        END. /* PAKKELINJE */    
    END. /* PAKKE */
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ErrorLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ErrorLogg Procedure 
PROCEDURE ErrorLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFilNavn AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
  
  IF NOT CAN-FIND(FIRST tt_Error) THEN
    RETURN.
    
IF AVAILABLE VPIFilHode THEN 
  pcTekst = "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.
ELSE 
  pcTekst = "ukjent/slettet VPI filhode (xsport1pakkeinnles.p).".    
  
  ASSIGN
    cFilNavn = SESSION:TEMP-DIRECTORY + "Error_xsport1pakkeinnles.Txt".  
  OUTPUT TO VALUE(cFilNavn).
    PUT UNFORMATTED
      "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      pcTekst SKIP
      .
    FOR EACH tt_Error:
      PUT UNFORMATTED "Linje: " tt_Error.LinjeNr " " tt_Error.Tekst SKIP.
      RUN bibl_logg.p ('VPIImport', 'xsport1pakkeinnles.p: FEIL på linje: ' + STRING(tt_Error.LinjeNr) + ' ' + tt_Error.Tekst + ' ' + string(TIME,"HH:MM:SS")).
    END.
  OUTPUT CLOSE.
  IF SEARCH(cFilNavn) <> ? THEN
  DO:
    DEF VAR hInstance AS INT.

    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH(cFilNavn),
                                  "",
                                  1,
                                  OUTPUT hInstance).

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:     
  Lev. nr	Pakke EAN	Linje EANnr	Ant i pakn
    
      /* ----------------
     1 * Lev.nr
     2 * PakkeEAN
     3 * LinjeEAN
     4 * AntIPakn
    ------------------ */

------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr AS INT  NO-UNDO.
  DEF VAR plVareNr  AS DEC  NO-UNDO.
  DEF VAR pcStr     AS CHAR NO-UNDO.
  DEF VAR piLoop    AS INT  NO-UNDO.
  
  DEFINE BUFFER pkStrekkode FOR Strekkode.
  
  /* Tømmer feillogg. */
  FOR EACH tt_Error:
    DELETE tt_Error.
  END.
  /* Tømmer pricat */
  FOR EACH ttPakke:
      DELETE ttPakke.
  END.
  RUN TellOppLinjer.

  ASSIGN
      piLinjeNr  = 1.
      iAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.

    ASSIGN
      iAntLinjer = iAntLinjer + 1
      .
    ASSIGN
      iInt = INTEGER(ENTRY(2,pcLinje,';'))
      NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO: 
      CREATE tt_Error.
      ASSIGN
        tt_error.LinjeNr = iAntLinjer
        tt_Error.Tekst   = "** Ugyldig verdi i levnr. kolonne. " + pcLinje + '.'. 
      /*NEXT LESERLINJER.*/
    END.
    IF iInt = 0 THEN 
    DO:
      CREATE tt_Error.
      ASSIGN
        tt_error.LinjeNr = iAntLinjer
        tt_Error.Tekst   = "** Levnr = 0 på linje. " + pcLinje + '.'. 
      /*NEXT LESERLINJER.*/
    END.

    IF NUM-ENTRIES(pcLinje,";") < 4 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        pcLinje = pcLinje + ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
        .
    END.
    
    ASSIGN lDec = DECIMAL(TRIM(ENTRY( 4,pcLinje,";"),'"')) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
        CREATE tt_Error.
        ASSIGN
          tt_error.LinjeNr = iAntLinjer
          tt_Error.Tekst   = "** Ugyldig strekkode på pakke. " + pcLinje + '.'. 
        NEXT LESERLINJER.
    END.

    ASSIGN lDec = DECIMAL(TRIM(ENTRY( 5,pcLinje,";"),'"')) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
        CREATE tt_Error.
        ASSIGN
          tt_error.LinjeNr = iAntLinjer
          tt_Error.Tekst   = "** Ugyldig strekkode på pakkelinje. " + pcLinje + '.'. 
        NEXT LESERLINJER.
    END.
    
    /* Konverterer strekkode. Sjekker sjekksiffer og legger på ledende nuller. */
    KONV_STREKKODE:
    DO:
      ASSIGN cTekst = TRIM(ENTRY( 4,pcLinje,";"),'"').
      IF cTekst <> '' THEN 
      DO:
        cTekst = FILL("0",13 - LENGTH(cTekst)) + cTekst.
        RUN bibl_chkean.p (INPUT-OUTPUT cTekst).
        ASSIGN ENTRY(4,pcLinje,';') = cTekst.
      END.

      ASSIGN cTekst = TRIM(ENTRY( 5,pcLinje,";"),'"').
      IF cTekst <> '' THEN 
      DO:
        cTekst = FILL("0",13 - LENGTH(cTekst)) + cTekst.
        RUN bibl_chkean.p (INPUT-OUTPUT cTekst).
        ASSIGN ENTRY(5,pcLinje,';') = cTekst.
      END.
    END. /* KONV_STREKKODE */ 

    FIND pkStrekkode NO-LOCK WHERE
      pkStrekkode.Kode = trim(ENTRY( 4,pcLinje,";"),'"') NO-ERROR.    
    IF NOT AVAILABLE pkStrekkode THEN
      DO:
        CREATE tt_Error.
        ASSIGN
          tt_error.LinjeNr = iAntLinjer
          tt_Error.Tekst   = "** Ukjent strekkode på pakke. " + pcLinje + '.'. 
        NEXT LESERLINJER.
      END.
    FIND ArtBas OF pkStrekkode NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
      DO:
        CREATE tt_Error.
        ASSIGN
          tt_error.LinjeNr = iAntLinjer
          tt_Error.Tekst   = "** Ukjent pakkeartikkel på pakkelinje. " + pcLinje + '.'. 
        NEXT LESERLINJER.
      END.
    IF ArtBas.OPris OR ArtBas.Pant OR ArtBas.SanertDato <> ? THEN 
      DO: 
        CREATE tt_Error.
        ASSIGN
          tt_error.LinjeNr = iAntLinjer.
        IF ArtBas.Pant THEN tt_Error.Tekst   = "** Pantvare på pakke. " + STRING(artbas.artikkelNr) + '.'. 
        ELSE IF ArtBas.OPris THEN tt_Error.Tekst   = "** Åpen pris på pakke. " + STRING(artbas.artikkelNr) + '.'. 
        ELSE IF ArtBas.SanertDato <> ? THEN tt_Error.Tekst = "** Sanert artikkel på pakke. " + STRING(artbas.artikkelNr) + '.'. 
        NEXT LESERLINJER.
      END.

    FIND Strekkode NO-LOCK WHERE
      Strekkode.Kode = trim(ENTRY( 5,pcLinje,";"),'"') NO-ERROR.    
    IF NOT AVAILABLE Strekkode THEN
      DO:
        CREATE tt_Error.
        ASSIGN
          tt_error.LinjeNr = iAntLinjer
          tt_Error.Tekst   = "** Ukjent strekkode på pakkelinje. " + pcLinje + '.'. 
        NEXT LESERLINJER.
      END.
    FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
      DO:
        CREATE tt_Error.
        ASSIGN
          tt_error.LinjeNr = iAntLinjer
          tt_Error.Tekst   = "** Ukjent artikkel på pakkelinje. " + pcLinje + '.'. 
        NEXT LESERLINJER.
      END.
    IF ArtBas.Pakke OR ArtBas.OPris OR ArtBas.Pant OR ArtBas.SanertDato <> ? THEN 
      DO: 
        CREATE tt_Error.
        ASSIGN
          tt_error.LinjeNr = iAntLinjer.
        IF ArtBas.Pant THEN tt_Error.Tekst   = "** Pantvare på pakkelinje. " + STRING(artbas.artikkelNr) + '.'. 
        ELSE IF ArtBas.Pakke THEN tt_Error.Tekst   = "** Pakkevare lagt inn på pakkelinje. " + STRING(artbas.artikkelNr) + '.'. 
        ELSE IF ArtBas.OPris THEN tt_Error.Tekst   = "** Åpen pris på pakkelinje. " + STRING(artbas.artikkelNr) + '.'. 
        ELSE IF ArtBas.SanertDato <> ? THEN tt_Error.Tekst = "** Sanert artikkel på pakkelinje. " + STRING(artbas.artikkelNr) + '.'. 
        NEXT LESERLINJER.
      END.

    CREATE ttPakke.
    ASSIGN
        ttPakke.RecType  = TRIM(ENTRY( 1,pcLinje,";"),'"')
        ttPakke.LinjeNr  = iAntLinjer
        /*ttPakke.LevNr    = INTEGER(TRIM(ENTRY( 2,pcLinje,";"),'"'))*/
        ttPakke.PakkeEAN = TRIM(ENTRY( 4,pcLinje,";"),'"')
        ttPakke.LinjeEAN = TRIM(ENTRY( 5,pcLinje,";"),'"')
        ttPakke.AntIPkn  = DECIMAL(TRIM(ENTRY( 6,pcLinje,";"),'"'))
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
      CREATE tt_Error.
      ASSIGN
        tt_error.LinjeNr = iAntLinjer
        tt_Error.Tekst   = "** Ugyldige verdier på linje. " + pcLinje + '.'.
      NEXT LESERLINJER. 
    END.

    STATUS DEFAULT "Leser linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.
  
  RUN bibl_logg.p ('VPIImport', 'xsport1pakkeinnles.p: Lest inn fil - antall linjer: ' + STRING(iTotAntLinjer) + ' ' + string(TIME,"HH:MM:SS")).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-sendVPI) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendVPI Procedure
PROCEDURE sendVPI:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    DEFINE VARIABLE cEDBSystem  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iAntSlett   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER   NO-UNDO.
    
    cEDBSystem = "ARTBUTSEND" + STRING(TIME).
    SEND_PAKKE:                    
    FOR EACH ttPakke WHERE
        ttPakke.AntIPkn > 0
        BREAK BY ttPakke.PakkeEAN
              BY ttPakke.LinjeEAN:

        IF FIRST-OF(ttPakke.PakkeEAN) THEN 
        DO:
          FIND Strekkode NO-LOCK WHERE Strekkode.Kode = ttPakke.PakkeEAN NO-ERROR.
          IF AVAILABLE Strekkode THEN 
          DO:
            DYNAMIC-FUNCTION("RunProc","art_to_vpi.p",("ARTNR|" + cEDBSystem) + "|" + TRIM(STRING(Strekkode.ArtikkelNr),","),?).
            RUN vpieksport.w (cEDBSystem,
                      "", /* Ekstent skal være butikk som har sendt filen. */
                      1,  /* Bildedata skal sendes med */
                      OUTPUT iAntSlett,
                      OUTPUT iAntNyEndre).
          END. 
        END.
    END. /* SEND_PAKKE */
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-TellOppLinjer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellOppLinjer Procedure 
PROCEDURE TellOppLinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      iTotAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) NO-ECHO.
  REPEAT:
    IMPORT STREAM InnFil UNFORMATTED cLinje.
    ASSIGN
        iTotAntLinjer = iTotAntLinjer + 1
        .
  END.
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

