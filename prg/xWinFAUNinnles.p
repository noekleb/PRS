&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xWinEDIinnles.p
    Purpose     :

    Syntax      :

    Description : Leser inn fil med sendingsnummer for kundeordre.

    Author(s)   : Tom Nøkleby
    Created     : 1/7-09
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

DEF STREAM InnFil.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .
DEFINE TEMP-TABLE ttPakke
  FIELD LinjeNr  AS INTEGER
  FIELD KOrdre_Id AS DECIMAL 
  FIELD SendingsNr AS CHARACTER  
  INDEX KOrdre KOrdre_Id.

DEFINE BUFFER bArtBas FOR ArtBas.

{windows.i}

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

IF SEARCH(cFilnavn) = ? THEN 
DO:
    RETURN " ** Ukjent fil (" + cFilNavn + ").".
END.    

ASSIGN
    ctmpKatalog = SESSION:TEMP-DIRECTORY
    .

RUN LesInnFil.
IF NOT CAN-FIND(FIRST tt_Error) THEN
  RUN OpprettPakker.

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

&IF DEFINED(EXCLUDE-ErrorLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ErrorLogg Procedure 
PROCEDURE ErrorLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFilNavn AS CHARACTER NO-UNDO.
  
  IF NOT CAN-FIND(FIRST tt_Error) THEN
    RETURN.
  
  ASSIGN
    cFilNavn = SESSION:TEMP-DIRECTORY + "Error_xWinEDIinnles.Txt".  
  OUTPUT TO VALUE(cFilNavn).
    PUT UNFORMATTED
      "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn SKIP
      .
    FOR EACH tt_Error:
      PUT UNFORMATTED "Linje: " tt_Error.LinjeNr " " tt_Error.Tekst SKIP.
    END.
  OUTPUT CLOSE.
  IF SEARCH(cFilNavn) <> ? THEN
  DO:
    DEF VAR hInstance AS INT.
    /*
    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH(cFilNavn),
                                  "",
                                  1,
                                  OUTPUT hInstance).
    */

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
  Lev. nr       Pakke EAN       Linje EANnr     Ant i pakn
    
      /* ----------------
     1 * LinjeNr
     2 * KORdre_Id
     3 * SendingsNr
    ------------------ */

------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr AS INT  NO-UNDO.
  DEF VAR plVareNr  AS DEC  NO-UNDO.
  DEF VAR pcStr     AS CHAR NO-UNDO.
  DEF VAR piLoop    AS INT  NO-UNDO.
  DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
  
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
    
    IF TRIM(pcLinje) = '' THEN NEXT.

    ASSIGN
      iAntLinjer = iAntLinjer + 1
      .
    ASSIGN
      lDec = DECIMAL(ENTRY(1,pcLinje,';'))
      NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO: 
      CREATE tt_Error.
      ASSIGN
        tt_error.LinjeNr = iAntLinjer
        tt_Error.Tekst   = "** Postpakke: Ugyldig verdi i Kundeordre_id. kolonne. " + pcLinje + '.'. 
      NEXT LESERLINJER.
    END.
    IF lDec = 0 THEN 
    DO:
      CREATE tt_Error.
      ASSIGN
        tt_error.LinjeNr = iAntLinjer
        tt_Error.Tekst   = "** Postpakke: Kundeordre_id = 0 på linje. " + pcLinje + '.'. 
      NEXT LESERLINJER.
    END.

    IF NUM-ENTRIES(pcLinje,";") < 2 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        pcLinje = pcLinje + ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
        .
    END.
    
    IF TRIM(ENTRY( 2,pcLinje,";"),'"') = '' THEN 
    DO:
        CREATE tt_Error.
        ASSIGN
          tt_error.LinjeNr = iAntLinjer
          tt_Error.Tekst   = "** Postpakke: Sendingsnr er blankt på linje. " + pcLinje + '.'. 
        NEXT LESERLINJER.
    END.

    CREATE ttPakke.
    ASSIGN
        ttPakke.LinjeNr    = iAntLinjer
        ttPakke.KOrdre_Id  = DECIMAL(TRIM(ENTRY( 1,pcLinje,";"),'"'))
        ttPakke.SendingsNr = TRIM(ENTRY( 2,pcLinje,";"),'"')
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

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
DEF VAR plKOrdre_Id  AS DEC NO-UNDO.

DEFINE BUFFER pkKOrdreHode FOR KOrdreHode.
DEFINE BUFFER trgEkstEDBSystem FOR EkstEDBSystem.

IF iAntLinjer > 0 THEN
DO TRANSACTION:
    PAKKE:                    
    FOR EACH ttPakke
        BREAK BY ttPakke.KOrdre_Id:

        FIND pkKOrdreHode EXCLUSIVE-LOCK WHERE
          pkKOrdreHode.KORdre_Id = ttPakke.KOrdre_Id NO-ERROR.
/*             pkKOrdreHode.EkstOrdreNr = STRING(ttPakke.KOrdre_Id) NO-ERROR. */
        IF AVAILABLE pkKOrdreHode THEN
          DO:
            pkKOrdreHode.SendingsNr = (IF pkKOrdreHode.SendingsNr <> '' THEN pkKOrdreHode.SendingsNr ELSE ttPakke.SendingsNr).
            
            NETTBUTIKK:
            DO:
              FIND FIRST trgEkstEDBSystem WHERE 
                  trgEkstEDBSystem.DataType = "WEBBUT" AND 
                  trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
              IF AVAILABLE trgEkstEDBSystem THEN
              WEBBUTIKK:
              DO:
                  FIND ELogg WHERE 
                       ELogg.TabellNavn     = "KOrdreHode" AND
                       ELogg.EksterntSystem = "WEBBUT"    AND
                       ELogg.Verdier        = STRING(pkKOrdreHode.KOrdre_Id) NO-ERROR.
                  IF NOT AVAIL Elogg THEN DO:
                      CREATE Elogg.
                      ASSIGN ELogg.TabellNavn     = "KOrdreHode"
                             ELogg.EksterntSystem = "WEBBUT"   
                             ELogg.Verdier        = STRING(pkKOrdreHode.KOrdre_Id).
                  END.
                  ASSIGN ELogg.EndringsType = 1 
                         ELogg.Behandlet    = FALSE.
                  RELEASE ELogg.
              END. /* WEBBUTIKK */
            END. /* NETTBUTIKK */  
            RELEASE pkKOrdreHode.
          END.
    END. /* PAKKE */
END.


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

