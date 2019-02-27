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
ROUTINE-LEVEL ON ERROR UNDO, THROW.

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
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTxt AS CHARACTER NO-UNDO.
DEFINE VARIABLE iX AS INTEGER NO-UNDO.

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

ASSIGN 
    cLogg = 'xWinEDIinnles' + REPLACE(STRING(TODAY),'/','')
    .
RUN bibl_logg.p (cLogg, 'Start innlesning av postpakke etikett.' ).

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RUN bibl_logg.p (cLogg, "   ** Ukjent VPIFilHode post (" + STRING(lFilId) + ")." ).
    RETURN.
END.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

IF SEARCH(cFilnavn) = ? THEN 
DO:
    RUN bibl_logg.p (cLogg, "   ** Ukjent fil (" + cFilNavn + ")." ).
    RETURN.
END.    

ASSIGN 
    FILE-INFO:FILE-NAME = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.
    
/*
/* Sjekker filens tidsstempel. Den skal ligge i 10 sec før den tas tak i */
IF TIME - FILE-INFO:FILE-MOD-TIME < 10 THEN 
    RETURN.     
*/

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

ASSIGN
  pcBkuFil = VPIFilHode.Katalog + "~\bku" + "\" + 
             VPIFilHode.FilNavn
  .

BACKUP: 
DO:
    RUN bibl_logg.p (cLogg, '   Flytter fil til backup:' + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn + '.' ).
    
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
END. /* BACKUP */

RUN bibl_logg.p (cLogg, 'Ferdig med innlesning av postpakke etikett.' ).

/* PEtikett filen skal ikke slettes av datamottaks server. Derfor returneres 'OK' */
RETURN 'OK'.

/* Trapper feil ved åpning av fil. Ref feilmeldinger på skjerm. */
CATCH eAnyError AS Progress.Lang.SysError:  
    RUN bibl_logg.p (cLogg, "   ** Catch Block." ).
    DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
        cTxt = '    ** FeilNr: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix).      
        RUN bibl_logg.p (cLogg, cTxt ).
    END.
    RETURN.
END CATCH.


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
  DEFINE VARIABLE bError AS LOG NO-UNDO.
  
  /* Tømmer feillogg. */
  FOR EACH tt_Error:
    DELETE tt_Error.
  END.
  /* Tømmer pricat */
  FOR EACH ttPakke:
      DELETE ttPakke.
  END.
  RUN TellOppLinjer.
  IF iTotAntLinjer = 0 THEN 
       RETURN.

  ASSIGN
      piLinjeNr  = 1.
      iAntLinjer = 0
      .
  DO ON ERROR UNDO, LEAVE:      
      IF SEARCH(cFilnavn) = ? THEN RETURN.
      ELSE INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  END.
  
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
        RUN bibl_logg.p (cLogg, "   ** Feil ved åpning av fil for import." ).        
        bError = TRUE.
        LEAVE LESERLINJER. 
    END.
    
    IF TRIM(pcLinje) = '' THEN NEXT.

    ASSIGN
      iAntLinjer = iAntLinjer + 1
      .
    ASSIGN
      lDec = DECIMAL(ENTRY(1,pcLinje,';'))
      NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO: 
        RUN bibl_logg.p (cLogg, "   ** Postpakke: Ugyldig verdi i Kundeordre_id. kolonne. " + pcLinje + '.' ).        
        NEXT LESERLINJER.
    END.
    IF lDec = 0 THEN 
    DO:
        RUN bibl_logg.p (cLogg, "   ** Postpakke: Kundeordre_id = 0 på linje.  " + pcLinje + '.' ).        
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
        RUN bibl_logg.p (cLogg, "   ** Postpakke: Sendingsnr er blankt på linje.  " + pcLinje + '.' ).        
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
        RUN bibl_logg.p (cLogg, "   ** ** Ugyldige verdier på linje.  " + pcLinje + '.' ).        
      NEXT LESERLINJER. 
    END.

    STATUS DEFAULT "Leser linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  IF bError THEN RETURN.

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

        FIND LAST pkKOrdreHode EXCLUSIVE-LOCK WHERE
            pkKOrdreHode.EkstOrdreNr = STRING(ttPakke.KOrdre_Id) NO-ERROR.

        IF AVAILABLE pkKOrdreHode AND pkKOrdreHode.SendingsNr = '' THEN
          DO:
              ASSIGN 
              pkKOrdreHode.SendingsNr = ttPakke.SendingsNr.
            
              RELEASE pkKOrdreHode.
          END.
          
          RUN bibl_logg.p (cLogg, "   Oppdatert kOrdre.  " + STRING(ttPakke.KOrdre_Id) + ' Sendingsnr: ' + ttPakke.SendingsNr + '.' ).        
          
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
DO ON ERROR UNDO, THROW:
    ASSIGN
        iTotAntLinjer = 0
        .
    IF SEARCH(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) = ? THEN 
        RETURN.
      
    INPUT STREAM InnFil FROM VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn).
    REPEAT:
        IMPORT STREAM InnFil UNFORMATTED cLinje NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            RETURN.
        ASSIGN
            iTotAntLinjer = iTotAntLinjer + 1
            .
    END.
    INPUT STREAM InnFil CLOSE.

    RUN bibl_logg.p (cLogg, "   Antall linjer i fil.  " + STRING(iTotAntLinjer) + '.' ).            
END.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

