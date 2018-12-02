&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xWinEDIinnlesDirekte.p
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
DEFINE INPUT PARAMETER cEkstOrdreNr AS CHARACTER NO-UNDO.

DEF VARIABLE iAntLinjer  AS INT    NO-UNDO.

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
  INDEX KOrdre LinjeNr KOrdre_Id.

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

FIND EkstVPIFil NO-LOCK WHERE
    EkstVPIFil.EkstVPILevNr = 885 AND 
    EkstVPIFil.VPIFilNr = 1 NO-ERROR.
IF AVAILABLE EkstVPIFil THEN 
DO:
    /* Kun gant morro her. */
    IF EkstVPIFil.VPIFilNavn <> 'PETIKETT' THEN 
        RETURN.
        
    ASSIGN
        cFilNavn = EkstVPIFil.VPIKatalog + '\' + 
                   EkstVPIFil.VPIFilNavn + '.' + 
                   EkstVPIFil.VPIEkst
        pcBkuFil = EkstVPIFil.VPIKatalog + "~\bku" + "\" + 
                   EkstVPIFil.VPIFilNavn + '.' +
                   EkstVPIFil.VPIEkst + '_' + 
                   REPLACE(STRING(TODAY),'/','') + '_' + 
                   REPLACE(STRING(TIME,"HH:MM:SS"),':','')  
        .
END.
ELSE     
    ASSIGN
        cFilNavn = 'c:\home\lindbak\ankommet\PETIKETT.txt'
        .

IF SEARCH(cFilnavn) = ? THEN 
    RETURN " ** Ukjent fil (" + cFilNavn + ").".

RUN LesInnFil.
RUN OpprettPakker.

IF iAntLinjer > 500 THEN 
DO:
    /* Sikrer at backup katalog finnes. */
    OS-CREATE-DIR value(EkstVPIFil.VPIKatalog + "~\bku").
    /* Flytter filen til backup katalog. */
    OS-COPY value(cFilNavn) 
            value(pcBkuFil).
    /* Renser bort fil */
    IF SEARCH(pcBkuFil) <> ? THEN
    DO:
        /* Filen tas bort fra katalogen. */
        IF SEARCH(cFilNavn) <> ? THEN
            OS-DELETE VALUE(cFilNavn).
    END.
END.

RETURN.

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
        NEXT.
    IF lDec = 0 THEN
        NEXT. 
    IF NUM-ENTRIES(pcLinje,";") < 2 THEN
        NEXT.
            
    IF TRIM(ENTRY( 2,pcLinje,";"),'"') = '' THEN
        NEXT. 

    IF cEkstOrdreNr <> TRIM(ENTRY( 1,pcLinje,";"),'"') THEN 
        NEXT.

    CREATE ttPakke.
    ASSIGN
        ttPakke.LinjeNr    = iAntLinjer
        ttPakke.KOrdre_Id  = DECIMAL(TRIM(ENTRY( 1,pcLinje,";"),'"'))
        ttPakke.SendingsNr = TRIM(ENTRY( 2,pcLinje,";"),'"')
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT.
        
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

DO TRANSACTION:
    FIND LAST pkKOrdreHode EXCLUSIVE-LOCK WHERE
        pkKOrdreHode.EkstOrdreNr = STRING(ttPakke.KOrdre_Id) NO-ERROR.
    IF AVAILABLE pkKOrdreHode THEN 
        pkKOrdreHode.SendingsNr = ''.    
    IF AVAILABLE pkKOrdreHode THEN 
        RELEASE pkKOrdreHode. 
END.

PAKKE:                    
FOR EACH ttPakke
    BREAK BY ttPakke.LinjeNr
          BY ttPakke.KOrdre_Id:

    IF cEkstOrdreNr <> STRING(ttPakke.KOrdre_Id) THEN 
        NEXT.

    FIND LAST pkKOrdreHode NO-LOCK WHERE
        pkKOrdreHode.EkstOrdreNr = STRING(ttPakke.KOrdre_Id) NO-ERROR.
    IF AVAILABLE pkKOrdreHode AND pkKOrdreHode.SendingsNr = ''    
    THEN
      DO TRANSACTION:
        FIND CURRENT pkKOrdreHode EXCLUSIVE-LOCK NO-ERROR.  
        pkKOrdreHode.SendingsNr = ttPakke.SendingsNr.

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
      END. /*  TRANSACTION */
END. /* PAKKE */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
