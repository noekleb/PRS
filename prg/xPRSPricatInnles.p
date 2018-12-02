&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xPRSPricatInnles.p
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
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.
DEF VAR iLevNr        AS INT  NO-UNDO.
DEFINE VARIABLE bUndertrykkErrLogg      AS LOG NO-UNDO.
DEFINE VARIABLE cFilPrefix AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.

DEF STREAM InnFil.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHARACTER
  FIELD ErrNr   AS INTEGER 
  .
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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

bTest = FALSE.

{syspara.i 102 1 1 cFilPrefix}
IF cFilPrefix = '' THEN cFilPrefix = "VPILog_".
cFilPrefix = cFilPrefix + REPLACE(STRING(TODAY,"99/99/99"),'/','') + '_'.

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

/* Setter leverandørnnumer som skal benyttes hvis dette ikke står i filen. */
ASSIGN
    iLevNr = int(SUBSTRING(VPIFilHode.FilNavn,4,3))
    NO-ERROR.

RUN LesInnFil.

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
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

  IF CAN-FIND(FIRST tt_error) AND bUndertrykkErrLogg = FALSE THEN  
  UTSKRIFT_AV_ERR_Logg:
  DO:
    IF AVAILABLE VPIFilHode 
      THEN cTekst = "  Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn. 
    ELSE cTekst = "  Ukjent/slettet VPI fil (xinnTradeItemPrice).".    
    RUN bibl_logg.p (cFilPrefix, 'xPRSPricatInnles.p - ' + cTekst).
    
    FOR EACH tt_Error 
      BREAK BY ErrNr:
      RUN bibl_logg.p (cFilPrefix, 'xPRSPricatInnles.p - ' + tt_Error.Tekst).
      DELETE tt_Error.
    END.
  END. /* UTSKRIFT_AV_ERR_Logg */
  
  /* Sender eMail hvis det skapes error fil ved import. */
  /*
  IF bSendEMail AND SEARCH(cErrorFil) <> ? THEN
    RUN sendEMail(SEARCH(cErrorFil)).
  */

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
------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr AS INT  NO-UNDO.
  DEF VAR pcLinje   AS CHAR NO-UNDO.
  DEF VAR piAntFeil AS INT  NO-UNDO.
  
  /* Tømmer feillogg. */
  FOR EACH tt_Error:
    DELETE tt_Error.
  END.

  RUN TellOppLinjer.
  IF bTest THEN RUN bibl_logg.p (cFilPrefix, 'xPRSPricatInnles.p - ' + VPIFilHode.FilNavn + ' Antall linjer: ' + STRING(iTotAntLinjer)).
  
  FIND LAST VPIFilLinje OF VPIFilHode NO-LOCK NO-ERROR.
  IF AVAILABLE VPIFilLinje THEN
      piLinjeNr = VPIFilLinje.LinjeNr + 1.
  ELSE
      piLinjeNr = 1.

  ASSIGN
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
    /* Skipper tomme linjer */
    IF pcLinje = "" THEN
        NEXT LESERLINJER.

    /* Skipper linjer med for få entries. */
    IF NUM-ENTRIES(pcLinje,";") < 36 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      CREATE tt_Error.
      ASSIGN
        tt_Error.LinjeNr = piAntFeil
        tt_Error.Tekst   = "** Feil på linje (Færre enn 36 entries) " + STRING(iAntLinjer) + ": " + pcLinje
        .
        NEXT LESERLINJER.
    END.

    /* Legger på leverandørnummer hvis ikke dette er satt i filen. */
    IF ENTRY(2,pcLinje,";") = "" THEN
        ASSIGN
            ENTRY(2,pcLinje,";") = STRING(iLevNr)
            .

    IF SUBSTRING(pcLinje,1,2) = "**" THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      CREATE tt_Error.
      ASSIGN
        tt_Error.LinjeNr = piAntFeil
        tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + ": " + pcLinje
        .
      NEXT LESERLINJER.
    END.
    IF NUM-ENTRIES(pcLinje,";") < 36 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      CREATE tt_Error.
      ASSIGN
        tt_Error.LinjeNr = piAntFeil
        tt_Error.Tekst   = "** Feil antall entries på linje " + STRING(iAntLinjer) + " (skal være >= 36): " + string(NUM-ENTRIES(pcLinje,";")) + "."
        .
      NEXT LESERLINJER.
    END.

    /* Posterer linjen */
    CREATE VPIFilLinje.
    ASSIGN
        VPIFilLinje.FilId      = lFilId
        VPIFilLinje.LinjeNr    = piLinjeNr
        VPIFilLinje.StorTekst  = pcLinje
        VPIFilLinje.VareNr     = IF NUM-ENTRIES(pcLinje,";") >= 4
                                   THEN ENTRY(4,pcLinje,";")
                                   ELSE ""
        piLinjeNr              = piLinjeNr  + 1
        .

    STATUS DEFAULT "Lese linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  /* Stempler posten som innlest. */
  DO TRANSACTION:
      FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
      ASSIGN
          VPIFilHode.VPIFilStatus = 8
          .
  END.
  IF AVAILABLE VPIFilHode THEN
      FIND CURRENT VPIFilHode    NO-LOCK.

  IF CAN-FIND(FIRST tt_Error) THEN
    RUN ErrorLogg.

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

