&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :


    Author(s)   : Tom Nøkleby
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
DEF VAR cFilNavn      AS CHAR NO-UNDO.
DEF VAR cVPIFil       AS CHAR NO-UNDO.
DEF VAR cErrFil       AS CHAR NO-UNDO.
DEF VAR ctmpKatalog   AS CHAR NO-UNDO.

DEF STREAM InnFil.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR.

{windows.i}

{xwbartinnles.i &NEW=NEW}

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
    cErrFil  = OS-GETENV('TMP') + '\' + "ErrPPT8800.txt"
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

ASSIGN
    ctmpKatalog = SESSION:TEMP-DIRECTORY.

/* Nullstiller temp-table. */
FOR EACH tmpArtBas:
    DELETE tmpArtBas.
END.

RUN LesInnFil.

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
  DEF VAR cFilnavn AS CHAR NO-UNDO.

  ASSIGN
      cFilnavn = OS-GETENV('TMP') + '\' 
                 + "Error_" 
                 + entry(1,VPIFilHode.FilNavn,".")
                 + ".Txt".
  
  OUTPUT TO VALUE(cFilnavn).
    PUT UNFORMATTED
      "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn skip
      .
    FOR EACH tt_Error:
      PUT UNFORMATTED tt_Error.Tekst SKIP.
    END.
  OUTPUT CLOSE.
  IF SEARCH(cFilnavn) <> ? THEN
  DO:
    DEF VAR hInstance AS INT.

    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH(cFilnavn),
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
------------------------------------------------------------------------------*/
  
  DEF VAR dArtikkelnr AS DECIMAL  NO-UNDO.
  DEF VAR lAktiver    AS LOGICAL  NO-UNDO.
  DEFINE VARIABLE lPubliser AS LOGICAL NO-UNDO.
  DEF VAR piLinjeNr   AS INT      NO-UNDO.
  DEF VAR piAntFeil   AS INT      NO-UNDO.
  
  DEF BUFFER bArtBas FOR ArtBas.

  /* Tømmer feillogg. */
  FOR EACH tt_Error:
    DELETE tt_Error.
  END.

  ASSIGN
      piLinjeNr  = 1.
      iAntLinjer = 1
      .

  RUN xmlReadWBART.p (cFilNavn,OUTPUT dArtikkelnr,OUTPUT lAktiver, OUTPUT lPubliser).

  ARTIKKELLISTE:
  FOR EACH tmpArtBas:
      ASSIGN
          dArtikkelNr = tmpArtBas.ArtikkelNr
          lAktiver    = tmpArtBas.Aktiver
          lPubliser   = tmpArtBas.Publiser
          .
      IF (dArtikkelNr = 0 OR dArtikkelNr = ?) OR error-status:ERROR THEN
      DO:
          IF ERROR-STATUS:ERROR THEN
          DO:
              ASSIGN
                piAntFeil = piAntFeil + 1.
              CREATE tt_Error.
              ASSIGN
                tt_Error.LinjeNr = piAntFeil
                tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer).

          END.
      END.
      ELSE DO FOR bArtBas TRANSACTION:
          FIND bArtBas EXCLUSIVE-LOCK WHERE 
              bArtBas.ArtikkelNr = dArtikkelNr NO-ERROR.
          IF NOT AVAILABLE bArtBas THEN
          DO:
              ASSIGN
                piAntFeil = piAntFeil + 1.
              CREATE tt_Error.
              ASSIGN
                tt_Error.LinjeNr = piAntFeil
                tt_Error.Tekst   = "** Ukjent artikkel på linje: Artikkel/linje " + STRING(dArtikkelNr) + "/" + STRING(iAntLinjer).
          END.
          ELSE DO:
              ASSIGN
                  bArtBas.WebButikkArtikkel   = (IF lAktiver  <> ? THEN lAktiver  ELSE bArtBas.WebButikkArtikkel)
                  bArtBas.PubliserINettbutikk = (IF lPubliser <> ? THEN lPubliser ELSE bArtBas.PubliserINettbutikk).
              FIND CURRENT bArtBas NO-LOCK NO-ERROR.
          END.
          IF AVAILABLE bArtBas THEN
              RELEASE bArtBas.
      END. /* TRANSACTION */

  END. /* ARTIKKELLISTE */

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
  DEF VAR cLinje AS CHAR NO-UNDO.

  ASSIGN
      iTotAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) NO-ECHO.
  repeat:
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

