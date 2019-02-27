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
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.
DEF VAR cVPIFil       AS CHAR NO-UNDO.
DEF VAR cErrFil       AS CHAR NO-UNDO.
DEF VAR iLevNr        AS INT  NO-UNDO.
DEF VAR cLevNavn      AS CHAR NO-UNDO.
DEF VAR ctmpKatalog   AS CHAR NO-UNDO.
DEF VAR pcLinje       AS CHAR NO-UNDO.
DEF VAR cPrefiks      AS CHAR NO-UNDO.

DEF STREAM InnFil.
DEF STREAM UtFil.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR.

{xppt880.i &NEW="new" &SHARED="shared"}
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
    cErrFil  = OS-GETENV('TMP') + '\' + "ErrMxMobileInv.txt"
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn
    cPrefiks = 'VARETRAN'.

ASSIGN
    ctmpKatalog = SESSION:TEMP-DIRECTORY.

/* Filen finnes ikke */
if search(cFilNavn) = ? then
  return.

RUN LesInnFil.
RUN EksporterFil.

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-EksporterFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterFil Procedure 
PROCEDURE EksporterFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  Eksporterer til varetran fil som så kan leses inn på vanlig måte.     
------------------------------------------------------------------------------*/
  DEF VAR bStream AS LOG  NO-UNDO.
  DEF VAR cUtFil  AS CHAR NO-UNDO.
  DEF VAR cFilKat AS CHAR NO-UNDO.
  DEF VAR cTmpFil AS CHAR NO-UNDO.


  ASSIGN
      cFilKat = VPIFilHode.Katalog + "\" + cPrefiks
      cTmpFil = SESSION:TEMP-DIRECTORY + cPrefiks
      .
                           
  UTLEGG:
  FOR EACH tmpPDA /*WHERE tmpPDA.EAN > 0*/
      BREAK BY tmpPDA.ButNr
            BY tmpPDA.BrukerId
            BY tmpPDA.Dato:

      /* Åpner stream */
      IF FIRST-OF(tmpPDA.Dato) THEN
      DO:
          ASSIGN
              cUtfil = cFilKat + tmpPDA.BrukerId + '_' + 
                       replace(STRING(TODAY),"/","-") + "_" + 
                       STRING(TIME) + "." +
                       STRING(tmpPDA.ButNr).
          OUTPUT STREAM Utfil TO VALUE(cTmpFil) NO-ECHO.
      END.

      /* Legger ut data i standard Progres eksportformat. */
      EXPORT STREAM UtFil tmpPDA.

      /* Lukker stream */
      IF LAST-OF(tmpPDA.Dato) THEN
      DO:
          OUTPUT STREAM UtFil CLOSE.
          FILE-INFO:FILE-NAME = cTmpFil.
          IF FILE-INFO:FILE-SIZE > 0 THEN DO:
              OS-COPY VALUE(cTmpFil) VALUE(cUtFil).
          END.
          OS-DELETE VALUE(cTmpFil).
      END.

  END. /* UTLEGG */
          
          
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
  DEF VAR cFilnavn AS CHAR NO-UNDO.

  FIND FIRST tmpPDA NO-LOCK NO-ERROR.
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
    IF AVAILABLE tmpPDA THEN DO:
        PUT UNFORMATTED
            "Lok.liste " STRING(tmpPDA.Dato) " " tmpPDA.BrukerId " " STRING(tmpPDA.Tid,"HH:MM:SS") skip.
    END.

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
    /* ----------------- */
      1 · butnr         Parameterstyrt
      2 . ean           Scannes eller tastes
      3 · dato          Registreringsdato
      4 · tid           Registreringstidspunkt. Lagres som sekunder etter midnatt.
      5 · loggnr        0
      6 · transtype     Velges blant lovlige verdier (1 – 7 og 9)
      7 . transtekst    Varetekst
      8 · brukerid      Forhåndsvalgt ved oppstart registreringssekvens
      9 · antall        Tastes (default 1)                             
     10 · kostpris      0 (kan tastes ved spesielle behov)             
     11 · salggsum      0 (kan tastes ved spesielle behov)             
     12 · nylagant      0                                              
     13 · gmlagant      0                                              
     
     Filnavn – varetran.butnr (butnr er uformatert, butnr = 1 à varetran.1, butnr = 11 à varetran.11)
     Eksempelfil – varetran.1

    /* Fileksempel */
    1 54491229 01/12/04 19565 0 5 "" "1" 45 9.90 9.90 0 0
    1 7044610022420 03/12/04 17839 0 7 "" "1" 1 0.00 0.00 0 0
    1 7031580812045 03/12/04 17866 0 7 "" "1" 1 0.00 0.00 0 0
    1 7031580812045 03/12/04 17870 0 7 "" "1" 1 0.00 0.00 0 0
                         

------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr  AS INT  NO-UNDO.
  DEF VAR piAntFeil  AS INT  NO-UNDO.
  DEF VAR pcBkuFil   AS CHAR NO-UNDO.
  DEF VAR plVareNr   AS DEC  NO-UNDO.
  DEF VAR pcStr      AS CHAR NO-UNDO.
  DEF VAR piLoop     AS INT  NO-UNDO.
  DEF VAR cUkjentEAN AS CHAR NO-UNDO.
  DEF VAR cTekst     AS CHAR NO-UNDO.
  DEF VAR pcLinje    AS CHAR NO-UNDO.
  DEF VAR pcEAN      AS CHAR NO-UNDO.
  DEF VAR pcDato     AS CHAR NO-UNDO.
  DEF VAR pdDato     AS DATE NO-UNDO.
  DEF VAR pfEAN      AS DEC  NO-UNDO.
  
  /* Tømmer feillogg. */
  FOR EACH tt_Error:
    DELETE tt_Error.
  END.
  /* PDA fil */
  FOR EACH tmpPDA:
    DELETE tmpPDA.
  END.
  
  RUN TellOppLinjer.

  ASSIGN
      piLinjeNr  = 1.
      iAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    assign
      iAntLinjer = iAntLinjer + 1.

    CREATE tmpPDA.
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED 
        pcLinje /*NO-ERROR*/.
    /*
    IF ERROR-STATUS:ERROR THEN
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer).

        IF AVAILABLE tmpPDA THEN DELETE tmpPDA.
        NEXT LESERLINJER.
    END.
    */
    ASSIGN
        pcEAN  = TRIM(ENTRY(5,pcLinje,";"))
        pcDato = TRIM(ENTRY(3,pcLinje,";"))
        .

    /* Legger på ledende nuller i EAN koden */
    IF LENGTH(pcEAN) > 6 AND length(pcEAN) < 13 THEN
        pcEAN = FILL("0",13 - LENGTH(pcEAN)) + pcEAN.
        .
    /* Kontroll av EAN kode */
    ASSIGN
        pfEAN = DEC(pcEAN)
        /*NO-ERROR*/.
    /*
    IF ERROR-STATUS:ERROR THEN
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " ** Feil i EAN " + pcEAN.

        IF AVAILABLE tmpPDA THEN DELETE tmpPDA.
        NEXT LESERLINJER.
    END.
    */
    FIND Strekkode NO-LOCK WHERE
        Strekkode.Kode = pcEAN NO-ERROR.
    IF AVAILABLE Strekkode THEN
        FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
    ELSE
        cTekst = "** Ukjent EAN".
    IF AVAILABLE ArtBas THEN
        cTekst = REPLACE(ArtBas.Beskr,' ','_').
    ELSE
        cTekst = "** Ukjent EAN".

    /* Trekker ut dato */
    ASSIGN
        pdDato = TODAY 
        /*
        pdDato = DATE(INT(SUBSTRING(pcDato,5,2)),
                      INT(SUBSTRING(pcDato,7,2)),
                      INT(SUBSTRING(pcDato,1,4)))
        */
        NO-ERROR.
    /*
    IF ERROR-STATUS:ERROR THEN
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " ** Feil i dato " + pcDato.

        IF AVAILABLE tmpPDA THEN DELETE tmpPDA.
        NEXT LESERLINJER.
    END.
    */
    
    /* Sjekker butikk */
    /*
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = INT(ENTRY(4,pcLinje,";")) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " ** Ukjent butikknr " + ENTRY(4,pcLinje,";").

        IF AVAILABLE tmpPDA THEN DELETE tmpPDA.
        NEXT LESERLINJER.
    END.
    */
    
    ASSIGN
        tmpPDA.Butnr        = int(ENTRY(4,pcLinje,";"))
        tmpPDA.Ean          = DEC(pcEAN)
        tmpPDA.Dato         = pdDato
        tmpPDA.Tid          = 0 /* Denne må senere legges inn i utlegget fra håndterminalen */
        tmpPDA.Loggnr       = 0
        tmpPDA.Transtype    = 7 /* Varetelling */
        tmpPDA.Transtekst   = cTekst
        tmpPDA.Brukerid     = REPLACE(ENTRY(6,pcLinje,";"),' ','_')
        tmpPDA.Antall       = dec(replace(ENTRY(2,pcLinje,";"),".",","))
        tmpPDA.Kostpris     = 0
        tmpPDA.Salgssum     = 0
        tmpPDA.Nylagant     = 0
        tmpPDA.Gmlagant     = 0
        tmpPDA.LinjeNr      = iAntLinjer
        /*NO-ERROR*/.
    /*
    IF ERROR-STATUS:ERROR THEN
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " ** Feil i assign av felt ".

        IF AVAILABLE tmpPDA THEN DELETE tmpPDA.
        NEXT LESERLINJER.
    END.
    */
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

