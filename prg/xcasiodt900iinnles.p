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

DEF STREAM InnFil.

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
    cErrFil  = OS-GETENV('TMP') + '\' + "Err_" + ENTRY(1,VPIFilHode.FilNavn,".")
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

ASSIGN
    ctmpKatalog = SESSION:TEMP-DIRECTORY.

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
  DEF VAR cLinje     AS CHAR NO-UNDO.
  DEF VAR lEAN       AS DEC  NO-UNDO.
  DEF VAR cUkjentEAN AS CHAR NO-UNDO.

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
        cLinje NO-ERROR.
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

    VPIFILLINJE:
    DO TRANSACTION:
        /* Sikrer at det er et ledig linjenummer */
        DO WHILE CAN-FIND(VPIFilLinje WHERE
                          VPIFilLinje.FilID = lFilId AND
                          VPIFilLinje.LinjeNr = iAntLinjer):
            iAntLinjer = iAntLinjer + 1.
        END.
       /* Posterer linjen */
       CREATE VPIFilLinje.
       ASSIGN
           VPIFilLinje.FilId      = lFilId
           VPIFilLinje.LinjeNr    = iAntLinjer            
           VPIFilLinje.VareNr     = string(tmpPDA.Ean)
           .

       FIND Strekkode NO-LOCK WHERE
           Strekkode.Kode = ENTRY(6,cLinje) NO-ERROR.
       IF AVAILABLE Strekkode THEN
           FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.

       /* Leser inn data fra record.    */
       /* VAL,,,,E,7023890030165,00001, */
       ASSIGN                  
           tmpPDA.ButNr      = int(entry(2,VPIFilHode.FilNavn,"."))     
           tmpPDA.Ean        = dec(ENTRY(6,cLinje))        
           tmpPDA.Dato       = VPIFilHode.Dato     
           tmpPDA.Tid        = int(ENTRY(1,VPIFilHode.Kl,":")) * 3600 + 
                               int(ENTRY(2,VPIFilHode.Kl,":")) * 60   +
                               int(ENTRY(3,VPIFilHode.Kl,":"))
           tmpPDA.Loggnr     = 0   
           tmpPDA.Transtype  = 7 
           tmpPDA.Transtekst = (IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE "")
           tmpPDA.Brukerid   = ""  
           tmpPDA.Antall     = dec(entry(7,cLinje))
           tmpPDA.Kostpris   = 0  
           tmpPDA.Salgssum   = 0
           tmpPDA.Nylagant   = 0
           tmpPDA.Gmlagant   = 0
           .

       /* Sjekker om det er en kjent EAN kode */
       IF NOT CAN-FIND(Strekkode WHERE
                       Strekkode.Kode = trim(string(tmpPDA.Ean))) THEN
           cUkjentEAN = 'Ukjent EAN'.
       ELSE cUkjentEAN = "".

       ASSIGN
           VPIFilLinje.Tekst      = cLinje
           VPIFilLinje.StorTekst  = trim(string(tmpPDA.Butnr)) + ";" +      
                                    trim(string(tmpPDA.Ean)) + ";" +        
                                    trim(string(tmpPDA.Dato)) + ";" +       
                                    trim(string(tmpPDA.Tid)) + ";" +        
                                    trim(string(tmpPDA.Loggnr)) + ";" +    
                                    trim(string(tmpPDA.Transtype)) + ";" + 
                                    trim(string(tmpPDA.Transtekst)) + ";" +
                                    trim(string(tmpPDA.Brukerid)) + ";" +  
                                    trim(string(tmpPDA.Antall)) + ";" +    
                                    trim(string(tmpPDA.Kostpris)) + ";" +  
                                    trim(string(tmpPDA.Salgssum)) + ";" +  
                                    trim(string(tmpPDA.Nylagant)) + ";" +  
                                    trim(string(tmpPDA.Gmlagant)) + ";" +
                                    STRING(iAntLinjer) + ";" +
                                    cUkjentEAN
                                    .
    END. /* VPIFILLINJE TRANSACTION */

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

