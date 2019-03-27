&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xbxljustinnles.p
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
DEF VAR cTekst     AS CHAR NO-UNDO.
DEFINE VARIABLE iButikkNr  AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTTID      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iInt       AS INTEGER   NO-UNDO.
DEFINE VARIABLE bOk AS LOG NO-UNDO.

DEF STREAM InnFil.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR.

{windows.i}
{overforing.i}

DEFINE TEMP-TABLE impOverfor LIKE tmpOverfor.

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
    cErrFil  = OS-GETENV('TMP') + '\' + "Errxbxoverforinnles.txt"
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.
    
/* Filen finnes ikke */
IF SEARCH(cFilNavn) = ? THEN
  RETURN.

RUN LesInnFil.

/*
bOk = TEMP-TABLE impOverfor:WRITE-JSON('file', 'konv\overfor.json', TRUE).
*/

RUN OppdaterFil.

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

  FIND FIRST impOverfor NO-LOCK NO-ERROR.
  ASSIGN
      cFilnavn = OS-GETENV('TMP') + '\' 
                 + "Error_" 
                 + entry(1,VPIFilHode.FilNavn,".")
                 + ".Txt".
  
  OUTPUT TO VALUE(cFilnavn).
    PUT UNFORMATTED
      "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn SKIP
      .
    IF AVAILABLE impOverfor THEN DO:
        PUT UNFORMATTED
            "Overføringsordre " STRING(impOverfor.OrdreNr) SKIP.
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

Eks:  10 entries. Ingen transaksjonskode.    
8584453;1;'2003-01-03 06:59:28.662';3;0710845610394;System;;;090;3
2538053;1;'2003-01-03 06:59:56.525';3;0200003139824;System;;;090;3
2535985;1;'2003-01-03 07:00:09.443';3;0200003081024;System;;;090;3
------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr  AS INT  NO-UNDO.
  DEF VAR piAntFeil  AS INT  NO-UNDO.
  DEF VAR cBackup    AS CHAR NO-UNDO.
  DEF VAR plVareNr   AS DEC  NO-UNDO.
  DEF VAR pcStr      AS CHAR NO-UNDO.
  DEF VAR piLoop     AS INT  NO-UNDO.
  DEF VAR cUkjentEAN AS CHAR NO-UNDO.
  DEF VAR pcLinje    AS CHAR NO-UNDO.
  DEF VAR pcEAN      AS CHAR NO-UNDO.
  DEF VAR pcDato     AS CHAR NO-UNDO.
  DEFINE VARIABLE pcTid AS CHARACTER NO-UNDO.
  DEFINE VARIABLE piTid AS INTEGER   NO-UNDO. 
  DEF VAR pdDato     AS DATE NO-UNDO.
  DEF VAR pfEAN      AS DEC  NO-UNDO.
  DEFINE VARIABLE cMBut AS CHARACTER NO-UNDO.
  
  /* Tømmer feillogg. */
  FOR EACH tt_Error:
    DELETE tt_Error.
  END.
  EMPTY TEMP-TABLE impOverfor. 
   
  RUN TellOppLinjer.

  ASSIGN
      piLinjeNr  = 1.
      iAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      pcLinje    = ''.

    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED 
        pcLinje NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Finner ikke fil " + cFilNavn + '.'.
        NEXT LESERLINJER.
    END.
    
    /* tomme linjer skippes. */
    IF TRIM(pcLinje) = '' THEN 
        NEXT.

    IF NUM-ENTRIES(pcLinje,';') < 12 THEN 
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** For få antall entries på linje " + STRING(iAntLinjer) + '.'.
        NEXT LESERLINJER.
    END.

    ASSIGN /* Dato og tid fra fil: '2012-11-4 12:28:00.000' */
        pcEAN   = TRIM(ENTRY(5,pcLinje,";"))
        cTTID   = TRIM(ENTRY(11,pcLinje,";"))
        cMBut   = IF TRIM(ENTRY(12,pcLinje,";")) <> '' THEN TRIM(ENTRY(12,pcLinje,";")) ELSE TRIM(ENTRY(4,pcLinje,";"))
        .

    /* Legger på ledende nuller i EAN koden */
    IF LENGTH(pcEAN) > 6 AND length(pcEAN) < 13 THEN
        pcEAN = FILL("0",13 - LENGTH(pcEAN)) + pcEAN.
        .
    RUN bibl_chkean.p (INPUT-OUTPUT pcEAN).

    /* Sjekk av butikknr på mottagende butikk. */
    ASSIGN iInt = INT(cMBut) NO-ERROR.
    IF ERROR-STATUS:ERROR OR NOT CAN-FIND(Butiker WHERE Butiker.Butik = iInt) THEN 
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " ** Ukjent mottagende butikk " + cMBut.
        NEXT LESERLINJER.
    END.
    
    /* Skal bare ha inn overføringstransaksjoner */
    IF cTTID <> '4' THEN 
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " Feil transaksjonstype (Skal være 4 fra pda): " + cTTID + '.'.
        NEXT LESERLINJER.
    END.

    /* Kontroll av EAN kode */
    ASSIGN
        pfEAN = DEC(pcEAN)
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " Feil i EAN " + pcEAN + '.'.
        NEXT LESERLINJER.
    END.

    FIND Strekkode NO-LOCK WHERE
        Strekkode.Kode = pcEAN NO-ERROR.
    IF AVAILABLE Strekkode THEN
        FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
    IF NOT AVAILABLE StrekKode OR NOT AVAILABLE ArtBas THEN 
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " Ukjent EAN/Vare: " + pcEAN + '.'.
        NEXT LESERLINJER.
    END. 

    /* Sjekker butikk */
    ASSIGN iButikkNr = INT(ENTRY(10,pcLinje,";")) NO-ERROR.
    IF ERROR-STATUS:ERROR OR NOT CAN-FIND(Butiker WHERE Butiker.Butik = iButikkNr) THEN
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " ** Ukjent butikknr " + ENTRY(10,pcLinje,";").
        NEXT LESERLINJER.
    END.
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = iButikkNr NO-ERROR.
    FIND FIRST StrKonv NO-LOCK WHERE 
        StrKonv.StrKode = StrekKode.StrKode NO-ERROR. 

    CREATE impOverfor.
    ASSIGN
        impOverfor.ArtikkelNr = ArtBas.ArtikkelNr
        impOverfor.Vg         = ArtBas.Vg
        impOverfor.LopNr      = ArtBas.LopNr
        impOverfor.FraBut     = iButikkNr
        impOverfor.TilBut     = INT(cMBut)
        impOverfor.FraStorl   = (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')
        impOverfor.TilStorl   = impOverfor.FraStorl
        impOverfor.Antall     = DECIMAL(REPLACE(ENTRY(2,pcLinje,";"),".",","))
        impOverfor.BuntNr     = 999 /* dummy */
        impOverfor.Kode       = pcEAN
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " ** Feil i assign av linje felt ".
        IF AVAILABLE impOverfor THEN DELETE impOverfor.
        NEXT LESERLINJER.
    END.
        
    IF NUM-ENTRIES(pcLinje,';') >= 14 THEN 
        ASSIGN
        impOverfor.OrdreNr = ENTRY(13,pcLinje,';') 
        impOverfor.Rab%    = DECIMAL(REPLACE(ENTRY(14,pcLinje,";"),".",","))
        NO-ERROR. 
    IF ERROR-STATUS:ERROR THEN
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " ** Feil i assign av ordre og rabattnr felt ".
        IF AVAILABLE impOverfor THEN DELETE impOverfor.
        NEXT LESERLINJER.
    END.

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
          VPIFilHode.VPIFilStatus = 5
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

&IF DEFINED(EXCLUDE-OppdaterFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterFil Procedure 
PROCEDURE OppdaterFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  Oppretter overføringsbong.     
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ihbuffer AS HANDLE NO-UNDO.
    DEFINE VARIABLE pcReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pbOk     AS LOG NO-UNDO.     

    EMPTY TEMP-TABLE tmpOverfor.
    
    SKAPER_BONG:
    FOR EACH impOverfor 
        BREAK BY impOverfor.OrdreNr
              BY impOverfor.FraBut
              BY impOverfor.TilBut:
                  
        CREATE tmpOverfor.
        BUFFER-COPY impOverfor
            TO tmpOverfor.

        /* Oppretter bong. pr. overføring. Det kan ligge overføring til flere butikker i filen. */
        IF LAST-OF(impOverfor.TilBut) THEN
        DO:
            ihBuffer = BUFFER tmpOverfor:HANDLE.
            RUN ovbunt_overfor.p (STRING(impOverfor.FraBut) + '|' + STRING(impOverfor.TilBut),
                               ihBuffer,
                               '',
                               OUTPUT pcReturn,
                               OUTPUT pbOk
                              ).

            EMPTY TEMP-TABLE tmpOverfor.
        END.
    END. /* SKAPER_BONG */
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

