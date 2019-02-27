&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xbxvmotinnles.p
    Purpose     :

    Syntax      :



    Author(s)   : Tom Nøkleby
    Created     : 
    Notes       : Import av varemottak/pakkseddel fra PDA.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR l2FilId       AS DEC    NO-UNDO.
DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.
DEF VAR cFilPkSdl     AS CHAR NO-UNDO.
DEF VAR cVPIFil       AS CHAR NO-UNDO.
DEF VAR cErrFil       AS CHAR NO-UNDO.
DEF VAR ctmpKatalog   AS CHAR NO-UNDO.
DEF VAR pcLinje       AS CHAR NO-UNDO.
DEF VAR cPrefiks      AS CHAR NO-UNDO.
DEF VAR cTekst        AS CHAR NO-UNDO.
DEFINE VARIABLE iButikkNr     AS INTEGER NO-UNDO.
DEFINE VARIABLE cTTID         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iInt          AS INTEGER NO-UNDO.
DEFINE VARIABLE iCL           AS INTEGER NO-UNDO.
DEFINE VARIABLE iProfilNr     AS INTEGER NO-UNDO.
DEFINE VARIABLE iClProfilNr   AS INTEGER NO-UNDO.
DEFINE VARIABLE iLevNr        AS INTEGER NO-UNDO.
DEFINE VARIABLE cKatalogPkSdl AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAnt          AS INTEGER NO-UNDO.

DEFINE STREAM InnFil.
DEFINE STREAM UtPkSdl.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR.

DEFINE TEMP-TABLE ttPKSdl
    /*  1 */ FIELD RecTyp AS CHARACTER
    /*  2 */ FIELD PkSdlNr AS CHARACTER 
    /*  3 */ FIELD OrdreLevNr AS CHARACTER 
    /*  4 */ FIELD Dummy4 AS CHARACTER 
    /*  5 */ FIELD EkstOrdreNr AS CHARACTER 
    /*  6 */ FIELD Dummy6 AS CHARACTER 
    /*  7 */ FIELD SendtDato AS CHARACTER 
    /*  8 */ FIELD Dummy8 AS CHARACTER 
    /*  9 */ FIELD LevKod AS CHARACTER 
    /* 10 */ FIELD Varetekst AS CHARACTER 
    /* 11 */ FIELD OrdreLevDato AS CHARACTER 
    /* 12 */ FIELD ValutaPris AS CHARACTER 
    /* 13 */ FIELD InnkjPris AS CHARACTER 
    /* 14 */ FIELD RabKr AS CHARACTER 
    /* 15 */ FIELD Rab% AS CHARACTER 
    /* 16 */ FIELD Pris AS CHARACTER 
    /* 17 */ FIELD OrdreBestNr AS CHARACTER 
    /* 18 */ FIELD ButikkNr AS CHARACTER 
    /* 19 */ FIELD Str AS CHARACTER 
    /* 20 */ FIELD Dummy20 AS CHARACTER 
    /* 21 */ FIELD EAN AS CHARACTER 
    /* 22 */ FIELD Varekost AS CHARACTER 
    /* 23 */ FIELD BestAnt AS CHARACTER 
    /* 24 */ FIELD LevertAnt AS CHARACTER
    INDEX Pakkseddel ButikkNr OrdreLevNr PkSdlNr EkstOrdreNr 
    . 

DEFINE BUFFER clButiker FOR Butiker.

{xppt880.i &NEW="new" &SHARED="shared" &FIELD=" FIELD OrdreNo AS CHAR FORMAT 'x(15)'  FIELD PackingNo AS CHAR FORMAT 'x(15)' "}
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

{syspara.i 5 1 1 iCl INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE clButiker THEN 
  RETURN.
iClProfilNr = clButiker.ProfilNr. 

/* Setter leverandørnnumer som skal benyttes. */
{syspara.i 210 100 1 iLevNr INT}
IF iLevNr = 0 THEN
    iLevNr = 40.

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cErrFil       = OS-GETENV('TMP') + '\' + "Errxbxvmotinnles.txt"
    cFilNavn      = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.
    
/* Filen finnes ikke */
IF SEARCH(cFilNavn) = ? THEN
  RETURN.

RUN LesInnFil.

RUN OppdaterFil.

RUN eksporterFil.

IF CAN-FIND(FIRST tt_Error) THEN
    RUN ErrorLogg.

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

 
&IF DEFINED(EXCLUDE-eksporterFil) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE eksporterFil Procedure
PROCEDURE eksporterFil:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:
	------------------------------------------------------------------------------*/
DEFINE VARIABLE cFil2 AS CHARACTER NO-UNDO.

FOR EACH ttPkSdl 
  BREAK BY ttPkSdl.ButikkNr
        BY ttPkSdl.OrdreLevNr
        BY ttPkSdl.EkstOrdreNr
        BY ttPkSdl.PkSdlNr:

  IF FIRST-OF(ttPkSdl.PkSdlNr) THEN 
  DO:
    ASSIGN 
        cKatalogPkSdl = VPIFilHode.Katalog
        cFilPkSdl     = cKatalogPkSdl + '\' + '_PRSPkSdl-' + ttPkSdl.ButikkNr + '-' + ttPkSdl.OrdreLevNr + '-'+ ttPkSdl.EkstOrdreNr + '-' + ttPkSdl.PkSdlNr + '.csv'
        cFil2         = cFilPkSdl
        .
        
    FIND LevBas NO-LOCK WHERE 
        LevBas.LevNr = INT(ttPkSdl.OrdreLevNr) NO-ERROR.
    OUTPUT STREAM UtPkSdl TO VALUE(cFilPkSdl) NO-ECHO.
    PUT STREAM UtPkSdl UNFORMATTED
        /*  1 */ '99;'
        /*  2 */ ttPkSdl.EkstOrdreNr  ';'
        /*  3 */ ttPkSdl.SendtDato    ';'
        /*  4 */ 'BxMobile;'
        /*  5 */ ttPkSdl.PkSdlNr      ';'
        /*  6 */ ttPkSdl.OrdreLevNr   ';'
        /*  7 */ (IF AVAILABLE LevBas THEN LevBas.LevNamn ELSE '') 
        SKIP.
  END.  
    
  PUT STREAM UtPkSdl UNFORMATTED 
      /*  1 */ ttPkSdl.RecTyp       ';'
      /*  2 */ ttPkSdl.PkSdlNr      ';'
      /*  3 */ ttPkSdl.OrdreLevNr   ';'
      /*  4 */ ttPkSdl.Dummy4       ';'
      /*  5 */ ttPkSdl.EkstOrdreNr  ';'
      /*  6 */ ttPkSdl.Dummy6       ';'
      /*  7 */ ttPkSdl.SendtDato    ';'
      /*  8 */ ttPkSdl.Dummy8       ';'
      /*  9 */ ttPkSdl.LevKod       ';'
      /* 10 */ ttPkSdl.Varetekst    ';'
      /* 11 */ ttPkSdl.OrdreLevDato ';'
      /* 12 */ ttPkSdl.ValutaPris   ';'
      /* 13 */ ttPkSdl.InnkjPris    ';'
      /* 14 */ ttPkSdl.RabKr        ';'
      /* 15 */ ttPkSdl.Rab%         ';'
      /* 16 */ ttPkSdl.Pris         ';'
      /* 17 */ ttPkSdl.OrdreBestNr  ';'
      /* 18 */ ttPkSdl.ButikkNr     ';'
      /* 19 */ ttPkSdl.Str          ';'
      /* 20 */ ttPkSdl.Dummy20      ';'
      /* 21 */ ttPkSdl.EAN          ';' 
      /* 22 */ ttPkSdl.Varekost     ';'
      /* 23 */ ttPkSdl.BestAnt      ';'
      /* 24 */ ttPkSdl.LevertAnt
      SKIP. 
    
  IF LAST-OF(ttPkSdl.PkSdlNr) THEN 
  DO:
    OUTPUT STREAM UtPkSdl CLOSE.
    IF SEARCH(cFilPkSdl) <> ? THEN 
    DO:
      RUN OpprettFilHode.
      IF l2FilId <> ? THEN 
          RUN xPRSPkSdlInnles.p (l2FilId, h_Parent, OUTPUT iAnt).
    END.    
  END.  
          
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
      "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn SKIP
      .
    IF AVAILABLE tmpPDA THEN DO:
        PUT UNFORMATTED
            "Varemottak " STRING(tmpPDA.Dato) " " tmpPDA.BrukerId " " STRING(tmpPDA.Tid,"HH:MM:SS") SKIP.
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
    ASSIGN
      iAntLinjer = iAntLinjer + 1.

    CREATE tmpPDA.
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
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer).

        IF AVAILABLE tmpPDA THEN DELETE tmpPDA.
        NEXT LESERLINJER.
    END.
    
    IF pcLinje BEGINS ';;;;;;' THEN 
        NEXT LESERLINJER.

    ASSIGN
        pfEAN = DEC(ENTRY(5,pcLinje,";"))
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT LESERLINJER.

    IF TRIM(ENTRY(5,pcLinje,";")) = '' THEN
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Blank strekkode på linje " + STRING(iAntLinjer) + '. ' + pcLinje.

        IF AVAILABLE tmpPDA THEN DELETE tmpPDA.
        NEXT LESERLINJER.
    END.

    ASSIGN /* Dato og tid fra fil: '2012-11-4 12:28:00.000' */
        pcEAN   = TRIM(ENTRY(5,pcLinje,";"))
        cTekst  = RIGHT-TRIM(LEFT-TRIM(TRIM(ENTRY(3,pcLinje,";")),"'"),"'")
        pcDato  = ENTRY(1,cTekst,' ')
        pcTid   = ENTRY(2,cTekst,' ')
        pcTid   = ENTRY(1,pcTid,'.')
        cTTID   = TRIM(ENTRY(11,pcLinje,";"))
        .

    /* Legger på ledende nuller i EAN koden */
    IF LENGTH(pcEAN) > 6 AND LENGTH(pcEAN) < 13 THEN
        pcEAN = FILL("0",13 - LENGTH(pcEAN)) + pcEAN.
        .

    /* Skal bare ha inn varemottakstransaksjoner */
    IF NOT CAN-DO('4,8',cTTID) THEN 
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " ** Feil transaksjonstype (Skal være 4 eller 8) " + cTTID.

        IF AVAILABLE tmpPDA THEN DELETE tmpPDA.
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
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " ** Feil i EAN " + pcEAN.

        IF AVAILABLE tmpPDA THEN DELETE tmpPDA.
        NEXT LESERLINJER.
    END.

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
        pdDato = DATE(INT(ENTRY(2,pcDato,'-')),
                      INT(ENTRY(3,pcDato,'-')),
                      INT(ENTRY(1,pcDato,'-')))
        */
        NO-ERROR.
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
    /* Trekker ut tid 12:28:00 */
    ASSIGN
        piTid = (INT(SUBSTRING(pcTid,1,2)) * 60 * 60) + 
                (INT(SUBSTRING(pcTid,4,2)) * 60) +
                 INT(SUBSTRING(pcTid,7,2))
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " ** Feil i tidsangivelse " + pcDato.

        IF AVAILABLE tmpPDA THEN DELETE tmpPDA.
        NEXT LESERLINJER.
    END.

    /* Sjekker butikk */
    IF TRIM(ENTRY(4,pcLinje,";")) <> '' THEN 
        ASSIGN iButikkNr = INT(ENTRY(4,pcLinje,";")) NO-ERROR.
    ELSE 
        ASSIGN iButikkNr = INT(ENTRY(10,pcLinje,";")) NO-ERROR.
    IF ERROR-STATUS:ERROR OR NOT CAN-FIND(Butiker WHERE Butiker.Butik = iButikkNr) THEN
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
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = iButikkNr NO-ERROR.

    ASSIGN
        tmpPDA.Butnr        = iButikkNr
        tmpPDA.Ean          = DEC(pcEAN)
        tmpPDA.Dato         = pdDato
        tmpPDA.Tid          = piTid
        tmpPDA.Loggnr       = 0
        tmpPDA.Transtype    = 5 /* Varemottak */
        tmpPDA.Transtekst   = cTekst
        tmpPDA.Brukerid     = REPLACE(ENTRY(9,pcLinje,";"),' ','_')
        tmpPDA.Antall       = DECIMAL(REPLACE(ENTRY(2,pcLinje,";"),".",","))
        tmpPDA.Kostpris     = 0
        tmpPDA.Salgssum     = 0
        tmpPDA.Nylagant     = 0
        tmpPDA.Gmlagant     = 0
        tmpPDA.LinjeNr      = iAntLinjer
        tmpPda.OrdreNo      = TRIM(ENTRY(12,pcLinje,";"))
        tmpPda.PackingNo    = TRIM(ENTRY(13,pcLinje,";"))
        NO-ERROR.
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
  /*
  IF SEARCH(cFilNavn) <> ? THEN 
  DO:
    cBackup = REPLACE(cFilNavn,VPIFilHode.FilNavn,'bku\' + VPIFilHode.FilNavn). 
    OS-COPY VALUE(cFilNavn) VALUE(cBackup).

    IF SEARCH(cBackup) <> ? THEN DO:
      OS-COMMAND VALUE('del ' + cFilNavn).
    END.
  END.
  */
  
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
  Notes:  Oppretter overføringsordre.     
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iLinjeNr   AS INTEGER NO-UNDO.
  DEFINE VARIABLE cEANNr     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE wVVareKost AS DECIMAL NO-UNDO.
  
  OPPDATERING:
  FOR EACH tmpPDA WHERE tmpPDA.EAN > 0
      BREAK BY tmpPDA.ButNr
            BY tmpPDA.BrukerId
            BY tmpPDA.Dato:

      cEANNr = STRING(tmpPDA.EAN).
      RUN bibl_chkean.p (INPUT-OUTPUT cEANNr).
      FIND Strekkode NO-LOCK WHERE
        Strekkode.Kode = cEANNr NO-ERROR.
      IF AVAILABLE Strekkode THEN 
        FIND StrKonv OF Strekkode NO-LOCK NO-ERROR.
      IF AVAILABLE StrKonv THEN
      SKAPELSEN:
      DO: 
        FIND ArtBas NO-LOCK WHERE 
          ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN LEAVE SKAPELSEN.

        FIND Butiker NO-LOCK WHERE 
            Butiker.Butik = tmpPDA.ButNr NO-ERROR.
        IF NOT AVAILABLE Butiker THEN LEAVE SKAPELSEN.
        FIND ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
            ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
        FIND FIRST ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN LEAVE SKAPELSEN.
        
        /* Setter VVareKost. */
        FIND Lager NO-LOCK WHERE Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
                                 Lager.Butik      = tmpPDA.ButNr NO-ERROR.
        /* Setter varekost */
        IF AVAILABLE Lager THEN
            wVVareKost = Lager.VVareKost.
        ELSE   
            wVVareKost = 0.

        IF (wVVareKost = 0 OR wVVareKost = ? OR ArtBas.Lager = FALSE) THEN 
        DO:
            IF AVAILABLE ArtPris THEN
                 wVVareKost = ArtPris.VareKost[1]. /* Tar alltid normalpris */
        END.
        /* Ukjent varekost */
        IF wVVareKost = ? THEN
            wVVareKost = 0.              

        CREATE ttPkSdl.
        ASSIGN
                   iLinjeNr             = iLinjeNr + 1
          /*  1 */ ttPkSdl.RecTyp       = '1'
          /*  2 */ ttPkSdl.PkSdlNr      = tmpPda.PackingNo
          /*  3 */ ttPkSdl.OrdreLevNr   = STRING(ArtBAs.LevNr)
          /*  4 */ ttPkSdl.Dummy4       = ''
          /*  5 */ ttPkSdl.EkstOrdreNr  = tmpPda.OrdreNo
          /*  6 */ ttPkSdl.Dummy6       = ''
          /*  7 */ ttPkSdl.SendtDato    = STRING(tmpPDA.Dato)
          /*  8 */ ttPkSdl.Dummy8       = ''
          /*  9 */ ttPkSdl.LevKod       = ArtBas.LevKod
          /* 10 */ ttPkSdl.Varetekst    = tmpPDA.Transtekst
          /* 11 */ ttPkSdl.OrdreLevDato = STRING(tmpPDA.Dato)
          /* 12 */ ttPkSdl.ValutaPris   = STRING(ArtPris.ValPris[1])
          /* 13 */ ttPkSdl.InnkjPris    = STRING(ArtPris.InnkjopsPris[1])
          /* 14 */ ttPkSdl.RabKr        = STRING(ArtPris.Rab1Kr[1])
          /* 15 */ ttPkSdl.Rab%         = STRING(ArtPris.Rab1%[1])
          /* 16 */ ttPkSdl.Pris         = STRING(ArtPris.Pris[1])
          /* 17 */ ttPkSdl.OrdreBestNr  = ''
          /* 18 */ ttPkSdl.ButikkNr     = STRING(tmpPDA.ButNr)
          /* 19 */ ttPkSdl.Str          = StrKonv.Storl
          /* 20 */ ttPkSdl.Dummy20      = ''
          /* 21 */ ttPkSdl.EAN          = Strekkode.Kode 
          /* 22 */ ttPkSdl.Varekost     = STRING(wVVareKost)
          /* 23 */ ttPkSdl.BestAnt      = STRING(tmpPDA.Antall)
          /* 24 */ ttPkSdl.LevertAnt    = STRING(tmpPDA.Antall) 
          .
      END. /* SKAPELSEN */
      
  END. /* UTLEGG */
          
          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-OpprettFilHode) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettFilHode Procedure
PROCEDURE OpprettFilHode:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/

DEFINE BUFFER bufVPIFilHode FOR VPIFilHode.

DO FOR bufVPIFilHode:
        FILE-INFO:FILE-NAME = cFilPkSdl.

        /* Finner FilId */
        FIND LAST bufVPIFilHode NO-LOCK NO-ERROR.
        IF AVAILABLE bufVPIFilHode THEN
          l2FilId = bufVPIFilHode.FilId + 1.
        ELSE
          l2FilId = 1.
        DO TRANSACTION:
            CREATE bufVPIFilHode.
            ASSIGN
              bufVPIFilHode.FilId        = l2FilId
              bufVPIFilHode.FilNavn      = ENTRY(NUM-ENTRIES(cFilPkSdl,'\'),cFilPkSdl,'\')
              bufVPIFilHode.Katalog      = VPIFilHode.Katalog
              bufVPIFilHode.Dato         = FILE-INFO:FILE-MOD-DATE
              bufVPIFilHode.Kl           = STRING(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS")
              bufVPIFilHode.Storrelse    = FILE-INFO:FILE-SIZE
              bufVPIFilHode.AntLinjer    = 0
              bufVPIFilHode.VPIFilType   = 2 
              bufVPIFilHode.VPIFilStatus = 1
              bufVPIFilHode.EkstVPILevNr = VPIFilHode.EkstVPILevNr
              .
            RELEASE bufVPIFilHode.
            
        END. /* TRANSACTION */
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

