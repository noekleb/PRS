&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xbxpriskinnles.p
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
DEFINE VARIABLE iButikkNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cTTID     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iInt      AS INTEGER NO-UNDO.
DEF STREAM InnFil.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR.

{xppt880.i &NEW="new" &SHARED="shared" &FIELD=" FIELD MButNr AS INT  FORMAT '>>>>>9' "}
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
    cErrFil  = OS-GETENV('TMP') + '\' + "Errxbxoverforinnles.txt"
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.
    
/* Filen finnes ikke */
IF SEARCH(cFilNavn) = ? THEN
  RETURN.

RUN LesInnFil.
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
            "Overføringsordre " STRING(tmpPDA.Dato) " " tmpPDA.BrukerId " " STRING(tmpPDA.Tid,"HH:MM:SS") SKIP.
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

    ASSIGN /* Dato og tid fra fil: '2012-11-4 12:28:00.000' */
        pcEAN   = TRIM(ENTRY(6,pcLinje,";"))
        cTekst  = RIGHT-TRIM(LEFT-TRIM(TRIM(ENTRY(4,pcLinje,";")),"'"),"'")
        pcDato  = ENTRY(1,cTekst,' ')
        pcTid   = ENTRY(2,cTekst,' ')
        pcTid   = ENTRY(1,pcTid,'.')
        cTTID   = TRIM(ENTRY(11,pcLinje,";"))
        .

    /* Legger på ledende nuller i EAN koden */
    IF LENGTH(pcEAN) > 6 AND length(pcEAN) < 13 THEN
        pcEAN = FILL("0",13 - LENGTH(pcEAN)) + pcEAN.
        .

    /* Skal bare ha inn overføringstransaksjoner */
    IF cTTID <> '5' THEN 
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " ** Feil transaksjonstype (Skal være 5) " + cTTID.

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
        pdDato = DATE(INT(ENTRY(2,pcDato,'-')),
                      INT(ENTRY(3,pcDato,'-')),
                      INT(ENTRY(1,pcDato,'-')))
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
    ASSIGN iButikkNr = INT(ENTRY(10,pcLinje,";")) NO-ERROR.
    IF ERROR-STATUS:ERROR OR NOT CAN-FIND(Butiker WHERE Butiker.Butik = iButikkNr) THEN
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " ** Ukjent butikknr " + ENTRY(10,pcLinje,";").

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
        tmpPDA.Transtype    = 7 /* Lagerjustering */
        tmpPDA.Transtekst   = cTekst
        tmpPDA.Brukerid     = REPLACE(ENTRY(9,pcLinje,";"),' ','_')
        tmpPDA.Antall       = DECIMAL(REPLACE(ENTRY(3,pcLinje,";"),".",","))
        tmpPDA.Kostpris     = DECIMAL(REPLACE(ENTRY(8,pcLinje,";"),".",","))
        tmpPDA.Salgssum     = 0
        tmpPDA.Nylagant     = 0
        tmpPDA.Gmlagant     = 0
        tmpPDA.LinjeNr      = iAntLinjer
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
  Notes:  Oppretter overføringsordre.     
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cEANNr     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ocReturn   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE obOk       AS LOG NO-UNDO.
  DEFINE VARIABLE iBuntNr    AS INTEGER NO-UNDO.
  DEFINE VARIABLE lMvaKr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lDbKr  AS DECIMAL NO-UNDO.
              
  
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
      PRISKLINJEN:
      DO TRANSACTION:  
        FIND ArtBas NO-LOCK WHERE 
          ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN LEAVE PRISKLINJEN.

        FIND Farg OF ArtBas NO-LOCK NO-ERROR.
              
        FIND Butiker NO-LOCK WHERE 
          Butiker.Butik = tmpPDA.ButNr NO-ERROR.
        FIND ArtPris NO-LOCK WHERE
             ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
             ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
          FIND FIRST ArtPris NO-LOCK WHERE
             ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.

        /* Oppstandelsen */
        CREATE VPIMottak.
        ASSIGN
            VPIMottak.BehStatus        = 1
            VPIMottak.VPIType          = 6 /* PDA VPI */
            VPIMottak.ArtikkelNr       = ArtBas.ArtikkelNr
            VPIMottak.LevKod           = ArtBas.LevKod
            VPIMottak.Beskr            = ArtBas.Beskr
            VPIMottak.LevFargKod       = ArtBas.LevFargKod
            VPIMottak.InnkjopsPris     = ArtPris.InnkjopsPris[1]
            VPIMottak.Varekost         = ArtPris.Varekost[1]
            VPIMottak.Rab1%            = ArtPris.Rab1%[1]
            VPIMottak.Mva%             = ArtPris.Mva%[1]
            VPIMottak.OrgPris          = ArtPris.Pris[1]
            VPIMottak.OrgDb%           = ArtPris.Db%[1]
            VPIMottak.ProfilNr         = ArtPris.ProfilNr
            VPIMottak.MomsKod          = ArtPris.MomsKod[1]
            VPIMottak.Vg               = ArtBas.Vg
            VPIMottak.Sasong           = ArtBas.Sasong
            VPIMottak.LevNr            = ArtBas.LevNr
              
            /* Utpris overstyres med pris fra PDA. */
            VPIMottak.Pris             = tmpPDA.Kostpris
            lMvaKr                     = VPIMottak.Pris * (ArtPris.Mva%[1] / (100 + ArtPris.Mva%[1]))
            lDbKr                      = VPIMottak.Pris - lMvaKr - ArtPris.Varekost[1]

            /* Regnes om i forhold til gammel utpris som fortsatt gjelder. */
            VPIMottak.Db%              = ROUND((lDbKr * 100)/ (VPIMottak.Pris - lMvaKr),2)
            VPIMottak.Db%              = IF VPIMottak.Db% = ? THEN 0 ELSE VPIMottak.Db%
            
            /* Endringer i innpris skal aktiveres direkte. */
            VPIMottak.AktiveresDato    = TODAY 
            VPIMottak.OrgAktiveresDato = TODAY 
            VPIMottak.GyldigTilDato    = ?
            VPIMottak.OrgGyldigTilDato = ?
            .
                  
        RELEASE VPIMottak. /* Denne MÅ få stå */  
      END. /* PRISKLINJEN TRANSACTION */
      
      /* Beregner summer og lukker lokasjonsliste */
      IF LAST-OF(tmpPDA.Dato) THEN
      DO:

      END.
  END. /* UTLEGG */
          
          
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

