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

DEF VAR piAntFeil  AS INT  NO-UNDO. /* flyttat från lesinnfil för att logga artiklar som inte finns i Oppdater */


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
DEFINE VARIABLE cTTId AS CHARACTER NO-UNDO.
DEF VAR h_dproclib AS HANDLE NO-UNDO.

DEFINE VARIABLE iButikkNr AS INTEGER NO-UNDO.

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
    cErrFil  = OS-GETENV('TMP') + '\' + "ErrBxWuinnles.txt"
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.
    
/* Filen finnes ikke */
IF SEARCH(cFilNavn) = ? THEN
  RETURN.

RUN dproclib.w PERSISTENT SET h_dproclib.

RUN LesInnFil.
RUN OppdaterFil.
IF CAN-FIND(FIRST tt_Error) THEN
    RUN ErrorLogg.

IF VALID-HANDLE(h_dproclib) THEN
    DELETE PROCEDURE h_dproclib.

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
            "Lok.liste " STRING(tmpPDA.Dato) " " tmpPDA.BrukerId " " STRING(tmpPDA.Tid,"HH:MM:SS") SKIP.
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
  DEF VAR iVg        AS INT NO-UNDO.
  DEF VAR iLopNr     AS INT NO-UNDO.
  DEF VAR iStrKode   AS INT NO-UNDO.
  DEF VAR cProduktNr AS CHAR NO-UNDO.
  DEF VAR piLoop     AS INT  NO-UNDO.
  DEF VAR cUkjentEAN AS CHAR NO-UNDO.
  DEF VAR pcLinje    AS CHAR NO-UNDO.
  DEF VAR pcEAN      AS CHAR NO-UNDO.
  DEF VAR pcEAN_7    AS CHAR NO-UNDO.
  DEF VAR pcDato     AS CHAR NO-UNDO.
  DEFINE VARIABLE pcTid AS CHARACTER NO-UNDO.
  DEFINE VARIABLE piTid AS INTEGER   NO-UNDO. 
  DEF VAR pdDato     AS DATE NO-UNDO.
  DEF VAR pfEAN      AS DEC  NO-UNDO.
  DEFINE VARIABLE dVikt AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cVGlopnr AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cVikt AS CHARACTER   NO-UNDO.
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
        cProduktNr = TRIM(ENTRY(2,pcLinje,";"))
        pcEAN   = TRIM(ENTRY(2,pcLinje,";"))
        cTekst  = RIGHT-TRIM(LEFT-TRIM(TRIM(ENTRY(4,pcLinje,";")),"'"),"'")
        pcDato  = ENTRY(1,cTekst,' ')
        pcTid   = ENTRY(2,cTekst,' ')
        pcTid   = ENTRY(1,pcTid,'.')
        cTTId   = TRIM(ENTRY(11,pcLinje,";"))
        .
    /* servicehandel: vi har upptäckt att checksiffran inte kommer med på vissa EAN8 */
    /* och vi använder pcEAN_7 om vi inte har hittat streckkod på vanligt sätt */
    IF LENGTH(pcEAN) = 7 THEN
        pcEAN_7 = "00000" + pcEAN.
    ELSE
        pcEAN_7 = "".
    /* Legger på ledende nuller i EAN koden */
    IF LENGTH(pcEAN) > 6 AND LENGTH(pcEAN) < 13 THEN
        pcEAN = FILL("0",13 - LENGTH(pcEAN)) + pcEAN.
        .
        
    /* Skal bare ha inn telletransaksjoner */
    IF cTTID <> '7' THEN 
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " ** Feil transaksjonstype (Skal være 7) " + cTTID.

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

    IF NOT AVAIL strekkode AND pcEAN_7 <> "" THEN DO:
        FIND FIRST strekkode WHERE strekkode.kode BEGINS pcEAN_7 NO-LOCK NO-ERROR.
        pcEAN = strekkode.kode.
    END.

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
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " ** Ukjent butikkNr: " + ENTRY(10,pcLinje,";") + ' ' + pcLinje.

        IF AVAILABLE tmpPDA THEN DELETE tmpPDA.
        NEXT LESERLINJER.
    END.
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = iButikkNr NO-ERROR.
    cVikt = "".
    IF pcEAN BEGINS "23" AND LENGTH(pcEAN) = 13 THEN DO:
        cVikt = SUBSTR(pcEan,9,4).
        IF cVikt <> "0000" THEN DO:
            dVikt = DECI(cVikt) / 1000 NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                dVikt = 0.
        END.
        ELSE
            cVikt = "".
    END.
    ASSIGN
        tmpPDA.Butnr        = iButikkNr
        tmpPDA.Ean          = DEC(pcEAN)
        tmpPDA.Dato         = pdDato
        tmpPDA.Tid          = piTid
        tmpPDA.Loggnr       = 0
        tmpPDA.Transtype    = 9 /* Varetelling */
        tmpPDA.Transtekst   = cTekst
        tmpPDA.Brukerid     = REPLACE(ENTRY(9,pcLinje,";"),' ','_')
        tmpPDA.Antall       = IF cVikt <> "" THEN dVikt ELSE DECIMAL(REPLACE(ENTRY(3,pcLinje,";"),".",","))
        tmpPDA.Kostpris     = 0
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
  
/*   IF CAN-FIND(FIRST tt_Error) THEN */
/*     RUN ErrorLogg.                 */

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
  Notes:  Eksporterer til varetran fil som så kan leses inn på vanlig måte.     
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iTelleNr   AS INTEGER NO-UNDO.
  DEFINE VARIABLE cEANNr     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE wVVareKost AS DECIMAL FORMAT "->>>>>>>>>9.99" NO-UNDO.
  DEFINE VARIABLE ocReturn   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE obOk       AS LOG NO-UNDO.
  OPPDATERING:
  FOR EACH tmpPDA WHERE tmpPDA.EAN > 0
      BREAK BY tmpPDA.ButNr
            BY tmpPDA.BrukerId
            BY tmpPDA.Dato:

      /* Oppretter lokasjonsliste */
      IF FIRST-OF(tmpPDA.Dato) THEN
      DO TRANSACTION:
        FIND LAST TelleHode NO-LOCK USE-INDEX TelleHode NO-ERROR.
        IF AVAILABLE TelleHode THEN 
          iTelleNr = TelleHode.TelleNr + 1.
        ELSE
          iTelleNr = 1.
        CREATE TelleHode.
        ASSIGN 
          TelleHode.TelleNr     = iTelleNr
          TelleHode.ButikkListe = STRING(tmpPDA.ButNr)
          TelleHode.TTId        = tmpPDA.Transtype 
          TelleHode.TBId        = 1
          TelleHode.Notat       = 'Automtisk innlest ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS") + '.'
          TelleHode.StartDato   = TODAY /*tmpPDA.Dato*/
          TelleHode.Beskrivelse = tmpPDA.Brukerid + ' ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS") + ' ' + VPIFilHode.FilNavn + '.'
          TelleHode.TelleType   = 2 /* Lokasjonsliste */
          TelleHode.LokasjonsId = ''
          TelleHode.BrukerIdPDA = tmpPDA.Brukerid
          TelleHode.FilDatoPDA  = VPIFilHode.Dato
          TelleHode.FilTidPDA   = 0
          .
        FIND CURRENT Tellehode NO-LOCK.
      END. /* TRANSACTION */

      cEANNr = STRING(tmpPDA.EAN).
      RUN bibl_chkean.p (INPUT-OUTPUT cEANNr).
      FIND Strekkode NO-LOCK WHERE
        Strekkode.Kode = cEANNr NO-ERROR.
      IF NOT AVAIL strekkode AND cEANNr BEGINS "7388" AND LENGTH(cEANNr) = 13 THEN DO:
          cEANNr = SUBSTR(cEANNr,1,12) + "0".
          FIND Strekkode NO-LOCK WHERE
            Strekkode.Kode = cEANNr NO-ERROR.
      END.
      IF AVAILABLE Strekkode THEN 
        FIND StrKonv OF Strekkode NO-LOCK NO-ERROR.
      ELSE DO:
        ASSIGN piAntFeil = piAntFeil + 1.
            CREATE tt_Error.
            ASSIGN tt_Error.LinjeNr = piAntFeil
                   tt_Error.Tekst   = "Artikel saknas " + STRING(tmpPDA.EAN).
      END.
      IF AVAILABLE StrKonv THEN 
      TELLELINJEN:
      DO TRANSACTION:  
        FIND ArtBas NO-LOCK WHERE 
          ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN DO:
            LEAVE TELLELINJEN.
        END.

        FIND Farg OF ArtBas NO-LOCK NO-ERROR.
              
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
            FIND Butiker NO-LOCK WHERE 
              Butiker.Butik = tmpPDA.ButNr NO-ERROR.
            FIND ArtPris NO-LOCK WHERE
                 ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                 ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
            IF AVAILABLE ArtPris THEN
                 wVVareKost = ArtPris.VareKost[1]. /* Tar alltid normalpris */
            ELSE DO:
                FIND FIRST artpris WHERE artpris.artikkelnr = ArtBas.ArtikkelNr AND
                                         ArtPris.ProfilNr   = 1 NO-LOCK NO-ERROR.
                IF AVAILABLE ArtPris THEN
                     wVVareKost = ArtPris.VareKost[1]. /* Tar alltid normalpris */
            END.
        END.

        /* Ukjent varekost */
        IF wVVareKost = ? THEN
            wVVareKost = 0.              
                  
        FIND ArtLag NO-LOCK WHERE
          ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND 
          ArtLag.Storl      = StrKonv.Storl AND 
          ArtLag.Butik      = tmpPDA.ButNr NO-ERROR.           
                  
        FIND FIRST TelleLinje EXCLUSIVE-LOCK WHERE 
             TelleLinje.TelleNr    = TelleHode.TelleNr AND        
             TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr AND
             TelleLinje.Butik      = tmpPDA.ButNr AND
             TelleLinje.Storl      = StrKonv.Storl NO-ERROR.

        IF NOT AVAILABLE TelleLinje THEN 
        DO:
            CREATE TelleLinje.

            /* Setter index. */
            ASSIGN
              TelleLinje.TelleNr    = TelleHode.TelleNr 
              TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr
              TelleLinje.Butik      = tmpPDA.ButNr
              TelleLinje.Storl      = StrKonv.Storl
              Tellelinje.Beskr      = ArtBas.Beskr
              TelleLinje.LevKod     = ArtBas.LevKod
              TelleLinje.Vg         = ArtBas.Vg
              TelleLinje.LopNr      = ArtBas.LopNr
              TelleLinje.LevFargKod = IF ArtBas.LevFargKod <> "" THEN ArtBas.LevFargKod ELSE IF AVAIL Farg THEN Farg.Farbeskr ELSE ""
              TelleLinje.AntallPar  = IF AVAILABLE ArtLag THEN ArtLag.Lagant ELSE 0
              TelleLinje.OpprVerdi  = TelleLinje.AntallPar * wVVareKost
              TelleLinje.VVareKost  = wVVAreKost
              TelleLinje.LevNr      = ArtBas.LevNr
              TelleLinje.Sasong     = ArtBas.SaSong
              TelleLinje.Farg       = ArtBas.Farg
              TelleLinje.MatKod     = ArtBas.MatKod
              TelleLinje.VgLopNr    = TRIM(STRING(ArtBas.Vg,">>>>>9")) + "/" + TRIM(STRING(ArtBas.LopNr,">>>>>9"))
              TelleLinje.Kode       = STRING(tmpPDA.Ean) /*Strekkode.Kode*/
              TelleLinje.NedSkrevet = wVVAreKost
              .
        END.

        ASSIGN
            TelleLinje.OpprAntalTalt = TelleLinje.OpprAntalTalt + tmpPDA.Antall
            TelleLinje.AntallTalt    = TelleLinje.AntallTalt    + tmpPDA.Antall 
            TelleLinje.OpptVerdi     = TelleLinje.AntallTalt * wVVarekost
            TelleLinje.AntallDiff    = TelleLinje.AntallPar - TelleLinje.AntallTalt
            TelleLinje.VerdiDiff     = TelleLinje.AntallDiff * TelleLinje.VVareKost
            .                          
        RELEASE TelleLinje. /* Denne MÅ få stå */            
      END. /* TELLELINJEN TRANSACTION */
      
      /* Beregner summer og lukker lokasjonsliste */
      IF LAST-OF(tmpPDA.Dato) THEN
      DO:
          RUN tellehode_oppdatersum.p (STRING(ROWID(TelleHode)),?,cTekst,OUTPUT ocReturn,OUTPUT obOk).
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

