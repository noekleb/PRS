&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xbxvmotRFIDinnles.p
    Purpose     :

    Syntax      :



    Author(s)   : Tom Nøkleby
    Created     : 
    Notes       : Import av varemottak/pakkseddel fra PDA.
                  Skaper RFID etikettfiler direkte.
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
DEF VAR cFilRFID     AS CHAR NO-UNDO.
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
DEFINE VARIABLE cEan AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPris AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRfidSeqNr          AS DECIMAL   FORMAT ">>>>>>>>>>>9". 
DEFINE VARIABLE cRfid1              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRfid2              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRfid3              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRfid4              AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSeqNr              AS DECIMAL   FORMAT ">>>>>>>9" NO-UNDO.
DEFINE VARIABLE cKatalog            AS CHARACTER NO-UNDO.
DEF VAR piAntFeil  AS INT  NO-UNDO.

DEFINE VARIABLE cRFIDHeader AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRFIDLinje AS CHARACTER NO-UNDO.

DEFINE STREAM InnFil.
DEFINE STREAM UtRFID.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR.

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

/* Tømmer feillogg. */
EMPTY TEMP-TABLE tt_Error.

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cErrFil       = 'log\' + "xbxvmotRFIDinnles.txt"
    cFilNavn      = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.
    
/* Filen finnes ikke */
IF SEARCH(cFilNavn) = ? THEN
  RETURN.

ASSIGN
    cKatalog    = 'filer'
    cRFIDHeader = 'Ean;Etitekst1;Enhetstekst;Utpris;Brukerid;StyleCode;ButNr;Antall;Antpkn;Emb;Vgr;Sortiment;Levnr;Bestnr;Enhetspris;Pristekst;Prisntekst;Levvnr;Veilpris;Varegrtekst;Fabrikatnavn;Fargetekst;Størrelsestekst;Levnavn;Modellnr2;Rfid1;Rfid2;Rfid3;Rfid4;Etitype' 
    cRFIDLinje  = '&Ean;&Etitekst1;&enhtekst;&utpris;&Brukerid;&StyleCode;&ButNr;1;&antpkn;;&hgr;&sortkode;&LevNr;;&enhpris;;;&levvnr;0;&hgrtekst;&fabrikatnavn;&Farge;&Str;&LevNavn;&ModellNr;&Rfid1;&Rfid2;&Rfid3;&Rfid4;&Etitype'     
    .

OS-CREATE-DIR VALUE(cKatalog) NO-ERROR.
RUN LesInnFil.

RUN eksporterFil.

IF CAN-FIND(FIRST tt_Error) THEN
    RUN ErrorLogg.

/* Stempler posten som innlest. */
DO TRANSACTION:
    FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
    ASSIGN
        VPIFilHode.VPIFilStatus = 5
        .
END.
IF AVAILABLE VPIFilHode THEN
    FIND CURRENT VPIFilHode    NO-LOCK.

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

lRfidSeqNr = 0.
IF lSeqNr > 99999999 THEN 
    lSeqNr = 0.
    
FOR EACH tmpPda 
  BREAK BY tmpPda.butNr
        BY tmpPDA.LinjeNr:
            
  ASSIGN 
    cLinje = cRFIDLinje
    .

  IF FIRST-OF(tmpPda.butNr) THEN 
  DO:
    OS-CREATE-DIR VALUE(cKatalog + '\' + STRING(tmpPda.butNr)) NO-ERROR.
          
    ASSIGN 
        lSeqNr        = lSeqNr + 1
        cKatalogPkSdl = VPIFilHode.Katalog
        cTekst        = STRING(NOW).
        cTekst        = ENTRY(2,cTekst,' ').
        cTekst        = REPLACE(cTekst,':','').
        cTekst        = REPLACE(cTekst,',','').
        cTekst        = REPLACE(cTekst,'+','').        
        cFilRFID      = cKatalog + '\' + STRING(tmpPda.butNr) + '\' + 'ETI_' + STRING(tmpPda.butNr) + '_'  + STRING(lSeqNr) + '_' + tmpPda.Bruker  + '_' + cTekst + '.csv'.
        cFil2         = cFilRFID
        .
        
    OUTPUT STREAM UtRFID TO VALUE(cFilRFID) NO-ECHO.
    PUT STREAM UtRFID UNFORMATTED
        cRFIDHeader
        SKIP.
  END.  
  
  IF AVAILABLE StrekKode THEN RELEASE StrekKode.
  IF AVAILABLE ArtBas THEN RELEASE ArtBas.
  
  ASSIGN 
    cEan = STRING(tmpPDA.Ean).
  /* Legger på ledende nuller i EAN koden */
  IF LENGTH(cEAN) > 6 AND LENGTH(cEAN) < 13 THEN
      cEAN = FILL("0",13 - LENGTH(cEAN)) + cEAN.
      .
  
  FIND StrekKode NO-LOCK WHERE 
    StrekKode.Kode = cEan NO-ERROR.
  IF AVAILABLE StrekKode THEN 
    FIND ArtBas OF StrekKode NO-LOCK NO-ERROR.  
  FIND Butiker NO-LOCK WHERE 
    Butiker.Butik = tmpPDA.Butnr NO-ERROR. 
  FIND ArtPris OF ArtBas NO-LOCK WHERE 
    ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN 
    FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.  
  FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
  FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
  FIND Farg OF ArtBas NO-LOCK NO-ERROR.
  FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
  FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
  
  BLOKK: 
  DO:
      ASSIGN
          lRfidSeqNr = lRfidSeqNr + 1
          cRfid1     = '1'
          cRfid2     = SUBSTRING(cEan,1,7)
          cRfid3     = '0' + SUBSTRING(cEan,8,5)
          cRfid4     = TRIM(STRING(lRfidSeqNr,">>>>>>>>>>>9"))
          .    

      ASSIGN
        cPris  = REPLACE(STRING(IF AVAILABLE ArtPris THEN ArtPris.Pris[1] ELSE 0),',','.')
        cLinje = REPLACE(cLinje,'&Ean',cEan) 
        cLinje = REPLACE(cLinje,'&Etitekst1',TRIM(SUBSTRING(IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE tmpPda.Transtekst,1,17)))
        cLinje = REPLACE(cLinje,'&enhtekst',IF AVAILABLE ArtBas THEN ArtBas.Salgsenhet ELSE 'Stk')
        cLinje = REPLACE(cLinje,'&utpris',cPris)
        cLinje = REPLACE(cLinje,'&Brukerid',tmpPDA.Brukerid)
        cLinje = REPLACE(cLinje,'&StyleCode','1')
        cLinje = REPLACE(cLinje,'&antpkn','1')        
        cLinje = REPLACE(cLinje,'&ButNr',STRING(tmpPDA.ButNr)) 
        cLinje = REPLACE(cLinje,'&StyleCode','0')
        cLinje = REPLACE(cLinje,'&hgr',STRING(IF AVAILABLE ArtBas THEN ArtBas.Vg ELSE 0))
        cLinje = REPLACE(cLinje,'&sortkode','')
        cLinje = REPLACE(cLinje,'&LevNr',STRING(IF AVAILABLE ArtBas THEN ArtBas.LevNr ELSE 0))
        cLinje = REPLACE(cLinje,'&enhpris',cPris)
        cLinje = REPLACE(cLinje,'&levvnr',IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE '')
        cLinje = REPLACE(cLinje,'&hgrtekst',IF AVAILABLE VarGr THEN VarGr.VgBeskr ELSE '')
        cLinje = REPLACE(cLinje,'&fabrikatnavn',IF AVAILABLE Varemerke THEN Varemerke.Beskrivelse ELSE '')
        cLinje = REPLACE(cLinje,'&Farge',IF AVAILABLE Farg THEN Farg.FarBeskr ELSE '')
        cLinje = REPLACE(cLinje,'&Str',IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')
        cLinje = REPLACE(cLinje,'&LevNavn',IF AVAILABLE LevBas THEN LevBas.levnamn ELSE '')
        cLinje = REPLACE(cLinje,'&ModellNr',IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE '')
        cLinje = REPLACE(cLinje,'&Rfid1',cRfid1)
        cLinje = REPLACE(cLinje,'&Rfid2',cRfid2)
        cLinje = REPLACE(cLinje,'&Rfid3',cRfid3)
        cLinje = REPLACE(cLinje,'&Rfid4',cRfid4)
        cLinje = REPLACE(cLinje,'&Etitype','1')        
        NO-ERROR.  

      IF ERROR-STATUS:ERROR THEN
      DO:
          ASSIGN
            piAntFeil = piAntFeil + 1.
          CREATE tt_Error.
          ASSIGN
            tt_Error.LinjeNr = piAntFeil
            tt_Error.Tekst   = "** Feil ved put" + STRING(iAntLinjer) + " " + cLinje.
      END.
      ELSE    
          PUT STREAM UtRFID UNFORMATTED
              cLinje 
              SKIP. 
      
  END. /* BLOKK */
        
  IF LAST-OF(tmpPda.butNr) THEN 
  DO:
    OUTPUT STREAM UtRFID CLOSE.
    IF SEARCH(cFilRFID) <> ? THEN 
    DO:
        OS-RENAME VALUE(cFilRFID) VALUE(SUBSTRING(cFilRFID,2)).
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
      cFilnavn = 'log' + '\' 
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
  
  /* PDA fil */
  EMPTY TEMP-TABLE tmpPDA.
  
  RUN TellOppLinjer.

  ASSIGN
      piLinjeNr  = 1.
      iAntLinjer = 0
      .
      
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    ASSIGN
      pcLinje    = ''
      iAntLinjer = iAntLinjer + 1.
    IF iAntLinjer > iTotantLinjer THEN 
    DO:
        iAntLinjer = iAntLinjer - 1.
        LEAVE LESERLINJER.
    END.
        
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
        NEXT LESERLINJER.
    END.
    
    IF pcLinje BEGINS ';;;;;;' OR TRIM(pcLinje) = '' THEN 
    DO:
        NEXT LESERLINJER.
    END.

    ASSIGN
        pfEAN = DEC(ENTRY(5,pcLinje,";"))
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        NEXT LESERLINJER.
    END.
    
    IF TRIM(ENTRY(5,pcLinje,";")) = '' THEN
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Blank strekkode på linje " + STRING(iAntLinjer) + '. ' + pcLinje.
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
    ELSE DO:
        cTekst = "** Ukjent EAN".
        
        ASSIGN
          piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Ukjent EAN " + STRING(iAntLinjer) + ' Record: ' + pcLinje.
    END.
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
        NEXT LESERLINJER.
    END.
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = iButikkNr NO-ERROR.

    CREATE tmpPDA.

    ASSIGN
        tmpPDA.Butnr        = iButikkNr
        tmpPDA.Ean          = DEC(pcEAN)
        tmpPDA.Dato         = pdDato
        tmpPDA.Tid          = piTid
        tmpPDA.Loggnr       = 0
        tmpPDA.Transtype    = 5 /* Varemottak */
        tmpPDA.Transtekst   = cTekst
        tmpPDA.Brukerid     = REPLACE(ENTRY(6,pcLinje,";"),' ','_')
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

  IF SEARCH(cFilNavn) <> ? THEN 
  DO:
    cBackup = REPLACE(cFilNavn,VPIFilHode.FilNavn,'bku\' + VPIFilHode.FilNavn). 
    OS-COPY VALUE(cFilNavn) VALUE(cBackup).

    IF SEARCH(cBackup) <> ? THEN DO:
      OS-COMMAND SILENT VALUE('del ' + cFilNavn).
    END.
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

