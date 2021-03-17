&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xbxAJoverforinnles.p
    Purpose     :

    Syntax      :


    Author(s)   : Tom Nøkleby
    Created     : 
    Notes       : variant av xbxoverforinnles.p, där vi tar hänsyn till SKO
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
DEFINE VARIABLE cMailEnablat AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hdovbunt AS HANDLE      NO-UNDO.
DEF STREAM InnFil.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR.
DEFINE TEMP-TABLE TT_buntar
    FIELD iBuntnr AS INTE.

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
{syspar2.i 50 50 25 cMailEnablat}

RUN LesInnFil.
RUN OppdaterFil.
IF 1 = 1 /* om direktuppdatering */ AND CAN-FIND(FIRST tt_buntar) THEN DO:
    RUN dovbunt.w PERSISTENT SET hdovbunt NO-ERROR.
    IF VALID-HANDLE(hdovbunt) THEN DO:
        FOR EACH tt_buntar:
/*               DYNAMIC-FUNCTION('setQueryString':U IN hdovbunt,                                  */
/*                    INPUT "for each ovbunt where ovbunt.buntnr = " + STRING(tt_buntar.iBuntnr)). */
/*               DYNAMIC-FUNCTION('openQuery':U IN hdovbunt).                                      */
              RUN OppdaterTransLogg IN hdovbunt (tt_buntar.iBuntnr).
              FIND ovBunt WHERE ovBunt.buntnr = tt_buntar.iBuntnr EXCLUSIVE NO-WAIT NO-ERROR.
              IF AVAIL ovBunt THEN DO:
                  IF ovBunt.BatchNr > 0 THEN
                      ASSIGN OvBunt.DatoOppdatert = TODAY
                             OvBunt.TidOppdatert  = TIME.
                  FIND CURRENT ovbunt NO-LOCK.
                  RELEASE ovbunt.
              END.
        END.
        DELETE PROCEDURE hdovbunt.
    END.
END.

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

    IF cMailEnablat = "1" THEN DO:
        RUN sendmail_tsl.p ("OVERFORERROR","Överföringsfel PRS",cFilnavn,"","","") NO-ERROR.
    END.

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

Eks:  12 entries. Ingen transaksjonskode.    
737429079101;1;'2014-12-16 11:48:54.241';3;;System;;0;1;3;4;0
737429079101;1;'2014-12-16 11:48:55.190';3;;System;;0;1;3;4;0
737429079101;1;'2014-12-16 11:48:55.944';3;;System;;0;1;3;4;0
825840789768;1;'2014-12-16 11:48:59.261';3;;System;;0;1;3;4;0
4002092087514;1;'2014-12-16 11:49:04.656';3;;System;;0;1;3;4;0
4002092087514;1;'2014-12-16 11:49:05.235';3;;System;;0;1;3;4;0
188021/38;1;'2014-12-16 11:49:13.590';3;;System;;0;1;3;4;0
634246914687;1;'2014-12-16 11:49:18.542';3;;System;;0;1;3;4;0
+ 1 Produktno
+ 2 Antal
+ 3 Datum - tid
+ 4 till butik
 5
 6
 7
 8
+ 9 från butik
10 
+11 TTID
12

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
  DEF VAR cProduktNr AS CHAR NO-UNDO.
  
  DEFINE VARIABLE e1     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE e2     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cVg    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cLopNr AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cSize  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cNYScanned AS CHARACTER   NO-UNDO.
  
  DEFINE VARIABLE pcTid AS CHARACTER NO-UNDO.
  DEFINE VARIABLE piTid AS INTEGER   NO-UNDO. 
  DEF VAR pdDato     AS DATE NO-UNDO.
  DEF VAR pfEAN      AS DEC  NO-UNDO.
  DEFINE VARIABLE cMBut AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cInterleaved AS CHARACTER   NO-UNDO.
  
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

    ASSIGN /* Dato og tid fra fil: '2012-11-4 12:28:00.000' */
        cProduktNr = TRIM(ENTRY(1,pcLinje,";"))
        pcEAN   = TRIM(ENTRY(5,pcLinje,";"))
        cTekst  = RIGHT-TRIM(LEFT-TRIM(TRIM(ENTRY(3,pcLinje,";")),"'"),"'")
        pcDato  = ENTRY(1,cTekst,' ')
        pcTid   = ENTRY(2,cTekst,' ')
        pcTid   = ENTRY(1,pcTid,'.')
        cTTID   = TRIM(ENTRY(11,pcLinje,";"))
        cMBut   = TRIM(ENTRY(4,pcLinje,";"))
        .


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
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " ** Feil transaksjonstype (Skal være 4) " + cTTID.
        NEXT LESERLINJER.
    END.

    IF NUM-ENTRIES(cProduktNr,"/") = 2 THEN DO:
        e1 = ENTRY(1,cProduktNr,"/").
        e2 = ENTRY(2,cProduktNr,"/").
        cVg = SUBSTR(e1,1,LENGTH(e1) - 3).
        cVg = FILL("0",3 - LENGTH(cVg)) + cVg.
        cLopNr = SUBSTR(e1,LENGTH(e1) - 2).
        cLopNr = FILL("0",4 - LENGTH(cLopNr)) + cLopNr.
        cSize = IF NUM-ENTRIES(e2,".") = 2 THEN REPLACE(e2,".","") ELSE e2 + "0".
        cSize = FILL("0",4 - LENGTH(cSize)) + cSize.
        cNYScanned = cVg + cLopNr + "0" + cSize.
        cNYScanned = FILL("0",12 - LENGTH(cNYScanned)) + cNYScanned.
        cInterleaved = cNYScanned.
        FIND strekkode WHERE StrekKode.Bestillingsnummer = cInterleaved NO-LOCK NO-ERROR.
        IF NOT AVAIL strekkode THEN DO:
            ASSIGN
              piAntFeil = piAntFeil + 1.
            CREATE tt_Error.
            ASSIGN
              tt_Error.LinjeNr = piAntFeil
              tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " ** Okänd produkt " + cProduktNr.
            NEXT LESERLINJER.
        END.
        pcEAN = strekkode.kode.
    END.
    ELSE DO:
        IF LENGTH(cProduktNr) = 13 THEN
            FIND strekkode WHERE StrekKode.kode = cProduktNr NO-LOCK NO-ERROR.
        ELSE IF LENGTH(cProduktNr) = 12 THEN
            FIND strekkode WHERE StrekKode.Bestillingsnummer = cProduktNr NO-LOCK NO-ERROR.
        IF NOT AVAIL strekkode THEN
            FIND strekkode WHERE strekkode.kode = "0" + cProduktNr NO-LOCK NO-ERROR.
        IF NOT AVAIL strekkode THEN DO:
            ASSIGN
              piAntFeil = piAntFeil + 1.
            CREATE tt_Error.
            ASSIGN
              tt_Error.LinjeNr = piAntFeil
              tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " ** Okänd produkt " + cProduktNr.
            NEXT LESERLINJER.
        END.
        pcEAN = strekkode.kode.
    END.

    /* Legger på ledende nuller i EAN koden */
    IF LENGTH(pcEAN) > 6 AND length(pcEAN) < 13 THEN
        pcEAN = FILL("0",13 - LENGTH(pcEAN)) + pcEAN.
        .
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
    ASSIGN iButikkNr = INT(ENTRY(9,pcLinje,";")) NO-ERROR.
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
    CREATE tmpPDA.
    ASSIGN
        tmpPDA.Butnr        = iButikkNr
        tmpPDA.Ean          = DEC(pcEAN)
        tmpPDA.Dato         = pdDato
        tmpPDA.Tid          = piTid
        tmpPDA.Loggnr       = 0
        tmpPDA.Transtype    = 7 /* Lagerjustering */
        tmpPDA.Transtekst   = cTekst
        tmpPDA.Brukerid     = REPLACE(ENTRY(6,pcLinje,";"),' ','_')
        tmpPDA.Antall       = DECIMAL(REPLACE(ENTRY(2,pcLinje,";"),".",","))
        tmpPDA.Kostpris     = 0
        tmpPDA.Salgssum     = 0
        tmpPDA.Nylagant     = 0
        tmpPDA.Gmlagant     = 0
        tmpPDA.MButNr       = INTEGER(cMBut)
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
  DEFINE VARIABLE iTelleNr   AS INTEGER NO-UNDO.
  DEFINE VARIABLE cEANNr     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE wVVareKost AS DECIMAL FORMAT "->>>>>>>>>9.99" NO-UNDO.
  DEFINE VARIABLE ocReturn   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE obOk       AS LOG NO-UNDO.
  DEFINE VARIABLE iBuntNr    AS INTEGER NO-UNDO.
  DEFINE VARIABLE iLinjeNr   AS INTEGER NO-UNDO.
  
  OPPDATERING:
  FOR EACH tmpPDA WHERE tmpPDA.EAN > 0
      BREAK BY tmpPDA.ButNr
            BY tmpPDA.BrukerId
            BY tmpPDA.Dato:

      /* Oppretter lokasjonsliste */
      IF FIRST-OF(tmpPDA.Dato) THEN
      DO TRANSACTION:
        
/*        FIND LAST ovBunt NO-LOCK NO-ERROR.*/
        FIND LAST OvBunt NO-LOCK WHERE 
          Ovbunt.buntNr < 1000000000 
          USE-INDEX BuntNr NO-ERROR.        
        IF AVAILABLE ovBunt THEN
            iBuntNr = ovBunt.BuntNr + 1.
        ELSE
            iBuntNr = 1.
        IF iBuntNr > 9999999 THEN
            RETURN "AVBRYT".

        CREATE ovBunt.
        ASSIGN ovBunt.BuntNr         = iBuntNr
               ovBunt.Merknad        = 'Overføring BxMobile ' + tmpPDA.BrukerId + ' ' + STRING(tmpPDA.Dato) + ' ' + STRING(TIME,"HH:MM:SS")
               ovBunt.Opphav         = 1
               ovbunt.DatoOppdatert  = ?
               ovBunt.TidOppdatert   = 0
               ovBunt.OppdatertAv    = ''
               iLinjeNr              = 0.
        FIND CURRENT ovBunt NO-LOCK.
        CREATE TT_buntar.
        ASSIGN TT_buntar.iBuntnr = iBuntnr.
      END. /* TRANSACTION */

      cEANNr = STRING(tmpPDA.EAN).
      IF LENGTH(cEANNr) > 7 THEN
          cEANNr = FILL("0",13 - LENGTH(cEANNr)) + cEanNr.
/*       RUN bibl_chkean.p (INPUT-OUTPUT cEANNr). */
      FIND Strekkode NO-LOCK WHERE
        Strekkode.Kode = cEANNr NO-ERROR.
      IF AVAILABLE Strekkode THEN 
        FIND StrKonv OF Strekkode NO-LOCK NO-ERROR.
      IF AVAILABLE StrKonv THEN 
      OVLINJEN:
      DO TRANSACTION:  
        FIND ArtBas NO-LOCK WHERE 
          ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN LEAVE OVLINJEN.

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
        END.

        /* Ukjent varekost */
        IF wVVareKost = ? THEN
            wVVareKost = 0.              
                  
        FIND ArtLag NO-LOCK WHERE
          ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND 
          ArtLag.Storl      = StrKonv.Storl AND 
          ArtLag.Butik      = tmpPDA.ButNr NO-ERROR.           
                  
        ASSIGN iLinjeNr = iLinjeNr + 1.
  
        CREATE ovBuffer.
        ASSIGN OvBuffer.BuntNr      = iBuntNr     
               OvBuffer.LinjeNr     = iLinjeNr
               OvBuffer.ButikkNrFra = tmpPDA.Butnr
               OvBuffer.ButikkNrTil = tmpPDA.MButNr        
               OvBuffer.ArtikkelNr  = ArtBas.ArtikkelNr 
               OvBuffer.Vg          = ArtBas.Vg         
               OvBuffer.LopNr       = ArtBas.LopNr      
               OvBuffer.Antall      = tmpPDA.Antall     
               OvBuffer.Merknad     = ArtBas.Beskr    
               OvBuffer.Storl       = StrKonv.Storl
               OvBuffer.TilStorl    = StrKonv.Storl
               OvBuffer.Varekost    = wVVareKost.
        RELEASE ovBuffer. /* Denne MÅ få stå */            
      END. /* OVLINJEN TRANSACTION */
      
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

