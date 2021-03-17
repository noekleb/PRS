&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xedivpiinnles.pp
    Purpose     :

    Syntax      :

    Description : Leser inn EDI vpi filer og omformer den til en pricat fil.

    Author(s)   : Tom Nøkleby
    Created     : 23/11-07
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
DEF VAR cVPIUtFil     AS CHAR NO-UNDO.
DEF VAR cVreFil       AS CHAR NO-UNDO.
DEF VAR iLevNr        AS INT  NO-UNDO.
DEF VAR cLevNavn      AS CHAR NO-UNDO.
DEF VAR cValKod       AS CHAR NO-UNDO.
DEF VAR ctmpKatalog   AS CHAR NO-UNDO.
DEF VAR ipksdlnr      AS INT  NO-UNDO.
DEF VAR pcLinje       AS CHAR NO-UNDO.
DEF VAR piLinjeNr     AS INT  NO-UNDO.
DEF VAR pcBkuFil      AS CHAR NO-UNDO.
DEFINE VARIABLE bLager   AS LOG NO-UNDO.
DEFINE VARIABLE iEtikett AS INTEGER NO-UNDO.
DEF VAR hPgmHandle    AS HANDLE NO-UNDO.
DEF VAR h_dvpifilhode AS HANDLE NO-UNDO.
DEF VAR h_dvpiartbas  AS HANDLE NO-UNDO.
DEF VAR h_PrisKo      AS HANDLE NO-UNDO.
DEFINE VARIABLE iEkstVPILevNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iNyLevNr      AS INTEGER NO-UNDO.
DEFINE VARIABLE iLoop         AS INTEGER NO-UNDO.
DEFINE VARIABLE iHKVPILomme AS INTEGER NO-UNDO.
DEFINE VARIABLE bErrFil AS LOG NO-UNDO.
DEFINE VARIABLE cErrorFil AS CHAR NO-UNDO.
DEFINE VARIABLE cFilPrefix AS CHARACTER NO-UNDO.

DEF VAR h_vartkor     AS HANDLE NO-UNDO.
DEF VAR hField1       AS HANDLE NO-UNDO.
DEF VAR c02SjekkListe AS CHAR NO-UNDO.
DEF VAR cGenEan       AS CHAR NO-UNDO.
DEF VAR cEnhet        AS CHAR NO-UNDO.
DEFINE VARIABLE cTekst                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDec                    AS DECIMAL NO-UNDO.
DEFINE VARIABLE bGenEAN                 AS LOG NO-UNDO.
DEFINE VARIABLE bGenMapping             AS LOG NO-UNDO.
DEFINE VARIABLE bTillatBlankEAN         AS LOG NO-UNDO.
DEFINE VARIABLE bAvbrytVedFeil          AS LOG NO-UNDO.
DEFINE VARIABLE bGenSalgsenhet          AS LOG NO-UNDO.
DEFINE VARIABLE bGenButikk              AS LOG NO-UNDO.
DEFINE VARIABLE bHK                     AS LOG NO-UNDO.
DEFINE VARIABLE iCL                     AS INTEGER NO-UNDO.
DEFINE VARIABLE bSjekkBestillingsnr     AS LOG NO-UNDO.
DEFINE VARIABLE bDistribIPS             AS LOG NO-UNDO.
DEFINE VARIABLE cFilial                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cbutKatalog             AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk                    AS LOG NO-UNDO.
DEFINE VARIABLE bBeholdLokArtInfo       AS LOG NO-UNDO.
DEFINE VARIABLE iOverstyrLevNr          AS INTEGER NO-UNDO.
DEFINE VARIABLE cEkstVPILevLst          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLoggKatalog AS CHARACTER NO-UNDO.

DEF VAR cEDB-System   LIKE ImpKonv.EDB-System NO-UNDO.
DEF VAR cImpTabell    LIKE ImpKonv.Tabell     NO-UNDO.

DEFINE VARIABLE cOKVersion AS CHARACTER INIT "00Artikel" NO-UNDO.
DEF VAR cRigalversion AS CHAR NO-UNDO.

DEF STREAM InnFil.
DEF STREAM UtVPI.
DEF STREAM UtVre.

DEFINE TEMP-TABLE tmpALLA
  FIELD Informationsmangd AS CHARACTER FORMAT "x(20)"
  FIELD Filler1 AS CHARACTER FORMAT "x(4)"
  FIELD Kundnr AS CHARACTER FORMAT "x(6)" 
  FIELD Filler2 AS CHARACTER FORMAT "x(6)" 
  FIELD Datum AS CHARACTER FORMAT "x(8)" 
  FIELD Transaktionsvecka AS CHARACTER FORMAT "x(6)" 
  FIELD Filler3 AS CHARACTER FORMAT "x(4)" 
  FIELD Timestamp AS CHARACTER FORMAT "x(14)" 
  FIELD KundEAN AS CHARACTER FORMAT "x(13)" 
  INDEX Informationsmangd Informationsmangd
  .  

DEFINE TEMP-TABLE tmpARTIKEL
  FIELD Produktid  AS CHARACTER FORMAT "x(13)"
  FIELD Forpackning_id  AS CHARACTER FORMAT "x(2)"
  FIELD Artikelnr  AS CHARACTER FORMAT "x(13)"
  FIELD Benamning  AS CHARACTER FORMAT "x(30)"    
  FIELD Forp_enh  AS CHARACTER FORMAT "x(3)"
  FIELD Forp_stl  AS CHARACTER FORMAT "x(4)"
  FIELD Jmfprisfakt  AS CHARACTER FORMAT "x(10)"
  FIELD Pantstatus  AS CHARACTER FORMAT "x(1)"    
  FIELD Eannr  AS CHARACTER FORMAT "x(13)"
  FIELD Nyttigkod  AS CHARACTER FORMAT "x(1)"
  FIELD Ean_namn  AS CHARACTER FORMAT "x(22)"
  FIELD Ean_viktvol  AS CHARACTER FORMAT "x(8)"
  FIELD Ean_leverantor  AS CHARACTER FORMAT "x(14)"
  FIELD Fiberrik  AS CHARACTER FORMAT "x(1)"
  FIELD Fettsnal  AS CHARACTER FORMAT "x(1)"
  FIELD Oblekt  AS CHARACTER FORMAT "x(1)"
  FIELD Ean_textnr  AS CHARACTER FORMAT "x(3)"
  FIELD Ean_kvittotext  AS CHARACTER FORMAT "x(12)"
  FIELD Butiksvarugrupp  AS CHARACTER FORMAT "x(4)"
  FIELD Leverantorsid  AS CHARACTER FORMAT "x(7)"
  FIELD SMG_funktionstext  AS CHARACTER FORMAT "x(10)"
  FIELD Pantkod  AS CHARACTER FORMAT "x(2)"
  FIELD Pallstorlek  AS CHARACTER FORMAT "x(4)"
  FIELD Direktvara  AS CHARACTER FORMAT "x(1)"
  FIELD Lev_artikelnr  AS CHARACTER FORMAT "x(15)"
  FIELD Orskakskod_ny_forp  AS CHARACTER FORMAT "x(2)"
  FIELD Utbytt_tidigarenr  AS CHARACTER FORMAT "x(13)"
  FIELD Produktklassid  AS CHARACTER FORMAT "x(4)"
  FIELD Filler1  AS CHARACTER FORMAT "x(3)"
  FIELD Anskaffningsvara  AS CHARACTER FORMAT "x(1)"
  FIELD Transitvara  AS CHARACTER FORMAT "x(1)"
  FIELD Jamforprisflagga  AS CHARACTER FORMAT "x(1)"
  FIELD Jamforprisenhet  AS CHARACTER FORMAT "x(8)"
  FIELD EAN_alt_kvittotext AS CHARACTER FORMAT "x(20)"
  FIELD Filler2 AS CHARACTER FORMAT "x(2)"
  FIELD Moms AS CHARACTER FORMAT "x(8)".
  
DEFINE TEMP-TABLE tmpPKPRIS
  FIELD Produktid  AS CHARACTER FORMAT "x(13)"
  FIELD Forpacknings_id  AS CHARACTER FORMAT "x(2)"
  FIELD Artikelnr  AS CHARACTER FORMAT "x(17)"
  FIELD Pkpris  AS CHARACTER FORMAT "x(12)"
  FIELD Datum_kommande  AS CHARACTER FORMAT "x(8)"
  FIELD Pkpris_kommande  AS CHARACTER FORMAT "x(12)"
  FIELD Filler1  AS CHARACTER FORMAT "x(4)"
  INDEX Produktid Produktid.

DEFINE TEMP-TABLE tmpCPKPRIS
  FIELD Produktid  AS CHARACTER FORMAT "x(13)"
  FIELD Forpacknings_id  AS CHARACTER FORMAT "x(2)"
  FIELD Artikelnr  AS CHARACTER FORMAT "x(17)"
  FIELD CPkpris  AS CHARACTER FORMAT "x(12)"
  FIELD Datum_kommande  AS CHARACTER FORMAT "x(8)"
  FIELD CPkpris_kommande  AS CHARACTER FORMAT "x(12)"
  FIELD Filler1  AS CHARACTER FORMAT "x(4)"
  INDEX Produktid Produktid.
  
DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr   AS INT
  FIELD Tekst     AS CHAR
  FIELD Gradering AS INT 
  FIELD ErrNr     AS INTEGER
  FIELD ButikkNr  AS INTEGER
  INDEX Feil ErrNr
  INDEX Linje LinjeNr.
   
DEFINE TEMP-TABLE tmpVare
    FIELD VareNr AS CHAR.
DEFINE TEMP-TABLE tmpEDI
    FIELD ButikkNr AS INTEGER FORMAT '->>>>>9'
    FIELD cLinje   AS CHARACTER 
    INDEX Butikk ButikkNr
    .
    
DEFINE BUFFER hkArtPris FOR ArtPris.
DEFINE BUFFER clButiker FOR Butiker.

{ttpricat.i &NEW=" " &SHARED=" "}
DEF BUFFER bttPrikat FOR ttPrikat.

{windows.i}
/*{RigalVare.i}*/
 
/* Eksportkatalog for IPS filer. */
{syspara.i 50 15 19 cbutKatalog} 
IF cbutKatalog = '' THEN cbutKatalog = 'c:\home\lindbak\sendes'. 
 
{syspara.i 50 15 18 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bDistribIPS = TRUE.
 
{syspara.i 50 15 11 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bBeholdLokArtInfo = TRUE.

{syspara.i 50 15 2 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bAvbrytVedFeil = TRUE.

{syspara.i 50 15 3 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bTillatBlankEAN = TRUE.

{syspara.i 50 15 4 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bGenEAN = TRUE.

{syspara.i 50 15 5 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bGenMapping = TRUE.

{syspara.i 50 15 7 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bGenSalgsenhet = TRUE.

{syspara.i 50 15 9 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bGenButikk = TRUE.

{syspara.i 1 1 18 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bHK = TRUE.

/* Setter HK's VPI lomme */
{syspara.i 50 18 1 iHKVPILomme INT}
IF iHKVPILomme = 0 THEN iHKVPILomme = 907.

/* Setting av lagerstyring. */
DO:
  cTekst = ''.
  {syspara.i 2 4 12 cTekst}
  IF cTekst = '' THEN bLager = TRUE.
  ELSE IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
    bLager = TRUE.
  ELSE 
    bLager = FALSE.
END.

{syspara.i 2 4 13 iEtikett INT}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-FixChk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FixChk Procedure 
FUNCTION FixChk RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VareIdent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD VareIdent Procedure
FUNCTION VareIdent RETURNS CHARACTER 
    (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEAN Procedure
FUNCTION getEAN RETURNS CHARACTER 
	(  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

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

{syspara.i 102 1 1 cFilPrefix}
IF cFilPrefix = '' THEN cFilPrefix = "VPILog_".
 
{syspara.i 1 1 59 cLoggKatalog}
IF cLoggKatalog <> '' THEN DO:
    /* Sikrer at katalog finnes. */
    OS-CREATE-DIR VALUE(RIGHT-TRIM(cLoggKatalog,'\')).    
    cLoggKatalog = RIGHT-TRIM(cLoggKatalog,'\') + '\'.
END.

ASSIGN
    cFilNavn  = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn
    cErrorFil = cLoggKatalog /*OS-GETENV('TMP') + '\'*/ 
               + cFilPrefix 
               + REPLACE(STRING(TODAY),'/','') + '_' 
               + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '_' 
               + VPIFilHode.FilNavn
               + ".Txt".

/* Sjekk bestillingsnr før strekkode. */
{syspara.i 50 15 12 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bSjekkBestillingsnr = TRUE.
ELSE
  bSjekkBestillingsnr = FALSE.

{syspara.i 2 4 8 cGenEan}
{syspara.i 2 4 10 cEnhet}
{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE clButiker THEN 
DO:
  MESSAGE 'Ukjent butiknr satt for sentrallager.'
  VIEW-AS ALERT-BOX.
  RETURN.
END.

ASSIGN
    ctmpKatalog = SESSION:TEMP-DIRECTORY
    cVPIFil     = "GVPI" + STRING(VPIFilHode.EkstVPILevNr) + "-" + STRING(TODAY,"99-99-9999") + "-" + string(TIME,"HH:MM:SS") + ".csv"
    cVPIFil     = REPLACE(cVPIFil,":","-")
    cVPIUtFil   = cVPIFil
    .

/* Henter ID for konvertering */
FIND EkstVPILev NO-LOCK WHERE
    EkstVPILev.EkstVPILevNr = VPIFilHode.EkstVPILevNr NO-ERROR.
IF NOT AVAILABLE EkstVPILev THEN
DO:
    MESSAGE "Ingen ekstern VPI leverandør tilgjengelig." SKIP
            "Id: " + STRING(VPIFilHode.EkstVPILevNr) + "."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.
IF EkstVPILev.KortNavn = '' THEN
DO:
    MESSAGE "Det er ikke satt opp KortNavn på ekstern VPI leverandør." SKIP
            "Dette må satt for at konvertering av varegrupper skal kunne skje." SKIP
            "Id: " + STRING(VPIFilHode.EkstVPILevNr) + "."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

/* Skal leverandørnr overstyres fra oppsett på VPILev? */
{syspara.i 50 15 20 iOverstyrLevNr INT}
ASSIGN
    cEDB-System   = EkstVPILev.EDB-System
    iLevNr        = IF iOverstyrLevNr = 1 THEN EkstVPILev.LevNr ELSE 0
    iNyLevNr      = EkstVPILev.LevNr
    iEkstVPILevNr = EkstVPILev.EkstVPILevNr.

/* Leser inn filen i ttPrikat. */
RUN LesInnARTIKELFil.
IF RETURN-VALUE <> 'AVBRYT' THEN 
DO:
    /* Leser inn HK priser */
    RUN LesInnPKPRISFil. 
    /* Leser inn Butikk priser */
    RUN LesInnCPKPRISFil. 

    RUN EksportImportLogg.
    
    /* Eksporterer til VPI fil hvis det ikke er funnet feil med gradering under 50. */
    /*
    IF NOT CAN-FIND(FIRST tt_Error WHERE
                    tt_Error.Gradering < 50) OR 
       bAvbrytVedFeil = FALSE THEN
        RUN EksportVPIFil.*/
    IF CAN-FIND(FIRST tt_Error) THEN
      RUN ErrorLogg.

    RUN EksportVPIFil.
    
END.
ELSE bErrFil = TRUE.
    
/* Stempler posten som innlest. */
DO TRANSACTION:
    FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
    /*
    IF CAN-FIND(FIRST tt_Error WHERE
            tt_Error.Gradering < 50) THEN
        ASSIGN
            VPIFilHode.VPIFilStatus = 9.
    ELSE
    */
    ASSIGN
        VPIFilHode.VPIFilStatus = (IF bErrFil THEN 9 ELSE 5).
    FIND CURRENT VPIFilHode NO-LOCK.    
END.
IF AVAILABLE VPIFilHode THEN
    FIND CURRENT VPIFilHode NO-LOCK.

/* Markerer datasettet med innlest fil. */
DO TRANSACTION:
  /* Markerer på hovedlev. */
  FIND VPIDatasett EXCLUSIVE-LOCK WHERE VPIDatasett.EkstVPILevNr = VPIFilHode.EkstVPILevNr NO-ERROR.
  IF AVAILABLE VPIDataSett THEN 
  DO:
    ASSIGN
      VPIDatasett.Beskrivelse = (IF NOT bErrFil THEN 'OK' ELSE 'FEIL') + 
                                ': innlest fil' + cFilNavn.
      FIND CURRENT VPIDatasett NO-LOCK.
      RELEASE VPIDatasett.
  END.
  
  /* Markerer filealenes lev */
  IF TRIM(cEkstVPILevLst) <> '' THEN 
  DO iLoop = 1 TO NUM-ENTRIES(cEkstVPILevLst):
    FIND VPIDatasett EXCLUSIVE-LOCK WHERE VPIDatasett.EkstVPILevNr = 1000000 + INT(ENTRY(iLoop,cEkstVPILevLst)) NO-ERROR.
    IF AVAILABLE VPIDataSett THEN 
    DO:
      ASSIGN
        VPIDatasett.Beskrivelse = (IF NOT bErrFil THEN 'OK' ELSE 'FEIL') + 
                                ': innlest fil' + cFilNavn.
        FIND CURRENT VPIDatasett NO-LOCK.
        RELEASE VPIDatasett.
    END.
  END.
END.

/* Er ikke filen innlest, skal den ligge igjen urørt. */
IF NOT bErrFil THEN 
DO: 
    IF bAvbrytVedFeil = FALSE THEN
      RUN flyttFil. 
    ELSE IF bAvbrytVedFeil AND NOT CAN-FIND(FIRST tt_Error) THEN
      RUN flyttFil.
END. 

/* Stopper innlesningsprogram for håndterminalfil. */
IF VALID-HANDLE(hPgmHandle) THEN
    DELETE PROCEDURE hPgmHandle.
IF VALID-HANDLE(h_dvpifilhode) THEN
    DELETE PROCEDURE h_dvpifilhode.
IF VALID-HANDLE(h_dvpiartbas) THEN
    DELETE PROCEDURE h_dvpiartbas.
IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_PrisKo.

/* Distribuerer filer til butikkene. */
IF bDistribIPS AND NOT bErrFil THEN
EKSPORT_BUTIKK:
DO:
  IF NOT CAN-FIND(FIRST tmpEDI) THEN 
    LEAVE EKSPORT_BUTIKK.
  ELSE RUN eksportButikk.  
END. /* EKSPORT_BUTIKK */

IF bErrFil THEN 
  RETURN 'AVBRYT'.
ELSE 
  RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

 
&IF DEFINED(EXCLUDE-eksportButikk) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE eksportButikk Procedure
PROCEDURE eksportButikk:
	/*------------------------------------------------------------------------------
			Purpose:  Eksport av rigalfil pr. butikk.																	  
			Notes:    Ved innlesning er de linjer som har butikknr. logget.
			          Her legges det ut en fil som inneholder de linjer som 
			          gjelder butikken, pr. butikk som er angitt i filen.
			          
			          TN 23/11-10 Dette skal gjøres om til et XML utlegg.
			          Anna vil ha det.
			          																  
	------------------------------------------------------------------------------*/
  DEFINE VARIABLE cbutFilNavn AS CHARACTER NO-UNDO.
  /*
  cButFilNavn = 'V' + REPLACE(REPLACE(STRING(TODAY),'/',''),'-','') + REPLACE(REPLACE(STRING(TIME,"HH:MM:SS"),':',''),'-','') + '.'.
  
  BUTIKK:
  FOR EACH tmpEDI
    BREAK BY tmpEDI.ButikkNr:
    
    IF FIRST-OF(tmpEDI.ButikkNr) THEN
    DO:     
MESSAGE ctmpKatalog + '~\' + cButFilNavn + STRING(tmpEDI.ButikkNr,">>>999")
VIEW-AS ALERT-BOX.    
      OUTPUT STREAM UtVPI TO VALUE(ctmpKatalog + '~\' + cButFilNavn + STRING(tmpEDI.ButikkNr,">>>999")).
      PUT STREAM UtVPI UNFORMATTED 'RIGAL02,8.0' SKIP.
    END.
    
    PUT STREAM UtVPI UNFORMATTED tmpEDI.cLinje SKIP.
    
    IF LAST-OF(tmpEDI.ButikkNr) THEN
    DO: 
      OUTPUT STREAM UtVPI CLOSE.
      /* Flytter filen til ankommet katalogen */
      OS-COPY VALUE(ctmpKatalog + "~\" + cbutFilNavn + STRING(tmpEDI.ButikkNr,">>>999"))
         value(cbutKatalog + "~\" + cbutFilNavn + STRING(tmpEDI.ButikkNr,">>>999")).
      /* Renser bort temp fil */
      IF SEARCH(cbutKatalog + "~\" + cbutFilNavn + STRING(tmpEDI.ButikkNr,">>>999")) <> ? THEN
        OS-DELETE VALUE(ctmpKatalog + "~\" + cbutFilNavn + STRING(tmpEDI.ButikkNr,">>>999")).      
    END.
  END. /* BUTIKK */
  */
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



 
&IF DEFINED(EXCLUDE-EksportImportLogg) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportImportLogg Procedure
PROCEDURE EksportImportLogg:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
DEFINE VARIABLE cStreng AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAnt    AS INTEGER   NO-UNDO.
    
    IF SEARCH('GVPINy.csv') <> ? THEN 
    DO:
        RUN bibl_logg.p ('VPIEDIImport', 'xedivpiinnles.p - START Import av GVPINy').
        INPUT FROM VALUE('GVPINy.csv') NO-ECHO.
        REPEAT:
              IMPORT UNFORMATTED cStreng.    
              cTekst = TRIM(ENTRY(4,cStreng,';')).
              iAnt = iant + 1.
              FIND FIRST ttPriKat WHERE 
                  ttPriKat.EANNr = cTekst NO-ERROR.
              IF AVAILABLE ttPriKat THEN 
                  ASSIGN
                      ttPriKat.MarkedsPris    = TRIM(ENTRY(32,cStreng,';'))
                      ttPriKat.LevPrisEngros  = TRIM(ENTRY(13,cStreng,';'))
                      ttPriKat.GrunnSortiment = TRIM(ENTRY(98,cStreng,';'))
                      .
        END. 
        INPUT CLOSE.
        RUN bibl_logg.p ('VPIEDIImport', 'xedivpiinnles.p - SLUTT Import av GVPINy ' + STRING(iAnt)).
    END.
    
    RUN bibl_logg.p ('VPIEDIImport', 'xedivpiinnles.p - START Importkontroll logg.').

    RUN bibl_logg.p ('VPIEDIImport', 'xedivpiinnles.p - Header: '
       + ';' + 'tmpALLA.Kundnr' 
       + ';' + 'tmpARTIKEL.Leverantorsid'
       + ';' + 'tmpARTIKEL.ProduktId'
       + ';' + 'tmpARTIKEL.Artikelnr'
       + ';' + 'tmpARTIKEL.Eannr'
       + ';' + 'tmpARTIKEL.Ean_namn'
       + ';' + 'tmpARTIKEL.Forp_enh'
       + ';' + 'tmpARTIKEL.Forp_stl' 
       + ';' + 'tmpARTIKEL.Produktklassid' 
       + ';' + 'tmpARTIKEL.Lev_artikelnr'
       + ';' + 'tmpARTIKEL.Forp_enh'
       + ';' + 'tmpARTIKEL.Ean_namn'
       + ';' + 'tmpARTIKEL.Moms'
       + ';' + 'tmpARTIKEL.Pantkod'
       + ';' + 'tmpARTIKEL.Jmfprisfakt'
       + ';' + 'tmpARTIKEL.Jamforprisenhet'
       + ';' + 'tmpARTIKEL.SMG_funktionstext'
       + ';' + 'tmpARTIKEL.ttPriKat.GrunnSortiment'
       + ';' + 'tmpCPKPRIS.cpkpris'
       + ';' + 'ttPriKat.MarkedsPris'
       + ';' + 'tmpPKPRIS.pkpris'
       + ';' + 'ttPriKat.LevPrisEngros'
                   ).

    FOR EACH tmpARTIKEL:
        FIND FIRST tmpCPKPRIS WHERE
            tmpCPKPRIS.Produktid = tmpARTIKEL.Produktid NO-ERROR.
        FIND FIRST tmpPKPRIS WHERE
            tmpPKPRIS.Produktid = tmpARTIKEL.Produktid NO-ERROR.
        FIND FIRST ttPriKat WHERE
            ttPriKat.Eannr = tmpARTIKEL.Eannr NO-ERROR.

        RUN bibl_logg.p ('VPIEDIImport', 'xedivpiinnles.p - Linje: '
        + ';' + tmpALLA.Kundnr 
        + ';' + tmpARTIKEL.Leverantorsid
        + ';' + tmpARTIKEL.Produktid
        + ';' + tmpARTIKEL.Artikelnr
        + ';' + tmpARTIKEL.Eannr
        + ';' + tmpARTIKEL.Ean_namn
        + ';' + tmpARTIKEL.Forp_enh
        + ';' + tmpARTIKEL.Forp_stl 
        + ';' + tmpARTIKEL.Produktklassid 
        + ';' + tmpARTIKEL.Lev_artikelnr
        + ';' + tmpARTIKEL.Forp_enh
        + ';' + tmpARTIKEL.Ean_namn
        + ';' + tmpARTIKEL.Moms
        + ';' + tmpARTIKEL.Pantkod
        + ';' + tmpARTIKEL.Jmfprisfakt
        + ';' + tmpARTIKEL.Jamforprisenhet
        + ';' + tmpARTIKEL.SMG_funktionstext
        + ';' + (IF AVAILABLE ttPriKat THEN ttPriKat.GrunnSortiment ELSE 'UKJENT')
        + ';' + (IF AVAILABLE tmpCPKPRIS THEN tmpCPKPRIS.cpkpris ELSE 'UKJENT')
        + ';' + (IF AVAILABLE ttPriKat THEN ttPriKat.MarkedsPris ELSE 'UKJENT')        
        + ';' + (IF AVAILABLE tmpPKPRIS THEN tmpPKPRIS.pkpris ELSE 'UKJENT')
        + ';' + (IF AVAILABLE ttPriKat THEN ttPriKat.LevPrisEngros ELSE 'UKJENT')        
                        ).
    END.

    RUN bibl_logg.p ('VPIEDIImport', 'xedivpiinnles.p - SLUTT Importkontroll logg.').
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-EksportVPIFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportVPIFil Procedure 
PROCEDURE EksportVPIFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR plFilId      AS DEC NO-UNDO.
DEF VAR pbOk         AS LOG NO-UNDO.
DEF VAR piAntLinjer  AS LOG NO-UNDO.
DEF VAR plArtikkelNr AS DEC NO-UNDO.
DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.

DEF BUFFER bVPIFilHode FOR VPIFilHode.

/* MESSAGE ctmpKatalog + cVPIUtFil SKIP   */
/*     iAntLinjer                         */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */

IF iAntLinjer > 0 THEN
DO:
    EKSPORTFIL:                    
    FOR EACH ttPrikat 
        BREAK BY ttPriKat.ButikkNr
              BY ttPriKat.LevModellNr
              BY ttPriKat.VareTekst     
              BY ttPriKat.FargeTekst    
              BY ttPriKat.SeqNrStr           
              BY ttPriKat.MarkedsPris:
        /* Er avdeling angitt, skal dette reflekteres i filnavn */
        IF FIRST-OF(ttPriKat.ButikkNr) THEN 
          DO:
            IF ttPriKat.ButikkNr = iCL THEN ttPriKat.ButikkNr.
            ELSE IF ttPriKat.ButikkNr > 0 THEN ENTRY(2,cVPIUtFil,'.') = STRING(ttPriKat.ButikkNr).
          END.
              
        OUTPUT STREAM UtVPI TO VALUE(ctmpKatalog + cVPIUtFil) NO-ECHO APPEND.                    
        PUT STREAM UtVpi UNFORMATTED 
        /*  1 */ ttPriKat.R1 ";"           
        /*  2 */ ttPriKat.LevNr ";"        
        /*  3 */ ttPriKat.LevModellNr ";"   
        /*  4 */ ttPriKat.EANnr ";"         
        /*  5 */ ttPriKat.VareTekst ";"     
        /*  6 */ ttPriKat.FargeKode ";"     
        /*  7 */ ttPriKat.FargeTekst ";"    
        /*  8 */ ttPriKat.Str ";"           
        /*  9 */ ttPriKat.StrTab ";"        
        /* 10 */ ttPriKat.Varemerke ";"     
        /* 11 */ ttPriKat.Enh ";"           
        /* 12 */ ttPriKat.AntIEnh ";"       
        /* 13 */ ttPriKat.LevPrisEngros ";" 
        /* 14 */ ttPriKat.ValKod ";"        
        /* 15 */ ttPriKat.forhRab% ";"      
        /* 16 */ ttPriKat.suppRab% ";"      
        /* 17 */ ttPriKat.VeilPris ";"      
        /* 18 */ ttPriKat.PAKstru ";"       
        /* 19 */ ttPriKat.LevUke1 ";"       
        /* 20 */ ttPriKat.LevUke2 ";"       
        /* 21 */ ttPriKat.LevUke3 ";"       
        /* 22 */ ttPriKat.LevUke4 ";"       
        /* 23 */ ttPriKat.VareGruppe ";"    
        /* 24 */ ttPriKat.LevNavn ";"       
        /* 25 */ ttPriKat.LevKod ";"  
        /* 26 */ ttPriKat.nettoForh ";"     
        /* 27 */ ttPriKat.kalkForh ";"      
        /* 28 */ ttPriKat.BFforh ";"        
        /* 29 */ ttPriKat.nettoSupp ";"     
        /* 30 */ ttPriKat.kalkSupp ";"      
        /* 31 */ ttPriKat.BFsupp ";"        
        /* 32 */ ttPriKat.MarkedsPris ";"   
        /* 33 */ ttPriKat.Sortiment ";"     
        /* 34 */ ttPriKat.Sesong ";"        
        /* 35 */ ttPriKat.VPIBildeKode ";"
        /* 36 */ ttPriKat.Merknad ";"     
        /* 37 */ ttPriKat.KjedeValutaPris ";"  
        /* 38 */ ttPriKat.KjedeProdusent ";"    
        /* 39 */ ttPriKat.ERPNr ";"           
        /* 40 */ ttPriKat.SalgsEnhetsType ";" 
        /* 41 */ ttPriKat.AktivFraDato ";"    
        /* 42 */ ttPriKat.AktivTilDato ";"    
        /* 43 */ ttPriKat.Bongtekst ";"       
        /* 44 */ ttPriKat.Etikettekst1 ";"    
        /* 45 */ ttPriKat.Funksjonskode ";"   
        /* 46 */ ttPriKat.Mva_Proc ";"        
        /* 47 */ ttPriKat.LinkVare ";"         
        /* 48 */ ttPriKat.PantBelop ";"       
        /* 49 */ ttPriKat.Filial ";"          
        /* 50 */ ttPriKat.Produsent ";"       
        /* 51 */ ttPriKat.Mengde ";"          
        /* 52 */ ttPriKat.JamforEnhet ";"     
        /* 53 */ ttPriKat.Kontrolleres ";"
        /* 54 */ ttPriKat.ArtikkelNr ";"
        /* 55 */ ttPriKat.OpprettArtikkel ";" 
        /* 56 */ ttPriKat.PosterPrisending ";" 
        /* 57 */ ";" 
        /* 58 */ ";" 
        /* 59 */ ";" 
        /* 60 */ ";" 
        /* 61 */ ";" 
        /* 62 */ ";" 
        /* 63 */ ";" 
        /* 64 */ ";" 
        /* 65 */ ";" 
        /* 66 */ ";" 
        /* 67 */ ";" 
        /* 68 */ ";" 
        /* 69 */ ";" 
        /* 70 */ ";" 
        /* 71 */ ";" 
        /* 72 */ ";" 
        /* 73 */ ";" 
        /* 74 */ ";" 
        /* 75 */ ";" 
        /* 76 */ ";" 
        /* 77 */ ";" 
        /* 78 */ ";" 
        /* 79 */ ";" 
        /* 80 */ ";" 
        /* 81 */ ";" 
        /* 82 */ ";" 
        /* 83 */ ";" 
        /* 84 */ ";" 
        /* 85 */ ";" 
        /* 86 */ ";" 
        /* 87 */ ";" 
        /* 88 */ ";" 
        /* 89 */ ";" 
        /* 90 */ ttPriKat.Etikett ";"        
        /* 91 */ ";" 
        /* 92 */ ";" 
        /* 93 */ ";" 
        /* 94 */ ";" 
        /* 95 */ ";" 
        /* 96 */ ";" 
        /* 97 */ ttPriKat.BehStatus ";" 
        /* 98 */ ttPriKat.Grunnsortiment ";"
        /* 99 */ ";"
        /*100 */ ";"
        /*101 */ cFilNavn ";"
        /*102 */ cErrorFil
        SKIP      
        .

        OUTPUT STREAM UtVpi CLOSE.
        /* Behandler filen */
        IF LAST-OF(ttPriKat.ButikkNr) THEN 
        DO:
          /* Flytter filen til ankommet katalogen */
          OS-COPY VALUE(RIGHT-TRIM(ctmpKatalog,'\') + "~\" + cVPIUtFil)
             value(VPIFilHode.Katalog + "~\" + cVPIUtFil).
          /* Renser bort temp fil */
          IF SEARCH(VPIFilHode.Katalog + "~\" + cVPIUtFil) <> ? THEN
            OS-DELETE VALUE(RIGHT-TRIM(ctmpKatalog,'\') + "~\" + cVPIUtFil).
          
          /* Opprett VPIFilHode og les inn. */
          LES-INN-OPPDATER:
          DO:
            FILE-INFO:FILE-NAME = VPIFilHode.Katalog + "~\" + cVPIUtFil.

            /* Finner FilId */
            FIND LAST bVPIFilHode NO-LOCK NO-ERROR.
            IF AVAILABLE bVPIFilHode THEN
              plFilId = bVPIFilHode.FilId + 1.
            ELSE
              plFilId = 1.
            DO TRANSACTION:
              CREATE bVPIFilHode.
              ASSIGN
                bVPIFilHode.FilId        = plFilId
                bVPIFilHode.FilNavn      = cVPIUtFil
                bVPIFilHode.Katalog      = VPIFilHode.Katalog
                bVPIFilHode.Dato         = FILE-INFO:FILE-MOD-DATE
                bVPIFilHode.Kl           = STRING(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS")
                bVPIFilHode.Storrelse    = FILE-INFO:FILE-SIZE
                bVPIFilHode.AntLinjer    = 0
                bVPIFilHode.VPIFilType   = 1 /* VPI */
                bVPIFilHode.VPIFilStatus = 1
                bVPIFilHode.EkstVPILevNr = (IF ttPriKat.ButikkNr = iCL THEN iHKVPILomme
                                            ELSE IF ttPriKat.ButikkNr = 0 THEN EkstVPILev.EkstVPILevNr
                                            ELSE (1000000 + ttPriKat.ButikkNr)) 
                .
              FIND CURRENT bVPIFilHode NO-LOCK.
            END. /* TRANSACTION */

            FIND FIRST EkstVPIFil NO-LOCK WHERE
              EkstVPIFil.EkstVPILevNr = bVPIFilHode.EkstVPILevNr AND
              EkstVPIFil.VPIFilTypeNr = 1 AND
              EkstVPIFil.VPIFilNavn   BEGINS SUBSTRING(bVPIFilHode.FilNavn,1,4) NO-ERROR.
              
            IF AVAILABLE EkstVPIFil THEN 
              DO:
                /* Leser inn fillinjene */
                IF SEARCH(EkstVPIFil.VPIInnlesningsrutine + ".r") <> ? THEN
                  DO:
                    RUN VALUE(EkstVPIFil.VPIInnlesningsrutine + '.p') 
                      (INPUT  plFilId,
                       INPUT  THIS-PROCEDURE,
                       OUTPUT piAntLinjer 
                      ).
                  END.
                /* Pakker ut fil. */
                IF SEARCH(EkstVPIFil.VPIUtpakkingsrutine + ".r") <> ? THEN
                  DO:
                    RUN VALUE(EkstVPIFil.VPIUtpakkingsrutine + '.p')
                      (INPUT bVPIFilHode.FilId).              
                  END.
              END.

            IF SEARCH(VPIFilHode.Katalog + "~\" + cVPIUtFil) <> ? THEN
              OS-DELETE VALUE(VPIFilHode.Katalog + "~\" + cVPIUtFil).
            RELEASE bVPIFilHode.

          END. /* LES-INN-OPPDATER */
        END.
    END. /* EKSPORTFIL */
    
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
  OUTPUT TO VALUE(cErrorFil).
    PUT UNFORMATTED SKIP(1)
        "   ***   EDIFIL KONVERTERT TIL UTVIDET PRICAT FORMAT    ***" SKIP
        "   ********************************************************" SKIP(1)
        .

    PUT UNFORMATTED
      "Innlesning            : " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      "Logg fra innlesning av: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn SKIP
      .

    /* Feil som avbryter innlesning av fil. */
    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 1) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** UGYLDIGE VERDIER (ErrorLogg) ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 1
            BY tt_Error.LinjeNr:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.
    /* Linjer med ugyldige koder */
    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 2) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** UKJENTE TRANSAKSJONSkoder ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   Det ligger ukjente transaksjonstyper på disse linjene." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 2
            BY tt_Error.LinjeNr:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.
    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 3) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** BLANKE EAN KODER (Linjevalidering) - FEIL FUNNET VED KONTROLL AV LINJER ***" + chr(10) +
            (IF bTillatBlankEAN = FALSE THEN "   Linjer som inneholder ugyldige verdier er ikke importert."
             ELSE "   Linjer med blanke EAN koder.") SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 3
            BY tt_Error.LinjeNr:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.
    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 4) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** UGYLDIGE VERDIER i EAN koder (Linjevalidering) - FEIL FUNNET VED KONTROLL AV LINJER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 4
            BY tt_Error.LinjeNr:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.
    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 5) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** EAN MED FEIL SJEKKSIFFER (Linjevalidering) - FEIL FUNNET VED KONTROLL AV LINJER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 5
            BY tt_Error.LinjeNr:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.

    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 6) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** LEVERANDØRNR SOM MANGLER MAPPING (Linjevalidering) - FEIL FUNNET VED KONTROLL AV LINJER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 6
            BY tt_Error.LinjeNr:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.

    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 7) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** PRODUSENTNR SOM MANGLER MAPPING (Linjevalidering) - FEIL FUNNET VED KONTROLL AV LINJER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 7
            BY tt_Error.LinjeNr:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.

    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 8) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** VAREGRUPPER SOM MANGLER MAPPING (Linjevalidering) - FEIL FUNNET VED KONTROLL AV LINJER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 8
            BY tt_Error.LinjeNr:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.

    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 9) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** FEIL VED PANTVARER/LINK (Linjevalidering) - FEIL FUNNET VED KONTROLL AV LINJER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 9
            BY tt_Error.LinjeNr:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.

    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 10) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** FEIL SALGSENHET (Linjevalidering) - FEIL FUNNET VED KONTROLL AV LINJER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 10
            BY tt_Error.LinjeNr:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.

    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 11) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** FEIL FUNKSJONSKODE (Linjevalidering) - FEIL FUNNET VED KONTROLL AV LINJER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 11
            BY tt_Error.LinjeNr:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.

    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 12) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** UKJENT BUTIKKNUMMER - FILIALNUMMER (Linjevalidering) - FEIL FUNNET VED KONTROLL AV LINJER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 12
            BY tt_Error.LinjeNr:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.

    /* PKPRIS */
    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 14) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** FEIL I PKPRIS - FEIL FUNNET VED KONTROLL AV LINJER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 14
            BY tt_Error.LinjeNr:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.

    /* CPKPRIS */
    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 15) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** FEIL I CPKPRIS - FEIL FUNNET VED KONTROLL AV LINJER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 15
            BY tt_Error.LinjeNr:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.

    /* Artikkel - utmeldte artikler */
    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 16) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   ***   UTGÅTTE ARTIKLER SOM IGNORERES VED INNLESNING   ***" SKIP
            "   Linjer som inneholder utgåtte artikler er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 16
            BY tt_Error.LinjeNr:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.

    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_Error) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** ØVRIGE FEILMELDINGER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.
  
  OUTPUT CLOSE.
  /*
  IF SEARCH(cErrorFil) <> ? THEN
  DO:
    DEF VAR hInstance AS INT.

    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH(cErrorFil),
                                  "",
                                  1,
                                  OUTPUT hInstance).

  END.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-flyttFil) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE flyttFil Procedure
PROCEDURE flyttFil:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    ASSIGN
      pcBkuFil = VPIFilHode.Katalog + "~\bku" + "\" + 
                 VPIFilHode.FilNavn.

    /* Sikrer at backup katalog finnes. */
    OS-CREATE-DIR value(VPIFilHode.Katalog + "~\bku").
    /* Flytter filen til backup katalog. */
    OS-COPY value(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) 
            value(pcBkuFil).
    /* Renser bort fil */
    IF SEARCH(pcBkuFil) <> ? THEN
    DO:
        /* Filen tas bort fra katalogen. */
        IF SEARCH(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) <> ? THEN
            OS-DELETE VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn).
    END.
    /* Renser bort temp fil */
    IF SEARCH(VPIFilHode.Katalog + "~\" + cVPIFil) <> ? THEN
      OS-DELETE VALUE(VPIFilHode.Katalog + "~\" + cVPIFil).
    IF SEARCH(VPIFilHode.Katalog + "~\" + cVPIUtFil) <> ? THEN
      OS-DELETE VALUE(VPIFilHode.Katalog + "~\" + cVPIUtFil).
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-LesInnARTIKELFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnARTIKELFil Procedure 
PROCEDURE LesInnARTIKELFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       

------------------------------------------------------------------------------*/
  DEF VAR piAntFeil AS INT  NO-UNDO.
  DEF VAR plVareNr  AS DEC  NO-UNDO.
  DEF VAR pcStr     AS CHAR NO-UNDO.
  DEF VAR piLoop    AS INT  NO-UNDO.
  DEFINE VARIABLE pcTekst        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE bErstattModus  AS LOG       NO-UNDO.
  DEFINE VARIABLE pcOldChar      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cEAN           AS CHARACTER NO-UNDO.
  
  DEFINE BUFFER bButiker FOR Butiker.
  
  /* Tømmer feillogg. */
  FOR EACH tt_Error:
    DELETE tt_Error.
  END.
  /* Tømmer pricat */
  FOR EACH ttPrikat:
      DELETE ttPrikat.
  END.

  RUN TellOppLinjer.
  IF RETURN-VALUE = "FEIL" THEN DO:
      CREATE tt_Error.
      ASSIGN
        tt_Error.LinjeNr   = 1
        tt_Error.Tekst     = "** Feil på linje 1. IKKE gyldig Artikkelfil header"
        tt_Error.Gradering = 1.
        .
      RETURN.
  END.

  ASSIGN
      piLinjeNr  = 1.
      iAntLinjer = 0
      .
  /*INPUT STREAM InnFil FROM VALUE(cFilNavn) CONVERT SOURCE "IBM850" NO-ECHO.*/
  INPUT STREAM InnFil FROM VALUE(cFilNavn) CONVERT SOURCE "ISO8859-1" NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.

    ASSIGN
      iAntLinjer = iAntLinjer + 1
      pcLinje    = REPLACE(pcLinje,'"',' ')
      pcLinje    = TRIM(pcLinje,'"') /* Preem legger ut med " i første og siste posisjon. */
      .
      
    IF TRIM(TRIM(SUBSTRING(pcLinje,1,2)),'"') = '00' THEN 
    DO:
      /* Sjekker transaksjonskoden */
      IF TRIM(SUBSTRING(pcLinje,1,2)) <> '00' THEN
        DO:
          CREATE tt_Error.
          ASSIGN
            piAntFeil        = piAntFeil + 1
            tt_Error.LinjeNr = iAntLinjer
            tt_Error.Tekst   = "** Feil på linje " + string(iAntLinjer) + ". Feil kode. Skal være(00). Den er (" + ENTRY(1,pcLinje) + ").".
            tt_Error.Gradering = 2.
          NEXT.
        END.
        
      CREATE tmpALLA.
      ASSIGN
        tmpALLA.Informationsmangd = TRIM(TRIM(SUBSTRING(pcLinje,3,10)),'"')
        tmpALLA.Filler1           = TRIM(TRIM(SUBSTRING(pcLinje,13,4)),'"')
        tmpALLA.Kundnr            = TRIM(TRIM(SUBSTRING(pcLinje,17,6)),'"')
        tmpALLA.Filler2           = TRIM(TRIM(SUBSTRING(pcLinje,23,6)),'"')
        tmpALLA.Datum             = TRIM(TRIM(SUBSTRING(pcLinje,29,8)),'"')
        tmpALLA.Transaktionsvecka = TRIM(TRIM(SUBSTRING(pcLinje,37,6)),'"')
        tmpALLA.Filler3           = TRIM(TRIM(SUBSTRING(pcLinje,43,4)),'"')
        tmpALLA.Timestamp         = TRIM(TRIM(SUBSTRING(pcLinje,47,14)),'"')
        tmpALLA.KundEAN           = TRIM(TRIM(SUBSTRING(pcLinje,61,13)),'"')
        NO-ERROR.    

      /* Hvis filial er prefikset med BS, skal dette strippes. */
      IF tmpALLA.Kundnr BEGINS 'BS' THEN 
        tmpALLA.Kundnr = SUBSTRING(tmpALLA.Kundnr,3).
      
      /* Konvertering av filial til butikknummer */
      IF cEDB-System <> '' AND 
        CAN-FIND(FIRST ImpKonv NO-LOCK WHERE
          ImpKonv.EDB-System = cEDB-System AND
          ImpKonv.Tabell     = 'Butiker') THEN 
      DO:
        FIND FIRST ImpKonv NO-LOCK WHERE
          ImpKonv.EDB-System = cEDB-System AND
          ImpKonv.Tabell     = 'Butiker' AND
          ImpKonv.EksterntId = TRIM(tmpALLA.Kundnr) NO-ERROR. 
        IF AVAILABLE ImpKonv THEN tmpALLA.Kundnr = ImpKonv.InterntId.
        ELSE DO:
            CREATE tt_Error.
            ASSIGN
              tt_Error.LinjeNr   = iAntLinjer
              tt_Error.Tekst     = "** Butikk (" + tmpALLA.Kundnr + ") uten mapping i konverteringstabell: " + string(iAntLinjer) + ": " + pcLinje + "."
              tt_Error.Gradering = 12.
            IF bGenMapping THEN RUN opprettMapping (cEDB-System,'Butiker',STRING(iCl),TRIM(tmpALLA.Kundnr)).
            ELSE DO: 
              CREATE tt_Error.
              ASSIGN
                tt_Error.LinjeNr   = iAntLinjer
                tt_Error.Tekst     = "** Innlesning av fil til butikk  (" + tmpALLA.Kundnr + ") er avbrutt."
                tt_Error.Gradering = 12.
              RETURN 'AVBRYT'.
            END. 
        END.
      END.
      
      /* Hvis pcLinje er 293 karrakterer lang, inneholder den også 2. linje i filen.     */
      /* Årsaken er at det ligger 4 null byte karrakterer i posisjon 43 til 46.          */
      /* Her sendes resten av linjen videre slik at vi ikke mister første linje i filen. */
      IF LENGTH(pcLinje) = 293 THEN
        pcLinje = SUBSTRING(pcLinje,43).
      /* Er linjen ok, leser vi neste linje. */
      ELSE NEXT.
    END.
    ELSE FIND FIRST tmpALLA WHERE
      tmpALLA.Informationsmangd = 'Artikel' NO-ERROR.

    /* Tomme linjer  */
    IF TRIM(pcLinje) = "" THEN
      DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil          = piAntFeil + 1
          tt_Error.LinjeNr   = iAntLinjer
          tt_Error.Tekst     = VareIdent() + "* Feil transaksjonskode på linje " + string(iAntLinjer) + ". Blank linje.  (" + ENTRY(1,pcLinje) + ")."
          tt_Error.ButikkNr  = INT(tmpALLA.Kundnr)
          tt_Error.Gradering = 2.
        NEXT.
      END.
      
    /* Sjekker transaksjonskoden */
    IF NOT CAN-DO('10,20,30',TRIM(SUBSTRING(pcLinje,1,2))) THEN
      DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil          = piAntFeil + 1
          tt_Error.LinjeNr   = iAntLinjer
          tt_Error.Tekst     = VareIdent() + "* Feil transaksjonskode linje i ARTIKEL " + string(iAntLinjer) + ". Feil kode. Skal være(10, 20 eller 30). Den er (" + ENTRY(1,pcLinje) + ")."
          tt_Error.ButikkNr  = INT(tmpALLA.Kundnr)
          tt_Error.Gradering = 15.
        NEXT.
      END.

    /* Kode 30 - Utgått skal ignoreres. */
    IF CAN-DO('30',TRIM(SUBSTRING(pcLinje,1,2))) THEN
      DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil          = piAntFeil + 1
          tt_Error.LinjeNr   = iAntLinjer
          tt_Error.Tekst     = VareIdent() + "* Trans.kod på linje i ARTIKEL " + string(iAntLinjer) + ". Ignoreres. Koden er (" + ENTRY(1,pcLinje) + ")."
          tt_Error.ButikkNr  = INT(tmpALLA.Kundnr)
          tt_Error.Gradering = 16.
        NEXT.
      END.

    /* Er det for få kolonner på linjen */
    IF LENGTH(pcLinje) <> 251 THEN
    DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil          = piAntFeil + 1
          tt_Error.LinjeNr   = iAntLinjer
          tt_Error.Tekst     = VareIdent() +  "** Feil på linje " + STRING(iAntLinjer) + ". Feil antall tegn. Skal være(251). Den er (" + STRING(LENGTH(pcLinje)) + ")."
          tt_Error.ButikkNr  = INT(tmpALLA.Kundnr)
          tt_Error.Gradering = 2.
        NEXT.
    END.

    /* Legger opp linjen */
    CREATE tmpARTIKEL.
    ASSIGN       
      tmpARTIKEL.Produktid          = TRIM(TRIM(SUBSTRING(pcLinje,3,13)),'"')
      tmpARTIKEL.Forpackning_id     = TRIM(TRIM(SUBSTRING(pcLinje,16,2)),'"')
      tmpARTIKEL.Artikelnr          = TRIM(TRIM(SUBSTRING(pcLinje,18,13)),'"')
      tmpARTIKEL.Benamning          = TRIM(TRIM(SUBSTRING(pcLinje,31,30)),'"')
      tmpARTIKEL.Forp_enh           = TRIM(TRIM(SUBSTRING(pcLinje,61,3)),'"')
      tmpARTIKEL.Forp_stl           = TRIM(TRIM(SUBSTRING(pcLinje,64,4)),'"')
      tmpARTIKEL.Jmfprisfakt        = TRIM(TRIM(SUBSTRING(pcLinje,68,5)),'"')
      tmpARTIKEL.Pantstatus         = TRIM(TRIM(SUBSTRING(pcLinje,73,1)),'"')
      tmpARTIKEL.Eannr              = TRIM(TRIM(SUBSTRING(pcLinje,74,13)),'"')
      tmpARTIKEL.Nyttigkod          = TRIM(TRIM(SUBSTRING(pcLinje,87,1)),'"')
      tmpARTIKEL.Ean_namn           = TRIM(TRIM(SUBSTRING(pcLinje,88,22)),'"')
      tmpARTIKEL.Ean_viktvol        = TRIM(TRIM(SUBSTRING(pcLinje,110,8)),'"')
      tmpARTIKEL.Ean_leverantor     = TRIM(TRIM(SUBSTRING(pcLinje,118,14)),'"')
      tmpARTIKEL.Fiberrik           = TRIM(TRIM(SUBSTRING(pcLinje,132,1)),'"')
      tmpARTIKEL.Fettsnal           = TRIM(TRIM(SUBSTRING(pcLinje,133,1)),'"')
      tmpARTIKEL.Oblekt             = TRIM(TRIM(SUBSTRING(pcLinje,134,1)),'"')
      tmpARTIKEL.Ean_textnr         = TRIM(TRIM(SUBSTRING(pcLinje,135,3)),'"')
      tmpARTIKEL.Ean_kvittotext     = TRIM(TRIM(SUBSTRING(pcLinje,138,12)),'"')
      tmpARTIKEL.Butiksvarugrupp    = TRIM(TRIM(SUBSTRING(pcLinje,150,4)),'"')
      tmpARTIKEL.Leverantorsid      = IF iLevNr > 0 THEN STRING(iLevNr) ELSE TRIM(TRIM(SUBSTRING(pcLinje,154,7)),'"')
      tmpARTIKEL.SMG_funktionstext  = TRIM(TRIM(SUBSTRING(pcLinje,161,10)),'"')
      tmpARTIKEL.Pantkod            = TRIM(TRIM(SUBSTRING(pcLinje,171,2)),'"')
      tmpARTIKEL.Pallstorlek        = TRIM(TRIM(SUBSTRING(pcLinje,173,4)),'"')
      tmpARTIKEL.Direktvara         = TRIM(TRIM(SUBSTRING(pcLinje,177,1)),'"')
      tmpARTIKEL.Lev_artikelnr      = TRIM(TRIM(SUBSTRING(pcLinje,178,15)),'"')
      tmpARTIKEL.Orskakskod_ny_forp = TRIM(TRIM(SUBSTRING(pcLinje,193,2)),'"')
      tmpARTIKEL.Utbytt_tidigarenr  = TRIM(TRIM(SUBSTRING(pcLinje,195,13)),'"')
      tmpARTIKEL.Produktklassid     = TRIM(TRIM(SUBSTRING(pcLinje,208,4)),'"')
      tmpARTIKEL.Filler1            = TRIM(TRIM(SUBSTRING(pcLinje,212,3)),'"')
      tmpARTIKEL.Anskaffningsvara   = TRIM(TRIM(SUBSTRING(pcLinje,215,1)),'"')
      tmpARTIKEL.Transitvara        = TRIM(TRIM(SUBSTRING(pcLinje,216,1)),'"')
      tmpARTIKEL.Jamforprisflagga   = TRIM(TRIM(SUBSTRING(pcLinje,217,1)),'"')
      tmpARTIKEL.Jamforprisenhet    = TRIM(TRIM(SUBSTRING(pcLinje,218,8)),'"')
      tmpARTIKEL.EAN_alt_kvittotext = TRIM(TRIM(SUBSTRING(pcLinje,226,20)),'"')
      tmpARTIKEL.Filler2            = TRIM(TRIM(SUBSTRING(pcLinje,246,2)),'"')
      tmpARTIKEL.Moms               = TRIM(TRIM(SUBSTRING(pcLinje,248,4)),'"')
      .      
    /* Konverterer flagg som setter Grunnsortiment. */
    IF tmpARTIKEL.SMG_funktionstext = 'P' THEN 
      tmpARTIKEL.SMG_funktionstext = 'TRUE'.
    ELSE tmpARTIKEL.SMG_funktionstext = ''.

    /* Konvertering av EAN koder med pris eller vekt i kode. */
    /* Gjøres temporært her. For å kunne hente ArtPris.      */
    /* Retter også opp sjekksiffer.                          */
    IF tmpARTIKEL.Eannr <> '' THEN
      DO:
        cEAN = tmpARTIKEL.Eannr.
        RUN bibl_chkean.p (INPUT-OUTPUT cEAN).
      END.
    ELSE cEAN = ''. 

    /* Hvis IPS EDI filen skal splittes og sendes til butikk, logges linjene her. */
    /* Linjene logges og hver linje merkes med butikknr, slik at det blir         */
    /* enkelt å legge dem ut til respektive butikker.                             */
    /* Det er kun linjer som er merket med et avdelingsnr. som logges.            */  
    /* Utlegget til butikkene, startes etter at prisfilene med inn og utpris er   */
    /* lest inn.                                                                  */
    IF bDistribIPS THEN
    DISTRIB_BUT: 
    DO:
      ASSIGN cFilial = tmpALLA.Kundnr NO-ERROR.
      IF ERROR-STATUS:ERROR OR cFilial = '' OR cFilial = '0' THEN LEAVE DISTRIB_BUT.
      ASSIGN lDec = DECIMAL(cFilial) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN LEAVE DISTRIB_BUT.
      /* Logger posten for distribusjon. */
      CREATE tmpEDI.
      ASSIGN
        tmpEDI.ButikkNr = INTEGER(cFilial)
        tmpEDI.cLinje   = pcLinje
        .
    END. /* DISTRIB_BUT */

    /* Linjevalidering.                                                 */
    /* Linjer med feil flagges, og blir ikke med vidre i oppdateringen. */
    /* De skrives ut i Error fil.                                       */
    RUN Linjevalidering.
    IF RETURN-VALUE <> "OK" THEN
      DO: 
        CREATE tt_Error.
        ASSIGN
          piAntFeil          = piAntFeil + 1
          tt_Error.LinjeNr   = iAntLinjer
          tt_Error.Tekst     = VareIdent() + "* Feil på linje " + string(iAntLinjer) + ". Feilkode returnert fra linjevaliderng. (" + ENTRY(1,pcLinje) + ")."
          tt_Error.ButikkNr  = INT(tmpALLA.Kundnr)
          tt_Error.Gradering = 2.
        NEXT.
      END.
    
    /* Henter artikkelens pris hvis den finnes fra før. */
    IF AVAILABLE ArtPris THEN RELEASE ArtPris.
    IF cEAN <> '' THEN 
      FIND Strekkode NO-LOCK WHERE
           Strekkode.Kode = cEAN NO-ERROR.
    /*       
    IF AVAILABLE Strekkode THEN
      FIND ArtPris NO-LOCK WHERE 
        ArtPris.ArtikkelNr = Strekkode.ArtikkelNr AND
        ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
      FIND FIRST ArtPris NO-LOCK WHERE 
        ArtPris.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
    */
    
    CREATE ttPriKat.
    ASSIGN
         ttPriKat.EkstVPILevNr    = VPIFilHode.EkstVPILevNr
         ttPriKat.LinjeNr         = piLinjeNr
         piLinjeNr                = piLinjeNr  + 1
/*  1 */ ttPriKat.R1              = "R1"
/*  2 */ ttPriKat.LevNr           = tmpARTIKEL.Leverantorsid
/*  3 */ ttPriKat.LevModellNr     = tmpARTIKEL.Artikelnr
/*  4 */ ttPriKat.EANnr           = tmpARTIKEL.Eannr
/*  5 */ ttPriKat.VareTekst       = tmpARTIKEL.Ean_namn /*tmpARTIKEL.Benamning*/
         ttPriKat.Varetekst       = REPLACE(ttPriKat.VareTekst,';',',')
/*  6 */ ttPriKat.FargeKode       = "" /* Farge er ikke i bruk */
/*  7 */ ttPriKat.FargeTekst      = "" /* Settes blank */
/*  8 */ ttPriKat.Str             = " 1" /* Størrelse 1 */
/*  9 */ ttPriKat.StrTab          = "2" /* Størrelsestype 2 */
/* 10 */ ttPriKat.Varemerke       = "" 
/* 11 */ ttPriKat.Enh             = tmpARTIKEL.Forp_enh
         ttPriKat.Enh             = REPLACE(ttPriKat.Enh,';',',')
/* 12 */ ttPriKat.AntIEnh         = tmpARTIKEL.Forp_stl 
/* 13 */ ttPriKat.LevPrisEngros   = IF AVAILABLE ArtPris THEN STRING(ArtPris.InnkjopsPris[1]) ELSE "" /* Skal fylles ut */
/* 14 */ ttPriKat.ValKod          = "NOK" 
/* 15 */ ttPriKat.forhRab%        = IF AVAILABLE ArtPris THEN STRING(ArtPris.Rab1%[1]) ELSE "" 
/* 16 */ ttPriKat.suppRab%        = IF AVAILABLE ArtPris THEN STRING(ArtPris.Rab1%[1]) ELSE "" 
/* 17 */ ttPriKat.VeilPris        = IF AVAILABLE ArtPris THEN STRING(ArtPris.Pris[1]) ELSE "" /* Skal fylles ut */
/* 18 */ ttPriKat.PAKstru         = "" 
/* 19 */ ttPriKat.LevUke1         = "" 
/* 20 */ ttPriKat.LevUke2         = "" 
/* 21 */ ttPriKat.LevUke3         = "" 
/* 22 */ ttPriKat.LevUke4         = "" 
/* 23 */ ttPriKat.VareGruppe      = tmpARTIKEL.Produktklassid 
/* 24 */ ttPriKat.LevNavn         = "" 
/* 25 */ ttPriKat.LevKod          = tmpARTIKEL.ProduktId
/* 26 */ ttPriKat.nettoForh       = "" 
/* 27 */ ttPriKat.kalkForh        = "" 
/* 28 */ ttPriKat.BFforh          = "" 
/* 29 */ ttPriKat.nettoSupp       = "" 
/* 30 */ ttPriKat.kalkSupp        = "" 
/* 31 */ ttPriKat.BFsupp          = "" 
/* 32 */ ttPriKat.MarkedsPris     = IF AVAILABLE ArtPris THEN STRING(ArtPris.Pris[1]) ELSE "" /* Skal fylles ut */  
/* 33 */ ttPriKat.Sortiment       = "" 
/* 34 */ ttPriKat.Sesong          = "" 
/* 35 */ ttPriKat.VPIBildeKode    = "" 
/* 36 */ ttPriKat.Merknad         = "" 
/* 37 */ ttPriKat.KjedeValutaPris = "" 
/* 38 */ ttPriKat.KjedeProdusent  = ""  
/* 39 */ ttPriKat.ERPNr           = tmpARTIKEL.Lev_artikelnr
/* 40 */ ttPriKat.SalgsEnhetsType = tmpARTIKEL.Forp_enh
/* 41 */ ttPriKat.AktivFraDato    = "" /* Skal fylles ut */
/* 42 */ ttPriKat.AktivTilDato    = "" /* Skal fylles ut */
/* 43 */ ttPriKat.Bongtekst       = tmpARTIKEL.Ean_namn
         ttPriKat.Bongtekst       = REPLACE(ttPriKat.Bongtekst,';',',')
/* 44 */ ttPriKat.Etikettekst1    = tmpARTIKEL.Ean_namn
         ttPriKat.Etikettekst1    = REPLACE(ttPriKat.Etikettekst1,';',',')
         ttPriKat.Etikettekst1    = (IF ttPriKat.Etikettekst1 = '' THEN ttPriKat.Varetekst ELSE ttPriKat.Etikettekst1)
/* 45 */ ttPriKat.Funksjonskode   = "N" /* N = "Normal", dvs. vare/prisendring, K = Kampanje, M = Medlemstilbud, U = Utmelding (tolkes som sletting), A = Slett ikke påbegynt kampanje, avslutt påbegynt kampanje */
/* 46 */ ttPriKat.Mva_Proc        = STRING(DECIMAL(tmpARTIKEL.Moms) / 100)
/* 47 */ ttPriKat.LinkVare        = tmpARTIKEL.Pantkod /* Skal fylles ut hvis pant */ 
/* 48 */ ttPriKat.PantBelop       = "" /* Skal fylles ut hvis pant */
/* 49 */ ttPriKat.Filial          = tmpALLA.Kundnr 
/* 50 */ ttPriKat.Produsent       = "" /* Skal fylles ut. */
/* 51 */ ttPriKat.Mengde          = STRING(1 / (DECIMAL(tmpARTIKEL.Jmfprisfakt) / 1000)) /* Konv. faktor jamførpris */
/* 52 */ ttPriKat.JamforEnhet     = tmpARTIKEL.Jamforprisenhet
/* 53 */ ttPriKat.Kontrolleres    = FALSE 
/* 54  ttPriKat.ArtikkelNr      */
/* 55  ttPriKat.OpprettArtikkel */
/* 98 */ ttPriKat.GrunnSortiment  = tmpARTIKEL.SMG_funktionstext
         ttPriKat.Etikett         = iEtikett
         ttPriKat.Lager           = bLager
         ttPriKat.BehStatus       = 1 /* Ubehandlet */
        NO-ERROR.

     IF ERROR-STATUS:ERROR THEN 
     DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil          = piAntFeil + 1
          tt_Error.LinjeNr   = iAntLinjer
          tt_Error.Tekst     = VareIdent() + "* Feil på linje " + STRING(iAntLinjer) + ". Feil ved assign ttPrikat."
          tt_Error.ButikkNr  = INT(tmpALLA.Kundnr)          
          tt_Error.Gradering = 2.
        NEXT.
     END.      
     
     /* NB: Filial er mappet om til PRS butikknr eller til HK's VPILevNr lenger oppe.               */
     /* Er filial angitt og denne matcher med et butikknummer, skal butikknr settes i prikat filen. */
     /* Det skal legges opp en post i butikkens VPI lomme, og nye poster legges også i hk's lomme.  */
     /* Posten som legges i hk's VPI lomme, skal ha hk's utpris og den nye innprisen. Og posten som */
     /* legges i butikkens VPI lomme, skal ha den nye utpris og hk's innpris.                       */
     lDec = 0.
     ASSIGN lDec = DECIMAL(ttPriKat.Filial) NO-ERROR.
     IF lDec > 0 AND CAN-FIND(Butiker WHERE Butiker.Butik = INTEGER(lDec)) THEN 
     DO:
       IF AVAILABLE hkArtPris  THEN RELEASE hkartPris.
       IF AVAILABLE Strekkode  THEN RELEASE Strekkode.
       
       /* Henter artikkelnr og ArtPris for profilene. */
       FIND Strekkode NO-LOCK WHERE
            Strekkode.Kode = ttPriKat.EANnr NO-ERROR.
       FIND VPIStrekkode NO-LOCK WHERE 
            VPIStrekkode.EkstVPILevNr = VPIFilHode.EkstVPILevNr AND 
            VPIStrekkode.Kode = ttPriKat.EANnr NO-ERROR.          
       /* Henter HK's kalkyle hvis den finnes.      */
       IF AVAILABLE Strekkode THEN 
         FIND hkArtPris NO-LOCK WHERE 
           hkArtPris.ArtikkelNr = Strekkode.ArtikkelNr AND 
           hkArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.    
       
       /* Setter priser i butikkens VPI lomme. */ 
       ASSIGN
         ttPriKat.LevPrisEngros = (IF AVAILABLE hkArtPris 
                                     THEN STRING(hkArtPris.InnkjopsPris[1]) 
                                     ELSE ttPriKat.LevPrisEngros)
         ttPriKat.ButikkNr = INTEGER(lDec)
         .
       IF NOT CAN-DO(cEkstVPILevLst,STRING(lDec)) THEN 
         cEkstVPILevLst = cEkstVPILevLst + (IF cEkstVPILevLst = '' THEN '' ELSE ',') + STRING(lDec).
       RUN opprettEkstVPILev (INTEGER(lDec)).
       
     END.
     ELSE IF INTEGER(ttPriKat.Filial) > 0 THEN 
       DO:
         /* I et driftsmiljø, skal denne variabelen alltid være satt til false. */
         IF bGenButikk = FALSE THEN 
           DO:
             CREATE tt_Error.
             ASSIGN
               tt_Error.LinjeNr   = iAntLinjer
               tt_Error.Tekst     = VareIdent() + "* Ukjent butikknummer (Fillal: " + ttPriKat.Filial + ") på linje: " + string(iAntLinjer) + ":  " + pcLinje + "."
               tt_Error.ButikkNr  = INT(tmpALLA.Kundnr)
               tt_Error.Gradering = 12.
           END.
         ELSE DO: /* Dette gjøres kun i testsammenheng hvor man ønsker å generere opp butikkene. */
           FIND FIRST Butiker NO-LOCK WHERE Butiker.Butik > 0 NO-ERROR.
           IF AVAILABLE Butiker THEN 
             DO:
               CREATE bButiker.
               BUFFER-COPY Butiker
                 EXCEPT Butik  
               TO bButiker 
                 ASSIGN bButiker.Butik = INTEGER(ttPriKat.Filial) NO-ERROR. 
               /* Til HK's VPI lomme */
               CREATE bttPriKat.
               BUFFER-COPY ttPriKat TO bttPriKat
                 ASSIGN
                   bttPriKat.Filial = ''
                   bttPriKat.BehStatus = 10. /* Vent */
               /* Til butikkens VPI lomme */ 
               ttPriKat.ButikkNr = INTEGER(ttPriKat.Filial).
               RUN opprettEkstVPILev (INTEGER(ttPriKat.Filial)).
             END.
         END.
       END.
    
     IF iAntLinjer MODULO 10 = 0 THEN 
       STATUS DEFAULT "Leser linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-LesInnCPKPRISFil) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnCPKPRISFil Procedure
PROCEDURE LesInnCPKPRISFil:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr AS INT  NO-UNDO.
  DEF VAR piAntFeil AS INT  NO-UNDO.
  DEF VAR plVareNr  AS DEC  NO-UNDO.
  DEF VAR pcStr     AS CHAR NO-UNDO.
  DEF VAR piLoop    AS INT  NO-UNDO.
  DEFINE VARIABLE pcTekst       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE bErstattModus AS LOG       NO-UNDO.
  DEFINE VARIABLE pcOldChar     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcPkFilNavn   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcBkuFilNavn  AS CHARACTER NO-UNDO.
    
  DEFINE BUFFER bButiker FOR Butiker.
  
  ASSIGN
    obOk = FALSE
    pcPkFilNavn  = TRIM(cFilNavn)
    pcPkFilNavn  = REPLACE(pcPkFilNavn,'ARTIKEL','CPKPRIS')
    pcBkuFilNavn = REPLACE(pcPkFilNavn,'CPKPRIS','bku\CPKPRIS').

  IF SEARCH(pcPkFilNavn) = ? THEN
    DO: 
      RETURN.
    END.
 
  ASSIGN
      piLinjeNr  = 1.
      iAntLinjer = 0
      .
  /*INPUT STREAM InnFil FROM VALUE(cFilNavn) CONVERT SOURCE "IBM850" NO-ECHO.*/
  INPUT STREAM InnFil FROM VALUE(pcPkFilNavn) CONVERT SOURCE "ISO8859-1" NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.

    ASSIGN
      iAntLinjer = iAntLinjer + 1
      pcLinje    = TRIM(pcLinje,'"') /* Preem legger ut med " i første og siste posisjon. */
      .
    /* Den første linjen skal alltid inneholde RIGAL header. */
    /* Men vi skal også tåle at det ikke kommer header.      */
    IF iAntLinjer = 1 THEN 
    DO:
      /* Sjekker transaksjonskoden */
      IF TRIM(SUBSTRING(pcLinje,1,2)) <> '00' THEN
        DO:
          CREATE tt_Error.
          ASSIGN
            piAntFeil        = piAntFeil + 1
            tt_Error.LinjeNr = iAntLinjer
            tt_Error.Tekst   = "** Feil på linje i CPKPRIS " + string(iAntLinjer) + ". Feil kode. Skal være(00). Den er (" + ENTRY(1,pcLinje) + ").".
            tt_Error.Gradering = 15.
          NEXT.
        END.
      CREATE tmpALLA.
      ASSIGN
        tmpALLA.Informationsmangd = TRIM(TRIM(SUBSTRING(pcLinje,3,10)),'"')
        tmpALLA.Filler1           = TRIM(TRIM(SUBSTRING(pcLinje,13,4)),'"')
        tmpALLA.Kundnr            = TRIM(TRIM(SUBSTRING(pcLinje,17,6)),'"')
        tmpALLA.Filler2           = TRIM(TRIM(SUBSTRING(pcLinje,23,6)),'"')
        tmpALLA.Datum             = TRIM(TRIM(SUBSTRING(pcLinje,29,8)),'"')
        tmpALLA.Transaktionsvecka = TRIM(TRIM(SUBSTRING(pcLinje,37,6)),'"')
        tmpALLA.Filler3           = TRIM(TRIM(SUBSTRING(pcLinje,43,4)),'"')
        tmpALLA.Timestamp         = TRIM(TRIM(SUBSTRING(pcLinje,47,14)),'"')
        tmpALLA.KundEAN           = TRIM(TRIM(SUBSTRING(pcLinje,61,13)),'"')
        NO-ERROR.    
      NEXT.
    END.
    ELSE FIND FIRST tmpALLA WHERE
      tmpALLA.Informationsmangd = 'CPKPRIS' NO-ERROR.

    /* Tomme linjer  */
    IF TRIM(pcLinje) = "" THEN
        NEXT LESERLINJER.
    /* Sjekker transaksjonskoden */
    IF NOT CAN-DO('10,20,30',TRIM(SUBSTRING(pcLinje,1,2))) THEN
      DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil        = piAntFeil + 1
          tt_Error.LinjeNr = iAntLinjer
          tt_Error.Tekst   = "** Feil transaksjonskode linje i CPKPRIS " + string(iAntLinjer) + ". Feil kode. Skal være(10, 20 eller 30). Den er (" + ENTRY(1,pcLinje) + ").".
          tt_Error.Gradering = 15.
        NEXT.
      END.
    /* Er det for få kolonner på linjen */
    IF LENGTH(pcLinje) <> 60 THEN
    DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil        = piAntFeil + 1
          tt_Error.LinjeNr = iAntLinjer
          tt_Error.Tekst   = "** Feil på linje i CPKPRIS " + STRING(iAntLinjer) + ". Feil antall tegn. Skal være(60). Den er (" + STRING(LENGTH(pcLinje)) + ").".
          tt_Error.Gradering = 15.
        NEXT.
    END.
    /* Legger opp linjen */
    CREATE tmpCPKPRIS.
    ASSIGN       
      tmpCPKPRIS.Produktid        = TRIM(TRIM(SUBSTRING(pcLinje,3,13)),'"')
      tmpCPKPRIS.Forpacknings_id  = TRIM(TRIM(SUBSTRING(pcLinje,16,2)),'"')
      tmpCPKPRIS.Artikelnr        = TRIM(TRIM(SUBSTRING(pcLinje,18,13)),'"')
      tmpCPKPRIS.cpkpris          = TRIM(TRIM(SUBSTRING(pcLinje,31,7)),'"')
      tmpCPKPRIS.Datum_kommande   = TRIM(TRIM(SUBSTRING(pcLinje,38,8)),'"')
      tmpCPKPRIS.cpkpris_kommande = TRIM(TRIM(SUBSTRING(pcLinje,46,7)),'"')
      .      
/*
/* TEST */ 
MESSAGE 'FØRST CPKPRIS før oppslag'
      tmpCPKPRIS.Produktid
      tmpCPKPRIS.Artikelnr
      tmpCPKPRIS.cpkpris             
      tmpCPKPRIS.Datum_kommande     
      tmpCPKPRIS.Cpkpris_kommande SKIP
VIEW-AS ALERT-BOX.
FOR EACH ttPrikat:
  DISPLAY 
    ttPriKat.ERPNr tmpCPKPRIS.Produktid ttPriKat.ERPNr = tmpCPKPRIS.Produktid
    WITH FRAME F OVERLAY DOWN.
  DOWN WITH FRAME F.
END.
PAUSE.
HIDE FRAME F.
*/

    FIND FIRST ttPriKat WHERE
      ttPriKat.LevKod = tmpCPKPRIS.Produktid NO-ERROR.
    IF NOT AVAILABLE ttPriKat THEN 
        NEXT.
    ASSIGN
/* 17 */ ttPriKat.VeilPris        = STRING(DECIMAL(tmpCPKPRIS.cpkpris) / 100) 
/* 32 */ ttPriKat.MarkedsPris     = STRING(DECIMAL(tmpCPKPRIS.cpkpris) / 100)
/* 41 */ ttPriKat.AktivFraDato    = STRING(TODAY) 
     NO-ERROR.
     IF ERROR-STATUS:ERROR THEN 
     DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil          = piAntFeil + 1
          tt_Error.LinjeNr   = iAntLinjer
          tt_Error.Tekst     = VareIdent() + "** Feil på linje i CPKPRIS " + STRING(iAntLinjer) + ". Feil ved assign Veilpris " + ttPriKat.VeilPris + ' Markedspris '+ ttPriKat.MarkedsPris + '.'
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
          tt_Error.Gradering = 15.
        NEXT.
     END.      
    
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  ASSIGN
  obOk = TRUE.

  /* Flytter filen til bku katalogen */
  OS-COPY VALUE(pcPkFilNavn)
     VALUE(pcBkuFilNavn).
  /* Sletter filen */
  IF SEARCH(pcBkuFilNavn) <> ? THEN 
    OS-DELETE VALUE(pcPkFilNavn).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



 
&IF DEFINED(EXCLUDE-LesInnPKPRISFil) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnPKPRISFil Procedure
PROCEDURE LesInnPKPRISFil:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr AS INT  NO-UNDO.
  DEF VAR piAntFeil AS INT  NO-UNDO.
  DEF VAR plVareNr  AS DEC  NO-UNDO.
  DEF VAR pcStr     AS CHAR NO-UNDO.
  DEF VAR piLoop    AS INT  NO-UNDO.
  DEFINE VARIABLE pcTekst       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE bErstattModus AS LOG       NO-UNDO.
  DEFINE VARIABLE pcOldChar     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcPkFilNavn   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcBkuFilNavn  AS CHARACTER NO-UNDO.
    
  DEFINE BUFFER bButiker FOR Butiker.
  
  ASSIGN
    obOk = FALSE
    pcPkFilNavn  = cFilNavn
    pcPkFilNavn  = REPLACE(pcPkFilNavn,'ARTIKEL','PKPRIS')
    pcBkuFilNavn = REPLACE(pcPkFilNavn,'PKPRIS','bku\PKPRIS').
 
  IF SEARCH(pcPkFilNavn) = ? THEN
    DO: 
      RETURN.
    END.
 
  ASSIGN
      piLinjeNr  = 1.
      iAntLinjer = 0
      .

  /*INPUT STREAM InnFil FROM VALUE(cFilNavn) CONVERT SOURCE "IBM850" NO-ECHO.*/
  INPUT STREAM InnFil FROM VALUE(pcPkFilNavn) CONVERT SOURCE "ISO8859-1" NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje NO-ERROR.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      pcLinje    = TRIM(pcLinje,'"') /* Preem legger ut med " i første og siste posisjon. */
      .
    /* Den første linjen skal alltid inneholde RIGAL header. */
    /* Men vi skal også tåle at det ikke kommer header.      */
    IF iAntLinjer = 1 THEN 
    DO:
      /* Sjekker transaksjonskoden */
      IF TRIM(SUBSTRING(pcLinje,1,2)) <> '00' THEN
        DO:
          CREATE tt_Error.
          ASSIGN
            piAntFeil        = piAntFeil + 1
            tt_Error.LinjeNr = iAntLinjer
            tt_Error.Tekst   = "** Feil på header linje i PKPRIS " + string(iAntLinjer) + ". Feil kode. Skal være(00). Den er (" + ENTRY(1,pcLinje) + ").".
            tt_Error.Gradering = 14.
          NEXT.
        END.
      CREATE tmpALLA.
      ASSIGN
        tmpALLA.Informationsmangd = TRIM(TRIM(SUBSTRING(pcLinje,3,10)),'"')
        tmpALLA.Filler1           = TRIM(TRIM(SUBSTRING(pcLinje,13,4)),'"')
        tmpALLA.Kundnr            = TRIM(TRIM(SUBSTRING(pcLinje,17,6)),'"')
        tmpALLA.Filler2           = TRIM(TRIM(SUBSTRING(pcLinje,23,6)),'"')
        tmpALLA.Datum             = TRIM(TRIM(SUBSTRING(pcLinje,29,8)),'"')
        tmpALLA.Transaktionsvecka = TRIM(TRIM(SUBSTRING(pcLinje,37,6)),'"')
        tmpALLA.Filler3           = TRIM(TRIM(SUBSTRING(pcLinje,43,4)),'"')
        tmpALLA.Timestamp         = TRIM(TRIM(SUBSTRING(pcLinje,47,14)),'"')
        tmpALLA.KundEAN           = TRIM(TRIM(SUBSTRING(pcLinje,61,13)),'"')
        NO-ERROR. 
      NEXT LESERLINJER.
    END.
    ELSE FIND FIRST tmpALLA WHERE
      tmpALLA.Informationsmangd = 'PKPRIS' NO-ERROR.

    /* Tomme linjer  */
    IF TRIM(pcLinje) = "" THEN
        NEXT LESERLINJER.
    /* Sjekker transaksjonskoden */
    IF NOT CAN-DO('10,20,30',TRIM(SUBSTRING(pcLinje,1,2))) THEN
      DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil        = piAntFeil + 1
          tt_Error.LinjeNr = iAntLinjer
          tt_Error.Tekst   = "** Feil transaksjonskode på linje i PKPRIS " + string(iAntLinjer) + ". Feil kode. Skal være(10, 20 eller 30). Den er (" + ENTRY(1,pcLinje) + ").".
          tt_Error.Gradering = 14.
        NEXT.
      END.

    /* Er det for få kolonner på linjen */
    IF LENGTH(pcLinje) <> 60 THEN
    DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil        = piAntFeil + 1
          tt_Error.LinjeNr = iAntLinjer
          tt_Error.Tekst   = "** Feil på linje i PKPRIS " + STRING(iAntLinjer) + ". Feil antall tegn. Skal være(60). Den er (" + STRING(LENGTH(pcLinje)) + ").".
          tt_Error.Gradering = 14.
        NEXT.
    END.
    /* Legger opp linjen */
    CREATE tmpPKPRIS.
    ASSIGN       
      tmpPKPRIS.Produktid          = TRIM(TRIM(SUBSTRING(pcLinje,3,13)),'"')
      tmpPKPRIS.Forpacknings_id    = TRIM(TRIM(SUBSTRING(pcLinje,16,2)),'"')
      tmpPKPRIS.Artikelnr          = TRIM(TRIM(SUBSTRING(pcLinje,18,13)),'"')
      tmpPKPRIS.pkpris             = TRIM(TRIM(SUBSTRING(pcLinje,31,9)),'"')
      tmpPKPRIS.Datum_kommande     = TRIM(TRIM(SUBSTRING(pcLinje,38,8)),'"')
      tmpPKPRIS.pkpris_kommande    = TRIM(TRIM(SUBSTRING(pcLinje,46,7)),'"')
      .      
/*
MESSAGE 'FØRST PKPRIS før oppslag'
      tmpPKPRIS.Produktid
      tmpPKPRIS.Artikelnr
      tmpPKPRIS.pkpris             
      tmpPKPRIS.Datum_kommande     
      tmpPKPRIS.pkpris_kommande SKIP
VIEW-AS ALERT-BOX.
FOR EACH ttPrikat:
  DISPLAY 
    ttPriKat.ERPNr tmpPKPRIS.Produktid ttPriKat.ERPNr = tmpPKPRIS.Produktid
    WITH FRAME F OVERLAY DOWN.
  DOWN WITH FRAME F.
END.
PAUSE.
HIDE FRAME F.
*/

    FIND FIRST ttPriKat WHERE
      ttPriKat.LevKod = tmpPKPRIS.Produktid NO-ERROR.
    IF NOT AVAILABLE ttPriKat THEN 
        NEXT.
    ASSIGN
/* 13 */ ttPriKat.LevPrisEngros = STRING(DECIMAL(tmpPKPRIS.pkpris) / 100)
/* 41 */ ttPriKat.AktivFraDato  = STRING(TODAY)  
     NO-ERROR.
     IF ERROR-STATUS:ERROR THEN 
     DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil        = piAntFeil + 1
          tt_Error.LinjeNr = iAntLinjer
          tt_Error.Tekst   = "** Feil på linje i PKPRIS " + STRING(iAntLinjer) + ". Feil ved assign av innpris " + ttPriKat.LevPrisEngros + '. ' + pcLinje.
          tt_Error.Gradering = 14.
        NEXT.
     END.      
/*    
MESSAGE 'ETTER oppslag'
/* 17 */ ttPriKat.VeilPris      
/* 32 */ ttPriKat.MarkedsPris  
/* 41 */ ttPriKat.AktivFraDato  
VIEW-AS ALERT-BOX.    
  */
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  ASSIGN
  obOk = TRUE.

  /* Flytter filen til bku katalogen */
  OS-COPY VALUE(pcPkFilNavn)
     VALUE(pcBkuFilNavn).
  /* Sletter filen */
  IF SEARCH(pcBkuFilNavn) <> ? THEN 
    OS-DELETE VALUE(pcPkFilNavn).
  
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-Linjevalidering) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Linjevalidering Procedure 
PROCEDURE Linjevalidering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR cRetValue AS CHAR NO-UNDO.
  DEF VAR cEAN      AS CHAR NO-UNDO.
  DEF VAR cEAN2     AS CHAR NO-UNDO.
  DEF VAR lDec      AS DEC  NO-UNDO.

  ASSIGN 
    cRetValue = 'OK'.
      
  /* Sjekker om det er funksjonskode. */
  IF NOT CAN-DO('10,20,30',TRIM(TRIM(SUBSTRING(pcLinje,1,2)),'"')) THEN 
  DO:
    CREATE tt_Error.
    ASSIGN
      tt_Error.LinjeNr   = iAntLinjer
      tt_Error.Tekst     = "** Ukjent funksjonskode (" + TRIM(TRIM(SUBSTRING(pcLinje,1,2)),'"') + ") på linje: " + string(iAntLinjer) + ": " + pcLinje + "."
      tt_Error.Gradering = 11
      cRetValue          = 'FEIL'.
  END.

  RETURN cRetValue.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterArtikkel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterArtikkel Procedure 
PROCEDURE OppdaterArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  /* Oppdaterer valgte artikler.                                                   */
  /* Oppdateringsrutinen henter informasjonen fra første artikkel i listen. Fra de */
  /* øvrige artiklene hentes kun EAN koden.                                        */
  /* Oppdatering gjøres mot en eksisterende lokal artikkel, eller det opprettes en */
  /* ny lokal artikkel.                                                            */
    RUN OppdaterArtikkel (INPUT  piEkstVPILevNr,
                          INPUT  pcValgteArtikler, 
                          OUTPUT pbOk).

------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piEkstVpiLevNr AS INT  NO-UNDO.
  DEF INPUT  PARAMETER pcListe        AS CHAR NO-UNDO. /* CHR(1) */
  DEF INPUT  PARAMETER piModus        AS INT  NO-UNDO. /* 1-Ny, 2-Koble */
  DEF OUTPUT PARAMETER pbOk           AS LOG  NO-UNDO.

  DEF VAR pcVareNr     AS CHAR NO-UNDO.
  DEF VAR plArtikkelNr AS DEC  NO-UNDO.
  DEF VAR pcColValues  AS CHAR NO-UNDO.
  DEF VAR piLoop       AS INT  NO-UNDO.
  DEF VAR piLoop2      AS INT  NO-UNDO.
  DEF VAR pbSjekk      AS LOG  NO-UNDO.

  AUTOMIMPORT:
  DO:
      RUN SettAutoImport IN h_dvpiartbas (INPUT TRUE).
      RUN OpprettNy    IN h_dvpiartbas (piEkstVpiLevNr, pcListe, OUTPUT plArtikkelNr).
      IF RETURN-VALUE <> "" THEN
      DO:
          MESSAGE RETURN-VALUE
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN.
      END.
      RUN OppdaterInfo IN h_dvpiartbas (piEkstVpiLevNr, pcListe, plArtikkelNr).
  END. /* AUTOMIMPORT */


  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
 
 
&IF DEFINED(EXCLUDE-opprettEkstVPILev) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprettEkstVPILev Procedure
PROCEDURE opprettEkstVPILev:

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER piEkstVPILevNr LIKE EkstVPILev.EkstVPILevNr NO-UNDO.
  
    RUN oppdaterEkstVPILev.p (piEkstVPILevNr, cEDB-System, iNyLevNr, VPIFilHode.Katalog).
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-opprettMapping) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprettMapping Procedure
PROCEDURE opprettMapping:

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes: RUN opprettMapping (cEDB-System,'LevBas','999999',trim(ENTRY(6,pcLinje),'"')). 																	  			       
	------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER cEDB-System AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER cTabell     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER cInterntId  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER cEksterntId AS CHARACTER NO-UNDO.

  IF NOT CAN-FIND(ImpKonv WHERE
                  ImpKonv.EDB-System = cEDB-System AND
                  ImpKonv.Tabell     = cTabell AND
                  ImpKonv.InterntId  = cInterntId AND
                  ImpKonv.EksterntId = cEksterntId) THEN 
  DO:
    CREATE ImpKonv.
    ASSIGN
      ImpKonv.EDB-System = cEDB-System
      ImpKonv.Tabell     = cTabell
      ImpKonv.InterntId  = cInterntId
      ImpKonv.EksterntId = cEksterntId. 
  END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-opprettSalgsenhet) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprettSalgsenhet Procedure
PROCEDURE opprettSalgsenhet:

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER cSalgsEnhTekst AS CHARACTER NO-UNDO.

  DEF VAR piLoop       AS INT NO-UNDO.
  DEF VAR piSalgsEnhId AS INT NO-UNDO.

  IF NOT CAN-FIND(FIRST SalgsEnhet WHERE
                        SalgsEnhet.SalgsEnhTekst = TRIM(TRIM(cSalgsEnhTekst,'"'))) THEN
    DO:
      FIND LAST SalgsEnhet NO-LOCK NO-ERROR.
      IF AVAILABLE SalgsEnhet 
        THEN piSalgsEnhId = SalgsEnhet.SalgsEnhId + 1.
      ELSE piSalgsEnhId = 1.
      CREATE SalgsEnhet.
      ASSIGN
        SalgsEnhet.SalgsEnhId    = piSalgsEnhId
        SalgsEnhet.SalgsEnhTekst = TRIM(TRIM(cSalgsEnhTekst,'"')).
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
  DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
  
  ASSIGN
      iTotAntLinjer = -1 /* Första linjen är en header */
      .
  INPUT STREAM InnFil FROM VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) NO-ECHO.
  IMPORT STREAM InnFil UNFORMATTED cLinje.
  
  cLinje = TRIM(cLinje,'"').
  cTekst = SUBSTRING(cLinje,1,9).
  
  IF NOT CAN-DO(cOKVersion,cTekst) THEN DO:
      INPUT STREAM InnFil CLOSE.
      RETURN "FEIL".
  END.
  ASSIGN cRigalversion = ENTRY(1,cLinje).
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

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-FixChk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FixChk Procedure 
FUNCTION FixChk RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
      DEF VAR iCount1 AS INTE NO-UNDO.
      DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
      DEF VAR iSum AS INTE NO-UNDO.
        DO iCount1 = LENGTH(cKode) TO 1 BY -1:  
            ASSIGN iMulti = IF iMulti = 1 THEN 3 ELSE 1
                   iSum = iSum + INT(SUBSTR(cKode,iCount1,1)) * iMulti.
        END.
        RETURN cKode + string((10 - iSum MODULO 10) MODULO 10).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&IF DEFINED(EXCLUDE-getEAN) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEAN Procedure
FUNCTION getEAN RETURNS CHARACTER 
	(  ):

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/

		DEFINE VARIABLE result AS CHARACTER NO-UNDO.

        RUN hentEAN.p (13,2,OUTPUT result). 

		RETURN result.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VareIdent) = 0 &THEN
        
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION VareIdent Procedure
FUNCTION VareIdent RETURNS CHARACTER 
    (  ):
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/

        DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

        IF AVAILABLE tmpARTIKEL THEN 
        cTekst = " EkstVpiLev: " + STRING(VPIFilHode.EkstVPILevNr,"zzzzzz9") + " Linje: " + STRING(ttPriKat.LinjeNr,">>zzzzzz9") + ' '  
                 + (IF LENGTH(TRIM(TRIM(SUBSTRING(pcLinje,74,13)),'"')) < 13 THEN FILL(' ',13 - LENGTH(TRIM(TRIM(SUBSTRING(pcLinje,74,13)),'"'))) ELSE '') + TRIM(TRIM(SUBSTRING(pcLinje,74,13)),'"') + ' ' 
                 + (IF LENGTH(TRIM(TRIM(SUBSTRING(pcLinje,18,13)),'"')) < 10 THEN FILL(' ',10 - LENGTH(TRIM(TRIM(SUBSTRING(pcLinje,18,13)),'"'))) ELSE '') + TRIM(TRIM(SUBSTRING(pcLinje,18,13)),'"') + ' ' 
                 + (IF LENGTH(TRIM(TRIM(SUBSTRING(pcLinje, 3,13)),'"')) < 10 THEN FILL(' ',10 - LENGTH(TRIM(TRIM(SUBSTRING(pcLinje, 3,13)),'"'))) ELSE '') + TRIM(TRIM(SUBSTRING(pcLinje, 3,13)),'"') + ' ' 
                 + (IF LENGTH(TRIM(TRIM(SUBSTRING(pcLinje,88,22)),'"')) < 30 THEN FILL(' ',30 - LENGTH(TRIM(TRIM(SUBSTRING(pcLinje,88,22)),'"'))) ELSE '') + TRIM(TRIM(SUBSTRING(pcLinje,88,22)),'"') + ': ' 
                 . 
        RETURN cTekst.
END FUNCTION.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
