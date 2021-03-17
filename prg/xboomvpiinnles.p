&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xboomvpiinnles.p
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
DEF VAR pcFilNavn     AS CHAR NO-UNDO.
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

/* Org. tabell. Har ikke Color Code. */
DEFINE TEMP-TABLE tmpOrgVPIArtikler
  FIELD cVerifikationsnr AS CHARACTER FORMAT "x(10)"
  FIELD cLevNr AS CHARACTER FORMAT "x(10)"
  FIELD cBeskrivning AS CHARACTER FORMAT "x(40)"
  FIELD cBekreftad_leveransperiod AS CHARACTER FORMAT "x(10)"
  FIELD cVarugruppskod AS CHARACTER FORMAT "x(10)"
  FIELD cLeveransdatum AS CHARACTER FORMAT "x(10)"
  FIELD cEnhetskod AS CHARACTER FORMAT "x(5)"
  FIELD cAntal AS CHARACTER FORMAT "x(8)"
  FIELD cA-pris AS CHARACTER FORMAT "x(8)"
  FIELD cBelopp AS CHARACTER FORMAT "x(8)"
  FIELD cFOB AS CHARACTER FORMAT "x(8)"
  FIELD cLANDED AS CHARACTER FORMAT "x(8)"
  FIELD cPris AS CHARACTER FORMAT "x(8)"
  FIELD cValuta AS CHARACTER FORMAT "x(8)"
  FIELD cRekommenderat_forsaljningspris AS CHARACTER FORMAT "x(8)"
  FIELD cNorska_inpriser AS CHARACTER FORMAT "x(8)"
  FIELD cNorska_utpriser AS CHARACTER FORMAT "x(8)"
  INDEX tmpArtikler cLevNr
  .  

/* Har et entry for hver color code. */
DEFINE TEMP-TABLE tmpVPIEAN
  FIELD cVerifikationsnr AS CHARACTER FORMAT "x(10)"
  FIELD cLevNr AS CHARACTER FORMAT "x(10)"
  FIELD cBeskrivning AS CHARACTER FORMAT "x(40)"
  FIELD cBekreftad_leveransperiod AS CHARACTER FORMAT "x(10)"
  FIELD cVarugruppskod AS CHARACTER FORMAT "x(10)"
  FIELD cLeveransdatum AS CHARACTER FORMAT "x(10)"
  FIELD cEnhetskod AS CHARACTER FORMAT "x(5)"
  FIELD cAntal AS CHARACTER FORMAT "x(8)"
  FIELD cA-pris AS CHARACTER FORMAT "x(8)"
  FIELD cBelopp AS CHARACTER FORMAT "x(8)"
  FIELD cFOB AS CHARACTER FORMAT "x(8)"
  FIELD cLANDED AS CHARACTER FORMAT "x(8)"
  FIELD cPris AS CHARACTER FORMAT "x(8)"
  FIELD cValuta AS CHARACTER FORMAT "x(8)"
  FIELD cRekommenderat_forsaljningspris AS CHARACTER FORMAT "x(8)"
  FIELD cNorska_inpriser AS CHARACTER FORMAT "x(8)"
  FIELD cNorska_utpriser AS CHARACTER FORMAT "x(8)"
  FIELD cVaremerke AS CHARACTER FORMAT "x(8)"
  FIELD cCollor_code  AS CHARACTER FORMAT "x(20)"
  FIELD cCollor_name  AS CHARACTER FORMAT "x(30)"
  FIELD cSIZE  AS CHARACTER FORMAT "x(10)" 
  FIELD cLENGHT  AS CHARACTER FORMAT "x(10)" 
  FIELD cEancode  AS CHARACTER FORMAT "x(15)"
  FIELD cSasong AS CHARACTER FORMAT "x(10)"
  INDEX tmpArtikler cLevNr cCollor_code
  .

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr   AS INT
  FIELD Tekst     AS CHAR
  FIELD Gradering AS INT 
  FIELD ErrNr     AS INTEGER
  FIELD ButikkNr  AS INTEGER
  INDEX Feil ErrNr
  INDEX Linje LinjeNr.
     
DEFINE BUFFER clButiker FOR Butiker.
DEFINE BUFFER bVPIArtBas FOR VPIArtBas.

{ttpricat.i &NEW=" " &SHARED=" "}
DEF BUFFER bttPrikat FOR ttPrikat.

{windows.i}
 
/* Eksportkatalog for VPI filer. */
{syspara.i 50 24 1 cbutKatalog} 
IF cbutKatalog = '' THEN cbutKatalog = 'c:\home\lindbak\sendes'. 
 
{syspara.i 50 15 2 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bAvbrytVedFeil = TRUE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */
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
    RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: ** Ukjent VPIFilHode post.').
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.

ASSIGN cFilPrefix = "BOARTLog_".

{syspara.i 1 1 59 cLoggKatalog}
IF cLoggKatalog <> '' THEN DO:
    /* Sikrer at katalog finnes. */
    OS-CREATE-DIR VALUE(RIGHT-TRIM(cLoggKatalog,'\')).    
    cLoggKatalog = RIGHT-TRIM(cLoggKatalog,'\') + '\'.
END.

/* Henter ID for konvertering */
FIND EkstVPILev NO-LOCK WHERE
    EkstVPILev.EkstVPILevNr = VPIFilHode.EkstVPILevNr NO-ERROR.
IF NOT AVAILABLE EkstVPILev THEN
DO:
    RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: ** Ingen ekstern VPI leverandør tilgjengelig.. Id: ' + STRING(VPIFilHode.EkstVPILevNr) + ".").
    MESSAGE "Ingen ekstern VPI leverandør tilgjengelig." SKIP
            "Id: " + STRING(VPIFilHode.EkstVPILevNr) + "."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.
IF EkstVPILev.KortNavn = '' THEN
DO:
    RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: ** Det er ikke satt opp KortNavn på ekstern VPI leverandør... Id: ' + STRING(VPIFilHode.EkstVPILevNr) + ".").
    MESSAGE "Det er ikke satt opp KortNavn på ekstern VPI leverandør." SKIP
            "Dette må satt for at konvertering av varegrupper skal kunne skje." SKIP
            "Id: " + STRING(VPIFilHode.EkstVPILevNr) + "."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

IF AVAILABLE EkstVPILev
  THEN iLevNr = EkstVPILev.LevNr.
ELSE iLevNr = 100.

ASSIGN
    cFilNavn  = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn
    cErrorFil = cLoggKatalog /*OS-GETENV('TMP') + '\'*/ 
               + cFilPrefix 
               + REPLACE(STRING(TODAY),'/','') + '_' 
               + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '_' 
               + VPIFilHode.FilNavn
               + ".Txt".

{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE clButiker THEN 
DO:
  RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: ** Ukjent butiknr satt for sentrallager... CL: ' + STRING(iCL) + ".").
  MESSAGE 'Ukjent butiknr satt for sentrallager.'
  VIEW-AS ALERT-BOX.
  RETURN.
END.

ASSIGN
    ctmpKatalog = SESSION:TEMP-DIRECTORY
    cVPIFil     = "VPI" + STRING(iLevNr) + "-" + STRING(TIME) + ".csv"
    cVPIUtFil   = cVPIFil
    .

/* Skal leverandørnr overstyres fra oppsett på VPILev? */
{syspara.i 50 15 20 iOverstyrLevNr INT}
ASSIGN
    cEDB-System   = EkstVPILev.EDB-System
    iEkstVPILevNr = EkstVPILev.EkstVPILevNr.

/* Leser inn filen i BOART. */
RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: Start LesInnBOART.').
RUN LesInnBOART.
IF RETURN-VALUE = '' THEN 
DO:
    RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: LesInnBOEAN.').
    RUN LesInnBOEAN. 
    IF CAN-FIND(FIRST tt_Error) THEN
      RUN ErrorLogg.
    
    IF CAN-FIND(FIRST tmpVPIEAN) THEN
    DO: 
      RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: opprettPricat.').
      RUN opprettPricat.
    END.
    
    IF CAN-FIND(FIRST ttPriKat) THEN
    DO: 
      RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: EksportVPIFil.').
      RUN EksportVPIFil.
    END.
    RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: Slutt LesInnBOART.').
END.
ELSE DO: 
  RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: Feilmelding: ' + RETURN-VALUE + '.').
  bErrFil = TRUE.
END.
    
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
    RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: Oppdatert VPIFilhode: ' + STRING(VPIFilHode.VPIFilStatus) + '.').
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

IF bErrFil THEN 
  RETURN 'AVBRYT'.
ELSE 
  RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
DEF VAR piAntLinjer  AS INTEGER NO-UNDO.
DEF VAR plArtikkelNr AS DEC NO-UNDO.
DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.

DEF BUFFER bVPIFilHode FOR VPIFilHode.

IF iAntLinjer > 0 THEN
DO:
    RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: EksportVPIFil: Utlegg til fil' + ctmpKatalog + cVPIUtFil).     
    
    OUTPUT STREAM UtVPI TO VALUE(ctmpKatalog + cVPIUtFil) NO-ECHO.                    
    EKSPORTFIL:                    
    FOR EACH ttPrikat 
        BREAK BY ttPriKat.ButikkNr
              BY ttPriKat.LevModellNr
              BY ttPriKat.VareTekst     
              BY ttPriKat.FargeTekst    
              BY ttPriKat.SeqNrStr           
              BY ttPriKat.MarkedsPris:
        RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: EksportVPIFil: ' + ttPriKat.EANnr + ' '+ ttPriKat.VareTekst).     
                      
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
        /*101 */ ";"
        /*102 */ 
        SKIP      
        .

        
        /* Behandler filen */
        IF LAST-OF(ttPriKat.ButikkNr) THEN 
        DO:
          
        END.
        
    END. /* EKSPORTFIL */
    OUTPUT STREAM UtVpi CLOSE.
    
    /* Flytter filen til ankommet katalogen */
    OS-COPY VALUE(RIGHT-TRIM(ctmpKatalog,'\') + "~\" + cVPIUtFil)
            VALUE(VPIFilHode.Katalog + "~\" + cVPIUtFil).
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
        RUN xPRSPricatInnles.p (plFilId,?,OUTPUT piAntLinjer).
        IF piAntLinjer > 0 THEN
        DO: 
            IF NOT VALID-HANDLE(h_dvpiartbas) THEN
            DO:
                RUN dvpiartbas.w PERSISTENT SET h_dvpiartbas.
                RUN SettAutoImport IN h_dvpiartbas (INPUT TRUE).
                IF NOT VALID-HANDLE(h_PrisKo) THEN
                RUN prisko.p PERSISTENT SET h_PrisKo.
            END.
            RUN xPRSPricatUtpakk.p (plFilId).
            RUN oppdaterArtBas.
        END.
    END. /* LES-INN-OPPDATER */

    RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: EksportVPIFil: Flyttet til ankommet' + VPIFilHode.Katalog + "~\" + cVPIUtFil).     
        
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
    IF CAN-FIND(FIRST tt_Error) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error 
            BY tt_Error.LinjeNr:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.
  
  OUTPUT CLOSE.
  
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
        RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: Tar bort pcBackupFil: ' + STRING(pcBkuFil) + '.').
        /* Filen tas bort fra katalogen. */
        IF SEARCH(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) <> ? THEN
            OS-DELETE VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn).
    END.

    
    ASSIGN
      pcBkuFil = VPIFilHode.Katalog + "~\bku" + "\" + 
                 ENTRY(NUM-ENTRIES(pcFilNavn,'\'),pcFilNavn,'\').

    /* Flytter filen til backup katalog. */
    RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: Kopierer fil: ' + pcFilNavn + ' / ' + pcBkuFil + '.').
    OS-COPY value(pcFilNavn) 
            value(pcBkuFil).
    /* Renser bort fil */
    IF SEARCH(pcBkuFil) <> ? THEN
    DO:
        /* Filen tas bort fra katalogen. */
        IF SEARCH(pcBkuFil) <> ? THEN
            OS-DELETE VALUE(pcFilNavn).
    END.
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-LesInnBOART) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnBOART Procedure 
PROCEDURE LesInnBOART :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
/* Org. tabell. Har ikke Color Code. */
------------------------------------------------------------------------------*/
  DEFINE VARIABLE piAntFeil AS INTEGER   NO-UNDO.
  DEFINE VARIABLE pcTekst   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcReturn  AS CHARACTER NO-UNDO.
  
  /* Tømmer feillogg. */
  FOR EACH tt_Error:
    DELETE tt_Error.
  END.
  
  /* Tømmer pricat */
  FOR EACH ttPrikat:
      DELETE ttPrikat.
  END.
  
  /* Tømmer artikkelfil */
  FOR EACH tmpOrgVPIArtikler:
      DELETE tmpOrgVPIArtikler.
  END.
  
  RUN TellOppLinjer.
  IF RETURN-VALUE = "FEIL" THEN DO:
      RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: LesInnBOART: Feil: ** Feil på linje 1. IKKE gyldig Artikkelfil header.').
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
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.

    ASSIGN
      iAntLinjer = iAntLinjer + 1
      pcLinje    = REPLACE(pcLinje,'"',' ')
      .
    RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: LesInnBOART: Linje: ' + pcLinje).
      
    /* Den første linjen skal alltid inneholde overskrifter. */
    /* I varenr feltet får vi alltid bare tall.              */
    /* Men vi skal også tåle at det ikke kommer header.      */
    ASSIGN lDec = DEC(ENTRY(2,pcLinje,';')) NO-ERROR.
    IF iAntLinjer = 1 AND ERROR-STATUS:ERROR THEN 
      NEXT.

    /* Tomme linjer  */
    IF TRIM(pcLinje) = "" THEN
      DO:
        CREATE tt_Error.
        RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: LesInnBOART: * Blank line på linje ' + string(iAntLinjer)).
        ASSIGN
          piAntFeil          = piAntFeil + 1
          tt_Error.LinjeNr   = iAntLinjer
          tt_Error.Tekst     = "* Blank line på linje " + string(iAntLinjer) + "." 
          tt_Error.ButikkNr  = iCL
          tt_Error.Gradering = 2.
        NEXT.
      END.
      
    /* Er det for få kolonner på linjen */
    IF NUM-ENTRIES(pcLinje,';') < 17 THEN
    DO:
        CREATE tt_Error.
        RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: LesInnBOART: * ' + 'Feil antall kolonner. Skal være minst 17. Det er (' + STRING(NUM-ENTRIES(pcLinje,';')) + ").").
        ASSIGN
          piAntFeil          = piAntFeil + 1
          tt_Error.LinjeNr   = iAntLinjer
          tt_Error.Tekst     = "** Feil på linje " + STRING(iAntLinjer) + ". Feil antall kolonner. Skal være minst 17. Det er (" + STRING(NUM-ENTRIES(pcLinje,';')) + ")."
          tt_Error.ButikkNr  = iCL
          tt_Error.Gradering = 2.
        NEXT.
    END.

    /* Sjekker om den finnes fra før. */
    IF NOT CAN-FIND(FIRST tmpOrgVPIArtikler WHERE 
                    tmpOrgVPIArtikler.cLevNr = TRIM(ENTRY(2,pcLinje,';'))) THEN 
    DO: 
      CREATE tmpOrgVPIArtikler.
      ASSIGN 
      tmpOrgVPIArtikler.cVerifikationsnr          = TRIM(ENTRY(1,pcLinje,';'))
      tmpOrgVPIArtikler.cLevNr                    = TRIM(ENTRY(2,pcLinje,';'))
      tmpOrgVPIArtikler.cBeskrivning              = TRIM(ENTRY(3,pcLinje,';'))
      tmpOrgVPIArtikler.cBekreftad_leveransperiod = TRIM(ENTRY(4,pcLinje,';'))
      tmpOrgVPIArtikler.cVarugruppskod            = TRIM(ENTRY(5,pcLinje,';'))
      tmpOrgVPIArtikler.cLeveransdatum            = TRIM(ENTRY(6,pcLinje,';'))
      tmpOrgVPIArtikler.cEnhetskod                = TRIM(ENTRY(7,pcLinje,';'))
      tmpOrgVPIArtikler.cAntal                    = TRIM(ENTRY(8,pcLinje,';'))
      tmpOrgVPIArtikler.cA-pris                   = TRIM(ENTRY(9,pcLinje,';'))
      tmpOrgVPIArtikler.cBelopp                   = TRIM(ENTRY(10,pcLinje,';'))
      tmpOrgVPIArtikler.cFOB                      = TRIM(ENTRY(11,pcLinje,';'))
      tmpOrgVPIArtikler.cLANDED                   = TRIM(ENTRY(12,pcLinje,';'))
      tmpOrgVPIArtikler.cPris                     = TRIM(ENTRY(13,pcLinje,';'))
      tmpOrgVPIArtikler.cValuta                   = TRIM(ENTRY(14,pcLinje,';'))
      tmpOrgVPIArtikler.cRekommenderat_forsaljningspris = TRIM(ENTRY(15,pcLinje,';'))
      tmpOrgVPIArtikler.cNorska_inpriser          = TRIM(ENTRY(16,pcLinje,';'))
      tmpOrgVPIArtikler.cNorska_utpriser          =TRIM(ENTRY(17,pcLinje,';'))
      .
      RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: LesInnBOART: Opprettet tmpOrgVPIArtikler ' + pcLinje).
      
    END.
    ELSE DO:
      RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: LesInnBOART: IKKE Opprettet tmpOrgVPIArtikler. Linje finnes fra før ' + pcLinje).        
    END.
    
    IF iAntLinjer MODULO 10 = 0 THEN 
       STATUS DEFAULT "Leser linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.
  
  RETURN.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
 
&IF DEFINED(EXCLUDE-LesInnBOEAN) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnBOEAN Procedure
PROCEDURE LesInnBOEAN:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  													
			  				  
	------------------------------------------------------------------------------*/
  DEFINE VARIABLE piAntFeil AS INTEGER   NO-UNDO.
  DEFINE VARIABLE pcTekst   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcReturn  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iAnt      AS INTEGER NO-UNDO.
  
  /* Tømmer artikkelfil */
  FOR EACH tmpVPIEAN:
      DELETE tmpVPIEAN.
  END.
  
  ASSIGN pcFilNavn = REPLACE(cFilNavn,'ART','EAN').
  
  RUN TellOppLinjer.
  IF RETURN-VALUE = "FEIL" THEN DO:
      CREATE tt_Error.
      RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: LesInnBOEAN: ** Feil på linje 1. IKKE gyldig Artikkelfil header').
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
  INPUT STREAM InnFil FROM VALUE(pcFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.

    ASSIGN
      iAntLinjer = iAntLinjer + 1
      pcLinje    = REPLACE(pcLinje,'"',' ')
      .
      
    RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: LesInnBOEAN: Linje: ' + pcLinje).
      
    /* Den første linjen skal alltid inneholde overskrifter. */    
    /* I varenr feltet får vi alltid bare tall.              */
    /* Men vi skal også tåle at det ikke kommer header.      */
    ASSIGN lDec = DEC(ENTRY(1,pcLinje,';')) NO-ERROR.
    IF iAntLinjer = 1 AND ERROR-STATUS:ERROR THEN 
      NEXT.

    /* Tomme linjer  */
    IF TRIM(pcLinje) = "" THEN
      DO:
        CREATE tt_Error.
        RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: LesInnBOEAN: * Blank line på linje  ' + string(iAntLinjer)).
        ASSIGN
          piAntFeil          = piAntFeil + 1
          tt_Error.LinjeNr   = iAntLinjer
          tt_Error.Tekst     = "* Blank line på linje " + string(iAntLinjer) + "." 
          tt_Error.ButikkNr  = iCL
          tt_Error.Gradering = 2.
        NEXT.
      END.
      
    /* Er det for få kolonner på linjen */
    IF NUM-ENTRIES(pcLinje,';') < 6 THEN
    DO:
        CREATE tt_Error.
        RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: LesInnBOEAN: * Feil antall kolonner. Skal være minst 6. Det er  ' + STRING(NUM-ENTRIES(pcLinje,';'))).
        ASSIGN
          piAntFeil          = piAntFeil + 1
          tt_Error.LinjeNr   = iAntLinjer
          tt_Error.Tekst     = "** Feil på linje " + STRING(iAntLinjer) + ". Feil antall kolonner. Skal være minst 6. Det er (" + STRING(NUM-ENTRIES(pcLinje,';')) + ")."
          tt_Error.ButikkNr  = iCL
          tt_Error.Gradering = 2.
        NEXT.
    END.

    /* Sjekker om den finnes fra før. Hvis ikke opprettes linjen. */
    IF CAN-FIND(FIRST tmpOrgVPIArtikler WHERE 
                  tmpOrgVPIArtikler.cLevNr = TRIM(ENTRY(1,pcLinje,';'))) AND 
       NOT CAN-FIND(FIRST tmpVPIEAN WHERE 
                    tmpVPIEAN.cLevNr       = TRIM(ENTRY(1,pcLinje,';')) AND
                    tmpVPIEAN.cCollor_code = TRIM(ENTRY(2,pcLinje,';')) AND 
                    tmpVPIEAN.cEAN         = TRIM(ENTRY(6,pcLinje,';'))
                    ) THEN 
    DO: 
      FIND FIRST tmpOrgVPIArtikler WHERE 
                 tmpOrgVPIArtikler.cLevNr = TRIM(ENTRY(1,pcLinje,';')) NO-ERROR. 
      CREATE tmpVPIEAN.
      BUFFER-COPY tmpOrgVPIArtikler TO tmpVPIEAN
          ASSIGN
          tmpVPIEAN.cCollor_code = TRIM(ENTRY(2,pcLinje,';'))
          tmpVPIEAN.cCollor_name = TRIM(ENTRY(3,pcLinje,';'))
          tmpVPIEAN.cSIZE        = TRIM(ENTRY(4,pcLinje,';')) 
          tmpVPIEAN.cLENGHT      = TRIM(ENTRY(5,pcLinje,';')) 
          tmpVPIEAN.cEancode     = TRIM(ENTRY(6,pcLinje,';'))
          tmpVPIEAN.cSasong      = SUBSTRING(REPLACE(TRIM(ENTRY(1,pcLinje,';'),'"'),'USC','018'),1,2)
          tmpVPIEAN.cVaremerke   = SUBSTRING(REPLACE(TRIM(ENTRY(1,pcLinje,';'),'"'),'USC','018'),3,1)
          .
      RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: LesInnBOEAN: Oppretter : tmpVPIEAN ' + pcLinje).
    END.
   
    IF NOT CAN-FIND(Varemerke WHERE 
                    Varemerke.VmId = INT(SUBSTRING(REPLACE(TRIM(ENTRY(1,pcLinje,';'),'"'),'USC','018'),3,1))) THEN 
    DO TRANSACTION:
        CREATE Varemerke.
        ASSIGN
          Varemerke.VmID = INT(SUBSTRING(REPLACE(TRIM(ENTRY(1,pcLinje,';'),'"'),'USC','018'),3,1))
          Varemerke.Beskrivelse = (IF VareMerke.VmId = 5 
                                    THEN 'Dame' 
                                  ELSE IF Varemerke.VmId = 6 
                                    THEN 'Herre' 
                                  ELSE IF VareMerke.VmId = 8 
                                    THEN 'Acc' ELSE 
                                  STRING(Varemerke.VmId)) 
          Varemerke.KortNavn    = TRIM(ENTRY(3,pcLinje,';')).
        IF AVAILABLE Varemerke THEN RELEASE Varemerke.
    END. /* TRANSACTION */
    
    IF NOT CAN-FIND(Farg WHERE 
                    Farg.Farg = INT(TRIM(ENTRY(2,pcLinje,';')))) THEN 
    DO TRANSACTION:
        CREATE Farg.
        ASSIGN
          Farg.Farg     = INT(TRIM(ENTRY(2,pcLinje,';')))
          Farg.FarBeskr = TRIM(ENTRY(3,pcLinje,';')).
        IF AVAILABLE Farg THEN RELEASE Farg.
    END. /* TRANSACTION */

    IF NOT CAN-FIND(Sasong WHERE
                    Sasong.Sasong = INT(SUBSTRING(REPLACE(TRIM(ENTRY(1,pcLinje,';'),'"'),'USC','018'),1,2))) THEN
    DO TRANSACTION:
        CREATE Sasong.
        ASSIGN
            Sasong.Sasong   = INT(SUBSTRING(REPLACE(TRIM(ENTRY(1,pcLinje,';'),'"'),'USC','018'),1,2))
            Sasong.SasBeskr = "Sesong " + SUBSTRING(REPLACE(TRIM(ENTRY(1,pcLinje,';'),'"'),'USC','018'),1,2)
            .
        IF AVAILABLE Sasong THEN RELEASE Sasong.
    END. /* TRANSACTION */
    
    
    FIND LAST HuvGr NO-LOCK NO-ERROR.
    IF AVAILABLE HuvGr 
      THEN iAnt = HuvGr.Hg + 1.
    ELSE iAnt = 1.
  
    IF AVAILABLE tmpVPIEan AND 
       NOT CAN-FIND(FIRST HuvGr WHERE 
                  HuvGr.HgBeskr = tmpVPIEAN.cVarugruppskod) THEN
    DO TRANSACTION:
      CREATE HuvGr.
      ASSIGN
          HuvGr.Hg      = iant
          HuvGr.HgBeskr = tmpVPIEAN.cVarugruppskod
          HuvGr.AvdelingNr = 1
          .
      IF AVAILABLE HuvGr THEN RELEASE HuvGr.
    END. /* TRANSACTION */
  
    IF AVAILABLE tmpVPIEAN AND 
       NOT CAN-FIND(FIRST VarGr WHERE 
                    VarGr.Vg = INT(SUBSTRING(REPLACE(TRIM(ENTRY(1,pcLinje,';')),'USC','018'),4,3))) THEN
    DO TRANSACTION:
        CREATE VarGr.
        ASSIGN
            VarGr.Vg        = INT(SUBSTRING(REPLACE(TRIM(ENTRY(1,pcLinje,';')),'USC','018'),4,3))
            VarGr.VgBeskr   = "** Automatisk opprettet"
            VarGr.Hg        = iAnt
            VarGr.Kost_Proc = 65
            VarGr.MomsKod   = 1
            .
        CREATE vgkat.
        ASSIGN VgKat.KatNr = 1
               VgKat.Vg    = VarGr.Vg
               VgKat.VgKat = 1 NO-ERROR.
            
        IF AVAILABLE VarGr THEN RELEASE VarGr.
    END. /* TRANSACTION */
    IF AVAILABLE tmpVPIEAN THEN 
        ASSIGN tmpVPIEAN.cVarugruppskod = SUBSTRING(REPLACE(TRIM(ENTRY(1,pcLinje,';')),'USC','018'),4,3).
    
    IF iAntLinjer MODULO 10 = 0 THEN 
       STATUS DEFAULT "Leser linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.
  RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: LesInnBOEAN: Ferdig.').
  
  RETURN.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
 
&IF DEFINED(EXCLUDE-oppdaterArtBas) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdaterArtBas Procedure
PROCEDURE oppdaterArtBas:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    DEFINE VARIABLE plArtikkelNr AS DECIMAL NO-UNDO.
    
        FOR EACH VPIArtBas NO-LOCK WHERE
            VPIArtBas.EkstVPILevNr = VPIFilHode.EkstVPILevNr AND
            VPIArtBas.VPIDato      = TODAY AND
            VPIArtBas.BehStatus    = 1 TRANSACTION:
            
            RUN OpprettNy    IN h_dvpiartbas (VPIFilHode.EkstVPILevNr, VPIArtBas.VareNr, OUTPUT plArtikkelNr).
            RUN OppdaterInfo IN h_dvpiartbas (VPIFilHode.EkstVPILevNr, VPIArtBas.VareNr, plArtikkelNr).
            RUN OppdaterPris IN h_dvpiartbas (VPIFilHode.EkstVPILevNr, VPIArtBas.VareNr, plArtikkelNr).
            
            FIND ArtBas NO-LOCK WHERE
                ArtBas.ArtikkelNr = DEC(VPIArtBas.ArtikkelNr) NO-ERROR.
            IF AVAILABLE ArtBas THEN 
            PRISBLOKK:
            DO:
              /* Legger ut eventuelle interngenererte strekkoder */
              FOR EACH VPIStrekkode OF VPIArtBas NO-LOCK:
                IF NOT CAN-FIND(Strekkode WHERE
                                  Strekkode.Kode = VPIStrekkode.Kode) THEN 
                  DO:
                    CREATE Strekkode.
                    BUFFER-COPY VPIStrekkode 
                                TO Strekkode
                                ASSIGN
                                  Strekkode.ArtikkelNr = dec(VPIStrekkode.VareNr). 
                  
                    /* Slike artikler SKAL sendes til kassen. */
                    FIND ELogg WHERE 
                         ELogg.TabellNavn     = "ArtBas" AND
                         ELogg.EksterntSystem = "POS"    AND
                         ELogg.Verdier        = STRING(ArtBas.ArtikkelNr) NO-ERROR.
                    IF NOT AVAIL Elogg THEN DO:
                        CREATE Elogg.
                        ASSIGN ELogg.TabellNavn     = "ArtBas"
                               ELogg.EksterntSystem = "POS"   
                               ELogg.Verdier        = STRING(ArtBas.ArtikkelNr).
                    END.
                    ASSIGN ELogg.EndringsType = 1 
                           ELogg.Behandlet    = FALSE.
                    IF AVAILABLE ELogg THEN RELEASE ELogg.                    
                  END.                   
              END.
              FIND FIRST VPIArtPris OF VPIArtBas NO-LOCK NO-ERROR.
              IF NOT AVAILABLE VPIArtPris THEN
                  LEAVE PRISBLOKK.
              FIND VarGr OF VPIArtBas NO-LOCK NO-ERROR.

              FIND PrisKo EXCLUSIVE-LOCK WHERE
                  PrisKo.ArtikkelNr    = ArtBas.ArtikkelNr AND
                  PrisKo.ProfilNr      = VPIArtPris.ProfilNr AND
                  PrisKo.AktiveresDato = TODAY AND
                  PrisKo.AktiveresTid  = 0 AND
                  PrisKo.Tilbud        = FALSE NO-ERROR.
              IF NOT AVAILABLE PrisKo THEN
              DO:
                  CREATE PrisKo.
                  ASSIGN
                      PrisKo.ArtikkelNr    = ArtBas.ArtikkelNr
                      PrisKo.ProfilNr      = VPIArtPris.ProfilNr 
                      PrisKo.AktiveresDato = TODAY 
                      PrisKo.AktiveresTid  = 0 
                      PrisKo.Tilbud        = FALSE
                      .
              END.

              ASSIGN
                  Prisko.Pris           = VPIArtPris.Pris[1]
                  Prisko.Mva%           = VPIArtPris.Mva%[1]
                  Prisko.MvaKr          = VPIArtPris.MvaKr[1]
                  Prisko.VareKost       = VPIArtPris.VareKost[1]
                  Prisko.DB%            = VPIArtPris.DB%[1]
                  Prisko.DBKr           = VPIArtPris.DBKr[1]
                  Prisko.ValPris        = VPIArtPris.ValPris[1]
                  Prisko.InnkjopsPris   = VPIArtPris.InnkjopsPris[1]
                  Prisko.Rab1%          = VPIArtPris.Rab1%[1]
                  Prisko.Rab1Kr         = VPIArtPris.Rab1Kr[1]
                  Prisko.EuroPris       = VPIArtPris.EuroPris[1]

                  Prisko.LevNr          = ArtBas.LevNr
                  Prisko.Rab2Kr         = 0
                  Prisko.Rab2%          = 0
                  Prisko.Frakt          = 0
                  Prisko.Frakt%         = 0
                  Prisko.DivKostKr      = 0
                  Prisko.DivKost%       = 0
                  Prisko.Rab3Kr         = 0
                  Prisko.Rab3%          = 0
                  Prisko.EuroManuel     = FALSE
                  Prisko.Timestyrt      = FALSE
                  Prisko.Aktivert       = FALSE
                  Prisko.Type           = 1
                  Prisko.EndringsType   = 0
                  Prisko.KoNummer       = 0
                  Prisko.MomsKod        = IF AVAILABLE VarGr
                                            THEN VarGr.MomsKod
                                            ELSE 0
                  Prisko.EtikettStatus  = 1 
                  Prisko.KlargjorStatus = 1  
                  NO-ERROR.
              FIND CURRENT Prisko NO-LOCK.

              /* Klargjør priskø */
              RUN KlargjorPrisKoEn IN h_PrisKo (ROWID(ArtBas)).
              
              IF AVAILABLE PrisKo THEN RELEASE Prisko.
            END. /* PRISBLOKK */

            ELSE MESSAGE "Finner ikke artbas" VPIArtBas.ArtikkelNr VIEW-AS ALERT-BOX.
            
            DO:
              FIND bVPIArtBas EXCLUSIVE-LOCK WHERE
                RECID(bVPIArtBas) = RECID(VPIArtBas) NO-ERROR.
              IF AVAILABLE bVPIArtBas THEN DO:
                bVPIArtBas.BehStatus = 90. /* Behandlet. */
                RELEASE bVPIArtBas.
              END.
            END.
        END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-opprettPricat) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprettPricat Procedure
PROCEDURE opprettPricat:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  	
DEFINE TEMP-TABLE tmpVPIEAN
  FIELD cVerifikationsnr AS CHARACTER FORMAT "x(10)"
  FIELD cLevNr AS CHARACTER FORMAT "x(10)"
  FIELD cBeskrivning AS CHARACTER FORMAT "x(40)"
  FIELD cBekreftad_leveransperiod AS CHARACTER FORMAT "x(10)"
  FIELD cVarugruppskod AS CHARACTER FORMAT "x(10)"
  FIELD cLeveransdatum AS CHARACTER FORMAT "x(10)"
  FIELD cEnhetskod AS CHARACTER FORMAT "x(5)"
  FIELD cAntal AS CHARACTER FORMAT "x(8)"
  FIELD cA-pris AS CHARACTER FORMAT "x(8)"
  FIELD cBelopp AS CHARACTER FORMAT "x(8)"
  FIELD cFOB AS CHARACTER FORMAT "x(8)"
  FIELD cLANDED AS CHARACTER FORMAT "x(8)"
  FIELD cPris AS CHARACTER FORMAT "x(8)"
  FIELD cValuta AS CHARACTER FORMAT "x(8)"
  FIELD cRekommenderat_forsaljningspris AS CHARACTER FORMAT "x(8)"
  FIELD cNorska_inpriser AS CHARACTER FORMAT "x(8)"
  FIELD cNorska_utpriser AS CHARACTER FORMAT "x(8)"
  /* Nye felt */
  FIELD cCollor_code  AS CHARACTER FORMAT "x(20)"
  FIELD cCollor_name  AS CHARACTER FORMAT "x(30)"
  FIELD cSIZE  AS CHARACTER FORMAT "x(10)" 
  FIELD cLENGHT  AS CHARACTER FORMAT "x(10)" 
  FIELD cEancode  AS CHARACTER FORMAT "x(15)"
  INDEX tmpArtikler LevNr cCollor_code
			  																  
	------------------------------------------------------------------------------*/
DEFINE VARIABLE piLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iAnt      AS INTEGER NO-UNDO.

DEFINE BUFFER bufVarGr FOR VarGr.
DEFINE BUFFER bufMoms  FOR Moms.
FIND FIRST VarGr NO-LOCK.
FIND Moms OF VarGr NO-LOCK.

ASSIGN
  piLinjeNr  = 1
  iAntLinjer = 0.

LOOPEN:
FOR EACH tmpVPIEan
    BREAK BY tmpVPIEan.cLevNr
          BY tmpVPIEAN.cCollor_code:

    ASSIGN iAntLinjer = iAntLinjer + 1.
       
    FIND bufVarGr NO-LOCK WHERE
      bufVarGr.Vg = INT(tmpVPIEan.cVarugruppskod) NO-ERROR.
    IF AVAILABLE bufVarGr THEN 
      FIND bufMoms OF bufVarGr NO-LOCK NO-ERROR.
 
    RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: OpprettPricat: Create ' + tmpVPIEan.cEancode + ' ' + tmpVPIEan.cBeskrivning).
       
    CREATE ttPriKat.
    ASSIGN          
         ttPriKat.EkstVPILevNr    = VPIFilHode.EkstVPILevNr
         ttPriKat.LinjeNr         = piLinjeNr
         piLinjeNr                = piLinjeNr  + 1
/*  1 */ ttPriKat.R1              = "R1"
/*  2 */ ttPriKat.LevNr           = STRING(iLevNr)
/*  3 */ ttPriKat.LevModellNr     = tmpVPIEan.cLevNr
/*  4 */ ttPriKat.EANnr           = tmpVPIEan.cEancode
/*  5 */ ttPriKat.VareTekst       = tmpVPIEan.cBeskrivning
         ttPriKat.Varetekst       = REPLACE(ttPriKat.VareTekst,';',',')
/*  6 */ ttPriKat.FargeKode       = tmpVPIEan.cCollor_code
/*  7 */ ttPriKat.FargeTekst      = tmpVPIEan.cCollor_name
/*  8 */ ttPriKat.Str             = tmpVPIEan.cSIZE + (IF tmpVPIEan.cLENGHT <> '' THEN '/' + tmpVPIEan.cLENGHT ELSE '') 

/*  9 */ ttPriKat.StrTab          = "" /* Størrelsestype skal tildeles automatisk */
/* 10 */ ttPriKat.Varemerke       = tmpVPIEAN.cVaremerke 
/* 11 */ ttPriKat.Enh             = tmpVPIEan.cEnhetskod
         ttPriKat.Enh             = REPLACE(ttPriKat.Enh,';',',')
/* 12 */ ttPriKat.AntIEnh         = '1' 
/* 13 */ ttPriKat.LevPrisEngros   = tmpVPIEan.cNorska_inpriser
/* 14 */ ttPriKat.ValKod          = "NOK" 
/* 15 */ ttPriKat.forhRab%        = '' 
/* 16 */ ttPriKat.suppRab%        = '' 
/* 17 */ ttPriKat.VeilPris        = tmpVPIEan.cRekommenderat_forsaljningspris
/* 18 */ ttPriKat.PAKstru         = "" 
/* 19 */ ttPriKat.LevUke1         = "" 
/* 20 */ ttPriKat.LevUke2         = "" 
/* 21 */ ttPriKat.LevUke3         = "" 
/* 22 */ ttPriKat.LevUke4         = "" 
/* 23 */ ttPriKat.VareGruppe      = tmpVPIEan.cVarugruppskod 
/* 24 */ ttPriKat.LevNavn         = "" 
/* 25 */ ttPriKat.LevKod          = tmpVPIEan.cLevNr
/* 26 */ ttPriKat.nettoForh       = tmpVPIEan.cNorska_inpriser 
/* 27 */ ttPriKat.kalkForh        = "" 
/* 28 */ ttPriKat.BFforh          = "" 
/* 29 */ ttPriKat.nettoSupp       = "" 
/* 30 */ ttPriKat.kalkSupp        = "" 
/* 31 */ ttPriKat.BFsupp          = "" 
/* 32 */ ttPriKat.MarkedsPris     = tmpVPIEan.cNorska_utpriser  
/* 33 */ ttPriKat.Sortiment       = "" 
/* 34 */ ttPriKat.Sesong          = tmpVPIEAN.cSasong 
/* 35 */ ttPriKat.VPIBildeKode    = "" 
/* 36 */ ttPriKat.Merknad         = "" 
/* 37 */ ttPriKat.KjedeValutaPris = "" 
/* 38 */ ttPriKat.KjedeProdusent  = ""  
/* 39 */ ttPriKat.ERPNr           = ""
/* 40 */ ttPriKat.SalgsEnhetsType = "Stk"
/* 41 */ ttPriKat.AktivFraDato    = "" /* Skal fylles ut */
/* 42 */ ttPriKat.AktivTilDato    = "" /* Skal fylles ut */
/* 43 */ ttPriKat.Bongtekst       = tmpVPIEan.cBeskrivning
         ttPriKat.Bongtekst       = REPLACE(ttPriKat.Bongtekst,';',',')
/* 44 */ ttPriKat.Etikettekst1    = ttPriKat.Bongtekst
         ttPriKat.Etikettekst1    = REPLACE(ttPriKat.Etikettekst1,';',',')
         ttPriKat.Etikettekst1    = (IF ttPriKat.Etikettekst1 = '' THEN ttPriKat.Varetekst ELSE ttPriKat.Etikettekst1)
/* 45 */ ttPriKat.Funksjonskode   = "N" /* N = "Normal", dvs. vare/prisendring, K = Kampanje, M = Medlemstilbud, U = Utmelding (tolkes som sletting), A = Slett ikke påbegynt kampanje, avslutt påbegynt kampanje */
/* 46 */ ttPriKat.Mva_Proc        = STRING(Moms.MomsProc)
/* 47 */ ttPriKat.LinkVare        = "" /* Skal fylles ut hvis pant */ 
/* 48 */ ttPriKat.PantBelop       = "" /* Skal fylles ut hvis pant */
/* 49 */ ttPriKat.Filial          = "" 
/* 50 */ ttPriKat.Produsent       = "" /* Skal fylles ut. */
/* 51 */ ttPriKat.Mengde          = "1"
/* 52 */ ttPriKat.JamforEnhet     = "Stk"
/* 53 */ ttPriKat.Kontrolleres    = FALSE 
/* 54  ttPriKat.ArtikkelNr      */
/* 55  ttPriKat.OpprettArtikkel */
/* 98 */ ttPriKat.GrunnSortiment  = ''
         ttPriKat.Etikett         = 1
         ttPriKat.Lager           = TRUE
         ttPriKat.BehStatus       = 1 /* Ubehandlet */
        NO-ERROR.
        
END. /* LOOPEN */

RUN bibl_logg.p ('VPIImport', 'xboomvpiinnles.p: OpprettPricat: Ferdig.').

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
