&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xpricatmxinnles.pp
    Purpose     :

    Syntax      :

    Description : Leser inn pricat fra Mx kjeden og omformer den til en PRS pricat fil.

    Author(s)   : Tom Nøkleby
    Created     : 13/09-11
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
DEF VAR iLevNr        AS INT  NO-UNDO.
DEF VAR cLevNavn      AS CHAR NO-UNDO.
DEF VAR cValKod       AS CHAR NO-UNDO.
DEF VAR ctmpKatalog   AS CHAR NO-UNDO.
DEF VAR pcBkuFil      AS CHAR NO-UNDO.
DEF VAR hPgmHandle    AS HANDLE NO-UNDO.
DEFINE VARIABLE iEkstVPILevNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iLoop         AS INTEGER NO-UNDO.
DEFINE VARIABLE plFilId       LIKE VPIFilHode.FilId NO-UNDO.
DEF VAR h_dvpifilhode AS HANDLE NO-UNDO.
DEF VAR h_dvpiartbas  AS HANDLE NO-UNDO.

DEF VAR h_vartkor     AS HANDLE NO-UNDO.
DEF VAR hField1       AS HANDLE NO-UNDO.
DEF VAR c02SjekkListe AS CHAR NO-UNDO.
DEF VAR cGenEan       AS CHAR NO-UNDO.
DEF VAR cEnhet        AS CHAR NO-UNDO.
DEFINE VARIABLE cTekst                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDec                    AS DECIMAL NO-UNDO.
DEFINE VARIABLE bHK                     AS LOG NO-UNDO.
DEFINE VARIABLE iCL                     AS INTEGER NO-UNDO.
DEFINE VARIABLE cbutKatalog             AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk                    AS LOG NO-UNDO.

DEF STREAM InnFil.
DEF STREAM UtVPI.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr   AS INT
  FIELD Tekst     AS CHAR
  FIELD Gradering AS INT 
  .
    
DEFINE BUFFER clButiker   FOR Butiker.
DEFINE BUFFER bVPIFilHode FOR VPIFilHode.

{ttpricat.i &NEW=" " &SHARED=" "}
DEF BUFFER bttPrikat FOR ttPrikat.

{windows.i}
 
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
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE clButiker THEN 
DO:
  MESSAGE 'Ukjent butiknr satt for sentrallager.'
  VIEW-AS ALERT-BOX.
  RETURN.
END.

/* VPI lev.nr på konvertert fil. */
{syspara.i 50 22 1 iEkstVPILevNr INT}

ASSIGN
    ctmpKatalog = RIGHT-TRIM(SESSION:TEMP-DIRECTORY,'\')
    cVPIFil     = "MXVPI" + STRING(iEkstVPILevNr) + "-" + REPLACE(STRING(TODAY,"99-99-9999"),"-","") + "-" + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".csv"
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

/* Leser inn filen i ttPrikat. */
RUN LesInnFil.

/* Eksporterer til VPI fil. */
RUN EksportVPIFil.

/* Leser inn den konverterte filer i VPI mottakskontrollen */
RUN importPricat.

/* Stempler posten som innlest. */
DO TRANSACTION:
    FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
    IF CAN-FIND(FIRST tt_Error WHERE
            tt_Error.Gradering < 50) THEN
        ASSIGN
            VPIFilHode.VPIFilStatus = 9.
    ELSE
        ASSIGN
            VPIFilHode.VPIFilStatus = 5.
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
      VPIDatasett.Beskrivelse = (IF NOT CAN-FIND(FIRST tt_Error WHERE tt_Error.Gradering < 50) THEN 'OK' ELSE 'FEIL') + 
                                ': innlest fil' + cFilNavn.
      FIND CURRENT VPIDatasett NO-LOCK.
      RELEASE VPIDatasett.
  END.
END.

IF CAN-FIND(FIRST tt_Error) THEN
  RUN ErrorLogg.

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
DEF VAR piAntLinjer  AS LOG NO-UNDO.
DEF VAR plArtikkelNr AS DEC NO-UNDO.
DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.

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
              
        OUTPUT STREAM UtVPI TO VALUE(ctmpKatalog + "~\" + cVPIFil) NO-ECHO APPEND.                    
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
        /* 98 */ ttPriKat.Grunnsortiment 
        SKIP      
        .

        OUTPUT STREAM UtVpi CLOSE.


        /* Behandler filen */
        IF LAST-OF(ttPriKat.ButikkNr) THEN 
        DO:
          /* Flytter filen til ankommet katalogen */
          OS-COPY VALUE(ctmpKatalog + "~\" + cVPIFil)
             value(VPIFilHode.Katalog + "~\" + cVPIFil).
          /* Renser bort temp fil */
          IF SEARCH(VPIFilHode.Katalog + "~\" + cVPIFil) <> ? THEN
            OS-DELETE VALUE(ctmpKatalog + "~\" + cVPIFil).
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
  DEF VAR cErrorFil AS CHAR NO-UNDO.

  ASSIGN
      cErrorFil = OS-GETENV('TMP') + '\' 
                 + "Error_" 
                 + VPIFilHode.FilNavn
                 + ".Txt".

  IF CAN-FIND(FIRST tt_Error) THEN 
  OUTPUT TO VALUE(cErrorFil).
    PUT UNFORMATTED
      "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn SKIP
      .

    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** UGYLDIGE VERDIER (ErrorLogg) ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error:
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

 
&IF DEFINED(EXCLUDE-importPricat) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE importPricat Procedure
PROCEDURE importPricat:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	DEFINE VARIABLE piAntLinjer AS INTEGER NO-UNDO.
	

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
          bVPIFilHode.FilNavn      = cVPIFil
          bVPIFilHode.Katalog      = VPIFilHode.Katalog
          bVPIFilHode.Dato         = FILE-INFO:FILE-MOD-DATE
          bVPIFilHode.Kl           = STRING(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS")
          bVPIFilHode.Storrelse    = FILE-INFO:FILE-SIZE
          bVPIFilHode.AntLinjer    = 0
          bVPIFilHode.VPIFilType   = 1 /* VPI */
          bVPIFilHode.VPIFilStatus = 1
          bVPIFilHode.EkstVPILevNr = VPIFilHode.EkstVPILevNr
          .
        RELEASE bVPIFilHode.
    END.
    
    RUN xPRSPricatInnles.p (plFilId,?,output piAntLinjer).
    IF piAntLinjer > 0 THEN 
        RUN xPRSPricatUtpakk.p (plFilId).
    /*
    /* Starter program for lasting av varemottak */
    IF NOT VALID-HANDLE(h_dvpifilhode) THEN
        RUN dvpifilhode.w PERSISTENT SET h_dvpifilhode.
    IF NOT VALID-HANDLE(h_dvpiartbas) THEN
    DO:
        RUN dvpiartbas.w PERSISTENT SET h_dvpiartbas.
        RUN SettAutoImport IN h_dvpiartbas (INPUT TRUE).
    END.
    /* Leser inn filen. */
    RUN LesInnFil IN h_dvpifilhode (INPUT STRING(plFilId), 
                               OUTPUT obOk, 
                               OUTPUT piAntLinjer).
    /* Pakker ut fil. */
    RUN PakkUtFil IN h_dvpifilhode (INPUT STRING(plFilId)).
 
    IF VALID-HANDLE(h_dvpifilhode) THEN
        DELETE PROCEDURE h_dvpifilhode.
    IF VALID-HANDLE(h_dvpiartbas) THEN
        DELETE PROCEDURE h_dvpiartbas.
   */
    IF SEARCH(VPIFilHode.Katalog + "~\" + cVPIFil) <> ? THEN
      OS-DELETE VALUE(VPIFilHode.Katalog + "~\" + cVPIFil).
         
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
  DEF VAR piLinjeNr AS INT  NO-UNDO.
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
    IMPORT STREAM InnFil UNFORMATTED cLinje.

    ASSIGN
      iAntLinjer = iAntLinjer + 1
      cLinje    = REPLACE(cLinje,'"',' ')
      cLinje    = TRIM(cLinje,'"') /* Preem legger ut med " i første og siste posisjon. */
      .

    /* Tomme linjer  */
    IF TRIM(cLinje) = "" THEN
        NEXT.

    CREATE ttPriKat.
    ASSIGN
         ttPriKat.EkstVPILevNr    = VPIFilHode.EkstVPILevNr
         ttPriKat.LinjeNr         = piLinjeNr
         piLinjeNr                = piLinjeNr  + 1
/*  1 */ ttPriKat.R1              = "R1"
/*  2 */ ttPriKat.LevNr           = TRIM(ENTRY(2,cLinje,';'))
         ttPriKat.LevModellNr     = TRIM(TRIM(ENTRY( 3,cLinje,";"),'"'))
         ttPriKat.LevModellNr     = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(ttPriKat.LevModellNr,";"," "),'"',' '),"'"," "),CHR(13),""),CHR(10),""),',','.')

         ttPriKat.EANnr           = TRIM(TRIM(ENTRY( 4,cLinje,";"),'"'))
         ttPriKat.VareTekst       = TRIM(SUBSTRING(TRIM(ENTRY( 5,cLinje,";"),'"'),1,100))
         ttPriKat.VareTekst       = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(ttPriKat.VareTekst,";"," "),'"',' '),"'"," "),CHR(13),""),CHR(10),""),',','.')
         ttPriKat.FargeKode       = TRIM(TRIM(ENTRY( 6,cLinje,";"),'"'))
         ttPriKat.FargeTekst      = TRIM(SUBSTRING(TRIM(ENTRY( 6,cLinje,";"),'"'),1,100)) + 
                                    (IF ENTRY( 7,cLinje,";") <> "" THEN "/" ELSE "") +
                                    trim(SUBSTRING(TRIM(ENTRY( 7,cLinje,";"),'"'),1,100))
         ttPriKat.FargeTekst      = (IF ttPriKat.FargeTekst BEGINS '/' THEN SUBSTRING(ttPriKat.FargeTekst,2) ELSE ttPriKat.FargeTekst)       
         ttPriKat.Str             = TRIM(TRIM(ENTRY( 8,cLinje,";"),'"'))
         ttPriKat.Str             = IF ttPriKat.Str = "" THEN "1" ELSE ttPriKat.Str
         ttPriKat.Str             = IF ttPriKat.Str = "OS" THEN "1" ELSE ttPriKat.Str
         ttPriKat.StrTab          = TRIM(ENTRY( 9,cLinje,";"),'"')
         ttPriKat.Varemerke       = TRIM(TRIM(ENTRY(10,cLinje,";"),'"'))
         ttPriKat.Varemerke       = IF LENGTH(ttPriKat.Varemerke) > 6 THEN '' ELSE ttPriKat.Varemerke
         NO-ERROR.
     IF ERROR-STATUS:ERROR THEN 
     DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil        = piAntFeil + 1
          tt_Error.LinjeNr = iAntLinjer
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + ". Feil ved assign ttPrikat - Blokk 1.".
          tt_Error.Gradering = 2.
        NEXT.
     END.      
         
     ASSIGN 
         ttPriKat.Enh             = TRIM(TRIM(ENTRY(11,cLinje,";"),'"'))
         /*ttPriKat.Enh             = (IF ttPriKat.Enh = "" THEN "Stk" ELSE ttPriKat.Enh)*/
         ttPriKat.Enh             = "Stk"
         ttPriKat.AntIEnh         = TRIM(ENTRY(12,cLinje,";"),'"')
         ttPriKat.LevPrisEngros   = REPLACE(TRIM(TRIM(ENTRY(13,cLinje,";"),'"'),"%"),' ','')
         ttPriKat.ValKod          = TRIM(ENTRY(14,cLinje,";"),'"')
         ttPriKat.forhRab%        = TRIM(TRIM(TRIM(ENTRY(15,cLinje,";"),'"'),"%"),'')
         ttPriKat.suppRab%        = TRIM(TRIM(TRIM(ENTRY(16,cLinje,";"),'"'),"%"),'')
         ttPriKat.VeilPris        = TRIM(ENTRY(17,cLinje,";"),'"')
         ttPriKat.VeilPris        = TRIM(REPLACE(ttPriKat.VeilPris,' ',''),"%")
         ttPriKat.PAKstru         = TRIM(ENTRY(18,cLinje,";"),'"')         
         ttPriKat.LevUke1         = "" /*TRIM(ENTRY(19,cLinje,";"),'"')*/
         ttPriKat.LevUke2         = "" /*TRIM(ENTRY(20,cLinje,";"),'"')*/
         ttPriKat.LevUke3         = "" /*TRIM(ENTRY(21,cLinje,";"),'"')*/
         ttPriKat.LevUke4         = "" /*TRIM(ENTRY(22,cLinje,";"),'"')*/
         ttPriKat.VareGruppe      = LEFT-TRIM(TRIM(ENTRY(23,cLinje,";"),'"'),'0')
         ttPriKat.LevNavn         = TRIM(ENTRY(26,cLinje,";"),'"')
         ttPriKat.LevKod          = TRIM(TRIM(ENTRY(25,cLinje,";"),'"')) 
         ttPriKat.LevKod          = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(ttPriKat.LevKod,";"," "),'"',' '),"'"," "),CHR(13),""),CHR(10),""),',','.')
         ttPriKat.LevKod          = IF ttPriKat.LevKod = ""
                                      THEN ttPriKat.LevModellNr
                                      ELSE ttPriKat.LevKod
         NO-ERROR.

     IF ERROR-STATUS:ERROR THEN 
     DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil        = piAntFeil + 1
          tt_Error.LinjeNr = iAntLinjer
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + ". Feil ved assign ttPrikat - Blokk 2.".
          tt_Error.Gradering = 2.
        NEXT.
     END.      

    ASSIGN 
         ttPriKat.nettoForh       = "" 
         ttPriKat.kalkForh        = "" 
         ttPriKat.BFforh          = "" 
         ttPriKat.nettoSupp       = ""
         ttPriKat.kalkSupp        = ""
         ttPriKat.BFsupp          = ""
         ttPriKat.MarkedsPris     = REPLACE(TRIM(TRIM(ENTRY(17,cLinje,";"),'"'),"%"),' ','')
         ttPriKat.Sortiment       = ""
         ttPriKat.Sesong          = ""
         ttPriKat.VPIBildeKode    = ""
         ttPriKat.Merknad         = TRIM(TRIM(ENTRY(27,cLinje,";"),'"'))
/* 37 */ ttPriKat.KjedeValutaPris = "" 
/* 38 */ ttPriKat.KjedeProdusent  = ""
/* 39 */ ttPriKat.ERPNr           = TRIM(TRIM(ENTRY(25,cLinje,";"),'"'))
/* 40 */ ttPriKat.SalgsEnhetsType = ""
         NO-ERROR.
     IF ERROR-STATUS:ERROR THEN 
     DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil        = piAntFeil + 1
          tt_Error.LinjeNr = iAntLinjer
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + ". Feil ved assign ttPrikat - Blokk 3.".
          tt_Error.Gradering = 2.
        NEXT.
     END.      
     ASSIGN 
/* 41 */ ttPriKat.AktivFraDato    = "" /* Skal fylles ut */
/* 42 */ ttPriKat.AktivTilDato    = "" /* Skal fylles ut */
/* 43 */ ttPriKat.Bongtekst       = ttPriKat.VareTekst
/* 44 */ ttPriKat.Etikettekst1    = ttPriKat.VareTekst
/* 45 */ ttPriKat.Funksjonskode   = "N" /* N = "Normal", dvs. vare/prisendring, K = Kampanje, M = Medlemstilbud, U = Utmelding (tolkes som sletting), A = Slett ikke påbegynt kampanje, avslutt påbegynt kampanje */
/* 46 */ ttPriKat.Mva_Proc        = ""
/* 47 */ ttPriKat.LinkVare        = "0" 
/* 48 */ ttPriKat.PantBelop       = "" /* Skal fylles ut hvis pant */
/* 49 */ ttPriKat.Filial          = "" 
/* 50 */ ttPriKat.Produsent       = "" /* Skal fylles ut. */
/* 51 */ ttPriKat.Mengde          = "1"
/* 52 */ ttPriKat.JamforEnhet     = "Stk"
/* 53 */ ttPriKat.Kontrolleres    = FALSE
/* 54  ttPriKat.ArtikkelNr      */
/* 55  ttPriKat.OpprettArtikkel */
/* 98 */ ttPriKat.GrunnSortiment  = ""
         ttPriKat.Etikett         = 1
         ttPriKat.Lager           = TRUE
         ttPriKat.BehStatus       = 1 /* Ubehandlet */
        NO-ERROR.

     IF ERROR-STATUS:ERROR THEN 
     DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil        = piAntFeil + 1
          tt_Error.LinjeNr = iAntLinjer
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + ". Feil ved assign ttPrikat - Blokk 4.".
          tt_Error.Gradering = 2.
        NEXT.
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
