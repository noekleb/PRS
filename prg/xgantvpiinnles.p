&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xgantvpiinnles.p
    Purpose     :

    Syntax      :

    Description : Leser inn vpi filen og omformer den til en pricat fil.

    Author(s)   : Tom N�kleby
    Created     : 15/11-16
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
DEF VAR hPgmHandle    AS HANDLE NO-UNDO.
DEF VAR h_dvpifilhode AS HANDLE NO-UNDO.
DEF VAR h_dvpiartbas  AS HANDLE NO-UNDO.
DEF VAR h_PrisKo      AS HANDLE NO-UNDO.
DEFINE VARIABLE iInt AS INTEGER NO-UNDO.
DEF VAR h_vartkor     AS HANDLE NO-UNDO.
DEF VAR hField1       AS HANDLE NO-UNDO.
DEF VAR c02SjekkListe AS CHAR NO-UNDO.
DEF VAR cGenEan       AS CHAR NO-UNDO.
DEFINE VARIABLE cDefVg AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
DEF VAR iFarg LIKE Farg.Farg NO-UNDO.
DEFINE VARIABLE lMaksRab% AS DECIMAL NO-UNDO. 
DEFINE VARIABLE cVgLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE cCheckLst AS CHARACTER NO-UNDO.

DEF STREAM InnFil.
DEF STREAM UtVPI.
DEF STREAM UtVre.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .
DEF TEMP-TABLE tmpVare
    FIELD VareNr AS CHAR.

{ttpricat.i &NEW=" " &SHARED=" "}
DEF BUFFER bttPrikat FOR ttPrikat.

/* Nye artikler som leses inn som har ukjent varegruppe, tildeles default varegruppe. */
{syspara.i 50 15 25 cDefVg}.

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

&IF DEFINED(EXCLUDE-FixChk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FixChk Procedure 
FUNCTION FixChk RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER )  FORWARD.

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
ASSIGN
    lMaksRab% = 70
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

{syspara.i 2 4 8 cGenEan}

/* Bygger liste med sjekkm�nstre. */
DO iLoop = 2011 TO YEAR(TODAY):
    cCheckLst = cCheckLst + 
                (IF cCheckLst = '' THEN '' ELSE ',') + 
                SUBSTRING(STRING(iLoop,"9999"),3,2) + '5' + 
                (IF cCheckLst = '' THEN '' ELSE ',') + 
                SUBSTRING(STRING(iLoop,"9999"),3,2) + '6'.  
END.

ASSIGN
    cVgLst      =  '740050,710050' 
    ctmpKatalog = SESSION:TEMP-DIRECTORY
    cVPIFil     = "GNVPI" + STRING(VPIFilHode.EkstVPILevNr) + "-" + STRING(TODAY,"99-99-9999") + "-" + string(TIME,"HH:MM:SS") + ".csv"
    cVPIFil     = REPLACE(cVPIFil,":","-")
    cVPIUtFil   = SUBSTRING(cVPIFil,3)
    .

RUN LesInnFil.

IF CAN-FIND(FIRST ttPriKat) THEN 
    RUN flaggWebArtikkel.

RUN settHovedKategori. 

/* Stopper innlesningsprogram for h�ndterminalfil. */
IF VALID-HANDLE(hPgmHandle) THEN
    DELETE PROCEDURE hPgmHandle.
IF VALID-HANDLE(h_dvpifilhode) THEN
    DELETE PROCEDURE h_dvpifilhode.
IF VALID-HANDLE(h_dvpiartbas) THEN
    DELETE PROCEDURE h_dvpiartbas.
IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_PrisKo.

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

DEF BUFFER bVPIFilHode  FOR VPIFilHode.
DEF BUFFER b2VPIFilHode FOR VPIFilHode.

/* MESSAGE ctmpKatalog + cVPIUtFil SKIP   */
/*     iAntLinjer                         */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */

IF iAntLinjer > 0 THEN
DO:
    OUTPUT STREAM UtVPI TO VALUE(ctmpKatalog + cVPIUtFil) NO-ECHO.                    
    EKSPORTFIL:                    
    FOR EACH ttPrikat NO-LOCK
        BREAK BY ttPriKat.LevModellNr
              BY ttPriKat.VareTekst     
              BY ttPriKat.FargeTekst    
              BY ttPriKat.SeqNrStr           
              BY ttPriKat.MarkedsPris:
        EXPORT STREAM UtVpi DELIMITER ";"
        /*  1 */ ttPriKat.R1            
        /*  2 */ ttPriKat.LevNr         
        /*  3 */ ttPriKat.LevModellNr   
        /*  4 */ ttPriKat.EANnr         
        /*  5 */ ttPriKat.VareTekst     
        /*  6 */ ttPriKat.FargeKode     
        /*  7 */ ttPriKat.FargeTekst    
        /*  8 */ ttPriKat.Str           
        /*  9 */ ttPriKat.StrTab        
        /* 10 */ ttPriKat.Varemerke     
        /* 11 */ ttPriKat.Enh           
        /* 12 */ ttPriKat.AntIEnh       
        /* 13 */ ttPriKat.LevPrisEngros 
        /* 14 */ ttPriKat.ValKod        
        /* 15 */ ttPriKat.forhRab%      
        /* 16 */ ttPriKat.suppRab%      
        /* 17 */ ttPriKat.VeilPris      
        /* 18 */ ttPriKat.PAKstru       
        /* 19 */ ttPriKat.LevUke1       
        /* 20 */ ttPriKat.LevUke2       
        /* 21 */ ttPriKat.LevUke3       
        /* 22 */ ttPriKat.LevUke4       
        /* 23 */ ttPriKat.VareGruppe    
        /* 24 */ ttPriKat.LevNavn       
        /* 25 */ ttPriKat.LevKod  
        /* 26 */ ttPriKat.nettoForh     
        /* 27 */ ttPriKat.kalkForh      
        /* 28 */ ttPriKat.BFforh        
        /* 29 */ ttPriKat.nettoSupp     
        /* 30 */ ttPriKat.kalkSupp      
        /* 31 */ ttPriKat.BFsupp        
        /* 32 */ ttPriKat.MarkedsPris   
        /* 33 */ ttPriKat.Sortiment     
        /* 34 */ ttPriKat.Sesong        
        /* 35 */ ttPriKat.VPIBildeKode
        /* 36 */ ttPriKat.Merknad     
        .

    END. /* EKSPORTFIL */
    OUTPUT STREAM UtVpi CLOSE.

    /* Flytter filen til ankommet katalogen */
    OS-COPY VALUE(ctmpKatalog + "~\" + cVPIUtFil)
            value(VPIFilHode.Katalog + "~\" + cVPIUtFil).
    /* Renser bort temp fil */
    IF SEARCH(VPIFilHode.Katalog + "~\" + cVPIUtFil) <> ? THEN
        OS-DELETE VALUE(ctmpKatalog + "~\" + cVPIUtFil).

    /* Opprett VPIFilHode og les inn. */
    LES-INN-OPPDATER:
    DO:
        FILE-INFO:FILE-NAME = VPIFilHode.Katalog + "\" + cVPIUtFil.

        /* Finner FilId */
        FIND LAST bVPIFilHode NO-LOCK NO-ERROR.
        IF AVAILABLE bVPIFilHode THEN
          plFilId = bVPIFilHode.FilId + 1.
        ELSE
          plFilId = 1.
        DO FOR b2VPIFilHode TRANSACTION:
            CREATE b2VPIFilHode.
            ASSIGN
              b2VPIFilHode.FilId        = plFilId
              b2VPIFilHode.FilNavn      = cVPIUtFil
              b2VPIFilHode.Katalog      = VPIFilHode.Katalog
              b2VPIFilHode.Dato         = FILE-INFO:FILE-MOD-DATE
              b2VPIFilHode.Kl           = STRING(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS")
              b2VPIFilHode.Storrelse    = FILE-INFO:FILE-SIZE
              b2VPIFilHode.AntLinjer    = 0
              b2VPIFilHode.VPIFilType   = 1 /* VPI */
              b2VPIFilHode.VPIFilStatus = 1
              b2VPIFilHode.EkstVPILevNr = VPIFilHode.EkstVPILevNr
              .
            RELEASE b2VPIFilHode.
        END.
        /* Starter program for lasting av varemottak */
        IF NOT VALID-HANDLE(h_PrisKo) THEN
            RUN prisko.p PERSISTENT SET h_PrisKo.
        IF NOT VALID-HANDLE(h_dvpifilhode) THEN
            RUN dvpifilhode.w PERSISTENT SET h_dvpifilhode.
        IF NOT VALID-HANDLE(h_dvpiartbas) THEN
        DO:
            RUN dvpiartbas.w PERSISTENT SET h_dvpiartbas.
            RUN SettAutoImport IN h_dvpiartbas (INPUT TRUE).
        END.
        /* Leser inn filen. */
        RUN LesInnFil IN h_dvpifilhode (INPUT STRING(plFilId), 
                                   OUTPUT pbOk, 
                                   OUTPUT piAntLinjer).
        /* Pakker ut fil. */
        RUN PakkUtFil IN h_dvpifilhode (INPUT STRING(plFilId)).

        /* Oppretter alle nye poster */
        /*RUN OpprettUtvalg.*/
        FOR EACH VPIArtBas NO-LOCK WHERE
            VPIArtBas.EkstVPILevNr = VPIFilHode.EkstVPILevNr AND
            VPIArtBas.VPIDato      = TODAY:
            RUN OpprettNy    IN h_dvpiartbas (VPIFilHode.EkstVPILevNr, VPIArtBas.VareNr, OUTPUT plArtikkelNr).
            RUN OppdaterInfo IN h_dvpiartbas (VPIFilHode.EkstVPILevNr, VPIArtBas.VAreNr, plArtikkelNr).
            RUN OppdaterPris IN h_dvpiartbas (VPIFilHode.EkstVPILevNr, VPIArtBas.VAreNr, plArtikkelNr).
            FIND ArtBas NO-LOCK WHERE
                ArtBas.ArtikkelNr = DEC(VPIArtBAs.ArtikkelNr) NO-ERROR.
            IF AVAILABLE ArtBas THEN
                RUN KlargjorPrisKoEn IN h_PrisKo (ROWID(ArtBas)).
            ELSE MESSAGE "Finner ikke artbas" VPIArtBAs.ArtikkelNr VIEW-AS ALERT-BOX.

            DO TRANSACTION:
                FIND ArtBas EXCLUSIVE-LOCK WHERE
                    ArtBas.ArtikkelNr = DEC(VPIArtBAs.ArtikkelNr) NO-ERROR.
                IF (AVAILABLE ArtBas AND ArtBas.OPris = FALSE) THEN
                    ArtBas.Lager = TRUE.
                IF AVAILABLE ArtBas THEN
                    RELEASE ArtBas.
            END.
        END.
    END. /* LES-INN-OPPDATER */
    
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
  OUTPUT TO VALUE("Error.Txt").
    PUT UNFORMATTED
      "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn SKIP
      .
    FOR EACH tt_Error:
      PUT UNFORMATTED tt_Error.Tekst SKIP.
    END.
  OUTPUT CLOSE.
  IF SEARCH("Error.Txt") <> ? THEN
  DO:
    DEF VAR hInstance AS INT.

    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH("Error.Txt"),
                                  "",
                                  1,
                                  OUTPUT hInstance).

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-flaggWebArtikkel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE flaggWebArtikkel Procedure
PROCEDURE flaggWebArtikkel:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    FOR EACH ttPrikat NO-LOCK
        BREAK BY ttPriKat.LevModellNr
              BY ttPriKat.VareTekst     
              BY ttPriKat.FargeTekst    
              BY ttPriKat.SeqNrStr           
              BY ttPriKat.MarkedsPris:

        /*IF FIRST-OF(ttPriKat.FargeTekst) THEN*/
        OPPDATER: 
        DO:
            FIND Strekkode NO-LOCK WHERE 
                Strekkode.Kode = ttPriKat.EANnr NO-ERROR.
            IF NOT AVAILABLE Strekkode THEN 
                LEAVE OPPDATER.
            DO TRANSACTION:
                FIND ArtBas EXCLUSIVE-LOCK WHERE 
                    ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
                IF AVAILABLE ArtBas THEN 
                DO:
                    ASSIGN 
                        ArtBas.WebButikkArtikkel   = TRUE
                        ArtBas.PubliserINettbutikk = TRUE  
                        . 
                    RELEASE ArtBas.            
                END.
            END. /* TRANSACTION */
        END. /* OPPDATER */
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
     /* ----------------
      1 * Artikkelnr:
      2 * EANNo
      3 * Navn:
      4 * Fargekode
      5 * Str
      6 * Varemerke
      7 * Utpris
      8 * Innpris
      9 * Varegruppe 
     10 * Sesong
     11 * Forh. rabatt
     12 * Leverand�r
    ------------------ */


------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr AS INT  NO-UNDO.
  DEF VAR piAntFeil AS INT  NO-UNDO.
  DEF VAR pcBkuFil  AS CHAR NO-UNDO.
  DEF VAR plVareNr  AS DEC  NO-UNDO.
  DEF VAR pcStr     AS CHAR NO-UNDO.
  DEF VAR piLoop    AS INT  NO-UNDO.
  
  /* T�mmer feillogg. */
  FOR EACH tt_Error:
    DELETE tt_Error.
  END.
  /* T�mmer pricat */
  FOR EACH ttPrikat TRANSACTION:
      DELETE ttPrikat.
  END.
  RUN TellOppLinjer.

  ASSIGN
      piLinjeNr  = 1.
      iAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.

    ASSIGN
      iAntLinjer = iAntLinjer + 1
      .

    /* Tomme linjer fra Excel */
    IF pcLinje BEGINS ";;;;;" THEN
        NEXT LESERLINJER.
    /* OVerskriftsrad 1 */
    IF pcLinje BEGINS "1;2;3" THEN
        NEXT LESERLINJER.
    /* OVerskriftsrad 2 */
    IF pcLinje BEGINS "LevModellNr" THEN
        NEXT LESERLINJER.

    IF NUM-ENTRIES(pcLinje,";") < 12 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        pcLinje = pcLinje + ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
        .
      /*
      CREATE tt_Error.
      ASSIGN
        tt_Error.LinjeNr = piAntFeil
        tt_Error.Tekst   = "** Feil antall entries p� linje " + STRING(iAntLinjer) + " (skal v�re 27): " + string(NUM-ENTRIES(pcLinje,";")) + "."
        .
      */
    END.

    /* Dette feiler kun p� overskriftsraden */
    /*
    ASSIGN
        plVareNr = dec(ENTRY( 1,pcLinje,";"))
        NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
        NEXT LESERLINJER.
    */

    CREATE ttPriKat.
    ASSIGN
        ttPriKat.EkstVPILevNr  = VPIFilHode.EkstVPILevNr
        ttPriKat.LinjeNr       = piLinjeNr
        piLinjeNr              = piLinjeNr  + 1
        /*               */ ttPriKat.R1            = "R1"
        /*               */ ttPriKat.LevNr         = TRIM(ENTRY(12,pcLinje,";"),'"')
        /* nArtno        */ ttPriKat.LevModellNr   = TRIM(ENTRY( 1,pcLinje,";"),'"')                  
        /* cEANCode      */ ttPriKat.EANnr         = TRIM(ENTRY( 2,pcLinje,";"),'"')
        /* rArtName      */ ttPriKat.VareTekst     = TRIM(ENTRY( 3,pcLinje,";"),'"')
        /* NColCode      */ ttPriKat.FargeKode     = TRIM(ENTRY( 4,pcLinje,";"),'"')
        /* cColName      */ ttPriKat.FargeTekst    = TRIM(ENTRY( 4,pcLinje,";"),'"')
        /* cCode1        */ ttPriKat.Str           = TRIM(ENTRY( 5,pcLinje,";"),'"')
        /* nOrder        */ ttPriKat.SeqNrStr      = 0 
        /* nSeason(3) + nSizeCode(3) */
                            ttPriKat.StrTab        = "2"
        /* nMainGroup    */ ttPriKat.Varemerke     = TRIM(ENTRY( 6,pcLinje,";"),'"')
        /*               */ ttPriKat.Enh           = "Stk"
        /*               */ ttPriKat.AntIEnh       = "1"

        /* NWholeSaleNet */ ttPriKat.LevPrisEngros = REPLACE(REPLACE(REPLACE(TRIM(TRIM(ENTRY(8,pcLinje,";"),'"'),"%"),' ',''),'.',','),CHR(160),'')
        /*               */ ttPriKat.ValKod        = "NOK"
        /*               */ ttPriKat.forhRab%      = REPLACE(REPLACE(REPLACE(TRIM(TRIM(ENTRY(11,pcLinje,";"),'"'),"%"),' ',''),'.',','),CHR(160),'')
        /*               */ ttPriKat.suppRab%      = ""
        /* nRetailPrice  */ ttPriKat.VeilPris      = TRIM(ENTRY(7,pcLinje,";"),'"')
        /*               */ ttPriKat.PAKstru       = ""
        /*               */ ttPriKat.LevUke1       = ""
        /*               */ ttPriKat.LevUke2       = ""
        /*               */ ttPriKat.LevUke3       = ""
        /*               */ ttPriKat.LevUke4       = ""
        /* nArtgroup + nSubGroup */
                            ttPriKat.VareGruppe    = ENTRY(9,pcLinje,";") 
        /*               */ ttPriKat.LevNavn       = ""
        /* nArtno        */ ttPriKat.LevKod        = "" /*ttPriKat.LevModellNr*/                  
        /* NWholeSaleNet */ ttPriKat.nettoForh     = ""
        /*               */ ttPriKat.kalkForh      = ""
        /*               */ ttPriKat.BFforh        = ""
        /*               */ ttPriKat.nettoSupp     = ""
        /*               */ ttPriKat.kalkSupp      = ""
        /*               */ ttPriKat.BFsupp        = ""
        /* nRetailPrice  */ ttPriKat.MarkedsPris   = REPLACE(REPLACE(REPLACE(TRIM(TRIM(ENTRY(7,pcLinje,";"),'"'),"%"),' ',''),'.',','),CHR(160),'')
        /*               */ ttPriKat.Sortiment     = ""
        /* nSeason       */ ttPriKat.Sesong        = TRIM(ENTRY(10,pcLinje,";"),'"')
                            ttPriKat.VPIBildeKode  = ""
        /*               */ ttPriKat.Merknad       = ""
        .      
    
    /* Sjekker/korrigerer rabatt */
    IF DEC(ttPriKat.forhRab%) > lMaksRab% THEN 
        ttPriKat.forhRab% = STRING(lMaksRab%).    
        
    /* Tar h�nd om st�rrelsen. Settes alltid til det som er gitt ved strekkoden. */
    DO:
        FIND StrekKode NO-LOCK WHERE 
            StrekKode.Kode = ttPriKat.EANnr NO-ERROR.
        FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
        IF AVAILABLE StrKonv THEN 
            ASSIGN
                ttPriKat.Str  = StrKonv.Storl.
    END.    
        
    /* Tar h�nd om sesong */
    IF ttPriKat.Sesong = '' THEN 
    DO:
        IF AVAILABLE ArtBas THEN 
            RELEASE ArtBas.
        FIND StrekKode NO-LOCK WHERE 
            StrekKode.Kode = ttPriKat.EANnr NO-ERROR.
        IF AVAILABLE StrekKode THEN 
            FIND ArtBas OF StrekKode NO-ERROR.
        IF AVAILABLE ArtBas THEN 
        ASSIGN
            ttPriKat.Sesong  = STRING(ArtBas.SaSong).
    END.    
        
    /* Tar h�nd om varemerke. Integer = VmId */
    iInt = INT(TRIM(ttPriKat.Varemerke)) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN 
    DO:
        FIND Varemerke NO-LOCK WHERE 
             Varemerke.VMId = iInt NO-ERROR.
        IF AVAILABLE Varemerke THEN 
            ttPriKat.Varemerke = Varemerke.Beskrivelse.
    END.
    IF TRIM(ttPriKat.Varemerke) = '' THEN 
    DO:
        IF AVAILABLE ArtBas THEN 
            RELEASE ArtBas.
        FIND StrekKode NO-LOCK WHERE 
            StrekKode.Kode = ttPriKat.EANnr NO-ERROR.
        IF AVAILABLE StrekKode THEN 
            FIND ArtBas OF StrekKode NO-ERROR.
        IF AVAILABLE ArtBas THEN 
            FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
        IF AVAILABLE Varemerke THEN 
        ASSIGN
            ttPriKat.Varemerke  = Varemerke.Beskrivelse.
    END. 

    /* Tar h�nd om fargekoden. Legger den opp hvis den ikke finnes. */
    IF TRIM(ttPriKat.FargeKode) <> '' THEN 
    DO TRANSACTION:
        FIND FIRST Farg NO-LOCK WHERE
            Farg.KFarge = TRIM(ttPriKat.FargeKode) NO-ERROR.
        IF NOT AVAILABLE Farg THEN
        DO:
            FIND LAST Farg.
            IF AVAILABLE Farg THEN
                iFarg = Farg.Farg + 1.
            ELSE 
                ifarg = 1.
            CREATE Farg.
            ASSIGN
                Farg.Farg     = iFarg
                Farg.KFarge   = TRIM(ttPriKat.FargeKode)
                Farg.FarBeskr = TRIM(ttPriKat.FargeKode)
                ttPriKat.Farg = iFarg
                .
        END.
    END. /* TRANSACTION */
    /* Henter fargen p� eksisterende artikkel */
    ELSE IF ttPriKat.FargeKode = '' AND ttPriKat.Ean <> '' THEN 
    DO:
        IF AVAILABLE ArtBas THEN 
            RELEASE ArtBas.
        FIND StrekKode NO-LOCK WHERE 
            StrekKode.Kode = ttPriKat.EANnr NO-ERROR.
        IF AVAILABLE StrekKode THEN 
            FIND ArtBas OF StrekKode NO-ERROR.
        IF AVAILABLE ArtBas THEN 
            FIND Farg OF ArtBas NO-LOCK NO-ERROR.
        IF AVAILABLE Farg THEN 
        ASSIGN
            ttPriKat.FargeKode  = STRING(Farg.Farg)
            ttPriKat.Farg       = Farg.Farg
            ttPriKat.FargeTekst = Farg.FarBeskr
        .
    END.

    /* Sjekker varegruppen */
    ASSIGN lDec = DECIMAL(ttPriKat.VareGruppe) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
      ttPriKat.VareGruppe = ''.
    IF INT(ttPriKat.VareGruppe) = 0 OR NOT CAN-FIND(VarGr WHERE VarGr.Vg = INT(ttPriKat.VareGruppe)) THEN 
      RUN settVaregruppe.
      
    /* sjekker leverand�ren */       
    ASSIGN 
        lDec = DECIMAL(ttPriKat.LevNr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        ttPriKat.LevNr = ''.
    IF INT(ttPriKat.LevNr) = 0 OR NOT CAN-FIND(LevBas WHERE LevBas.LevNr = INT(ttPriKat.LevNr)) THEN 
        RUN settLevbas.
        
/* MESSAGE                                */
/*        ttPriKat.R1            SKIP     */
/*        ttPriKat.LevNr         SKIP     */
/*        ttPriKat.LevModellNr   SKIP     */
/*        ttPriKat.EANnr          SKIP    */
/*        ttPriKat.VareTekst      SKIP    */
/*        ttPriKat.AntIEnh        SKIP    */
/*        ttPriKat.LevPrisEngros  SKIP    */
/*        ttPriKat.VeilPris       SKIP    */
/*        ttPriKat.VareGruppe     SKIP    */
/*        ttPriKat.LevKod         SKIP    */
/*        ttPriKat.MarkedsPris            */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    
     STATUS DEFAULT "Lese linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  /* Eksporterer til VPI fil. */
  RUN EksportVPIFil.

  /* Stempler posten som innlest. */
  DO TRANSACTION:
      FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
      ASSIGN
          VPIFilHode.VPIFilStatus = 5
          .
      IF AVAILABLE VPIFilHode THEN
          FIND CURRENT VPIFilHode  NO-LOCK.
  END.

  IF CAN-FIND(FIRST tt_Error) THEN
    RUN ErrorLogg.

  ASSIGN
  pcBkuFil = VPIFilHode.Katalog + "~\bku" + "\" + 
             VPIFilHode.FilNavn
  .

  /* PAKKSEDDELFILEN */
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
  /* Oppdateringsrutinen henter informasjonen fra f�rste artikkel i listen. Fra de */
  /* �vrige artiklene hentes kun EAN koden.                                        */
  /* Oppdatering gj�res mot en eksisterende lokal artikkel, eller det opprettes en */
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

&IF DEFINED(EXCLUDE-settHovedKategori) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE settHovedKategori Procedure
PROCEDURE settHovedKategori:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE iVg AS INTEGER NO-UNDO.
DEFINE VARIABLE cChar AS CHARACTER NO-UNDO.

DO iLoop = 1 TO NUM-ENTRIES(cVgLst):
    iVg = INT(ENTRY(iLoop,cVgLst)).
    ARTLOOP:
    FOR EACH ArtBas EXCLUSIVE-LOCK WHERE 
        ArtBas.Vg = iVg AND 
        LENGTH(ArtBas.LevKod) > 3 AND
        ArtBas.EDato >= TODAY AND  
        CAN-DO(cCheckLst,SUBSTRING(ArtBas.LevKod,1,3)):

        cChar = SUBSTRING(ArtBas.LevKod,3,1).

        IF cChar = '5' THEN
            ArtBas.HovedKatNr = 55.
        ELSE IF cChar = '6' THEN
            ArtBas.HovedKatNr = 56.
    END. /* ARTLOOP */
END.


END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-settLevbas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE settLevbas Procedure 
PROCEDURE settLevbas :
/*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/
    IF NOT AVAILABLE ttPriKat 
        THEN RETURN.
    FIND Strekkode NO-LOCK WHERE 
        Strekkode.Kode = ttPriKat.EANnr NO-ERROR.
    IF AVAILABLE Strekkode THEN 
        FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
    IF AVAILABLE ArtBas THEN 
    DO:        
        IF CAN-FIND(LevBas OF ArtBas) THEN 
            ttPriKat.LevNr = STRING(ArtBas.LevNr).
        ELSE
            ttPriKat.LevNr = ''.   
    END.
    ELSE
        ttPriKat.LevNr = ''.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-settVaregruppe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE settVaregruppe Procedure 
PROCEDURE settVaregruppe :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  IF NOT AVAILABLE ttPriKat 
      THEN RETURN.
  FIND Strekkode NO-LOCK WHERE 
    Strekkode.Kode = ttPriKat.EANnr NO-ERROR.
  IF AVAILABLE Strekkode THEN 
    FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
  IF AVAILABLE ArtBas THEN 
  DO:
    IF CAN-FIND(VarGr OF ArtBas) THEN 
      ttPriKat.VareGruppe = STRING(ArtBas.Vg).
    ELSE
      ttPriKat.VareGruppe = cDefVg.   
  END.
  ELSE
    ttPriKat.VareGruppe = cDefVg.   

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

