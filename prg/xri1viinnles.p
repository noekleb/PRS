&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xhhvpiinnles.p
    Purpose     :

    Syntax      :

    Description : Leser inn vpi filen og omformer den til en pricat fil.

    Author(s)   : Tom Nøkleby
    Created     : 20/11-07
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
DEF VAR pcBkuFil      AS CHAR NO-UNDO.
DEFINE VARIABLE bLager   AS LOG NO-UNDO.
DEFINE VARIABLE iEtikett AS INTEGER NO-UNDO.
DEF VAR hPgmHandle    AS HANDLE NO-UNDO.
DEF VAR h_dvpifilhode AS HANDLE NO-UNDO.
DEF VAR h_dvpiartbas  AS HANDLE NO-UNDO.
DEF VAR h_PrisKo      AS HANDLE NO-UNDO.

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
DEFINE VARIABLE bTillatUgyldigeTegnIEAN AS LOG NO-UNDO.
DEFINE VARIABLE bGenSalgsenhet          AS LOG NO-UNDO.
DEFINE VARIABLE bGenButikk              AS LOG NO-UNDO.
DEFINE VARIABLE bDupliserTilHK          AS LOG NO-UNDO.
DEFINE VARIABLE bHK                     AS LOG NO-UNDO.
DEFINE VARIABLE iCL                     AS INTEGER NO-UNDO.
DEFINE VARIABLE bSjekkBestillingsnr AS LOG NO-UNDO.
DEFINE VARIABLE bDistribIPS             AS LOG NO-UNDO.
DEFINE VARIABLE cFilial                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cbutKatalog             AS CHARACTER NO-UNDO.
DEFINE VARIABLE iOverstyrLevNr          AS INT NO-UNDO.
DEFINE VARIABLE bUndertrykkNullbut      AS LOG NO-UNDO.
DEFINE VARIABLE bUndertrykkErrLogg      AS LOG NO-UNDO.
DEFINE VARIABLE bSkrivMsgLogg           AS LOG NO-UNDO.

DEF VAR cEDB-System   LIKE ImpKonv.EDB-System NO-UNDO.
DEF VAR cImpTabell    LIKE ImpKonv.Tabell     NO-UNDO.

DEFINE VARIABLE cOKVersion AS CHARACTER INIT "Rigal98,RIGAL02" NO-UNDO.
DEF VAR cRigalversion AS CHAR NO-UNDO.

DEF STREAM InnFil.
DEF STREAM UtVPI.
DEF STREAM UtVre.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr   AS INT
  FIELD Tekst     AS CHAR
  FIELD Gradering AS INT 
  .
DEFINE TEMP-TABLE tmpVare
    FIELD VareNr AS CHAR.
DEFINE TEMP-TABLE tmpRIGAL
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

{syspara.i 50 15 6 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bTillatUgyldigeTegnIEAN = TRUE.

{syspara.i 50 15 7 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bGenSalgsenhet = TRUE.

{syspara.i 50 15 9 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bGenButikk = TRUE.

{syspara.i 1 1 18 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bHK = TRUE.

{syspara.i 50 17 2 cTekst}
IF cTekst = '' OR CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bDupliserTilHK = TRUE.
ELSE
  bDupliserTilHK = FALSE.

{syspara.i 50 15 39 cTekst}
IF cTekst = '' OR CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bUndertrykkErrLogg = TRUE.
ELSE
  bUndertrykkErrLogg = FALSE.

{syspara.i 50 15 40 cTekst}
IF cTekst = '' OR CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bSkrivMsgLogg = TRUE.
ELSE
  bSkrivMsgLogg = FALSE.

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

/* Utlegg til PriCat på butikk = 0 skal ikke gjøres. */
IF lFilId < 0 THEN 
DO:
  ASSIGN 
  bUndertrykkNullbut = TRUE 
  lFilId             = ABS(lFilId).
END.

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
    
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

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
    cEDB-System = EkstVPILev.EDB-System
    iLevNr      = IF iOverstyrLevNr = 1 THEN EkstVPILev.LevNr ELSE 0.   

/* Leser inn filen i ttPrikat. */
RUN LesInnFil.

/* Eksporterer til VPI fil hvis det ikke er funnet feil med gradering under 50. */
IF NOT CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering < 50) OR 
   bAvbrytVedFeil = FALSE THEN
    RUN EksportVPIFil.

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
            VPIFilHode.VPIFilStatus = 5.
    FIND CURRENT VPIFilHode NO-LOCK.    
END.
IF AVAILABLE VPIFilHode THEN
    FIND CURRENT VPIFilHode NO-LOCK.

IF bAvbrytVedFeil = FALSE THEN
  RUN flyttFil. 
ELSE IF bAvbrytVedFeil AND NOT CAN-FIND(FIRST tt_Error) THEN
  RUN flyttFil. 
IF CAN-FIND(FIRST tt_Error) THEN
  RUN ErrorLogg.
RUN msgLogg.

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
IF bDistribIPS THEN
EKSPORT_BUTIKK:
DO:
  IF NOT CAN-FIND(FIRST tmpRIGAL) THEN 
    LEAVE EKSPORT_BUTIKK.
  ELSE RUN eksportButikk.  
END. /* EKSPORT_BUTIKK */

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-eksportButikk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE eksportButikk Procedure 
PROCEDURE eksportButikk :
/*------------------------------------------------------------------------------
                        Purpose:  Eksport av rigalfil pr. butikk.                                                                                                                                         
                        Notes:    Ved innlesning er de linjer som har butikknr. logget.
                                  Her legges det ut en fil som inneholder de linjer som 
                                  gjelder butikken, pr. butikk som er angitt i filen.                                                                                                                             
        ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cbutFilNavn AS CHARACTER NO-UNDO.
  
  cButFilNavn = 'V' + REPLACE(REPLACE(STRING(TODAY),'/',''),'-','') + REPLACE(REPLACE(STRING(TIME,"HH:MM:SS"),':',''),'-','') + '.'.
  
  BUTIKK:
  FOR EACH tmpRIGAL
    BREAK BY tmpRIGAL.ButikkNr:
    
    IF FIRST-OF(tmpRIGAL.ButikkNr) THEN
    DO:     
      OUTPUT STREAM UtVPI TO VALUE(ctmpKatalog + '~\' + cButFilNavn + STRING(tmpRIGAL.ButikkNr,">>>999")).
      PUT STREAM UtVPI UNFORMATTED 'RIGAL02,8.0' SKIP.
    END.
    
    PUT STREAM UtVPI UNFORMATTED tmpRIGAL.cLinje SKIP.
    
    IF LAST-OF(tmpRIGAL.ButikkNr) THEN
    DO: 
      OUTPUT STREAM UtVPI CLOSE.
      /* Flytter filen til ankommet katalogen */
      OS-COPY VALUE(ctmpKatalog + "~\" + cbutFilNavn + STRING(tmpRIGAL.ButikkNr,">>>999"))
         value(cbutKatalog + "~\" + cbutFilNavn + STRING(tmpRIGAL.ButikkNr,">>>999")).
      /* Renser bort temp fil */
      IF SEARCH(cbutKatalog + "~\" + cbutFilNavn + STRING(tmpRIGAL.ButikkNr,">>>999")) <> ? THEN
        OS-DELETE VALUE(ctmpKatalog + "~\" + cbutFilNavn + STRING(tmpRIGAL.ButikkNr,">>>999")).      
    END.
  END. /* BUTIKK */

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

IF iAntLinjer > 0 THEN
DO:
    EKSPORTFIL:                    
    FOR EACH ttPrikat WHERE
        (IF bUndertrykkNullbut 
           THEN ttPriKat.ButikkNr >  0
           ELSE ttPriKat.ButikkNr >= 0)
        BREAK BY ttPriKat.ButikkNr
              BY ttPriKat.LevModellNr
              BY ttPriKat.VareTekst     
              BY ttPriKat.FargeTekst    
              BY ttPriKat.SeqNrStr           
              BY ttPriKat.MarkedsPris:
        /* Er avdeling angitt, skal dette reflekteres i filnavn */
        IF FIRST-OF(ttPriKat.ButikkNr) THEN 
          DO:
            IF ttPriKat.ButikkNr > 0 THEN 
              ENTRY(2,cVPIUtFil,'.') = STRING(ttPriKat.ButikkNr).
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
        /* 89 */ ttPriKat.Lager ";" 
        /* 90 */ ttPriKat.Etikett ";"
        /* 91 */ ";" 
        /* 92 */ ";" 
        /* 93 */ ";" 
        /* 94 */ ";" 
        /* 95 */ ";" 
        /* 96 */ ";" 
        /* 97 */ ";" 
        /* 98 */ ttPriKat.Grunnsortiment ";" 
        /* 99 */ "ttPriKat.Opphav"                 
        SKIP      
        .

        OUTPUT STREAM UtVpi CLOSE.
        
        /* Behandler filen */
        IF LAST-OF(ttPriKat.ButikkNr) THEN 
        DO:
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
                bVPIFilHode.EkstVPILevNr = (IF ttPriKat.ButikkNr = 0 THEN 
                                             VPIFilHode.EkstVPILevNr
                                           ELSE (1000000 + ttPriKat.ButikkNr)) 
                .
              FIND CURRENT bVPIFilHode NO-LOCK.
            END. /* TRANSACTION */
            
            FIND FIRST EkstVPIFil NO-LOCK WHERE
              EkstVPIFil.EkstVPILevNr = VPIFilHode.EkstVPILevNr AND
              EkstVPIFil.VPIFilTypeNr = 1 AND
              EkstVPIFil.VPIFilNavn   BEGINS ENTRY(1,bVPIFilHode.FilNavn,'-') NO-ERROR.
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
              
            /* Renser bort GGVPI fil */
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
  DEF VAR cErrorFil AS CHAR NO-UNDO.

  /* Ingen utskrift */
  IF bUndertrykkErrLogg THEN 
    RETURN.
    
  ASSIGN
      cErrorFil = OS-GETENV('TMP') + '\' 
                 + "Error_" 
                 + VPIFilHode.FilNavn
                 + ".Txt".

  OUTPUT TO VALUE(cErrorFil).
    PUT UNFORMATTED
      "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn SKIP
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
            tt_Error.Gradering = 1:
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
            tt_Error.Gradering = 2:
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
            tt_Error.Gradering = 3:
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
            tt_Error.Gradering = 4:
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
            tt_Error.Gradering = 5:
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
            tt_Error.Gradering = 6:
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
            tt_Error.Gradering = 7:
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
            tt_Error.Gradering = 8:
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
            tt_Error.Gradering = 9:
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
            tt_Error.Gradering = 10:
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
            tt_Error.Gradering = 11:
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
            tt_Error.Gradering = 12:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.

    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_Error WHERE tt_Error.Gradering < 100) THEN
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
PROCEDURE flyttFil :
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
  DEFINE VARIABLE pcTekst       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE bErstattModus AS LOG       NO-UNDO.
  DEFINE VARIABLE pcOldChar     AS CHARACTER NO-UNDO.
  
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
        tt_Error.Tekst     = "** Feil på linje 1. IKKE gyldig Rigal-header"
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
      pcLinje    = TRIM(pcLinje,'"') /* Preem legger ut med " i første og siste posisjon. */
      .
    /* Den første linjen skal alltid inneholde RIGAL header. */
    /* Men vi skal også tåle at det ikke kommer header.      */
    IF iAntLinjer = 1 THEN NEXT.

    /* Tomme linjer fra Excel */
    IF pcLinje BEGINS ",,,,,,,,,,:" THEN
        NEXT LESERLINJER.
    /* Overskriftsrad 1 */
    IF pcLinje BEGINS "1,2,3,4,5,6:" THEN
        NEXT LESERLINJER.
    /* Overskriftsrad 2 */
    IF pcLinje BEGINS "Artik" THEN
        NEXT LESERLINJER.
    /* Sjekker transaksjonskoden */
    IF ENTRY(1,pcLinje) <> 'VAR' THEN
      DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil        = piAntFeil + 1
          tt_Error.LinjeNr = iAntLinjer
          tt_Error.Tekst   = "** Feil på linje " + string(iAntLinjer) + ". Feil kode (VAR). Den er " + ENTRY(1,pcLinje) + ".".
          tt_Error.Gradering = 2.
        NEXT.
      END.
      
    /* Tar hånd om tekstfelt som starter og slutter med " */
    /* Eksempeltekst: 20101231,"Norrl G 3,5% julpynt","™l 3,5% julprofil","™l 3,5% julprofil","6x50 cl",1,4,,3,34731000, */
    DO piLoop = 1 TO LENGTH(pcLinje):
      IF piLoop = 1 THEN pcOldChar = ''.
      ELSE pcOldChar = SUBSTRING(pcLinje,piLoop - 1,1).
    
      IF (SUBSTRING(pcLinje,piLoop,1) = '"' AND pcOldChar = ',') THEN
        bErstattmodus = TRUE.
      IF (SUBSTRING(pcLinje,piLoop,1) = ',' AND pcOldChar = '"') THEN
        bErstattmodus = FALSE.
        
      IF bErstattmodus AND SUBSTRING(pcLinje,piLoop,1) = ',' THEN
        OVERLAY(pcLinje,piLoop,1) = '.'.  
    END.

    /* Konvertering av EAN koder med pris eller vekt i kode. */
    IF TRIM(ENTRY( 3,pcLinje),'"') <> '' THEN
      DO:
        pcTekst = TRIM(ENTRY( 3,pcLinje),'"').
        RUN bibl_chkean.p (INPUT-OUTPUT pcTekst).
        ENTRY(3,pcLinje) = pcTekst.
      END. 

    /* Hvis rigalfilen skal splittes og sendes til butikk, logges linjene her. */
    /* Linjene logges og hver linje merkes med butikknr, slik at det blir      */
    /* enkelt å legge dem ut til respektive butikker.                          */
    /* Det er kun linjer som er merket med et avdelingsnr. som logges.         */  
    IF bDistribIPS THEN
    DISTRIB_BUT: 
    DO:
      ASSIGN cFilial = TRIM(LEFT-TRIM(TRIM(ENTRY(54,pcLinje),'"'),'0')) NO-ERROR.
      IF ERROR-STATUS:ERROR OR cFilial = '' THEN LEAVE DISTRIB_BUT.
      ASSIGN lDec = DECIMAL(cFilial) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN LEAVE DISTRIB_BUT.
      /* Hvis filial er prefikset med BS, skal dette strippes. */
      IF cFilial BEGINS 'BS' THEN cFilial = SUBSTRING(cFilial,3).
      /* Logger posten for distribusjon. */
      CREATE tmpRigal.
      ASSIGN
        tmpRigal.ButikkNr = INTEGER(cFilial)
        tmpRIGAL.cLinje   = pcLinje
        .
    END. /* DISTRIB_BUT */
    
    /* Er det for få entries på linjen, fyller vi på ... */
    IF NUM-ENTRIES(pcLinje) < 112 THEN
    DO:
      ASSIGN
        pcLinje = pcLinje + fill(",",112 - NUM-ENTRIES(pcLinje))
        .
    END.

    /* Linjevalidering.                                                 */
    /* Linjer med feil flagges, og blir ikke med vidre i oppdateringen. */
    /* De skrives ut i Error fil.                                       */
    RUN Linjevalidering.
    IF RETURN-VALUE <> "OK" THEN 
        NEXT.
    CREATE ttPriKat.
    ASSIGN
        ttPriKat.EkstVPILevNr     = VPIFilHode.EkstVPILevNr
        ttPriKat.LinjeNr          = piLinjeNr
        piLinjeNr                 = piLinjeNr  + 1
/*  1 */ ttPriKat.R1              = "R1"
/*  2 */ ttPriKat.LevNr           = TRIM(ENTRY(6,pcLinje),'"') 
/*  3 */ ttPriKat.LevModellNr     = TRIM(ENTRY( 4,pcLinje),'"')
/*  4 */ ttPriKat.EANnr           = TRIM(ENTRY( 3,pcLinje),'"')
/*  5 */ ttPriKat.VareTekst       = TRIM(ENTRY(10,pcLinje),'"')
         ttPriKat.Varetekst       = REPLACE(ttPriKat.VareTekst,';',',')
/*  6 */ ttPriKat.FargeKode       = "" /* Farge er ikke i bruk */
/*  7 */ ttPriKat.FargeTekst      = "" /* Settes blank */
/*  8 */ ttPriKat.Str             = " 1" /* Størrelse 1 */
/*  9 */ ttPriKat.StrTab          = "2" /* Størrelsestype 2 */
/* 10 */ ttPriKat.Varemerke       = TRIM(ENTRY(107,pcLinje),'"') 
/* 11 */ ttPriKat.Enh             = TRIM(ENTRY(13,pcLinje),'"')
         ttPriKat.Enh             = REPLACE(ttPriKat.Enh,';',',')
/* 12 */ ttPriKat.AntIEnh         = REPLACE(TRIM(ENTRY(15,pcLinje),'"'),'.',',') 
/* 13 */ ttPriKat.LevPrisEngros   = REPLACE(TRIM(ENTRY(20,pcLinje),'"'),'.',',') 
/* 14 */ ttPriKat.ValKod          = "NOK" 
/* 15 */ ttPriKat.forhRab%        = "" 
/* 16 */ ttPriKat.suppRab%        = "" 
/* 17 */ ttPriKat.VeilPris        = REPLACE(TRIM(ENTRY(23,pcLinje),'"'),'.',',') 
/* 18 */ ttPriKat.PAKstru         = "" 
/* 19 */ ttPriKat.LevUke1         = "" 
/* 20 */ ttPriKat.LevUke2         = "" 
/* 21 */ ttPriKat.LevUke3         = "" 
/* 22 */ ttPriKat.LevUke4         = "" 
/* 23 */ ttPriKat.VareGruppe      = TRIM(ENTRY(18,pcLinje),'"') 
/* 24 */ ttPriKat.LevNavn         = "" 
/* 25 */ ttPriKat.LevKod          = TRIM(ENTRY( 7,pcLinje),'"')
/* 26 */ ttPriKat.nettoForh       = "" 
/* 27 */ ttPriKat.kalkForh        = "" 
/* 28 */ ttPriKat.BFforh          = "" 
/* 29 */ ttPriKat.nettoSupp       = "" 
/* 30 */ ttPriKat.kalkSupp        = "" 
/* 31 */ ttPriKat.BFsupp          = "" 
/* 32 */ ttPriKat.MarkedsPris     = REPLACE(TRIM(ENTRY(22,pcLinje),'"'),'.',',') 
/* 33 */ ttPriKat.Sortiment       = "" 
/* 34 */ ttPriKat.Sesong          = "" 
/* 35 */ ttPriKat.VPIBildeKode    = "" 
/* 36 */ ttPriKat.Merknad         = "" 
/* 37 */ ttPriKat.KjedeValutaPris = "" 
/* 38 */ ttPriKat.KjedeProdusent  = ""  
/* 39 */ ttPriKat.ERPNr           = TRIM(TRIM(ENTRY( 7,pcLinje),'"'))
/* 40 */ ttPriKat.SalgsEnhetsType = TRIM(ENTRY(14,pcLinje),'"')
/* 41 */ ttPriKat.AktivFraDato    = '' /*TRIM(ENTRY( 8,pcLinje),'"')*/ /* Aktivering av pris */
/* 42 */ ttPriKat.AktivTilDato    = '' /* TRIM(ENTRY( 9,pcLinje),'"')*/ /* Deaktivering av pris */
/* 43 */ ttPriKat.Bongtekst       = TRIM(ENTRY(11,pcLinje),'"')
         ttPriKat.Bongtekst       = REPLACE(ttPriKat.Bongtekst,';',',')
/* 44 */ ttPriKat.Etikettekst1    = TRIM(ENTRY(12,pcLinje),'"')
         ttPriKat.Etikettekst1    = REPLACE(ttPriKat.Etikettekst1,';',',')
         ttPriKat.Etikettekst1    = (IF ttPriKat.Etikettekst1 = '' THEN ttPriKat.Varetekst ELSE ttPriKat.Etikettekst1)
/* 45 */ ttPriKat.Funksjonskode   = TRIM(ENTRY( 5,pcLinje),'"') /* N = "Normal", dvs. vare/prisendring, K = Kampanje, M = Medlemstilbud, U = Utmelding (tolkes som sletting), A = Slett ikke påbegynt kampanje, avslutt påbegynt kampanje */
/* 46 */ ttPriKat.Mva_Proc        = REPLACE(TRIM(ENTRY(45,pcLinje),'"'),'.',',')
/* 47 */ ttPriKat.LinkVare        = TRIM(TRIM(ENTRY(58,pcLinje),'"')) 
/* 48 */ ttPriKat.PantBelop       = REPLACE(TRIM(ENTRY(55,pcLinje),'"'),'.',',') /* Kommer det Y i dette feltet, skal posten stoppes for oppdatering. */
/* 49 */ ttPriKat.Filial          = TRIM(ENTRY(54,pcLinje),'"') 
/* 50 */ ttPriKat.Produsent       = TRIM(ENTRY(60,pcLinje),'"')
/* 51 */ ttPriKat.Mengde          = REPLACE(TRIM(ENTRY(79,pcLinje),'"'),'.',',') /* Konv. faktor jamførpris */
/* 52 */ ttPriKat.JamforEnhet     = TRIM(ENTRY(92,pcLinje),'"')
/* 53 */ ttPriKat.Kontrolleres    = (TRIM(ENTRY(55,pcLinje),'"') = 'Y' AND TRIM(ENTRY(58,pcLinje),'"') = '') /* Pant har Y som innhold. Da peker linknr (58) på panten.           */
                                                                                                             /* Har ikke (58) innhold, må linjen kontrolleres manuelt.            */
/* 98 */ ttPriKat.Grunnsortiment  = IF NUM-ENTRIES(pcLinje) >= 93 THEN TRIM(ENTRY(93,pcLinje),'"') ELSE ''
/* 99 */ ttPriKat.Opphav          = TRIM(ENTRY(71,pcLinje),'"') /* Opphav */                                                                                                             
/* 54  ttPriKat.ArtikkelNr      */
/* 55  ttPriKat.OpprettArtikkel */
         ttPriKat.Etikett         = iEtikett
         ttPriKat.Lager           = bLager 
        .      
     /* Hvis filial er prefikset med BS, skal dette strippes. */
     IF ttPriKat.Filial BEGINS 'BS' THEN 
       ttPriKat.Filial = SUBSTRING(ttPriKat.Filial,3).
     IF ttPriKat.GrunnSortiment = 'A' THEN 
       ttPriKAt.GrunnSortiment = 'TRUE'.
/*
MESSAGE 
'ttPriKat.EANnr' ttPriKat.EANnr SKIP        
'ttPriKat.VareTekst' ttPriKat.VareTekst SKIP    
'ttPriKat.LevPrisEngros' ttPriKat.LevPrisEngros SKIP 
'ttPriKat.VeilPris' ttPriKat.VeilPris SKIP   
'ttPriKat.MarkedsPris' ttPriKat.MarkedsPris 
VIEW-AS ALERT-BOX.
*/
     /* Konvertering av filial til butikknummer */
     IF cEDB-System <> '' AND 
       CAN-FIND(FIRST ImpKonv NO-LOCK WHERE
         ImpKonv.EDB-System = cEDB-System AND
         ImpKonv.Tabell     = 'Butiker') THEN 
     DO:
       FIND FIRST ImpKonv NO-LOCK WHERE
         ImpKonv.EDB-System = cEDB-System AND
         ImpKonv.Tabell     = 'Butiker' AND
         ImpKonv.EksterntId = LEFT-TRIM(ttPriKat.Filial,'0') NO-ERROR. 
       IF AVAILABLE ImpKonv THEN ttPriKat.Filial = ImpKonv.InterntId. /* Gjør ingenting */
       ELSE DO:
           CREATE tt_Error.
           ASSIGN
             tt_Error.LinjeNr   = iAntLinjer
             tt_Error.Tekst     = "** Butikk (" + ttPriKat.Filial + ") uten mapping i konverteringstabell: " + string(iAntLinjer) + ": " + pcLinje + "."
             tt_Error.Gradering = 12.
           IF bGenMapping THEN RUN opprettMapping (cEDB-System,'Butiker',STRING(iCl),LEFT-TRIM(ttPriKat.Filial,'0')). 
       END.
     END.
        
     /* Konverterer Janførenhet */
     CASE ttPriKat.JamforEnhet:
       WHEN  '1' THEN ttPriKat.JamforEnhet = 'stk'.
       WHEN  '2' THEN ttPriKat.JamforEnhet = 'kg'.
       WHEN  '3' THEN ttPriKat.JamforEnhet = 'l'.
       WHEN  '4' THEN ttPriKat.JamforEnhet = 'm'.
       WHEN  '5' THEN ttPriKat.JamforEnhet = 'm3'.
       WHEN  '6' THEN ttPriKat.JamforEnhet = 'm2'.
       WHEN  '7' THEN ttPriKat.JamforEnhet = '100m'.
       WHEN  '8' THEN ttPriKat.JamforEnhet = 'dos'.
       WHEN  '9' THEN ttPriKat.JamforEnhet = 'pors'.
       WHEN '10' THEN ttPriKat.JamforEnhet = 'tabl'.
       WHEN '11' THEN ttPriKat.JamforEnhet = 'beh'.
       WHEN '12' THEN ttPriKat.JamforEnhet = 'vask'.
       WHEN '13' THEN ttPriKat.JamforEnhet = 'par'.
       WHEN '14' THEN ttPriKat.JamforEnhet = 'hg'.
       OTHERWISE      ttPriKat.JamforEnhet = 'stk'.
     END CASE.
     
     IF bSkrivMsgLogg THEN 
     DO:
       CREATE tt_Error.
       ASSIGN
         tt_Error.LinjeNr   = 1
         tt_Error.Tekst     = pcLinje 
         tt_Error.Gradering = 100.
     END.
     
     /* Sikrer at det finnes en ekstVPILev. */
     lDec = 0.
     ASSIGN lDec = DECIMAL(ttPriKat.Filial) NO-ERROR.
     IF ERROR-STATUS:ERROR = FALSE AND CAN-FIND(Butiker WHERE Butiker.Butik = INTEGER(lDec)) THEN 
       RUN opprettEkstVPILev (INTEGER(ttPriKat.Filial)).
     
     /* Er filial angitt og denne matcher med et butikknummer, skal butikknr settes i prikat filen. */
     /* Det skal alltid legges opp en post i HK's VPI lomme, og en i butikkens egen lomme.          */
     /* Posten som legges i hk's VPI lomme, skal ha hk's utpris og den nye innprisen. Og posten som */
     /* legges i butikkens VPI lomme, skal ha den nye utpris og hk's innpris.                       */
     IF bDupliserTilHK 
        AND lDec > 0 
        AND lDec <> iCL 
        AND CAN-FIND(Butiker WHERE Butiker.Butik = INTEGER(lDec)) THEN 
     DO:
       IF AVAILABLE hkArtPris  THEN RELEASE hkartPris.
       IF AVAILABLE Strekkode  THEN RELEASE Strekkode.
       
       /* Henter artikkelnr og ArtPris for profilene. */
       IF bSjekkBestillingsnr THEN 
         FIND FIRST Strekkode NO-LOCK WHERE 
           Strekkode.BestillingsNummer = ttPriKat.LevKod NO-ERROR.
       IF NOT AVAILABLE Strekkod THEN 
         FIND Strekkode NO-LOCK WHERE
           Strekkode.Kode = ttPriKat.EANnr NO-ERROR.
       
       /* Henter HK's kalkyle hvis den finnes.      */
       IF AVAILABLE Strekkode THEN 
         FIND hkArtPris NO-LOCK WHERE 
           hkArtPris.ArtikkelNr = Strekkode.ArtikkelNr AND 
           hkArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.    
       
       /* Til HK's VPI lomme. */
       CREATE bttPriKat.
       BUFFER-COPY ttPriKat TO bttPriKat 
                 ASSIGN
                   bttPriKat.MarkedsPris = (IF AVAILABLE hkArtPris 
                                                THEN STRING(hkArtPris.Pris[1]) 
                                                ELSE bttPriKat.Markedspris)
                   bttPriKat.Filial = ''.
       /* Og denne til butikkens VPI lomme. */ 
       ASSIGN
         ttPriKat.LevPrisEngros = (IF AVAILABLE hkArtPris 
                                     THEN STRING(hkArtPris.InnkjopsPris[1]) 
                                     ELSE ttPriKat.LevPrisEngros)
         ttPriKat.ButikkNr = INTEGER(lDec).
       RUN opprettEkstVPILev (INTEGER(lDec)).
     END.
     /* Ved innlesning av IPS fil, er filalnr angitt. Da skal enventuelle endringer i innpris oppdateres direkte mot ArtBas/ArtPris. */
     IF ttPriKat.ButikkNr > 0 THEN 
       ASSIGN ttPriKat.PosterPrisending = TRUE.  
     ELSE   
       ASSIGN ttPriKat.PosterPrisending = FALSE.  
    
     IF (iAntLinjer MODULO 100 = 0) OR (iAntLinjer = iTotAntLinjer) THEN 
       STATUS DEFAULT "EDI import - leser linje " + 
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

  DEFINE BUFFER bufSalgsenhet FOR Salgsenhet.
  DEFINE BUFFER bufProdusent  FOR Produsent.

  ASSIGN 
    cRetValue = 'OK'.
      
  /* Sjekker om det er funksjonskode. */
  IF NOT CAN-DO('N,K,M,U,A',TRIM(ENTRY( 5,pcLinje),'"')) THEN 
  DO:
    CREATE tt_Error.
    ASSIGN
      tt_Error.LinjeNr   = iAntLinjer
      tt_Error.Tekst     = "** Ukjent funksjonskode (" + trim(ENTRY( 5,pcLinje),'"') + ") på linje: " + string(iAntLinjer) + ": " + pcLinje + "."
      tt_Error.Gradering = 11
      cRetValue          = 'FEIL'.
  END.

  RETURN cRetValue.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-msgLogg) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE msgLogg Procedure
PROCEDURE msgLogg:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
  DEF VAR cMsgFil AS CHAR NO-UNDO.

  /* Ingen utskrift */
  IF bSkrivMsgLogg = FALSE THEN 
    RETURN.
  IF NOT CAN-FIND(FIRST tt_Error WHERE
              tt_Error.Gradering = 100) THEN
    RETURN.
    
  ASSIGN
      cMsgFil = OS-GETENV('TMP') + '\' 
                 + "Msg_" 
                 + VPIFilHode.FilNavn
                 + ".Txt".

  OUTPUT TO VALUE(cMsgFil).
    PUT UNFORMATTED
      "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      "VPI fil mottatt: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn SKIP
      .

    /* Feil som avbryter innlesning av fil. */
    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 100) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** MOTTATTE ARTIKLER ***" SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 100:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.
  
  OUTPUT CLOSE.
  IF SEARCH(cMsgFil) <> ? THEN
  DO:
    DEF VAR hInstance AS INT.

    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH(cMsgFil),
                                  "",
                                  1,
                                  OUTPUT hInstance).

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
PROCEDURE opprettEkstVPILev :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER piEkstVPILevNr LIKE EkstVPILev.EkstVPILevNr NO-UNDO.
  
    DEFINE BUFFER bEkstVPIFil FOR EkstVPIFil.
    DEFINE BUFFER bEkstVPILev FOR EkstVPILev.
  
    /* HK's import av ukjent vare */
    IF piEkstVPILevNr > 0 AND piEkstVPILevNr < 1000000 THEN 
    HK-UKJENT-VARE:
    DO: 
        ASSIGN
          piekstVPILevNr = 1000000 + piEkstVPILevNr.

        /* Korreksjonsmelding fra HK til butikk legges her.             */
        /* Denne leverandøren skal være passiv på HK og aktiv i butikk. */
        IF NOT CAN-FIND(bEkstVPILev WHERE
                        bEkstVPILEv.EkstVPILevNr = piEkstVPILevNr) THEN
        DO:
            CREATE bEkstVPILev.
            ASSIGN
                bEkstVPILev.EkstVPILevNr = piEkstVPILevNr
                bEkstVPILev.KortNavn     = "Ukjent vare"
                bEkstVPILev.Navn         = "Ukjent vare fra ERP"
                bEkstVPILev.AktivLev     = TRUE
                bEkstVPILev.LevNr        = iLevNr
                bekstVPILev.EDB-System   = cEDB-System
                .
            /* Oppretter et datasett */
            IF NOT CAN-FIND(FIRST VPIDatasett WHERE
                            VPIDatasett.EkstVPILevNr = bEkstVPILev.EkstVPILevNr) THEN
            DO:
                CREATE VPIDatasett.
                ASSIGN
                    VPIDatasett.EkstVPILevNr = bEkstVPILev.EkstVPILEvNr.
            END.
        END.
        /* VPIFil */
        IF NOT CAN-FIND(bEkstVPIFil WHERE
                        bEkstVPIFil.EkstVPILevNr = piEkstVPILevNr AND
                        bEkstVPIFil.VPIFilNr     = 1) THEN
        DO:
            CREATE bEkstVPIFil.
            ASSIGN
                bEkstVPIFil.EkstVPILEvNr          = piEkstVPILevNr 
                bEkstVPIFil.VPIFilNr              = 1
                bEkstVPIFil.VPIFilType            = 2
                bEkstVPIFil.VPIFilBeskrivelse     = "Ukjent varer"
                bEkstVPIFil.VPIFilNavn            = "POSVPI"
                bEkstVPIFil.VPIEkst               = STRING(piEkstVPILevNr - 1000000)
                bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
                bEkstVPIFil.VPIInnlesningsrutine  = "xhkvpiinnles"
                bEkstVPIFil.VPIUtpakkingsrutine   = "xhkvpiutpakk"
                bEkstVPIFil.VPIOperator           = 2
                bEkstVPIFil.VPIFilAktiv           = bHK /* Aktiv på hk, passiv i butikk */
                .
            RELEASE bEkstVPIFil.
        END.
    END. /* HK-UKJENT-VARE */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-opprettMapping) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprettMapping Procedure 
PROCEDURE opprettMapping :
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
PROCEDURE opprettSalgsenhet :
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

  ASSIGN
      iTotAntLinjer = -1 /* Första linjen är en header */
      .
  INPUT STREAM InnFil FROM VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) NO-ECHO.
  IMPORT STREAM InnFil UNFORMATTED cLinje.
  cLinje = TRIM(cLinje,'"').
  IF NOT CAN-DO(cOKVersion,ENTRY(1,cLinje)) THEN DO:
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

