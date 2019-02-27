&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xNovas_Til_PRSPricat.p
    Purpose     :

    Syntax      :

    Description : Leser inn vpi Novas pricat filen og omformer den til en PRS pricat fil.

    Author(s)   : Tom Nøkleby
    Created     : 25/9-18
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.
DEFINE VARIABLE hStatus AS HANDLE NO-UNDO.
DEFINE VARIABLE iSheets AS INTEGER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.
DEF VAR cExcelFilNavn AS CHAR NO-UNDO.
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
DEF VAR cBkuFil AS CHAR NO-UNDO.
DEF VAR cBkuFil2 AS CHAR NO-UNDO.
DEFINE VARIABLE cEDB-System AS CHARACTER NO-UNDO.
DEF VAR cDato  AS CHAR NO-UNDO.
DEF VAR dDato  AS DATE FORMAT "99/99/9999" NO-UNDO.
DEF VAR iUkeNr AS INT  NO-UNDO.
DEF VAR iArUke AS INT  FORMAT ">>999999" NO-UNDO.

/* for TEST */
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.

DEF VAR h_vartkor     AS HANDLE NO-UNDO.
DEF VAR hField1       AS HANDLE NO-UNDO.
DEF VAR c02SjekkListe AS CHAR NO-UNDO.
DEF VAR cGenEan       AS CHAR NO-UNDO.
DEFINE VARIABLE cDefVg AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.

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

ON CLOSE OF THIS-PROCEDURE 
DO:
  IF VALID-HANDLE(hStatus) THEN
    DELETE PROCEDURE hStatus.
END.

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
SUBSCRIBE 'VPIFilLogg' ANYWHERE.

rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).

IF NOT VALID-HANDLE(hStatus) THEN
    RUN VisStatusMsg.w PERSISTENT SET h_PrisKo.

ASSIGN 
    bTest = TRUE 
    cLogg = 'xNovas_Til_PRSPricat' + REPLACE(STRING(TODAY),'/','')
    .

IF bTest THEN 
    RUN bibl_loggDbFri.p (cLogg,
        'Start xNovas_Til_PRSPricat.p'
        ).
    
FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    IF bTest THEN 
        RUN bibl_loggDbFri.p (cLogg,
            '** Ukjent VPIFilHode post' + STRING(lFilId) + "). Programmet avsluttes."
            ).
    
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn
    .

FIND EkstVPILev NO-LOCK WHERE 
    EkstVPILev.EkstVPILevNr = VPIFilHode.EkstVPILevNr NO-ERROR.
IF AVAILABLE EkstVPILev THEN 
    ASSIGN 
        iLevNr      = EkstVPILev.LevNr
        cEDB-System = EkstVPILev.EDB-System
        .
IF iLevNr = 0 THEN 
DO:
    IF bTest THEN 
        RUN bibl_loggDbFri.p (cLogg,
            " ** VPILev er ikke koblet mot leverandør. (" + STRING(VPIFilHode.EkstVPILevNr) + "). Programmet avsluttes."
            ).
    
    RETURN " ** VPILev er ikke koblet mot leverandør. (" + STRING(VPIFilHode.EkstVPILevNr) + ").".    
END.     

{syspara.i 2 4 8 cGenEan}

ASSIGN
    ctmpKatalog = SESSION:TEMP-DIRECTORY
    cVPIFil     = "_PRSPricat" + STRING(VPIFilHode.EkstVPILevNr) + "-" + REPLACE(STRING(TODAY,"99-99-9999"),'/','') + "-" + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + ".csv"
    cVPIUtFil   = SUBSTRING(cVPIFil,2)
    .

IF bTest THEN DO: 
    RUN bibl_loggDbFri.p (cLogg,
        '  Fil: ' + VPIFilHode.FilNavn
        ).
    RUN bibl_loggDbFri.p (cLogg,
        '  EkstVPILevNr: ' + STRING(VPIFilHode.EkstVPILevNr)
        ).
END.

/* Er det en excel fil, skal den kovnerteres til csv. */
IF CAN-DO('xls,xlsx',ENTRY(NUM-ENTRIES(cFilNavn,'.'),cFilNavn,'.')) THEN 
DO:
    cExcelFilNavn = cFilNavn.
    PUBLISH 'visStatusMsg' ('Konvertert excel fil fra: ' + cExcelFilNavn + ' til ' + cFilNavn + '.').    
    rStandardFunksjoner:konvExcel2csv(cExcelFilNavn,'',OUTPUT cFilNavn, OUTPUT iSheets).
    RUN bibl_loggDbFri.p (cLogg,
        '  Konvertert excel fil fra: ' + cExcelFilNavn + ' til ' + cFilNavn + '.'
        ).    
    PUBLISH 'visStatusMsg' ('Filkonvertering ferdig.').    
END.

RUN lesInnPricat.

/* Eksporterer til VPI fil. */
RUN EksportVPIFil.

/* Leser inn og oppdaterer. */
RUN LesInnOppdater.

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
    cBkuFil = VPIFilHode.Katalog + "~\bku" + "\" + 
             VPIFilHode.FilNavn
    .
ASSIGN
    cBkuFil2 = VPIFilHode.Katalog + "~\bku" + "\" + 
             VPIFilHode.FilNavn
    cBkuFil2 = REPLACE(cBkuFil2,'.xlsx','.csv')         
    cBkuFil2 = REPLACE(cBkuFil2,'.xls','.csv')         
    .

/* VPIFILEN - Excel*/
/* Sikrer at backup katalog finnes. */
OS-CREATE-DIR value(VPIFilHode.Katalog + "~\bku").
/* Flytter filen til backup katalog. */
OS-COPY value(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) 
    value(cBkuFil).
/* Renser bort fil */
IF SEARCH(cBkuFil) <> ? THEN
DO:
    /* Filen tas bort fra katalogen. */
    IF SEARCH(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) <> ? THEN
        OS-DELETE VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn).
END.

/* VPIFILEN - CSV*/
/* Flytter filen til backup katalog. */
OS-COPY value(cFilNavn) 
    value(cBkuFil2).
/* Renser bort fil */
IF SEARCH(cBkuFil2) <> ? THEN
DO:
    /* Filen tas bort fra katalogen. */
    IF SEARCH(cFilNavn) <> ? THEN
        OS-DELETE VALUE(cFilNavn).
END.

/* Stopper innlesningsprogram . */
IF VALID-HANDLE(hPgmHandle) THEN
    DELETE PROCEDURE hPgmHandle.
IF VALID-HANDLE(h_dvpifilhode) THEN
    DELETE PROCEDURE h_dvpifilhode.
IF VALID-HANDLE(h_dvpiartbas) THEN
    DELETE PROCEDURE h_dvpiartbas.
IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_PrisKo.

IF bTest THEN 
    RUN bibl_loggDbFri.p (cLogg,
        'Ferdig xNovas_Til_PRSPricat.p'
        ).

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

IF bTest THEN 
    RUN bibl_loggDbFri.p (cLogg,
        '  Start EksportVPIFil'
        ).

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
            /* 13 */ REPLACE(ttPriKat.LevPrisEngros,'.',',') ";"
             
            /* 14 */ ttPriKat.ValKod ";"        
            /* 15 */ REPLACE(ttPriKat.forhRab%,'.',',') ";"      
            /* 16 */ REPLACE(ttPriKat.suppRab%,'.',',') ";"      
            /* 17 */ REPLACE(ttPriKat.VeilPris,'.',',') ";"      
            
            /* 18 */ ttPriKat.PAKstru ";"       
            /* 19 */ ttPriKat.LevUke1 ";"       
            /* 20 */ ttPriKat.LevUke2 ";"       
            /* 21 */ ttPriKat.LevUke3 ";"       
            /* 22 */ ttPriKat.LevUke4 ";"       
            /* 23 */ ttPriKat.VareGruppe ";"    
            /* 24 */ ttPriKat.LevNavn ";"       
            /* 25 */ ttPriKat.LevKod ";"  
            /* 26 */ REPLACE(ttPriKat.nettoForh,'.',',') ";"     
            /* 27 */ REPLACE(ttPriKat.kalkForh,'.',',') ";"      
            /* 28 */ REPLACE(ttPriKat.BFforh,'.',',') ";"        
            /* 29 */ REPLACE(ttPriKat.nettoSupp,'.',',') ";"     
            /* 30 */ REPLACE(ttPriKat.kalkSupp,'.',',') ";"      
            /* 31 */ REPLACE(ttPriKat.BFsupp,'.',',') ";"        
            /* 32 */ REPLACE(ttPriKat.MarkedsPris,'.',',') ";"   
            
            /* 33 */ ttPriKat.Sortiment ";"     
            /* 34 */ ttPriKat.Sesong ";"        
            /* 35 */ ttPriKat.VPIBildeKode ";"
            /* 36 */ ttPriKat.Merknad ";"     
            /* 37 */ REPLACE(ttPriKat.KjedeValutaPris,'.',',') ";" 
            
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
            /* 48 */ REPLACE(ttPriKat.PantBelop,'.',',') ";"
            
            /* 49 */ ttPriKat.Filial ";"
            /* 50 */ ttPriKat.Produsent ";"
            /* 51 */ ttPriKat.Mengde ";"
            /* 52 */ ttPriKat.JamforEnhet ";"
            /* 53 */ ttPriKat.Kontrolleres ";" /* Dette feltet settes hvis posten skal legges med behandlingstatus 'på vent'. */
            /* 54 */ ttPriKat.ArtikkelNr ";"
            /* 55 */ ttPriKat.OpprettArtikkel ";" 
            /* 56 */ ttPriKat.PosterPrisending ";" 
            /* 57 */ REPLACE(STRING(ttPriKat.KjedeRab%,"->>>>>9.99"),'.',',') ";" 
            /* 58 */ REPLACE(STRING(ttPriKat.KjedeSupRab%,"->>>>>9.99"),'.',',') ";"
            /* 59 - 71 importeres p.t. ikke */
            /* 62 */ ttPriKat.Karakteristikk ";"
            /* 65 */ ttPriKat.Alder ";" 
            /* 72 */ ttPriKat.EkstStrTypeNavn ";" 
            /* 73 - 79 importeres p.t. ikke */
            /* 76 */ ttPriKat.AlfaKode2 ";"  
            /* 80 */ REPLACE(STRING(ttPriKat.KjedeInnkPris,"->>>>>9.99"),'.',',') ";" 
            /* 81 */ REPLACE(STRING(ttPriKat.KjedeSupInnkPris,"->>>>>9.99"),'.',',') ";" 
            /* 82 - 88 importeres p.t. ikke */
            /* 89 */ ttPriKat.Etikett ";" 
            /* 90 */ ttPriKat.Lager ";" 
            /* 91 */ ttPriKat.Sortimentkoder ";" 
            /* 92 */ ttPriKat.Lagerkoder ";" 
            /* 93 */ ttPriKat.Gjennomfaktureres ";" 
            /* 94 */ ttPriKat.KjedeVare ";" 
            /* 95 */ ttPriKat.Kampanjeuker ";" 
            /* 96 */ ttPriKat.Kampanjestotte ";"
            /* 97 */ ttPriKat.BehStatus ";"   
            /* 98 */ ttPriKat.Grunnsortiment ";" 
            /* 99 */ ttPriKat.Opphav ";" /* 0-Pricat, 1-Excel GUI import, 2-RIGAL, 3-RIGAL IPS, 4-EDI ARTIKEL, 5-EDI IPS ARTIKEL, 6-XML VPI import*/
            /*100 */ ttPriKat.RAvdNr ";" /* Vareområde */  
            /*101 */ ttPriKat.OrgFilNavn ";" /* Vareområde */  
            /*102 */ ttPriKat.LoggFilNavn ";" /* Navn på loggfil */  
            /*103 */ ttPriKat.Etikettekst2 ";"
            /*104 */ ttPriKat.ArtSlag ";" 
            /*105 */ ttPriKat.OPris ";" 
            /*106 */ ttPriKat.NON_Sale ";" 
            /*107 */ ttPriKat.NegVare ";" 
            /*108 */ ttPriKat.Pant ";" 
            /*109 */ ttPriKat.Telefonkort ";" 
            /*110 */ ttPriKat.WebButikkArtikkel ";" 
            /*111 */ ttPriKat.PubliserINettbutikk ";" 
            /*112 */ ttPriKat.HoyLav ";" 
            /*113 */ ttPriKat.WebLeveringstid ";" 
            /*114 */ ttPriKat.WebMinLager ";" 
            /*115 */ ttPriKat.KampanjeKode ";" 
            /*116 */ ttPriKat.LopNr ";" 
            /*117 */ ttPriKat.GarantiKl ";"
            /*118 */ ttPriKat.PostBredde ";"
            /*119 */ ttPriKat.PostHoyde ";"
            /*120 */ ttPriKat.PostLengde ";"
            /*121 */ ttPriKat.PostVekt ";"
            /*122 */ ttPriKat.AntLinkVare
            
        SKIP.

    END. /* EKSPORTFIL */
    OUTPUT STREAM UtVpi CLOSE.

    /* Flytter filen til ankommet katalogen */
    OS-COPY VALUE(ctmpKatalog + "~\" + cVPIUtFil)
            value(VPIFilHode.Katalog + "~\" + cVPIUtFil).
    /* Renser bort temp fil */
    IF SEARCH(VPIFilHode.Katalog + "~\" + cVPIUtFil) <> ? THEN
        OS-DELETE VALUE(ctmpKatalog + "~\" + cVPIUtFil).
END.

IF bTest THEN 
    RUN bibl_loggDbFri.p (cLogg,
        '  Ferdig EksportVPIFil'
        ).

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

&IF DEFINED(EXCLUDE-LesInnOppdater) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnOppdater Procedure
PROCEDURE LesInnOppdater:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF VAR plFilId      AS DEC NO-UNDO.
    DEF VAR pbOk         AS LOG NO-UNDO.
    DEF VAR piAntLinjer  AS INT NO-UNDO.
    DEF VAR plArtikkelNr AS DEC NO-UNDO.

    DEF BUFFER bVPIFilHode  FOR VPIFilHode.
    DEF BUFFER b2VPIFilHode FOR VPIFilHode.

    IF bTest THEN 
        RUN bibl_loggDbFri.p (cLogg,
            '  Start LesInnOppdatert'
            ).

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
            FIND CURRENT b2VPIFilHode NO-LOCK NO-ERROR.
        END.
        IF NOT VALID-HANDLE(h_PrisKo) THEN
            RUN prisko.p PERSISTENT SET h_PrisKo.
        IF NOT VALID-HANDLE(h_dvpiartbas) THEN
        DO:
            RUN dvpiartbas.w PERSISTENT SET h_dvpiartbas.
            RUN SettAutoImport IN h_dvpiartbas (INPUT TRUE).
        END.

        IF bTest THEN 
            RUN bibl_loggDbFri.p (cLogg,
                '  LesInnFil ') 
                ).
        /* Leser inn fillinjene */
        RUN xPRSPricatInnles 
            (INPUT  plFilId,
            INPUT  THIS-PROCEDURE,
            OUTPUT piAntLinjer
            ).            

        IF bTest THEN 
            RUN bibl_loggDbFri.p (cLogg,
                '  Pakk ut fil '). 
                                   
        /* Pakker ut fil. */
        RUN xPRSPricatUtpakk (INPUT plFilId).

        /* VPIFILEN */
        /* Sikrer at backup katalog finnes. */
        OS-CREATE-DIR value(b2VPIFilHode.Katalog + "~\bku").
        /* Flytter filen til backup katalog. */
        OS-COPY value(b2VPIFilHode.Katalog + "~\" + b2VPIFilHode.FilNavn) 
            VALUE(b2VPIFilHode.Katalog + "~\bku\" + b2VPIFilHode.FilNavn).
        /* Renser bort fil */
        IF SEARCH(b2VPIFilHode.Katalog + "~\bku\" + b2VPIFilHode.FilNavn) <> ? THEN
        DO:
            /* Filen tas bort fra katalogen. */
            OS-DELETE VALUE(b2VPIFilHode.Katalog + "~\" + b2VPIFilHode.FilNavn).
        END.

/*        /* Oppretter alle nye poster */                                                                       */
/*        /*RUN OpprettUtvalg.*/                                                                                */
/*        FOR EACH VPIArtBas NO-LOCK WHERE                                                                      */
/*            VPIArtBas.EkstVPILevNr = VPIFilHode.EkstVPILevNr AND                                              */
/*            VPIArtBas.VPIDato      = TODAY AND                                                                */
/*            NOT CAN-FIND(FIRST ttPriKat WHERE                                                                 */
/*                         ttPriKat.EkstVPILevNr = VPIFilHode.EkstVPILevNr AND                                  */
/*                         ttPriKat.ArtikkelNr   = VPIArtBas.ArtikkelNr AND                                     */
/*                         ttPriKat.OpprettArtikkel = TRUE):                                                    */
/*            RUN OpprettNy    IN h_dvpiartbas (VPIFilHode.EkstVPILevNr, VPIArtBas.VareNr, OUTPUT plArtikkelNr).*/
/*            RUN OppdaterInfo IN h_dvpiartbas (VPIFilHode.EkstVPILevNr, VPIArtBas.VAreNr, plArtikkelNr).       */
/*            RUN OppdaterPris IN h_dvpiartbas (VPIFilHode.EkstVPILevNr, VPIArtBas.VAreNr, plArtikkelNr).       */
/*            FIND ArtBas NO-LOCK WHERE                                                                         */
/*                ArtBas.ArtikkelNr = DEC(VPIArtBAs.ArtikkelNr) NO-ERROR.                                       */
/*            IF AVAILABLE ArtBas THEN                                                                          */
/*                RUN KlargjorPrisKoEn IN h_PrisKo (ROWID(ArtBas)).                                             */
/*            ELSE MESSAGE "Finner ikke artbas" VPIArtBAs.ArtikkelNr VIEW-AS ALERT-BOX.                         */
/*                                                                                                              */
/*            DO TRANSACTION:                                                                                   */
/*                FIND ArtBas EXCLUSIVE-LOCK WHERE                                                              */
/*                    ArtBas.ArtikkelNr = DEC(VPIArtBAs.ArtikkelNr) NO-ERROR.                                   */
/*                IF (AVAILABLE ArtBas AND ArtBas.OPris = FALSE) THEN                                           */
/*                    ArtBas.Lager = TRUE.                                                                      */
/*                IF AVAILABLE ArtBas THEN                                                                      */
/*                    RELEASE ArtBas.                                                                           */
/*            END.                                                                                              */
/*        END.                                                                                                  */
    END. /* LES-INN-OPPDATER */

    IF bTest THEN 
        RUN bibl_loggDbFri.p (cLogg,
            '  Ferdig LesInnOppdatert'
            ).

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-lesInnPricat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesInnPricat Procedure
PROCEDURE lesInnPricat:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF VAR piLinjeNr AS INT  NO-UNDO.
    DEF VAR piAntFeil AS INT  NO-UNDO.
    DEF VAR plVareNr  AS DEC  NO-UNDO.
    DEF VAR pcStr     AS CHAR NO-UNDO.
    DEF VAR piLoop    AS INT  NO-UNDO.
  
    IF bTest THEN 
        RUN bibl_loggDbFri.p (cLogg,
            '  Start lesInnPricat'
            ).
  
    /* Tømmer temp tabeller. */
    EMPTY TEMP-TABLE tt_Error.
    EMPTY TEMP-TABLE ttPriKat.
  
    RUN TellOppLinjer.

    ASSIGN
        piLinjeNr = 1.
    iAntLinjer = 0
        .
    INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
    LESERLINJER:
    REPEAT:
        /* Leser linje fra filen */
        IMPORT STREAM InnFil UNFORMATTED pcLinje.

        ASSIGN
            pcLinje = REPLACE(pcLinje,CHR(9),';')
            iAntLinjer = iAntLinjer + 1
            .

        /* Tomme linjer */
        IF pcLinje = '' THEN 
            NEXT LESERLINJER.
        /* Tomme linjer fra Excel */
        IF pcLinje BEGINS ";;;;;:" THEN
            NEXT LESERLINJER.
        /* OVerskriftsrad 1 */
        IF pcLinje BEGINS "1;2;3;4;5;6;" THEN
            NEXT LESERLINJER.
        /* OVerskriftsrad 2 */
        IF pcLinje BEGINS "It" THEN
            NEXT LESERLINJER.
        /* Ugyldig innhold i EAN kode felt*/
        lDec = DEC(TRIM(ENTRY( 4,pcLinje,";"),'"')) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            NEXT LESERLINJER.
        /* Ugyldig innhold - 0 i EAN kode felt*/
        lDec = DEC(TRIM(ENTRY( 4,pcLinje,";"),'"')) NO-ERROR.
        IF lDec = 0 THEN 
            NEXT LESERLINJER.

        RUN bibl_loggDbFri.p (cLogg,
            '  Linje ' + string(iAntLinjer) + ': ' + pcLinje 
            ).

        /* Fyller på tomme kolonner. */
        IF NUM-ENTRIES(pcLinje,";") < 46 THEN
        DO:
            ASSIGN
                piAntFeil = piAntFeil + 1
                pcLinje   = pcLinje + ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
                .
        END.

        /* Oppretter og fyller inn Pricat recorden. */
        CREATE ttPriKat.
        ASSIGN
            ttPriKat.EkstVPILevNr    = VPIFilHode.EkstVPILevNr
            ttPriKat.LinjeNr         = piLinjeNr
            piLinjeNr                = piLinjeNr  + 1
            /*               */ 
            ttPriKat.R1              = TRIM(ENTRY( 1,pcLinje,";"),'"')
            /*               */ 
            ttPriKat.LevNr           = STRING(iLevNr)
            /* nArtno        */ 
            ttPriKat.LevModellNr     = TRIM(ENTRY( 3,pcLinje,";"),'"')                  
            /* cEANCode      */ 
            ttPriKat.EANnr           = TRIM(ENTRY( 4,pcLinje,";"),'"')
            /* rArtName      */ 
            ttPriKat.VareTekst       = REPLACE(REPLACE(TRIM(ENTRY( 5,pcLinje,";"),'"'),CHR(10),''),CHR(13),'')
            ttPriKat.VareTekst       = REPLACE(ttPriKat.VareTekst,';',',')
            /* NColCode      */ 
            ttPriKat.FargeKode       = TRIM(ENTRY(6,pcLinje,";"),'"')
            /* cColName      */ 
            ttPriKat.FargeTekst      = TRIM(ENTRY(7,pcLinje,";"),'"')
            ttPriKat.FargeTekst      = REPLACE(ttPriKat.FargeTekst,';',',')
            /* cCode1        */ 
            ttPriKat.Str             = TRIM(ENTRY(8,pcLinje,";"),'"')
            /* nOrder        */ 
            ttPriKat.SeqNrStr        = 0 
            /* nSeason(3) + nSizeCode(3) */
            ttPriKat.StrTab          = TRIM(ENTRY(9,pcLinje,";"),'"')
            /* nMainGroup    */ 
            ttPriKat.Varemerke       = TRIM(ENTRY(10,pcLinje,";"),'"')
            /*               */ 
            ttPriKat.Enh             = TRIM(ENTRY(11,pcLinje,";"),'"')
            /*               */ 
            ttPriKat.AntIEnh         = TRIM(ENTRY(12,pcLinje,";"),'"')

            /* NWholeSaleNet */ 
            ttPriKat.LevPrisEngros   = REPLACE(TRIM(TRIM(ENTRY(13,pcLinje,";"),'"'),"%"),' ','')
            /*               */ 
            ttPriKat.ValKod          = 'NOK'
            /*               */ 
            ttPriKat.forhRab%        = REPLACE(TRIM(TRIM(ENTRY(15,pcLinje,";"),'"'),"%"),' ','')
            /*               */ 
            ttPriKat.suppRab%        = REPLACE(TRIM(TRIM(ENTRY(16,pcLinje,";"),'"'),"%"),' ','')
            /* nRetailPrice  */ 
            ttPriKat.VeilPris        = TRIM(ENTRY(17,pcLinje,";"),'"')
            /* Sesong */
            ttPriKat.Sesong          = TRIM(ENTRY(25,pcLinje,";"),'"')
            /*               */ 
            ttPriKat.PAKstru         = ""
            /* nArtgroup + nSubGroup */
            ttPriKat.VareGruppe      = TRIM(ENTRY(24,pcLinje,";")) 
            ttPriKat.VareGruppe      = (IF ttPriKat.VareGruppe = '' THEN TRIM(ENTRY(23,pcLinje,";")) ELSE ttPriKat.VareGruppe) 
            /*               */ 
            ttPriKat.LevNavn         = ""
            /* nArtno        */ 
            ttPriKat.LevKod          = ttPriKat.LevModellNr                  
            /* NWholeSaleNet */ 
            ttPriKat.nettoForh       = ""
            /*               */ 
            ttPriKat.kalkForh        = ""
            /*               */ 
            ttPriKat.BFforh          = ""
            /*               */ 
            ttPriKat.nettoSupp       = ""
            /*               */ 
            ttPriKat.kalkSupp        = ""
            /*               */ 
            ttPriKat.BFsupp          = ""
            /* nRetailPrice  */ 
            ttPriKat.MarkedsPris     = ttPriKat.VeilPris
            /*               */ 
            ttPriKat.Sortiment       = ""
            /* nSeason       */ 
            ttPriKat.Sesong          = "1"
            ttPriKat.VPIBildeKode    = ""
            /*               */ 
            ttPriKat.Merknad         = ""
            ttPriKat.ERPNr           = ""
            ttPriKat.EkstStrTypeNavn = TRIM(ENTRY(12,pcLinje,";"),'"')
            ttPriKat.AntIEnh         = TRIM(ENTRY(19,pcLinje,";"),'"')
            ttPriKat.LevUke1         = TRIM(ENTRY(19,pcLinje,";"),'"')
            ttPriKat.LevUke2         = TRIM(ENTRY(20,pcLinje,";"),'"')
            ttPriKat.LevUke3         = TRIM(ENTRY(21,pcLinje,";"),'"')
            ttPriKat.LevUke4         = TRIM(ENTRY(22,pcLinje,";"),'"')
            .      
        
        /* Legger inn bildenavn uten sti. */
        IF ttPriKat.VPIBildeKode <> '' AND NUM-ENTRIES(ttPriKat.VPIBildeKode,'/') > 1 THEN 
            ttPriKat.VPIBildeKode = ENTRY(NUM-ENTRIES(ttPriKat.VPIBildeKode,'/'),ttPriKat.VPIBildeKode,'/').
        
        cDato = ttPriKat.LevUke1.
        dDato = ?.
        IF NUM-ENTRIES(cDato,'-') = 3 THEN
            dDato = DATE(
                INT(ENTRY(2,cDato,'-')),
                INT(ENTRY(3,cDato,'-')),
                INT(ENTRY(1,cDato,'-'))
                ).
        IF dDato <> ? THEN
        DO:
            ASSIGN 
                iArUke = INT(STRING(YEAR(dDato),"9999") + STRING(rStandardFunksjoner:ISOWeekNumber(dDato),"99"))
                ttPriKat.LevUke1 = STRING(iArUke,"999999")
                NO-ERROR.
        END.
        cDato = ttPriKat.LevUke2.
        dDato = ?.
        IF NUM-ENTRIES(cDato,'-') = 3 THEN
            dDato = DATE(
                INT(ENTRY(2,cDato,'-')),
                INT(ENTRY(3,cDato,'-')),
                INT(ENTRY(1,cDato,'-'))
                ).
        IF dDato <> ? THEN
        DO:
            ASSIGN 
                iArUke = INT(STRING(YEAR(dDato),"9999") + STRING(rStandardFunksjoner:ISOWeekNumber(dDato),"99"))
                ttPriKat.LevUke2 = STRING(iArUke,"999999")
                NO-ERROR.
        END.
        cDato = ttPriKat.LevUke3.
        dDato = ?.
        IF NUM-ENTRIES(cDato,'-') = 3 THEN
            dDato = DATE(
                INT(ENTRY(2,cDato,'-')),
                INT(ENTRY(3,cDato,'-')),
                INT(ENTRY(1,cDato,'-'))
                ).
        IF dDato <> ? THEN
        DO:
            ASSIGN 
                iArUke = INT(STRING(YEAR(dDato),"9999") + STRING(rStandardFunksjoner:ISOWeekNumber(dDato),"99"))
                ttPriKat.LevUke3 = STRING(iArUke,"999999")
                NO-ERROR.
        END.
        cDato = ttPriKat.LevUke4.
        dDato = ?.
        IF NUM-ENTRIES(cDato,'-') = 3 THEN
            dDato = DATE(
                INT(ENTRY(2,cDato,'-')),
                INT(ENTRY(3,cDato,'-')),
                INT(ENTRY(1,cDato,'-'))
                ).
        IF dDato <> ? THEN
        DO:
            ASSIGN 
                iArUke = INT(STRING(YEAR(dDato),"9999") + STRING(rStandardFunksjoner:ISOWeekNumber(dDato),"99"))
                ttPriKat.LevUke4 = STRING(iArUke,"999999")
                NO-ERROR.
        END.
    
        STATUS DEFAULT "Lese linje " + 
            STRING(iAntLinjer) + 
            " av " + 
            STRING(iTotAntLinjer) + 
            ".".
    END. /* LESERLINJER */
    INPUT STREAM InnFil CLOSE.
    
    IF bTest THEN 
    DO:
        RUN bibl_loggDbFri.p (cLogg,
            '  Lest inn ' + STRING(iAntLinjer) + ' linjer. Totalt antall linjer i fil er ' + STRING(iTotAntLinjer) + '.' 
            ).
        RUN bibl_loggDbFri.p (cLogg,
            '  Ferdig lesInnPricat'
            ).
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

    IF bTest THEN 
        RUN bibl_loggDbFri.p (cLogg,
            '  Totalt antall linjer å lese inn ' + STRING(iTotAntLinjer) + '.'
            ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VPIFilLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VPIFilLogg Procedure
PROCEDURE VPIFilLogg:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cLoggTekst AS CHARACTER NO-UNDO.

    IF bTest THEN 
        RUN bibl_loggDbFri.p (cLogg,
            '  VPIFilLogg: ' + cLoggTekst
            ).

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


/* ************************  Function Implementations ***************** */
