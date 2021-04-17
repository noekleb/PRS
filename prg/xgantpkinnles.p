&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xgantpkinnles.p
    Purpose     : Liten endring.

    Syntax      :

    Description : Leser inn filen og splitter den i to filer.

    Author(s)   : Tom Nøkleby
    Created     : 17/7-05
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
DEFINE VARIABLE iFarg AS INTEGER NO-UNDO.
DEFINE VARIABLE cLoggFeilPris AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE iX AS INTEGER NO-UNDO.
DEFINE VARIABLE obOk AS LOG NO-UNDO.
DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE bOutlet AS LOG NO-UNDO.
DEFINE VARIABLE cNoArtLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSendeMailStockOrdre AS INTEGER NO-UNDO.
DEFINE VARIABLE icFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE iGantAktiv AS INTEGER   NO-UNDO. 

DEF VAR h_vartkor     AS HANDLE NO-UNDO.
DEF VAR hField1       AS HANDLE NO-UNDO.
DEF VAR c02SjekkListe AS CHAR NO-UNDO.
DEF VAR cGenEan       AS CHAR NO-UNDO.
DEFINE VARIABLE bGenEAN AS LOG NO-UNDO.
DEFINE VARIABLE cTekst  AS CHARACTER NO-UNDO.
DEF VAR iClProfilNr    AS INT  NO-UNDO.
DEFINE VARIABLE bKopierPrisko AS LOG       NO-UNDO.
DEFINE VARIABLE bEtiTvang     AS LOG       NO-UNDO.
DEFINE VARIABLE bSettEtikett  AS LOG       NO-UNDO.
DEFINE VARIABLE iCl           AS INTEGER   NO-UNDO.
DEF VAR bStdPrisOverf AS LOG  NO-UNDO.
DEFINE VARIABLE iPrikatButikkNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cEDB-System AS CHARACTER INITIAL 'Gant Global' NO-UNDO.
DEFINE VARIABLE lSumLandedCost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE cNettButLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOutletLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPrisRab%   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lforhRab%   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE fMvaKr      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE fDbKr       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cKommisjonsButLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE lLC% AS DECIMAL NO-UNDO.

DEF VAR lMvaKr AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF VAR lDbKr  AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF VAR lDb%   AS DEC FORMAT "->>,>>9.9" NO-UNDO.

DEF STREAM InnFil.
DEF STREAM UtVPI.
DEF STREAM UtVre.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .
DEF TEMP-TABLE tmpVare
    FIELD VareNr AS CHAR.

DEFINE BUFFER bPrisKo FOR PrisKo.
DEFINE BUFFER bArtPris FOR ArtPris.
DEFINE BUFFER bVPIArtBas FOR VPIArtBas.

{ttpricat.i &NEW=" " &SHARED=" "}
DEFINE BUFFER bufttPriKat FOR ttPriKat.

{ttvre.i    &NEW=" " &SHARED=" "}
{syspara.i 5 26 1 bStdPrisOverf LOGICAL}
{syspara.i 210 100 5 cNoArtLst}
{syspara.i 22 5 2 cOutletLst}
{syspara.i 22 1 10 iSendeMailStockOrdre INT}
{windows.i}
{syspara.i 210 100 8 iGantAktiv INT}

DEF TEMP-TABLE tmpEtikettlogg NO-UNDO 
    FIELD TelleNr AS DECIMAL FORMAT ">>>>>>>>>>>>9"
    FIELD cType AS CHARACTER 
    FIELD BELayout LIKE Butiker.BELayout
    FIELD BEPrinter LIKE Butiker.BEPrinter
    FIELD BETerminalklient LIKE Butiker.BETerminalklient
    .
    
DEFINE TEMP-TABLE tmpNyArt NO-UNDO 
    FIELD Kode AS CHARACTER FORMAT "x(30)"
    FIELD ArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>9"
    FIELD Rab% AS DECIMAL FORMAT "->>,>>9,99"
    FIELD Pris AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD LevPrisEngros AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    INDEX NyKode Kode
    .

DEFINE BUFFER clButiker FOR Butiker.
DEFINE BUFFER bVPIArtPris FOR VPIArtPris.
DEFINE BUFFER bVPIFilHode FOR VPIFilHode.

/* Er normalt satt til 45%. */
{syspara.i 210 100 10 lLC% DEC}

DEFINE VARIABLE rSendEMail AS cls.SendEMail.SendEMail NO-UNDO.
rSendEMail  = NEW cls.SendEMail.SendEMail( ) NO-ERROR.
DEFINE VARIABLE rPakkseddel AS cls.Pakkseddel.Pakkseddel NO-UNDO. 
rPakkseddel  = NEW cls.Pakkseddel.Pakkseddel() NO-ERROR.

SUBSCRIBE TO 'sendStockPkSdlMail' ANYWHERE.

/* Kommisjonsbutikker butikker */
IF iGantAktiv = 1 THEN
DO:
  cKommisjonsButLst = ''. 
  FOR EACH ImpKonv NO-LOCK WHERE 
    ImpKonv.EDB-System = 'Gant Global' AND 
    ImpKonv.Tabell = 'KommisjonBut':
      
    cKommisjonsButLst = cKommisjonsButLst + 
                       (IF cKommisjonsButLst = '' THEN '' ELSE ',') + 
                       ImpKonv.InterntID. 
  END. 
END.

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


&IF DEFINED(EXCLUDE-getPkSdlFilNavn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPkSdlFilNavn Procedure
FUNCTION getPkSdlFilNavn RETURNS CHARACTER 
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

ASSIGN 
    cLogg = 'GANTImport' + REPLACE(STRING(TODAY),'/','')
    .

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

/* Setter leverandørnnumer som skal benyttes hvis dette ikke står i filen. */
{syspara.i 210 100 1 iLevNr INT}
IF iLevNr = 0 THEN
    iLevNr = 40.
{syspar2.i 210 100 1 cLevNavn}
{syspara.i 2 4 8 cGenEan}

{syspara.i 50 15 4 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bGenEAN = TRUE.
  
/* Sjekker om etikettflagg skal settes på hk prisprofil. */
{syspara.i 2 4 42 cTekst}
IF CAN-DO('1,J,Ja,Y,YES,True',cTekst) THEN 
  bSettEtikett = TRUE.
ELSE
  bSettEtikett = FALSE. 
  
/* Sjekker om etikettflagg skal settes. */
{syspara.i 2 4 41 cTekst}
IF CAN-DO('1,J,Ja,Y,YES,True',cTekst) THEN 
  bEtiTvang = TRUE.
ELSE
  bEtiTvang = FALSE. 
  
/* Sjekker om priskøpost skal kopieres til alle andre prisprofiler. */
{syspara.i 2 4 40 cTekst}
IF CAN-DO('1,J,Ja,Y,YES,True',cTekst) THEN 
  bKopierPrisko = TRUE.
ELSE
  bKopierPrisko = FALSE. 

{syspara.i 150 1 2 cTekst}
cNettButLst = cTekst. 
{syspara.i 150 1 3 cTekst}
cNettButLst = cNettButLst + 
              (IF cNettButLst = '' THEN '' ELSE ',') + 
              cTekst. 

{syspara.i 5 1 1 iCl INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE clButiker THEN 
  RETURN.
iClProfilNr = clButiker.ProfilNr. 

RUN pksdlnr.

ASSIGN
    cLoggFeilPris = 'PkSdlMedFeilPris' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') 
    ctmpKatalog = SESSION:TEMP-DIRECTORY
    cVPIFil     = "GGVPI" + STRING(iLevNr,"9999999") + "-" + string(ipksdlnr,"9999999") + ".csv"
    cVreFil     = "GGVRE" + STRING(iLevNr,"9999999") + "-" + 
                  string(ipksdlnr,"9999999") + 
                  "-NNNNNN-OOOOOO-" + ".csv"
    .


RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Starter: ' + 
    ' Filnavn: ' + VPIFilHode.FilNavn + ' EkstVPILev: ' + STRING(VPIFilHode.EkstVPILevNr)).

RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Starter innlesning av fil: ' + VPIFilHode.FilNavn).
RUN LesInnFil.

RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Setter fargekoder start.').
RUN Sett_Farge_Fra_LevFargKod.
RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Setter fargekoder ferdig.').

RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Oppretter eventuelle manglende HK kalkyler.').
RUN opprettManglendeHKKalkyle.
RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Opprette HK kalkyle ferdig.').

DO TRANSACTION:
    FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
    ASSIGN
        VPIFilHode.VPIFilStatus = 5
        .
END.
FIND CURRENT VPIFilHode NO-LOCK.

RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Stopper program og rydder i memory.').
/* Stopper innlesningsprogram for håndterminalfil. */
IF VALID-HANDLE(hPgmHandle) THEN
    DELETE PROCEDURE hPgmHandle.
IF VALID-HANDLE(h_dvpifilhode) THEN
    DELETE PROCEDURE h_dvpifilhode.
IF VALID-HANDLE(h_dvpiartbas) THEN
    DELETE PROCEDURE h_dvpiartbas.
IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_PrisKo.

RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Sjekker størrelsestyper.').
RUN strtype_korr.p.

/* Skriver ut etiketter */
RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Starter etikettutskrift.').
ETIKETTBLOKK:
FOR EACH tmpEtikettLogg:
    FIND PkSdlHode NO-LOCK WHERE 
      PkSdlHode.PkSdlId = tmpEtikettLogg.TelleNr NO-ERROR.
    IF iGantAktiv = 1 AND AVAILABLE PkSdlHode AND 
      NOT CAN-DO(cKommisjonsButLst,STRING(PkSdlHode.butikkNr)) THEN 
      LEAVE ETIKETTBLOKK.   

    RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Skriver ut etiketter for telling: ' + 
                      STRING(tmpEtikettLogg.TelleNr) + ' til ' + 
                      STRING(tmpEtikettLogg.BEPrinter) + ' på klient ' + STRING(tmpEtikettLogg.BETerminalklient)).
    
    RUN batchEtikettTelling.p (tmpEtikettLogg.TelleNr,
                               tmpEtikettLogg.BELayout,
                               tmpEtikettLogg.BEPrinter,
                               tmpEtikettLogg.BETerminalklient,
                               "TELLING").
    RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Etikettutskrift klar for telling: ' + STRING(tmpEtikettLogg.TelleNr)).
    PAUSE 5 NO-MESSAGE.
END. /* ETIKETTBLOKK */
RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Ferdig med etikettutskrift.').

/* Setter alle ikke lagerstyrte varer til lagerstyrt. */
RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Setter aktiklene som lagerstyrte.').
FOR EACH ArtBas EXCLUSIVE-LOCK WHERE
  ArtBas.Opris = FALSE AND
  ArtBas.Lager = FALSE:
  
  ArtBas.Lager = TRUE.
END.

/* TN 24/9-18 Sender feillogg hvis det er feil på innpriser. */
IF SEARCH('log\' + cLoggFeilPris + '.log') <> ? THEN 
    RUN SendPrisFeilLogg(SEARCH('log\' + cLoggFeilPris + '.log')).
    
RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: FERDIG.').

EMPTY TEMP-TABLE ttPriKat.
EMPTY TEMP-TABLE tmpEtikettLogg.
EMPTY TEMP-TABLE tmpNyArt.
EMPTY TEMP-TABLE tmpVare.
EMPTY TEMP-TABLE ttVre.
EMPTY TEMP-TABLE tt_Error.

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


IF iAntLinjer > 0 THEN
DO:
    OUTPUT STREAM UtVPI TO VALUE(ctmpKatalog + "\" + cVPIFil) NO-ECHO.                    
    EKSPORTFIL:                    
    FOR EACH ttPrikat
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
            /* 80 */ ttPriKat.KjedeInnkPris ";" 
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
        SKIP.

    END. /* EKSPORTFIL */
    OUTPUT STREAM UtVpi CLOSE.

    /* Flytter filen til ankommet katalogen */
    OS-COPY VALUE(ctmpKatalog + "~\" + cVPIFil)
            value(VPIFilHode.Katalog + "~\" + cVPIFil).
    /* Renser bort temp fil */
    IF SEARCH('tnc.txt') <> ? AND SEARCH(VPIFilHode.Katalog + "~\" + cVPIFil) <> ? THEN
        OS-DELETE VALUE(ctmpKatalog + "~\" + cVPIFil).

    /* Opprett VPIFilHode og les inn. */
    LES-INN-OPPDATER:
    DO /*TRANSACTION*/:
        FILE-INFO:FILE-NAME = VPIFilHode.Katalog + "\" + cVPIFil.

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
        /* Starter program for lasting av VPI mottak */
        IF NOT VALID-HANDLE(h_PrisKo) THEN
            RUN prisko.p PERSISTENT SET h_PrisKo.
        IF NOT VALID-HANDLE(h_dvpifilhode) THEN
            RUN dvpifilhode.w PERSISTENT SET h_dvpifilhode.
        IF NOT VALID-HANDLE(h_dvpiartbas) THEN
        DO:
            RUN dvpiartbas.w PERSISTENT SET h_dvpiartbas.
            RUN SettAutoImport IN h_dvpiartbas (INPUT TRUE).
        END.
        /* Leser inn filen GGVPI --> xsport1vpiinnles.p/xsport1vpiutpakk.p --> xPRSPricatInnles.p/xPRSPricatUtpakk.p */
        RUN LesInnFil IN h_dvpifilhode (INPUT STRING(plFilId),
                                   OUTPUT pbOk,
                                   OUTPUT piAntLinjer).
        /* Pakker ut fil. */
        RUN PakkUtFil IN h_dvpifilhode (INPUT STRING(plFilId)).
        /* Oppretter alle nye poster */
        FOR EACH VPIArtBas NO-LOCK WHERE
            VPIArtBas.EkstVPILevNr = VPIFilHode.EkstVPILevNr AND
            VPIArtBas.VPIDato      = TODAY /* AND
            VPIArtBas.BehStatus    = 1 */ TRANSACTION:
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
                  PrisKo.Tilbud        = FALSE AND 
                  Prisko.Type          = 1 
                  NO-ERROR.
              IF NOT AVAILABLE PrisKo THEN
              DO:
                  CREATE PrisKo.
                  ASSIGN
                      PrisKo.ArtikkelNr    = ArtBas.ArtikkelNr
                      PrisKo.ProfilNr      = VPIArtPris.ProfilNr 
                      PrisKo.AktiveresDato = TODAY 
                      PrisKo.AktiveresTid  = 0 
                      PrisKo.Tilbud        = FALSE
                      Prisko.Type          = 1
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

            ELSE. /* MESSAGE "Finner ikke artbas" VPIArtBas.ArtikkelNr VIEW-AS ALERT-BOX.*/
            
            DO:
              FIND bVPIArtBas EXCLUSIVE-LOCK WHERE
                RECID(bVPIArtBas) = RECID(VPIArtBas) NO-ERROR.
              IF AVAILABLE bVPIArtBas THEN DO:
                bVPIArtBas.BehStatus = 90. /* Behandlet. */
                RELEASE bVPIArtBas.
              END.
            END.
        END.
    END. /* LES-INN-OPPDATER TRANSACTION */
    
    SETT_IKASSE_FLAGG:
    FOR EACH ttPrikat:
      IF TRIM(ttPriKat.EANnr) <> '' THEN 
      DO TRANSACTION:
        FIND Strekkode NO-LOCK WHERE
          Strekkode.Kode = TRIM(ttPriKat.EANNr) NO-ERROR.
        IF AVAILABLE Strekkode THEN 
        DO:
          FIND ArtBas OF Strekkode EXCLUSIVE-LOCK NO-ERROR.
          IF AVAILABLE ArtBas THEN 
          DO:
            ASSIGN
              ArtBas.IKasse  = TRUE
              ArtBas.Etikett = 1
              ArtBas.Lager   = TRUE 
              .

            IF CAN-DO(cNettButLst,STRING(ttPriKat.ButikkNr)) THEN 
            DO:
                ASSIGN 
                    ArtBas.WebButikkArtikkel   = TRUE
                    ArtBas.PubliserINettbutikk = TRUE  
                    . 
            END.  
            FIND CURRENT ArtBas NO-LOCK.
          END.
        END.      
      END.                 
    END. /* SETT_IKASSE_FLAGG */
    
    /* TEST TEST */
    TEMP-TABLE tmpNyArt:WRITE-JSON('file', 'konv\tmpNyArt' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.json', TRUE).
    
    /* Sjekker tmpNyArt loggen og ser hvilket artikkelnr som nå er satt på loggpostene. */
    FOR EACH tmpNyArt:
        FIND Strekkode NO-LOCK WHERE 
            Strekkode.Kode = tmpNyArt.Kode NO-ERROR.
        IF AVAILABLE Strekkode THEN 
            tmpNyArt.ArtikkelNr = Strekkode.ArtikkelNr
            .
    END.
    
    /* De artiklene som nå ligger i tmpNyArt, er artikler som er nyopplagt og som har fått en HK profil med Outlet priser. */
    /* NB: tmpNyArt loggen opprettes bare når det kommer pakksedler på en av Outlet butikkene. Ellers er den tom.          */
    KORRIGER_HK_KALKYLE:
    FOR EACH tmpNyArt WHERE 
        tmpNyArt.ArtikkelNr > 0
        BREAK BY tmpNyArt.ArtikkelNr:
        
        IF FIRST-OF(tmpNyArt.ArtikkelNr) THEN 
        DO TRANSACTION:
            FIND ArtPris EXCLUSIVE-LOCK WHERE 
                ArtPris.ArtikkelNr = tmpNyArt.ArtikkelNr AND 
                ArtPris.ProfilNr   = 1 NO-ERROR.
            IF AVAILABLE ArtPris THEN 
            DO:
                /* HK profilen skal beholde sin gamle rabatt. */
                FIND ArtBas OF ArtPris EXCLUSIVE-LOCK NO-ERROR.
                ASSIGN 
                    ArtPris.InnkjopsPris[1] = tmpNyArt.LevPrisEngros
                    ArtPris.ValPris[1]      = tmpNyArt.LevPrisEngros 
                    ArtPris.Pris[1]         = tmpNyArt.Pris
                    ArtBas.AnbefaltPris     = tmpNyArt.Pris
                    ArtPris.Rab1%[1]        = tmpNyArt.Rab%   
                    ArtPris.Rab1Kr[1]       = ROUND((ArtPris.InnkjopsPris[1] * ArtPris.Rab1%[1]) / 100,2)
                    ArtPris.Rab1Kr[1]       = IF ArtPris.Rab1Kr[1] = ? THEN 0 ELSE ArtPris.Rab1Kr[1] 
                    ArtPris.Varekost[1]     = ArtPris.InnkjopsPris[1] - ArtPris.Rab1Kr[1]                      
                    ArtPris.MvaKr[1]        = ArtPris.Pris[1] - ROUND((ArtPris.Pris[1] / (1 + (25 / 100))),2)
                    ArtPris.DbKr[1]         = ArtPris.Pris[1] - ArtPris.MvaKr[1] - ArtPris.VareKost[1]
                    ArtPris.DB%[1]          = ROUND((ArtPris.DbKr[1] * 100) / (ArtPris.Pris[1] - ArtPris.MvaKr[1]),2)
                    ArtPris.Db%[1]          = IF ArtPris.Db%[1] = ? THEN 0 ELSE ArtPris.Db%[1]
                    .
                RELEASE ArtPris.
                RELEASE ArtBas.
            END.
        END. /* TRANSACTION */   
    END. /* KORRIGER_HK_KALKYLE*/
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksportVREFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportVREFil Procedure 
PROCEDURE EksportVREFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cButikkLst AS CHAR   NO-UNDO.
DEF VAR cPakkseddelLst  AS CHAR   NO-UNDO.
DEF VAR cOrdreLst       AS CHAR   NO-UNDO.
DEF VAR piLoop     AS INT    NO-UNDO.
DEF VAR pi2Loop    AS INT    NO-UNDO.
DEF VAR cOVreFil   AS CHAR   NO-UNDO.
DEF VAR iTelleNr   AS INT    NO-UNDO.
DEF VAR iHtFilId   AS INT    NO-UNDO.
DEF VAR iParaNr    AS INT    NO-UNDO.

DEF BUFFER bSysPara FOR SysPara.

/* Bygger liste over butikker som skal ha varemottak */
FOR EACH ttVre
    BREAK BY ttVre.ButikkNr:
    /* Logger butikker */
    IF FIRST-OF(ttVre.ButikkNr) THEN
        ASSIGN
        cButikkLst = cButikkLst + 
                     (IF cButikkLst = ""
                        THEN ""
                        ELSE ",") + STRING(ttVre.ButikkNr,"999999")
        .
END.

RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: EksportVreFil: Butikker med varemottak: ' + cButikkLst).

ASSIGN
    cOVreFil = cVreFil
    .
IF iAntLinjer > 0 AND cButikkLst <> "" THEN
BUTIKKLOOP:
DO piLoop = 1 TO NUM-ENTRIES(cButikkLst):

    RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: EksportVreFil: Varemottak for butikk: ' + ENTRY(piLoop,cButikkLst)).

    ASSIGN
        cPakkseddelLst = ""
        cOrdreLst      = ""
        .
    /* Lister opp alle ordrene for butikken. */
    FOR EACH ttVre WHERE 
        ttVre.ButikkNr = INT(ENTRY(piLoop,cButikkLst))
        BREAK BY ttVre.ButikkNr
              BY ttVre.PakkseddelNr:
        IF FIRST-OF(ttVre.PakkseddelNr) THEN
        ASSIGN
            cPakkseddelLst  = cPakkseddelLst  + 
                         (IF cPakkseddelLst  = ""
                            THEN ""
                            ELSE ",") + STRING(ttVre.PakkSeddelNr)
            cOrdreLst  = cOrdreLst  + 
                         (IF cOrdreLst  = ""
                            THEN ""
                            ELSE ",") + STRING(ttVre.OrdreNr)
            .
    END.

END. /* BUTIKKLOOP */
RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: EksportVreFil: Varemottak ferdig.').

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

&IF DEFINED(EXCLUDE-Fasteregistre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Fasteregistre Procedure 
PROCEDURE Fasteregistre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.

  REGISTERSJEKK:
  FOR EACH ttPrikat:
      /* Denne skal alltid finnes - Farge */
      IF NOT CAN-FIND(Farg WHERE
                      Farg.Farg = 1) THEN
      DO:
          CREATE Farg.
          ASSIGN
              Farg.Farg     = 1
              Farg.FarBeskr = "* Automatisk opprettet"
              .
      END.
      
      /* Tar hånd om fargekoden. Legger den opp hvis den ikke finnes. */
      IF TRIM(ttPriKat.FargeKode) <> '' THEN 
      DO:
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
      END. 
      /* Henter fargen på eksisterende artikkel */
      ELSE IF ttPriKat.FargeKode = '' AND ttPriKat.Ean <> '' THEN 
      DO:
          IF AVAILABLE ArtBas THEN 
              RELEASE ArtBas.
          FIND StrekKode NO-LOCK WHERE 
              StrekKode.Kode = ttPriKat.EANnr NO-ERROR.
          IF AVAILABLE StrekKode THEN 
              FIND ArtBas OF StrekKode NO-ERROR.
          IF AVAILABLE ArtBas THEN 
              FIND FIRST Farg OF ArtBas NO-LOCK NO-ERROR.
          IF AVAILABLE Farg THEN 
          ASSIGN
              ttPriKat.FargeKode  = STRING(Farg.Farg)
              ttPriKat.Farg       = Farg.Farg
              ttPriKat.FargeTekst = Farg.FarBeskr
          .
      END.
      
      /* Sesong */
      IF NOT CAN-FIND(Sasong WHERE
                      Sasong.Sasong = int(ttPriKat.Sesong)) THEN 
      DO:
          CREATE Sasong.
          ASSIGN
              Sasong.Sasong   = int(ttPriKat.Sesong)
              Sasong.SasBeskr = "* Automtatisk opprettet"
              .
      END.      
  END. /* REGISTERSJEKK */
              
              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixStorl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixStorl Procedure 
PROCEDURE FixStorl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT-OUTPUT PARAMETER wStorl AS CHAR NO-UNDO.

  DEF VAR wDecimaler AS CHAR NO-UNDO.

  {syspara.i 1 1 16 wDecimaler}

  ASSIGN
     wStorl = TRIM(wStorl)
     wStorl = CAPS(wStorl)
     wStorl = IF (LENGTH(wStorl) = 1 OR
                  LENGTH(wStorl) = 3
                  )
                 then " " + wStorl
                 else wStorl.

  /* Bytter ut eventuelle comma med punkt. */
  IF INDEX(wStorl,",") <> 0 THEN
    OVERLAY(wStorl, INDEX(wStorl,","), 1, "CHARACTER") = ".".

  /* Sjekker om det er benyttet gyldige tegn i halvnummer. */
  /* Er det ikke det, tas halvnummeret bort.               */
  /*
  IF NUM-ENTRIES(wStorl,".") = 2 THEN
    DO:
      IF NOT CAN-DO(wDecimaler,ENTRY(2,wStorl,".")) THEN
        wStorl = ENTRY(1,wStorl,".").
    END.
  */
  RETURN wStorl.   /* Function return value. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-genEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genEAN Procedure 
PROCEDURE genEAN :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plArtikkelNr AS DEC  NO-UNDO.
  DEF INPUT PARAMETER cStorl       AS CHAR NO-UNDO.

  DEF VAR cKode AS CHAR NO-UNDO.

  FIND StrKonv WHERE StrKonv.Storl = cStorl USE-INDEX Storl NO-LOCK NO-ERROR.
  IF NOT AVAIL StrKonv THEN
      RETURN.
  /* Finnes det strekkode på størrrelsen fra før, skal vi ikke legge opp ny. */
  IF CAN-FIND(FIRST StrekKode WHERE StrekKode.ArtikkelNr = ArtBas.ArtikkelNr AND
                              StrekKode.KodeType = 1 AND
                              StrekKode.StrKode  = StrKonv.StrKode
                          /*  AND StrekKode.Kode BEGINS "02" */
                              ) THEN RETURN.

  ASSIGN cKode = "02" + STRING(ArtBas.ArtikkelNr,"9999999") + STRING(StrKonv.StrKode,"999")
         cKode = FixChk(cKode).

  CREATE StrekKode.
  ASSIGN StrekKode.ArtikkelNr = ArtBas.ArtikkelNr
         StrekKode.Kode       = cKode
         StrekKode.KodeType   = 1 /* använd inte iKodeType, vi kan ha 0 */
         StrekKode.StrKode    = StrKonv.StrKode 
         StrekKode.VareId     = ArtBas.ArtikkelNr
      NO-ERROR.
  /* TN Koden kan finnes fra før - 02 koder gav feilmelding. */
  IF ERROR-STATUS:ERROR THEN
  DO:
      IF AVAILABLE StrekKode THEN
          DELETE StrekKode.
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
  DEF VAR piLinjeNr AS INT  NO-UNDO.
  DEF VAR piAntFeil AS INT  NO-UNDO.
  DEF VAR pcBkuFil  AS CHAR NO-UNDO.
  DEF VAR plVareNr  AS INT  NO-UNDO.
  DEF VAR pcStr     AS CHAR NO-UNDO.
  
  DEFINE BUFFER innArtBas FOR ArtBas.
  
  /* Tømmer feillogg. */
  FOR EACH tt_Error:
    DELETE tt_Error.
  END.
  /* Tømmer pricat */
  FOR EACH ttPrikat:
      DELETE ttPrikat.
  END.
  /* Tømmer varefil */
  FOR EACH ttVre:
      DELETE ttVre.
  END.

  RUN TellOppLinjer.

  ASSIGN
      piLinjeNr  = 1
      iAntLinjer = 0
      bOutlet    = FALSE 
      .

  RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: LesInnFil: Åpner stream fra: ' + cFilNavn).

  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.

    ASSIGN
      iAntLinjer = iAntLinjer + 1
      .

    IF TRIM(pcLinje) = '' THEN 
      NEXT LESERLINJER.
    BLANKLINJE:
    DO:
      DO iLoop = 1 TO LENGTH(pcLinje):
        IF TRIM(SUBSTRING(pcLinje,iLoop,1)) <> '' THEN 
          LEAVE BLANKLINJE. 
      END.
      NEXT LESERLINJER.  
    END. /* BLANKLINJE */

    /* Legger på leverandørnummer hvis ikke dette er satt i filen. */
/*     IF ENTRY(2,pcLinje,";") = "" THEN                      */
/*         ASSIGN                                             */
/*             ENTRY(2,pcLinje,";") = "9999" /* No suplier */ */
/*             .                                              */

    IF SUBSTRING(pcLinje,1,2) = "**" THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      CREATE tt_Error.
      ASSIGN
        tt_Error.LinjeNr = piAntFeil
        tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + ": " + pcLinje
        .
      NEXT LESERLINJER.
    END.
    /* Sjekker om raden skal være med.  */
    /* VareNr < 100 skal ikke være med. */
    ASSIGN
        plVareNr = dec(TRIM(ENTRY( 1,pcLinje,";"),'"'))
        NO-ERROR.
    IF (ERROR-STATUS:ERROR = FALSE AND
        plVareNr > 0) THEN DO:
        IF plVareNr < 100 THEN
            NEXT LESERLINJER.
    END.

    /* Artikler som ikke skal leses inn */
    IF CAN-DO(cNoArtLst,TRIM(ENTRY( 1,pcLinje,";"),'"')) THEN 
            NEXT LESERLINJER.

    FIND FIRST Farg NO-LOCK NO-ERROR.

    CREATE ttPriKat.
    ASSIGN
        pcStr = TRIM(TRIM(
                                    LEFT-TRIM(TRIM(ENTRY( 6,pcLinje,";")))
                                    ) + 
                 trim(
                      LEFT-TRIM(TRIM(ENTRY( 7,pcLinje,";")))
                      ),'"')

        pcStr = IF ttPriKat.Str BEGINS "ONE"
                  THEN "1"
                  ELSE pcStr
        .
    /* Plukker ut størrelsen og formaterer den. */
    ASSIGN
        pcStr = REPLACE(pcStr, " 1/2", ".5")
        pcStr = REPLACE(pcStr, ",", ".")
        pcStr = REPLACE(pcStr, "-", "")
        pcStr = REPLACE(pcStr, "-", "")
        .
    RUN FixStorl (INPUT-OUTPUT pcStr).

    ASSIGN
        ttPriKat.EkstVPILevNr  = VPIFilHode.EkstVPILevNr
        ttPriKat.LinjeNr       = piLinjeNr
        piLinjeNr              = piLinjeNr  + 1
        /*               */ ttPriKat.R1            = "R1"
        /*               */ ttPriKat.LevNr         = STRING(iLevNr)
        /* nArtno        */ ttPriKat.LevModellNr   = TRIM(ENTRY( 1,pcLinje,";"),'"')                  
        /* cEANCode      */ ttPriKat.EANnr         = TRIM(TRIM(ENTRY( 2,pcLinje,";"),'"'))
        /* rArtName      */ ttPriKat.VareTekst     = TRIM(ENTRY( 3,pcLinje,";"),'"')
        /* NColCode      */ ttPriKat.FargeKode     = TRIM(ENTRY( 4,pcLinje,";"),'"')
        /* cColName      */ ttPriKat.FargeTekst    = TRIM(ENTRY( 4,pcLinje,";"),'"') /* TN 27/2-17. Are vil ha inn koden her */
                                                     /* Kol 6 er str + 7 som er livvidde */
        /* cCode1        */ ttPriKat.Str           = pcStr
        /* nOrder        */ ttPriKat.SeqNrStr      = INT(TRIM(ENTRY(20,pcLinje,";"),'"'))
        /* nSeason(3) + nSizeCode(3) */
                            ttPriKat.StrTab        = '' /*SUBSTRING(TRIM(ENTRY(22,pcLinje,";"),'"'),4,3) + string(int(TRIM(ENTRY( 8,pcLinje,";"),'"')),"999")*/
        /* nMainGroup    */ ttPriKat.Varemerke     = TRIM(ENTRY( 9,pcLinje,";"),'"')
        /*               */ ttPriKat.Enh           = ""
        /*               */ ttPriKat.Enh           = (IF ttPriKat.Enh = "" THEN "Stk" ELSE ttPriKat.Enh)
        /*               */ ttPriKat.AntIEnh       = ""
        /* NWholeSaleNet */ ttPriKat.LevPrisEngros = REPLACE(REPLACE(TRIM(TRIM(ENTRY(11,pcLinje,";"),'"'),"%"),' ',''),'.',',')
        /*               */ ttPriKat.ValKod        = cValKod
        /*               */ ttPriKat.forhRab%      = REPLACE(REPLACE(TRIM(TRIM(ENTRY(27,pcLinje,";"),'"'),"%"),' ',''),'.',',')
        /*               */ ttPriKat.suppRab%      = ""
        /* nRetailPrice  */ ttPriKat.VeilPris      = REPLACE(TRIM(ENTRY(10,pcLinje,";"),'"'),'.',',')
                            ttPriKat.VeilPris      = REPLACE(TRIM(REPLACE(ttPriKat.VeilPris,' ',''),"%"),'.',',')
        /* Kjedens innkj. pris */
        /*               */ ttPriKat.PAKstru       = TRIM(ENTRY(16,pcLinje,";"))
        /*               */ ttPriKat.LevUke1       = ""
        /*               */ ttPriKat.LevUke2       = ""
        /*               */ ttPriKat.LevUke3       = ""
        /*               */ ttPriKat.LevUke4       = ""
        /* nArtgroup + nSubGroup */
        /*               */ ttPriKat.LevNavn       = cLevNavn
        /* nArtno        */ ttPriKat.LevKod        = ttPriKat.LevModellNr                  
        /* NWholeSaleNet */ ttPriKat.nettoForh     = REPLACE(REPLACE(TRIM(TRIM(ENTRY(11,pcLinje,";"),'"'),"%"),' ',''),'.',',')
        /*               */ ttPriKat.kalkForh      = ""
        /*               */ ttPriKat.BFforh        = ""
        /*               */ ttPriKat.nettoSupp     = ""
        /*               */ ttPriKat.kalkSupp      = ""
        /*               */ ttPriKat.BFsupp        = ""
        /* nRetailPrice  */ ttPriKat.MarkedsPris   = REPLACE(REPLACE(TRIM(TRIM(ENTRY(10,pcLinje,";"),'"'),"%"),' ',''),'.',',')
        /*               */ ttPriKat.Sortiment     = ""
        /* nSeason       */ ttPriKat.Sesong        = TRIM(ENTRY(22,pcLinje,";"),'"')
        /* nSeason       */ ttPriKat.Sesong        = TRIM(ENTRY(22,pcLinje,";"),'"')
        ttPriKat.VPIBildeKode  = /*trim(ENTRY(22,pcLinje,";"),'"') + "~\" + 
                                 trim(ENTRY( 9,pcLinje,";"),'"') + "~\" +
                                 trim(ENTRY(14,pcLinje,";"),'"') + "~\" +*/
                                 ttPriKat.LevModellNr + "-" + trim(ENTRY( 4,pcLinje,";"),'"') + ".jpg"
        /*               */ ttPriKat.Merknad       = "nConcept=" + trim(ENTRY(17,pcLinje,";"),'"') + "|" + 
                                                     "cConceptname=" + trim(ENTRY(18,pcLinje,";"),'"') + "|" + 
                                                     "SeqNrStr=" + TRIM(STRING(ttPriKat.SeqNrStr))
        ttPriKat.BehStatus                         = 1
        .
       IF NUM-ENTRIES(pcLinje,';') >= 29 THEN
        DO: 
           ttPriKat.KjedeInnkPris = DEC(REPLACE(TRIM(REPLACE(TRIM(ENTRY(29,pcLinje,";"),'"'),' ',''),"%"),'.',',')).
           IF ttPriKat.KjedeInnkPris = 0 AND iGantAktiv = 1 THEN 
            ttPriKat.KjedeInnkPris = ROUND((DEC(ttPriKat.nettoForh) * lLC%) / 100,0).
        END.        
        
       ttPriKat.VareGruppe = TRIM(STRING(INT(ENTRY(9,pcLinje,";")),">>99")) + TRIM(STRING(INT(ENTRY(14,pcLinje,";")),"9999")).
    
    /* Genererer EAN kode hvis den er blank. */
    IF ttPriKat.EANnr = '' THEN 
    DO:
      ttPriKat.EANnr = getEAN(). 
    END.

        /*
        ttPriKat.ButikkNr = IF       CAN-DO("10000,70001,10013,7000,14012,95000",ENTRY(24,pcLinje,";")) THEN  2 /* Stortingsgata            */
                            ELSE IF  CAN-DO("10004,70002,7004,14041,95004",ENTRY(24,pcLinje,";"))       THEN  3 /* Aker brygge              */
                            ELSE IF  CAN-DO("70006,7005,14131",ENTRY(24,pcLinje,";"))                   THEN  4 /* Sten&Strøm Dame          */
                            ELSE IF  CAN-DO("10020,14023,95021",ENTRY(24,pcLinje,";"))                  THEN  5 /* Sten&Strøm Herre         */
                            ELSE IF  CAN-DO("10025,14000,70007,7007,95025",ENTRY(24,pcLinje,";"))       THEN  6 /* Sandvika                 */
                            ELSE IF  CAN-DO("70003,10008,14112,95008",ENTRY(24,pcLinje,";"))            THEN  8 /* Asker                    */
                            ELSE IF  CAN-DO("10018,70005,14114,95018,12179",ENTRY(24,pcLinje,";"))      THEN  9 /* Tønsberg                 */
                            ELSE IF  CAN-DO("50000",ENTRY(24,pcLinje,";"))                              THEN 10 /* Outlet                   */
                            ELSE IF  CAN-DO("10001,70000,14034",ENTRY(24,pcLinje,";"))                  THEN 11 /* Gant Strømmen Storsenter */
                            ELSE IF  CAN-DO("",ENTRY(24,pcLinje,";"))                                   THEN 12 /* GANT E-COMMERCE 55000    */
                            ELSE IF  CAN-DO("10003,70004",ENTRY(24,pcLinje,";"))                        THEN 14 /* Trondheim                */
                            ELSE IF  CAN-DO("55000,55001,55002,55003",ENTRY(24,pcLinje,";"))            THEN 15 /* eComerse                 */
                            ELSE 1 /* Gant Norge */
        */
    
      /* Varegruppe */
      IF NOT CAN-FIND(VarGr WHERE
                      VarGr.Vg = int(ttPriKat.VareGruppe)) THEN 
      DO TRANSACTION:
          CREATE VarGr.
          ASSIGN
              VarGr.Vg         = INTEGER(ttPriKat.VareGruppe)
              VarGr.VgBeskr    = TRIM(ENTRY(16,pcLinje,";"),'"')
              VarGr.Hg         = INT(ENTRY(9,pcLinje,";"))
              VarGr.MomsKod    = 1
              VarGr.Kost_Proc  = 65
              .
          FOR EACH Kategori NO-LOCK WHERE
              Kategori.KatNr <= 4:
              IF NOT CAN-FIND(FIRST VgKat WHERE
                              VgKat.Vg = VarGr.Vg AND
                              VgKat.VgKat = Kategori.KatNr) THEN
              DO:
                  CREATE VgKat.
                  ASSIGN
                  VgKat.Vg    = VarGr.Vg
                  VgKat.VgKat = Kategori.KatNr
                  VgKat.KatNr = Kategori.KatNr
                  .
              END.
          END.
      END.
      ELSE DO TRANSACTION:
        FIND VarGr EXCLUSIVE-LOCK WHERE 
          VarGr.Vg = int(ttPriKat.VareGruppe) NO-ERROR.
        IF AVAILABLE VarGr THEN 
          VarGr.VgBeskr = TRIM(ENTRY(16,pcLinje,";"),'"').
      END.
      /* Hovedgruppe */
      IF NOT CAN-FIND(HuvGr WHERE
                      HuvGr.Hg = INT(ENTRY(9,pcLinje,";"))) THEN
      DO TRANSACTION:
          CREATE HuvGr.
          ASSIGN
              HuvGr.Hg         = INT(ENTRY(9,pcLinje,";"))
              HuvGr.HgBeskr    = TRIM(ENTRY(15,pcLinje,";"),'"')
              HuvGr.AvdelingNr = IF ttPriKat.PAKstru = 'HOME' THEN 2 ELSE 1
              .
      END.
    
    /* Setter butikk og filialnr. */
    IF ttPriKat.ButikkNr = 0 THEN 
    DO:
      FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = 'Butiker' AND 
        ImpKonv.EksterntId = TRIM(ENTRY(24,pcLinje,";")) NO-ERROR.
      IF AVAILABLE ImpKonv THEN
      DO: 
        FIND Butiker NO-LOCK WHERE 
            Butiker.Butik = INT(ImpKonv.InterntId) NO-ERROR.
        ASSIGN 
            ttPriKat.ButikkNr = INT(ImpKonv.InterntId)
            ttPriKat.ProfilNr = IF AVAILABLE Butiker THEN 
                                    Butiker.ProfilNr
                                ELSE 
                                    iClProfilNr 
            .
      END.
      ELSE DO:
        ASSIGN 
            ttPriKat.ButikkNr = 1
            ttPriKat.ProfilNr = iClProfilNr
            .
      END.
    END.

    /* Sjekker om det er lov å endre sesongkode.                  */
    /* Kun 1 og 12 kan endre. Ellers skal gammel verdi stå urørt. */
    ttPriKat.KampanjeKode = ''. /* Nullstiller for å kunne logge sesongkode endring. */
    IF NUM-ENTRIES(pcLinje,";") > 27 AND 
       NOT CAN-DO('1,12',TRIM(ENTRY(28,pcLinje,";"))) THEN 
    DO:
      FIND Strekkode NO-LOCK WHERE 
          Strekkode.Kode = ttPriKat.EANnr NO-ERROR.
      IF AVAILABLE Strekkode THEN 
          FIND innArtBas OF Strekkode NO-LOCK NO-ERROR.
      IF AVAILABLE innArtBas THEN
      DO:
          /* Beholder opprinnelig sesongkode (Supplering i sesong) */
          ttPriKat.Sesong = STRING(innArtBas.Sasong).         
      END.
    END.   
    /* Ordretype 1 og 12 endrer sesongkode hvis den endres til en nyere sesong. */ 
    ELSE DO:
        FIND Strekkode NO-LOCK WHERE 
            Strekkode.Kode = ttPriKat.EANnr NO-ERROR.
        IF AVAILABLE Strekkode THEN 
            FIND innArtBas OF Strekkode NO-LOCK NO-ERROR.
        /* Slår av tilbud hvis sesongkoden endres. */
        IF AVAILABLE innArtBas AND (ttPriKat.Sesong > STRING(innArtBas.Sasong)) THEN
        DO:
          /* Denne teksten skal inn i PkSdlLinje.NySesong. */
          ttPriKat.KampanjeKode = 'Sesong endret: ' + STRING(innArtBas.Sasong) + ' --> ' + ttPriKat.Sesong.
        END.
    END.
    
    ASSIGN
        bOutlet           = (IF (ttPriKat.ButikkNr = 10 OR ttPriKat.ButikkNr = 40) THEN TRUE ELSE FALSE)
        ttPriKat.Filial   = STRING(ttPriKat.ButikkNr)
        iPrikatButikkNr   = ttPriKat.ButikkNr 
        .
      
    /*Tildeler løpenr hvis det står til ? */
    FIND Strekkode NO-LOCK WHERE 
      Strekkode.Kode = TRIM(ttPriKat.EANnr) NO-ERROR.
    IF AVAILABLE Strekkode THEN
    DO TRANSACTION: 
      FIND innArtBas NO-LOCK WHERE
        innArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
      IF AVAILABLE innartBas THEN
      DO: 
        IF innArtBas.LopNr = ? THEN
        DO: 
          FIND CURRENT innArtBas EXCLUSIVE-LOCK.
          RUN settlopnr.p (INPUT innArtBas.Vg, INPUT 'N', OUTPUT innArtBas.LopNr).
          RELEASE innArtBas.
        END.
      END.  
    END. 
      
    /* Legger ikke opp pakksedler som er lest inn fra før. */
    IF NOT CAN-FIND(LAST TelleHode WHERE
                    TelleHode.OrdreNr = INTEGER(TRIM(ENTRY(21,pcLinje,";"),'"')) AND
                    TelleHode.PkSdlNr = INTEGER(TRIM(ENTRY(26,pcLinje,";"),'"')) AND
                    YEAR(TelleHode.StartDato) = YEAR(TODAY)) THEN
    PAKKSEDDEL:
    DO:
        /* VAL,,,,E,3595961485869,00002, */
        CREATE ttVre.
        ASSIGN
            ttVre.EkstVPILevNr  = VPIFilHode.EkstVPILevNr
            ttVre.LinjeNr       = piLinjeNr
            /*ttVre.ButikkNr      = int(TRIM(ENTRY(28,pcLinje,";"),'"'))*/  
            ttVre.ButikkNr      = iPrikatButikkNr
            ttVre.OrdreNr       = (TRIM(ENTRY(21,pcLinje,";"),'"'))  /* OrdreNr */
            ttVre.PakkseddelNr  = (TRIM(ENTRY(26,pcLinje,";"),'"'))  /* PakkseddelNr */
            ttVre.LevModellNr   = ttPriKat.LevModellNr
            ttVre.VareTekst     = ttPriKat.VareTekst  
            ttVre.FargeTekst    = ttPriKat.FargeTekst 
            ttVre.Str           = ttPriKat.Str        
            ttVre.Felt1         = "VAL"
            ttVre.Felt2         = ""
            ttVre.Felt3         = ""
            ttVre.Felt4         = ""
            ttVre.Felt5         = "E"
            ttVre.Felt6         = ttPriKat.EANnr /* EAN kode */
            ttVre.Felt7         = STRING(INTEGER(TRIM(ENTRY(19,pcLinje,";"),'"')),"9999") /* Antall */
            ttVre.Felt8         = ttPriKat.KampanjeKode /* Logger sesongkode endring her. */
            ttVre.Felt9         = STRING(ttPriKat.SeqNrStr)
            .
            
        /* Ekstra for pakkseddel */    
        ASSIGN 
            ttVre.Kode          = ttPriKat.EANnr
            ttVre.Antall        = INTEGER(TRIM(ENTRY(19,pcLinje,";"),'"'))
            ttVre.Salgsenhet    = 'Stk'
            ttVre.ArtikkelNr    = 0
            ttVre.BestNr        = 0
            ttVre.LevNr         = iLevNr
            ttVre.StrKode       = 0
            ttVre.LevPrisEngros = DEC(ttPriKat.LevPrisEngros) 
            ttVre.VeilPris      = DEC(ttPriKat.VeilPris)
            ttVre.nettoForh     = DEC(ttPriKat.nettoForh)
            ttVre.MarkedsPris   = DEC(ttPriKat.MarkedsPris)
            ttVre.LandedCost    = ttPriKat.KjedeInnkPris
            ttVre.Sesong        = ttPriKat.Sesong
            .
        IF NUM-ENTRIES(pcLinje,';') > 27 THEN 
            ttVre.OrdreType     = TRIM(ENTRY(28,pcLinje,";"),'"').
            
        /* Setter Outlet rabatt%. */
        IF DEC(ttPriKat.forhRab%) = 0 /*OR CAN-DO('10,40',STRING(ttPriKat.ButikkNr))*/ THEN 
        DO:
            FIND FIRST ImpKonv NO-LOCK WHERE 
                ImpKonv.EDB-System = cEDB-System AND 
                ImpKonv.Tabell     = 'Def.Rab%' AND 
                ImpKonv.EksterntId = STRING(ttPriKat.ButikkNr) NO-ERROR.
            IF AVAILABLE ImpKonv 
                THEN ASSIGN 
                    ttVre.forhRab%      = DEC(ImpKonv.Merknad)
                    ttVre.PrisRab%      = DEC(ImpKonv.InterntId)
                    ttVre.InnkjopsPris  = IF ttVre.forhRab% > 0 
                                              THEN ttVre.LevPrisEngros - ((ttVre.LevPrisEngros * ttVre.forhRab%) / 100)
                                              ELSE ttVre.LevPrisEngros
                    . 
        END.
        /* Setter default rabatt%. */
        ELSE DO: 
            FIND FIRST ImpKonv NO-LOCK WHERE 
                ImpKonv.EDB-System = cEDB-System AND 
                ImpKonv.Tabell     = 'Def.Rab%' AND 
                ImpKonv.EksterntId = STRING(ttPriKat.ButikkNr) NO-ERROR.
            IF AVAILABLE ImpKonv 
                THEN ASSIGN 
                    ttVre.PrisRab%      = DEC(ImpKonv.InterntId)
                    .
            ELSE 
                ASSIGN 
                    ttVre.PrisRab%      = DEC(ttPriKat.PrisRab%).
            ASSIGN 
            ttVre.forhRab%      = DEC(ttPriKat.forhRab%)
            ttVre.InnkjopsPris  = IF ttVre.forhRab% > 0 
                                    THEN ttVre.LevPrisEngros - ((ttVre.LevPrisEngros * ttVre.forhRab%) / 100)
                                  ELSE ttVre.LevPrisEngros 
            .
        END.
        /* Dette gjøres for at HK pris skal bli riktig på helt nye artikler. */
        IF DEC(ttPriKat.forhRab%) = 0 THEN 
        HKPROFILEN:
        DO:
            ASSIGN 
              ttPriKat.forhRab% = IF DEC(ttPriKat.forhRab%) = 0 THEN "10" ELSE ttPriKat.forhRab% 
              ttPriKat.suppRab% = ""
              .
        END. /* HKPROFILEN */

/*        RUN bibl_logg.p (cLogg, '** TEST: ttPriKat.forhRab%: ' + ttPriKat.forhRab% + ' ' +        */
/*        'ttPriKat.PrisRab%: ' + STRING(ttPriKat.PrisRab%) + '. Butikk: ' + STRING(iPrikatButikkNr)*/
/*        ).                                                                                        */
            
         /* Outlet skal ha redusert utpris med samme faktor som innprisen. */
         /* Denne endringen skal også treffe prikat oppdateringen.         */ 
         IF iPrikatButikkNr = 10 OR iPriKatButikkNr = 40 THEN
         DO:
           ASSIGN 
               bOutlet              = TRUE 
               ttVre.MarkedsPris    = IF ttVre.PrisRab% > 0
                                        THEN ROUND(ttVre.MarkedsPris - ((ttVre.MarkedsPris * ttVre.PrisRab%) / 100),2)
                                        ELSE ttVre.MarkedsPris
/*               ttPriKat.MarkedsPris = STRING(ttVre.MarkedsPris)*/
               . 
         END.   
    END. /* PAKKSEDDEL */

    /* Logger ukjente EAN. De skal senere sjekkes og ArtikkelNr fylles ut. */
    /* Hensikten er å gjennfinne de artiklene som harfått opprettet en     */
    /* kalkyle på hk profil med Outlet rabatter.                           */
    IF CAN-DO(cOutletLst,STRING(ttPriKat.ButikkNr)) AND NOT CAN-FIND(Strekkode WHERE 
        Strekkode.Kode = ttPriKat.EanNr) THEN 
    DO:
        IF NOT CAN-FIND(FIRST tmpNyArt WHERE 
                            tmpNyArt.Kode = ttPrikat.EanNr) THEN 
        DO:
            CREATE tmpNyArt.
            ASSIGN 
                tmpNyArt.Kode = ttPrikat.EanNr
                tmpNyArt.Pris = DEC(ttPriKat.VeilPris)
                tmpNyArt.Rab% = DEC(ttPriKat.forhRab%)
                tmpNyArt.LevPrisEngros = DEC(ttPriKat.LevPrisEngros)
                .
        END.
    END.
    
    /* Logger Priser pr. EAN. når det leses inn priser på outlet  med kode 1 og 12. */
    /* Disse prisene skal også oppdatere hk profilen.                               */
    IF CAN-DO(cOutletLst,STRING(ttPriKat.ButikkNr)) AND NUM-ENTRIES(pcLinje,";") > 27 AND CAN-DO('1,12',TRIM(ENTRY(28,pcLinje,";"))) THEN 
    DO:
        IF NOT CAN-FIND(FIRST tmpNyArt WHERE 
                            tmpNyArt.Kode = ttPrikat.EanNr) THEN 
        DO:
            CREATE tmpNyArt.
            ASSIGN 
                tmpNyArt.Kode = ttPrikat.EanNr
                tmpNyArt.Pris = DEC(ttPriKat.VeilPris)
                tmpNyArt.Rab% = DEC(ttPriKat.forhRab%)
                tmpNyArt.LevPrisEngros = DEC(ttPriKat.LevPrisEngros)
                .
        END.
    END.

    STATUS DEFAULT "Lese linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.
   
  RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: LesInnFil: Lukker stream fra: ' + cFilNavn).

  /* TN 12/9-18 Lagt inn sjekk som fikser linjer som kommer med 0 i pris. */
  RUN SjekkPriser.

  /* Oppdaterer faste registre. */
  RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Starter oppdatering av faste registre.').
  RUN fasteregistre.
   
  /* Eksporterer til VPI fil. */
  RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Starter eksport av VPI fil.').
  RUN EksportVPIFil.

  /* Sikrer at alle varene er lagerstyrt før varemottak leses inn */
  RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Sikrer at alle varene er lagerstyrt før varemottak leses inn.').
  FOR EACH ArtBas EXCLUSIVE-LOCK WHERE 
    ArtBas.OPris = FALSE AND
    ArtBas.Lager = FALSE TRANSACTION:
    ArtBas.Lager = TRUE.
  END.

  /* Eksporterer til VPI fil. */
  /* 25/6-12 Varemottak via pakskeddel er koblet ut. Nå tas pakkseddlene via
     pakksedel varemottaket.
     
  RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Starter EksportVreFil.').
  RUN EksportVreFil.
  RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Ferdig EksportVreFil.').
  */

  TEMP-TABLE ttVre:WRITE-JSON('file', 'konv\tmpVre' + String(TIME) + '.json', TRUE).
   
  /* Legger opp pakksedler. */
  RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Starter EksportVreFil.').
  RUN OpprettPakksedler.
  RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Ferdig EksportVreFil.').

  /* Skriver liste med oppdaterte pakkseddler til fil. */
  RUN skrivPsdlListe.

  /* Stempler posten som innlest. */
  DO TRANSACTION:
      FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
      ASSIGN
          VPIFilHode.VPIFilStatus = 5
          .
  END.
  IF AVAILABLE VPIFilHode THEN
      FIND CURRENT VPIFilHode    NO-LOCK.

  IF CAN-FIND(FIRST tt_Error) THEN DO:
    RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Starter utskrift av feillogg.').
    RUN ErrorLogg.
  END.

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

  RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Ferdig med LesInnFil.').
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

/*  RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: OppdaterArtikkel start (Artikkel: ' + pcListe + ')').*/
/*  AUTOMIMPORT:                                                                                   */
/*  DO:                                                                                            */
/*      RUN SettAutoImport IN h_dvpiartbas (INPUT TRUE).                                           */
/*      RUN OpprettNy    IN h_dvpiartbas (piEkstVpiLevNr, pcListe, OUTPUT plArtikkelNr).           */
/*      IF RETURN-VALUE <> "" THEN                                                                 */
/*      DO:                                                                                        */
/*          /*                                                                                     */
/*          MESSAGE RETURN-VALUE                                                                   */
/*              VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                 */
/*          */                                                                                     */
/*          RETURN.                                                                                */
/*      END.                                                                                       */
/*      RUN OppdaterInfo IN h_dvpiartbas (piEkstVpiLevNr, pcListe, plArtikkelNr).                  */
/*  END. /* AUTOMIMPORT */                                                                         */
/*  RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: OppdaterArtikkel ferdig.').                          */

  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdatTellehode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdatTellehode Procedure 
PROCEDURE OppdatTellehode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER iTelleNr AS INT  NO-UNDO.
   
  DEF BUFFER bTelleHode FOR TelleHode.

  DEF VAR wNEdSkriv AS LOG NO-UNDO.

  RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: OppdatTellehode start: ' + STRING(iTelleNr)).

  DO TRANSACTION:
    FIND bTelleHode EXCLUSIVE-LOCK WHERE
        bTelleHode.TelleNr = iTelleNr NO-ERROR.
    IF NOT AVAILABLE Tellehode THEN
        RETURN.
    ASSIGN
      wNedSkriv             = IF TelleHode.TTId = 8 THEN TRUE ELSE FALSE
      bTelleHode.Oppdatert  = IF bTelleHode.Oppdatert <> ? 
                                THEN bTelleHode.Oppdatert
                                ELSE TODAY
      bTelleHode.AntallPar  = 0
      bTelleHode.AntallTalt = 0
      bTelleHode.OpptVerdi  = 0
      bTelleHode.VerdiDiff  = 0
      bTelleHode.AntallDiff = 0
      bTelleHode.OpprVerdi  = 0
      bTelleHode.AntLinjer  = 0.
    FOR EACH TelleLinje OF bTelleHode NO-LOCK:

      ASSIGN
        bTelleHode.AntallPar  = bTelleHode.AntallPar  + TelleLinje.AntallPar
        bTelleHode.AntallTalt = bTelleHode.AntallTalt + TelleLinje.AntallTalt
        bTelleHode.OpprVerdi  = bTelleHode.OpprVerdi  + TelleLinje.OpprVerdi      
        bTelleHode.OpptVerdi  = bTelleHode.OpptVerdi  + TelleLinje.OpptVerdi
        bTelleHode.AntLinjer  = bTelleHode.AntLinjer  + 1.      

      IF wNedSkriv THEN
        DO:
          ASSIGN
            bTelleHode.VerdiDiff  = bTelleHode.OpprVerdi  - bTelleHode.OpptVerdi
            bTelleHode.AntallDiff = bTelleHode.AntallPar  - bTelleHode.AntallTalt.      
        END.
      ELSE DO:
        ASSIGN
          bTelleHode.VerdiDiff  = bTelleHode.OpprVerdi  - bTelleHode.OpptVerdi
          bTelleHode.AntallDiff = bTelleHode.AntallPar  - bTelleHode.AntallTalt.      
      END.

    END.
  END.
  RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: OppdatTellehode slutt: ' + STRING(iTelleNr)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-opprettManglendeHKKalkyle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprettManglendeHKKalkyle Procedure
PROCEDURE opprettManglendeHKKalkyle:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

FIND FIRST ImpKonv NO-LOCK WHERE 
      ImpKonv.EDB-System = cEDB-System AND 
      ImpKonv.Tabell     = 'Def.Rab%' AND 
      ImpKonv.EksterntId = '10' NO-ERROR.
IF AVAILABLE ImpKonv 
      THEN ASSIGN 
          lforhRab%      = DEC(ImpKonv.Merknad)
          lPrisRab%      = DEC(ImpKonv.InterntId)
          .
ELSE 
    ASSIGN 
        lForHRab% = 0
        lPrisRab% = 0
        . 

GOD_MORGEN:
FOR EACH ArtBas NO-LOCK WHERE 
  ArtBas.EDato = TODAY AND 
  NOT CAN-FIND(ArtPris OF ArtBas WHERE 
               ArtPris.ProfilNr = 1):
                   
  /* Henter kalkyle fra Outlet */
  FIND ArtPris NO-LOCK WHERE 
    ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
    ArtPris.ProfilNr   = 2 NO-ERROR.
  IF AVAILABLE ArtPris THEN 
  DO:
      CREATE bArtPris.
      BUFFER-COPY ArtPris 
        EXCEPT ProfilNr
        TO bArtPris
        ASSIGN 
            bArtPris.ProfilNr = 1.
            
      /* Korrigerer kalkylen */
      ASSIGN 
        bArtPris.Pris[1]         = ROUND(
                                         ArtPris.Pris[1] / ((100 - lPrisRab%) / 100) 
                                         ,2) 
        bArtPris.InnkjopsPris[1] = ArtPris.InnkjopsPris[1]
        bArtPris.Rab1%[1]        = 0
        bArtPris.Varekost[1]     = ArtPris.InnkjopsPris[1] / ((100 - lForHRab%) / 100)                
        fMvaKr                   = bArtPris.Pris[1] - (bArtPris.Pris[1] / (1 + (bArtPris.Mva%[1] / 100)))
        fDbKr                    = bArtPris.Pris[1] - fMvaKr - bArtPris.Varekost[1]                   
        bArtPris.Db%[1]          = ROUND((fDbKr * 100) / (bArtPris.Pris[1] - fMvaKr),2)
        bArtPris.Db%[1]          = IF bArtPris.Db%[1] = ? THEN 0 ELSE bArtPris.Db%[1]
        .      
  END.

END. /* GOD_MORGEN */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-OpprettPakksedler) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettPakksedler Procedure 
PROCEDURE OpprettPakksedler :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR cButikkLst AS CHAR   NO-UNDO.

DEF VAR cPakkseddelLst  AS CHAR   NO-UNDO.
DEF VAR cOrdreLst       AS CHAR   NO-UNDO.
DEF VAR piLoop     AS INT    NO-UNDO.
DEF VAR pi2Loop    AS INT    NO-UNDO.
DEF VAR cOVreFil   AS CHAR   NO-UNDO.
DEF VAR iTelleNr   AS INT    NO-UNDO.
DEF VAR iHtFilId   AS INT    NO-UNDO.
DEF VAR iParaNr    AS INT    NO-UNDO.
DEFINE VARIABLE fMvaKr AS DECIMAL NO-UNDO.
DEFINE VARIABLE fDbKr  AS DECIMAL NO-UNDO.

DEFINE VARIABLE iLnr          AS INTEGER NO-UNDO.
DEFINE VARIABLE fPkSdlId      AS DECIMAL FORMAT ">>>>>>>>>>>>9".
DEFINE VARIABLE fPkSdlLinjeId AS DECIMAL.
DEFINE VARIABLE cEkstIds      AS CHARACTER NO-UNDO.

DEF BUFFER bSysPara FOR SysPara.

/* Bygger liste over butikker som skal ha varemottak */
FOR EACH ttVre
    BREAK BY ttVre.ButikkNr:
    /* Logger butikker */
    IF FIRST-OF(ttVre.ButikkNr) THEN
        ASSIGN
        cButikkLst = cButikkLst + 
                     (IF cButikkLst = ""
                        THEN ""
                        ELSE ",") + STRING(ttVre.ButikkNr,"999999")
        .
END.

RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: OpprettPakkseddel: Butikker med varemottak: ' + cButikkLst).

IF iAntLinjer > 0 AND cButikkLst <> "" THEN
BUTIKKLOOP:
DO piLoop = 1 TO NUM-ENTRIES(cButikkLst):

    RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: OpprettPakkseddel: Varemottak for butikk: ' + ENTRY(piLoop,cButikkLst)).

    FIND Butiker NO-LOCK WHERE
      Butiker.Butik = INT(ENTRY(piLoop,cButikkLst)) NO-ERROR.

    ASSIGN
        cPakkseddelLst = ""
        cOrdreLst      = ""
        .
    /* Lister opp alle ordrene for butikken. */
    FOR EACH ttVre WHERE 
        ttVre.ButikkNr = INT(ENTRY(piLoop,cButikkLst))
        BREAK BY ttVre.ButikkNr
              /*BY ttVre.OrdreNr*/
              BY ttVre.PakkseddelNr:
        
        IF NOT cEkstIds MATCHES("*" + STRING(ttVre.OrdreNr) + "*") THEN           
            cEkstIds = cEkstIds + (IF cEkstIds = '' THEN '' ELSE '|') + STRING(ttVre.OrdreNr).         
                  
        IF LAST-OF(ttVre.PakkseddelNr) THEN
        ASSIGN
            cPakkseddelLst  = cPakkseddelLst  + 
                         (IF cPakkseddelLst  = ""
                            THEN ""
                            ELSE ",") + STRING(ttVre.PakkSeddelNr)
            cOrdreLst  = cOrdreLst + 
                         (IF cOrdreLst  = ""
                            THEN ""
                            ELSE ",") + cEkstIds
            cEkstIds   = ''
            .
    END.

    ORDRELISTE:
    DO pi2Loop = 1 TO NUM-ENTRIES(cPakkseddelLst) TRANSACTION:
        RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: OpprettPakkseddel: Start med ordre: ' + ENTRY(pi2Loop,cPakkseddelLst)).

        ASSIGN
          fPkSdlId      = 0
          iLnr          = 0
          fPkSdlLinjeId = 0.

        /* Er pakkseddelen innlest fra før, skal den ikke leses inn igjen. */
        IF CAN-FIND(LAST PkSdlHode WHERE 
                    PkSdlHode.PkSdlNr = LEFT-TRIM(ENTRY(pi2Loop,cPakkseddelLst),"0") AND 
                    /*PkSdlHode.EkstId  = LEFT-TRIM(ENTRY(pi2Loop,cOrdreLst),"0") AND*/ 
                    PkSdlHode.PkSdlStatus = 10
                    ) THEN 
            NEXT ORDRELISTE.

        /* Oppretter Pakkseddel for varemottak */
        PKSDLHODE:
        DO:
            ASSIGN 
                lSumLandedCost = 0.
            FOR EACH ttVre WHERE
                ttVre.ButikkNr     = INT(ENTRY(piLoop,cButikkLst)) AND
                ttVre.PakkseddelNr = (ENTRY(pi2Loop,cPakkseddelLst)):
                ASSIGN lSumLandedCost = lSumLandedCost + (ttVre.Antall * ttVre.LandedCost).    
            END.
            FIND FIRST ttVre WHERE 
                ttVre.ButikkNr     = INT(ENTRY(piLoop,cButikkLst)) AND
                ttVre.PakkseddelNr = (ENTRY(pi2Loop,cPakkseddelLst)) NO-ERROR.
            
            FIND LAST PkSdlHode NO-LOCK NO-ERROR.
            CREATE PkSdlHode.
            ASSIGN PkSdlHode.PkSdlStatus    = 10
                   PkSdlHode.SendtDato      = TODAY
                   fPkSdlId                 = PkSdlHode.PkSdlId
                   PkSdlHode.Merknad        = "GANT GLOBAL" + CHR(10) + LEFT-TRIM(ENTRY(pi2Loop,cOrdreLst),"0")
                   PkSdlHode.CL             = iCl
                   PkSdlHode.PkSdlNr        = LEFT-TRIM(ENTRY(pi2Loop,cPakkseddelLst),"0")
                   PkSdlHode.EkstId         = LEFT-TRIM(ENTRY(pi2Loop,cOrdreLst),"0")
                   PkSdlHode.LevNr          = iLevNr
                   PkSdlHode.OrdreType      = ttVre.OrdreType
                   PkSdlHode.SesongKode     = ttVre.Sesong
                   PkSdlHode.LandedCost     = lSumLandedCost
                   PkSdlHode.MeldingFraLev  = 'Ordretype: ' + (IF AVAILABLE ttVre THEN ttVre.OrdreType ELSE '') + CHR(10) +
                                              'Sesongkode: ' + (IF AVAILABLE ttVre THEN ttVre.Sesong ELSE '') + CHR(10) + 
                                              'LandedCost: ' + STRING(lSumLandedCost) + CHR(10) +
                                              'Fil: ' + VPIFilHode.FilNavn + CHR(10) +
                                              'Katalog: ' +  VPIFilHode.Katalog
                   .
                   IF AVAILABLE ttVre THEN
                   DO:
                       PkSdlHode.ButikkNr = ttVre.ButikkNr.
                       IF CAN-DO(cOutletLst,STRING(ttVre.ButikkNr)) THEN 
                            PkSdlHode.PkSdlOpphav = 3. /* TN 8/1-20 Endret fra 5 */
                       ELSE 
                            PkSdlHode.PkSdlOpphav = 1.
                   END.
                   ELSE PkSdlHode.PkSdlOpphav = 1.
                   .
        END. /* PKSDLHODE */

        /* Setter linjeId. */
        FIND LAST PkSdlLinje NO-LOCK
             WHERE PkSdlLinje.PkSdlId = fPkSdlId
             NO-ERROR.
        fPkSdlLinjeId = IF AVAIL PkSdlLinje THEN PkSdlLinje.PkSdlLinjeId + 1 ELSE 1.

        OPPRETT_LINJER:                    
        FOR EACH ttVre WHERE
            ttVre.ButikkNr     = INT(ENTRY(piLoop,cButikkLst)) AND
            ttVre.PakkseddelNr = (ENTRY(pi2Loop,cPakkseddelLst)):

            ASSIGN 
              ttVre.PkSdl_Id     = STRING(fPkSdlId)
              .
            /* Sikrer at EAN kode finnes for størrelsen */
            /* Genererer EAN koder hvis det ikke er med fra før. */
            IF ttVre.Kode = "" THEN
            STREKKODE:
            DO:
                FIND FIRST ArtBas NO-LOCK WHERE
                    ArtBas.LevKod     = ttVre.LevModellNr AND
                    ArtBas.Beskr      = ttVre.VareTekst AND
                    ArtBAs.LevFargKod = ttVre.FargeTekst NO-ERROR.
                IF AVAILABLE ArtBas THEN DO:
                    RUN FixStorl (INPUT-OUTPUT ttVre.Str).
                    RUN genEAN (ArtBas.ArtikkelNr,ttVre.Str). 
                END.
                FIND FIRST StrKonv NO-LOCK WHERE
                    StrKonv.Storl = ttVre.STr NO-ERROR.
                IF AVAILABLE ArtBas AND AVAILABLE StrKonv THEN
                    FIND FIRST Strekkode NO-LOCK WHERE
                    Strekkode.ArtikkelNr = ArtBas.ArtikkelNr AND
                    Strekkode.StrKode    = StrKonv.StrKode NO-ERROR.
                IF AVAILABLE Strekkode THEN
                    ttVre.Kode = Strekkode.Kode.
            END. /* STREKKODE*/

            /* Slipper artikkel før nytt oppslag. */
            IF AVAILABLE ArtBas THEN
                RELEASE ArtBas.
            /* Henter artikkel */            
            FIND Strekkode NO-LOCK WHERE
              Strekkode.Kode = ttVre.Kode NO-ERROR.
            IF AVAILABLE Strekkode THEN 
              FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.    
            IF AVAILABLE Strekkode THEN 
              FIND StrKonv NO-LOCK WHERE 
                StrKonv.StrKode = Strekkode.StrKode NO-ERROR.          

            IF NOT AVAILABLE ArtBas THEN 
              NEXT OPPRETT_LINJER. 
              
            CREATE PkSdlLinje.
            ASSIGN iLnr                     = iLnr + 1 
                   PkSdlLinje.Linjenr       = iLnr
                   PkSdlLinje.PkSdlLinjeId  = fPkSdlLinjeId
                   PkSdlLinje.PkSdlId       = fPkSdlId
                   PkSdlLinje.ArtikkelNr    = (IF AVAILABLE Strekkode THEN Strekkode.ArtikkelNr ELSE 0)
                   PkSdlLinje.BestNr        = 0
                   PkSdlLinje.OrdreNr       = 0
                   PkSdlLinje.Beskr         = (IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE '')
                   PkSdlLinje.LevFargKod    = (IF AVAILABLE ArtBas THEN ArtBas.LevFargKod ELSE '')
                   PkSdlLinje.Antall        = ttVre.Antall
                   PkSdlLinje.AntLevert     = ttVre.Antall
                   PkSdlLinje.LevKod        = ArtBas.LevKod
                   PkSdlLinje.LevNr         = ArtBas.LevNr
                   PkSdlLinje.StrKode       = (IF AVAILABLE Strekkode THEN Strekkode.StrKode ELSE 0)
                   PkSdlLinje.Kode          = ttVre.Kode
                   PkSdlLinje.Salgsenhet    = ArtBas.SalgsEnhet
                   PkSdlLinje.ButikkNr      = ttVre.ButikkNr
                   PkSdlLinje.Pakke         = FALSE 
                   PkSdlLinje.PakkeNr       = 0
                   PkSdlLinje.NySesongkode  = ttVre.Felt8
                   fPkSdlLinjeId            = fPkSdlLinjeId + 1
                   .
            
            
            /* Oppretter pakkseddel pris */
            FIND FIRST ArtPris NO-LOCK
                 WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr
                   AND ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
            IF NOT AVAILABLE ArtPris THEN 
                FIND FIRST ArtPris NO-LOCK
                     WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr
                     AND ArtPris.ProfilNr     = iClProfilNr NO-ERROR.
        
            FIND PkSdlPris EXCLUSIVE-LOCK WHERE
              PkSdlPris.PkSdlId    = fPkSdlId AND
              PkSdlPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
            IF NOT AVAILABLE PkSdlPris THEN 
            DO:
                CREATE PkSdlPris.
                ASSIGN
                    PkSdlPris.PkSdlId    = fPkSdlId
                    PkSdlPris.ArtikkelNr = ArtBas.ArtikkelNr.        
                BUFFER-COPY ArtBas   
                  EXCEPT    ArtikkelNr BrukerId EDato Etid RegistrertDato RegistrertTid RegistrertAv 
                  TO        PkSdlPris.
            END.
            ASSIGN 
                   PkSdlPris.VareKost       = ArtPris.VareKost[1]
                   PkSdlPris.Rab1%          = ArtPris.Rab1%[1]
                   PkSdlPris.Pris           = ArtPris.Pris[1]
                   PkSdlPris.Frakt          = ArtPris.Frakt[1]
                   PkSdlPris.Db%            = ArtPris.Db%[1]
                   PkSdlPris.InnkjopsPris   = ArtPris.InnkjopsPris[1]
                   PkSdlPris.OverstyrPris   = bStdPrisOverf
                   /* Ny pris som skal gjelde i butikken */
                   PkSdlPris.NyPris         = ttVre.MarkedsPris
                   PkSdlPris.NyVarekost     = ttVre.InnkjopsPris 
                   PkSdlPris.NyRab1%        = ttVre.forhRab%
                   PkSdlPris.NyInnkjopsPris = ttVre.LevPrisEngros
                   PkSdlPris.NyFrakt        = 0
                   fMvaKr                   = PkSdlPris.NyPris - (PkSdlPris.NyPris / (1 + (ArtPris.Mva%[1] / 100)))
                   fDbKr                    = PkSdlPris.NyPris - fMvaKr - PkSdlPris.NyVarekost                   
                   PkSdlPris.NyDB%          = ROUND((fDbKr * 100) / (PkSdlPris.NyPris - fMvaKr),2)
                   PkSdlPris.NyDB%          = IF PkSdlPris.NyDB% = ? THEN 0 ELSE PkSdlPris.NyDB%
                   PkSdlPris.OverstyrPris   = YES
                   .
        END. /* OPPRETT_LINJER */
        
        /* Skriver ut etiketter - hvis det er aktivert etikettutskrift for batch. */
        RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: OpprettPakksedel: Logger etikett.').
        FIND Butiker NO-LOCK WHERE
            Butiker.Butik = INT(ENTRY(piLoop,cButikkLst)) NO-ERROR.
        IF AVAILABLE Butiker THEN
        DO:
            IF Butiker.BEAktiv THEN DO:
                CREATE tmpEtikettLogg.
                ASSIGN
                    tmpEtikettLogg.TelleNr          = fPkSdlId
                    tmpEtikettLogg.cType            = 'PKSDL'
                    tmpEtikettLogg.BELayout         = Butiker.BELayout
                    tmpEtikettLogg.BEPrinter        = Butiker.BEPrinter
                    tmpEtikettLogg.BETerminalklient = Butiker.BETerminalklient
                    .
            END.
        END.
        
        IF CAN-FIND(PkSdlHode WHERE 
                        PkSdlHode.PkSdlId = fPkSdlId) THEN 
          DO:
            RUN PkSdlSetLandedCost.p (STRING(fPkSdlId), ?, '', OUTPUT ocReturn, OUTPUT obOk) NO-ERROR. /* Nytt format */
            IF ERROR-STATUS:ERROR THEN 
              RUN PkSdlSetLandedCost.p (STRING(fPkSdlId)) NO-ERROR. /* Gammelt format. */
          END.    
                
        /* Sjekker alle varelinjer på pakkseddelen. er det feil på koblingen m.m. varsles dette med en email. */
        RUN sjekkStrekkoder (ENTRY(pi2Loop,cPakkseddelLst)).

        /* Varsler på mail om import av stock ordre. 1 og 12 er forwared ordre. */
        IF NOT CAN-DO('1,12',PksdlHode.OrdreType) THEN 
          RUN sendStockOrdreMelding (PkSdlHode.butikkNr, PkSdlHode.PkSdlId).

        IF iGantAktiv = 1 AND 
          CAN-DO(cKommisjonsButLst,STRING(PkSdlHode.butikkNr)) THEN 
          DO:
            rPakkseddel:EtikettUtskrift( INPUT PkSdlHode.PkSdlId, PkSdlHode.butikkNr  ).
            RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: OpprettPakkseddel: RFID etikett: ' + 
                             ENTRY(pi2Loop,cPakkseddelLst) + ' ' + 
                             'Id: ' + STRING(PkSdlHode.PkSdlId) + ' ' +
                             'ButikkNr: ' + STRING(PkSdlHode.ButikkNr)
                             ).
            RUN EDIKommisjon.p ( INPUT PkSdlHode.PkSdlId, cLogg ).
            RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: EDI filer: ' + 
                             ENTRY(pi2Loop,cPakkseddelLst) + ' ' + 
                             'Id: ' + STRING(PkSdlHode.PkSdlId) + ' ' +
                             'PkSdlNr: ' + STRING(PkSdlHode.PkSdlNr)
                             ).
            rPakkseddel:prisOppdatering(  INPUT PkSdlHode.PkSdlId ). 
            
            /* For kommisjonsbutikker. */
            RUN pksdl_korrVarekost.p (PkSdlHode.PkSdlId).
          END.

        RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: OpprettPakkseddel: Ferdig med ordre: ' + ENTRY(pi2Loop,cPakkseddelLst)).
    END. /* ORDRELISTE */
END. /* BUTIKKLOOP */
RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: OpprettPakkseddel: Varemottak ferdig.').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pksdlnr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pksdlnr Procedure 
PROCEDURE pksdlnr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO TRANSACTION:
    FIND SysPara EXCLUSIVE-LOCK WHERE 
      SysPara.SysHId = 210 AND
      SysPara.SysGr  = 100 AND
      SysPara.ParaNr = 3 NO-ERROR.
    IF NOT AVAILABLE sysPara THEN
    DO:
        CREATE sysPara.
        ASSIGN
            SysPara.SysHId       = 210 
            SysPara.SysGr        = 100 
            SysPara.ParaNr       = 3 
            SysPara.Beskrivelse  = "Pakkseddel filnummer"
            SysPara.Hjelpetekst1 = "Nummer som øker til 9999999. Deretter starter det om fra 0 igjen."
            .
    END.
    ASSIGN
        ipksdlnr = INT(SysPara.Parameter1) + 1
        SysPara.Parameter1 = STRING(ipksdlnr)
        .
    IF ipksdlnr > 9999999 THEN
        ASSIGN
            ipksdlnr = 1
            SysPara.Parameter1 = STRING(ipksdlnr)
            .

    RELEASE SysPara.
END.
              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RensVreFi) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RensVreFi Procedure 
PROCEDURE RensVreFi :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendPrisFeilLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendPrisFeilLogg Procedure
PROCEDURE SendPrisFeilLogg:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER icFil AS CHAR NO-UNDO.
    
    FILE-INFO:FILE-NAME = icFil.

    rSendEMail:parMailType = 'PAKKSEDDEL'.
    rSendEMail:parSUBJECT  = 'Pakkseddel importert ' + STRING(NOW) + '.'.
    rSendEMail:parMESSAGE  = 'FEIL PRIS i Pakkseddel fra Gant global ' + icFil + '.'.
    rSendEMail:parFILE     = FILE-INFO:FULL-PATHNAME.  
    obOk = rSendEMail:send( ).
                        
    IF ERROR-STATUS:ERROR OR obOk = FALSE THEN 
        DO:
            RUN bibl_loggDbFri.p (cLogg,'    **FEIL. eMail ikke sendt. Vedlegg ' + FILE-INFO:FULL-PATHNAME + '.').
            DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
                RUN bibl_loggDbFri.p (cLogg, '          ' 
                    + STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix)    
                    ).
            END.            
        END.

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-sendStokOrdreMelding) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendStockOrdreMelding Procedure
PROCEDURE sendStockOrdreMelding:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER piButNr AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER plPksdlId LIKE PkSdlHode.PkSdlId NO-UNDO.
    
    DEFINE VARIABLE picFil AS CHAR NO-UNDO.
    DEFINE VARIABLE pcButLst AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pcNettButLst AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pcMottager AS CHARACTER NO-UNDO.

    /* eMail varsling ikke aktiv. */
    IF iSendeMailStockOrdre <> 1 THEN
    DO: 
      RUN bibl_logg.p (cLogg, '  sendStockOrdreMelding: Ikke aktiv.').
      RETURN.
    END.    

    RUN bibl_logg.p (cLogg, '  sendStockOrdreMelding: Pksdl ' + STRING(plPksdlId) + '.').
      
    {syspara.i 22 1 11 pcButLst}
    {syspara.i 22 1 12 pcNettButLst}
    IF CAN-DO(pcButLst,STRING(piButNr)) THEN 
      {syspar2.i 22 1 11 pcMottager}
    ELSE IF CAN-DO(pcNettButLst,STRING(piButNr)) THEN 
      {syspar2.i 22 1 12 pcMottager}
    /* Mottagers eMail er ikke satt opp. */
    IF pcMottager = '' THEN
    DO: 
      RETURN.
    END.
      
    RUN skrivpakkseddel.p (STRING(PkSdlHode.PkSdlId) + "|",FALSE,'dummy',1,"",10).
    picFil = getPkSdlFilNavn().
    IF picFil <> '' THEN     
      FILE-INFO:FILE-NAME = picFil.

    rSendEMail:parMailType  = 'PAKKSEDDEL'.
    rSendEMail:parSUBJECT   = 'Pakkseddel med supplering er importert ' + STRING(NOW) + '.'.
    rSendEMail:parMESSAGE   = 'Pakkseddel med suppleringsvarer til butikk ' + STRING(piButNr) + ' er importert.'.
    rSendEMail:parToADDRESS = pcMottager. 
    rSendEMail:parFILE      = IF picFil <> '' THEN FILE-INFO:FULL-PATHNAME ELSE ''.   
    obOk = rSendEMail:send( ).
                        
    IF ERROR-STATUS:ERROR OR obOk = FALSE THEN 
        DO:
            RUN bibl_loggDbFri.p (cLogg,'    **FEIL. eMail ikke sendt. Vedlegg ' + FILE-INFO:FULL-PATHNAME + '.').
            DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
                RUN bibl_loggDbFri.p (cLogg, '          ' 
                    + STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix)    
                    ).
            END.            
        END.


END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-sendStockPkSdlMail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendStockPkSdlMail Procedure
PROCEDURE sendStockPkSdlMail:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER picFil AS CHARACTER NO-UNDO.
  
  ASSIGN 
    icFil = picFil
    .

  RUN bibl_logg.p (cLogg, '  sendStockPkSdlMail: Fil ' + picFil + '.').

END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-Sett_Farge_Fra_LevFargKod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Sett_Farge_Fra_LevFargKod Procedure 
PROCEDURE Sett_Farge_Fra_LevFargKod :
/*------------------------------------------------------------------------------
  Purpose:     Initierer fargekoderegisteret med fargene fra Gant Global.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Sett_Farge_Fra_LevFargKod: Starter.').

DEFINE BUFFER ArtBas FOR ArtBas.

DEF VAR iXAnt AS INT NO-UNDO.

RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Sett_Farge_Fra_LevFargKod: Test-1.').

FOR EACH ArtBas EXCLUSIVE-LOCK WHERE 
  ArtBas.EDato = TODAY AND 
  ArtBas.LevFargKod > "" TRANSACTION:
  ASSIGN
      iXAnt = int(ENTRY(1,ArtBas.LevFargKod,"/")) 
      no-error.
  IF ERROR-STATUS:ERROR = FALSE AND iXAnt < 99999 THEN
  DO:
    FIND FIRST Farg NO-LOCK WHERE 
        Farg.KFarge = ArtBas.LevFargKod NO-ERROR.
    IF AVAILABLE Farg THEN 
        ASSIGN
            ArtBas.Farg = Farg.Farg.
    ELSE
    DO:
      FIND LAST Farg NO-ERROR.
      IF AVAILABLE Farg THEN 
        iXAnt = Farg.Farg + 1.
      ELSE 
        iXAnt = 1.
        
      CREATE Farg.
      ASSIGN
          farg.farg     = iXAnt
          farg.farbeskr = ArtBas.LevFargKod /*entry(2,ArtBas.LevFargKod,"/")*/
          Farg.KFarge   = ArtBas.LevFargKod
          no-error.
    END.
    IF AVAILABLE Farg THEN
    ASSIGN  
        farg.farbeskr = ArtBas.LevFargKod /*entry(2,ArtBas.LevFargKod,"/")*/
        Farg.KFarge   = ArtBas.LevFargKod 
        no-error.
  END.
END.
IF AVAILABLE ArtBas THEN
    RELEASE ArtBas.

RUN bibl_logg.p (cLogg, 'xgantpkinnles.p: Sett_Farge_Fra_LevFargKod: Ferdig.').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SjekkPriser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkPriser Procedure
PROCEDURE SjekkPriser:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    
    FOR EACH bufttPrikat:
        /* Bare linjer uten pris skal behandles her. */
        IF NOT (DEC(bufttPriKat.VeilPris) = 0 OR 
               DEC(bufttPriKat.LevPrisEngros) = 0) THEN 
               NEXT.
             
        /* TN 24/9-19 Lagt inn logging av linjer som er kommet uten pris. */
        FIND StrekKode NO-LOCK WHERE 
            StrekKode.Kode = bufttPriKat.EanNr NO-ERROR.            
        RUN bibl_logg.p (cLoggFeilPris, cFilNavn + 
                                        ' Butikk: ' + STRING(bufttPriKat.ButikkNr) + 
                                        ' Ean mangler pris:' + bufttPriKat.EanNr + 
                                        ' (ArtikkelNr: ' + (IF AVAILABLE Strekkode THEN STRING(StrekKode.ArtikkelNr) ELSE "Ny artikkel")  + 
                                        ')' +  bufttPriKat.Str + ' på linje ' + STRING(bufttPriKat.LinjeNr) + '.').
             
        /* Hvis varen ligger på en av andre varelinjene i pakkseddelen, hentes prisen derfra. */     
        FIND FIRST ttPriKat WHERE 
            ttPriKat.LevModellNr = bufttPriKat.LevModellNr AND 
            ttPriKat.VareTekst   = bufttPriKat.VareTekst AND 
            ttPriKat.Sesong      = bufttPriKat.Sesong AND             
            (DEC(ttPriKat.VeilPris) > 0 OR DEC(ttPriKat.LevPrisEngros) > 0) NO-ERROR.
        IF AVAILABLE ttPriKat THEN
        FRISKOPP: 
        DO:          
            ASSIGN 
                bufttPriKat.LevPrisEngros = ttPriKat.LevPrisEngros
                bufttPriKat.forhRab%      = ttPriKat.forhRab%     
                bufttPriKat.suppRab%      = ttPriKat.suppRab%     
                bufttPriKat.VeilPris      = ttPriKat.VeilPris     
                bufttPriKat.nettoForh     = ttPriKat.nettoForh    
                bufttPriKat.MarkedsPris   = ttPriKat.MarkedsPris  
                bufttPriKat.KjedeInnkPris = ttPriKat.KjedeInnkPris        
                .
            FOR EACH ttVre WHERE 
                ttVre.LevModellNr   = bufttPriKat.LevModellNr AND 
                ttVre.VareTekst     = bufttPriKat.VareTekst  AND 
                ttVre.FargeTekst    = bufttPriKat.FargeTekst AND 
                ttVre.Str           = bufttPriKat.Str:

                ASSIGN 
                    ttVre.LevPrisEngros = DEC(bufttPriKat.LevPrisEngros) 
                    ttVre.VeilPris      = DEC(bufttPriKat.VeilPris)
                    ttVre.nettoForh     = DEC(bufttPriKat.nettoForh)
                    ttVre.MarkedsPris   = DEC(bufttPriKat.MarkedsPris)
                    ttVre.InnkjopsPris  = IF ttVre.forhRab% > 0 
                                              THEN ttVre.LevPrisEngros - ((ttVre.LevPrisEngros * ttVre.forhRab%) / 100)
                                              ELSE ttVre.LevPrisEngros
                    ttVre.LandedCost    = bufttPriKat.KjedeInnkPris
                    .
                /* Setter Outlet rabatt%. */
                IF DEC(bufttPriKat.forhRab%) = 0 AND CAN-DO('10,40',STRING(ttPriKat.ButikkNr)) THEN 
                DO:
                    FIND FIRST ImpKonv NO-LOCK WHERE 
                        ImpKonv.EDB-System = cEDB-System AND 
                        ImpKonv.Tabell     = 'Def.Rab%' AND 
                        ImpKonv.EksterntId = STRING(bufttPriKat.ButikkNr) NO-ERROR.
                    IF AVAILABLE ImpKonv 
                        THEN ASSIGN 
                            ttVre.forhRab%      = DEC(ImpKonv.Merknad)
                            ttVre.PrisRab%      = DEC(ImpKonv.InterntId)
                            ttVre.InnkjopsPris  = IF ttVre.forhRab% > 0 
                                                      THEN ttVre.LevPrisEngros - ((ttVre.LevPrisEngros * ttVre.forhRab%) / 100)
                                                      ELSE ttVre.LevPrisEngros
                            /* Dette gjøres for at HK pris skal bli riktig på helt nye artikler. */
                            ttPriKat.forhRab%      = "10"
                            ttPriKat.suppRab%      = ""
                            . 
                END.
                /* Setter default rabatt%. */
                ELSE IF DEC(bufttPriKat.forhRab%) = 0 THEN 
                DO: 
                    FIND FIRST ImpKonv NO-LOCK WHERE 
                        ImpKonv.EDB-System = cEDB-System AND 
                        ImpKonv.Tabell     = 'Def.Rab%' AND 
                        ImpKonv.EksterntId = STRING(bufttPriKat.ButikkNr) NO-ERROR.
                    IF AVAILABLE ImpKonv 
                        THEN ASSIGN 
                            ttVre.PrisRab%      = DEC(ImpKonv.InterntId)
                            .
                    ELSE 
                        ASSIGN 
                            ttVre.PrisRab%      = DEC(bufttPriKat.PrisRab%).
                    ASSIGN 
                    ttVre.forhRab%      = DEC(bufttPriKat.forhRab%)
                    ttVre.InnkjopsPris  = IF ttVre.forhRab% > 0 
                                            THEN ttVre.LevPrisEngros - ((ttVre.LevPrisEngros * ttVre.forhRab%) / 100)
                                          ELSE ttVre.LevPrisEngros 
                    .
                END.
            END.        
        END. /* FRISKOPP */
    END.
END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-sjekkStrekkoder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sjekkStrekkoder Procedure
PROCEDURE sjekkStrekkoder:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pcPkSdlNr AS CHARACTER NO-UNDO.
     
    DEFINE VARIABLE pbGyldig AS LOG NO-UNDO.
     
    DEFINE BUFFER bufPkSdlHode FOR PkSdlHode.
    
    FIND LAST bufPkSdlHode NO-LOCK WHERE 
        bufPkSdlHode.PkSDlNr = pcPkSdlNr NO-ERROR.

    IF AVAILABLE bufPkSdlhode THEN 
    DO:
        pbGyldig = TRUE.
        BLOKK1:
        FOR EACH PkSdlLinje OF bufPkSdlHode NO-LOCK:
            IF PkSdlLinje.Kode = '' THEN 
            DO:
               FIND FIRST Strekkode NO-LOCK 
                    WHERE Strekkode.ArtikkelNr = PkSdlLinje.ArtikkelNr
                      AND Strekkode.StrKode    = PkSdlLinje.StrKode
                      AND NOT Strekkode.Kode   BEGINS "02" 
                    NO-ERROR.
                pbGyldig = AVAIL StrekKode.
            END.
            ELSE DO:
               FIND FIRST Strekkode NO-LOCK 
                    WHERE Strekkode.Kode = PkSdlLinje.Kode NO-ERROR.
               IF NOT AVAILABLE(Strekkode) THEN 
                  pbGyldig = AVAIL StrekKode.
            END.
            IF pbGyldig = FALSE THEN 
              LEAVE BLOKK1.
        END. /* BLOKK1 */
        
        IF pbGyldig THEN 
        BLOKK2:
        FOR EACH PkSdlLinje OF bufPkSdlHode NO-LOCK:
            FIND FIRST Strekkode NO-LOCK 
                 WHERE Strekkode.Kode = PkSdlLinje.Kode NO-ERROR.
                 
            IF TRIM(PkSdlLinje.Kode) = '' OR NOT AVAILABLE Strekkode THEN 
                pbGyldig = FALSE.
            ELSE pbGyldig = IF (Strekkode.ArtikkelNr <> PkSdlLinje.ArtikkelNr OR 
                               Strekkode.StrKode    <> PkSdlLinje.StrKode) THEN FALSE ELSE TRUE.
            IF pbGyldig = FALSE THEN 
              LEAVE BLOKK2.
        END. /* BLOKK2 */
        IF pbGyldig = FALSE THEN 
        DO:
            rSendEMail:parMailType = 'PAKKSEDDEL'.
            rSendEMail:parSUBJECT  = '** FEIL i pakkseddel ' + bufPkSdlHode.PkSdlNr + ' som ble importert **' + STRING(NOW) + '.'.
            rSendEMail:parMESSAGE  = 'Pakkseddel: ' + bufPkSdlHode.PkSdlNr + ' Mangler strekkode, feilkoblet strekkode eller størrelse på en eller flere varelinjer.'.
            obOk = rSendEMail:send( ).
        END.
    END.
END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-skrivPsdlListe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skrivPsdlListe Procedure 
PROCEDURE skrivPsdlListe :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

/* Bygger liste over butikker som skal ha varemottak */
FOR EACH ttVre
    BREAK BY ttVre.ButikkNr
          BY ttVre.OrdreNr
          BY ttVre.PakkseddelNr:
    /* Logger butikker */
    IF FIRST-OF(ttVre.PakkseddelNr) THEN
      RUN bibl_logg.p ('GGPLog' + REPLACE(STRING(TODAY),"/","-"), VPIFilHode.FilNavn + ' Butikk: ' + 
                        STRING(ttVre.ButikkNr)  + ' Pksdlnr: ' +
                        STRING(ttVre.PakkSeddelNr) + ' OrdreNr: ' + 
                        STRING(ttVre.OrdreNr) + ' PakkseddelID: ' +
                        STRING(ttVre.PkSdl_Id))
                        .
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StrType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StrType Procedure 
PROCEDURE StrType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop      AS INT  NO-UNDO.
DEF VAR pcStrTypeId AS CHAR NO-UNDO.
DEF VAR pcTekst     AS CHAR NO-UNDO.
DEF VAR pcForste    AS CHAR NO-UNDO.
DEF VAR pcSiste     AS CHAR NO-UNDO.
DEF VAR pcStr       AS CHAR NO-UNDO.
DEF BUFFER bStrKonv FOR StrKonv.
DEF BUFFER bStrTStr FOR StrTStr.

/*
ASSIGN
  pcStrTypeId = ttPriKat.StrTab
  pcTekst     = ttPriKat.Str
  .

FIND FIRST StrType NO-LOCK WHERE
    StrType.StrtypeId = int(pcStrTypeId) NO-ERROR.
IF NOT AVAILABLE StrType THEN
DO:
    CREATE StrType.
    ASSIGN
        StrType.StrTypeId = INT(pcStrTypeId)
        .
END.
/* Finner første og siste størrelse.       */
/* Oppretter størrelsene i størrelsestypen */
ASSIGN
    pcForste = ""
    pcSiste  = ""
    .
STRLOOP:
DO piLoop = 1 TO NUM-ENTRIES(pcTekst,";") TRANSACTION:
    /* skipper blanke. */
    IF ENTRY(piLoop,pcTekst,";") = "" THEN
        NEXT.

    ASSIGN
        pcStr = ENTRY(piLoop,pcTekst,";")
        .

    /* Sjekker at den finnes i StrKonv tabellen */
    FIND StrKonv NO-LOCK WHERE
        StrKonv.Storl = pcStr NO-ERROR.
    IF NOT AVAILABLE StrKonv THEN
    DO:
        FIND LAST bStrKonv USE-INDEX StrKode NO-LOCK NO-ERROR.
        CREATE StrKonv.
        IF AVAILABLE bStrKonv THEN
            ASSIGN
            StrKonv.StrKode = bStrKonv.StrKode + 1
            StrKonv.Storl   = pcStr
            .
        ELSE
            ASSIGN
            StrKonv.StrKode = 1
            StrKonv.Storl   = pcStr
            .
    END. 
    
    /* Legger opp størrelsen for størrelsestypen. */
    IF NOT CAN-FIND(StrTStr WHERE
                    StrTStr.StrTypeId = StrType.StrTypeId AND
                    StrTStr.SoStorl   = pcStr) THEN
    DO:
        FIND LAST bStrTstr WHERE
            bStrTstr.StrTypeId = StrType.StrTypeId USE-INDEX StrTStr NO-ERROR.
        CREATE StrTstr.
        IF AVAILABLE bStrTstr THEN
            ASSIGN
            StrTStr.StrTypeId = StrType.StrTypeId
            StrTStr.SeqNr     = bStrTStr.SeqNr + 1
            StrTStr.SoStorl   = pcStr
            .
        ELSE
            ASSIGN
            StrTStr.StrTypeId = StrType.StrTypeId
            StrTStr.SeqNr     = 1
            StrTStr.SoStorl   = pcStr
            .
    END.
END. /* STRLOOP */

DO TRANSACTION:
    FIND CURRENT StrType EXCLUSIVE-LOCK.
    ASSIGN
        StrType.Beskrivelse   = pcStrTypeId
        StrType.KortNavn      = pcStrTypeId
        NO-ERROR.
END.
FIND CURRENT StrType NO-LOCK.
RUN settStrTypeFelt.p (StrType.StrTypeId).

IF AVAILABLE StrType THEN
    RELEASE StrType.
IF AVAILABLE StrTStr THEN
    RELEASE StrTStr.
IF AVAILABLE StrKonv THEN
    RELEASE StrKonv.
*/

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

&IF DEFINED(EXCLUDE-getPkSdlFilNavn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPkSdlFilNavn Procedure
FUNCTION getPkSdlFilNavn RETURNS CHARACTER 
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE result AS CHARACTER NO-UNDO.
    ASSIGN 
      RESULT = icFil.
      
    RUN bibl_logg.p (cLogg, '  getPkSdlFilNavn: Fil ' + icFil + '.').
      
    RETURN result.

END FUNCTION.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

