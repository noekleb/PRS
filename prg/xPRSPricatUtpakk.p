&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xPRSPricatUtpakk.p
    Purpose     : Import av PRS PriCat Fil

    Syntax      : xPRSPricatUtpakk.p (lFilId).

    Description : Importerer data fra fil som refereres i input parameter. 

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT PARAMETER lFilId AS DEC NO-UNDO.

DEF VAR iTotAntLinjer AS INT NO-UNDO.
DEF VAR iCL           AS INT NO-UNDO.
DEF VAR iClProfilNr     AS INT NO-UNDO.
DEFINE VARIABLE bAutImport AS LOG NO-UNDO.
DEFINE VARIABLE bSendEMail AS LOG NO-UNDO.
DEFINE VARIABLE cMailFil AS CHARACTER NO-UNDO.
DEF VAR h_PrisKo        AS HANDLE NO-UNDO.
DEF VAR rArtBasRecid    AS RECID  NO-UNDO.
DEF VAR cEndelse        AS CHAR   NO-UNDO.
DEF VAR cTekst          AS CHAR   NO-UNDO.
DEF VAR bHk             AS LOG    NO-UNDO.
DEF VAR hInstance AS INT.   
DEFINE VARIABLE iDefLevNr   AS INTEGER NO-UNDO. 
DEFINE VARIABLE cDefRavdNr  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgFilNavn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cErrorFil   AS CHAR NO-UNDO.
DEFINE VARIABLE cLoggKatalog AS CHAR NO-UNDO.
DEFINE VARIABLE cFilPrefix   AS CHARACTER NO-UNDO.
DEFINE VARIABLE bUndertrykkErrLogg      AS LOG NO-UNDO.
DEFINE VARIABLE bNyArt AS LOG NO-UNDO.
DEFINE VARIABLE bOpprettPrisKoVedNyArt AS LOG NO-UNDO.
DEFINE VARIABLE bSjekkLevKodBeskrFargKod AS LOG NO-UNDO.
DEFINE VARIABLE bSlettBehandlede AS LOG NO-UNDO.
DEFINE VARIABLE lMvaKr AS DECIMAL NO-UNDO.
DEFINE VARIABLE lLoggPrisendring AS LOG NO-UNDO.
DEFINE VARIABLE pdDato AS DATE NO-UNDO.
DEFINE VARIABLE iEtikett AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE iInt AS INT NO-UNDO.
DEFINE VARIABLE iX AS INTEGER NO-UNDO.
DEFINE VARIABLE bIkkeOverstyrForOutlet AS LOG NO-UNDO.
DEFINE VARIABLE cOutletLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOppdaterBasicVareinfo AS LOG NO-UNDO.
DEFINE VARIABLE iGantAktiv AS INTEGER NO-UNDO.
DEFINE VARIABLE cHgLst AS CHARACTER NO-UNDO.

DEF VAR dcValPris     AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcInnPris     AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcUtpris      AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcRabatt      AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcFrakt       AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR bAvbrytVPI    AS LOG  NO-UNDO.
DEFINE VARIABLE bPreemArtikkelNr AS LOG NO-UNDO.
DEFINE VARIABLE bGenEAN AS LOG NO-UNDO.
DEFINE VARIABLE bOpprettStr AS LOG NO-UNDO.
DEFINE VARIABLE bOpprettVaremerke AS LOG NO-UNDO.
DEFINE VARIABLE bOpprettProdusent AS LOG NO-UNDO.
DEFINE VARIABLE bOpprettSesong AS LOG NO-UNDO.
DEFINE VARIABLE piAntall AS INTEGER NO-UNDO.
DEFINE VARIABLE bGenSalgsEnhet AS LOG NO-UNDO.
DEFINE VARIABLE iTid     AS INTEGER NO-UNDO.
DEFINE VARIABLE bKonvPantLink AS LOG NO-UNDO.
DEFINE VARIABLE bOpprettNye AS LOG NO-UNDO.
DEFINE VARIABLE iDefaultVg AS INTEGER NO-UNDO.
DEFINE VARIABLE bKjorOppdAvPrisko AS LOG NO-UNDO.
DEFINE VARIABLE bOppdOgsaUtpris   AS LOG NO-UNDO.
DEFINE VARIABLE lAvvik% AS DECIMAL INITIAL 15 NO-UNDO.
DEFINE VARIABLE lDiff%  AS DECIMAL NO-UNDO.
DEFINE VARIABLE iAntall  AS INTEGER NO-UNDO.
DEFINE VARIABLE bFlagg   AS LOG     NO-UNDO.
DEFINE VARIABLE cSperreListeVPILev AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSperreListeVg     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSperreListeHg     AS CHARACTER NO-UNDO.
DEFINE VARIABLE bLeggTilStr AS LOG NO-UNDO.

DEF VAR cEDB-System        LIKE ImpKonv.EDB-System NO-UNDO.
DEF VAR cImpTabell         LIKE ImpKonv.Tabell     NO-UNDO.

DEFINE  VARIABLE ihBuffer    AS HANDLE NO-UNDO.
DEFINE  VARIABLE icSessionId AS CHAR NO-UNDO.
DEFINE  VARIABLE ocReturn    AS CHAR NO-UNDO.
DEFINE  VARIABLE obOK        AS LOG NO-UNDO.
DEFINE  VARIABLE cFieldList  AS CHAR   NO-UNDO.
DEFINE VARIABLE bTest        AS LOG NO-UNDO.
DEFINE VARIABLE bOpprettVg   AS LOG NO-UNDO.
DEFINE VARIABLE lDB%         AS DECIMAL NO-UNDO.

{syspara.i 210 100 8 iGantAktiv INT}
cHgLst = IF iGantAktiv = 1 THEN '90' ELSE ''.

/* TEST - utvidet logging pr. artikkel ved utpakking. */
IF SEARCH('tnc.txt') <> ? THEN 
    ASSIGN bTest = TRUE.
ELSE 
    ASSIGN bTest = FALSE.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr   AS INT
  FIELD Tekst     AS CHAR
  FIELD ErrNr     AS INTEGER
  FIELD ButikkNr  AS INTEGER
  INDEX Feil ErrNr
  INDEX Linje LinjeNr. 
{windows.i}

DEF TEMP-TABLE tmpVPIStrekkode LIKE VPIStrekkode.

DEF VAR lDec          AS DEC NO-UNDO.
DEF VAR cEnhet        AS CHAR NO-UNDO.
DEF VAR piAntFeil     AS INT  NO-UNDO.
DEF VAR plEAN         AS DEC  NO-UNDO.
DEF VAR pcVareNavn    AS CHAR NO-UNDO.
DEF VAR pcBongTekst   AS CHAR NO-UNDO.
DEF VAR pcHg          AS CHAR NO-UNDO.
DEF VAR pcVg          AS CHAR NO-UNDO.
DEF VAR pcUgr         AS CHAR NO-UNDO.
DEF VAR pcMerke       AS CHAR NO-UNDO.
DEF VAR pcBEstKode    AS CHAR NO-UNDO.
DEF VAR pcLevNr       AS CHAR NO-UNDO.
DEF VAR pcFormFarge   AS CHAR NO-UNDO.
DEF VAR pcForm        AS CHAR NO-UNDO.
DEF VAR pcFarge       AS CHAR NO-UNDO.
DEF VAR pcBasPris     AS CHAR NO-UNDO.
DEF VAR pcPris        AS CHAR NO-UNDO.
DEF VAR pcMva%        AS CHAR NO-UNDO.
DEF VAR pcF-Pris      AS CHAR NO-UNDO.
DEF VAR pcS-Pris      AS CHAR NO-UNDO.
DEF VAR pcSkjerm      AS CHAR NO-UNDO.
DEF VAR piTid         AS INT  NO-UNDO.
DEF VAR pcValKod      AS CHAR NO-UNDO.
DEF VAR pcAnbefaltPris AS CHAR NO-UNDO.
DEF VAR pcRab%        AS CHAR NO-UNDO.
DEF VAR pcRab2%       AS CHAR NO-UNDO.
DEF VAR pcKampanje    AS CHAR NO-UNDO.
DEF VAR pcSasong      AS CHAR NO-UNDO.
DEF VAR pcMerknad     AS CHAR NO-UNDO.
DEF VAR pcLevDato     AS CHAR NO-UNDO.

DEF VAR piLevNr       AS INT  NO-UNDO.
DEF VAR pcLevKod      AS CHAR NO-UNDO.
DEF VAR piStrTypeID   AS INT  NO-UNDO.
DEF VAR piSasong      AS INT  NO-UNDO.
DEF VAR piHg          AS INT  NO-UNDO.
DEF VAR piVg          AS INT  NO-UNDO.
DEF VAR piFarg        AS INT  NO-UNDO.
DEF VAR piMomsKod     AS INT  NO-UNDO.
DEF VAR piHovedGr     AS INT  NO-UNDO.
DEF VAR pcVareNr      AS CHAR NO-UNDO.
DEF VAR piTilbud      AS INT  NO-UNDO.
DEF VAR pbStatus      AS LOG  NO-UNDO.
DEF VAR piAntArtikler AS INT  NO-UNDO.
DEF VAR piAntKoblet   AS INT  NO-UNDO.
DEF VAR piAntOppdat   AS INT  NO-UNDO.
DEF VAR piLoop1       AS INT  NO-UNDO.
DEF VAR piVmId        AS INT  NO-UNDO.
DEF VAR plArtikkelNr AS DEC  NO-UNDO.
DEF VAR pcEANnrLst    AS CHAR NO-UNDO.
DEF VAR pcBestNrLst AS CHARACTER NO-UNDO.
DEF VAR pcStrLst      AS CHAR NO-UNDO.
DEF VAR p2lArtikkelNr AS DEC  NO-UNDO.
DEF VAR bBeholdLoalArtInfo AS LOG NO-UNDO.
DEF VAR bSjekkBestillingsnr AS LOG NO-UNDO.
DEF VAR bBrukEanTilArtikkelNr AS LOG NO-UNDO.
DEFINE VARIABLE bBrukBestNrTilArtikkelNr AS LOG NO-UNDO.
DEFINE VARIABLE bLagerstyrt AS LOG NO-UNDO.
DEFINE VARIABLE iOverstyrLevNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iKopierNyeArtikler AS INTEGER NO-UNDO.
DEFINE VARIABLE bSjekkSlask AS LOG NO-UNDO.
DEFINE VARIABLE lArtFra AS DECIMAL NO-UNDO.
DEFINE VARIABLE lArtTil AS DECIMAL NO-UNDO.
DEFINE VARIABLE bSjekkAntIPkn AS LOG NO-UNDO. 
DEFINE VARIABLE bOpprettELogg AS LOG NO-UNDO.

/* eMail oppsett */
DEFINE VARIABLE cMailhub  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDoAUTH   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAuthType AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUser     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPassword AS CHARACTER   NO-UNDO.

{ttpricat.i &NEW=" " &SHARED=" "}

DEFINE BUFFER buf-ttPriKat  FOR ttPriKat.
DEFINE BUFFER bStrKonv      FOR StrKonv.
DEFINE BUFFER bVPIArtBas    FOR VPIArtBas.
DEFINE BUFFER bStrekkode    FOR Strekkode.
DEFINE BUFFER bVPIStrekkode FOR VPIStrekkode.
DEFINE BUFFER bProdusent    FOR Produsent.
DEFINE BUFFER bSasong       FOR Sasong.
DEFINE STREAM Ut.
DEFINE STREAM Mail.

DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rStandardVPIFunksjoner AS CLASS cls.StdFunk.StandardVPIFunksjoner NO-UNDO.
DEFINE VARIABLE rSendEMail AS cls.SendEMail.SendEMail NO-UNDO.
rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).
rStandardVPIFunksjoner = NEW cls.StdFunk.StandardVPIFunksjoner( ).
rSendEMail  = NEW cls.SendEMail.SendEMail( ) NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */
&IF DEFINED(EXCLUDE-getEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEAN Procedure
FUNCTION getEAN RETURNS CHARACTER 
	(  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KalkStreng) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD KalkStreng Procedure 
FUNCTION KalkStreng RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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
         HEIGHT             = 23.19
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
ON CLOSE OF THIS-PROCEDURE 
DO:
  IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_Prisko.
  RUN disable_UI.
END.

ASSIGN 
    cLogg = 'xPRSPricatUtpakk' + REPLACE(STRING(TODAY),'/','')
    .
RUN InitAvParametre.

FOR EACH tt_Error:
  DELETE tt_Error.
END.

/* Start av procedurebibliotek */
IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN prisko.p PERSISTENT SET h_PrisKo.

/* Profilnr for sentrallager. */
FIND Butiker NO-LOCK WHERE
    Butiker.butik = iCl NO-ERROR.
ASSIGN
    iClProfilNr = Butiker.ProfilNr
    .

/* Filhode. */
FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
    RETURN.

/* er det en automatisk importert fil? */
IF VPIFilHode.FilNavn BEGINS 'AE' OR 
  VPIFilHode.FilNavn BEGINS 'SBA' THEN 
  bAutImport = TRUE.

/* Default sasong. */
FIND FIRST Sasong NO-LOCK NO-ERROR.
IF AVAILABLE Sasong THEN
    piSasong = Sasong.Sasong.

/* Henter ID for konvertering */
FIND EkstVPILev NO-LOCK WHERE
    EkstVPILev.EkstVPILevNr = VPIFilHode.EkstVPILevNr NO-ERROR.
IF NOT AVAILABLE EkstVPILev THEN
DO:
    MESSAGE "Ingen ekstern VPI leverandør tilgjengelig." SKIP
            "Id: " + STRING(VPIFilHode.EkstVPILevNr) + "."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* Skal leverandørnr overstyres fra oppsett på VPILev? */
{syspara.i 50 15 20 iOverstyrLevNr INT}
ASSIGN
    cEDB-System = EkstVPILev.EDB-System
    piLevNr     = IF iOverstyrLevNr = 1 THEN EkstVPILev.LevNr ELSE 0.

/* Datasett statuspost. */
FIND VPIDatasett NO-LOCK WHERE
    VPIDatasett.EkstVPILevNr = VPIFilHode.EkstVPILevNr NO-ERROR.
                                                
/* Standard Mva */
FIND FIRST Moms NO-LOCK WHERE 
    Moms.Momsproc > 0 NO-ERROR.
IF AVAILABLE Moms THEN
    piMomsKod = Moms.MomsKod.

/* Standard hovedgruppe */
FIND FIRST HuvGr NO-LOCK WHERE
    HuvGr.Hg > 0 NO-ERROR.
IF AVAILABLE HuvGr THEN
    piHovedGr = HuvGr.Hg.

/* Benytter default alltid laveste størrelsestypen ved importen */
FIND FIRST StrType NO-LOCK WHERE
    StrType.StrTypeId > 1 NO-ERROR.
IF AVAILABLE StrType THEN
    piStrTypeId = StrType.StrTypeId.
ELSE
    piStrTypeId = 2.

/* Oppretter VPIDatasett hvis det ikke finnes. */
IF NOT AVAILABLE VPIDatasett THEN
DO TRANSACTION:
    CREATE VPIDatasett.
    ASSIGN
        VPIDatasett.EkstVPILevNr   = VPIFilHode.EkstVPILevNr
        VPIDatasett.DatasettStatus = 1 /* Opprettet. */
        .
    RELEASE VPIDataSett.
END. /* TRANSACTION */
FIND VPIDatasett NO-LOCK WHERE
    VPIDatasett.EkstVPILevNr = VPIFilHode.EkstVPILevNr NO-ERROR.

/* Setter datasettets status. */
DO TRANSACTION:
  FIND CURRENT VPIDatasett EXCLUSIVE-LOCK.
  ASSIGN
      VPIDatasett.DatasettStatus = 2 /* VPI importeres */
      VPIDatasett.ImportDato     = TODAY
      VPIDatasett.ImportKl       = TIME
      VPIDatasett.FilId          = VPIFilHode.FilId
      .
  RELEASE VPIDAtasett.
END. /* TRANSACTION */
FIND VPIDatasett NO-LOCK WHERE
    VPIDatasett.EkstVPILevNr = VPIFilHode.EkstVPILevNr NO-ERROR.

IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p: Starter. Automodus: ' + string(bAutImport) + 
    ' Filnavn: ' + VPIFilHode.FilNavn + ' EkstVPILev: ' + STRING(VPIFilHode.EkstVPILevNr)).

STATUS DEFAULT "Bygger temp-tabell (ttPriKat)....".
PUBLISH 'visStatusMsg' ("Bygger temp-tabell (ttPriKat)....").    
IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - ByggTmpTabell: Bygger temp-tabell (ttPriKat).... ').
/* Leser opp filen i temp tabell og gjør linjevalidering av alle felt. */
RUN ByggTmpTabell.

IF bBrukEanTilArtikkelNr THEN /* om vi inte hittar streckkod men 'EANartikkel' finns så skapar vi streckkoden på den artikeln */
    RUN FixaStrekkode.

STATUS DEFAULT "Setter artikkelnummer (ttPriKat)....".
PUBLISH 'visStatusMsg' ("Setter artikkelnummer (ttPriKat)....").    
IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - SettArtikkelNr: Setter artikkelnummer (ttPriKat).... ').
/* Setter artikkelnummer på linjene */
RUN SettArtikkelNr.

STATUS DEFAULT "Setter modellfarge (ttPriKat)....".
PUBLISH 'visStatusMsg' ("Setter modellfarge (ttPriKat)....").    
IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - SettModellFarge: Setter modellfarge (ttPriKat).... ').
/* Sett modellNr på linjene. */
RUN SettModellFarge.

/* Renser bort artikler på varegrupper som ikke skal leses inn for VPIleverandør. */
IF (TRIM(cSperreListeVPILev) <> '' AND 
    TRIM(cSperreListeVg) <> '') THEN 
    RUN stoppVaregruppe.

/* Utpakking av VPI til VPI bufferet. */
IF NOT CAN-FIND(FIRST tt_Error) OR bAvbrytVPI = FALSE THEN
DO:
    STATUS DEFAULT "Pakker ut VPI (ttPriKat)....".
    PUBLISH 'visStatusMsg' ("Pakker ut VPI (ttPriKat)....").    
    IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - UtpakkVpi: Utpakking av VPI til VPI bufferet. ').
    RUN UtpakkVpi.
    IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - UtpakkVpi: Ferdig ').
END.
ELSE  DO:
    STATUS DEFAULT "Markerer fil som har feilet...".
    PUBLISH 'visStatusMsg' ("Markerer fil som har feilet...").    
    IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - Feil: Markerer fil som har feilet... ').
    RUN FeilVpi.
END.

/* Legger ut kontrollmeldinger */
IF bTest THEN 
DO:
    FOR EACH ttPriKat WHERE
        ttPriKat.ArtikkelNr > 0 AND 
        ttPriKat.Fargetekst >= '' AND 
        ttPriKat.BehStatus > 1
        BREAK 
        BY ttPriKat.ArtikkelNr
        BY ttPriKat.FargeTekst:
        RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - Har beh.status' + VareIdent() + ' Msg: ' + ttPriKat.BehStatTekst).
    END.    
END.

IF CAN-FIND(FIRST tt_Error) AND bAvbrytVPI = TRUE THEN
DO:
    IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - Avsluttet med feil. Innlesning avbrutt. Ingen poster er innlest. ').
    RUN ErrorLogg.
    RETURN "ERROR".
END.
ELSE IF CAN-FIND(FIRST tt_Error) THEN 
DO:
    IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - Avsluttet med feil. Fil innlest og linjer med feil undertrykket. ').
    RUN ErrorLogg.
    RETURN "ERROR".
END.
ELSE DO:
    IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - Avsluttet OK. Tidsbruk: ' + string(TIME - iTid,'HH:MM:SS')).
    RETURN "OK".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggTmpTabell) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabell Procedure 
PROCEDURE ByggTmpTabell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  

------------------------------------------------------------------------------*/
/* Tømmer feillogg */
FOR EACH tt_Error:
    DELETE tt_error.
END.
/* Tømmer temp-tabell hvis den inneholder noe */
FOR EACH ttPriKat:
    DELETE ttPriKat.
END.

iTotAntLinjer = 0.
FOR EACH VPIFilLinje EXCLUSIVE-LOCK WHERE 
    VPIFilLinje.FilId = VPIFilHode.FilId
    BREAK BY VPIFilLinje.FilId
          BY VPIFilLinje.LinjeNr:
    
    IF FIRST-OF(VPIFilLinje.LinjeNr) OR 
       LAST-OF(VPIFilLinje.LinjeNr) OR
       (VPIFilLinje.LinjeNr MODULO 100 = 0) THEN 
    DO:
        STATUS DEFAULT "Bygger temp-tabell - FilId/FilLinje: " + string(VPIFilLinje.FilId) + "/" + STRING(VPIFilLinje.LinjeNr) + ".".
        PUBLISH 'visStatusMsg' ("Bygger temp-tabell. Leser fillinje: " + STRING(VPIFilLinje.LinjeNr) + ".").    
        
    END.
    /* Renser bort ugyldig verdier i feltene */
    IF ASC(ENTRY( 6,VPIFilLinje.StorTekst,";")) = 160 THEN ENTRY( 6,VPIFilLinje.StorTekst,";") = ''.       
    IF ASC(ENTRY( 7,VPIFilLinje.StorTekst,";")) = 160 THEN ENTRY( 7,VPIFilLinje.StorTekst,";") = ''.       
    IF ASC(ENTRY( 9,VPIFilLinje.StorTekst,";")) = 160 THEN ENTRY( 9,VPIFilLinje.StorTekst,";") = ''.       
    IF    (ENTRY( 9,VPIFilLinje.StorTekst,";")) = '0' THEN ENTRY( 9,VPIFilLinje.StorTekst,";") = ''.       
    IF    (ENTRY( 9,VPIFilLinje.StorTekst,";")) = ' ' THEN ENTRY( 9,VPIFilLinje.StorTekst,";") = ''.       
    IF ASC(ENTRY(19,VPIFilLinje.StorTekst,";")) = 160 THEN ENTRY(19,VPIFilLinje.StorTekst,";") = ''.       
    IF ASC(ENTRY(20,VPIFilLinje.StorTekst,";")) = 160 THEN ENTRY(20,VPIFilLinje.StorTekst,";") = ''.       
    IF ASC(ENTRY(21,VPIFilLinje.StorTekst,";")) = 160 THEN ENTRY(21,VPIFilLinje.StorTekst,";") = ''.       
    IF ASC(ENTRY(22,VPIFilLinje.StorTekst,";")) = 160 THEN ENTRY(22,VPIFilLinje.StorTekst,";") = ''.       

    CREATE ttPriKat.
    ASSIGN
        iTotAntLinjer           = iTotAntLinjer + 1
        ttPriKat.EkstVPILevNr   = VPIFilHode.EkstVPILevNr
        ttPriKat.LinjeNr        = VPIFilLinje.LinjeNr
        ttPriKat.R1             = TRIM(ENTRY( 1,VPIFilLinje.StorTekst,";"),'"')        
        ttPriKat.LevNr          = TRIM(IF piLevNr > 0 THEN STRING(piLevNr) ELSE TRIM(ENTRY( 2,VPIFilLinje.StorTekst,";"),'"'))
        ttPriKat.LevModellNr    = TRIM(TRIM(ENTRY( 3,VPIFilLinje.StorTekst,";"),'"'))
        ttPriKat.LevModellNr    = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(ttPriKat.LevModellNr,";"," "),'"',' '),"'"," "),CHR(13),""),CHR(10),""),',','.')
        ttPriKat.EANnr          = TRIM(TRIM(ENTRY( 4,VPIFilLinje.StorTekst,";"),'"'))
        ttPriKat.VareTekst      = TRIM(SUBSTRING(TRIM(ENTRY( 5,VPIFilLinje.StorTekst,";"),'"'),1,100))
        ttPriKat.VareTekst      = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(ttPriKat.VareTekst,";"," "),'"',' '),"'"," "),CHR(13),""),CHR(10),""),',','.')
        ttPriKat.FargeKode      = TRIM(TRIM(ENTRY( 6,VPIFilLinje.StorTekst,";"),'"'))
        ttPriKat.FargeTekst     = TRIM(SUBSTRING(TRIM(ENTRY( 7,VPIFilLinje.StorTekst,";"),'"'),1,100))
        ttPriKat.Str            = TRIM(TRIM(ENTRY( 8,VPIFilLinje.StorTekst,";"),'"'))
        ttPriKat.Str            = IF ttPriKat.Str = "" THEN "1" ELSE ttPriKat.Str
        ttPriKat.Str            = IF ttPriKat.Str = "OS" THEN "1" ELSE ttPriKat.Str
        ttPriKat.StrTab         = TRIM(ENTRY( 9,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.Varemerke      = TRIM(TRIM(ENTRY(10,VPIFilLinje.StorTekst,";"),'"'))
        ttPriKat.Enh            = TRIM(TRIM(ENTRY(11,VPIFilLinje.StorTekst,";"),'"'))
        ttPriKat.Enh            = (IF ttPriKat.Enh = "" THEN "Stk" ELSE ttPriKat.Enh)
        ttPriKat.Enh            = (IF ttPriKat.Enh = "st" THEN "Stk" ELSE ttPriKat.Enh)
        ttPriKat.AntIEnh        = TRIM(ENTRY(12,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.LevPrisEngros  = REPLACE(TRIM(TRIM(ENTRY(13,VPIFilLinje.StorTekst,";"),'"'),"%"),' ','')
        ttPriKat.ValKod         = TRIM(ENTRY(14,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.forhRab%       = TRIM(TRIM(TRIM(ENTRY(15,VPIFilLinje.StorTekst,";"),'"'),"%"),'')
        ttPriKat.suppRab%       = TRIM(TRIM(TRIM(ENTRY(16,VPIFilLinje.StorTekst,";"),'"'),"%"),'')
        ttPriKat.VeilPris       = TRIM(ENTRY(17,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.VeilPris       = TRIM(REPLACE(ttPriKat.VeilPris,' ',''),"%")
        ttPriKat.PAKstru        = TRIM(ENTRY(18,VPIFilLinje.StorTekst,";"),'"')
        NO-ERROR.
        
     IF ERROR-STATUS:ERROR THEN 
     DO:
       MESSAGE 'Feil i ByggTempTable blokk 1 på linje ' iTotAntLinjer + 1 SKIP 
         VPIFilLinje.StorTekst
       VIEW-AS ALERT-BOX.
     END.
     ASSIGN 
        ttPriKat.LevUke1        = TRIM(ENTRY(19,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.LevUke2        = TRIM(ENTRY(20,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.LevUke3        = TRIM(ENTRY(21,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.LevUke4        = TRIM(ENTRY(22,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.VareGruppe     = TRIM(ENTRY(23,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.LevNavn        = TRIM(ENTRY(24,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.LevKod         = TRIM(TRIM(ENTRY(25,VPIFilLinje.StorTekst,";"),'"')) 
        ttPriKat.LevKod         = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(ttPriKat.LevKod,";"," "),'"',' '),"'"," "),CHR(13),""),CHR(10),""),',','.')
        /*
        ttPriKat.LevKod         = IF ttPriKat.LevKod = ""
                                    THEN ttPriKat.LevModellNr
                                    ELSE ttPriKat.LevKod
        */
        ttPriKat.nettoForh      = REPLACE(TRIM(TRIM(ENTRY(26,VPIFilLinje.StorTekst,";"),'"'),"%"),' ','')
        ttPriKat.kalkForh       = REPLACE(TRIM(TRIM(ENTRY(27,VPIFilLinje.StorTekst,";"),'"'),"%"),' ','')
        ttPriKat.BFforh         = REPLACE(TRIM(TRIM(ENTRY(28,VPIFilLinje.StorTekst,";"),'"'),"%"),' ','')
        ttPriKat.nettoSupp      = REPLACE(TRIM(TRIM(ENTRY(29,VPIFilLinje.StorTekst,";"),'"'),"%"),' ','')
        ttPriKat.kalkSupp       = REPLACE(TRIM(TRIM(ENTRY(30,VPIFilLinje.StorTekst,";"),'"'),"%"),' ','')
        ttPriKat.BFsupp         = REPLACE(TRIM(TRIM(ENTRY(31,VPIFilLinje.StorTekst,";"),'"'),"%"),' ','')
        ttPriKat.MarkedsPris    = REPLACE(TRIM(TRIM(ENTRY(32,VPIFilLinje.StorTekst,";"),'"'),"%"),' ','')
        ttPriKat.Sortiment      = TRIM(TRIM(ENTRY(33,VPIFilLinje.StorTekst,";"),'"'))
        ttPriKat.Sesong         = TRIM(TRIM(ENTRY(34,VPIFilLinje.StorTekst,";"),'"'))
        ttPriKat.VPIBildeKode   = TRIM(TRIM(ENTRY(35,VPIFilLinje.StorTekst,";"),'"'))
        ttPriKat.Merknad        = TRIM(TRIM(ENTRY(36,VPIFilLinje.StorTekst,";"),'"'))
        NO-ERROR.
     IF ERROR-STATUS:ERROR THEN 
     DO:
       MESSAGE 'Feil i ByggTempTable blokk 2 på linje ' iTotAntLinjer + 1 SKIP 
         VPIFilLinje.StorTekst
       VIEW-AS ALERT-BOX.
     END.
        
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 37 THEN ttPriKat.KjedeValutaPris  = TRIM(ENTRY(37,VPIFilLinje.StorTekst,";"),'"').
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 38 THEN ttPriKat.KjedeProdusent   = TRIM(ENTRY(38,VPIFilLinje.StorTekst,";"),'"').
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 39 THEN ttPriKat.ERPNr            = TRIM(ENTRY(39,VPIFilLinje.StorTekst,";"),'"').
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 40 THEN ttPriKat.SalgsEnhetsType  = TRIM(ENTRY(40,VPIFilLinje.StorTekst,";"),'"'). 
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 41 THEN ttPriKat.AktivFraDato     = TRIM(ENTRY(41,VPIFilLinje.StorTekst,";"),'"').    
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 42 THEN ttPriKat.AktivTilDato     = TRIM(ENTRY(42,VPIFilLinje.StorTekst,";"),'"').    
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 43 THEN ttPriKat.Bongtekst        = TRIM(ENTRY(43,VPIFilLinje.StorTekst,";"),'"').       
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 44 THEN ttPriKat.Etikettekst1     = TRIM(ENTRY(44,VPIFilLinje.StorTekst,";"),'"').    
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 45 THEN ttPriKat.Funksjonskode    = TRIM(ENTRY(45,VPIFilLinje.StorTekst,";"),'"').   
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 46 THEN ttPriKat.Mva_Proc         = REPLACE(TRIM(TRIM(ENTRY(46,VPIFilLinje.StorTekst,";"),'"'),"%"),' ',''). 
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 47 THEN ttPriKat.LinkVare         = TRIM(ENTRY(47,VPIFilLinje.StorTekst,";"),'"').         
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 48 THEN ttPriKat.PantBelop        = TRIM(ENTRY(48,VPIFilLinje.StorTekst,";"),'"').       
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 49 THEN ttPriKat.Filial           = TRIM(ENTRY(49,VPIFilLinje.StorTekst,";"),'"').          
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 49 THEN ttPriKat.ButikkNr         = INTEGER(TRIM(ENTRY(49,VPIFilLinje.StorTekst,";"),'"')).          
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 50 THEN ttPriKat.Produsent        = TRIM(ENTRY(50,VPIFilLinje.StorTekst,";"),'"').       
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 51 THEN ttPriKat.Mengde           = TRIM(ENTRY(51,VPIFilLinje.StorTekst,";"),'"').          
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 52 THEN 
      ASSIGN ttPriKat.JamforEnhet = TRIM(ENTRY(52,VPIFilLinje.StorTekst,";"),'"').
    ttPriKat.JamforEnhet = IF ttPriKat.JamforEnhet = '' THEN 'Stk' ELSE ttPriKat.JamforEnhet.     
    /*IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 53 THEN ttPriKat.Kontrolleres     = IF CAN-DO('1,J,ja,Y,YES,TRUE',TRIM(ENTRY(53,VPIFilLinje.StorTekst,";"),'"')) THEN TRUE ELSE FALSE.*/
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 54 THEN ttPriKat.ArtikkelNr       = DECIMAL(TRIM(ENTRY(54,VPIFilLinje.StorTekst,";"),'"')).
    /*IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 55 THEN ttPriKat.OpprettArtikkel  = IF CAN-DO('1,J,ja,Y,YES,TRUE',TRIM(ENTRY(55,VPIFilLinje.StorTekst,";"),'"')) THEN TRUE ELSE FALSE.*/  
    /*IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 56 THEN ttPriKat.PosterPrisending = IF CAN-DO('1,J,ja,Y,YES,TRUE',TRIM(ENTRY(56,VPIFilLinje.StorTekst,";"),'"')) THEN TRUE ELSE FALSE.*/
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 57 THEN ttPriKat.KjedeRab%        = DEC(REPLACE(TRIM(TRIM(ENTRY(57,VPIFilLinje.StorTekst,";"),'"'),"%"),' ','')). 
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 58 THEN ttPriKat.KjedeSupRab%     = DEC(REPLACE(TRIM(TRIM(ENTRY(58,VPIFilLinje.StorTekst,";"),'"'),"%"),' ','')).
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 62 THEN ttPriKat.Karakteristikk   = TRIM(ENTRY(62,VPIFilLinje.StorTekst,";"),'"').       
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 65 THEN ttPriKat.Alder            = TRIM(ENTRY(65,VPIFilLinje.StorTekst,";"),'"').       
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 72 THEN ttPriKat.EkstStrTypeNavn  = TRIM(ENTRY(72,VPIFilLinje.StorTekst,";"),'"').       
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 76 THEN ttPriKat.AlfaKode2        = TRIM(ENTRY(76,VPIFilLinje.StorTekst,";"),'"').       
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 80 THEN ttPriKat.KjedeInnkPris    = DECIMAL(TRIM(ENTRY(80,VPIFilLinje.StorTekst,";"),'"')).
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 82 THEN ttPriKat.KjedeSupInnkPris = DECIMAL(TRIM(ENTRY(82,VPIFilLinje.StorTekst,";"),'"')).        
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 89 THEN ttPriKat.Lager            = IF CAN-DO('1,J,ja,Y,YES,TRUE',TRIM(ENTRY(89,VPIFilLinje.StorTekst,";"),'"')) THEN TRUE ELSE FALSE.
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 90 THEN ttPriKat.Etikett          = INTEGER(TRIM(ENTRY(90,VPIFilLinje.StorTekst,";"),'"')).
    ttPriKat.Etikett = (IF ttPriKat.Etikett = 0 THEN iEtikett ELSE ttPriKat.Etikett).
    ttPriKat.Etikettekst1 = (IF ttPriKat.Etikettekst1 = '' THEN ttPriKat.VareTekst ELSE ttPriKat.Etikettekst1).    
      
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 91 THEN ttPriKat.Sortimentkoder    = TRIM(ENTRY(91,VPIFilLinje.StorTekst,";"),'"').
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 92 THEN ttPriKat.Lagerkoder        = TRIM(ENTRY(92,VPIFilLinje.StorTekst,";"),'"').
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 93 THEN ttPriKat.Gjennomfaktureres = IF TRIM(ENTRY(93,VPIFilLinje.StorTekst,";"),'"') = '' OR 
                                        CAN-DO('1,J,ja,Y,YES,TRUE',TRIM(ENTRY(93,VPIFilLinje.StorTekst,";"),'"')) THEN TRUE ELSE FALSE.
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 94 THEN ttPriKat.KjedeVare         = IF CAN-DO('1,J,ja,Y,YES,TRUE',TRIM(ENTRY(94,VPIFilLinje.StorTekst,";"),'"')) THEN TRUE ELSE FALSE.
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 95 THEN ttPriKat.Kampanjeuker      = TRIM(ENTRY(95,VPIFilLinje.StorTekst,";"),'"').       
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 96 THEN ttPriKat.Kampanjestotte    = TRIM(ENTRY(96,VPIFilLinje.StorTekst,";"),'"').       
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 97 THEN ttPriKat.BehStatus         = INT(TRIM(ENTRY(97,VPIFilLinje.StorTekst,";"),'"')).       
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 98 THEN ttPriKat.Grunnsortiment    = TRIM(ENTRY(98,VPIFilLinje.StorTekst,";"),'"').  
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 99 THEN ttPriKat.Opphav            = TRIM(ENTRY(99,VPIFilLinje.StorTekst,";"),'"').  
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 100 THEN ttPriKat.RAvdNr           = TRIM(ENTRY(100,VPIFilLinje.StorTekst,";"),'"').  
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 101 THEN ttPriKat.OrgFilNavn       = TRIM(ENTRY(101,VPIFilLinje.StorTekst,";"),'"').
    IF cOrgFilNavn = '' THEN cOrgFilNavn = ttPriKat.OrgFilNavn.
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 102 THEN ttPriKat.LoggFilNavn      = TRIM(ENTRY(102,VPIFilLinje.StorTekst,";"),'"').
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 103 THEN ttPriKat.Etikettekst2     = TRIM(ENTRY(103,VPIFilLinje.StorTekst,";"),'"').
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 104 THEN ttPriKat.ArtSlag          = TRIM(ENTRY(104,VPIFilLinje.StorTekst,";"),'"').
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 105 THEN ttPriKat.OPris            = TRIM(ENTRY(105,VPIFilLinje.StorTekst,";"),'"'). 
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 106 THEN ttPriKat.NON_Sale         = TRIM(ENTRY(106,VPIFilLinje.StorTekst,";"),'"'). 
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 107 THEN ttPriKat.NegVare          = TRIM(ENTRY(107,VPIFilLinje.StorTekst,";"),'"'). 
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 107 THEN ttPriKat.Pant             = TRIM(ENTRY(108,VPIFilLinje.StorTekst,";"),'"'). 
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 109 THEN ttPriKat.Telefonkort      = TRIM(ENTRY(109,VPIFilLinje.StorTekst,";"),'"'). 
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 110 THEN ttPriKat.WebButikkArtikkel = TRIM(ENTRY(110,VPIFilLinje.StorTekst,";"),'"'). 
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 111 THEN ttPriKat.PubliserINettbutikk = TRIM(ENTRY(111,VPIFilLinje.StorTekst,";"),'"'). 
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 112 THEN ttPriKat.HoyLav           = TRIM(ENTRY(112,VPIFilLinje.StorTekst,";"),'"'). 
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 113 THEN ttPriKat.WebLeveringstid  = TRIM(ENTRY(113,VPIFilLinje.StorTekst,";"),'"').
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 114 THEN ttPriKat.WebMinLager      = TRIM(ENTRY(114,VPIFilLinje.StorTekst,";"),'"').
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 115 THEN ttPriKat.KampanjeKode     = TRIM(ENTRY(115,VPIFilLinje.StorTekst,";"),'"').
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 116 THEN ttPriKat.LopNr            = TRIM(ENTRY(116,VPIFilLinje.StorTekst,";"),'"').
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 117 THEN ttPriKat.GarantiKl        = TRIM(ENTRY(117,VPIFilLinje.StorTekst,";"),'"').
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 118 THEN ttPriKat.PostBredde       = TRIM(ENTRY(118,VPIFilLinje.StorTekst,";"),'"').
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 119 THEN ttPriKat.PostHoyde        = TRIM(ENTRY(119,VPIFilLinje.StorTekst,";"),'"').
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 120 THEN ttPriKat.PostLengde       = TRIM(ENTRY(120,VPIFilLinje.StorTekst,";"),'"').
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 121 THEN ttPriKat.PostVekt         = TRIM(ENTRY(121,VPIFilLinje.StorTekst,";"),'"').
    IF NUM-ENTRIES(VPIFilLinje.StorTekst,";") >= 122 THEN ttPriKat.AntLinkVare      = TRIM(ENTRY(122,VPIFilLinje.StorTekst,";"),'"').
    
    IF cErrorFil = '' THEN cErrorFil = ttPriKat.LoggFilNavn.    
      
/*
MESSAGE 'xsport1vpiutpakk' SKIP
'ttPriKat.EANnr' ttPriKat.EANnr SKIP        
'ttPriKat.VareTekst' ttPriKat.VareTekst SKIP    
'ttPriKat.LevPrisEngros' ttPriKat.LevPrisEngros SKIP 
'ttPriKat.VeilPris' ttPriKat.VeilPris SKIP   
'ttPriKat.MarkedsPris' ttPriKat.MarkedsPris SKIP  
'ttPriKat.RAvdNr' ttPriKat.RAvdNr SKIP(1)
NUM-ENTRIES(VPIFilLinje.StorTekst,";") SKIP 
VPIFilLinje.StorTekst
VIEW-AS ALERT-BOX.
*/        
    /* Håndterer mapping av pris/vekt i kode og tidskrift EAN. */
    RUN bibl_chkean.p (INPUT-OUTPUT ttPriKat.EANnr).
    IF RETURN-VALUE <> '' THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            piAntFeil          = piAntFeil + 1
            ttPriKat.ErrFlag = TRUE
            tt_Error.LinjeNr = ttPriKat.LinjeNr.
          ASSIGN
            tt_Error.Tekst   = VareIdent() + "* " + RETURN-VALUE.
          ASSIGN
            tt_Error.ErrNr     = 2
            tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
            ttPriKat.behStatus = 20. /* Kontroller */
    END.
    
    /* Genererer EAN hvis det er blankt. */
    IF bGenEAN AND TRIM(ttPriKat.EANNr) = '' THEN
      ttPriKat.EANNr = getEAN().

    /* Artikler uten EAN skal kontrolleres i VPIMottakskontrollen */
    IF TRIM(ttPriKat.EANNr) = "" THEN
      ASSIGN 
      ttPriKat.Kontrolleres = TRUE.

    /* Formaterer størrelsen korrekt */
    RUN FixStorl IN h_dproclib (INPUT-OUTPUT ttPriKat.Str). /* Størrelse i standard SkoTex */
    /* Kobler til størrelseskode. */
    FIND StrKonv NO-LOCK WHERE
        StrKonv.Storl = ttPriKat.Str NO-ERROR.
    IF AVAILABLE StrKonv THEN
          ASSIGN ttPriKat.StrKode = StrKonv.StrKode.
    
    /* Slår av at lokal Artikkelinfo skal beholdes.                                         */
    /* 0-Pricat, 1-Excel GUI import, 2-RIGAL, 3-RIGAL IPS, 4-EDI ARTIKEL, 5-EDI IPS ARTIKEL, 6 XML VPI import */
    IF bBeholdLoalArtInfo AND CAN-DO('1',ttPriKat.Opphav) THEN 
      bBeholdLoalArtInfo = FALSE.            
          
    /* Bildereferanse skal alltid ha jpg som ekstent. */
    IF ttPrikat.VPIBildeKode <> "" THEN
        ASSIGN
        ttPrikat.VPIBildeKode = RIGHT-TRIM(TRIM(ttPrikat.VPIBildeKode,'-'),'_')
        ttPrikat.VPIBildeKode = REPLACE(ttPrikat.VPIBildeKode,'.jpg','') + ".jpg".
       
    /* Setter på profilnr og flagger automatisk opprettelse av nye artikler */
    /* hvis det ikke er sentrallageret.                                     */
    IF ttPriKat.ButikkNr > 0 THEN 
      DO:
        IF ttPriKat.ButikkNr <> iCL THEN 
          ASSIGN ttPriKat.OpprettArtikkel = TRUE.
          
        FIND Butiker NO-LOCK WHERE
          Butiker.Butik = ttPriKat.ButikkNr NO-ERROR.
        IF AVAILABLE Butiker THEN
          ttPriKat.ProfilNr = Butiker.ProfilNr.
      END.
    ELSE ASSIGN ttPriKat.OpprettArtikkel = FALSE.
    
    /* Er ikke butikknr angitt, benyttes sentrallagerets profil. */
    IF ttPriKat.ProfilNr = 0 THEN 
      ttPriKat.ProfilNr = iClProfilNr.
    
    /* Er det angitt pant, skal den kontrolleres i mottakskontrollen hvis ikke konvertering går bra. */
    /* Pant sjekkes i konverteringsrutinen og i linjevalideringen.                                   */
    IF ttPriKat.LinkVare <> '' THEN ttPriKat.Kontrolleres = TRUE.
      
    /* Konverterer informasjonen ut fra mapping i ImpKonv. */
    IF cEDB-System <> '' THEN 
      RUN Konverter.

    /* Validerer informasjonen mot underliggende registre */
    RUN LinjeValidering.

    /* EAN skal alltid ha 13 siffer hvis det ikke er plu*/
    IF LENGTH(ttPriKat.EANnr) > 6 THEN
    DO: 
      ttPriKat.EANnr = IF ttPriKat.EANnr <> "" 
                       THEN FILL("0",13 - LENGTH(ttPriKat.EANnr))+ ttPriKat.EANnr
                       ELSE ttPriKat.EANnr.
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
  DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcErrFil AS CHARACTER NO-UNDO.
  
  IF CAN-FIND(FIRST tt_error) AND bUndertrykkErrLogg = FALSE THEN  
  UTSKRIFT_AV_ERR_Logg:
  DO:
    /* Logger nye artikler i egen logg i tillegg.*/
    FOR EACH tt_Error WHERE 
      tt_Error.ErrNr = 99 
      BREAK BY ErrNr:
      RUN bibl_loggDbFri.p (RIGHT-TRIM(cFilPrefix,'_') + 'NYE', 'xPRSPricatUtpakk.p - ' + tt_Error.Tekst).
    END.

    /* Logger prisendringer i egen logg i tillegg.*/
    FOR EACH tt_Error WHERE 
      tt_Error.ErrNr = 98 
      BREAK BY ErrNr:
      RUN bibl_loggDbFri.p (RIGHT-TRIM(cFilPrefix,'_') + 'Prisendring', 'xPRSPricatUtpakk.p - ' + tt_Error.Tekst).
    END.

    RUN bibl_loggDbFri.p (RIGHT-TRIM(cFilPrefix,'_'), 'xPRSPricatUtpakk.p - ' + "Innlesning av fil ").
    IF AVAILABLE VPIFilHode 
      THEN pcTekst = "  Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn. 
      ELSE pcTekst = "  Ukjent/slettet VPI fil (xsport1vpiutpakk.p).".    
    RUN bibl_loggDbFri.p (RIGHT-TRIM(cFilPrefix,'_'), 'xPRSPricatUtpakk.p - ' + pcTekst).
    IF cOrgFilNavn <> '' THEN
        RUN bibl_loggDbFri.p (RIGHT-TRIM(cFilPrefix,'_'), 'xPRSPricatUtpakk.p - ' + 'Orginalfil: ' + cOrgFilNavn).
    
    FOR EACH tt_Error 
      BREAK BY ErrNr:
      RUN bibl_loggDbFri.p (RIGHT-TRIM(cFilPrefix,'_'), 'xPRSPricatUtpakk.p - ' + tt_Error.Tekst).
    END.
    
    pcErrFil = "log\Error" + REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + ".Txt".
    OUTPUT TO VALUE(pcErrFil).
      PUT UNFORMATTED
        "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
        "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn SKIP
        .
      FOR EACH tt_Error:
        PUT UNFORMATTED tt_Error.Tekst SKIP.
      END.
      EMPTY TEMP-TABLE tt_Error.
    OUTPUT CLOSE.
    IF SEARCH(pcErrFil) <> ? THEN
    DO:
      DEF VAR hInstance AS INT.
    
      RUN ShellExecute{&A} IN hpApi(0,
                                    "open",
                                    "notepad.exe",
                                    SEARCH(pcErrFil),
                                    "",
                                    1,
                                    OUTPUT hInstance).
  END.
    
    
  END. /* UTSKRIFT_AV_ERR_Logg */
  
  
  /* Sender eMail hvis det skapes error fil ved import. */
  /*
  IF bSendEMail AND SEARCH(cErrorFil) <> ? THEN
    RUN sendEMail(SEARCH(cErrorFil)).
  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FeilVpi) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FeilVpi Procedure 
PROCEDURE FeilVpi :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO TRANSACTION:
  FIND CURRENT VPIDatasett EXCLUSIVE-LOCK.
  ASSIGN
      VPIDatasett.DatasettStatus = 9 /* Feil */
      .
  FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
  ASSIGN
      VPIFilHode.VPIFilStatus = 9
      .

  FIND CURRENT VPIDatasett NO-LOCK.
  FIND CURRENT VPIFilHode  NO-LOCK.
END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixaStrekkode) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixaStrekkode Procedure
PROCEDURE FixaStrekkode:
	/*------------------------------------------------------------------------------
			Purpose: Om vi inte hittar streckkod men 'EANartikkel' = ttPriKat.EANNr
                     så skapar vi streckkod.
			Notes:  																	  
	------------------------------------------------------------------------------*/
    DEFINE VARIABLE cKode AS CHARACTER   NO-UNDO.
    FOR EACH ttPrikat:
        IF ttPriKat.ErrFlag THEN
            NEXT.
        cKode = ttPriKat.EANNr.
        IF LENGTH(cKode) < 13 THEN DO:
            cKode = FILL("0",13 - LENGTH(cKode)) + cKode.
        END.
        IF NOT CAN-FIND(strekkode WHERE strekkode.kode = cKode) THEN DO:
            FIND artbas WHERE artbas.artikkelnr = DECI(cKode) NO-LOCK NO-ERROR.
            IF AVAIL artbas THEN DO TRANSACTION:
                  CREATE StrekKode.
                  ASSIGN StrekKode.ArtikkelNr = ArtBas.ArtikkelNr
                         StrekKode.Kode       = cKode
                         StrekKode.KodeType   = 1 /* använd inte iKodeType, vi kan ha 0 */
                         StrekKode.StrKode    = 1 NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN
                  DO:
                      IF AVAILABLE StrekKode THEN
                          DELETE StrekKode.
                  END.
            END.
        END.
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

PROCEDURE InitAvParametre:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
ASSIGN
  iTid     = TIME /* Tidsteller :) */
  .

{syspara.i 22 5 2 cOutletLst}

{syspara.i 102 1 1 cFilPrefix}
IF cFilPrefix = '' THEN cFilPrefix = "VPILog_".
cFilPrefix = cFilPrefix + REPLACE(STRING(TODAY,"99/99/99"),'/','') + '_'.
 
{syspara.i 1 1 59 cLoggKatalog}
IF cLoggKatalog <> '' THEN DO:
    /* Sikrer at katalog finnes. */
    OS-CREATE-DIR VALUE(RIGHT-TRIM(cLoggKatalog,'\')).    
    cLoggKatalog = RIGHT-TRIM(cLoggKatalog,'\') + '\'.
END.

/* Sender eMail */
{syspara.i 50 15 31 cTekst}
IF CAN-DO('Ja,Yes,True,1,J,Y',cTekst) THEN 
  ASSIGN bSendEMail = TRUE.
ELSE
  ASSIGN bSendEMail = FALSE.

/* Centrallager */
{syspara.i 5 1 1 iCl INT}

/* Avbryt innlesning ved feil */
{syspara.i 50 15 2 cTekst}             
IF CAN-DO("1,yes,true,Ja",cTekst) OR cTekst = '' THEN 
    bAvbrytVPI = TRUE.                 
ELSE                                   
    bAvbrytVPI = FALSE.                

/* Overstyring av artikkelnr (Preem). */
{syspara.i 50 15 22 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bPreemArtikkelNr = TRUE.
ELSE
  bPreemArtikkelNr = FALSE.

/* Styrer om størrelser skal legges til i størrelsestypen. */
{syspara.i 50 15 45 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bLeggTilStr = TRUE.
ELSE
  bLeggTilStr = FALSE.

/* Oppdaterte linjer skal slettes */
{syspara.i 50 15 47 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bSlettBehandlede = TRUE.
ELSE
  bSlettBehandlede = FALSE.
  
/* Aktiver logging av prisendringer */
{syspara.i 50 15 48 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  lLoggPrisendring = TRUE.
ELSE
  lLoggPrisendring = FALSE.

/* Etikett */
{syspara.i 50 15 49 iEtikett INT}
  
/* Sjekk om artikkel finnes med Levkod, Beskr og LevFargKod. */
{syspara.i 50 15 43 cTekst}.
IF cTekst = '' THEN 
  bSjekkLevKodBeskrFargKod = TRUE.
ELSE IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bSjekkLevKodBeskrFargKod = TRUE.
ELSE
  bSjekkLevKodBeskrFargKod = FALSE.

/* Oppretter ELogg poster for IPS filer. */
{syspara.i 50 15 34 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bOpprettELogg = TRUE.
ELSE
  bOpprettELogg = FALSE.
  
/* Lag priskø post bli liggende ubehandlet for nye artikler i IPS filen. */
/* Slik at den synes i prisregisteret.                                   */
{syspara.i 50 15 42 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bOpprettPrisKoVedNyArt = TRUE.
ELSE
  bOpprettPrisKoVedNyArt = FALSE.

/* Kopierer nye artikler til HK VPIlomme m/status kontroller.                   */
/* Kopiering skjer bare når VPI leses inn til en butikk, ikke til hk VPI lomme. */
{syspara.i 50 15 24 iKopierNyeArtikler INT}.

/* Nye artikler som leses inn som har ukjent varegruppe, tildeles default varegruppe. */
{syspara.i 50 15 25 iDefaultVg INT}.

/* Nye artikler som leses inn som har ukjent leverandør, tildeles default leverandør. */
{syspara.i 50 15 32 iDefLevNr INT}.

/* Oppdaterer basic vareinfor direkte i artikkelregister. */
{syspara.i 50 15 54 cTekst}
IF CAN-DO('1,J,Ja,Yes,TRUE',cTekst) THEN 
  bOppdaterBasicVareinfo = TRUE.
  
/* Størrelser skal opprettes hvis de ikke finnes. */
{syspara.i 50 15 10 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bOpprettStr = TRUE.
ELSE
  bOpprettStr = FALSE.

/* Opprett sesonger. */
{syspara.i 50 15 41 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bOpprettSesong = TRUE.
ELSE
  bOpprettSesong = FALSE.

/* Oppretter varemerker hvis de ikke finnes. */
{syspara.i 50 15 27 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bOpprettVaremerke = TRUE.
ELSE
  bOpprettVaremerke = FALSE.

/* Oppretter produsenter hvis de ikke finnes. */
{syspara.i 50 15 28 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bOpprettProdusent = TRUE.
ELSE
  bOpprettProdusent = FALSE.
    
/* Kjør oppdatering av priskø automatisk for filimport. */
{syspara.i 50 15 29 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bKjorOppdAvPrisko = TRUE.
ELSE
  bKjorOppdAvPrisko = FALSE.
    
/* Skal også utpris settes i priskø?. */
{syspara.i 50 15 30 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bOppdOgsaUtpris = TRUE.
ELSE
  bOppdOgsaUtpris = FALSE.
    
/* Nye artikler skal ha EAN koden som artikkelNr. */
{syspara.i 50 15 15 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bBrukEanTilArtikkelNr = TRUE.
ELSE
  bBrukEanTilArtikkelNr = FALSE.

/* EAN koder som ligger på artikler med en annen leverandørs forpakningsenhet, frigjøres */
/* og legges opp på ny artikkel.                                                         */
{syspara.i 50 15 33 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bSjekkAntIPkn = TRUE.
ELSE
  bSjekkAntIPkn = FALSE.
  
/* Importert link til Pant er EAN kode. Den skal konverteres til artikkelNr.                                              */
/* Brukes hvis mapping mapper til EAN kode eller at mapping ikke benyttes og det kommer EAN koder i importfilen i feltet. */
{syspara.i 50 15 21 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bKonvPantLink = TRUE.
ELSE
  bKonvPantLink = FALSE.  

/* Opprette varegruppe hvis den ikke finnes. */
{syspara.i 50 15 37 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bOpprettVg = TRUE.
ELSE
  bOpprettVg = FALSE.  
 
/* Lokal artikkelinformasjon skal beholdes. */
{syspara.i 50 15 11 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bBeholdLoalArtInfo = TRUE.
ELSE
  bBeholdLoalArtInfo = FALSE.
  
/* Ikke overstyr med Outlett vareinfo. */
{syspar2.i 50 15 11 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bIkkeOverstyrForOutlet = TRUE.
ELSE
  bIkkeOverstyrForOutlet = FALSE.

/* Opprette nye artikler som importeres automatisk i artikkelregisteret. */
{syspara.i 50 15 23 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) OR TRIM(cTekst) = '' THEN 
  bOpprettNye = TRUE.
ELSE
  bOpprettNye = FALSE.

/* Sjekk bestillingsnr før strekkode. */
{syspara.i 50 15 12 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bSjekkBestillingsnr = TRUE.
ELSE
  bSjekkBestillingsnr = FALSE.

/* Alltid opprette artikkelene som lagerstyrt. */
{syspara.i 50 15 17 cTekst}.
IF CAN-DO("1,yes,y,true,Ja,j",cTekst) THEN 
  bLagerstyrt = TRUE.
ELSE
  bLagerstyrt = FALSE.

/* Generer EAN hvis det er blank EAN kode. */
{syspara.i 50 15 4 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bGenEAN = TRUE.

/* Opprette salgsenhet. */
{syspara.i 50 15 7 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bGenSalgsenhet = TRUE.

/* Tillatt avvik. */
{syspara.i 50 15 35 cTekst}
ASSIGN lAvvik% = DEC(cTekst) NO-ERROR.
IF lAvvik% = 0 THEN lAvvik% = 15.

/* Sperreliste VPILeverandør/Hovedgruppe. */
{syspara.i 50 15 36 cSperreListeVPILev}
{syspar2.i 50 15 36 cSperreListeHg}
IF cSperreListeHg <> '' THEN 
DO: 
  FOR EACH VarGr NO-LOCK WHERE
    CAN-DO(cSperreListeHg,STRING(VarGr.Hg)):
    cSperreListeVg = cSperreListeVg + 
                     (IF cSperreListeVg <> '' THEN ',' ELSE '') + 
                     STRING(VarGr.Vg).  
  END.
END.

/* Default vareområde for nye artikler. */
{syspara.i 50 15 38 cDefRavdNr}

{syspara.i 50 15 39 cTekst}
IF cTekst = '' OR CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bUndertrykkErrLogg = TRUE.
ELSE
  bUndertrykkErrLogg = FALSE.

/* Hvis slaskartikler er i bruk skal EAN koden flyttes fra disse når VPI importeres. */
{syspara.i 50 15 26 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN
DO: 
  ASSIGN bSjekkSlask = TRUE.
  {syspar2.i 50 15 26 cTekst}
  IF NUM-ENTRIES(cTekst,',') = 2 THEN 
  ASSIGN lArtFra = DECIMAL(ENTRY(1,cTekst))
         lArtTil = DECIMAL(ENTRY(2,cTekst)).  
END.

END PROCEDURE.

&IF DEFINED(EXCLUDE-Kalkulasjon) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kalkulasjon Procedure 
PROCEDURE Kalkulasjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT        PARAMETER pcFraFelt  AS CHAR NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER pcSkjerm   AS CHAR NO-UNDO.
  
  DEF VAR pcFeltListe AS CHAR NO-UNDO.
  DEF VAR piFeltNr    AS INT  NO-UNDO.
  DEF VAR lTilbud     AS LOG  NO-UNDO.
  DEF VAR lDirekte    AS LOG  NO-UNDO.

  DEF BUFFER bArtBas FOR VPIArtBas.
  DEF BUFFER bVarGr  FOR VarGr.
  DEF BUFFER bMoms   FOR Moms.
  DEF BUFFER bValuta FOR Valuta.

  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN prisko.p PERSISTENT SET h_PrisKo.

  ASSIGN
    lTilbud     = FALSE
    lDirekte    = FALSE
    pcFeltListe = "ValPris,InnPris,Rab1,Rab1%,Rab2,Rab2%,Frakt,Frakt%," + 
                  "DivKost,DivKost%,Rab3,Rab3%,VareKost,DB,DB%," +
                  "FI-Mva,FI-Mva%,Pris,EU-Pris".
                 
  /* Finner i hvilket felt markøren sto når prosedyren ble kalt. */
  ASSIGN
    /*pcFraFelt = substring(pcFraFelt,4)*/
    piFeltNr  = LOOKUP(pcFraFelt,pcFeltListe)
    .

  /* Ukjent felt. */  
  IF piFeltNr = 0 THEN
    DO:
      MESSAGE "Ukjent felt!" VIEW-AS ALERT-BOX TITLE "Kalkylefeil".
      RETURN NO-APPLY.  
    END.

  /* Henter nødvendige buffere */
  FIND bArtBas NO-LOCK WHERE
      RECID(bArtBas) = rArtBasRecid NO-ERROR.
  IF NOT AVAILABLE bArtBas THEN
    RETURN "AVBRYT".
  FIND bVarGr OF bArtBas NO-ERROR.
  IF NOT AVAILABLE bVarGr THEN
    RETURN "AVBRYT".
  FIND bMoms  OF bVarGr  NO-ERROR.
  IF NOT AVAILABLE bMoms THEN
    RETURN "AVBRYT".
  IF DECIMAL(ttPriKat.Mva_Proc) = 0 THEN ttPriKat.Mva_Proc = STRING(bMoms.MomsProc).
  
  FIND bValuta OF bArtBas NO-ERROR.
  IF NOT AVAILABLE bValuta THEN
    RETURN "AVBRYT".

  /* Starter omkalkulering.                         */
  RUN Omregning IN h_PrisKo
       (INPUT rArtBasRecid, 
        INPUT iClProfilNr,
        INPUT-OUTPUT pcSkjerm,
        INPUT ttPriKat.Mva_Proc,
        INPUT bValuta.ValKurs, 
        INPUT piFeltNr,
        INPUT lTilbud).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Konverter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Konverter Procedure 
PROCEDURE Konverter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:    Sjekk av konvertering gjøres bare hvis det finnes opprettet
            konverteringstabell for aktuell tabell.       
------------------------------------------------------------------------------*/
    DEF VAR pbFlagg AS LOG NO-UNDO.
    DEF VAR piInt   AS INT NO-UNDO.

    /* Konverteringstabell - PLU */
    IF (LEFT-TRIM(ttPriKat.EANNr,'0') <> '' AND 
        LENGTH(LEFT-TRIM(ttPriKat.EANNr,'0')) <= 6 AND 
        CAN-FIND(FIRST ImpKonv NO-LOCK WHERE 
          ImpKonv.EDB-System = cEDB-System AND 
          ImpKonv.Tabell     = 'PLU')) THEN 
    DO:
      FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = 'PLU' AND 
        ImpKonv.EksterntId = LEFT-TRIM(ttPriKat.EANNr,'0') NO-ERROR.
      IF AVAILABLE ImpKonv THEN ASSIGN ttPriKat.EANNr = ImpKonv.InterntId.
      ELSE DO:
        CREATE tt_Error.
        ASSIGN
          ttPriKat.ErrFlag   = TRUE 
          tt_Error.LinjeNr   = ttPriKat.LinjeNr.
          
        ASSIGN  
          tt_Error.Tekst     = VareIdent() + "* PLU (" + ttPriKat.EANNr + ") uten mapping i konverteringstabell linje: " + STRING(ttPriKat.LinjeNr) + ": " + VPIFilLinje.StorTekst + ".".
        
        ASSIGN
          tt_Error.ErrNr = IF bAvbrytVPI THEN 2 ELSE 52
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
          ttPriKat.behStatus = 20. /* Kontroller */
        ASSIGN
          ttPriKat.EANNr        = '' /* Er mapping aktivert og det ikke finnes mapping, skal PLUNr blankes. */
          ttPriKat.Kontrolleres = TRUE.
      END.
    END. 

    /* Konverteringstabell - Pant (Link til pant) */
    IF (ttPriKat.LinkVare <> '' AND 
        CAN-FIND(FIRST ImpKonv NO-LOCK WHERE 
          ImpKonv.EDB-System = cEDB-System AND 
          ImpKonv.Tabell     = 'Pant')) THEN 
    DO:
      FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = 'Pant' AND 
        ImpKonv.EksterntId = ttPriKat.LinkVare NO-ERROR.
      IF AVAILABLE ImpKonv THEN ASSIGN ttPriKat.LinkVare = ImpKonv.InterntId.
      ELSE DO:
        CREATE tt_Error.
        ASSIGN
          ttPriKat.ErrFlag   = TRUE 
          tt_Error.LinjeNr   = ttPriKat.LinjeNr.
        ASSIGN
          tt_Error.Tekst     = VareIdent() + "* Mapping: Link til pant (" + ttPriKat.LinkVare + ") uten mapping i konverteringstabell.".
        ASSIGN
          tt_Error.ErrNr     = 1
            ttPriKat.BehStatTekst = tt_Error.Tekst
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
          ttPriKat.behStatus = 20. /* Kontroller */
      END.
    END. 

    /* Konverteringstabell - Varegruppe */
    IF TRIM(ttPriKat.Varegruppe) <> '' AND 
       CAN-FIND(FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = 'VarGr') THEN 
    DO:
      FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = 'VarGr' AND 
        ImpKonv.EksterntId = ttPriKat.Varegruppe NO-ERROR.
      IF AVAILABLE ImpKonv THEN ASSIGN ttPriKat.Varegruppe = ImpKonv.InterntId.
      ELSE IF iDefaultVg = 0 THEN DO:
        CREATE tt_Error.
        ASSIGN
          ttPriKat.ErrFlag   = TRUE 
          tt_Error.LinjeNr   = ttPriKat.LinjeNr.
        ASSIGN
          tt_Error.Tekst     = VareIdent() + "* Mapping: Varegruppe (" + ttPriKat.Varegruppe + ") uten mapping i konverteringstabell." .
        ASSIGN
          tt_Error.ErrNr     = 1
            ttPriKat.BehStatTekst = tt_Error.Tekst
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
          ttPriKat.behStatus = 20. /* Kontroller */
      END.
      /* Default varegruppe benyttes. */
      ELSE DO:
        CREATE tt_Error.
        ASSIGN
          /*ttPriKat.ErrFlag   = TRUE*/ 
          tt_Error.LinjeNr   = ttPriKat.LinjeNr.
        ASSIGN
          tt_Error.Tekst     = VareIdent() + "* Mapping: Ukjent Varegruppe (" + ttPriKat.Varegruppe + ") Default varegruppe (" + STRING(iDefaultVg) + ") benyttes.".
        ASSIGN
          tt_Error.ErrNr     = 1
            ttPriKat.BehStatTekst = tt_Error.Tekst
          tt_Error.ButikkNr  = ttPriKat.ButikkNr.
          /*ttPriKat.behStatus = 20.*/ /* Kontroller */
        
        /* Oppretter mapping for FEDAS varegrupper mot default varegruppe iDefaultVg. */
        IF /*LENGTH(ttPriKat.VareGruppe) = 6 AND */ iDefaultVg > 0 THEN
        FEDAS: 
        DO:
            IF AVAILABLE ArtBas THEN RELEASE ArtBas.
            FIND StrekKode NO-LOCK WHERE 
                StrekKode.Kode = ttPriKat.EANNr NO-ERROR.
            IF AVAILABLE StrekKode THEN 
                FIND ArtBas OF StrekKode NO-LOCK NO-ERROR.
            FIND FedasProduct_Group NO-LOCK WHERE 
                FedasProduct_Group.ProdGroupId = INTEGER(ttPriKat.VareGruppe) NO-ERROR. 
            CREATE ImpKonv.
            ASSIGN 
                ImpKonv.EDB-System = cEDB-System
                ImpKonv.Tabell     = 'VarGr'
                ImpKonv.InterntID  = (IF AVAILABLE ArtBas THEN STRING(ArtBas.Vg) ELSE STRING(iDefaultVg))
                ImpKonv.EksterntId = ttPriKat.VareGruppe
                ImpKonv.Merknad    = (IF AVAILABLE FedasProduct_Group 
                                        THEN 
                                          'Lev: ' + STRING(ttPriKat.LevNr) + ' ' + FedasProduct_Group.ProdGroupDescription 
                                        ELSE 
                                          'Lev: ' + STRING(ttPriKat.LevNr) + ' ' + 'Automatisk opprettet ' + REPLACE(STRING(TODAY),'/','')).
        END. /* FEDAS */
        ASSIGN 
            ttPriKat.VareGruppe = STRING(iDefaultVg).
             
      END.
    END. 

    /* Setting av leverandørnr. hvis ikke dette er overstyrt.                        */
    /* Er piLevNr satt, er dette tilordnet ttPrikat.LevNr ved bygging av temptabell. */
    IF piLevNr = 0 AND 
      CAN-FIND(FIRST ImpKonv NO-LOCK WHERE
          ImpKonv.EDB-System = cEDB-System AND
          ImpKonv.Tabell     = 'LevBas') THEN 
    SETT_LEVNR:
    DO:
      /* Henter mapping på leverandør.                                     */
      /* Er det satt opp konvertering og mapping ikke finnes, er det feil. */
      FIND ImpKonv NO-LOCK WHERE
        ImpKonv.EDB-System = cEDB-System AND
        ImpKonv.Tabell     = 'LevBas' AND
        ImpKonv.EksterntId = STRING(ttPriKat.LevNr) NO-ERROR. 
      IF AVAILABLE ImpKonv THEN ttPriKat.LevNr = ImpKonv.InterntId.
      ELSE IF iDefLevNr = 0 THEN  DO:
        CREATE tt_Error.
        ASSIGN
          ttPriKat.ErrFlag   = TRUE 
          tt_Error.LinjeNr   = ttPriKat.LinjeNr.
        ASSIGN
          tt_Error.Tekst     = VareIdent() + "* Mapping: Leverandørnr. (" + ttPriKat.LevNr + ") uten mapping i konverteringstabell.".
        ASSIGN
          tt_Error.ErrNr     = 1
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
          ttPriKat.behStatus = 20. /* Kontroller */.
      END.
      ELSE DO:
        CREATE tt_Error.
        ASSIGN
          ttPriKat.ErrFlag   = TRUE 
          tt_Error.LinjeNr   = ttPriKat.LinjeNr.
        ASSIGN
          tt_Error.Tekst     = VareIdent() + "* Mapping: Ukjent leverandør (" + ttPriKat.LevNr + ") Default leverandør benyttes(" + STRING(iDefLevNr) + ").".
        ASSIGN
          tt_Error.ErrNr     = 1
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
          ttPriKat.behStatus = 20. /* Kontroller */.
        ttPriKat.LevNr = STRING(iDefLevNr).
      END.
    END. /* SETT_LEVNR */
    
    /* Konverteringstabell - Produsent */
    IF CAN-FIND(FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = 'Produsent') THEN 
    DO:
        FIND FIRST ImpKonv NO-LOCK WHERE 
            ImpKonv.EDB-System = cEDB-System AND 
            ImpKonv.Tabell     = 'Produsent' AND 
            ImpKonv.EksterntId = ttPriKat.Produsent NO-ERROR.
        IF AVAILABLE ImpKonv THEN ASSIGN ttPriKat.Produsent = ImpKonv.InterntId.
        ELSE DO:
            CREATE tt_Error.
            ASSIGN
              ttPriKat.ErrFlag   = TRUE 
              tt_Error.LinjeNr   = ttPriKat.LinjeNr.
            ASSIGN
              tt_Error.Tekst     = VareIdent() + "* Mapping: Produsentnr (" + ttPriKat.Produsent + ") uten mapping i konverteringstabell.".
            ASSIGN
              tt_Error.ErrNr     = 1
              tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
              ttPriKat.behStatus = 20. /* Kontroller */
        END.
    END.

    /* Konverteringstabell - Farg */
    IF CAN-FIND(FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = 'Farg') THEN 
    DO:
        FIND FIRST ImpKonv NO-LOCK WHERE 
            ImpKonv.EDB-System = cEDB-System AND 
            ImpKonv.Tabell     = 'Farg' AND 
            ImpKonv.EksterntId = ttPriKat.FargeKode NO-ERROR.
        IF AVAILABLE ImpKonv THEN ttPriKat.FargeKode = ImpKonv.InterntId.
        ELSE DO:
            CREATE tt_Error.
            ASSIGN
              ttPriKat.ErrFlag   = TRUE 
              tt_Error.LinjeNr   = ttPriKat.LinjeNr.
            ASSIGN
              tt_Error.Tekst     = VareIdent() + "* Mapping: Fargekode (" + ttPriKat.FargeKode + ") uten mapping i konverteringstabell.".
            ASSIGN
              tt_Error.ErrNr     = 1
              tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
              ttPriKat.behStatus = 20. /* Kontroller */
        END.
    END.
     
    /* Konverteringstabell - Varemerke */
    IF CAN-FIND(FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = 'Varemerke') THEN 
    DO:
        FIND FIRST ImpKonv NO-LOCK WHERE 
            ImpKonv.EDB-System = cEDB-System AND 
            ImpKonv.Tabell     = 'Varemerke' AND 
            ImpKonv.EksterntId = ttPriKat.Varemerke NO-ERROR.
        IF AVAILABLE ImpKonv THEN ttPriKat.Varemerke = ImpKonv.InterntId.
        ELSE DO:
            CREATE tt_Error.
            ASSIGN
              ttPriKat.ErrFlag   = TRUE 
              tt_Error.LinjeNr   = ttPriKat.LinjeNr.
            ASSIGN
              tt_Error.Tekst     = VareIdent() + "* Mapping: Varemerke (" + ttPriKat.Varemerke + ") uten mapping i konverteringstabell.".
            ASSIGN
              tt_Error.ErrNr     = 1
              tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
              ttPriKat.behStatus = 20. /* Kontroller */
        END.    
    END.
    /* Konverteringstabell - Jamforenhet */
    IF CAN-FIND(FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = 'Jamforenhet') THEN 
    DO:
        FIND FIRST ImpKonv NO-LOCK WHERE 
            ImpKonv.EDB-System = cEDB-System AND 
            ImpKonv.Tabell     = 'Jamforenhet' AND 
            ImpKonv.EksterntId = ttPriKat.Jamforenhet NO-ERROR.
        IF AVAILABLE ImpKonv THEN ASSIGN ttPriKat.Jamforenhet = ImpKonv.InterntId.
        ELSE DO:
            CREATE tt_Error.
            ASSIGN
              ttPriKat.ErrFlag   = TRUE 
              tt_Error.LinjeNr   = ttPriKat.LinjeNr.
            ASSIGN
              tt_Error.Tekst     = VareIdent() + "* Mapping: Jamforenhet (" + ttPriKat.Jamforenhet + ") uten mapping i konverteringstabell.".
            ASSIGN
              tt_Error.ErrNr     = 1
              tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
              ttPriKat.behStatus = 20. /* Kontroller */
        END.
    END.

    /* Konverteringstabell - Salgsenhet */
    IF CAN-FIND(FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = 'Salgsenhet') THEN 
    DO:
        FIND FIRST ImpKonv NO-LOCK WHERE 
            ImpKonv.EDB-System = cEDB-System AND 
            ImpKonv.Tabell     = 'Salgsenhet' AND 
            ImpKonv.EksterntId = ttPriKat.Enh NO-ERROR.
        IF AVAILABLE ImpKonv THEN ASSIGN ttPriKat.Enh = ImpKonv.InterntId.
        ELSE DO:
            CREATE tt_Error.
            ASSIGN
              ttPriKat.ErrFlag   = TRUE 
              tt_Error.LinjeNr   = ttPriKat.LinjeNr.
            ASSIGN
              tt_Error.Tekst     = VareIdent() + "* Mapping: SalgsEnhet (" + ttPriKat.Enh + ") uten mapping i konverteringstabell.".
            ASSIGN
              tt_Error.ErrNr     = 1
              tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
              ttPriKat.behStatus = 20. /* Kontroller */
        END.
    END.

    /* Konverteringstabell - StrKonv */
    IF CAN-FIND(FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = 'StrKonv') THEN 
    DO:
        FIND FIRST ImpKonv NO-LOCK WHERE 
            ImpKonv.EDB-System = cEDB-System AND 
            ImpKonv.Tabell     = 'StrKonv' AND 
            ImpKonv.EksterntId = ttPriKat.Str NO-ERROR.
        IF AVAILABLE ImpKonv THEN
        DO: 
            FIND FIRST StrKonv NO-LOCK WHERE 
                StrKonv.Storl = ImpKonv.InterntId NO-ERROR.
            IF NOT AVAILABLE StrKonv THEN 
            DO:
                LOOPEN:
                DO iInt = 1 TO 99999:
                    IF NOT CAN-FIND(FIRST StrKonv WHERE 
                                    StrKonv.StrKode = iInt) THEN 
                      LEAVE LOOPEN.
                    
                END. /* LOOPEN */
                IF iInt <= 99999 THEN 
                DO:
                    CREATE StrKonv.
                    ASSIGN 
                        StrKonv.StrKode = iInt
                        StrKonv.Storl   = ttPriKat.Str
                        .
                END.
            END.
            IF AVAILABLE StrKonv THEN 
            ASSIGN 
                ttPriKat.Str     = ImpKonv.InterntId
                ttPriKat.StrKode = StrKonv.StrKode
                .
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LagreStrekkoder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreStrekkoder Procedure 
PROCEDURE LagreStrekkoder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR plDec   AS DEC  NO-UNDO.
  DEF VAR pcTekst AS CHAR NO-UNDO.
  DEF VAR plEAN   AS DEC FORMAT "9999999999999" NO-UNDO.
  DEF VAR pcTest  AS CHAR NO-UNDO.
                            
  /* Lagrer de gyldige loggede strekkodene */
  LAGRER:
  FOR EACH tmpVPIStrekkode:
      /* Blanke strekkoder lagres ikke */
      IF TRIM(tmpVPIStrekkode.Kode) = "" THEN
          NEXT LAGRER.

      /* Har den ugyldige tegn? */
      ASSIGN
          plDec = DEC(tmpVPISTrekkode.Kode)
          NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
        NEXT LAGRER.
      
      ELSE IF (LENGTH(tmpVPISTrekkode.Kode) < 13 AND TRIM(tmpVPISTrekkode.Kode) <> "") THEN
      PLU_OG_ANNET:
      DO:
          FIND VpiStrekkode EXCLUSIVE-LOCK WHERE
              VpiStrekkode.EkstVpiLevNr = VPIArtBas.EkstVpiLevNr AND
              VpiStrekkode.VareNr       = VPIArtBas.VareNr AND
              VpiStrekkode.Kode         = tmpVPISTrekkode.Kode NO-ERROR.
          /* Har strekkoden blitt lagt over på en annen størrelse, må dette fikses */
          IF AVAILABLE vpiStrekkode THEN 
          DO:
              FIND StrKonv NO-LOCK WHERE
                StrKonv.StrKode = tmpVPIStrekkode.StrKode NO-ERROR.
              ASSIGN
                  VpiStrekkode.EkstVpiLevNr      = VPIArtBas.EkstVpiLevNr
                  VpiStrekkode.VareNr            = VPIArtBas.VareNr 
                  VpiStrekkode.Kode              = tmpVPIStrekkode.Kode
                  VPIStrekkode.Bestillingsnummer = IF tmpVPIStrekkode.Bestillingsnummer <> '' 
                                                     THEN tmpVPIStrekkode.Bestillingsnummer 
                                                     ELSE VPIStrekkode.Bestillingsnummer
                    VPIStrekkode.StrKode           = tmpVPIStrekkode.StrKode
                    VPIStrekkode.Storl             = tmpVPIStrekkode.Storl
                  VPIStrekkode.ERPNr             = tmpVPIStrekkode.ERPNR
                  .
          END.
          IF NOT AVAILABLE VpiStrekkode THEN
          DO:
              CREATE VpiStrekkode.
              BUFFER-COPY tmpVPIStrekkode TO VPIStrekkode
                  ASSIGN
                    VpiStrekkode.EkstVpiLevNr      = VPIArtBas.EkstVpiLevNr
                    VpiStrekkode.VareNr            = VPIArtBas.VareNr 
                    VpiStrekkode.Kode              = tmpVPIStrekkode.Kode
                    VPIStrekkode.Bestillingsnummer = tmpVPIStrekkode.Bestillingsnummer
                    VPIStrekkode.ERPNr             = tmpVPIStrekkode.ERPNR
                    VPIStrekkode.StrKode           = tmpVPIStrekkode.StrKode
                    VPIStrekkode.Storl             = tmpVPIStrekkode.Storl
                  .
          END.
      END. /* PLU_OG_ANNET */
      ELSE 
      LAGRE-OK-EAN-KODE:
      DO:
          /* Sjekk av EAN kode. */
          RUN bibl_chkean.p (INPUT-OUTPUT tmpVPIStrekkode.Kode).
          
          /* Finnes strekkoden uten ledende nuller, skal den legges inn uten ledende nuller. */
          IF CAN-FIND(Strekkode WHERE Strekkode.Kode = LEFT-TRIM(tmpVPIStrekkode.Kode,'0')) THEN 
            tmpVPIStrekkode.Kode = LEFT-TRIM(tmpVPIStrekkode.Kode,'0').
            
          /* Lagrer strekkoden. */
          FIND VpiStrekkode EXCLUSIVE-LOCK WHERE
              VpiStrekkode.EkstVpiLevNr = VPIArtBas.EkstVpiLevNr AND
              VpiStrekkode.VareNr       = VPIArtBas.VareNr AND
              VpiStrekkode.Kode         = tmpVPISTrekkode.Kode NO-ERROR.
          /* Har strekkoden blitt lagt over på en annen størrelse, må dette fikses */
          IF AVAILABLE vpiStrekkode THEN 
          DO:
/*              FIND StrKonv NO-LOCK WHERE                           */
/*                StrKonv.StrKode = tmpVPIStrekkode.StrKode NO-ERROR.*/
              ASSIGN
                  VpiStrekkode.EkstVpiLevNr      = VPIArtBas.EkstVpiLevNr
                  VpiStrekkode.VareNr            = VPIArtBas.VareNr 
                  VpiStrekkode.Kode              = tmpVPIStrekkode.Kode
                  VPIStrekkode.StrKode           = tmpVPIStrekkode.StrKode
                  VPIStrekkode.Storl             = tmpVPIStrekkode.Storl
                  VPIStrekkode.Bestillingsnummer = IF tmpVPIStrekkode.Bestillingsnummer <> '' THEN tmpVPIStrekkode.Bestillingsnummer ELSE VPIStrekkode.Bestillingsnummer
                  VPIStrekkode.ERPNr             = tmpVPIStrekkode.ERPNR
                  .
          END.
          IF NOT AVAILABLE VpiStrekkode THEN
          DO:
              CREATE VpiStrekkode.
              BUFFER-COPY tmpVPIStrekkode TO VPIStrekkode
                  ASSIGN
                    VpiStrekkode.EkstVpiLevNr      = VPIArtBas.EkstVpiLevNr
                    VpiStrekkode.VareNr            = VPIArtBas.VareNr 
                    VpiStrekkode.Kode              = tmpVPIStrekkode.Kode
                    VPIStrekkode.StrKode           = tmpVPIStrekkode.StrKode
                    VPIStrekkode.Storl             = tmpVPIStrekkode.Storl
                    VPIStrekkode.Bestillingsnummer = tmpVPIStrekkode.Bestillingsnummer
                    VPIStrekkode.ERPNr             = tmpVPIStrekkode.ERPNR                    
                  .
          END.
      END. /* LAGRE-OK-EAN-KODE */
  END. /* LAGRER */

  /* Tømmer loggen */
  FOR EACH tmpVPIStrekkode:
      DELETE tmpVPIStrekkode.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LinjeValidering) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LinjeValidering Procedure 
PROCEDURE LinjeValidering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR piLoop  AS INT  NO-UNDO.
    DEF VAR pcTekst AS CHAR NO-UNDO.
    DEF VAR lEANNr  AS DEC  NO-UNDO.
    DEFINE VARIABLE cEAN AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
    DEF VAR piInt   AS INT NO-UNDO.

  DEFINE BUFFER bufSalgsenhet FOR Salgsenhet.
  DEFINE BUFFER bufProdusent  FOR Produsent.
    
    /* Kontroll av pant */
    IF ttPriKat.LinkVare <> '' THEN 
    PANTSJEKK:
    DO:
      /* Kontroll av ugyldige tegn iLinkvareNr */
      ASSIGN lEANNr = DEC(ttPriKat.LinkVare) NO-ERROR.
      IF ERROR-STATUS:ERROR = TRUE THEN
      DO:
        CREATE tt_Error.
        ASSIGN
            piAntFeil          = piAntFeil + 1
            ttPriKat.ErrFlag   = TRUE
            tt_Error.LinjeNr   = ttPriKat.LinjeNr.
          ASSIGN
            tt_Error.Tekst     = VareIdent() + "* Pant: Ugyldige verdier i Link til pant feltet. (" + ttPriKat.LinkVare + ").".
          ASSIGN  
            tt_Error.ErrNr     = 1
            tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
            ttPriKat.behStatus = 20. /* Kontroller */
        LEAVE PANTSJEKK.
      END.
      /* Konverterer LinkVareNr fra EAN kode til ArtikkelNr. */
      IF bKonvPantLink THEN 
      DO:
        FIND Strekkode NO-LOCK WHERE Strekkode.Kode = ttPriKat.LinkVare NO-ERROR.
        IF AVAILABLE Strekkode THEN ttPriKat.LinkVare = STRING(Strekkode.ArtikkelNr).
        ELSE DO: 
          CREATE tt_Error.
          ASSIGN
            piAntFeil          = piAntFeil + 1
            ttPriKat.ErrFlag   = TRUE
            tt_Error.LinjeNr   = ttPriKat.LinjeNr.
          ASSIGN
            tt_Error.Tekst     = VareIdent() + "* Pant: Kan ikke konvertere Link til pant (NB: systemparameter) til artikkelnr. (" + ttPriKat.LinkVare + ").".
          ASSIGN
            ttPriKat.LinkVare  = '' /* Ukjent EAN. Blanker. */
            tt_Error.ErrNr     = 1
            tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
            ttPriKat.behStatus = 30. /* Feil */
        END.
      END.
      /* Pantvare skal finnes. */
      IF DECIMAL(ttPriKat.LinkVare) > 0 AND NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = DECIMAL(ttPriKat.LinkVare)) THEN 
      DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil          = piAntFeil + 1
          ttPriKat.ErrFlag   = TRUE
          tt_Error.LinjeNr   = ttPriKat.LinjeNr.
        ASSIGN
          tt_Error.Tekst     = VareIdent() + "* Pant: Ukjent pantvare i Link til pant feltet. (" + ttPriKat.LinkVare + ").".
        ASSIGN
          tt_Error.ErrNr     = 1
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
          ttPriKat.behStatus = 30. /* Feil */
      END.           
      /* Er link satt på her, er det ikke nødvendig å kontrollere dette i mottakskontrollen. */
      ELSE IF ttPriKat.LinkVare <> '' THEN
         ASSIGN 
         ttPriKat.behStatus    = (IF ttPriKat.behStatus < 30 THEN 1 ELSE ttPriKat.behStatus). /* Feil */ 
         ttPriKat.Kontrolleres = FALSE.
    END. /* PANTSJEKK */
    /* Hvis pant ikke er angitt, sjekk om varen er satt opp med pant i systemet. Hvis ja, skal den beholde panten. */
    IF ttPriKat.LinkVare = '' THEN
    PANT: 
    DO:
      FIND Strekkode NO-LOCK WHERE Strekkode.Kode = ttPriKat.EANnr NO-ERROR.
      IF AVAILABLE Strekkode THEN 
      DO:
        FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN 
          LEAVE PANT.
        /* Legger på eventuell link til pant som er satt opp i artikkelregisteret. */
        ttPriKat.LinkVare = STRING(ArtBas.LinkVareNr).
      END.    
    END. /* PANT */

    /* Oppretter størrelsene hvis parameter tilsier dette. */
    IF bOpprettStr = TRUE AND
       NOT CAN-FIND(FIRST StrKonv WHERE StrKonv.Storl = ttPriKat.Str)  THEN
    DO:
        FIND LAST bStrKonv NO-ERROR.
        CREATE StrKonv.
        ASSIGN
            StrKonv.StrKode = (IF AVAILABLE bStrKonv
                                 THEN bStrKonv.StrKode + 1
                                 ELSE 1)
            StrKonv.Storl   = ttPriKat.Str
            StrKonv.Merknad = "VPI import"
            .
        FIND CURRENT StrKonv NO-LOCK.
    END.
    /*Kontroll av størrelse */
    IF NOT CAN-FIND(StrKonv WHERE StrKonv.Storl = ttPriKat.Str) THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            piAntFeil          = piAntFeil + 1
            ttPriKat.ErrFlag   = TRUE
            tt_Error.LinjeNr   = ttPriKat.LinjeNr.
          ASSIGN
            tt_Error.Tekst     = VareIdent() + "* Størrelse: Ukjent størrelse. (" + ttPriKat.Str + ").".
          ASSIGN
            tt_Error.ErrNr     = 3
            tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
            ttPriKat.behStatus = 20. /* Kontroller */
    END.
    
    /* Er størrelsestypens navn nagitt, skal det brukes. */
    IF ttPriKat.StrTypeId = 0 AND ttPriKat.StrTab <> '' THEN 
    DO:
        /* Er navn på en størrelsestype angitt, skal den sjekkes */
        ASSIGN ttPriKat.StrTypeId = DEC(ttPriKat.StrTab) NO-ERROR.
        IF ERROR-STATUS:ERROR = TRUE THEN
        ERRBLK:
        DO:
            FIND FIRST StrType NO-LOCK WHERE 
                StrType.Beskrivelse = ttPriKat.StrTab NO-ERROR.
            IF AVAILABLE StrType THEN 
            DO:
                ASSIGN ttPriKat.StrTypeId = StrType.StrTypeID.
                LEAVE ERRBLK.
            END.
        END. /* ERRBLK */
    END.
    /* Kontroll at størrelsestypen finnes. */
    IF ttPriKat.StrTypeId > 0 THEN 
    STRTYPESJEKK:
    DO:          
      IF NOT CAN-FIND(StrType WHERE
                      StrType.StrTypeId = ttPriKat.StrTypeId) THEN
      DO:
        ttPriKat.StrtypeId = 0.
        LEAVE STRTYPESJEKK.
      END.
        
      /* Kontroll at størrelsen finnes i størrelsestypen. */
      IF NOT CAN-FIND(FIRST StrTStr WHERE
                      StrTStr.StrTypeId     = ttPriKat.StrTypeId AND
                      TRIM(StrTstr.SoStorl) = TRIM(ttPriKat.Str)) THEN
      DO:
        ttPriKat.StrtypeId = 0.
        LEAVE STRTYPESJEKK.
      END.
    END. /* STRTYPE */

    /* Kontroll av in/utmelding av grunnsortiment. */
    IF TRIM(ttPriKat.EANnr) <> '' AND ttPriKat.ProfilNr = IClProfilNr THEN 
    DO:
        FIND Strekkode NO-LOCK WHERE Strekkode.Kode = ttPriKat.EANnr NO-ERROR.
        IF NOT AVAILABLE Strekkode THEN 
            FIND Strekkode NO-LOCK WHERE Strekkode.Kode = LEFT-TRIM(ttPriKat.EANnr,'0') NO-ERROR.
        IF AVAILABLE Strekkode THEN 
        DO:
          FIND ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.          
          IF AVAILABLE ArtBas THEN 
          DO:
              IF CAN-DO('1,J,ja,Y,YES,TRUE',ttPriKat.Grunnsortiment) <> ArtBas.Grunnsortiment THEN 
              DO:
                  ASSIGN 
                    ttPriKat.Grunnsortiment = 'YES'.
               END.          
          END.
        END.
    END.
    
    /* Kontroll av leverandør. */
    ASSIGN ttPriKat.iLevNr = INT(ttPriKat.LevNr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
      CREATE tt_Error.
      ASSIGN
        piAntFeil          = piAntFeil + 1
        ttPriKat.ErrFlag   = TRUE 
        tt_Error.LinjeNr   = ttPriKat.LinjeNr.
      ASSIGN
        tt_Error.Tekst     = VareIdent() + "* Leverandør: Ugyldig tegn i leverandørnr : " + ttPriKat.LevNr + ".".
      ASSIGN
        tt_Error.ErrNr     = 4
        tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
        ttPriKat.behStatus = 30. /* Feil */
    END.
    ELSE DO:
      IF NOT CAN-FIND(LevBas WHERE LevBas.LevNr = INTEGER(ttPriKat.LevNr)) AND iDefLevNr = 0 THEN 
      DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil          = piAntFeil + 1
          ttPriKat.ErrFlag   = TRUE 
          tt_Error.LinjeNr   = ttPriKat.LinjeNr.
        ASSIGN
          tt_Error.Tekst     = VareIdent() + "* Leverandør: Ukjent leverandørnr: " + STRING(ttPriKat.LevNr) + ".".
        ASSIGN
          tt_Error.ErrNr     = 4
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
          ttPriKat.behStatus = 30. /* Feil */
      END.
      ELSE IF  NOT CAN-FIND(LevBas WHERE LevBas.LevNr = INTEGER(ttPriKat.LevNr))  AND iDefLevNr <> 0 THEN 
      DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil          = piAntFeil + 1
          ttPriKat.ErrFlag   = TRUE 
          tt_Error.LinjeNr   = ttPriKat.LinjeNr.
        ASSIGN
          tt_Error.Tekst     = VareIdent() + "* Leverandør: Ukjent leverandørnr: " + STRING(ttPriKat.LevNr) + ". Default leverandørnr benyttet.".
        ASSIGN
          tt_Error.ErrNr     = 4
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
          ttPriKat.behStatus = 30. /* Feil */
        ASSIGN 
          ttPriKat.LevNr  = STRING(iDefLevNr)
          ttPriKat.iLevNr = iDefLevNr.
      END.
      ELSE ASSIGN
        ttPriKat.iLevNr = INTEGER(ttPriKat.LevNr).      
    END.

    /* Kontroll at landkoden finnes. Oppretter ukjente. Ingen feilhåndtering, bare logging. */
    IF ttPriKat.AlfaKode2 <> '' AND  
       NOT CAN-FIND(FIRST AlfaLandKode WHERE
                    AlfaLandKode.AlfaKode2 = ttPriKat.AlfaKode2) THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            piAntFeil          = piAntFeil + 1
            tt_Error.LinjeNr   = ttPriKat.LinjeNr.
          ASSIGN
            tt_Error.Tekst     = VareIdent() + "* Landkode: Ukjent landkode. (" + ttPriKat.AlfaKode2 + "). Landkode opprettet.".
          ASSIGN
            tt_Error.ErrNr     = 3
            ttPriKat.BehStatTekst = tt_Error.Tekst
            tt_Error.ButikkNr  = ttPriKat.ButikkNr.
            
         DO:
           FIND LAST NumLandKode NO-LOCK NO-ERROR.
           IF NOT AVAILABLE NumLandKode 
             THEN piInt = 1.
             ELSE piInt = NumLandKode.NumLandKode + 1. 
           
           CREATE NumLandKode.
           ASSIGN
             NumLandKode.NumLandKode = piInt
             NumLandKode.Land        = 'Automatisk opprettet'.
             
           CREATE AlfaLandKode.
           ASSIGN
             AlfaLandKode.AlfaKode2   = ttPriKat.AlfaKode2
             AlfaLandKode.AlfaKode3   = ttPriKat.AlfaKode2
             AlfaLandKode.NumLandKode = piInt.
             .    
           IF AVAILABLE NumLandKode THEN RELEASE NumLandKode.
           IF AVAILABLE AlfaLandKode THEN RELEASE AlfaLandKode.
         END.
    END.
    
    /* Kontroll av gyldige tegn i varegruppe */
    ASSIGN ttPriKat.Vg = INTEGER(ttPriKat.VareGruppe) 
           pcMva%      = "" NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
    DO:
      CREATE tt_Error.
      ASSIGN
        piAntFeil          = piAntFeil + 1
        ttPriKat.ErrFlag   = TRUE 
        tt_Error.LinjeNr   = ttPriKat.LinjeNr.
      ASSIGN
        tt_Error.Tekst     = VareIdent() + "* Varegruppe: Ugyldige verdier i varegruppefeltet: " + STRING(ttPriKat.Varegruppe) + ".".
      ASSIGN
        tt_Error.ErrNr     = 5
        tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
        ttPriKat.behStatus = 30. /* Feil */
    END.
    /* Har artikkelen er varegruppe fra før, og det ikke er oppgitt i filen, settes den gamle varegruppen på varen. */
    ELSE IF INTEGER(ttPriKat.VareGruppe) = 0 THEN 
    DO:
        IF AVAILABLE ArtBas THEN RELEASE ArtBas.
        FIND StrekKode NO-LOCK WHERE 
            StrekKode.Kode = ttPriKat.EANNr NO-ERROR.
        IF AVAILABLE StrekKode THEN 
            FIND ArtBas OF StrekKode NO-LOCK NO-ERROR.
        IF AVAILABLE ArtBas THEN 
            ASSIGN 
                ttPriKat.VareGruppe = STRING(ArtBas.Vg)
                ttPriKat.Vg         = INTEGER(ttPriKat.VareGruppe)
                .
    END.
    
    /* Kontroll av varegruppe og setting av mva% */
    IF NOT CAN-FIND(VarGr WHERE VarGr.Vg = ttPriKat.Vg) THEN
    DO:
      IF iDefaultVg <> 0 THEN 
      DO:
        FIND Strekkode NO-LOCK WHERE Strekkode.Kode = ttPriKat.EANNr NO-ERROR.
        IF AVAILABLE Strekkode THEN FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
        IF AVAILABLE ArtBas THEN ttPriKat.Vg = ArtBas.Vg.
        ELSE ttPriKat.Vg = iDefaultVg.
      END.
      IF NOT CAN-FIND(VarGr WHERE VarGr.Vg = ttPriKat.Vg) THEN 
      DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil          = piAntFeil + 1
          ttPriKat.ErrFlag   = TRUE. 
        ASSIGN
          tt_Error.LinjeNr   = ttPriKat.LinjeNr
          tt_Error.Tekst     = VareIdent() + "* Varegruppe: Ukjent varegruppe: " + STRING(ttPriKat.Varegruppe) + "."
          tt_Error.ErrNr     = 5
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
          ttPriKat.behStatus = 30. /* Feil */
        IF bOpprettVg THEN 
        DO:
          FIND FIRST Moms NO-LOCK WHERE
              Moms.MomsProc = 25.00 NO-ERROR.
          IF NOT AVAILABLE Moms THEN 
            FIND FIRST Moms NO-LOCK WHERE
              Moms.MomsProc = DEC(ttPriKat.Mva_Proc) NO-ERROR.
          
          FIND FIRST HuvGr NO-LOCK NO-ERROR.
          CREATE VarGr.
          ASSIGN
            VarGr.Vg      = ttPriKat.Vg
            VarGr.VgBeskr = 'Opprettet via VPI import'
            VarGr.MomsKod = (IF AVAILABLE Moms THEN Moms.MomsKod ELSE piMomsKod)
            VarGr.Hg      = (IF LENGTH(STRING(VarGr.Vg)) = 4 THEN INT(SUBSTRING(STRING(VarGr.Vg),1,2))                                
                             ELSE IF AVAILABLE HuvGr THEN HuvGr.Hg ELSE 1)
            .
          IF AVAILABLE VarGr THEN 
          DO:
              FIND FIRST VgKat NO-LOCK WHERE
                VgKat.Vg    = VarGr.Vg AND
                VgKat.VgKAt = 1 NO-ERROR.
              IF NOT AVAILABLE VgKat THEN
                DO:
                  CREATE VgKat.
                  ASSIGN
                    VgKat.Vg = VarGr.Vg
                    VgKat.VgKat = 1 
                    VgKat.KatNr = 1.
                END.
          END.
        END.
      END.
    END.
    /* Henter mva for varegruppen */
    DO:        
      FIND VarGr NO-LOCK WHERE VarGr.Vg = INT(ttPriKat.Varegruppe) NO-ERROR.
      IF AVAILABLE VarGr THEN
      DO:
        ASSIGN
          ttPriKat.Hg = VarGr.Hg.
        pcMva% = "".
        FIND Moms NO-LOCK WHERE Moms.MomsKod = VarGr.MomsKod NO-ERROR.
        IF AVAILABLE Moms THEN pcMva% = STRING(Moms.MomsProc).
      END.
    END.
    
    /* Setting av Mva% */
    IF ttPriKat.Mva_Proc = '' THEN ttPriKat.Mva_Proc = pcMva%. 
    ASSIGN lDec = DEC(ttPriKat.Mva_Proc) NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
    DO:
      CREATE tt_Error.
      ASSIGN
        piAntFeil          = piAntFeil + 1
        ttPriKat.ErrFlag   = TRUE 
        tt_Error.LinjeNr   = ttPriKat.LinjeNr.
      ASSIGN
        tt_Error.Tekst     = VareIdent() + "* Mva%: Ugyldige verdier i mva% feltet: " + STRING(ttPriKat.Mva_Proc) + ".".
      ASSIGN
        tt_Error.ErrNr     = 6
        tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
        ttPriKat.behStatus = 30. /* Feil */
    END.   
    ELSE DO: 
      FIND FIRST Moms NO-LOCK WHERE Moms.MomsProc = DEC(ttPriKat.Mva_Proc) NO-ERROR.
      IF AVAILABLE Moms THEN ASSIGN ttPriKat.MomsKod = Moms.MomsKod.
      ELSE DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil          = piAntFeil + 1
          ttPriKat.ErrFlag   = TRUE 
          tt_Error.LinjeNr   = ttPriKat.LinjeNr.
        ASSIGN
          tt_Error.Tekst     = VareIdent() + "* Mva%: Mva% ikke definert i mva tabell: " + STRING(ttPriKat.Mva_Proc) + ".".
        ASSIGN
          tt_Error.ErrNr     = 6
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
          ttPriKat.behStatus = 30. /* Feil */
      END.
    END.
    
    /* Kontroll av Vareområde (RAvdNr) */
    IF TRIM(ttPriKat.RAvdNr) <> '' THEN
    VAREOMRADE:
    DO:
      lDec = DECIMAL(ttPriKat.RAvdNr) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
      DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil          = piAntFeil + 1
          ttPriKat.ErrFlag   = TRUE 
          tt_Error.LinjeNr   = ttPriKat.LinjeNr.
        ASSIGN
          tt_Error.Tekst     = VareIdent() + "* Vareområde: Vareområdefeltet inneholder ugyldige tegn: " + STRING(ttPriKat.RAvdNr) + ".".
        ASSIGN
          tt_Error.ErrNr     = 17
          ttPriKat.RAvdNr    = '' /* !! Blanker feltet men oppdaterer linjen alikevel. */   
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
          ttPriKat.behStatus = 20. /* Kontroll */
      END.
      IF lDec > 999 THEN 
      DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil          = piAntFeil + 1
          ttPriKat.ErrFlag   = TRUE 
          tt_Error.LinjeNr   = ttPriKat.LinjeNr.
        ASSIGN
          tt_Error.Tekst     = VareIdent() + "* Vareområde: For stor verdi i vareområde feltet. Maks er 999: " + STRING(ttPriKat.RAvdNr) + ".".
        ASSIGN
          tt_Error.ErrNr     = 17
          ttPriKat.RAvdNr    = '' /* !! Blanker feltet men oppdaterer linjen alikevel. */   
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
          ttPriKat.behStatus = 20. /* Kontroll */
      END.
      /* Oppretter regnskapsavdelingen hvis den ikke finnes.*/
      IF NOT CAN-FIND(Regnskapsavdeling WHERE
                      Regnskapsavdeling.RAvdNr = INTEGER(ttPriKat.RAvdNr)) THEN 
      DO:
        CREATE Regnskapsavdeling.
        ASSIGN 
          Regnskapsavdeling.RAvdNr = INT(ttPriKat.RAvdNr)
          Regnskapsavdeling.RAvdBeskrivelse = 'VPI import ' + STRING(TODAY)
          .
      END.
    END. /* VAREOMRADE */
    
    /* Kontroll av produsent */
    ttPriKat.iProdNr = 0. 
    ASSIGN ttPriKat.iProdNr = INT(ttPriKat.Produsent) NO-ERROR.
    RELEASE Produsent.
    IF ERROR-STATUS:ERROR THEN
    PRODUSENT:
    DO:
      /* Det kan være at det ligger navn på produsent i filen. Slår mot databasen for å sjekke det. */
      IF ttPriKat.Produsent <> '' THEN 
      DO:
        FIND FIRST Produsent NO-LOCK WHERE Produsent.Beskrivelse = ttPriKat.Produsent NO-ERROR.
        IF AVAILABLE Produsent THEN ttPriKat.iProdNr = Produsent.ProdNr.
      END.
      IF NOT AVAILABLE Produsent AND bOpprettProdusent AND ttPriKat.iProdNr = 0 THEN 
      DO:
        FIND LAST bProdusent NO-LOCK NO-ERROR.
        CREATE Produsent.
        ASSIGN
          Produsent.ProdNr      = IF AVAILABLE bProdusent THEN bProdusent.ProdNr + 1 ELSE 1
          Produsent.Beskrivelse = ttPriKat.Produsent
          Produsent.Notat       = 'Automatisk opprettet ved VPI import ' + STRING(TODAY)
          ttPriKat.iProdNr      = Produsent.ProdNr.
      END.
      ELSE IF ttPrikat.iProdNr = 0 THEN 
      DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil          = piAntFeil + 1
          ttPriKat.ErrFlag   = TRUE 
          tt_Error.LinjeNr   = ttPriKat.LinjeNr.
        ASSIGN
          tt_Error.Tekst     = VareIdent() + "* Produsent: Ugyldige verdier i produsentfelt: " + STRING(ttPriKat.Produsent) + ".".
        ASSIGN
          tt_Error.ErrNr     = 7
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
          ttPriKat.behStatus = 20. /* Kontroll */
      END.
    END. /* PRODUSENT */
    ELSE IF NOT CAN-FIND(Produsent WHERE Produsent.ProdNr = ttPriKat.iProdNr) THEN
    DO:
      ASSIGN piAntFeil = piAntFeil + 1.
      CREATE tt_Error.
      ASSIGN
        piAntFeil          = piAntFeil + 1
        ttPriKat.ErrFlag = TRUE
        tt_Error.LinjeNr   = ttPriKat.LinjeNr.
      ASSIGN
        tt_Error.Tekst     = VareIdent() + "* Produsent: Ukjent produsent: " + STRING(ttPriKat.Produsent) + ".".
      ASSIGN
        tt_Error.ErrNr     = 7
        tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
        ttPriKat.behStatus = 20. /* Kontroll */
    END.

    /* Hvis fargen finnes. */
    IF ttPriKat.FargeTekst <> '' THEN 
    DO:
        FIND FIRST Farg NO-LOCK WHERE 
            Farg.FarBeskr = ttPriKat.FargeTekst NO-ERROR.
        IF AVAILABLE Farg THEN 
        DO:
            ASSIGN 
                ttPriKat.Farg = Farg.Farg.
        END.
        ELSE DO:
            FIND LAST Farg NO-LOCK NO-ERROR.
            IF AVAILABLE Farg THEN 
                iInt = Farg.Farg + 1.
            ELSE 
                iInt = 1.    
            CREATE Farg.
            ASSIGN 
                Farg.Farg     = iInt
                Farg.FarBeskr = ttPriKat.FargeTekst
                ttPriKat.Farg = iInt
                .
        END.
    END. 
    /* Kontroll av fargekode */
    IF ttPriKat.Farg = 0 THEN 
    DO:
        ASSIGN ttPriKat.Farg = INTEGER(ttPriKat.Fargekode) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO:
          CREATE tt_Error.
          ASSIGN
            piAntFeil          = piAntFeil + 1
            ttPriKat.ErrFlag   = TRUE 
            tt_Error.LinjeNr   = ttPriKat.LinjeNr.
          ASSIGN
            tt_Error.Tekst     = VareIdent() + "* Fargekode: Ugyldige verdier i fargekodefelt: " + STRING(ttPriKat.Fargekode) + ".".
          ASSIGN
            tt_Error.ErrNr     = 8
            tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
            ttPriKat.behStatus = 20. /* Kontroll */
        END.
    END.
    IF NOT CAN-FIND(Farg WHERE Farg.Farg = ttPriKat.Farg) THEN
    DO:
      ASSIGN piAntFeil = piAntFeil + 1.
      CREATE tt_Error.
      ASSIGN
        piAntFeil          = piAntFeil + 1
        ttPriKat.Farg    = 0 /* Da skal vi ikke ha med fargekoden inn. */
        ttPriKat.ErrFlag = TRUE
        tt_Error.LinjeNr   = ttPriKat.LinjeNr.
      ASSIGN
        tt_Error.Tekst     = VareIdent() + "* Fargekode: Ukjent fargekode: " + STRING(ttPriKat.Fargekode) + ".".
      ASSIGN
        tt_Error.ErrNr     = 8
        tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
        ttPriKat.behStatus = 20. /* Kontroll */
    END.

    /* Blankt varemerke - sjekk på artikkel og bruk det som står der. */
    IF TRIM(ttPriKat.Varemerke) = '' THEN 
    DO:
      FIND Strekkode NO-LOCK WHERE Strekkode.Kode = ttPriKat.EANnr NO-ERROR.
      IF AVAILABLE Strekkode THEN 
        FIND ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
      IF AVAILABLE ArtBas THEN 
        FIND Varemerke NO-LOCK WHERE
          VareMerke.VmId = ArtBas.VmId NO-ERROR.
      IF AVAILABLE VareMerke THEN
        ASSIGN
          ttPriKat.VmId      = VareMerke.VmId
          ttPriKat.Varemerke = VareMerke.KortNavn.
      RELEASE Varemerke.
    END.
    /* Oppretter varemerke hvis det ikke finnes. */
    ELSE DO:
      IF bOpprettVaremerke THEN 
      DO:
        FIND FIRST Varemerke NO-LOCK WHERE TRIM(Varemerke.KortNavn) BEGINS trim(ttPriKat.Varemerke) NO-ERROR.
        IF NOT AVAILABLE Varemerke THEN
          FIND FIRST VareMerke WHERE TRIM(Varemerke.Beskrivelse) BEGINS TRIM(ttPriKat.Varemerke) NO-ERROR.
        IF NOT AVAILABLE Varemerke THEN
        OPPRETT-VAREMERKE:
        DO:
          FIND LAST Varemerke NO-LOCK NO-ERROR.
          IF AVAILABLE Varemerke THEN piInt = Varemerke.VmId + 1.
          ELSE piInt = 1.
          CREATE Varemerke.
          ASSIGN
              VareMerke.VmId        = piInt
              VareMerke.KortNavn    = TRIM(ttPriKat.Varemerke)
              Varemerke.Beskrivelse = TRIM(ttPriKat.Varemerke).
          FIND CURRENT varemerke NO-LOCK.
        END. /*OPPRETT-VAREMERKE */
      END.
      ELSE DO:
        ASSIGN piAntFeil = piAntFeil + 1.
        CREATE tt_Error.
        ASSIGN
          piAntFeil          = piAntFeil + 1
          ttPriKat.ErrFlag = TRUE
          tt_Error.LinjeNr   = ttPriKat.LinjeNr.
        ASSIGN
          tt_Error.Tekst     = VareIdent() + "* Varemerke: Ukjent varemerke: " + STRING(ttPriKat.Varemerke) + ".".
        ASSIGN
          tt_Error.ErrNr     = 9
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
          ttPriKat.behStatus = 20. /* Kontroll */
      END.  
      IF AVAILABLE Varemerke THEN ttPriKat.VmId = VareMerke.VmId.
    END.

    /* Kontroll av enhet */
    IF NOT CAN-FIND(FIRST Salgsenhet WHERE SalgsEnhet.SalgsEnhTekst = TRIM(ttPriKat.Enh)) THEN
    DO:
      IF bGenSalgsEnhet THEN 
        RUN opprettSalgsenhet(TRIM(ttPriKat.Enh)).
      ELSE DO:
        CREATE tt_Error.
        ASSIGN
            piAntFeil          = piAntFeil + 1
            ttPriKat.ErrFlag   = TRUE
            tt_Error.LinjeNr   = ttPriKat.LinjeNr.
          ASSIGN
            tt_Error.Tekst     = VareIdent() + "* Enhet: Ukjent enhet. (" + ttPriKat.Enh + ").".
          ASSIGN
            tt_Error.ErrNr     = 10
            tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
            ttPriKat.behStatus = 20. /* Kontroll */
      END.
    END.

    /* Kontroll av antall i enhet */
    ASSIGN
        lDec = dec(ttPriKat.AntIEnh)
        NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            piAntFeil          = piAntFeil + 1
            ttPriKat.ErrFlag   = TRUE
            tt_Error.LinjeNr   = ttPriKat.LinjeNr.
          ASSIGN
            tt_Error.Tekst     = VareIdent() + "* Antall i enhet: Ugyldige verdier i AntIEnh feltet. (" + ttPriKat.AntIEnh + ").".
          ASSIGN
            tt_Error.ErrNr     = 10
            tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
            ttPriKat.behStatus = 20. /* Kontroll */
    END.
    /* Kontroll av LevPrisEngros */
    ASSIGN
        lDec = dec(ttPriKat.LevPrisEngros)
        NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            piAntFeil          = piAntFeil + 1
            ttPriKat.ErrFlag = TRUE
            tt_Error.LinjeNr = ttPriKat.LinjeNr.
          ASSIGN
            tt_Error.Tekst   = VareIdent() + "* Engrospris: Ugyldige verdier i engrosprisfeltet. (" + ttPriKat.LevPrisEngros + ").".
          ASSIGN
            tt_Error.ErrNr     = 10
            tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
            ttPriKat.behStatus = 20. /* Kontroll */
    END.
    /* Kontroll av forhåndsrabatt */
    ASSIGN
        lDec = dec(ttPriKat.forhRab%)
        NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            piAntFeil          = piAntFeil + 1
            ttPriKat.ErrFlag   = TRUE
            tt_Error.LinjeNr   = ttPriKat.LinjeNr.
          ASSIGN
            tt_Error.Tekst     = VareIdent() + "* Forhåndsrabatt: Ugyldige verdier i forhåndsrabattfeltet. (" + ttPriKat.forhRab% + ").".
          ASSIGN
            tt_Error.ErrNr     = 10
            tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
            ttPriKat.behStatus = 20. /* Kontroll */
    END.
    /* Kontroll av suppleringsrabatt */
    ASSIGN
        lDec = dec(ttPriKat.suppRab%)
        NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            piAntFeil          = piAntFeil + 1
            ttPriKat.ErrFlag   = TRUE
            tt_Error.LinjeNr   = ttPriKat.LinjeNr.
          ASSIGN
            tt_Error.Tekst     = VareIdent() + "* Suppleringsrabatt: Ugyldige verdier i suppleringsrabattfeltet. (" + ttPriKat.suppRab% + ").".
          ASSIGN
            tt_Error.ErrNr     = 11
            tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
            ttPriKat.behStatus = 20. /* Kontroll */
    END.
    /* Kontroll av veiledende pris */
    ASSIGN
        lDec = dec(ttPriKat.VeilPris)
        NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            piAntFeil          = piAntFeil + 1
            ttPriKat.ErrFlag   = TRUE
            tt_Error.LinjeNr   = ttPriKat.LinjeNr.
          ASSIGN
            tt_Error.Tekst     = VareIdent() + "* VeilPris: Ugyldige verdier i veiledendepris feltet. (" + ttPriKat.VeilPris + ").".
          ASSIGN
            tt_Error.ErrNr     = 11
            tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
            ttPriKat.behStatus = 20. /* Kontroll */
    END.
    pcTekst = ''.
    /* Kontroll av leveringsdato1-4 */
    DO piLoop = 1 TO 4:
        ASSIGN
            pcTekst = (IF piLoop = 1 
                     THEN (ttPriKat.LevUke1)
                    ELSE IF piLoop = 2 
                     THEN (ttPriKat.LevUke2)
                    ELSE IF piLoop = 3 
                     THEN (ttPriKat.LevUke3)
                    ELSE (ttPriKat.LevUke4))
            .
        ASSIGN
            lDec = dec(pcTekst)
            NO-ERROR.
        IF ERROR-STATUS:ERROR = TRUE THEN
        DO:
            CREATE tt_Error.
            ASSIGN
                piAntFeil          = piAntFeil + 1
                ttPriKat.ErrFlag = TRUE
                tt_Error.LinjeNr = ttPriKat.LinjeNr.
              ASSIGN
                tt_Error.Tekst   = VareIdent() + "* Leveringsdato: Ugyldige verdier i et av leveringsdatofeltene (år-uke). (" + pcTekst + ").".
              ASSIGN
                tt_Error.ErrNr     = 12
                tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
                ttPriKat.behStatus = 20. /* Kontroll */
        END.
        /* Det ligger bare space i datofeltet. */
        IF lDec = 0 THEN pcTekst = ''.
        IF TRIM(pcTekst) <> "" AND LENGTH(TRIM(pcTekst)) <> 6 THEN
        DO:
            CREATE tt_Error.
            ASSIGN
                piAntFeil          = piAntFeil + 1
                ttPriKat.ErrFlag   = TRUE
                tt_Error.LinjeNr   = ttPriKat.LinjeNr.
              ASSIGN
                tt_Error.Tekst     = VareIdent() + "* Leveringsdato: Feil angivelse av år og måned i et av leveringsdato feltene. (" + pcTekst + ").".
              ASSIGN
                tt_Error.ErrNr     = 12
                tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
                ttPriKat.behStatus = 20. /* Kontroll */
        END.
    END.

    /* Kontroll av decimalfelt */
    DO piLoop = 1 TO 4:
        ASSIGN
            pcTekst = (IF piLoop = 1 THEN (ttPriKat.nettoForh)
                    ELSE IF piLoop = 2 THEN (ttPriKat.kalkForh)
                    ELSE IF piLoop = 3 THEN (ttPriKat.BFforh)
                    ELSE IF piLoop = 4 THEN (ttPriKat.nettoSupp)
                    ELSE IF piLoop = 5 THEN (ttPriKat.kalkSupp)
                    ELSE IF piLoop = 6 THEN (ttPriKat.BFsupp)
                    ELSE (ttPriKat.Markedspris))
            .
        ASSIGN
            lDec = dec(pcTekst)
            NO-ERROR.
        IF ERROR-STATUS:ERROR = TRUE THEN
        DO:
            CREATE tt_Error.
            ASSIGN
                piAntFeil          = piAntFeil + 1
                ttPriKat.ErrFlag   = TRUE
                tt_Error.LinjeNr   = ttPriKat.LinjeNr.
              ASSIGN
                tt_Error.Tekst     = VareIdent() + "* Ugyldige verdier: " + pcTekst + ": Ugyldig verdi i et av feltene nettoForh, kalkForh, BFforh, nettoSupp, kalkSupp, BFsupp, Markedspris.".
              ASSIGN
                tt_Error.ErrNr     = 11
                tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
                ttPriKat.behStatus = 20. /* Kontroll */
        END.
    END.

    IF ttPriKat.Karakteristikk <> '' THEN 
    DO piLoop = 1 TO NUM-ENTRIES(ttPriKat.Karakteristikk,','):
        IF NOT CAN-FIND(FIRST Karakteristikk WHERE 
                              Karakteristikk.KarakteristikkId = ENTRY(piLoop,ttPriKat.Karakteristikk,',')) THEN 
        DO:
            CREATE tt_Error.
            ASSIGN
                piAntFeil          = piAntFeil + 1
                ttPriKat.ErrFlag   = TRUE
                tt_Error.LinjeNr   = ttPriKat.LinjeNr.
              ASSIGN
                tt_Error.Tekst     = VareIdent() + "* Ugyldige verdier: " + pcTekst + ": Ugyldig karakteristikk:" + ENTRY(piLoop,ttPriKat.Karakteristikk,',') + '.'.
              ASSIGN
                tt_Error.ErrNr     = 11
                tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
                ttPriKat.behStatus = 20. /* Kontroll */
        END.
    END. 

    /* Kontroll av Sesong */
    ASSIGN
        lDec = DECIMAL(ttPriKat.Sesong)
        ttPriKat.Sasong = INT(ttPriKat.Sesong)
        NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            piAntFeil          = piAntFeil + 1
            ttPriKat.ErrFlag   = TRUE
            tt_Error.LinjeNr   = ttPriKat.LinjeNr.
          ASSIGN
            tt_Error.Tekst     = VareIdent() + "* Sesong: Ugyldige verdier i feltet. (" + ttPriKat.Sesong + ").".
          ASSIGN
            tt_Error.ErrNr     = 13
            tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
            ttPriKat.behStatus = 20. /* Kontroll */
    END.
    IF NOT CAN-FIND(Sasong WHERE
                    Sasong.SaSong = ttPriKat.Sasong) THEN
    DO:
        IF bOpprettSesong THEN 
        DO:
          CREATE bSasong.
          ASSIGN
            bSasong.Sasong   = INT(ttPriKat.Sasong)
            bSasong.SasBeskr = 'Aut. opprettet ' + STRING(TODAY)
            .
          RELEASE bSasong.          
        END.
        ELSE DO:
          CREATE tt_Error.
          ASSIGN
              piAntFeil          = piAntFeil + 1
              ttPriKat.ErrFlag   = TRUE
              tt_Error.LinjeNr   = ttPriKat.LinjeNr.
            ASSIGN
              tt_Error.Tekst     = VareIdent() + "* Sesong: Ugyldige sesongkode i feltet. (" + ttPriKat.Sesong + ").".
            ASSIGN
              tt_Error.ErrNr     = 13
              tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
              ttPriKat.behStatus = 20. /* Kontroll */
        END.
    END.
    
    /* Kontroll av valuta */
    IF NOT CAN-FIND(Valuta WHERE Valuta.ValKod = ttPriKat.ValKod) THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            piAntFeil          = piAntFeil + 1
            ttPriKat.ErrFlag   = TRUE
            tt_Error.LinjeNr   = ttPriKat.LinjeNr.
          ASSIGN
            tt_Error.Tekst     = VareIdent() + "* Valuta: Ukjent valutakode. (" + ttPriKat.ValKod + ").".
          ASSIGN
            tt_Error.ErrNr     = 14
            tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
            ttPriKat.behStatus = 20. /* Kontroll */
    END.

    /* Kontroll av jamførenhet */
    IF NOT CAN-FIND(FIRST Jamforenhet WHERE Jamforenhet.Jamforenhet = ttPriKat.Jamforenhet) THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            piAntFeil          = piAntFeil + 1
            ttPriKat.ErrFlag   = TRUE
            tt_Error.LinjeNr   = ttPriKat.LinjeNr.
          ASSIGN
            ttPriKat.Jamforenhet = 'Stk'
            tt_Error.Tekst     = VareIdent() + "* Jamførenhet: Ukjent jamførenhet. (" + ttPriKat.Jamforenhet + "). Konvertert til 'Stk'.".
          ASSIGN
            tt_Error.ErrNr     = 15
            tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
            ttPriKat.behStatus = 20. /* Kontroll */
    END.

    /* Priskontroll. Innpris = 0?. */
    IF DEC(ttPriKat.LevPrisEngros) = 0 THEN
    DO:
      DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil          = piAntFeil + 1
          ttPriKat.ErrFlag   = TRUE 
          tt_Error.LinjeNr   = ttPriKat.LinjeNr.
        ASSIGN
          tt_Error.Tekst     = VareIdent() + "* Pris: Innpris = 0: " + STRING(ttPriKat.LevPrisEngro) + ".".
        ASSIGN
          tt_Error.ErrNr     = 18
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
          ttPriKat.behStatus = 20. /* Kontroller */
      END.
    END.
    
/*    
MESSAGE 'ttPriKat.Markedspris' + '|' + ttPriKat.Markedspris + '|' 
VIEW-AS ALERT-BOX.
*/    
    /* Priskontroll. Utpris = 0?. */
    IF DEC(ttPriKat.Markedspris) = 0 THEN
    DO:
      DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil          = piAntFeil + 1
          ttPriKat.ErrFlag   = TRUE 
          tt_Error.LinjeNr   = ttPriKat.LinjeNr.
        ASSIGN
          tt_Error.Tekst     = VareIdent() + "* Pris: Utpris = 0: " + STRING(ttPriKat.Markedspris) + ".".
        ASSIGN
          tt_Error.ErrNr     = 19
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
          ttPriKat.behStatus = 20. /* Kontroller */
      END.
    END.

    /* Priskontroll. Utpris <= Innpris?. */
    IF DEC(ttPriKat.Markedspris) <= DECIMAL(ttPriKat.LevPrisengros) THEN
    DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil          = piAntFeil + 1
          ttPriKat.ErrFlag   = FALSE 
          tt_Error.LinjeNr   = ttPriKat.LinjeNr.
        ASSIGN
          tt_Error.Tekst     = VareIdent() + "* Pris: Innpris >= Utpris: " + STRING(ttPriKat.LevPrisengros) + " <= " + STRING(ttPriKat.Markedspris) + ".".
        ASSIGN
          tt_Error.ErrNr     = 20
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
          ttPriKat.behStatus = 1. /* Ubehandlet - Dette kan være en riktig situasjon os skal slippes videre, men logges. */
    END.

    /* Priskontroll. Har innpris endret seg mer enn tillatt?. */
    IF lAvvik% > 0 AND DEC(ttPriKat.LevPrisEngros) > 0 THEN
    DO:
        IF AVAILABLE Strekkode THEN RELEASE Strekkode.
        IF AVAILABLE ArtBas    THEN RELEASE ArtBas.
        IF AVAILABLE ArtPris   THEN RELEASE ArtPris.
        FIND Strekkode NO-LOCK WHERE
          Strekkode.Kode = ttPriKat.EAN NO-ERROR.
        IF AVAILABLE Strekkode THEN 
        DO:
            FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
            IF AVAILABLE ArtBas THEN 
                FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
            IF AVAILABLE ArtPris AND ArtPris.InnkjopsPris[1] > 0 THEN 
            DO:
                
              ASSIGN
                dcValPris  = DECIMAL(ttPriKat.LevPrisEngros)
                dcUtpris   = DECIMAL(ttPriKat.MarkedsPris)
                dcRabatt   = DECIMAL(ttPriKat.forhRab%)
                NO-ERROR.

              lMvaKr = dcUtpris - (dcUtpris / (1 + (ArtPris.Mva%[1] / 100))).
              IF lMvaKr = ? THEN lMvaKr = 0.
              lDB% = ROUND(((dcUtpris - lMvaKr - dcValPris) * 100) / (dcUtpris - lMvaKr),2).
              IF lDB% = ? THEN lDB% = 0.

              /*lDiff% = ROUND(ABS(((ArtPris.InnkjopsPris[1] - DEC(ttPriKat.LevPrisEngros)) * 100) / ArtPris.InnkjopsPris[1]),2).*/
              lDiff% = ABS(ABS(ArtPris.DB%[1]) - ABS(lDb%)). 
              IF lDiff% >= lAvvik% THEN 
              DO:
                CREATE tt_Error.
                ASSIGN
                  piAntFeil          = piAntFeil + 1
                  ttPriKat.ErrFlag   = TRUE 
                  tt_Error.LinjeNr   = ttPriKat.LinjeNr.
                ASSIGN
                  tt_Error.Tekst     = VareIdent() + "* Pris: Db% avviker mer enn tilatt. Ny db%: " + STRING(lDb%) 
                                                   + " Gammel Db%: " + STRING(ArtPris.Db%[1]) 
                                                   + " Avvik: " + STRING(lDiff%) + " Grense: " + STRING(lAvvik%) + ".".
                ASSIGN
                  tt_Error.ErrNr     = 23
                  tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
                  ttPriKat.behStatus = 20. /* Kontroller */
              END.
            END.
        END.
    END.

    /* Kontroll av gyldig størrelse */
    IF NOT CAN-FIND(StrKonv NO-LOCK WHERE StrKonv.Storl = ttPriKat.Str) THEN
    DO:
        CREATE tt_Error.
        ASSIGN
          piAntFeil          = piAntFeil + 1
          ttPriKat.ErrFlag   = TRUE 
          tt_Error.LinjeNr   = ttPriKat.LinjeNr.
        ASSIGN
          tt_Error.Tekst     = VareIdent() + "* Størrelse: Ukjent størrelse: " + ttPriKat.Str + ".".
        ASSIGN
          tt_Error.ErrNr     = 21
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
          ttPriKat.behStatus = 20. /* Kontroll */
    END.

    /* Tester valutapris */
    ASSIGN
      dcValPris  = DECIMAL(ttPriKat.LevPrisEngros)
      dcUtpris   = DECIMAL(ttPriKat.MarkedsPris)
      dcRabatt   = DECIMAL(ttPriKat.forhRab%)
      NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
    DO:
      CREATE tt_Error.
      ASSIGN
        piAntFeil          = piAntFeil + 1
        ttPriKat.ErrFlag   = TRUE 
        tt_Error.LinjeNr   = ttPriKat.LinjeNr.
      ASSIGN
        tt_Error.Tekst     = VareIdent() + "* Pris: Ugyldige verdier i et av kalkylefeltene. (Innpris: " + ttPriKat.LevPrisEngros + ", Pris: " + ttPriKat.Markedspris + ", Forh.rab: " + ttPriKat.forhRab% + ").".
      ASSIGN
        tt_Error.ErrNr     = 22
        tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
        ttPriKat.behStatus = 30. /* Feil */
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LogStrekkoder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LogStrekkoder Procedure 
PROCEDURE LogStrekkoder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      /* Lagrer strekkoden.                                                 */
      FIND tmpVpiStrekkode EXCLUSIVE-LOCK WHERE
          tmpVpiStrekkode.EkstVpiLevNr = VPIFilHode.EkstVpiLevNr AND
          tmpVpiStrekkode.VareNr       = string(ttPriKat.ArtikkelNr) AND
          tmpVpiStrekkode.Kode         = ttPriKat.EANnr NO-ERROR.
      IF NOT AVAILABLE tmpVpiStrekkode THEN
      DO:
          CREATE tmpVpiStrekkode.
          ASSIGN
              tmpVpiStrekkode.EkstVpiLevNr      = VPIFilHode.EkstVpiLevNr
              tmpVpiStrekkode.VareNr            = STRING(ttPriKat.ArtikkelNr) 
              tmpVpiStrekkode.Kode              = ttPriKat.EANnr
              tmpVPIStrekkode.Bestillingsnummer = TRIM(ttPriKat.LevKod)
              tmpVPIStrekkode.ERPNr             = (IF ttPriKat.ERPNr <> '' THEN ttPriKat.ERPNr ELSE tmpVPIStrekkode.ERPNr)
              .
      END.
      ASSIGN
          tmpVpiStrekkode.Storl             = ttPriKat.Str
          tmpVpiStrekkode.EkstStorl         = ttPriKat.Str
          tmpVpiStrekkode.StrKode           = ttPriKat.StrKode
          tmpVPIStrekkode.Bestillingsnummer = TRIM(ttPriKat.LevKod)
          tmpVPIStrekkode.ERPNr             = (IF ttPriKat.ERPNr <> '' THEN ttPriKat.ERPNr ELSE tmpVPIStrekkode.ERPNr)
          tmpVpiStrekkode.KodeType          = 1 /* EAN */
          .
          
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



 
&IF DEFINED(EXCLUDE-sendEMail) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendEMail Procedure
PROCEDURE sendEMail:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER icFil AS CHAR NO-UNDO.

FILE-INFO:FILE-NAME = icFil.

/*RUN sendmail_tsl.p ("VPI",                             */
/*                    "Strekkode flyttet " + icFil + '.',*/
/*                    FILE-INFO:FULL-PATHNAME,           */
/*                    "Strekkode flyttet",               */
/*                    "",                                */
/*                    "") NO-ERROR.                      */
                    
rSendEMail:parMailType = 'VPI'.
rSendEMail:parSUBJECT  = 'Strekkode flyttet ' + STRING(NOW) + '.'.
rSendEMail:parMESSAGE  = "Strekkode flyttet " + icFil + '.'.
rSendEMail:parFILE     = FILE-INFO:FULL-PATHNAME.  
obOk = rSendEMail:send( ).
                    
IF ERROR-STATUS:ERROR THEN 
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



&IF DEFINED(EXCLUDE-SettArtikkelNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettArtikkelNr Procedure 
PROCEDURE SettArtikkelNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop       AS INT  NO-UNDO.
DEF VAR oldplArtikkelNr AS DEC  NO-UNDO.
DEF VAR pcRecidLst   AS CHAR NO-UNDO.
DEF VAR p2cEANnrLst  AS CHAR NO-UNDO.
DEFINE VARIABLE plDec AS DECIMAL NO-UNDO.

ASSIGN
    oldplArtikkelNr = 0
    pcEANnrLst      = ""
    pcBestNrLst     = ""
    p2cEANnrLst     = "" /* Brukes på artikkel/størrelse */
    pcStrLst        = ""
    p2lArtikkelNr   = 0
    pcRecidLst      = ""
    piantall        = 0. 

/*    
    INDEX SettArtikkelNr
        LevModellNr
        VareTekst
        FargeTekst
        Markedspris
        ArtikkelNr
*/    
/* Behandler fillinjene og lager modell der hvor vi klarer det. */
VPIFILLINJE:
FOR EACH ttPriKat WHERE 
    ttPriKat.ArtikkelNr   = 0 AND  
    ttPriKat.LevModellNr >= '' AND 
    ttPriKat.VareTekst   >= '' AND 
    ttPriKat.FargeTekst  >= '' AND
    ttPriKat.AntIEnh     >= '' AND 
    ttPriKat.Markedspris >= ''    
    USE-INDEX SettArtikkelNr TRANSACTION
    BREAK 
    BY ttPriKat.ArtikkelNr
    BY ttPriKat.LevModellNr  /* Modell                                        */
    BY ttPriKat.VareTekst    /* Varetekst                                     */
    BY ttPriKat.FargeTekst   /* LevFargeKode                                  */
    BY ttPriKat.AntIEnh      /* Antall salgsenheter i leverandørsforpakkning. */
    BY ttPriKat.Markedspris  /* Unik artikkel når pris varierer.              */:

    ASSIGN
        piantall = piAntall + 1
        .
    /* Ny artikkel nullstilles variablene. */
    IF FIRST-OF(ttPriKat.Markedspris) THEN
        ASSIGN
        pcEANnrLst  = ""
        p2cEANnrLst = ""
        pcStrLst    = ""
        pcRecidLst  = ""
        pcBestNrLst = ""
        .

    /* Logger recid til alle EANnr (Pricat linjer) i artikkelen */
    ASSIGN
        pcRecidLst = pcRecidLst + 
                      (IF pcRecidLst = ""
                         THEN ""
                         ELSE CHR(1)) + 
                      string(RECID(ttPriKat)).
                      
    /* Logger EANnr i artikkel    */
    IF (ttPriKat.EANnr <> "" OR ttPriKat.LevKod <> "") THEN
    ASSIGN
        pcEANnrLst = pcEANnrLst + 
                      (IF pcEANnrLst = ""
                         THEN ""
                         ELSE CHR(1)) + 
                      ttPriKat.EANnr
        pcBestNrLst = pcBestNrLst + 
                      (IF pcBestNrLst = ""
                         THEN ""
                         ELSE CHR(1)) + 
                      ttPriKat.LevKod
        p2cEANnrLst = p2cEANnrLst + 
                      (IF p2cEANnrLst = ""
                         THEN ""
                         ELSE CHR(1)) + 
                      ttPriKat.EANnr
                      .

    /* Siste posten i en artikkel.                             */
    /* Hvis vi klarer å sette sammen en artikkel, gjør vi det. */
    IF LAST-OF(ttPriKat.Markedspris) THEN
    BRYTGRYUPPE:
    DO: 
        STATUS DEFAULT "Setter artikkelnr. - på fillinje: " + STRING(piantall) + ". (Totalt antall: " + STRING(iTotAntLinjer) + ")".
        PUBLISH 'visStatusMsg' ("Setter artikkelnr. - på fillinje: " + STRING(piantall) + ". (Totalt antall: " + STRING(iTotAntLinjer) + ")").    

        p2lArtikkelNr = 0.
        
        /* Ligger EAN koden på en slaskartikkel. Skal den frigjøres. */
        /* Slaskartikler kommer opprinneling fra Preem.              */
        IF bSjekkSlask THEN 
        DO:
          STREKKODE:
          DO piLoop = 1 TO NUM-ENTRIES(pcEANnrLst,CHR(1)):
            IF ENTRY(piLoop,pcEANnrLst,CHR(1)) = "" THEN
                NEXT STREKKODE.
            /* Sjekker med nullutfylling */
            FIND Strekkode NO-LOCK WHERE
                Strekkode.Kode = ENTRY(piLoop,pcEANnrLst,CHR(1)) NO-ERROR.
            /* Sjekker uten nullutfylling. */
            IF NOT AVAILABLE Strekkode THEN
              FIND Strekkode NO-LOCK WHERE 
                Strekkode.Kode = LEFT-TRIM(ENTRY(piLoop,pcEANnrLst,CHR(1)),'0') NO-ERROR.
            IF AVAILABLE Strekkode THEN
            DO:
               IF Strekkode.ArtikkelNr >= lArtFra AND Strekkode.ArtikkelNr <= lArtTil THEN 
               DO:
                  FIND bStrekkode WHERE RECID(bStrekkode) = RECID(Strekkode) EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAILABLE bStrekkode THEN DELETE bStrekkode.
               END.
            END.
          END. /* STREKKODE */
        END. 

        /* Leverandørsforpakkning.                                                   */
        /* Ligger EAN koden på en artikkel med annet antall i leverandørsforpakning, */
        /* skal EAN koden frigjøres slik at det kan dannes ny artikkel.              */
        IF bSjekkAntIPkn AND INT(ttPriKat.AntIEnh) > 0 THEN 
        DO:
          STREKKODE:
          DO piLoop = 1 TO NUM-ENTRIES(pcEANnrLst,CHR(1)):
            IF ENTRY(piLoop,pcEANnrLst,CHR(1)) = "" THEN
                NEXT STREKKODE.

            FIND Strekkode NO-LOCK WHERE
                Strekkode.Kode = ENTRY(piLoop,pcEANnrLst,CHR(1)) NO-ERROR.
            IF NOT AVAILABLE Strekkode THEN
              FIND Strekkode NO-LOCK WHERE 
                Strekkode.Kode = LEFT-TRIM(ENTRY(piLoop,pcEANnrLst,CHR(1)),'0') NO-ERROR.
            IF AVAILABLE Strekkode THEN
            FUNNET_STREKKODE:
            DO:
              FIND ArtBas NO-LOCK OF Strekkode NO-ERROR.
              /* ----
              IF AVAILABLE ArtBas AND ArtBas.AntIPakn <> INT(ttPriKat.AntIEnh) THEN 
              DO:
                  FIND bStrekkode WHERE RECID(bStrekkode) = RECID(Strekkode) EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAILABLE bStrekkode THEN DELETE bStrekkode.
              END.
              ------- */
              /* Har den nye artikkelen lik eller lavere antipkn, skal den skrive over den som ligger der fra før. */
              IF AVAILABLE ArtBas AND  INT(ttPriKat.AntIEnh) <= ArtBas.AntIPakn THEN 
              DO:
                  p2lArtikkelNr = ArtBas.ArtikkelNr.
                  LEAVE STREKKODE.
              END.
              /* Hvis ikke, skal ikke artikkelen leses inn. Det gjøres ved ikke å tildele den artikkelnr. */
              ELSE SLETTES: DO:
                LEAVE BRYTGRYUPPE.
              END. /* SLETTES*/
            END. /* FUNNET_STREKKODE*/
          END. /* STREKKODE */
        END. 

        /* TN 28/5-06 Åpnet opp for sjekk mot eksisterende artikler.                  */
        /* Sjekker om artikkelen finnes fra før. Sjekker alle ean koder på varianten. */
        /* Kobler mot den første vi finner.                                           */
        /* Sjekker også om strekkoden ligger uten ledende nuller. Gjør den det skal   */
        /* Strekkoden legges inn uten ledende nuller.                                 */
        STREKKODE:
        DO piLoop = 1 TO NUM-ENTRIES(pcEANnrLst,CHR(1)):
            IF ENTRY(piLoop,pcEANnrLst,CHR(1)) = "" THEN
                NEXT STREKKODE.

            FIND Strekkode NO-LOCK WHERE
                Strekkode.Kode = ENTRY(piLoop,pcEANnrLst,CHR(1)) NO-ERROR.
            IF NOT AVAILABLE Strekkode THEN
              FIND Strekkode NO-LOCK WHERE 
                Strekkode.Kode = LEFT-TRIM(ENTRY(piLoop,pcEANnrLst,CHR(1)),'0') NO-ERROR.
            /* Tar artikkelnr. fra den første strekkoden vi finner en artikkel på. */
            IF AVAILABLE Strekkode THEN 
            DO:
                p2lArtikkelNr = Strekkode.ArtikkelNr.
                LEAVE STREKKODE.
            END.
        END. /* STREKKODE */
        
        /* TN 28/5-06 Åpnet opp for sjekk mot eksisterende artikler. */
        /* Sjekker VPI register for aktuell leverandør */
        IF p2lArtikkelNr = 0 THEN 
        VPISTREKKODE:
        DO piLoop = 1 TO NUM-ENTRIES(pcEANnrLst,CHR(1)):
            IF ENTRY(piLoop,pcEANnrLst,CHR(1)) = "" THEN
                NEXT VPISTREKKODE.
            /* Sjkker mot denne VPI leverandørs register. */
            FIND FIRST VPIStrekkode NO-LOCK WHERE
                VPIStrekkode.EkstVPILevNr = ttPriKat.EkstVPILevNr AND
                VPIStrekkode.Kode = ENTRY(piLoop,pcEANnrLst,CHR(1)) NO-ERROR.
            IF NOT AVAILABLE VPIStrekkode THEN 
              FIND FIRST VPIStrekkode NO-LOCK WHERE
                  VPIStrekkode.EkstVPILevNr = ttPriKat.EkstVPILevNr AND
                  VPIStrekkode.Kode = LEFT-TRIM(ENTRY(piLoop,pcEANnrLst,CHR(1)),'0') NO-ERROR.
            IF AVAILABLE VPIStrekkode THEN
            DO:
                p2lArtikkelNr = dec(VPIStrekkode.VareNr).
                LEAVE VPISTREKKODE.
            END.            
            /* Sjekker på tvers av VPI leverandørene. */
            IF NOT AVAILABLE VPIStrekkode THEN 
            FIND FIRST VPIStrekkode NO-LOCK WHERE
                VPIStrekkode.Kode = ENTRY(piLoop,pcEANnrLst,CHR(1)) NO-ERROR.
            IF AVAILABLE VPIStrekkode THEN
            DO:
                p2lArtikkelNr = dec(VPIStrekkode.VareNr).
                LEAVE VPISTREKKODE.
            END.            
        END. /* VPISTREKKODE */

        /* Sjekker mot bestillingsnr etter at det sjekkes mot strekkode.   */
        /* Ref. Time og Preem. Nye EAN koder skal komme som 'Tandem'.      */
        /* Dette skal normalt bare gjøres for servicehandel ikke           */
        /* for andre bransjer.                                             */
        IF bSjekkBestillingsnr AND ttPriKat.iLevNr > 0 THEN 
        BESTILLINGSNR:
        DO piLoop = 1 TO NUM-ENTRIES(pcBestNrLst,CHR(1)):
            IF ENTRY(piLoop,pcBestNrLst,CHR(1)) = "" THEN
                NEXT BESTILLINGSNR.
            FOR EACH Strekkode NO-LOCK WHERE
                Strekkode.Bestillingsnummer = ENTRY(piLoop,pcBestNrLst,CHR(1)):                
                FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
                IF AVAILABLE ArtBas AND ArtBas.LevNr = ttPriKat.iLevNr THEN
                DO: 
                    p2lArtikkelNr = Strekkode.ArtikkelNr.
                    LEAVE BESTILLINGSNR.
                END.
            END.
        END. /* BESTILLINGSNR */

        IF bSjekkLevKodBeskrFargKod THEN
        LOKALSJEKK: 
        DO:
          /* Oppslag mot lokalt artikkelregister. */
          IF p2lArtikkelNr = 0 THEN
          ARTBASVARETEKST:
          DO:        
              FIND FIRST ArtBas EXCLUSIVE-LOCK WHERE
                  ArtBas.LevKod       = ttPriKat.LevModellNr AND
                  ArtBas.Beskr        = ttPriKat.VareTekst   AND
                  ArtBas.LevFargKod   = ttPriKat.FargeTekst USE-INDEX SetArtikkelNrFraVPI NO-ERROR.
              IF AVAILABLE ArtBas THEN
                  p2lArtikkelNr = dec(ArtBas.ArtikkelNr).
          END. /* ARTBASVARETEKST */
          
          /* Oppslag mot VPI register. */
          IF p2lArtikkelNr = 0 THEN
          VPIVARETEKST:
          DO:
              FIND FIRST VPIArtBas EXCLUSIVE-LOCK WHERE
                  VPIArtBas.EkstVPILevNr = ttPriKat.EkstVPILevNr AND
                  VPIArtBas.LevKod       = ttPriKat.LevModellNr AND
                  VPIArtBas.Beskr        = ttPriKat.VareTekst   AND
                  VPIArtBas.LevFargKod   = ttPriKat.FargeTekst USE-INDEX SettArtikkelNrFraVPI NO-ERROR.
              IF AVAILABLE VPIArtBas THEN
                  p2lArtikkelNr = dec(VPIArtBas.VareNr).
          END. /* VPIVARETEKST */
        END. /* LOKALSJEKK */
        
        /* TN 11/12-06 Finnes noen av strekkodene på en annen artikkel     */
        /* Skal disse slettes. Det er ny VPI som leses inn som skal vinne. */
        /* Dvs. alle strekkodene skal bli hengende på den nye artikkelen.  */
        IF p2lArtikkelNr > 0 AND p2cEANnrLst <> "" THEN
        SLETTFEILKOBLETSTREKKODE:
        DO:
          VPI2STREKKODE:
          DO piLoop = 1 TO NUM-ENTRIES(p2cEANnrLst,CHR(1)):
              IF ENTRY(piLoop,p2cEANnrLst,CHR(1)) = "" THEN
                  NEXT VPI2STREKKODE.         
              FOR EACH VPIStrekkode EXCLUSIVE-LOCK WHERE
                  VPIStrekkode.EkstVPILevNr = ttPriKat.EkstVPILevNr AND
                  VPIStrekkode.Kode = ENTRY(piLoop,p2cEANnrLst,CHR(1)):
                IF (dec(VPIStrekkode.VareNr) <> p2lArtikkelNr) THEN
                DO:
                  /* DØDEN - Fjerner strekkoden fra den gaml artikkelen. */
                  DELETE VPIStrekkode.
                END.
              END.
          END. /* VPI2STREKKODE */
        END. /* SLETTFEILKOBLETSTREKKODE */

        /* Her settes artikkelens artikkelnr hvis den fortsatt ikke er satt etter alle kontrollene. */
        IF p2lArtikkelNr = 0 THEN
        NYTT-ARTIKKELNR:
        DO:
            /* EAN kode skal benyttes som artikkelNr på nye artikler */
            IF bBrukEanTilArtikkelNr AND TRIM(ttPriKat.EANnr) <> '' AND DECIMAL(ttPriKat.EANnr) > 999999 THEN 
            DO:
              IF NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = dec(ttPriKat.EANnr)) THEN 
                p2lArtikkelNr = dec(ttPriKat.EANnr).
            END.

            /* I servicehandelen er det ønskelig at artikkelnr er sammensatt av   */
            /* leverandørnr. og bestillingsnr.                                    */
            IF (p2lArtikkelNr = 0 AND bPreemArtikkelNr) THEN 
            DO:
              FIND LevBas NO-LOCK WHERE LevBas.LevNr = INTEGER(ttPriKat.LevNr) NO-ERROR.
              IF AVAILABLE LevBas AND ttPriKat.ERPNr <> '' THEN 
              DO:
                ASSIGN plDec = DECIMAL(TRIM(TRIM(STRING(ttPriKat.LevNr,'>>9999')),'>')) * 1000000 + DECIMAL(TRIM(TRIM(STRING(ttPriKat.LevKod,'>999999')),'>')) NO-ERROR.            
                IF NOT ERROR-STATUS:ERROR AND 
                   NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = plDec) THEN 
                  ASSIGN p2lArtikkelNr = plDec NO-ERROR.
              END.
            END. 

            /* PRS standard artikkelNr */
            IF p2lArtikkelNr = 0 THEN 
            DO:
              p2lArtikkelNr = oldplArtikkelNr + 1.
              RUN genVpiArtikkelnr.p (INPUT-OUTPUT p2lArtikkelNr).             
              oldplArtikkelNr = p2lArtikkelNr.
            END.
            
            /* Full nummerserie */
            IF p2lArtikkelNr = 0 THEN
            DO:
                CREATE tt_Error.
                ASSIGN
                    ttPriKat.ErrFlag = TRUE
                    tt_Error.LinjeNr = ttPriKat.LinjeNr
                    tt_Error.Tekst   = "* ArtikkelNr: VPI nummerserie ( Syspara 1 1 26) er full."
                    .
            END.
        END. /* NYTT-ARTIKKELNR */

        /* Setter artikkelnummeret på alle størrelser for artikkelen. */
        IF p2lArtikkelNr > 0 THEN
        DO:
            ASSIGN
                ttPriKat.ArtikkelNr = p2lArtikkelNr
                .
            SETT-ARTIKKELNR:
            DO piLoop = 1 TO NUM-ENTRIES(pcRecidLst,CHR(1)):
                FOR EACH buf-ttPriKat EXCLUSIVE-LOCK WHERE
                    buf-ttPriKat.EkstVPILevNr = VPIFilHode.EkstVPILevNr AND
                    RECID(buf-ttPriKat)       = int(ENTRY(piLoop,pcRecidLst,CHR(1))):

                    /* Setter på artikkelNr. */
                    /* TN 17/7-09 Dette skal ikke gjøres på de linjer hvor et ERPnr er lest inn og lagt i ArtikkelNr feltet. */
                    IF buf-ttPriKat.ERPNr = '' THEN
                      ASSIGN
                        buf-ttPriKat.ArtikkelNr = p2lArtikkelNr.
                    /* Her er det ERPnr som er satt inn i artikkelnr som skal gjelde. Dvs. ikke overskrives. */
                    /* P.t. gjelder dette bare Preem og RIGAL innlesning (xri1viinnles.p).                   */
                    /* Her skal EAN koden ligge på den artikkel den er innlest på. Dvs den flyttes fra       */
                    /* den artikkel den ligger på fra før, hvis den er innlest tidligere.                    */
                    ELSE IF buf-ttPriKat.ArtikkelNr = 0 THEN 
                      ASSIGN
                        buf-ttPriKat.ArtikkelNr = p2lArtikkelNr.
                END.
            END. /* SETT-ARTIKKELNR */
            
          /* ----- TN 28/9-18 Mail ved flytting av EAN kode --- */
          cTekst = ''.
          ORGSTREKKODE:
          DO piLoop = 1 TO NUM-ENTRIES(p2cEANnrLst,CHR(1)):
              IF ENTRY(piLoop,p2cEANnrLst,CHR(1)) = "" THEN
                  NEXT ORGSTREKKODE.         
              FOR EACH Strekkode EXCLUSIVE-LOCK WHERE
                  Strekkode.Kode = ENTRY(piLoop,p2cEANnrLst,CHR(1)):
                IF (dec(Strekkode.ArtikkelNr) <> p2lArtikkelNr) THEN
                DO:
                    ASSIGN 
                      cTekst = cTekst +
                               (IF cTekst = '' THEN '' ELSE CHR(10)) +  
                               'Strekkode ' + Strekkode.Kode + ' flyttet fra artikkel ' + STRING(Strekkode.ArtikkelNr) + ' til artikkel ' + STRING(p2lArtikkelNr) + '.'.
                END.
              END.
          END. /* ORGSTREKKODE */
          IF cTekst <> '' THEN 
          DO:
              /* TN og Gurre 28/9-18 Her sendes mail for å varsle om forholdet. */
              cMailFil = 'log\StrekkodeFlyttet' + REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.log'.
              OUTPUT STREAM Mail TO VALUE(cMailFil).
              PUT STREAM Mail UNFORMATTED
                cTekst 
              SKIP.
              OUTPUT STREAM Mail CLOSE.
              IF SEARCH(cMailFil) <> ? THEN 
                  RUN sendEMail(SEARCH(cMailFil)).
          END.          
          /* ------------------------------------------------------ */
        END.
        /* Man havner bare her hvis allt annet har gått feil og artikkelnr fortsatt er 0 */
        ELSE 
        FEIL:
        DO:        
          MESSAGE "xsport1vpiutpakk.p - Klarte ikke å tildele artikkelnr." SKIP
            'ttPriKat.EkstVPILevNr' ttPriKat.EkstVPILevNr SKIP
            'ttPriKat.LevModellNr' ttPriKat.LevModellNr SKIP
            'ttPriKat.Varetekst' ttPriKat.Varetekst SKIP
            'ttPriKat.FargeTekst' ttPriKat.FargeTekst SKIP
            'ttPriKat.ArtikkelNr' ttPriKat.ArtikkelNr SKIP
            'p2lArtikkelNr' p2lArtikkelNr
            VIEW-AS ALERT-BOX.
        
        END. /* FEIL */
    
        ASSIGN p2cEANnrLst = "". /* Nullstilles pr. artikkel */
        
        /* Nye artikler skal opprettes automatisk i artikkelregisteret. */
        IF bOpprettNye THEN 
          ttPriKat.OpprettArtikkel = TRUE.
        ELSE 
          ttPriKat.OpprettArtikkel = FALSE.

    END. /* BRYTGRYUPPE */
    
END. /* VPIFILLINJE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SettModellFarge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettModellFarge Procedure 
PROCEDURE SettModellFarge :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop        AS INT  NO-UNDO.
DEF VAR pcEANnrLst    AS CHAR NO-UNDO.
DEF VAR plArtikkelNr  AS DEC  NO-UNDO.
DEF VAR p2lArtikkelNr AS DEC  NO-UNDO.
DEF VAR pcModell      AS CHAR NO-UNDO.
DEF VAR plHovedModellFarge AS LOG NO-UNDO.

ASSIGN 
  piAntall = 0
  .
/* Behandler fillinjene og lager modell der hvor vi klarer det. */
VPIFILLINJE:
FOR EACH ttPriKat WHERE 
    ttPriKat.LevModellNr >= '' AND 
    ttPriKat.VareTekst   >= '' AND 
    ttPriKat.FargeTekst  >= '' AND 
    ttPriKat.ArtikkelNr  > 0 AND 
    LOOKUP(STRING(ttPriKat.HG),cHgLst) = 0 /* TN 27/8-20 Artikler som ligger i hovedgruppelisten skal ikke settes sammen til modeller. */  
    USE-INDEX ArtikkelNr TRANSACTION
    BREAK 
    BY ttPriKat.LevModellNr  /* Modell    */
    BY ttPriKat.VareTekst    /* Varetekst */
    BY ttPriKat.FargeTekst   
    BY ttPriKat.ArtikkelNr:

    piAntall = piAntall + 1.
    STATUS DEFAULT "Setter ModellFarge - på fillinje: " + STRING(piantall) + ". (Totalt: " + STRING(iTotAntLinjer) + ")".
    PUBLISH 'visStatusMsg' ("Setter ModellFarge - på fillinje: " + STRING(piantall) + ". (Totalt: " + STRING(iTotAntLinjer) + ")").    

    /* Ny modell */
    IF FIRST-OF(ttPriKat.VareTekst) THEN
        ASSIGN
        pcEANnrLst = ""
        pcModell    = ""
        plHovedModellFarge = FALSE
        .

    /* Logger VareNr i modell */
    IF ttPriKat.EANnr <> "" THEN
    ASSIGN
        pcEANnrLst = pcEANnrLst + 
                      (IF pcEANnrLst = ""
                         THEN ""
                         ELSE CHR(1)) + 
                      ttPriKat.EANnr.

    /* Logger fargeliste for modellen */
    IF NOT CAN-DO(pcModell,ttPriKat.FargeTekst) THEN
        ASSIGN
        pcModell = pcModell +
                   (IF pcModell = ""
                      THEN ""
                      ELSE ",") +
                   ttPriKat.FargeTekst
        .

    /* Siste posten i en modell.                             */
    /* Hvis vi klarer å sette sammen en modell, gjør vi det. */
    IF LAST-OF(ttPriKat.VareTekst) THEN
    BRYTGRYUPPE:
    DO piLoop = 1 TO NUM-ENTRIES(pcEANnrLst,CHR(1)):

        IF piLoop = 1 THEN
            plArtikkelNr = 0.

        IF ENTRY(piLoop,pcEANnrLst,CHR(1)) <> "" THEN
        PREPPAVMODELL:
        DO:
            /* Setter inn modellnummer på alle variantene */
            FOR EACH buf-ttPriKat EXCLUSIVE-LOCK WHERE
                buf-ttPriKat.EkstVPILevNr = VPIFilHode.EkstVPILevNr AND
                buf-ttPriKat.EANnr        = ENTRY(piLoop,pcEANnrLst,CHR(1)) USE-INDEX SettModell:
                /* Logger siste brukte artikkelNr */
                IF plArtikkelNr  = 0 THEN 
                    plArtikkelNr  = buf-ttPriKat.ArtikkelNr.


                /* Setter på modellnummer hvis det er flere farger. */
                IF NUM-ENTRIES(pcModell) >= 2 THEN
                ASSIGN
                    buf-ttPriKat.ModellFarge = plArtikkelNr
                    plHovedModellFarge       = TRUE
                    .
                ELSE
                    ASSIGN
                        buf-ttPriKat.ModellFarge = 0
                        .
            END.
            /* Setter inn hovedmodellflagg */
            IF plHovedModellFarge AND plArtikkelNr > 0 THEN
            FOR EACH buf-ttPriKat EXCLUSIVE-LOCK WHERE
                buf-ttPriKat.EkstVPILevNr = VPIFilHode.EkstVPILevNr AND
                buf-ttPriKat.ArtikkelNr   = plArtikkelNr USE-INDEX SettModell2:
                ASSIGN buf-ttPriKat.HovedModellFarge = TRUE.
            END.
        END. /* PREPPAVMODELL */
    END.
END. /* VPIFILLINJE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-stoppVaregruppe) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stoppVaregruppe Procedure
PROCEDURE stoppVaregruppe:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
      IF (TRIM(cSperreListeVPILev) <> '' AND 
          TRIM(cSperreListeVg) <> '') THEN 
          RUN stoppVaregruppe.
	------------------------------------------------------------------------------*/
DEFINE VARIABLE piLoop2 AS INTEGER NO-UNDO.

EKSTVPILEV:
DO piLoop2 = 1 TO NUM-ENTRIES(cSperreListeVPILev):
  PRIKAT_LOOP:
  FOR EACH ttPriKat WHERE
    ttPriKat.EkstVPILevNr = INT(ENTRY(piLoop2,cSperreListeVPILev))
    BY ttPriKat.EkstVPILevNr
    BY ttPriKat.Vg:
    
    IF CAN-DO(cSperreListeVg,STRING(ttPriKat.Vg)) THEN 
    DO:
        CREATE tt_Error.
        ASSIGN
            piAntFeil          = piAntFeil + 1
            ttPriKat.ErrFlag   = TRUE
            tt_Error.LinjeNr   = ttPriKat.LinjeNr
            tt_Error.Tekst     = VareIdent() + "* Sprerret varegruppe: (" + STRING(ttPriKat.Vg) + "). Artikkelen er ikke importert."
            tt_Error.ErrNr     = 1
            tt_Error.ButikkNr  = ttPriKat.ButikkNr
            ttPriKat.BehStatTekst = tt_Error.Tekst
            ttPriKat.behStatus = 20. /* Kontroller */
    
      DELETE ttPriKat.
    END.
    
  END. /* PRIKAT_LOOP */  
END. /* EKSTVPILEV */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-StrekkodeValidering) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StrekkodeValidering Procedure 
PROCEDURE StrekkodeValidering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* DEF VAR pcTekst       AS CHAR NO-UNDO.                                                                                                */
/* DEF VAR pc2Tekst      AS CHAR NO-UNDO.                                                                                                */
/* DEF VAR pc3Tekst      AS CHAR NO-UNDO.                                                                                                */
/* DEF VAR p3lArtikkelNr AS DEC  NO-UNDO.                                                                                                */
/*                                                                                                                                       */
/* IF AVAILABLE Strekkode    THEN RELEASE Strekkode.                                                                                     */
/* IF AVAILABLE VPIStrekkode THEN RELEASE VPIStrekkode.                                                                                  */
/*                                                                                                                                       */
/* IF AVAILABLE ArtBas THEN RELEASE ArtBas.                                                                                              */
/* IF AVAILABLE VPIArtBas THEN RELEASE VPIArtBas.                                                                                        */
/*                                                                                                                                       */
/* /* Sjekker om artikkelen finnes fra før. Sjekker alle ean koder på varianten. */                                                      */
/* /* Kontroll gjøres ved hjelp av strekkode.                                    */                                                      */
/* STREKKODE:                                                                                                                            */
/* DO:                                                                                                                                   */
/*     IF ttPriKat.EANnr = "" THEN                                                                                                       */
/*         LEAVE STREKKODE.                                                                                                              */
/*                                                                                                                                       */
/*     FIND Strekkode NO-LOCK WHERE                                                                                                      */
/*         Strekkode.Kode = ttPriKat.EANnr NO-ERROR.                                                                                     */
/*     IF AVAILABLE Strekkode THEN                                                                                                       */
/*     DO:                                                                                                                               */
/*         p2lArtikkelNr = Strekkode.ArtikkelNr.                                                                                         */
/*         LEAVE STREKKODE.                                                                                                              */
/*     END.                                                                                                                              */
/* END. /* STREKKODE */                                                                                                                  */
/*                                                                                                                                       */
/* /* Sjekker VPI register for aktuell leverandør */                                                                                     */
/* IF p2lArtikkelNr = 0 THEN VPISTREKKODE:                                                                                               */
/* DO:                                                                                                                                   */
/*     IF ttPriKat.EANnr = "" THEN                                                                                                       */
/*         LEAVE VPISTREKKODE.                                                                                                           */
/*                                                                                                                                       */
/*     FIND VPIStrekkode NO-LOCK WHERE                                                                                                   */
/*         VPIStrekkode.EkstVPILevNr = ttPriKat.EkstVPILevNr AND                                                                         */
/*         VPIStrekkode.Kode = ttPriKat.EANnr NO-ERROR.                                                                                  */
/*     IF AVAILABLE VPIStrekkode THEN                                                                                                    */
/*     DO:                                                                                                                               */
/*         p2lArtikkelNr = dec(VPIStrekkode.VareNr).                                                                                     */
/*         LEAVE VPISTREKKODE.                                                                                                           */
/*     END.                                                                                                                              */
/* END. /* VPISTREKKODE */                                                                                                               */
/*                                                                                                                                       */
/* /* Kontroll på at EAN kode peker på samme artikkel. */                                                                                */
/* IF AVAILABLE VPIStrekkode THEN                                                                                                        */
/* DO:                                                                                                                                   */
/*     FIND VPIArtBas OF VPIStrekkode NO-ERROR.                                                                                          */
/*     IF AVAILABLE VPIArtBAs THEN                                                                                                       */
/*         ASSIGN                                                                                                                        */
/*         pcTekst  = trim(VPIArtBas.LevKod) + "+" +                                                                                     */
/*                    trim(VPIArtBas.Beskr)   + "+" +                                                                                    */
/*                    trim(VPIArtBas.LevFargKod)                                                                                         */
/*         .                                                                                                                             */
/*     ELSE                                                                                                                              */
/*         pcTekst = "".                                                                                                                 */
/* END.                                                                                                                                  */
/* ELSE IF AVAILABLE Strekkode THEN                                                                                                      */
/* DO:                                                                                                                                   */
/*     FIND ArtBas OF Strekkode NO-ERROR.                                                                                                */
/*     IF AVAILABLE ArtBAs THEN                                                                                                          */
/*         ASSIGN                                                                                                                        */
/*         pcTekst = trim(ArtBas.LevKod) +  "+" +                                                                                        */
/*                   trim(ArtBas.Beskr)   +  "+" +                                                                                       */
/*                   trim(ArtBas.LevFargKod)                                                                                             */
/*         .                                                                                                                             */
/*     ELSE                                                                                                                              */
/*         pcTekst = "".                                                                                                                 */
/* END.                                                                                                                                  */
/*                                                                                                                                       */
/* /* Bygger kontrollstreng */                                                                                                           */
/* ASSIGN                                                                                                                                */
/*     pc2Tekst = trim(ttPriKat.LevModellNr) +  "+" +                                                                                    */
/*                trim(ttPriKat.VareTekst)   +  "+" +                                                                                    */
/*                trim(ttPriKat.FargeTekst)                                                                                              */
/*     /* Denne skal sjekke mot blank lev.argekode. */                                                                                   */
/*     pc3Tekst = trim(ttPriKat.LevModellNr) +  "+" +                                                                                    */
/*                trim(ttPriKat.VareTekst)   +  "+"                                                                                      */
/*     .                                                                                                                                 */
/*                                                                                                                                       */
/* /* Kontroll av EAN kode kobling mot tidligere innleste artikler. */                                                                   */
/* IF pcTekst <> "" AND (pcTekst <> pc2Tekst) THEN                                                                                       */
/* DO:                                                                                                                                   */
/*     /* Er det bare fargen som avviker, retter vi opp avviket. */                                                                      */
/*     /* Her kommer artikkelen inn påny, men denne gangen har   */                                                                      */
/*     /* leverandøren levert med fargekoden.                    */                                                                      */
/*     IF pcTekst = pc3Tekst THEN                                                                                                        */
/*     DO:                                                                                                                               */
/*         DO TRANSACTION:                                                                                                               */
/*             IF AVAILABLE ArtBas THEN                                                                                                  */
/*                 FIND CURRENT ArtBas EXCLUSIVE-LOCK NO-ERROR.                                                                          */
/*             ELSE IF AVAILABLE VPIArtBas THEN                                                                                          */
/*                 FIND CURRENT VPIArtBAs EXCLUSIVE-LOCK NO-ERROR.                                                                       */
/*             IF AVAILABLE ArtBas THEN                                                                                                  */
/*                 ArtBas.LevFargKod = ttPriKat.FargeTekst.                                                                              */
/*             ELSE IF AVAILABLE VPIArtBAs THEN                                                                                          */
/*                 VPIArtBas.LevFargKod = ttPriKat.FargeTekst.                                                                           */
/*             IF AVAILABLE ArtBas THEN                                                                                                  */
/*                 FIND CURRENT ArtBas NO-LOCK NO-ERROR.                                                                                 */
/*             ELSE IF AVAILABLE VPIArtBas THEN                                                                                          */
/*                 FIND CURRENT VPIArtBAs NO-LOCK NO-ERROR.                                                                              */
/*         END.                                                                                                                          */
/*         CREATE tt_Error.                                                                                                              */
/*         ASSIGN                                                                                                                        */
/*             ttPriKat.ErrFlag   = TRUE                                                                                                 */
/*             tt_Error.LinjeNr   = ttPriKat.LinjeNr                                                                                     */
/*             tt_Error.ErrNr = 11                                                                                                   */
/*             tt_Error.Tekst     = "* Korrigert lev.fargekode: " + ttPriKat.EANNr + " Ny: '" + pc2Tekst + "' Gammel: '" + pcTekst + "'" */
/*             ttPriKat.EANnr     = ""                                                                                                   */
/*             .                                                                                                                         */
/*     END.                                                                                                                              */
/*     /* Feilkobling. */                                                                                                                */
/*     ELSE DO:                                                                                                                          */
/*         CREATE tt_Error.                                                                                                              */
/*         ASSIGN                                                                                                                        */
/*             ttPriKat.ErrFlag   = TRUE                                                                                                 */
/*             tt_Error.LinjeNr   = ttPriKat.LinjeNr                                                                                     */
/*             tt_Error.ErrNr = 10                                                                                                   */
/*             tt_Error.Tekst     = "* Feilkoble EAN kode: " + ttPriKat.EANNr + " Ny: '" + pc2Tekst + "' Gammel: '" + pcTekst + "'"      */
/*             ttPriKat.EANnr     = ""                                                                                                   */
/*             .                                                                                                                         */
/*     END.                                                                                                                              */
/* END.                                                                                                                                  */
/*                                                                                                                                       */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UtPakkVpi) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtPakkVpi Procedure 
PROCEDURE UtPakkVpi :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER bMoms FOR Moms.
DEF BUFFER bArtBas FOR ArtBas.
DEF VAR piLoop   AS INT  NO-UNDO.
DEF VAR pcTekst  AS CHAR NO-UNDO.
DEF VAR cStrek   AS CHAR NO-UNDO.
DEFINE VARIABLE ihHandle AS HANDLE NO-UNDO.

ASSIGN
    piantall = 0
    piLoop1 = 0
    piTid   = TIME
    cTekst  = "Starter utpakking av VPI.".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

/* Aldri tilbud */
ASSIGN
    piTilbud = 1
    pbStatus = FALSE
    .

/* Sikrer at tmp filen er tømt. */
FOR EACH tmpVPIStrekkode:
    DELETE tmpVPIStrekkode.
END.

/* Behandler linjene i filen.                                                   */
/* Alle linjer som hører til samme artikkel, har samme artikkelnr (Størrelser). */
/* Løkken under tar derfor for seg en og en artikkel og dens ulike farger.      */
VPIFILLINJE:
FOR EACH ttPriKat WHERE
    ttPriKat.ArtikkelNr > 0 AND 
    ttPriKat.Fargetekst >= ''
    BREAK 
    BY ttPriKat.ArtikkelNr
    BY ttPriKat.FargeTekst TRANSACTION:

    piAntall = piAntall + 1.
    STATUS DEFAULT "Pakker ut VPI - fra fillinje: " + STRING(piAntall) + ". (Totalt antall: " + STRING(iTotAntLinjer) + ")".
    PUBLISH 'visStatusMsg' ("Pakker ut VPI - fra fillinje: " + STRING(piAntall) + ". (Totalt antall: " + STRING(iTotAntLinjer) + ")").    

    ASSIGN
        dcValPris      = 0
        dcInnPris      = 0  
        dcUtpris       = 0  
        piSasong       = 0
        cEndelse       = ""
        piLoop1        = piLoop1 + 1
        .

    /* Sikker konvertering eller 0 i kode. */
    ASSIGN
        piSasong = INT(ttPriKat.Sesong) 
        NO-ERROR.

    /* Setter default vareområde på nye artikler. */
    IF TRIM(cDefRavdNr) <> '' AND 
       ttPriKat.RAvdNr = '' AND 
       NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = ttPriKat.ArtikkelNr) THEN 
      ttPriKat.RAvdNr = cDefRavdNr.

    /* Håndterer Strekkodene */
    IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - UtpakkVPI: Start logg strekkoder. Artikkel: ' +  STRING(ttPriKat.ArtikkelNr) + ' ' + ttPriKat.Varetekst).
    RUN LogStrekkoder.
    IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - UtpakkVPI: Stopp logg strekkoder. Artikkel: ' +  STRING(ttPriKat.ArtikkelNr) + ' ' + ttPriKat.Varetekst).

    /* Legger opp den siste posten i brytgruppen. */
    /* Behandler en artikkel i en farge.          */
    IF LAST-OF(ttPriKat.FargeTekst) THEN
    BRYTGRUPPE-FIRST:
    DO:
        ASSIGN plArtikkelNr = ttPriKat.ArtikkelNr.
        IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - UtpakkVPI: BRYTGRUPPE-FIRST Før assign .... Artikkel: ' +  STRING(ttPriKat.ArtikkelNr) + ' ' + ttPriKat.Varetekst).

        /* Henter/oppretter artikkelen. */
        FIND FIRST VPIArtBas EXCLUSIVE-LOCK WHERE
            VPIArtBas.EkstVpiLevNr  = VPIFilHode.EkstVpiLevNr AND
            VPIArtBas.VareNr        = string(ttPriKat.ArtikkelNr) NO-ERROR.
        IF NOT AVAILABLE VPIArtBas THEN
        DO:
            CREATE VPIArtBas.
            ASSIGN
                VPIArtBas.EkstVPILevNr = VPIFilHode.EkstVpiLevNr
                VPIArtBas.VareNr       = STRING(ttPriKat.ArtikkelNr)
                VPIArtBas.ArtikkelNr   = ttPriKat.ArtikkelNr /*plArtikkelNr*/
                VPIArtBas.LokPris      = TRUE /* Nye artikler skal ha dette flagget satt. */
                VPIArtBas.BehStatus    = 0
                NO-ERROR.
        END.
        ELSE
            ASSIGN
                VPIArtBas.BehStatus = 0
                piAntOppdat         = piAntOppdat + 1.

        ASSIGN
            rArtBasRecid           = RECID(VPIArtBas)
            VPIArtBas.ArtSlag      = 0 /* Stykkvare */
            VPIArtBas.Vg           = ttPriKat.Vg
            VPIArtBas.Hg           = ttPriKat.Hg
            VPIArtBas.VgKat        = 1
            VPIArtBas.LopNr        = IF ttPriKat.LopNr <> '' AND ttPriKat.LopNr <> '0' 
                                       THEN INT(ttPriKat.LopNr)
                                       ELSE ?
            VPIArtBas.Storrelser   = TRUE
            VPIArtBas.Beskr        = ttPriKat.Varetekst
            VPIArtBas.BongTekst    = ttPriKat.Varetekst  
            VPIArtBas.LevKod       = ttPriKat.LevModellNr   
            VPIArtBas.LevNr        = ttPriKat.iLevNr
            VPIArtBas.ProdNr       = (IF ttPriKat.iProdNr > 0 THEN ttPriKat.iProdNr ELSE VPIArtBas.ProdNr)      
            VPIArtBas.Notat        = "Fargekode/tekst:" + ttPriKat.Fargekode + "/" + ttPriKat.FargeTekst + chr(10) + 
                                     (IF piVmId = 0
                                        THEN CHR(10) + "Varemerke: " + ttPriKat.Varemerke
                                        ELSE "") +
                                     (IF ttPriKat.Merknad <> "" THEN
                                        CHR(10) + "Merknad: " + ttPriKat.Merknad
                                        ELSE "")  
            VPIArtBas.Farg         = ttPriKat.Farg
            VPIArtBas.StrTypeId    = ttPriKat.StrTypeId
            VPIArtBas.StrTypeId    = IF ttPriKat.StrTypeId = 1 /* 0 skal tillates her. Strtype opprettes senere */
                                       THEN piStrTypeId /* Setter inn default som er 2. */
                                       ELSE ttPriKat.StrTypeId /* Beholder det som er lagt inn i filen */
            VPIArtBas.Sasong       = ttPriKat.Sasong
            VPIArtBas.SaSong       = IF VPIArtBas.SaSong = 0
                                       THEN piSasong
                                       ELSE VPIArtBas.Sasong
            VPIArtBas.ValKod       = ttPriKat.Valkod
            VPIArtBas.LevFargKod   = ttPriKat.FargeTekst
            VPIArtBas.VmId         = ttPriKat.VmId
            
            VPIArtBas.AnonseArtikkel = (IF ttPriKat.Sortiment = "1"
                                          THEN TRUE
                                          ELSE FALSE)
            VPIArtBas.LinjeMerknad = ttPriKat.Merknad + 
                                     (IF ttPriKat.Merknad <> ""
                                        THEN CHR(13)
                                        ELSE "")  
            VPIArtBas.ManRabIKas      = TRUE
            VPIArtBas.ModellFarge     = ttPriKat.ModellFarge
            VPIArtBas.HovedModellFarge = ttPriKat.HovedModellFarge
            VPIArtBas.VpiDato         = TODAY
            VPIArtBas.VPIBildeKode    = ttPriKat.VPIBildeKode
            VPIArtBas.AntIPkn         = INTEGER(ttPriKat.AntIEnh)
            VPIArtBas.KjedeProdusent  = IF ttPriKat.KjedeProdusent  <> '' THEN ttPriKat.KjedeProdusent  ELSE VPIArtBas.KjedeProdusent
            VPIArtBas.Mengde          = DECIMAL(ttPriKat.Mengde)                    
            VPIArtBas.JamforEnhet     = ttPriKat.JamforEnhet
            VPIArtBas.SalgsEnhetsType = 0 /* Feltet er ikke lenger i bruk. */
            VPIArtBas.LinkVareNr      = DECIMAL(ttPriKat.LinkVare)
            /* Sport1 utvidelse */    
            VPIArtBas.KjedeSupRab%    = ttPriKat.KjedeSupRab%     
            VPIArtBas.EkstStrTypeNavn = ttPriKat.EkstStrTypeNavn         
            VPIArtBas.KjedeSupInnkPris = ttPriKat.KjedeSupInnkPris 
            VPIArtBas.Etikett         = ttPriKat.Etikett
            VPIArtBas.Lager           = IF bLagerstyrt THEN TRUE ELSE ttPriKat.Lager
            VPIArtBas.Etikettekst1    = ttPriKat.Etikettekst1    
            VPIArtBas.Etikettekst2    = ttPriKat.Etikettekst2    
            VPIArtBas.ProdNr          = ttPriKat.iProdNr      
            VPIArtBas.Sortimentkoder  = ttPriKat.Sortimentkoder
            VPIArtBas.Lagerkoder      = ttPriKat.Lagerkoder        
            VPIArtBas.Gjennomfaktureres = ttPriKat.Gjennomfaktureres 
            VPIArtBas.KjedeVare       = ttPriKat.KjedeVare         
            VPIArtBas.Kampanjeuker    = ttPriKat.Kampanjeuker                         
            VPIArtBas.Kampanjestotte  = ttPriKat.Kampanjestotte
            VPIArtBas.BehStatus       = IF ttPriKat.BehStatus = 0 THEN 1 ELSE ttPriKat.BehStatus /* Ubehandlet */  
            VPIArtBas.Grunnsortiment  = IF CAN-DO('1,J,ja,Y,YES,TRUE',ttPriKat.Grunnsortiment) THEN TRUE ELSE FALSE
            VPIArtBas.RAvdNr          = (IF INT(ttPriKat.RAvdNr) > 0 THEN INT(ttPriKat.RAvdNr) ELSE VPIArtBas.RAvdNr)  
            VPIArtBas.Salgsenhet      = ttPriKat.Enh        
            VPIArtBas.ArtSlag         = IF ttPriKat.ArtSLag <> '' THEN INT(ttPriKat.ArtSlag) ELSE VPIArtBas.ArtSLag 
            VPIArtBas.OPris           = IF CAN-DO('1,J,ja,Y,YES,TRUE',ttPriKat.OPris) THEN TRUE ELSE FALSE
            VPIArtBas.NON_Sale        = IF CAN-DO('1,J,ja,Y,YES,TRUE',ttPriKat.NON_Sale) THEN TRUE ELSE FALSE 
            VPIArtBas.NegVare         = IF CAN-DO('1,J,ja,Y,YES,TRUE',ttPriKat.NegVare) THEN TRUE ELSE FALSE 
            VPIArtBas.Pant            = IF CAN-DO('1,J,ja,Y,YES,TRUE',ttPriKat.Pant) THEN TRUE ELSE FALSE
            VPIArtBas.Telefonkort     = IF CAN-DO('1,J,ja,Y,YES,TRUE',ttPriKat.Telefonkort) THEN TRUE ELSE FALSE
            VPIArtBas.WebButikkArtikkel = IF CAN-DO('1,J,ja,Y,YES,TRUE',ttPriKat.WebButikkArtikkel) THEN TRUE ELSE FALSE
            VPIArtBas.PubliserINettbutikk = IF CAN-DO('1,J,ja,Y,YES,TRUE',ttPriKat.PubliserINettbutikk) THEN TRUE ELSE FALSE 
            VPIArtBas.HoyLav          = IF CAN-DO('1,J,ja,Y,YES,TRUE',ttPriKat.HoyLav) THEN TRUE ELSE FALSE        
            VPIArtBas.WebLeveringstid = INT(ttPriKat.WebLeveringstid)   
            VPIArtBas.WebMinLager     = DEC(ttPriKat.WebMinLager)   
            VPIArtBas.KampanjeKode    = ttPriKat.KampanjeKode  
            VPIArtBas.AlfaKode2       = ttPriKat.AlfaKode2   
            VPIArtBas.GarantiKl       = INT(ttPriKat.GarantiKl)
            VPIArtBas.PostBredde      = DEC(ttPriKat.PostBredde)
            VPIArtBas.PostHoyde       = DEC(ttPriKat.PostHoyde)
            VPIArtBas.PostLengde      = DEC(ttPriKat.PostLengde)
            VPIArtBas.PostVekt        = DEC(ttPriKat.PostVekt)
            VPIArtBas.Alder           = INT(ttPriKat.Alder)
            VPIArtBas.LinkVareAnt     = IF INT(ttPriKat.AntLinkVare) > 0 THEN INT(ttPriKat.AntLinkVare) ELSE 1                                     
            pbStatus                  = TRUE
            NO-ERROR.
       IF ERROR-STATUS:ERROR THEN 
       DO:
         CREATE tt_Error.
         ASSIGN
            piAntFeil          = piAntFeil + 1
            ttPriKat.ErrFlag   = TRUE
            tt_Error.LinjeNr   = ttPriKat.LinjeNr
            tt_Error.Tekst     = "* ERROR-STATUS i Assign av VPIArtBas."
            tt_Error.ErrNr = IF bAvbrytVPI THEN 16 ELSE 66.
         IF bTest THEN 
             RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - UtpakkVPI: * ERROR-STATUS i Assign av VPIArtBas.').
       END.
        IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - UtpakkVPI: BRYTGRUPPE-FIRST Etter assign .... Artikkel: ' +  STRING(ttPriKat.ArtikkelNr) + ' ' + ttPriKat.Varetekst).
       
       /* Sjekker om varen skal settes på vent fordi det mangler link til pant. */
       IF ttPriKat.Kontrolleres THEN 
       PANT:
       DO:
         IF VPIArtBas.LinkVareNr > 0 THEN LEAVE PANT.
         FIND ArtBas NO-LOCK WHERE
           ArtBas.ArtikkelNr = DECIMAL(VPIArtBas.VareNr) NO-ERROR.
         IF NOT AVAILABLE ArtBas THEN 
           VPIArtBas.ArtStatus = IF ttPriKat.Kontrolleres THEN 10 ELSE VPIArtBas.ArtStatus.
         ELSE 
           VPIArtBas.ArtStatus = IF (ttPriKat.Kontrolleres AND ArtBas.LinkVareNr = 0) THEN 10 ELSE VPIArtBas.ArtStatus.
       END. /* PANT */            
            
       /* Setter områdenummer hvis det er satt på artikkelen fra før. */
       IF VPIArtBas.RAvdNr = 0 AND VPIArtBas.ArtikkelNr > 0 THEN
       DO:
           FIND bArtBas NO-LOCK WHERE
               bArtBas.ArtikkelNr = VPIArtBas.ArtikkelNr NO-ERROR.
           IF AVAILABLE bArtBas THEN
               VPIArtBas.RAvdNr = bArtBas.RAvdNr.
       END. /* Områdenummer */

       /* Beholder lokal pant hvis det ikke kommer ny pant i filen. */
       IF VPIArtBas.LinkVareNr = 0 AND VPIArtBas.ArtikkelNr > 0 THEN
       DO:
           FIND bArtBas NO-LOCK WHERE
               bArtBas.ArtikkelNr = VPIArtBas.ArtikkelNr NO-ERROR.
           IF AVAILABLE bArtBas THEN
               VPIArtBas.LinkVareNr = IF bArtBas.LinkVareNr > 0 THEN bArtBas.LinkVareNr ELSE VPIArtBas.LinkVareNr.
       END. /* Pant */

       /* Disse feltene skal ikke overstyres når det importeres VPI fra Outlet. */
       IF CAN-DO(cOutletLst,STRING(ttPriKat.ButikkNr)) AND bIkkeOverstyrForOutlet THEN 
       DO:
           FIND bArtBas NO-LOCK WHERE
               bArtBas.ArtikkelNr = VPIArtBas.ArtikkelNr NO-ERROR.
           IF AVAILABLE bArtBas THEN
             ASSIGN 
               VPIArtBas.KatalogPris[1]  = bArtBas.KatalogPris 
               VPIArtBas.AnbefaltPris    = bArtBas.AnbefaltPris
               VPIArtBas.forhRab%[1]     = bArtBas.forhRab%
               VPIArtBas.suppRab%[1]     = bArtBas.supRab%
               VPIArtBas.KjedeInnkPris   = bArtBas.KjedeInnkPris    
               VPIArtBas.KjedeValutaPris = bArtBas.KjedeValutaPris
               VPIArtBas.KjedeRab%       = bArtBas.KjedeRab%        
               .         
       END.  
       /* Ellers tar vi ny info. */
       ELSE DO:
         ASSIGN 
            VPIArtBas.AnbefaltPris    = DEC(ttPriKat.Markedspris)
            VPIArtBas.KatalogPris[1]  = DEC(ttPriKat.LevPrisEngros)
            VPIArtBas.forhRab%[1]     = DEC(ttPriKat.forhRab%)
            VPIArtBas.suppRab%[1]     = DEC(ttPriKat.suppRab%)
            VPIArtBas.KjedeInnkPris   = ttPriKat.KjedeInnkPris    
            VPIArtBas.KjedeValutaPris = IF ttPriKat.KjedeValutaPris <> '' THEN ttPriKat.KjedeValutaPris ELSE VPIArtBas.KjedeValutaPris
            VPIArtBas.KjedeRab%       = ttPriKat.KjedeRab%        
           .
       END.

       /* Lokal artikkelinformasjon skal beholdes på endel av artikkelinformasjonen. */
       /* Denne er satt til FALSE på Sport1HK                                        */
       IF bBeholdLoalArtInfo THEN 
       DO:
           FIND bArtBas NO-LOCK WHERE
               bArtBas.ArtikkelNr = VPIArtBas.ArtikkelNr NO-ERROR.
           IF AVAILABLE bArtBas THEN
             ASSIGN 
             VPIArtBas.Beskr             = bArtBas.Beskr
             VPIArtBas.BongTekst         = bArtBas.BongTekst
             VPIArtBas.Etikettekst1      = bArtBas.Etikettekst1
             VPIArtBas.Etikettekst2      = bArtBas.Etikettekst2
             VPIArtBas.VG                = bArtBas.Vg
             VPIArtBas.LopNr             = bArtBas.LopNr
             VPIArtBas.Etikett           = bArtBas.Etikett
             VPIArtBas.KjedeVare         = bArtBas.KjedeVare
             VPIArtBas.LokPris           = bArtBas.LokPris
             VPIArtBas.Lager             = bArtBas.Lager
             VPIArtBas.Gjennomfaktureres = bArtBas.Gjennomfaktureres
             VPIArtBas.VPIBildeKode      = IF TRIM(VPIArtBas.VPIBildeKode) <> '' THEN VPIArtBas.VPIBildeKode ELSE bArtBas.VPIBildeKode
             VPIArtBas.RAvdNr            = IF VPIArtBas.RAvdNr   = 0 THEN bArtBas.RAvdNr ELSE VPIArtBas.RAvdNr /* Vareområde */
             VPIArtBas.LinkVareNr        = IF VPIArtBas.LinkVareNr > 0 THEN VPIArtBas.LinkVareNr ELSE bArtBas.LinkVareNr
             VPIArtBas.VmId              = IF bArtBas.VmId       = 0 THEN VPIArtBas.VmId ELSE bArtBas.VMId
             VPIArtBas.ProdNr            = IF bArtBas.ProdNr     = 0 THEN VPIArtBas.ProdNr ELSE bArtBas.ProdNr
             VPIArtBas.SalgsStopp        = IF bArtBas.SalgsStopp = 0 THEN VPIArtBas.SalgsStopp ELSE bArtBas.SalgsStopp
             .
       END.

       IF ttPriKat.Merknad <> "" THEN
       DO piLoop = 1 TO NUM-ENTRIES(ttPriKat.Merknad,"|"):
           ASSIGN
               pcTekst = ENTRY(1,ttPriKat.Merknad,"|")
               .
           IF ENTRY(1,pcTekst,"=") = 'nConcept' THEN
               ASSIGN
               VPIArtBas.ProdNr = INT(ENTRY(2,pcTekst,"=")) NO-ERROR.
       END.

      /* Setter leveringsdatofeltene */
      IF ttPriKat.LevUke1 <> "" AND LENGTH(ttPriKat.LevUke1) = 6 THEN
          VPIArtBas.LevDato1 = DATE(1,1, INT(SUBSTRING(ttPriKat.LevUke1,1,4)) ) + (7 * INT(SUBSTRING(ttPriKat.LevUke1,5,2))).       
      IF ttPriKat.LevUke2 <> "" AND LENGTH(ttPriKat.LevUke2) = 6 THEN
          VPIArtBas.LevDato2 = DATE(1,1, INT(SUBSTRING(ttPriKat.LevUke2,1,4)) )  + (7 * INT(SUBSTRING(ttPriKat.LevUke2,5,2))).       
      IF ttPriKat.LevUke3 <> "" AND LENGTH(ttPriKat.LevUke3) = 6 THEN
          VPIArtBas.LevDato3 = DATE(1,1, INT(SUBSTRING(ttPriKat.LevUke3,1,4)) )  + (7 * INT(SUBSTRING(ttPriKat.LevUke3,5,2))).       
      IF ttPriKat.LevUke4 <> "" AND LENGTH(ttPriKat.LevUke4) = 6 THEN
          VPIArtBas.LevDato4 = DATE(1,1, INT(SUBSTRING(ttPriKat.LevUke4,1,4)) )  + (7 * INT(SUBSTRING(ttPriKat.LevUke4,5,2))).       

      /* Blanker ugyldige leveringsdatoer. */
      IF YEAR(VPIArtBas.LevDato1) - YEAR(TODAY) > 2 THEN VPIArtBas.LevDato1 = ?. 
      IF YEAR(VPIArtBas.LevDato2) - YEAR(TODAY) > 2 THEN VPIArtBas.LevDato2 = ?. 
      IF YEAR(VPIArtBas.LevDato3) - YEAR(TODAY) > 2 THEN VPIArtBas.LevDato3 = ?. 
      IF YEAR(VPIArtBas.LevDato4) - YEAR(TODAY) > 2 THEN VPIArtBas.LevDato4 = ?. 

      /* Setter opp karakteristikken */
      IF ttPriKat.Karakteristikk <> '' THEN 
      DO piLoop = 1 TO NUM-ENTRIES(ttPriKat.Karakteristikk,','):
/*         DETTA GÅR INTE AV NÅGON ANLEDNING */
/*         IF NOT CAN-FIND(FIRST VPIArtBasKarakteristikk OF VPIArtBas WHERE                                                 */
/*                               VPIArtBasKarakteristikk.KarakteristikkId = ENTRY(piLoop,ttPriKat.Karakteristikk,',')) THEN */
        IF NOT CAN-FIND(FIRST VPIArtBasKarakteristikk WHERE VPIArtBasKarakteristikk.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
                                  VPIArtBasKarakteristikk.VareNr = VPIArtBas.VareNr AND
                                  VPIArtBasKarakteristikk.KarakteristikkId = ENTRY(piLoop,ttPriKat.Karakteristikk,',')) THEN 
        DO:
            CREATE VPIArtBasKarakteristikk.
            ASSIGN
                VPIArtBasKarakteristikk.EkstVPILevNr      = VPIArtBas.EkstVPILevNr 
                VPIArtBasKarakteristikk.VareNr            = VPIArtBas.VareNr
                VPIArtBasKarakteristikk.KarakteristikkId  = ENTRY(piLoop,ttPriKat.Karakteristikk,',')
                .
        END.
      END. 

      /* Henter Valuta for omregning av rabatten. */
      FIND Valuta OF VPIArtBas NO-LOCK NO-ERROR.

      ASSIGN
        dcValPris  = DECIMAL(ttPriKat.LevPrisEngros)
        dcUtpris   = DECIMAL(ttPriKat.MarkedsPris)
        dcRabatt   = DECIMAL(ttPriKat.forhRab%)
        NO-ERROR.
      /* Bygger kalkulasjonsstrengen */
      ASSIGN
        pcSkjerm = KalkStreng()
        .

      /* Simulerer kalkulering */
      RUN Kalkulasjon ("ValPris",INPUT-OUTPUT pcSkjerm). /* Enter i valutapris */
      ASSIGN
        pcSkjerm  = pcSkjerm + "0;0;" + cEndelse
        .

      RUN Kalkulasjon ("InnPris",INPUT-OUTPUT pcSkjerm). /* Enter i innkjøpspris */
      ASSIGN
        pcSkjerm  = pcSkjerm + "0;0;" + cEndelse
        .
      RUN Kalkulasjon ("Rab1%",   INPUT-OUTPUT pcSkjerm). /* Enter i rabatt1    */
      ASSIGN
          pcSkjerm  = pcSkjerm + "0;0;" + cEndelse
          .
      RUN Kalkulasjon ("Pris",   INPUT-OUTPUT pcSkjerm). /* Enter i Pris       */
      ASSIGN
        pcSkjerm  = pcSkjerm + "0;0;" + cEndelse
        .
        
      FIND VPIArtPris EXCLUSIVE-LOCK WHERE
          VPIArtPris.EkstVpiLevNr = VPIFilHode.EkstVPILevNr AND
          VPIArtPris.VareNr       = string(ttPriKat.ArtikkelNr) AND
          VPIArtPris.ProfilNr     = ttPriKat.ProfilNr NO-ERROR.
      IF NOT AVAILABLE VPIArtPris THEN
      DO:
          CREATE VPIArtPris.
          ASSIGN
              VPIArtPris.EkstVpiLevNr = VPIFilHode.EkstVPILevNr 
              VPIArtPris.VareNr       = STRING(ttPriKat.ArtikkelNr) 
              VPIArtPris.ArtikkelNr   = ttPriKat.ArtikkelNr
              VPIArtPris.ProfilNr     = ttPriKat.ProfilNr
              VPIArtPris.Tilbud       = FALSE
              .
      END.
      FIND ArtPris NO-LOCK WHERE 
        ArtPris.ArtikkelNr = VPIArtPris.ArtikkelNr AND
        ArtPris.ProfilNr   = ttPriKat.ProfilNr NO-ERROR.
      /* TN 1/4-20 Tar vare på gamle priser m.m., aktiv tilbud. */
      IF AVAILABLE ArtPris THEN 
        BUFFER-COPY ArtPris 
          EXCEPT ArtikkelNr ProfilNr LevNr /* Levnr er ikke satt i ArtPris i PRS!!! */
          TO VPIArtBas
          .
      /* PiTilbud står alltid til 1.   */
      /* Her endres bare normalprisen. */    
      ASSIGN
          VPIArtPris.ValPris[piTilbud]      = dec(ENTRY(1,pcSkjerm,";"))
          VPIArtPris.InnKjopsPris[piTilbud] = dec(ENTRY(2,pcSkjerm,";"))
          VPIArtPris.Rab1Kr[piTilbud]       = dec(ENTRY(3,pcSkjerm,";"))
          VPIArtPris.Rab1%[piTilbud]        = dec(ENTRY(4,pcSkjerm,";"))
          VPIArtPris.Rab2Kr[piTilbud]       = dec(ENTRY(5,pcSkjerm,";"))
          VPIArtPris.Rab2%[piTilbud]        = dec(ENTRY(6,pcSkjerm,";"))
          VPIArtPris.Frakt[piTilbud]        = dec(ENTRY(7,pcSkjerm,";"))
          VPIArtPris.Frakt%[piTilbud]       = dec(ENTRY(8,pcSkjerm,";"))
          VPIArtPris.DivKostKr[piTilbud]    = dec(ENTRY(9,pcSkjerm,";"))
          VPIArtPris.DivKost%[piTilbud]     = dec(ENTRY(10,pcSkjerm,";"))
          VPIArtPris.Rab3Kr[piTilbud]       = dec(ENTRY(11,pcSkjerm,";"))
          VPIArtPris.Rab3%[piTilbud]        = dec(ENTRY(12,pcSkjerm,";"))
          VPIArtPris.VareKost[piTilbud]     = dec(ENTRY(13,pcSkjerm,";"))
          VPIArtPris.MvaKr[piTilbud]        = dec(ENTRY(14,pcSkjerm,";"))
          VPIArtPris.Mva%[piTilbud]         = dec(ENTRY(15,pcSkjerm,";"))
          VPIArtPris.DbKr[piTilbud]         = dec(ENTRY(16,pcSkjerm,";"))
          VPIArtPris.Db%[piTilbud]          = dec(ENTRY(17,pcSkjerm,";"))
          VPIArtPris.Db%[piTilbud]          = ROUND(VPIArtPris.Db%[piTilbud],2)
          VPIArtPris.Pris[piTilbud]         = dec(ENTRY(18,pcSkjerm,";"))
          VPIArtPris.EuroPris[piTilbud]     = dec(ENTRY(19,pcSkjerm,";"))
          VPIArtPris.EuroManuel             = FALSE
          VPIArtPris.AktivFraDato           = DATE(ENTRY(21,pcSkjerm,";"))
          VPIArtPris.AktivFraTid            = INT(ENTRY(22,pcSkjerm,";"))
          .
      IF VPIArtPris.Db%[piTilbud] < 0 THEN
          ttPriKat.behStatus = 30.
      /* Lagrer strekkoder og tømmer loggen */
      IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - UtpakkVPI: Før lagreStrekkoder .... ').
      RUN LagreStrekkoder.
      IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - UtpakkVPI: Etter lagreStrekkoder .... ').

      /* Kopierer til VPI register 1 før det sendes. */
      IF (iKopierNyeArtikler > 0 AND VPIFilHode.EkstVPILevNr <> iKopierNyeArtikler) THEN
        DO: 
/*          IF NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = VPIArtBas.ArtikkelNr) THEN*/
          DO: 
            IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - UtpakkVPI: Før vpiartbas_kopier_vpi_til_vpi .... ').
            RUN vpiartbas_kopier_vpi_til_vpi.p (VPIArtBas.EkstVPILevNr,iKopierNyeArtikler,VPIArtBas.ArtikkelNr).
            FIND bVPIArtBas EXCLUSIVE-LOCK WHERE
              bVPIArtBas.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND 
              bVPIArtBas.ArtikkelNr   = VPIArtBas.ArtikkelNr NO-ERROR.
            IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - UtpakkVPI: Etter vpiartbas_kopier_vpi_til_vpi .... ').
          END.
        END.
    END. /* BRYTGRUPPE-FIRST */

    /* Håndterer en artikkel MED alle dens farger. */
    IF LAST-OF(ttPriKat.ArtikkelNr) THEN 
    BRYTGRUPPE-LAST:
    DO:
      IF bTest THEN 
        RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - UtpakkVPI: start BRYTGRUPPE-LAST ....Artikkel: ' + string(ttPriKat.ArtikkelNr)).

      /* Ligger det størrelsestype 2 på artikkelen, skal det sjekkes at det bare finnes størrelse ' 1'. */
      /* Ligger det noe annet, skal artikkelen ha en egen størrelsestype.                               */
      IF VPIArtBas.StrTypeId = 2 THEN
      STRTYPESJEKK: 
      DO:
        FOR EACH VPIStrekkode OF VPIArtBas NO-LOCK:
          IF VPISTrekkode.StrKode <> 1 THEN DO: 
            VPIArtBas.STrTypeId = 0.
            LEAVE STRTYPESJEKK.
          END.
        END.
      END. /* STRTYPESJEKK */

      /* Ligger bare str = 1 på artikkelen, SKAL den ha størrelsestype 2 uansett. */
      IF VPIArtBas.StrTypeId <> 2 THEN 
      DO:
        ASSIGN iAntall = 0 bFlagg = FALSE.
        FOR EACH VPIStrekkode OF VPIArtBas NO-LOCK:
          iAntall = iAntall + 1.
          IF VPISTrekkode.StrKode = 1 THEN 
            bFlagg = TRUE.
        END.
        IF iAntall = 1 AND bFlagg THEN
          VPIArtBAs.StrTypeId = 2. 
      END.
      
      /* Sjekker StrType og eventuelt setter StrType. */
      rStandardVPIFunksjoner:setVPIStdStrType(VPIArtBas.EkstVPILevNr,VPIArtBas.ArtikkelNr, INPUT-OUTPUT VPIArtBas.StrTypeId).

      /* Oppretter strørrelsestype på artikkelen hvis det ikke finnes fra før */
      IF VPIArtBas.StrTypeId <= 1 THEN
      STRTYPE0: 
      DO:
        FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = DECIMAL(VPIArtBas.VareNr) NO-ERROR.
        IF AVAILABLE ArtBas THEN 
            VPIArtBas.StrTypeId = ArtBas.STrTypeID.
        ELSE    
            RUN setStorrelsestype.p (DECIMAL(VPIArtBas.VareNr), VPIArtBas.EkstVPILevNr, TRUE, OUTPUT VPIArtBas.StrTypeId).
      END. /* STRTYPE0*/

      /* Kontroll av pris og logging av prisendringer. Gjøres før priser overføres til artikkelregister. */
      IF lLoggPrisendring THEN 
      LOGG_PRISENDRING:
      DO:
          FIND bArtBas NO-LOCK WHERE
               bArtBas.ArtikkelNr = VPIArtBas.ArtikkelNr NO-ERROR.
          IF AVAILABLE bArtBas THEN  
          PRIS_AVVIK_LOGGING:
          DO:
              FIND FIRST VPIArtPris OF VPIArtBas NO-LOCK NO-ERROR.
              FIND ArtPris NO-LOCK WHERE 
                ArtPris.ArtikkelNr = bArtBas.ArtikkelNr AND 
                ArtPris.ProfilNr   = VPIArtPris.ProfilNr NO-ERROR.
              IF NOT AVAILABLE ArtPris THEN 
                FIND FIRST ArtPris NO-LOCK WHERE 
                  ArtPris.ArtikkelNr = bArtBas.ArtikkelNr NO-ERROR.
              IF AVAILABLE ArtPris AND 
                 (
                   (VPIArtPris.Pris[1] <> ArtPris.Pris[1]) OR
                   (VPIArtPris.VareKost[1] <> ArtPris.VareKost[1]) 
                 )  THEN 
              DO:
                  LOOPEN:
                  DO  pdDato = TODAY TO TODAY - 160 BY -1: 
                      FIND LAST TransLogg NO-LOCK WHERE 
                          TransLogg.ArtikkelNr = bArtBas.ArtikkelNr AND 
                          Translogg.Dato       = pdDato AND 
                          TransLogg.Butik      = ttPriKat.ButikkNr AND 
                          TransLogg.TTId       = 1 USE-INDEX OppslagDatoTid NO-ERROR.
                  END. /* LOOPEN */
                      
                  CREATE tt_Error.
                  ASSIGN
                      tt_Error.Tekst     = VareIdent()          + 
                                           ";Prisendring: "     + 
                                           ";Butikk: "          + STRING(ttPriKat.ButikkNr) +
                                           (IF VPIArtPris.VareKost[1] <> ArtPris.VareKost[1] THEN ";Endret varekost: " ELSE ";Varekost: ") + STRING(VPIArtPris.Varekost[1]) +
                                           ";Aktiv varekost: "      + STRING(ArtPris.Varekost[1]) +
                                           (IF VPIArtPris.Pris[1] <> ArtPris.Pris[1] THEN ";Endret pris: " ELSE ";Pris: ") + STRING(VPIArtPris.Pris[1]) +
                                           ";Aktiv pris: "      + STRING(ArtPris.Pris[1]) +
                                           ";Sist solgt pris: " + STRING(IF AVAILABLE TransLogg THEN STRING(TransLogg.Pris) ELSE '') + 
                                           ";Rabatt: "          + STRING(IF AVAILABLE TransLogg THEN STRING(TransLogg.RabKr) ELSE '') 
                      tt_Error.ErrNr     = 98
                      tt_Error.ButikkNr  = ttPriKat.ButikkNr.
              END.
          END. /* PRIS_AVVIK_LOGGING */    
      END. /* LOGG_PRISENDRING*/
    
      /* Oppretter artikler automatisk hvis de ikke finnes fra før og opprettelse er flagget.  */
      /* Ved import av nye artikler fra IPS RIGAL filer, skal artikklene opprettes automatisk. */  
      bNyArt = FALSE.    
      IF (bOpprettNye OR ttPriKat.OpprettArtikkel) AND 
        ttPriKat.behStatus <> 30 AND  
        NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = ttPriKat.ArtikkelNr) THEN
      NYART:
      DO:
        /* Felt liste oppdatert pr. 1/7-10 TN */
        IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - UtpakkVPI: NY artbas_new.p .... ').
        cFieldList = {tbchooseAll.i}.
        RUN artbas_new.p (STRING(VPIArtBas.EkstVPILevNr) + ';' + cFieldList + ';' + STRING(ttPriKat.ArtikkelNr), 
                          ihBuffer, 
                          icSessionid, 
                          OUTPUT ocReturn, 
                          OUTPUT obOk).
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr   = ttPriKat.LinjeNr
          tt_Error.Tekst     = VareIdent() + "* Ny artikkel."
          tt_Error.ErrNr     = 99
          tt_Error.ButikkNr  = ttPriKat.ButikkNr.
                           
        bNyArt = TRUE.     
      END. /* NYART */
      /* Oppdaterer prisendringer på artikler som finnes hvis dette er flagget. */
      /* Dette gjøres for IPS RIGAL filer. Sjekk på om utpris skal  overføres.  */
      IF bKjorOppdAvPrisko AND  
        /*ttPriKat.ButikkNr > 0 AND*/ /* TN 25/11-13 Skal også gjøres for hk */ 
        ttPriKat.behStatus < 10 AND 
        CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = ttPriKat.ArtikkelNr) THEN
      OPPDATER_PRISKO:
      DO:
        /* Er det HK som leses inn, skal også artikkelinformasjonen overføres. */
        IF ttPriKat.ProfilNr = iClProfilNr THEN 
        DO:
          IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - UtpakkVPI: HK OPPD artbas_new.p .... ').
          /* {tbchooseAll.i} Bruker ikke den her da vi ikke skal ha over prisene. De legges i priskøen. */
          cFieldList = "tbChooseAll|LevKod|LinkVareNr|KjedeVare|Beskr|EkstStrTypeNavn|Gjennomfaktureres|LevFargKod|AntIPkn" +
                       "|LokPris|LevNr|Salgsenhet|KjedeRab%|ProdNr|Etikettekst1|KjedeSupRab%|VmId|BongTekst|Sasong" +
                       "|Anonseartikkel|LevDato1|LevDato2|LevDato3|LevDato4|Vg|InnkjopsPris|VPIBildekode|supRab%|RAvdNr" +
                       "|forhRab%|Mengde|AnbefaltPris|JamforEnhet|Pris" +
                       "|NyStrekkode|KorrStrekkode|Grunnsortiment".
          RUN artbas_new.p (STRING(VPIArtBas.EkstVPILevNr) + ';' + cFieldList + ';' + STRING(ttPriKat.ArtikkelNr), 
                            ihBuffer, 
                            icSessionid, 
                            OUTPUT ocReturn, 
                            OUTPUT obOk).
        END.        
        /* Legger pris i priskø. */
        IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - UtpakkVPI: OPPDATER_PRISKO Start legge pris i priskø - vpiartbas_pris.p .... ').
        
        ihHandle = BUFFER VPIArtPris:HANDLE.
        IF ttPriKat.ProfilNr = iClProfilNr THEN 
        DO:
          IF bOppdOgsaUtpris THEN 
            RUN vpiartbas_pris.p (STRING(TODAY) + '|yes|yes|yes',ihHandle,icSessionId,OUTPUT ocReturn,OUTPUT obOk).
          ELSE 
            RUN vpiartbas_pris.p (STRING(TODAY) + '|yes|yes|no',ihHandle,icSessionId,OUTPUT ocReturn,OUTPUT obOk).          
        END.
        ELSE DO:
          /* Her skal priskøposten på nye artikler bli liggende slik at de kan se den i prisregisteret. */
          IF bNyArt AND bOpprettPrisKoVedNyArt THEN 
          DO:
            IF bOppdOgsaUtpris THEN 
              RUN vpiartbas_pris.p (STRING(TODAY) + '|yes|yes|yes|yes',ihHandle,icSessionId,OUTPUT ocReturn,OUTPUT obOk).
            ELSE 
              RUN vpiartbas_pris.p (STRING(TODAY) + '|yes|yes|no|yes',ihHandle,icSessionId,OUTPUT ocReturn,OUTPUT obOk).
          END.
          ELSE DO:
            IF bOppdOgsaUtpris THEN 
              RUN vpiartbas_pris.p (STRING(TODAY) + '|yes|yes|yes',ihHandle,icSessionId,OUTPUT ocReturn,OUTPUT obOk).
            ELSE 
              RUN vpiartbas_pris.p (STRING(TODAY) + '|yes|yes|no',ihHandle,icSessionId,OUTPUT ocReturn,OUTPUT obOk).
          END.          
        END.
        /* Klargjør priskø for artikkelen. */
        FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = DECIMAL(VPIArtBas.VareNr) NO-ERROR.
        IF AVAILABLE ArtBas THEN 
            RUN KlargjorPrisKoEn IN h_PrisKo (ROWID(ArtBas)).
        
        IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - UtpakkVPI: Slutt legge pris i priskø - vpiartbas_pris.p .... ').
        
        /* Logger i ELogg for IPS filer. */
        IF (bOpprettELogg AND ttPriKat.ProfilNr <> iClProfilNr AND ttPriKat.BehStatus <> 20) THEN 
            RUN vpiartbas_elogg.p (STRING(TODAY) + '|yes|yes|no',ihHandle,icSessionId,OUTPUT ocReturn,OUTPUT obOk).
        
        /* Setter VPI Status. */
        ASSIGN
          VPIArtBas.behStatus = IF ttPrikat.BehStatus > 1 THEN ttPrikat.BehStatus ELSE 90. /* Behandlet */    
      END. /* OPPDATER_PRISKO */

      /* (Gurres) Direkte oppdatering av vareinfo. */
      IF bOppdaterBasicVareinfo THEN 
      DO:
        cFieldList = "tbChooseAll|Vg|LevKod|Beskr|Sasong".
        RUN artbas_new.p (STRING(VPIArtBas.EkstVPILevNr) + ';' + cFieldList + ';' + STRING(ttPriKat.ArtikkelNr), 
                          ihBuffer, 
                          icSessionid, 
                          OUTPUT ocReturn, 
                          OUTPUT obOk).
      END. 
      /* automatisk importerte filer som skal videre til varebok */
      IF bAutImport THEN 
        ASSIGN VPIArtBas.behStatus = IF ttPrikat.BehStatus > 0 THEN ttPrikat.BehStatus ELSE 2. /* Aut.importert */      

      /* Linjer med feil skal kontrolleres. */
      ELSE IF ttPriKat.ErrFlag  THEN 
      DO:
        ASSIGN VPIArtBas.behStatus = 20. /* Kontrolleres */
      END.        
      /* Pantartikkel som skal kontrolleres. */
      ELSE IF ttPriKat.Kontrolleres THEN
      DO: 
        ASSIGN VPIArtBas.behStatus = IF ttPrikat.BehStatus > 0 THEN ttPrikat.BehStatus ELSE 20. /* Kontrolleres */
      END.            
        
      IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - UtpakkVPI: Slutt BRYTGRUPPE-LAST ....Artikkel: ' + string(ttPriKat.ArtikkelNr)).
    END. /* BRYTGRUPPE-LAST LAST-OF */   
END. /* TRANSACTION - VPIFILLINJE */

IF bSlettBehandlede THEN 
DO:
  FOR EACH ttPriKat WHERE 
    ttPriKat.ArtikkelNr > 0 TRANSACTION:
      FIND FIRST VPIArtBas EXCLUSIVE-LOCK WHERE
          VPIArtBas.EkstVpiLevNr  = ttPriKat.EkstVPILevNr AND
          VPIArtBas.ArtikkelNr    = ttPriKat.ArtikkelNr /*AND  
          VPIArtBas.BehStatus     = 90 TN 17/4-19 */ NO-ERROR.
      IF AVAILABLE VPIArtBas THEN
      DO: 
        DELETE VPIArtBas.
      END.
  END.    
END.

IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - UtpakkVPI: Start - oppdater datasett').
IF pbStatus = TRUE THEN
DO TRANSACTION:
  FIND CURRENT VPIDatasett EXCLUSIVE-LOCK.
  ASSIGN
      VPIDatasett.DatasettStatus = 5 /* VPI mottatt og behandlet */
      .
  FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
  ASSIGN
      VPIFilHode.VPIFilStatus = 5
      .

  FOR EACH VPIArtBas OF VPIDatasett NO-LOCK:
      IF VPIArtBas.ArtikkelNr <> 0 THEN
          piAntKoblet = piAntKoblet + 1.
      piAntArtikler = piAntArtikler + 1.
  END.
  ASSIGN
      VPIDatasett.AntallArtikler = piAntArtikler
      VPIDatasett.AntallKoblet   = piAntKoblet
      .

  FIND CURRENT VPIDatasett NO-LOCK.
  FIND CURRENT VPIFilHode  NO-LOCK.
END. /* TRANSACTION */
IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'xsport1vpiutpakk.p - UtpakkVPI: Slutt - oppdater datasett').

STATUS DEFAULT "".
PUBLISH 'visStatusMsg' ("").    

ASSIGN
    cTekst = "Behandlet " + STRING(iTotAntLinjer) + " VPI poster. Av disse var " + string(piAntOppdat) + " oppdateringer.".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").
ASSIGN
    cTekst = "Antall artikler i datasettet er " + STRING(piAntArtikler) + "." + 
             " Av disse er " + STRING(piAntKoblet) + " koblet til lokale artikler.".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").
ASSIGN
    cTekst = "Utpakking av VPI ferdig.Tidsbruk " + STRING(TIME - piTid,"HH:MM:SS") + ".".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */
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

&IF DEFINED(EXCLUDE-KalkStreng) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION KalkStreng Procedure 
FUNCTION KalkStreng RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcTekst   AS CHAR NO-UNDO.
  DEF VAR pdAktDato AS DATE NO-UNDO.
  DEF VAR plTilbud AS LOG  NO-UNDO.
  DEF VAR plManuel AS LOG  NO-UNDO.
  
  ASSIGN
    plTilbud = FALSE
    plManuel = FALSE
    pdAktDato =  IF (ttPriKat.AktivFraDato <> ? AND ttPriKat.AktivFraDato <> '') THEN DATE(ttPriKat.AktivFraDato) ELSE TODAY - 1
    pcTekst   =   
      /*string(input FI-ValPris) */ STRING(dcValPris) + ";" +
      /*string(input FI-InnPris) */ string(dcInnPris) + ";" +
      /*string(input FI-Rab1)    */  "0" + ";" +
      /*string(input FI-Rab1%)   */ string(dcRabatt)  + ";" +
      /*string(input FI-Rab2)    */ "0" + ";" +
      /*string(input FI-Rab2%)   */ "0" + ";" +
      /*string(input FI-Frakt)   */ "0" + ";" +
      /*string(input FI-Frakt%)  */ "0" + ";" +
      /*string(input FI-DivKost) */ "0" + ";" +
      /*string(input FI-DivKost%)*/ "0" + ";" +
      /*string(input FI-Rab3)    */ "0" + ";" +
      /*string(input FI-Rab3%)   */ "0" + ";" +
      /*string(input FI-VareKost)*/ "0" + ";" +
      /*string(input FI-Mva)     */ "0" + ";" +
      /*string(input FI-Mva%)    */ "0" + ";" +
      /*string(input FI-DB)      */ "0" + ";" +
      /*string(input FI-DB%)     */ "0" + ";" +
      /*string(input FI-Pris)    */ STRING(dcUtpris) + ";" +
      /*string(input FI-EUPris)  */ "0" + ";" +
      /*plManuel                 */ "no" + ";"
       .
  /* Normal aktiveringsdag/tid */                 
  ASSIGN  
    cEndelse = cEndelse +              
             (IF pdAktDato <> ?
                THEN STRING(pdAktDato)
                ELSE "") + ";" +
              "0;"
    cEndelse = cEndelse + 
             ";0;;0;no"
    pcTekst = pcTekst + cEndelse
    .
  
  RETURN pcTekst.   /* Function return value. */

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

        cTekst = " EkstVpiLev: " + STRING(ttPriKat.EkstVPILevNr,"zzzzzz9") + " Linje: " + STRING(ttPriKat.LinjeNr,">>zzzzzz9") + ' '  
                 + (IF LENGTH(ttPriKat.EANnr)       < 13 THEN FILL(' ',13 - LENGTH(ttPriKat.EANnr))       ELSE '') + ttPriKat.EANnr       + ' ' 
                 + (IF LENGTH(ttPriKat.LevModellNr) < 10 THEN FILL(' ',10 - LENGTH(ttPriKat.LevModellNr)) ELSE '') + ttPriKat.LevModellNr + ' ' 
                 + (IF LENGTH(ttPriKat.LevKod)      < 10 THEN FILL(' ',10 - LENGTH(ttPriKat.LevKod))      ELSE '') + ttPriKat.LevKod      + ' ' 
                 + (IF LENGTH(ttPriKat.Varetekst)   < 30 THEN FILL(' ',30 - LENGTH(ttPriKat.Varetekst))   ELSE '') + ttPriKat.Varetekst   + ': ' 
                 . 

		RETURN cTekst.
END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


