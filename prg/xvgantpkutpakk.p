&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

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
DEF VAR lLapTop       AS LOG NO-UNDO.
DEF VAR iProfilNr     AS INT NO-UNDO.

DEF VAR h_PrisKo      AS HANDLE NO-UNDO.
DEF VAR rArtBasRecid  AS RECID  NO-UNDO.
DEF VAR cEndelse      AS CHAR   NO-UNDO.
DEF VAR cTekst        AS CHAR   NO-UNDO.

DEF VAR dcValPris     AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcInnPris     AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcUtpris      AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcRabatt      AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcFrakt       AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR bAvbrytVPI    AS LOG  NO-UNDO.

DEF VAR cEDB-System        LIKE ImpKonv.EDB-System NO-UNDO.
DEF VAR cImpTabell         LIKE ImpKonv.Tabell     NO-UNDO.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .
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
DEF VAR plArtikkelNr  AS DEC  NO-UNDO.

{ttpricat.i &NEW=" " &SHARED=" "}

DEF BUFFER buf-ttPriKat FOR ttPriKat.
DEF BUFFER bStrKonv     FOR StrKonv.
DEF STREAM Ut.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */
&IF DEFINED(EXCLUDE-KalkStreng) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD KalkStreng Procedure 
FUNCTION KalkStreng RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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
         HEIGHT             = 15
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

/* Centrallager */
{syspara.i 5 1 1 iCl INT}
{syspara.i 2 4 10 cEnhet}
/* Skal VPI innlesning av brytes hvis det oppdages feil. */
{syspara.i 50 15 2 cTekst}
IF CAN-DO("1,yes,true,Ja",cTekst) THEN
    bAvbrytVPI = TRUE.
ELSE
    bAvbrytVPI = FALSE.

FOR EACH tt_Error:
  DELETE tt_Error.
END.

/* Start av procedurebibliotek */
IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN prisko.p PERSISTENT SET h_PrisKo.

/* Sjekker om det kjøres på en LapTop */
IF VALID-HANDLE(h_dproclib) THEN
  RUN SjekkLapTop IN h_dproclib (OUTPUT lLapTop).
ELSE 
  lLapTop = FALSE.

/* Profilnr for sentrallager. */
FIND Butiker NO-LOCK WHERE
    Butiker.butik = iCl NO-ERROR.
ASSIGN
    iProfilNr = Butiker.ProfilNr
    .

/* Filhode. */
FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    MESSAGE "Ingen VPIFilHode tilgjengelig"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

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
ASSIGN
    cEDB-System = EkstVPILev.KortNavn
    .

/* Datasett statuspost. */
FIND VPIDatasett NO-LOCK WHERE
    VPIDatasett.EkstVPILevNr = VPIFilHode.EkstVPILevNr NO-ERROR.
                                                
/* Er datasettet ferdig oppdatert? */
IF AVAILABLE VPIDatasett THEN
DO:
    IF VPIDatasett.DatasettStatus >= 3 AND
       VPIDatasett.DatasettStatus <= 4 THEN
    DO:
        MESSAGE "Forrige datasett er ikke oppdatert." SKIP
                "Det må være ferdig oppdatert før ny import av VPI kan gjøres."                
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
END.

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

/* Benytter alltid laveste størrelsestypen ved importen */
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

STATUS DEFAULT "Bygger temp-tabell (ttPriKat)....".
/* Leser opp filen i temp tabell og gjør linjevalidering av alle felt. */
RUN ByggTmpTabell.

STATUS DEFAULT "Kjører registervalidering (ttPriKat)....".
/* Validerer filens innhold mot aktuelle registre. */
RUN Registervalidering.

STATUS DEFAULT "Setter artikkelnummer (ttPriKat)....".
/* Setter artikkelnummer på linjene */
RUN SettArtikkelNr.

STATUS DEFAULT "Setter modellfarge (ttPriKat)....".
/* Sett modellNr på linjene. */
RUN SettModellFarge.

STATUS DEFAULT "Skriver til fillogg (ttPriKat)....".
/* Skriver til loggfil */
/*IF CAN-FIND(FIRST tt_Error) THEN*/
    RUN SkrivTilLogg.

/* Utpakking av VPI til VPI bufferet. */
IF NOT CAN-FIND(FIRST tt_Error) OR bAvbrytVPI = FALSE THEN
DO:
    STATUS DEFAULT "Pakker ut VPI (ttPriKat)....".
    RUN UtpakkVpi.
END.
ELSE  DO:
    STATUS DEFAULT "Sletter feiloppdatert VPI (ttPriKat)....".
    RUN SlettVpi.
END.

ON CLOSE OF THIS-PROCEDURE 
DO:
  IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_Prisko.
  RUN disable_UI.
END.

IF CAN-FIND(FIRST tt_Error) THEN
  RUN ErrorLogg.

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

FOR EACH VPIFilLinje NO-LOCK WHERE 
    VPIFilLinje.FilId = VPIFilHode.FilId:

    STATUS DEFAULT "Bygger temp-tabell - FilId/FilLinje: " + string(VPIFilLinje.FilId) + "/" + STRING(VPIFilLinje.LinjeNr) + ".".

    CREATE ttPriKat.
    ASSIGN
        ttPriKat.EkstVPILevNr  = VPIFilHode.EkstVPILevNr
        ttPriKat.LinjeNr       = VPIFilLinje.LinjeNr
        ttPriKat.R1            = TRIM(ENTRY( 1,VPIFilLinje.StorTekst,";"),'"')        
        ttPriKat.LevNr         = TRIM(ENTRY( 2,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.LevModellNr   = TRIM(ENTRY( 3,VPIFilLinje.StorTekst,";"),'"')                  
        ttPriKat.EANnr         = TRIM(ENTRY( 4,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.VareTekst     = TRIM(ENTRY( 5,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.FargeKode     = TRIM(ENTRY( 6,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.FargeTekst    = TRIM(ENTRY( 7,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.Str           = TRIM(ENTRY( 8,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.StrTab        = TRIM(ENTRY( 9,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.Varemerke     = TRIM(ENTRY(10,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.Enh           = TRIM(ENTRY(11,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.Enh           = (IF ttPriKat.Enh = "" THEN "Stk" ELSE ttPriKat.Enh)
        ttPriKat.AntIEnh       = TRIM(ENTRY(12,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.LevPrisEngros = REPLACE(TRIM(TRIM(ENTRY(13,VPIFilLinje.StorTekst,";"),'"'),"%"),' ','')
        ttPriKat.ValKod        = TRIM(ENTRY(14,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.forhRab%      = TRIM(TRIM(TRIM(ENTRY(15,VPIFilLinje.StorTekst,";"),'"'),"%"),'')
        ttPriKat.suppRab%      = TRIM(TRIM(TRIM(ENTRY(16,VPIFilLinje.StorTekst,";"),'"'),"%"),'')
        ttPriKat.VeilPris      = TRIM(ENTRY(17,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.VeilPris      = TRIM(REPLACE(ttPriKat.VeilPris,' ',''),"%")
        ttPriKat.PAKstru       = TRIM(ENTRY(18,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.LevUke1       = TRIM(ENTRY(19,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.LevUke2       = TRIM(ENTRY(20,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.LevUke3       = TRIM(ENTRY(21,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.LevUke4       = TRIM(ENTRY(22,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.VareGruppe    = TRIM(ENTRY(23,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.LevNavn       = TRIM(ENTRY(24,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.nettoForh     = REPLACE(TRIM(TRIM(ENTRY(25,VPIFilLinje.StorTekst,";"),'"'),"%"),' ','')
        ttPriKat.kalkForh      = REPLACE(TRIM(TRIM(ENTRY(26,VPIFilLinje.StorTekst,";"),'"'),"%"),' ','')
        ttPriKat.BFforh        = REPLACE(TRIM(TRIM(ENTRY(27,VPIFilLinje.StorTekst,";"),'"'),"%"),' ','')
        ttPriKat.nettoSupp     = REPLACE(TRIM(TRIM(ENTRY(28,VPIFilLinje.StorTekst,";"),'"'),"%"),' ','')
        ttPriKat.kalkSupp      = REPLACE(TRIM(TRIM(ENTRY(29,VPIFilLinje.StorTekst,";"),'"'),"%"),' ','')
        ttPriKat.BFsupp        = REPLACE(TRIM(TRIM(ENTRY(30,VPIFilLinje.StorTekst,";"),'"'),"%"),' ','')
        ttPriKat.MarkedsPris   = REPLACE(TRIM(TRIM(ENTRY(31,VPIFilLinje.StorTekst,";"),'"'),"%"),' ','')
        ttPriKat.Sortiment     = TRIM(ENTRY(32,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.Sesong        = TRIM(ENTRY(33,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.VPIBildeKode  = TRIM(ENTRY(34,VPIFilLinje.StorTekst,";"),'"')
        ttPriKat.Merknad       = TRIM(ENTRY(35,VPIFilLinje.StorTekst,";"),'"')
        .      
    /*
    /* fikser komma i feltene */
    ASSIGN
        ttPriKat.LevPrisEngr  = REPLACE(ttPriKat.LevPrisEngr,',','.')
        ttPriKat.forhRab%     = REPLACE(ttPriKat.forhRab%,',','.')
        ttPriKat.suppRab%     = REPLACE(ttPriKat.suppRab%,',','.')
        ttPriKat.VeilPris     = REPLACE(ttPriKat.VeilPris,',','.')
        ttPriKat.nettoForh    = REPLACE(ttPriKat.nettoForh,',','.')
        ttPriKat.kalkForh     = REPLACE(ttPriKat.kalkForh,',','.')
        ttPriKat.BFforh       = REPLACE(ttPriKat.BFforh,',','.')
        ttPriKat.nettoSupp    = REPLACE(ttPriKat.nettoSupp,',','.')
        ttPriKat.kalkSupp     = REPLACE(ttPriKat.kalkSupp,',','.')
        ttPriKat.BFsupp       = REPLACE(ttPriKat.BFsupp,',','.')
        ttPriKat.MarkedsPris  = REPLACE(ttPriKat.MarkedsPris,',','.')
        .
    */

    /* Validerer informasjonen */
    RUN LinjeValidering.

    /* Konverterer informasjonen */
    RUN Konverter.

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
  DO:
    CREATE tt_Error.
    ASSIGN
      piAntFeil = piAntFeil + 1
      tt_Error.LinjeNr = piAntFeil
      tt_Error.Tekst   = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent artikkel."
      .
    RETURN "AVBRYT".
  END.
  FIND bVarGr OF bArtBas NO-ERROR.
  IF NOT AVAILABLE bVarGr THEN
  DO:
    CREATE tt_Error.
    ASSIGN
      piAntFeil = piAntFeil + 1
      tt_Error.LinjeNr = piAntFeil
      tt_Error.Tekst   = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent varegruppe."
      .
    RETURN "AVBRYT".
  END.
  FIND bMoms  OF bVarGr  NO-ERROR.
  IF NOT AVAILABLE bMoms THEN
  DO:
    MESSAGE "Ukjent bMoms"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
  END.
  FIND bValuta OF bArtBas NO-ERROR.
  IF NOT AVAILABLE bValuta THEN
  DO:
    CREATE tt_Error.
    ASSIGN
      piAntFeil = piAntFeil + 1
      tt_Error.LinjeNr = piAntFeil
      tt_Error.Tekst   = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent valuta."
      .
    RETURN "AVBRYT".
  END.

  /* Starter omkalkulering.                         */
  RUN Omregning IN h_PrisKo
       (INPUT rArtBasRecid, 
        INPUT iProfilNr,
        INPUT-OUTPUT pcSkjerm,
        INPUT bMoms.MomsProc,
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
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR pbFlagg AS LOG NO-UNDO.
    DEF VAR piInt   AS INT NO-UNDO.
            
    /* Henter varegruppen */
    FIND VarGr NO-LOCK WHERE
        VarGr.Vg = INT(ttPriKat.Varegruppe) NO-ERROR.
    IF AVAILABLE VarGr THEN
    DO:
        pcMva% = "".
        FIND Moms NO-LOCK WHERE
            Moms.MomsKod = VarGr.MomsKod NO-ERROR.
        IF AVAILABLE Moms THEN
            pcMva% = STRING(Moms.MomsProc).
        ASSIGN
            ttPriKat.Vg = VarGr.Vg
            ttPriKat.Hg = VarGr.Hg
            .
    END.

    /* Konverteringstabell - Leverandør */
    {getvpitabell.i &LevNr = VPIFilHode.EkstVPILevNr
                    &Nr = 1000
                    &Felt = cImpTabell}
    FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = cImpTabell AND 
        ImpKonv.EksterntId = ttPriKat.LevNr NO-ERROR.
    IF AVAILABLE ImpKonv THEN
    DO:
        ASSIGN
            cTekst = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": Konvertert leverandør " + ttPriKat.LevNr + " til " + ImpKonv.InterntId + ".".
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "20").
        ASSIGN
          ttPriKat.iLevNr = int(ImpKonv.InterntId)
          .
    END.
    ELSE DO:
        ASSIGN
            ttPriKat.iLevNr = INT(ttPriKat.LevNr)
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO:
            CREATE tt_Error.
            ASSIGN
             ttPriKat.ErrFlag = TRUE
              tt_Error.LinjeNr = ttPriKat.LinjeNr
              tt_Error.Tekst   = "* LevNr: Ugyldige tegn i leverandørnr " + ttPriKat.LevNr + "."
              .
        END.
    END.
    /* Kontroll av leverandørnummer */ 
    IF NOT CAN-FIND(LevBas WHERE
                    LevBas.LevNr = ttPriKat.iLevNr) THEN
    DO:
        ASSIGN
            piAntFeil = piAntFeil + 1
            cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent leverandørnummer " + ttPriKat.LevNr + ".".
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "30").

        CREATE tt_Error.
        ASSIGN
         ttPriKat.ErrFlag = TRUE
          tt_Error.LinjeNr = ttPriKat.LinjeNr
          tt_Error.Tekst   = "* LevNr: Ukjent leverandørnummer " + ttPriKat.LevNr + "."
          .
    END.

    /* Konverteringstabell - Varegruppe */
    {getvpitabell.i &LevNr =VPIFilHode.EkstVPILevNr
                    &Nr = 1003
                    &Felt = cImpTabell}
    FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = cImpTabell AND 
        ImpKonv.EksterntId = ttPriKat.Varegruppe NO-ERROR.
    IF AVAILABLE ImpKonv THEN
    DO:
        ASSIGN
            cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": Konvertert varegruppe " + string(ttPriKat.VareGruppe) + " til " + ImpKonv.InterntId + ".".
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "20").
        ASSIGN
          ttPriKat.Vg = int(ImpKonv.InterntId)
          .
    END.
    /* Kontroll av varegruppe */
    IF NOT CAN-FIND(VarGr WHERE
                    VarGr.Vg = ttPriKat.Vg) THEN
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1            
          cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent varegruppe " + string(ttPriKat.Vg) + ".".
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "30").
        CREATE tt_Error.
        ASSIGN
          ttPriKat.ErrFlag = TRUE
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "* Varegruppe: * Ukjent varegruppe " + string(ttPriKat.Vg) + "."
          .
    END.

    /* Konverteringstabell - Moms */
    {getvpitabell.i &LevNr =VPIFilHode.EkstVPILevNr
                    &Nr = 1013
                    &Felt = cImpTabell}
    FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = cImpTabell AND 
        ImpKonv.EksterntId = pcMva% NO-ERROR.
    IF AVAILABLE ImpKonv THEN
    DO:
        ASSIGN
            cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": Konvertert mva% til mvakode " + string(pcMva%) + " til " + ImpKonv.InterntId + ".".
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "20").
        ASSIGN
          ttPriKat.MomsKod = int(ImpKonv.InterntId)
          .
    END.
    ELSE FIND FIRST Moms NO-LOCK WHERE
        Moms.MomsProc = DEC(pcMva%) NO-ERROR.
    IF AVAILABLE Moms THEN
        ASSIGN
        ttPriKat.MomsKod = Moms.MomsKod
        .
    ELSE DO:
        ASSIGN
            cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent mva% " + pcMva% + ".".
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "30").
        ASSIGN
          piAntFeil = piAntFeil + 1
          ttPriKat.MomsKod = 0
          .
        CREATE tt_Error.
        ASSIGN
          ttPriKat.ErrFlag = TRUE
          tt_Error.LinjeNr = ttPriKat.LinjeNr
          tt_Error.Tekst   = "* Mva: Ukjent mva% " + pcMva% + "."
          .
    END.

    /* Konverteringstabell - Farg */
    {getvpitabell.i &LevNr =VPIFilHode.EkstVPILevNr
                    &Nr = 1004
                    &Felt = cImpTabell}
    FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = cImpTabell AND 
        ImpKonv.EksterntId = ttPriKat.FargeKode NO-ERROR.
    IF AVAILABLE ImpKonv THEN
    DO:
        ASSIGN
            cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": Konvertert farge til fargekode " + ttPriKat.FargeKode + " til " + ImpKonv.InterntId + ".".
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "20").
        ASSIGN
          ttPriKat.Farg = int(ImpKonv.InterntId)
          piFarg  = 0
          .
    END.
    ELSE DO:
        ASSIGN
            ttPriKat.Farg  = INT(ttPriKat.FargeKode)
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO:
            CREATE tt_Error.
            ASSIGN
             ttPriKat.ErrFlag = TRUE
              tt_Error.LinjeNr = ttPriKat.LinjeNr
              tt_Error.Tekst   = "* Fargekode: Ugyldige tegn i fargekode " + ttPriKat.FargeKode + "."
              .
        END.
    END.
    /* Kontroll av fargekode */
    IF NOT CAN-FIND(Farg WHERE
                    Farg.Farg = ttPriKat.Farg OR ttPriKat.Farg = 0) THEN
    DO:
        FIND FIRST Farg NO-LOCK WHERE
            Farg.FarBeskr BEGINS trim(ttPriKat.FargeTekst) NO-ERROR.
        IF AVAILABLE Farg THEN
            ttPriKat.Farg = Farg.Farg.
        ELSE DO:
          IF ttPriKat.FargeKode <> ""  THEN
          DO:
              ASSIGN
                piAntFeil = piAntFeil
                cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent fargekode " + ttPriKat.FargeTekst + ".".
              PUBLISH "VPIFilLogg" (cTekst + chr(1) + "30").
              CREATE tt_Error.
              ASSIGN
                ttPriKat.ErrFlag = TRUE
                tt_Error.LinjeNr = piAntFeil
                tt_Error.Tekst   = "* Fargekode: Ukjent fargekode og fargekodetekst " + FargeKode + ", " + ttPriKat.FargeTekst + "."
                .
          END.
        END.
    END.

    /* Varemerke */
    FIND FIRST Varemerke NO-LOCK WHERE
        TRIM(Varemerke.KortNavn) BEGINS trim(ttPriKat.Varemerke) NO-ERROR.
    IF NOT AVAILABLE Varemerke THEN
        FIND FIRST VareMerke WHERE 
            TRIM(Varemerke.Beskrivelse) BEGINS TRIM(ttPriKat.Varemerke) NO-ERROR.
    IF NOT AVAILABLE Varemerke THEN
    OPPRETT-VAREMERKE:
    DO:
        FIND LAST Varemerke NO-LOCK NO-ERROR.
        IF AVAILABLE Varemerke THEN
            piInt = Varemerke.VmId + 1.
        ELSE
            piInt = 1.
        CREATE Varemerke.
        ASSIGN
            VareMerke.VmId        = piInt
            VareMerke.KortNavn    = TRIM(ttPriKat.Varemerke)
            Varemerke.Beskrivelse = TRIM(ttPriKat.Varemerke)
            .
        FIND CURRENT varemerke NO-LOCK.
    END. /*OPPRETT-VAREMERKE */
    IF AVAILABLE Varemerke THEN
        ttPriKat.VmId = VareMerke.VmId.

    /* Sjekker strtypeid                                            */
    /* Vi sjekker gjennom alle til vi finner det første som funker. */
    IF ttPriKat.StrTypeId <= 2 OR NOT CAN-FIND(StrType WHERE
                                               StrType.StrTypeId = ttPriKat.StrTypeId) THEN
    DO:
        pbFlagg = TRUE.
        STRSJEKK:
        FOR EACH StrType NO-LOCK WHERE StrType.StrTypeId >= 1:
            FOR EACH tmpVPIStrekkode NO-LOCK:
                IF NOT CAN-FIND(FIRST StrTstr OF StrType 
                            WHERE TRIM(StrTStr.SoStorl) = trim(tmpVPIStrekkode.Storl)) THEN
                    pbFlagg = FALSE.
            END.
            IF pbFlagg = TRUE THEN
            DO:
                ttPriKat.StrTypeId = StrType.StrTypeId.
                LEAVE STRSJEKK.
            END.
            ELSE
                pbFlagg = TRUE.
        END. /* STRSJEKK */
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
      IF ERROR-STATUS:ERROR  OR 
         (LENGTH(tmpVPISTrekkode.Kode) <> 13 AND TRIM(tmpVPISTrekkode.Kode) <> "") THEN
      EAN-128-KODER:
      DO:
          /* Her legger vi opp kodene. Det genereres o2 koder ved overføring til artbas. */
          FIND VpiStrekkode EXCLUSIVE-LOCK WHERE
              VpiStrekkode.EkstVpiLevNr = VPIArtBas.EkstVpiLevNr AND
              VpiStrekkode.VareNr       = VPIArtBas.VareNr AND
              VpiStrekkode.Kode         = tmpVPISTrekkode.Kode NO-ERROR.
          IF NOT AVAILABLE VpiStrekkode THEN
          DO:
              CREATE VpiStrekkode.
              BUFFER-COPY tmpVPIStrekkode TO VPIStrekkode
                  ASSIGN
                    VpiStrekkode.EkstVpiLevNr = VPIArtBas.EkstVpiLevNr
                    VpiStrekkode.VareNr       = VPIArtBas.VareNr 
                    VpiStrekkode.Kode         = tmpVPIStrekkode.Kode
                    VPIStrekkode.Kodetype     = 9
                  .
          END.
      END. /* EAN-128-KODER */
      ELSE 
      LAGRE-OK-EAN-KODE:
      DO:
          RUN bibl_chkEan.p (INPUT-OUTPUT tmpVPIStrekkode.Kode).
          /* Lagrer strekkoden. */
          FIND VpiStrekkode EXCLUSIVE-LOCK WHERE
              VpiStrekkode.EkstVpiLevNr = VPIArtBas.EkstVpiLevNr AND
              VpiStrekkode.VareNr       = VPIArtBas.VareNr AND
              VpiStrekkode.Kode         = tmpVPISTrekkode.Kode NO-ERROR.
          IF NOT AVAILABLE VpiStrekkode THEN
          DO:
              CREATE VpiStrekkode.
              BUFFER-COPY tmpVPIStrekkode TO VPIStrekkode
                  ASSIGN
                    VpiStrekkode.EkstVpiLevNr = VPIArtBas.EkstVpiLevNr
                    VpiStrekkode.VareNr       = VPIArtBas.VareNr 
                    VpiStrekkode.Kode         = tmpVPIStrekkode.Kode
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
    DEF VAR piLoop  AS INT NO-UNDO.
    DEF VAR pcTekst AS CHAR NO-UNDO.

    /* Kontroll av strTypeId */
    ASSIGN
        ttPriKat.StrTypeId = DEC(ttPriKat.StrTab)
        NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            ttPriKat.ErrFlag = TRUE
            tt_Error.LinjeNr = ttPriKat.LinjeNr
            tt_Error.Tekst   = "* StrTypeId: Ugyldige verdier i feltet. (" + ttPriKat.StrTab + ")."
            .
    END.
    /* Kontroll av varegruppe */
    ASSIGN
        ttPriKat.Vg = int(ttPriKat.VareGruppe)
        NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            ttPriKat.ErrFlag = TRUE
            tt_Error.LinjeNr = ttPriKat.LinjeNr
            tt_Error.Tekst   = "* Varegruppe: Ugyldige verdier i feltet. (" + ttPriKat.VareGruppe + ")."
            .
    END.
    
    /* Kontroll av fargekode */
    ASSIGN
        ttPriKat.Farg = int(ttPriKat.Fargekode)
        NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            ttPriKat.ErrFlag = TRUE
            tt_Error.LinjeNr = ttPriKat.LinjeNr
            tt_Error.Tekst   = "* Fargekode: Ugyldige verdier i feltet. (" + ttPriKat.Fargekode + ")."
            .
    END.

    /* Kontroll av EAN kode.                                   */
    /* Sjekker kun gyldige EAN koder. Koder med alfanumeriske  */
    /* egn, legges opp som ean 128.                            */
/*     ASSIGN                                                                                                         */
/*         lDec = dec(ttPriKat.EANnr)                                                                                 */
/*         NO-ERROR.                                                                                                  */
/*     IF ERROR-STATUS:ERROR = FALSE THEN                                                                             */
/*     EAN-SJEKK:                                                                                                     */
/*     DO:                                                                                                            */
/*         IF ttPriKat.EANnr <> "" THEN                                                                               */
/*         DO:                                                                                                        */
/*             IF LENGTH(ttPriKat.EANnr) <> 13 THEN                                                                   */
/*             DO:                                                                                                    */
/*                 CREATE tt_Error.                                                                                   */
/*                 ASSIGN                                                                                             */
/*                     ttPriKat.ErrFlag = TRUE                                                                        */
/*                     tt_Error.LinjeNr = ttPriKat.LinjeNr                                                            */
/*                     tt_Error.Tekst   = "* EANnr: Ugyldige EAN kode. Har ikke 13 siffer. (" + ttPriKat.EANnr + ")." */
/*                     .                                                                                              */
/*             END.                                                                                                   */
/*             ELSE DO:                                                                                               */
/*                 pcTekst = fixChkEan(substring(ttPriKat.EANnr,1,12)).                                               */
/*                 IF pcTekst <> ttPriKat.EANnr THEN                                                                  */
/*                 DO:                                                                                                */
/*                     CREATE tt_Error.                                                                               */
/*                     ASSIGN                                                                                         */
/*                         ttPriKat.ErrFlag = TRUE                                                                    */
/*                         tt_Error.LinjeNr = ttPriKat.LinjeNr                                                        */
/*                         tt_Error.Tekst   = "* EANnr: Ugyldige sjekksiffer i EAN kode. (" + ttPriKat.EANnr + ")."   */
/*                         .                                                                                          */
/*                 END.                                                                                               */
/*             END.                                                                                                   */
/*         END.                                                                                                       */
/*     END. /* EAN-SJEKK */                                                                                           */

    /* Kontroll av fargekode */
    ASSIGN
        lDec = dec(ttPriKat.StrTab)
        NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            ttPriKat.ErrFlag = TRUE
            tt_Error.LinjeNr = ttPriKat.LinjeNr
            tt_Error.Tekst   = "* StrTab: Ugyldige verdier i feltet. (" + ttPriKat.StrTab + ")."
            .
    END.


    /* Kontroll av enhet */
    IF NOT CAN-DO(cEnhet,ttPriKat.Enh) THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            ttPriKat.ErrFlag = TRUE
            tt_Error.LinjeNr = ttPriKat.LinjeNr
            tt_Error.Tekst   = "* Enhet: Ukjent enhet. (" + ttPriKat.Enh + ")."
            .
    END.

    /* Kontroll av antall i enhet */
    ASSIGN
        lDec = dec(ttPriKat.AntIEnh)
        NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            ttPriKat.ErrFlag = TRUE
            tt_Error.LinjeNr = ttPriKat.LinjeNr
            tt_Error.Tekst   = "* AntIEnh: Ugyldige verdier i feltet. (" + ttPriKat.AntIEnh + ")."
            .
    END.
    /* Kontroll av LevPrisEngros */
    ASSIGN
        lDec = dec(ttPriKat.LevPrisEngros)
        NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            ttPriKat.ErrFlag = TRUE
            tt_Error.LinjeNr = ttPriKat.LinjeNr
            tt_Error.Tekst   = "* LevPrisEngros: Ugyldige verdier i feltet. (" + ttPriKat.LevPrisEngros + ")."
            .
    END.
    /* Kontroll av forhåndsrabatt */
    ASSIGN
        lDec = dec(ttPriKat.forhRab%)
        NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            ttPriKat.ErrFlag = TRUE
            tt_Error.LinjeNr = ttPriKat.LinjeNr
            tt_Error.Tekst   = "* forhRab%: Ugyldige verdier i feltet. (" + ttPriKat.forhRab% + ")."
            .
    END.
    /* Kontroll av suppleringsrabatt */
    ASSIGN
        lDec = dec(ttPriKat.suppRab%)
        NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            ttPriKat.ErrFlag = TRUE
            tt_Error.LinjeNr = ttPriKat.LinjeNr
            tt_Error.Tekst   = "* suppRab%: Ugyldige verdier i feltet. (" + ttPriKat.suppRab% + ")."
            .
    END.
    /* Kontroll av veiledende pris */
    ASSIGN
        lDec = dec(ttPriKat.VeilPris)
        NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            ttPriKat.ErrFlag = TRUE
            tt_Error.LinjeNr = ttPriKat.LinjeNr
            tt_Error.Tekst   = "* VeilPris: Ugyldige verdier i feltet. (" + ttPriKat.VeilPris + ")."
            .
    END.
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
                ttPriKat.ErrFlag = TRUE
                tt_Error.LinjeNr = ttPriKat.LinjeNr
                tt_Error.Tekst   = "* " + pcTekst + ": Ugyldige verdier i feltet. (" + pcTekst + ")."
                .
        END.
        IF pcTekst <> "" AND LENGTH(pcTekst) <> 6 THEN
        DO:
            CREATE tt_Error.
            ASSIGN
                ttPriKat.ErrFlag = TRUE
                tt_Error.LinjeNr = ttPriKat.LinjeNr
                tt_Error.Tekst   = "* " + pcTekst + ": Feil angivelse av år og måned. (" + pcTekst + ")."
                .
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
                ttPriKat.ErrFlag = TRUE
                tt_Error.LinjeNr = ttPriKat.LinjeNr
                tt_Error.Tekst   = "* " + pcTekst + ": Ugyldige verdier i feltet. (" + pcTekst + ")."
                .
        END.
    END.

    /* Kontroll av Sesong */
    ASSIGN
        lDec = dec(ttPriKat.Sesong)
        ttPriKat.Sasong = INT(ttPriKat.Sesong)
        NO-ERROR.
    IF ERROR-STATUS:ERROR = TRUE THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            ttPriKat.ErrFlag = TRUE
            tt_Error.LinjeNr = ttPriKat.LinjeNr
            tt_Error.Tekst   = "* Sesong: Ugyldige verdier i feltet. (" + ttPriKat.Sesong + ")."
            .
    END.
    IF NOT CAN-FIND(Sasong WHERE
                    Sasong.SaSong = ttPriKat.Sasong) THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            ttPriKat.ErrFlag = TRUE
            tt_Error.LinjeNr = ttPriKat.LinjeNr
            tt_Error.Tekst   = "* Sesong: Ugyldige sesongkode i feltet. (" + ttPriKat.Sesong + ")."
            .
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
      /* Konverteringstabell - Størrelse */
      {getvpitabell.i &LevNr =VPIFilHode.EkstVPILevNr
                      &Nr = 1012
                      &Felt = cImpTabell}

      /* Lagrer strekkoden.                                                 */
      FIND tmpVpiStrekkode EXCLUSIVE-LOCK WHERE
          tmpVpiStrekkode.EkstVpiLevNr = VPIFilHode.EkstVpiLevNr AND
          tmpVpiStrekkode.VareNr       = string(ttPriKat.ArtikkelNr) AND
          tmpVpiStrekkode.Kode         = ttPriKat.EANnr NO-ERROR.
      IF NOT AVAILABLE tmpVpiStrekkode THEN
      DO:
          CREATE tmpVpiStrekkode.
          ASSIGN
              tmpVpiStrekkode.EkstVpiLevNr = VPIFilHode.EkstVpiLevNr
              tmpVpiStrekkode.VareNr       = STRING(ttPriKat.ArtikkelNr) 
              tmpVpiStrekkode.Kode         = ttPriKat.EANnr
              .
      END.
      ASSIGN
          tmpVpiStrekkode.Storl             = ttPriKat.Str
          tmpVpiStrekkode.EkstStorl         = ttPriKat.Str
          tmpVpiStrekkode.KodeType          = 1 /* EAN */
          .
      FIND FIRST ImpKonv NO-LOCK WHERE 
          ImpKonv.EDB-System = cEDB-System AND 
          ImpKonv.Tabell     = cImpTabell AND 
          ImpKonv.EksterntId = tmpVpiStrekkode.Storl NO-ERROR.
      IF AVAILABLE ImpKonv THEN
      DO:
          ASSIGN
              cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": Konvertert størrelse " + tmpVpiStrekkode.Storl + " til " + ImpKonv.InterntId + ".".
          PUBLISH "VPIFilLogg" (cTekst + chr(1) + "20").
          ASSIGN
            tmpVpiStrekkode.Storl = ImpKonv.InterntId
            .
      END.

      RUN FixStorl IN h_dproclib (INPUT-OUTPUT tmpVpiStrekkode.Storl). /* Størrelse i standard SkoTex */

      /* Kobler til størrelseskode. */
      FIND StrKonv NO-LOCK WHERE
          StrKonv.Storl = tmpVpiStrekkode.Storl NO-ERROR.
      IF AVAILABLE StrKonv THEN
          ASSIGN
          tmpVpiStrekkode.StrKode = StrKonv.StrKode
          .
      /* Kontroll av gyldig størrelse */
      ELSE DO:
          ASSIGN
            piAntFeil = piAntFeil + 1
            cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent størrelse " + tmpVpiStrekkode.Storl + ").".
          PUBLISH "VPIFilLogg" (cTekst + chr(1) + "30").
          CREATE tt_Error.
          ASSIGN
            tt_Error.LinjeNr = piAntFeil
            tt_Error.Tekst   = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent størrelse " + tmpVpiStrekkode.Storl + ")."
            .
      END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Registervalidering) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Registervalidering Procedure 
PROCEDURE Registervalidering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR pcTekst AS CHAR NO-UNDO.

VALIDER:
FOR EACH ttPriKat NO-LOCK:
    STATUS DEFAULT "Registervalidering - fillinje: " + STRING(ttPriKat.LinjeNr) + ".".
    /* Kontroll av varegruppe */
    IF NOT CAN-FIND(VarGr NO-LOCK WHERE
                    VarGr.Vg = ttPriKat.Vg) THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            ttPriKat.ErrFlag = TRUE
            tt_Error.LinjeNr = ttPriKat.LinjeNr
            tt_Error.Tekst   = "* Varegruppe: Ukjent varegruppe. (" + ttPriKat.VareGruppe + ")."
            .
    END.
    /* Kontroll av farge */
    IF NOT CAN-FIND(Farg NO-LOCK WHERE
                    Farg.Farg = ttPriKat.Farg) THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            ttPriKat.ErrFlag = TRUE
            tt_Error.LinjeNr = ttPriKat.LinjeNr
            tt_Error.Tekst   = "* Fargekode: Ukjent fargekode. (" + ttPriKat.Fargekode + ")."
            .
    END.

    pcTekst = ttPriKat.Str.
    RUN FixStorl IN h_dproclib (INPUT-OUTPUT pcTekst). /* Størrelse i standard SkoTex */
    /* Oppretter størrelsene hvis parameter tilsier dette. */
    IF bAvbrytVPI = FALSE AND
       NOT CAN-FIND(StrKonv WHERE
                    StrKonv.Storl = pcTekst)  THEN
    DO:
        FIND LAST bStrKonv NO-ERROR.
        CREATE StrKonv.
        ASSIGN
            StrKonv.StrKode = (IF AVAILABLE bStrKonv
                                 THEN bStrKonv.StrKode + 1
                                 ELSE 1)
            StrKonv.Storl   = pcTekst
            StrKonv.Merknad = "VPI import"
            .
        FIND CURRENT StrKonv NO-LOCK.
    END.
    /*Kontroll av størrelse */
    IF NOT CAN-FIND(StrKonv WHERE
                    StrKonv.Storl = pcTekst) THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            ttPriKat.ErrFlag = TRUE
            tt_Error.LinjeNr = ttPriKat.LinjeNr
            tt_Error.Tekst   = "* Str: Ukjent størrelse. (" + ttPriKat.Str + ")."
            .
    END.
    /* Kontroll av størrelsestype */
    IF ttPriKat.StrTab <> "" AND 
        NOT CAN-FIND(StrType NO-LOCK WHERE
                     StrType.StrTypeId = int(ttPriKat.StrTab)) THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            ttPriKat.ErrFlag = TRUE
            tt_Error.LinjeNr = ttPriKat.LinjeNr
            tt_Error.Tekst   = "* StrTab: Ukjent størrelsestype. (" + ttPriKat.StrTab + ")."
            .
    END.
/*     /*Kontroll av varemerke */                                                                */
/*     IF NOT CAN-FIND(FIRST VareMerke WHERE                                                     */
/*                     Varemerke.Beskrivelse = ttPriKat.Varemerke) OR                            */
/*        NOT CAN-FIND(FIRST VareMerke WHERE                                                     */
/*                     Varemerke.KortNavn = ttPriKat.VareMerke) THEN                             */
/*     DO:                                                                                       */
/*         CREATE tt_Error.                                                                      */
/*         ASSIGN                                                                                */
/*             ttPriKat.ErrFlag = TRUE                                                           */
/*             tt_Error.LinjeNr = ttPriKat.LinjeNr                                               */
/*             tt_Error.Tekst   = "* VareMerke: Ukjent varemerke. (" + ttPriKat.VareMerke + ")." */
/*             .                                                                                 */
/*     END.                                                                                      */
    /* Kontroll av valuta */
    IF NOT CAN-FIND(Valuta WHERE Valuta.ValKod = ttPriKat.ValKod) THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            ttPriKat.ErrFlag = TRUE
            tt_Error.LinjeNr = ttPriKat.LinjeNr
            tt_Error.Tekst   = "* ValKod: Ukjent valutakode. (" + ttPriKat.ValKod + ")."
            .
    END.
    /* Kontroll av sesong */
    IF NOT CAN-FIND(Sasong NO-LOCK WHERE
                    Sasong.Sasong = int(lDec)) THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            ttPriKat.ErrFlag = TRUE
            tt_Error.LinjeNr = ttPriKat.LinjeNr
            tt_Error.Tekst   = "* Sesong: Ukjent sesong. (" + ttPriKat.Sesong + ")."
            .
    END.

END. /* VALIDER */

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
DEF VAR pcEANnrLst  AS CHAR NO-UNDO.
DEF VAR plArtikkelNr AS DEC  NO-UNDO.
DEF VAR oldplArtikkelNr AS DEC  NO-UNDO.
DEF VAR pcStrLst     AS CHAR NO-UNDO.
DEF VAR piAntall     AS INT NO-UNDO.

oldplArtikkelNr = 0. 
/* Behandler fillinjene og lager modell der hvor vi klarer det. */
VPIFILLINJE:
FOR EACH ttPriKat WHERE ttPriKat.ArtikkelNr = 0 TRANSACTION
    BREAK 
    BY ttPriKat.LevModellNr  /* Modell                  */
    BY ttPriKat.VareTekst    /* Varetekst               */
    BY ttPriKat.FargeTekst   /* Leverandørs fargetekst  */ 
    BY ttPriKat.Markedspris: /* Anbefalt pris.          */

    ASSIGN
        piantall = piAntall + 1
        .
    STATUS DEFAULT "Setter artikkelnr. - fillinje: " + STRING(ttPriKat.LinjeNr) + ". (" + STRING(piAntall) + ")".

    /* Ny artikkel */
    IF FIRST-OF(ttPriKat.Markedspris) THEN
        ASSIGN
        pcEANnrLst = ""
        pcStrLst    = ""
        .

    /* Logger EANnr i artikkel */
    IF ttPriKat.EANnr <> "" THEN
    ASSIGN
        pcEANnrLst = pcEANnrLst + 
                      (IF pcEANnrLst = ""
                         THEN ""
                         ELSE CHR(1)) + 
                      ttPriKat.EANnr.

    /* Logger størrelsene for artikkelen */
    IF NOT CAN-DO(pcStrLst,ttPriKat.Str) THEN
        ASSIGN
        pcStrLst = pcStrLst +
                   (IF pcStrLst = ""
                      THEN ""
                      ELSE ",") +
                   ttPriKat.Str
        .

    /* Siste posten i en artikkel.                             */
    /* Hvis vi klarer å sette sammen en artikkel, gjør vi det. */
    IF LAST-OF(ttPriKat.Markedspris) THEN
    BRYTGRYUPPE:
    DO: 
        plArtikkelNr = 0.
        /* Sjekker om artikkelen finnes fra før. Sjekker alle ean koder på varianten. */
        STREKKODE:
        DO piLoop = 1 TO NUM-ENTRIES(pcEANnrLst,CHR(1)):
            IF ENTRY(1,pcEANnrLst,CHR(1)) = "" THEN
                NEXT STREKKODE.

            FIND Strekkode NO-LOCK WHERE
                Strekkode.Kode = ENTRY(1,pcEANnrLst,CHR(1)) NO-ERROR.
            IF AVAILABLE Strekkode THEN
            DO:
                plArtikkelNr = Strekkode.ArtikkelNr.
                LEAVE STREKKODE.
            END.
        END. /* STREKKODE */

        /* Sjekker VPI register for aktuell leverandør */
        IF plArtikkelNr = 0 THEN VPISTREKKODE:
        DO piLoop = 1 TO NUM-ENTRIES(pcEANnrLst,CHR(1)):
            IF ENTRY(1,pcEANnrLst,CHR(1)) = "" THEN
                NEXT VPISTREKKODE.

            FIND VPIStrekkode NO-LOCK WHERE
                VPIStrekkode.EkstVPILevNr = ttPriKat.EkstVPILevNr AND
                VPIStrekkode.Kode = ENTRY(1,pcEANnrLst,CHR(1)) NO-ERROR.
            IF AVAILABLE VPIStrekkode THEN
            DO:
                plArtikkelNr = dec(VPIStrekkode.VareNr).
                LEAVE VPISTREKKODE.
            END.
        END. /* VPISTREKKODE */

        /* Oppslag på varetekst - nest siste sjangse - og for blanke EAN koder. */
        IF plArtikkelNr = 0 THEN
        VARETEKST:
        DO:
            FIND FIRST VPIArtBas EXCLUSIVE-LOCK WHERE
                VPIArtBas.EkstVPILevNr = ttPriKat.EkstVPILevNr AND
                VPIArtBas.LevKod       = ttPriKat.LevModellNr AND
                VPIArtBas.Beskr        = ttPriKat.VareTekst   AND
                VPIArtBas.LevFargKod   = ttPriKat.FargeTekst NO-ERROR.
            IF AVAILABLE VPIArtBas THEN
                plArtikkelNr = dec(VPIArtBas.VareNr).
        END. /* VARETEKST */

        /* Oppslag på varetekst - siste sjangse - og for blanke EAN koder. */
        IF plArtikkelNr = 0 THEN
        ARTBASVARETEKST:
        DO:
            FIND FIRST ArtBas EXCLUSIVE-LOCK WHERE
                ArtBas.LevKod       = ttPriKat.LevModellNr AND
                ArtBas.Beskr        = ttPriKat.VareTekst   AND
                ArtBas.LevFargKod   = ttPriKat.FargeTekst NO-ERROR.
            IF AVAILABLE ArtBas THEN
                plArtikkelNr = dec(ArtBas.ArtikkelNr).
        END. /* ARTBASVARETEKST */

        IF plArtikkelNr = 0 THEN
        NYTT-ARTIKKELNR:
        DO:
            plArtikkelNr = oldplArtikkelNr.
            RUN genVpiArtikkelnr.p (INPUT-OUTPUT plArtikkelNr).
            oldPlArtikkelNr = plArtikkelNr.
            /* Full nummerserie */
            IF plArtikkelNr = 0 THEN
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
        IF plArtikkelNr > 0 THEN
        DO:
            ASSIGN
                ttPriKat.ArtikkelNr = plArtikkelNr
                .
            SETT-ARTIKKELNR:
            DO piLoop = 1 TO NUM-ENTRIES(pcEANnrLst,CHR(1)):
                FOR EACH buf-ttPriKat EXCLUSIVE-LOCK WHERE
                    buf-ttPriKat.EkstVPILevNr = VPIFilHode.EkstVPILevNr AND
                    buf-ttPriKat.EANnr        = ENTRY(piLoop,pcEANnrLst,CHR(1)):

                    /* Setter på artikkelNr. */
                    ASSIGN
                        buf-ttPriKat.ArtikkelNr = plArtikkelNr
                        .
                END.
            END. /* SETT-ARTIKKELNR */
        END.
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
DEF VAR piLoop       AS INT  NO-UNDO.
DEF VAR pcEANnrLst  AS CHAR NO-UNDO.
DEF VAR plArtikkelNr AS DEC  NO-UNDO.
DEF VAR pcModell     AS CHAR NO-UNDO.
DEF VAR piAntall     AS INT  NO-UNDO.

/* Behandler fillinjene og lager modell der hvor vi klarer det. */
VPIFILLINJE:
FOR EACH ttPriKat WHERE ttPriKat.ArtikkelNr > 0 TRANSACTION
    BREAK 
    BY ttPriKat.LevModellNr  /* Modell    */
    BY ttPriKat.VareTekst: /* Varetekst */

    piAntall = piAntall + 1.
    STATUS DEFAULT "Setter ModellFarge - fillinje: " + STRING(ttPriKat.LinjeNr) + ". (" + STRING(piAntall) + ")".

    /* Ny modell */
    IF FIRST-OF(ttPriKat.VareTekst) THEN
        ASSIGN
        pcEANnrLst = ""
        pcModell    = ""
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
        FOR EACH buf-ttPriKat EXCLUSIVE-LOCK WHERE
            buf-ttPriKat.EkstVPILevNr = VPIFilHode.EkstVPILevNr AND
            buf-ttPriKat.EANnr        = ENTRY(piLoop,pcEANnrLst,CHR(1)):
            /* Logger siste brukte artikkelNr */
            IF plArtikkelNr  = 0 THEN 
                plArtikkelNr = buf-ttPriKat.ArtikkelNr.

            /* Setter på modellnummer hvis det er flere farger. */
            IF NUM-ENTRIES(pcModell) > 2 THEN
            ASSIGN
                buf-ttPriKat.ModellFarge      = plArtikkelNr
                buf-ttPriKat.HovedModellFarge = (IF piLoop =1 THEN TRUE ELSE FALSE)
                .
            ELSE
                ASSIGN
                    buf-ttPriKat.ModellFarge = 0
                    .
        END.
    END.
END. /* VPIFILLINJE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivTilLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivTilLogg Procedure 
PROCEDURE SkrivTilLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.

  OUTPUT STREAM Ut TO VALUE(VPIFilHode.Katalog + "\" + "ERR-" + VPIFilHode.FilNavn) NO-ECHO.

  FOR EACH ttPriKat 
      BY ttPriKat.LinjeNr:

      pcTekst = "".
      FOR EACH tt_Error WHERE
          tt_Error.LinjeNr = ttPriKat.LinjeNr:
          pcTekst = pcTekst + 
                    (IF pcTekst = ""
                       THEN ""
                       ELSE ";") + 
                     "Linje " + string(tt_error.LinjeNr) + ": " + tt_Error.Tekst.
      END.

      EXPORT STREAM ut DELIMITER ";"
          ttPriKat.R1            
          ttPriKat.LevNr         
          ttPriKat.LevModellNr   
          ttPriKat.EANnr         
          ttPriKat.VareTekst     
          ttPriKat.FargeKode     
          ttPriKat.FargeTekst    
          ttPriKat.Str           
          ttPriKat.StrTab        
          ttPriKat.Varemerke     
          ttPriKat.Enh           
          ttPriKat.AntIEnh       
          ttPriKat.LevPrisEngros 
          ttPriKat.ValKod        
          ttPriKat.forhRab%      
          ttPriKat.suppRab%      
          ttPriKat.VeilPris      
          ttPriKat.PAKstru       
          ttPriKat.LevUke1       
          ttPriKat.LevUke2       
          ttPriKat.LevUke3       
          ttPriKat.LevUke4       
          ttPriKat.VareGruppe    
          ttPriKat.LevNavn       
          ttPriKat.nettoForh     
          ttPriKat.kalkForh      
          ttPriKat.BFforh        
          ttPriKat.nettoSupp     
          ttPriKat.kalkSupp      
          ttPriKat.BFsupp        
          ttPriKat.MarkedsPris   
          ttPriKat.Sortiment     
          ttPriKat.Sesong        
          ttPriKat.VPIBildeKode     
          ttPriKat.Merknad       
          ttPriKat.ArtikkelNr
          ttPriKat.ModellFarge
          pcTekst
          .
  END.

  OUTPUT STREAM Ut CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettVpi) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettVpi Procedure 
PROCEDURE SlettVpi :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
DEF VAR piAntall AS INT NO-UNDO.

ASSIGN
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

/* Behandler linjene i filen. */
VPIFILLINJE:
FOR EACH ttPriKat TRANSACTION
    BREAK 
    BY ttPriKat.ArtikkelNr
    BY ttPriKat.FargeTekst:

    piAntall = piAntall + 1.
    STATUS DEFAULT "Pakker ut VPI - fillinje: " + STRING(ttPriKat.LinjeNr) + ". (" + STRING(piAntall) + ")".

    ASSIGN
        dcValPris      = 0
        dcInnPris      = 0  
        dcUtpris       = 0  
        piSasong       = 0
        cEndelse       = ""
        piLoop1        = piLoop1 + 1
        iTotAntLinjer  = iTotAntLinjer + 1
        .

    /* Sikker konvertering eller 0 i kode. */
    ASSIGN
        piSasong = INT(ttPriKat.Sesong) 
        NO-ERROR.

    /* Håndterer Strekkodene */
    RUN LogStrekkoder.

    /* Legger opp den siste posten i brytgruppen. */
    IF LAST-OF(ttPriKat.FargeTekst) THEN
    BRYTGRYUPPE:
    DO:
/*         /* Tildeler temporært Artikkelnummer */                                            */
/*         IF VPIFilHode.EkstVpiLevNr > 100 THEN                                              */
/*             RUN trg/genVPIArtikkelnr (INPUT VPIFilHode.EkstVpiLevNr, OUTPUT plArtikkelNr). */
        plArtikkelNr = ttPriKat.ArtikkelNr.

        /* Konverterer data og initierer variabler. */
        RUN Konverter.

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
                NO-ERROR.
        END.
        ELSE
            ASSIGN
                piAntOppdat = piAntOppdat + 1
                .

/*         FIND Strekkode NO-LOCK WHERE                    */
/*             Strekkode.Kode = VPIArtBas.VareNr NO-ERROR. */

        ASSIGN
            rArtBasRecid           = RECID(VPIArtBas)
            VPIArtBas.Vg           = ttPriKat.Vg
            VPIArtBas.Hg           = ttPriKat.Hg
            VPIArtBas.VgKat        = 1
            VPIArtBas.LopNr        = ?
            VPIArtBas.Storrelser   = TRUE
            VPIArtBas.Beskr        = ttPriKat.Varetekst
            VPIArtBas.BongTekst    = ttPriKat.Varetekst  
            VPIArtBas.LevKod       = ttPriKat.LevModellNr   
            VPIArtBas.LevNr        = ttPriKat.iLevNr      
            VPIArtBas.Notat        = ttPriKat.FargeTekst  + chr(1) + 
                                     ttPriKat.Str + CHR(1) + ttPriKat.Fargekode + 
                                     (IF piVmId = 0
                                        THEN CHR(10) + "Varemerke: " + ttPriKat.Varemerke
                                        ELSE "") +
                                     (IF ttPriKat.Merknad <> "" THEN
                                        CHR(10) + "Merknad: " + ttPriKat.Merknad
                                        ELSE "")  
            VPIArtBas.Farg         = ttPriKat.Farg
            VPIArtBas.StrTypeId    = IF ttPriKat.StrTypeId <= 1
                                       THEN piStrTypeId /* Setter 2 som default. */
                                       ELSE ttPriKat.StrTypeId /* Beholder det som er lagt inn i filen */
            VPIArtBas.Sasong       = ttPriKat.Sasong
            VPIArtBas.SaSong       = IF VPIArtBas.SaSong = 0
                                       THEN piSasong
                                       ELSE VPIArtBas.Sasong
            VPIArtBas.ValKod       = ttPriKat.Valkod
            /*
            VPIArtBas.ArtikkelNr   = IF AVAILABLE Strekkode
                                       THEN Strekkode.ArtikkelNr
                                       ELSE VPIArtBas.ArtikkelNr
            */
            VPIArtBas.LevFargKod   = ttPriKat.FargeTekst
            VPIArtBas.AnbefaltPris = DEC(ttPriKat.Markedspris)
            VPIArtBas.VmId         = ttPriKat.VmId
            VPIArtBas.AnonseArtikkel = (IF ttPriKat.Sortiment = "1"
                                          THEN TRUE
                                          ELSE FALSE)
            VPIArtBas.LinjeMerknad = ttPriKat.Merknad + 
                                     (IF ttPriKat.Merknad <> ""
                                        THEN CHR(13)
                                        ELSE "")  
            VPIArtBas.ManRabIKas     = TRUE
            VPIArtBas.ModellFarge    = ttPriKat.ModellFarge
            VPIArtBas.HovedModellFarge = ttPriKat.HovedModellFarge
            VPIArtBas.KatalogPris[1] = DEC(ttPriKat.LevPrisEngros)
            VPIArtBas.forhRab%[1]    = DEC(ttPriKat.forhRab%)
            VPIArtBas.suppRab%[1]    = DEC(ttPriKat.suppRab%)
            VPIArtBas.VpiDato      = TODAY
            VPIArtBas.VPIBildeKode = ttPriKat.VPIBildeKode
            pbStatus               = TRUE
            .
      /* Setter leveringsdatofeltene */
      IF ttPriKat.LevUke1 <> "" AND LENGTH(ttPriKat.LevUke1) = 6 THEN
          VPIArtBas.LevDato1 = DATE(1,1, INT(SUBSTRING(ttPriKat.LevUke1,1,4)) ) + (7 * INT(SUBSTRING(ttPriKat.LevUke1,5,2))).       
      IF ttPriKat.LevUke2 <> "" AND LENGTH(ttPriKat.LevUke2) = 6 THEN
          VPIArtBas.LevDato2 = DATE(1,1, INT(SUBSTRING(ttPriKat.LevUke2,1,4)) )  + (7 * INT(SUBSTRING(ttPriKat.LevUke1,5,2))).       
      IF ttPriKat.LevUke3 <> "" AND LENGTH(ttPriKat.LevUke3) = 6 THEN
          VPIArtBas.LevDato3 = DATE(1,1, INT(SUBSTRING(ttPriKat.LevUke3,1,4)) )  + (7 * INT(SUBSTRING(ttPriKat.LevUke1,5,2))).       
      IF ttPriKat.LevUke4 <> "" AND LENGTH(ttPriKat.LevUke4) = 6 THEN
          VPIArtBas.LevDato4 = DATE(1,1, INT(SUBSTRING(ttPriKat.LevUke4,1,4)) )  + (7 * INT(SUBSTRING(ttPriKat.LevUke1,5,2))).       

      /* Henter Valuta for omregning av rabatten. */
      FIND Valuta OF VPIArtBas NO-LOCK NO-ERROR.

      /* Taster valutapris */
      ASSIGN
        dcValPris  = DEC(ttPriKat.LevPrisEngros)
        dcUtpris   = DEC(ttPriKat.MarkedsPris)
        dcRabatt   = ((dcValPris * DEC(ttPriKat.forhRab%)) / 100)
        NO-ERROR.

      IF ERROR-STATUS:ERROR = TRUE THEN
      DO:
          CREATE tt_Error.
          ASSIGN
              ttPriKat.ErrFlag = TRUE
              tt_Error.LinjeNr = ttPriKat.LinjeNr
              tt_Error.Tekst   = "* Kalkulasjon: Ugyldige verdier i et av kalkylefeltene. (" + ttPriKat.LevPrisEngros + ", " + ttPriKat.Markedspris + ", " + ttPriKat.forhRab% + ")."
              .
      END.

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
      RUN Kalkulasjon ("Rab1",   INPUT-OUTPUT pcSkjerm). /* Enter i rabatt1    */
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
          VPIArtPris.ProfilNr     = iProfilNr NO-ERROR.
      IF NOT AVAILABLE VPIArtPris THEN
      DO:
          CREATE VPIArtPris.
          ASSIGN
              VPIArtPris.EkstVpiLevNr = VPIFilHode.EkstVPILevNr 
              VPIArtPris.VareNr       = STRING(ttPriKat.ArtikkelNr) 
              VPIArtPris.ProfilNr     = iProfilNr
              .
      END.
      ASSIGN
          VPIArtPris.Tilbud                 = IF piTilbud = 1
                                                THEN FALSE
                                                ELSE TRUE
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
          VPIArtPris.Pris[piTilbud]         = dec(ENTRY(18,pcSkjerm,";"))
          VPIArtPris.EuroPris[piTilbud]     = dec(ENTRY(19,pcSkjerm,";"))
          VPIArtPris.EuroManuel             = FALSE
          VPIArtPris.AktivFraDato           = DATE(ENTRY(21,pcSkjerm,";"))
          VPIArtPris.AktivFraTid            = INT(ENTRY(22,pcSkjerm,";"))
          .

      /* Lagrer strekkoder og tømmer loggen */
      RUN LagreStrekkoder.

    END. /* BRYTGRYUPPE */

END. /* TRANSACTION - VPIFILLINJE */

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

STATUS DEFAULT "".

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
    pdAktDato =  TODAY - 1
    pcTekst   =   
      /*string(input FI-ValPris) */ STRING(dcValPris) + ";" +
      /*string(input FI-InnPris) */ string(dcInnPris) + ";" +
      /*string(input FI-Rab1)    */ string(dcRabatt)  + ";" +
      /*string(input FI-Rab1%)   */ "0" + ";" +
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

