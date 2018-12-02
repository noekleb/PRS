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

DEF TEMP-TABLE ttPriKat NO-UNDO
    FIELD R1 AS CHAR 
    FIELD LevNr AS CHAR 
    FIELD LevModellNr AS CHAR
    FIELD EANnr AS CHAR 
    FIELD VareTekst AS CHAR
    FIELD FargeKode AS CHAR 
    FIELD FargeTekst AS CHAR 
    FIELD Str        AS CHAR
    FIELD StrTab AS CHAR 
    FIELD Varemerke AS CHAR
    FIELD Enh AS CHAR
    FIELD AntIEnh AS CHAR
    FIELD LevPrisEngros AS CHAR
    FIELD ValKod AS CHAR
    FIELD forhRab% AS CHAR
    FIELD suppRab% AS CHAR
    FIELD VeilPris AS CHAR
    FIELD PAKstru AS CHAR
    FIELD LevUke1 AS CHAR
    FIELD LevUke2 AS CHAR
    FIELD LevUke3 AS CHAR
    FIELD LevUke4 AS CHAR
    FIELD VareGruppe AS CHAR
    FIELD LevNavn AS CHAR
    FIELD nettoForh AS CHAR
    FIELD kalkForh AS CHAR
    FIELD BFforh AS CHAR
    FIELD nettoSupp AS CHAR
    FIELD kalkSupp AS CHAR
    FIELD BFsupp AS CHAR 
    FIELD MarkedsPris AS CHAR
    FIELD Sortiment AS CHAR
    FIELD Sesong AS CHAR
    FIELD VPIBildeKode AS CHAR
    FIELD Merknad AS CHAR
    /* Tilleggsfelter */
    FIELD ErrFlag AS LOG
    FIELD LinjeNr AS INT
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    FIELD ModellNr LIKE ArtBas.Modell
    FIELD ModellFarge LIKE ArtBas.ModellFarge
    FIELD HovedModellFarge LIKE ArtBas.HovedModellFarge
    FIELD EkstVPILevNr LIKE VPIArtBas.EkstVPILevNr
    FIELD Vg AS INT
    FIELD Hg AS INT
    FIELD Farg AS INT
    FIELD StrTypeId LIKE StrType.StrTypeId
    FIELD iLevNr LIKE LevBas.LevNr
    FIELD MomsKod AS INT
    FIELD VmId LIKE Varemerke.VmId
    FIELD Sasong AS INT
    INDEX ArtikkelNr
        LevModellNr
        VareTekst
        FargeTekst
    INDEX VPIUtpakk
        ArtikkelNr
        FargeTekst
    INDEX LinjeNr
        LinjeNr
    .

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

FOR EACH tt_Error:
  DELETE tt_Error.
END.

/* Start av procedurebibliotek */
IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN prisko.p PERSISTENT SET h_PrisKo.

/* Sjekker om det kjøres på en LapTop */
if valid-handle(h_dproclib) then
  run SjekkLapTop in h_dproclib (output lLapTop).
ELSE 
  lLapTop = FALSE.

/* Filhode. */
FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    MESSAGE "Ingen VPIFilHode tilgjengelig"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

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

RUN UtpakkVpi.

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
      "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn skip
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
    cTekst  = "Starter utpakking av Pakkseddel.".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

DO TRANSACTION:
  FIND CURRENT VPIDatasett EXCLUSIVE-LOCK.
  ASSIGN
      VPIDatasett.DatasettStatus = 5 /* VPI mottatt og behandlet */
      .
  FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
  ASSIGN
      VPIFilHode.VPIFilStatus = 5
      .

  FIND CURRENT VPIDatasett NO-LOCK.
  FIND CURRENT VPIFilHode  NO-LOCK.
END. /* TRANSACTION */

STATUS DEFAULT "".

ASSIGN
    cTekst = "Utpakking av Pakkseddel ferdig.Tidsbruk " + STRING(TIME - piTid,"HH:MM:SS") + ".".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

