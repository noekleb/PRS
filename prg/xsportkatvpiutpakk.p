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

DEF VAR cEDB-System        LIKE ImpKonv.EDB-System NO-UNDO.
DEF VAR cImpTabell         LIKE ImpKonv.Tabell     NO-UNDO.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .
{windows.i}

DEF VAR piAntFeil     AS INT  NO-UNDO.

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



/* Utpakking av VPI til VPI bufferet. */
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

&IF DEFINED(EXCLUDE-Kalkulasjon) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kalkulasjon Procedure 
PROCEDURE Kalkulasjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  def INPUT        PARAMETER pcFraFelt  as char no-undo.
  DEF INPUT-OUTPUT PARAMETER pcSkjerm   AS CHAR NO-UNDO.
  
  def var pcFeltListe as char no-undo.
  def var piFeltNr    as int  no-undo.
  DEF VAR lTilbud     AS LOG  NO-UNDO.
  DEF VAR lDirekte    AS LOG  NO-UNDO.

  DEF BUFFER bArtBas FOR VPIArtBas.
  DEF BUFFER bVarGr  FOR VarGr.
  DEF BUFFER bMoms   FOR Moms.
  DEF BUFFER bValuta FOR Valuta.

  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN prisko.p PERSISTENT SET h_PrisKo.

  assign
    lTilbud     = FALSE
    lDirekte    = FALSE
    pcFeltListe = "ValPris,InnPris,Rab1,Rab1%,Rab2,Rab2%,Frakt,Frakt%," + 
                  "DivKost,DivKost%,Rab3,Rab3%,VareKost,DB,DB%," +
                  "FI-Mva,FI-Mva%,Pris,EU-Pris".
                 
  /* Finner i hvilket felt markøren sto når prosedyren ble kalt. */
  assign
    /*pcFraFelt = substring(pcFraFelt,4)*/
    piFeltNr  = lookup(pcFraFelt,pcFeltListe)
    .

  /* Ukjent felt. */  
  if piFeltNr = 0 then
    do:
      message "Ukjent felt!" view-as alert-box title "Kalkylefeil".
      return no-apply.  
    end.

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
  run Omregning in h_PrisKo
       (input rArtBasRecid, 
        input iProfilNr,
        input-output pcSkjerm,
        input bMoms.MomsProc,
        input bValuta.ValKurs, 
        input piFeltNr,
        INPUT lTilbud).
  
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

DEF BUFFER bMoms FOR Moms.

ASSIGN
    piTid  = TIME
    cTekst = "Starter utpakking av VPI.".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

/* Aldri tilbud */
ASSIGN
    piTilbud = 1
    pbStatus = FALSE
    .

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
FIND FIRST StrType NO-LOCK NO-ERROR.
IF AVAILABLE StrType THEN
    piStrTypeId = StrType.StrTypeId.
ELSE
    piStrTypeId = 0.

/* Benytter alltid første sesongen. */
FIND FIRST Sasong NO-LOCK NO-ERROR.
IF AVAILABLE Sasong THEN
    piSasong = SaSong.Sasong.
ELSE
    piSasong = 0.

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

/* Behandler fillinjene */
VPIFILLINJE:
FOR EACH VPIFilLinje OF VPIFilHode TRANSACTION:
    ASSIGN
        dcValPris     = 0
        dcInnPris     = 0  
        dcUtpris      = 0  
        cEndelse      = ""
        piLoop1       = piLoop1 + 1
        iTotAntLinjer = iTotAntLinjer + 1
        pcVareNavn    = trim(ENTRY( 2,VPIFilLinje.StorTekst,";"),'"')
        pcBongTekst   = trim(ENTRY( 4,VPIFilLinje.StorTekst,";"),'"')
        pcVareNr      = trim(ENTRY( 5,VPIFilLinje.StorTekst,";"),'"')
        pcBestKode    = trim(ENTRY( 6,VPIFilLinje.StorTekst,";"),'"')
        pcHg          = trim(ENTRY( 9,VPIFilLinje.StorTekst,";"),'"')
        pcVg          = trim(ENTRY(10,VPIFilLinje.StorTekst,";"),'"')
        pcUgr         = trim(ENTRY(11,VPIFilLinje.StorTekst,";"),'"')
        pcMerke       = trim(ENTRY(12,VPIFilLinje.StorTekst,";"),'"')
        pcLevNr       = trim(ENTRY(13,VPIFilLinje.StorTekst,";"),'"')
        pcFormFarge   = trim(ENTRY(14,VPIFilLinje.StorTekst,";"),'"')
        pcForm        = trim(ENTRY(15,VPIFilLinje.StorTekst,";"),'"')
        pcFarge       = trim(ENTRY(16,VPIFilLinje.StorTekst,";"),'"')
        pcBasPris     = trim(ENTRY(18,VPIFilLinje.StorTekst,";"),'"')
        pcPris        = trim(ENTRY(19,VPIFilLinje.StorTekst,";"),'"')
        pcMva%        = trim(ENTRY(20,VPIFilLinje.StorTekst,";"),'"')
        pcF-Pris      = trim(ENTRY(28,VPIFilLinje.StorTekst,";"),'"')
        pcS-Pris      = trim(ENTRY(29,VPIFilLinje.StorTekst,";"),'"')
        .

    /* Melding til brukeren. */
    IF piLoop1 MODULO 50 = 0 THEN
        STATUS DEFAULT "Behandler linje " + STRING(iTotAntLinjer) + ".".

    /* Konverteringstabell - Leverandør */
    {getvpitabell.i &LevNr = VPIFilHode.EkstVPILevNr
                    &Nr = 1000
                    &Felt = cImpTabell}
    FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = cImpTabell AND 
        ImpKonv.EksterntId = pcLevNr NO-ERROR.
    IF AVAILABLE ImpKonv THEN
    DO:
        ASSIGN
            cTekst = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": Konvertert leverandør " + pcLevNr + " til " + ImpKonv.InterntId + ".".
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "20").
        ASSIGN
          piLevNr = int(ImpKonv.InterntId)
          .
    END.
    ELSE
        ASSIGN
            piLevNr = INT(pcLevNr).
    /* Kontroll av leverandørnummer */ 
    IF NOT CAN-FIND(LevBas WHERE
                    LevBas.LevNr = piLevNr) THEN
    DO:
        ASSIGN
            piAntFeil = piAntFeil + 1
            cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent leverandørnummer " + pcLevNr + ".".
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "30").

        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent leverandørnummer " + pcLevNr + "."
          .
    END.
    /* Konverterer verdier. */
    ASSIGN
        pcLevKod = pcBestKode
        piVg     = INT(STRING(INT(pcHg),"9") + 
                       STRING(INT(pcVg),"9") +
                       STRING(INT(pcUgr),"99"))
        /* HG overstyres nedenfor hvis varegruppen finnes. */
        piHg     = INT(STRING(INT(pcHg),"9") + 
                       STRING(INT(pcVg),"9"))
        NO-ERROR.

    /* Konverteringstabell - Varegruppe */
    {getvpitabell.i &LevNr =VPIFilHode.EkstVPILevNr
                    &Nr = 1003
                    &Felt = cImpTabell}
    FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = cImpTabell AND 
        ImpKonv.EksterntId = string(piVg) NO-ERROR.
    IF AVAILABLE ImpKonv THEN
    DO:
        ASSIGN
            cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": Konvertert varegruppe " + string(piVg) + " til " + ImpKonv.InterntId + ".".
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "20").
        ASSIGN
          piVg = int(ImpKonv.InterntId)
          .
    END.
    /* Kontroll av varegruppe */
    IF NOT CAN-FIND(VarGr WHERE
                    VarGr.Vg = piVg) THEN
    DO:
        ASSIGN
          piAntFeil = piAntFeil + 1            
          cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent varegruppe " + string(piVg) + ".".
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "30").
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent varegruppe " + string(piVg) + "."
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
          piMomsKod = int(ImpKonv.InterntId)
          .
    END.
    ELSE FIND FIRST Moms NO-LOCK WHERE
        Moms.MomsProc = DEC(pcMva%) NO-ERROR.
    IF AVAILABLE Moms THEN
        ASSIGN
        piMomsKod = Moms.MomsKod
        .
    ELSE DO:
        ASSIGN
            cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent mva% " + pcMva% + ".".
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "30").
        ASSIGN
          piAntFeil = piAntFeil + 1
          piMomsKod = 0
          .
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent mva% " + pcMva% + "."
          .
    END.

    /* Setter opp korrekt kobling til HG */
    DO:
        FIND VarGr NO-LOCK WHERE
            VarGr.Vg = piVg NO-ERROR.
        IF AVAILABLE VarGr THEN
            piHg = VarGr.Hg.
    END.

    /* Konverteringstabell - Farg */
    {getvpitabell.i &LevNr =VPIFilHode.EkstVPILevNr
                    &Nr = 1004
                    &Felt = cImpTabell}
    FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = cImpTabell AND 
        ImpKonv.EksterntId = pcFarge NO-ERROR.
    IF AVAILABLE ImpKonv THEN
    DO:
        ASSIGN
            cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": Konvertert farge til fargekode " + pcFarge + " til " + ImpKonv.InterntId + ".".
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "20").
        ASSIGN
          pcFarge = ImpKonv.InterntId
          piFarg  = 0
          .
    END.
    ASSIGN
        piFarg  = INT(pcFarge)
        NO-ERROR.
    /* Kontroll av fargekode */
    IF NOT CAN-FIND(Farg WHERE
                    Farg.Farg = piFarg) THEN
    DO:
        ASSIGN
          piAntFeil = piAntFeil
          cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent fargekode " + string(pcFarge) + ".".
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "30").
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent fargekode " + string(pcFarge) + "."
          .
    END.

    /* Henter/oppretter artikkelen. */
    FIND FIRST VPIArtBas EXCLUSIVE-LOCK WHERE
        VPIArtBas.EkstVpiLevNr  = VPIFilHode.EkstVpiLevNr AND
        VPIArtBas.VareNr        = pcVareNr NO-ERROR.
    IF NOT AVAILABLE VPIArtBas THEN
    DO:
        CREATE VPIArtBas.
        ASSIGN
            VPIArtBas.EkstVPILevNr = VPIFilHode.EkstVpiLevNr
            VPIArtBas.VareNr       = pcVareNr
            NO-ERROR.
    END.
    ELSE
        ASSIGN
            piAntOppdat = piAntOppdat + 1
            .

    FIND Strekkode NO-LOCK WHERE
        Strekkode.Kode = VPIArtBas.VareNr NO-ERROR.

    ASSIGN
        rArtBasRecid          = RECID(VPIArtBas)
        VPIArtBas.Vg          = piVg
        VPIArtBas.Hg          = piHg
        VPIArtBas.VgKat       = 1
        VPIArtBas.LopNr       = ?
        VPIArtBas.Storrelser  = TRUE
        VPIArtBas.Beskr       = pcVareNavn
        VPIArtBas.BongTekst   = pcBongTekst  
        VPIArtBas.LevKod      = pcBestKode   
        VPIArtBas.VmId        = int(pcMerke)      
        VPIArtBas.LevNr       = piLevNr      
        VPIArtBas.Notat       = pcFormFarge  + chr(1) + 
                                pcForm + CHR(1) + pcFarge       
        VPIArtBas.Farg        = piFarg
        VPIArtBas.StrTypeId   = IF VPIArtBas.StrTypeId = 0
                               THEN piStrTypeId
                               ELSE VPIArtBas.StrTypeId
        VPIArtBas.SaSong      = IF VPIArtBas.SaSong = 0
                               THEN piSasong
                               ELSE VPIArtBas.Sasong
        VPIArtBas.ValKod      = "" /* Blank gir omregningsfaktor = 1. */
        VPIArtBas.ArtikkelNr  = IF AVAILABLE Strekkode
                                   THEN Strekkode.ArtikkelNr
                                   ELSE VPIArtBas.ArtikkelNr
        pbStatus              = TRUE
        .

  /* Henter Valuta for omregning av rabatten. */
  FIND Valuta OF VPIArtBas NO-LOCK NO-ERROR.

  /* Taster valutapris */
  ASSIGN
    dcValPris  = DEC(pcBasPris)
    dcUtpris   = DEC(pcPris)
    .
  /* Det er gitt rabattert pris. */
  IF dec(pcF-Pris) <> 0 THEN
      ASSIGN
      dcRabatt   = DEC(pcBasPris) - DEC(pcF-Pris)
      .

  /* Bygger kalkulasjonsstrengen */
  ASSIGN
    pcSkjerm = KalkStreng()
    .
  /* Simulerer kalkulering */
  RUN Kalkulasjon ("ValPris",INPUT-OUTPUT pcSkjerm). /* Enter i valutapris */
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
      VPIArtPris.VareNr       = pcVareNr AND
      VPIArtPris.ProfilNr     = iProfilNr NO-ERROR.
  IF NOT AVAILABLE VPIArtPris THEN
  DO:
      CREATE VPIArtPris.
      ASSIGN
          VPIArtPris.EkstVpiLevNr = VPIFilHode.EkstVPILevNr 
          VPIArtPris.VareNr       = pcVareNr 
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

  /* Konverteringstabell - Størrelse */
  {getvpitabell.i &LevNr =VPIFilHode.EkstVPILevNr
                  &Nr = 1012
                  &Felt = cImpTabell}

  /* Lagrer strekkoden.                                                 */
  /* VPI kommer med en linje pr. strekkode. Det blir derfor alltid bare */
  /* en post i strekkodetabellen.                                       */
  FIND VpiStrekkode EXCLUSIVE-LOCK WHERE
      VPIStrekkode.EkstVpiLevNr = VPIFilHode.EkstVpiLevNr AND
      VPIStrekkode.VareNr       = pcVareNr AND
      VPIStrekkode.Kode         = pcVareNr NO-ERROR.
  IF NOT AVAILABLE VpiStrekkode THEN
  DO:
      CREATE VPIStrekkode.
      ASSIGN
          VPIStrekkode.EkstVpiLevNr = VPIFilHode.EkstVpiLevNr
          VPIStrekkode.VareNr       = pcVareNr 
          VPIStrekkode.Kode         = pcVareNr
          .
  END.
  ASSIGN
      VPIStrekkode.EkstStorl    = pcForm
      VPIStrekkode.KodeType     = 1 /* EAN */
      .
  FIND FIRST ImpKonv NO-LOCK WHERE 
      ImpKonv.EDB-System = cEDB-System AND 
      ImpKonv.Tabell     = cImpTabell AND 
      ImpKonv.EksterntId = VPIStrekkode.EkstStorl NO-ERROR.
  IF AVAILABLE ImpKonv THEN
  DO:
      ASSIGN
          cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": Konvertert størrelse " + VPIStrekkode.EkstStorl + " til " + ImpKonv.InterntId + ".".
      PUBLISH "VPIFilLogg" (cTekst + chr(1) + "20").
      ASSIGN
        VPIStrekkode.Storl = ImpKonv.InterntId
        .
  END.
  ELSE
   ASSIGN
     VPIStrekkode.Storl = VPIStrekkode.EkstStorl
     .

  RUN FixStorl IN h_dproclib (INPUT-OUTPUT VPIStrekkode.Storl). /* Størrelse i standard SkoTex */

  /* Kobler til størrelseskode. */
  FIND StrKonv NO-LOCK WHERE
      StrKonv.Storl = VPIStrekkode.Storl NO-ERROR.
  IF AVAILABLE StrKonv THEN
      ASSIGN
      VPIStrekkode.StrKode = StrKonv.StrKode
      .
  /* Kontroll av gyldig størrelse */
  ELSE DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent størrelse " + VPIStrekkode.Storl + " (Ekstern: " + VPIStrekkode.EkstStorl + ").".
      PUBLISH "VPIFilLogg" (cTekst + chr(1) + "30").
      CREATE tt_Error.
      ASSIGN
        tt_Error.LinjeNr = piAntFeil
        tt_Error.Tekst   = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent størrelse " + VPIStrekkode.Storl + " (Ekstern: " + VPIStrekkode.EkstStorl + ")."
        .
  END.
END. /* TRANSACTION - VPIFILLINJE */

IF pbStatus = TRUE THEN
DO TRANSACTION:
  FIND CURRENT VPIDatasett EXCLUSIVE-LOCK.
  ASSIGN
      VPIDatasett.DatasettStatus = 5 /* VPI mottatt */
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
  def var pcTekst   as char no-undo.
  def var pdAktDato as date no-undo.
  DEF VAR plTilbud AS LOG  NO-UNDO.
  DEF VAR plManuel AS LOG  NO-UNDO.
  
  assign
    plTilbud = false
    plManuel = FALSE
    pdAktDato =  TODAY - 1
    pcTekst   =   
      /*string(input FI-ValPris) */ string(dcValPris) + ";" +
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
             (if pdAktDato <> ?
                then string(pdAktDato)
                else "") + ";" +
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

