&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xpresslevtpakk.p
    Purpose     : Utpakking av produsent og lverandørinformasjon fra MEgaDisk.

    Syntax      :

    Description : Fra MegaDisk leveres en fil som inneholder:
                    1. Leverandører
                    2. Produsenter
                  Disse pakkes opp og oppdateres direkte inn i motsvarende
                  tabeller i SkoTex databasen.

    Author(s)   : Tom Nøkleby
    Created     : 8/4-03
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
DEF VAR iAntNya       AS INT NO-UNDO.
DEF VAR iAntUppdat    AS INT NO-UNDO.
DEF VAR iAntNyaAvd    AS INT NO-UNDO.
DEF VAR iAntNyaHg     AS INT NO-UNDO.
DEF VAR iAntFel       AS INT NO-UNDO.

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

DEF VAR piAntFeil   AS INT  NO-UNDO.
DEF VAR bProfitBase AS LOG  NO-UNDO.

  DEFINE TEMP-TABLE TT_Leverant
/* INDIVID_ID */  FIELD Lev_Tillv_ID AS CHAR LABEL "Lev. ID/Tillverkar-ID"
/* NAME       */  FIELD Namn         AS CHAR LABEL "Namn" 
/* M_ADDRESS  */  FIELD Postadress   AS CHAR LABEL "Postadress" 
/* M_ADDRESS2 */  FIELD Postadress2  AS CHAR LABEL "Postadress2" 
/* M_POSTCODE */  FIELD Postnr       AS CHAR LABEL "Postnr" 
/* M_CITY     */  FIELD Ort          AS CHAR LABEL "Ort" 
/* COUNTRY_ID */  FIELD Landskod     AS CHAR LABEL "Landskod" 
/* V_ADDRESS  */  FIELD Besoksadress AS CHAR LABEL "Besöksadress" 
/* TELEPHONE  */  FIELD TelNr        AS CHAR LABEL "Telefonnummer" 
/* ORG_ID     */  FIELD Orgnr        AS CHAR LABEL "Organisationsnr" 
     FIELD OrsakDel     AS CHAR LABEL "Orsak ej inläst"
     FIELD Radnr        AS INTE LABEL "Ordningsföljd inläsning"
      INDEX Radnr IS PRIMARY Radnr
      INDEX LevId Lev_Tillv_ID.

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

/* Centrallager */
{syspara.i 5 1 1 iCl INT}

FOR EACH tt_Error:
  DELETE tt_Error.
END.

SUBSCRIBE TO 'PBR' ANYWHERE.
{syspara.i 50 200 1 cTekst}
IF TRIM(cTekst) = "1" THEN
  bProfitBase = TRUE.
ELSE 
  bProfitBase = FALSE.

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
IF NOT AVAILABLE Butiker THEN
DO:
    MESSAGE "Sentrallager " + STRING(iCL) + " er ikke lagt inn i butikkregister."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
END.
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

/* Utpakking av Gruppeinndelinger.                       */
/* Her kommer også avdleinger, hovedgrupper og mvakoder. */
RUN UtpakkLev.

/* /* Kjører oppdatering av faste registre til ProfitBase. */ */
/* IF bProfitBase THEN                                        */
/* PROFITBASE:                                                */
/* DO:                                                        */
/*   RUN pfxoppdatfastereg.p.                                 */
/* END. /* PROFITBASE */                                      */

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

&IF DEFINED(EXCLUDE-PBR) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PBR Procedure 
PROCEDURE PBR :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcTekst AS CHAR NO-UNDO.

  PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaPostNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaPostNr Procedure 
PROCEDURE SkapaPostNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cPostNr     AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cPostadress AS CHARACTER  NO-UNDO.
  CREATE Post.
  ASSIGN Post.FylkesNr    = "1"
         Post.KommNr      = "1"
         Post.PostNr      = cPostnr
         Post.Beskrivelse = cPostadress
         Post.Merknad     = "Skapad vid import".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UtPakkLev) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtPakkLev Procedure 
PROCEDURE UtPakkLev :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR pcLev         AS CHAR NO-UNDO.
DEF VAR piTid         AS INT  NO-UNDO.

DEF VAR piLev         AS INT  NO-UNDO.
DEF VAR pbStatus      AS LOG  NO-UNDO.
DEF VAR piAntKoblet   AS INT  NO-UNDO.
DEF VAR piAntOppdat   AS INT  NO-UNDO.
DEF VAR iTstNr        AS INT  NO-UNDO.

DEF VAR piLoop1       AS INT  NO-UNDO.

ASSIGN
    piTid  = TIME
    cTekst = "Starter utpakking av Leverandører.".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

/* Aldri tilbud */
ASSIGN
    pbStatus = FALSE
    .

/* Oppretter VPIDatasett hvis det ikke finnes. */
IF NOT AVAILABLE VPIDatasett THEN
DO TRANSACTION:
    CREATE VPIDatasett.
    ASSIGN
        VPIDatasett.EkstVPILevNr   = VPIFilHode.EkstVPILevNr
        VPIDatasett.DatasettStatus = 1 /* Opprettet. */
        .
    FIND CURRENT VPIDatasett NO-LOCK.
END. /* TRANSACTION */

/* Tømmer temp-table */
FOR EACH TT_Leverant:
  DELETE TT_Leverant.
END.

/* Setter datasettets status. */
DO TRANSACTION:
  FIND CURRENT VPIDatasett EXCLUSIVE-LOCK.
  ASSIGN
      VPIDatasett.DatasettStatus = 2 /* VPI importeres */
      VPIDatasett.ImportDato     = TODAY
      VPIDatasett.ImportKl       = TIME
      VPIDatasett.FilId          = VPIFilHode.FilId
      .
  FIND CURRENT VPIDatasett NO-LOCK.
END. /* TRANSACTION */

ASSIGN
    iAntNya     = 0
    iAntUppdat  = 0
    iAntNyaHg   = 0
    iAntNyaAvd  = 0
    iAntFel     = 0
/*     cErrorFil   = ENTRY(NUM-ENTRIES(cFilNavn,".") - 1,cFilNavn,".") + ".fel" */
    .

/* Behandler fillinjene */
VPIFILLINJE:
FOR EACH VPIFilLinje OF VPIFilHode TRANSACTION:
    CREATE TT_Leverant.
    ASSIGN
        cEndelse                       = ""
        piLoop1                        = piLoop1 + 1
        iTotAntLinjer                  = iTotAntLinjer + 1
        TT_Leverant.Lev_Tillv_ID = trim(ENTRY( 1,VPIFilLinje.StorTekst,";"),'"')
        TT_Leverant.Namn         = trim(ENTRY( 2,VPIFilLinje.StorTekst,";"),'"')
        TT_Leverant.Postadress   = trim(ENTRY( 3,VPIFilLinje.StorTekst,";"),'"')
        TT_Leverant.Postadress2  = trim(ENTRY( 4,VPIFilLinje.StorTekst,";"),'"')
        TT_Leverant.Postnr       = trim(ENTRY( 5,VPIFilLinje.StorTekst,";"),'"')
        TT_Leverant.Ort          = trim(ENTRY( 6,VPIFilLinje.StorTekst,";"),'"')
        TT_Leverant.Landskod     = trim(ENTRY( 7,VPIFilLinje.StorTekst,";"),'"')
        TT_Leverant.Besoksadress = trim(ENTRY( 8,VPIFilLinje.StorTekst,";"),'"')
        TT_Leverant.TelNr        = trim(ENTRY( 9,VPIFilLinje.StorTekst,";"),'"')
        TT_Leverant.Orgnr        = trim(ENTRY(10,VPIFilLinje.StorTekst,";"),'"')
        TT_Leverant.Radnr              = piLoop1 
        TT_Leverant.OrsakDel     = IF TT_Leverant.Lev_Tillv_ID = "" THEN "Levid saknas" ELSE "".
        .
        IF TT_Leverant.OrsakDel = "" THEN DO:
            /* om det finns flera TT_leverant görs TT_Leverant-assignen flera gånge, vi lever med det */
            IF CAN-FIND(FIRST TT_Leverant WHERE TT_Leverant.Lev_Tillv_ID = TT_Leverant.Lev_Tillv_ID) THEN
                ASSIGN TT_Leverant.OrsakDel = "Dublett Levid".
        END.

    /* Melding til brukeren. */
    IF piLoop1 MODULO 50 = 0 THEN
        STATUS DEFAULT "Behandler linje " + STRING(iTotAntLinjer) + ".".
    
    KONVERTER:
    DO:
        /* ----- Konverteringstabell - Leverandør ----- */
        {getvpitabell.i &LevNr = VPIFilHode.EkstVPILevNr
                        &Nr = 1500
                        &Felt = cImpTabell}
        FIND FIRST ImpKonv NO-LOCK WHERE
            ImpKonv.EDB-System = cEDB-System AND
            ImpKonv.Tabell     = cImpTabell AND
            ImpKonv.EksterntId = TT_Leverant.Lev_Tillv_ID NO-ERROR.
        IF AVAILABLE ImpKonv THEN
        DO:
            ASSIGN
                cTekst = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": Konvertert leverandør " + TT_Leverant.Lev_Tillv_ID + " til " + ImpKonv.InterntId + "."
                piLev  = INT(ImpKonv.InterntID)
                .
            PUBLISH "VPIFilLogg" (cTekst + chr(1) + "20").
        END.
        ELSE 
        DO:
            FIND LAST LevBas NO-LOCK NO-ERROR.
            ASSIGN 
                piLev = IF AVAIL LevBas THEN LevBas.LevNr + 1 ELSE 1
                .
            CREATE 
                LevBas.
            ASSIGN 
                LevBas.LevNr        = piLev
                .
            CREATE ImpKonv.
            ASSIGN 
                ImpKonv.EDB-System = cEDB-System
                ImpKonv.Tabell     = cImpTabell
                ImpKonv.EksterntID = TT_Leverant.Lev_Tillv_ID
                ImpKonv.InterntID  = STRING(piLev)
                ImpKonv.Merknad    = STRING(TODAY)
                iAntNya          = iAntNya + 1
                .
            FIND CURRENT ImpKonv NO-LOCK.
            ASSIGN
              piAntFeil = piAntFeil + 1
              cTekst   =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ny leverandør opprettet " + string(piLev) + ".".
            PUBLISH "VPIFilLogg" (cTekst + chr(1) + "30").
            CREATE tt_Error.
            ASSIGN
              tt_Error.LinjeNr = piAntFeil
              tt_Error.Tekst   = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ny leverandør opprettet " + string(piLev) + "."
              .
        END.
    END. /* KONVERTER */

    /* Oppretter/oppdaterer poster i registrene */
    REGISTEROPPDAT:
    DO:
        FIND LevBas EXCLUSIVE-LOCK WHERE
            LevBas.LevNr = piLev NO-ERROR.
        IF AVAIL LevBas THEN DO:
            IF LevBas.LevNamn <> TT_Leverant.Namn     OR LevBas.LevAdr       <> TT_Leverant.Postadress   OR
               LevBas.LevPonr <> TT_Leverant.Postnr   OR LevBas.LevPadr      <> TT_Leverant.Ort          OR
               LevBas.LevLand <> TT_Leverant.Landskod OR LevBas.Koadr        <> TT_Leverant.Besoksadress OR
               LevBas.LevTel  <> TT_Leverant.TelNr    OR LevBas.Kommentar[4] <> TT_Leverant.Lev_Tillv_ID THEN
            ASSIGN 
                LevBas.LevNamn      = TT_Leverant.Namn        
                LevBas.LevAdr       = TT_Leverant.Postadress  
                LevBas.LevPonr      = TT_Leverant.Postnr      
                LevBas.LevPadr      = TT_Leverant.Ort         
                LevBas.LevLand      = TT_Leverant.Landskod    
                LevBas.Koadr        = TT_Leverant.Besoksadress
                LevBas.LevTel       = TT_Leverant.TelNr
                LevBas.Kommentar[4] = TT_Leverant.Lev_Tillv_ID
                .
        END.
        IF AVAIL LevBas AND NOT CAN-FIND(Post WHERE Post.Postnr = LevBas.LevPonr) THEN
            RUN SkapaPostNr(LevBas.LevPonr,LevBas.LevPadr).
    END. /* REGISTEROPPDAT */

    ASSIGN
        pbStatus = TRUE
        .
END. /* TRANSACTION - VPIFILLINJE */

IF pbStatus = TRUE THEN
DO TRANSACTION:
  FIND CURRENT VPIDatasett EXCLUSIVE-LOCK.
  ASSIGN
/*       VPIDatasett.DatasettStatus = 3 /* Gruppeinfo mottatt og oppdatert lokalt */ */
      VPIDatasett.DatasettStatus = 5 /* Her MÅ vi ferdigstille med en gang     */
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
    cTekst = "Behandlet " + STRING(iTotAntLinjer) + " VPI poster. " + 
             "Antall linjer med ukjente koder " + STRING(piAntFeil) + ".".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").
ASSIGN
    cTekst = "Utpakking av VPI ferdig.Tidsbruk " + STRING(TIME - piTid,"HH:MM:SS") + ".".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

