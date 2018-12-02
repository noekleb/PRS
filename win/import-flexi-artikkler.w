&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER iBatchNr AS INT  NO-UNDO.
DEF INPUT PARAMETER cTabell  AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
def var cEDB-System      as char no-undo.
def var cEDB-Tabell      as char no-undo.
DEF VAR iCl              AS INT  NO-UNDO.
DEF VAR dcValPris        AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcInnPris        AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcUtpris         AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcRabatt         AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcFrakt          AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR rArtBasRecid     AS RECID                     NO-UNDO.
DEF VAR iProfilNr        AS INT                       NO-UNDO.
DEF VAR cEndelse         AS CHAR                      NO-UNDO.
DEF VAR cBildeFil        AS CHAR                      NO-UNDO.
DEF VAR lLapTop          AS LOG                       NO-UNDO.
DEF VAR lArtikkelNye     AS   LOG                     NO-UNDO.
DEF VAR lArtikkelOverskriv AS LOG                     NO-UNDO.

DEF VAR h_PrisKo AS HANDLE NO-UNDO.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-46 
&Scoped-Define DISPLAYED-OBJECTS FI-Info 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ArtikkelNr C-Win 
FUNCTION ArtikkelNr RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD KalkStreng C-Win 
FUNCTION KalkStreng RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LevDato C-Win 
FUNCTION LevDato RETURNS DATE
  ( pcDato AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE Image-Sko AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chImage-Sko AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FI-Info AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-46
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 45.6 BY 9.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-Info AT ROW 9.24 COL 3 NO-LABEL
     RECT-46 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 45.6 BY 9.57.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Import av artikkler"
         HEIGHT             = 9.57
         WIDTH              = 45.6
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR FILL-IN FI-Info IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME Image-Sko ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.38
       COLUMN          = 3
       HEIGHT          = 7.52
       WIDTH           = 42.2
       HIDDEN          = no
       SENSITIVE       = yes.
      Image-Sko:NAME = "Image-Sko":U .
/* Image-Sko OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Import av artikkler */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Import av artikkler */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Image-Sko
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Image-Sko C-Win OCX.DblClick
PROCEDURE Image-Sko.Picbuf.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  if available BildeRegister then
    run d-visbil.w (input recid(BildeRegister)).
  return no-apply.    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
  IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_Prisko.
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Kode for konvertering av artikkelnummer ved import. */
{syspara.i 1 2 6 cEDB-System}
{syspar2.i 1 2 6 cEDB-Tabell}

/* Centrallager */
{syspara.i 5 1 1 iCl INT}

/* Sjekker om det kjøres på en LapTop */
if valid-handle(wLibHAndle) then
  run SjekkLapTop in wLibHandle (output lLapTop).
ELSE 
  lLapTop = FALSE.

/* Profilnr for sentrallager. */
FIND Butiker NO-LOCK WHERE
    Butiker.butik = iCl NO-ERROR.
ASSIGN
    iProfilNr = Butiker.ProfilNr
    .

FIND ImportHode NO-LOCK WHERE
    ImportHode.BatchNr = iBatchNr.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  {lng.i}

  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN prisko.p PERSISTENT SET h_PrisKo.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_Prisko.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "import-flexi-artikkler.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chImage-Sko = Image-Sko:COM-HANDLE
    UIB_S = chImage-Sko:LoadControls( OCXFile, "Image-Sko":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "import-flexi-artikkler.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  RUN control_load.
  DISPLAY FI-Info 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-46 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Importer C-Win 
PROCEDURE Importer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
 
------------------------------------------------------------------------------*/
  DEF VAR piLoop        AS INT   NO-UNDO.
  DEF VAR pi2Loop       AS INT   NO-UNDO.
  DEF VAR pcEkstId      AS CHAR  NO-UNDO.
  DEF VAR pcBilde       AS CHAR  NO-UNDO.
  DEF VAR prArtBasRowId AS ROWID NO-UNDO.
  DEF VAR pcSkjerm      AS CHAR  NO-UNDO.
  DEF VAR plTilbud      AS LOG   NO-UNDO.
  DEF VAR plDirekte     AS LOG   NO-UNDO.
  DEF VAR pdcArtikkelNr AS DEC   NO-UNDO.
  DEF VAR piBildeNr     AS INT   NO-UNDO.
  DEF VAR cSkoBilde     AS CHAR  NO-UNDO.
  DEF VAR cSkoKatalog   AS CHAR  NO-UNDO.
  DEF VAR plOk          AS LOG   NO-UNDO.
  DEF VAR pcRetValue    AS CHAR  NO-UNDO.
  DEF VAR piFarge       AS INT   NO-UNDO.
  DEF VAR piMaterial    AS INT   NO-UNDO.
  DEF VAR piVg          AS INT   NO-UNDO.
  DEF VAR piLevNr       AS INT   NO-UNDO.
  DEF VAR pcImpTabell   AS CHAR  NO-UNDO.
  DEF VAR piVmId        AS INT   NO-UNDO.
  DEF VAR piKatNr       AS INT   NO-UNDO.
  DEF VAR piVgKat       AS INT   NO-UNDO.
  DEF VAR piSaSong      AS INT   NO-UNDO.
  DEF VAR piStrTypeId   AS INT   NO-UNDO.
  DEF VAR pcValKod      AS CHAR  NO-UNDO.
  
  DEF BUFFER bArtBas        FOR ArtBas.
  DEF BUFFER bArtPris       FOR ArtPris.
  DEF BUFFER bBildeRegister FOR BildeRegister.
  DEF BUFFER bImportLinje   FOR ImportLinje.

  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN prisko.p PERSISTENT SET h_PrisKo.
  
  ASSIGN
      plTilbud  = FALSE
      plDirekte = TRUE
      piLoop    = 0
      .

  MAINLOOP:
  FOR EACH ImportLinje EXCLUSIVE-LOCK WHERE
    ImportLinje.BatchNr = iBatchNr AND
    ImportLinje.Tabell  = cTabell  
    :

    ASSIGN
        piMaterial = 0
        piFarge    = 0
        dcFrakt    = 0
        dcRabatt   = 0
        dcInnpris  = 0
        dcUtpris   = 0
        pcBilde    = string(int(ImportLinje.Felt[1]),"999") + "_" +  
                     string(int(ImportLinje.Felt[2]),"999") + "_" +
                     lc(ImportLinje.Felt[3]) + ".bmp"
        cBildeFil  = ImportHode.BildeKatalog + "/" + pcBilde
        piLoop     = piLoop + 1
        FI-Info    = "Behandler post " + STRING(piLoop) + "."
        .
    DISPLAY
        FI-Info
    WITH FRAME Default-Frame.

    /* Henter og kontrollerer varegruppekoblingen.   */
    /* Sjekker også om varegruppen skal konverteres. */
    {syspar2.i 1 2 1003 pcImpTabell}
    ASSIGN
        piVg = INT(ImportLinje.Felt[23])
        .
    FIND FIRST ImpKonv NO-LOCK WHERE 
      ImpKonv.EDB-System = cEDB-System AND 
      ImpKonv.Tabell     = pcImpTabell AND 
      ImpKonv.EksterntId = trim(ImportLinje.Felt[23]) NO-ERROR.
    IF AVAILABLE ImpKonv THEN
      ASSIGN
        piVg = int(ImpKonv.InterntId)
    .

    FIND VarGr NO-LOCK WHERE
        VarGr.Vg = piVg NO-ERROR.
    IF NOT AVAILABLE VarGr THEN
    DO:
      MESSAGE "Ukjent varegruppe på artikkel: " 
              pcEkstId "LinjeNr:" ImportLinje.LinjeNr
              "Varegruppe:" ImportLinje.Felt[23]
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      NEXT MAINLOOP.
    END.

    /* Kontrollerer leverandøren.                         */
    /* Sjekker også om leverandørnummer skal konverteres. */
    {syspar2.i 1 2 1000 pcImpTabell}
    ASSIGN
        piLevNr = INT(ImportLinje.Felt[2])
        .
    /* Utfører konvertering. */
    FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = pcImpTabell AND 
        ImpKonv.EksterntId = trim(ImportLinje.Felt[2]) NO-ERROR.
    IF AVAILABLE ImpKonv THEN
      ASSIGN
        piLevNr = int(ImpKonv.InterntId)
        .

    FIND LevBas NO-LOCK WHERE
        LevBas.LevNr = piLevNr NO-ERROR.
    IF NOT AVAILABLE VarGr THEN
    DO:
      MESSAGE "Ukjent leverandør på artikkel: " 
              pcEkstId "LinjeNr:" ImportLinje.LinjeNr
              "Leverandør:" ImportLinje.Felt[2]
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      NEXT MAINLOOP.
    END.

    /* Kontrollerer kategorien. Sjekker også eventuell */
    /* Konvertering.                                   */
    ASSIGN
        piKatNr = INT(ImportLinje.Felt[ 4])
        .
    /* Utfører konvertering. */
    {syspar2.i 1 2 1006 pcImpTabell}
    FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = pcImpTabell AND 
        ImpKonv.EksterntId = ImportLinje.Felt[4] NO-ERROR.
    IF AVAILABLE ImpKonv THEN
      ASSIGN
        piKatNr = int(ImpKonv.InterntId)
        .
    /* Henter nå VgKatNr som motsvarer KatNr fra koblingstabell */
    FIND FIRST VgKat NO-LOCK WHERE
        VgKat.KatNr = piKatNr AND
        VgKat.Vg    = piVg NO-ERROR.
    IF AVAILABLE VgKat THEN
        piVgKat = VgKat.VgKat.
    ELSE
        piVgKat = piKatNr.

    /* Henter farge og materialkode på 1. variant. */
    FIND FIRST ImportDetalj NO-LOCK WHERE
      ImportDetalj.BatchNr = ImportLinje.BatchNr AND
      ImportDetalj.Tabell  = ImportLinje.Tabell  AND
      ImportDetalj.TYPE    = "VARIANT"           AND
      ImportDetalj.LinjeNr = ImportLinje.LinjeNr /* AND 
      ImportDetalj.Felt[9] = "BESTILLING" */ NO-ERROR.
    IF NOT AVAILABLE ImportDetalj THEN
        NEXT MAINLOOP.
    ASSIGN
      piFarge    = int(ImportDetalj.Felt[3])
      piMaterial = int(ImportDetalj.Felt[1])
      .

    /* Bygger eksternt ID på artikkelen. */
    ASSIGN
      pcEkstId   = ImportLinje.Felt[2]   + "|" +
                   ImportLinje.Felt[3]   + "|" +
                   string(piFarge,"999") + "|" + 
                   STRING(piMaterial,"999")
      .
      
    /* Henter/oppretter artikkelen. */
    find first KonvReg no-lock where
      KonvReg.EDB-System = cEDB-System and
      KonvReg.Tabell     = cEDB-Tabell AND
      KonvReg.EkstId     = pcEkstId no-error.
    if not available KonvReg then
      do:
        /* NB: ArtikkelNr settes i trigger. */
        create ArtBas.
        assign
          rArtBasRecid  = recid(ArtBas)
          ArtBas.Vg     = piVg
          ArtBas.LopNr  = ?
          ArtBas.HG     = VarGr.Hg
          ArtBas.LapTop = lLapTop
          .    
        /* Flagger at artikkelen er importert */  
        create KonvReg.
        assign
          KonvReg.EDB-System = cEDB-System 
          KonvReg.Tabell     = cEDB-Tabell   
          KonvReg.EkstId     = pcEkstId
          KonvReg.InterntID  = string(ArtBas.ArtikkelNr).
      end.
    ELSE do: /* Artikkelen har vært importert tidligere */ 
      find ArtBas exclusive-lock where
        ArtBas.ArtikkelNr = dec(KonvReg.InterntId) no-error.
      ASSIGN
          rArtBasRecid = RECID(ArtBas).
    END.

    ASSIGN
        piFarge    = int(ImportDetalj.Felt[3])
        piMaterial = int(ImportDetalj.Felt[1])
        pcValKod   = trim(ImportDetalj.Felt[6])
        .
    IF CAN-DO(",0",pcValkod) AND AVAILABLE LevBas THEN
    DO:
        ASSIGN
            pcValKod = LevBas.ValKod
            .
    END.

    /* Konv av farge */
    {syspar2.i 1 2 1004 pcImpTabell}
    FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = pcImpTabell AND 
        ImpKonv.EksterntId = ImportDetalj.Felt[3] NO-ERROR.
    IF AVAILABLE ImpKonv THEN
      ASSIGN
        piFarge = int(ImpKonv.InterntId)
        .

    /* Konv av Material */
    {syspar2.i 1 2 1005 pcImpTabell}
    FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = pcImpTabell AND 
        ImpKonv.EksterntId = ImportDetalj.Felt[1] NO-ERROR.
    IF AVAILABLE ImpKonv THEN
      ASSIGN
        piMaterial = int(ImpKonv.InterntId)
        .
    /* Setter på farge, material og valutakode på artikkelen */
    ASSIGN
        ArtBas.Farg       = piFarge
        ArtBas.MatKod     = piMaterial
        ArtBas.LevFargKod = ImportDetalj.Felt[5]
        ArtBas.DivInfo[2] = ImportDetalj.Felt[2]
        ArtBas.ValKod     = pcValKod
        .

    /* Kontrollerer og korrigerer varemerke */
    ASSIGN
        piVmId = INT(ImportLinje.Felt[ 6])
        .
    {syspar2.i 1 2 1007 pcImpTabell}
    FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = pcImpTabell AND 
        ImpKonv.EksterntId = ImportLinje.Felt[6] NO-ERROR.
    IF AVAILABLE ImpKonv THEN
      ASSIGN
        piVmId = int(ImpKonv.InterntId)
        .

    /* Kontroll og konvertering av sesongen */
    ASSIGN
        piSaSong = INT(ImportLinje.Felt[ 1]).
    {syspar2.i 1 2 1001 pcImpTabell}
    FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = pcImpTabell AND 
        ImpKonv.EksterntId = ImportLinje.Felt[ 1] NO-ERROR.
    IF AVAILABLE ImpKonv THEN
      ASSIGN
        piSaSong = int(ImpKonv.InterntId)
        .
    FIND Sasong NO-LOCK WHERE
         Sasong.Sasong = piSaSong NO-ERROR.

    /* Kontroll og konvertering av størrelsestyper */
    ASSIGN
        piStrTypeId = INT(ImportLinje.Felt[ 8])
        .
    {syspar2.i 1 2 1008 pcImpTabell}
    FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = pcImpTabell AND 
        ImpKonv.EksterntId = ImportLinje.Felt[8] NO-ERROR.
    IF AVAILABLE ImpKonv THEN
      ASSIGN
        piStrTypeId = int(ImpKonv.InterntId)
        .

    ASSIGN
      /*#SÄSONG      */ ArtBas.Sasong          = piSaSong
      /*#LEVKOD      */ ArtBas.LevNr           = piLevNr
      /*#LEVART      */ ArtBas.LevKod          = ImportLinje.Felt[ 3]
      /*#KAT         */ ArtBas.VgKat           = piVgKat
      /*#VM          */ ArtBas.VmId            = piVmId
      /*#STLKINT     */ ArtBas.StrTypeId       = piStrTypeId
      /*#BESK        */ ArtBas.Beskr           = ImportLinje.Felt[ 9]
      /*#ÖVRIGT      */ ArtBas.Notat           = ImportLinje.Felt[10]
      /*#ANNONS      */ ArtBas.AnonseArtikkel  = IF int(ImportLinje.Felt[11]) = 0
                                                   THEN FALSE
                                                   ELSE TRUE
      .
    ASSIGN
      /*#LEVTID1     */ ArtBas.LevDato1        = LevDato(ImportLinje.Felt[14]) 
      .

    ASSIGN
      ArtBas.Lager        = TRUE
      ArtBas.Storrelser   = TRUE
      ArtBas.AktivDato    = TODAY
      ArtBas.Aktivert     = TRUE
      ArtBas.BongTekst    = VarGr.VgBeskr
      .

    /* Henter Valuta for omregning av rabatten. */
    FIND Valuta OF ArtBas NO-LOCK NO-ERROR.

    /* Så satser vi på at første prisen er riktig. */
    FIND FIRST ImportDetalj NO-LOCK WHERE
      ImportDetalj.BatchNr = ImportLinje.BatchNr AND 
      ImportDetalj.Tabell  = ImportLinje.Tabell AND
      ImportDetalj.TYPE    = "PRIS" AND
      ImportDetalj.LinjeNr = ImportLinje.LinjeNr NO-ERROR.
    IF AVAILABLE ImportDetalj THEN
    DO:
      /* Taster valutapris */
      ASSIGN
        dcValPris      = DEC(ImportDetalj.Felt[5])
        .
      /* Bygger kalkulasjonsstrengen */
      ASSIGN
        pcSkjerm = KalkStreng()
        .
      /* Simulerer ENTER i innkjøpspris og får regnet ut innkjøpspris. */
      RUN Kalkulasjon ("ValPris",INPUT-OUTPUT pcSkjerm).
      ASSIGN
        pcSkjerm  = pcSkjerm + "0;0;" + cEndelse
        dcInnPris = dec(ENTRY(2,pcSkjerm,";")).

      /* Regner ut rabatt eller frakt */
      IF DEC(ImportDetalj.Felt[7]) - dcInnPris >= 0 THEN
      FRAKT:
      DO:
        ASSIGN
            dcFrakt = DEC(ImportDetalj.Felt[7]) - dcInnPris
            ENTRY ( 7, pcSkjerm, ";") = string(dcFrakt)
            .
        /* Simulerer ENTER i frakt og får regnet ut innkjøpspris. */
        RUN Kalkulasjon ("Frakt",INPUT-OUTPUT pcSkjerm).
        ASSIGN
          pcSkjerm  = pcSkjerm + "0;0;" + cEndelse
          .
      END. /* FRAKT */
      ELSE
      RABATT:
      DO:
        ASSIGN
            dcRabatt = abs(DEC(ImportDetalj.Felt[7]) - dcInnPris)
            ENTRY ( 3, pcSkjerm, ";") = string(dcRabatt)
            .
        /* Simulerer ENTER i rabatt. */
        RUN Kalkulasjon ("Rab1",INPUT-OUTPUT pcSkjerm).
        ASSIGN
          pcSkjerm  = pcSkjerm + "0;0;" + cEndelse
          .
      END. /* RABATT */

    END.
    /* Ingen kalkyle tilgjengelig. 0-Kalkyle */
    ELSE DO:
      ASSIGN
        dcValPris      = 0
        dcInnPris      = 0  
        dcUtpris       = 0  
        .

      /* Bygger kalkulasjonsstrengen */
      ASSIGN
        pcSkjerm = KalkStreng()
        .

      /* Simulerer ENTER i innkjøpspris. */
      RUN Kalkulasjon ("InnPris",INPUT-OUTPUT pcSkjerm).
      ASSIGN
        pcSkjerm  = pcSkjerm + "0;0;" + cEndelse.

      /* Simulerer ENTER i Rabatt1Kroner. */
      RUN Kalkulasjon ("Rab1",INPUT-OUTPUT pcSkjerm).
      ASSIGN
        pcSkjerm  = pcSkjerm + "0;0;" + cEndelse.
    
      /* Simulerer ENTER i pris. */
      RUN Kalkulasjon ("Pris",INPUT-OUTPUT pcSkjerm).
      ASSIGN
        pcSkjerm  = pcSkjerm + "0;0;" + cEndelse.
    END.

    /* Lagrer kalkylen */  
    run LagreArtPris in h_PrisKo
        (input rArtBasRecid, 
         input iProfilNr,
         input-output pcSkjerm,
         input plTilbud,
         input plDirekte,
         input true,
         ?).

    /* Bildehåndtering */
    IF SEARCH(cBildeFil) <> ? THEN
    BILDE:
    DO:
      /* Henter bildenummer, filnavn og katalog. */
      run BildeNummer in wLibHandle (input "I",
                                     output piBildeNr,
                                     output cSkoBilde,
                                     output cSkoKatalog).

      assign
        chIMAGE-Sko:Picbuf:filename  = SEARCH(cBildeFil)
        chIMAGE-Sko:Picbuf:AutoScale = True.        
      plOk = chIMAGE-Sko:Picbuf:load.    
  
      /* Tildeler filnavn */
      chIMAGE-Sko:Picbuf:FileName = cSkoKatalog + "\" + cSkoBilde.
    
      /* Lagrer bilde på hd. ------------------------------------------------ */
      chIMAGE-Sko:Picbuf:Store.                 /* Lagre filen til HD.        */
      If chIMAGE-Sko:Picbuf:WriteCompression <> 0 Then /* Filen skal komprimeres.    */
        chIMAGE-Sko:Picbuf:WriteCompression = 0.   
  
      /* Setter bildenummer i ArtBas. */
      ASSIGN
          ArtBas.BildNr = piBildeNr
          .

      /* Setter bildenavn i bilderegisteret. */         
      find BildeRegister exclusive-lock where
           Bilderegister.BildNr = piBildeNr.
      if available BildeRegister then
         BildeRegister.FilNavn = cSkoBilde.
      FIND CURRENT BildeRegister NO-LOCK.

      /* Leser inn bilde i databasen. */
      run LesInnBilde in wLibHandle (piBildeNr, chIMAGE-Sko:Picbuf:FileName, output pcRetValue).

    END. /* BILDE */

    /* Henter prisen */
    FIND ArtPris NO-LOCK WHERE
        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
        ArtPris.ProfilNr   = iProfilNr NO-ERROR.

    /* Håndtering av varianter */
    assign
      pi2Loop = 0
      .
    VARIANT:
    FOR EACH ImportDetalj NO-LOCK WHERE
        ImportDetalj.BatchNr = ImportLinje.BatchNr AND
        ImportDetalj.Tabell  = ImportLinje.Tabell  AND
        ImportDetalj.TYPE    = "VARIANT"           AND
        ImportDetalj.LinjeNr = ImportLinje.LinjeNr:
        /* Legges inn senere 
        AND
        ImportDetalj.Felt[9] = "BESTILLING":
        */

        ASSIGN
            pi2Loop = pi2Loop + 1
            piFarge    = int(ImportDetalj.Felt[3])
            piMaterial = int(ImportDetalj.Felt[1])
            pcValKod   = trim(ImportDetalj.Felt[6])
            .
        IF CAN-DO(",0",pcValkod) AND AVAILABLE LevBas THEN
        DO:
            ASSIGN
            pcValKod = LevBas.ValKod
            .
        END.

        /* Konv av farge */
        {syspar2.i 1 2 1004 pcImpTabell}
        FIND FIRST ImpKonv NO-LOCK WHERE 
            ImpKonv.EDB-System = cEDB-System AND 
            ImpKonv.Tabell     = pcImpTabell AND 
            ImpKonv.EksterntId = ImportDetalj.Felt[3] NO-ERROR.
        IF AVAILABLE ImpKonv THEN
          ASSIGN
            piFarge = int(ImpKonv.InterntId)
            .

        /* Konv av Material */
        {syspar2.i 1 2 1005 pcImpTabell}
        FIND FIRST ImpKonv NO-LOCK WHERE 
            ImpKonv.EDB-System = cEDB-System AND 
            ImpKonv.Tabell     = pcImpTabell AND 
            ImpKonv.EksterntId = ImportDetalj.Felt[1] NO-ERROR.
        IF AVAILABLE ImpKonv THEN
          ASSIGN
            piMaterial = int(ImpKonv.InterntId)
            .

        IF pi2Loop = 1 THEN
          ASSIGN
            ArtBas.Farg   = piFarge
            ArtBas.MatKod = piMaterial
            ArtBas.ValKod = pcValKod
            .
        ELSE DO:
            ASSIGN
                pdcArtikkelNr = ArtikkelNr().
            CREATE bArtBas.
            BUFFER-COPY ArtBas EXCEPT ArtikkelNr TO bArtBas
                ASSIGN
                  bArtBas.ArtikkelNr = pdcArtikkelNr
                  bArtBas.Farg       = piFarge
                  bArtBas.MatKod     = piMaterial
                  bArtBas.LevFargKod = ImportDetalj.Felt[5]
                  bArtBas.DivInfo[2] = ImportDetalj.Felt[2]
                  bArtBas.ValKod     = pcValKod

                .
        END.

        /* Kopierer også prisen. */
        IF AVAILABLE ArtPris THEN
        DO:
          IF NOT CAN-FIND(bArtPris WHERE
                          bArtPris.ArtikkelNr = pdcArtikkelNr AND
                          bArtPris.ProfilNr   = iProfilNr) THEN
            BUFFER-COPY ArtPris EXCEPT ArtikkelNr TO bArtPris
              ASSIGN bArtPris.ArtikkelNr = pdcArtikkelNr.
        END.

        /* Legger opp kobling i KonvReg hvis den mangler. */
        /* Bygger eksternt ID på artikkelen. */
        ASSIGN
          pcEkstId   = ImportLinje.Felt[2]   + "|" +
                       ImportLinje.Felt[3]   + "|" +
                       string(int(ImportDetalj.Felt[3]),"999") + "|" + 
                       STRING(int(ImportDetalj.Felt[1]),"999")
          .
        IF NOT CAN-FIND(KonvReg no-lock where
                        KonvReg.EDB-System = cEDB-System and
                        KonvReg.Tabell     = cEDB-Tabell AND
                        KonvReg.EkstId     = pcEkstId) THEN
        do:
          /* Flagger at artikkelen er importert */  
          create KonvReg.
          assign
            KonvReg.EDB-System = cEDB-System 
            KonvReg.Tabell     = cEDB-Tabell   
            KonvReg.EkstId     = pcEkstId
            KonvReg.InterntID  = string(bArtBas.ArtikkelNr).
        end.
    END. /* VARIANT */
  END. /* MAINLOOP */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kalkulasjon C-Win 
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

  DEF BUFFER bArtBas FOR ArtBas.
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
    MESSAGE "Ukjent bArtBas"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
  END.
  FIND bVarGr OF bArtBas NO-ERROR.
  IF NOT AVAILABLE bVarGr THEN
  DO:
    MESSAGE "Ukjent bVarGr"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
  END.
  FIND bMoms  OF bVarGr  NO-ERROR.
  IF NOT AVAILABLE bMoms THEN
  DO:
    MESSAGE "Ukjent bMoms"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
  END.
  FIND bValuta OF ArtBas NO-ERROR.
  IF NOT AVAILABLE bValuta THEN
  DO:
    MESSAGE "Ukjent bValuta for Artikkel/leverandør:" ArtBas.ArtikkelNr ArtBas.LevNr ":" + ArtBas.ValKod + ":"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettPara C-Win 
PROCEDURE SettPara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plArtikkelNye       AS LOG NO-UNDO.
  DEF INPUT PARAMETER plArtikkelOverskriv AS LOG NO-UNDO.
  
  ASSIGN
    lArtikkelNye       = plArtikkelNye      
    lArtikkelOverskriv = plArtikkelOverskriv
    .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartImport C-Win 
PROCEDURE StartImport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN Importer.
  RUN StoppImport.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StoppImport C-Win 
PROCEDURE StoppImport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY "close" TO THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ArtikkelNr C-Win 
FUNCTION ArtikkelNr RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var trgArtikkelNr as dec no-undo.
  def buffer trgArtBas for SkoTex.ArtBas.

  LOOPEN:
  do while true:
    trgArtikkelNr = NEXT-VALUE(ArtikkelNr,SkoTex).
    if not can-find(first trgArtBas where trgArtBAs.ArtikkelNr = 
                    trgArtikkelNr) then
      leave LOOPEN.
  end. /* LOOPEN */

  RETURN trgArtikkelNr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION KalkStreng C-Win 
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
      /*string(input FI-Pris)    */ "0" + ";" +
      /*string(input FI-EUPris)  */ STRING(dcUtpris) + ";" +
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LevDato C-Win 
FUNCTION LevDato RETURNS DATE
  ( pcDato AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pdDato AS DATE NO-UNDO.
  DEF VAR piLoop AS INT  NO-UNDO.
  
  /* Ugyldig år/uke angivelse */
  IF LENGTH(pcDato) <> 3 THEN
    pdDato = ?.
  ELSE 
  LOOPEN:
  DO:
    DO piLoop = 1 TO 3:
      IF NOT CAN-DO("0,1,2,3,4,5,6,7,8,9",SUBSTRING(pcDato,piLoop,1)) THEN
      DO:
          pdDato = ?.
          LEAVE LOOPEN.
      END.
    END.
    ASSIGN
      pdDato = (date(1, 1, 2000 + INT(SUBSTRING(pcDato,3,1))) - 4) 
               + (7 * INT(SUBSTRING(pcDato,1,2)))
      .
  END. /* LOOPEN */

  RETURN pdDato.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

