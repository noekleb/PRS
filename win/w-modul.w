&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: w-modul.w

  Description: Hovedmeny - Gir bruker mulighet for å starte en eller 
               flere av applikasjonens moduler.

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Tom Nøkleby

  Created: 13/6-98

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var wAnt-Buts  as int           no-undo.
def var wTemp-Hand as widget-handle no-undo.
def var wKunde     as char          no-undo.
def var wSvar      as log           no-undo.
def var wSprak     as char          no-undo.
def var wMenyListe as char          no-undo.
def var wLoop      as int           no-undo.
DEF VAR wKnapper   AS CHAR          NO-UNDO.
DEF VAR hTransLoggButton AS HANDLE NO-UNDO.
DEF VAR hPkSdlButton     AS HANDLE NO-UNDO.
DEF VAR hVPIMottakButton AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-HK IMAGE-LapTop RECT-7 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE IMAGE IMAGE-HK
     FILENAME "icon/skotex.bmp":U
     SIZE 5.4 BY 12.29.

DEFINE IMAGE IMAGE-LapTop
     FILENAME "icon/laptop.bmp":U
     SIZE 5.4 BY 12.29.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 6.6 BY 12.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     IMAGE-HK AT ROW 1.24 COL 1.8
     IMAGE-LapTop AT ROW 1.24 COL 1.8
     RECT-7 AT ROW 1.1 COL 1.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 39.6 BY 12.91.


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
         TITLE              = "SkoTex modulmeny"
         HEIGHT             = 12.91
         WIDTH              = 39.6
         MAX-HEIGHT         = 20.1
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 20.1
         VIRTUAL-WIDTH      = 100
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{dproclibstart.i}
{incl/devmode.i}
{incl/custdevmode.i}
{hjelp.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* SkoTex modulmeny */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* SkoTex modulmeny */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-MINIMIZED OF C-Win /* SkoTex modulmeny */
DO:
  run Minimer (false).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESTORED OF C-Win /* SkoTex modulmeny */
DO:
  run Maksimer (False).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
assign
  wMenyListe = "Arkiv,Moduler,System,Hjelp"
  wKnapper   = "MODULMENY"
  .
{syspara.i 1 1  15 wMenyListe}
{syspar2.i 1 1  15 wKnapper}

{syspara.i 1 1 100 wKunde}
{genlib.i &WindowName = "ModulMeny"} /* Starter prosedurebibliotek m.m. */

/* Sjekker om det kjøres på en LapTop - Innkjøpssystemet.       */ 
/* Retur verdier for wSvar er:                                  */
/*     True  - Systemet startes på en LapTop.                   */
/*     False - Systemet startes på HK.                          */
/*     ?     - Systemet startes på LapTop med ugyldig LapTopNr. */
if valid-handle(wLibHandle) then
  run SjekkLapTop in wLibHandle (output wSvar). 
/* Ugyldig LapTopNr satt i IniFil. */
if wSvar = ? then
  do:
    message "Ukjent LapTop nummer satt i Ini fil." skip
            "Programmet kan ikke starte!"
            view-as alert-box message title "Melding".
    apply "CLOSE":U to this-procedure.
  end.
/* Legger opp meny i vinduet. */
else if wSvar then /* LapTop */
  do:
    RUN AddMeny({&WINDOW-NAME},"LapTopMeny").
    run SetSysParaFraIniFil.
  end.
/* Setter opp menyen på HK. */  
else do: /* HK */ 
  find Bruker no-lock where
    Bruker.BrukerId = userid("SkoTex") no-error.
  if available Bruker then
    find BrukerGrp no-lock where
      BrukerGrp.BrGrpNr = Bruker.BrGrpNr no-error.
  if available BrukerGrp then
    do:
      if BrukerGrp.Navn <> "" then
        ASSIGN
          wMenyListe   = BrukerGrp.Navn
          wKnapper     = BrukerGrp.KnappeListe.
    end.
  do wLoop = 1 to num-entries(wMenyListe):
    RUN AddMeny({&WINDOW-NAME},entry(wLoop,wMenyListe)).  
  end.
end.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

status default " ".
status input   " ".

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  run ByggKnapperad.
  
  /* Henter sprkkode som bruker er satt opp med */
  /* Här ändrar vi också wKunde: butiksnamn i window:title */
  FIND Bruker NO-LOCK WHERE
      Bruker.BrukerId = USERID("SkoTex") NO-ERROR.
  IF AVAILABLE Bruker THEN
  DO:
      IF NOT CAN-FIND(FIRST Sprak WHERE
                      sprak.Lng = Bruker.Lng) THEN
          wSprak = "".
      ELSE
          wSprak = Bruker.Lng.
      IF Bruker.ButikkNr <> 0 THEN DO:
          FIND Butiker WHERE Butiker.Butik = Bruker.ButikkNr NO-LOCK NO-ERROR.
          IF AVAIL Butiker AND Butiker.Sentrallager = FALSE THEN
              ASSIGN wKunde = Butiker.Butnamn.
      END.

  END.
  
  /* Benytter default språkkode, hvis det er benyttet en ugyldig kode */
  IF NOT CAN-FIND(FIRST Sprak WHERE
                  sprak.Lng = Bruker.Lng) THEN
  DO:
      {syspara.i 1 3 1 wSprak}
  END.
  {lng.i
    &NewCode = "ASSIGN wCurrLng = if wSprak = '' then 'DES' else wSprak."
  }

  /* Setter språkkode i procedurebiblioteket. */
  IF valid-handle(h_dproclib) THEN
      RUN SetLng IN h_dproclib (wCurrLng). 

  assign
    C-Win:Title = wKunde.
  /* Här tar vi vara på default-printer och lägger in den i ett nytt */
  /* sessionsattribut: SE_PRINTER */
  /* för att hämta: DYNAMIC-FUNCTION("getAttribute",SESSION,"SE_PRINTER") */
  /* {incl/devmode.i} och {incl/custdevmode.i} måste vara inkluderade */
  DYNAMIC-FUNCTION("setAttribute",SESSION,"SE_PRINTER",SESSION:PRINTER-NAME).

  RUN enable_UI. 
  
  /* skapar extraknapp hvis det ligger ikke mottatte pakkseddler. */
  IF NOT wSvar THEN DO: /* ikke laptop */
    IF CAN-FIND(FIRST PkSdlHode WHERE PkSdlHode.PkSdlStatus = 10) THEN DO:
        ASSIGN C-Win:HEIGHT-PIXELS               = C-Win:HEIGHT-PIXELS + 30
               FRAME {&FRAME-NAME}:HEIGHT-PIXELS = C-Win:HEIGHT-PIXELS.
        CREATE BUTTON hPkSdlButton
            ASSIGN X = 5
                   Y = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 30
                  HEIGHT-PIXELS = 24
                  WIDTH-PIXELS = C-Win:WIDTH-PIXELS - 10
                  FRAME = FRAME {&FRAME-NAME}:HANDLE
                  SENSITIVE = TRUE
                  VISIBLE   = TRUE
                  LABEL     = "Varemottak/Pakkseddel"
                  TRIGGERS:
                       ON CHOOSE PERSISTENT
                           RUN StartPkSdl IN THIS-PROCEDURE.
                   END TRIGGERS.
    END.
    IF VALID-HANDLE(hPkSdlButton) AND SEARCH("icon\pksdl.bmp") <> ? THEN
        hPkSdlButton:LOAD-IMAGE("icon\pksdl.bmp").
  END.
  
  /* skapar extraknapp hvis det ligger Transaksjoner med feil. */
  IF NOT wSvar THEN DO: /* ikke laptop */
    IF CAN-FIND(FIRST TransLogg WHERE TransLogg.Postert = no) THEN DO:
        ASSIGN C-Win:HEIGHT-PIXELS               = C-Win:HEIGHT-PIXELS + 30
               FRAME {&FRAME-NAME}:HEIGHT-PIXELS = C-Win:HEIGHT-PIXELS.
        CREATE BUTTON hTransLoggButton
            ASSIGN X = 5
                   Y = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 30
                  HEIGHT-PIXELS = 24
                  WIDTH-PIXELS = C-Win:WIDTH-PIXELS - 10
                  FRAME = FRAME {&FRAME-NAME}:HANDLE
                  SENSITIVE = TRUE
                  VISIBLE   = TRUE
                  LABEL     = "BatchLogg"
                  TRIGGERS:
                       ON CHOOSE PERSISTENT
                           RUN StartBatchLogg IN THIS-PROCEDURE.
                   END TRIGGERS.
    END.
    IF VALID-HANDLE(hTransLoggButton) AND SEARCH("icon\translogg.bmp") <> ? THEN
        hTransLoggButton:LOAD-IMAGE("icon\translogg.bmp").
  END.

  /* skapar extraknapp hvis det ligger Transaksjoner med feil. */
  IF NOT wSvar THEN DO: /* ikke laptop */
    IF CAN-FIND(FIRST VPIMottak WHERE VPIMottak.behStatus < 90) THEN DO:
        ASSIGN C-Win:HEIGHT-PIXELS               = C-Win:HEIGHT-PIXELS + 30
               FRAME {&FRAME-NAME}:HEIGHT-PIXELS = C-Win:HEIGHT-PIXELS.
        CREATE BUTTON hVPIMottakButton
            ASSIGN X = 5
                   Y = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 30
                  HEIGHT-PIXELS = 24
                  WIDTH-PIXELS = C-Win:WIDTH-PIXELS - 10
                  FRAME = FRAME {&FRAME-NAME}:HANDLE
                  SENSITIVE = TRUE
                  VISIBLE   = TRUE
                  LABEL     = "VPIMottak"
                  TRIGGERS:
                       ON CHOOSE PERSISTENT
                           RUN StartVPIMottak IN THIS-PROCEDURE.
                   END TRIGGERS.
    END.
    IF VALID-HANDLE(hVPIMottakButton) AND SEARCH("icon\VPIMottak.bmp") <> ? THEN
        hVPIMottakButton:LOAD-IMAGE("icon\VPIMottak.bmp").
  END.

  assign
    C-Win:hidden = false.
    
  /* Setter opp riktig SkoTex bilde. */
  if wSvar then
    IMAGE-LapTop:move-to-top().
  else 
    IMAGE-HK:move-to-top(). 
    
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* Proseduredefinisjoner for menyhåndtering */
{meny.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggKnapperad C-Win 
PROCEDURE ByggKnapperad :
/*------------------------------------------------------------------------------
  Purpose:     Bygger knapperaden dynamisk.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wProgramListe as char no-undo.
  def var wTekst        as char no-undo.
  def var wLoop         as int  no-undo.
  DEF VAR cFilnavn      AS CHAR NO-UNDO.
  DEF VAR lProgramTilgang AS LOG NO-UNDO.

  IF wKnapper = "" THEN
      wKnapper = "MODULMENY".

  if wSvar then
    find Meny no-lock where 
      Meny.Navn = "LapModul" no-error.  
  else 
    find Meny no-lock where 
      Meny.Navn = wKnapper no-error.  
  if not available Meny then
    do:
      message "Meny er ikke definert"
              view-as alert-box 
              title "Feil ved menysystem".
      return no-apply.
    end.

  assign 
    wAnt-Buts = 0
    .
  BYGG-KNAPPER:
  do wLoop = 1 to num-entries(Meny.MData,";"):
    assign
      wTekst    = entry(wLoop,Meny.MData,";")
      wAnt-Buts = wAnt-Buts + 1.

      /* TN 29/7-04 */
      cFilnavn = (IF NUM-ENTRIES(ENTRY(5, wTekst, "|"), ".") > 1 
                  THEN ENTRY(1, ENTRY(5, wTekst, "|"), ".") ELSE wTekst).

      FIND Bruker WHERE Bruker.BrukerId = USERID("SkoTex") NO-LOCK NO-ERROR.
      FIND BrukerGrp NO-LOCK WHERE 
          BrukerGrp.BrGrpNr = Bruker.BrGrpNr NO-ERROR.
      IF CAN-FIND(FIRST ProgBrGrp 
                  WHERE ProgBrGrp.BrGrpNr = BrukerGrp.BrGrpNr 
                  AND ProgBrGrp.ProgNavn BEGINS cFilnavn ) THEN 
          lProgramTilgang = TRUE.
      ELSE
          lProgramTilgang = FALSE.

    create button wTemp-Hand
      assign 
        label        = entry(1,wTekst,"|")
        private-data = entry(2,wTekst,"|") + ";" +
                       entry(3,wTekst,"|") + ";" +
                       entry(4,wTekst,"|") + ";" +
                       entry(5,wTekst,"|")                       
        height-chars = 1.14
        width-chars  = 28.8        
        frame        = frame {&FRAME-NAME}:handle
        row          = wAnt-Buts * 1.24
        column       = 9.0
        sensitive    = lProgramTilgang /* TN 29/7-04 */
      triggers:
        on choose persistent 
          run StartProgram in this-procedure (input wTemp-Hand:private-data) .
      end triggers.      
  end. /* BYGG-KNAPPER */

  /* Øker frame størrelse hvis det er behov for det. */
  if frame {&FRAME-NAME}:height-chars < (wAnt-Buts / 3) + 2 then
    frame {&FRAME-NAME}:height-chars = (wAnt-Buts / 3) + 2.
/*   IF NOT wSvar THEN DO: /* ikke laptop */                                        */
/*     IF CAN-FIND(FIRST BatchLogg WHERE BatchLogg.OppdStatus = 3) THEN DO:         */
/*         ASSIGN C-Win:HEIGHT-PIXELS               = C-Win:HEIGHT-PIXELS + 30      */
/*                FRAME {&FRAME-NAME}:HEIGHT-PIXELS = C-Win:HEIGHT-PIXELS.          */
/*         CREATE BUTTON hTransLoggButton                                           */
/*             ASSIGN X = 5                                                         */
/*                    Y = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 30                    */
/*                   HEIGHT-PIXELS = 24                                             */
/*                   WIDTH-PIXELS = C-Win:WIDTH-PIXELS - 10                         */
/*                   FRAME = FRAME {&FRAME-NAME}:HANDLE                             */
/*                   SENSITIVE = TRUE                                               */
/*                   VISIBLE   = TRUE                                               */
/*                   LABEL     = "BatchLogg"                                        */
/*                   TRIGGERS:                                                      */
/*                        ON CHOOSE PERSISTENT                                      */
/*                            RUN StartBatchLogg IN THIS-PROCEDURE.                 */
/*                    END TRIGGERS.                                                 */
/*     END.                                                                         */
/*     IF VALID-HANDLE(hTransLoggButton) AND SEARCH("icon\translogg.bmp") <> ? THEN */
/*         hTransLoggButton:LOAD-IMAGE("icon\translogg.bmp").                       */
/*   END.                                                                           */

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
  ENABLE IMAGE-HK IMAGE-LapTop RECT-7 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Maksimer C-Win 
PROCEDURE Maksimer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ipFlagg as log no-undo.

  if valid-handle(wLibHandle) then
    run Maksimer in wLibHandle (this-procedure, ipFlagg).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Minimer C-Win 
PROCEDURE Minimer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ipFlagg as log no-undo.

  if valid-handle(wLibHandle) then
    run Minimer in wLibHandle (this-procedure, ipFlagg).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetSysParaFraIniFil C-Win 
PROCEDURE SetSysParaFraIniFil :
/*------------------------------------------------------------------------------
  Purpose:     Henter parameterverider fra INI filen og overstyrer de 
               som er satt i SysPara.
               Dette gjøres kun på LapTop.
               

               [SYSPARA]
               LapWinKat=c:\windows
               LapAppdir=\appdir\skotex
               LapHD=c:
               LapSkoTexDB=skotex = -db c:\db\jf-laptop\sko9
               LapRappDB=wr = -db db\wr9
               

  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wTekst as char no-undo.

  def buffer bufSysPara for SysPara.
  
  INIFIL:
  do for bufSysPara:

    GET-KEY-VALUE SECTION "SYSPARA" KEY "LapWinKat" VALUE wTekst.
    if wTekst <> ? then
      do:
        FIND bufSysPara EXCLUSIVE-LOCK where
          bufSysPara.SysHId = 1 and
          bufSysPara.SysGr  = 1 and
          bufSysPara.ParaNr = 6 NO-ERROR.
        if AVAILABLE bufSysPara then
          ASSIGN bufSysPara.Parameter1 = wTekst.
      end.

    GET-KEY-VALUE SECTION "SYSPARA" KEY "LapAppdir" VALUE wTekst.
    if wTekst <> ? then
      do:
        FIND bufSysPara EXCLUSIVE-LOCK where
          bufSysPara.SysHId = 1 and
          bufSysPara.SysGr  = 1 and
          bufSysPara.ParaNr = 7 NO-ERROR.
        if AVAILABLE bufSysPara then
          ASSIGN bufSysPara.Parameter1 = wTekst.
      end.
      
    GET-KEY-VALUE SECTION "SYSPARA" KEY "LapHD" VALUE wTekst.
    if wTekst <> ? then
      do:
        FIND bufSysPara EXCLUSIVE-LOCK where
          bufSysPara.SysHId = 1 and
          bufSysPara.SysGr  = 1 and
          bufSysPara.ParaNr = 9 NO-ERROR.
        if AVAILABLE bufSysPara then
          ASSIGN bufSysPara.Parameter1 = wTekst.
      end.

    GET-KEY-VALUE SECTION "SYSPARA" KEY "LapSkoTexDB" VALUE wTekst.
    if wTekst <> ? then
      do:
        FIND bufSysPara EXCLUSIVE-LOCK where
          bufSysPara.SysHId = 1 and
          bufSysPara.SysGr  = 1 and
          bufSysPara.ParaNr = 10 NO-ERROR.
        if AVAILABLE bufSysPara then
          ASSIGN bufSysPara.Parameter1 = wTekst.
      end.

    GET-KEY-VALUE SECTION "SYSPARA" KEY "LapRappDB" VALUE wTekst.
    if wTekst <> ? then
      do:
        FIND bufSysPara EXCLUSIVE-LOCK where
          bufSysPara.SysHId = 1 and
          bufSysPara.SysGr  = 1 and
          bufSysPara.ParaNr = 11 NO-ERROR.
        if AVAILABLE bufSysPara then
          ASSIGN bufSysPara.Parameter1 = wTekst.
      end.

    if available bufSysPara then
      release bufSysPara.
  end. /* INIFIL */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartBatchLogg C-Win 
PROCEDURE StartBatchLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR iBatchNr LIKE BatchLogg.BatchNr INIT ? NO-UNDO.
  IF CAN-FIND(FIRST TransLogg WHERE TransLogg.Postert = FALSE ) THEN
      RUN w-feiltrans.w.
  IF NOT CAN-FIND(FIRST TransLogg WHERE TransLogg.Postert = false) THEN DO:
      DELETE OBJECT hTransLoggButton.
      ASSIGN
      {&WINDOW-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS - 30.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartHjelp C-Win 
PROCEDURE StartHjelp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {winhlp.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartPkSdl C-Win 
PROCEDURE StartPkSdl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ipProgHandle AS HANDLE NO-UNDO.  
                                                 
  IF CAN-FIND(FIRST PkSdlHode WHERE PkSdlHode.PkSdlStatus = 10) THEN
  DO:
      RUN pksdlhode.w PERSISTENT SET ipProgHandle.
      if VALID-HANDLE(ipProgHandle) THEN
      DO:
          RUN initializeObject in ipProgHandle NO-ERROR.
          RUN MoveToTop in ipProgHandle NO-ERROR.
          run OpprettProgramListe in wLibHandle (ipProgHandle, ?, ?).
      END.
  END.
  IF NOT CAN-FIND(FIRST PkSdlHode WHERE PkSdlHode.PkSdlStatus = 10) THEN DO:
      DELETE OBJECT hPkSdlButton.
      ASSIGN
      {&WINDOW-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS - 30.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartProgram C-Win 
PROCEDURE StartProgram :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ipInnData as char no-undo.
  
  DEF var wProgram     AS CHAR   NO-UNDO.
  DEF VAR wPara        AS CHAR   NO-UNDO.
  DEF VAR wMsgPara     AS CHAR   NO-UNDO.
  DEF VAR wPrivData    AS CHAr   NO-UNDO.
  DEF VAR wSvar        AS LOGI   NO-UNDO.
  DEF VAR wLib         AS LOGI   NO-UNDO.
  DEF VAR wLok         AS LOGI   NO-UNDO.
  DEF VAR wWinH        AS HANDLE NO-UNDO.
  DEF VAR wMain        AS CHAR   NO-UNDO.
  DEF VAR wExt         AS CHAR   NO-UNDO.
  DEF VAR wPersistent  as LOGI   NO-UNDO.
  DEF VAR wSendParam   as LOGI   NO-UNDO.
  DEF VAR ipProgHandle AS HANDLE NO-UNDO.
  DEF VAR wHandle      AS HANDLE NO-UNDO.
  DEF VAR pcTekst      AS CHAR   NO-UNDO.

  assign
    wLib = false
    wLok = FALSE.

  IF ipInnData <> "" THEN 
     ASSIGN
       wLib        = if index(entry(1,ipInnData,";"),"L") <> 0 THEN TRUE ELSE false
       wLok        = if index(entry(1,ipInnData,";"),"O") <> 0 THEN TRUE ELSE false
       wPara       = entry(3,ipInnData,";")
       wProgram    = entry(4,ipInnData,";")
       wPersistent = if index(entry(1,ipInnData,";"),"P") <> 0 THEN FALSE ELSE TRUE
       wSendParam  = if index(entry(1,ipInnData,";"),"F") <> 0 THEN TRUE ELSE false.

  if TRANSACTION then
   do:
     message "Det er en aktiv transaksjon i et av de åpne vinduene." skip
             "Nye programmer kan ikke startes så lenge det er en   " skip
             "aktiv transaksjon." view-as alert-box WARNING Title "Feil ved åpning av program".
     return no-apply.
   end.

  RunProg: DO:
     IF wProgram = "" THEN DO:
        MESSAGE "Programnavn er ikke oppgitt i menyen, kan ikke kjøre."
           VIEW-AS ALERT-BOX ERROR TITLE "Feil".
        RETURN NO-APPLY.   
     END.
     
     IF R-INDEX(wProgram,".") > 0 THEN 
          ASSIGN wExt  = SUBSTR(wProgram,R-INDEX(wProgram,".") + 1)
                 wMain = SUBSTR(wProgram,1,LENGTH(wProgram) - (LENGTH(wExt) + 1)).
     ELSE ASSIGN wMain = wProgram.       

     /* Starter progress rutiner. */
     IF wExt <> "EXE" THEN DO:
        /* Sjekker at programmet finnes. */
        IF index(entry(1,ipInnData,";"),"L") <> 0 and
           index(entry(1,ipInnData,";"),"O") <> 0 and
           SEARCH(wMain + ".r") = ? AND
           SEARCH(wMain + ".w") = ? AND
           SEARCH(wMain + ".p") = ? 
        THEN DO:
           MESSAGE 'Finner ikke programmet "' + wProgram + '".' SKIP(1)
                   'Kan ikke utføre menyvalget.'
              VIEW-AS ALERT-BOX ERROR TITLE "Feil".
           RETURN NO-APPLY.   
        END.

        /* Oppretter vindu hvis dette er satt i menyen. */
        IF index(entry(1,ipInnData,";"),"V") <> 0 THEN DO:
          if wPersistent then
            RUN w-nyvin.w PERSISTENT SET wWinH (wProgram, wPara).
          ELSE
            RUN w-nyvin.w (wProgram, wPara).

          IF VALID-HANDLE(wWinH) THEN DO:
             ASSIGN CURRENT-WINDOW = wWinh:CURRENT-WINDOW.
             APPLY "ENTRY" TO CURRENT-WINDOW.
          END.
          return no-apply.  
        END.

        /* Starter programmet. */
        /* Menyvalget er en intern procedure som skal kjøres lokalt. */
        IF wLok then
          DO:
            if wSendParam then
              RUN VALUE(wProgram) in THIS-PROCEDURE (wPara).
            ELSE
              RUN VALUE(wProgram) in THIS-PROCEDURE.
          END.
        /* Menyvalget er en intern prosedyre som skal kjøres i prosedyrebibloteket. */
        ELSE IF wLib then
          DO:
            if wSendParam then
              RUN VALUE(wProgram) in wLibHandle (wPara).
            ELSE
              RUN VALUE(wProgram) in wLibHandle.
          END.

        /* Menyvalget er et GUI program som skal startes. */
        ELSE DO:
          if wSendParam THEN
            DO:
              if wPersistent then
              DO:
                RUN VALUE(wProgram) PERSISTENT SET ipProgHandle (wPara).
                if VALID-HANDLE(ipProgHandle) THEN DO:
                    RUN initializeObject in ipProgHandle NO-ERROR.
                    RUN MoveToTop in ipProgHandle NO-ERROR.
                END.
                IF VALID-HANDLE(ipProgHandle) THEN
                   run OpprettProgramListe in wLibHandle (ipProgHandle, ?, ?).
              END.
              else
                RUN VALUE(wProgram) (wPara).
            END.
          ELSE DO:
            if wPersistent then
            DO:            
              RUN VALUE(wProgram) PERSISTENT SET ipProgHandle.
              if VALID-HANDLE(ipProgHandle) THEN DO:
                  RUN initializeObject in ipProgHandle NO-ERROR.
                  RUN MoveToTop in ipProgHandle NO-ERROR.
              END.
              IF VALID-HANDLE(ipProgHandle) THEN
                 run OpprettProgramListe in wLibHandle (ipProgHandle, ?, ?).
            END.
            else
              RUN VALUE(wProgram).
          END.
        END.
     END.

     /* Starter Windows programm. */
     ELSE RUN WinExec(wProgram,1). /* 1=Normalt vindu, 2=Minimert */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartVPIMottak C-Win 
PROCEDURE StartVPIMottak :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ipProgHandle AS HANDLE NO-UNDO.  
                                                 
  IF CAN-FIND(FIRST VPIMottak WHERE VPIMottak.BehStatus < 90) THEN
  DO:
      RUN vpimottak.w PERSISTENT SET ipProgHandle.
      if VALID-HANDLE(ipProgHandle) THEN DO:
          RUN initializeObject in ipProgHandle NO-ERROR.
          RUN MoveToTop in ipProgHandle NO-ERROR.
      END.
      IF VALID-HANDLE(ipProgHandle) THEN
         run OpprettProgramListe in wLibHandle (ipProgHandle, ?, ?).
  END.
  IF NOT CAN-FIND(FIRST VPIMottak WHERE VPIMottak.BehStatus < 90) THEN DO:
      DELETE OBJECT hPkSdlButton.
      ASSIGN
      {&WINDOW-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS - 30.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

