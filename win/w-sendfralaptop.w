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

/* Local Variable Definitions ---                                       */
def var wConnectParam as char no-undo.
def var wSentralDB    as char no-undo.  
def var wLapTopNr     as int  no-undo.
def var wRecid        as int  no-undo.
def var wLogFil       as char no-undo.
def var wwLogFil      as char no-undo.
def var wLogKatalog   as char no-undo.
def var wBruktTid     as int  no-undo.
def var wStartTid     as int  no-undo.
def var wOkStatus     as char no-undo.

/* Delte variabler */
def new shared var wAvbryt       as log  no-undo.

/* Definerer stream */
def stream LoggData.

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
&Scoped-Define ENABLED-OBJECTS RECT-50 RECT-51 RECT-52 B-Koble B-KobleNed ~
Btn_Cancel B-AvbrytOverfor B-Overfor B-VisLogg B-Parametre Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS FI-DbInfo RS-1 RS-2 RS-3 FI-Linje1 ~
FI-Linje2 FI-DbKontakt FI-Datatyper FI-FasteRegistre FI-GrunnRegistre ~
FI-Bestillinger FI-Fremdrift 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-DEFAULT-FRAME 
       MENU-ITEM m_Fil          LABEL "&Fil"          .


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-AvbrytOverfor 
     LABEL "&Avbryt overføring" 
     SIZE 23 BY 1.14.

DEFINE BUTTON B-Koble 
     LABEL "&Koble opp" 
     SIZE 15 BY 1.

DEFINE BUTTON B-KobleNed 
     LABEL "&Koble ned" 
     SIZE 15 BY 1.

DEFINE BUTTON B-Overfor 
     LABEL "&Start overføring" 
     SIZE 23 BY 1.14.

DEFINE BUTTON B-Parametre 
     LABEL "Parametre" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-VisLogg 
     LABEL "&Vis logg..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY DEFAULT 
     LABEL "Avslutt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help DEFAULT 
     LABEL "&Help" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-Bestillinger AS CHARACTER FORMAT "X(256)":U INITIAL "Artikler, bestillinger og ordrer" 
      VIEW-AS TEXT 
     SIZE 28 BY .62 NO-UNDO.

DEFINE VARIABLE FI-Datatyper AS CHARACTER FORMAT "X(256)":U INITIAL "Datatyper" 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE FI-DbInfo AS CHARACTER FORMAT "X(256)":U INITIAL "Ikke oppkoblet" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1
     BGCOLOR 12 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FI-DbKontakt AS CHARACTER FORMAT "X(256)":U INITIAL "Oppkobling" 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE FI-FasteRegistre AS CHARACTER FORMAT "X(256)":U INITIAL "Faste registre" 
      VIEW-AS TEXT 
     SIZE 21 BY .62 NO-UNDO.

DEFINE VARIABLE FI-Fremdrift AS CHARACTER FORMAT "X(256)":U INITIAL "Fremdrift" 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE FI-GrunnRegistre AS CHARACTER FORMAT "X(256)":U INITIAL "Grunnregistre" 
      VIEW-AS TEXT 
     SIZE 21 BY .62 NO-UNDO.

DEFINE VARIABLE FI-Linje1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Datatype" 
     VIEW-AS FILL-IN 
     SIZE 67 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Linje2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Behandler" 
     VIEW-AS FILL-IN 
     SIZE 67 BY 1 NO-UNDO.

DEFINE VARIABLE RS-1 AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Nye og endrede", 1,
"Nye", 2,
"Ingen", 3
     SIZE 47 BY .71 NO-UNDO.

DEFINE VARIABLE RS-2 AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Nye og endrede", 1,
"Nye", 2,
"Ingen", 3
     SIZE 47 BY .71 NO-UNDO.

DEFINE VARIABLE RS-3 AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Nye og endrede", 1,
"Nye", 2,
"Ingen", 3
     SIZE 47 BY .71 NO-UNDO.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 83 BY 4.52.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 83 BY 3.33.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 83 BY 1.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-DbInfo AT ROW 2.81 COL 9 COLON-ALIGNED NO-LABEL
     B-Koble AT ROW 2.86 COL 57
     B-KobleNed AT ROW 2.86 COL 73.6
     RS-1 AT ROW 5.52 COL 41 NO-LABEL NO-TAB-STOP 
     RS-2 AT ROW 6.48 COL 41 NO-LABEL NO-TAB-STOP 
     RS-3 AT ROW 7.43 COL 41 NO-LABEL NO-TAB-STOP 
     FI-Linje1 AT ROW 11.1 COL 19 COLON-ALIGNED
     FI-Linje2 AT ROW 12.29 COL 19 COLON-ALIGNED
     Btn_Cancel AT ROW 15.71 COL 2
     B-AvbrytOverfor AT ROW 15.71 COL 18.8
     B-Overfor AT ROW 15.71 COL 18.8
     B-VisLogg AT ROW 15.71 COL 42
     B-Parametre AT ROW 15.71 COL 59
     Btn_Help AT ROW 15.76 COL 76
     FI-DbKontakt AT ROW 1.95 COL 21 RIGHT-ALIGNED NO-LABEL
     FI-Datatyper AT ROW 4.52 COL 22 RIGHT-ALIGNED NO-LABEL
     FI-FasteRegistre AT ROW 5.52 COL 32 RIGHT-ALIGNED NO-LABEL
     FI-GrunnRegistre AT ROW 6.48 COL 32 RIGHT-ALIGNED NO-LABEL
     FI-Bestillinger AT ROW 7.43 COL 39 RIGHT-ALIGNED NO-LABEL
     FI-Fremdrift AT ROW 10.05 COL 21 RIGHT-ALIGNED NO-LABEL
     RECT-50 AT ROW 5.05 COL 8
     RECT-51 AT ROW 10.52 COL 8
     RECT-52 AT ROW 2.33 COL 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.6 BY 16
         CANCEL-BUTTON Btn_Cancel.


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
         TITLE              = "Overføring av data fra LapTop til sentral database"
         HEIGHT             = 16
         WIDTH              = 90.6
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 105.2
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 105.2
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
ASSIGN 
       FRAME DEFAULT-FRAME:POPUP-MENU       = MENU POPUP-MENU-DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FILL-IN FI-Bestillinger IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN FI-Datatyper IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN FI-DbInfo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-DbKontakt IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN FI-FasteRegistre IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN FI-Fremdrift IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN FI-GrunnRegistre IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN FI-Linje1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Linje2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RS-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RS-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RS-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Overføring av data fra LapTop til sentral database */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Overføring av data fra LapTop til sentral database */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-AvbrytOverfor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AvbrytOverfor C-Win
ON CHOOSE OF B-AvbrytOverfor IN FRAME DEFAULT-FRAME /* Avbryt overføring */
DO:
  assign
    wAvbryt = true.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Koble
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Koble C-Win
ON CHOOSE OF B-Koble IN FRAME DEFAULT-FRAME /* Koble opp */
DO:
  run Oppkobling.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KobleNed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KobleNed C-Win
ON CHOOSE OF B-KobleNed IN FRAME DEFAULT-FRAME /* Koble ned */
DO:
  run Nedkobling.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Overfor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Overfor C-Win
ON CHOOSE OF B-Overfor IN FRAME DEFAULT-FRAME /* Start overføring */
DO:
  run OverforData.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Parametre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Parametre C-Win
ON CHOOSE OF B-Parametre IN FRAME DEFAULT-FRAME /* Parametre */
DO:

  if available LapTop then
    BLOKK:
    do:
      wRecid = recid(LapTop).
      run d-vlaptop.w (input-output wRecid,"Endre").
      if return-value = "AVBRYT" then
        leave BLOKK.
      find LapTop no-lock where
        LapTop.LapTopNr = wLapTopNr no-error.
      wConnectParam = LapTop.OppkoblParam.        
    end. /* BLOKK */
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VisLogg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VisLogg C-Win
ON CHOOSE OF B-VisLogg IN FRAME DEFAULT-FRAME /* Vis logg... */
DO:
  /* Henter siste loggfil fra systemparameter. */
  {syspara.i 50 14 11 wwLogFil}
  
  /* viser resultat til bruker */
  if search(wwLogFil) <> ? then
    os-command no-wait value("notepad.exe") value(wwLogFil).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Avslutt */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {winhlp.i}
END.

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
  do:
     IF CONNECTED(wSentralDB) then 
       do:
         message "Sentral database er fortsatt oppkoblet. Overføring pågår." skip
                 "Programmet kan ikke avsluttet så lenge overføring pågår."
                 view-as alert-box message title "Melding".
         return no-apply.
       end.
     run NedKobling.
     RUN disable_UI.
  end.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Henter defaultverdier. */
{syspara.i 1 1 12 wConnectParam}
/* {syspar2.i 1 1 12 wSentralDB} */
wSentralDB = "SentralDB". /* Ikke tillatt å endre på dette */
assign
  wAvbryt = false.

/* Flagger at systemet kjøres på en LapTop (Innkjøpssystem) */
if valid-handle(wLibHandle) then
  run HentLapTopNr in wLibHandle (output wLapTopNr).
find LapTop no-lock where
  LapTop.LapTopNr = wLapTopNr no-error.
if not available LapTop then
  do:
    message "Ugyldig LapTopNr (" + string(wLapTopNr) + ") satt i ini fil."
            view-as alert-box message title "Melding".
    return.
  end.
else 
  wConnectParam = LapTop.OppkoblParam.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  {lng.i} 

  B-Overfor:move-to-top() in frame DEFAULT-FRAME.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggLoggFilNavn C-Win 
PROCEDURE ByggLoggFilNavn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var wDatoTid as char no-undo.

def buffer bufSysPara for SysPara.

/* Filnavn */
if available SysPara then release SysPara.
{syspara.i 50 14 10 wLogFil}
if wLogFil = "" then
  assign
    wLogFil = "LapTopDDMMAAHHMM.log".

/* Ugyldig filnavn i parameteroppsett. */
if index(wLogFil,"DDMMAAHHMM") = 0 then
  return "02".

/* Lager Dato og Tid strengen */
assign
  wDatoTid = string(day(today),"99") + 
             string(month(today),"99") +
             substring(string(year(today),"9999"),3,2) + 
             substring(string(time,"HH:MM"),1,2) +
             substring(string(time,"HH:MM"),4,2) +
             substring(string(time,"HH:MM"),6,2) +
             substring(string(time,"HH:MM"),8,2).

/* Setter inn dato og klokkeslett i filnavnet. */
OVERLAY(wLogFil, index(wLogFil,"DDMMAAHHMM"), 10, "CHARACTER") = wDatoTid.

/* Katalognavn */
if available SysPara then release SysPara.
if opsys = "unix" 
  then {syspar2.i 50 14 9 wLogKatalog}
else {syspara.i 50 14 9 wLogKatalog}

if wLogKatalog = "" then
  wLogKatalog = if opsys = "unix" then "." else ".".
if substring(wLogKatalog,length(wLogKatalog),1) = "/" or
   substring(wLogKatalog,length(wLogKatalog),1) = "\" then
 wLogKatalog = substring(wLogKatalog,1,length(wLogKatalog) - 1).
    
/* Bygger full path til fil */
assign
  wLogFil = wLogKatalog +
            (if opsys = "unix" 
               then "/"
               else "\") +
            wLogFil.

SETSYSPARA:
do for bufSysPara:
  FIND bufSysPara EXCLUSIVE-LOCK where
    bufSysPara.SysHId = 50 and
    bufSysPara.SysGr  = 14 and
    bufSysPara.ParaNr = 11 NO-ERROR.
    if AVAILABLE bufSysPara then
  ASSIGN bufSysPara.Parameter1 = wLogFil.
end.


return "OK".

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
  DISPLAY FI-DbInfo RS-1 RS-2 RS-3 FI-Linje1 FI-Linje2 FI-DbKontakt FI-Datatyper 
          FI-FasteRegistre FI-GrunnRegistre FI-Bestillinger FI-Fremdrift 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-50 RECT-51 RECT-52 B-Koble B-KobleNed Btn_Cancel B-AvbrytOverfor 
         B-Overfor B-VisLogg B-Parametre Btn_Help 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Footer C-Win 
PROCEDURE Footer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Ferdig melding i loggfil */
  /* Åpner stream til datafil */
  OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
  put stream LoggData unformatted 
    " " skip
    " -----------------------------------------------------------------" skip
    " Ferdigstatus        : " wOkStatus skip
    " Brukt tid           : " string(wBruktTid,"HH:MM:SS") skip
    " Ferdig              : " string(today) " " string(time,"HH:MM:SS") skip
    " -----------------------------------------------------------------" skip(1).
  OUTPUT STREAM LoggData close.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Nedkobling C-Win 
PROCEDURE Nedkobling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  if CONNECTED(wSentralDB) then
    disconnect value(wSentralDB).

  /* Sjekker at databasen er koblet opp. */
  IF not CONNECTED(wSentralDB) then
    assign
      FI-DbInfo:BGCOLOR in frame DEFAULT-FRAME = 12
      FI-DbInfo:screen-value in frame DEFAULT-FRAME = "Ikke oppkoblet".
  else 
    assign
      FI-DbInfo:BGCOLOR in frame DEFAULT-FRAME = 2
      FI-DbInfo:screen-value in frame DEFAULT-FRAME = "Sentral database oppkoblet".

  if connected(wSentralDB) then
    message "Klarte ikke å koble fra " + wSentralDB + " database!"
      VIEW-AS ALERT-BOX WARNING BUTTONS OK Title "Feil ved nedkobling av sentral database!".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Oppkobling C-Win 
PROCEDURE Oppkobling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  if not connected(wSentralDB) then
    connect value(wConnectParam + (if wSentralDB <> ""
                                    then " -ld "
                                    else "") + wSentralDB) no-error.  
  
  /* Sjekker at databasen er koblet opp. */
  IF NOT CONNECTED(wSentralDB) then
    DO:
      MESSAGE "** Klarte ikke å koble opp sentral database! **" skip
              "Kontroller oppkoblingsparametrene og forsøk igjen." skip
              "Følgende parametre ble benyttet:" skip(1)
              wConnectParam + (if wSentralDB <> ""
                                    then " -ld "
                                    else "") + wSentralDB
              VIEW-AS ALERT-BOX message title "Melding".
      assign
        FI-DbInfo:BGCOLOR in frame DEFAULT-FRAME = 12
        FI-DbInfo:screen-value in frame DEFAULT-FRAME = "Ikke oppkoblet".
      return no-apply.
    END.
  
  assign
    FI-DbInfo:BGCOLOR in frame DEFAULT-FRAME = 2
    FI-DbInfo:screen-value in frame DEFAULT-FRAME = "Sentral database oppkoblet".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforBestillinger C-Win 
PROCEDURE OverforBestillinger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  do with frame DEFAULT-FRAME:
    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = "Overfører artikler, ordrer og bestillinger...".
    run ov-artbas.p 
      (input input RS-3,
       INPUT wLogFil,
       INPUT wwLogFil,
       INPUT wLogKatalog, 
       INPUT wBruktTid, 
       INPUT wStartTid, 
       INPUT wOkStatus
      ).
    if return-value = "AVBRYT" then 
      return "AVBRYT".
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforData C-Win 
PROCEDURE OverforData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  run ByggLoggFilNavn.
  if return-value = "02" then
    do:
      message "Feil på loggfil satt i systemparameter (" wLogFil ")."
              view-as alert-box message title "Melding".
      return.
    end.
  
  assign 
    wOkStatus = "OK"
    wStartTid = time.

  /* Åpner stream til datafil */
  OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
  /* Åpner ningsmelding i loggfil */
  put stream LoggData unformatted 
    " " skip
    " -----------------------------------------------------------------" skip
    " Overføring av data fra LapTop til SentralDB startet         " skip
    " " string(today) " " string(time,"HH:MM:SS") " " userid("dictdb") skip
    " -----------------------------------------------------------------" skip.
  OUTPUT STREAM LoggData close.

  if not connected(wSentralDB) then
    do:
      message "Sentral database er ikke oppkoblet." skip
              "Utfør oppkobling før overføring startes."
        VIEW-AS ALERT-BOX WARNING BUTTONS OK Title "Melding".

      OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
      put stream LoggData unformatted skip(1)
      " SentralDB ikke oppkoblet ved start av overføring." skip 
        "(" + 
        wConnectParam + 
        (if wSentralDB <> ""
          then " -ld "
          else "") + 
        wSentralDB + ")." skip. 
      OUTPUT STREAM LoggData close.
      return no-apply.
    end.

  assign 
    FI-Linje1:screen-value in frame DEFAULT-FRAME = "Overføring starter. Vent litt...".
    B-AvbrytOverfor:move-to-top() in frame DEFAULT-FRAME.
  pause 1 no-message.
  
  OVERFOR:
  do:
    assign 
      wBruktTid = time - wStartTid
      FI-Linje1:screen-value in frame DEFAULT-FRAME = "Overfører faste registre".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted skip(1)
      (if input RS-1 = 2
         then " Overfører nye poster faste registre ---------  " 
       else if input RS-1 = 1
         then " Overfører nye og endrede poster faste registre "
       else   " Ingen overføring av faste registre ----------  ")         
      string(wStartTid,"HH:MM:SS") "  " string(wBruktTid,"HH:MM:SS") skip. 
    OUTPUT STREAM LoggData close.
    run OverforFasteRegistre.
    if return-value = "AVBRYT" then 
      do:
        wAvbryt = true.
        leave OVERFOR.
      end.

    assign 
      wBruktTid = time - wStartTid
      FI-Linje1:screen-value in frame DEFAULT-FRAME = "Overfører grunnregistre".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted skip(1)
      (if input RS-2 = 2
         then " Overfører nye poster grunnregistre ----------  " 
       else if input RS-2 = 1
         then " Overfører nye og endrede poster grunnregistre  "
       else   " Ingen overføring av faste av grunnregistre --  ")         
      string(wStartTid,"HH:MM:SS") "  " string(wBruktTid,"HH:MM:SS") skip. 
    OUTPUT STREAM LoggData close.
    run OverforGrunnregistre.
    if return-value = "AVBRYT" then
      do:
        wAvbryt = true.
        leave OVERFOR.
      end.

    assign 
      wBruktTid = time - wStartTid
      FI-Linje1:screen-value in frame DEFAULT-FRAME = "Overfører artikler, bestillinger og ordrer".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted skip(1)
      " Overfører artikler, bestillinger og ordrer --  " 
      string(wStartTid,"HH:MM:SS") "  " string(wBruktTid,"HH:MM:SS") skip. 
    OUTPUT STREAM LoggData close.
    run OverforBestillinger.
    if return-value = "AVBRYT" then
      do:
        wAvbryt = true.
        leave OVERFOR.
      end.
  end. /* OVERFOR */

  OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
  put stream LoggData unformatted skip(1)
    " Kobler ned den sentrale databasen -----------  "  
    string(wStartTid,"HH:MM:SS") "  " string(wBruktTid,"HH:MM:SS") skip. 
  OUTPUT STREAM LoggData close.
  RUN Nedkobling.

  /* Sjekker at databasen er koblet opp. */
  IF not CONNECTED(wSentralDB) then
    assign
      FI-DbInfo:BGCOLOR in frame DEFAULT-FRAME = 12
      FI-DbInfo:screen-value in frame DEFAULT-FRAME = "Ikke oppkoblet".
  else 
    assign
      FI-DbInfo:BGCOLOR in frame DEFAULT-FRAME = 2
      FI-DbInfo:screen-value in frame DEFAULT-FRAME = "Sentral database oppkoblet".

  if wAvbryt = false then
    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = "Overføring av data til sentral database er ferdig.".
  else do:
    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = "Overføring av data AVBRUTT!.".  
    wAvbryt = false.
  end.
  pause 1 no-message.
  assign 
    FI-Linje1:screen-value in frame DEFAULT-FRAME = "".
    B-Overfor:move-to-top() in frame DEFAULT-FRAME.

  /* Legger ut sluttmelding i filen. */
  run Footer.  
  
  /* viser resultat til bruker */
  if search(wLogFil) <> ? then
    os-command no-wait value("notepad.exe") value(wLogFil).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforFasteRegistre C-Win 
PROCEDURE OverforFasteRegistre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  BLOKKEN:
  do with frame DEFAULT-FRAME:
    if input RS-1 = 3 then
      leave BLOKKEN.
      
    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører brukskoder...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører brukskoder..." skip.
    OUTPUT STREAM LoggData close.
    run ov-anv-kod.p (input input RS-1).
    if return-value = "AVBRYT" then 
      return "AVBRYT".

    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører fargekoder...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører fargekoder..." skip.
    OUTPUT STREAM LoggData close.
    run ov-farg.p (input input RS-1).
    if return-value = "AVBRYT" then 
      return "AVBRYT".

    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører sesongkoder...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører sesongkoder..." skip.
    OUTPUT STREAM LoggData close.
    run ov-sasong.p (input input RS-1).
    if return-value = "AVBRYT" then 
      return "AVBRYT".

    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører forkoder...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører forkoder..." skip.
    OUTPUT STREAM LoggData close.
    run ov-foder.p (input input RS-1).
    if return-value = "AVBRYT" then 
      return "AVBRYT".

    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører hovedgrupper...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører hovedgrupper..." skip.
    OUTPUT STREAM LoggData close.
    run ov-huvgr.p (input input RS-1).
    if return-value = "AVBRYT" then 
      return "AVBRYT".

    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører innersålekoder...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører innersålekoder..." skip.
    OUTPUT STREAM LoggData close.
    run ov-innersula.p (input input RS-1).
    if return-value = "AVBRYT" then 
      return "AVBRYT".

    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører kategorier...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører kategorier..." skip.
    OUTPUT STREAM LoggData close.
    run ov-kategori.p (input input RS-1).
    if return-value = "AVBRYT" then 
      return "AVBRYT".
      
    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører læstkoder".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører læstkoder" skip.
    OUTPUT STREAM LoggData close.
    run ov-last-sko.p (input input RS-1).
    if return-value = "AVBRYT" then 
      return "AVBRYT".
      
    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører materialkoder...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører materialkoder..." skip.
    OUTPUT STREAM LoggData close.
    run ov-material.p (input input RS-1).
    if return-value = "AVBRYT" then 
      return "AVBRYT".
      
    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører momskoder...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører momskoder..." skip.
    OUTPUT STREAM LoggData close.
    run ov-moms.p (input input RS-1).
    if return-value = "AVBRYT" then 
      return "AVBRYT".
      
    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører overdelkoder...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører overdelkoder..." skip.
    OUTPUT STREAM LoggData close.
    run ov-ovandel.p (input input RS-1).
    if return-value = "AVBRYT" then 
      return "AVBRYT".
      
    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører faste provisjonskoder...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører faste provisjonskoder..." skip.
    OUTPUT STREAM LoggData close.
    run ov-prov.p (input input RS-1).
    if return-value = "AVBRYT" then 
      return "AVBRYT".
      
    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører rabattkoder...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører rabattkoder..." skip.
    OUTPUT STREAM LoggData close.
    run ov-rabatt.p (input input RS-1).
    if return-value = "AVBRYT" then 
      return "AVBRYT".
      
    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører sesongkoder...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører sesongkoder..." skip.
    OUTPUT STREAM LoggData close.
    run ov-sasong.p (input input RS-1).
    if return-value = "AVBRYT" then 
      return "AVBRYT".
      
    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører slitesålekoder...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører slitesålekoder..." skip.
    OUTPUT STREAM LoggData close.
    run ov-slitsula.p (input input RS-1).
    if return-value = "AVBRYT" then 
      return "AVBRYT".
      
    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører størrelsestyper...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører størrelsestyper..." skip.
    OUTPUT STREAM LoggData close.
    run ov-strtype.p (input input RS-1).
    if return-value = "AVBRYT" then 
      return "AVBRYT".
      
    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører størrelsesdefinisjonene...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører størrelsesdefinisjonene..." skip.
    OUTPUT STREAM LoggData close.
    run ov-strtstr.p (input input RS-1).
    if return-value = "AVBRYT" then 
      return "AVBRYT".
      
    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører valutakoder...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører valutakoder..." skip.
    OUTPUT STREAM LoggData close.
    run ov-valuta.p (input input RS-1).
    if return-value = "AVBRYT" then 
      return "AVBRYT".
      
    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører varegrupper...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører varegrupper..." skip.
    OUTPUT STREAM LoggData close.
    run ov-vargr.p (input input RS-1).
    if return-value = "AVBRYT" then 
      return "AVBRYT".
      
    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører varegruppekategorier...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører varegruppekategorier..." skip.
    OUTPUT STREAM LoggData close.
    run ov-vgkat.p (input input RS-1).
    if return-value = "AVBRYT" then 
      return "AVBRYT".
  end. /* BLOKKEN */
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforGrunnregistre C-Win 
PROCEDURE OverforGrunnregistre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  BLOKKEN:
  do with frame DEFAULT-FRAME:
    if input RS-2 = 3 then
      leave BLOKKEN.
  
    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører leverandørregister...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører leverandørregister..." skip.
    OUTPUT STREAM LoggData close.
    run ov-levbas.p (input input RS-2).
    if return-value = "AVBRYT" then 
      return "AVBRYT".
  
    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører leverandørsortimenter...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører leverandørsortimenter..." skip.
    OUTPUT STREAM LoggData close.
    run ov-levsort.p (input input RS-2).
    if return-value = "AVBRYT" then 
      return "AVBRYT".

    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører størrelsesdefinisjoner...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører størrelsesdefinisjoner..." skip.
    OUTPUT STREAM LoggData close.
    run ov-levsant.p (input input RS-2).
    if return-value = "AVBRYT" then 
      return "AVBRYT".

    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører produsenter...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører produsenter..." skip.
    OUTPUT STREAM LoggData close.
    run ov-produsent.p (input input RS-2).
    if return-value = "AVBRYT" then 
      return "AVBRYT".

    assign FI-Linje1:screen-value in frame DEFAULT-FRAME = " Overfører varemerker...".
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    put stream LoggData unformatted 
       " Overfører varemerker..." skip.
    OUTPUT STREAM LoggData close.
    run ov-varemerke.p (input input RS-2).
    if return-value = "AVBRYT" then 
      return "AVBRYT".
  end. /* BLOKKEN */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

