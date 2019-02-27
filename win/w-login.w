&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Login
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Login 
/*------------------------------------------------------------------------

  File: w-login.w

  Description: Innlogging i Flexi systemet.

  Input Parameters:
      rpBrukerID - Brukers identitet i systemet.
      rpPassord  - Brukers passord i systemet.
      AUTOLOGIN - -param "USER=ken1;ken1"
      Om vi inte har autologin och OSGETENV("PRSWINUSER") = "0" så
      blankas loginnamnet

  Output Parameters:
      rpStatus - Indikerer avbrudd eller OK.

  Author: Tom N›kleby

  Created: 21/3-98

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
&Scoped-define APP-DB  SkoTex
&Scoped-define RAPP-DB WR
&Scoped-define DATA-DB Data
&Scoped-define VPI-DB VPI

/*
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  def var rpStatus   as char no-undo.
  def var rpBrukerID as char no-undo.
  def var rpPassord  as char no-undo.
&ELSE
  def output        parameter rpStatus   as char no-undo.
  def input-output  parameter rpBrukerID as char no-undo.
  def input-output  parameter rpPassord  as char no-undo.
&ENDIF
*/

/* Local Variable Definitions ---                                       */
def var rpStatus       as char no-undo.
def var rpBrukerID     as char no-undo.
def var rpPassord      as char no-undo.
def var wAntallForsok  as int  no-undo.
def var wKundeListe    as char no-undo.
def var wKunder        as char no-undo.
def var wProsjektListe as char no-undo.
def var wLoop          as int  no-undo.
def var wAppDBpf       as char no-undo.
def var wRappDBpf      as char no-undo.
def var wDataDBpf      as char no-undo.
DEF VAR wVpiDBpf       AS CHAR NO-UNDO.
def var wValgtKunde    as int  no-undo.
def var wWindows       as handle no-undo.
DEF VAR ReturnValue    AS INT  NO-UNDO.
def var wVerNr         as char no-undo.
def var wVerDato       as date no-undo.
DEF VAR cAutologin     AS CHARACTER NO-UNDO.
DEF VAR cAutoPassord   AS CHARACTER NO-UNDO.
DEF VAR iCount         AS INTEGER NO-UNDO.

DEF VAR cWintitle      AS CHAR NO-UNDO.
DEF VAR cKunde         AS CHAR NO-UNDO.
DEF VAR cDatabase      AS CHAR NO-UNDO.
DEF VAR cVersion       AS CHAR NO-UNDO.
DEF VAR cBrukerID      AS CHAR NO-UNDO.
DEF VAR cPassord       AS CHAR NO-UNDO.
DEF VAR cPRSWinUser    AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-3 RECT-5 RECT-58 COMBO-BOX-Kunde ~
COMBO-BOX-Prosjekt FILL-IN-BrukerID FILL-IN-Passord Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-Kunde COMBO-BOX-Prosjekt ~
FILL-IN-BrukerID FILL-IN-Passord FI-VerInfo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Login AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY DEFAULT 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.15
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO DEFAULT 
     LABEL "&OK" 
     SIZE 15 BY 1.15
     BGCOLOR 8 .

DEFINE VARIABLE COMBO-BOX-Kunde AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-Prosjekt AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VerInfo AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 29.14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-BrukerID AS CHARACTER FORMAT "X(256)":U 
     LABEL "BrukerID" 
     VIEW-AS FILL-IN 
     SIZE 19.86 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Passord AS CHARACTER FORMAT "X(256)":U 
     LABEL "Passord" 
     VIEW-AS FILL-IN 
     SIZE 19.86 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-3
     FILENAME "icon/prssplashscreen2.bmp":U
     STRETCH-TO-FIT
     SIZE 69 BY 17.62.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70.43 BY 17.96.

DEFINE RECTANGLE RECT-58
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 2.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     COMBO-BOX-Kunde AT ROW 1.23 COL 2 NO-LABEL
     COMBO-BOX-Prosjekt AT ROW 1.23 COL 35.86 COLON-ALIGNED NO-LABEL
     FILL-IN-BrukerID AT ROW 20.69 COL 25 COLON-ALIGNED
     FILL-IN-Passord AT ROW 21.65 COL 25 COLON-ALIGNED BLANK 
     Btn_OK AT ROW 23.15 COL 2
     Btn_Cancel AT ROW 23.15 COL 56
     FI-VerInfo AT ROW 23.62 COL 20.57 COLON-ALIGNED NO-LABEL
     IMAGE-3 AT ROW 2.54 COL 2.14
     RECT-5 AT ROW 2.35 COL 1.57
     RECT-58 AT ROW 20.35 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 71.8 BY 23.52
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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
  CREATE WINDOW C-Login ASSIGN
         HIDDEN             = YES
         TITLE              = "Innlogging"
         HEIGHT             = 23.46
         WIDTH              = 71.43
         MAX-HEIGHT         = 35.65
         MAX-WIDTH          = 204.43
         VIRTUAL-HEIGHT     = 35.65
         VIRTUAL-WIDTH      = 204.43
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Login:LOAD-ICON("icon/sk-ico.ico":U) THEN
    MESSAGE "Unable to load icon: icon/sk-ico.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Login
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR COMBO-BOX COMBO-BOX-Kunde IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-VerInfo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Login)
THEN C-Login:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Login
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Login C-Login
ON END-ERROR OF C-Login /* Innlogging */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  /* IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY. */
  run Avbryt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Login C-Login
ON WINDOW-CLOSE OF C-Login /* Innlogging */
DO:
  run Avbryt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Login
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  assign
    rpStatus = "Avbryt".
  run Avbryt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Login
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:

  run Oppkobling. /* Kobler opp mot valgt database.         */
  if not connected("{&APP-DB}") then
    do:
      message "Database '{&APP-DB}' er ikke oppkoblet!"
        VIEW-AS ALERT-BOX WARNING BUTTONS OK Title "Feil ved innlogging!".
      return no-apply.
    end.
  if not connected("{&RAPP-DB}") then
    do:
      message "Rapportdatabase  '{&RAPP-DB}' er ikke oppkoblet!"
        VIEW-AS ALERT-BOX WARNING BUTTONS OK Title "Feil ved innlogging!".
      return no-apply.
    end.
  IF wDataDBpf <> "" THEN
  DO:
  if not connected("{&DATA-DB}") then
    do:
      message "Data database  '{&DATA-DB}' er ikke oppkoblet!"
        VIEW-AS ALERT-BOX WARNING BUTTONS OK Title "Feil ved innlogging!".
      return no-apply.
    end.
  END.

  assign
    FILL-IN-BrukerID
    FILL-IN-Passord.
    
  If SetUserId(FILL-IN-BrukerID, FILL-IN-Passord, "{&APP-DB}") then
    do:
      assign
        rpStatus = "OK"
        rpBrukerID = FILL-IN-BrukerID
        rpPassord  = FILL-IN-Passord.
      run Avslutt.    /* Avslutter innlogging og starter Flexi. */
    end.
  else do:
    MESSAGE "Ugyldig brukerid eller passord for database {&APP-DB}!" 
      VIEW-AS ALERT-BOX WARNING BUTTONS OK Title "Feil ved innlogging!".
    return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-Kunde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-Kunde C-Login
ON VALUE-CHANGED OF COMBO-BOX-Kunde IN FRAME DEFAULT-FRAME
DO:
  run KundeProsjekt (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-Prosjekt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-Prosjekt C-Login
ON VALUE-CHANGED OF COMBO-BOX-Prosjekt IN FRAME DEFAULT-FRAME
DO:
  run KundeProsjekt (2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Login 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
  do:
    IF VALID-HANDLE(wWindows) then 
      DELETE PROCEDURE wWindows.
    
    /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
  end.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

RUN SetLoginTranslate.

/* Initiering */
assign
  wAntallForsok = 0.

/* test om vi skall logga in automatiskt */
IF TRIM(SESSION:PARAMETER) <> "" THEN DO iCount = 1 TO NUM-ENTRIES(TRIM(SESSION:PARAMETER)):
    IF ENTRY(iCount,TRIM(SESSION:PARAMETER)) BEGINS "USER" THEN DO:
        ASSIGN cAutoLogin = TRIM(ENTRY(iCount,TRIM(SESSION:PARAMETER))).
        IF NUM-ENTRIES(cAutoLogin,"=") = 2 THEN DO:
            ASSIGN cAutoLogin = ENTRY(2,cAutoLogin,"=").
            IF NUM-ENTRIES(cAutoLogin,";") = 2 THEN
                ASSIGN cAutoPassord = ENTRY(2,cAutoLogin,";")
                       cAutoLogin   = ENTRY(1,cAutoLogin,";").
            ELSE
                QUIT.
        END.
        ELSE
            ASSIGN cAutoLogin = "".
        LEAVE.
    END.
END.


/* Starter bibliotek for API manuelt. */
RUN VALUE("windows.p") PERSISTENT SET wWindows.

run KundeNavn.          /* Henter kundeliste fra INI filen      */
run KundeProsjekt (1).  /* Henter prosjekt liste fra INI filen. */
run NedKobling.         /* Sikrer at databasene er koblet fra.  */

{swn.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  run skoversion.w (output wVerNr, output wVerDato).
  assign
    FI-VerInfo = wVerNr + "  " + string(wVerDato). 

  IF cAutoLogin <> "" THEN DO:
      ASSIGN C-Login:WINDOW-STATE    = 2.
  END.
  
  CURRENT-WINDOW:X = SESSION:WIDTH-PIXELS / 2 - CURRENT-WINDOW:WIDTH-PIXELS / 2.
  CURRENT-WINDOW:Y = SESSION:HEIGHT-PIXELS / 2 - CURRENT-WINDOW:HEIGHT-PIXELS / 2.

  RUN enable_UI.

  RUN Oversett IN THIS-PROCEDURE.

  run NedKobling. /* Sikrer at databasene er koblet fra.  */

  FILL-IN-BrukerID = "".
  cPRSWinUser = OS-GETENV("PRSWINUSER").
  /* normal inloggning */
  IF cAutoLogin = "" THEN DO:
      IF cPRSWinUser = "0" THEN
          .
      ELSE DO:
          run WinUserName(output FILL-IN-BrukerID).
          FILL-IN-BrukerID:screen-value = FILL-IN-BrukerID.

          RUN SendMessageA in wWindows (FILL-IN-Passord:hWnd, 
                                        204, 
                                        ASC("*"),                                0,
                                        OUTPUT ReturnValue).
      END.
      if FILL-IN-BrukerID = "" then
        apply "Entry" to FILL-IN-BrukerID in frame {&FRAME-NAME}.
      else 
        apply "Entry" to FILL-IN-Passord in frame {&FRAME-NAME}.
  END.
  ELSE
      ASSIGN FILL-IN-BrukerID = cAutoLogin
             FILL-IN-BrukerID:screen-value = FILL-IN-BrukerID
             FILL-IN-Passord  = cAutoPassord
             FILL-IN-Passord:SCREEN-VALUE = FILL-IN-Passord.
  assign
    COMBO-BOX-Prosjekt:screen-value in frame {&FRAME-NAME} = 
           entry(1,entry(1,wProsjektListe),";")
    COMBO-BOX-Kunde:sensitive = if num-entries(COMBO-BOX-Kunde:list-items) = 1
                                  then false
                                  else true 
    COMBO-BOX-Prosjekt:sensitive = if num-entries(COMBO-BOX-Prosjekt:list-items) = 1
                                  then false
                                  else true.
  IF cAutoLogin <> "" THEN
      APPLY "CHOOSE" TO Btn_OK.
  ELSE
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Avbryt C-Login 
PROCEDURE Avbryt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(wWindows) then 
    DELETE PROCEDURE wWindows.

  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  return no-apply "AVBRYT".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Avslutt C-Login 
PROCEDURE Avslutt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Login  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Login)
  THEN DELETE WIDGET C-Login.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Login  _DEFAULT-ENABLE
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
  DISPLAY COMBO-BOX-Kunde COMBO-BOX-Prosjekt FILL-IN-BrukerID FILL-IN-Passord 
          FI-VerInfo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Login.
  ENABLE IMAGE-3 RECT-5 RECT-58 COMBO-BOX-Kunde COMBO-BOX-Prosjekt 
         FILL-IN-BrukerID FILL-IN-Passord Btn_OK Btn_Cancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Login.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Login.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KundeNavn C-Login 
PROCEDURE KundeNavn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Setter kundeliste */
  GET-KEY-VALUE SECTION "KUNDENAVN" KEY "" value wKundeListe.

  assign
    COMBO-BOX-Kunde:List-Items in frame {&FRAME-NAME} = wKundeListe
    COMBO-BOX-Kunde:screen-value in frame {&FRAME-NAME} = entry(1,wKundeListe).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KundeProsjekt C-Login 
PROCEDURE KundeProsjekt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  ipMode   = 1 --> Oppstart - Default kunde.
                          2 --> Valgt kunde, hente prosjekt.
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ipMode as int  no-undo.
  
  def var wTekst      as char no-undo.
  def var wProsjektNr as int no-undo.
  
  if ipMode = 1 then
    VELG-KUNDE:
    do:
      assign wValgtKunde = LOOKUP(COMBO-BOX-Kunde:screen-value in frame {&FRAME-NAME},
                           wKundeListe).

      case wValgtKunde:
        when 1 then
          GET-KEY-VALUE SECTION "Kunder" KEY "Kunde-001" value wProsjektListe.
        when 2 then
          GET-KEY-VALUE SECTION "Kunder" KEY "Kunde-002" value wProsjektListe.
        when 3 then
          GET-KEY-VALUE SECTION "Kunder" KEY "Kunde-003" value wProsjektListe.
        when 4 then
          GET-KEY-VALUE SECTION "Kunder" KEY "Kunde-004" value wProsjektListe.
        when 5 then
          GET-KEY-VALUE SECTION "Kunder" KEY "Kunde-005" value wProsjektListe.
        when 6 then
          GET-KEY-VALUE SECTION "Kunder" KEY "Kunde-006" value wProsjektListe.
        when 7 then
          GET-KEY-VALUE SECTION "Kunder" KEY "Kunde-007" value wProsjektListe.
        when 8 then
          GET-KEY-VALUE SECTION "Kunder" KEY "Kunde-008" value wProsjektListe.
        when 9 then
          GET-KEY-VALUE SECTION "Kunder" KEY "Kunde-009" value wProsjektListe.
        when 10 then
          GET-KEY-VALUE SECTION "Kunder" KEY "Kunde-010" value wProsjektListe.
        when 11 then
          GET-KEY-VALUE SECTION "Kunder" KEY "Kunde-011" value wProsjektListe.
        when 12 then
          GET-KEY-VALUE SECTION "Kunder" KEY "Kunde-012" value wProsjektListe.
        when 13 then
          GET-KEY-VALUE SECTION "Kunder" KEY "Kunde-013" value wProsjektListe.
        when 14 then
          GET-KEY-VALUE SECTION "Kunder" KEY "Kunde-014" value wProsjektListe.
        when 15 then
          GET-KEY-VALUE SECTION "Kunder" KEY "Kunde-015" value wProsjektListe.
        when 16 then
          GET-KEY-VALUE SECTION "Kunder" KEY "Kunde-016" value wProsjektListe.
        when 17 then
          GET-KEY-VALUE SECTION "Kunder" KEY "Kunde-017" value wProsjektListe.
        when 18 then
          GET-KEY-VALUE SECTION "Kunder" KEY "Kunde-018" value wProsjektListe.
        when 19 then
          GET-KEY-VALUE SECTION "Kunder" KEY "Kunde-019" value wProsjektListe.
        when 20 then
          GET-KEY-VALUE SECTION "Kunder" KEY "Kunde-020" value wProsjektListe.
       otherwise
          wProsjektListe = "".
      end case.

      /* Lager prosjektliste */
      assign wTekst = "".
      do wLoop = 1 to num-entries(wProsjektListe):
        wTekst = wTekst + 
                 (if wTekst = "" then "" else ",") +
                 entry(1,entry(wLoop,wProsjektListe),";").
      end.
    end. /* VELG-KUNDE */      

  /* Setter prosjektnummer */
  if ipMode = 1 then
    wProsjektNr = 1.
  else
    wProsjektNr = lookup(COMBO-BOX-Prosjekt:screen-value,COMBO-BOX-Prosjekt:List-Items).
    
  assign
    COMBO-BOX-Prosjekt:List-Items   in frame {&FRAME-NAME} = 
          if ipMode = 1 
            then wTekst 
            else COMBO-BOX-Prosjekt:List-Items   in frame {&FRAME-NAME}
    COMBO-BOX-Prosjekt:screen-value in frame {&FRAME-NAME} = 
          entry(1,entry(wProsjektNr,wProsjektListe),";")
    COMBO-BOX-Prosjekt:sensitive = if num-entries(COMBO-BOX-Prosjekt:list-items) = 1
                                  then false
                                  else true
    wAppDBpf  = entry(2,entry(wProsjektNr,wProsjektListe),";")
    wRappDBpf = entry(3,entry(wProsjektNr,wProsjektListe),";")
    wDataDBpf = entry(4,entry(wProsjektNr,wProsjektListe),";").    
 IF NUM-ENTRIES(wProsjektListe,";") > 4 THEN
   wVpiDBpf = entry(5,entry(wProsjektNr,wProsjektListe),";").  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NedKobling C-Login 
PROCEDURE NedKobling :
/*------------------------------------------------------------------------------
  Purpose:     Kobler ned tilkoblede databaser
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  if CONNECTED("{&APP-DB}") then
    disconnect "{&APP-DB}".
  if connected("{&APP-DB}") then
    message "Klarte ikke † koble fra {&APP-DB} database!"
      VIEW-AS ALERT-BOX WARNING BUTTONS OK Title "Feil ved utlogging!".

  if CONNECTED("{&RAPP-DB}") then
    disconnect "{&RAPP-DB}".
  if connected("{&RAPP-DB}") then
    message "Klarte ikke † koble fra Rapport database!"
      VIEW-AS ALERT-BOX WARNING BUTTONS OK Title "Feil ved utlogging!".

  if CONNECTED("{&DATA-DB}") then
    disconnect "{&DATA-DB}".
  if connected("{&DATA-DB}") then
    message "Klarte ikke † koble fra Data database!"
      VIEW-AS ALERT-BOX WARNING BUTTONS OK Title "Feil ved utlogging!".

  if CONNECTED("{&VPI-DB}") then
    disconnect "{&VPI-DB}".
  if connected("{&VPI-DB}") then
    message "Klarte ikke † koble fra VPI database!"
      VIEW-AS ALERT-BOX WARNING BUTTONS OK Title "Feil ved utlogging!".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Oppkobling C-Login 
PROCEDURE Oppkobling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Kobler opp applikasjonsdatabasen databasen */
IF NOT CONNECTED("{&APP-DB}") then
  do:
    connect -pf value(wAppDBpf) no-error.
    /* Sjekker at databasen er koblet opp. */
    IF NOT CONNECTED("{&APP-DB}") then
      DO:
        MESSAGE "** Klarte ikke å koble opp (Applik) - {&APP-DB} databasen! **" skip
                "Applikasjonen avsluttes." VIEW-AS ALERT-BOX.
        QUIT.
      END.
end.

/* Kobler opp rapportdatabasen */
IF NOT CONNECTED("{&RAPP-DB}") then
  do:
    connect -pf value(wRappDBpf) no-error.
    /* Sjekker at databasen er koblet opp. */
    IF NOT CONNECTED("{&RAPP-DB}") then
      DO:
        MESSAGE "** Klarte ikke å koble opp (Rapport) - {&RAPP-DB} databasen! **" skip
                "Applikasjonen avsluttes." VIEW-AS ALERT-BOX.
        QUIT.
      END.
end.

/* Kobler opp Data databasen */
IF wDataDBpf <> "" THEN
DO:
  IF NOT CONNECTED("{&DATA-DB}") then
  do:
    connect -pf value(wDataDBpf) no-error.
    /* Sjekker at databasen er koblet opp. */
    IF NOT CONNECTED("{&DATA-DB}") then
      DO:
        MESSAGE "** Klarte ikke å koble opp (Data) - {&DATA-DB} databasen! **" skip
                "Applikasjonen avsluttes." VIEW-AS ALERT-BOX.
        QUIT.
      END.
  END.
end.

/* Kobler opp VPI databasen */
IF wVPIDBpf <> "" THEN
DO:
  IF NOT CONNECTED("{&VPI-DB}") then
  do:
    connect -pf value(wVPIDBpf) no-error.

    /* Sjekker at databasen er koblet opp. */
    IF NOT CONNECTED("{&VPI-DB}") then
      DO:
        MESSAGE "** Klarte ikke å koble opp (VPI) - {&VPI-DB} databasen! **" skip
                "Pf fil: " wVPIDBpf
                "Applikasjonen avsluttes." VIEW-AS ALERT-BOX.
        QUIT.
      END.
  END.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Oversett C-Login 
PROCEDURE Oversett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
       IF cWintitle <> "" THEN 
           {&WINDOW-NAME}:TITLE = cWintitle NO-ERROR.
       IF cKunde    <> "" THEN 
           COMBO-BOX-Kunde:LABEL = cKunde NO-ERROR.
       IF cVersion  <> "" THEN 
           FI-VerInfo:LABEL = cVersion NO-ERROR.
       IF cDatabase <> "" THEN 
           COMBO-BOX-Prosjekt:LABEL = cDatabase NO-ERROR.
       IF cBrukerID <> "" THEN
           FILL-IN-BrukerID:LABEL = cBrukerId NO-ERROR.
       IF cPassord  <> "" THEN 
           FILL-IN-Passord:LABEL = cPassord NO-ERROR.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetLoginTranslate C-Login 
PROCEDURE SetLoginTranslate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wTekst as char no-undo.
  
  INIFIL:
  do:
    GET-KEY-VALUE SECTION "LOGINTRANSLATE" KEY "WinTitle" VALUE wTekst.
    IF wTekst <> ? THEN
        ASSIGN cWintitle = TRIM(wTekst).
    GET-KEY-VALUE SECTION "LOGINTRANSLATE" KEY "Kunde" VALUE wTekst.
    IF wTekst <> ? THEN
        ASSIGN cKunde = TRIM(wTekst).
    GET-KEY-VALUE SECTION "LOGINTRANSLATE" KEY "Database" VALUE wTekst.
    IF wTekst <> ? THEN
        ASSIGN cDatabase = TRIM(wTekst).
    GET-KEY-VALUE SECTION "LOGINTRANSLATE" KEY "Version" VALUE wTekst.
    IF wTekst <> ? THEN
        ASSIGN cVersion = TRIM(wTekst).
    GET-KEY-VALUE SECTION "LOGINTRANSLATE" KEY "BrukerID" VALUE wTekst.
    IF wTekst <> ? THEN
        ASSIGN cBrukerID = TRIM(wTekst).
    GET-KEY-VALUE SECTION "LOGINTRANSLATE" KEY "Passord" VALUE wTekst.
    IF wTekst <> ? THEN
        ASSIGN cPassord = TRIM(wTekst).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WinUserName C-Login 
PROCEDURE WinUserName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define output parameter wName as char.
                                                                     
  def var nr as integer no-undo initial 100.
  def var ReturnValue as integer no-undo.
  wName = fill(" ", nr).
  run GetUserNameA in wWindows (input-output wName,
                                input-output nr,
                                output ReturnValue).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

