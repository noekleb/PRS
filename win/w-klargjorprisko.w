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
def var wTotAntall as dec format "zzz,zzz,zz9" no-undo.
def var wWork            as dec  no-undo.
def var wWork2           as dec  no-undo.
def var wWork3           as dec  no-undo.
def var wStop            as log  initial false no-undo.
def var wDato            as date no-undo.
def var wTid             as int  no-undo.
def var wSkjerm          as char no-undo.
def var wTilbud          as log  no-undo.
def var wOk              as int  no-undo.

def buffer bufLager  for Lager.
def buffer bufArtLag for ArtLag.

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
&Scoped-Define ENABLED-OBJECTS B-TellOpp B-Start RECT-47 RECT-48 
&Scoped-Define DISPLAYED-OBJECTS FI-TotAntall FI-Profil FI-Transaksjon ~
FI-Oppdatert FI-StartInfo FI-SluttInfo FI-TidBrukt FI-Tittel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Start 
     LABEL "&Start oppdatering" 
     SIZE 36 BY 1.14.

DEFINE BUTTON B-TellOpp 
     LABEL "&Tell opp" 
     SIZE 10 BY 1.1.

DEFINE VARIABLE FI-Oppdatert AS CHARACTER FORMAT "X(256)":U 
     LABEL "Antall oppdatert" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Profil AS CHARACTER FORMAT "X(256)":U 
     LABEL "Profil" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SluttInfo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ferdig" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE FI-StartInfo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Startet" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TidBrukt AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tid brukt" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Tittel AS CHARACTER FORMAT "X(256)":U INITIAL "           Klargjøring av priskø" 
      VIEW-AS TEXT 
     SIZE 77 BY 1.38
     FGCOLOR 1 FONT 8 NO-UNDO.

DEFINE VARIABLE FI-TotAntall AS CHARACTER FORMAT "X(256)":U 
     LABEL "Antall å oppdatere" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Transaksjon AS CHARACTER FORMAT "X(256)":U 
     LABEL "Poster" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-47
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 5.71.

DEFINE RECTANGLE RECT-48
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 4.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-TellOpp AT ROW 4.1 COL 65
     FI-TotAntall AT ROW 4.14 COL 23 COLON-ALIGNED
     FI-Profil AT ROW 5.33 COL 23 COLON-ALIGNED
     FI-Transaksjon AT ROW 6.52 COL 23 COLON-ALIGNED
     FI-Oppdatert AT ROW 7.71 COL 23 COLON-ALIGNED
     FI-StartInfo AT ROW 10.05 COL 23 COLON-ALIGNED
     FI-SluttInfo AT ROW 11.24 COL 23 COLON-ALIGNED
     FI-TidBrukt AT ROW 12.43 COL 23 COLON-ALIGNED
     B-Start AT ROW 15.29 COL 25
     FI-Tittel AT ROW 1.95 COL 1 COLON-ALIGNED NO-LABEL
     RECT-47 AT ROW 3.62 COL 3
     RECT-48 AT ROW 9.57 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16.14.


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
         TITLE              = "Klargjøring av priskø"
         HEIGHT             = 16.19
         WIDTH              = 80
         MAX-HEIGHT         = 16.19
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16.19
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE                                                          */
ASSIGN 
       B-Start:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "OPPDAT".

/* SETTINGS FOR FILL-IN FI-Oppdatert IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Profil IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-SluttInfo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-StartInfo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TidBrukt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Tittel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TotAntall IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Transaksjon IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Klargjøring av priskø */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Klargjøring av priskø */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Start
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Start C-Win
ON CHOOSE OF B-Start IN FRAME DEFAULT-FRAME /* Start oppdatering */
DO:
  RUN KlargjorPrisKo.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-TellOpp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-TellOpp C-Win
ON CHOOSE OF B-TellOpp IN FRAME DEFAULT-FRAME /* Tell opp */
DO:

  assign
    wTotAntall = 0
    wDato      = today
    wTid       = time
    FI-TotAntall:screen-value in frame DEFAULT-FRAME = "Teller opp transaksjoner...".
    
  BUTIKKER-PROFIL:  
  for each PrisProfil no-lock 
    break by PrisProfil.ProfilNr:
    
    /* For hver profil klarjøres PrisKøen. */
    PRISKO:
    for each PrisKo no-lock where
      PrisKo.ProfilNr       = PrisProfil.ProfilNr and
      PrisKo.AktiveresDato <= wDato /* and
      PrisKo.AktiveresTid  <= wTid */:    
      
      /* Har priskøposten dagens dat, skal tidspunktt sjekkes. */
      IF prisko.AktiveresDato = today then
        do:
          IF PrisKo.AktiveresTid >= time THEN
            NEXT PRISKO.    
        END.

      /* Antall poster som skal klagjøres. */
      assign
        wTotAntall = wTotAntall + 1.
    
    end. /* PRISKO */
  end. /* PROFIL */

  assign
    FI-TotAntall:screen-value in frame DEFAULT-FRAME = 
            " " + string(wTotAntall) + " klagjøres " + 
            string(wDato) + " " +
            string(wTid,"HH:MM:SS").

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
/* Biblotek, inn/utmelding og ON CLOSE OF THIS... */
{genlib.i 
  &NoLibCall      = "Nei"
  &WindowName     = "Klargjøring av priskø"
}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
{lng.i} 
  status input "".
  
  assign
    C-Win:hidden = false.
  APPLY "ENTRY":U TO B-TellOpp.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AvbrytOppdatering C-Win 
PROCEDURE AvbrytOppdatering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
do with frame DEFAULT-FRAME:
  
  apply "choose":U to B-TellOpp.
  
  if wTotAntall = 0 then
    do:
      message "Ingen transaksjoner igjen å oppdatere"
        view-as alert-box MESSAGE title "Melding".
      apply "close":U to this-procedure.
    end.
    
  assign
    B-TellOpp:sensitive  = true
    B-Start:label        = "&Start oppdatering"
    B-Start:Private-Data = "OPPDAT"
    FI-Profil            = ""
    FI-Transaksjon       = ""
    FI-Oppdatert         = ""
    FI-SluttInfo         = ""
    FI-StartInfo         = ""
    FI-TidBrukt          = "".

  display
    FI-Profil
    FI-Transaksjon
    FI-Oppdatert  
    FI-SluttInfo  
    FI-StartInfo  
    FI-TidBrukt   
  with frame DEFAULT-FRAME.

end. /* FRAME */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BruktInfo C-Win 
PROCEDURE BruktInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter wDato      as date no-undo.
  def input parameter wFerdigTid as int  no-undo.
  def input parameter wBruktTid  as int  no-undo.
  
  do with frame DEFAULT-FRAME:
    assign
      FI-SluttInfo:screen-value = string(wDato) + " " +
                                  string(wFerdigTid,"HH:MM:SS")
      FI-TidBrukt:screen-value  = string(wBruktTid,"HH:MM:SS").
  end.

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
  DISPLAY FI-TotAntall FI-Profil FI-Transaksjon FI-Oppdatert FI-StartInfo 
          FI-SluttInfo FI-TidBrukt FI-Tittel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-TellOpp B-Start RECT-47 RECT-48 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KlargjorPrisKo C-Win 
PROCEDURE KlargjorPrisKo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wProgram-Handle as handle no-undo.
  
  DO WITH FRAME {&FRAME-NAME}:
  assign
    wDato = today
    wTid  = time.

  if B-Start:private-data = "OPPDAT" then
    run x-klargjorprisko.w persistent set wProgram-Handle (this-procedure:handle).      
  else if B-Start:private-data = "AVSLUTT" then
    apply "close":U to this-procedure.
  else
    assign wStop = true.
    
  assign
    B-TellOpp:sensitive  = false
    B-Start:label        = "&Avslutt"
    B-Start:Private-Data = "AVSLUTT".
  END.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProfilInfo C-Win 
PROCEDURE ProfilInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter wProfilInfo as char no-undo.
  
  do with frame DEFAULT-FRAME:
    assign
      FI-Profil:screen-value = wProfilInfo.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartInfo C-Win 
PROCEDURE StartInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter wDato     as date no-undo.
  def input parameter wStartTid as int no-undo.
  
  do with frame DEFAULT-FRAME:
    assign
      FI-StartInfo:screen-value = string(wDato) + " " +
                                  string(wStartTid,"HH:MM:SS").
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TransInfo C-Win 
PROCEDURE TransInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter wTransaksjon as char no-undo.
  def input parameter wOppdatert   as char no-undo.

  do with frame DEFAULT-FRAME:
    assign
      FI-Transaksjon:screen-value = wTransaksjon
      FI-Oppdatert:screen-value   = wOppdatert.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

