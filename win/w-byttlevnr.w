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
def var wEDB-System      as char no-undo.
def var wTabell          as char no-undo.

/* Local Variable Definitions ---                                       */
def var wSvar as log no-undo.

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
&Scoped-Define ENABLED-OBJECTS BUTTON-SokLev1 FI-LevNr1 FI-LevNr2 ~
T-EndreLevNr Btn_Done B-Start Btn_Help BUTTON-SokLev2 RECT-58 
&Scoped-Define DISPLAYED-OBJECTS FI-LevNr1 FI-LevNamn1 FI-LevNr2 ~
FI-LevNamn2 T-EndreLevNr FI-Info 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Start 
     LABEL "Start flytting" 
     SIZE 20 BY 1.14.

DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Avslutt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help DEFAULT 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SokLev1 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.6 BY 1.05.

DEFINE BUTTON BUTTON-SokLev2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.6 BY 1.05.

DEFINE VARIABLE FI-Info AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 82 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNamn1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNamn2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr1 AS INTEGER FORMAT "zzzzz9":U INITIAL 0 
     LABEL "Fra leverandør" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr2 AS INTEGER FORMAT "zzzzz9":U INITIAL 0 
     LABEL "Til leverandør" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-58
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 85 BY 6.19.

DEFINE VARIABLE T-EndreLevNr AS LOGICAL INITIAL no 
     LABEL "Gi gammel leverandør nytt nummer hvis ny leverandør ikke finnes fra før." 
     VIEW-AS TOGGLE-BOX
     SIZE 80 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-SokLev1 AT ROW 3.38 COL 34.4
     FI-LevNr1 AT ROW 3.38 COL 21 COLON-ALIGNED
     FI-LevNamn1 AT ROW 3.38 COL 37.4 COLON-ALIGNED NO-LABEL
     FI-LevNr2 AT ROW 4.57 COL 21 COLON-ALIGNED
     FI-LevNamn2 AT ROW 4.57 COL 37.4 COLON-ALIGNED NO-LABEL
     T-EndreLevNr AT ROW 6.24 COL 5
     FI-Info AT ROW 7.19 COL 1 COLON-ALIGNED NO-LABEL
     Btn_Done AT ROW 9.1 COL 2
     B-Start AT ROW 9.1 COL 34
     Btn_Help AT ROW 9.1 COL 72
     BUTTON-SokLev2 AT ROW 4.57 COL 34.4
     RECT-58 AT ROW 2.43 COL 1.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 87 BY 9.43
         DEFAULT-BUTTON Btn_Done.


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
         TITLE              = "Bytte leller flytte everandørnummer/leverandørs artikkler"
         HEIGHT             = 9.43
         WIDTH              = 87
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 87
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 87
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
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevNamn1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevNamn2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Bytte leller flytte everandørnummer/leverandørs artikkler */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Bytte leller flytte everandørnummer/leverandørs artikkler */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Start
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Start C-Win
ON CHOOSE OF B-Start IN FRAME DEFAULT-FRAME /* Start flytting */
DO:
  if input FI-LevNr1 = input FI-LevNr2 then
  do:
    message "Du kan ikke flytte fra - til  samme leverandørnummer!"
      view-as alert-box message title "Melding".
    return no-apply.
  end.

  if input FI-LevNr1 = 0 or input FI-LevNr2 = 0 then
  do:
    message "Fra eller Til leverandørnummer er ikke angitt!"
      view-as alert-box message title "Melding".
    return no-apply.
  end.
  
  assign
    T-EndreLevNr.
    
  /* Sjekker at mottagende leverandør finnes. Det er en forutsettning når ikke */
  /* leverandøren skal endre sitt leverandørnummer.                            */
  if T-EndreLevNr = false then
    do:
      if not can-find(LevBas where LevBas.LevNr = input FI-LevNr2) then
        do:
          message "Det finnes ingen leverandør å flytte til!" 
                  view-as alert-box message title "Melding".
          return no-apply.
        end.
    end.
  
  /* Sjekker at mottagende leverandør IKKE finnes. Det er en forutsettning når */
  /* leverandøren skal endre sitt leverandørnummer.                            */
  else if T-EndreLevNr = true then
    do:
      if can-find(LevBas where LevBas.LevNr = input FI-LevNr2) then
        do:
          message "Det finnes allerede en leverandør med dette leveradnørnummer!" 
                  view-as alert-box message title "Melding".
          return no-apply.
        end.
    end.
    
  /* Sjekker at det ikke finnes statistikk på noen av leverandørene hvis kun */
  /* artikklene skal flyttes.                                                */
  if T-EndreLevNr = false then
    do:
      /* Sjekker om noen av leverandørene har stiatistikker.             */
      /* Har en eller begge av disse statistikker, avbrytes operasjonen. */
      if can-find(first StLinje where
          StLinje.StTypeId   = "LEVERAN" and
          StLinje.DataObjekt = string(FI-LevNr1,"999999")) or
        can-find(first StLinje where
          StLinje.StTypeId   = "LEVERAN" and
          StLinje.DataObjekt = string(FI-LevNr2,"999999")) then
        do:
          message "En eller begge av leverandørene har statistikker. Flytting kan ikke utføres!" 
                  view-as alert-box message title "Melding".
          return no-apply.
        end.
    end.  

  /* Flytter artikkler */
  assign
    FI-LevNr1
    FI-LevNr2.

  run FlyttLev.

  /* Pause så bruker får se info i skjermen. */
  message "Ferdig med flytting!" view-as alert-box message title "Melding".

  /* Rydder opp etter kjøring. */
  assign
    FI-LevNr1    = 0
    FI-LevNr2    = 0
    FI-LevNamn1  = ""
    FI-LevNamn2  = ""
    FI-Info      = ""
    T-EndreLevNr = false.
    
  display
    FI-LevNr1   
    FI-LevNr2   
    FI-LevNamn1 
    FI-LevNamn2 
    FI-Info
    T-EndreLevNr
  with frame DEFAULT-FRAME.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME DEFAULT-FRAME /* Avslutt */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokLev1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLev1 C-Win
ON CHOOSE OF BUTTON-SokLev1 IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-LevNr1
DO:
  {soek.i
    &Felt       = FI-LevNr1
    &Frame-Name = DEFAULT-FRAME
    &Program    = d-blevbas.w 
    &OptDisp    = "LevBas.LevNr    when available LevBas @ FI-LevNr1 
                   LevBas.LevNamn  when available LevBas @ FI-LevNamn1"
    &PostRun    = "find LevBas no-lock where
                     recid(LevBas) = int(return-value) no-error."
  }
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokLev2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLev2 C-Win
ON CHOOSE OF BUTTON-SokLev2 IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-LevNr2
DO:
  {soek.i
    &Felt       = FI-LevNr2
    &Frame-Name = DEFAULT-FRAME
    &Program    = d-blevbas.w 
    &OptDisp    = "LevBas.LevNr    when available LevBas @ FI-LevNr2 
                   LevBas.LevNamn  when available LevBas @ FI-LevNamn2"
    &PostRun    = "find LevBas no-lock where
                     recid(LevBas) = int(return-value) no-error."
  }
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevNr1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevNr1 C-Win
ON TAB OF FI-LevNr1 IN FRAME DEFAULT-FRAME /* Fra leverandør */
or "RETURN":U of FI-LevNr1
DO:
do with frame DEFAULT-FRAME:
  if input FI-LevNr1 = 0 or 
    not can-find(LevBas where 
      LevBas.LevNr = input FI-LevNr1) then
    do:
      display "" @ FI-LevNamn1.
      message "Ugyldig leverandørnummer!"
        view-as alert-box message title "Melding".
      return no-apply.
    end.
  find LevBas no-lock where
    LevBas.LevNr = input FI-LevNr1.
  FI-LevNamn1 = LevBas.LevNamn.
  display 
    FI-LevNamn1.
end.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevNr2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevNr2 C-Win
ON TAB OF FI-LevNr2 IN FRAME DEFAULT-FRAME /* Til leverandør */
or "RETURN":U of FI-LevNr2
DO:
do with frame DEFAULT-FRAME:
  if input FI-LevNr1 = input FI-LevNr2 then
    do:
      message "Du kan ikke flytte fra - til  samme leverandørnummer!"
        view-as alert-box message title "Melding".
      return no-apply.
    end.

  /* Det er tillatt å sette ukjent leverandør inn her. */
  /* Flytting av leverandør til nytt nummer.           */
  find LevBas no-lock where
    LevBas.LevNr = input FI-LevNr2 no-error.
  if available LevBas then
    FI-LevNamn2 = LevBas.LevNamn.
  else 
    FI-LevNamn2 = if input FI-LevNr2 > 0 
                    then "[** Nytt leverandørnummer **]"
                    else "".
  display  FI-LevNamn2.
end.  
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
{genlib.i
  &NoLibCall  = "Nei"
  &WindowName = "FlyttLeverandør"
}

/* Henter parametre for konvertering. */
{syspara.i 1 2 1000 wEDB-System}
{syspar2.i 1 2 1000 wTabell}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  {lng.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY FI-LevNr1 FI-LevNamn1 FI-LevNr2 FI-LevNamn2 T-EndreLevNr FI-Info 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BUTTON-SokLev1 FI-LevNr1 FI-LevNr2 T-EndreLevNr Btn_Done B-Start 
         Btn_Help BUTTON-SokLev2 RECT-58 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlyttLev C-Win 
PROCEDURE FlyttLev :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  def var wAntall as int no-undo.
  def var wLoop   as int no-undo.

  /* Leser alle FRA leverandørs artikkler og flytter disse til TIL leverandøren. */
  DO TRANSACTION:
    for each ArtBas exclusive-lock where
      ArtBas.LevNr = FI-LevNr1:
    
      /* Flytter artikkelen og teller antall artikkler som er flyttet.. */
      assign
        wAntall = wAntall + 1.
        ArtBas.LevNr = FI-LevNr2.
        
      /* Info til bruker */
      FI-Info = "Flytter artikkel: " + 
                string(ArtBAs.Vg) + "/" + 
                string(ArtBAs.LopNr) + " " + 
                ArtBAs.Beskr + " LevKod: " + 
                ArtBas.LevKod + " (" + string(wAntall) + ").".
      pause 0.
      display FI-Info with frame DEFAULT-FRAME.
    
    end.
    
    /* Flytter leverandøren med tilhørende informasjon.                    */
    /* Ingen beskyttelse her. Går det i dass skal det gå skikkelig i dass. */
    /* Utbakkning blir da håndtert av transaksjonshåndteringen.            */
    if T-EndreLevNr then
      do:
        find LevBas exclusive-lock where
          LevBas.LevNr = FI-LevNr1.
        
        /* Info til bruker */
        FI-Info = "Flytter leverandørens sortimenter.". 
        pause 0.
        display FI-Info with frame DEFAULT-FRAME.

        /* Flytter leverandørens sortimenter. */
        for each LevSort of LevBas exclusive-lock:
          for each LevSAnt of LevSort exclusive-lock:
            assign 
              LevSAnt.LevNr = FI-LevNr2.
          end.
          assign 
            LevSort.LevNr = FI-LevNr2.
        end.
        
        /* Info til bruker */
        FI-Info = "Flytter leverandørens bestillinger.". 
        pause 0.
        display FI-Info with frame DEFAULT-FRAME.

        /* Flytter leverandørens bestillinger.                                  */
        /* Er noen av betillingene innlevert, er det statistikk på leverandøren */
        /* og de kan ikke flyttes!!. Det er sperret tildigere i programmet for  */
        /* flytting av leverandører med statistikk.                             */
        for each BestHode of LevBas:
          assign
            BestHode.LevNr = FI-LevNr2.
        end.
        
        /* Flytter leverandøren. */
        assign 
          LevBas.LevNr = FI-LevNr2.
          
        /* Info til bruker */
        FI-Info = "Flytter leverandøren.". 
        pause 1 no-message.
        display FI-Info with frame DEFAULT-FRAME.
        
      end.
    
    /* FLytter eksterne koblinger */
    do wLoop = 1 to num-entries(wTabell):
      for each KonvReg exclusive-lock where
        KonvReg.EDB-System = wEDB-System and
        KonvReg.Tabell     = entry(wLoop,wTabell) and
        KonvReg.InterntId  = string(FI-LevNr1):
        
        assign
          KonvReg.InterntId  = string(FI-LevNr2).
      end.
    end.
    
  end. /* TRANSACTION */
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

