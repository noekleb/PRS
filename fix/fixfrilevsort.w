&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var wStop as log no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-44 RECT-45 FI-LevNr1 FI-LevNr2 Btn_OK ~
Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS FI-LevNr1 FI-LevNr2 FI-Info 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Start" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-Info AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr1 AS INTEGER FORMAT "zzzzz9":U INITIAL 0 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr2 AS INTEGER FORMAT "zzzzz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-44
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 43 BY 2.62.

DEFINE RECTANGLE RECT-45
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 61 BY 1.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FI-LevNr1 AT ROW 2.91 COL 16 COLON-ALIGNED
     FI-LevNr2 AT ROW 2.91 COL 29 COLON-ALIGNED NO-LABEL
     FI-Info AT ROW 7.67 COL 1 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 1.95 COL 49
     Btn_Cancel AT ROW 3.38 COL 49
     Btn_Help AT ROW 6.19 COL 49
     RECT-44 AT ROW 1.95 COL 3
     RECT-45 AT ROW 8.86 COL 3
     "Oppretter fri sortiment pr. størrelsestype pr. leverandør" VIEW-AS TEXT
          SIZE 58 BY .62 AT ROW 9.57 COL 5
     "Fra" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.19 COL 18
     "Til" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.19 COL 31
     "Kriterier" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.48 COL 4
          FONT 6
     SPACE(50.79) SKIP(8.99)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Oppretter frie sortiment på leverandør(er)"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   Custom                                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-Info IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Oppretter frie sortiment på leverandør(er) */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Avbryt */
DO:
  assign
    wStop = true.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Start */
DO:
  run OpprettFriSort.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Biblotek, inn/utmelding og ON CLOSE OF THIS... */
{genlib.i &WindowName = "Fix av Lager"}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  RUN enable_UI.
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY FI-LevNr1 FI-LevNr2 FI-Info 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-44 RECT-45 FI-LevNr1 FI-LevNr2 Btn_OK Btn_Cancel Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettFriSort Dialog-Frame 
PROCEDURE OpprettFriSort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wOk          as log format "Ja/Nei" no-undo.
  def var wLoop        as int                 no-undo.
  def var wSkjerm      as char                no-undo.
  def var wTekst1      as char                no-undo.
  def var wTilbud      as log                 no-undo.
  def var wUnique      as char                no-undo.
  def var  wGrense     as int                 no-undo.
  
  {syspara.i 5 4 3 wTekst1}
  {syspara.i 5 4 4 wGrense INT}
  
  assign frame Dialog-Frame
    FI-LevNr1
    FI-LevNr2
    wOk = false
    FI-Info:screen-value in frame Dialog-Frame = "Sett inn avgrensning.".

  message "Skal oppdatering/opprettelse startes? (Grense" + string(wGrense) + ")?"
         view-as alert-box BUTTONS YES-NO title "Start av jobb"
         update wOk.
  if wOk <> true then
    return no-apply.
  
  /* Leser aktuelle leverandører. */
  LEVBAS:
  for each LevBas no-lock where
    LevBas.LevNr >= FI-LevNr1 and
    LevBas.LevNr <= FI-LevNr2:

    /* Bruker avbryter. */
    PROCESS EVENTS.
    if wStop then
      do:
        message "Skal korreksjonsrutinen stoppes? " view-as alert-box buttons YES-NO 
                 set wStop.
        if wStop = true then
          return no-apply.
      end.

    /* Info til bruker. */
    assign
      FI-Info:screen-value in frame Dialog-Frame =
        "Leverandør: " + string(LevBas.LevNr) + " - " +
        LevBas.LevNamn.
        
    /* Løper rundt størrelsestypene */
    STRTYPE:
    for each StrType no-lock where 
      StrType.StrTypeId < wGrense:
      find first LevSort no-lock where
        LevSort.LevNr     = LevBas.LevNr and
        LevSort.StrTypeId = StrType.StrTypeId and
        LevSort.Fri       = true no-error.
      if not available LevSort then
        do:
          /* Finn et ledig navn */
          find LevSort no-lock where
            LevSort.LevNr  = LevBas.LevNr and
            LevSort.SortId = wTekst1 + "-" + StrType.KortNavn no-error.
          wUnique = "".          
          UNIQUE-NAVN:
          do while available LevSort:
            wUnique = wUnique + ".".
            find LevSort no-lock where
              LevSort.LevNr  = LevBas.LevNr and
              LevSort.SortId = wTekst1 + "-" + StrType.KortNavn + wUnique no-error.
            if not available LevSort then
              leave UNIQUE-NAVN.         
          end. /* UNIQUE-NAVN */
        
          create LevSort.
          assign
            LevSort.LevNr       = LevBas.LevNr 
            LevSort.SortId      = wTekst1 + "-" + StrType.KortNavn + wUnique
            LevSort.StrTypeId   = StrType.StrTypeId 
            LevSort.Beskrivelse = StrType.Beskrivelse
            LevSort.Fri         = true.
          for each StrTStr of StrType no-lock:
            find LevSAnt no-lock where
              LevSAnt.LevNr    = LevBas.LevNr and
              LevSAnt.SortId   = wTekst1 + "-" + StrType.KortNavn + wUnique and
              LevSAnt.SoStorl  = StrTStr.SoStorl no-error.
            if not available LevSAnt then
              do:
                create LevSAnt.
                assign
                  LevSAnt.LevNr    = LevBas.LevNr
                  LevSAnt.SortId   = wTekst1 + "-" + StrType.KortNavn + wUnique
                  LevSAnt.SoStorl  = StrTStr.SoStorl
                  LevSAnt.SeqNr    = StrTStr.SeqNr
                  LevSAnt.SoAnt    = 0.
              end.
          end.  
        end.
    end. /* STRTYPE */
    
    
    /* Flagger at oppdatering er OK */
    assign
      wOk = true.
  end. /* LEVBAS */
  
  if available LevSort then release LevSort.
  if available LevSAnt then release LevSAnt.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

