&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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

DEF VAR bOk               AS LOG  NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
DEF VAR iReturn           AS INT  NO-UNDO.
                          
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.

DEF VAR hQuery            AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Butik ButNamn KortNavn BuAdr BuPonr ~
btnSokPostnr BuPadr BuTel BuKon Sentrallager clButikkNr btnSokCL 
&Scoped-Define DISPLAYED-OBJECTS Butik ButNamn KortNavn BuAdr BuPonr BuPadr ~
BuTel BuKon Sentrallager clButikkNr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSokCL 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokPostnr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE BuAdr AS CHARACTER FORMAT "x(30)" 
     LABEL "Adresse" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE BuKon AS CHARACTER FORMAT "x(30)" 
     LABEL "Kontaktperson" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE BuPadr AS CHARACTER FORMAT "x(20)" 
     LABEL "Postadresse" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE BuPonr AS CHARACTER FORMAT "x(6)" 
     LABEL "Postnummer" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1.

DEFINE VARIABLE BuTel AS CHARACTER FORMAT "x(20)" 
     LABEL "Telefon" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE Butik AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikknummer" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE ButNamn AS CHARACTER FORMAT "x(30)" 
     LABEL "Butikknavn" 
     VIEW-AS FILL-IN 
     SIZE 28.2 BY 1.

DEFINE VARIABLE clButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Sentr.lager" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE KortNavn AS CHARACTER FORMAT "X(8)" 
     LABEL "KortNavn" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1.

DEFINE VARIABLE Sentrallager AS LOGICAL INITIAL no 
     LABEL "Sentrallager" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Butik AT ROW 2.19 COL 15 COLON-ALIGNED HELP
          "Butikknummer"
     ButNamn AT ROW 3.33 COL 15 COLON-ALIGNED HELP
          "Butikkens navn"
     KortNavn AT ROW 4.43 COL 15 COLON-ALIGNED HELP
          "Kortnavn som identifiserer butikken."
     BuAdr AT ROW 5.76 COL 15 COLON-ALIGNED HELP
          "Butikkens adresse"
     BuPonr AT ROW 6.81 COL 15 COLON-ALIGNED HELP
          "Butikkens postnummer"
     btnSokPostnr AT ROW 6.86 COL 28.8
     BuPadr AT ROW 7.86 COL 15 COLON-ALIGNED HELP
          "Butikkens postadresse"
     BuTel AT ROW 8.91 COL 15 COLON-ALIGNED HELP
          "Butikkens telefonnummer"
     BuKon AT ROW 9.91 COL 15 COLON-ALIGNED HELP
          "Kontaktperson. Normalt butikksjef."
     Sentrallager AT ROW 11.48 COL 17
     clButikkNr AT ROW 12.48 COL 15 COLON-ALIGNED HELP
          "Kobling til sentrallager for gruppering."
     btnSokCL AT ROW 12.48 COL 27.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SCROLLABLE SIZE 89.2 BY 15.86.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Customer"
         HEIGHT             = 15.91
         WIDTH              = 89.2
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 15.86
       FRAME DEFAULT-FRAME:WIDTH            = 89.2.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Customer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Customer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Customer */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
  DYNAMIC-FUNCTION("ViewCurrentTab" IN hParent).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokCL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokCL C-Win
ON CHOOSE OF btnSokCL IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cLookupValue  AS CHAR NO-UNDO.

  cLookupValue = "Butik".

  RUN JBoxDLookup.w ("Butiker;Butik;ButNamn;BuAdr;BuPonr;BuPadr;BuKon", "where Sentrallager", INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    clButikkNr:SCREEN-VALUE = cLookupValue.
    APPLY "any-printable" TO clButikkNr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokPostnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokPostnr C-Win
ON CHOOSE OF btnSokPostnr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cLookupValue  AS CHAR NO-UNDO.

  cLookupValue = "Postnr".

  RUN JBoxDLookup.w ("Post;Beskrivelse;Postnr", "where false", INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    BuPonr:SCREEN-VALUE = cLookupValue.
    APPLY "any-printable" TO BuPonr.
    APPLY "tab" TO BuPonr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BuPonr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BuPonr C-Win
ON TAB OF BuPonr IN FRAME DEFAULT-FRAME /* Postnummer */
OR "return" OF BuPonr DO:
  BuPadr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Post","WHERE Postnr = '" + BuPonr:SCREEN-VALUE + "'","Beskrivelse").   
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
ON CLOSE OF THIS-PROCEDURE DO:
  PUBLISH "InvalidateHandle".
  RUN disable_UI.
END.

{incl/wintrigg.i}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
RUN FillTreeView IN hParent.
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
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN Butik:SENSITIVE      = FALSE
         clButikkNr:SENSITIVE = NOT Sentrallager:CHECKED
         btnSokCL:SENSITIVE   = clButikkNr:SENSITIVE
         .
END.

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
  DISPLAY Butik ButNamn KortNavn BuAdr BuPonr BuPadr BuTel BuKon Sentrallager 
          clButikkNr 
      WITH FRAME DEFAULT-FRAME.
  ENABLE Butik ButNamn KortNavn BuAdr BuPonr btnSokPostnr BuPadr BuTel BuKon 
         Sentrallager clButikkNr btnSokCL 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  /* A FieldMap describes the relationship between the retrieved record (in browse or query) and the screen input and disp. fields */
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                             hQuery,
                             FRAME {&FRAME-NAME}:HANDLE,
                             "Butik,ButNamn,KortNavn,BuAdr,BuPonr,BuPadr,BuTel,BuKon,Sentrallager,clButikkNr","",     /* Updateable buffer fields and their correspondign input fields (blank if the same) */
                             "","", /* Fields for display and corresponding display widgets */                             
                             "").          /* If any other fields are used for input these are listed here */

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customupdatevalproc","=butiker_updval.p").

  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hFieldMap).
  DYNAMIC-FUNCTION("createObjectLink",hFieldMap,THIS-PROCEDURE).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

Butik:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bRebuild AS LOG NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF Sentrallager:MODIFIED OR clButikkNr:MODIFIED THEN
    bRebuild = TRUE.
END.

RUN SUPER.

IF bRebuild THEN DO:
  RUN FillTreeView IN hParent.
  DYNAMIC-FUNCTION("setExpandNode" IN hParent,Butik:SCREEN-VALUE).
  RUN "OpenTreeNode" IN hParent.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChangedField C-Win 
PROCEDURE ValueChangedField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icFieldName AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  CASE icFieldName:
    WHEN "Sentrallager" THEN DO:
       IF Sentrallager:CHECKED THEN DO:
         ASSIGN clButikkNr:SCREEN-VALUE = Butik:SCREEN-VALUE
                clButikkNr:SENSITIVE = FALSE
                btnSokCL:SENSITIVE = FALSE
                .
       END.
       ELSE 
         ASSIGN clButikkNr:SENSITIVE = TRUE
                btnSokCL:SENSITIVE = TRUE
                .
    END.
  END CASE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME {&FRAME-NAME}:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParent = ihParent.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hQuery = ihQuery.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

