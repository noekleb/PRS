&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT         PARAMETER cType      AS CHARACTER   NO-UNDO.
DEFINE INPUT         PARAMETER iAnalyseId AS INTEGER     NO-UNDO.
DEFINE INPUT         PARAMETER cListItems AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT  PARAMETER rAnalyse   AS ROWID       NO-UNDO.
/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cRetValue AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 Btn_OK Btn_Cancel FI_radnr CB-Typ ~
Btn_Help FI_beskr 
&Scoped-Define DISPLAYED-OBJECTS FI_AnalyseId FI_radnr CB-Typ FI_beskr 

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
     LABEL "&Help" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE CB-Typ AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Typ" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE FI_AnalyseId AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Analysid" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE FI_beskr AS CHARACTER FORMAT "X(30)" 
     LABEL "Beskriving" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FI_radnr AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "Radnr" 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 1.48 COL 57
     FI_AnalyseId AT ROW 2.71 COL 14.8 COLON-ALIGNED HELP
          "Unikt nummer på analysen."
     Btn_Cancel AT ROW 2.71 COL 57
     FI_radnr AT ROW 3.71 COL 14.8 COLON-ALIGNED
     CB-Typ AT ROW 4.71 COL 14.8 COLON-ALIGNED
     Btn_Help AT ROW 4.71 COL 57
     FI_beskr AT ROW 5.71 COL 14.8 COLON-ALIGNED HELP
          "Navn på analysen"
     RECT-1 AT ROW 1.48 COL 2
     SPACE(19.19) SKIP(0.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ny/Ändra analys"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI_AnalyseId IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Ny/Ändra analys */
DO:
    ASSIGN FI_AnalyseId 
           FI_radnr
           CB-typ 
           FI_beskr.
  IF cType = "Endre" THEN
      FIND CURRENT preemanalyserad EXCLUSIVE.
  ELSE DO:
      CREATE preemanalyserad.
      ASSIGN preemanalyserad.AnalyseId = FI_AnalyseId.
      rAnalyse = ROWID(preemanalyserad).
  END.
  ASSIGN preemanalyserad.radnr     = FI_radnr
         preemanalyserad.typ       = CB-typ
         preemanalyserad.beskr     = FI_beskr.   

  cRetValue = "OK".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Ny/Ändra analys */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  RUN kontrollerInput.
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Typ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Typ Dialog-Frame
ON VALUE-CHANGED OF CB-Typ IN FRAME Dialog-Frame /* Typ */
DO:
    DEFINE VARIABLE iLU AS INTEGER     NO-UNDO.
  IF cType = "Ny" THEN DO:
      iLU = LOOKUP(CB-Typ:SCREEN-VALUE,CB-Typ:LIST-ITEM-PAIRS).
      FI_Beskr = ENTRY(iLU - 1,CB-Typ:LIST-ITEM-PAIRS).
      DISP FI_Beskr WITH FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  IF cType = "Ny" THEN DO:
      FIND LAST preemanalyserad WHERE preemanalyserad.AnalyseId = iAnalyseId NO-LOCK NO-ERROR.
      FI_Analyseid = iAnalyseId.
      FI_Radnr     = IF AVAIL preemanalyserad THEN preemanalyserad.radnr + 1 ELSE 1.
  END.
  ELSE IF cType = "Endre" THEN DO:
      FIND preemanalyserad WHERE ROWID(preemanalyserad) = rAnalyse NO-LOCK NO-ERROR.
      ASSIGN
          FI_AnalyseId = preemanalyserad.AnalyseId
          FI_beskr     = preemanalyserad.beskr
          FI_radnr     = preemanalyserad.radnr.
  END.
  CB-Typ:LIST-ITEM-PAIRS = cListItems.
  RUN enable_UI.
  IF cType = "Ny" THEN DO:
      CB-Typ:SCREEN-VALUE = ENTRY(2,CB-Typ:LIST-ITEM-PAIRS).
      APPLY "VALUE-CHANGED" TO CB-Typ.
  END.
  ELSE IF cType = "Endre" THEN
      CB-Typ:SCREEN-VALUE = STRING(preemanalyserad.typ).
  APPLY "ENTRY" TO FI_beskr IN FRAME {&FRAME-NAME}.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
RETURN cRetValue.

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
  DISPLAY FI_AnalyseId FI_radnr CB-Typ FI_beskr 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 Btn_OK Btn_Cancel FI_radnr CB-Typ Btn_Help FI_beskr 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kontrollerInput Dialog-Frame 
PROCEDURE kontrollerInput :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
/*       IF INPUT FI_radnr = "" THEN DO:            */
/*           MESSAGE "Ange namn"                    */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*           APPLY "ENTRY" TO FI_beskr.             */
/*           RETURN NO-APPLY "AVBRYT".              */
/*       END.                                       */
      ASSIGN INPUT CB-Typ
             FI_beskr = TRIM(INPUT FI_beskr).
      DISPLAY FI_beskr WITH FRAME {&FRAME-NAME}.
      IF FI_beskr = "" THEN DO:
          MESSAGE "Fyll i beskrivelse"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY "ENTRY" TO FI_beskr.
          RETURN NO-APPLY "AVBRYT" .
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

