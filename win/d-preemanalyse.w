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
DEFINE INPUT         PARAMETER cType AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT  PARAMETER rAnalyse AS ROWID    NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS B-Teamnr RECT-1 Btn_OK Btn_Cancel FI_Navn ~
Btn_Help FI_StartDato FI_SluttDato FI_Aktiv 
&Scoped-Define DISPLAYED-OBJECTS FI_AnalyseId FI_Navn FI_teamnr FI_Teamnamn ~
FI_StartDato FI_SluttDato FI_Aktiv 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Teamnr 
     IMAGE-UP FILE "icon/e-sokpr.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Teamnr" 
     SIZE 4.6 BY 1.05 TOOLTIP "Ny post (Alt-N)".

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

DEFINE VARIABLE FI_Aktiv AS LOGICAL FORMAT "J/N" INITIAL NO 
     LABEL "Aktiv" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1.

DEFINE VARIABLE FI_AnalyseId AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Analysid" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE FI_Navn AS CHARACTER FORMAT "X(30)" 
     LABEL "Namn" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FI_SluttDato AS DATE FORMAT "99/99/99" 
     LABEL "Slut datum" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE FI_StartDato AS DATE FORMAT "99/99/99" 
     LABEL "Start datum" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE FI_Teamnamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI_teamnr AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "Teamnr" 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 8.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     B-Teamnr AT ROW 5.1 COL 27.4 NO-TAB-STOP 
     Btn_OK AT ROW 1.48 COL 57
     Btn_Cancel AT ROW 2.71 COL 57
     FI_AnalyseId AT ROW 3.14 COL 17.8 COLON-ALIGNED HELP
          "Unikt nummer på analysen."
     FI_Navn AT ROW 4.14 COL 17.8 COLON-ALIGNED HELP
          "Navn på analysen"
     Btn_Help AT ROW 4.71 COL 57
     FI_teamnr AT ROW 5.14 COL 17.8 COLON-ALIGNED
     FI_Teamnamn AT ROW 5.14 COL 29.8 COLON-ALIGNED NO-LABEL
     FI_StartDato AT ROW 6.14 COL 17.8 COLON-ALIGNED HELP
          "Dato for start av analysen."
     FI_SluttDato AT ROW 7.14 COL 17.8 COLON-ALIGNED HELP
          "Siste dag analysen skal kjøres."
     FI_Aktiv AT ROW 8.14 COL 17.8 COLON-ALIGNED HELP
          "Analysen er aktiv."
     RECT-1 AT ROW 1.48 COL 2
     SPACE(17.19) SKIP(0.94)
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
/* SETTINGS FOR FILL-IN FI_Teamnamn IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI_teamnr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Ny/Ändra analys */
DO:
    ASSIGN FI_AnalyseId 
           FI_Navn 
           FI_SluttDato 
           FI_StartDato 
           FI_teamnr
           FI_Aktiv. 
  IF cType = "Endre" THEN
      FIND CURRENT preemanalyse EXCLUSIVE.
  ELSE DO:
      CREATE preemanalyse.
      ASSIGN preemanalyse.AnalyseId = FI_AnalyseId.
      rAnalyse = ROWID(preemanalyse).
  END.
  ASSIGN preemanalyse.Navn      = FI_Navn     
         preemanalyse.SluttDato = FI_SluttDato
         preemanalyse.StartDato = FI_StartDato
         preemanalyse.teamnr    = FI_teamnr   
         preemanalyse.Aktiv     = FI_Aktiv.   

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


&Scoped-define SELF-NAME B-Teamnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Teamnr Dialog-Frame
ON CHOOSE OF B-Teamnr IN FRAME Dialog-Frame /* Teamnr */
DO:
    DEFINE VARIABLE cLookupValue AS CHARACTER   NO-UNDO.
    cLookupValue = "TeamNr,Beskrivelse".
    RUN JBoxDLookup.w ("Butikkteam;Teamnr;TeamTypeid;Beskrivelse", "where BrGrpNr = 1 and TeamTypeId = 2", INPUT-OUTPUT cLookupValue).
    IF NUM-ENTRIES(cLookupValue,"|") = 2 THEN DO:
        ASSIGN FI_Teamnr   = INT(ENTRY(1,cLookupValue,"|"))
               FI_Teamnamn = ENTRY(2,cLookupValue,"|").
        DISPLAY FI_Teamnr FI_Teamnamn WITH FRAME {&FRAME-NAME}.
    END.
    FI_Aktiv:SENSITIVE = FI_Teamnr <> 0.
    APPLY "ENTRY" TO FI_StartDato.
    RETURN NO-APPLY.
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
      FIND LAST preemanalyse NO-LOCK NO-ERROR.
      FI_Analyseid = IF AVAIL preemanalyse THEN preemanalyse.AnalyseId + 1 ELSE 1.
  END.
  ELSE IF cType = "Endre" THEN DO:
      FIND preemanalyse WHERE ROWID(preemanalyse) = rAnalyse NO-LOCK NO-ERROR.
      ASSIGN
          FI_AnalyseId = preemanalyse.AnalyseId
          FI_Navn      = preemanalyse.Navn     
          FI_teamnr    = preemanalyse.teamnr   
          FI_SluttDato = preemanalyse.SluttDato
          FI_StartDato = preemanalyse.StartDato
          FI_Aktiv     = preemanalyse.Aktiv.
  END.
  RUN enable_UI.
/*   FI_Aktiv:SENSITIVE = FI_Aktiv = TRUE. */

  APPLY "ENTRY" TO FI_Navn IN FRAME {&FRAME-NAME}.
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
  DISPLAY FI_AnalyseId FI_Navn FI_teamnr FI_Teamnamn FI_StartDato FI_SluttDato 
          FI_Aktiv 
      WITH FRAME Dialog-Frame.
  ENABLE B-Teamnr RECT-1 Btn_OK Btn_Cancel FI_Navn Btn_Help FI_StartDato 
         FI_SluttDato FI_Aktiv 
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
      IF INPUT FI_Navn = "" THEN DO:
          MESSAGE "Ange namn"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY "ENTRY" TO FI_Navn.
          RETURN NO-APPLY "AVBRYT".
      END.
      IF INPUT FI_teamnr = 0 THEN DO:
          MESSAGE "Välj temnr"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY "AVBRYT".
      END.
      IF INPUT FI_SluttDato = ? OR INPUT FI_StartDato = ? OR
         INPUT FI_SluttDato < INPUT FI_StartDato THEN DO:
          MESSAGE "Ogiltigt datum" SKIP
              "Slutdato skall vara > startdatum"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY "ENTRY" TO FI_StartDato.
          RETURN NO-APPLY "AVBRYT" .
      END.
      IF YEAR(INPUT FI_StartDato) <> YEAR(INPUT FI_SluttDato) THEN DO:
          MESSAGE "Start- och slutdatum skall vara i samma år"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY "ENTRY" TO FI_StartDato.
          RETURN NO-APPLY "AVBRYT" .
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

