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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT  PARAM icMessage     AS CHAR NO-UNDO.
DEF INPUT  PARAM iiNumSelected AS INT  NO-UNDO.
DEF INPUT  PARAM iiNumTotal    AS INT  NO-UNDO.
DEF OUTPUT PARAM oiReturn      AS INT  NO-UNDO.
DEF OUTPUT PARAM cValues       AS char NO-UNDO.

/* Local Variable Definitions ---                                       */
/* DEF VAR icMessage     AS CHAR NO-UNDO. */
/* DEF VAR iiNumSelected AS INT  NO-UNDO. */
/* DEF VAR iiNumTotal    AS INT  NO-UNDO. */
/* DEF VAR oiReturn      AS INT  NO-UNDO. */
/* DEF VAR cValues       AS char NO-UNDO. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rsAlle fiLevNr B-SokLev Btn_OK Btn_Cancel ~
fi-cMessage fi-cNumTotal fi-cNumSelected 
&Scoped-Define DISPLAYED-OBJECTS rsAlle fiLevNr FILL-IN-21 fi-cMessage ~
fi-cNumTotal fi-cNumSelected 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-SokLev 
     LABEL "..." 
     SIZE 4.6 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fi-cMessage AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 93 BY .95
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-cNumSelected AS CHARACTER FORMAT "X(256)":U 
     LABEL "Antall valgt" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fi-cNumTotal AS CHARACTER FORMAT "X(256)":U 
     LABEL "Totalt antall" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fiLevNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 12.6 BY 1.

DEFINE VARIABLE FILL-IN-21 AS CHARACTER FORMAT "X(256)":U INITIAL "Det blir ikke endret leverandør på PLU artikler og artikler som inngår i en MODELL." 
     VIEW-AS FILL-IN NATIVE 
     SIZE 84 BY 1
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE rsAlle AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Alle", 1,
"Valgte poster", 2
     SIZE 21 BY 1.57 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     rsAlle AT ROW 3.38 COL 32 NO-LABEL
     fiLevNr AT ROW 3.86 COL 67 COLON-ALIGNED HELP
          "'varegruppenummer"
     B-SokLev AT ROW 3.86 COL 82
     FILL-IN-21 AT ROW 5.76 COL 1 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 7.19 COL 65
     Btn_Cancel AT ROW 7.19 COL 81
     fi-cMessage AT ROW 1.95 COL 2 NO-LABEL
     fi-cNumTotal AT ROW 3.52 COL 16 COLON-ALIGNED
     fi-cNumSelected AT ROW 4.33 COL 16 COLON-ALIGNED
     SPACE(64.39) SKIP(3.71)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Endre leverandør"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-cMessage IN FRAME Dialog-Frame
   ALIGN-L                                                              */
ASSIGN 
       fi-cMessage:RESIZABLE IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-21 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Endre leverandør */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokLev Dialog-Frame
ON CHOOSE OF B-SokLev IN FRAME Dialog-Frame /* ... */
OR "MOUSE-SELECT-DBLCLICK":U OF B-SokLev
OR "F10" OF B-SokLev
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "LevNr,LevNamn".

  RUN JBoxDLookup.w ("LevBas;LevNamn;LevNr", "where LevNr > 0", INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO: 
    fiLevNr:SCREEN-VALUE = entry(1,cLookupValue,"|").
    APPLY "return" TO fiLevNr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  ASSIGN fiLevNr rsAlle.
  ASSIGN cValues = (IF fiLevNr <> ?
                      THEN STRING(fiLevNr)
                      ELSE "?")
         oiReturn = rsAlle
         .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{incl/frametrigg.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN InitWindow.
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
  DISPLAY rsAlle fiLevNr FILL-IN-21 fi-cMessage fi-cNumTotal fi-cNumSelected 
      WITH FRAME Dialog-Frame.
  ENABLE rsAlle fiLevNr B-SokLev Btn_OK Btn_Cancel fi-cMessage fi-cNumTotal 
         fi-cNumSelected 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow Dialog-Frame 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iBlanks AS INT NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN iBlanks                      = IF LENGTH(icMessage) < 90 THEN 90 - LENGTH(icMessage) ELSE 2
         iBlanks                      = iBlanks / 2
         fi-cMessage:SCREEN-VALUE     = FILL(" ",iBlanks) + icMessage 
         fi-cNumSelected:SCREEN-VALUE = STRING(iiNumSelected)
         fi-cNumTotal:SCREEN-VALUE    = STRING(iiNumTotal)
         .
/*   IF iiNumSelected > 1 THEN    */
/*     rsAlle:SCREEN-VALUE = "2". */
/*   ELSE                         */
/*     rsAlle:SCREEN-VALUE = "1". */
  rsAlle:SCREEN-VALUE = "2".

  IF iiNumSelected = 0 THEN 
    rsAlle:SENSITIVE = FALSE.

  DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

