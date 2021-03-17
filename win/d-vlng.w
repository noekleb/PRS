&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File:               d-vlng.w
  Description:        Detaljer for en widget (språk)
  Input Parameters:   INPUT ROWID wRowid, INPUT CHAR wPrgNavn
  Output Parameters:  none
  Author:             Sturla Johnsen
  Created:            18.12.98
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER wRowId   AS ROWID NO-UNDO.
DEF INPUT PARAMETER wPrgNavn AS CHAR  NO-UNDO.
/* Local Variable Definitions ---                                       */
DEF SHARED VAR wDesign AS CHAR NO-UNDO.

DEF VAR wh      AS WIDGET NO-UNDO.
DEF VAR i       AS INTE   NO-UNDO.
DEF VAR wId     AS CHAR   NO-UNDO.
DEF VAR wStart  AS INTE   NO-UNDO.
DEF VAR wRetVal AS CHAR   NO-UNDO INIT "Avbryt".

DEF SHARED TEMP-TABLE tWidgets NO-UNDO
   FIELD wName    AS CHAR  
   FIELD wParent  AS CHAR
   FIELD wType    AS CHAR    /* CELL for browse-kolonne     */
   FIELD wCaption AS CHAR    /* Label, title, radio-buttons */
   FIELD wSv      AS CHAR    /* Screen-value                */
   FIELD wHelp    AS CHAR
   FIELD wTooltip AS CHAR
   FIELD wLi      AS CHAR    /* List-items                  */
   FIELD wDelim   AS CHAR    /* Delimiter for list-items    */
   FIELD wWh      AS WIDGET. /* Handle til widgeten         */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-OK BUTTON-Avbryt COMBO-BOX-Lng ~
EDITOR-ListC FILL-IN-Lng FILL-IN-2 FILL-IN-3 RECT-4 RECT-5 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-CaptionC FILL-IN-CaptionE ~
FILL-IN-SVC FILL-IN-SvE FILL-IN-HelpC FILL-IN-HelpE FILL-IN-ToolTipC ~
FILL-IN-ToolTipE COMBO-BOX-Lng FILL-IN-Type FILL-IN-Lng FILL-IN-Name ~
FILL-IN-2 FILL-IN-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Avbryt AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-OK AUTO-GO 
     LABEL "OK" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE VARIABLE COMBO-BOX-Lng AS CHARACTER FORMAT "X(256)":U INITIAL "Design" 
     LABEL "&Sammenlign med" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE EDITOR-ListC AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 35 BY 5.71 NO-UNDO.

DEFINE VARIABLE EDITOR-ListE AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 34 BY 5.71 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL "List-items/" 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Radio:" 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-CaptionC AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 71 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-CaptionE AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tittel / &Label" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 71 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-HelpC AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-HelpE AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Help" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Lng AS CHARACTER FORMAT "X(256)":U 
     LABEL "Språkkode" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-Name AS CHARACTER FORMAT "X(256)":U 
     LABEL "Navn" 
      VIEW-AS TEXT 
     SIZE 33 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-SVC AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SvE AS CHARACTER FORMAT "X(256)":U 
     LABEL "Screen-&value" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ToolTipC AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ToolTipE AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Tooltip" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Type AS CHARACTER FORMAT "X(256)":U 
     LABEL "Type" 
      VIEW-AS TEXT 
     SIZE 33 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 2.62.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 16.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-CaptionC AT ROW 4.57 COL 15 COLON-ALIGNED NO-LABEL
     FILL-IN-CaptionE AT ROW 5.52 COL 15 COLON-ALIGNED
     EDITOR-ListE AT ROW 6.95 COL 54 NO-LABEL
     FILL-IN-SVC AT ROW 13.14 COL 15 COLON-ALIGNED NO-LABEL
     FILL-IN-SvE AT ROW 14.1 COL 15 COLON-ALIGNED
     FILL-IN-HelpC AT ROW 15.52 COL 15 COLON-ALIGNED NO-LABEL
     FILL-IN-HelpE AT ROW 16.48 COL 15 COLON-ALIGNED
     FILL-IN-ToolTipC AT ROW 17.91 COL 15 COLON-ALIGNED NO-LABEL
     BUTTON-OK AT ROW 17.91 COL 91
     FILL-IN-ToolTipE AT ROW 18.86 COL 15 COLON-ALIGNED
     BUTTON-Avbryt AT ROW 19.1 COL 91
     COMBO-BOX-Lng AT ROW 2.43 COL 70 COLON-ALIGNED
     EDITOR-ListC AT ROW 6.95 COL 17 NO-LABEL
     FILL-IN-Type AT ROW 1.71 COL 15 COLON-ALIGNED
     FILL-IN-Lng AT ROW 1.71 COL 70 COLON-ALIGNED
     FILL-IN-Name AT ROW 2.67 COL 15 COLON-ALIGNED
     FILL-IN-2 AT ROW 8.62 COL 3 COLON-ALIGNED NO-LABEL
     FILL-IN-3 AT ROW 9.33 COL 3 COLON-ALIGNED NO-LABEL
     RECT-4 AT ROW 1.24 COL 2
     RECT-5 AT ROW 4.1 COL 2
     SPACE(13.00) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Detaljer"
         DEFAULT-BUTTON BUTTON-OK CANCEL-BUTTON BUTTON-Avbryt.


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

/* SETTINGS FOR EDITOR EDITOR-ListC IN FRAME Dialog-Frame
   NO-DISPLAY                                                           */
ASSIGN 
       EDITOR-ListC:RETURN-INSERTED IN FRAME Dialog-Frame  = TRUE
       EDITOR-ListC:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR EDITOR EDITOR-ListE IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       EDITOR-ListE:RETURN-INSERTED IN FRAME Dialog-Frame  = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-CaptionC IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-CaptionE IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-HelpC IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-HelpE IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Name IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SVC IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SvE IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-ToolTipC IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-ToolTipE IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Type IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Detaljer */
DO:
  DEF VAR wValue AS CHAR.
  ASSIGN wRetVal = "Ok".
  DO WITH FRAME {&FRAME-NAME}: 
     ASSIGN tWidgets.wCaption = FILL-IN-CaptionE:SCREEN-VALUE
            tWidgets.wSv      = FILL-IN-SvE:SCREEN-VALUE
            tWidgets.wHelp    = FILL-IN-HelpE:SCREEN-VALUE
            tWidgets.wToolTip = FILL-IN-ToolTipE:SCREEN-VALUE.
     EDITOR-ListE:REPLACE(CHR(13),"££",8).
     EDITOR-ListE:REPLACE(CHR(10),"",8).
     IF tWidgets.wLi <> "" THEN DO:
        IF TRIM(tWidgets.wType) = "RADIO-SET" THEN
        DO i = 2 TO NUM-ENTRIES(tWidgets.wLi,tWidgets.wDelim) BY 2:  
           ASSIGN wValue = wValue + (IF wValue <> "" THEN "££" ELSE "") + ENTRY(i,tWidgets.wLi,tWidgets.wDelim).
        END.
        ASSIGN tWidgets.wLi = "".
        DO i = 1 TO NUM-ENTRIES(EDITOR-ListE:SCREEN-VALUE,"££"):
           IF ENTRY(i,EDITOR-ListE:SCREEN-VALUE,"££") = "" THEN NEXT.
           IF TRIM(tWidgets.wType) = "RADIO-SET" THEN 
                ASSIGN tWidgets.wLi = tWidgets.wLi + (IF tWidgets.wLi <> "" THEN tWidgets.wDelim ELSE "") + ENTRY(i,EDITOR-ListE:SCREEN-VALUE,"££") + tWidgets.wDelim + ENTRY(i,wValue,"££").
           ELSE ASSIGN tWidgets.wLi = tWidgets.wLi + (IF tWidgets.wLi <> "" THEN tWidgets.wDelim ELSE "") + ENTRY(i,EDITOR-ListE:SCREEN-VALUE,"££"). 
        END.
     END.   
  END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Detaljer */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-Lng
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-Lng Dialog-Frame
ON VALUE-CHANGED OF COMBO-BOX-Lng IN FRAME Dialog-Frame /* Sammenlign med */
DO:
  IF SELF:SCREEN-VALUE <> "Design" THEN DO:
     FIND Lng WHERE Lng.PrgNavn = ENTRY(1,wPrgNavn,".") AND Lng.Lng = SELF:SCREEN-VALUE NO-LOCK.
     RUN InitCompare(Lng.Tekster).
  END.
  ELSE RUN InitCompare(wDesign).  
  DISPL FILL-IN-CaptionC
        FILL-IN-SvC
        FILL-IN-HelpC 
        FILL-IN-ToolTipC
  WITH FRAME {&FRAME-NAME}.      
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

  FIND tWidgets WHERE ROWID(tWidgets) = wRowid.
  
  RUN InitCombo.
  ASSIGN FILL-IN-Type     = TRIM(tWidgets.wType)
         FILL-IN-Name     = tWidgets.wName
         FILL-IN-CaptionE = tWidgets.wCaption
         FILL-IN-SvE      = tWidgets.wSv
         FILL-IN-HelpE    = tWidgets.wHelp
         FILL-IN-ToolTipE = tWidgets.wToolTip.
         wId              = TRIM(tWidgets.wType) 
                          + tWidgets.wName
                          + tWidgets.wParent.
                          
  ASSIGN wStart           = INDEX(wDesign,wId) + 1.
  
  RUN InitCompare(wDesign).
  RUN ToggleSensitive.
  
  IF tWidgets.wLi <> "" THEN DO:
     IF TRIM(tWidgets.wType) = "RADIO-SET" THEN
     DO i = 1 TO NUM-ENTRIES(tWidgets.wLi,tWidgets.wDelim) BY 2: /* BY kan ikke være expression!! */
        EDITOR-ListE:INSERT-STRING(ENTRY(i,tWidgets.wLi,tWidgets.wDelim) + CHR(13)).
     END.  
     ELSE
     IF LENGTH(tWidgets.wLi) < 200 THEN
     DO i = 1 TO NUM-ENTRIES(tWidgets.wLi,tWidgets.wDelim):
        EDITOR-ListE:INSERT-STRING(ENTRY(i,tWidgets.wLi,tWidgets.wDelim) + CHR(13)).
     END.
  END.
  
  /* Flytter labler */
  ASSIGN wh = FILL-IN-CaptionE:SIDE-LABEL-HANDLE
         wh:Y = wh:Y - 11
         wh = FILL-IN-SvE:SIDE-LABEL-HANDLE
         wh:Y = wh:Y - 11
         wh = FILL-IN-HelpE:SIDE-LABEL-HANDLE
         wh:Y = wh:Y - 11
         wh = FILL-IN-ToolTipE:SIDE-LABEL-HANDLE
         wh:Y = wh:Y - 11.
  {lng.i}  
  ASSIGN FILL-IN-Lng = wCurrLng.
  ON "ALT->" ANYWHERE BELL.     
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
RETURN wRetVal.

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
  DISPLAY FILL-IN-CaptionC FILL-IN-CaptionE FILL-IN-SVC FILL-IN-SvE 
          FILL-IN-HelpC FILL-IN-HelpE FILL-IN-ToolTipC FILL-IN-ToolTipE 
          COMBO-BOX-Lng FILL-IN-Type FILL-IN-Lng FILL-IN-Name FILL-IN-2 
          FILL-IN-3 
      WITH FRAME Dialog-Frame.
  ENABLE BUTTON-OK BUTTON-Avbryt COMBO-BOX-Lng EDITOR-ListC FILL-IN-Lng 
         FILL-IN-2 FILL-IN-3 RECT-4 RECT-5 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCombo Dialog-Frame 
PROCEDURE InitCombo :
/*------------------------------------------------------------------------------
  Purpose:     Initierer combo med sammenlikningsspråk
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR wLi AS CHAR NO-UNDO.
   ASSIGN wLi = "Design".
   FOR EACH Lng FIELDS(Lng) WHERE Lng.PrgNavn = ENTRY(1,wPrgNavn,".") NO-LOCK:
       ASSIGN wLI = wLI + "," + Lng.Lng.
   END.
   ASSIGN COMBO-BOX-Lng:LIST-ITEMS IN FRAME {&FRAME-NAME} = wLI.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCompare Dialog-Frame 
PROCEDURE InitCompare :
/*------------------------------------------------------------------------------
  Purpose:     Viser sammenlikning
  Parameters:  INPUT CHAR wSamStr
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wSamStr AS CHAR NO-UNDO.                        
  ASSIGN wStart = INDEX(wSamStr,wId) + 1.
  IF wStart > 0 THEN DO WITH FRAME {&FRAME-NAME}:
     ASSIGN FILL-IN-CaptionC = ENTRY(2,SUBSTR(wSamStr,wStart),"$")
            FILL-IN-SvC      = ENTRY(4,SUBSTR(wSamStr,wStart),"$")
            FILL-IN-HelpC    = ENTRY(5,SUBSTR(wSamStr,wStart),"$") 
            FILL-IN-ToolTipC = ENTRY(6,SUBSTR(wSamStr,wStart),"$")
            EDITOR-ListC:SCREEN-VALUE = "".
            
     IF TRIM(tWidgets.wType) = "RADIO-SET" THEN
     DO i = 1 TO NUM-ENTRIES(ENTRY(3,SUBSTR(wSamStr,wStart),"$"),tWidgets.wDelim) BY 2: /* BY kan ikke være expression!! */
        EDITOR-ListC:INSERT-STRING(ENTRY(i,ENTRY(3,SUBSTR(wSamStr,wStart),"$"),tWidgets.wDelim) + CHR(13)).
     END.  
     ELSE
     DO i = 1 TO NUM-ENTRIES(ENTRY(3,SUBSTR(wSamStr,wStart),"$"),tWidgets.wDelim):
        EDITOR-ListC:INSERT-STRING(ENTRY(i,ENTRY(3,SUBSTR(wSamStr,wStart),"$"),tWidgets.wDelim) + CHR(13)).
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToggleSensitive Dialog-Frame 
PROCEDURE ToggleSensitive :
/*------------------------------------------------------------------------------
  Purpose:     Setter felter sensitiv
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR wColor AS INTE NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      /*
      ASSIGN FILL-IN-CaptionE:SENSITIVE = NOT (CAN-DO("RADIO-SET,SLIDER,SELECTION-LIST,WINDOW",TRIM(tWidgets.wType)) OR CAN-DO("FRAME,BROWSE,CELL",TRIM(tWidgets.wType)) AND tWidgets.wCaption = "")
            EDITOR-ListE:SENSITIVE     = NOT (NUM-ENTRIES(tWidgets.wName,".") >= 2 OR NOT CAN-DO("COMBO-BOX,SELECTION-LIST,RADIO-SET",TRIM(tWidgets.wType)))
            FILL-IN-SvE:SENSITIVE      = NOT (NUM-ENTRIES(tWidgets.wName,".") >= 2 OR NOT CAN-DO("COMBO-BOX,EDITOR,FILL-IN,RADIO-SET,SELECTION-LIST,SLIDER,TEXT,TOGGLE-BOX",TRIM(tWidgets.wType)))
            FILL-IN-HelpE:SENSITIVE    = NOT (CAN-DO("WINDOW,FRAME,DIALOG-BOX",TRIM(tWidgets.wType)))
            FILL-IN-ToolTipE:SENSITIVE = NOT (CAN-DO("WINDOW,FRAME,DIALOG-BOX,CELL",TRIM(tWidgets.wType))).
      */
      ASSIGN FILL-IN-CaptionE:SENSITIVE = NOT (CAN-DO("RADIO-SET,SLIDER,SELECTION-LIST,WINDOW",TRIM(tWidgets.wType)) OR CAN-DO("FRAME,BROWSE,CELL",TRIM(tWidgets.wType)) AND tWidgets.wCaption = "")
            EDITOR-ListE:SENSITIVE     = NOT (NUM-ENTRIES(tWidgets.wName,".") >= 2 OR NOT CAN-DO("RADIO-SET",TRIM(tWidgets.wType)))
            FILL-IN-SvE:SENSITIVE      = NOT (NUM-ENTRIES(tWidgets.wName,".") >= 2 OR NOT CAN-DO("FILL-IN,SLIDER,TEXT",TRIM(tWidgets.wType)))
            FILL-IN-HelpE:SENSITIVE    = NOT (CAN-DO("WINDOW,FRAME,DIALOG-BOX",TRIM(tWidgets.wType)))
            FILL-IN-ToolTipE:SENSITIVE = NOT (CAN-DO("WINDOW,FRAME,DIALOG-BOX,CELL",TRIM(tWidgets.wType))).
  END.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

