&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File:               d-blng.w
  Description:        Browser for widgets som det kan endres språk for
  Input Parameters:   INPUT CHAR wPrgNavn, INPUT WIDGET wiH
  Output Parameters:  none
  Author:             Sturla Johnsen
  Created:            18.12.98
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER wPrgNavn  AS CHAR NO-UNDO.
DEF INPUT PARAMETER wiH      AS WIDGET NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF NEW SHARED VAR wDesign AS CHAR NO-UNDO.

DEF VAR wLevel AS INTE   NO-UNDO.
DEF VAR wLng   AS CHAR   NO-UNDO.
DEF VAR wLI    AS CHAR   NO-UNDO INIT "Design".
DEF VAR whC    AS WIDGET NO-UNDO. 
DEF VAR wSaveCurrLng AS CHAR NO-UNDO.

DEF NEW SHARED TEMP-TABLE tWidgets NO-UNDO
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
&Scoped-define BROWSE-NAME BROWSE-Widgets

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tWidgets

/* Definitions for BROWSE BROWSE-Widgets                                */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Widgets tWidgets.wType tWidgets.wName tWidgets.wCaption tWidgets.wLi tWidgets.wSv tWidgets.wHelp tWidgets.wTooltip   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Widgets tWidgets.wCaption ~
tWidgets.wHelp ~
tWidgets.wTooltip   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-Widgets tWidgets
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-Widgets tWidgets
&Scoped-define SELF-NAME BROWSE-Widgets
&Scoped-define OPEN-QUERY-BROWSE-Widgets OPEN QUERY {&SELF-NAME} FOR EACH tWidgets.
&Scoped-define TABLES-IN-QUERY-BROWSE-Widgets tWidgets
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Widgets tWidgets


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-Widgets}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS COMBO-BOX-CurrLng BUTTON-Lagresom ~
BUTTON-Detaljer BROWSE-Widgets BUTTON-Avbryt BUTTON-Bruk RECT-3 
&Scoped-Define DISPLAYED-OBJECTS TOGGLE-Readonly FILL-IN-PrgNavn 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetCurrent Dialog-Frame 
FUNCTION GetCurrent RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Avbryt AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1.05
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Bruk 
     LABEL "&Bruk" 
     SIZE 14 BY 1.05.

DEFINE BUTTON BUTTON-Detaljer DEFAULT 
     LABEL "&Detaljer..." 
     SIZE 14 BY 1.05.

DEFINE BUTTON BUTTON-Lagresom 
     LABEL "&Lagre som..." 
     SIZE 14 BY 1.05.

DEFINE BUTTON BUTTON-OK AUTO-GO 
     LABEL "OK" 
     SIZE 14 BY 1.05.

DEFINE BUTTON BUTTON-Slett 
     LABEL "&Slett!" 
     SIZE 14 BY 1.05.

DEFINE VARIABLE COMBO-BOX-CurrLng AS CHARACTER FORMAT "X(256)":U INITIAL "Design" 
     LABEL "&Aktivt språk" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Design" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PrgNavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Program" 
      VIEW-AS TEXT 
     SIZE 25.4 BY .71
     BGCOLOR 0 FGCOLOR 14 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL 
     SIZE 37 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE TOGGLE-Readonly AS LOGICAL INITIAL no 
     LABEL "&Enable browse (Husk mulig plassmangel)" 
     VIEW-AS TOGGLE-BOX
     SIZE 48 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Widgets FOR 
      tWidgets SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Widgets
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Widgets Dialog-Frame _FREEFORM
  QUERY BROWSE-Widgets DISPLAY
      tWidgets.wType    format "x(23)" COLUMN-LABEL "Type"
      tWidgets.wName    format "x(19)" COLUMN-LABEL "Navn"
      tWidgets.wCaption format "x(15)" COLUMN-LABEL "Label/Tittel"
      tWidgets.wLi      format "x(15)" COLUMN-LABEL "List-items/Radio"
      tWidgets.wSv      format "x(15)" COLUMN-LABEL "Screen-value"
      tWidgets.wHelp    format "x(15)" COLUMN-LABEL "Help"
      tWidgets.wTooltip format "x(15)" COLUMN-LABEL "Tooltip"
ENABLE      
      tWidgets.wCaption
      tWidgets.wHelp
      tWidgets.wTooltip
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS SIZE 127.8 BY 16.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     COMBO-BOX-CurrLng AT ROW 1.24 COL 66 COLON-ALIGNED
     BUTTON-Slett AT ROW 1.24 COL 85
     BUTTON-Lagresom AT ROW 1.24 COL 100
     BUTTON-Detaljer AT ROW 1.24 COL 116
     BROWSE-Widgets AT ROW 2.43 COL 2
     TOGGLE-Readonly AT ROW 19.1 COL 2
     BUTTON-OK AT ROW 19.1 COL 86
     BUTTON-Avbryt AT ROW 19.1 COL 101
     BUTTON-Bruk AT ROW 19.1 COL 116
     FILL-IN-PrgNavn AT ROW 1.38 COL 10.6 COLON-ALIGNED
     RECT-3 AT ROW 1.24 COL 2
     SPACE(91.00) SKIP(17.91)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Widgets"
         DEFAULT-BUTTON BUTTON-Detaljer CANCEL-BUTTON BUTTON-Avbryt.


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
                                                                        */
/* BROWSE-TAB BROWSE-Widgets BUTTON-Detaljer Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-OK IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Slett IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX COMBO-BOX-CurrLng IN FRAME Dialog-Frame
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN FILL-IN-PrgNavn IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TOGGLE-Readonly IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Widgets
/* Query rebuild information for BROWSE BROWSE-Widgets
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tWidgets.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-Widgets */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Widgets */
DO:
  IF COMBO-BOX-CurrLng:SCREEN-VALUE <> "Design" THEN RUN Save.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Widgets */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Widgets
&Scoped-define SELF-NAME BROWSE-Widgets
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Widgets Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-Widgets IN FRAME Dialog-Frame
DO:
  RUN d-vlng.w(ROWID(tWidgets),wPrgNavn).
  IF RETURN-VALUE = "Ok" THEN SELF:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Widgets Dialog-Frame
ON RETURN OF BROWSE-Widgets IN FRAME Dialog-Frame
DO:
  APPLY "DEFAULT-ACTION" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Widgets Dialog-Frame
ON ROW-DISPLAY OF BROWSE-Widgets IN FRAME Dialog-Frame
DO:
  DEF VAR wColor AS INTE NO-UNDO.
  ASSIGN tWidgets.wCaption:FGCOLOR IN BROWSE {&BROWSE-NAME} = 15
         wColor = INT(STRING(CAN-DO("RADIO-SET,SLIDER,SELECTION-LIST",TRIM(tWidgets.wType)) OR CAN-DO("FRAME,BROWSE,CELL",TRIM(tWidgets.wType)) AND tWidgets.wCaption = "","12/2"))
         tWidgets.wCaption:BGCOLOR IN BROWSE {&BROWSE-NAME} = IF wColor = 2 AND tWidgets.wCaption = "" THEN 3 ELSE wColor 
         tWidgets.wLi:FGCOLOR      IN BROWSE {&BROWSE-NAME} = 15
         wColor = INT(STRING(NUM-ENTRIES(tWidgets.wName,".") >= 2 
                          OR NOT CAN-DO("COMBO-BOX,SELECTION-LIST,RADIO-SET",TRIM(tWidgets.wType)),"12/2"))
         tWidgets.wLi:BGCOLOR      IN BROWSE {&BROWSE-NAME} = IF wColor = 2 AND tWidgets.wLi = "" THEN 7 ELSE wColor 
         tWidgets.wSv:FGCOLOR      IN BROWSE {&BROWSE-NAME} = 15
         wColor = INT(STRING(NUM-ENTRIES(tWidgets.wName,".") >= 2 
                          OR NOT CAN-DO("COMBO-BOX,EDITOR,FILL-IN,RADIO-SET,SELECTION-LIST,SLIDER,TEXT,TOGGLE-BOX",TRIM(tWidgets.wType)),"12/2"))
         tWidgets.wSv:BGCOLOR      IN BROWSE {&BROWSE-NAME} = IF wColor = 2 AND tWidgets.wSv = "" THEN 7 ELSE wColor
         tWidgets.wHelp:FGCOLOR    IN BROWSE {&BROWSE-NAME} = 15
         wColor = INT(STRING(CAN-DO("WINDOW,FRAME,DIALOG-BOX",TRIM(tWidgets.wType)),"12/2"))
         tWidgets.wHelp:BGCOLOR    IN BROWSE {&BROWSE-NAME} = IF wColor = 2 AND tWidgets.wHelp = "" THEN 7 ELSE wColor
         tWidgets.wToolTip:FGCOLOR IN BROWSE {&BROWSE-NAME} = 15 
         wColor = INT(STRING(CAN-DO("WINDOW,FRAME,DIALOG-BOX,CELL",TRIM(tWidgets.wType)),"12/2"))
         tWidgets.wToolTip:BGCOLOR IN BROWSE {&BROWSE-NAME} = IF wColor = 2 AND tWidgets.wToolTip = "" THEN 7 ELSE wColor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Widgets Dialog-Frame
ON VALUE-CHANGED OF BROWSE-Widgets IN FRAME Dialog-Frame
DO:
  IF VALID-HANDLE(tWidgets.wWh) THEN RUN Marker.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Bruk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Bruk Dialog-Frame
ON CHOOSE OF BUTTON-Bruk IN FRAME Dialog-Frame /* Bruk */
DO:
  RUN BrukLng.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Detaljer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Detaljer Dialog-Frame
ON CHOOSE OF BUTTON-Detaljer IN FRAME Dialog-Frame /* Detaljer... */
DO:
  APPLY "DEFAULT-ACTION" TO BROWSE {&BROWSE-NAME}.
  APPLY "ENTRY"          TO BROWSE {&BROWSE-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Lagresom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Lagresom Dialog-Frame
ON CHOOSE OF BUTTON-Lagresom IN FRAME Dialog-Frame /* Lagre som... */
DO:
  RUN d-slng.w(THIS-PROCEDURE).
  RUN InitCombo.
  RUN SetComboSV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett Dialog-Frame
ON CHOOSE OF BUTTON-Slett IN FRAME Dialog-Frame /* Slett! */
DO:
   RUN Slett.
   RUN InitCombo.
   RUN SetComboSV.
   APPLY "VALUE-CHANGED" TO COMBO-BOX-CurrLng IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-CurrLng
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-CurrLng Dialog-Frame
ON VALUE-CHANGED OF COMBO-BOX-CurrLng IN FRAME Dialog-Frame /* Aktivt språk */
DO:
   DEF VAR wTekster AS CHAR NO-UNDO.
   DEF VAR wId      AS CHAR NO-UNDO.
   DEF VAR wStart   AS INTE NO-UNDO.
   DEF VAR wSaveSV  AS CHAR NO-UNDO.
   
   DEF BUFFER btWidgets FOR tWidgets.
   
   ASSIGN wSaveSV = SELF:SCREEN-VALUE.
   
   IF SELF = FOCUS THEN DO:
      RUN Save.
      RUN SetCurrLng(wSaveSV).
   END.
   
   ASSIGN SELF:SCREEN-VALUE   = wSaveSV
          BUTTON-Slett:SENSITIVE = SELF:SCREEN-VALUE <> "Design"
          BUTTON-OK:SENSITIVE    = SELF:SCREEN-VALUE <> "Design".
   
   IF SELF <> FOCUS THEN RETURN NO-APPLY.
   
   IF SELF:SCREEN-VALUE = "Design" THEN 
      ASSIGN wTekster = wDesign.
   ELSE DO:
      FIND Lng WHERE Lng.Lng     = SELF:SCREEN-VALUE AND
                     Lng.PrgNavn = ENTRY(1,wPrgNavn,".") NO-LOCK NO-ERROR.
      ASSIGN wTekster = IF AVAIL Lng THEN Lng.Tekster ELSE wDesign.
   END.
  
   FOR EACH btWidgets:
      ASSIGN wId    = TRIM(btWidgets.wType) 
                    + btWidgets.wName
                    + btWidgets.wParent
             wStart = INDEX(wTekster,wId) + 1.
      IF wStart > 0 THEN 
         ASSIGN btWidgets.wCaption = ENTRY(2,SUBSTR(wTekster,wStart),"$")
                btWidgets.wLi      = ENTRY(3,SUBSTR(wTekster,wStart),"$")
                btWidgets.wSv      = ENTRY(4,SUBSTR(wTekster,wStart),"$")
                btWidgets.wHelp    = ENTRY(5,SUBSTR(wTekster,wStart),"$") 
                btWidgets.wToolTip = ENTRY(6,SUBSTR(wTekster,wStart),"$").
   END.
   BROWSE {&BROWSE-NAME}:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-Readonly
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-Readonly Dialog-Frame
ON VALUE-CHANGED OF TOGGLE-Readonly IN FRAME Dialog-Frame /* Enable browse (Husk mulig plassmangel) */
DO:
   ASSIGN tWidgets.wCaption:READ-ONLY IN BROWSE {&BROWSE-NAME} = SELF:SCREEN-VALUE = "no"
          /*tWidgets.wLi:READ-ONLY      IN BROWSE {&BROWSE-NAME} = SELF:SCREEN-VALUE = "no"*/
          tWidgets.wHelp:READ-ONLY    IN BROWSE {&BROWSE-NAME} = SELF:SCREEN-VALUE = "no"
          /*tWidgets.wSV:READ-ONLY      IN BROWSE {&BROWSE-NAME} = SELF:SCREEN-VALUE = "no"*/
          tWidgets.wToolTip:READ-ONLY IN BROWSE {&BROWSE-NAME} = SELF:SCREEN-VALUE = "no".
          
   APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

RUN FindWidgets.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK: 
  {lng.i 
      &PreALT-<  = "wLng = wCurrLng."
      &PostALT-< = "wCurrLng = wLng."
  }

  ON "ALT-1" ANYWHERE BELL.
  ASSIGN wSaveCurrLng      = wCurrLng
         wCurrLng          = "Design"
         wLng              = wCurrLng
         FILL-IN-PrgNavn   = " " + wPrgNavn
         tWidgets.wCaption:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES
         /*tWidgets.wLi:READ-ONLY      IN BROWSE {&BROWSE-NAME} = YES*/
         /*tWidgets.wSv:READ-ONLY      IN BROWSE {&BROWSE-NAME} = YES*/
         tWidgets.wHelp:READ-ONLY    IN BROWSE {&BROWSE-NAME} = YES
         tWidgets.wToolTip:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES.
  RUN InitCombo.
  RUN SetComboSV.
  RUN SaveDesign.
  RUN enable_UI.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
IF VALID-HANDLE(whC) THEN DELETE WIDGET whC.
ASSIGN wCurrLng = wSaveCurrLng.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Browse Dialog-Frame 
PROCEDURE Browse :
/*------------------------------------------------------------------------------
  Purpose:     Finner widgets i en browse
  Parameters:  INPUT WIDGET wh
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wh AS WIDGET NO-UNDO.

  DEF VAR wSaveH   AS WIDGET NO-UNDO.
  
  ASSIGN wSaveH = wh
         wh     = wh:FIRST-COLUMN.
  
  IF VALID-HANDLE(wh) THEN DO:
     RUN CREATE(wSaveH). /* Selve browseren */
     ASSIGN wLevel = wLevel + 1.
  END.
  ELSE RETURN.
  
  DO WHILE VALID-HANDLE(wh):
     RUN Create(wh).
     ASSIGN wh = wh:NEXT-COLUMN.
  END.
  ASSIGN wLevel = wLevel - 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrukLng Dialog-Frame 
PROCEDURE BrukLng :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF wCurrLng <> "Design" THEN DO:
      RUN Save.
      RUN SetComboSV.
   END.
   IF NOT VALID-HANDLE(wLngHandle) THEN RUN lng.p PERSISTENT SET wLngHandle.
   RUN GetLng IN wLngHandle (GetCurrent(),wPrgNavn,wiH) NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Create Dialog-Frame 
PROCEDURE Create :
/*------------------------------------------------------------------------------
  Purpose:     Lager temp-file for browser
  Parameters:  INPUT WIDGET wh
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAMETER wh AS WIDGET NO-UNDO.
  
   DEF VAR whParent    AS WIDGET NO-UNDO.
   DEF VAR wMainType   AS LOGI   NO-UNDO.
   DEF VAR wPar        AS CHAR NO-UNDO.
   
   
   ASSIGN whparent = wh:PARENT.
  
   IF VALID-HANDLE(whparent) THEN DO:
      IF whParent:NAME <> ? THEN ASSIGN wPar = whParent:NAME.
      ELSE DO:
         ASSIGN whParent = IF CAN-QUERY(wh,"FRAME") THEN wh:FRAME ELSE ?.
         IF VALID-HANDLE(whParent) THEN ASSIGN wPar = whParent:NAME.
      END.
   END.      
      
   CREATE tWidgets.
   
   ASSIGN whparent          = wh:PARENT
          wMainType         = CAN-DO("WINDOW,DIALOG-BOX,FRAME,BROWSE",wh:TYPE)
          tWidgets.wName    = (IF CAN-QUERY(wh,"TABLE") AND wh:Table <> ? THEN (wh:Table + ".") ELSE "") + wh:NAME
          tWidgets.wParent  = wpar
          tWidgets.wType    = FILL(" ",wLevel * 4) + (IF wMainType THEN "" ELSE "") +
                              ( IF VALID-HANDLE(whParent) AND whParent:TYPE = "BROWSE" THEN "CELL" 
                               ELSE wh:TYPE) +
                               (IF wMainType THEN "" ELSE "")
                                 
          tWidgets.wCaption = IF CAN-QUERY(wh,"LABEL")         THEN wh:LABEL 
                         ELSE IF CAN-QUERY(wh,"TITLE")         THEN wh:TITLE
                         ELSE ""
          tWidgets.wLi      = IF wh:TYPE = "RADIO-SET"         THEN wh:RADIO-BUTTONS
                         /*
                         ELSE IF CAN-DO("SELECTION-LIST,COMBO-BOX",wh:TYPE) THEN wh:LIST-ITEMS
                         */
                         ELSE ""               
          tWidgets.wDelim   = IF CAN-QUERY(wh,"DELIMITER")    THEN wh:DELIMITER
                         ELSE ""               
          tWidgets.wSv      = IF CAN-QUERY(wh,"SCREEN-VALUE") THEN wh:SCREEN-VALUE 
                         ELSE ""               
          tWidgets.wHelp    = IF CAN-QUERY(wh,"HELP")         THEN wh:HELP
                         ELSE ""
          tWidgets.wTooltip = IF CAN-QUERY(wh,"TOOLTIP")      THEN wh:TOOLTIP
                         ELSE ""
          tWidgets.wWh      = IF CAN-DO("WINDOW,DIALOG-BOX",wh:TYPE) OR TRIM(tWidgets.wType) = "CELL" AND wh:LABEL = ? THEN ? ELSE wh.
                         
   IF tWidgets.wName    = ? THEN ASSIGN tWidgets.wName    = "".
   IF tWidgets.wLi      = ? THEN ASSIGN tWidgets.wLi      = "".
   IF tWidgets.wSv      = ? THEN ASSIGN tWidgets.wSv      = "".
   IF tWidgets.wCaption = ? THEN ASSIGN tWidgets.wCaption = "".
   IF tWidgets.wHelp    = ? THEN ASSIGN tWidgets.wHelp    = "".
   IF tWidgets.wTooltip = ? THEN ASSIGN tWidgets.wToolTip = "".
   
   IF NOT wMainType AND tWidgets.wCaption + tWidgets.wLi + tWidgets.wSV + tWidgets.wHelp + tWidgets.wToolTip = "" THEN 
        DELETE tWidgets.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY TOGGLE-Readonly FILL-IN-PrgNavn 
      WITH FRAME Dialog-Frame.
  ENABLE COMBO-BOX-CurrLng BUTTON-Lagresom BUTTON-Detaljer BROWSE-Widgets 
         BUTTON-Avbryt BUTTON-Bruk RECT-3 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FindWidgets Dialog-Frame 
PROCEDURE FindWidgets :
/*------------------------------------------------------------------------------
  Purpose:     Finner widgets i et vindu, en dialogboks eller en frame
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  CASE wiH:type:
     WHEN "DIALOG-BOX" OR
     WHEN "FRAME"      THEN RUN Frame  (wiH).
     WHEN "WINDOW"     THEN RUN Window (wiH).
  END CASE.   
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Frame Dialog-Frame 
PROCEDURE Frame :
/*------------------------------------------------------------------------------
  Purpose:     Finner widgets i en frame eller dialog-box
  Parameters:  INPUT WIDGET wh
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wh AS WIDGET NO-UNDO.

  IF wh:TYPE = "DIALOG-BOX" THEN RUN Create(wh).
  
  ASSIGN wh     = wh:CURRENT-ITERATION
         wh     = wh:FIRST-CHILD
         wLevel = wLevel + INT(VALID-HANDLE(wh)).
     
  DO WHILE VALID-HANDLE(wh):
     IF NOT CAN-DO("BROWSE,LITERAL",wh:TYPE) AND CAN-QUERY(wh,"NAME") THEN
        RUN Create(wh).
     
     IF wh:TYPE = "FRAME"  THEN RUN Frame (wh). ELSE
     IF wh:TYPE = "BROWSE" THEN RUN Browse(wh).
     
     ASSIGN wh = wh:NEXT-SIBLING.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCombo Dialog-Frame 
PROCEDURE InitCombo :
/*------------------------------------------------------------------------------
  Purpose:     Initierer combo for aktivt språk
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN wLi = "Design".
   FOR EACH Lng FIELDS(Lng) WHERE Lng.PrgNavn = ENTRY(1,wPrgNavn,".") NO-LOCK:
       ASSIGN wLI = wLI + "," + Lng.Lng.
   END.
   ASSIGN COMBO-BOX-CurrLng:LIST-ITEMS   IN FRAME {&FRAME-NAME} = wLI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Marker Dialog-Frame 
PROCEDURE Marker :
/*------------------------------------------------------------------------------
  Purpose:     Markerer widgeten i skjermbildet
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wFrameH  AS WIDGET NO-UNDO.
  DEF VAR whParent AS WIDGET NO-UNDO.
 
  IF VALID-HANDLE(whC) THEN DELETE WIDGET whC.
 
  IF NOT CAN-QUERY(tWidgets.wWh,"FRAME") AND NOT CAN-QUERY(tWidgets.wWh,"PARENT") THEN
     RETURN NO-APPLY.
     
  IF CAN-QUERY(tWidgets.wWh,"FRAME") THEN
       ASSIGN wFrameH = tWidgets.wWh:FRAME.
  ELSE 
  IF CAN-QUERY(tWidgets.wWh,"PARENT") THEN
       ASSIGN whParent = tWidgets.wWh:PARENT
              wFrameH  = whParent:FRAME.      
  CREATE RECTANGLE whC.
 
  ASSIGN whC:X             = MAX(1,tWidgets.wWh:X - 2) + IF TRIM(tWidgets.wType) = "CELL" THEN (whParent:X + 2)  ELSE 0
         whC:Y             = MAX(1,tWidgets.wWh:Y - 2) + IF TRIM(tWidgets.wType) = "CELL" THEN (whParent:Y - (17 * wHParent:FOCUSED-ROW + 2)) ELSE 0
         whC:FRAME         = wFrameH
         whC:WIDTH-PIXELS  = tWidgets.wWh:WIDTH-PIXELS  + (IF VALID-HANDLE(wFrameH) AND whC:X + tWidgets.wWh:WIDTH-PIXELS  + (IF TRIM(tWidgets.wType) <> "CELL" THEN 4 ELSE 0) > wFrameH:WIDTH-PIXELS  THEN 2 ELSE (IF TRIM(tWidgets.wType) <> "CELL" THEN 4 ELSE 0))
         whC:HEIGHT-PIXELS = IF TRIM(tWidgets.wType) = "CELL" THEN 2 ELSE tWidgets.wWh:HEIGHT-PIXELS + (IF VALID-HANDLE(wFrameH) AND whC:Y + tWidgets.wWh:HEIGHT-PIXELS + (IF TRIM(tWidgets.wType) <> "CELL" THEN 4 ELSE 0) > wFrameH:HEIGHT-PIXELS THEN 2 ELSE (IF TRIM(tWidgets.wType) <> "CELL" THEN 4 ELSE 0))
         whC:EDGE-PIXELS   = 0
         whC:BGCOLOR       = 12
         whC:FILLED        = YES
         whC:SENSITIVE     = NO
         whC:VISIBLE       = YES NO-ERROR. /* Ikke fjern NO-ERROR. Ikke legg ASSIGN sammen med CREATE-statementet */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Save Dialog-Frame 
PROCEDURE Save :
/*------------------------------------------------------------------------------
  Purpose:     Lagrer 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO TRANSACTION:
     
     IF wCurrLng <> "Design" THEN DO:
        FIND Lng WHERE Lng.Lng = "@@@" AND Lng.PrgNavn = "@@@@@@@" EXCLUSIVE NO-ERROR .
        IF NOT AVAIL Lng THEN DO:
           CREATE Lng.
           ASSIGN Lng.Lng     = "@@@"
                  Lng.PrgNavn = "@@@@@@@".
        END.
        IF NOT CAN-DO(Lng.Tekster,wCurrLng) THEN 
           ASSIGN Lng.Tekster = Lng.Tekster + (IF Lng.Tekster <> "" THEN "," ELSE "") + wCurrLng.
     END.
     
     RUN InitCombo.
 
     IF wCurrLng <> "Design" THEN DO:
        FIND Lng WHERE Lng.Lng = wCurrLng AND Lng.PrgNavn = ENTRY(1,wPrgNavn,".") EXCLUSIVE NO-ERROR.
        IF NOT AVAIL Lng THEN DO:
           CREATE Lng.
           ASSIGN Lng.Lng     = wCurrLng
                  Lng.PrgNavn = ENTRY(1,wPrgNavn,".").
        END.
     END.
    
     IF wCurrLng <> "Design" THEN DO:
        ASSIGN Lng.Tekster = GetCurrent().
        RELEASE Lng.
     END.   
     ELSE ASSIGN wDesign = GetCurrent().   
  END. /* Trans */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveDesign Dialog-Frame 
PROCEDURE SaveDesign :
/*------------------------------------------------------------------------------
  Purpose:     Lagrer evt. design ved første gangs start
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wCurrLngHusk AS CHAR NO-UNDO.
 
  ASSIGN wCurrLngHusk = wCurrLng
         wCurrLng     = "Design".
  RUN Save.
     
  ASSIGN wCurrLng = wCurrLngHusk
         COMBO-BOX-CurrLng:SCREEN-VALUE IN FRAME {&FRAME-NAME} = wCurrLng NO-ERROR. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetComboSV Dialog-Frame 
PROCEDURE SetComboSV :
/*------------------------------------------------------------------------------
  Purpose:     Setter scree-value for combo
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN COMBO-BOX-CurrLng:SCREEN-VALUE IN FRAME {&FRAME-NAME} = wCurrLng NO-ERROR. 
    APPLY "VALUE-CHANGED" TO COMBO-BOX-CurrLng IN FRAME {&FRAME-NAME}. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCurrLng Dialog-Frame 
PROCEDURE SetCurrLng :
/*------------------------------------------------------------------------------
  Purpose:     Setter wCurrLng
  Parameters:  INPUT CHAR wNewLng
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAMETER wNewLng AS CHAR NO-UNDO.
   ASSIGN wCurrLng = wNewLng.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Slett Dialog-Frame 
PROCEDURE Slett :
/*------------------------------------------------------------------------------
  Purpose:     Sletter språk for aktuelt program
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND Lng WHERE Lng.PrgNavn = wPrgNavn AND 
                  Lng.Lng     = wCurrLng EXCLUSIVE NO-ERROR.
   IF AVAIL Lng THEN DELETE Lng.
   ASSIGN wCurrLng = "Design".
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Window Dialog-Frame 
PROCEDURE Window :
/*------------------------------------------------------------------------------
  Purpose:     Finner widgets i et indu
  Parameters:  INPUT WIDGET wh
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wh AS WIDGET NO-UNDO.

  RUN Create(wh).

  ASSIGN wh     = wh:FIRST-CHILD
         wLevel = wLevel + INT(VALID-HANDLE(wh)).
         
  DO WHILE VALID-HANDLE(wh):
     IF wh = FRAME {&FRAME-NAME}:HANDLE THEN LEAVE.
     
     IF wh:TYPE <> "LITERAL" AND CAN-QUERY(wh,"NAME") THEN
           RUN Create(wh).
     
     IF wh:TYPE = "FRAME"  THEN RUN Frame (wh). ELSE
     IF wh:TYPE = "BROWSE" THEN RUN Browse(wh).
          
     ASSIGN wh = wh:NEXT-SIBLING.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetCurrent Dialog-Frame 
FUNCTION GetCurrent RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Bygger opp en stren med innholdet akkurat nå
    Notes:  
------------------------------------------------------------------------------*/
     DEF VAR wTekster AS CHAR NO-UNDO.
     DEF BUFFER btWidgets FOR tWidgets.    
     FOR EACH btWidgets:
         ASSIGN wTekster = wTekster 
                         + "£"  
                         + TRIM(btWidgets.wType)
                         + btWidgets.wName
                         + btWidgets.wParent
                         + "$"
                         + btWidgets.wCaption
                         + "$"
                         + btWidgets.wLi
                         + "$"
                         + btWidgets.wSv
                         + "$"
                         + btWidgets.wHelp
                         + "$"
                         + btWidgets.wToolTip
                         + "$".
     END.
     RETURN wTekster + "£".
     
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

