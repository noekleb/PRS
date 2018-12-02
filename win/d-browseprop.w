&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  Forfatter:   Sturla Johnsen
  Beskrivelse: Endrer instillinger for en browse 
  Parametere:  INPUT WIDGET wh (browse), INPUT HANDLE wCaller 
  Endringer:
------------------------------------------------------------------------*/
/* {shared.i &New = " "} */
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEF INPUT PARAMETER wh       AS WIDGET NO-UNDO.
DEF INPUT PARAMETER wCaller  AS HANDLE NO-UNDO.
DEF INPUT PARAMETER wBrowse# AS INTE   NO-UNDO.

/* Parameter Definisjoner ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
&ELSE
&ENDIF

/* Preprossessor direktiver ---                                         */
&SCOP Laast    "-----------------------------------------------------"   
&SCOP FGkol    IF wchSpin-FGkol:Value    = 1 THEN ? ELSE wchSpin-FGkol:Value - 2
&SCOP BGkol    IF wchSpin-BGkol:Value    = 1 THEN ? ELSE wchSpin-BGkol:Value - 2
&SCOP SepColor IF wchSpin-SepColor:Value = 1 THEN ? ELSE wchSpin-SepColor:Value - 2
&SCOP Standard "Std."

/* Buffer og Temp-Table Definisjoner ---                                */
DEF TEMP-TABLE tH NO-UNDO
    FIELD tFelt   AS CHAR
    FIELD tHandle AS HANDLE
    FIELD tBredde AS INTE
    FIELD tFGkol  AS INTE
    FIELD tBGkol  AS INTE
    INDEX tFelt IS PRIMARY UNIQUE tFelt ASCENDING.   
/* Lokale variabler ---                                                 */
def var retur-verdi as char initial "<avbryt>" no-undo.
DEF VAR wchSpin-Hoyde    AS COM-HANDLE NO-UNDO.
DEF VAR wchSpin-FGkol    AS COM-HANDLE NO-UNDO.
DEF VAR wchSpin-BGkol    AS COM-HANDLE NO-UNDO.
DEF VAR wchSpin-SepColor AS COM-HANDLE NO-UNDO.
DEF VAR wInit            AS LOGI       NO-UNDO INIT YES.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-20 RECT-22 SELECT-Kolonner ~
BUTTON-Opp BUTTON-Ned TOGGLE-SkilleLinjer BUTTON-Std Btn_OK Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS SELECT-Kolonner FILL-IN-FGkol ~
FILL-IN-BGkol-2 FILL-IN-BGkol SLIDER-Bredde FILL-IN-Hoyde FILL-IN-SepColor ~
TOGGLE-SkilleLinjer 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame-BGkol AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-BGkol AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-FGkol AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-FGkol AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-Hoyde AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-Hoyde AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-SEPcolor AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-SEPcolor AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Lukk" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-AlleLik 
     LABEL "Sett alle li&k" 
     SIZE 13 BY 1.1.

DEFINE BUTTON BUTTON-Ned 
     LABEL "Flytt &ned" 
     SIZE 17 BY 1.14.

DEFINE BUTTON BUTTON-Opp 
     LABEL "Flytt &opp" 
     SIZE 17 BY 1.14.

DEFINE BUTTON BUTTON-Std 
     LABEL "&Standardinnstillinger" 
     SIZE 21 BY 1.14.

DEFINE VARIABLE FILL-IN-BGkol AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BGkol-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FGkol AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Hoyde AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Radhøyde" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SepColor AS CHARACTER FORMAT "X(256)":U 
     LABEL "F&arge" 
     VIEW-AS FILL-IN 
     SIZE 4.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 54 BY 13.1.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 17 BY 6.19.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 30 BY 1.91.

DEFINE VARIABLE SELECT-Kolonner AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 10.48 TOOLTIP "Linjen ----------------- indikerer at kolonnene over er låst ved siderulling" NO-UNDO.

DEFINE VARIABLE SLIDER-Bredde AS INTEGER INITIAL 2 
     VIEW-AS SLIDER MIN-VALUE 2 MAX-VALUE 1000 HORIZONTAL NO-CURRENT-VALUE 
     SIZE 52 BY 1.19 NO-UNDO.

DEFINE VARIABLE TOGGLE-SkilleLinjer AS LOGICAL INITIAL no 
     LABEL "Bru&k" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     SELECT-Kolonner AT ROW 1.95 COL 4 NO-LABEL
     BUTTON-Opp AT ROW 2.86 COL 38
     BUTTON-Ned AT ROW 4.29 COL 38
     FILL-IN-FGkol AT ROW 7.62 COL 38.2 COLON-ALIGNED NO-LABEL
     FILL-IN-BGkol-2 AT ROW 9.57 COL 38.2 COLON-ALIGNED NO-LABEL
     FILL-IN-BGkol AT ROW 9.57 COL 38.2 COLON-ALIGNED NO-LABEL
     BUTTON-AlleLik AT ROW 11.05 COL 40
     SLIDER-Bredde AT ROW 13.1 COL 3 NO-LABEL
     FILL-IN-Hoyde AT ROW 15.05 COL 12 COLON-ALIGNED
     FILL-IN-SepColor AT ROW 15.62 COL 44.4 COLON-ALIGNED
     TOGGLE-SkilleLinjer AT ROW 15.76 COL 28.2
     BUTTON-Std AT ROW 17.24 COL 2
     Btn_OK AT ROW 17.19 COL 31
     Btn_Help AT ROW 17.19 COL 44
     RECT-1 AT ROW 1.48 COL 2
     RECT-20 AT ROW 6.29 COL 38
     RECT-22 AT ROW 15.05 COL 26
     "Kolonner" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 1.24 COL 3
     "Br&edde:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.67 COL 4
     "&Forgrunn" VIEW-AS TEXT
          SIZE 9.4 BY .62 AT ROW 6.91 COL 39.8
     "B&akgr." VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 8.86 COL 40.2
     "Farger" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 6.05 COL 39
     "&Bakgr." VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 8.86 COL 40.2
     "Skillelinjer" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 14.81 COL 27
     SPACE(19.00) SKIP(2.95)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Innstillinger for listen"
         DEFAULT-BUTTON Btn_OK.


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

/* SETTINGS FOR BUTTON BUTTON-AlleLik IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-BGkol IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-BGkol-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-FGkol IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Hoyde IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SepColor IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR SLIDER SLIDER-Bredde IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame-FGkol ASSIGN
       FRAME        = FRAME Dialog-Frame:HANDLE
       ROW          = 7.62
       COLUMN       = 50
       HEIGHT       = 1.1
       WIDTH        = 3
       HIDDEN       = no
       SENSITIVE    = yes.

CREATE CONTROL-FRAME CtrlFrame-BGkol ASSIGN
       FRAME        = FRAME Dialog-Frame:HANDLE
       ROW          = 9.57
       COLUMN       = 50
       HEIGHT       = 1.1
       WIDTH        = 3
       HIDDEN       = no
       SENSITIVE    = yes.

CREATE CONTROL-FRAME CtrlFrame-Hoyde ASSIGN
       FRAME        = FRAME Dialog-Frame:HANDLE
       ROW          = 15.05
       COLUMN       = 19.2
       HEIGHT       = 1.1
       WIDTH        = 2.8
       HIDDEN       = no
       SENSITIVE    = yes.

CREATE CONTROL-FRAME CtrlFrame-SEPcolor ASSIGN
       FRAME        = FRAME Dialog-Frame:HANDLE
       ROW          = 15.62
       COLUMN       = 51.4
       HEIGHT       = 1.1
       WIDTH        = 3
       HIDDEN       = no
       SENSITIVE    = yes.
      CtrlFrame-FGkol:NAME = "CtrlFrame-FGkol":U .
/* CtrlFrame-FGkol OCXINFO:CREATE-CONTROL from: {EAF26C8F-9586-101B-9306-0020AF234C9D} type: CSSpin */
      CtrlFrame-BGkol:NAME = "CtrlFrame-BGkol":U .
/* CtrlFrame-BGkol OCXINFO:CREATE-CONTROL from: {EAF26C8F-9586-101B-9306-0020AF234C9D} type: CSSpin */
      CtrlFrame-Hoyde:NAME = "CtrlFrame-Hoyde":U .
/* CtrlFrame-Hoyde OCXINFO:CREATE-CONTROL from: {EAF26C8F-9586-101B-9306-0020AF234C9D} type: CSSpin */
      CtrlFrame-SEPcolor:NAME = "CtrlFrame-SEPcolor":U .
/* CtrlFrame-SEPcolor OCXINFO:CREATE-CONTROL from: {EAF26C8F-9586-101B-9306-0020AF234C9D} type: CSSpin */
      CtrlFrame-FGkol:MOVE-AFTER(FILL-IN-FGkol:HANDLE IN FRAME Dialog-Frame).
      CtrlFrame-BGkol:MOVE-AFTER(FILL-IN-BGkol:HANDLE IN FRAME Dialog-Frame).
      CtrlFrame-Hoyde:MOVE-AFTER(FILL-IN-Hoyde:HANDLE IN FRAME Dialog-Frame).
      CtrlFrame-SEPcolor:MOVE-AFTER(FILL-IN-SepColor:HANDLE IN FRAME Dialog-Frame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ALT-A OF FRAME Dialog-Frame /* Innstillinger for listen */
ANYWHERE DO:
  APPLY "ENTRY" TO CtrlFrame-SEPcolor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ALT-B OF FRAME Dialog-Frame /* Innstillinger for listen */
ANYWHERE DO:
   APPLY "ENTRY" TO CtrlFrame-BGkol.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ALT-E OF FRAME Dialog-Frame /* Innstillinger for listen */
ANYWHERE DO:
  APPLY "ENTRY" TO SLIDER-Bredde.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ALT-F OF FRAME Dialog-Frame /* Innstillinger for listen */
ANYWHERE DO:
  APPLY "ENTRY" TO CtrlFrame-FGkol.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ALT-R OF FRAME Dialog-Frame /* Innstillinger for listen */
ANYWHERE DO:
   APPLY "ENTRY" TO CtrlFrame-Hoyde.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Innstillinger for listen */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: 
  /* {diahelp.i} */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-AlleLik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-AlleLik Dialog-Frame
ON CHOOSE OF BUTTON-AlleLik IN FRAME Dialog-Frame /* Sett alle lik */
DO:
  FOR EACH tH:
     ASSIGN tH.tFGkol                 = {&FGkol}
            tH.tBGkol                 = {&BGkol}
            tH.tHandle:COLUMN-FGCOLOR = {&FGkol}
            tH.tHandle:COLUMN-BGCOLOR = {&BGkol}.
  END.          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ned
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ned Dialog-Frame
ON CHOOSE OF BUTTON-Ned IN FRAME Dialog-Frame /* Flytt ned */
DO:
   DEF VAR wCurrPos     AS INT  NO-UNDO.
   DEF VAR wCurrEntry   AS CHAR NO-UNDO.
   DEF VAR wNextEntry   AS CHAR NO-UNDO.
   DEF VAR wLaasPos     AS INTE NO-UNDO.
  
   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN wCurrEntry = SELECT-Kolonner:SCREEN-VALUE
             wCurrPos   = SELECT-Kolonner:LOOKUP(SELECT-Kolonner:SCREEN-VALUE)
             wNextEntry = SELECT-Kolonner:ENTRY(wCurrPos + 1)
             wLaasPos   = SELECT-Kolonner:LOOKUP({&Laast}).
             
      SELECT-Kolonner:REPLACE(SELECT-Kolonner:SCREEN-VALUE,wCurrPos + 1).
      SELECT-Kolonner:REPLACE(wNextEntry,wCurrPos).
      ASSIGN SELECT-Kolonner:SCREEN-VALUE = wCurrEntry.   
   END. 
   
   IF wCurrPos > wLaasPos THEN ASSIGN wCurrPos = wCurrPos - 1.
   
   IF wCurrPos + 1 <> wLaasPos AND wCurrEntry <> {&Laast} THEN
        wh:MOVE-COLUMN(wCurrPos, wCurrPos + 1). 
   
   ASSIGN BUTTON-Opp:SENSITIVE = SELECT-Kolonner:SCREEN-VALUE <> SELECT-Kolonner:ENTRY(1)
          SELF:SENSITIVE = SELECT-Kolonner:SCREEN-VALUE <> SELECT-Kolonner:ENTRY(NUM-ENTRIES(SELECT-Kolonner:LIST-ITEMS))
          wh:NUM-LOCKED-COLUMNS = SELECT-Kolonner:LOOKUP({&Laast}) - 1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Opp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Opp Dialog-Frame
ON CHOOSE OF BUTTON-Opp IN FRAME Dialog-Frame /* Flytt opp */
DO:
   DEF VAR wCurrPos   AS INT  NO-UNDO.
   DEF VAR wCurrEntry AS CHAR NO-UNDO.
   DEF VAR wPrevEntry AS CHAR NO-UNDO.
   DEF VAR wLaasPos   AS INTE NO-UNDO.
   
   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN wCurrEntry = SELECT-Kolonner:SCREEN-VALUE
             wCurrPos   = SELECT-Kolonner:LOOKUP(SELECT-Kolonner:SCREEN-VALUE)
             wPrevEntry = SELECT-Kolonner:ENTRY(wCurrPos - 1)
             wLaasPos   = SELECT-Kolonner:LOOKUP({&Laast}).
             
      SELECT-Kolonner:REPLACE(SELECT-Kolonner:SCREEN-VALUE,wCurrPos - 1).
      SELECT-Kolonner:REPLACE(wPrevEntry,wCurrPos).
      ASSIGN SELECT-Kolonner:SCREEN-VALUE = wCurrEntry.
      
   END. 
   
   IF wCurrPos > wLaasPos THEN ASSIGN wCurrPos = wCurrPos - 1. 
   
   IF wCurrPos <> wLaasPos THEN  wh:MOVE-COLUMN(wCurrPos, wCurrPos - 1).  
   
   ASSIGN BUTTON-Ned:SENSITIVE = SELECT-Kolonner:SCREEN-VALUE <> SELECT-Kolonner:ENTRY(NUM-ENTRIES(SELECT-Kolonner:LIST-ITEMS))
          SELF:SENSITIVE = SELECT-Kolonner:SCREEN-VALUE <> SELECT-Kolonner:ENTRY(1)
          wh:NUM-LOCKED-COLUMNS = SELECT-Kolonner:LOOKUP({&Laast}) - 1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Std
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Std Dialog-Frame
ON CHOOSE OF BUTTON-Std IN FRAME Dialog-Frame /* Standardinnstillinger */
DO:
  RUN VALUE("StdBrowseSettings0" + TRIM(STRING(wBrowse#,"zzz"))) IN wCaller NO-ERROR.
  RUN initialize-controls.
  RUN Initiering.
  ASSIGN BUTTON-Opp:SENSITIVE = NO.
  DISPL TOGGLE-SkilleLinjer WITH FRAME {&FRAME-NAME}.
  ASSIGN SELECT-Kolonner:SCREEN-VALUE = SELECT-Kolonner:ENTRY(1).
  APPLY "VALUE-CHANGED" TO SELECT-Kolonner.
  ASSIGN wchSpin-SepColor:Enabled = wh:SEPARATORS
         wchSpin-SepColor:Value   = IF wh:SEPARATOR-FGCOLOR = ? THEN 1 ELSE wh:SEPARATOR-FGCOLOR + 2  
         FILL-IN-SepColor:BGCOLOR = {&SepColor}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-BGkol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-BGkol Dialog-Frame OCX.Change
PROCEDURE CtrlFrame-BGkol.CSSpin.Change .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}: 
   ASSIGN FILL-IN-BGkol:BGCOLOR      = {&BGkol}
          FILL-IN-BGkol:SCREEN-VALUE = IF ({&BGkol}) = ? THEN {&Standard} ELSE "".
   IF SELF = FOCUS THEN DO:
      FIND tH WHERE tH.tFelt = SELECT-Kolonner:SCREEN-VALUE.
      ASSIGN tH.tBGkol                 = {&BGkol}
             tH.tHandle:COLUMN-BGCOLOR = {&BGkol}.
   END.          
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-FGkol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-FGkol Dialog-Frame OCX.Change
PROCEDURE CtrlFrame-FGkol.CSSpin.Change .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}: 
   ASSIGN FILL-IN-FGkol:BGCOLOR      = {&FGkol}
          FILL-IN-FGkol:SCREEN-VALUE = IF ({&FGkol}) = ? THEN {&Standard} ELSE "".
   IF SELF = FOCUS THEN DO:
      FIND tH WHERE tH.tFelt = SELECT-Kolonner:SCREEN-VALUE.
      ASSIGN tH.tFGkol                 = {&FGkol}
             tH.tHandle:COLUMN-FGCOLOR = {&FGkol}.
   END.          
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-Hoyde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-Hoyde Dialog-Frame OCX.Change
PROCEDURE CtrlFrame-Hoyde.CSSpin.Change .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}: 
     ASSIGN FILL-IN-Hoyde:SCREEN-VALUE = STRING(wchSpin-Hoyde:VALUE)
            wh:ROW-HEIGHT-PIXELS       = wchSpin-Hoyde:VALUE NO-ERROR.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-SEPcolor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-SEPcolor Dialog-Frame OCX.Change
PROCEDURE CtrlFrame-SEPcolor.CSSpin.Change .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
&SCOP SepColor IF wchSpin-SepColor:Value = 1 THEN ? ELSE wchSpin-SepColor:Value - 2
DO WITH FRAME {&FRAME-NAME}: 
   ASSIGN FILL-IN-SepColor:BGCOLOR      = {&SepColor}
          FILL-IN-SepColor:SCREEN-VALUE = IF ({&SepColor}) = ? THEN {&Standard} ELSE ""
          wh:SEPARATOR-FGCOLOR = {&SepColor}.
   
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SELECT-Kolonner
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-Kolonner Dialog-Frame
ON VALUE-CHANGED OF SELECT-Kolonner IN FRAME Dialog-Frame
DO:
   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN BUTTON-Opp:SENSITIVE = SELF:SCREEN-VALUE <> SELF:ENTRY(1)
             BUTTON-Ned:SENSITIVE = SELF:SCREEN-VALUE <> SELF:ENTRY(NUM-ENTRIES(SELF:LIST-ITEMS)).
      IF SELF:SCREEN-VALUE <> {&Laast} THEN DO:
         ASSIGN BUTTON-AlleLik:SENSITIVE = YES
                wchSpin-FGkol:Enabled    = YES
                wchSpin-BGkol:Enabled    = YES
                SLIDER-Bredde:SENSITIVE  = YES.
         FIND tH WHERE tH.tFelt = SELF:SCREEN-VALUE.
         ASSIGN SLIDER-Bredde:SCREEN-VALUE = STRING(tH.tBredde)
                wchSpin-FGkol:Value    = IF tH.tFGkol = ? THEN 1 ELSE tH.tFGkol + 2
                wchSpin-BGkol:Value    = IF tH.tBGkol = ? THEN 1 ELSE tH.tBGkol + 2
                wchSpin-SepColor:Value = IF wh:SEPARATOR-FGCOLOR = ? THEN 1 ELSE wh:SEPARATOR-FGCOLOR + 2.       
      END.          
      
      ELSE 
      IF NOT wInit THEN
      ASSIGN BUTTON-AlleLik:SENSITIVE   = NO
             wchSpin-FGkol:Enabled      = NO
             wchSpin-BGkol:Enabled      = NO
             SLIDER-Bredde:SENSITIVE    = NO
             SLIDER-Bredde:SCREEN-VALUE = "2"
             FILL-IN-FGkol:FGCOLOR      = ?
             FILL-IN-BGkol:BGCOLOR      = ?
             FILL-IN-SepColor:BGCOLOR   = ?.
         
   END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SLIDER-Bredde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SLIDER-Bredde Dialog-Frame
ON VALUE-CHANGED OF SLIDER-Bredde IN FRAME Dialog-Frame
DO:
  DO WITH FRAME {&FRAME-NAME}: 
   FIND tH WHERE tH.tFelt = SELECT-Kolonner:SCREEN-VALUE.
   ASSIGN tH.tBredde = int(self:screen-value)
          tH.tHandle:WIDTH-PIXELS = int(self:screen-value).    
END.
/* slider */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-SkilleLinjer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-SkilleLinjer Dialog-Frame
ON VALUE-CHANGED OF TOGGLE-SkilleLinjer IN FRAME Dialog-Frame /* Bruk */
DO:
  ASSIGN wh:SEPARATORS = SELF:SCREEN-VALUE = "yes"
         wchSpin-SepColor:Enabled = wh:SEPARATORS.
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

  RUN Initiering.

  RUN enable_UI.
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN SELECT-Kolonner:SCREEN-VALUE = SELECT-Kolonner:ENTRY(SELECT-Kolonner:LOOKUP({&Laast}))
            FILL-IN-SepColor:BGCOLOR = {&SepColor}.
     APPLY "VALUE-CHANGED" TO SELECT-Kolonner.
     ASSIGN wInit = NO.
  END.   
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
RUN QueryCustomSettings IN wCaller ("") NO-ERROR.

&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
 return retur-verdi.
&else
 message retur-verdi view-as alert-box.
&endif

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load Dialog-Frame  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "d-browseprop.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame-BGkol = CtrlFrame-BGkol:COM-HANDLE
    UIB_S = chCtrlFrame-BGkol:LoadControls( OCXFile, "CtrlFrame-BGkol":U)
    chCtrlFrame-FGkol = CtrlFrame-FGkol:COM-HANDLE
    UIB_S = chCtrlFrame-FGkol:LoadControls( OCXFile, "CtrlFrame-FGkol":U)
    chCtrlFrame-Hoyde = CtrlFrame-Hoyde:COM-HANDLE
    UIB_S = chCtrlFrame-Hoyde:LoadControls( OCXFile, "CtrlFrame-Hoyde":U)
    chCtrlFrame-SEPcolor = CtrlFrame-SEPcolor:COM-HANDLE
    UIB_S = chCtrlFrame-SEPcolor:LoadControls( OCXFile, "CtrlFrame-SEPcolor":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "d-browseprop.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  RUN control_load.
  DISPLAY SELECT-Kolonner FILL-IN-FGkol FILL-IN-BGkol-2 FILL-IN-BGkol 
          SLIDER-Bredde FILL-IN-Hoyde FILL-IN-SepColor TOGGLE-SkilleLinjer 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 RECT-20 RECT-22 SELECT-Kolonner BUTTON-Opp BUTTON-Ned 
         TOGGLE-SkilleLinjer BUTTON-Std Btn_OK Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initialize-controls Dialog-Frame 
PROCEDURE Initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     Initierer com-objects
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN wchSpin-Hoyde            = chCtrlFrame-Hoyde:CSSpin
          wchSpin-Hoyde:ToolTip    = ""
          wchSpin-FGkol            = chCtrlFrame-FGkol:CSSpin
          wchSpin-FGkol:ToolTip    = ""
          wchSpin-FGkol:Max        = COLOR-TABLE:NUM-ENTRIES + 1
          wchSpin-BGkol            = chCtrlFrame-BGkol:CSSpin
          wchSpin-BGkol:ToolTip    = ""
          wchSpin-BGkol:Max        = COLOR-TABLE:NUM-ENTRIES + 1
          wchSpin-Hoyde:VALUE      = wh:ROW-HEIGHT-PIXELS
          wchSpin-SepColor         = chCtrlFrame-SepColor:CSSpin
          wchSpin-SepColor:Max     = COLOR-TABLE:NUM-ENTRIES + 1
          wchSpin-SepColor:ToolTip = ""
          wchSpin-SepColor:Value   = IF wh:SEPARATOR-FGCOLOR = ? THEN 1 ELSE wh:SEPARATOR-FGCOLOR + 2.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initiering Dialog-Frame 
PROCEDURE Initiering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR wLI AS CHAR   NO-UNDO.
   DEF VAR whc AS WIDGET NO-UNDO.
   DEF VAR i   AS INTE   NO-UNDO.
   
   EMPTY TEMP-TABLE tH NO-ERROR.
   
   ASSIGN whc = wh:FIRST-COLUMN
          wLI = IF wh:NUM-LOCKED-COLUMNS = 0 THEN {&Laast} + "," ELSE ""
          wLI = wLI + whc:LABEL.
   CREATE tH.
   ASSIGN tH.tFelt   = whc:LABEL
          tH.tHandle = whc
          tH.tBredde = whc:WIDTH-PIXELS
          tH.tFGkol  = whc:COLUMN-FGCOLOR
          tH.tBGkol  = whc:COLUMN-BGCOLOR.

   DO WHILE VALID-HANDLE(whc):
      whc = whc:NEXT-COLUMN.
      IF VALID-HANDLE(whc) THEN DO:
         ASSIGN 
            i   = i + 1
            wLI = wLI + IF i = wh:NUM-LOCKED-COLUMNS THEN "," + {&Laast} ELSE ""
            wLI = wLI + "," + whc:LABEL.
         CREATE tH.
         ASSIGN tH.tFelt   = whc:LABEL
                tH.tHandle = whc
                tH.tBredde = whc:WIDTH-PIXELS
                tH.tFGkol  = whc:COLUMN-FGCOLOR
                tH.tBGkol  = whc:COLUMN-BGCOLOR.
      END.   
   END. 
   IF NOT CAN-DO(wLI,{&Laast}) THEN ASSIGN wLI = wLI + "," + {&Laast}.
   
   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN SELECT-Kolonner:LIST-ITEMS   = wLI
             FILL-IN-Hoyde                = STRING(wh:ROW-HEIGHT-PIXELS)
             TOGGLE-SkilleLinjer          = wh:SEPARATORS.
   END.         
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

