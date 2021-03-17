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
def var wButiker      as char no-undo.
def var wAlle         as char   no-undo.
def var wvButiker     as char no-undo.
def var hInstance     as int no-undo.
DEF VAR ch_Grid       AS COM-HANDLE NO-UNDO.
DEF VAR ch_Spin       AS COM-HANDLE NO-UNDO.
DEF VAR ch_Timer      AS COM-HANDLE NO-UNDO.
def var wExcEkst      as char no-undo.
DEF VAR lExcelRapp    AS LOGI NO-UNDO.
DEF STREAM Eksport.

DEFINE VAR cButikString AS CHAR NO-UNDO.

DEFINE TEMP-TABLE ttDag 
    FIELD LopNr      AS INTEGER
    FIELD Dato1      AS DATE
    FIELD Dato2      AS DATE
    FIELD Verdier1   AS CHAR
    FIELD Verdier2   AS CHAR
    FIELD JmfVerdier AS CHAR
    INDEX Lopnr IS PRIMARY Lopnr ASCENDING.

{runlib.i}
{windows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-51 RECT-52 B-SpinFraUp B-SpinTilUp ~
B-Butikker FI-FraDat B-SpinFraDown B-SpinTilDown RADIO-SET-Type ~
TOGGLE-Timer Btn_Avslut Btn_Help BUTTON-SokDato 
&Scoped-Define DISPLAYED-OBJECTS FI-FraDat FI-TilDat FI-Butiker FILL-IN-Tid ~
RADIO-SET-Type TOGGLE-Timer 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValgteAnt C-Win 
FUNCTION ValgteAnt RETURNS CHARACTER
  ( INPUT wValgte AS CHARACTER, INPUT wAlle AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Fil 
       MENU-ITEM m_Avslut       LABEL "&Avslut"       .

DEFINE MENU MENU-BAR-C-Win MENUBAR
       SUB-MENU  m_Fil          LABEL "&Fil"          .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-3 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-3 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Butikker 
     LABEL "&Butiker..." 
     SIZE 14 BY 1.1.

DEFINE BUTTON B-Excel 
     IMAGE-UP FILE "icon/excel":U NO-FOCUS
     LABEL "Excel..." 
     SIZE 4.6 BY 1.1 TOOLTIP "Excel".

DEFINE BUTTON B-Html 
     IMAGE-UP FILE "icon/htmldok":U NO-FOCUS
     LABEL "Html..." 
     SIZE 4.6 BY 1.1 TOOLTIP "Html".

DEFINE BUTTON B-HtmlExcel 
     IMAGE-UP FILE "icon/htmldok":U NO-FOCUS
     LABEL "HtmlExcel..." 
     SIZE 4.6 BY 1.1 TOOLTIP "Åpne html i excel".

DEFINE BUTTON B-Jmf 
     IMAGE-UP FILE "adeicon/tran%.ico":U NO-FOCUS
     LABEL "Excel..." 
     SIZE 4.6 BY 1.1 TOOLTIP "Excel".

DEFINE BUTTON B-SpinFraDown 
     LABEL "-" 
     SIZE 2.6 BY .62
     FONT 6.

DEFINE BUTTON B-SpinFraUp 
     LABEL "+" 
     SIZE 2.6 BY .62
     FONT 6.

DEFINE BUTTON B-SpinTilDown 
     LABEL "-" 
     SIZE 2.6 BY .62
     FONT 6.

DEFINE BUTTON B-SpinTilUp 
     LABEL "+" 
     SIZE 2.6 BY .62
     FONT 6.

DEFINE BUTTON Btn_Avslut AUTO-GO 
     IMAGE-UP FILE "icon/e-exit":U NO-FOCUS
     LABEL "Avslut" 
     SIZE 4.6 BY 1.1 TOOLTIP "Avslutt"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon/e-help":U NO-FOCUS
     LABEL "&Hjelp" 
     SIZE 4.6 BY 1.1 TOOLTIP "Hjelp"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDato-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-Butiker AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraDat AS DATE FORMAT "99/99/99":U 
     LABEL "&Fra" 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilDat AS DATE FORMAT "99/99/99":U 
     LABEL "&Til" 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Tid AS CHARACTER FORMAT "X(256)":U 
     LABEL "Oppdatert" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE RADIO-SET-Type AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Omsetning", 1,
"DB %", 3
     SIZE 35 BY .76 NO-UNDO.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 133 BY .1.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 133 BY .1.

DEFINE VARIABLE TOGGLE-Timer AS LOGICAL INITIAL no 
     LABEL "Automatisk oppdatering" 
     VIEW-AS TOGGLE-BOX
     SIZE 31.6 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-SokDato-2 AT ROW 1.24 COL 82.6
     B-Html AT ROW 1.24 COL 7
     B-SpinFraUp AT ROW 1.1 COL 49.2
     B-SpinTilUp AT ROW 1.14 COL 79.4
     B-Butikker AT ROW 1.19 COL 89.6
     FI-FraDat AT ROW 1.24 COL 30.8 COLON-ALIGNED
     FI-TilDat AT ROW 1.24 COL 60.8 COLON-ALIGNED
     FI-Butiker AT ROW 1.24 COL 102.2 COLON-ALIGNED NO-LABEL
     B-HtmlExcel AT ROW 1.24 COL 12.2
     B-SpinFraDown AT ROW 1.71 COL 49.2
     B-SpinTilDown AT ROW 1.76 COL 79.4
     FILL-IN-Tid AT ROW 2.67 COL 117 COLON-ALIGNED
     B-Excel AT ROW 1.24 COL 2.2
     RADIO-SET-Type AT ROW 2.81 COL 38 NO-LABEL
     TOGGLE-Timer AT ROW 2.81 COL 75.6
     B-Jmf AT ROW 1.24 COL 18
     Btn_Avslut AT ROW 1.24 COL 129.2
     Btn_Help AT ROW 1.24 COL 124
     BUTTON-SokDato AT ROW 1.24 COL 52.6
     RECT-51 AT ROW 1 COL 1
     RECT-52 AT ROW 2.43 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 133 BY 21.29.


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
         TITLE              = "Dagsrapport"
         HEIGHT             = 21.29
         WIDTH              = 133
         MAX-HEIGHT         = 32.38
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 32.38
         VIRTUAL-WIDTH      = 204.8
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-Win:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE                                                          */
/* SETTINGS FOR BUTTON B-Excel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Html IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       B-Html:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON B-HtmlExcel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       B-HtmlExcel:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON B-Jmf IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SokDato-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Butiker IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TilDat IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Tid IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-Tid:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame-3 ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1
       COLUMN          = 116
       HEIGHT          = 1.33
       WIDTH           = 5.6
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 3.95
       COLUMN          = 1.8
       HEIGHT          = 18.33
       WIDTH           = 132
       HIDDEN          = no
       SENSITIVE       = yes.
      CtrlFrame-3:NAME = "CtrlFrame-3":U .
/* CtrlFrame-3 OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {0F026C11-5A66-4c2b-87B5-88DDEBAE72A1} type: VSFlexGrid */
      CtrlFrame:MOVE-AFTER(TOGGLE-Timer:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Dagsrapport */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Dagsrapport */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Butikker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Butikker C-Win
ON CHOOSE OF B-Butikker IN FRAME DEFAULT-FRAME /* Butiker... */
DO:
    def var IO-Liste as char no-undo.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN IO-Liste = wvButiker.
    
        run d-tagbutiker.w (input-output IO-Liste).
        IF RETURN-VALUE = "Avbryt" THEN
            RETURN NO-APPLY.
        ASSIGN wvButiker                = IF IO-Liste = "" THEN wAlle ELSE IO-Liste
               FI-Butiker:SCREEN-VALUE  = ValgteAnt(wvButiker,wAlle).
        RUN Dagsrapp.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Excel C-Win
ON CHOOSE OF B-Excel IN FRAME DEFAULT-FRAME /* Excel... */
DO:
  run ExportToExcel.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Html
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Html C-Win
ON CHOOSE OF B-Html IN FRAME DEFAULT-FRAME /* Html... */
DO:
  run ExportToHtml ("HTM").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-HtmlExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-HtmlExcel C-Win
ON CHOOSE OF B-HtmlExcel IN FRAME DEFAULT-FRAME /* HtmlExcel... */
DO:
  run ExportToHtml ("EX").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Jmf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Jmf C-Win
ON CHOOSE OF B-Jmf IN FRAME DEFAULT-FRAME /* Excel... */
DO:
  DEFINE VARIABLE dJmfStart AS DATE       NO-UNDO.
  DEFINE VARIABLE iVeckoDag AS INTEGER    NO-UNDO.
  DO TRANSACTION:
      ASSIGN dJmfStart = FI-FraDat.
      RUN d-velgdagstyp.w (INPUT-OUTPUT dJmfStart).
      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
/*     MESSAGE "Startdag jämförelse:" UPDATE dJmfStart. */
/*     ASSIGN iVeckoDag = WEEKDAY(FI-FraDat).           */
/*     IF iVeckoDag <> WEEKDAY(dJmfStart) THEN DO:      */
/*       MESSAGE "Startdatumen har olika veckodag"      */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.         */
/*       RETURN.                                        */
/*     END.                                             */
  END.
  RUN AckumJmf2(FI-FraDat,FI-TilDat,dJmfStart).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SpinFraDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SpinFraDown C-Win
ON CHOOSE OF B-SpinFraDown IN FRAME DEFAULT-FRAME /* - */
DO:
    ASSIGN FI-FraDat = FI-FraDat - 1.
           FI-TilDat = FI-FraDat.
    DISPLAY FI-FraDat FI-TilDat with frame {&FRAME-NAME}.
    RUN DagsRapp.
    RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SpinFraUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SpinFraUp C-Win
ON CHOOSE OF B-SpinFraUp IN FRAME DEFAULT-FRAME /* + */
DO:
    IF FI-FraDat = TODAY THEN
        RETURN NO-APPLY.
    ASSIGN FI-FraDat = FI-FraDat + 1
           FI-TilDat = FI-FraDat.
    DISPLAY FI-FraDat FI-TilDat with frame {&FRAME-NAME}.
    RUN DagsRapp.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SpinTilDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SpinTilDown C-Win
ON CHOOSE OF B-SpinTilDown IN FRAME DEFAULT-FRAME /* - */
DO:
    IF FI-TilDat = FI-FraDat THEN
        RETURN NO-APPLY.
    ASSIGN FI-TilDat = FI-TilDat - 1.
    DISPLAY FI-TilDat with frame {&FRAME-NAME}.
    RUN DagsRapp.
    RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SpinTilUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SpinTilUp C-Win
ON CHOOSE OF B-SpinTilUp IN FRAME DEFAULT-FRAME /* + */
DO:
    IF FI-TilDat = TODAY THEN
        RETURN NO-APPLY.
    ASSIGN FI-TilDat = FI-TilDat + 1.
    DISPLAY FI-TilDat with frame {&FRAME-NAME}.
    RUN DagsRapp.
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Avslut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Avslut C-Win
ON CHOOSE OF Btn_Avslut IN FRAME DEFAULT-FRAME /* Avslut */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato C-Win
ON CHOOSE OF BUTTON-SokDato IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-FraDat
DO:

  def var wTittel as char no-undo.
  assign FI-FraDat = date(FI-FraDat:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-FraDat
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato-2 C-Win
ON CHOOSE OF BUTTON-SokDato-2 IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-TilDat
DO:

  def var wTittel as char no-undo.
  assign FI-TilDat = date(FI-TilDat:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-TilDat
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-3 C-Win OCX.Tick
PROCEDURE CtrlFrame-3.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
    RUN Dagsrapp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FraDat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FraDat C-Win
ON TAB OF FI-FraDat IN FRAME DEFAULT-FRAME /* Fra */
OR "RETURN" OF FI-FraDat OR "LEAVE" OF FI-FraDat
DO:
    IF INPUT FI-FraDat > TODAY THEN DO:
        MESSAGE "Fel datum"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ASSIGN FI-FraDat:SCREEN-VALUE = string(FI-FraDat).
    END.
    ELSE
        ASSIGN INPUT FI-FraDat
               FI-TilDat = FI-FraDat
               FI-TilDat:SCREEN-VALUE = FI-FraDat:SCREEN-VALUE.
        RUN Dagsrapp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-TilDat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilDat C-Win
ON TAB OF FI-TilDat IN FRAME DEFAULT-FRAME /* Til */
OR "RETURN" OF FI-TilDat OR "LEAVE" OF FI-TilDat
DO:
    DO WITH FRAME {&FRAME-NAME}:
      IF INPUT FI-TilDat < FI-FraDat OR INPUT FI-TilDat > TODAY THEN DO:
          MESSAGE "Fel datum"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          ASSIGN FI-TilDat:SCREEN-VALUE = string(FI-TilDat).
      END.
      ELSE DO:
          ASSIGN INPUT FI-TilDat.
          RUN Dagsrapp.
      END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Avslut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Avslut C-Win
ON CHOOSE OF MENU-ITEM m_Avslut /* Avslut */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RADIO-SET-Type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RADIO-SET-Type C-Win
ON VALUE-CHANGED OF RADIO-SET-Type IN FRAME DEFAULT-FRAME
DO:
    DEF VAR wCount AS INTE NO-UNDO.
    ASSIGN ch_Grid:Redraw = FALSE.
    DO wCount = 1 TO 18:
        IF wCount < 7 THEN
            ASSIGN ch_Grid:ColHidden(wCount) = RADIO-SET-Type:SCREEN-VALUE <> "1".
        ELSE IF wCount < 13 THEN
            ASSIGN ch_Grid:ColHidden(wCount) = RADIO-SET-Type:SCREEN-VALUE <> "2".
        ELSE
            ASSIGN ch_Grid:ColHidden(wCount) = RADIO-SET-Type:SCREEN-VALUE <> "3".
    END.
    ASSIGN ch_Grid:Redraw = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-Timer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-Timer C-Win
ON VALUE-CHANGED OF TOGGLE-Timer IN FRAME DEFAULT-FRAME /* Automatisk oppdatering */
DO:
   ASSIGN ch_Timer:Enabled   = SELF:CHECKED
          FILL-IN-Tid:HIDDEN = NOT SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{inutmld.i &Modus = "Opprett"} /* Melder fra at programmet har startet. */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
{genlib.i
  &NoLibCall      = "Nei"
  &WindowName     = "Dagsrapp hovedgrupper"
  &PreIClose      = " "
  &PostIClose     = " "
  &PostDisable_ui = " "
}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{syspara.i 1 4 1 wExcEkst}
if wExcEkst = "" then
  wExcEkst = "sdv".

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN InitButiker.
  ASSIGN FI-FraDat = TODAY
         FI-TilDat = TODAY
         wvButiker  = wAlle
         FI-Butiker = ValgteAnt(wvButiker,wAlle).
  {lng.i}
  RUN enable_UI.
  Run DagsRapp.
  assign
    C-Win:hidden = false.
  APPLY "ENTRY":U TO FI-FraDat.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

PROCEDURE ShellExecute{&A} EXTERNAL "shell32" :
   define input parameter hwnd as long.
   define input parameter lpOperation as char.
   define input parameter lpFile as char.
   define input parameter lpParameters as char.
   define input parameter lpDirectory as char.
   define input parameter nShowCmd as long.
   define return parameter hInstance as long.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AckumJmf C-Win 
PROCEDURE AckumJmf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER dStartDat AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER dSluttDat AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER dJmfStart AS DATE NO-UNDO.
  
  def var wtmpFileName as char no-undo.
  DEF VAR iCount       AS INTE NO-UNDO.
  def var wExcEkstent  as char no-undo.
  DEFINE VARIABLE cVerdier1   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cVerdier2   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iLopNr      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dOldFra     AS DATE       NO-UNDO.
  DEFINE VARIABLE cJmfVerdier AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iButik AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cUkeDag AS CHARACTER INIT "S,M,T,O,T,F,L" NO-UNDO.
  
  {sww.i}             
  DO WITH FRAME {&FRAME-NAME}:
      {syspara.i 1 4 1 wExcEkstent}
      wExcEkstent = IF wExcEkstent = "" THEN "sdv" ELSE wExcEkstent.    
      DO iCount = 1 TO ch_Grid:Rows - 3:
          ASSIGN iButik = INT(ch_Grid:TextMatrix(iCount,0)) NO-ERROR.
          IF NOT ERROR-STATUS:ERROR THEN
              FIND Butiker WHERE Butiker.Butik = iButik NO-LOCK NO-ERROR.
          ASSIGN cButikString = cButikString + (IF cButikString = "" THEN "" ELSE ";") +
                                (IF AVAIL Butiker THEN Butiker.KortNavn ELSE ch_Grid:TextMatrix(iCount,0)).
          RELEASE Butiker.
      END.
      ASSIGN ch_Grid:Redraw = FALSE
             lExcelRapp     = TRUE.
      REPEAT WHILE FI-FraDat <= dSluttDat:
          ASSIGN FI-TilDat   = FI-FraDat
                 cVerdier1   = FILL(";",NUM-ENTRIES(cButikString,";") - 1)
                 cVerdier2   = cVerdier1
                 cJmfVerdier = cVerdier1.
          RUN DagsRapp.
          DO iCount = 1 TO ch_Grid:Rows - 3:
              ASSIGN ENTRY(iCount,cVerdier1,";") = STRING(ROUND(INT(ch_Grid:TextMatrix(iCount,19)) / 1000,1)).
          END.                                     
          ASSIGN dOldFra   = FI-FraDat
                 FI-FraDat = dJmfStart + (FI-FraDat - dStartDat)
                 FI-TilDat = FI-FraDat.
          RUN DagsRapp.
          DO iCount = 1 TO ch_Grid:Rows - 3:
              ASSIGN ENTRY(iCount,cVerdier2,";") = STRING(ROUND(INT(ch_Grid:TextMatrix(iCount,19)) / 1000,1)).
          END.
          DO iCount = 1 TO NUM-ENTRIES(cVerdier1,";"):
              ASSIGN ENTRY(iCount,cJmfVerdier,";") = STRING(DECI(ENTRY(iCount,cVerdier1,";")) - DECI(ENTRY(iCount,cVerdier2,";"))).
          END.
          CREATE ttDag.
          ASSIGN iLopNr           = iLopNr + 1
                 ttDag.LopNr      = iLopNr
                 ttDag.Dato1      = dOldFra
                 ttDag.Dato2      = FI-FraDat
                 ttDag.Verdier1   = cVerdier1
                 ttDag.Verdier2   = cVerdier2
                 ttDag.JmfVerdier = cJmfVerdier
                 FI-FraDat        = dOldFra + 1.
      END.
  END.
/* OUTPUT TO "CLIPBOARD".              */
/*       FOR EACH ttDag. EXPORT ttDag. */
/*       END.                          */
/* OUTPUT CLOSE.                       */
  {swn.i}
    ASSIGN ch_Grid:Redraw = FALSE.

  /* Henter temporært filnavn. */
  IF VALID-HANDLE(wLibHandle) THEN
    RUN GetTempFileName IN wLibHandle (INPUT "dagsrapp", INPUT wExcEkstent, OUTPUT wtmpFileName).
  OUTPUT STREAM Eksport TO VALUE(wtmpFileName).
  EXPORT STREAM Eksport DELIMITER ";"
   "Periodjämförelse " +
   STRING(dStartDat,"99/99/99") + " - " + STRING(dSluttDat,"99/99/99") + " med " +
   STRING(dJmfStart,"99/99/99") + " - " + STRING(dJmfStart + (dSluttDat - dStartDat),"99/99/99").
/*    FI-FraDat:SCREEN-VALUE IN FRAME {&FRAME-NAME} + IF FI-FraDat <> FI-TilDat THEN " - " + */
/*       FI-TilDat:SCREEN-VALUE IN FRAME {&FRAME-NAME} ELSE "".                              */
   PUT STREAM Eksport SKIP.
   PUT STREAM Eksport UNFORMATTED ";" ";" ";" cButikString SKIP.
   FOR EACH ttDag:
       PUT STREAM Eksport UNFORMATTED
           DAY(ttDag.Dato1) ";"
           ENTRY(WEEKDAY(ttDag.Dato1),cUkeDag) ";"
           DAY(ttDag.Dato2) ";"     
           ttDag.JmfVerdier SKIP.
  END.
  OUTPUT STREAM Eksport CLOSE.
  EMPTY TEMP-TABLE ttDag.
  {swn.i}
  ASSIGN FI-FraDat = dStartDat
         FI-TilDat = dSluttDat
         lExcelRapp     = TRUE.
  RUN Dagsrapp.
  ch_Grid:Redraw = TRUE.
  IF VALID-HANDLE(wLibHandle) THEN
    RUN OpenExcelDocument in wLibHandle (wtmpFileName, " ").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AckumJmf2 C-Win 
PROCEDURE AckumJmf2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER dStartDat AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER dSluttDat AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER dJmfStart AS DATE NO-UNDO.
  
  def var wtmpFileName as char no-undo.
  DEF VAR iCount       AS INTE NO-UNDO.
  def var wExcEkstent  as char no-undo.
  DEFINE VARIABLE cVerdier1   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cVerdier2   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iLopNr      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dOldFra     AS DATE       NO-UNDO.
  DEFINE VARIABLE cJmfVerdier AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iButik AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cUkeDag AS CHARACTER INIT "S,M,T,O,T,F,L" NO-UNDO.
  
  {sww.i}             
  DO WITH FRAME {&FRAME-NAME}:
      CLIPBOARD:MULTIPLE = FALSE.
/*       {syspara.i 1 4 1 wExcEkstent}                                 */
/*       wExcEkstent = IF wExcEkstent = "" THEN "sdv" ELSE wExcEkstent */
      ASSIGN cButikString = "".    
      DO iCount = 1 TO ch_Grid:Rows - 3:
          ASSIGN iButik = INT(ch_Grid:TextMatrix(iCount,0)) NO-ERROR.
          IF NOT ERROR-STATUS:ERROR THEN
              FIND Butiker WHERE Butiker.Butik = iButik NO-LOCK NO-ERROR.
              ASSIGN cButikString = cButikString + (IF cButikString = "" THEN "" ELSE ";") +
                                (IF AVAIL Butiker THEN Butiker.KortNavn ELSE ch_Grid:TextMatrix(iCount,0)).
          RELEASE Butiker.
      END.
      ASSIGN ch_Grid:Redraw = FALSE
             lExcelRapp     = TRUE.
      REPEAT WHILE FI-FraDat <= dSluttDat:
          ASSIGN FI-TilDat   = FI-FraDat
                 cVerdier1   = FILL(";",NUM-ENTRIES(cButikString,";") - 2) /* 1 */
                 cVerdier2   = cVerdier1
                 cJmfVerdier = cVerdier1.
          RUN DagsRapp.
          DO iCount = 1 TO ch_Grid:Rows - 4: /* 3 */
              ASSIGN ENTRY(iCount,cVerdier1,";") = STRING(ROUND(INT(ch_Grid:TextMatrix(iCount,19)) / 1000,1)).
          END.                                     
          ASSIGN dOldFra   = FI-FraDat.
          IF dJmfStart <> ? THEN DO:
              ASSIGN FI-FraDat = dJmfStart + (FI-FraDat - dStartDat)
                     FI-TilDat = FI-FraDat.
              RUN DagsRapp.
              DO iCount = 1 TO ch_Grid:Rows - 4:  /* 3 */
                  ASSIGN ENTRY(iCount,cVerdier2,";") = STRING(ROUND(INT(ch_Grid:TextMatrix(iCount,19)) / 1000,1)).
              END.
          END.
          DO iCount = 1 TO NUM-ENTRIES(cVerdier1,";"):
              ASSIGN ENTRY(iCount,cJmfVerdier,";") = STRING(DECI(ENTRY(iCount,cVerdier1,";")) - DECI(ENTRY(iCount,cVerdier2,";"))).
          END.
          CREATE ttDag.
          ASSIGN iLopNr           = iLopNr + 1
                 ttDag.LopNr      = iLopNr
                 ttDag.Dato1      = dOldFra
                 ttDag.Dato2      = FI-FraDat
                 ttDag.Verdier1   = cVerdier1
                 ttDag.Verdier2   = cVerdier2
                 ttDag.JmfVerdier = cJmfVerdier
                 FI-FraDat        = dOldFra + 1.
      END.
  END.
/* OUTPUT TO "CLIPBOARD".              */
/*       FOR EACH ttDag. EXPORT ttDag. */
/*       END.                          */
/* OUTPUT CLOSE.                       */
  {swn.i}
    ASSIGN ch_Grid:Redraw = FALSE.

  /* Henter temporært filnavn. */
/*   IF VALID-HANDLE(wLibHandle) THEN                                                                */
/*     RUN GetTempFileName IN wLibHandle (INPUT "dagsrapp", INPUT wExcEkstent, OUTPUT wtmpFileName). */
/*   OUTPUT STREAM Eksport TO VALUE(wtmpFileName).                                                   */
/*   EXPORT STREAM Eksport DELIMITER ";"                                                             */
/*   MESSAGE cButikString SKIP                */
/*           REPLACE(cButikString,";",CHR(9)) */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.   */
  OUTPUT TO "CLIPBOARD".
      IF dJmfStart = ? THEN
          PUT UNFORMATTED 
          "Omsättningsrapport " + STRING(dStartDat,"99/99/99") + " - " + STRING(dSluttDat,"99/99/99") SKIP.
      ELSE
          PUT UNFORMATTED 
          "Periodjämförelse " +
          STRING(dStartDat,"99/99/99") + " - " + STRING(dSluttDat,"99/99/99") + " med " +
           STRING(dJmfStart,"99/99/99") + " - " + STRING(dJmfStart + (dSluttDat - dStartDat),"99/99/99") SKIP.

   PUT UNFORMATTED CHR(9) CHR(9) CHR(9) REPLACE(cButikString,";",CHR(9)) SKIP.
   FOR EACH ttDag:
       PUT UNFORMATTED
           DAY(ttDag.Dato1) CHR(9)
           ENTRY(WEEKDAY(ttDag.Dato1),cUkeDag) CHR(9)
           DAY(ttDag.Dato2) CHR(9)     
           REPLACE(ttDag.JmfVerdier,";",CHR(9)) SKIP.
  END.
  OUTPUT CLOSE.
  EMPTY TEMP-TABLE ttDag.
  {swn.i}
  ASSIGN FI-FraDat = dStartDat
         FI-TilDat = dSluttDat
         lExcelRapp     = FALSE.
  RUN Dagsrapp.
/*   ch_Grid:Redraw = TRUE. */
/*   IF VALID-HANDLE(wLibHandle) THEN                           */
/*     RUN OpenExcelDocument in wLibHandle (wtmpFileName, " "). */
  RUN pDagsExcel.p (3 + NUM-ENTRIES(cButikString,";"),iLopNr + 2).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
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

OCXFile = SEARCH( "w-dagsrappJFSpec.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    chCtrlFrame-3 = CtrlFrame-3:COM-HANDLE
    UIB_S = chCtrlFrame-3:LoadControls( OCXFile, "CtrlFrame-3":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-dagsrappJFSpec.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dagsrapp C-Win 
PROCEDURE Dagsrapp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var wRow       AS INTE NO-UNDO.
def var wCount     AS INTE NO-UNDO.
def var wCount2    AS INTE NO-UNDO.
def var wOmsButTot AS INTE NO-UNDO.
def var wTbButTot  AS INTE NO-UNDO.
def var wTbPros    AS DECI DECIMALS 1 NO-UNDO.
def var wTot    AS DECI EXTENT 21 NO-UNDO.
DEF VAR wWork      AS DEC NO-UNDO.
DEF VAR wMva%      AS DEC NO-UNDO.
DEF VAR wStripMva  AS CHAR NO-UNDO.
DEFINE VARIABLE iOmskr AS INTEGER    NO-UNDO.
DEFINE VARIABLE iDbkr  AS INTEGER    NO-UNDO.
{syspara.i 6 4 1 wStripMva}
IF CAN-DO("Ja,True,Yes",wStripMva) THEN
  {syspara.i 6 4 2 wMva% DEC}
ELSE
  wMva% = 0.
    ASSIGN ch_Timer:Enabled = FALSE
           ch_Grid:Rows     = 1
           ch_Grid:Rows     = 4.
    IF NOT lExcelRapp THEN
           ch_Grid:Redraw   = FALSE.
    DO wCount = 1 TO NUM-ENTRIES(wvButiker):
        ASSIGN wRow         = wRow + 1
               ch_Grid:Rows = ch_Grid:Rows + 1
               ch_Grid:TextMatrix(wRow,0) = ENTRY(wCount,wvButiker).
        DO wCount2 = 1 TO 21:
            ASSIGN ch_Grid:TextMatrix(wRow,wCount2) = 0.
        END.
        FOR EACH dags_rap WHERE dags_rap.butik = INT(ENTRY(wCount,wvButiker)) AND
                            dags_rap.dato >= FI-FraDat AND dags_rap.dato <= FI-TilDat NO-LOCK:
            ASSIGN /* Omsetning - 6 kollonner */
                   ch_Grid:TextMatrix(wRow,1) = INT(ch_Grid:TextMatrix(wRow,1)) + ROUND(dags_rap.hg1_oms,0)
                   ch_Grid:TextMatrix(wRow,2) = INT(ch_Grid:TextMatrix(wRow,2)) + ROUND(dags_rap.hg2_oms,0)
                   ch_Grid:TextMatrix(wRow,3) = INT(ch_Grid:TextMatrix(wRow,3)) + ROUND(dags_rap.hg3_oms,0)
                   ch_Grid:TextMatrix(wRow,4) = INT(ch_Grid:TextMatrix(wRow,4)) + ROUND(dags_rap.hg4_oms,0)
                   ch_Grid:TextMatrix(wRow,5) = INT(ch_Grid:TextMatrix(wRow,5)) + ROUND(dags_rap.hg5_oms,0)
                   ch_Grid:TextMatrix(wRow,6) = INT(ch_Grid:TextMatrix(wRow,6)) + ROUND(dags_rap.hg6_oms,0)
                   /* DB - 6 kollonner */
                   ch_Grid:TextMatrix(wRow,7) = INT(ch_Grid:TextMatrix(wRow,7)) + ROUND(dags_rap.tb1,0)
                   ch_Grid:TextMatrix(wRow,8) = INT(ch_Grid:TextMatrix(wRow,8)) + ROUND(dags_rap.tb2,0)
                   ch_Grid:TextMatrix(wRow,9) = INT(ch_Grid:TextMatrix(wRow,9)) + ROUND(dags_rap.tb3,0)
                   ch_Grid:TextMatrix(wRow,10) = INT(ch_Grid:TextMatrix(wRow,10)) + ROUND(dags_rap.tb4,0)
                   ch_Grid:TextMatrix(wRow,11) = INT(ch_Grid:TextMatrix(wRow,11)) + ROUND(dags_rap.tb5,0)
                   ch_Grid:TextMatrix(wRow,12) = INT(ch_Grid:TextMatrix(wRow,12)) + ROUND(dags_rap.tb6,0).
            ASSIGN
/*                    /* TB% - 6 kollonner */                                                                                                  */
/*                    wWork                       = (dags_rap.hg1_oms * (1 / (1 + (wMva% / 100))))                                             */
/*                    ch_Grid:TextMatrix(wRow,13) = INT(ch_Grid:TextMatrix(wRow,13)) + IF dags_rap.hg1_oms = 0 OR dags_rap.tb1 = 0 THEN 0 ELSE */
/*                             ROUND(dags_rap.tb1  / wWork * 100,1)                                                                            */
/*                    wWork                       = (dags_rap.hg2_oms * (1 / (1 + (wMva% / 100))))                                             */
/*                    ch_Grid:TextMatrix(wRow,14) = INT(ch_Grid:TextMatrix(wRow,14)) + IF dags_rap.hg2_oms = 0 OR dags_rap.tb2 = 0 THEN 0 ELSE */
/*                             ROUND(dags_rap.tb2  / wWork * 100,1)                                                                            */
/*                    wWork                       = (dags_rap.hg3_oms * (1 / (1 + (wMva% / 100))))                                             */
/*                    ch_Grid:TextMatrix(wRow,15) = INT(ch_Grid:TextMatrix(wRow,15)) + IF dags_rap.hg3_oms = 0 OR dags_rap.tb3 = 0 THEN 0 ELSE */
/*                             ROUND(dags_rap.tb3  / wWork * 100,1)                                                                            */
/*                    wWork                       = (dags_rap.hg4_oms * (1 / (1 + (wMva% / 100))))                                             */
/*                    ch_Grid:TextMatrix(wRow,16) = INT(ch_Grid:TextMatrix(wRow,16)) + IF dags_rap.hg4_oms = 0 OR dags_rap.tb4 = 0 THEN 0 ELSE */
/*                             ROUND(dags_rap.tb4  / wWork * 100,1)                                                                            */
/*                    wWork                       = (dags_rap.hg5_oms * (1 / (1 + (wMva% / 100))))                                             */
/*                    ch_Grid:TextMatrix(wRow,17) = INT(ch_Grid:TextMatrix(wRow,17)) + IF dags_rap.hg5_oms = 0 OR dags_rap.tb5 = 0 THEN 0 ELSE */
/*                             ROUND(dags_rap.tb5  / wWork * 100,1)                                                                            */
/*                    wWork                       = (dags_rap.hg6_oms * (1 / (1 + (wMva% / 100))))                                             */
/*                    ch_Grid:TextMatrix(wRow,18) = INT(ch_Grid:TextMatrix(wRow,18)) + IF dags_rap.hg6_oms = 0 OR dags_rap.tb6 = 0 THEN 0 ELSE */
/*                             ROUND(dags_rap.tb6  / wWork * 100,1)                                                                            */
                   /* Total omsetning for butikken */
                   ch_Grid:TextMatrix(wRow,19) = INT(ch_Grid:TextMatrix(wRow,19)) + dags_rap.hg1_oms +
                                                       dags_rap.hg2_oms +
                                                       dags_rap.hg3_oms +
                                                       dags_rap.hg4_oms +
                                                       dags_rap.hg5_oms +
                                                       dags_rap.hg6_oms
                   /* Totalt DB for butikken */
                   ch_Grid:TextMatrix(wRow,20) = INT(ch_Grid:TextMatrix(wRow,20)) + dags_rap.tb1 +
                                                       dags_rap.tb2 +
                                                       dags_rap.tb3 +
                                                       dags_rap.tb4 +
                                                       dags_rap.tb5 +
                                                       dags_rap.tb6
                   /* Totalt DB% for butikken, */
                   wWork                       = (INT(ch_Grid:TextMatrix(wRow,19)) * (1 / (1 + (wMva% / 100))))
                   ch_Grid:TextMatrix(wRow,21) = IF INT(ch_Grid:TextMatrix(wRow,19)) = 0 OR 
                                                    INT(ch_Grid:TextMatrix(wRow,20)) = 0 THEN 0 ELSE
                           ROUND(INT(ch_Grid:TextMatrix(wRow,20)) / wWork * 100,1)
ch_Grid:TextMatrix(wRow,20) = INT(0)
                   wTot[1] = wTot[1] + dags_rap.hg1_oms
                   wTot[2] = wTot[2] + dags_rap.hg2_oms
                   wTot[3] = wTot[3] + dags_rap.hg3_oms
                   wTot[4] = wTot[4] + dags_rap.hg4_oms
                   wTot[5] = wTot[5] + dags_rap.hg5_oms
                   wTot[6] = wTot[6] + dags_rap.hg6_oms
                   wTot[7] = wTot[7] + dags_rap.tb1
                   wTot[8] = wTot[8] + dags_rap.tb2
                   wTot[9] = wTot[9] + dags_rap.tb3
                   wTot[10] = wTot[10] + dags_rap.tb4
                   wTot[11] = wTot[11] + dags_rap.tb5
                   wTot[12] = wTot[12] + dags_rap.tb6.
/*
MESSAGE wMva% wWork (INT(ch_Grid:TextMatrix(wRow,19)) * (1 / (1 + (wMva% / 100)))) VIEW-AS ALERT-BOX.
*/        
        END.
    END.
    DO wCount = 1 TO NUM-ENTRIES(wvButiker):
        DO wCount2 = 1 TO 6:
            IF INT(ch_Grid:TextMatrix(wCount,wCount2)) <> 0 AND INT(ch_Grid:TextMatrix(wCount,wCount2 + 6)) <> 0 THEN
                ASSIGN iOmskr = INT(ch_Grid:TextMatrix(wCount,wCount2))
                       iDbkr  = INT(ch_Grid:TextMatrix(wCount,wCount2 + 6))
                      ch_Grid:TextMatrix(wCount,wCount2 + 12) = 
                                                     ROUND(iDbKr / (iOmskr * (1 / (1 + (wMva% / 100)))) * 100,1).
            ELSE
                ASSIGN ch_Grid:TextMatrix(wCount,wCount2 + 12) = 0.
        END.
    END.
    ASSIGN  wRow = wRow + 1
            wWork                       = (wTot[1] * (1 / (1 + (wMva% / 100))))
            wTot[13] = IF wTot[1] = 0 OR wTot[7] = 0 THEN 0 ELSE
                       ROUND(wTot[7] / wWork * 100,1)
            wWork                       = (wTot[2] * (1 / (1 + (wMva% / 100))))
            wTot[14] = IF wTot[2] = 0 OR wTot[8] = 0 THEN 0 ELSE
                       ROUND(wTot[8] / wWork * 100,1)
            wWork                       = (wTot[3] * (1 / (1 + (wMva% / 100))))
            wTot[15] = IF wTot[3] = 0 OR wTot[9] = 0 THEN 0 ELSE
                       ROUND(wTot[9] / wWork * 100,1)
            wWork                       = (wTot[4] * (1 / (1 + (wMva% / 100))))
            wTot[16] = IF wTot[4] = 0 OR wTot[10] = 0 THEN 0 ELSE
                       ROUND(wTot[10] / wWork * 100,1)
            wWork                       = (wTot[5] * (1 / (1 + (wMva% / 100))))
            wTot[17] = IF wTot[5] = 0 OR wTot[11] = 0 THEN 0 ELSE
                       ROUND(wTot[11] / wWork * 100,1)
            wWork                       = (wTot[6] * (1 / (1 + (wMva% / 100))))
            wTot[18] = IF wTot[6] = 0 OR wTot[12] = 0 THEN 0 ELSE
                       ROUND(wTot[12] / wWork * 100,1)
            ch_Grid:TextMatrix(wRow,0) = "Totalt"
            ch_Grid:TextMatrix(wRow,1) = ROUND(wTot[1],0)
            ch_Grid:TextMatrix(wRow,2) = ROUND(wTot[2],0)
            ch_Grid:TextMatrix(wRow,3) = ROUND(wTot[3],0)
            ch_Grid:TextMatrix(wRow,4) = ROUND(wTot[4],0)
            ch_Grid:TextMatrix(wRow,5) = ROUND(wTot[5],0)
            ch_Grid:TextMatrix(wRow,6) = ROUND(wTot[6],0)
            ch_Grid:TextMatrix(wRow,7) = ROUND(wTot[7],0)
            ch_Grid:TextMatrix(wRow,8) = ROUND(wTot[8],0)
            ch_Grid:TextMatrix(wRow,9) = ROUND(wTot[9],0)
            ch_Grid:TextMatrix(wRow,10) = ROUND(wTot[10],0)
            ch_Grid:TextMatrix(wRow,11) = ROUND(wTot[11],0)
            ch_Grid:TextMatrix(wRow,12) = ROUND(wTot[12],0)
            ch_Grid:TextMatrix(wRow,13) = ROUND(wTot[13],1)
            ch_Grid:TextMatrix(wRow,14) = ROUND(wTot[14],1)
            ch_Grid:TextMatrix(wRow,15) = ROUND(wTot[15],1)
            ch_Grid:TextMatrix(wRow,16) = ROUND(wTot[16],1)
            ch_Grid:TextMatrix(wRow,17) = ROUND(wTot[17],1)
            ch_Grid:TextMatrix(wRow,18) = ROUND(wTot[18],1)
            ch_Grid:TextMatrix(wRow,19) = ROUND(wTot[1] +
                                               wTot[2] +
                                               wTot[3] +
                                               wTot[4] +
                                               wTot[5] +
                                               wTot[6],0)
            ch_Grid:TextMatrix(wRow,20) = ROUND(wTot[7] +
                                               wTot[8] +
                                               wTot[9] +
                                               wTot[10] +
                                               wTot[11] +
                                               wTot[12],0)
            wWork                       = (INT(ch_Grid:TextMatrix(wRow,19)) * (1 / (1 + (wMva% / 100))))
            ch_Grid:TextMatrix(wRow,21) = IF INT(ch_Grid:TextMatrix(wRow,19)) = 0 OR
                                             INT(ch_Grid:TextMatrix(wRow,20)) = 0 THEN 0 ELSE
                                            ROUND(INT(ch_Grid:TextMatrix(wRow,20)) / wWork * 100,1)
ch_Grid:TextMatrix(wRow,20) = 0
            B-Excel:SENSITIVE IN FRAME {&FRAME-NAME}     = INT(ch_Grid:TextMatrix(wRow,19)) > 0
            B-Html:SENSITIVE IN FRAME {&FRAME-NAME}      = INT(ch_Grid:TextMatrix(wRow,19)) > 0
            B-HtmlExcel:SENSITIVE IN FRAME {&FRAME-NAME} = INT(ch_Grid:TextMatrix(wRow,19)) > 0
            B-Jmf:SENSITIVE IN FRAME {&FRAME-NAME} = INT(ch_Grid:TextMatrix(wRow,19)) > 0.
    DO wCount = 0 TO 21:
        ASSIGN ch_Grid:Cell(13,wRow,wCount,wRow,wCount) = TRUE.
    END.
    DO wCount = 1 TO wRow:
        ASSIGN ch_Grid:Cell(13,wCount,19,wCount,19) = TRUE
               ch_Grid:Cell(13,wCount,20,wCount,20) = TRUE.
               ch_Grid:Cell(13,wCount,21,wCount,21) = TRUE.
    END.
    IF NOT lExcelRapp THEN DO:
      ASSIGN ch_Grid:Row = 0
             ch_Grid:Col = 0
             ch_Grid:Redraw = true.
      IF (RADIO-SET-Type:SCREEN-VALUE = "2" OR RADIO-SET-Type:SCREEN-VALUE = "3") AND INT(ch_Grid:TextMatrix(ch_Grid:Rows - 3,15)) = 0 THEN DO:
          ASSIGN RADIO-SET-Type:SCREEN-VALUE = "1".
          APPLY "VALUE-CHANGED" TO RADIO-SET-Type.
      END.
      ch_Grid:Refresh().
      ASSIGN TOGGLE-Timer:SENSITIVE   = FI-FraDat = TODAY
             ch_Timer:Enabled         = TOGGLE-Timer:SENSITIVE AND TOGGLE-Timer:CHECKED
             FILL-IN-Tid:HIDDEN       = NOT ch_Timer:Enabled
             FILL-IN-Tid:SCREEN-VALUE = STRING(TIME,"HH:MM:SS").
    END.
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
  RUN control_load.
  DISPLAY FI-FraDat FI-TilDat FI-Butiker FILL-IN-Tid RADIO-SET-Type TOGGLE-Timer 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-51 RECT-52 B-SpinFraUp B-SpinTilUp B-Butikker FI-FraDat 
         B-SpinFraDown B-SpinTilDown RADIO-SET-Type TOGGLE-Timer Btn_Avslut 
         Btn_Help BUTTON-SokDato 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportToExcel C-Win 
PROCEDURE ExportToExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wtmpFileName as char no-undo.
  def var lAck         as log INIT TRUE no-undo.
  DEF VAR wCount       AS INTE NO-UNDO.
  def var wExcEkstent  as char no-undo.
  
  DEF VAR dStartDat AS DATE NO-UNDO.
  DEF VAR dSlutDat  AS DATE NO-UNDO.

  
  DO WITH FRAME {&FRAME-NAME}:
  {syspara.i 1 4 1 wExcEkstent}
  wExcEkstent = if wExcEkstent = "" then "sdv" else wExcEkstent.    
  IF FI-FraDat <> FI-TilDat THEN
      MESSAGE "Ackumulerad dagsrapport?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekräfta" UPDATE lAck.
  ASSIGN dStartDat = FI-FraDat
         dSlutDat  = FI-TilDat.
  IF NOT lAck THEN
      ASSIGN ch_Grid:Redraw = FALSE
             lExcelRapp     = TRUE.
  /* Henter temporært filnavn. */
  if valid-handle(wLibHandle) then
    run GetTempFileName in wLibHandle (input "dagsrapp", input wExcEkstent, output wtmpFileName).
  output stream Eksport to value(wtmpFileName).
  {sww.i}
 REPEAT WHILE FI-FraDat <= dSlutDat:
  IF NOT lAck AND dStartDat <> dSlutDat THEN DO:
      ASSIGN FI-TilDat = FI-FraDat.
      RUN DagsRapp.
      IF INT(ch_Grid:TextMatrix(ch_Grid:Rows - 3,19)) = 0 THEN DO:
          ASSIGN FI-FraDat = FI-FraDat + 1.
          NEXT.
      END.
  END.
   EXPORT STREAM Eksport DELIMITER ";"
   "Dagsrapport "
   STRING(FI-FraDat,"99/99/99") + IF lAck = TRUE AND FI-FraDat <> FI-TilDat THEN 
                       " - " + STRING(FI-TilDat,"99/99/99") ELSE "".
/*    FI-FraDat:SCREEN-VALUE IN FRAME {&FRAME-NAME} + IF FI-FraDat <> FI-TilDat THEN " - " + */
/*       FI-TilDat:SCREEN-VALUE IN FRAME {&FRAME-NAME} ELSE "".                              */
   EXPORT STREAM Eksport " ".
   EXPORT STREAM Eksport ENTRY(1,RADIO-SET-Type:RADIO-BUTTONS).
    DO wCount = 0 TO ch_Grid:Rows - 3:          
    export stream Eksport delimiter ";"
    ch_Grid:TextMatrix(wCount,0)
    ch_Grid:TextMatrix(wCount,1)
    ch_Grid:TextMatrix(wCount,2)
    ch_Grid:TextMatrix(wCount,3)
    ch_Grid:TextMatrix(wCount,4)
    ch_Grid:TextMatrix(wCount,5)
    ch_Grid:TextMatrix(wCount,6)
    IF wCount = 0 THEN ch_Grid:TextMatrix(wCount,19) ELSE STRING(INT(ch_Grid:TextMatrix(wCount,19))).
  END.
   EXPORT STREAM Eksport " ".
   EXPORT STREAM Eksport ENTRY(3,RADIO-SET-Type:RADIO-BUTTONS).
/*   DO wCount = 0 TO ch_Grid:Rows - 3:                                                                  */
/*     export stream Eksport delimiter ";"                                                               */
/*     ch_Grid:TextMatrix(wCount,0)                                                                      */
/*     ch_Grid:TextMatrix(wCount,7)                                                                      */
/*     ch_Grid:TextMatrix(wCount,8)                                                                      */
/*     ch_Grid:TextMatrix(wCount,9)                                                                      */
/*     ch_Grid:TextMatrix(wCount,10)                                                                     */
/*     ch_Grid:TextMatrix(wCount,11)                                                                     */
/*     ch_Grid:TextMatrix(wCount,12)                                                                     */
/*     IF wCount = 0 THEN ch_Grid:TextMatrix(wCount,20) ELSE STRING(INT(ch_Grid:TextMatrix(wCount,20))). */
/*   END.                                                                                                */
/*    EXPORT STREAM Eksport " ".                                                                         */
/*    EXPORT STREAM Eksport ENTRY(5,RADIO-SET-Type:RADIO-BUTTONS).                                       */
  DO wCount = 0 TO ch_Grid:Rows - 3:          
    export stream Eksport delimiter ";"
    ch_Grid:TextMatrix(wCount,0)
    IF wCount = 0 THEN ch_Grid:TextMatrix(wCount,13) ELSE STRING(ROUND(DECI(ch_Grid:TextMatrix(wCount,13)),1),"->>>9.9")
    IF wCount = 0 THEN ch_Grid:TextMatrix(wCount,14) ELSE STRING(ROUND(DECI(ch_Grid:TextMatrix(wCount,14)),1),"->>>9.9")
    IF wCount = 0 THEN ch_Grid:TextMatrix(wCount,15) ELSE STRING(ROUND(DECI(ch_Grid:TextMatrix(wCount,15)),1),"->>>9.9")
    IF wCount = 0 THEN ch_Grid:TextMatrix(wCount,16) ELSE STRING(ROUND(DECI(ch_Grid:TextMatrix(wCount,16)),1),"->>>9.9")
    IF wCount = 0 THEN ch_Grid:TextMatrix(wCount,17) ELSE STRING(ROUND(DECI(ch_Grid:TextMatrix(wCount,17)),1),"->>>9.9")
    IF wCount = 0 THEN ch_Grid:TextMatrix(wCount,18) ELSE STRING(ROUND(DECI(ch_Grid:TextMatrix(wCount,18)),1),"->>>9.9")
    IF wCount = 0 THEN ch_Grid:TextMatrix(wCount,21) ELSE STRING(ROUND(DECI(ch_Grid:TextMatrix(wCount,21)),1),"->>>9.9").
  END.
  {swn.i}
    IF NOT lAck AND FI-FraDat < dSlutDat THEN
        ASSIGN FI-FraDat = FI-FraDat + 1.
    ELSE LEAVE.
    EXPORT STREAM Eksport " ".
  END.
  output stream Eksport close.
  END.
  IF dStartDat <> dSlutDat THEN DO:
      ASSIGN FI-FraDat = dStartDat
             FI-TilDat = dSLutDat
             lExcelRapp     = TRUE.
      RUN Dagsrapp.
  END.
  IF NOT lAck THEN
      ch_Grid:Redraw = TRUE.
  if valid-handle(wLibHandle) then
    run OpenExcelDocument in wLibHandle (wtmpFileName, " ").
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportToHtml C-Win 
PROCEDURE ExportToHtml :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAMETER wOpenIn      AS CHAR NO-UNDO.
   DEF VAR             wtmpFileName AS CHAR NO-UNDO.
   DEF VAR             wRadio       AS CHAR NO-UNDO.
   DO WITH FRAME {&FRAME-NAME}:
       if valid-handle(wLibHandle) then
           run GetTempFileName in wLibHandle ("HTM","HTM",output wtmpFileName). 
       ASSIGN wRadio = ENTRY(1,RADIO-SET-Type:RADIO-BUTTONS) + "|" + 
                       ENTRY(3,RADIO-SET-Type:RADIO-BUTTONS) 
/*            + "|" + ENTRY(5,RADIO-SET-Type:RADIO-BUTTONS) */
           .
       RUN dagsrapphtml.p (ch_Grid,
                           FI-FraDat:SCREEN-VALUE +
                                   IF FI-FraDat < FI-TilDat THEN " - " + FI-TilDat:SCREEN-VALUE ELSE "",
                           wRadio,
                           wtmpFileName).
       if valid-handle(wLibHandle) then DO:
          IF wOpenIn = "HTM" THEN
             RUN OpenWeb in wLibHandle (wtmpFileName).
          ELSE run OpenExcelDocument in wLibHandle (wtmpFileName," ").
       END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitButiker C-Win 
PROCEDURE InitButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Butikker */
  for each Butiker no-lock:
    assign
      wAlle = wAlle + 
                 (if wAlle = "" 
                    then ""
                    else ",") +
                 string(Butiker.Butik).
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls C-Win 
PROCEDURE initialize-controls :
DEF VAR wCount AS INTE NO-UNDO.
    ASSIGN ch_Timer = chCtrlFrame-3:PSTimer
           ch_Timer:Interval = 5000
           ch_Timer:Enabled  = FALSE
           ch_Grid = chCtrlFrame:vsFlexGrid
           ch_Grid:Redraw = FALSE /* disable repaint while populating */
           ch_Grid:Cols   = 22
           ch_Grid:Rows   = 21
           ch_Grid:TextMatrix(0,0) = "Butik"
           ch_Grid:TextMatrix(0,1) = "Oms Hg 1"
           ch_Grid:TextMatrix(0,2) = "Oms Hg 2"
           ch_Grid:TextMatrix(0,3) = "Oms Hg 3"
           ch_Grid:TextMatrix(0,4) = "Oms Hg 4"
           ch_Grid:TextMatrix(0,5) = "Oms Hg 5"
           ch_Grid:TextMatrix(0,6) = "Oms Hg 6"
           ch_Grid:TextMatrix(0,7) = "DB Hg 1"
           ch_Grid:TextMatrix(0,8) = "DB Hg 2"
           ch_Grid:TextMatrix(0,9) = "DB Hg 3"
           ch_Grid:TextMatrix(0,10) = "DB Hg 4"
           ch_Grid:TextMatrix(0,11) = "DB Hg 5"
           ch_Grid:TextMatrix(0,12) = "DB Hg 6"
           ch_Grid:TextMatrix(0,13) = "DB% Hg 1"
           ch_Grid:TextMatrix(0,14) = "DB% Hg 2"
           ch_Grid:TextMatrix(0,15) = "DB% Hg 3"
           ch_Grid:TextMatrix(0,16) = "DB% Hg 4"
           ch_Grid:TextMatrix(0,17) = "DB% Hg 5"
           ch_Grid:TextMatrix(0,18) = "DB% Hg 6"
           ch_Grid:TextMatrix(0,19) = "Oms totalt"
           ch_Grid:TextMatrix(0,20) = "DB totalt"
           ch_Grid:TextMatrix(0,21) = "DB %"
           ch_Grid:ColHidden(7)      = TRUE
           ch_Grid:ColHidden(8)      = TRUE
           ch_Grid:ColHidden(9)      = TRUE
           ch_Grid:ColHidden(10)     = TRUE
           ch_Grid:ColHidden(11)     = TRUE
           ch_Grid:ColHidden(12)     = TRUE
           ch_Grid:ColHidden(13)      = TRUE
           ch_Grid:ColHidden(14)      = TRUE
           ch_Grid:ColHidden(15)      = TRUE
           ch_Grid:ColHidden(16)     = TRUE
           ch_Grid:ColHidden(17)     = TRUE
           ch_Grid:ColHidden(18)     = TRUE
           ch_Grid:Cell(13,0,19,0,19) = TRUE
           ch_Grid:Cell(13,0,20,0,20) = TRUE
           ch_Grid:Cell(13,0,21,0,21) = TRUE
           ch_Grid:ColWidth(19) = 1.3 * ch_Grid:ColWidth(19)
           ch_Grid:ColWidth(20) = 1.3 * ch_Grid:ColWidth(20)
           ch_Grid:ColWidth(21) = 0.5 * ch_Grid:ColWidth(21)
           ch_Grid:AllowSelection  = FALSE
           ch_Grid:ExtendLastCol   = TRUE.
    DO wCount = 0 TO 20:
        ASSIGN ch_Grid:ColFormat(wCount)    = IF wCount < 13 OR wCount > 18 THEN "###,###,###"
                                              ELSE "###,###,###.#"
               ch_Grid:ColAlignment(wCount) = 7.
    END.
    ASSIGN ch_Grid:ColFormat(21)    = "###,###,###.#"
           ch_Grid:ColAlignment(21) = 7.
    ASSIGN ch_Grid:Redraw = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValgteAnt C-Win 
FUNCTION ValgteAnt RETURNS CHARACTER
  ( INPUT wValgte AS CHARACTER, INPUT wAlle AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN " " + STRING(NUM-ENTRIES(wValgte)) + "/" + 
               STRING(NUM-ENTRIES(wAlle)).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

