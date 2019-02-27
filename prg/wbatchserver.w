&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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

/* DEF VAR h_wfiler    AS HANDLE NO-UNDO. */
DEF VAR h_wdatasett AS HANDLE NO-UNDO.
/* 4 variabler som läser 3 systemparametrar 200 2 */
/* läses in i initialize-controls                 */
/* iIntervall minsta 60, om fel i systempara 60   */
DEFINE VARIABLE lEnaOnStart AS LOGICAL    NO-UNDO. /* 1                         */
DEFINE VARIABLE iIntervall  AS INTEGER    NO-UNDO. /* 2   Timertick iSek        */
DEFINE VARIABLE iFirstTick  AS INTEGER    NO-UNDO. /* 4:1 Sek från morgon 00:00 */
DEFINE VARIABLE iLastTick   AS INTEGER    NO-UNDO. /* 4:2 Sek från morgon 00:00 */
DEFINE VARIABLE iEnaOnStartBG AS INTEGER INIT ? NO-UNDO.
DEFINE VARIABLE iIntervallBG  AS INTEGER INIT ? NO-UNDO.
DEFINE VARIABLE iFirstTickBG  AS INTEGER INIT ? NO-UNDO.
DEFINE VARIABLE iLastTickBG   AS INTEGER INIT ? NO-UNDO.
DEFINE VARIABLE iNesteTidBG   AS INTEGER INIT ? NO-UNDO.
DEFINE VARIABLE iHPix         AS INTEGER    NO-UNDO.
DEFINE VARIABLE iWPix         AS INTEGER    NO-UNDO.
DEFINE VARIABLE lRunEksportXlnt AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cXlntDir        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lRunOverfor     AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lRunVpi         AS LOG        NO-UNDO.
DEFINE VARIABLE lRunRigalut     AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cRigalEkstent   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRigalDir       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lFinansPro      AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cFlyttDir       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFlyttKommando  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFinansProDir   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lFinansPreem    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cLoggFil        AS CHAR       NO-UNDO.
DEFINE VARIABLE cDelimiter      AS CHAR INIT ";" NO-UNDO.
DEFINE VARIABLE iForrige        AS INT        NO-UNDO.
DEFINE VARIABLE lHTutlegg       AS LOGICAL    NO-UNDO.
DEFINE VARIABLE iHTutleggTid    AS INTEGER    NO-UNDO.
DEFINE VARIABLE dHTKjort        AS DATE       NO-UNDO.
DEFINE VARIABLE lIconExist      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cRedIcon        AS CHARACTER INIT ".\icon\bullet_red.ico"    NO-UNDO.
DEFINE VARIABLE cGreenIcon      AS CHARACTER INIT ".\icon\bullet_green.ico"  NO-UNDO.
DEFINE VARIABLE cYellowIcon     AS CHARACTER INIT ".\icon\bullet_yellow.ico"    NO-UNDO.
DEF VAR pcSession     AS CHAR NO-UNDO.
DEF VAR bPBR   AS LOG  NO-UNDO.
DEF VAR cTekst AS CHAR NO-UNDO.
DEFINE VARIABLE hServer AS HANDLE     NO-UNDO.
DEFINE VARIABLE cConnect AS CHARACTER INIT "-S datamottak" NO-UNDO.

DEF STREAM Logg.

{initjukebox.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-1 B-StartStop RECT-51 RECT-52 RECT-53 ~
RECT-54 RECT-55 B-LesInn B-Oppdater B-Overfor B-OppdaterVPI ~
FI-ServerparamTxt FI-UtforTxt FI-ServerstatTxt 
&Scoped-Define DISPLAYED-OBJECTS TG-Profitbase TG-Overfor TG-Xlent ~
FI-DatoTid TG-Rigal TG-FinansPro TG-FinansPreem FI-Oppstart FI-Utforer ~
TG-Vpi FI-Intervall FILL-IN-1 EDITOR-1 FI-FirstTid FILL-IN-2 FI-LastTid ~
FI-Status FI-NesteTid FI-NesteDato FI-StartTid FI-StartDato ~
FI-ServerparamTxt FI-UtforTxt FI-ServerstatTxt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD settLoggFil wWin 
FUNCTION settLoggFil RETURNS CHARACTER
  ( OUTPUT pcLoggFil AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD socketconnect wWin 
FUNCTION socketconnect RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD startup wWin 
FUNCTION startup RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE PSKlocka AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chPSKlocka AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE PSTimer AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chPSTimer AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_wfiler AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-DataMottak 
     LABEL "Vis datamottak" 
     SIZE 37 BY 1.14.

DEFINE BUTTON B-Datasett 
     LABEL "Vis Datasett og kvitteringer" 
     SIZE 37 BY 1.14.

DEFINE BUTTON B-LesInn 
     LABEL "Les inn filer" 
     SIZE 27 BY 1.14.

DEFINE BUTTON B-Oppdater 
     LABEL "Oppdater datasett" 
     SIZE 27 BY 1.14.

DEFINE BUTTON B-OppdaterVPI 
     LABEL "Leser og oppdater VPI" 
     SIZE 27 BY 1.14.

DEFINE BUTTON B-Overfor 
     LABEL "Overfør datasett" 
     SIZE 27 BY 1.14.

DEFINE BUTTON B-StartStop  NO-FOCUS
     LABEL "Start/Stopp server" 
     SIZE 25 BY 1.14.

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 29 BY 9.33 NO-UNDO.

DEFINE VARIABLE FI-DatoTid AS CHARACTER FORMAT "X(256)":U 
     LABEL "Dato/Tid" 
     VIEW-AS FILL-IN 
     SIZE 27.8 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-FirstTid AS CHARACTER FORMAT "X(256)":U 
     LABEL "Første tid" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-Intervall AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Intervall" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-LastTid AS CHARACTER FORMAT "X(256)":U 
     LABEL "Siste tid" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-NesteDato AS CHARACTER FORMAT "X(256)":U 
     LABEL "Dato" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-NesteTid AS CHARACTER FORMAT "X(256)":U 
     LABEL "Neste tid" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-Oppstart AS CHARACTER FORMAT "X(256)":U 
     LABEL "Oppstart" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-ServerparamTxt AS CHARACTER FORMAT "X(256)":U INITIAL " Serverparameter" 
      VIEW-AS TEXT 
     SIZE 21 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-ServerstatTxt AS CHARACTER FORMAT "X(256)":U INITIAL " Serverstatus" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-StartDato AS CHARACTER FORMAT "X(256)":U 
     LABEL "Startdato" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-StartTid AS CHARACTER FORMAT "X(256)":U 
     LABEL "Start tid" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-Status AS CHARACTER FORMAT "X(256)":U 
     LABEL "Statusmelding" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Utforer AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utfører" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE FI-UtforTxt AS CHARACTER FORMAT "X(256)":U INITIAL " Utfører" 
      VIEW-AS TEXT 
     SIZE 11 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fil" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Datasett" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "icon/redlight.jpg":U
     SIZE 32 BY 3.33.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 5.19.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 3.57.

DEFINE RECTANGLE RECT-53
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32 BY 5.19.

DEFINE RECTANGLE RECT-54
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32 BY 5.38.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 29 BY 5.95.

DEFINE VARIABLE TG-FinansPreem AS LOGICAL INITIAL no 
     LABEL "Finans til Preem" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE TG-FinansPro AS LOGICAL INITIAL no 
     LABEL "Eksport finans til Pro" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Overfor AS LOGICAL INITIAL no 
     LABEL "Overføring" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Profitbase AS LOGICAL INITIAL no 
     LABEL "Profitbase" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Rigal AS LOGICAL INITIAL no 
     LABEL "Eksport Rigal" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Vpi AS LOGICAL INITIAL no 
     LABEL "VPI" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Xlent AS LOGICAL INITIAL no 
     LABEL "Eksport Xlent" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B-StartStop AT ROW 14.67 COL 41
     TG-Profitbase AT ROW 1.33 COL 121
     TG-Overfor AT ROW 2.05 COL 121
     TG-Xlent AT ROW 2.81 COL 121
     FI-DatoTid AT ROW 2.91 COL 60.2 COLON-ALIGNED
     TG-Rigal AT ROW 3.57 COL 121
     TG-FinansPro AT ROW 4.33 COL 121
     TG-FinansPreem AT ROW 5.19 COL 121
     FI-Oppstart AT ROW 5.67 COL 13.4 COLON-ALIGNED
     FI-Utforer AT ROW 5.67 COL 60.2 COLON-ALIGNED
     TG-Vpi AT ROW 6.05 COL 121
     FI-Intervall AT ROW 6.76 COL 13.4 COLON-ALIGNED
     FILL-IN-1 AT ROW 6.76 COL 60.2 COLON-ALIGNED
     EDITOR-1 AT ROW 7.14 COL 119 NO-LABEL
     FI-FirstTid AT ROW 7.81 COL 13.4 COLON-ALIGNED
     FILL-IN-2 AT ROW 7.81 COL 60.2 COLON-ALIGNED
     FI-LastTid AT ROW 8.86 COL 13.4 COLON-ALIGNED
     FI-Status AT ROW 8.86 COL 60.2 COLON-ALIGNED
     B-Datasett AT ROW 11.14 COL 40.2
     B-LesInn AT ROW 11.14 COL 91
     FI-NesteTid AT ROW 11.67 COL 13.2 COLON-ALIGNED
     B-DataMottak AT ROW 12.33 COL 40.2
     B-Oppdater AT ROW 12.33 COL 91
     FI-NesteDato AT ROW 12.76 COL 13.2 COLON-ALIGNED
     B-Overfor AT ROW 13.52 COL 91
     FI-StartTid AT ROW 13.86 COL 13.2 COLON-ALIGNED
     B-OppdaterVPI AT ROW 14.67 COL 91
     FI-StartDato AT ROW 14.91 COL 13.2 COLON-ALIGNED
     FI-ServerparamTxt AT ROW 4.81 COL 2 COLON-ALIGNED NO-LABEL
     FI-UtforTxt AT ROW 4.81 COL 40 COLON-ALIGNED NO-LABEL
     FI-ServerstatTxt AT ROW 10.81 COL 2.2 COLON-ALIGNED NO-LABEL
     "Data mottaksserver" VIEW-AS TEXT
          SIZE 33 BY 1.43 AT ROW 1.24 COL 60
          FGCOLOR 9 
     IMAGE-1 AT ROW 1 COL 1
     RECT-51 AT ROW 5.14 COL 40
     RECT-52 AT ROW 1.14 COL 40
     RECT-53 AT ROW 5.14 COL 3
     RECT-54 AT ROW 11.14 COL 3
     RECT-55 AT ROW 1.14 COL 119
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 148 BY 15.76.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Datamottaksserver"
         HEIGHT             = 15.76
         WIDTH              = 148
         MAX-HEIGHT         = 15.81
         MAX-WIDTH          = 168.2
         VIRTUAL-HEIGHT     = 15.81
         VIRTUAL-WIDTH      = 168.2
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}
{dproclibstart.i}
{hjelp.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON B-DataMottak IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       B-DataMottak:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR BUTTON B-Datasett IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       B-Datasett:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR EDITOR EDITOR-1 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-DatoTid IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-FirstTid IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Intervall IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LastTid IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-NesteDato IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-NesteTid IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Oppstart IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-StartDato IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-StartTid IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Status IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Utforer IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-2 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-FinansPreem IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-FinansPro IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Overfor IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Profitbase IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Rigal IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Vpi IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Xlent IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME PSTimer ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 1.24
       COLUMN          = 105
       HEIGHT          = 2.14
       WIDTH           = 10
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME PSKlocka ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 3.62
       COLUMN          = 105
       HEIGHT          = 2.14
       WIDTH           = 10
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      PSTimer:NAME = "PSTimer":U .
/* PSTimer OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      PSKlocka:NAME = "PSKlocka":U .
/* PSKlocka OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      PSKlocka:MOVE-AFTER(TG-Rigal:HANDLE IN FRAME fMain).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Datamottaksserver */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Datamottaksserver */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  IF chPSTimer:ENABLED = TRUE THEN DO:
      MESSAGE "Du må stoppe serveren for å kunne stenge vinduet."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.

/*   RETURN NO-APPLY. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-DataMottak
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-DataMottak wWin
ON CHOOSE OF B-DataMottak IN FRAME fMain /* Vis datamottak */
DO:
    RUN wdatasett PERSISTENT SET h_wdatasett.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Datasett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Datasett wWin
ON CHOOSE OF B-Datasett IN FRAME fMain /* Vis Datasett og kvitteringer */
DO:
  RUN wfiler PERSISTENT SET h_wfiler.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LesInn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LesInn wWin
ON CHOOSE OF B-LesInn IN FRAME fMain /* Les inn filer */
DO:
  IF VALID-HANDLE(h_wfiler) THEN DO:
      ASSIGN FI-Utforer:SCREEN-VALUE = SELF:LABEL
             FI-Status:SCREEN-VALUE = "Leser inn filer. Pågår...".
      EDITOR-1:INSERT-STRING("ScannKataloger" + CHR(10)).
      PROCESS EVENTS.
      RUN bibl_logg.p ('DataMottak', 'wbatchserver.w B-LesInn: Starter. Starter ScannKataloger.').
      RUN ScannKataloger IN h_wfiler.
/*       PAUSE 5 NO-MESSAGE. */
      EDITOR-1:INSERT-STRING("LesInnBatch" + CHR(10)).
      PROCESS EVENTS.
      RUN bibl_logg.p ('DataMottak', 'wbatchserver.w B-LesInn: Starter. Starter LesInnBatch.').
      RUN LesInnBatch IN h_wfiler.
      ASSIGN FI-Utforer:SCREEN-VALUE = ""
             FI-Status:SCREEN-VALUE = SELF:LABEL + " - OK".
/*       FILL-IN-1:SCREEN-VALUE = "KLAR". */
      RUN bibl_logg.p ('DataMottak', 'wbatchserver.w B-LesInn: Ferdig.').
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Oppdater
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Oppdater wWin
ON CHOOSE OF B-Oppdater IN FRAME fMain /* Oppdater datasett */
DO:
    IF VALID-HANDLE(h_wfiler) THEN DO:
        ASSIGN FI-Utforer:SCREEN-VALUE = SELF:LABEL
               FI-Status:SCREEN-VALUE = "Oppdaterer datasett. Pågår...".
        EDITOR-1:INSERT-STRING("OppdaterBatch" + CHR(10)).
        PROCESS EVENTS.
        RUN bibl_logg.p ('DataMottak', 'wbatchserver.w B-Oppdater: Starter. Starter OppdaterBatch.').
        RUN OppdaterBatch IN h_wfiler.
        IF bPBR = TRUE THEN
        DO:
            ASSIGN FI-Utforer:SCREEN-VALUE = "PBR"
                   FI-Status:SCREEN-VALUE = "Oppdaterer PBR. Pågår...".
            EDITOR-1:INSERT-STRING("pfxoppdatstat" + CHR(10)).
            PROCESS EVENTS.
            RUN bibl_logg.p ('DataMottak', 'wbatchserver.w B-Oppdater: Starter. Starter pfxoppdatstat.p.').
            RUN pfxoppdatstat.p.
        END.
        IF lRunEksportXlnt = TRUE THEN DO:
            ASSIGN FI-Utforer:SCREEN-VALUE = "Xlnt"
                   FI-Status:SCREEN-VALUE = "Utlägg Xlnt. Pågår...".
            EDITOR-1:INSERT-STRING("batchXlntEksport" + CHR(10)).
            PROCESS EVENTS.
            RUN bibl_logg.p ('DataMottak', 'wbatchserver.w B-Oppdater: Starter. Starter batchXlntEksport.p.').
            RUN batchXlntEksport.p (cXlntDir).
        END.
        ASSIGN FI-Utforer:SCREEN-VALUE = ""
               FI-Status:SCREEN-VALUE = SELF:LABEL + " - OK".
        RUN bibl_logg.p ('DataMottak', 'wbatchserver.w B-Oppdater: Ferdig.').
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-OppdaterVPI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-OppdaterVPI wWin
ON CHOOSE OF B-OppdaterVPI IN FRAME fMain /* Leser og oppdater VPI */
DO:
    DEFINE VARIABLE lBehandlet AS LOGICAL    NO-UNDO.
  DO:
      RUN bibl_logg.p ('DataMottak', 'wbatchserver.w B-OppdaterVPI: Aktiver?:' + (IF lRunVPI THEN 'Ja' ELSE 'Nei')).
      /* VPI oppdatering kan skrus av. */
      IF lRunVpi THEN DO:
          ASSIGN FI-Utforer:SCREEN-VALUE = SELF:LABEL
                 FI-Status:SCREEN-VALUE = "Leser og oppdaterer VPI. Pågår...".
          /*EDITOR-1:INSERT-STRING("vpiimportpitogregister" + CHR(10)).*/
          EDITOR-1:INSERT-STRING("vpi_og_register_import" + CHR(10)).
          PROCESS EVENTS.

          IF AVAILABLE VPIArtBas THEN
              RELEASE VPIArtBas.

          /*RUN vpiimportpitogregister.w.*/
          /* Nytt program som gjør det samme, men uten adm2 rutinene. */
          RUN bibl_logg.p ('DataMottak', 'wbatchserver.w B-OppdaterVPI: Starter vpi_og_register_import.p.')).
          RUN vpi_og_register_import.p.
          IF AVAILABLE VPIArtBas THEN
              RELEASE VPIArtBas.

          IF lRunRigalut = TRUE THEN DO:
              ASSIGN FI-Utforer:SCREEN-VALUE = "Rigal"
                     FI-Status:SCREEN-VALUE = "Utlegg Rigal VPI. Pågår...".
              EDITOR-1:INSERT-STRING("batchRigalut" + CHR(10)).
              PROCESS EVENTS.
              RUN bibl_logg.p ('DataMottak', 'wbatchserver.w B-OppdaterVPI: Starter batchRigalut.p.')).
              RUN batchRigalut.p (cRigalEkstent,cRigalDir).
          END.
          IF lFinansPro = TRUE /* OR lFinansPreem = TRUE */ THEN DO:
              ASSIGN FI-Utforer:SCREEN-VALUE = "Finans"
                     FI-Status:SCREEN-VALUE = "Utlegg Finans. Pågår...".
              EDITOR-1:INSERT-STRING("batchOppdaterpro" + CHR(10)).
              PROCESS EVENTS.
              RUN bibl_logg.p ('DataMottak', 'wbatchserver.w B-OppdaterVPI: Starter batchOppdaterpro.p.')).
              RUN batchOppdaterpro.p (lFinansPro,TRUE,cFinansProDir,OUTPUT lBehandlet).
              IF lBehandlet = TRUE AND cFlyttkommando <> "" THEN
              DO:
                  RUN bibl_logg.p ('DataMottak', 'wbatchserver.w B-OppdaterVPI: Starter flyttprofiler.p.')).
                  RUN flyttprofiler.p (cFlyttDir,cFlyttKommando,"") NO-ERROR.
              END.
          END.
          IF lFinansPreem = TRUE THEN DO:
              ASSIGN FI-Utforer:SCREEN-VALUE = "Datalager"
                     FI-Status:SCREEN-VALUE = "Utlegg til datalager. Pågår...".
              EDITOR-1:INSERT-STRING("oppdater_pr_skift" + CHR(10)).
              PROCESS EVENTS.
              RUN bibl_logg.p ('DataMottak', 'wbatchserver.w B-OppdaterVPI: Starter oppdater_pr_skift.p.')).
              RUN oppdater_pr_skift.p.
          END.
      END.
      RUN bibl_logg.p ('DataMottak', 'wbatchserver.w B-OppdaterVPI: Ferdig.')).

      /* Korreksjonsposter skal alltid behandles. */
      /*
      ASSIGN FI-Utforer:SCREEN-VALUE = SELF:LABEL
             FI-Status:SCREEN-VALUE = "Leser korrigering VPI. Pågår...".
      EDITOR-1:INSERT-STRING("vpikorreksjon" + CHR(10)).
      PROCESS EVENTS.
      RUN vpikorreksjon.w.
      */

      ASSIGN FI-Utforer:SCREEN-VALUE = ""
             FI-Status:SCREEN-VALUE = SELF:LABEL + " - OK".

  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Overfor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Overfor wWin
ON CHOOSE OF B-Overfor IN FRAME fMain /* Overfør datasett */
DO:
    IF VALID-HANDLE(h_wfiler) THEN DO:
        ASSIGN FI-Utforer:SCREEN-VALUE = SELF:LABEL
               FI-Status:SCREEN-VALUE = "Overfører datasett. Pågår...".
        EDITOR-1:INSERT-STRING("OverforFilBatch" + CHR(10)).
        PROCESS EVENTS.
        RUN bibl_logg.p ('DataMottak', 'wbatchserver.w B-OVerfor: Starter. Starter OverforFilBatch.').
        RUN OverforFilBatch IN h_wfiler.
        ASSIGN FI-Utforer:SCREEN-VALUE = ""
               FI-Status:SCREEN-VALUE = SELF:LABEL + " - OK".
        RUN bibl_logg.p ('DataMottak', 'wbatchserver.w B-OVerfor: Ferdig.').
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-StartStop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-StartStop wWin
ON CHOOSE OF B-StartStop IN FRAME fMain /* Start/Stopp server */
DO:
    RUN StartStopServer(NOT chPSTimer:ENABLED).
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN FI-Oppstart:SCREEN-VALUE  = FI-Oppstart
               FI-Intervall:SCREEN-VALUE = STRING(FI-Intervall)
               FI-FirstTid:SCREEN-VALUE  = FI-FirstTid
               FI-LastTid:SCREEN-VALUE   = FI-LastTid
               .
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IMAGE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-1 wWin
ON MOUSE-SELECT-DBLCLICK OF IMAGE-1 IN FRAME fMain
DO:
    ASSIGN
     {&WINDOW-NAME}:WIDTH-PIXELS  = IF SELF:PRIVATE-DATA = "MINI" THEN iWPix ELSE 160
     {&WINDOW-NAME}:HEIGHT-PIXELS = IF SELF:PRIVATE-DATA = "MINI" THEN iHPix ELSE  72
     SELF:PRIVATE-DATA = STRING(SELF:PRIVATE-DATA = "MINI","STOR/MINI").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PSKlocka
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PSKlocka wWin OCX.Tick
PROCEDURE PSKlocka.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

    IF chPSKlocka:interval <> 60000 THEN
        chPSKlocka:interval = 60000.

    ASSIGN FI-DatoTid:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 
                 STRING(TODAY,"99/99/99") + " - " + STRING(TIME,"HH:MM").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PSTimer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PSTimer wWin OCX.Tick
PROCEDURE PSTimer.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    /* Bug hider for å bli kvitt record lock */
    FIND FIRST VPIArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE VPIArtBas THEN
        RELEASE VPIArtBas.

    ASSIGN chPSTimer:ENABLED = FALSE.
    IMAGE-1:LOAD-IMAGE(".\icon\yellowlight.jpg") NO-ERROR.
    IF lIconExist THEN
        {&WINDOW-NAME}:LOAD-ICON(cYellowIcon) NO-ERROR.
    EDITOR-1:SCREEN-VALUE = "".
    ASSIGN INPUT EDITOR-1.

    RUN bibl_logg.p ('DataMottak', 'wbatchserver.w (OCX.Tick): Starter. Starter ny innlesning.').

    /* Innlesning av filer fra filkatalog. */
    RUN bibl_logg.p ('DataMottak', 'wbatchserver.w: Starter. Starter CHOOSE B-LesInn.').
    APPLY "CHOOSE" TO B-LesInn.

    ASSIGN SESSION:NUMERIC-FORMAT = pcSession.

    /* Oppdaterer innleste filer. */
    RUN bibl_logg.p ('DataMottak', 'wbatchserver.w: Starter. Starter CHOOSE B-Oppdater.').
    APPLY "CHOOSE" TO B-Oppdater. /* Här körs ev utlägg Xlnt */

    /* Overfør utpakkede filer. */
    IF lRunOverfor = TRUE THEN
    DO:
        RUN bibl_logg.p ('DataMottak', 'wbatchserver.w: Starter. Starter CHOOSE B-Overfor.').
        APPLY "CHOOSE" TO B-Overfor.
    END.

    /* Behandler VPI meldinger */
    RUN bibl_logg.p ('DataMottak', 'wbatchserver.w: Starter. Starter CHOOSE B-OppdaterVPI.').
    APPLY "CHOOSE" TO B-OppdaterVPI. /* Här körs ev utlägg Rigal */

    /* Jobber som kjøres en gang pr. døgn. */
    IF dHTkjort = ? AND lHTutlegg = TRUE AND TIME > iHTutleggTid THEN
     DO:
        RUN bibl_logg.p ('DataMottak', 'wbatchserver.w: Starter. Starter UtleggHTFil.').
        RUN UtleggHTfil.
     END.
    IF dHTKjort <> ? AND TODAY > dHTKjort THEN
        dHTKjort = ?.

    /*RUN oppdaterUtvidetsok.*/

    RUN bibl_logg.p ('DataMottak', 'wbatchserver.w: Starter. Starter SlettSanerteArtikler.').
    RUN SlettSanerteArtikler.

    /*RUN bibl_logg.p ('DataMottak', 'wbatchserver.w: Starter. Starter SlettEloggKorrVPI.').*/
    /*RUN SlettEloggKorrVPI.*/

    RUN bibl_logg.p ('DataMottak', 'wbatchserver.w: Starter. Starter pakkUtZipFiler.').
    RUN pakkUtZipFiler.

    ASSIGN FI-Status:SCREEN-VALUE = FI-NesteTid:SCREEN-VALUE + " - OK".
    RUN bibl_logg.p ('DataMottak', 'wbatchserver.w: Starter. Starter NesteTick.').
    RUN NesteTick.
    ASSIGN chPSTimer:ENABLED = TRUE.
    IMAGE-1:LOAD-IMAGE(".\icon\greenlight.jpg") NO-ERROR.
    IF lIconExist THEN
        {&WINDOW-NAME}:LOAD-ICON(cGreenIcon) NO-ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
IF socketconnect() = TRUE THEN DO:
  QUIT.
END.

{src/adm2/windowmn.i}
FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
{lng.i &SDO="SDO"}
/* IF AVAIL Bruker THEN DO:                                           */
/*     wCurrLng = Bruker.Lng.                                         */
/* END.                                                               */
/* RUN SwitchLng.                                                     */
/* DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW). */

/* Setter loggfilnavn */
settLoggFil(OUTPUT cLoggFil).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'prg/wfiler.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_wfiler ).
       /* Position in AB:  ( 1.24 , 93.40 ) */
       /* Size in AB:  ( 1.86 , 10.80 ) */

       /* Adjust the tab order of the smart objects. */
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ApplHjelp wWin 
PROCEDURE ApplHjelp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN Hjelp ("","").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButtonEnaDis wWin 
PROCEDURE ButtonEnaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER lDisable AS LOGICAL    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN B-DataMottak:SENSITIVE  = lDisable
             B-Datasett:SENSITIVE    = lDisable
             B-LesInn:SENSITIVE      = lDisable
             B-Oppdater:SENSITIVE    = lDisable
             B-Overfor:SENSITIVE     = IF lDisable = TRUE AND
                             lRunOverfor = FALSE THEN lRunOverfor ELSE lDisable
             B-OppdaterVPI:SENSITIVE = IF lDisable = TRUE AND
                             lRunVpi = FALSE THEN lRunVpi ELSE lDisable
             .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConnectHandler wWin 
PROCEDURE ConnectHandler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ip_hSocket AS HANDLE NO-UNDO. /* handle to the client socket that just connected */
 ip_hSocket:SET-READ-RESPONSE-PROCEDURE("ReadMessage":U,THIS-PROCEDURE).
RETURN. /* all done */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load wWin  _CONTROL-LOAD
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

OCXFile = SEARCH( "wbatchserver.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chPSKlocka = PSKlocka:COM-HANDLE
    UIB_S = chPSKlocka:LoadControls( OCXFile, "PSKlocka":U)
    chPSTimer = PSTimer:COM-HANDLE
    UIB_S = chPSTimer:LoadControls( OCXFile, "PSTimer":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "wbatchserver.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject wWin 
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  IF VALID-HANDLE(chPSKlocka) THEN
      RELEASE OBJECT chPSKlocka NO-ERROR.
  IF VALID-HANDLE(PSKlocka) THEN
      DELETE OBJECT PSKlocka NO-ERROR.
  IF VALID-HANDLE(chPSTimer) THEN
      RELEASE OBJECT chPSTimer   NO-ERROR.
  IF VALID-HANDLE(PSTimer) THEN
      DELETE OBJECT PSTimer   NO-ERROR.
  ASSIGN chPSKlocka = ?
         chPSTimer  = ?.
  IF  PROGRAM-NAME(1) + "," + 
      PROGRAM-NAME(2) + "," + 
      PROGRAM-NAME(3) + "," + 
      PROGRAM-NAME(4) + "," + 
      PROGRAM-NAME(5) + "," + 
      PROGRAM-NAME(6) MATCHES "*w-modul.w*" THEN . /* Gjør ingenting */
  ELSE
      QUIT.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY TG-Profitbase TG-Overfor TG-Xlent FI-DatoTid TG-Rigal TG-FinansPro 
          TG-FinansPreem FI-Oppstart FI-Utforer TG-Vpi FI-Intervall FILL-IN-1 
          EDITOR-1 FI-FirstTid FILL-IN-2 FI-LastTid FI-Status FI-NesteTid 
          FI-NesteDato FI-StartTid FI-StartDato FI-ServerparamTxt FI-UtforTxt 
          FI-ServerstatTxt 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE IMAGE-1 B-StartStop RECT-51 RECT-52 RECT-53 RECT-54 RECT-55 B-LesInn 
         B-Oppdater B-Overfor B-OppdaterVPI FI-ServerparamTxt FI-UtforTxt 
         FI-ServerstatTxt 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      IF NOT IMAGE-1:IMAGE MATCHES "*redlight.jpg*" THEN
          RETURN.
  END.
  hServer:DISABLE-CONNECTIONS() NO-ERROR.
  DELETE OBJECT hServer NO-ERROR.

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls wWin 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN chPSTimer = chPSTimer:PSTimer
           chPSTimer:ENABLED  = FALSE
           chPSKlocka = chPSKlocka:PSTimer
           chPSKlocka:ENABLED  = FALSE.
    
    RUN InitServerParam.
    
/*                                                                                         */
/*                                                                                         */
/*         chPSTimer:ENABLED  = lEnaOnStart                                                */
/*            chPSTimer:Interval = IF lEnaOnStart = TRUE THEN 60 ELSE iIntervall           */
/*            FI-NesteTid = IF lEnaOnStart = FALSE THEN                                    */
/*                "Not running" ELSE  IF TIME < iFirstTick THEN STRING(iFirstTick,"HH:MM") */
/*                    ELSE STRING(iFirstTick + iInterVall,"HH:MM").                        */
                                      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iTid  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTick AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cTmpTxt AS CHARACTER  NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
/*   IF socketconnect() = TRUE THEN DO:                    */
/*       MESSAGE "Programmet kjører allerede (kontroller)" */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.            */
/*       APPLY "CLOSE" TO THIS-PROCEDURE.                  */
/*       RETURN.                                           */
/*   END.                                                  */
  ASSIGN pcSession = SESSION:NUMERIC-FORMAT.

  IF SEARCH(cRedIcon) <> ? AND SEARCH(cGreenIcon) <> ? AND SEARCH(cYellowIcon) <> ? THEN
      lIconExist = TRUE.
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  startup().
  SUBSCRIBE "SkrivTilDataMottaksLogg"  ANYWHERE.
  SUBSCRIBE "loggEditorBatchServerInn" ANYWHERE.

  ASSIGN iWPix = {&WINDOW-NAME}:WIDTH-PIXELS 
         iHPix = {&WINDOW-NAME}:HEIGHT-PIXELS.

  {syspara.i 50 200 1 cTekst}
  IF CAN-DO("1,yes,true,ja",cTekst) THEN
      bPBR = TRUE.
  ELSE
      bPBR = FALSE.
  /* Kontroll av om överföring av datasett skall köras */
  ASSIGN cTekst = "".
  {syspara.i 200 2 5 cTekst}
  IF cTekst = "" OR CAN-DO("1,yes,true,ja",cTekst) THEN
      lRunOverfor = TRUE.
  ELSE
      lRunOverfor = FALSE.
  /* Kontroll av om oppdatering av VPI skal kjøres. */
  ASSIGN cTekst = "".
  {syspara.i 200 2 6 cTekst}
  IF cTekst = "" OR CAN-DO("1,yes,true,ja",cTekst) THEN
      lRunVpi = TRUE.
  ELSE
      lRunVpi = FALSE.

  /* Kontroll av om Export till Xlnt skall köras */
  ASSIGN cTekst = "".
  {syspara.i 1 6 1 cTekst}
  IF CAN-DO("1,yes,true,ja",cTekst) THEN
      lRunEksportXlnt = TRUE.
  ELSE
      lRunEksportXlnt = FALSE.
  IF lRunEksportXlnt = TRUE THEN
      {syspar2.i 1 6 1 cXlntDir}
  /* Kontroll av om Export till Rigal skall köras */
  ASSIGN cTekst = "".
  {syspara.i 1 7 1 cTekst}
  IF CAN-DO("1,yes,true,ja",cTekst) THEN
      lRunRigalut = TRUE.
  ELSE
      lRunRigalut = FALSE.
  IF lRunRigalut = TRUE THEN DO:
      {syspar2.i 1 7 1 cRigalDir}
      {syspara.i 1 7 2 cRigalEkstent}
  END.
  /* Kontroll av om Export till Pro skall köras */
  ASSIGN cTekst = "".
  {syspara.i 200 2 100 cTekst}
  IF CAN-DO("1,yes,true,ja",cTekst) THEN
      lFinansPro = TRUE.
  ELSE
      lFinansPro = FALSE.
  IF lFinansPro = TRUE THEN DO:
      {syspar2.i 200 2 100 cFinansProDir}
      /* Skall flyttning av filer ske */
      ASSIGN cTekst = "".
      {syspara.i 200 2 102 cTekst}
      IF CAN-DO("1,yes,true,ja",cTekst) THEN DO:
          /* Hämta kommando */
          {syspar2.i 200 2 102 cTmpTxt}
          IF NUM-ENTRIES(cTmpTxt,";") = 2 THEN DO:
              ASSIGN cFlyttDir      = ENTRY(1,cTmpTxt,";")
                     cFlyttKommando = ENTRY(2,cTmpTxt,";").
              IF SEARCH(cFlyttKommando) = ? THEN
                  ASSIGN cFlyttDir      = ""
                         cFlyttKommando = "".
          END.
      END.
  END.
  /* Kontroll av om Oppdater Preem skall köras */
  ASSIGN cTekst = "".
  {syspara.i 200 2 101 cTekst}
  IF CAN-DO("1,yes,true,ja",cTekst) THEN
      lFinansPreem = TRUE.
  ELSE
      lFinansPreem = FALSE.
  RUN SwitchLng.

  RUN NoneVisible IN h_wfiler.

  DO WITH FRAME {&FRAME-NAME}:
      IF lEnaOnStart = TRUE THEN
          RUN StartStopServer (TRUE).
      ELSE DO:
          ASSIGN FI-Oppstart:BGCOLOR  = iEnaOnStartBG
                 FI-Intervall:BGCOLOR = iIntervallBG 
                 FI-FirstTid:BGCOLOR  = iFirstTickBG 
                 FI-LastTid:BGCOLOR   = iLastTickBG
                 FI-NesteTid:BGCOLOR  = iNesteTidBG.
      END.
      ASSIGN  iTid = TIME
              iTick = 1000 * (INT(SUBSTR(STRING(iTid,"HH:MM"),1,2)) * 3600 + 
                             INT(SUBSTR(STRING(iTid,"HH:MM"),4)) * 60 + 60 - iTid)
              chPSKlocka:interval = iTick
              chPSKlocka:ENABLED = TRUE
              FI-DatoTid:SCREEN-VALUE = STRING(TODAY,"99/99/99") + " - " +
                                        STRING(iTid,"HH:MM").
  END.
  ASSIGN 
      TG-Overfor:CHECKED     = lRunOverfor
      TG-Profitbase:CHECKED  = bPBR
      TG-Rigal:CHECKED       = lRunRigalut
      TG-Xlent:CHECKED       = lRunEksportXlnt
      TG-FinansPro:CHECKED   = lFinansPro
      TG-FinansPro:TOOLTIP   = cFinansProDir
      TG-FinansPreem:CHECKED = lFinansPreem
      TG-Vpi:CHECKED         = lRunVpi
      .
  IF B-Overfor:SENSITIVE AND lRunOverfor = FALSE THEN
      ASSIGN B-Overfor:SENSITIVE = FALSE.
  IF B-OppdaterVPI:SENSITIVE AND lRunVpi = FALSE THEN
      ASSIGN B-OppdaterVPI:SENSITIVE = FALSE.
  IF lIconExist THEN
      {&WINDOW-NAME}:LOAD-ICON(cRedIcon) NO-ERROR.
  IF lEnaOnStart = FALSE AND 
      (CAN-DO(SESSION:PARAMETER,"AUTOSTART") OR
       CAN-DO(SESSION:PARAMETER,"BATCH")) THEN
    APPLY "CHOOSE" TO B-StartStop IN FRAME fMain.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitServerParam wWin 
PROCEDURE InitServerParam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iTest  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTest2 AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iIntInlest AS INTEGER    NO-UNDO.
  /* här läser vi in sytemparametrar */
  DO WITH FRAME {&FRAME-NAME}:
    {syspara.i 200 2 1 cTekst}
    ASSIGN cTekst = TRIM(cTekst)
           lEnaOnStart = cTekst = "1"
           FI-Oppstart = cTekst
           iEnaOnStartBG = IF cTekst = "0" OR cTekst = "1" THEN 15 ELSE 12.
    {syspara.i 200 2 2 cTekst}
    ASSIGN cTekst      = TRIM(cTekst)
           iIntervall  = INT(cTekst) NO-ERROR.
    IF ERROR-STATUS:ERROR OR iIntervall > 86399 OR iIntervall < 5 THEN DO:
        ASSIGN iIntervallBG = 12
               iIntervall   = 5
               iIntInlest   = IF cTekst = "" OR cTekst = "0" THEN 0 ELSE 5.
               /* OM iIntInlest = 0 används den om förste tid = siste tid eller
                                  bara en tid angivits, se nedan */
    END.
    ELSE DO:
        ASSIGN iIntervallBG = 15
               iIntInlest   = iIntervall.
    END.
    ASSIGN FI-Intervall = iIntervall.
        
    {syspara.i 200 2 4 cTekst}
    ASSIGN cTekst = TRIM(cTekst).
    IF cTekst = "" OR NUM-ENTRIES(cTekst) > 2 THEN DO:
        ASSIGN iFirstTick = 0
               iLastTick  = 86399
               iFirstTickBG = 12
               FI-FirstTid = STRING(iFirstTick,"HH:MM")
               iLastTickBG = 12
               FI-LastTid = STRING(iLastTick,"HH:MM").
    END.
    ELSE IF NUM-ENTRIES(cTekst) = 1 THEN DO:
        ASSIGN iTest = INT(cTekst) NO-ERROR.
        IF ERROR-STATUS:ERROR OR iTest > 86399 OR cTekst = "" THEN DO:
            ASSIGN iFirstTick = 0
                   iLastTick  = 86399
                   iFirstTickBG = 12
                   FI-FirstTid = STRING(iFirstTick,"HH:MM")
                   iLastTickBG = 12
                   FI-LastTid = STRING(iLastTick,"HH:MM").
        END.
        ELSE DO:
            ASSIGN iFirstTick   = iTest
                   iLastTick    = iTest
                   FI-FirstTid  = STRING(iFirstTick,"HH:MM")
                   FI-LastTid   = STRING(iLastTick,"HH:MM")
                   iFirstTickBG = 15
                   iLastTickBG  = 15
                   iIntervall   = IF iIntInlest = 0 THEN iIntInlest ELSE iIntervall
                   iIntervallBG = IF iIntervall = 0 THEN 15 ELSE 12.
        END.
    END.
    ELSE IF NUM-ENTRIES(cTekst) = 2 THEN DO:
        ASSIGN iTest  = INT(ENTRY(1,cTekst))
               iTest2 = INT(ENTRY(2,cTekst)) NO-ERROR.
        IF ERROR-STATUS:ERROR OR iTest > 86399 
                              OR iTest2 > 86399 OR iTest > iTest2 THEN DO:
            ASSIGN iFirstTick = 0
                   iLastTick  = 86399
                   iFirstTickBG = 12
                   FI-FirstTid = STRING(iFirstTick,"HH:MM")
                   iLastTickBG = 12
                   FI-LastTid = STRING(iLastTick,"HH:MM").
        END.
        ELSE DO:
            ASSIGN iFirstTick = iTest
                   iLastTick  = iTest2
                   iFirstTickBG = IF iLastTick = iFirstTick OR iLastTick - iFirstTick > iIntervall THEN 15 ELSE 12
                   FI-FirstTid = STRING(iFirstTick,"HH:MM")
                   iLastTickBG = IF iLastTick = iFirstTick OR iLastTick - iFirstTick > iIntervall THEN 15 ELSE 12
                   FI-LastTid = STRING(iLastTick,"HH:MM")
                   iIntervall = IF iFirstTick = iLastTick AND iIntInlest = 0 THEN iIntInlest ELSE iIntervall
                   iIntervallBG = IF iFirstTick = iLastTick AND iIntervall > 0 THEN 12 ELSE 
                                  IF iFirstTick = iLastTick AND iIntervall = 0 THEN 15 ELSE iIntervallBG
                   FI-Intervall = iIntervall.    

        END.
    END.
    {syspara.i 200 6 1 cTekst}
    IF cTekst = "1" THEN DO:
        lHTutlegg = TRUE.
        {syspar2.i 200 6 1 cTekst}
        iHTutleggTid = INT(cTekst) NO-ERROR.
        IF ERROR-STATUS:ERROR OR iHtUtleggTid = 0 THEN
            lHTutlegg = FALSE.
    END.
    ASSIGN chPSTimer:Interval = iIntervall
           iNesteTidBG = IF FI-NesteTid = "" AND NOT lEnaOnStart THEN 12 ELSE ?.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loggEditorBatchServerInn wWin 
PROCEDURE loggEditorBatchServerInn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER cMsg AS CHAR NO-UNDO.

  DO WITH FRAME fMain:
      IF cMsg <> "" THEN
          EDITOR-1:INSERT-STRING(cMsg + CHR(10)).
  END.

  PUBLISH "SkrivTilDataMottaksLogg" (";Editor:;" + cMsg).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NesteTick wWin 
PROCEDURE NesteTick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iSek AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTid AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTick AS INTEGER    NO-UNDO.

  /* iIntervall = 10 sec normalt */

  DO WITH FRAME {&FRAME-NAME}:
      
      ASSIGN iTid = TIME.

      IF iTid < iFirstTick THEN
          ASSIGN iSek = iFirstTick - iTid
                 chPSTimer:interval = iSek * 1000
                 FI-NesteTid = STRING(iFirstTick,"HH:MM:SS")
                 FI-NesteDato = STRING(TODAY,"99/99/99").

      ELSE IF iTid > iLastTick OR iTid + iIntervall > iLastTick THEN 
      DO:
          ASSIGN iSek = 86400 - iTid + iFirstTick
                 chPSTimer:interval = iSek * 1000
                 FI-NesteTid = STRING(iFirstTick,"HH:MM:SS")
                 FI-NesteDato = STRING(TODAY + 1,"99/99/99").
      END.
      
      ELSE DO:
/*           ASSIGN iTick = 1000 * (INT(SUBSTR(STRING(iTid,"HH:MM"),1,2)) * 3600 +  */
/*                    INT(SUBSTR(STRING(iTid,"HH:MM"),4)) * 60 + iIntervall - iTid) */
/*                  chPSTimer:interval = iTick                                      */
/*                  iTid = iTid + iTick / 1000                                      */
          ASSIGN iTick = iIntervall * 1000
                 chPSTimer:interval = iTick
                 iTid = iTid + iTick / 1000
                 FI-NesteTid = STRING(iTid,"HH:MM:SS")
                 FI-NesteDato = STRING(TODAY,"99/99/99").
      END.
      
      ASSIGN FI-NesteTid:SCREEN-VALUE = FI-NesteTid
             FI-NesteDato:SCREEN-VALUE = FI-NesteDato.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdaterUtvidetsok wWin 
PROCEDURE oppdaterUtvidetsok :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR piLoop AS INT NO-UNDO.

BLOKK:
DO WITH FRAME fMain:
  LOOPEN:
  FOR EACH VPIArtBas EXCLUSIVE-LOCK WHERE
    VPIArtBas.Utvidetsok = '':
    EDITOR-1:INSERT-STRING("Initierer utvidetsøk." + CHR(10)).
    /*{w_artbas.i &vpi="vpi"}*/
    RUN init_utvidetsok_vpiartbas.p (VPIArtBAs.ArtikkelNr).
    piLoop = piLoop + 1.
    IF piLoop >= 100 THEN
        LEAVE BLOKK.
  END. /* LOOPEN */
END. /* BLOKK */
IF AVAILABLE VPIArtBas THEN
    RELEASE VPIArtBas.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettKnapper wWin 
PROCEDURE OpprettKnapper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*    DEFINE VARIABLE hFrame  AS HANDLE     NO-UNDO.                                 */
/*    DEFINE VARIABLE hHandle AS HANDLE     NO-UNDO.                                 */
/*    DEFINE VARIABLE hButton AS HANDLE     NO-UNDO.                                 */
/*    DEFINE VARIABLE iPos    AS INTEGER    NO-UNDO.                                 */
/*    DEFINE VARIABLE piX     AS INTEGER    NO-UNDO.                                 */
/*    &SCOP dlmt + CHR(1) +                                                          */
/*                                                                                   */
/*    ASSIGN hFrame  = DYNAMIC-FUNCTION('getContainerHandle':U IN h_dyntoolbar)           */
/*           hHandle = hFrame                                                        */
/*           hHandle = hHandle:FIRST-CHILD  /* första field-group          */        */
/*           hHandle = hHandle:FIRST-CHILD. /* första widget i field-group */        */
/*    REPEAT WHILE VALID-HANDLE(hHandle):                                            */
/*        /* hämtar X-pos för sista 'rulen' i toolbaren */                           */
/*        IF hHandle:TYPE = "RECTANGLE" AND hHandle:X > piX THEN                     */
/*            ASSIGN iPos = hHandle:X + hHandle:WIDTH-PIXELS                         */
/*                   piX  = hHandle:X.                                               */
/*        ASSIGN hHandle = hHandle:NEXT-SIBLING.                                     */
/*    END.                                                                           */
/*                                                                                   */
/*    /*                                                                             */
/*    /* Printbutton */                                                              */
/*    ASSIGN  piX = piX + 4 /* lägger till 'luft' */                                 */
/*            hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,           */
/*               INPUT hFrame,                                                       */
/*               INPUT-OUTPUT piX           /* INTEGER   */,                         */
/*               INPUT "Print"              /* CHARACTER */,                         */
/*               INPUT "Print"              /* CHARACTER */,                         */
/*               INPUT "Rapport"            /* CHARACTER */,                         */
/*               INPUT "icon\e-print.bmp"   /* CHARACTER */,                         */
/*               INPUT TRUE                 /* LOGICAL   */                          */
/*               ).                                                                  */
/*                                                                                   */
/*    DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,                             */
/*       INPUT "Print":U /* CHARACTER */,                                            */
/*       INPUT "Name,Caption,Image,Link,Type,OnChoose,Parent" /* CHARACTER */,       */
/*       INPUT "Print"             {&dlmt}                                           */
/*             "Print Record"      {&dlmt}                                           */
/*             "":U                {&dlmt}                                           */
/*             "":U                {&dlmt}                                           */
/*             "PUBLISH":U         {&dlmt}                                           */
/*             "PRINT":U           {&dlmt}                                           */
/*             "Options":U).                                                         */
/*    */                                                                             */
/*                                                                                   */
/*    /* Action EXIT finns i toolbar sedan tidigare */                               */
/*    ASSIGN piX = hFrame:WIDTH-PIXELS - 26 /*hButton:WIDTH-PIXELS - 4*/             */
/*           hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,            */
/*             INPUT hFrame,                                                         */
/*             INPUT-OUTPUT piX        /* INTEGER   */,                              */
/*             INPUT "exit"            /* CHARACTER */,                              */
/*             INPUT "exit"            /* CHARACTER */,                              */
/*             INPUT "exit"            /* CHARACTER */,                              */
/*             INPUT "icon\e-exit.bmp" /* CHARACTER */,                              */
/*             INPUT TRUE              /* LOGICAL   */                               */
/*             ).                                                                    */
/*                                                                                   */
/*    ASSIGN piX = hButton:X - hButton:WIDTH-PIXELS - 1.                             */
/*           hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,            */
/*             INPUT hFrame,                                                         */
/*             INPUT-OUTPUT piX        /* INTEGER */,                                */
/*             INPUT "HELP"            /* CHARACTER */,                              */
/*             INPUT "HELP"            /* CHARACTER */,                              */
/*             INPUT "HELP"            /* CHARACTER */,                              */
/*             INPUT "icon\e-help.bmp" /* CHARACTER */,                              */
/*             INPUT TRUE              /* LOGICAL */).                               */
/*                                                                                   */
/*    DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,                             */
/*       INPUT "HELP":U /* CHARACTER */,                                             */
/*       INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */, */
/*       INPUT "Help"      {&dlmt}                                                   */
/*             "Help"      {&dlmt}                                                   */
/*             "":U        {&dlmt}                                                   */
/*             "PUBLISH":U {&dlmt}                                                   */
/*             "ApplHjelp":U   {&dlmt}                                               */
/*             "":U        {&dlmt}                                                   */
/*             "":U).                                                                */
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pakkUtZipFiler wWin 
PROCEDURE pakkUtZipFiler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
DO WITH FRAME {&FRAME-NAME}:

    EDITOR-1:INSERT-STRING("Pakker ut Patch ZIP fil" + CHR(10)).
    PROCESS EVENTS.

    RUN prg/PatchUnpack.p .  

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReadMessage wWin 
PROCEDURE ReadMessage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowStatus wWin 
PROCEDURE ShowStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcStatusMessage AS CHAR NO-UNDO. 
    DEFINE INPUT PARAMETER ipcFileName AS CHAR NO-UNDO. 
    DEFINE INPUT PARAMETER ipcAction AS CHAR NO-UNDO. 
    
    DO WITH FRAME fMain: 
        FI-Utforer:SCREEN-VALUE = ipcAction.
        FI-Status:SCREEN-VALUE = ipcStatusMessage. 
        FILL-IN-1:SCREEN-VALUE = ipcFileName. 
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivTilDataMottaksLogg wWin 
PROCEDURE SkrivTilDataMottaksLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER pcLoggTekst AS CHAR NO-UNDO.

DEF VAR pcDatoTid AS CHAR NO-UNDO.
DEF VAR piDiff    AS INT  NO-UNDO.
                          
/* Sikrer at det er et gyldig filnavn */                                 
IF cLoggFil = "" THEN
    settLoggFil(OUTPUT cLoggFil).

/* Dato og tidsstempel. */
ASSIGN
    piDiff    = IF iForrige = 0 
                  THEN 0
                  ELSE (TIME - iForrige)
    pcDatoTid = STRING(TODAY)             + cDelimiter +
                STRING(TIME,"HH:MM:SS")   + cDelimiter +
                STRING(piDiff,"HH:MM:SS")
                . 
OUTPUT STREAM Logg TO VALUE(cLoggFil) APPEND.
  PUT STREAM Logg UNFORMATTED  
      pcDatoTid + cDelimiter + 
      pcLoggTekst
      SKIP.
OUTPUT STREAM Logg CLOSE.

/* Tar vare på tidspunkt for sist gang det ble skrevet til loggen. */
ASSIGN
    iForrige = TIME
    .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettEloggKorrVPI wWin 
PROCEDURE SlettEloggKorrVPI :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

    FOR EACH ELogg WHERE 
             ELogg.TabellNavn     = "ArtBas"  AND
             ELogg.EksterntSystem = "TILKORRPOSLOGG" AND
             ELogg.Verdier        = StLinje.DataObjekt AND
             ELogg.RegistrertDato < TODAY:
        DELETE ELogg.     
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettSanerteArtikler wWin 
PROCEDURE SlettSanerteArtikler :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:
    EDITOR-1:INSERT-STRING("Slett sanerte" + CHR(10)).
    PROCESS EVENTS.
    FOR EACH ArtBas NO-LOCK WHERE
      ArtBas.SanertDato < ?:
      RUN slettartbasbatch.w (ArtBas.ArtikkelNr).
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartStopServer wWin 
PROCEDURE StartStopServer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER lStart AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE iSek   AS INTEGER    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    IF lStart = TRUE THEN DO:
        RUN InitServerParam.
        RUN NesteTick.
        ASSIGN chPSTimer:ENABLED = TRUE
               FI-NesteTid:BGCOLOR = 10
               FI-Oppstart:BGCOLOR  = iEnaOnStartBG
               FI-Intervall:BGCOLOR = iIntervallBG 
               FI-FirstTid:BGCOLOR  = iFirstTickBG 
               FI-LastTid:BGCOLOR   = iLastTickBG
               FI-Status:SCREEN-VALUE = ""
               FI-StartTid:SCREEN-VALUE = STRING(TIME,"HH:MM")
               FI-StartDato:SCREEN-VALUE = STRING(TODAY).
/*         APPLY "MOUSE-SELECT-DBLCLICK" TO IMAGE-1. */
    END.
    ELSE DO:
        ASSIGN chPSTimer:ENABLED         = FALSE
               FI-NesteTid:SCREEN-VALUE  = ""
               FI-NesteDato:SCREEN-VALUE = ""
               FI-NesteTid:BGCOLOR       = 12
               FI-Status:SCREEN-VALUE = "Server stop - " + STRING(TODAY) + " " + STRING(TIME,"HH:MM")
               FI-StartTid:SCREEN-VALUE = ""
               FI-StartDato:SCREEN-VALUE = "".
    END.
  END.
  RUN ButtonEnaDis(NOT chPSTimer:ENABLED).
  IMAGE-1:LOAD-IMAGE(IF NOT chPSTimer:ENABLED THEN ".\icon\redlight.jpg" ELSE ".\icon\greenlight.jpg").

  IF lIconExist THEN DO:
      IF NOT chPSTimer:ENABLED THEN
          {&WINDOW-NAME}:LOAD-ICON(cRedIcon) NO-ERROR.
      ELSE
          {&WINDOW-NAME}:LOAD-ICON(cGreenIcon) NO-ERROR.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtleggHTfil wWin 
PROCEDURE UtleggHTfil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    EDITOR-1:INSERT-STRING("batch_htutlegg" + CHR(10)).
    RUN batch_htutlegg.p NO-ERROR.
    dHTKjort = TODAY.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION settLoggFil wWin 
FUNCTION settLoggFil RETURNS CHARACTER
  ( OUTPUT pcLoggFil AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  ASSIGN
      pcLoggFil = "log\datamottak" + replace(STRING(TODAY),"/","-") + ".csv"
      .
  RETURN pcLoggFil.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION socketconnect wWin 
FUNCTION socketconnect RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 CREATE SOCKET hServer NO-ERROR.
/*  hServer:CONNECT("-H ken1lap3 -S 9101":U). */
 hServer:CONNECT(cConnect) NO-ERROR.
 IF hServer:CONNECTED() THEN DO:
     /* Detta är ett serverprogram */
     /* Om vi får kontakt så måste vi avsluta. Vi kan inte starta 2 instanser */
     hServer:DISCONNECT() NO-ERROR.
     DELETE OBJECT hServer NO-ERROR.
     RETURN TRUE.
 END.
 ELSE
     RETURN FALSE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION startup wWin 
FUNCTION startup RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 DEFINE VARIABLE lOkEnable AS LOGICAL    NO-UNDO.
 CREATE SERVER-SOCKET hServer. /* create the server socket and store the handle in hServer */

 /* ensure that the server can received connection requests. In this example, the server
    is using the localmachine and a service of 1234. Obviously the host and service
    can be replaced with any machine or service that you need */
 ASSIGN lOkEnable = hServer:ENABLE-CONNECTIONS(cConnect) NO-ERROR.

 IF lOkEnable THEN DO:
     /* when a client connects to this server, run the procedure "ConnectHandler" in THIS-PROCEDURE */
     hServer:SET-CONNECT-PROCEDURE("ConnectHandler":U,THIS-PROCEDURE).
 END.
 ELSE RETURN FALSE.
 RETURN TRUE. /* all's ok !*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

