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
DEFINE VARIABLE cLanButiker       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFtpButiker       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cOrgLanButiker       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cOrgFtpButiker       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lVare             AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lVarGr            AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lKasValuta        AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lKasserere        AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lSelgere          AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lKunde            AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lFarger           AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lStrKonv          AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lStrType          AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lTekster          AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lGaranti          AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lButiker          AS LOGICAL    NO-UNDO.
DEFINE VARIABLE iHPix             AS INTEGER    NO-UNDO.
DEFINE VARIABLE iWPix             AS INTEGER    NO-UNDO.
DEFINE VARIABLE cDatoTid          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lActiveTick       AS LOGICAL    NO-UNDO. /* */
DEFINE VARIABLE cFilNavn          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lTimerOff         AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lKlargjor         AS LOGICAL    NO-UNDO.

DEFINE VARIABLE lKampMixMatch     AS LOGICAL    NO-UNDO.

DEFINE VARIABLE lKasseEksport AS LOGICAL    NO-UNDO. /* Om aktiva kassor */
DEFINE VARIABLE lErpEksport   AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lOrdHK        AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cHKinst       AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cOutputDir AS CHARACTER INIT "f:\home\lindbak\sendes" NO-UNDO.

DEF VAR bKlargjorPrisko AS LOG  NO-UNDO.
DEF VAR bUtleggXmlFiler AS LOG  NO-UNDO.
DEF VAR cTekst          AS CHAR NO-UNDO.
DEF VAR bSendPris       AS LOG NO-UNDO.

/* DEFINE VARIABLE lErpFin    AS LOGICAL    NO-UNDO. */
/* DEFINE VARIABLE lErpOrd    AS LOGICAL    NO-UNDO. */
/* DEFINE VARIABLE lErpVpi    AS LOGICAL    NO-UNDO. */
DEFINE VARIABLE cFinEksportRutine AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cOrdEksportRutine AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVpiEksportRutine AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cOrdButikkEksportRutine AS CHARACTER  NO-UNDO.
DEF VAR hInstance AS INT NO-UNDO.
DEF VAR cULOGfil        AS CHAR INIT "c:\home\lindbak\kasse\ULOG" NO-UNDO.

DEFINE VARIABLE hTT_N9Butiker AS HANDLE     NO-UNDO.

DEFINE TEMP-TABLE TT_N9Butiker NO-UNDO
    FIELD ButikkNr AS INTEGER.

{windows.i}
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
&Scoped-Define ENABLED-OBJECTS IMAGE-1 CLanbutikerParam BUTTON-Kontroller ~
B-Prisko cFtpbutikerParam B-StartStop 
&Scoped-Define DISPLAYED-OBJECTS CLanbutikerParam cFtpbutikerParam ~
FI-Serverstatus 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD canfindElogg wWin 
FUNCTION canfindElogg RETURNS LOGICAL
  ( INPUT cTabell AS CHARACTER, INPUT cEksternt AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFiltxt wWin 
FUNCTION getFiltxt RETURNS CHARACTER
  ( INPUT cFilNavn AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setError wWin 
FUNCTION setError RETURNS CHARACTER
  ( INPUT fiHandle AS HANDLE,INPUT-OUTPUT lSendError AS LOGICAL,INPUT-OUTPUT cToolTip AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE PSTimer AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chPSTimer AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Ftpbut  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-Klargjor 
     LABEL "Klargjør" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Lanbut  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-Prisko 
     LABEL "Klargjør priskø.." 
     SIZE 20 BY 1.14.

DEFINE BUTTON B-StartStop  NO-FOCUS
     LABEL "Start/Stopp" 
     SIZE 14.6 BY 1.14.

DEFINE BUTTON BUTTON-Kontroller 
     LABEL "Kontroller" 
     SIZE 14.6 BY 1.14.

DEFINE VARIABLE cFtpbutikerParam AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ftpbutiker" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CLanbutikerParam AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lanbutiker" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-Serverstatus AS CHARACTER FORMAT "X(256)":U INITIAL "Det finnes data å overføre" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "icon/redlight.jpg":U
     SIZE 32 BY 3.43.

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 51 BY 8.48
     FONT 2 NO-UNDO.

DEFINE VARIABLE FI-FilTxt AS CHARACTER FORMAT "X(256)":U INITIAL " Filinfo" 
      VIEW-AS TEXT 
     SIZE 8 BY .62
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE FI-MixFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mixfiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VareFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varefiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-59
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 109 BY 19.05.

DEFINE BUTTON B-SendKampMixMatch  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendVare  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-VareAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON BUTTON-1 
     LABEL "Registertype" 
     SIZE 17.8 BY .67.

DEFINE BUTTON BUTTON-2 
     LABEL "Til overføring" 
     SIZE 17 BY .67.

DEFINE BUTTON BUTTON-3 
     LABEL "Auto" 
     SIZE 8 BY .67.

DEFINE BUTTON BUTTON-4 
     LABEL "Hele register" 
     SIZE 17 BY .67.

DEFINE BUTTON BUTTON-6 
     LABEL "Antall overførte" 
     SIZE 17.8 BY .67.

DEFINE BUTTON BUTTON-7 
     LABEL "Kl." 
     SIZE 8.4 BY .67.

DEFINE VARIABLE FI-AntKampMixMatch AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntPakker AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntVarer AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-KampMixMatchDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-KampMixMatchError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KampMixMatchTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-KampMixMatchTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Kampanjer" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-MixDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-MixError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-OverfTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Til overføring" 
      VIEW-AS TEXT 
     SIZE 13 BY .62
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE FI-PakkeMixTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Pakker/Mix" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-TilExpInfo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rød = Poster til overføring" 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 12  NO-UNDO.

DEFINE VARIABLE FI-VareDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-VareError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VarerTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Varer" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-VareTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 109 BY 11.67.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE 101.2 BY 5.76.

DEFINE RECTANGLE RECT-58
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE .4 BY 5.76.

DEFINE VARIABLE TG-KampMixMatchAuto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Skjul AS LOGICAL INITIAL yes 
     LABEL "Skjul send dialog" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE TG-VareAuto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B-Ftpbut AT ROW 2.81 COL 81 NO-TAB-STOP 
     CLanbutikerParam AT ROW 1.57 COL 64 COLON-ALIGNED
     BUTTON-Kontroller AT ROW 2.67 COL 35.6
     B-Prisko AT ROW 2.67 COL 96
     cFtpbutikerParam AT ROW 2.81 COL 64 COLON-ALIGNED
     B-Klargjor AT ROW 4 COL 65.6
     FI-Serverstatus AT ROW 4.43 COL 1 NO-LABEL
     B-Lanbut AT ROW 1.57 COL 81 NO-TAB-STOP 
     B-StartStop AT ROW 1.48 COL 35.6
     IMAGE-1 AT ROW 1.05 COL 1
     SPACE(84.00) SKIP(22.95)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         DEFAULT-BUTTON B-StartStop.

DEFINE FRAME FRAME-Filinfo
     FI-VareFiler AT ROW 2.33 COL 70.4 COLON-ALIGNED
     EDITOR-1 AT ROW 2.52 COL 3.4 NO-LABEL
     FI-MixFiler AT ROW 3.43 COL 70.4 COLON-ALIGNED
     FI-FilTxt AT ROW 1 COL 37 COLON-ALIGNED NO-LABEL
     RECT-59 AT ROW 2.19 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.8 ROW 7.14
         SIZE 115.2 BY 20.29.

DEFINE FRAME FRAME-Para
     B-SendKampMixMatch AT ROW 5.67 COL 29 NO-TAB-STOP 
     BUTTON-1 AT ROW 2.05 COL 4.4 NO-TAB-STOP 
     BUTTON-2 AT ROW 2.05 COL 23 NO-TAB-STOP 
     BUTTON-3 AT ROW 2.05 COL 40.2
     BUTTON-4 AT ROW 2.05 COL 48.2
     BUTTON-6 AT ROW 2.05 COL 65.2 NO-TAB-STOP 
     BUTTON-7 AT ROW 2.05 COL 83 NO-TAB-STOP 
     TG-VareAuto AT ROW 3.19 COL 42.4
     B-VareAlle AT ROW 3.19 COL 51.6
     FI-AntVarer AT ROW 3.19 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-VareDatoTid AT ROW 3.19 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-VareError AT ROW 3.19 COL 91.2 COLON-ALIGNED NO-LABEL
     FI-AntPakker AT ROW 4.43 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-MixDatoTid AT ROW 4.43 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-MixError AT ROW 4.43 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-KampMixMatchAuto AT ROW 5.67 COL 42.4
     FI-AntKampMixMatch AT ROW 5.67 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-KampMixMatchDatoTid AT ROW 5.67 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-KampMixMatchError AT ROW 5.67 COL 91.2 COLON-ALIGNED NO-LABEL
     B-SendVare AT ROW 3.19 COL 29 NO-TAB-STOP 
     TG-Skjul AT ROW 10.14 COL 42.4
     FI-OverfTxt AT ROW 1 COL 37 COLON-ALIGNED NO-LABEL
     FI-VarerTxt AT ROW 3.19 COL 3 COLON-ALIGNED NO-LABEL
     FI-VareTilExp AT ROW 3.19 COL 24 NO-LABEL
     FI-PakkeMixTxt AT ROW 4.43 COL 3 COLON-ALIGNED NO-LABEL
     FI-KampMixMatchTxt AT ROW 5.67 COL 3 COLON-ALIGNED NO-LABEL
     FI-KampMixMatchTilExp AT ROW 5.67 COL 24 NO-LABEL
     FI-TilExpInfo AT ROW 10.14 COL 4.2
     RECT-56 AT ROW 1.24 COL 1
     RECT-57 AT ROW 1.91 COL 3.8
     RECT-58 AT ROW 1.86 COL 22.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.8 ROW 7.14
         SIZE 115.2 BY 20.29.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Dataeksport til kasser/ERP"
         HEIGHT             = 26.38
         WIDTH              = 116.6
         MAX-HEIGHT         = 27.95
         MAX-WIDTH          = 185.8
         VIRTUAL-HEIGHT     = 27.95
         VIRTUAL-WIDTH      = 185.8
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-Filinfo:FRAME = FRAME fMain:HANDLE
       FRAME FRAME-Para:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME fMain
   FRAME-NAME Size-to-Fit                                               */
ASSIGN 
       FRAME fMain:SCROLLABLE       = FALSE.

/* SETTINGS FOR BUTTON B-Ftpbut IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       B-Ftpbut:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR BUTTON B-Klargjor IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       B-Klargjor:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR BUTTON B-Lanbut IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       B-Lanbut:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       cFtpbutikerParam:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       CLanbutikerParam:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN FI-Serverstatus IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FI-Serverstatus:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR FRAME FRAME-Filinfo
                                                                        */
/* SETTINGS FOR FRAME FRAME-Para
                                                                        */
/* SETTINGS FOR BUTTON B-SendKampMixMatch IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendVare IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntKampMixMatch IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntPakker IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntVarer IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KampMixMatchDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KampMixMatchError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KampMixMatchTilExp IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-MixDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-MixError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TilExpInfo IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-VareDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VareError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VareTilExp IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME PSTimer ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 3.86
       COLUMN          = 51
       HEIGHT          = 1.91
       WIDTH           = 8
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      PSTimer:NAME = "PSTimer":U .
/* PSTimer OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      PSTimer:MOVE-AFTER(cFtpbutikerParam:HANDLE IN FRAME fMain).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Dataeksport til kasser/ERP */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Dataeksport til kasser/ERP */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  IF chPSTimer:ENABLED = TRUE THEN DO:
      MESSAGE "Du må stoppe serveren for å kunne stenge vinduet."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Ftpbut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Ftpbut wWin
ON CHOOSE OF B-Ftpbut IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iReturnValue AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cValgteFTP   AS CHARACTER  NO-UNDO.
    DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
        cValgteFTP = cValgteFTP + (IF cValgteFTP <> "" THEN "," ELSE "") + STRING(iCount).
    END.
    RUN JBoxDynFieldChoose.w (THIS-PROCEDURE:CURRENT-WINDOW,?,cOrgFtpButiker,"ROW",INPUT-OUTPUT cValgteFTP,OUTPUT iReturnValue).
    IF iReturnValue = 1 THEN
    ASSIGN cFtpButiker = cOrgFtpButiker.
    ELSE DO:
        ASSIGN cFtpButiker = "".
        DO iCount = 1 TO NUM-ENTRIES(cOrgFtpButiker):
            IF CAN-DO(cValgteFtp,STRING(iCount)) THEN
                cFtpButiker = cFtpButiker + (IF cFtpButiker <> "" THEN "," ELSE "") + ENTRY(iCount,cOrgFtpButiker).
        END.
    END.
    ASSIGN cFtpButikerParam:SCREEN-VALUE = cFtpButiker.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Klargjor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Klargjor wWin
ON CHOOSE OF B-Klargjor IN FRAME fMain /* Klargjør */
DO:
    IF cLanButiker = "" AND cFtpButiker = "" THEN DO:
        MESSAGE
               "Ingen butikker er valgt"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    DO WITH FRAME FRAME-Para:
        APPLY "CHOOSE" TO B-VareAlle.
    END.
    ASSIGN lVare      = TRUE
            .
    RUN KontrollerElogg.
    RUN StartEksport.
    FOR EACH ELogg WHERE ELogg.Verdier = "KLARGJOR":
        DELETE ELogg.
    END.
    RUN KlargjorModus (FALSE).
    RUN KontrollerElogg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Lanbut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Lanbut wWin
ON CHOOSE OF B-Lanbut IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iReturnValue AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cValgteLan   AS CHARACTER  NO-UNDO.
    DO iCount = 1 TO NUM-ENTRIES(cLanButiker):
        cValgteLan = cValgteLan + (IF cValgteLan <> "" THEN "," ELSE "") + STRING(iCount).
    END.
    RUN JBoxDynFieldChoose.w (THIS-PROCEDURE:CURRENT-WINDOW,?,cOrgLanButiker,"ROW",INPUT-OUTPUT cValgteLan,OUTPUT iReturnValue).
    IF iReturnValue = 1 THEN
    ASSIGN cLanButiker = cOrgLanButiker.
    ELSE DO:
        ASSIGN cLanButiker = "".
        DO iCount = 1 TO NUM-ENTRIES(cOrgLanButiker):
            IF CAN-DO(cValgteLan,STRING(iCount)) THEN
                cLanButiker = cLanButiker + (IF cLanButiker <> "" THEN "," ELSE "") + ENTRY(iCount,cOrgLanButiker).
        END.
    END.
    ASSIGN cLanButikerParam:SCREEN-VALUE = cLanButiker.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Prisko
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Prisko wWin
ON CHOOSE OF B-Prisko IN FRAME fMain /* Klargjør priskø.. */
DO:
    RUN KontrollerPrisko IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Para
&Scoped-define SELF-NAME B-SendKampMixMatch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendKampMixMatch wWin
ON CHOOSE OF B-SendKampMixMatch IN FRAME FRAME-Para /* Send */
DO:
  ASSIGN lKampMixMatch = TRUE.
  RUN StartEksport IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SendVare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendVare wWin
ON CHOOSE OF B-SendVare IN FRAME FRAME-Para /* Send */
DO:
  ASSIGN lVare = TRUE.
  RUN StartEksport IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME B-StartStop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-StartStop wWin
ON CHOOSE OF B-StartStop IN FRAME fMain /* Start/Stopp */
DO:
    RUN StartStopServer(NOT chPSTimer:ENABLED).
/*     DO WITH FRAME {&FRAME-NAME}:                                */
/*         ASSIGN FI-Oppstart:SCREEN-VALUE  = FI-Oppstart          */
/*                FI-Intervall:SCREEN-VALUE = STRING(FI-Intervall) */
/*                FI-FirstTid:SCREEN-VALUE  = FI-FirstTid          */
/*                FI-LastTid:SCREEN-VALUE   = FI-LastTid           */
/*                .                                                */
/*     END.                                                        */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Para
&Scoped-define SELF-NAME B-VareAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VareAlle wWin
ON CHOOSE OF B-VareAlle IN FRAME FRAME-Para /* Initier */
DO:
    IF NOT lKlarGjor THEN
        MESSAGE "Alle varer til kasse? Bekreft"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lOk AS LOGICAL.
    IF lOk OR lKlargjor THEN DO:
        RUN SkapELoggAlle ("ArtBas",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
        RUN SkapELoggAlle ("MixMatch",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON ENTRY OF BUTTON-1 IN FRAME FRAME-Para /* Registertype */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON MOUSE-SELECT-DBLCLICK OF BUTTON-1 IN FRAME FRAME-Para /* Registertype */
DO:
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON MOUSE-SELECT-DOWN OF BUTTON-1 IN FRAME FRAME-Para /* Registertype */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME FRAME-Para /* Til overføring */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON ENTRY OF BUTTON-2 IN FRAME FRAME-Para /* Til overføring */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON MOUSE-SELECT-DBLCLICK OF BUTTON-2 IN FRAME FRAME-Para /* Til overføring */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON MOUSE-SELECT-DOWN OF BUTTON-2 IN FRAME FRAME-Para /* Til overføring */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 wWin
ON MOUSE-SELECT-DOWN OF BUTTON-3 IN FRAME FRAME-Para /* Auto */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 wWin
ON MOUSE-SELECT-DOWN OF BUTTON-4 IN FRAME FRAME-Para /* Hele register */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 wWin
ON ENTRY OF BUTTON-6 IN FRAME FRAME-Para /* Antall overførte */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 wWin
ON MOUSE-SELECT-DBLCLICK OF BUTTON-6 IN FRAME FRAME-Para /* Antall overførte */
DO:
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 wWin
ON MOUSE-SELECT-DOWN OF BUTTON-6 IN FRAME FRAME-Para /* Antall overførte */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 wWin
ON ENTRY OF BUTTON-7 IN FRAME FRAME-Para /* Kl. */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 wWin
ON MOUSE-SELECT-DBLCLICK OF BUTTON-7 IN FRAME FRAME-Para /* Kl. */
DO:
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 wWin
ON MOUSE-SELECT-DOWN OF BUTTON-7 IN FRAME FRAME-Para /* Kl. */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BUTTON-Kontroller
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Kontroller wWin
ON CHOOSE OF BUTTON-Kontroller IN FRAME fMain /* Kontroller */
DO:
  IF bKlargjorPrisko THEN
    RUN KontrollerPrisko IN THIS-PROCEDURE.
  IF lKasseEksport = TRUE THEN
      RUN KontrollerElogg IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IMAGE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-1 wWin
ON MOUSE-SELECT-DBLCLICK OF IMAGE-1 IN FRAME fMain
DO:
 ASSIGN
  {&WINDOW-NAME}:WIDTH-PIXELS  = IF SELF:PRIVATE-DATA = "MINI" THEN iWPix ELSE 160
  {&WINDOW-NAME}:HEIGHT-PIXELS = IF SELF:PRIVATE-DATA = "MINI" THEN iHPix ELSE 
              IF FI-Serverstatus:HIDDEN = TRUE THEN 72 ELSE 95
  SELF:PRIVATE-DATA = STRING(SELF:PRIVATE-DATA = "MINI","STOR/MINI").
END.

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
    IF lActiveTick = TRUE THEN
        RETURN.
    ASSIGN lActiveTick = TRUE.
    IF bKlargjorPrisko THEN
        RUN KontrollerPrisko.
    IF lKasseEksport = TRUE THEN DO:
        RUN KontrollerElogg.
        RUN StartEksport.
    END.
    ASSIGN lActiveTick = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
    ON ALT-K ANYWHERE
    DO:
        IF NOT DYNAMIC-FUNCTION('getCurrentPage':U) = 1 THEN
            RETURN.
        IF chPSTimer:ENABLED THEN
            RETURN.
        IF NOT lKasseEksport THEN
            RETURN.
        ASSIGN lKlargjor = NOT lKlargjor.
        RUN KlargjorModus (lKlargjor).
    END.


{src/adm2/windowmn.i}
/* RUN InitierTimer. */

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
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Paremeter|Filinfo' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 6.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 21.43 , 116.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_folder ,
             FI-Serverstatus:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

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
      ASSIGN BUTTON-Kontroller:SENSITIVE IN FRAME {&FRAME-NAME} = lDisable
             B-Prisko:SENSITIVE = lDisable.
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

OCXFile = SEARCH( "w-ELoggNucleus.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chPSTimer = PSTimer:COM-HANDLE
    UIB_S = chPSTimer:LoadControls( OCXFile, "PSTimer":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-ELoggNucleus.wrx":U SKIP(1)
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
  DEF VAR pcTekst AS CHAR NO-UNDO.

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  IF VALID-HANDLE(chPSTimer) THEN
      RELEASE OBJECT chPSTimer NO-ERROR.
  IF VALID-HANDLE(PSTimer) THEN
      DELETE OBJECT PSTimer NO-ERROR.
  ASSIGN PSTimer   = ?
         chPSTimer = ?
         pcTekst   = IF PROGRAM-NAME(1) = ? THEN '' ELSE PROGRAM-NAME(1) + "," + 
                     IF PROGRAM-NAME(2) = ? THEN '' ELSE PROGRAM-NAME(2) + "," + 
                     IF PROGRAM-NAME(3) = ? THEN '' ELSE PROGRAM-NAME(3) + "," + 
                     IF PROGRAM-NAME(4) = ? THEN '' ELSE PROGRAM-NAME(4) + "," + 
                     IF PROGRAM-NAME(5) = ? THEN '' ELSE PROGRAM-NAME(5) + "," + 
                     IF PROGRAM-NAME(6) = ? THEN '' ELSE PROGRAM-NAME(6).
  IF  pcTekst MATCHES "*start.p*" THEN . /* Gjør ingenting */
  ELSE
      /*QUIT */ .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportTyper wWin 
PROCEDURE EksportTyper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cErpModuler AS CHARACTER  NO-UNDO.
    IF cHKinst = "yes" THEN DO:
        ASSIGN lKasseEksport = FALSE.
    END.
    ELSE DO:
        /* InfoPos kassa */
        KASSEKOLL:
            FOR EACH Butiker WHERE 
            Butiker.ApningsDato     <> ? AND 
            Butiker.harButikksystem  = TRUE AND  
            Butiker.NedlagtDato      = ? NO-LOCK:
            /* Butikken nedlegges først om noen dager. */
            IF Butiker.NedlagtDato <> ? AND Butiker.NedlagtDato <= TODAY  THEN
                NEXT KASSEKOLL.
            /* Butikken åpner først om noen dager. */
            IF Butiker.ApningsDato <> ? AND Butiker.ApningsDato > TODAY THEN 
                NEXT KASSEKOLL.

            IF CAN-FIND(FIRST kasse WHERE kasse.butik = butiker.butik AND kasse.modell = 51 AND kasse.aktiv = TRUE) THEN DO:
                ASSIGN lKasseEksport = TRUE.
                LEAVE KASSEKOLL.
            END.
        END.
    END.
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
  DISPLAY CLanbutikerParam cFtpbutikerParam FI-Serverstatus 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE IMAGE-1 CLanbutikerParam BUTTON-Kontroller B-Prisko cFtpbutikerParam 
         B-StartStop 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  DISPLAY FI-VareFiler EDITOR-1 FI-MixFiler FI-FilTxt 
      WITH FRAME FRAME-Filinfo IN WINDOW wWin.
  ENABLE RECT-59 FI-VareFiler EDITOR-1 FI-MixFiler FI-FilTxt 
      WITH FRAME FRAME-Filinfo IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Filinfo}
  DISPLAY TG-VareAuto FI-AntVarer FI-VareDatoTid FI-VareError FI-AntPakker 
          FI-MixDatoTid FI-MixError TG-KampMixMatchAuto FI-AntKampMixMatch 
          FI-KampMixMatchDatoTid FI-KampMixMatchError TG-Skjul FI-OverfTxt 
          FI-VarerTxt FI-VareTilExp FI-PakkeMixTxt FI-KampMixMatchTxt 
          FI-KampMixMatchTilExp FI-TilExpInfo 
      WITH FRAME FRAME-Para IN WINDOW wWin.
  ENABLE RECT-56 RECT-57 RECT-58 BUTTON-1 BUTTON-2 BUTTON-3 BUTTON-4 BUTTON-6 
         BUTTON-7 TG-VareAuto B-VareAlle TG-KampMixMatchAuto TG-Skjul 
         FI-OverfTxt FI-VarerTxt FI-PakkeMixTxt FI-KampMixMatchTxt 
      WITH FRAME FRAME-Para IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Para}
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

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FinnesIkkeOverforte wWin 
PROCEDURE FinnesIkkeOverforte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME FRAME-Para:
    IF FI-VareTilExp:BGCOLOR = 12 THEN 
        ASSIGN 
            /*   FI-Serverstatus:SCREEN-VALUE IN FRAME fMain = "Ikke overført data finnes" */
               FI-Serverstatus:HIDDEN IN FRAME fMain = FALSE.
    IF FI-KampMixMatchTilExp:BGCOLOR = 12 THEN 
        ASSIGN 
            /*   FI-Serverstatus:SCREEN-VALUE IN FRAME fMain = "Ikke overført data finnes" */
               FI-Serverstatus:HIDDEN IN FRAME fMain = FALSE.
    ELSE 
        ASSIGN FI-Serverstatus:HIDDEN = TRUE.
    RUN FixWindowSize.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixWindowSize wWin 
PROCEDURE FixWindowSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME fMain:
  IF {&WINDOW-NAME}:WIDTH-PIXELS <> iWPix THEN
      ASSIGN {&WINDOW-NAME}:HEIGHT-PIXELS = IF FI-Serverstatus:HIDDEN = TRUE THEN 72 ELSE 95.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitColors wWin 
PROCEDURE InitColors :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iFarge AS INTEGER    NO-UNDO.
    DO WITH FRAME FRAME-Para:
        ASSIGN FI-VareTilExp:BGCOLOR         = iFarge
               FI-KampMixMatchTilExp:BGCOLOR = iFarge.
    END.
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
    ASSIGN chPSTimer          = chPSTimer:PSTimer
           chPSTimer:ENABLED  = FALSE.
           chPSTimer:interval = 5000.
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

  /* Code placed here will execute PRIOR to standard behavior. */
  {syspara.i 1 1 18 cHKinst}
  {syspara.i 1 1 56 cOutputDir}
  cOutputDir = TRIM(RIGHT-TRIM(cOutputDir,'\')).

  /* Kjør klargjøring av prisko. */
  {syspara.i 210 257 1 cTekst}             
  IF CAN-DO("1,yes,true,Ja",cTekst) OR cTekst = '' THEN 
    bKlargjorPrisko = TRUE.                 
  ELSE                                   
    bKlargjorPrisko = FALSE.                

  /* Kjør utlegg av xml filer. */
  {syspara.i 210 257 2 cTekst}             
  IF CAN-DO("1,yes,true,Ja",cTekst) OR cTekst = '' THEN 
    bUtleggXmlFiler = TRUE.                 
  ELSE                                   
    bUtleggXmlFiler = FALSE.    

  /* Ta med prisendringer i eLogg. */
  {syspara.i 210 257 3 cTekst}             
  IF CAN-DO("1,yes,true,Ja",cTekst) OR cTekst = '' THEN 
    bSendPris = TRUE.                 
  ELSE                                   
    bSendPris = FALSE.    

  FOR EACH ELogg WHERE ELogg.Verdier = "KLARGJOR":
      DELETE ELogg.
  END.
  RUN EksportTyper.
  RUN InitColors(?).
  RUN SUPER.
  IF lKasseEksport THEN
      RUN InitierButiker.
  ELSE IF NOT lKasseEksport THEN
      RUN ToggleOff.
  RUN SlettErpFiler.
  IF lOrdHK = FALSE THEN
      RUN SlettHKord.
  /* Code placed here will execute AFTER standard behavior.    */
  FRAME FRAME-Para:MOVE-TO-TOP().
  ASSIGN iWPix = {&WINDOW-NAME}:WIDTH-PIXELS 
         iHPix = {&WINDOW-NAME}:HEIGHT-PIXELS.
  RUN KontrollerPrisko IN THIS-PROCEDURE.
  /* Vid aktiv(a) kassa/kassor */
  IF lKasseEksport THEN DO:
      RUN KontrollerElogg  IN THIS-PROCEDURE.
  END.
  PROCESS EVENTS.
  RUN FinnesIkkeOverforte.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitierButiker wWin 
PROCEDURE InitierButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN cLanbutikerParam = ""
           cFtpbutikerParam = "".
/*     FOR EACH Butiker NO-LOCK WHERE Butiker.LanButikk = TRUE:                                       */
/*         ASSIGN cLanbutikerParam = cLanbutikerParam + (IF cLanbutikerParam = "" THEN "" ELSE ",") + */
/*             STRING(Butiker.Butik).                                                                 */
/*     END.                                                                                           */
    DO WITH FRAME Frame-Filinfo:
      FOR EACH butiker WHERE Butiker.VPI = 1 AND CAN-FIND(FIRST Kasse WHERE kasse.butik = butiker.butik AND Kasse.GruppeNr = 1 AND kasse.aktiv = TRUE AND kasse.modell = 51):
          CREATE TT_N9Butiker.
          ASSIGN TT_N9Butiker.ButikkNr = Butiker.Butik.
              ASSIGN cFtpButiker = cFtpButiker + (IF cFtpButiker = "" THEN "" ELSE ",") + STRING(Butiker.Butik).
              EDITOR-1:INSERT-STRING(STRING(butiker.butik) + " " + STRING(Butiker.butnamn,"x(15)") + " " + "<FILnavn>." + STRING(butiker.butik) + CHR(10)).
      END.
    END.
    ASSIGN cLanbutikerParam = cLanButiker
           cLanbutikerParam:SCREEN-VALUE = cLanbutikerParam
           cFtpbutikerParam:SCREEN-VALUE = cFtpButiker.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitierTimer wWin 
PROCEDURE InitierTimer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     ASSIGN chPSTimer = chPSTimer:PSTimer
            chPSTimer:interval = 1000.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KlargjorModus wWin 
PROCEDURE KlargjorModus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cAktiv AS LOGICAL  NO-UNDO.
  DO WITH FRAME fMain:
      ASSIGN lKlargjor = cAktiv
             B-Klargjor:HIDDEN    = NOT cAktiv
             B-Klargjor:SENSITIVE = cAktiv
             B-Ftpbut:HIDDEN      = NOT cAktiv
             B-Ftpbut:SENSITIVE   = cAktiv
             B-Lanbut:HIDDEN      = NOT cAktiv
             B-Lanbut:SENSITIVE   = cAktiv.
  END.
  IF cAktiv THEN
      ASSIGN cOrgLanButiker = cLanButiker
             cOrgFtpButiker = cFtpButiker.
  ELSE
      ASSIGN  cLanButiker = cOrgLanButiker
              cFtpButiker = cOrgFtpButiker
              cLanButikerParam = cLanButiker
              cFtpButikerParam = cFtpButiker
              cLanButikerParam:SCREEN-VALUE = cLanButiker
              cFtpButikerParam:SCREEN-VALUE = cFtpButiker.
   FRAME fMain:BGCOLOR = IF cAktiv THEN 13 ELSE ?.           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerElogg wWin 
PROCEDURE KontrollerElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lFinnes AS LOGICAL    NO-UNDO.
  DO WITH FRAME FRAME-Para:

     IF FI-VareTilExp:BGCOLOR = ? AND (canfindElogg("ArtBas","POS") OR 
                                       (canfindElogg("ArtPris","POS") AND bSendPris) OR
                                       canfindElogg("PakkeLinje","POS") OR
                                       canfindElogg("MixMatch","POS"))  THEN DO:
         ASSIGN FI-VareTilExp:BGCOLOR = 12
                B-SendVare:SENSITIVE  = TRUE.
     END.
     IF FI-KampMixMatchTilExp:BGCOLOR = ? AND canfindElogg("KampanjeMixMatch","POS") THEN DO:
         ASSIGN FI-KampMixMatchTilExp:BGCOLOR = 12
                B-SendKampMixMatch:SENSITIVE  = TRUE.
     END.
     RUN FinnesIkkeOverforte.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerPrisko wWin 
PROCEDURE KontrollerPrisko :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF CAN-FIND(FIRST prisko WHERE PrisKo.AktiveresDato < TODAY) OR
       CAN-FIND(FIRST prisko WHERE PrisKo.AktiveresDato = TODAY AND 
                                   PrisKo.AktiveresTid < TIME) THEN DO:
        RUN StartPriskoOppdat.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectPage wWin 
PROCEDURE selectPage :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER piPageNum AS INTEGER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT piPageNum).

  /* Code placed here will execute AFTER standard behavior.    */
  CASE piPageNum:
      WHEN 1 THEN
          FRAME FRAME-Para:MOVE-TO-TOP().
      WHEN 2 THEN
          FRAME FRAME-Filinfo:MOVE-TO-TOP().
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetDatoTidColor wWin 
PROCEDURE SetDatoTidColor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcDatoTid AS CHARACTER  NO-UNDO.
  DO WITH FRAME FRAME-Para:
      
      ASSIGN FI-VareDatoTid:BGCOLOR     = INT(STRING(FI-VareDatoTid:SCREEN-VALUE     = ipcDatoTid,"10/15"))
             FI-MixDatoTid:BGCOLOR      = INT(STRING(FI-MixDatoTid:SCREEN-VALUE      = ipcDatoTid,"10/15"))
          .
/*              FI-VarGrDatoTid:BGCOLOR    = INT(STRING(FI-VarGrDatoTid:SCREEN-VALUE    = ipcDatoTid,"10/15"))       */
/*              FI-KasValutaDatoTid:BGCOLOR   = INT(STRING(FI-KasValutaDatoTid:SCREEN-VALUE   = ipcDatoTid,"10/15")) */
/*              FI-KassererDatoTid:BGCOLOR = INT(STRING(FI-KassererDatoTid:SCREEN-VALUE = ipcDatoTid,"10/15"))       */
/*              FI-SelgerDatoTid:BGCOLOR   = INT(STRING(FI-SelgerDatoTid:SCREEN-VALUE   = ipcDatoTid,"10/15"))       */
/*              FI-KundeDatoTid:BGCOLOR    = INT(STRING(FI-KundeDatoTid:SCREEN-VALUE    = ipcDatoTid,"10/15"))       */
/*              FI-FargerDatoTid:BGCOLOR   = INT(STRING(FI-FargerDatoTid:SCREEN-VALUE   = ipcDatoTid,"10/15"))       */
/*              FI-StrKonvDatoTid:BGCOLOR  = INT(STRING(FI-StrKonvDatoTid:SCREEN-VALUE  = ipcDatoTid,"10/15"))       */
/*              FI-StrTypeDatoTid:BGCOLOR  = INT(STRING(FI-StrTypeDatoTid:SCREEN-VALUE  = ipcDatoTid,"10/15"))       */
/*              FI-TeksterDatoTid:BGCOLOR = INT(STRING(FI-TeksterDatoTid:SCREEN-VALUE = ipcDatoTid,"10/15"))         */
/*              FI-GarantiDatoTid:BGCOLOR = INT(STRING(FI-GarantiDatoTid:SCREEN-VALUE = ipcDatoTid,"10/15"))         */
/*              FI-ButikerDatoTid:BGCOLOR = INT(STRING(FI-ButikerDatoTid:SCREEN-VALUE = ipcDatoTid,"10/15")).        */
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapELoggAlle wWin 
PROCEDURE SkapELoggAlle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cTabell AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cType AS CHARACTER  NO-UNDO. /* 'ALLE' eller 'KLARGJOR' */
    FIND ELogg WHERE ELogg.TabellNavn     = cTabell AND
                     ELogg.EksterntSystem = "POS"   AND
                     ELogg.Verdier        = cType NO-ERROR. 
    IF NOT AVAIL ELogg THEN DO:
        CREATE ELogg.
        ASSIGN ELogg.TabellNavn     = cTabell
               ELogg.EksterntSystem = "POS"   
               ELogg.Verdier        = cType.
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
    RUN KontrollerElogg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettErpFiler wWin 
PROCEDURE SlettErpFiler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH ELogg WHERE ELogg.TabellNavn     = "ArtBas" AND
                         ELogg.EksterntSystem = "ERP":
        DELETE ELogg.
    END.
    FOR EACH ELogg WHERE ELogg.TabellNavn     = "Bokforingsbilag" AND
                         ELogg.EksterntSystem = "ERP":
        DELETE ELogg.
    END.
    FOR EACH ELogg WHERE ELogg.TabellNavn     = "Ordre" AND
                         ELogg.EksterntSystem = "ERP":
        DELETE ELogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettHKord wWin 
PROCEDURE SlettHKord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH ELogg WHERE ELogg.Tabellnavn = "ORDHK" AND
                         ELogg.EksterntSystem = "POS":
        DELETE ELogg.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettPosFiler wWin 
PROCEDURE SlettPosFiler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     FOR EACH ELogg WHERE ELogg.TabellNavn     = "ArtBas" AND */
/*                          ELogg.EksterntSystem = "POS":       */
/*         DELETE ELogg.                                        */
/*     END.                                                     */
    FOR EACH ELogg WHERE ELogg.TabellNavn     = "VarGr" AND
                         ELogg.EksterntSystem = "POS":
        DELETE ELogg.
    END.
    FOR EACH ELogg WHERE ELogg.TabellNavn     = "KasValuta" AND
                         ELogg.EksterntSystem = "POS":
        DELETE ELogg.
    END.
    FOR EACH ELogg WHERE ELogg.TabellNavn     = "ButikkForsalj" AND
                         ELogg.EksterntSystem = "POS":
        DELETE ELogg.
    END.
    FOR EACH ELogg WHERE ELogg.TabellNavn     = "ButikkSelger" AND
                         ELogg.EksterntSystem = "POS":
        DELETE ELogg.
    END.
    FOR EACH ELogg WHERE ELogg.TabellNavn     = "Kunde" AND
                         ELogg.EksterntSystem = "POS":
        DELETE ELogg.
    END.
    FOR EACH ELogg WHERE ELogg.TabellNavn     = "Farg" AND
                         ELogg.EksterntSystem = "POS":
        DELETE ELogg.
    END.
    FOR EACH ELogg WHERE ELogg.TabellNavn     = "StrKonv" AND
                         ELogg.EksterntSystem = "POS":
        DELETE ELogg.
    END.
    FOR EACH ELogg WHERE ELogg.TabellNavn     = "StrType" AND
                         ELogg.EksterntSystem = "POS":
        DELETE ELogg.
    END.
    FOR EACH ELogg WHERE ELogg.TabellNavn     = "FeilKode" AND
                         ELogg.EksterntSystem = "POS":
        DELETE ELogg.
    END.
    FOR EACH ELogg WHERE ELogg.TabellNavn     = "KravKode" AND
                         ELogg.EksterntSystem = "POS":
        DELETE ELogg.
    END.
    FOR EACH ELogg WHERE ELogg.TabellNavn     = "GaveKType" AND
                         ELogg.EksterntSystem = "POS":
        DELETE ELogg.
    END.
    FOR EACH ELogg WHERE ELogg.TabellNavn     = "UtbetType" AND
                         ELogg.EksterntSystem = "POS":
        DELETE ELogg.
    END.
    FOR EACH ELogg WHERE ELogg.TabellNavn     = "InnBetType" AND
                         ELogg.EksterntSystem = "POS":
        DELETE ELogg.
    END.
    FOR EACH ELogg WHERE ELogg.TabellNavn     = "Garanti" AND
                         ELogg.EksterntSystem = "POS":
        DELETE ELogg.
    END.
    FOR EACH ELogg WHERE ELogg.TabellNavn     = "Butiker" AND
                         ELogg.EksterntSystem = "POS":
        DELETE ELogg.
    END.
    FOR EACH ELogg WHERE ELogg.TabellNavn     = "KjedensButikker" AND
                         ELogg.EksterntSystem = "POS":
        DELETE ELogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartEksport wWin 
PROCEDURE StartEksport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR iCount          AS INTE  NO-UNDO.
  DEF VAR cHgrFiler       AS CHAR  NO-UNDO.
  DEF VAR cKundeFiler     AS CHAR  NO-UNDO.
  DEF VAR cKasValutaFiler AS CHAR  NO-UNDO.
  DEF VAR cVareFiler      AS CHAR  NO-UNDO.
  DEF VAR cMixFiler       AS CHAR  NO-UNDO.
  DEF VAR cKasserereFiler AS CHAR  NO-UNDO.
  DEF VAR cSelgerFiler  AS CHAR  NO-UNDO.
  DEF VAR cFargFiler    AS CHAR  NO-UNDO.
  DEF VAR cStrKonvFiler AS CHAR  NO-UNDO.
  DEF VAR cStrTypeFiler AS CHAR  NO-UNDO.
  DEF VAR cTeksterFiler AS CHAR  NO-UNDO.
  DEF VAR cGarantiFiler  AS CHAR  NO-UNDO.
  DEF VAR cButikerFiler  AS CHAR  NO-UNDO.
  DEF VAR lOverfort  AS LOGI  NO-UNDO.
  DEF VAR cLoadedJpg AS CHAR  NO-UNDO.
  DEF VAR cToolTip   AS CHAR  NO-UNDO.
  DEF VAR lSendError AS LOGI  NO-UNDO.
  DEF VAR cSkjul AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lKlargoring AS LOGICAL    NO-UNDO.

  ASSIGN lTimerOff = NOT chPSTimer:ENABLED.

  DO WITH FRAME FRAME-Para:
    ASSIGN cSkjul = IF TG-Skjul:CHECKED THEN "-q " ELSE "".
    DO WITH FRAME fMain:
        ASSIGN cLoadedJpg = IMAGE-1:IMAGE.
        IMAGE-1:LOAD-IMAGE(".\icon\yellowlight.jpg").
    END.
    {sww.i}
    ASSIGN cDatoTid    = STRING(TIME,"HH:MM").
           lKlargoring = NOT B-Klargjor:HIDDEN.
    /* Hvis varedata skal sendes fra Windows Scheduler, skal varedata ikke sendes herfra. */
    IF bUtleggXmlFiler THEN
    SEND_VAREDATA:
    DO:
        IF (lVare OR TG-VareAuto:CHECKED) AND (canfindElogg("ArtBas","POS") OR 
                                               (canfindElogg("ArtPris","POS") AND bSendPris)) THEN 
        VARE: 
        DO:
          IF lVare = FALSE AND lTimerOff THEN LEAVE VARE.
          /* 2 Parameter efter cOutputdir ær en ingång vi skall anvænda før utlægg av kampanjer, Kommer nedan */
          RUN ArtBas2Nucleus.p (?,
                                INPUT (IF lKlargoring THEN cFtpButiker ELSE ""),
                                INPUT cOutputDir,
                                0,
                                TRUE,
                                'POS',
                                OUTPUT cVareFiler,
                                OUTPUT cMixFiler,
                                OUTPUT FI-AntVarer,
                                OUTPUT FI-AntPakker).

          ASSIGN FI-VareFiler:SCREEN-VALUE IN FRAME FRAME-Filinfo = cVareFiler
                 FI-MixFiler:SCREEN-VALUE = cMixFiler
                 FI-VareTilExp:BGCOLOR = ?
                 lVare                 = FALSE
                 B-SendVare:SENSITIVE  = FALSE
                 FI-AntVarer:SCREEN-VALUE  = STRING(FI-AntVarer).
                 FI-AntPakker:SCREEN-VALUE = STRING(FI-AntPakker).
          ASSIGN cToolTip   = ""
                 lSendError = FALSE.
        END.
    END. /* SEND_VAREDATA */

    IF (lKampMixMatch OR TG-KampMixMatchAuto:CHECKED) AND canfindElogg("KampanjeMixMatch","POS") THEN KAMPANJE: DO:
      IF lKampMixMatch = FALSE AND lTimerOff THEN LEAVE KAMPANJE.
      /* 2 Parameter efter cOutputdir ær en ingång vi skall anvænda før utlægg av kampanjer, Kommer nedan */
      RUN Kampanje2Nucleus.p (INPUT cOutputDir,OUTPUT FI-AntKampMixMatch).
      ASSIGN
/*           FI-VareFiler:SCREEN-VALUE IN FRAME FRAME-Filinfo = cVareFiler */
/*              FI-MixFiler:SCREEN-VALUE = cMixFiler                       */
             FI-KampMixMatchTilExp:BGCOLOR = ?
             lKampMixMatch                 = FALSE
             B-SendKampMixMatch:SENSITIVE  = FALSE
             FI-AntKampMixMatch:SCREEN-VALUE  = STRING(FI-AntKampMixMatch)
             FI-KampMixMatchDatoTid = cDatoTid.
      ASSIGN cToolTip   = ""
             lSendError = FALSE.

    END.
    /* Premm tar inte hand om ERP-elogg */
    RUN SlettErpFiler.
    /* Premm skall slette en massa POS-elogg */
    RUN SlettPosFiler.
    IF lOverfort THEN
      RUN SetDatoTidColor (cDatoTid).
    RUN FinnesIkkeOverforte.
    {swn.i}
    IMAGE-1:LOAD-IMAGE(cLoadedJpg).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartPriskoOppdat wWin 
PROCEDURE StartPriskoOppdat :
DEFINE VARIABLE cLoadedJpg      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOrgTxt         AS CHARACTER  NO-UNDO.
  DO WITH FRAME FRAME-Para:
      {sww.i}
     DO WITH FRAME fMain:
         ASSIGN cLoadedJpg = IMAGE-1:IMAGE
                cOrgTxt    = FI-Serverstatus:SCREEN-VALUE
                FI-Serverstatus:SCREEN-VALUE = "Prisoppdatering pågår"
                FI-Serverstatus:HIDDEN IN FRAME fMain = FALSE.
         IMAGE-1:LOAD-IMAGE(".\icon\yellowlight.jpg").
         RUN FixWindowSize.
         RUN x-klargjorprisko.w (?).
         ASSIGN FI-Serverstatus:SCREEN-VALUE = cOrgTxt.
         IMAGE-1:LOAD-IMAGE(cLoadedJpg).
     END.
     {swn.i}
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
        ASSIGN chPSTimer:ENABLED = TRUE
            .
    END.
    ELSE DO:
        ASSIGN chPSTimer:ENABLED         = FALSE
            .
    END.
  END.
  RUN ButtonEnaDis(NOT chPSTimer:ENABLED).
  IMAGE-1:LOAD-IMAGE(IF NOT chPSTimer:ENABLED THEN ".\icon\redlight.jpg" ELSE ".\icon\greenlight.jpg").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToggleOff wWin 
PROCEDURE ToggleOff :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME FRAME-Para:
       ASSIGN TG-VareAuto:CHECKED = FALSE 
              TG-Skjul:CHECKED = FALSE
              TG-VareAuto:SENSITIVE = FALSE
              B-VareAlle:SENSITIVE = FALSE.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION canfindElogg wWin 
FUNCTION canfindElogg RETURNS LOGICAL
  ( INPUT cTabell AS CHARACTER, INPUT cEksternt AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 RETURN CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = cTabell AND ELogg.EksterntSystem = cEksternt).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFiltxt wWin 
FUNCTION getFiltxt RETURNS CHARACTER
  ( INPUT cFilNavn AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cReturTxt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cInRad    AS CHARACTER  NO-UNDO.
  IF SEARCH(cFilNavn) <> ? THEN DO:
      INPUT FROM VALUE(cFilNavn).
      REPEAT:
          IMPORT UNFORMATTED cInrad.
          IF cInrad <> "" THEN
              ASSIGN cReturTxt = cReturTxt + (IF cReturTxt = "" THEN "" ELSE CHR(10)) +
                                 cInrad.
      END.
      INPUT CLOSE.
  END.
  RETURN cReturTxt.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setError wWin 
FUNCTION setError RETURNS CHARACTER
  ( INPUT fiHandle AS HANDLE,INPUT-OUTPUT lSendError AS LOGICAL,INPUT-OUTPUT cToolTip AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
           ASSIGN fiHandle:SCREEN-VALUE = STRING(lSendError,"ERROR/OK")
                  fiHandle:BGCOLOR      = IF lSendError THEN 12 ELSE 10
                  fiHandle:TOOLTIP      = cToolTip
                  lSendError            = FALSE
                  cToolTip              = "".

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

