&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
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

DEFINE VARIABLE lVare             AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lGruppe            AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lKasValuta        AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lAvdeling        AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lHuvGr          AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lLevBas            AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lProdusent           AS LOGICAL    NO-UNDO.
DEFINE VARIABLE iHPix             AS INTEGER    NO-UNDO.
DEFINE VARIABLE iWPix             AS INTEGER    NO-UNDO.
DEFINE VARIABLE lActiveTick       AS LOGICAL    NO-UNDO. /* */
DEF VAR hInstance AS INT NO-UNDO.

{windows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-Status CLanbutikerParam BUTTON-Kontroller ~
cFtpbutikerParam IMAGE-1 
&Scoped-Define DISPLAYED-OBJECTS CLanbutikerParam cFtpbutikerParam ~
FI-Serverstatus 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

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
DEFINE BUTTON B-StartStop 
     LABEL "Start/Stopp" 
     SIZE 14.6 BY 1.14.

DEFINE BUTTON B-Status 
     LABEL "Kassestatus..." 
     SIZE 20 BY 1.14.

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

DEFINE VARIABLE FI-AvdelingFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "AvdelingFiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FilTxt AS CHARACTER FORMAT "X(256)":U INITIAL " Filinfo" 
      VIEW-AS TEXT 
     SIZE 8 BY .62
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE FI-HgrFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Hgruppefiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-HuvGrFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "HuvgrFiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KasValutaFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Valutafiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevBasFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "LevbasFiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MixFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mixfiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ProdusentFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Produsentfiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VareFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varefiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-59
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 105 BY 19.05.

DEFINE BUTTON B-GruppeAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-LevBasAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-ProdusentAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendGruppe  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendLevBas  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendProdusent  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendVare  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-VargrAlle-2 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON BUTTON-1 
     LABEL "Registertype" 
     SIZE 17.8 BY .71.

DEFINE BUTTON BUTTON-2 
     LABEL "Til overføring" 
     SIZE 17 BY .71.

DEFINE BUTTON BUTTON-3 
     LABEL "Auto" 
     SIZE 8 BY .71.

DEFINE BUTTON BUTTON-4 
     LABEL "Hele register" 
     SIZE 17 BY .71.

DEFINE BUTTON BUTTON-6 
     LABEL "Antall overførte" 
     SIZE 17.8 BY .71.

DEFINE BUTTON BUTTON-7 
     LABEL "Kl." 
     SIZE 8.4 BY .71.

DEFINE VARIABLE FI-AntGruppe AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntLevBas AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntPakker AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntProdusent AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntVarer AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-GruppeDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-GruppeError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-GrupperTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Grupper" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-GruppeTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-LevBasDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-LevBasError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevBasTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-LevBasTxt AS CHARACTER FORMAT "X(256)":U INITIAL "LevBas" 
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

DEFINE VARIABLE FI-ProdusentDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-ProdusentError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ProdusentTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-ProdusentTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Produsent" 
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

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL 
     SIZE 101.2 BY 16.43.

DEFINE RECTANGLE RECT-58
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL 
     SIZE .4 BY 16.38.

DEFINE VARIABLE TG-GruppeAuto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-LevBasAuto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-ProdusentAuto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-VareAuto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B-StartStop AT ROW 1.48 COL 35.6
     B-Status AT ROW 1.48 COL 96
     CLanbutikerParam AT ROW 1.57 COL 64 COLON-ALIGNED
     BUTTON-Kontroller AT ROW 2.67 COL 35.6
     cFtpbutikerParam AT ROW 2.81 COL 64 COLON-ALIGNED
     FI-Serverstatus AT ROW 4.43 COL 1 NO-LABEL
     IMAGE-1 AT ROW 1.05 COL 1
     SPACE(84.00) SKIP(22.71)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         DEFAULT-BUTTON B-StartStop.

DEFINE FRAME FRAME-Filinfo
     FI-VareFiler AT ROW 2.29 COL 70.4 COLON-ALIGNED
     EDITOR-1 AT ROW 2.52 COL 3.4 NO-LABEL
     FI-MixFiler AT ROW 3.43 COL 70.4 COLON-ALIGNED
     FI-HgrFiler AT ROW 4.57 COL 70.4 COLON-ALIGNED
     FI-KasValutaFiler AT ROW 5.71 COL 70.4 COLON-ALIGNED
     FI-AvdelingFiler AT ROW 6.86 COL 70.4 COLON-ALIGNED
     FI-HuvGrFiler AT ROW 8 COL 70.4 COLON-ALIGNED
     FI-LevBasFiler AT ROW 9.14 COL 70.4 COLON-ALIGNED
     FI-ProdusentFiler AT ROW 10.29 COL 70.4 COLON-ALIGNED
     FI-FilTxt AT ROW 1 COL 37 COLON-ALIGNED NO-LABEL
     RECT-59 AT ROW 1.24 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.8 ROW 7.14
         SIZE 113 BY 19.81.

DEFINE FRAME FRAME-Para
     BUTTON-1 AT ROW 2.05 COL 4.4 NO-TAB-STOP 
     BUTTON-2 AT ROW 2.05 COL 23 NO-TAB-STOP 
     BUTTON-3 AT ROW 2.05 COL 40.2
     BUTTON-4 AT ROW 2.05 COL 48.2
     BUTTON-6 AT ROW 2.05 COL 65.2 NO-TAB-STOP 
     BUTTON-7 AT ROW 2.05 COL 83 NO-TAB-STOP 
     B-VargrAlle-2 AT ROW 3.19 COL 51.6
     FI-AntVarer AT ROW 3.19 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-VareDatoTid AT ROW 3.19 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-VareError AT ROW 3.19 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-VareAuto AT ROW 3.33 COL 42.4
     FI-AntPakker AT ROW 4.43 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-MixDatoTid AT ROW 4.43 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-MixError AT ROW 4.43 COL 91.2 COLON-ALIGNED NO-LABEL
     B-GruppeAlle AT ROW 5.67 COL 51.6
     FI-AntGruppe AT ROW 5.67 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-GruppeDatoTid AT ROW 5.67 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-GruppeError AT ROW 5.67 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-GruppeAuto AT ROW 5.81 COL 42.4
     B-LevBasAlle AT ROW 6.91 COL 51.6
     FI-AntLevBas AT ROW 6.91 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-LevBasDatoTid AT ROW 6.91 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-LevBasError AT ROW 6.95 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-LevBasAuto AT ROW 7.05 COL 42.4
     B-ProdusentAlle AT ROW 8.14 COL 51.6
     B-SendProdusent AT ROW 8.14 COL 29 NO-TAB-STOP 
     FI-AntProdusent AT ROW 8.14 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-ProdusentDatoTid AT ROW 8.14 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-ProdusentError AT ROW 8.14 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-ProdusentAuto AT ROW 8.29 COL 42.4
     B-SendLevBas AT ROW 6.91 COL 29 NO-TAB-STOP 
     B-SendGruppe AT ROW 5.67 COL 29 NO-TAB-STOP 
     B-SendVare AT ROW 3.19 COL 29 NO-TAB-STOP 
     FI-OverfTxt AT ROW 1 COL 37 COLON-ALIGNED NO-LABEL
     FI-VareTilExp AT ROW 3.19 COL 24 NO-LABEL
     FI-VarerTxt AT ROW 3.33 COL 3 COLON-ALIGNED NO-LABEL
     FI-GruppeTilExp AT ROW 5.67 COL 24 NO-LABEL
     FI-GrupperTxt AT ROW 5.81 COL 3 COLON-ALIGNED NO-LABEL
     FI-LevBasTilExp AT ROW 6.91 COL 24 NO-LABEL
     FI-LevBasTxt AT ROW 7.05 COL 3 COLON-ALIGNED NO-LABEL
     FI-ProdusentTilExp AT ROW 8.14 COL 24 NO-LABEL
     FI-ProdusentTxt AT ROW 8.29 COL 3 COLON-ALIGNED NO-LABEL
     FI-TilExpInfo AT ROW 18.91 COL 4.2
     RECT-57 AT ROW 1.91 COL 3.8
     RECT-58 AT ROW 1.91 COL 22.4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.8 ROW 7.14
         SIZE 113 BY 19.81.


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
         TITLE              = "Eksport Rigalformat"
         HEIGHT             = 26.19
         WIDTH              = 116
         MAX-HEIGHT         = 26.19
         MAX-WIDTH          = 116
         VIRTUAL-HEIGHT     = 26.19
         VIRTUAL-WIDTH      = 116
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
   Size-to-Fit                                                          */
ASSIGN 
       FRAME fMain:SCROLLABLE       = FALSE.

/* SETTINGS FOR BUTTON B-StartStop IN FRAME fMain
   NO-ENABLE                                                            */
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
/* SETTINGS FOR BUTTON B-SendGruppe IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendLevBas IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendProdusent IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendVare IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntGruppe IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntLevBas IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntPakker IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntProdusent IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntVarer IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-GruppeDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-GruppeError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-GruppeTilExp IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-LevBasDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevBasError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevBasTilExp IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-MixDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-MixError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ProdusentDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ProdusentError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ProdusentTilExp IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
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
ON END-ERROR OF wWin /* Eksport Rigalformat */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Eksport Rigalformat */
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


&Scoped-define FRAME-NAME FRAME-Para
&Scoped-define SELF-NAME B-GruppeAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-GruppeAlle wWin
ON CHOOSE OF B-GruppeAlle IN FRAME FRAME-Para /* Initier */
DO:
    DEFINE VARIABLE iCount   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cGrupper AS CHARACTER  NO-UNDO.
    ASSIGN cGrupper = "Avdeling,HuvGr,VarGr".
    DO iCount = 1 TO NUM-ENTRIES(cGrupper):
        RUN SkapELoggAlle (ENTRY(iCount,cGrupper)).
    END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevBasAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevBasAlle wWin
ON CHOOSE OF B-LevBasAlle IN FRAME FRAME-Para /* Initier */
DO:
  RUN SkapELoggAlle ("LevBas").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ProdusentAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ProdusentAlle wWin
ON CHOOSE OF B-ProdusentAlle IN FRAME FRAME-Para /* Initier */
DO:
  RUN SkapELoggAlle ("Produsent").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SendGruppe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendGruppe wWin
ON CHOOSE OF B-SendGruppe IN FRAME FRAME-Para /* Send */
DO:
  ASSIGN lGruppe = TRUE.
  RUN StartEksport IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SendLevBas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendLevBas wWin
ON CHOOSE OF B-SendLevBas IN FRAME FRAME-Para /* Send */
DO:
  ASSIGN lLevBas = TRUE.
  RUN StartEksport IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SendProdusent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendProdusent wWin
ON CHOOSE OF B-SendProdusent IN FRAME FRAME-Para /* Send */
DO:
  ASSIGN lProdusent = TRUE.
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


&Scoped-define SELF-NAME B-Status
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Status wWin
ON CHOOSE OF B-Status IN FRAME fMain /* Kassestatus... */
DO:
  RUN ShellExecute{&A} IN hpApi(0,
                                "open",
                                "C:\Home\Lindbak\kasse\menu.htm",
                                "",
                                "",
                                1,
                                OUTPUT hInstance).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Para
&Scoped-define SELF-NAME B-VargrAlle-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VargrAlle-2 wWin
ON CHOOSE OF B-VargrAlle-2 IN FRAME FRAME-Para /* Initier */
DO:
    MESSAGE "Alle varer til kasse? Bekreft"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lOk AS LOGICAL.
    IF lOk THEN
        RUN SkapELoggAlle ("ArtBas").
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
    RUN KontrollerElogg.
    RUN StartEksport.
    ASSIGN lActiveTick = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}
/* RUN InitierTimer. */

PROCEDURE ShellExecute{&A} EXTERNAL "shell32" :
     DEFINE INPUT PARAMETER HWND AS LONG.
     DEFINE INPUT PARAMETER lpOperation AS CHAR.
     DEFINE INPUT PARAMETER lpFile AS CHAR.
     DEFINE INPUT PARAMETER lpParameters AS CHAR.
     DEFINE INPUT PARAMETER lpDirectory AS CHAR.
     DEFINE INPUT PARAMETER nShowCmd AS LONG.
     DEFINE RETURN PARAMETER hInstance AS LONG.
END.

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
       RUN resizeObject IN h_folder ( 21.19 , 116.00 ) NO-ERROR.

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
      ASSIGN BUTTON-Kontroller:SENSITIVE IN FRAME {&FRAME-NAME} = lDisable.
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

OCXFile = SEARCH( "w-RigalUt.wrx":U ).
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
ELSE MESSAGE "w-RigalUt.wrx":U SKIP(1)
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
  IF VALID-HANDLE(chPSTimer) THEN
      RELEASE OBJECT chPSTimer NO-ERROR.
  IF VALID-HANDLE(PSTimer) THEN
      DELETE OBJECT PSTimer NO-ERROR.
  ASSIGN PSTimer   = ?
         chPSTimer = ?.
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
  DISPLAY CLanbutikerParam cFtpbutikerParam FI-Serverstatus 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE B-Status CLanbutikerParam BUTTON-Kontroller cFtpbutikerParam IMAGE-1 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  DISPLAY FI-VareFiler EDITOR-1 FI-MixFiler FI-HgrFiler FI-KasValutaFiler 
          FI-AvdelingFiler FI-HuvGrFiler FI-LevBasFiler FI-ProdusentFiler 
          FI-FilTxt 
      WITH FRAME FRAME-Filinfo IN WINDOW wWin.
  ENABLE FI-VareFiler EDITOR-1 FI-MixFiler FI-HgrFiler FI-KasValutaFiler 
         FI-AvdelingFiler FI-HuvGrFiler FI-LevBasFiler FI-ProdusentFiler 
         FI-FilTxt RECT-59 
      WITH FRAME FRAME-Filinfo IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Filinfo}
  DISPLAY FI-AntVarer FI-VareDatoTid FI-VareError TG-VareAuto FI-AntPakker 
          FI-MixDatoTid FI-MixError FI-AntGruppe FI-GruppeDatoTid FI-GruppeError 
          TG-GruppeAuto FI-AntLevBas FI-LevBasDatoTid FI-LevBasError 
          TG-LevBasAuto FI-AntProdusent FI-ProdusentDatoTid FI-ProdusentError 
          TG-ProdusentAuto FI-OverfTxt FI-VareTilExp FI-VarerTxt FI-GruppeTilExp 
          FI-GrupperTxt FI-LevBasTilExp FI-LevBasTxt FI-ProdusentTilExp 
          FI-ProdusentTxt FI-TilExpInfo 
      WITH FRAME FRAME-Para IN WINDOW wWin.
  ENABLE BUTTON-1 BUTTON-2 BUTTON-3 BUTTON-4 BUTTON-6 BUTTON-7 B-VargrAlle-2 
         TG-VareAuto B-GruppeAlle TG-GruppeAuto B-LevBasAlle TG-LevBasAuto 
         B-ProdusentAlle TG-ProdusentAuto FI-OverfTxt FI-VarerTxt FI-GrupperTxt 
         FI-LevBasTxt FI-ProdusentTxt RECT-57 RECT-58 
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
    IF FI-VareTilExp:BGCOLOR = 12     OR
       FI-GruppeTilExp:BGCOLOR = 12    OR
       FI-LevBasTilExp:BGCOLOR = 12    OR
       FI-ProdusentTilExp:BGCOLOR = 12   THEN
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
        ASSIGN FI-VareTilExp:BGCOLOR     = iFarge 
               FI-GruppeTilExp:BGCOLOR    = iFarge
               FI-LevBasTilExp:BGCOLOR    = iFarge
               FI-ProdusentTilExp:BGCOLOR   = iFarge.
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
  RUN InitColors(?).
  RUN SUPER.
  RUN InitierButiker.

  /* Code placed here will execute AFTER standard behavior.    */
  FRAME FRAME-Para:MOVE-TO-TOP().
  ASSIGN iWPix = {&WINDOW-NAME}:WIDTH-PIXELS 
         iHPix = {&WINDOW-NAME}:HEIGHT-PIXELS.
  RUN KontrollerElogg  IN THIS-PROCEDURE.
  PROCESS EVENTS.

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
    FOR EACH Butiker NO-LOCK WHERE 
        CAN-FIND(FIRST Kasse WHERE kasse.butik = butiker.butik AND Kasse.GruppeNr = 1 AND kasse.aktiv = TRUE AND
                                         kasse.modellnr = 10):
        ASSIGN cLanbutiker = cLanbutiker + (IF cLanbutiker = "" THEN "" ELSE ",") + 
            STRING(Butiker.Butik).
    END.
  END.
  ASSIGN cLanbutikerParam = cLanButiker
         cLanbutikerParam:SCREEN-VALUE = cLanbutikerParam
         cFtpbutikerParam:SCREEN-VALUE = cFtpButiker.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerElogg wWin 
PROCEDURE KontrollerElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lFinnes AS LOGICAL    NO-UNDO.
  DO WITH FRAME FRAME-Para:
     IF FI-VareTilExp:BGCOLOR = ? AND (CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = "ArtBas" AND
                                               ELogg.EksterntSystem = "POS") OR 
                                          CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = "PakkeLinje" AND
                                          ELogg.EksterntSystem = "POS")) THEN DO:
         ASSIGN FI-VareTilExp:BGCOLOR = 12
                B-SendVare:SENSITIVE  = TRUE.
     END.
     IF FI-GruppeTilExp:BGCOLOR = ? AND CAN-FIND(FIRST ELogg WHERE (ELogg.TabellNavn = "Avdeling" OR 
                                                                   ELogg.TabellNavn = "HuvGr"    OR 
                                                                   ELogg.TabellNavn = "VarGr")   AND
                                                                   ELogg.EksterntSystem = "POS") THEN DO:
         ASSIGN FI-GruppeTilExp:BGCOLOR = 12
                B-SendGruppe:SENSITIVE  = TRUE.
     END.
     IF FI-LevBasTilExp:BGCOLOR = ? AND CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = "LevBas" AND
                                          ELogg.EksterntSystem = "POS") THEN DO:
         ASSIGN FI-LevBasTilExp:BGCOLOR = 12
                B-SendLevBas:SENSITIVE = TRUE.
     END.
     IF FI-ProdusentTilExp:BGCOLOR = ? AND CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = "Produsent" AND
                                          ELogg.EksterntSystem = "POS") THEN DO:
         ASSIGN FI-ProdusentTilExp:BGCOLOR = 12
                B-SendProdusent:SENSITIVE = TRUE.
     END.
     RUN FinnesIkkeOverforte.
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
             FI-GruppeDatoTid:BGCOLOR    = INT(STRING(FI-GruppeDatoTid:SCREEN-VALUE    = ipcDatoTid,"10/15"))
             FI-LevBasDatoTid:BGCOLOR    = INT(STRING(FI-LevBasDatoTid:SCREEN-VALUE    = ipcDatoTid,"10/15"))
             FI-ProdusentDatoTid:BGCOLOR   = INT(STRING(FI-ProdusentDatoTid:SCREEN-VALUE   = ipcDatoTid,"10/15")).
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
    FIND ELogg WHERE ELogg.TabellNavn     = cTabell AND
                     ELogg.EksterntSystem = "POS"   AND
                     ELogg.Verdier        = "ALLE" NO-ERROR. 
    IF NOT AVAIL ELogg THEN DO:
    CREATE ELogg.
    ASSIGN ELogg.TabellNavn     = cTabell
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = "ALLE".
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RUN KontrollerElogg.
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
  DEF VAR cLevbasFiler     AS CHAR  NO-UNDO.
  DEF VAR cKasValutaFiler AS CHAR  NO-UNDO.
  DEF VAR cVareFiler      AS CHAR  NO-UNDO.
  DEF VAR cMixFiler       AS CHAR  NO-UNDO.
  DEF VAR cAvdelingFiler AS CHAR  NO-UNDO.
  DEF VAR cHuvgrFiler    AS CHAR  NO-UNDO.
  DEF VAR cProdusentfiler      AS CHAR  NO-UNDO.
  DEF VAR iAntVarer       AS INTE  NO-UNDO.
  DEF VAR iAntPakkeLinjer AS INTE  NO-UNDO.
  DEF VAR iAntGruppe       AS INTE  NO-UNDO.
  DEF VAR iAntKasValuta   AS INTE  NO-UNDO.
  DEF VAR iAntAvdeling   AS INTE  NO-UNDO.
  DEF VAR iAntHuvGr     AS INTE  NO-UNDO.
  DEF VAR iAntLevBas      AS INTE  NO-UNDO.
  DEF VAR iAntProdusent      AS INTE  NO-UNDO.
  DEF VAR cDatoTid        AS CHAR  NO-UNDO.
  DEF VAR lOverfort       AS LOGI  NO-UNDO.
  DEF VAR cLoadedJpg      AS CHAR  NO-UNDO.
  DEF VAR cToolTip        AS CHAR  NO-UNDO.
  DEF VAR lSendError      AS LOGI  NO-UNDO.
  DEF VAR lTimerOff       AS LOGI  NO-UNDO.
  DEF VAR cFilNavn        AS CHAR  NO-UNDO.
  DEF VAR cFtpSendes      AS CHAR  NO-UNDO.
  DEF VAR cULOGfil        AS CHAR  NO-UNDO.
  ASSIGN lTimerOff = NOT chPSTimer:ENABLED.
  DO WITH FRAME FRAME-Para:
    DO WITH FRAME fMain:
        ASSIGN cLoadedJpg = IMAGE-1:IMAGE.
        IMAGE-1:LOAD-IMAGE(".\icon\yellowlight.jpg").
    END.
    {sww.i}
    ASSIGN cDatoTid = STRING(TIME,"HH:MM")
           cULOGfil = "C:\home\lindbak\kasse\ULOG".
    IF (lVare OR TG-VareAuto:CHECKED) AND (CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = "ArtBas" AND
         ELogg.EksterntSystem = "POS") OR CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = "PakkeLinje" AND
         ELogg.EksterntSystem = "POS")) THEN VARE: DO:
      IF lVare = FALSE AND lTimerOff THEN
        LEAVE VARE.
      RUN eksportRigalArtBas.p (INPUT cLanButiker,INPUT cFtpButiker,"\home\lindbak\sendes\",OUTPUT cVareFiler, OUTPUT cMixFiler,OUTPUT iAntVarer, OUTPUT iAntPakkeLinjer).
      ASSIGN FI-VareFiler:SCREEN-VALUE IN FRAME FRAME-Filinfo = cVareFiler
             FI-MixFiler:SCREEN-VALUE = cMixFiler
             FI-VareTilExp:BGCOLOR    = ?
             lVare                    = FALSE
             B-SendVare:SENSITIVE     = FALSE
             FI-AntVarer:SCREEN-VALUE = STRING(iAntVarer).
      ASSIGN cToolTip   = ""
             lSendError = FALSE.
      IF cVareFiler <> "" THEN DO:
        DO iCount = 1 TO NUM-ENTRIES(cVareFiler):
            IF NOT ENTRY(iCount,cVareFiler) MATCHES("*.txt") THEN
                NEXT.
            OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -s " + ENTRY(iCount,cVareFiler)).
            IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cVareFiler)) <> ? THEN
                ASSIGN lSendError = TRUE.
            ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
                   lOverfort      = TRUE
                   FI-VareDatoTid:SCREEN-VALUE = cDatoTid.
        END.
          setError(FI-VareError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
      END.
      IF cMixFiler <> "" THEN DO:
          DO iCount = 1 TO NUM-ENTRIES(cMixFiler):
              IF NOT ENTRY(iCount,cMixFiler) MATCHES("*.txt") THEN
                  NEXT.
/*               OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -s " + ENTRY(iCount,cMixFiler)). */
              IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cMixFiler)) <> ? THEN
                  ASSIGN lSendError = TRUE.
              ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
                     lOverfort      = TRUE
                     FI-MixDatoTid:SCREEN-VALUE = cDatoTid.
          END.
          setError(FI-MixError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
      END.
    END.
    IF (lGruppe OR TG-GruppeAuto:CHECKED) AND CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = "VarGr" AND
                                         ELogg.EksterntSystem = "POS") THEN GRUPPE: DO:
      IF lGruppe = FALSE AND lTimerOff THEN
          LEAVE GRUPPE.
      RUN eksportRigalGruppe.p (INPUT cLanButiker,INPUT cFtpButiker,"\home\lindbak\sendes\", OUTPUT cHgrFiler, OUTPUT iAntGruppe).
      ASSIGN FI-HgrFiler:SCREEN-VALUE = cHgrFiler
             FI-GruppeTilExp:BGCOLOR   = ?
             lGruppe                   = FALSE
             B-SendGruppe:SENSITIVE    = FALSE
             FI-AntGruppe:SCREEN-VALUE = STRING(iAntGruppe)
              .
      IF cHgrFiler <> "" THEN DO:
          DO iCount = 1 TO NUM-ENTRIES(cHgrFiler):
              IF NOT ENTRY(iCount,cHgrFiler) MATCHES("*.txt") THEN
                  NEXT.
/*               OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -g " + ENTRY(iCount,cHgrFiler)). */
              IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cHgrFiler)) <> ? THEN
                  ASSIGN lSendError = TRUE.
              ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
                     lOverfort      = TRUE
                     FI-GruppeDatoTid:SCREEN-VALUE = cDatoTid.
          END.
          setError(FI-GruppeError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
      END.
    END.
    IF (lLevBas OR TG-LevBasAuto:CHECKED) AND CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = "LevBas" AND
                 ELogg.EksterntSystem = "POS") THEN LEVBAS: DO:
      IF lLevBas = FALSE AND lTimerOff THEN
          LEAVE LEVBAS.
      RUN eksportRigalLevBas.p (INPUT cLanButiker,INPUT cFtpButiker,"\home\lindbak\sendes\", OUTPUT cLevbasFiler, OUTPUT iAntLevBas).
      ASSIGN FI-LevBasFiler:SCREEN-VALUE = cLevbasFiler
             FI-LevBasTilExp:BGCOLOR     = ?
             lLevBas                     = FALSE
             B-SendLevBas:SENSITIVE      = FALSE
             FI-AntLevBas:SCREEN-VALUE   = STRING(iAntLevBas).
      IF cLevbasFiler <> "" THEN DO:
          DO iCount = 1 TO NUM-ENTRIES(cLevbasFiler):
              IF NOT ENTRY(iCount,cLevbasFiler) MATCHES("*.txt") THEN
                  NEXT.
/*               OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -k " + ENTRY(iCount,cLevbasFiler)). */
              IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cLevbasFiler)) <> ? THEN
                  ASSIGN lSendError = TRUE.
              ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
                     lOverfort      = TRUE
                     FI-LevBasDatoTid:SCREEN-VALUE = cDatoTid.
          END.
          setError(FI-LevBasError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
      END.
    END.
    IF (lProdusent OR TG-ProdusentAuto:CHECKED) AND CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = "Produsent" AND
         ELogg.EksterntSystem = "POS") THEN PRODUSENT: DO:
      IF lProdusent = FALSE AND lTimerOff THEN
          LEAVE PRODUSENT.
      RUN eksportRigalProdusent.p (INPUT cLanButiker,INPUT cFtpButiker,"\home\lindbak\sendes\", OUTPUT cProdusentfiler, OUTPUT iAntProdusent).
      ASSIGN FI-ProdusentFiler:SCREEN-VALUE = cProdusentfiler
             FI-ProdusentTilExp:BGCOLOR   = ?
             lProdusent                   = FALSE
             B-SendProdusent:SENSITIVE    = FALSE
             FI-AntProdusent:SCREEN-VALUE = STRING(iAntProdusent).
      IF cProdusentfiler <> "" THEN DO:
          DO iCount = 1 TO NUM-ENTRIES(cProdusentfiler):
              IF NOT ENTRY(iCount,cProdusentfiler) MATCHES("*.txt") THEN
                  NEXT.
/*               OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -f " + ENTRY(iCount,cProdusentfiler)). */
              IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cProdusentfiler)) <> ? THEN
                  ASSIGN lSendError = TRUE.
              ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
                     lOverfort        = TRUE
                     FI-ProdusentDatoTid:SCREEN-VALUE = cDatoTid.
          END.
          setError(FI-ProdusentError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
      END.
    END.
/*     INPUT FROM OS-DIR("C:\home\lindbak\kasse") NO-ECHO.                                          */
/*     REPEAT:                                                                                      */
/*         SET cFilNavn FORMAT "X(50)".                                                             */
/*         FILE-INFO:FILE-NAME = "C:\home\lindbak\kasse\" + TRIM(cFilNavn).                         */
/*         IF FILE-INFO:FILE-TYPE BEGINS "F" AND NUM-ENTRIES(cFilNavn,".") = 2 AND                  */
/*             ENTRY(1,cFilNavn,".") <> "" AND CAN-DO(cFtpButiker,ENTRY(2,cFilNavn,".")) THEN DO:   */
/*             ASSIGN cFtpSendes = "c:\home\lindbak\sendes\" + cFilNavn.                            */
/*             IF SEARCH(cFtpSendes) = ? THEN                                                       */
/*                 OS-RENAME VALUE(FILE-INFO:FULL-PATHNAME) VALUE(cFtpSendes).                      */
/*             ELSE DO:                                                                             */
/*                 OS-COMMAND SILENT VALUE("TYPE " + FILE-INFO:FULL-PATHNAME + ">> " + cFtpSendes). */
/*                 OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).                                        */
/*             END.                                                                                 */
/*         END.                                                                                     */
/*     END.                                                                                         */
/*     INPUT CLOSE.                                                                                 */
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

/* ************************  Function Implementations ***************** */

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

