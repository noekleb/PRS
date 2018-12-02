&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
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

DEFINE TEMP-TABLE TT_Email NO-UNDO
    FIELD Emailadr AS CHAR FORMAT "x(50)" LABEL "Mailmottagare".
DEFINE TEMP-TABLE TT_EmailTag NO-UNDO
    FIELD Emailadr AS CHAR FORMAT "x(50)".

DEFINE VARIABLE cMottakEmail  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDatalagerEmail AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cMottagDir    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iFordrojDTL   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iFordrojXML   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iFordrojPRS   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iFordrojI80   AS INTEGER    NO-UNDO.
DEFINE VARIABLE lVisaOK       AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cDatalagerDir AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lSendMottakEmail AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lSendDatalagerEmail AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cErrorlogDir AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lSendErrorlogEmail AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cErrorlogEmail AS CHARACTER  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrTT_Email

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_Email

/* Definitions for BROWSE BrTT_Email                                    */
&Scoped-define FIELDS-IN-QUERY-BrTT_Email Emailadr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrTT_Email   
&Scoped-define SELF-NAME BrTT_Email
&Scoped-define QUERY-STRING-BrTT_Email FOR EACH TT_Email
&Scoped-define OPEN-QUERY-BrTT_Email OPEN QUERY {&SELF-NAME} FOR EACH TT_Email.
&Scoped-define TABLES-IN-QUERY-BrTT_Email TT_Email
&Scoped-define FIRST-TABLE-IN-QUERY-BrTT_Email TT_Email


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BrTT_Email}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-LagraNyEmailErrorlog RECT-2 RECT-3 RECT-4 ~
RECT-5 RECT-62 RECT-28 RECT-6 RECT-7 BrTT_Email RS-Rapporttyp B-Rapport ~
FI-MottagDir FI-FordrojDTL FI-FordrojPRS FI-FordrojI80 FI-FordrojXML ~
TG-VisaOK TG-SendMottakEmail FI-NyEmailMottag FI-SlettEmailMottag ~
FI-DatalagerDir TG-SendDatalagerEmail FI-NyEmailDatalager ~
FI-SlettEmailDatalager FI-ErrorlogDir TG-SendErrorlogEmail ~
FI-NyEmailErrorlog FI-SlettEmailErrorlog B-LagraNyEmailDatalager ~
B-AngreErrorlog B-SlettEmailErrorlog B-LagreErrorlog B-LagraNyEmailMottag ~
B-AngreDatalager B-SlettEmailDatalager B-SlettEmailMottak BUTTON-Ok ~
B-AngreMottak B-LagreDatalager B-LagreMottak FI-Mottagrubrik ~
FI-Datalagerrubrik FI-Errorlogrubrik 
&Scoped-Define DISPLAYED-OBJECTS RS-Rapporttyp FI-MottagDir FI-FordrojDTL ~
FI-FordrojPRS FI-FordrojI80 FI-FordrojXML TG-VisaOK TG-SendMottakEmail ~
FI-NyEmailMottag FI-SlettEmailMottag FI-DatalagerDir TG-SendDatalagerEmail ~
FI-NyEmailDatalager FI-SlettEmailDatalager FI-ErrorlogDir ~
TG-SendErrorlogEmail FI-NyEmailErrorlog FI-SlettEmailErrorlog ~
FI-Mottagrubrik FI-Datalagerrubrik FI-Errorlogrubrik 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-AngreDatalager 
     IMAGE-UP FILE "icon/reset.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ångra" 
     SIZE 4.57 BY 1.04 TOOLTIP "Angre (Alt-A)".

DEFINE BUTTON B-AngreErrorlog 
     IMAGE-UP FILE "icon/reset.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ångra" 
     SIZE 4.57 BY 1.04 TOOLTIP "Angre (Alt-A)".

DEFINE BUTTON B-AngreMottak 
     IMAGE-UP FILE "icon/reset.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ångra" 
     SIZE 4.57 BY 1.04 TOOLTIP "Angre (Alt-A)".

DEFINE BUTTON B-LagraNyEmailDatalager 
     IMAGE-UP FILE "icon/saverec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Lagra" 
     SIZE 4.57 BY 1.04 TOOLTIP "Lagre post (Alt-L)".

DEFINE BUTTON B-LagraNyEmailErrorlog 
     IMAGE-UP FILE "icon/saverec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Lagra" 
     SIZE 4.57 BY 1.04 TOOLTIP "Lagre post (Alt-L)".

DEFINE BUTTON B-LagraNyEmailMottag 
     IMAGE-UP FILE "icon/saverec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Lagra" 
     SIZE 4.57 BY 1.04 TOOLTIP "Lagre post (Alt-L)".

DEFINE BUTTON B-LagreDatalager 
     IMAGE-UP FILE "icon/saverec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Lagra" 
     SIZE 4.57 BY 1.04 TOOLTIP "Lagre post (Alt-L)".

DEFINE BUTTON B-LagreErrorlog 
     IMAGE-UP FILE "icon/saverec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Lagra" 
     SIZE 4.57 BY 1.04 TOOLTIP "Lagre post (Alt-L)".

DEFINE BUTTON B-LagreMottak 
     IMAGE-UP FILE "icon/saverec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Lagra" 
     SIZE 4.57 BY 1.04 TOOLTIP "Lagre post (Alt-L)".

DEFINE BUTTON B-Rapport 
     LABEL "Kör rapport" 
     SIZE 15 BY 1.15.

DEFINE BUTTON B-SlettEmailDatalager 
     IMAGE-UP FILE "icon/deleterec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Radera" 
     SIZE 4.57 BY 1.04 TOOLTIP "Slett post (Alt-D)".

DEFINE BUTTON B-SlettEmailErrorlog 
     IMAGE-UP FILE "icon/deleterec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Radera" 
     SIZE 4.57 BY 1.04 TOOLTIP "Slett post (Alt-D)".

DEFINE BUTTON B-SlettEmailMottak 
     IMAGE-UP FILE "icon/deleterec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Radera" 
     SIZE 4.57 BY 1.04 TOOLTIP "Slett post (Alt-D)".

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.57 BY 1.04 TOOLTIP "Lagre og avslutt".

DEFINE VARIABLE FI-DatalagerDir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rapportkatalog" 
     VIEW-AS FILL-IN 
     SIZE 54.57 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Datalagerrubrik AS CHARACTER FORMAT "X(256)":U INITIAL "  Datalager" 
      VIEW-AS TEXT 
     SIZE 15 BY .96
     BGCOLOR 11 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-ErrorlogDir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Errorlogkatalog" 
     VIEW-AS FILL-IN 
     SIZE 54.57 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Errorlogrubrik AS CHARACTER FORMAT "X(256)":U INITIAL "  Errorlog" 
      VIEW-AS TEXT 
     SIZE 15 BY .96
     BGCOLOR 11 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-FordrojDTL AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "DTL fördröjning" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FordrojI80 AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Infopos fördröjning" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FordrojPRS AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "PrsPOS fördröjning" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FordrojXML AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "XML fördröjning" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MottagDir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rapportkatalog" 
     VIEW-AS FILL-IN 
     SIZE 54.57 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Mottagrubrik AS CHARACTER FORMAT "X(256)":U INITIAL "  Mottagsrapport" 
      VIEW-AS TEXT 
     SIZE 20 BY .96
     BGCOLOR 11 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-NyEmailDatalager AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ny emailmottagare" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE FI-NyEmailErrorlog AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ny emailmottagare" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE FI-NyEmailMottag AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ny emailmottagare" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SlettEmailDatalager AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SlettEmailErrorlog AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SlettEmailMottag AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE RS-Rapporttyp AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Mottagsrapport", 1,
"Datalager", 2,
"Errorlog", 3
     SIZE 57.57 BY 1.42 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 11.65.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145.43 BY .12.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 8.58.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY .15.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY .15.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 8.58.

DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 2.15.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY .15.

DEFINE VARIABLE TG-SendDatalagerEmail AS LOGICAL INITIAL no 
     LABEL "Sänd email" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendErrorlogEmail AS LOGICAL INITIAL no 
     LABEL "Sänd email" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendMottakEmail AS LOGICAL INITIAL no 
     LABEL "Sänd email" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE TG-VisaOK AS LOGICAL INITIAL no 
     LABEL "Rapportera OK-butiker" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrTT_Email FOR 
      TT_Email SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrTT_Email
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrTT_Email C-Win _FREEFORM
  QUERY BrTT_Email DISPLAY
      Emailadr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 54 BY 23.62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-LagraNyEmailErrorlog AT ROW 33.81 COL 125.86 NO-TAB-STOP 
     BrTT_Email AT ROW 3.35 COL 3
     RS-Rapporttyp AT ROW 3.62 COL 62.43 NO-LABEL
     B-Rapport AT ROW 3.69 COL 122.14
     FI-MottagDir AT ROW 8.46 COL 79.14 COLON-ALIGNED
     FI-FordrojDTL AT ROW 9.62 COL 79.14 COLON-ALIGNED
     FI-FordrojPRS AT ROW 9.62 COL 105.14 COLON-ALIGNED
     FI-FordrojI80 AT ROW 10.73 COL 105.14 COLON-ALIGNED
     FI-FordrojXML AT ROW 10.77 COL 79.14 COLON-ALIGNED
     TG-VisaOK AT ROW 11.92 COL 81.14
     TG-SendMottakEmail AT ROW 13.04 COL 81.14
     FI-NyEmailMottag AT ROW 14.77 COL 79.14 COLON-ALIGNED
     FI-SlettEmailMottag AT ROW 15.92 COL 79.14 COLON-ALIGNED NO-LABEL
     FI-DatalagerDir AT ROW 20.92 COL 79.14 COLON-ALIGNED
     TG-SendDatalagerEmail AT ROW 22.46 COL 81.14
     FI-NyEmailDatalager AT ROW 24.19 COL 79.14 COLON-ALIGNED
     FI-SlettEmailDatalager AT ROW 25.42 COL 79.14 COLON-ALIGNED NO-LABEL
     FI-ErrorlogDir AT ROW 30.42 COL 79 COLON-ALIGNED
     TG-SendErrorlogEmail AT ROW 32 COL 81
     FI-NyEmailErrorlog AT ROW 33.69 COL 79 COLON-ALIGNED
     FI-SlettEmailErrorlog AT ROW 34.96 COL 79 COLON-ALIGNED NO-LABEL
     B-LagraNyEmailDatalager AT ROW 24.31 COL 126 NO-TAB-STOP 
     B-AngreErrorlog AT ROW 28.81 COL 85.57 NO-TAB-STOP 
     B-SlettEmailErrorlog AT ROW 35.04 COL 125.86 NO-TAB-STOP 
     B-LagreErrorlog AT ROW 28.81 COL 81 NO-TAB-STOP 
     B-LagraNyEmailMottag AT ROW 14.77 COL 126 NO-TAB-STOP 
     B-AngreDatalager AT ROW 19.31 COL 85.86 NO-TAB-STOP 
     B-SlettEmailDatalager AT ROW 25.54 COL 126 NO-TAB-STOP 
     B-SlettEmailMottak AT ROW 15.92 COL 126 NO-TAB-STOP 
     BUTTON-Ok AT ROW 1.31 COL 140.14 NO-TAB-STOP 
     B-AngreMottak AT ROW 7 COL 85.86 NO-TAB-STOP 
     B-LagreDatalager AT ROW 19.31 COL 81.14 NO-TAB-STOP 
     B-LagreMottak AT ROW 7 COL 81.14 NO-TAB-STOP 
     FI-Mottagrubrik AT ROW 5.58 COL 83.57 COLON-ALIGNED NO-LABEL
     FI-Datalagerrubrik AT ROW 17.92 COL 83.14 COLON-ALIGNED NO-LABEL
     FI-Errorlogrubrik AT ROW 27.42 COL 83 COLON-ALIGNED NO-LABEL
     RECT-2 AT ROW 6.04 COL 59.14
     RECT-3 AT ROW 18.38 COL 59.14
     RECT-4 AT ROW 14.38 COL 59.14
     RECT-5 AT ROW 23.92 COL 59.14
     RECT-62 AT ROW 3.23 COL 59.14
     RECT-28 AT ROW 2.42 COL 1
     RECT-6 AT ROW 27.92 COL 59
     RECT-7 AT ROW 33.42 COL 59
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 145.4 BY 37.48.


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
         TITLE              = "Rapportadministration"
         HEIGHT             = 37.46
         WIDTH              = 145.43
         MAX-HEIGHT         = 37.46
         MAX-WIDTH          = 152.86
         VIRTUAL-HEIGHT     = 37.46
         VIRTUAL-WIDTH      = 152.86
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BrTT_Email RECT-7 DEFAULT-FRAME */
ASSIGN 
       FI-SlettEmailDatalager:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-SlettEmailErrorlog:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-SlettEmailMottag:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrTT_Email
/* Query rebuild information for BROWSE BrTT_Email
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_Email.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BrTT_Email */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Rapportadministration */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Rapportadministration */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-AngreDatalager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AngreDatalager C-Win
ON CHOOSE OF B-AngreDatalager IN FRAME DEFAULT-FRAME /* Ångra */
DO:
    RUN InitScreenValues ("2").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-AngreErrorlog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AngreErrorlog C-Win
ON CHOOSE OF B-AngreErrorlog IN FRAME DEFAULT-FRAME /* Ångra */
DO:
    RUN InitScreenValues ("3").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-AngreMottak
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AngreMottak C-Win
ON CHOOSE OF B-AngreMottak IN FRAME DEFAULT-FRAME /* Ångra */
DO:
    RUN InitScreenValues ("1").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LagraNyEmailDatalager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LagraNyEmailDatalager C-Win
ON CHOOSE OF B-LagraNyEmailDatalager IN FRAME DEFAULT-FRAME /* Lagra */
DO:
    IF TRIM(FI-NyEmailDatalager:SCREEN-VALUE) <> "" AND NOT CAN-DO(REPLACE(cDatalagerEmail,";",","),TRIM(FI-NyEmailDatalager:SCREEN-VALUE)) THEN DO:
        cDatalagerEmail = cDatalagerEmail + ";" + FI-NyEmailDatalager:SCREEN-VALUE.
        RUN UpdatePara (210,251,10,cDatalagerEmail).
        RUN FriskaUppBrowser.
    END.
    FI-NyEmailDatalager:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LagraNyEmailErrorlog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LagraNyEmailErrorlog C-Win
ON CHOOSE OF B-LagraNyEmailErrorlog IN FRAME DEFAULT-FRAME /* Lagra */
DO:
    IF TRIM(FI-NyEmailErrorlog:SCREEN-VALUE) <> "" AND NOT CAN-DO(REPLACE(cErrorlogEmail,";",","),TRIM(FI-NyEmailErrorlog:SCREEN-VALUE)) THEN DO:
        cErrorlogEmail = cErrorlogEmail + ";" + FI-NyEmailErrorlog:SCREEN-VALUE.
        RUN UpdatePara (210,252,10,cErrorlogEmail).
        RUN FriskaUppBrowser.
    END.
    FI-NyEmailErrorlog:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LagraNyEmailMottag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LagraNyEmailMottag C-Win
ON CHOOSE OF B-LagraNyEmailMottag IN FRAME DEFAULT-FRAME /* Lagra */
DO:
    IF TRIM(FI-NyEmailMottag:SCREEN-VALUE) <> "" AND NOT CAN-DO(REPLACE(cMottakEmail,";",","),TRIM(FI-NyEmailMottag:SCREEN-VALUE)) THEN DO:
        cMottakEmail = cMottakEmail + ";" + FI-NyEmailMottag:SCREEN-VALUE.
        RUN UpdatePara (210,250,10,cMottakEmail).
        RUN FriskaUppBrowser.
    END.
    FI-NyEmailMottag:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LagreDatalager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LagreDatalager C-Win
ON CHOOSE OF B-LagreDatalager IN FRAME DEFAULT-FRAME /* Lagra */
DO:
    IF FI-DatalagerDir:MODIFIED THEN DO:
        ASSIGN cDatalagerDir = FI-DatalagerDir:SCREEN-VALUE.
        RUN UpdatePara (210,251,1,cDatalagerDir).    
    END.
    IF TG-SendDatalagerEmail:MODIFIED THEN DO:
        ASSIGN lSendDatalagerEmail = TG-SendDatalagerEmail:CHECKED.
        RUN UpdatePara (210,251,5,STRING(lSendDatalagerEmail,"J/N")).    
    END.
    RUN InitScreenValues ("2").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LagreErrorlog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LagreErrorlog C-Win
ON CHOOSE OF B-LagreErrorlog IN FRAME DEFAULT-FRAME /* Lagra */
DO:
    IF FI-ErrorlogDir:MODIFIED THEN DO:
        ASSIGN cErrorlogDir = FI-ErrorlogDir:SCREEN-VALUE.
        RUN UpdatePara (210,252,1,cErrorlogDir).    
    END.
    IF TG-SendErrorlogEmail:MODIFIED THEN DO:
        ASSIGN lSendErrorlogEmail = TG-SendErrorlogEmail:CHECKED.
        RUN UpdatePara (210,252,5,STRING(lSendErrorlogEmail,"J/N")).    
    END.
    RUN InitScreenValues ("3").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LagreMottak
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LagreMottak C-Win
ON CHOOSE OF B-LagreMottak IN FRAME DEFAULT-FRAME /* Lagra */
DO:
    IF FI-MottagDir:MODIFIED THEN DO:
        ASSIGN cMottagDir = FI-MottagDir:SCREEN-VALUE.
        RUN UpdatePara (210,250,1,cMottagDir).    
    END.
    IF FI-FordrojDTL:MODIFIED THEN DO:
        ASSIGN iFordrojDTL = INT(FI-FordrojDTL:SCREEN-VALUE).
        RUN UpdatePara (210,250,2,STRING(iFordrojDTL)).    
    END.
    IF FI-FordrojXML:MODIFIED THEN DO:
        ASSIGN iFordrojXML = INT(FI-FordrojXML:SCREEN-VALUE).
        RUN UpdatePara (210,250,3,STRING(iFordrojXML)).    
    END.
    IF TG-VisaOK:MODIFIED THEN DO:
        ASSIGN lVisaOK = TG-VisaOK:CHECKED.
        RUN UpdatePara (210,250,4,STRING(lVisaOK,"J/N")).    
    END.
    IF FI-FordrojPRS:MODIFIED THEN DO:
        ASSIGN iFordrojPRS = INT(FI-FordrojPRS:SCREEN-VALUE).
        RUN UpdatePara (210,250,12,STRING(iFordrojPRS)).    
    END.
    IF FI-FordrojI80:MODIFIED THEN DO:
        ASSIGN iFordrojI80 = INT(FI-FordrojI80:SCREEN-VALUE).
        RUN UpdatePara (210,250,13,STRING(iFordrojI80)).    
    END.
    IF TG-SendMottakEmail:MODIFIED THEN DO:
        ASSIGN lSendMottakEmail = TG-SendMottakEmail:CHECKED.
        RUN UpdatePara (210,250,5,STRING(lSendMottakEmail,"J/N")).    
    END.
    RUN InitScreenValues ("1").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Rapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Rapport C-Win
ON CHOOSE OF B-Rapport IN FRAME DEFAULT-FRAME /* Kör rapport */
DO:
    IF RS-Rapporttyp:SCREEN-VALUE = "1" THEN
        RUN Mottaksrapport.p PERSISTENT.
    ELSE IF RS-Rapporttyp:SCREEN-VALUE = "2" THEN
        RUN bfdagrapport.p PERSISTENT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SlettEmailDatalager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SlettEmailDatalager C-Win
ON CHOOSE OF B-SlettEmailDatalager IN FRAME DEFAULT-FRAME /* Radera */
DO:
    IF RS-Rapporttyp:SCREEN-VALUE = "2" AND BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN DO:
        DELETE TT_Email.
        cDatalagerEmail = "".
        FOR EACH TT_Email:
            cDatalagerEmail = cDatalagerEmail + (IF cDatalagerEmail <> "" THEN ";" ELSE "") + TT_Email.Emailadr.
        END.
        RUN UpdatePara (210,251,10,cDatalagerEmail).
        RUN FriskaUppBrowser.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SlettEmailErrorlog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SlettEmailErrorlog C-Win
ON CHOOSE OF B-SlettEmailErrorlog IN FRAME DEFAULT-FRAME /* Radera */
DO:
    IF RS-Rapporttyp:SCREEN-VALUE = "3" AND BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN DO:
        DELETE TT_Email.
        cErrorlogEmail = "".
        FOR EACH TT_Email:
            cErrorlogEmail = cErrorlogEmail + (IF cErrorlogEmail <> "" THEN ";" ELSE "") + TT_Email.Emailadr.
        END.
        RUN UpdatePara (210,252,10,cErrorlogEmail).
        RUN FriskaUppBrowser.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SlettEmailMottak
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SlettEmailMottak C-Win
ON CHOOSE OF B-SlettEmailMottak IN FRAME DEFAULT-FRAME /* Radera */
DO:
    IF RS-Rapporttyp:SCREEN-VALUE = "1" AND BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN DO:
        DELETE TT_Email.
        cMottakEmail = "".
        FOR EACH TT_Email:
            cMottakEmail = cMottakEmail + (IF cMottakEmail <> "" THEN ";" ELSE "") + TT_Email.Emailadr.
        END.
        RUN UpdatePara (210,250,10,cMottakEmail).
        RUN FriskaUppBrowser.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrTT_Email
&Scoped-define SELF-NAME BrTT_Email
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BrTT_Email C-Win
ON VALUE-CHANGED OF BrTT_Email IN FRAME DEFAULT-FRAME
DO:
    ASSIGN FI-SlettEmailMottag:SCREEN-VALUE = IF RS-Rapporttyp:SCREEN-VALUE = "1" AND AVAIL TT_Email THEN
                                                    TT_Email.Emailadr ELSE ""
           FI-SlettEmailDatalager:SCREEN-VALUE = IF RS-Rapporttyp:SCREEN-VALUE = "2" AND AVAIL TT_Email THEN
                                                    TT_Email.Emailadr ELSE "".
           FI-SlettEmailErrorlog:SCREEN-VALUE = IF RS-Rapporttyp:SCREEN-VALUE = "3" AND AVAIL TT_Email THEN
                                                    TT_Email.Emailadr ELSE "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-Win
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.  
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-DatalagerDir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DatalagerDir C-Win
ON VALUE-CHANGED OF FI-DatalagerDir IN FRAME DEFAULT-FRAME /* Rapportkatalog */
DO:
    RUN EnaDisButtons ("2",TRUE).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ErrorlogDir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ErrorlogDir C-Win
ON VALUE-CHANGED OF FI-ErrorlogDir IN FRAME DEFAULT-FRAME /* Errorlogkatalog */
DO:
    RUN EnaDisButtons ("3",TRUE).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FordrojDTL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FordrojDTL C-Win
ON VALUE-CHANGED OF FI-FordrojDTL IN FRAME DEFAULT-FRAME /* DTL fördröjning */
DO:
    RUN EnaDisButtons ("1",TRUE).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FordrojI80
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FordrojI80 C-Win
ON VALUE-CHANGED OF FI-FordrojI80 IN FRAME DEFAULT-FRAME /* Infopos fördröjning */
DO:
    RUN EnaDisButtons ("1",TRUE).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FordrojPRS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FordrojPRS C-Win
ON VALUE-CHANGED OF FI-FordrojPRS IN FRAME DEFAULT-FRAME /* PrsPOS fördröjning */
DO:
    RUN EnaDisButtons ("1",TRUE).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FordrojXML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FordrojXML C-Win
ON VALUE-CHANGED OF FI-FordrojXML IN FRAME DEFAULT-FRAME /* XML fördröjning */
DO:
    RUN EnaDisButtons ("1",TRUE).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-MottagDir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-MottagDir C-Win
ON VALUE-CHANGED OF FI-MottagDir IN FRAME DEFAULT-FRAME /* Rapportkatalog */
DO:
    RUN EnaDisButtons ("1",TRUE).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-Rapporttyp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Rapporttyp C-Win
ON VALUE-CHANGED OF RS-Rapporttyp IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE cRS AS CHARACTER  NO-UNDO.
    ASSIGN cRS = RS-Rapporttyp:SCREEN-VALUE
           B-LagraNyEmailMottag:SENSITIVE = cRS = "1"
           FI-FordrojDTL:SENSITIVE = cRS = "1"
           FI-FordrojXML:SENSITIVE = cRS = "1"
           FI-FordrojPRS:SENSITIVE = cRS = "1"
           FI-FordrojI80:SENSITIVE = cRS = "1"
           FI-MottagDir:SENSITIVE = cRS = "1"
           FI-Mottagrubrik:SENSITIVE = cRS = "1"
           FI-NyEmailMottag:SENSITIVE = cRS = "1"
           TG-VisaOK:SENSITIVE = cRS = "1"
           TG-SendMottakEmail:SENSITIVE = cRS = "1"
           B-SlettEmailMottak:SENSITIVE = cRS = "1"
           FI-DatalagerDir:SENSITIVE = cRS = "2"
           FI-NyEmailDatalager:SENSITIVE = cRS = "2"
           TG-SendDatalagerEmail:SENSITIVE = cRS = "2"
           B-LagraNyEmailDatalager:SENSITIVE = cRS = "2"
           B-SlettEmailDatalager:SENSITIVE = cRS = "2"
           FI-ErrorlogDir:SENSITIVE = cRS = "3"
           FI-NyEmailErrorlog:SENSITIVE = cRS = "3"
           TG-SendErrorlogEmail:SENSITIVE = cRS = "3"
           B-LagraNyEmailErrorlog:SENSITIVE = cRS = "3"
           B-SlettEmailErrorlog:SENSITIVE = cRS = "3"
           B-Rapport:SENSITIVE = cRS <> "3".
    RUN FriskaUppBrowser.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendDatalagerEmail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendDatalagerEmail C-Win
ON VALUE-CHANGED OF TG-SendDatalagerEmail IN FRAME DEFAULT-FRAME /* Sänd email */
DO:
    RUN EnaDisButtons ("2",TRUE).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendErrorlogEmail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendErrorlogEmail C-Win
ON VALUE-CHANGED OF TG-SendErrorlogEmail IN FRAME DEFAULT-FRAME /* Sänd email */
DO:
    RUN EnaDisButtons ("3",TRUE).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-SendMottakEmail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-SendMottakEmail C-Win
ON VALUE-CHANGED OF TG-SendMottakEmail IN FRAME DEFAULT-FRAME /* Sänd email */
DO:
    RUN EnaDisButtons ("1",TRUE).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-VisaOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-VisaOK C-Win
ON VALUE-CHANGED OF TG-VisaOK IN FRAME DEFAULT-FRAME /* Rapportera OK-butiker */
DO:
    RUN EnaDisButtons ("1",TRUE).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN InitFromSyspara.
  RUN enable_UI.
  APPLY "VALUE-CHANGED" TO RS-Rapporttyp.
  RUN InitScreenValues ("1,2,3").

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY RS-Rapporttyp FI-MottagDir FI-FordrojDTL FI-FordrojPRS FI-FordrojI80 
          FI-FordrojXML TG-VisaOK TG-SendMottakEmail FI-NyEmailMottag 
          FI-SlettEmailMottag FI-DatalagerDir TG-SendDatalagerEmail 
          FI-NyEmailDatalager FI-SlettEmailDatalager FI-ErrorlogDir 
          TG-SendErrorlogEmail FI-NyEmailErrorlog FI-SlettEmailErrorlog 
          FI-Mottagrubrik FI-Datalagerrubrik FI-Errorlogrubrik 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-LagraNyEmailErrorlog RECT-2 RECT-3 RECT-4 RECT-5 RECT-62 RECT-28 
         RECT-6 RECT-7 BrTT_Email RS-Rapporttyp B-Rapport FI-MottagDir 
         FI-FordrojDTL FI-FordrojPRS FI-FordrojI80 FI-FordrojXML TG-VisaOK 
         TG-SendMottakEmail FI-NyEmailMottag FI-SlettEmailMottag 
         FI-DatalagerDir TG-SendDatalagerEmail FI-NyEmailDatalager 
         FI-SlettEmailDatalager FI-ErrorlogDir TG-SendErrorlogEmail 
         FI-NyEmailErrorlog FI-SlettEmailErrorlog B-LagraNyEmailDatalager 
         B-AngreErrorlog B-SlettEmailErrorlog B-LagreErrorlog 
         B-LagraNyEmailMottag B-AngreDatalager B-SlettEmailDatalager 
         B-SlettEmailMottak BUTTON-Ok B-AngreMottak B-LagreDatalager 
         B-LagreMottak FI-Mottagrubrik FI-Datalagerrubrik FI-Errorlogrubrik 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnaDisButtons C-Win 
PROCEDURE EnaDisButtons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cType   AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER lEnable AS LOGICAL    NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        CASE cType:
            WHEN "1" THEN
                ASSIGN B-LagreMottak:SENSITIVE = lEnable
                       B-AngreMottak:SENSITIVE = lEnable.
            WHEN "2" THEN
                ASSIGN B-LagreDatalager:SENSITIVE = lEnable
                       B-AngreDatalager:SENSITIVE = lEnable.
            WHEN "3" THEN
                ASSIGN B-LagreErrorlog:SENSITIVE = lEnable
                       B-AngreErrorlog:SENSITIVE = lEnable.
        END CASE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FriskaUppBrowser C-Win 
PROCEDURE FriskaUppBrowser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
    IF RS-Rapporttyp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1" THEN DO:
        FOR EACH TT_Email:
            DELETE TT_Email.
        END.
        DO ii = 1 TO NUM-ENTRIES(cMottakEmail,";"):
            CREATE TT_Email.
            ASSIGN TT_Email.Emailadr = ENTRY(ii,cMottakEmail,";").
            RELEASE TT_Email.
        END.
    END.
    ELSE IF RS-Rapporttyp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "2" THEN DO:
        FOR EACH TT_Email:
            DELETE TT_Email.
        END.
        DO ii = 1 TO NUM-ENTRIES(cDatalagerEmail,";"):
            CREATE TT_Email.
            ASSIGN TT_Email.Emailadr = ENTRY(ii,cDatalagerEmail,";").
            RELEASE TT_Email.
        END.
    END.
    ELSE DO:
        FOR EACH TT_Email:
            DELETE TT_Email.
        END.
        DO ii = 1 TO NUM-ENTRIES(cErrorlogEmail,";"):
            CREATE TT_Email.
            ASSIGN TT_Email.Emailadr = ENTRY(ii,cErrorlogEmail,";").
            RELEASE TT_Email.
        END.
    END.
    {&OPEN-QUERY-{&BROWSE-NAME}}
    IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN
        APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
    APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitFromSysPara C-Win 
PROCEDURE InitFromSysPara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
FIND Syspara WHERE SysPara.SysHId         = 210 AND
                           SysPara.SysGr  = 250 AND
                           SysPara.ParaNr = 1   NO-LOCK NO-ERROR.
IF AVAIL SysPara THEN
    cMottagDir = Syspara.parameter1.

FIND Syspara WHERE SysPara.SysHId         = 210 AND
                           SysPara.SysGr  = 250 AND
                           SysPara.ParaNr = 2   NO-LOCK NO-ERROR.
IF AVAIL SysPara THEN
    iFordrojDTL = INT(Syspara.parameter1).

FIND Syspara WHERE SysPara.SysHId         = 210 AND
                           SysPara.SysGr  = 250 AND
                           SysPara.ParaNr = 3   NO-LOCK NO-ERROR.
IF AVAIL SysPara THEN
    iFordrojXML = INT(Syspara.parameter1).

FIND Syspara WHERE SysPara.SysHId         = 210 AND
                           SysPara.SysGr  = 250 AND
                           SysPara.ParaNr = 4   NO-LOCK NO-ERROR.
IF AVAIL SysPara THEN
    lVisaOK     = Syspara.parameter1 = "J".

FIND Syspara WHERE SysPara.SysHId         = 210 AND
                           SysPara.SysGr  = 250 AND
                           SysPara.ParaNr = 5   NO-LOCK NO-ERROR.
IF AVAIL SysPara THEN
    lSendMottakEmail = Syspara.parameter1 = "J".

FIND Syspara WHERE SysPara.SysHId         = 210 AND
                           SysPara.SysGr  = 250 AND
                           SysPara.ParaNr = 10  NO-LOCK NO-ERROR.
IF AVAIL SysPara THEN
    cMottakEmail = Syspara.parameter1.

FIND Syspara WHERE SysPara.SysHId         = 210 AND
                           SysPara.SysGr  = 250 AND
                           SysPara.ParaNr = 12   NO-LOCK NO-ERROR.
IF AVAIL SysPara THEN
    iFordrojPRS = INT(Syspara.parameter1).

FIND Syspara WHERE SysPara.SysHId         = 210 AND
                           SysPara.SysGr  = 250 AND
                           SysPara.ParaNr = 13   NO-LOCK NO-ERROR.
IF AVAIL SysPara THEN
    iFordrojI80 = INT(Syspara.parameter1).

FIND Syspara WHERE SysPara.SysHId         = 210 AND
                           SysPara.SysGr  = 251 AND
                           SysPara.ParaNr = 1   NO-LOCK NO-ERROR.
IF AVAIL SysPara THEN
    cDatalagerDir = Syspara.parameter1.

FIND Syspara WHERE SysPara.SysHId         = 210 AND
                           SysPara.SysGr  = 251 AND
                           SysPara.ParaNr = 10  NO-LOCK NO-ERROR.
IF AVAIL SysPara THEN
    cDatalagerEmail = Syspara.parameter1.

FIND Syspara WHERE SysPara.SysHId         = 210 AND
                           SysPara.SysGr  = 251 AND
                           SysPara.ParaNr = 5   NO-LOCK NO-ERROR.
IF AVAIL SysPara THEN
    lSendDatalagerEmail = Syspara.parameter1 = "J".

FIND Syspara WHERE SysPara.SysHId         = 210 AND
                           SysPara.SysGr  = 252 AND
                           SysPara.ParaNr = 1   NO-LOCK NO-ERROR.
IF AVAIL SysPara THEN
    cErrorlogDir = Syspara.parameter1.

FIND Syspara WHERE SysPara.SysHId         = 210 AND
                           SysPara.SysGr  = 252 AND
                           SysPara.ParaNr = 10  NO-LOCK NO-ERROR.
IF AVAIL SysPara THEN
    cErrorlogEmail = Syspara.parameter1.

FIND Syspara WHERE SysPara.SysHId         = 210 AND
                           SysPara.SysGr  = 252 AND
                           SysPara.ParaNr = 5   NO-LOCK NO-ERROR.
IF AVAIL SysPara THEN
    lSendErrorlogEmail = Syspara.parameter1 = "J".

/* DO ii = 1 TO NUM-ENTRIES(cMottakEmail):       */
/*     CREATE TT_Email.                          */
/*     ASSIGN Emailadr = ENTRY(ii,cMottakEmail). */
/*     RELEASE TT_Email.                         */
/* END.                                          */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitScreenValues C-Win 
PROCEDURE InitScreenValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cTypes AS CHARACTER  NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:
    IF CAN-DO(cTypes,"1") THEN DO:
        ASSIGN FI-MottagDir:SCREEN-VALUE      = cMottagDir
               FI-FordrojDTL:SCREEN-VALUE     = STRING(iFordrojDTL)
               FI-FordrojXML:SCREEN-VALUE     = STRING(iFordrojXML)
               FI-FordrojPRS:SCREEN-VALUE     = STRING(iFordrojPRS)
               FI-FordrojI80:SCREEN-VALUE     = STRING(iFordrojI80)
               TG-VisaOK:SCREEN-VALUE         = STRING(lVisaOK)
               TG-SendMottakEmail:SCREEN-VALUE = STRING(lSendMottakEmail).
        ASSIGN FI-MottagDir:MODIFIED     = FALSE
               FI-FordrojDTL:MODIFIED     = FALSE
               FI-FordrojXML:MODIFIED     = FALSE
               TG-VisaOK:MODIFIED     = FALSE
               TG-SendMottakEmail:MODIFIED     = FALSE.
        RUN EnaDisButtons("1",FALSE).
    END.
    IF CAN-DO(cTypes,"2") THEN DO:
        ASSIGN FI-DatalagerDir:SCREEN-VALUE = cDatalagerDir
               FI-DatalagerDir:MODIFIED     = FALSE
               TG-SendDatalagerEmail:SCREEN-VALUE = STRING(lSendDatalagerEmail)
               TG-SendDatalagerEmail:MODIFIED = FALSE.
        RUN EnaDisButtons("2",FALSE).
    END.
    IF CAN-DO(cTypes,"3") THEN DO:
        ASSIGN FI-ErrorlogDir:SCREEN-VALUE = cErrorlogDir
               FI-ErrorlogDir:MODIFIED     = FALSE
               TG-SendErrorlogEmail:SCREEN-VALUE = STRING(lSendErrorlogEmail)
               TG-SendErrorlogEmail:MODIFIED = FALSE.
        RUN EnaDisButtons("3",FALSE).
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendDatalagerPara C-Win 
PROCEDURE SendDatalagerPara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER cOutDir AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER lSMail AS LOGICAL    NO-UNDO.
    DEFINE OUTPUT PARAMETER cMAdr  AS CHARACTER  NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN cOutDir = RIGHT-TRIM(FI-DatalagerDir:SCREEN-VALUE,"\") + "\"
               lSMail  = TG-SendDatalagerEmail:CHECKED
               cMAdr   = TT_Email.Emailadr.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendMottakPara C-Win 
PROCEDURE SendMottakPara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER cOutDir AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER iDTL AS INTEGER    NO-UNDO.
    DEFINE OUTPUT PARAMETER iXML AS INTEGER    NO-UNDO.
    DEFINE OUTPUT PARAMETER iPRS AS INTEGER    NO-UNDO.
    DEFINE OUTPUT PARAMETER iI80 AS INTEGER    NO-UNDO.
    DEFINE OUTPUT PARAMETER lVis AS LOGICAL    NO-UNDO.
    DEFINE OUTPUT PARAMETER lSMail AS LOGICAL    NO-UNDO.
    DEFINE OUTPUT PARAMETER cMAdr  AS CHARACTER  NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN cOutDir = RIGHT-TRIM(FI-MottagDir:SCREEN-VALUE,"\") + "\"
               iDTL    = INT(FI-FordrojDTL:SCREEN-VALUE)
               iXML    = INT(FI-FordrojXML)
               iPRS    = INT(FI-FordrojPRS)
               iI80    = INT(FI-FordrojI80)
               lVis    = TG-VisaOK:CHECKED
               lSMail  = TG-SendMottakEmail:CHECKED
               cMAdr   = TT_Email.Emailadr.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdatePara C-Win 
PROCEDURE UpdatePara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iSysHId     AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER iSysGr      AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER iParaNr     AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER cParameter1 AS CHARACTER  NO-UNDO.

    FIND Syspara WHERE SysPara.SysHId = iSysHId AND
                       SysPara.SysGr  = iSysGr  AND
                       SysPara.ParaNr = iParaNr.
    Syspara.Parameter1 = cParameter1.
    RELEASE SysPara.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

