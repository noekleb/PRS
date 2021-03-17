&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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

DEF VAR ix           AS INT    NO-UNDO.
DEF VAR bOk          AS LOG    NO-UNDO.
DEF VAR hParent      AS HANDLE NO-UNDO.

DEF VAR cRowIdList        AS CHAR   NO-UNDO.
DEF VAR cIdList           AS CHAR   NO-UNDO.
DEF VAR cTillgButikker    AS CHAR   NO-UNDO.
DEF VAR cAlle             AS CHAR   NO-UNDO.
DEF VAR hParentHandle     AS HANDLE NO-UNDO.
DEFINE VARIABLE iType AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmUtvalg

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnStartDato FI-FraDAto FI-TilDato ~
B-AvdelingBlank B-HgBlank B-VgBlank BtnStart BtnDone B-Avdeling ~
btnStoppDato B-SokBut B-HuvGr B-VarGr B-LevNrBlank B-LevNr RECT-1 
&Scoped-Define DISPLAYED-OBJECTS cButikerIdList FI-FraDAto FI-Avdeling ~
FI-HuvGr FI-VarGr FI-TilDato FI-LevNr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Avdeling  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-AvdelingBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-HgBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-HuvGr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-LevNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-LevNrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SokBut 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-VarGr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-VgBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Avslutt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnStart AUTO-GO DEFAULT 
     LABEL "&Start generering" 
     SIZE 28 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnStartDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnStoppDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE cButikerIdList AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikkliste" 
     VIEW-AS FILL-IN 
     SIZE 74 BY 1 TOOLTIP "Filområde for lagring av ordrebekreftelse(r)" NO-UNDO.

DEFINE VARIABLE FI-Avdeling AS CHARACTER FORMAT "X(10)":U 
     LABEL "Avdeling" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraDAto AS DATE FORMAT "99/99/99":U 
     LABEL "Periode" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-HuvGr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Hovedgruppe" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VarGr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 7.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmUtvalg
     btnStartDato AT ROW 2.91 COL 34 NO-TAB-STOP 
     cButikerIdList AT ROW 1.86 COL 18 COLON-ALIGNED
     FI-FraDAto AT ROW 2.91 COL 18 COLON-ALIGNED HELP
          "Fra dato"
     FI-Avdeling AT ROW 3.86 COL 18 COLON-ALIGNED HELP
          "Avgrensning på avdeling"
     FI-HuvGr AT ROW 4.81 COL 18 COLON-ALIGNED HELP
          "Avgrensing på hovedgruppe"
     FI-VarGr AT ROW 5.76 COL 18 COLON-ALIGNED HELP
          "Avgrensning på varegruppe"
     FI-TilDato AT ROW 2.91 COL 36 COLON-ALIGNED HELP
          "Til dato" NO-LABEL
     B-AvdelingBlank AT ROW 3.86 COL 38.8
     B-HgBlank AT ROW 4.81 COL 38.8
     B-VgBlank AT ROW 5.76 COL 38.8
     BtnStart AT ROW 9.43 COL 4
     BtnDone AT ROW 9.33 COL 84.6
     B-Avdeling AT ROW 3.86 COL 34.2 NO-TAB-STOP 
     btnStoppDato AT ROW 2.91 COL 52 NO-TAB-STOP 
     B-SokBut AT ROW 1.86 COL 94.6 NO-TAB-STOP 
     B-HuvGr AT ROW 4.81 COL 34.2 NO-TAB-STOP 
     B-VarGr AT ROW 5.76 COL 34.2 NO-TAB-STOP 
     FI-LevNr AT ROW 6.71 COL 18 COLON-ALIGNED HELP
          "Avgrensning på varegruppe" WIDGET-ID 6
     B-LevNrBlank AT ROW 6.71 COL 38.8 WIDGET-ID 4
     B-LevNr AT ROW 6.71 COL 34.2 WIDGET-ID 2 NO-TAB-STOP 
     RECT-1 AT ROW 1.43 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 102.6 BY 9.91
         DEFAULT-BUTTON BtnDone.


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
         TITLE              = "Generering av suppleringsordre"
         HEIGHT             = 9.91
         WIDTH              = 102.6
         MAX-HEIGHT         = 23.33
         MAX-WIDTH          = 192.6
         VIRTUAL-HEIGHT     = 23.33
         VIRTUAL-WIDTH      = 192.6
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frmUtvalg
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN cButikerIdList IN FRAME frmUtvalg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Avdeling IN FRAME frmUtvalg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-HuvGr IN FRAME frmUtvalg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevNr IN FRAME frmUtvalg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VarGr IN FRAME frmUtvalg
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Generering av suppleringsordre */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Generering av suppleringsordre */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Avdeling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Avdeling C-Win
ON CHOOSE OF B-Avdeling IN FRAME frmUtvalg /* ... */
OR F10 OF FI-Avdeling
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "Avdeling;AvdelingNr;AvdelingNavn",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "AvdelingNr",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        ASSIGN
          FI-Avdeling:SCREEN-VALUE = IF cIdList = ""
                            THEN cAlle
                          ELSE "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-Avdeling     = IF cIdList = ""
                            THEN ""
                            ELSE REPLACE(cIdList,"|",",")
          FI-Avdeling:TOOLTIP = IF FI-Avdeling = "" THEN "" ELSE FI-Avdeling.
        IF FI-Avdeling <> "" THEN DO:
            APPLY "CHOOSE" TO B-HgBlank.
            APPLY "CHOOSE" TO B-VgBlank.
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
                   FI-Avdeling:BGCOLOR  = 11.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-Avdeling:BGCOLOR   = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-AvdelingBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AvdelingBlank C-Win
ON CHOOSE OF B-AvdelingBlank IN FRAME frmUtvalg /* Blank */
DO:
    IF FI-Avdeling:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-Avdeling:SCREEN-VALUE = cAlle
               FI-Avdeling              = "*"
               FI-Avdeling:TOOLTIP      = ""
               FI-Avdeling:BGCOLOR      = ?
               B-Avdeling:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-HgBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-HgBlank C-Win
ON CHOOSE OF B-HgBlank IN FRAME frmUtvalg /* Blank */
DO:
    IF FI-HuvGr:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-HuvGr:SCREEN-VALUE = cAlle
               FI-HuvGr              = "*"
               FI-HuvGr:TOOLTIP      = ""
               FI-HuvGr:BGCOLOR      = ?
               B-HuvGr:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-HuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-HuvGr C-Win
ON CHOOSE OF B-HuvGr IN FRAME frmUtvalg /* ... */
OR F10 OF FI-HuvGr
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "HuvGr;Hg;HgBeskr",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "Hg",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        ASSIGN
          FI-HuvGr:SCREEN-VALUE = IF cIdList = ""
                            THEN cAlle
                          ELSE "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-HuvGr     = IF cIdList = ""
                            THEN ""
                            ELSE REPLACE(cIdList,"|",",")
          FI-HuvGr:TOOLTIP = IF FI-Huvgr = "" THEN "" ELSE FI-HuvGr.
        IF FI-HuvGr <> "" THEN DO:
            APPLY "CHOOSE" TO B-AvdelingBlank.
            APPLY "CHOOSE" TO B-VgBlank.
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
               FI-HuvGr:BGCOLOR      = 11.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-HuvGr:BGCOLOR      = ?.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevNr C-Win
ON CHOOSE OF B-LevNr IN FRAME frmUtvalg /* ... */
OR F10 OF FI-LevNr
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "LevBas;LevNr;LevNamn",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "LevNr",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        ASSIGN
          FI-LevNr:SCREEN-VALUE = IF cIdList = ""
                            THEN cAlle
                          ELSE "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-LevNr     = IF cIdList = ""
                            THEN ""
                            ELSE REPLACE(cIdList,"|",",")
          FI-LevNr:TOOLTIP = IF FI-LevNr = "" THEN "" ELSE FI-LevNr.
        IF FI-LevNr <> "" THEN DO:
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
               FI-LevNr:BGCOLOR      = 11.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-LevNr:BGCOLOR      = ?.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevNrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevNrBlank C-Win
ON CHOOSE OF B-LevNrBlank IN FRAME frmUtvalg /* Blank */
DO:
    IF FI-LevNr:SCREEN-VALUE <> cAlle THEN 
    DO:
        ASSIGN FI-LevNr:SCREEN-VALUE = cAlle
               FI-LevNr              = "*"
               FI-LevNr:TOOLTIP      = ""
               FI-LevNr:BGCOLOR      = ?
               B-LevNr:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokBut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokBut C-Win
ON CHOOSE OF B-SokBut IN FRAME frmUtvalg /* ... */
OR F10 OF B-SokBut
DO:
    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "Butiker;Butik|Butikk;ButNamn|Beskrivelse",
                        "where CAN-DO('" + cTillgButikker + "',STRING(Butiker.Butik)) by Butiker.Butik",
                        INPUT-OUTPUT cRowIdList,
                        "Butik",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        ASSIGN
          cButikerIdList:SCREEN-VALUE = IF cIdList = ""
                            THEN cAlle
                            ELSE "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          cButikerIdList = IF cIdList = ""
                              THEN ""
                              ELSE REPLACE(cIdList,"|",",")
          cButikerIdList:TOOLTIP = IF cButikerIdList = "" THEN "" ELSE cButikerIdList.
        IF cButikerIdList <> "" THEN DO:
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
                   cButikerIdList:BGCOLOR  = 11.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               cButikerIdList:BGCOLOR   = ?.
     END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VarGr C-Win
ON CHOOSE OF B-VarGr IN FRAME frmUtvalg /* ... */
OR F10 OF FI-VarGr
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "VarGr;Vg;VgBeskr",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "Vg",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        ASSIGN
          FI-VarGr:SCREEN-VALUE = IF cIdList = ""
                            THEN cAlle
                          ELSE "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-VarGr     = IF cIdList = ""
                            THEN ""
                            ELSE REPLACE(cIdList,"|",",")
          FI-VarGr:TOOLTIP = IF FI-VarGr = "" THEN "" ELSE FI-VarGr.
        IF FI-VarGr <> "" THEN DO:
            APPLY "CHOOSE" TO B-AvdelingBlank.
            APPLY "CHOOSE" TO B-HgBlank.
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
               FI-VarGr:BGCOLOR      = 11.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-VarGr:BGCOLOR      = ?.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VgBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VgBlank C-Win
ON CHOOSE OF B-VgBlank IN FRAME frmUtvalg /* Blank */
DO:
    IF FI-VarGr:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-VarGr:SCREEN-VALUE = cAlle
               FI-VarGr              = "*"
               FI-VarGr:TOOLTIP      = ""
               FI-VarGr:BGCOLOR      = ?
               B-VarGr:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME frmUtvalg /* Avslutt */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnStart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnStart C-Win
ON CHOOSE OF BtnStart IN FRAME frmUtvalg /* Start generering */
DO:
  RUN genSuplOrdre.
  APPLY 'CHOOSE' TO BtnDone.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStartDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStartDato C-Win
ON CHOOSE OF btnStartDato IN FRAME frmUtvalg /* ... */
OR "F10" OF FI-FraDato  
DO:
   RUN Cal.w (FI-FraDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStoppDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStoppDato C-Win
ON CHOOSE OF btnStoppDato IN FRAME frmUtvalg /* ... */
OR "F10" OF FI-TilDato
DO:
  RUN Cal.w (FI-TilDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FraDAto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FraDAto C-Win
ON LEAVE OF FI-FraDAto IN FRAME frmUtvalg /* Periode */
DO:
  IF INPUT FI-TilDato = ? THEN
      FI-TilDato:SCREEN-VALUE = FI-FraDato:SCREEN-VALUE.
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
ON CLOSE OF THIS-PROCEDURE DO:
    
    RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {lng.i}
  RUN enable_UI.
  
  {syspara.i 5 24 8 iType INT}
  {syspara.i 1 100 1 cAlle}
  
  IF iType = 1 THEN 
    ASSIGN 
        btnStartDato:SENSITIVE = FALSE  
        FI-FraDAto:SENSITIVE = FALSE  
        FI-TilDato:SENSITIVE = FALSE   
        btnStoppDato:SENSITIVE = FALSE 
        . 
  ASSIGN 
    cLogg = 'Suppleringsordre' + REPLACE(STRING(TODAY),'/','')
    .
  
  IF cAlle = "" THEN
      ASSIGN cAlle = "[Alle]".

  cTillgButikker = DYNAMIC-FUNCTION("getFieldList","Butiker;Butik","where Butiker.NedlagtDato = ?").
  cTillgButikker = REPLACE(cTillgButikker,"|",",").

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
  DISPLAY cButikerIdList FI-FraDAto FI-Avdeling FI-HuvGr FI-VarGr FI-TilDato 
          FI-LevNr 
      WITH FRAME frmUtvalg IN WINDOW C-Win.
  ENABLE btnStartDato FI-FraDAto FI-TilDato B-AvdelingBlank B-HgBlank B-VgBlank 
         BtnStart BtnDone B-Avdeling btnStoppDato B-SokBut B-HuvGr B-VarGr 
         B-LevNrBlank B-LevNr RECT-1 
      WITH FRAME frmUtvalg IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmUtvalg}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genSuplOrdre C-Win 
PROCEDURE genSuplOrdre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR bOk     AS LOG NO-UNDO.
  DEF VAR icParam AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      IF cButikerIdList:SCREEN-VALUE = "" THEN
      DO:
          MESSAGE "Det er ikke valgt noen butikker."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      IF itype <> 1 THEN 
      DO:
          IF (DATE(FI-TilDato:SCREEN-VALUE) < DATE(FI-FraDato:SCREEN-VALUE)) OR 
             DATE(FI-TilDato:SCREEN-VALUE) = ? OR
             DATE(FI-FraDato:SCREEN-VALUE) = ? THEN
          DO:
              MESSAGE "Ugyldig datoangivelse. " SKIP
                      "Fra dato må være mindre eller lik til dat." SKIP
                      "Både fra og til dato må være angitt."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN NO-APPLY.
          END.
      END.
      ASSIGN
          icParam = (IF NUM-ENTRIES(B-SokBut:PRIVATE-DATA,CHR(1)) = 2
                       THEN REPLACE(ENTRY(2,B-SokBut:PRIVATE-DATA,CHR(1)),"|",",") 
                       ELSE "") + "|" + 
                    (IF FI-FraDato:SCREEN-VALUE <> ? THEN FI-FraDato:SCREEN-VALUE ELSE '') + "|" + 
                    (IF FI-TilDato:SCREEN-VALUE <> ? THEN FI-TilDato:SCREEN-VALUE ELSE '') + "|" + 
                    (IF NUM-ENTRIES(B-Avdeling:PRIVATE-DATA,CHR(1)) = 2
                       THEN REPLACE(ENTRY(2,B-Avdeling:PRIVATE-DATA,CHR(1)),"|",",") 
                       ELSE "") + "|" +
                    (IF NUM-ENTRIES(B-HuvGr:PRIVATE-DATA,CHR(1)) = 2
                       THEN REPLACE(ENTRY(2,B-HuvGr:PRIVATE-DATA,CHR(1)),"|",",") 
                       ELSE "") + "|" +
                    (IF NUM-ENTRIES(B-VarGr:PRIVATE-DATA,CHR(1)) = 2
                       THEN REPLACE(ENTRY(2,B-VarGr:PRIVATE-DATA,CHR(1)),"|",",") 
                       ELSE "") + "|" +
                    (IF NUM-ENTRIES(B-LevNr:PRIVATE-DATA,CHR(1)) = 2
                       THEN REPLACE(ENTRY(2,B-LevNr:PRIVATE-DATA,CHR(1)),"|",",") 
                       ELSE "")
          .
      IF iType <> 1 THEN     
        bOk =  DYNAMIC-FUNCTION('runProc','pllisteordre_linje_nullstill_translogg.p',icParam,?).

      RUN bibl_loggDbFri.p (cLogg,'Starter manuell generering for butikker: ' + cButikerIdList:SCREEN-VALUE + '.' ).
      bok =  DYNAMIC-FUNCTION('runProc','pllisteordre_linje_generer.p',icParam,?).
      RUN bibl_loggDbFri.p (cLogg,'Ferdig manuell generering' + '.' ).
      
      IF VALID-HANDLE(hParentHandle) THEN
          RUN runOpenQueryInBrowse IN hParentHandle.

/*       IF NOT bOk THEN MESSAGE DYNAMIC-FUNCTION("getTransactionMessage") */
/*                       VIEW-AS ALERT-BOX INFO BUTTONS OK.                */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:MOVE-TO-TOP().

DO WITH FRAME frmUtvalg:
    APPLY "entry" TO FI-FraDato.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setParentHandle C-Win 
PROCEDURE setParentHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER phParentHandle AS HANDLE NO-UNDO.

  ASSIGN
      hParentHandle = phParentHandle
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME {&FRAME-NAME}:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

