&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME strAnalyse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS strAnalyse 
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
DEF VAR cButikerIdList      AS CHAR NO-UNDO.
DEF VAR cButikerRowIdList   AS CHAR NO-UNDO.
DEF VAR cAvdelingRowIdList  AS CHAR NO-UNDO.
DEF VAR cAvdelingIdList     AS CHAR NO-UNDO.
DEF VAR cHuvGrRowIdList     AS CHAR NO-UNDO.
DEF VAR cHuvGrIdList        AS CHAR NO-UNDO.
DEF VAR cVarGrRowIdList     AS CHAR NO-UNDO.
DEF VAR cVarGrIdList        AS CHAR NO-UNDO.
DEF VAR cSaSongRowIdList    AS CHAR NO-UNDO.
DEF VAR cSaSongIdList       AS CHAR NO-UNDO.
DEF VAR cVaremerkeRowIdList AS CHAR NO-UNDO.
DEF VAR cVaremerkeIdList    AS CHAR NO-UNDO.
DEF VAR cLevBasRowIdList    AS CHAR NO-UNDO.
DEF VAR cLevBasIdList       AS CHAR NO-UNDO.
DEF VAR cToggelLst             AS CHAR NO-UNDO.
DEF VAR bOk                 AS LOG  NO-UNDO.
DEF VAR cUtFilNavn          AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnCalFraDato FraDato TilDato ~
btnUtvalgButiker-2 AntButikker T1 btnUtvalgAvdeling fi-cListAvdeling T2 ~
btnUtvalgHuvGr fi-cListHuvGr T3 btnUtvalgVarGr fi-cListVarGr T4 ~
btnUtvalgSaSong fi-cListSaSong T5 btnUtvalgVaremerke fi-cListVaremerke T6 ~
btnUtvalgLevBas fi-cListLevBas T7 BtnDone-2 BtnDone btnCalTilDato 
&Scoped-Define DISPLAYED-OBJECTS FraDato TilDato AntButikker T1 ~
fi-cListAvdeling T2 fi-cListHuvGr T3 fi-cListVarGr T4 fi-cListSaSong T5 ~
fi-cListVaremerke T6 fi-cListLevBas T7 FILL-IN-2 FILL-IN-3 FILL-IN-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR strAnalyse AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalFraDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalTilDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Avslutt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnDone-2 DEFAULT 
     LABEL "&Start" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnUtvalgAvdeling 
     LABEL "Avdeling..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgButiker-2 
     LABEL "Butikk ..." 
     SIZE 15 BY 1 TOOLTIP "Hvis butikk(liste) ikke angis sjekkes kun lagertall".

DEFINE BUTTON btnUtvalgHuvGr 
     LABEL "Hovedgr..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgLevBas 
     LABEL "Leverandør..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgSaSong 
     LABEL "Sesong..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgVaremerke 
     LABEL "Varemerke..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgVarGr 
     LABEL "Varegr..." 
     SIZE 15 BY 1.14.

DEFINE VARIABLE AntButikker AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 12 BY 1 TOOLTIP "Antall butikker valgt" NO-UNDO.

DEFINE VARIABLE fi-cListAvdeling AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListHuvGr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListLevBas AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListSaSong AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListVaremerke AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListVarGr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Vis element" 
      VIEW-AS TEXT 
     SIZE 29 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Periode" 
      VIEW-AS TEXT 
     SIZE 36 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Filter" 
      VIEW-AS TEXT 
     SIZE 26 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FraDato AS DATE FORMAT "99/99/99":U 
     LABEL "Fra / til dato" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 TOOLTIP "Periode det skal hentes data fra." NO-UNDO.

DEFINE VARIABLE TilDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 TOOLTIP "Periode det skal hentes data fra." NO-UNDO.

DEFINE VARIABLE T1 AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE T2 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE T3 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE T4 AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE T5 AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE T6 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE T7 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnCalFraDato AT ROW 2.86 COL 35 WIDGET-ID 14
     FraDato AT ROW 2.86 COL 19 COLON-ALIGNED
     TilDato AT ROW 2.86 COL 37.4 COLON-ALIGNED NO-LABEL
     btnUtvalgButiker-2 AT ROW 5.24 COL 21 WIDGET-ID 8
     AntButikker AT ROW 5.24 COL 34.2 COLON-ALIGNED NO-LABEL
     T1 AT ROW 5.38 COL 50 WIDGET-ID 52
     btnUtvalgAvdeling AT ROW 6.33 COL 21 WIDGET-ID 20
     fi-cListAvdeling AT ROW 6.38 COL 34.2 COLON-ALIGNED NO-LABEL
     T2 AT ROW 6.52 COL 50 WIDGET-ID 54
     btnUtvalgHuvGr AT ROW 7.52 COL 21 WIDGET-ID 22
     fi-cListHuvGr AT ROW 7.57 COL 34.4 COLON-ALIGNED NO-LABEL
     T3 AT ROW 7.71 COL 50 WIDGET-ID 56
     btnUtvalgVarGr AT ROW 8.71 COL 21 WIDGET-ID 24
     fi-cListVarGr AT ROW 8.76 COL 34.4 COLON-ALIGNED NO-LABEL
     T4 AT ROW 8.91 COL 50 WIDGET-ID 58
     btnUtvalgSaSong AT ROW 9.95 COL 21 WIDGET-ID 32
     fi-cListSaSong AT ROW 9.95 COL 34.2 COLON-ALIGNED NO-LABEL
     T5 AT ROW 10.1 COL 50 WIDGET-ID 60
     btnUtvalgVaremerke AT ROW 11.14 COL 21 WIDGET-ID 36
     fi-cListVaremerke AT ROW 11.19 COL 34.2 COLON-ALIGNED NO-LABEL
     T6 AT ROW 11.33 COL 50 WIDGET-ID 62
     btnUtvalgLevBas AT ROW 12.33 COL 21 WIDGET-ID 40
     fi-cListLevBas AT ROW 12.38 COL 34.2 COLON-ALIGNED NO-LABEL
     T7 AT ROW 12.52 COL 50 WIDGET-ID 64
     BtnDone-2 AT ROW 15.52 COL 2 WIDGET-ID 18
     BtnDone AT ROW 15.52 COL 65 WIDGET-ID 12
     btnCalTilDato AT ROW 2.86 COL 53.4 WIDGET-ID 16
     FILL-IN-2 AT ROW 1.95 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     FILL-IN-3 AT ROW 4.43 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     FILL-IN-1 AT ROW 4.43 COL 48 COLON-ALIGNED NO-LABEL WIDGET-ID 66
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16
         DEFAULT-BUTTON BtnDone-2 WIDGET-ID 100.


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
  CREATE WINDOW strAnalyse ASSIGN
         HIDDEN             = YES
         TITLE              = "Data utvalg størrelsesanalyse"
         HEIGHT             = 16
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
/* SETTINGS FOR WINDOW strAnalyse
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       AntButikker:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       btnUtvalgButiker-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       fi-cListAvdeling:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fi-cListHuvGr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fi-cListLevBas:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fi-cListSaSong:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fi-cListVaremerke:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fi-cListVarGr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(strAnalyse)
THEN strAnalyse:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME strAnalyse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL strAnalyse strAnalyse
ON END-ERROR OF strAnalyse /* Data utvalg størrelsesanalyse */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL strAnalyse strAnalyse
ON WINDOW-CLOSE OF strAnalyse /* Data utvalg størrelsesanalyse */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato strAnalyse
ON CHOOSE OF btnCalFraDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF FraDato
DO:
  RUN Cal.w (FraDato:HANDLE).
  IF TilDato:SCREEN-VALUE = ? OR TRIM(TilDato:SCREEN-VALUE) = '/  /' THEN
      TilDato:SCREEN-VALUE = FraDato:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalTilDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalTilDato strAnalyse
ON CHOOSE OF btnCalTilDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF Tildato
DO:
  RUN Cal.w (TilDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone strAnalyse
ON CHOOSE OF BtnDone IN FRAME DEFAULT-FRAME /* Avslutt */
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


&Scoped-define SELF-NAME BtnDone-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone-2 strAnalyse
ON CHOOSE OF BtnDone-2 IN FRAME DEFAULT-FRAME /* Start */
DO:
  IF DATE(FraDato:SCREEN-VALUE) > DATE(TilDato:SCREEN-VALUE) THEN
  DO:
      MESSAGE 'Ugyldig datoangivelse.'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  IF DATE(FraDato:SCREEN-VALUE) = ? OR DATE(TilDato:SCREEN-VALUE) = ? THEN
  DO:
      MESSAGE 'Fra og til dato må angis.'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  IF cButikerIdList = '' THEN
  DO:
      MESSAGE 'Butikk må være angitt.' 
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  ASSIGN
      cToggelLst = STRING(T1:SCREEN-VALUE) + ',' +
                   STRING(T2:SCREEN-VALUE) + ',' +
                   STRING(T3:SCREEN-VALUE) + ',' +
                   STRING(T4:SCREEN-VALUE) + ',' +
                   STRING(T5:SCREEN-VALUE) + ',' +
                   STRING(T6:SCREEN-VALUE) + ',' +
                   STRING(T7:SCREEN-VALUE)
                   .
  RUN strAnalyse.p 
          (REPLACE(cButikerIdList,'|',','),
           DATE(FraDato:SCREEN-VALUE),
           DATE(TilDato:SCREEN-VALUE),
           REPLACE(cAvdelingIdList,'|',','),
           REPLACE(cHuvGrIdList,'|',','),  
           REPLACE(cVarGrIdList,'|',','),  
           REPLACE(cSaSongIdList,'|',','), 
           REPLACE(cVaremerkeIdList,'|',','),  
           REPLACE(cLevBasIdList,'|',','),
           cToggelLst,
           OUTPUT cUtFilNavn
          ).

  MESSAGE 'Data eksportert til : ' + cUtFilNavn
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

/*   &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN */
/*     &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN      */
/*       RUN dispatch IN THIS-PROCEDURE ('exit').  */
/*     &ELSE                                       */
/*       RUN exitObject.                           */
/*     &ENDIF                                      */
/*   &ELSE                                         */
/*       APPLY "CLOSE":U TO THIS-PROCEDURE.        */
/*   &ENDIF                                        */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgAvdeling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgAvdeling strAnalyse
ON CHOOSE OF btnUtvalgAvdeling IN FRAME DEFAULT-FRAME /* Avdeling... */
DO:

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Avdeling;AvdelingNr;AvdelingNavn",
                      "where true",
                      INPUT-OUTPUT cAvdelingRowIdList,
                      "AvdelingNr",
                      INPUT-OUTPUT cAvdelingIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN 
      ASSIGN 
          fi-cListAvdeling:SCREEN-VALUE = STRING(NUM-ENTRIES(cAvdelingIdList,'|'))
          .
  ELSE ASSIGN 
          fi-cListAvdeling:SCREEN-VALUE = ''
          .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgButiker-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgButiker-2 strAnalyse
ON CHOOSE OF btnUtvalgButiker-2 IN FRAME DEFAULT-FRAME /* Butikk ... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Butiker;Butik;ButNamn",
                      "where true",
                      INPUT-OUTPUT cButikerRowIdList,
                      "Butik",
                      INPUT-OUTPUT cButikerIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN 
      ASSIGN 
          AntButikker:SCREEN-VALUE = STRING(NUM-ENTRIES(cButikerIdList,'|'))
          .
  ELSE ASSIGN 
          AntButikker:SCREEN-VALUE = '0'
          .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgHuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgHuvGr strAnalyse
ON CHOOSE OF btnUtvalgHuvGr IN FRAME DEFAULT-FRAME /* Hovedgr... */
DO:

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "HuvGr;Hg;HgBeskr,Avdeling;AvdelingNr;AvdelingNavn",
                      "where true, first Avdeling OF HuvGr NO-LOCK " + 
                         (IF cAvdelingRowIdList NE "" THEN
                            "WHERE CAN-DO('" + cAvdelingRowIdList + "',STRING(ROWID(Avdeling)))"
                          ELSE ""),
                      INPUT-OUTPUT cHuvGrRowIdList,
                      "Hg",
                      INPUT-OUTPUT cHuvGrIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN 
      ASSIGN 
          fi-cListHuvGr:SCREEN-VALUE = STRING(NUM-ENTRIES(cHuvGrIdList,'|'))
          .
  ELSE ASSIGN 
          fi-cListHuvGr:SCREEN-VALUE = ''
          .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgLevBas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgLevBas strAnalyse
ON CHOOSE OF btnUtvalgLevBas IN FRAME DEFAULT-FRAME /* Leverandør... */
DO:
    
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "LevBas;levnr;levnamn;KjedeAvtale|Kjedeavtale|J/N",
                      "where true",
                      INPUT-OUTPUT cLevBasRowIdList,
                      "Levnr",
                      INPUT-OUTPUT cLevBasIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN 
      ASSIGN 
          fi-cListLevBas:SCREEN-VALUE = STRING(NUM-ENTRIES(cLevBasIdList,'|'))
          .
  ELSE ASSIGN 
          fi-cListLevBas:SCREEN-VALUE = ''
          .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgSaSong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgSaSong strAnalyse
ON CHOOSE OF btnUtvalgSaSong IN FRAME DEFAULT-FRAME /* Sesong... */
DO:

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "SaSong;Sasong;SasBeskr",
                      "where true",
                      INPUT-OUTPUT cSaSongRowIdList,
                      "Sasong",
                      INPUT-OUTPUT cSaSongIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN 
      ASSIGN 
          fi-cListSaSong:SCREEN-VALUE = STRING(NUM-ENTRIES(cSaSongIdList,'|'))
          .
  ELSE ASSIGN 
          fi-cListSaSong:SCREEN-VALUE = ''
          .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgVaremerke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgVaremerke strAnalyse
ON CHOOSE OF btnUtvalgVaremerke IN FRAME DEFAULT-FRAME /* Varemerke... */
DO:

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Varemerke;VMid;Beskrivelse",
                      "where true",
                      INPUT-OUTPUT cVaremerkeRowIdList,
                      "VMid",
                      INPUT-OUTPUT cVaremerkeIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN 
      ASSIGN 
          fi-cListVaremerke:SCREEN-VALUE = STRING(NUM-ENTRIES(cVaremerkeIdList,'|'))
          .
  ELSE ASSIGN 
          fi-cListVaremerke:SCREEN-VALUE = ''
          .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgVarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgVarGr strAnalyse
ON CHOOSE OF btnUtvalgVarGr IN FRAME DEFAULT-FRAME /* Varegr... */
DO:

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "VarGr;Vg;VgBeskr,HuvGr;Hg;HgBeskr",
                      "where true, first HuvGr OF VarGr NO-LOCK " + 
                         (IF cHuvGrRowIdList NE "" THEN
                            "WHERE CAN-DO('" + cHuvGrRowIdList + "',STRING(ROWID(HuvGr)))"
                          ELSE ""),
                      INPUT-OUTPUT cVarGrRowIdList,
                      "Vg",
                      INPUT-OUTPUT cVarGrIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN 
      ASSIGN 
          fi-cListVarGr:SCREEN-VALUE = STRING(NUM-ENTRIES(cVarGrIdList,'|'))
          .
  ELSE ASSIGN 
          fi-cListVarGr.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK strAnalyse 


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
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI strAnalyse  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(strAnalyse)
  THEN DELETE WIDGET strAnalyse.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI strAnalyse  _DEFAULT-ENABLE
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
  DISPLAY FraDato TilDato AntButikker T1 fi-cListAvdeling T2 fi-cListHuvGr T3 
          fi-cListVarGr T4 fi-cListSaSong T5 fi-cListVaremerke T6 fi-cListLevBas 
          T7 FILL-IN-2 FILL-IN-3 FILL-IN-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW strAnalyse.
  ENABLE btnCalFraDato FraDato TilDato btnUtvalgButiker-2 AntButikker T1 
         btnUtvalgAvdeling fi-cListAvdeling T2 btnUtvalgHuvGr fi-cListHuvGr T3 
         btnUtvalgVarGr fi-cListVarGr T4 btnUtvalgSaSong fi-cListSaSong T5 
         btnUtvalgVaremerke fi-cListVaremerke T6 btnUtvalgLevBas fi-cListLevBas 
         T7 BtnDone-2 BtnDone btnCalTilDato 
      WITH FRAME DEFAULT-FRAME IN WINDOW strAnalyse.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW strAnalyse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

