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
DEF INPUT  PARAM icType        AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocFieldList   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValueList   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocLevNr       AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR bOK           AS LOG NO-UNDO.
DEF VAR cMineButikker AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS ButikkListe btnSokProfilNr btnButikker ~
VarebehBeskrivelse EkstRef MesseNr ProfilNr VarebehNotat sokLevNr btnLev ~
sokLevNamn btnOk btnCancel btnSokMesseNr 
&Scoped-Define DISPLAYED-OBJECTS ButikkListe VarebehBeskrivelse EkstRef ~
MesseNr MesseBeskrivelse ProfilNr KortNavn VarebehNotat sokLevNr sokLevNamn 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnButikker 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnCancel 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnLev 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnOk 
     LABEL "&Lagre" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnSokMesseNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokProfilNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE VarebehNotat AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 60 BY 6.19 NO-UNDO.

DEFINE VARIABLE ButikkListe AS CHARACTER FORMAT "X(60)" 
     LABEL "Butikkliste" 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1.

DEFINE VARIABLE EkstRef AS CHARACTER FORMAT "X(60)" 
     LABEL "Ekst.referanse" 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1.

DEFINE VARIABLE KortNavn AS CHARACTER FORMAT "X(15)" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.

DEFINE VARIABLE MesseBeskrivelse AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 37.2 BY 1.

DEFINE VARIABLE MesseNr AS DECIMAL FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "MesseNr" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE ProfilNr AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Prisprofil" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE sokLevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE sokLevNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE VarebehBeskrivelse AS CHARACTER FORMAT "X(40)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     ButikkListe AT ROW 1.24 COL 15 COLON-ALIGNED
     btnSokProfilNr AT ROW 5.76 COL 26.8
     btnButikker AT ROW 1.24 COL 72.4 NO-TAB-STOP 
     VarebehBeskrivelse AT ROW 2.38 COL 15 COLON-ALIGNED HELP
          "Kort beskrivelse av Varebehen"
     EkstRef AT ROW 3.52 COL 15 COLON-ALIGNED HELP
          "Ekstern referanse"
     MesseNr AT ROW 4.67 COL 15 COLON-ALIGNED HELP
          "Messenummer"
     MesseBeskrivelse AT ROW 4.67 COL 32.8 COLON-ALIGNED HELP
          "Navn eller kort beskrivelse av messen." NO-LABEL
     ProfilNr AT ROW 5.76 COL 15 COLON-ALIGNED HELP
          "Prisprofil"
     KortNavn AT ROW 5.76 COL 29 COLON-ALIGNED HELP
          "Kort betegnelse på prisprofilen" NO-LABEL
     VarebehNotat AT ROW 6.95 COL 17 NO-LABEL
     sokLevNr AT ROW 13.38 COL 15 COLON-ALIGNED HELP
          "Varegruppe"
     btnLev AT ROW 13.38 COL 28 NO-TAB-STOP 
     sokLevNamn AT ROW 13.38 COL 31 COLON-ALIGNED NO-LABEL
     btnOk AT ROW 15.67 COL 45.8
     btnCancel AT ROW 15.67 COL 61.8
     btnSokMesseNr AT ROW 4.67 COL 30.2 NO-TAB-STOP 
     "(Leverandørfilter for artikler)" VIEW-AS TEXT
          SIZE 55 BY .62 AT ROW 14.57 COL 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 77.6 BY 16.1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 16.14
         WIDTH              = 77.8
         MAX-HEIGHT         = 17.62
         MAX-WIDTH          = 133.6
         VIRTUAL-HEIGHT     = 17.62
         VIRTUAL-WIDTH      = 133.6
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       ButikkListe:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN KortNavn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN MesseBeskrivelse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       sokLevNamn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       VarebehNotat:RETURN-INSERTED IN FRAME DEFAULT-FRAME  = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnButikker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnButikker C-Win
ON CHOOSE OF btnButikker IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cRowIdList    AS CHAR NO-UNDO.
  DEF VAR cIdList       AS CHAR NO-UNDO.
  
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Butiker;Butik;Butnamn",
                      "WHERE CAN-DO('" + cMineButikker + "',STRING(Butik))",
                      INPUT-OUTPUT cRowIdList,
                      "Butik",
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN 
    ButikkListe:SCREEN-VALUE = REPLACE(cIdList,"|",",").
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLev C-Win
ON CHOOSE OF btnLev IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF sokLevNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "LevNr;Levnamn".
  RUN JBoxDLookup.w ("LevBas;levnamn;levnr;KjedeAvtale|Kjedeavtale|J/N","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
    ASSIGN sokLevNr:SCREEN-VALUE = ENTRY(1,cLookupValue,"|")
           sokLevNamn:SCREEN-VALUE = ENTRY(2,cLookupValue,"|")
           .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk C-Win
ON CHOOSE OF btnOk IN FRAME DEFAULT-FRAME /* Lagre */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF VarebehBeskrivelse:SCREEN-VALUE = "" THEN
      DO:
          MESSAGE 
          "Beskrivelse må angis."           
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
  END.

  ASSIGN ocFieldList = "VarebehType,ButikkListe,EkstRef,MesseNr,ProfilNr,VarebehBeskrivelse,VarebehNotat"
         ocValueList = icType
               + "|" + ButikkListe:SCREEN-VALUE 
               + "|" + EkstRef:SCREEN-VALUE
               + "|" + MesseNr:SCREEN-VALUE
               + "|" + ProfilNr:SCREEN-VALUE
               + "|" + VarebehBeskrivelse:SCREEN-VALUE
               + "|" + VarebehNotat:SCREEN-VALUE
          ocLevNr = sokLevNr:SCREEN-VALUE
          .     
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokMesseNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokMesseNr C-Win
ON CHOOSE OF btnSokMesseNr IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF MesseNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "MesseNr;MesseBeskrivelse".
  RUN JBoxDLookup.w ("Messe;MesseNr;MesseBeskrivelse","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
    ASSIGN MesseNr:SCREEN-VALUE = ENTRY(1,cLookupValue,"|")
           MesseBeskrivelse:SCREEN-VALUE = ENTRY(2,cLookupValue,"|").
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokProfilNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokProfilNr C-Win
ON CHOOSE OF btnSokProfilNr IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF ProfilNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "ProfilNr".
  RUN JBoxDLookup.w ("PrisProfil;ProfilNr;KortNavn","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    ProfilNr:SCREEN-VALUE = cLookupValue.
    APPLY "ANY-PRINTABLE":U TO ProfilNr.
    APPLY "TAB":U TO ProfilNr.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MesseNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MesseNr C-Win
ON RETURN OF MesseNr IN FRAME DEFAULT-FRAME /* MesseNr */
OR "TAB" OF MesseNr
DO:
  IF SELF:MODIFIED THEN
  DO:
      MesseBeskrivelse:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Messe",
                                              "WHERE MesseNr = '" + MesseNr:SCREEN-VALUE + "'","MesseBeskrivelse").  
      APPLY "ANY-PRINTABLE":U TO MesseNr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ProfilNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ProfilNr C-Win
ON RETURN OF ProfilNr IN FRAME DEFAULT-FRAME /* Prisprofil */
OR "TAB" OF ProfilNr
DO:
  IF SELF:MODIFIED THEN
  DO:
      KortNavn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","PrisProfil",
                                              "WHERE ProfilNr = '" + ProfilNr:SCREEN-VALUE + "'","KortNavn").  
      APPLY "ANY-PRINTABLE":U TO ProfilNr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNr C-Win
ON RETURN OF sokLevNr IN FRAME DEFAULT-FRAME /* Leverandør */
OR TAB OF sokLevNr DO:
  IF sokLevNr:MODIFIED THEN DO:
    sokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokLevNr:SCREEN-VALUE).
  END.
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

ON 'alt-l':U OF FRAME {&FRAME-NAME} ANYWHERE
  APPLY "choose" TO btnOK.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  ASSIGN THIS-PROCEDURE:CURRENT-WINDOW:X = SOURCE-PROCEDURE:CURRENT-WINDOW:X
         THIS-PROCEDURE:CURRENT-WINDOW:Y = SOURCE-PROCEDURE:CURRENT-WINDOW:Y
         THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE
         .

  RUN InitWindow.

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
  DISPLAY ButikkListe VarebehBeskrivelse EkstRef MesseNr MesseBeskrivelse 
          ProfilNr KortNavn VarebehNotat sokLevNr sokLevNamn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE ButikkListe btnSokProfilNr btnButikker VarebehBeskrivelse EkstRef 
         MesseNr ProfilNr VarebehNotat sokLevNr btnLev sokLevNamn btnOk 
         btnCancel btnSokMesseNr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow C-Win 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cCl    AS CHAR NO-UNDO.
DEF VAR cTitle AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cCL            = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                       "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1")
         cTitle         = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                       "WHERE SysHId = 5 and SysGr = 6 and ParaNr = " + icType,"Parameter2")
         ProfilNr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Butiker",
                       "WHERE Butik = " + cCl,"ProfilNr")
         MesseNr:SENSITIVE = icType = "1"        
         /*VarebehBeskrivelse:SCREEN-VALUE = cTitle + " " + STRING(TODAY)*/
         .
  IF cTitle = ? OR cTitle = "" THEN
    cTitle = IF icType = "1" THEN "Suppleringsordre" ELSE "Direkte varemottak".
    
  THIS-PROCEDURE:CURRENT-WINDOW:TITLE = cTitle.

  IF DYNAMIC-FUNCTION("runproc","mine_varemottak_butikker.p",DYNAMIC-FUNCTION("getASuserId") + ",2",?) THEN
    cMineButikker = DYNAMIC-FUNCTION("getTransactionMessage").

  APPLY "tab" TO ProfilNr.

  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

/*   IF NUM-ENTRIES(cMineButikker) NE 1 THEN  */
/*     APPLY "choose" TO btnButikker.         */
/*   ELSE                                     */
    ButikkListe:SCREEN-VALUE = cMineButikker.
  APPLY "entry" TO VarebehBeskrivelse.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSelectorAttributes C-Win 
PROCEDURE setSelectorAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrwSrc AS HANDLE NO-UNDO.
DEF INPUT PARAM ihBrwTrg AS HANDLE NO-UNDO.

ASSIGN ihBrwSrc:WINDOW:X = THIS-PROCEDURE:CURRENT-WINDOW:X + 83
       ihBrwSrc:WINDOW:Y = THIS-PROCEDURE:CURRENT-WINDOW:Y + 50
       ihBrwSrc:WINDOW:TITLE = "Velg butikker for " + THIS-PROCEDURE:CURRENT-WINDOW:TITLE
       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

