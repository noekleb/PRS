&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
/* Procedure Description
"Basic Window Template

Use this template to create a new window. Alter this default template or create new ones to accomodate your needs for different default sizes and/or attributes."
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*********************************************************************
* Copyright (C) 2001 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
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

DEF VAR bOK             AS LOG NO-UNDO.
DEF VAR ix              AS INT NO-UNDO.

DEF VAR hToolbar        AS HANDLE NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.
DEF VAR hUpdToolbar      AS HANDLE NO-UNDO.

DEF VAR cAllCompIds     AS CHAR NO-UNDO.
DEF VAR cAllCompNames   AS CHAR NO-UNDO.
DEF VAR cDeleteList     AS CHAR NO-UNDO.
DEF VAR cNewList        AS CHAR NO-UNDO.
DEF VAR cSelected       AS CHAR NO-UNDO.
DEF VAR cTekst          AS CHAR NO-UNDO.

PROCEDURE SendMessageA EXTERNAL "USER32.dll":
  DEFINE INPUT PARAMETER hHWND AS LONG.
  DEFINE INPUT PARAMETER iCmd  AS LONG.
  DEFINE INPUT PARAMETER iChar AS LONG.
  DEFINE INPUT PARAMETER ulParam AS LONG.
END PROCEDURE.

PROCEDURE PostPWChar:
  DEFINE INPUT PARAMETER hHWND AS INT.
  RUN SendMessageA(hHWND, 204, ASC("*"), 0).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolBar rectSearchField ~
RECT-1 RegistrertDato RegistrertAv EDato BrukerID Ukedag Tid HasterTid 
&Scoped-Define DISPLAYED-OBJECTS ButikkNr ButNamn RegistrertDato ~
RegistrertAv LevNr levnamn EDato BrukerID Ukedag Tid HasterTid Tekst1 ~
Tekst-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSokButiker  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokLevBas  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE HasterTid AS CHARACTER FORMAT "X(256)" INITIAL "43200" 
     LABEL "Hasteordrefrist" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     LIST-ITEM-PAIRS "item1","item1"
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE Tid AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ordrefrist" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE Ukedag AS INTEGER FORMAT "9" INITIAL 0 
     LABEL "Ukedag" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEM-PAIRS "Mandag",1,
                     "Tirsdag",2,
                     "Onsdag",3,
                     "Torsdag",4,
                     "Fredag",5,
                     "Lørdag",6,
                     "Søndag",7
     DROP-DOWN-LIST
     SIZE 15.4 BY 1 NO-UNDO.

DEFINE VARIABLE BrukerID AS CHARACTER FORMAT "X(10)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE ButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE ButNamn AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1.

DEFINE VARIABLE EDato AS DATE FORMAT "99/99/9999" 
     LABEL "Endret" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE levnamn AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1.

DEFINE VARIABLE LevNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE RegistrertAv AS CHARACTER FORMAT "X(10)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE RegistrertDato AS DATE FORMAT "99/99/9999" 
     LABEL "Registrert" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE Tekst-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Liste" 
      VIEW-AS TEXT 
     SIZE 11 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE Tekst1 AS CHARACTER FORMAT "X(256)":U INITIAL "Detaljer" 
      VIEW-AS TEXT 
     SIZE 11 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 121 BY 3.57.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 121 BY 14.05.

DEFINE RECTANGLE rectSearchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 20.6 BY .91.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 28 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSokButiker AT ROW 4.52 COL 25.6 NO-TAB-STOP 
     ButikkNr AT ROW 4.57 COL 13 COLON-ALIGNED HELP
          "Butikknummer"
     ButNamn AT ROW 4.57 COL 28 COLON-ALIGNED HELP
          "Butikkens navn" NO-LABEL
     RegistrertDato AT ROW 4.57 COL 87.4 COLON-ALIGNED HELP
          "Dato da posten ble registrert i registeret"
     RegistrertAv AT ROW 4.57 COL 104.4 COLON-ALIGNED HELP
          "Brukerid på den som registrerte posten" NO-LABEL
     LevNr AT ROW 5.57 COL 13 COLON-ALIGNED
     levnamn AT ROW 5.62 COL 28 COLON-ALIGNED NO-LABEL
     EDato AT ROW 5.67 COL 87.4 COLON-ALIGNED HELP
          "Endret dato"
     BrukerID AT ROW 5.67 COL 104.4 COLON-ALIGNED HELP
          "Bruker som registrerte/endret posten" NO-LABEL
     Ukedag AT ROW 6.62 COL 13 COLON-ALIGNED
     Tid AT ROW 6.71 COL 44 COLON-ALIGNED
     HasterTid AT ROW 6.71 COL 71.8 COLON-ALIGNED HELP
          "Tidsfrist for hasteordre"
     btnSokLevBas AT ROW 5.57 COL 25.6 NO-TAB-STOP 
     Tekst1 AT ROW 3.62 COL 1 COLON-ALIGNED NO-LABEL
     Tekst-2 AT ROW 8.05 COL 1 COLON-ALIGNED NO-LABEL
     rectBrowse AT ROW 8.86 COL 2
     rectToolBar AT ROW 1.14 COL 1.8
     rectSearchField AT ROW 2.43 COL 2.4
     RECT-1 AT ROW 4.29 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 122.8 BY 22.05.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window Template
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Vedlikehold leveringsdag lev. og butikk"
         HEIGHT             = 22.05
         WIDTH              = 122.8
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR BUTTON btnSokButiker IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnSokLevBas IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ButikkNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ButNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN levnamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Tekst-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Tekst-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Tekst1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Tekst1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Vedlikehold leveringsdag lev. og butikk */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vedlikehold leveringsdag lev. og butikk */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Vedlikehold leveringsdag lev. og butikk */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokButiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokButiker C-Win
ON CHOOSE OF btnSokButiker IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF ButikkNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "Butik".
  RUN JBoxDLookup.w ("Butiker;Butik;ButNamn","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue <> "" THEN 
  DO:
      ASSIGN
          ButikkNr:SCREEN-VALUE = cLookupValue
          .
      APPLY "TAB" TO ButikkNr.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokLevBas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokLevBas C-Win
ON CHOOSE OF btnSokLevBas IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF LevNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "LevNr".
  RUN JBoxDLookup.w ("LevBas;LevNr;LevNamn","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue <> "" THEN 
  DO:
      ASSIGN
          LevNr:SCREEN-VALUE = cLookupValue
          .
      APPLY "TAB" TO LevNr.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButikkNr C-Win
ON TAB OF ButikkNr IN FRAME DEFAULT-FRAME /* Butikk */
OR "RETURN" OF ButikkNr
DO:
  IF SELF:MODIFIED THEN
  DO:
      ASSIGN
          cTekst = DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE Butik = '" + ButikkNr:SCREEN-VALUE + "'","ButNamn")
          ButNamn:SCREEN-VALUE = ENTRY(1,cTekst,"|")  
          .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevNr C-Win
ON TAB OF LevNr IN FRAME DEFAULT-FRAME /* Leverandør */
OR "RETURN" OF LevNr
DO:
  IF SELF:MODIFIED THEN
  DO:
      ASSIGN
          cTekst = DYNAMIC-FUNCTION("getFieldValues","LevBas","WHERE LevNr = '" + LevNr:SCREEN-VALUE + "'","LevNamn")
          LevNamn:SCREEN-VALUE = ENTRY(1,cTekst,"|")  
          .
  END.
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
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
  RUN enable_UI.
  RUN InitWindow.
  DYNAMIC-FUNCTION("DoLockWindow",?).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").

  RUN SUPER.

  DO WITH FRAME default-frame:
      ASSIGN 
          ButikkNr:SENSITIVE      = FALSE
          btnSokButiker:SENSITIVE = FALSE
          LevNr:SENSITIVE         = FALSE
          btnSokLevBas:SENSITIVE  = FALSE
          .
  END.
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
  DYNAMIC-FUNCTION("DoLockWindow",?).

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
  DISPLAY ButikkNr ButNamn RegistrertDato RegistrertAv LevNr levnamn EDato 
          BrukerID Ukedag Tid HasterTid Tekst1 Tekst-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectBrowse rectToolBar rectSearchField RECT-1 RegistrertDato 
         RegistrertAv EDato BrukerID Ukedag Tid HasterTid 
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
DEF VAR cTider AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
      Tid:DELIMITER       = "|"
      HasterTid:DELIMITER = "|"
      .

  DO ix = 7 TO 17:
    cTider = cTider + STRING(ix * 3600,"HH:MM") + "|" + STRING(ix * 3600) + "|".
    IF ix < 17 THEN
      cTider = cTider + STRING(ix * 3600 + 1800,"HH:MM") + "|" + STRING(ix * 3600 + 1800) + "|".
  END.
  ASSIGN
      Tid:LIST-ITEM-PAIRS = "|0|" + TRIM(cTider,"|")
      HasterTid:LIST-ITEM-PAIRS = "|0|" + TRIM(cTider,"|")
      .

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",           /* Create a browse object */
                    rectBrowse:HANDLE,              /* Rectangle to define coordinates for browse */
                    100,                            /* Rows to batch */
                    "",                             /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "DefaultLevDato;ButikkNr|Butikk;LevNr|Leverandør" 
                             + ";+cUkedag|CHARACTER|x(15)|get_ukedag_navn.p(Ukedag)|Ukedag"
                             + ";+cTid|CHARACTER|x(5)|jb_hhmm(Tid)|Frist"
                             + ";+cHTid|CHARACTER|x(5)|jb_hhmm(HasterTid)|Hastefrist"
                             + ";!Ukedag;!Tid;!HasterTid;!RegistrertAv;!RegistrertDato;!BrukerID;!EDato,Butiker;ButNamn|Butikknavn@2,LevBas;LevNamn|Lev.navn@4",    /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/
                    "WHERE true,FIRST Butiker NO-LOCK where Butiker.Butik = DefaultLevDato.ButikkNr OUTER-JOIN" +
                              ",FIRST LevBas NO-LOCK where LevBas.LevNr = DefaultLevDato.LevNr OUTER-JOIN", 
                    "").                           /* Misc - for something I might need in next version.. */
  hBrowse:NAME = "brwDefaultLevDato". /* This name is neccessary because the browser is due to special treatment during resize */

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"SortMap","cTid;Tid").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                    "ButikkNr,LevNr,Ukedag,Tid,HasterTid",        /* Update columns in buffer */
                    "",                             /* Corresponding input fields (fill-in..)*/
                    "ButNamn,LevNamn,RegistrertAv,RegistrertDato,BrukerID,EDato",
                    "",                             /* Additional buffer and displ.fields - not updateable*/
                    "Sort|DefaultLevDato").                            /* Misc - for something I might need in next version.. */
  /*DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customCreateProc","create_DefaultLevDato.p")*/.
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore").

  hUpdToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                          /* Corresponding menu label - no menu if blank */
                    "new;Ny,copy;Kopier,undo;Angre,delete;Slett,save;Lagre,excel;Eksporter til E&xcel",
                                                    /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    "maxborder").                    /* Misc - for something I might need in next version.. */

  DYNAMIC-FUNCTION("NewBrowseSearchField",rectSearchField:HANDLE,hBrowse,1).

  /* Flytter kolonner */
/*   hBrowse:MOVE-COLUMN(5,2).  */
/*   hBrowse:MOVE-COLUMN(6,4).  */

  /* Link objects: */

  DYNAMIC-FUNCTION("LinkAllObjects",                /* Link all created objects. Linktype is type of "to" object,
                                                      f.ex link from browse to combo-box is combo-box link */
                    THIS-PROCEDURE:CURRENT-WINDOW,  /* Link only objects created for current window */
                    TRUE,                           /* Replace any existing links */
                    "").                            /* Except these objects - the combo must only be linked to one browse */

  DYNAMIC-FUNCTION("setCompanyHeader",THIS-PROCEDURE:CURRENT-WINDOW).

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar,rect-1").
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectSearchField").
  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,550,250,0,0).

  DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  APPLY "value-changed" TO hBrowse.

  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
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
APPLY "entry" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN SUPER.

  DO WITH FRAME default-frame:
      ASSIGN
          btnSokButiker:SENSITIVE = true
          btnSokLevBas:SENSITIVE  = TRUE
          Ukedag:SCREEN-VALUE     = "4"
          .

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveRecord C-Win 
PROCEDURE saveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cState     AS CHAR                      NO-UNDO.

  ASSIGN
    cState = DYNAMIC-FUNCTION('getToolbarState',hUpdToolBar)
    .

  DO WITH FRAME default-frame:

    IF int(ButikkNr:screen-value) = 0 THEN
    DO:
        MESSAGE "Butikk må angis."
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        APPLY "ENTRY" TO ButikkNr.
        RETURN NO-APPLY.
    END.

    IF int(LevNr:screen-value) = 0 THEN
    DO:
        MESSAGE "Leverandørnummer må angis."
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        APPLY "ENTRY" TO LevNr.
        RETURN NO-APPLY.
    END.

    IF int(Ukedag:screen-value) = 0 THEN
    DO:
        MESSAGE "Ukedag må angis."
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        APPLY "ENTRY" TO Ukedag.
        RETURN NO-APPLY.
    END.

    IF cState = "NEW" THEN
    NEWPOST:
    DO:
      cTekst = "".
      IF DYNAMIC-FUNCTION("getFieldValues","DefaultLevDato","WHERE ButikkNr = '" + ButikkNr:SCREEN-VALUE + "' and LevNr = '" + LevNr:SCREEN-VALUE + "'","ButikkNr,LevNr,Ukedag") <> ? THEN
      DO:
          MESSAGE "Oppsett av default dato for butikk/leverandør eksisterer fra før."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK  TITLE "Lagringskontroll".
          APPLY "ENTRY" TO ButikkNr.
          RETURN NO-APPLY.
      END.
    END. /* NEWPOST */

    IF DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE Butik = '" + ButikkNr:SCREEN-VALUE + "'","ButNamn") = ? THEN
    DO:
        MESSAGE "Ugyldig butikk."
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        APPLY "ENTRY" TO ButikkNr.
        RETURN NO-APPLY.
    END.
    IF DYNAMIC-FUNCTION("getFieldValues","LevBas","WHERE LevNr = '" + LevNr:SCREEN-VALUE + "'","LevNamn") = ? THEN
    DO:
        MESSAGE "Ugyldig leverandør."
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        APPLY "ENTRY" TO LevNr.
        RETURN NO-APPLY.
    END.

    ASSIGN
        ButikkNr:SENSITIVE = FALSE
        LevNr:SENSITIVE    = FALSE
        .

  END.

  RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

