&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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

DEF VAR hGenPlListe     AS HANDLE NO-UNDO.

/* Local Variable Definitions ---                                       */
{incl/Excel_1.3.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolBar rectWinToolbar RECT-2 ~
RECT-3 PlListeId PlMerknad tbSkjulOverforte FI-1 FI-2 
&Scoped-Define DISPLAYED-OBJECTS PlListeId PlMerknad tbSkjulOverforte FI-1 ~
FI-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PrintToExcel C-Win 
FUNCTION PrintToExcel RETURNS LOGICAL
  ( INPUT icFileAndCount AS CHAR,
    INPUT ibTestPrint    AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FI-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Filter" 
      VIEW-AS TEXT 
     SIZE 9 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Rediger merknad" 
      VIEW-AS TEXT 
     SIZE 24 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE PlListeId AS DECIMAL FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Plukkliste" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE PlMerknad AS CHARACTER FORMAT "X(256)":U 
     LABEL "Merknad" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 3.86.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 148 BY 3.86.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 201 BY 25.95.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE VARIABLE tbSkjulOverforte AS LOGICAL INITIAL yes 
     LABEL "Skjul overførte" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 TOOLTIP "Vis kun artikler med valgt inndeling" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     PlListeId AT ROW 3.14 COL 159 COLON-ALIGNED
     PlMerknad AT ROW 4.19 COL 159 COLON-ALIGNED
     tbSkjulOverforte AT ROW 5.52 COL 120
     FI-1 AT ROW 2.29 COL 2 COLON-ALIGNED NO-LABEL
     FI-2 AT ROW 2.29 COL 150 COLON-ALIGNED NO-LABEL
     rectBrowse AT ROW 6.95 COL 2
     rectToolBar AT ROW 1.14 COL 1.8
     rectWinToolbar AT ROW 1.14 COL 193
     RECT-2 AT ROW 2.91 COL 150
     RECT-3 AT ROW 2.91 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 203.2 BY 31.95.


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
         TITLE              = "Plukklisteregister"
         HEIGHT             = 31.95
         WIDTH              = 203.2
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
   FRAME-NAME                                                           */
ASSIGN 
       FI-1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

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
ON END-ERROR OF C-Win /* Plukklisteregister */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Plukklisteregister */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Plukklisteregister */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbSkjulOverforte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbSkjulOverforte C-Win
ON VALUE-CHANGED OF tbSkjulOverforte IN FRAME DEFAULT-FRAME /* Skjul overførte */
DO:
  RUN OpenQuery.
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
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

  RUN enable_UI.
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF

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

  RUN SUPER.
  PlListeId:SENSITIVE IN FRAME {&FRAME-NAME} =  DYNAMIC-FUNCTION('getToolbarState',hToolbar) = 'New'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterRecord C-Win 
PROCEDURE EksporterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR bOk        AS LOG  NO-UNDO.
  DEF VAR icParam    AS CHAR NO-UNDO.
  DEF VAR cRowIdList AS CHAR NO-UNDO.
  DEF VAR ix         AS INT  NO-UNDO.
  DEF VAR iReturn    AS INT NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('SendtPda'):BUFFER-VALUE <> ? THEN
      DO:
          MESSAGE "Listen ble sendt til PDA " +
                  STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('SendtPda'):BUFFER-VALUE) + "." SKIP
                  "Skal den sendes en gang til"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
          IF bOk = FALSE THEN RETURN NO-APPLY.
          bOk = FALSE.
      END.

      DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
        IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
          cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
      END.

      IF NOT DYNAMIC-FUNCTION("RunProc","pllistehode_send_pda.p",    
                               STRING(hFieldMap:BUFFER-FIELD("PlListeId"):BUFFER-VALUE)
                               ,?) THEN
        DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil ved eksport av plukkliste",""). 
  END.
  /*APPLY "value-changed" TO hBrowse.*/
  DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).

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
  DISPLAY PlListeId PlMerknad tbSkjulOverforte FI-1 FI-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectBrowse rectToolBar rectWinToolbar RECT-2 RECT-3 PlListeId 
         PlMerknad tbSkjulOverforte FI-1 FI-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      InitializeObject is called automatically when using JBoxDynMenu.w 
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",          /* Create a browse object */
                    rectBrowse:HANDLE,              /* Rectangle to define coordinates for browse */
                    100,                            /* Rows to batch */
                    "",                             /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "PlListeHode"
                    + ";PlListeId"
                    + ";PrioPlukket"
                    + ";FraButikkNr|Fra"
                    + ";TilbutikkNr|Til"
                    + ";PlNavn"
                    + ";PlMerknad"
                    + ";Antall|Antall|>>>>9"
                    + ";AntallPlukket|Plukket|>>>>9" 
                    + ";RegistrertDato|Opprettet"
                    + ";SendtPda"
                    + ";PlListeStatus|Status|>>>9"
                    + ";DatoPlukket"
                    + ";OverfortDato|Overført"
                    + ";BuntNr"
                    + ";!RegistrertTid;!RegistrertAv;EDato|Endret;!ETid;BrukerID"
                   ,"WHERE false"
                    ,"sort|PlListeId;DESC").             /* Initial sort column */

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                    "PlListeId,PlMerknad",   /* Update columns in buffer */
                      "",                           /* Corresponding input fields (fill-in..). blank if equal to update columns */
                      "",        /* Additional buffer and displ.fields - not updateable*/
                      "",                           /* Corresponding fill-ins */
                    "").                            /* other input widgets and lookup buttons for update fill-ins */

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hBrowse).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "delete,save,Refresh"
                    + ",excel;Eksporter til E&xcel"
                    + ",Rule"
                    + ",Plukkliste¤enable;&Generer plukkliste"
                    + ",Eksporter;&Eksporter" /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    + ",settAntPlukket;Sett antall plukket"
                    + ",Oppdater;&Overfør til overføringsordre"
                    + ",ListeTilExcel;Eksporter &liste til Excel"
                    + ",SendBrukere;Send brukere til PDA"
                    ,"maxborder").                  /* Misc - enable, maxborder.. */
  
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,          /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "close",
                    "right,enable").                      /* Misc - enable, maxborder.. */


  APPLY "value-changed" TO hBrowse.

END.

DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,400,150,0,250).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
RUN InvokeMethod (hBrowse,'OpenQuery').

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ListeTilExcelRecord C-Win 
PROCEDURE ListeTilExcelRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR cFileName          AS CHAR       NO-UNDO.
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.
DEF VAR iCount             AS INT        NO-UNDO.

cFileName = "PlListe-" + STRING(hFieldMap:BUFFER-FIELD("PlListeId"):BUFFER-VALUE) + ".csv".

SYSTEM-DIALOG GET-FILE cFileName FILTERS ".csv" "*.csv" USE-FILENAME UPDATE bOK.

IF bOK THEN DO:
  cFileName = RIGHT-TRIM(cFileName,".csv") + ".csv".

  IF NOT DYNAMIC-FUNCTION("RunProc","plliste_eksport.p",    
                           cFileName + "¤" + STRING(hFieldMap:BUFFER-FIELD("PlListeId"):BUFFER-VALUE)
                           ,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil ved eksport av plukkliste",""). 
  ELSE DO:

    iCount = DYNAMIC-FUNCTION("getTransactionMessage").

    chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
    IF NOT VALID-HANDLE(chExcelApplication) THEN DO:
      IF DYNAMIC-FUNCTION("setWebDoc","Open",cFileName) NE "" THEN
        MESSAGE "Could not open file: " cFileName VIEW-AS ALERT-BOX ERROR.
    END.

    chExcelApplication:Workbooks:OpenText(cFileName,2,,,,,TRUE).

    PrintToExcel(cFileName + "|" + string(iCount),TRUE).

    chExcelApplication:VISIBLE = TRUE.
  END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cFilter AS CHAR NO-UNDO.

  DO WITH FRAME DEFAULT-FRAME:
      ASSIGN tbSkjulOverforte
             tbSkjulOverforte:MODIFIED = FALSE
          .

      DYNAMIC-FUNCTION("SetCurrentObject",hBrowse).
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter","").

      cFilter =      (IF tbSkjulOverforte:CHECKED THEN
                       " AND OverfortDato = ?"
                      ELSE "")
                      .
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cFilter).

  END.

  RUN SUPER.

  APPLY "entry" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterRecord C-Win 
PROCEDURE OppdaterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR bOk        AS LOG  NO-UNDO.
  DEF VAR icParam    AS CHAR NO-UNDO.
  DEF VAR cRowIdList AS CHAR NO-UNDO.
  DEF VAR ix         AS INT  NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('OverfortDato'):BUFFER-VALUE <> ? THEN
      DO:
          MESSAGE "Listen er allerede oppdatert mot overføringsordre. Overført dato " + 
                  STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('OverfortDato'):BUFFER-VALUE) + "." SKIP
                  "Den kan ikke overføres flere ganger."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('DatoPlukket'):BUFFER-VALUE = ? THEN
      DO:
          MESSAGE "Listen er ikke plukket. Kan ikke overføres." 
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('AntallPlukket'):BUFFER-VALUE = 0 THEN
      DO:
          MESSAGE "Antall plukker = 0. Det er ingenting å overføre." 
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.

      MESSAGE "Skal listen oppdateres mot overføringsordre? "
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
      IF bOk = FALSE THEN RETURN NO-APPLY.
      bOk = FALSE.

      DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
        IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
          cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
      END.

      IF NOT DYNAMIC-FUNCTION("RunProc","pllistehode_send_overforingsordre.p",    
                               STRING(hFieldMap:BUFFER-FIELD("PlListeId"):BUFFER-VALUE)
                               ,?) THEN
        DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil ved opprettelse av overføringsordre",""). 
  END.
  /*APPLY "value-changed" TO hBrowse.*/
  DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE plukklisteRecord C-Win 
PROCEDURE plukklisteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  IF NOT VALID-HANDLE(hGenPlListe) THEN
  DO WITH FRAME {&FRAME-NAME}:
    RUN genPlListeHode.w PERSIST SET hGenPlListe.
    PAUSE 1 NO-MESSAGE.
    RUN setParentHandle IN hGenPlListe (THIS-PROCEDURE:HANDLE).
  END.
  APPLY "value-changed" TO hBrowse.
  /*DYNAMIC-FUNCTION('setColour' IN hGenPlListe).*/

  RUN MoveToTop IN hGenPlListe.
  SUBSCRIBE TO 'InvalidateHandle' IN hGenPlListe.

  RUN InvokeMethod (hBrowse,'OpenQuery').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE runOpenQueryInBrowse C-Win 
PROCEDURE runOpenQueryInBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
 RUN InvokeMethod (hBrowse,'OpenQuery').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendBrukereRecord C-Win 
PROCEDURE SendBrukereRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR bOk        AS LOG  NO-UNDO.
  DEF VAR icParam    AS CHAR NO-UNDO.
  DEF VAR cRowIdList AS CHAR NO-UNDO.
  DEF VAR ix         AS INT  NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      IF NOT DYNAMIC-FUNCTION("RunProc","pllistehode_send_brukere_til_pda.p",    
                               STRING(hFieldMap:BUFFER-FIELD("PlListeId"):BUFFER-VALUE)
                               ,?) THEN
        DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil ved eksport av brukere til pda",""). 
  END.

  MESSAGE "Liste med brukere er sendt til PDA." SKIP
          "Gå inn i 'Innstillinger' på PDA og benytt 'Hent brukere' for å få oppdatert brukerlisten" 
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE settAntPlukketRecord C-Win 
PROCEDURE settAntPlukketRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR bOk        AS LOG  NO-UNDO.
  DEF VAR icParam    AS CHAR NO-UNDO.
  DEF VAR cRowIdList AS CHAR NO-UNDO.
  DEF VAR ix         AS INT  NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('DatoPlukket'):BUFFER-VALUE <> ? THEN
      DO:
          MESSAGE "Listen er allerede plukket. Plukket dato " + 
                  STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('DatoPlukket'):BUFFER-VALUE) + "." SKIP
                  "Den kan ikke plukkes flere ganger."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('OverfortDato'):BUFFER-VALUE <> ? THEN
      DO:
          MESSAGE "Listen er overført. Kan ikke plukkes." 
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      MESSAGE "Skal antall plukket settes lik antall foreslått plukket? "
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
      IF bOk = FALSE THEN RETURN NO-APPLY.
      bOk = FALSE.

      DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
        IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
          cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
      END.

      IF NOT DYNAMIC-FUNCTION("RunProc","pllistehode_sett_plukket_lik_forslag.p",    
                               STRING(hFieldMap:BUFFER-FIELD("PlListeId"):BUFFER-VALUE)
                               ,?) THEN
        DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil ved initiering av antall plukket",""). 
  END.
  /*APPLY "value-changed" TO hBrowse.*/
  DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PrintToExcel C-Win 
FUNCTION PrintToExcel RETURNS LOGICAL
  ( INPUT icFileAndCount AS CHAR,
    INPUT ibTestPrint    AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR chExcelApplication      AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook              AS COM-HANDLE NO-UNDO.
DEF VAR chWorksheet             AS COM-HANDLE NO-UNDO.
DEF VAR chInterior              AS COM-HANDLE NO-UNDO.
DEF VAR chBorder                AS COM-HANDLE NO-UNDO.
DEF VAR chBefore                AS COM-HANDLE NO-UNDO.
DEF VAR iCount                  AS INTEGER NO-UNDO.
DEF VAR iNumCols                AS INT NO-UNDO INIT 14.
DEF VAR cRange                  AS CHAR NO-UNDO.
DEF VAR cFileName               AS CHAR NO-UNDO.
DEF VAR iCntStr                 AS INT NO-UNDO.
DEF VAR cCurrLevNavn            AS CHAR NO-UNDO.              
DEF VAR cPrevLevNavn            AS CHAR NO-UNDO.              
DEF VAR iShade                  AS INT NO-UNDO.
DEF VAR bFirstLevLine           AS LOG NO-UNDO.

ASSIGN cFileName = ENTRY(1,icFileAndCount,"|") 
       iCount    = INT(ENTRY(2,icFileAndCount,"|"))
       .

chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
IF NOT VALID-HANDLE(chExcelApplication) THEN DO:
  IF DYNAMIC-FUNCTION("setWebDoc","Open",cFileName) NE "" THEN
    MESSAGE "Could not open file: " cFileName VIEW-AS ALERT-BOX ERROR.
  RETURN TRUE.
END.
  
chExcelApplication:Workbooks:OpenText(cFileName,2,,,,,TRUE).

cRange = "A2:T" + STRING(iCount + 2). 

ASSIGN chWorkbook                           = chExcelApplication:WorkBooks:ITEM(1)
       chWorkSheet                          = chExcelApplication:Sheets:ITEM(1)
       chWorkSheet:NAME                     = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('PlNavn'):BUFFER-VALUE

       chWorkSheet:Range(cRange):FONT:NAME  = "Arial"
       chWorkSheet:Range(cRange):FONT:SIZE  = 12

       chWorkSheet:Rows(1):FONT:Bold        = TRUE
       chWorkSheet:Rows(2):FONT:Bold        = TRUE
       chWorkSheet:Rows(2):FONT:NAME        = "Arial Narrow"
       chWorkSheet:Rows(2):HorizontalAlignment = 3
       chWorkSheet:PageSetup:CenterHeader   = "Plukkliste " + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('PlListeId'):BUFFER-VALUE)
       chWorkSheet:PageSetup:LeftFooter     = DYNAMIC-FUNCTION("getAppTitle")
       chWorkSheet:PageSetup:CenterFooter   = STRING(TODAY,"99/99/9999")  + " " + STRING(TIME,"HH:MM") /*"&D &T"*/
       chWorkSheet:PageSetup:RightFooter    = "Page &P of &N"
       chWorkSheet:PageSetup:Orientation    = 2
       chWorkSheet:PageSetup:FitToPagesWide = 1
       chWorkSheet:PageSetup:TopMargin      = 50
       chWorkSheet:PageSetup:LeftMargin     = 25
       chWorkSheet:PageSetup:RightMargin    = 25
       chWorkSheet:PageSetup:Zoom           = 65
       chWorkSheet:PageSetup:PrintTitleRows = "$1:$2"
       .

chWorkSheet:Range("A1:T2"):SELECT().
chInterior = chExcelApplication:SELECTION:Interior.
chInterior:ColorIndex = 37.

chWorkSheet:Range("A2:F2"):HorizontalAlignment = {&xlLeft}.
chWorkSheet:Range("P2:P2"):HorizontalAlignment = {&xlLeft}.
chWorkSheet:Range("A1"):FONT:SIZE = 18.

chWorkSheet:Range("B:B"):NumberFormat = "0".
chWorkSheet:Range("E:E"):NumberFormat = "@".

chBorder = chWorkSheet:Range("a1:t" + STRING(iCount + 2)):Borders({&xlEdgeTop}).
ASSIGN chBorder:LineStyle = {&xlContinuous}
       chBorder:Weight    = {&xlThin}.
chBorder = chWorkSheet:Range("a1:t" + STRING(iCount + 2)):Borders({&xlEdgeBottom}).
ASSIGN chBorder:LineStyle = {&xlContinuous}
       chBorder:Weight    = {&xlThin}.
chBorder = chWorkSheet:Range("a1:t" + STRING(iCount + 2)):Borders({&xlEdgeRight}).
ASSIGN chBorder:LineStyle = {&xlContinuous}
       chBorder:Weight    = {&xlThin}.
chBorder = chWorkSheet:Range("a1:t" + STRING(iCount + 2)):Borders({&xlEdgeLeft}).
ASSIGN chBorder:LineStyle = {&xlContinuous}
       chBorder:Weight    = {&xlThin}.

chWorkSheet:Range("p:p"):columnwidth = 15.
chWorkSheet:Range("p1:p" + STRING(iCount + 2)):WrapText = TRUE.

/* chWorkSheet:Columns("B:O"):AutoFit().                                              */
/* chWorkSheet:Range("A:A"):columnwidth = chWorkSheet:Range("A:A"):columnwidth * 2.5. */
chWorkSheet:Columns("A:T"):AutoFit().

chWorkSheet:Range("A3"):Select().
chExcelApplication:ActiveWindow:FreezePanes = TRUE.

chExcelApplication:VISIBLE = TRUE.

/* release com-handles */
RELEASE OBJECT chBorder NO-ERROR.
RELEASE OBJECT chBefore NO-ERROR.
RELEASE OBJECT chInterior NO-ERROR.
RELEASE OBJECT chWorksheet NO-ERROR.
RELEASE OBJECT chWorkbook NO-ERROR.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

