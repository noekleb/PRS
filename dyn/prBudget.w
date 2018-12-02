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
DEF VAR hBrwOsumfgaar        AS HANDLE NO-UNDO.
DEF VAR hBrwOp01        AS HANDLE NO-UNDO.
DEF VAR hBrwOp02        AS HANDLE NO-UNDO.
DEF VAR hBrwOp03        AS HANDLE NO-UNDO.
DEF VAR hBrwOp04        AS HANDLE NO-UNDO.
DEF VAR hBrwOp05        AS HANDLE NO-UNDO.
DEF VAR hBrwOp06        AS HANDLE NO-UNDO.
DEF VAR hBrwOp07        AS HANDLE NO-UNDO.
DEF VAR hBrwOp08        AS HANDLE NO-UNDO.
DEF VAR hBrwOp09        AS HANDLE NO-UNDO.
DEF VAR hBrwOp10        AS HANDLE NO-UNDO.
DEF VAR hBrwOp11        AS HANDLE NO-UNDO.
DEF VAR hBrwOp12        AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolBar rectWinToolbar aar ~
station 
&Scoped-Define DISPLAYED-OBJECTS aar station butnamn 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE aar AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "År" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE butnamn AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 28.72 BY 1 NO-UNDO.

DEFINE VARIABLE station AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Station" 
     VIEW-AS FILL-IN 
     SIZE 13.86 BY 1 NO-UNDO.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 141 BY 23.92.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .96.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.86 BY .92.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     aar AT ROW 2.65 COL 7.14 COLON-ALIGNED
     station AT ROW 3.69 COL 7.14 COLON-ALIGNED
     butnamn AT ROW 3.69 COL 21.29 COLON-ALIGNED NO-LABEL
     rectBrowse AT ROW 5.35 COL 2
     rectToolBar AT ROW 1.15 COL 1.86
     rectWinToolbar AT ROW 1.15 COL 133
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


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
         TITLE              = "Salgsenhets register"
         HEIGHT             = 28.46
         WIDTH              = 142.86
         MAX-HEIGHT         = 29.73
         MAX-WIDTH          = 147
         VIRTUAL-HEIGHT     = 29.73
         VIRTUAL-WIDTH      = 147
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
   FRAME-NAME Size-to-Fit                                               */
ASSIGN 
       FRAME DEFAULT-FRAME:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN butnamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
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
ON END-ERROR OF C-Win /* Salgsenhets register */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Salgsenhets register */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Salgsenhets register */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME aar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL aar C-Win
ON RETURN OF aar IN FRAME DEFAULT-FRAME /* År */
DO:
  ASSIGN aar station.
  IF aar > 0 AND station > 0 THEN DO:
      RUN InvokeMethod(hBrowse,"OpenQuery").
      APPLY "ENTRY" TO hBrowse.
      RETURN NO-APPLY.
  END.
  ELSE
  APPLY "TAB" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME station
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL station C-Win
ON RETURN OF station IN FRAME DEFAULT-FRAME /* Station */
DO:
    ASSIGN aar station.
    IF aar > 0 AND station > 0 THEN DO:
        RUN InvokeMethod(hBrowse,"OpenQuery").
        butnamn:SCREEN-VALUE IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION("getFieldList","Butiker;ButNamn","WHERE Butik = " + station:SCREEN-VALUE).
        IF NOT hBrowse:FOCUSED-ROW = ? THEN
            APPLY "ENTRY" TO hBrowse.
        ELSE
            APPLY "ENTRY" TO station.
        RETURN NO-APPLY.
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
/*   aar:SENSITIVE IN FRAME {&FRAME-NAME} =  DYNAMIC-FUNCTION('getToolbarState',hToolbar) = 'New'. */
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
  DISPLAY aar station butnamn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectBrowse rectToolBar rectWinToolbar aar station 
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
                    "prBudget"
                    + ";!aar"
                    + ";!station"
                    + ";hg|Grupp"
                    + ";sumfgaar"
                    + ";p01"
                    + ";p02"
                    + ";p03"
                    + ";p04"
                    + ";p05"
                    + ";p06"
                    + ";p07"
                    + ";p08"
                    + ";p09"
                    + ";p10"
                    + ";p11"
                    + ";p12"
                   ,"WHERE false"
                    ,"").             /* Initial sort column */

  DYNAMIC-FUNCTION("setSortString",hBrowse,"hg;desc").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"enableOnDblClick","yes").
  hBrwOsumfgaar = DYNAMIC-FUNCTION("NewBrowseFillIn",
                                        hBrowse,          
                                        "sumfgaar",     
                                        "sumfgaar",     
                                         "","","","").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hBrwOsumfgaar,"sumfgaar").
   hBrwOp01 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                                        hBrowse,          
                                        "p01",     
                                        "p01",     
                                         "","","","").                
   DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hBrwOp01,"p01").
/*    DYNAMIC-FUNCTION("setAttribute",hBrwOp01,"refreshrow","yes"). */

   hBrwOp02 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                                        hBrowse,          
                                        "p02",     
                                        "p02",     
                                         "","","","").                
   DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hBrwOp02,"p02").
   hBrwOp03 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                                        hBrowse,          
                                        "p03",     
                                        "p03",     
                                         "","","","").                
   DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hBrwOp03,"p03").
   hBrwOp04 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                                        hBrowse,          
                                        "p04",     
                                        "p04",     
                                         "","","","").                
   DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hBrwOp04,"p04").
   hBrwOp05 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                                        hBrowse,          
                                        "p05",     
                                        "p05",     
                                         "","","","").                
   DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hBrwOp05,"p05").
   hBrwOp06 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                                        hBrowse,          
                                        "p06",     
                                        "p06",     
                                         "","","","").                
   DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hBrwOp06,"p06").
   hBrwOp07 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                                        hBrowse,          
                                        "p07",     
                                        "p07",     
                                         "","","","").                
   DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hBrwOp07,"p07").
   hBrwOp08 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                                        hBrowse,          
                                        "p08",     
                                        "p08",     
                                         "","","","").                
   DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hBrwOp08,"p08").
   hBrwOp09 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                                        hBrowse,          
                                        "p09",     
                                        "p09",     
                                         "","","","").                
   DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hBrwOp09,"p09").
   hBrwOp10 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                                        hBrowse,          
                                        "p10",     
                                        "p10",     
                                         "","","","").                
   DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hBrwOp10,"p10").
   hBrwOp11 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                                        hBrowse,          
                                        "p11",     
                                        "p11",     
                                         "","","","").                
   DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hBrwOp11,"p11").
   hBrwOp12 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                                        hBrowse,          
                                        "p12",     
                                        "p12",     
                                         "","","","").                
   DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hBrwOp12,"p12").


/*   hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins)          */
/*                                                        and their corresponding buffer columns return handle equals the buffer handle */   */
/*                     hBrowse:QUERY,                                                                                                        */
/*                     FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */  */
/*                     "station",   /* Update columns in buffer */                                                                           */
/*                       "",                           /* Corresponding input fields (fill-in..). blank if equal to update columns */        */
/*                     "aar",        /* Additional buffer and displ.fields - not updateable*/                                                */
/*                       "",                           /* Corresponding fill-ins */                                                          */
/*                     "").                            /* other input widgets and lookup buttons for update fill-ins */                      */
/*                                                                                                                                           */
/*   DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hBrowse).                                                                                 */

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
/*                     "undo,save"  */
/*                     "new,delete" */
/*                     "delete" */
                    "excel;Eksporter til E&xcel" /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    ,"maxborder").                  /* Misc - enable, maxborder.. */
  
/*   DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar). */
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,          /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "close",
                    "right,enable").                      /* Misc - enable, maxborder.. */


  APPLY "value-changed" TO hBrowse.

END.

DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,150,0,250).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

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
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN aar station.
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryFilter",
                   "WHERE true"
                 + (IF aar NE 0 THEN
                     " AND aar = " + aar:SCREEN-VALUE
                    ELSE "")
                 + (IF station NE 0 THEN
                     " AND station = " + station:SCREEN-VALUE
                    ELSE "")
                   ).
  RUN SUPER.                   
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

