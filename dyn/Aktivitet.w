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

DEF VAR cAllCompIds     AS CHAR NO-UNDO.
DEF VAR cAllCompNames   AS CHAR NO-UNDO.
DEF VAR cDeleteList     AS CHAR NO-UNDO.
DEF VAR cNewList        AS CHAR NO-UNDO.
DEF VAR cSelected       AS CHAR NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolBar rectSearchField AktNr ~
Beskrivelse Merknad RegistrertDato RegistrertAv EDato BrukerID ~
fi-cMerknadLabel 
&Scoped-Define DISPLAYED-OBJECTS AktNr Beskrivelse Merknad RegistrertDato ~
RegistrertAv EDato BrukerID fi-cMerknadLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE Merknad AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 40 BY 4 NO-UNDO.

DEFINE VARIABLE AktNr AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Aktivitetnr" 
     VIEW-AS FILL-IN 
     SIZE 6.8 BY 1.

DEFINE VARIABLE Beskrivelse AS CHARACTER FORMAT "X(30)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.

DEFINE VARIABLE BrukerID AS CHARACTER FORMAT "X(10)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE EDato AS DATE FORMAT "99/99/9999" 
     LABEL "Endret" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE fi-cMerknadLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Merknad:" 
      VIEW-AS TEXT 
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE RegistrertAv AS CHARACTER FORMAT "X(10)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE RegistrertDato AS DATE FORMAT "99/99/9999" 
     LABEL "Registrert" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 38 BY 13.33.

DEFINE RECTANGLE rectSearchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 20.6 BY .91.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 28 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     AktNr AT ROW 3.57 COL 51 COLON-ALIGNED HELP
          "Aktivitetnr"
     Beskrivelse AT ROW 4.62 COL 51 COLON-ALIGNED HELP
          "Kort beskrivelse av aktivitet"
     Merknad AT ROW 5.71 COL 53 NO-LABEL
     RegistrertDato AT ROW 14.81 COL 58 COLON-ALIGNED HELP
          "Dato da posten ble registrert i registeret"
     RegistrertAv AT ROW 14.81 COL 75 COLON-ALIGNED HELP
          "Brukerid på den som registrerte posten" NO-LABEL
     EDato AT ROW 15.91 COL 58 COLON-ALIGNED HELP
          "Endret dato"
     BrukerID AT ROW 15.91 COL 75 COLON-ALIGNED HELP
          "Bruker som registrerte/endret posten" NO-LABEL
     fi-cMerknadLabel AT ROW 5.76 COL 41.8 COLON-ALIGNED NO-LABEL
     rectBrowse AT ROW 3.62 COL 2
     rectToolBar AT ROW 1.14 COL 1.8
     rectSearchField AT ROW 2.43 COL 2.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 93 BY 16.


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
         TITLE              = "Vedlikehold aktiviteter"
         HEIGHT             = 16
         WIDTH              = 93
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
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

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
ON END-ERROR OF C-Win /* Vedlikehold aktiviteter */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vedlikehold aktiviteter */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Vedlikehold aktiviteter */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
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
  DISPLAY AktNr Beskrivelse Merknad RegistrertDato RegistrertAv EDato BrukerID 
          fi-cMerknadLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectBrowse rectToolBar rectSearchField AktNr Beskrivelse Merknad 
         RegistrertDato RegistrertAv EDato BrukerID fi-cMerknadLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
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
DO WITH FRAME {&FRAME-NAME}:
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",          /* Create a browse object */
                    rectBrowse:HANDLE,              /* Rectangle to define coordinates for browse */
                    100,                            /* Rows to batch */
                    "",                             /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "Aktivitet;AktNr;Beskrivelse;Merknad;!RegistrertAv;!RegistrertDato;!BrukerID;!EDato",    /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/
                    "WHERE true", 
                    "sort|AktNr").                   /* Misc - for something I might need in next version.. */
  hBrowse:NAME = "brwAktivitet". /* This name is neccessary because the browser is due to special treatment during resize */

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                    "Beskrivelse,Merknad",          /* Update columns in buffer */
                    "",                             /* Corresponding input fields (fill-in..)*/
                    "RegistrertAv,RegistrertDato,BrukerID,EDato,AktNr",
                    "",                             /* Additional buffer and displ.fields - not updateable*/
                    "").                            /* Misc - for something I might need in next version.. */
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customCreateProc","create_aktivitet.p").

  DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                          /* Corresponding menu label - no menu if blank */
                    "new;Ny,copy;Kopier,undo;Angre,delete;Slett,save;Lagre,excel;Eksporter til E&xcel",
                                                    /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    "maxborder").                    /* Misc - for something I might need in next version.. */

  DYNAMIC-FUNCTION("NewBrowseSearchField",rectSearchField:HANDLE,hBrowse,1).

  /* Link objects: */

  DYNAMIC-FUNCTION("LinkAllObjects",                /* Link all created objects. Linktype is type of "to" object,
                                                      f.ex link from browse to combo-box is combo-box link */
                    THIS-PROCEDURE:CURRENT-WINDOW,  /* Link only objects created for current window */
                    TRUE,                           /* Replace any existing links */
                    "").                            /* Except these objects - the combo must only be linked to one browse */

  DYNAMIC-FUNCTION("setCompanyHeader",THIS-PROCEDURE:CURRENT-WINDOW).

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar").
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "Merknad,rectSearchField").
  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,150,0,0).

  DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  APPLY "value-changed" TO hBrowse.
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

