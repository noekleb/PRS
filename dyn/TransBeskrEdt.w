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
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.
DEF VAR hParent         AS HANDLE NO-UNDO.
DEF VAR hParentBrowse   AS HANDLE NO-UNDO.
DEF VAR hWinToolbar     AS HANDLE NO-UNDO.
DEF VAR hToolBarParent AS HANDLE NO-UNDO.
    DEFINE VARIABLE ittid   AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectToolBar rectWinToolbar TBId beskrivelse ~
Innloser KontoNr Royalty% VisPaBokfBilag Notat 
&Scoped-Define DISPLAYED-OBJECTS TBId beskrivelse Innloser KontoNr Royalty% ~
VisPaBokfBilag Notat 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE Notat AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 47 BY 4.62 NO-UNDO.

DEFINE VARIABLE beskrivelse AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE Innloser AS CHARACTER FORMAT "x(10)" 
     LABEL "Innløser" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE KontoNr AS INTEGER FORMAT ">>>9999":U INITIAL 0 
     LABEL "Kontonr" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE Royalty% AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Royalty" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE TBId AS INTEGER FORMAT ">>9" INITIAL 1 
     LABEL "Kode" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE VARIABLE VisPaBokfBilag AS LOGICAL INITIAL no 
     LABEL "Vis på bokf.bilag" 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     TBId AT ROW 3.48 COL 13 COLON-ALIGNED HELP
          "Transaksjonstype beskrivelse"
     beskrivelse AT ROW 4.52 COL 13 COLON-ALIGNED
     Innloser AT ROW 5.48 COL 13 COLON-ALIGNED HELP
          "Kortnavn eller bokstavkode som identifiserer kortinnløser"
     KontoNr AT ROW 6.48 COL 13.2 COLON-ALIGNED WIDGET-ID 2
     Royalty% AT ROW 7.48 COL 13 COLON-ALIGNED HELP
          "Royalty% som skal betales til innløser av salgsbeløpet."
     VisPaBokfBilag AT ROW 8.57 COL 15 HELP
          "Angir om innløser skal spesifiseres på bokføringsbilag og finansrapport"
     Notat AT ROW 9.57 COL 15 NO-LABEL
     rectToolBar AT ROW 1.14 COL 1.8
     rectWinToolbar AT ROW 1 COL 51
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 64 BY 13.38.


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
         TITLE              = "Simple maintenance screen"
         HEIGHT             = 13.38
         WIDTH              = 63.6
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
   FRAME-NAME                                                           */
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
ON END-ERROR OF C-Win /* Simple maintenance screen */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Simple maintenance screen */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Simple maintenance screen */
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
ON CLOSE OF THIS-PROCEDURE 
DO:

  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
/*   DYNAMIC-FUNCTION("ReplaceObjectLink",hParentBrowse,hToolBarParent). /* the detail win toolbar should now be linked to the browse */ */
/*   DYNAMIC-FUNCTION("setToolbar",hToolBarParent,"enable").                                                                             */
  
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
/*   DYNAMIC-FUNCTION("DeleteObject",hFieldMap).   */
/*   DYNAMIC-FUNCTION("DeleteObject",hToolBar).    */
/*   DYNAMIC-FUNCTION("DeleteObject",hWinToolBar). */

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
TBId:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

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
  DISPLAY TBId beskrivelse Innloser KontoNr Royalty% VisPaBokfBilag Notat 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectToolBar rectWinToolbar TBId beskrivelse Innloser KontoNr Royalty% 
         VisPaBokfBilag Notat 
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
  
  IF VALID-HANDLE(hParent) THEN ASSIGN hParentBrowse = DYNAMIC-FUNCTION("getParentBrowse" IN hParent) .
  IF VALID-HANDLE( hParentBrowse)  THEN
  DO:
      hQuery = DYNAMIC-FUNCTION("NewQuery"
              ,100
              ,""
              ,"TransBeskr"
              ,"WHERE false"
              ,"").

      DYNAMIC-FUNCTION("CreateOneToOneLink",hQuery,hParentBrowse,"tBid").


      hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                           and their corresponding buffer columns return handle equals the buffer handle */
                        hQuery,
                        FRAME {&FRAME-NAME}:HANDLE,   /* Frame for the input/display fields (might not be the same frame as the browse) */
                        "TBID,Beskrivelse,Innloser,KontoNr,Royalty%,VisPaBokfBilag,Notat",   /* Update columns in buffer */
                        "",                           /* Corresponding input fields (fill-in..). blank if equal to update columns */
                        "",                           /* Additional buffer and displ.fields - not updateable*/
                        "",                           /* Corresponding fill-ins */
                        "").                          /* other input widgets and lookup buttons for update fill-ins */
    

      DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hQuery).
      DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore"). 
      
/*       hToolBarParent = DYNAMIC-FUNCTION("getLinkedObject",hParentBrowse,"toolbar","from"). */
/*       DYNAMIC-FUNCTION("setToolbar",hToolBarParent,"disable").                             */

      hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                        rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                        "File",                         /* Corresponding menu label - no menu if blank */
                        "undo,delete,save,rule,first,prev,next,last"
                        ,"maxborder").                  /* Misc - enable, maxborder.. */
            
      DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).
      DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hToolbar).
      DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hFieldMap).

      RUN InvokeMethod(hParentBrowse,"DisplayRecord").
/*       DYNAMIC-FUNCTION("ReplaceObjectLink",hParentBrowse,hToolbar). /* the detail win toolbar should now be linked to the browse */ */
  
 END. 


  hWinToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,          /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "close",
                    "right,enable").                      /* Misc - enable, maxborder.. */

  IF VALID-HANDLE(hParentBrowse) THEN APPLY "value-changed" TO hParentBrowse.

END.

DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,150,0,250).
DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

TBId:SENSITIVE IN FRAME default-frame = FALSE.

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
    
    APPLY "entry" TO beskrivelse IN FRAME {&FRAME-NAME}.

 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newRecord C-Win 
PROCEDURE newRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setCurrentObject",hFieldMap).
  RUN SUPER.

/*     DO WITH FRAME Default-Frame: */
/*       TBId:SENSITIVE = TRUE.     */
/*       APPLY 'entry' TO TBId.     */
/*     END.                         */
/*                                  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*
    DEFINE VARIABLE iNextId AS INTEGER NO-UNDO.

    ittid = hFieldMap:BUFFER-FIELD("ttid"):BUFFER-VALUE. 
    iNextId = INTEGER(DYNAMIC-FUNCTION("getFieldValues","LAST transbeskr", 'WHERE ttid = ' + STRING(iTtid),"tbid")).
    iNextid  = IF iNextId = ? THEN 1 ELSE iNextId + 1.
                                          
    /*DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues",STRING(iNextid)).*/
    */
    
    RUN SUPER.

    DO WITH FRAME Default-Frame:
      TBId:SENSITIVE = FALSE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetParent C-Win 
PROCEDURE SetParent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iphParent AS HANDLE NO-UNDO. 

hparent = iphParent. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

