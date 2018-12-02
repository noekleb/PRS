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

DEF VAR hToolbar2       AS HANDLE NO-UNDO.
DEF VAR hBrowse2        AS HANDLE NO-UNDO.
DEF VAR hFieldMap2      AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnGyldigFra kupongid KTypeNr Aktiv IdKrav ~
TaVarePaKupong EANKode InterleaveKode KupBeskrivelse GyldigFra GyldigTil ~
SisteInnlDato Belop MinBelop MaksBelop RabattVerdi btnSisteInnlDato ~
KupongNotat KupVareKode btnGyldigTil rectBrowse rectToolBar rectWinToolbar ~
rectToolBar-2 rectBrowse-2 
&Scoped-Define DISPLAYED-OBJECTS kupongid KTypeNr Aktiv IdKrav ~
TaVarePaKupong EANKode InterleaveKode KupBeskrivelse GyldigFra GyldigTil ~
SisteInnlDato Belop MinBelop MaksBelop RabattVerdi KupongNotat KupVareKode ~
lblNotat 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnGyldigFra 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnGyldigTil 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSisteInnlDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE KTypeNr AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Kupongtype" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE KupongNotat AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 39 BY 3.81 NO-UNDO.

DEFINE VARIABLE Belop AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Beløp" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE EANKode AS CHARACTER FORMAT "X(256)":U 
     LABEL "EANkode" 
     VIEW-AS FILL-IN 
     SIZE 66 BY 1 NO-UNDO.

DEFINE VARIABLE GyldigFra AS DATE FORMAT "99/99/9999":U 
     LABEL "Gyldig fra/til" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE GyldigTil AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE InterleaveKode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Interleave kode" 
     VIEW-AS FILL-IN 
     SIZE 66 BY 1 NO-UNDO.

DEFINE VARIABLE KupBeskrivelse AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 66 BY 1 NO-UNDO.

DEFINE VARIABLE kupongid AS DECIMAL FORMAT "->>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Kupong id" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE KupVareKode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Vare kode" 
     VIEW-AS FILL-IN 
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE lblNotat AS CHARACTER FORMAT "X(256)":U INITIAL "Notat:" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE MaksBelop AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Maks beløp" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE MinBelop AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Min. beløp" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE RabattVerdi AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Rabatt verdi" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE SisteInnlDato AS DATE FORMAT "99/99/9999":U 
     LABEL "Siste innl.dato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45 BY 25.48.

DEFINE RECTANGLE rectBrowse-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82 BY 8.1.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectToolBar-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE VARIABLE Aktiv AS LOGICAL INITIAL no 
     LABEL "Aktiv" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE IdKrav AS LOGICAL INITIAL no 
     LABEL "Krav til ID" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE TaVarePaKupong AS LOGICAL INITIAL no 
     LABEL "Ta vare på kupong" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnGyldigFra AT ROW 9.81 COL 81 NO-TAB-STOP 
     kupongid AT ROW 2.38 COL 63 COLON-ALIGNED
     KTypeNr AT ROW 3.38 COL 63 COLON-ALIGNED
     Aktiv AT ROW 4.57 COL 65
     IdKrav AT ROW 5.29 COL 65
     TaVarePaKupong AT ROW 6 COL 65
     EANKode AT ROW 6.95 COL 63 COLON-ALIGNED
     InterleaveKode AT ROW 7.91 COL 63 COLON-ALIGNED
     KupBeskrivelse AT ROW 8.86 COL 63 COLON-ALIGNED
     GyldigFra AT ROW 9.81 COL 63 COLON-ALIGNED
     GyldigTil AT ROW 9.81 COL 84 COLON-ALIGNED NO-LABEL
     SisteInnlDato AT ROW 10.76 COL 63 COLON-ALIGNED
     Belop AT ROW 12.43 COL 63 COLON-ALIGNED
     MinBelop AT ROW 13.38 COL 63 COLON-ALIGNED
     MaksBelop AT ROW 14.33 COL 63 COLON-ALIGNED
     RabattVerdi AT ROW 15.29 COL 63 COLON-ALIGNED
     btnSisteInnlDato AT ROW 10.76 COL 81 NO-TAB-STOP 
     KupongNotat AT ROW 12.43 COL 92 NO-LABEL
     KupVareKode AT ROW 18.38 COL 58 COLON-ALIGNED
     btnGyldigTil AT ROW 9.81 COL 102 NO-TAB-STOP 
     lblNotat AT ROW 11.71 COL 90 COLON-ALIGNED NO-LABEL
     rectBrowse AT ROW 2.43 COL 2
     rectToolBar AT ROW 1.14 COL 1.8
     rectWinToolbar AT ROW 1.14 COL 121.8
     rectToolBar-2 AT ROW 16.95 COL 49
     rectBrowse-2 AT ROW 19.81 COL 49
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 131.4 BY 26.91.


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
         TITLE              = "Kupong register"
         HEIGHT             = 26.95
         WIDTH              = 131.8
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN lblNotat IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblNotat:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

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
ON END-ERROR OF C-Win /* Kupong register */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kupong register */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Kupong register */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGyldigFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGyldigFra C-Win
ON CHOOSE OF btnGyldigFra IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF btnGyldigFra
DO:
  RUN Cal.w (GyldigFra:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGyldigTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGyldigTil C-Win
ON CHOOSE OF btnGyldigTil IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF btnGyldigTil
DO:
  RUN Cal.w (GyldigTil:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSisteInnlDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSisteInnlDato C-Win
ON CHOOSE OF btnSisteInnlDato IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF btnSisteInnlDato
DO:
  RUN Cal.w (SisteInnlDato:HANDLE).
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
  DISPLAY kupongid KTypeNr Aktiv IdKrav TaVarePaKupong EANKode InterleaveKode 
          KupBeskrivelse GyldigFra GyldigTil SisteInnlDato Belop MinBelop 
          MaksBelop RabattVerdi KupongNotat KupVareKode lblNotat 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnGyldigFra kupongid KTypeNr Aktiv IdKrav TaVarePaKupong EANKode 
         InterleaveKode KupBeskrivelse GyldigFra GyldigTil SisteInnlDato Belop 
         MinBelop MaksBelop RabattVerdi btnSisteInnlDato KupongNotat 
         KupVareKode btnGyldigTil rectBrowse rectToolBar rectWinToolbar 
         rectToolBar-2 rectBrowse-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeChild C-Win 
PROCEDURE InitializeChild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      InitializeObject is called automatically when using JBoxDynMenu.w 
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  hBrowse2 = DYNAMIC-FUNCTION("NewBrowse",          /* Create a browse object */
                    rectBrowse-2:HANDLE,              /* Rectangle to define coordinates for browse */
                    100,                            /* Rows to batch */
                    "",                             /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "KupongVareKode"
                    + ";KupongId"
                    + ";KupVareKode"
                    + ";!RegistrertTid;!RegistrertDato;!RegistrertAv;!ETid;!EDato;!BrukerID"
                    ," WHERE false"
                    ,"sort|KupVareKode").             /* Initial sort column */
  
  hBrowse2:NAME = 'brwKVK'.
  hFieldMap2 = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowse2:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                      "KupVareKode",
                      "",                           /* Corresponding input fields (fill-in..). blank if equal to update columns */
                      "KupongId",        /* Additional buffer and displ.fields - not updateable*/
                      "",                           /* Corresponding fill-ins */
                    "").                            /* other input widgets and lookup buttons for update fill-ins */

  DYNAMIC-FUNCTION("setAttribute",hFieldMap2,"bufferextrafields","KupongId").
  
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap2,hBrowse2).

  hToolbar2 = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar-2:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "new,undo,delete,save"
/*                     + ",excel;Eksporter til E&xcel" /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. */
                                                       Any number of properties accepted (one ok - if predef. action) */
                    + ",Filter"
                    ,"").                  /* Misc - enable, maxborder.. */
  
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap2,hToolbar2).
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse2,hToolbar2).

  APPLY "value-changed" TO hBrowse2.

END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeHead C-Win 
PROCEDURE InitializeHead :
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
                    "Kupong"
                    + ";KupongId"
                    + ";KupBeskrivelse"
                    + ";!Belop"
                    + ";!MinBelop"
                    + ";!MaksBelop"
                    + ";!RabattVerdi"
                    + ";!KTypeNr"
                    + ";!GyldigFra"
                    + ";!GyldigTil"
                    + ";!SisteinnlDato"
                    + ";!EANKode"
                    + ";!InterleaveKode"
                    + ";!Aktiv"
                    + ";!IdKrav"
                    + ";!TaVarePaKupong"
                    + ";!KupongNotat"
                    + ";!RegistrertTid;!RegistrertDato;!RegistrertAv;!ETid;!EDato;!BrukerID"
                   + ",KupongType"
                    + ";!KTypeNr;KTypeNavn@2" 
                    ,"WHERE true"
                    + ", FIRST kupongType OF Kupong no-lock"
                    ,"sort|KupongId").             /* Initial sort column */

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                      "KTypeNr,Aktiv,IdKrav,TaVarePaKupong,EANKode,InterleaveKode,KupBeskrivelse,GyldigFra,GyldigTil,SisteInnlDato,Belop,MinBelop,MaksBelop,RabattVerdi,KupongNotat",
                      "",                           /* Corresponding input fields (fill-in..). blank if equal to update columns */
                      "KupongId",        /* Additional buffer and displ.fields - not updateable*/
                      "",                           /* Corresponding fill-ins */
                    "btnGyldigFra,btnGyldigTil,btnSisteInnlDato").                            /* other input widgets and lookup buttons for update fill-ins */

  ASSIGN 
    KTypeNr:DELIMITER = "|"
    KTypeNr:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("getFieldList","KupongType;KTypeNavn;KTypeNr","WHERE TRUE")
  .
  
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hBrowse).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "new,undo,delete,save"
                    + ",excel;Eksporter til E&xcel" /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    + ",Filter,BrowseConfig"
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


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:      
    RUN InitializeHead.
    RUN InitializeChild.
    
    DYNAMIC-FUNCTION("CreateParentLink",hBrowse2,hBrowse,"KupongId").  
    
    APPLY "value-changed" TO hBrowse.
    
    DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar,KupongNotat").
    DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "brwKVK,KupongNotat").
    DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,1000,0,250).
    
    DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
    THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF hFieldMap2:AVAIL THEN
      DYNAMIC-FUNCTION("setAttribute",hFieldMap2,"BufferExtraValues",string(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('KupongId'):BUFFER-VALUE)).
  RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

