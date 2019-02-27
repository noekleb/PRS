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

DEF VAR hToolbar            AS HANDLE NO-UNDO.
DEF VAR hBrowse             AS HANDLE NO-UNDO.
DEF VAR hFieldMap           AS HANDLE NO-UNDO.

DEFINE VARIABLE hDetailQuery           AS HANDLE        NO-UNDO.
DEFINE VARIABLE hParent                AS HANDLE        NO-UNDO.
DEFINE VARIABLE bCloseOnSelect         AS LOGICAL       NO-UNDO.
DEFINE VARIABLE hDetail                AS HANDLE        NO-UNDO.


/*Need to use it to differanciate between browser and detail regarding functionality for toolbar.
  When starting with browse, I need to start detailwindow, but not close the brwoser Program
  When starting with detail prorgram I will close the browse when defaultaction.*/

DEFINE VARIABLE bFlagToolbarUpdateable AS LOGICAL INIT TRUE    NO-UNDO.

{buildfunction.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnCalFraDato rectBrowse rectToolBar ~
rectWinToolbar rectSearchField eier_fornavn ButikkNr Rammenummer ~
fraSalgsdato tilSalgsdato btnCalTilDato eier_etternavn Arsmodell Eier_Mobil ~
Fabrikat Eier_Telefon Sykkeltype 
&Scoped-Define DISPLAYED-OBJECTS eier_fornavn ButikkNr Rammenummer ~
fraSalgsdato tilSalgsdato eier_etternavn Arsmodell Eier_Mobil Fabrikat ~
Eier_Telefon Sykkeltype 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBrowseHandle C-Win 
FUNCTION getBrowseHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setToolbarUpdateable C-Win 
FUNCTION setToolbarUpdateable RETURNS CHARACTER
  (INPUT ibToolbarUpdateable AS LOGICAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD startDetailWindow C-Win 
FUNCTION startDetailWindow RETURNS CHARACTER
  (INPUT icToolbarFormat AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalFraDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalTilDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE VARIABLE Arsmodell AS CHARACTER FORMAT "X(4)" 
     LABEL "Årsmodell" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE ButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikknr" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE eier_etternavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Etternavn" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE eier_fornavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fornavn" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE Eier_Mobil AS CHARACTER FORMAT "X(16)" 
     LABEL "Mobil" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE Eier_Telefon AS CHARACTER FORMAT "X(16)" 
     LABEL "Telefon" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE Fabrikat AS CHARACTER FORMAT "X(30)" 
     LABEL "Fabrikat" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE fraSalgsdato AS DATE FORMAT "99/99/99":U 
     LABEL "Salgsdato fra/til" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Rammenummer AS CHARACTER FORMAT "X(30)" 
     LABEL "Rammenr" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE Sykkeltype AS CHARACTER FORMAT "X(30)" 
     LABEL "Sykkeltype" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE tilSalgsdato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 201 BY 25.71.

DEFINE RECTANGLE rectSearchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnCalFraDato AT ROW 2.33 COL 137.8 
     eier_fornavn AT ROW 2.33 COL 13 COLON-ALIGNED
     ButikkNr AT ROW 2.33 COL 47.8 COLON-ALIGNED HELP
          "Butikknummer"
     Rammenummer AT ROW 2.33 COL 72 COLON-ALIGNED HELP
          "Rammenummer" 
     fraSalgsdato AT ROW 2.33 COL 121.8 COLON-ALIGNED
     tilSalgsdato AT ROW 2.33 COL 139.8 COLON-ALIGNED NO-LABEL
     btnCalTilDato AT ROW 2.33 COL 155.8
     eier_etternavn AT ROW 3.33 COL 13 COLON-ALIGNED
     Arsmodell AT ROW 3.33 COL 47.8 COLON-ALIGNED HELP
          "Årsmodell"
     Eier_Mobil AT ROW 3.33 COL 72 COLON-ALIGNED HELP
          "Eiers mobiltelefonnummer" 
     Fabrikat AT ROW 3.33 COL 121.8 COLON-ALIGNED HELP
          "Rammenummer"
     Eier_Telefon AT ROW 4.33 COL 72 COLON-ALIGNED HELP
          "Eiers mobiltelefonnummer"
     Sykkeltype AT ROW 4.33 COL 121.8 COLON-ALIGNED HELP
          "Rammenummer"
     rectBrowse AT ROW 6.95 COL 2
     rectToolBar AT ROW 1.14 COL 1.8
     rectWinToolbar AT ROW 1 COL 192.8
     rectSearchField AT ROW 5.95 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202.6 BY 31.86.


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
         TITLE              = "Falck sykkelregister"
         HEIGHT             = 31.86
         WIDTH              = 202.6
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
ON END-ERROR OF C-Win /* Falck sykkelregister */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Falck sykkelregister */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Falck sykkelregister */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Arsmodell
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Arsmodell C-Win
ON LEAVE OF Arsmodell IN FRAME DEFAULT-FRAME /* Årsmodell */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato C-Win
ON CHOOSE OF btnCalFraDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fraSalgsdato
DO:
  RUN Cal.w (fraSalgsdato:HANDLE).
  IF fraSalgsdato:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalTilDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalTilDato C-Win
ON CHOOSE OF btnCalTilDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF tilSalgsDato
DO:
  RUN Cal.w (tilSalgsDato:HANDLE).
  IF tilSalgsDato:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButikkNr C-Win
ON LEAVE OF ButikkNr IN FRAME DEFAULT-FRAME /* Butikknr */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eier_etternavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eier_etternavn C-Win
ON LEAVE OF eier_etternavn IN FRAME DEFAULT-FRAME /* Etternavn */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eier_fornavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eier_fornavn C-Win
ON LEAVE OF eier_fornavn IN FRAME DEFAULT-FRAME /* Fornavn */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Eier_Mobil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Eier_Mobil C-Win
ON LEAVE OF Eier_Mobil IN FRAME DEFAULT-FRAME /* Mobil */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Eier_Telefon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Eier_Telefon C-Win
ON LEAVE OF Eier_Telefon IN FRAME DEFAULT-FRAME /* Telefon */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Fabrikat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fabrikat C-Win
ON LEAVE OF Fabrikat IN FRAME DEFAULT-FRAME /* Fabrikat */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fraSalgsdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fraSalgsdato C-Win
ON LEAVE OF fraSalgsdato IN FRAME DEFAULT-FRAME /* Salgsdato fra/til */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rammenummer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rammenummer C-Win
ON LEAVE OF Rammenummer IN FRAME DEFAULT-FRAME /* Rammenr */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Sykkeltype
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Sykkeltype C-Win
ON LEAVE OF Sykkeltype IN FRAME DEFAULT-FRAME /* Sykkeltype */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tilSalgsdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tilSalgsdato C-Win
ON LEAVE OF tilSalgsdato IN FRAME DEFAULT-FRAME
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
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
{incl/supptrigg.i hBrowse}
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.
  hParent = SOURCE-PROCEDURE.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF
    
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearFilterRecord C-Win 
PROCEDURE ClearFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN 
    Arsmodell:SCREEN-VALUE      = ''
    ButikkNr:SCREEN-VALUE       = ''
    eier_etternavn:SCREEN-VALUE = ''
    eier_fornavn:SCREEN-VALUE   = ''
    Eier_Mobil:SCREEN-VALUE     = ''
    fraSalgsdato:SCREEN-VALUE   = ''
    tilSalgsdato:SCREEN-VALUE   = ''
    Rammenummer:SCREEN-VALUE    = ''
    Arsmodell:MODIFIED      = FALSE
    ButikkNr:MODIFIED       = FALSE
    eier_etternavn:MODIFIED = FALSE
    eier_fornavn:MODIFIED   = FALSE
    Eier_Mobil:MODIFIED     = FALSE
    fraSalgsdato:MODIFIED   = FALSE
    tilSalgsdato:MODIFIED   = FALSE
    Rammenummer:MODIFIED    = FALSE

  .  
  RUN InvokeMethod (hBrowse,'OpenQuery').
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN 
DO:

  IF bCloseOnSelect THEN 
    APPLY "close" TO THIS-PROCEDURE.
  ELSE 
  DO:
    startDetailWindow('Update').
/*     DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBrowse:QUERY::BUFFER-FIELD("RowIdent1"):BUFFER-VALUE). */
    IF bOk THEN APPLY "entry" TO hDetailQuery.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY eier_fornavn ButikkNr Rammenummer fraSalgsdato tilSalgsdato 
          eier_etternavn Arsmodell Eier_Mobil Fabrikat Eier_Telefon Sykkeltype 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnCalFraDato rectBrowse rectToolBar rectWinToolbar rectSearchField 
         eier_fornavn ButikkNr Rammenummer fraSalgsdato tilSalgsdato 
         btnCalTilDato eier_etternavn Arsmodell Eier_Mobil Fabrikat 
         Eier_Telefon Sykkeltype 
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
  bCloseOnSelect = IF NOT bFlagToolbarUpdateable THEN TRUE ELSE FALSE.

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",          /* Create a browse object */
                    rectBrowse:HANDLE,              /* Rectangle to define coordinates for browse */
                    100,                            /* Rows to batch */
                    "",                             /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                   "Falck_Sykkelregister"
                   + ";ButikkNr|Butikk|>>>>>9"
                   + ";Transsaksjonsnumer|Trans.nr"
                   + ";Eier_Fornavn|Fornavn|x(15)"
                   + ";Eier_Etternavn|Etternavn|x(20)"
                   + ";Kjonn|Kjønn"
                   + ";Eier_Mobil|Mobil|x(10)"
                   + ";Eier_Telefon|Telefon|x(10)"
                   + ";Eier_epost|eMail|x(25)"
                   + ";Fabrikat|Fabrikat|x(25)"
                   + ";Sykkeltype|Sykkeltype|x(25)"
                   + ";Rammenummer|Rammenr.|x(25)"
                   + ";Arsmodell"
                   + ";Pris"
                   + ";Dato_Solgt|Solgt"
                   + ";EksportId"
                   + ";Eier_Adresse"
                   + ";eier_postnr"
                   + ";Eier_Fodt"
                   + ";Foresatt_ForNavn|Foresatt fornavn"
                   + ";Foresatt_Etternavn|Foresatt etternavn"
                   + ";Foresatt_Fodt|Foresatt født"
                   + ",post;Beskrivelse"
                   ,"WHERE false"
                    + ", FIRST post outer-join NO-LOCK WHERE post.postnr = eier_postnr"
                    ,"sort|Transsaksjonsnumer").             /* Initial sort column */
  
  DYNAMIC-FUNCTION("NewBrowseSearchField",rectSearchField:HANDLE,hBrowse,1).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                     (IF bFlagToolbarUpdateable THEN "New,Undo,Save,Delete,print,rule," ELSE '') +
                    "Excel,clearFilter;Blank &filter¤ENABLE" /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    ,"maxborder").                  /* Misc - enable, maxborder.. */
  
/*   DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar). */

  DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,          /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "close",
                    "right,enable").                      /* Misc - enable, maxborder.. */
  
  DYNAMIC-FUNCTION("LinkAllObjects",                /* Link all created objects. Linktype is type of "to" object,
                                                      f.ex link from browse to combo-box is combo-box link */
                    THIS-PROCEDURE:CURRENT-WINDOW,  /* Link only objects created for current window */
                    TRUE,                           /* Replace any existing links */
                    "").                            /* Except these objects - the combo must only be linked to one browse */

  RUN InvokeMethod(hBrowse,"OpenQuery").
/*   APPLY "value-changed" TO hBrowse.  */

END.

DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,500,250,0,250).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvalidateHandle C-Win 
PROCEDURE InvalidateHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ihProgram AS HANDLE NO-UNDO.

  DYNAMIC-FUNCTION('setToolbar',hToolbar,'avail').

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
  startDetailWindow('New').
  
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
DEF VAR cWhere            AS CHAR NO-UNDO.
DEF VAR cSalgsdato          AS CHAR NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:

    ASSIGN
        cSalgsdato = '¤' + 
                   (IF INPUT fraSalgsDato <> ? THEN STRING(INPUT fraSalgsDato) ELSE '') + '¤' +
                   (IF INPUT tilSalgsDato <> ? THEN STRING(INPUT tilSalgsDato) ELSE '')
        .

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",''). 
    
    cWhere = buildFilter(cWhere,eier_fornavn:HANDLE,'eier_fornavn','BEGINS').      
    cWhere = cWhere + buildFilter(cWhere,eier_etternavn:HANDLE,'eier_etternavn','BEGINS').
    cWhere = cWhere + buildFilter(cWhere,fraSalgsDato:HANDLE,'dato_solgt','GE').
    cWhere = cWhere + buildFilter(cWhere,tilSalgsDato:HANDLE,'dato_solgt','LE').
    cWhere = cWhere + buildFilter(cWhere,butikknr:HANDLE,'butikknr','EQ').
    cWhere = cWhere + buildFilter(cWhere,arsmodell:HANDLE,'arsmodell','EQ').
    cWhere = cWhere + buildFilter(cWhere,Eier_Mobil:HANDLE,'Eier_Mobil','EQ').
    cWhere = cWhere + buildFilter(cWhere,Eier_Telefon:HANDLE,'Eier_Telefon','EQ').
    cWhere = cWhere + buildFilter(cWhere,Fabrikat:HANDLE,'Fabrikat','EQ').
    cWhere = cWhere + buildFilter(cWhere,Sykkeltype:HANDLE,'Sykkeltype','EQ').

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cWhere). 

    ASSIGN
      fraSalgsDato:MODIFIED   = FALSE 
      tilSalgsDato:MODIFIED   = FALSE  
      eier_fornavn:MODIFIED   = FALSE  
      eier_etternavn:MODIFIED = FALSE   
      butikknr:MODIFIED       = FALSE
      arsmodell:MODIFIED      = FALSE 
      rammenummer:MODIFIED    = FALSE 
      Eier_Mobil:MODIFIED     = FALSE
      Eier_Telefon:MODIFIED   = FALSE
    .

  END.

  RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBrowseHandle C-Win 
FUNCTION getBrowseHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN hBrowse.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setToolbarUpdateable C-Win 
FUNCTION setToolbarUpdateable RETURNS CHARACTER
  (INPUT ibToolbarUpdateable AS LOGICAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  bFlagToolbarUpdateable = ibToolbarUpdateable.

  RETURN ''.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION startDetailWindow C-Win 
FUNCTION startDetailWindow RETURNS CHARACTER
  (INPUT icToolbarFormat AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
   
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hMyToolbar AS HANDLE      NO-UNDO.
  IF NOT VALID-HANDLE(hDetail) THEN
  DO:
    RUN falck_detail.w PERSISTEN SET hDetail.
    IF VALID-HANDLE(hDetail) THEN 
    DO:
      DYNAMIC-FUNCTION('setParentType' IN hDetail,'Browse').
      RUN initializeObject IN hDetail.
      SUBSCRIBE TO "InvalidateHandle" IN hDetail.
      hDetailQuery = DYNAMIC-FUNCTION("getQueryhandle" IN hDetail).
      
      DYNAMIC-FUNCTION("CreateOneToOneLink",hDetailQuery,hBrowse,"Transsaksjonsnumer").
  
  /*     RUN MoveToTop IN hDetail. */
    END.
  END.
  
  
  CASE icToolbarFormat:
    WHEN 'Update' THEN
    DO:
      RUN InvokeMethod(hBrowse,"DisplayRecord").
    END.
    WHEN 'New' THEN
    DO:
      hMyToolbar = DYNAMIC-FUNCTION('getLinkedObject',hDetailQuery,'Toolbar','from').
      RUN InvokeMethod(hMyToolbar,"NewRecord").      
      DYNAMIC-FUNCTION('setToolbar',hToolbar,'disable').      

    END.
    WHEN 'Delete' THEN
    DO:
        
    END.
  END CASE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

