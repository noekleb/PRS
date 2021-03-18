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

DEF VAR bOK               AS LOG NO-UNDO.
DEF VAR ix                AS INT NO-UNDO.
DEF VAR iCl               AS INT NO-UNDO.
                          
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hBrowseLev        AS HANDLE NO-UNDO.
DEF VAR hBrowseUserGroup  AS HANDLE NO-UNDO.
DEF VAR hBrowseCompany    AS HANDLE NO-UNDO.
DEF VAR hToolbarCompany   AS HANDLE NO-UNDO.
DEF VAR hAdminOverlay     AS HANDLE NO-UNDO.
DEF VAR hSearchField      AS HANDLE NO-UNDO.
DEF VAR hBrwColSuperUser  AS HANDLE NO-UNDO.
DEF VAR hBrwColCompAdmin  AS HANDLE NO-UNDO.
DEF VAR iFontWingdings    AS INT    NO-UNDO.
DEF VAR cDefaultCompany   AS CHAR   NO-UNDO.

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

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolBar BrwUserGroup ~
rectWinToolbar RectBrowseSearch BrwLev brwCompanyUser cJBoxUserId cUserName ~
cEmail cWrkPhone cCellPhone BrGrpNr ButikkNr btnButikkNr ButNamn BrukerType ~
Lng bSuperUser fi-cPwd1 fi-cPwd2 
&Scoped-Define DISPLAYED-OBJECTS cJBoxUserId cUserName cEmail cWrkPhone ~
cCellPhone BrGrpNr ButikkNr ButNamn BrukerType Lng bSuperUser fi-cPwd1 ~
fi-cPwd2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnButikkNr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE VARIABLE BrGrpNr AS INTEGER FORMAT "zz9" INITIAL 0 
     LABEL "But.gruppe" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 40.8 BY 1 TOOLTIP "Brukergruppenummer.".

DEFINE VARIABLE BrukerType AS INTEGER FORMAT "zz9" INITIAL 0 
     LABEL "Brukertype" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 40.8 BY 1 TOOLTIP "Brukergruppenummer.".

DEFINE VARIABLE Lng AS CHARACTER FORMAT "X(3)" INITIAL "DES" 
     LABEL "Språkkode" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "DES" 
     DROP-DOWN-LIST
     SIZE 10.6 BY 1 TOOLTIP "Kobler bruker til språk som bruker skal benytte.".

DEFINE VARIABLE ButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Std but.nr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1 TOOLTIP "Butikktilhøringhet".

DEFINE VARIABLE ButNamn AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 26.4 BY 1 TOOLTIP "Butikkens navn".

DEFINE VARIABLE cCellPhone AS CHARACTER FORMAT "x(15)" 
     LABEL "Mobil" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE cEmail AS CHARACTER FORMAT "x(50)" 
     LABEL "Email" 
     VIEW-AS FILL-IN 
     SIZE 40.8 BY 1.

DEFINE VARIABLE cJBoxUserId AS CHARACTER FORMAT "X(40)" 
     LABEL "Brukerid" 
     VIEW-AS FILL-IN 
     SIZE 40.8 BY 1 NO-UNDO.

DEFINE VARIABLE cUserName AS CHARACTER FORMAT "X(50)" 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 40.8 BY 1 NO-UNDO.

DEFINE VARIABLE cWrkPhone AS CHARACTER FORMAT "x(15)" 
     LABEL "Tlf.arb" 
     VIEW-AS FILL-IN 
     SIZE 15.4 BY 1.

DEFINE VARIABLE fi-cPwd1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Passord" 
     VIEW-AS FILL-IN 
     SIZE 15.4 BY 1 TOOLTIP "Passord må bare angis ved registrering - kan også endres" NO-UNDO.

DEFINE VARIABLE fi-cPwd2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bekreft" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE RECTANGLE brwCompanyUser
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35.6 BY 6.86.

DEFINE RECTANGLE BrwLev
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31.8 BY 6.86.

DEFINE RECTANGLE BrwUserGroup
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26.4 BY 6.86.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 17.62.

DEFINE RECTANGLE RectBrowseSearch
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY .95.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE VARIABLE bSuperUser AS LOGICAL INITIAL yes 
     LABEL "Superbruker" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 TOOLTIP "Superbruker kan administrere alle firma" NO-UNDO.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tableft.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 1.6 BY 17.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cJBoxUserId AT ROW 3.52 COL 60.2 COLON-ALIGNED
     cUserName AT ROW 4.57 COL 60.2 COLON-ALIGNED
     cEmail AT ROW 5.62 COL 60.2 COLON-ALIGNED
     cWrkPhone AT ROW 6.67 COL 60.2 COLON-ALIGNED
     cCellPhone AT ROW 6.67 COL 86 COLON-ALIGNED
     BrGrpNr AT ROW 8 COL 60.2 COLON-ALIGNED
     ButikkNr AT ROW 9.05 COL 60.2 COLON-ALIGNED
     btnButikkNr AT ROW 9.05 COL 72.6 NO-TAB-STOP 
     ButNamn AT ROW 9.05 COL 74.6 COLON-ALIGNED NO-LABEL
     BrukerType AT ROW 10.14 COL 60.2 COLON-ALIGNED
     Lng AT ROW 11.52 COL 60.2 COLON-ALIGNED
     bSuperUser AT ROW 11.67 COL 88
     fi-cPwd1 AT ROW 12.86 COL 60.2 COLON-ALIGNED
     fi-cPwd2 AT ROW 12.86 COL 86 COLON-ALIGNED
     rectBrowse AT ROW 3.62 COL 2
     rectToolBar AT ROW 1.14 COL 1.8
     BrwUserGroup AT ROW 14.33 COL 44.6
     rectWinToolbar AT ROW 1.14 COL 129.8
     RectBrowseSearch AT ROW 2.38 COL 2.2
     BrwLev AT ROW 14.33 COL 72
     brwCompanyUser AT ROW 14.33 COL 104.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 139.8 BY 20.48.

DEFINE FRAME frmSplitBarX
     btnSplitBarX AT ROW 1.24 COL 13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 29.8 ROW 3.62
         SIZE 16.2 BY 17.62.


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
         TITLE              = "Vedlikehold brukere"
         HEIGHT             = 20.48
         WIDTH              = 139.8
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("ico/cntrlhry.ico":U) THEN
    MESSAGE "Unable to load icon: ico/cntrlhry.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
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
/* REPARENT FRAME */
ASSIGN FRAME frmSplitBarX:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       cJBoxUserId:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FRAME frmSplitBarX
                                                                        */
ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frmSplitBarX          = TRUE.

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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmSplitBarX
/* Query rebuild information for FRAME frmSplitBarX
     _Query            is NOT OPENED
*/  /* FRAME frmSplitBarX */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Vedlikehold brukere */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vedlikehold brukere */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Vedlikehold brukere */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
  DEF VAR hColumn AS HANDLE NO-UNDO.
  hColumn = hBrowseLev:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bSuperUser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bSuperUser C-Win
ON VALUE-CHANGED OF bSuperUser IN FRAME DEFAULT-FRAME /* Superbruker */
DO:
  ASSIGN bSuperUser.
  IF bSuperUser THEN
    HIDE {&List-1}.
  ELSE VIEW {&List-1}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnButikkNr C-Win
ON CHOOSE OF btnButikkNr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,200,
                    "Butiker"
                    + ";Butik"
                    + ";Butnamn"
                   ,"WHERE true"
                    ,""                                                  
                    ,"Butik,Butnamn",   /* <- return values for these fields */
                    OUTPUT cReturnValues,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    ASSIGN ButikkNr:SCREEN-VALUE = ENTRY(1,cReturnValues,"|")
           Butnamn:SCREEN-VALUE  = ENTRY(2,cReturnValues,"|")
           .

    APPLY "any-printable" TO ButikkNr.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmSplitBarX
&Scoped-define SELF-NAME btnSplitBarX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarX C-Win
ON END-MOVE OF btnSplitBarX IN FRAME frmSplitBarX /* Button 1 */
DO:
  DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,NO).
  DEF VAR hColumn AS HANDLE NO-UNDO.
  hColumn = hBrowseLev:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
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


  iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","").
  RUN enable_UI.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AnyPrintableKey C-Win 
PROCEDURE AnyPrintableKey :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

DO WITH FRAME {&FRAME-NAME}:
  IF DYNAMIC-FUNCTION("getCurrentWidget") = fi-cPwd1:HANDLE THEN
    RUN PostPWChar(fi-cPwd1:HWND).
  IF DYNAMIC-FUNCTION("getCurrentWidget") = fi-cPwd2:HANDLE THEN
    RUN PostPWChar(fi-cPwd2:HWND).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR bOk AS LOG NO-UNDO.

  MESSAGE 'Skal bruker ' ' tas bort?'
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.

  IF bOk THEN DO:
      DYNAMIC-FUNCTION("runproc",
                         "bruker_delete.p",
                         STRING(hFieldMap:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE),
                         ?
                       ).
  END.

  RUN InvokeMethod (hBrowse,'OpenQuery').
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

ASSIGN cJBoxUserId:READ-ONLY IN FRAME {&FRAME-NAME} = TRUE
       fi-cPwd1:SCREEN-VALUE = "*****************"
       fi-cPwd2:SCREEN-VALUE = "*****************"
       .

DYNAMIC-FUNCTION("CheckModified",FRAME {&FRAME-NAME}:HANDLE,"clear").
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
  DISPLAY cJBoxUserId cUserName cEmail cWrkPhone cCellPhone BrGrpNr ButikkNr 
          ButNamn BrukerType Lng bSuperUser fi-cPwd1 fi-cPwd2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectBrowse rectToolBar BrwUserGroup rectWinToolbar RectBrowseSearch 
         BrwLev brwCompanyUser cJBoxUserId cUserName cEmail cWrkPhone 
         cCellPhone BrGrpNr ButikkNr btnButikkNr ButNamn BrukerType Lng 
         bSuperUser fi-cPwd1 fi-cPwd2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frmSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmSplitBarX}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraDeleteRecord C-Win 
PROCEDURE ExtraDeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM obOk AS LOG NO-UNDO INIT YES.

/* obOk = DYNAMIC-FUNCTION("DoDelete","JBoxCompanyUser","Avail",               */
/*                  "cJBoxUserId,iJBoxCompanyId",                              */
/*                  hFieldMap:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE + "|1", */
/*                  NO).                                                       */

obOk = DYNAMIC-FUNCTION("DoDelete","Bruker","",
                 "BrukerId",
                 hFieldMap:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE,
                 NO).

obOk = DYNAMIC-FUNCTION("DoDelete","_user","",
                 "_userid",
                 hFieldMap:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE,
                 YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraSaveRecord C-Win 
PROCEDURE ExtraSaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icTBstate AS CHAR NO-UNDO. /* Toolbar status */
DEF OUTPUT PARAM obOk      AS LOG NO-UNDO INIT TRUE.

DEF VAR cEncoPass   AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF fi-cPwd1:MODIFIED OR fi-cPwd2:MODIFIED OR icTBstate = "new" THEN DO:
    ASSIGN fi-cPwd1 fi-cPwd2.
    IF fi-cPwd1 = "" THEN DO:
      MESSAGE "Passord må oppgis"
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fi-cPwd1.
      obOk = FALSE.
      RETURN ERROR.
    END.
    IF fi-cPwd1 NE fi-cPwd2 THEN DO:
      MESSAGE "Feil i bekreftelse av passord"
              VIEW-AS ALERT-BOX ERROR.
      obOk = FALSE.
      APPLY "entry" TO fi-cPwd1.
      RETURN ERROR.
    END.
    ELSE DO:
      cEncoPass = ENCODE(fi-cPwd1).
      DYNAMIC-FUNCTION("DoDelete","skotex._User","Avail",
            "_UserId",
            cJBoxUserId:SCREEN-VALUE,
            FALSE).
      DYNAMIC-FUNCTION("DoCreate","skotex._User","",
            "_UserId,_password",
            cJBoxUserId:SCREEN-VALUE + "|" + cEncoPass,
            FALSE).
    END.
  END.
  IF icTBstate = "new" THEN DO:
    DYNAMIC-FUNCTION("DoDelete","Bruker","Avail",
        "BrukerId",
        cJBoxUserId:SCREEN-VALUE,
        FALSE).
    DYNAMIC-FUNCTION("DoCreate","Bruker","Ignore",
        "BrukerId,Navn,ButikkNr,BrGrpNr,Lng,BrukerType",
        cJBoxUserId:SCREEN-VALUE + "|" 
      + cUserName:SCREEN-VALUE + "|"
      + ButikkNr:SCREEN-VALUE + "|"
      + BrGrpNr:SCREEN-VALUE + "|"
      + Lng:SCREEN-VALUE + "|"
      + BrukerType:SCREEN-VALUE
       ,FALSE).
    DYNAMIC-FUNCTION("DoDelete","JBoxCompanyUser","Avail",
        "cJBoxUserId,iJBoxCompanyId",
        cJBoxUserId:SCREEN-VALUE + "|1",
        FALSE).
    IF cDefaultCompany NE "" THEN
      DYNAMIC-FUNCTION("DoCreate","JBoxCompanyUser","Ignore",
          "cJBoxUserId,iJBoxCompanyId",
          cJBoxUserId:SCREEN-VALUE + "|1" 
         ,FALSE).
  END.
  ELSE DO:
    IF DYNAMIC-FUNCTION("getRowidList","bruker","",
                        "WHERE BrukerId = '" + cJBoxUserId:SCREEN-VALUE + "'") NE hFieldMap:BUFFER-FIELD("RowIdent2"):BUFFER-VALUE
                        THEN DO:
      MESSAGE "Oppdatering av tilsvarende bruker i tabellen Bruker feilet (Bruker eksisterer ikke)"
               VIEW-AS ALERT-BOX INFO BUTTONS OK. 
      obOk = NO.
      RETURN.
    END.
    IF DYNAMIC-FUNCTION("getFieldValues","bruker","WHERE brukerid = '" + cJBoxUserId:SCREEN-VALUE + "'",
                        "brukerid") = ? THEN DO:
      MESSAGE "Oppdatering av tilsvarende bruker i tabellen Bruker feilet (Bruker eksisterer ikke)"
               VIEW-AS ALERT-BOX INFO BUTTONS OK.        
      obOk = NO.
      RETURN.
    END.
    DYNAMIC-FUNCTION("DoUpdate","Bruker","Ignore","",
        hFieldMap:BUFFER-FIELD("RowIdent2"):BUFFER-VALUE,
        "Navn,ButikkNr,BrGrpNr,Lng,BrukerType",
        cUserName:SCREEN-VALUE + "|"
      + ButikkNr:SCREEN-VALUE + "|"
      + BrGrpNr:SCREEN-VALUE + "|"
      + Lng:SCREEN-VALUE + "|"
      + BrukerType:SCREEN-VALUE
       ,FALSE).
  END.

END.

obOk = DYNAMIC-FUNCTION("DoCommit",TRUE).

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

  ASSIGN iCl = INT(DYNAMIC-FUNCTION("getFieldValues","SysPara",
                             "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1")) 

         BrGrpNr:DELIMITER = "|"
         BrGrpNr:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                    "BrukerGrp;Beskrivelse;BrGrpNr","WHERE true")
         Lng:DELIMITER = "|"
         Lng:LIST-ITEMS = DYNAMIC-FUNCTION("getFieldList","sprak;Lng","WHERE true")
         
         BrukerType:DELIMITER = "|"
         BrukerType:LIST-ITEM-PAIRS = 'Kjede/System (1)|1|Butikk (2)|2|Leverandør (3)|3'
        
         cDefaultCompany = DYNAMIC-FUNCTION("getFieldList","JBoxCompany;iJBoxCompanyId","WHERE true")
         .

  IF NUM-ENTRIES(cDefaultCompany,"|") > 1 THEN 
    ASSIGN cDefaultCompany = DYNAMIC-FUNCTION("getFieldList","JBoxCompanyUser;iJBoxCompanyId","WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "'")
           cDefaultCompany = ENTRY(1,cDefaultCompany,"|").


  hBrowse = DYNAMIC-FUNCTION("NewBrowse",          
                    rectBrowse:HANDLE,             
                    100,                           
                    "",                            
                    "JBoxUser"                   
                    + ";cJBoxUserId"
                    + ";cUserName"
                    + ";bSuperUser"
                    + ";cWrkPhone|Tlf"
                    + ";cCellPhone|Mobil"
                    + ";cEmail|Epost"
                    + ";dCreated"
                    + ";cCreatedBy"
                    + ";dModified"
                    + ";cModifiedBy"
                  + ",Bruker"
                    + ";BrGrpNr"
                    + ";ButikkNr"
                    + ";BrukerType"
                    + ";Lng"
                  + ",Butiker"
                    + ";ButNamn"
                    ,"WHERE false"
                    + ",FIRST Bruker NO-LOCK WHERE Bruker.BrukerID = JBoxUser.cJBoxUserId"
                    + ",FIRST Butiker NO-LOCK WHERE Butiker.Butik = Bruker.ButikkNr OUTER-JOIN"
                    ,"").
  hBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 60.
  hBrwColSuperUser = hBrowse:GET-BROWSE-COLUMN(3).

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearch:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hSearchField).

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",     
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,
                    "cJBoxUserId,cUserName,bSuperUser,cWrkPhone,cCellPhone,cEmail,BrGrpNr,ButikkNr,BrukerType,Lng",
                    "",
                    "ButNamn","",
                    "fi-cPwd1,fi-cPwd2,btnButikkNr").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"CustomUpdateValProc","Ignore").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,
                    "Fil",
                    "new;Ny,undo;Angre,delete;Slett,save;Lagre,filter,flatview,excel;Eksporter til E&xcel"
/*                   + ",UserCompany;Knytt til firma */
                  + ",MemberOf;Medlem av"
                  + ",LevBruker;Knytt til lev"
                  + ",UserCompany;Knytt til firma"
                    ,"maxborder").
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hBrowse).
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hFieldMap).

  hBrowseUserGroup = DYNAMIC-FUNCTION("NewBrowse"
                    ,BrwUserGroup:HANDLE
                    ,100
                    ,""
                    ,"JBoxUserGroupMembers"
                     + ";!iJboxUserGroupId"
                  + ",JBoxUserGroup"
                     + ";cUserGroupName|Brukergruppe"
                    ,"WHERE false,FIRST JBoxUserGroup NO-LOCK OF JBoxUserGroupMembers"
                    ,"").
  DYNAMIC-FUNCTION("CreateParentLink",hBrowseUserGroup,hBrowse,"cJBoxUserId").

  hBrowseLev = DYNAMIC-FUNCTION("NewBrowse", 
                    BrwLev:HANDLE,      
                    100,
                    "",
                    "BrukerLev"
                    + ";LevNr;!BrukerId"
                    + ",LevBas"
                    + ";LevNamn",
                    "WHERE false, FIRST LevBas NO-LOCK WHERE LevBas.levnr = BrukerLev.LevNr",
                    "").
  DYNAMIC-FUNCTION("CreateParentLink",hBrowseLev,hBrowse,"BrukerId;cJBoxUserId").

  hBrowseCompany = DYNAMIC-FUNCTION("NewBrowse",  
                    brwCompanyUser:HANDLE,    
                    100,                          
                    "",                           
                    "JBoxCompanyUser"             
                    + ";bSuperUserCompany;!cJBoxUserId"
                    + ",JBoxCompany"
                    + ";cCompanyName|Firmanavn",
                    "WHERE false, FIRST JBoxCompany OF JBoxCompanyUser", 
                    "").                          
  hBrowseCompany:NAME = "brwCompanyUser".         
  DYNAMIC-FUNCTION("CreateParentLink",hBrowseCompany,hBrowse,"cJBoxUserId").

  hBrwColCompAdmin = hBrowseCompany:GET-BROWSE-COLUMN(1).

  hAdminOverlay = DYNAMIC-FUNCTION("NewBrowseToggle",
                    hBrowse,          
                    "bSuperUserCompany",     
                    "bSuperUserCompany",
                    "").                                         
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseCompany,hAdminOverlay,"bSuperUserCompany").
  DYNAMIC-FUNCTION("setAttribute",hAdminOverlay,"visible","yes").

  DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,           /* Rectangle to define coordinates for toolbar */
                    "Fil",                           /* Corresponding menu label - no menu if blank */
                    "close;Avslutt",
                                                     /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    "right,enable").                  /* Misc - enable, maxborder.. */

  RUN InvokeMethod(hBrowse,"OpenQuery").

END.

DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,NO).
DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,
                  STRING(hBrowse) + "," +
                  STRING(rectBrowse:HANDLE) + "," +
                  STRING(hBrowseUserGroup) + "," +
                  STRING(BrwUserGroup:HANDLE) + "," +

                  STRING(cWrkPhone:HANDLE) + "," +
                  STRING(cEmail:HANDLE) + "," +
                  STRING(cJBoxUserId:HANDLE) + "," +
                  STRING(cUserName:HANDLE) + "," +
                  STRING(fi-cPwd1:HANDLE) + "," +
                  STRING(BrukerType:HANDLE) + "," +
                  STRING(Lng:HANDLE) + "," +
                  STRING(BrGrpNr:HANDLE) + "," + 
                  STRING(ButikkNr:HANDLE) + "," + 
                  STRING(btnButikkNr:HANDLE) + "," +
                  STRING(ButNamn:HANDLE)
                  ).

DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 hBrowse:NAME + ",rectBrowse,"
               + hBrowseCompany:NAME + ",brwCompanyUser,"
               + hBrowseLev:NAME + ",BrwLev").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar").
DYNAMIC-FUNCTION("setAddResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "cJBoxUserId,cUserName,cEmail,BrGrpNr,ButNamn,BrukerType").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,250,350,0,250).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveOfFieldHandle C-Win 
PROCEDURE LeaveOfFieldHandle :
/*------------------------------------------------------------------------------
  Purpose:     Retrieve lookup values when a foreign key field is modified
  Parameters:  Handle to foreign key field
  Notes:       The cReturn variable should be replaced with a fill-in
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihField AS HANDLE NO-UNDO.

DEF VAR cReturn AS CHAR NO-UNDO.

IF ihField:MODIFIED THEN DO WITH FRAME {&FRAME-NAME}:
  CASE ihField:NAME:
    WHEN "ButikkNr" THEN ButNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE Butik = '" + ihField:SCREEN-VALUE + "'","Butnamn").
  END CASE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LevBrukerRecord C-Win 
PROCEDURE LevBrukerRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cBrukerLevRowIdList AS CHAR NO-UNDO.
DEF VAR cBrukerLevIdList    AS CHAR NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
IF hBrowseLev:QUERY:IS-OPEN THEN DO:
  hBrowseLev:QUERY:GET-FIRST().
  REPEAT WHILE NOT hBrowseLev:QUERY:QUERY-OFF-END:
    cBrukerLevRowIdList = cBrukerLevRowIdList + hBrowseLev:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent2"):BUFFER-VALUE + ",".
    hBrowseLev:QUERY:GET-NEXT().
  END.
  cBrukerLevRowIdList = TRIM(cBrukerLevRowIdList,",").
END.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "LevBas;LevNr;LevNamn",
                    "where true",
                    INPUT-OUTPUT cBrukerLevRowIdList,
                    "LevNr",
                    INPUT-OUTPUT cBrukerLevIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  IF NOT DYNAMIC-FUNCTION("runproc","brukerlev_edit_user_lev.p",
                          hFieldMap:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE + "|" + cBrukerLevIdList,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
  ELSE APPLY "value-changed" TO hBrowse.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MemberOfRecord C-Win 
PROCEDURE MemberOfRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cGroupRowIdList AS CHAR NO-UNDO.
DEF VAR cGroupIdList    AS CHAR NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.

cGroupRowIdList = DYNAMIC-FUNCTION("getRowIdList","JBoxUserGroupMembers,JBoxUserGroup","",
                                   "WHERE cJBoxUserId = '" + hFieldMap:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE 
                                 + "',FIRST JBoxUserGroup OF JBoxUserGroupMembers NO-LOCK"). 
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "JBoxUserGroup;cUserGroupName|Gruppe;!iJBoxUserGroupId",
                    "where true",
                    INPUT-OUTPUT cGroupRowIdList,
                    "iJBoxUserGroupId",
                    INPUT-OUTPUT cGroupIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  IF NOT DYNAMIC-FUNCTION("runproc","jbadmin_edituser_to_groups.p",
                          hFieldMap:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE + "|" + cGroupIdList,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
  ELSE APPLY "value-changed" TO hBrowse.
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
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  
{&WINDOW-NAME}:WINDOW-STATE = 3.

{&WINDOW-NAME}:MOVE-TO-TOP().
APPLY "end-move" TO btnSplitBarX IN FRAME frmSplitBarX.
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
DYNAMIC-FUNCTION("setAttribute",hBrowseLev,"basequery","where false").
RUN InvokeMethod(hBrowseLev,"OpenQuery").

DYNAMIC-FUNCTION("setAttribute",hBrowseUserGroup,"basequery","where false").
RUN InvokeMethod(hBrowseUserGroup,"OpenQuery").

DYNAMIC-FUNCTION("setCurrentObject",hToolbar).
DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","UserCompany").

RUN SUPER.  

DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","").
ASSIGN cJBoxUserId:READ-ONLY IN FRAME {&FRAME-NAME} = FALSE
       Lng:SCREEN-VALUE = "DES"
       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN DO:
  hBrwColSuperUser:FONT = iFontWingdings.
  hBrwColSuperUser:FORMAT = CHR(254) + "/"  + CHR(168).
END.
ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseCompany THEN DO:
  hBrwColCompAdmin:FONT = iFontWingdings.
  hBrwColCompAdmin:FORMAT = CHR(254) + "/"  + CHR(168).
END.
RUN SUPER.
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
IF DYNAMIC-FUNCTION("getToolbarState",hToolbar) = "new" THEN
  DO WITH FRAME {&FRAME-NAME}:
    DYNAMIC-FUNCTION("DoDelete","_User","Avail",
          "_UserId",
          cJBoxUserId:SCREEN-VALUE,
          FALSE).
  END.

RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSearch C-Win 
PROCEDURE StartSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
APPLY "window-resized" TO {&WINDOW-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UserCompanyRecord C-Win 
PROCEDURE UserCompanyRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cCompanyRowIdList AS CHAR NO-UNDO.
DEF VAR cCompanyIdList    AS CHAR NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
IF hBrowseCompany:QUERY:IS-OPEN THEN DO:
  hBrowseCompany:QUERY:GET-FIRST().
  REPEAT WHILE NOT hBrowseCompany:QUERY:QUERY-OFF-END:
    cCompanyRowIdList = cCompanyRowIdList + hBrowseCompany:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent2"):BUFFER-VALUE + ",".
    hBrowseCompany:QUERY:GET-NEXT().
  END.
  cCompanyRowIdList = TRIM(cCompanyRowIdList,",").
END.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "JBoxCompany;iJBoxCompanyId;cCompanyName",
                    "where true",
                    INPUT-OUTPUT cCompanyRowIdList,
                    "iJBoxCompanyId",
                    INPUT-OUTPUT cCompanyIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  IF NOT DYNAMIC-FUNCTION("runproc","jbserv_editcompanyuser.p",
                          hFieldMap:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE + "|" + cCompanyIdList,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
  ELSE APPLY "value-changed" TO hBrowse.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

