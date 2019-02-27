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
DEF VAR hToolbar2       AS HANDLE NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hBrowse2        AS HANDLE NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR hParentBrowse   AS HANDLE NO-UNDO.
DEF VAR hParent         AS HANDLE NO-UNDO.
DEF VAR hArtBasSok      AS HANDLE NO-UNDO.

DEF VAR hbcPromoType    AS HANDLE NO-UNDO.
DEF VAR hbfPromoType    AS HANDLE NO-UNDO.
DEF VAR hbcVareNr       AS HANDLE NO-UNDO.
DEF VAR hbfVareNr       AS HANDLE NO-UNDO.
DEF VAR hbcVareTekst    AS HANDLE NO-UNDO.
DEF VAR hbfVareTekst    AS HANDLE NO-UNDO.
DEF VAR hbcArtId        AS HANDLE NO-UNDO.
DEF VAR hbfArtId        AS HANDLE NO-UNDO.
DEF VAR hbcVarePris     AS HANDLE NO-UNDO.
DEF VAR hbfVarePris     AS HANDLE NO-UNDO.
DEF VAR hbcVarePrisH    AS HANDLE NO-UNDO.
DEF VAR hbfVarePrisH    AS HANDLE NO-UNDO.
DEF VAR hfiAntall       AS HANDLE NO-UNDO.
DEF VAR hcbRabattType   AS HANDLE NO-UNDO.
DEF VAR hbcRabattType   AS HANDLE NO-UNDO.
DEF VAR hbfRabattType   AS HANDLE NO-UNDO.
DEF VAR hfiBelop        AS HANDLE NO-UNDO.
DEF VAR hbcBelop        AS HANDLE NO-UNDO.
DEF VAR hbfBelop        AS HANDLE NO-UNDO.
DEF VAR hbcMinAnt       AS HANDLE NO-UNDO.
DEF VAR hbfMinAnt       AS HANDLE NO-UNDO.

DEF VAR hbfMaksAntall   AS HANDLE NO-UNDO.
DEF VAR hbcMaksAntall   AS HANDLE NO-UNDO.

DEF VAR bHideStr        AS LOG    NO-UNDO.
DEF VAR cTekst          AS CHAR   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolBar rectWinToolbar ~
rectToolBar-2 RECT-1 KampTilbId KampTilbTypeId HapHourId KampTilbNavn ~
KampTilbKvitteringstekst KampTilbPopUpTekst KampTilbPopUpTekstBruk ~
KampTilbBelop KampTIlbGrenseAntall KampTIlbGrenseAntallBruk ~
KampTilbPropBetalFor KampTilbOkning 
&Scoped-Define DISPLAYED-OBJECTS KampTilbId KampTilbTypeId HapHourId ~
KampTilbNavn KampTilbKvitteringstekst KampTilbPopUpTekst ~
KampTilbPopUpTekstBruk KampTilbBelop KampTIlbGrenseAntall ~
KampTIlbGrenseAntallBruk KampTilbPropBetalFor KampTilbOkning lblOrigPrice ~
lblCustomerPrice lblCustomerSave OrigPayL CustPayL CustSaveL OrigPayH ~
CustPayH CustSaveH 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddStr C-Win 
FUNCTION AddStr RETURNS LOGICAL
  ( INPUT ifArtikkelNr AS DEC,
    INPUT icStorl      AS CHAR,
    INPUT ifPlukkAnt   AS DEC,
    INPUT icAction     AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddStrFam C-Win 
FUNCTION AddStrFam RETURNS LOGICAL
    ( INPUT ifProdFamId AS DEC)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addStrKupong C-Win 
FUNCTION addStrKupong RETURNS LOGICAL
  ( INPUT ifKupongId AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fillRabattType C-Win 
FUNCTION fillRabattType RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setButton C-Win 
FUNCTION setButton RETURNS LOGICAL
  (INPUT ibFlag AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE HapHourId AS DECIMAL FORMAT ">>>>>>>>9":U INITIAL ? 
     LABEL "Happyhour" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE KampTilbTypeId AS DECIMAL FORMAT ">>>>>>>>9":U INITIAL ? 
     LABEL "Kampanje type" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE CustPayH AS DECIMAL FORMAT "-zzzzz9.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE CustPayL AS DECIMAL FORMAT "-zzzzz9.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE CustSaveH AS DECIMAL FORMAT "-zzzzz9.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE CustSaveL AS DECIMAL FORMAT "-zzzzz9.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE KampTilbBelop AS DECIMAL FORMAT ">>>>>>9.99":U INITIAL 0 
     LABEL "Beløp" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE KampTIlbGrenseAntall AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Maks antall" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE KampTilbId AS DECIMAL FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "TilbudsId" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE KampTilbKvitteringstekst AS CHARACTER FORMAT "X(14)":U 
     LABEL "Kvitterings tekst" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE KampTilbNavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 98 BY 1 NO-UNDO.

DEFINE VARIABLE KampTilbPopUpTekst AS CHARACTER FORMAT "X(14)":U 
     LABEL "Popup tekst" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE lblCustomerPrice AS CHARACTER FORMAT "X(256)":U INITIAL "Kunde betaler" 
      VIEW-AS TEXT 
     SIZE 15 BY .62 NO-UNDO.

DEFINE VARIABLE lblCustomerSave AS CHARACTER FORMAT "X(256)":U INITIAL "Kunde sparer" 
      VIEW-AS TEXT 
     SIZE 15 BY .62 NO-UNDO.

DEFINE VARIABLE lblOrigPrice AS CHARACTER FORMAT "X(256)":U INITIAL "Original pris" 
      VIEW-AS TEXT 
     SIZE 15 BY .62 NO-UNDO.

DEFINE VARIABLE OrigPayH AS DECIMAL FORMAT "-zzzzz9.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE OrigPayL AS DECIMAL FORMAT "-zzzzz9.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58 BY 2.86.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 182 BY 9.86.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectToolBar-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE VARIABLE KampTIlbGrenseAntallBruk AS LOGICAL INITIAL no 
     LABEL "Grense" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.4 BY .81 NO-UNDO.

DEFINE VARIABLE KampTilbOkning AS LOGICAL INITIAL no 
     LABEL "Stegvis økning" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE KampTilbPopUpTekstBruk AS LOGICAL INITIAL no 
     LABEL "Popup tekst" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE KampTilbPropBetalFor AS LOGICAL INITIAL no 
     LABEL "Proporsjonal fordeling" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     KampTilbId AT ROW 2.48 COL 17 COLON-ALIGNED
     KampTilbTypeId AT ROW 3.52 COL 17 COLON-ALIGNED
     HapHourId AT ROW 4.52 COL 17 COLON-ALIGNED
     KampTilbNavn AT ROW 5.52 COL 17 COLON-ALIGNED
     KampTilbKvitteringstekst AT ROW 6.48 COL 2.8
     KampTilbPopUpTekst AT ROW 7.43 COL 17 COLON-ALIGNED
     KampTilbPopUpTekstBruk AT ROW 7.62 COL 40.4 NO-TAB-STOP 
     KampTilbBelop AT ROW 6.57 COL 94 COLON-ALIGNED
     KampTIlbGrenseAntall AT ROW 7.67 COL 94 COLON-ALIGNED
     KampTIlbGrenseAntallBruk AT ROW 7.76 COL 117.6 NO-TAB-STOP 
     KampTilbPropBetalFor AT ROW 5.57 COL 151
     KampTilbOkning AT ROW 6.62 COL 151
     lblOrigPrice AT ROW 2.67 COL 119 COLON-ALIGNED NO-LABEL
     lblCustomerPrice AT ROW 3.38 COL 119 COLON-ALIGNED NO-LABEL
     lblCustomerSave AT ROW 4.1 COL 119 COLON-ALIGNED NO-LABEL
     OrigPayL AT ROW 2.67 COL 136 COLON-ALIGNED NO-LABEL
     CustPayL AT ROW 3.38 COL 136 COLON-ALIGNED NO-LABEL
     CustSaveL AT ROW 4.1 COL 138 NO-LABEL
     OrigPayH AT ROW 2.67 COL 159.8 RIGHT-ALIGNED NO-LABEL
     CustPayH AT ROW 3.38 COL 159.8 RIGHT-ALIGNED NO-LABEL
     CustSaveH AT ROW 4.1 COL 159.8 RIGHT-ALIGNED NO-LABEL
     rectBrowse AT ROW 10.29 COL 1
     rectToolBar AT ROW 1.14 COL 1.8
     rectWinToolbar AT ROW 1.14 COL 172.8
     rectToolBar-2 AT ROW 9.33 COL 2
     RECT-1 AT ROW 2.43 COL 120
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 182.2 BY 19.33.


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
         TITLE              = "Kampanje tilbud"
         HEIGHT             = 19.33
         WIDTH              = 182.2
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
   FRAME-NAME L-To-R,COLUMNS                                            */
/* SETTINGS FOR FILL-IN CustPayH IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN CustPayL IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CustSaveH IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN CustSaveL IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN KampTilbKvitteringstekst IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN lblCustomerPrice IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lblCustomerSave IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lblOrigPrice IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN OrigPayH IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN OrigPayL IN FRAME DEFAULT-FRAME
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
ON END-ERROR OF C-Win /* Kampanje tilbud */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kampanje tilbud */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Kampanje tilbud */
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  IF VALID-HANDLE(hArtBasSok) THEN DELETE OBJECT hArtBasSok.
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

  IF SOURCE-PROCEDURE:FILE-NAME MATCHES "*Kampanje*" THEN 
    hParent = SOURCE-PROCEDURE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BeforeNavBrowseFillIn C-Win 
PROCEDURE BeforeNavBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iphFillIn AS HANDLE NO-UNDO.
DEF INPUT PARAM iphBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM opbOk    AS LOG    NO-UNDO.

IF iphFillIn:MODIFIED THEN
  RUN getInfo.

opbOk = TRUE.
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
DEF VAR iTmp AS INT NO-UNDO.
  iTmp = INT(DYNAMIC-FUNCTION('getFieldValues','KampTilbTemplate','WHERE KampTilbTemplate.kampid = DEC(' + STRING(hFieldMap:BUFFER-FIELD('kampid'):BUFFER-VALUE) + ')'
                                                                   + ' AND KampTilbTemplate.kamptilbid = INT(' + STRING(hFieldMap:BUFFER-FIELD('KampTilbId'):BUFFER-VALUE) + ')'
                                                                   ,'KampTilbTempNr')).
  IF iTmp GT 0 THEN
  DO:
    MESSAGE 'Template nr.: ' STRING(iTmp) ' finnes knyttet til dette tilbudet.' SKIP 'Om posten blir slettet, vil også template knyttet til posten bli slettet.'
      VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE bOk.

    IF bOk THEN RUN SUPER.
  END.
  ELSE RUN SUPER.
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
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      KampTilbPopUpTekst:SENSITIVE       = KampTilbPopUpTekstBruk:CHECKED
      /*KampTilbBelop:SENSITIVE            = KampTilbPropBetalFor:CHECKED */
      KampTilbGrenseAntall:SENSITIVE     = KampTilbGrenseAntallBruk:CHECKED
      KampTilbTypeId:SENSITIVE           = NOT hFieldMap:AVAIL
    .
  END.
  IF VALID-HANDLE(hFieldMap) THEN 
    setButton(hFieldMap:AVAIL).

  IF DYNAMIC-FUNCTION('getCurrentObject') = hQuery THEN 
  DO:
    RUN ValueChangedField ("KampTilbTypeId").
    RUN getInfo.
  END.

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
  DISPLAY KampTilbId KampTilbTypeId HapHourId KampTilbNavn 
          KampTilbKvitteringstekst KampTilbPopUpTekst KampTilbPopUpTekstBruk 
          KampTilbBelop KampTIlbGrenseAntall KampTIlbGrenseAntallBruk 
          KampTilbPropBetalFor KampTilbOkning lblOrigPrice lblCustomerPrice 
          lblCustomerSave OrigPayL CustPayL CustSaveL OrigPayH CustPayH 
          CustSaveH 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectBrowse rectToolBar rectWinToolbar rectToolBar-2 RECT-1 KampTilbId 
         KampTilbTypeId HapHourId KampTilbNavn KampTilbKvitteringstekst 
         KampTilbPopUpTekst KampTilbPopUpTekstBruk KampTilbBelop 
         KampTIlbGrenseAntall KampTIlbGrenseAntallBruk KampTilbPropBetalFor 
         KampTilbOkning 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlatViewRecord C-Win 
PROCEDURE FlatViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hFlatView AS HANDLE NO-UNDO.
DEF VAR hFlatBrw  AS HANDLE NO-UNDO.

DYNAMIC-FUNCTION("CreateParentLink",hBrowse,DYNAMIC-FUNCTION("getParentBrowse" IN hParent),"KampId,KampTilbId").

RUN SUPER.

hFlatView = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolBar,"flatviewhandle")) NO-ERROR.
IF NOT VALID-HANDLE(hFlatView) THEN RETURN.

hFlatBrw  = DYNAMIC-FUNCTION("getBrowseHandle" IN hFlatView).

/* DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"availdistinctcolumns",                                                                                                                                                                                                */
/*                  "Navn,KOrdre_Id,KundeNr,LevStatus,ProdStatus,ProduksjonsDato,AnsvVerksted,LeveringsDato,FakturertDato,Utsendelsesdato|Levert dato,RegistrertDato,RegistrertAv,Adresse1,DeresRef,VaarRef,Referanse,VerkstedMerknad,ButikkNr,VareNr,VareTekst"). */
/* DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"availaccumfields",                                                                                                                                                                                                    */
/*                  "NettoLinjesum,SumEksMvaKr,DbKr").                                                                                                                                                                                                             */

DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,DYNAMIC-FUNCTION("getParentBrowse" IN hParent)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getInfo C-Win 
PROCEDURE getInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cReturn AS CHAR NO-UNDO.
IF hFieldMap:AVAIL THEN
DO:
  IF NOT DYNAMIC-FUNCTION("runproc","getkampanjeinfo.p",
                          hFieldMap:BUFFER-FIELD("KampId"):BUFFER-VALUE
                           + '|' + hFieldMap:BUFFER-FIELD("KampTilbId"):BUFFER-VALUE,?) 
    THEN
    DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
  ELSE 
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      cReturn = DYNAMIC-FUNCTION("getTransactionMessage")
      OrigPayL  = DEC(ENTRY(1,cReturn,'|'))
      OrigPayH  = DEC(ENTRY(2,cReturn,'|'))
      CustPayL  = DEC(ENTRY(3,cReturn,'|'))
      CustPayH  = DEC(ENTRY(4,cReturn,'|'))
      CustSaveL = DEC(ENTRY(5,cReturn,'|'))
      CustSaveH = DEC(ENTRY(6,cReturn,'|'))
    .
    DISP OrigPayL OrigPayH CustPayL CustPayH CustSaveL CustSaveH WITH FRAME {&FRAME-NAME}.
/*     ASSIGN                                       */
/*       OrigPayH:HIDDEN  = (CustSaveL = CustSaveH) */
/*       CustPayH:HIDDEN  = (CustSaveL = CustSaveH) */
/*       CustSaveH:HIDDEN = (CustSaveL = CustSaveH) */
/*     .                                            */
  END.
END. /*IF AVAIL*/
ELSE
DO:
  ASSIGN 
    OrigPayL  = 0
    OrigPayH  = 0
    CustPayL  = 0
    CustPayH  = 0
    CustSaveL = 0
    CustSaveH = 0
  .
  DISP OrigPayL OrigPayH CustPayL CustPayH CustSaveL CustSaveH WITH FRAME {&FRAME-NAME}.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeChild C-Win 
PROCEDURE InitializeChild :
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
                      "KampanjeTilbArtikkel"
                      + ";!KampTilbArtId"
                      + ";+VareNr|decimal|>>>>>>>>>>>>9|VareNr|VareId"
                      + ";+VareTekst|character|X(200)|VareTekst|Vare"
                      + ";+VarePris|decimal|>>><>>9.99|VarePris|Enhetspris/Lavest"
                      + ";+VarePrisH|decimal|>>><>>9.99|VarePrisH|Høyest pris"
                      + ";KampTilbArtMinAntall"
                      + ";KampTilbArtBelop"
                      + ";+MaksAntall|character|xxx|MaksAntall|Maks"
                       + ";!+PromoType|character|x|PromoType|Type" 
                       + ";!KampId"
                       + ";!KampTilbId"
                       + ";!ProdFamId"
                       + ";!KampRabattTypeId"
                       + ";!RegistrertDato;!RegistrertTid;!RegistrertAv;!ETid;!EDato;!BrukerID"
                       + ";!KupongId"
                       + ",KampRabattType"
                       + ";KampRabattTypeNavn@7"

                      ,"WHERE false"
                        + ", FIRST KampRabattType OUTER-JOIN OF KampanjeTilbArtikkel"
                      ,"sort|KampTilbId").             /* Initial sort column */
    
    DYNAMIC-FUNCTION('setAttribute',hBrowse,'calcfieldproc','kampanjetilbartikkel_brwcalc.p').
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"customDeleteValProc","=delval_kampanjetilbartikkel.p").


    hToolbar2 = DYNAMIC-FUNCTION("NewToolBar",
                      rectToolBar-2:HANDLE,             /* Rectangle to define coordinates for toolbar */
                      "File",                         /* Corresponding menu label - no menu if blank */
                      "newArt;Le&gg til Artikkel¤ENABLE,newFam;Legg til &Familie¤ENABLE,newKupong;Legg til &Kupong¤ENABLE,delete"
                      + ",rule,Refresh" /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                         Any number of properties accepted (one ok - if predef. action) */
                      + ",rule,Filter,BrowseConfig"
                      ,"maxborder").                  /* Misc - enable, maxborder.. */
    
    DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar2).    
    DYNAMIC-FUNCTION("CreateObjectLink",hToolbar2,hToolbar).    

    hcbRabattType = DYNAMIC-FUNCTION("NewBrowseDropDown",
                      hBrowse,                            /* Handle to browse */
                      "KampRabattTypeNavn",                          /* Browse column (display) */
                      "KampRabattTypeId",                          /* Buffer column (to update - foreign key. Maps to value - under) */
                      "KampRabattType;KampRabattTypeNavn;KampRabattTypeId",         /* DB buffers and fields for drop-down values: Label,value */
                      "where false",                       /* Query to get drop-down values */
                      "|0").                                 /* I've got the values. Don't go to the database */
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hcbRabattType,"KampRabattTypeNavn"). /* Link the dropdown to the browse with column-name info */
    DYNAMIC-FUNCTION("setAttribute",hcbRabattType,"refreshrow","yes").          /* Refresh the row after update */
    fillRabattType().
    
    hfiAntall = DYNAMIC-FUNCTION("NewBrowseFillIn",
                      hBrowse,          
                      "KampTilbArtMinAntall",     
                      "KampTilbArtMinAntall",     
                      "","","","").                
    DYNAMIC-FUNCTION("setAttribute",hfiAntall,"refreshrow","yes").          /* Refresh the row after update */    
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hfiAntall,"KampTilbArtMinAntall").
    
    hfiBelop = DYNAMIC-FUNCTION("NewBrowseFillIn",
                      hBrowse,          
                      "KampTilbArtBelop",     
                      "KampTilbArtBelop",     
                      "","","","").                
    DYNAMIC-FUNCTION("setAttribute",hfiBelop,"refreshrow","yes").          /* Refresh the row after update */
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hfiBelop,"KampTilbArtBelop").
    
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeHead C-Win 
PROCEDURE InitializeHead :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cTekst    AS CHAR NO-UNDO.
DEFINE VARIABLE cTilbTLst AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF VALID-HANDLE(hParent) THEN
    ASSIGN 
      hParentBrowse = DYNAMIC-FUNCTION("getParentBrowse" IN hParent)
    .
  
  ASSIGN 
    KampTilbTypeId:DELIMITER = "|"
    cTilbTLst = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                             "WHERE SysHId = 17 and SysGr = 1 and ParaNr = 4","Parameter1")
    cTekst    = DYNAMIC-FUNCTION("getFieldList","KampanjeTilbType;KampTilbTypeId|KampTilbTypeNavn;KampTilbTypeId","where not can-do('" + cTilbTLst + "',string(KampanjeTilbType.KampTilbTypeId))")
  . 
  IF cTekst = '' THEN KampTilbTypeId:LIST-ITEM-PAIRS = "|0".
  ELSE KampTilbTypeId:LIST-ITEM-PAIRS = "|0|" + cTekst. 
  
  ASSIGN 
    HapHourId:DELIMITER = "|"
    cTekst              = DYNAMIC-FUNCTION("getFieldList","HappyHourHode;HapHourId|HapHourNavn;HapHourId","where true")
  . 
  IF cTekst = '' THEN HapHourId:LIST-ITEM-PAIRS = "|0".
  ELSE HapHourId:LIST-ITEM-PAIRS = "|0|" + cTekst.
  
  hQuery  = DYNAMIC-FUNCTION("NewQuery",1,"",
                               "KampanjeTilbud"
                             + ";KampId"
                             + ";KampTilbId"
                             + ";KampTilbNavn"
                             + ";KampTilbPopUpTekst"
                             + ";KampTilbKvitteringstekst"
                             + ";KampTilbGrenseAntall"
                             + ";KampTilbBelop"
                             + ";HapHourId"
                             + ";KampTilbTypeId"
                             + ";KampTilbOkning"
                             + ";KamptilbGrenseAntallBruk"
                             + ";KamptilbPopUpTekstBruk"
                             + ";KampTilbPropBetalFor",
                               "WHERE FALSE",
                               "").
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                hQuery,
                FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                "KampTilbTypeId,HapHourId,KampTilbGrenseAntallBruk,KamptilbOkning,KampTilbPropBetalFor,KampTilbPopUpTekstBruk" +
                ",KampTilbBelop,KampTilbGrenseAntall,KampTilbNavn,KampTilbKvitteringstekst,KampTilbPopUpTekst",   /* Update columns in buffer */
                  "",                           /* Corresponding input fields (fill-in..). blank if equal to update columns */
                "KampTilbId",        /* Additional buffer and displ.fields - not updateable*/
                  "",                           /* Corresponding fill-ins */
                "").                            /* other input widgets and lookup buttons for update fill-ins */
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextrafields","KampId").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customCreateproc","kampanjetilbud_create.p").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValproc","+kampanjetilbud_update.p").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customDeleteValProc","=delval_kampanjetilbud.p").
  
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hQuery).
  DYNAMIC-FUNCTION("CreateOneToOneLink",hQuery,hParentBrowse,"KampId,KampTilbId"). 

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "new,undo,delete,save"
                    + ",excel;Eksporter til E&xcel,Filter,flatview" /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */

                    ,"maxborder").                  /* Misc - enable, maxborder.. */
  
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hQuery).

  DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,          /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "close",
                    "right,enable").                      /* Misc - enable, maxborder.. */
END.

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
cTekst = DYNAMIC-FUNCTION("getFieldValues","SysPara", "WHERE SysHId = 17 and SysGr = 1 and ParaNr = 5","Parameter1").
IF CAN-DO('1,J,Ja,Y,YES,TRUE',cTekst) THEN
    bhideStr = TRUE.
  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,500,450,0,350).

RUN InitializeHead.

RUN InitializeChild.

DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("CreateParentLink",hBrowse,hQuery,'kampid,kamptilbid').    

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar,rect-1").
/*   DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,500,450,0,350). */
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
DYNAMIC-FUNCTION("DoLockWindow",?).
APPLY "entry" TO KampTilbTypeId IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewArtRecord C-Win 
PROCEDURE NewArtRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(hArtBasSok) THEN 
    APPLY "close" TO hArtBasSok.

/*   RUN ProdFamArtBasSok.w PERSIST SET hArtBasSok. */
  RUN ArtBasSok.w PERSIST SET hArtBasSok.
  IF VALID-HANDLE(hArtBasSok) THEN DO:       
      IF bhideStr THEN DYNAMIC-FUNCTION('setHideStr' IN hArtBasSok).
      DYNAMIC-FUNCTION('setKampanjeInfo' IN hArtBasSok, 
                       hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD('KampId'):BUFFER-VALUE, 
                       hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD('KampTilbId'):BUFFER-VALUE).
  END.
  RUN InitializeObject IN hArtBasSok.
  RUN MoveToTop IN hArtBasSok.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewFamRecord C-Win 
PROCEDURE NewFamRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cInOut AS CHAR NO-UNDO INIT "ProdFamId".

  DO WITH FRAME {&FRAME-NAME}:
    RUN JBoxDLookup.w ("ProduktFamilie"
                       + ";ProdFamId"
                       + ";ProdFamNavn"
                       ,"WHERE ProdFamAktiv = TRUE" 
                       ,INPUT-OUTPUT cInOut).
    IF cInOut NE "" THEN 
    DO:
      AddStrFam(DEC(cInOut)).
/*       DYNAMIC-FUNCTION('applyEvent',hQuery,'value-changed'). */
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewKupongRecord C-Win 
PROCEDURE NewKupongRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cInOut AS CHAR NO-UNDO INIT "KupongId".

  DO WITH FRAME {&FRAME-NAME}:
    RUN JBoxDLookup.w ("Kupong"
                       + ";KupongId"
                       + ";KupBeskrivelse"
                       ,"WHERE TRUE" 
                       ,INPUT-OUTPUT cInOut).
    IF cInOut NE "" THEN 
    DO:
      AddStrKupong(DEC(cInOut)).
/*       DYNAMIC-FUNCTION('applyEvent',hQuery,'value-changed'). */
    END.
  END.
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
IF DYNAMIC-FUNCTION('getToolbarState',hToolbar) = 'new' THEN
  setButton(FALSE).
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
  DEF VAR iGreen  AS INT INIT 10 NO-UNDO.
  DEF VAR iRed    AS INT INIT 12 NO-UNDO.
  DEF VAR iYellow AS INT INIT 14 NO-UNDO.

  IF hbfPromoType:BUFFER-VALUE = 'F' THEN
    ASSIGN 
      hbcVareTekst:BGCOLOR = iGreen
      /*hbcArtId:BGCOLOR     = iGreen*/      
      hbcVarePris:BGCOLOR  = iGreen
      hbcVarePrisH:BGCOLOR = iGreen
    .
  IF hbfPromoType:BUFFER-VALUE = 'K' THEN
    ASSIGN 
      hbcVareTekst:BGCOLOR = iYellow
      hbcVarePris:BGCOLOR  = iYellow
      hbcVarePrisH:BGCOLOR = iYellow
    .
  IF hbfRabattType:BUFFER-VALUE = '3' THEN /*3 - Undertrykk rabatt*/
    ASSIGN 
      hbcRabattType:BGCOLOR  = iRed
      hbcBelop:BGCOLOR       = iRed
    .
  IF hbfRabattType:BUFFER-VALUE = '2' THEN /*2 - Gratis*/
    ASSIGN 
      hbcRabattType:BGCOLOR  = 11
      hbcBelop:BGCOLOR       = 11
    .
  

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
  DEF VAR hParentsParent AS HANDLE NO-UNDO.

  DO WITH FRAME Default-Frame:
      IF INT(KampTilbTypeId:SCREEN-VALUE) = 9 THEN
      DO:
          MESSAGE 'Denne kampanjetilbudstypen kan ikke benyttes ved registrering av kombinasjonskampanjer.' SKIP 
                  'Den benyttes bare på kombinasjonskampanjer som automatisk legges opp ved registrering av ekstrapris kampanjer'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
  END.

  hParentsParent = DYNAMIC-FUNCTION('getLinkedObject',hParentBrowse,'Parent','From').
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraValues",STRING(hParentsParent:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('kampid'):BUFFER-VALUE)).
  RUN SUPER.

/*   bok =  DYNAMIC-FUNCTION('runProc','kampanjetilbud_update.p',STRING(hFieldMap:BUFFER-FIELD('KampId'):BUFFER-VALUE)     */
/*                                                       + ';' + STRING(hFieldMap:BUFFER-FIELD('KampTilbId'):BUFFER-VALUE) */
/*                                                       + ';' + 'KampTilbBelop'                                           */
/*                                                       + ';' + KampTilbBelop:SCREEN-VALUE                                */
/*                         ,?).                                                                                            */

  /*hcbRabattType:LIST-ITEM-PAIRS = '1|Test'.*/

  DO WITH FRAME Default-Frame:
      RUN ValueChangedField ("KampTilbTypeId").
      KampTilbTypeId:SENSITIVE = FALSE.
  END.
  RUN MoveToTop.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValChngBrowseDropDown C-Win 
PROCEDURE ValChngBrowseDropDown :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN SUPER.
  IF DYNAMIC-FUNCTION('getCurrentObject') = hcbRabattType THEN RUN getInfo.
/*   IF DYNAMIC-FUNCTION('getCurrentObject') = hfiBelop THEN                                                   */
/*   DO:                                                                                                       */
/*     IF DEC(KampTilbBelop:SCREEN-VALUE IN FRAME {&FRAME-NAME}) GT 0 AND DEC(hbcBelop:SCREEN-VALUE) GT 0 THEN */
/*     DO:                                                                                                     */
/*       MESSAGE 'Kan ikke legge inn belop i feltet da tilbudsbeløp er satt'.                                  */
/*     END.                                                                                                    */
/*   END.                                                                                                      */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChangedField C-Win 
PROCEDURE ValueChangedField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icField AS CHAR NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:
  CASE icField:
    WHEN "KampTilbTypeId" THEN
    DO:
      /* Default */  
      ASSIGN
          HapHourId:SENSITIVE                = TRUE 
          KampTilbBelop:SENSITIVE            = TRUE
          KampTilbPropBetalFor:SENSITIVE     = TRUE 
          KampTIlbGrenseAntall:SENSITIVE     = FALSE  
          KampTIlbGrenseAntallBruk:SENSITIVE = TRUE 
          KampTilbOkning:SENSITIVE           = TRUE 
          KampTilbPopUpTekstBruk:SENSITIVE   = TRUE 
          KampTilbPopUpTekst:SENSITIVE       = FALSE
          hfiAntall:SENSITIVE                = TRUE
          hfiBelop:SENSITIVE                 = TRUE
          hcbRabattType:SENSITIVE            = TRUE
          .

      IF KampTilbTypeId:SCREEN-VALUE = '1' THEN
      DO:
        ASSIGN 
/*           KampTilbGrenseAntallBruk:SENSITIVE = FALSE */
/*           KampTilbGrenseAntallBruk:CHECKED   = FALSE */
/*           KampTilbPropBetalFor:CHECKED       = TRUE */
          KampTilbPropBetalFor:SENSITIVE     = TRUE
          KampTilbBelop:SENSITIVE            = TRUE 
/*           KampTilbBelop:SENSITIVE            = KampTilbPropBetalFor:CHECKED */
        .
      END.
      ELSE IF KampTilbTypeId:SCREEN-VALUE = '2' THEN
      DO:
        ASSIGN
/*           KampTilbGrenseAntallBruk:SENSITIVE = KampTilbPropBetalFor:CHECKED */
/*           KampTilbGrenseAntallBruk:CHECKED   = KampTilbPropBetalFor:CHECKED */
          KampTilbPropBetalFor:CHECKED       = FALSE
          KampTilbPropBetalFor:SENSITIVE     = FALSE
          KampTilbBelop:SENSITIVE            = TRUE 
        .
      END.
      ELSE IF KampTilbTypeId:SCREEN-VALUE = '3' THEN
      DO:
        ASSIGN
/*           KampTilbGrenseAntallBruk:SENSITIVE = TRUE */
/*           KampTilbGrenseAntallBruk:CHECKED   = TRUE */
          KampTilbPropBetalFor:CHECKED       = FALSE
          KampTilbPropBetalFor:SENSITIVE     = FALSE
          KampTilbBelop:SENSITIVE            = TRUE 
        .
      END.
      ELSE IF KampTilbTypeId:SCREEN-VALUE = '4' THEN
      DO:
        ASSIGN 
/*           KampTilbGrenseAntallBruk:SENSITIVE = FALSE */
/*           KampTilbGrenseAntallBruk:CHECKED   = FALSE */
          KampTilbPropBetalFor:CHECKED       = TRUE
          KampTilbPropBetalFor:SENSITIVE     = TRUE
          KampTilbBelop:SENSITIVE            = TRUE
        .
      END.
      ELSE IF KampTilbTypeId:SCREEN-VALUE = '10' THEN
      DO:
          ASSIGN
              HapHourId:SENSITIVE                = FALSE   
              KampTilbBelop:SENSITIVE            = TRUE   
              KampTilbPropBetalFor:SENSITIVE     = FALSE   
              KampTIlbGrenseAntall:SENSITIVE     = TRUE   
              KampTIlbGrenseAntallBruk:SENSITIVE = FALSE  
              KampTilbOkning:SENSITIVE           = FALSE   
              KampTilbPopUpTekstBruk:SENSITIVE   = FALSE   
              KampTilbPopUpTekst:SENSITIVE       = FALSE 
              hfiAntall:SENSITIVE                = FALSE
              hfiBelop:SENSITIVE                 = FALSE
              hcbRabattType:SENSITIVE            = FALSE
              KampTIlbGrenseAntallBruk:CHECKED   = TRUE 
              KampTIlbGrenseAntallBruk:MODIFIED  = TRUE
              .
      END.
      ELSE IF KampTilbTypeId:SCREEN-VALUE = '11' THEN
      DO:
          ASSIGN
              HapHourId:SENSITIVE                = FALSE   
              KampTilbBelop:SENSITIVE            = TRUE   
              KampTilbPropBetalFor:SENSITIVE     = FALSE   
              KampTIlbGrenseAntall:SENSITIVE     = FALSE   
              KampTIlbGrenseAntallBruk:SENSITIVE = FALSE  
              KampTilbOkning:SENSITIVE           = FALSE   
              KampTilbPopUpTekstBruk:SENSITIVE   = FALSE   
              KampTilbPopUpTekst:SENSITIVE       = FALSE 
              hfiAntall:SENSITIVE                = TRUE
              hfiBelop:SENSITIVE                 = FALSE
              hcbRabattType:SENSITIVE            = FALSE
              KampTIlbGrenseAntallBruk:CHECKED   = FALSE  
              KampTIlbGrenseAntallBruk:MODIFIED  = TRUE
              .
      END.
      fillRabattType().
      IF VALID-HANDLE(hcbRabattType) AND hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN hcbRabattType:SCREEN-VALUE = hbfRabattType:BUFFER-VALUE.
    END.
    WHEN "KampTilbGrenseAntallBruk" THEN 
    DO:
      KampTilbGrenseAntall:SENSITIVE = KampTilbGrenseAntallBruk:CHECKED.
      IF NOT KampTilbGrenseAntallBruk:CHECKED THEN KampTilbGrenseAntall:SCREEN-VALUE = '0'.
    END.
    WHEN "KampTilbOkning" THEN 
      KampTilbGrenseAntall:SENSITIVE = KampTilbGrenseAntallBruk:CHECKED.
    WHEN "KampTilbPropBetalFor" THEN 
    DO:
      KampTilbBelop:SENSITIVE = KampTilbPropBetalFor:CHECKED.
      IF NOT KampTilbPropBetalFor:CHECKED THEN 
      DO:
        KampTilbBelop:SCREEN-VALUE = '0'.
        APPLY 'ENTRY' TO KampTilbBelop.
        RETURN NO-APPLY.
      END.
    END.
    WHEN "KampTilbPopUpTekstBruk" THEN 
      KampTilbPopUpTekst:SENSITIVE = KampTilbPopUpTekstBruk:CHECKED.
    
  END CASE.


END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddStr C-Win 
FUNCTION AddStr RETURNS LOGICAL
  ( INPUT ifArtikkelNr AS DEC,
    INPUT icStorl      AS CHAR,
    INPUT ifPlukkAnt   AS DEC,
    INPUT icAction     AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Legg til eller endre artikkel. Kalles fra artbassok.w 
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR bReturnFocus  AS LOG   NO-UNDO.
  DEF VAR cStrList      AS CHAR  NO-UNDO.
  DEF VAR bOkStr        AS LOG   NO-UNDO.
  DEF VAR iArtnr        AS DEC   NO-UNDO.
  DEF VAR fArtikkelPris AS DEC   NO-UNDO.
  DEF VAR bCurrentFlag  AS LOG   NO-UNDO.

  iArtNr = DYNAMIC-FUNCTION('getFieldValues','KampanjeTilbArtikkel','WHERE KampanjeTilbArtikkel.kampid = DEC(' + STRING(hFieldMap:BUFFER-FIELD('kampid'):BUFFER-VALUE) + ')'
                                                                   + ' AND KampanjeTilbArtikkel.kamptilbid = INT(' + STRING(hFieldMap:BUFFER-FIELD('KampTilbId'):BUFFER-VALUE) + ')'
                                                                   + ' AND KampanjeTilbArtikkel.kamptilbartid = DEC(' + STRING(ifArtikkelNr) + ')' ,'KampTilbArtId').
  
  IF iArtNr NE 0 AND iArtNr NE ? THEN
  DO:
    MESSAGE 'allerede registrert !'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    LEAVE.
  END.
  DO WITH FRAME {&FRAME-NAME}:
    bok =  DYNAMIC-FUNCTION('runProc','kampanjetilbartikkel_create.p',STRING(hFieldMap:BUFFER-FIELD('KampId'):BUFFER-VALUE) 
                                                                + ';' + STRING(hFieldMap:BUFFER-FIELD('KampTilbId'):BUFFER-VALUE) 
                                                                + ';' + STRING(ifArtikkelNr)
                                                                + ';' + '0'
                                                                + ';' + '0'
                           ,?).
    DYNAMIC-FUNCTION('applyEvent',hQuery,'value-changed').
    RUN DisplayRecord.

    RETURN bOk.    
  END. /*FRAME*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddStrFam C-Win 
FUNCTION AddStrFam RETURNS LOGICAL
    ( INPUT ifProdFamId AS DEC) :
  /*------------------------------------------------------------------------------
    Purpose: Legg til eller endre artikkel. Kalles fra artbassok.w 
      Notes:  
  ------------------------------------------------------------------------------*/
    DEF VAR bReturnFocus  AS LOG   NO-UNDO.
    DEF VAR cStrList      AS CHAR  NO-UNDO.
    DEF VAR bOkStr        AS LOG   NO-UNDO.
    DEF VAR iProdFamId    AS INT   NO-UNDO.
    DEF VAR fArtikkelPris AS DEC   NO-UNDO.
    DEF VAR bCurrentFlag  AS LOG   NO-UNDO.

    iProdFamId = DYNAMIC-FUNCTION('getFieldValues','KampanjeTilbArtikkel','WHERE KampanjeTilbArtikkel.kampid = DEC(' + STRING(hFieldMap:BUFFER-FIELD('kampid'):BUFFER-VALUE) + ')'
                                                                     + ' AND KampanjeTilbArtikkel.kamptilbid = INT(' + STRING(hFieldMap:BUFFER-FIELD('KampTilbId'):BUFFER-VALUE) + ')'
                                                                     + ' AND KampanjeTilbArtikkel.ProdFamId  = DEC(' + STRING(ifProdFamId) + ')' ,'ProdFamId').

    IF iProdFamId NE 0 AND iProdFamId NE ? THEN
    DO:
      MESSAGE 'Allerede registrert !'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      LEAVE.
    END.
    DO WITH FRAME {&FRAME-NAME}:
      bok =  DYNAMIC-FUNCTION('runProc','kampanjetilbartikkel_create.p',STRING(hFieldMap:BUFFER-FIELD('KampId'):BUFFER-VALUE) 
                                                                  + ';' + STRING(hFieldMap:BUFFER-FIELD('KampTilbId'):BUFFER-VALUE) 
                                                                  + ';' + '0'
                                                                  + ';' + STRING(ifProdFamId)
                                                                  + ';0'
                             ,?).
      IF NOT bOk THEN MESSAGE DYNAMIC-FUNCTION("getTransactionMessage")
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      DYNAMIC-FUNCTION("SetCurrentObject",hQuery).
      RUN DisplayRecord.

      RETURN bOk.    
    END. /*FRAME*/
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addStrKupong C-Win 
FUNCTION addStrKupong RETURNS LOGICAL
  ( INPUT ifKupongId AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEF VAR bReturnFocus  AS LOG   NO-UNDO.
    DEF VAR iKeyId        AS DEC   NO-UNDO.

    iKeyId = DYNAMIC-FUNCTION('getFieldValues','KampanjeTilbArtikkel','WHERE KampanjeTilbArtikkel.kampid     = DEC(' + STRING(hFieldMap:BUFFER-FIELD('KampId'):BUFFER-VALUE) + ')'
                                                                     + ' AND KampanjeTilbArtikkel.kamptilbid = INT(' + STRING(hFieldMap:BUFFER-FIELD('KampTilbId'):BUFFER-VALUE) + ')'
                                                                     + ' AND KampanjeTilbArtikkel.KupongId   = DEC(' + STRING(ifKupongId) + ')' ,'KupongId').

    IF iKeyId NE 0 AND iKeyId NE ? THEN
    DO:
      MESSAGE 'Allerede registrert !'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      LEAVE.
    END.
    DO WITH FRAME {&FRAME-NAME}:
      bok =  DYNAMIC-FUNCTION('runProc','kampanjetilbartikkel_create.p',STRING(hFieldMap:BUFFER-FIELD('KampId'):BUFFER-VALUE) 
                                                                  + ';' + STRING(hFieldMap:BUFFER-FIELD('KampTilbId'):BUFFER-VALUE) 
                                                                  + ';' + '0'
                                                                  + ';' + '0'
                                                                  + ';' + STRING(ifKupongId)
                             ,?).
      DYNAMIC-FUNCTION("SetCurrentObject",hQuery).
      RUN DisplayRecord.
      RETURN bOk.
    END. /*FRAME*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  ASSIGN 
    ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 200
    ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 90    
    ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS = 70    
    ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS = 70    
    ihBrowse:GET-BROWSE-COLUMN(7):WIDTH-PIXELS = 200    
    ihBrowse:GET-BROWSE-COLUMN(8):WIDTH-PIXELS = 70    
    
/*     hbcArtId      = ihBrowse:GET-BROWSE-COLUMN(1) */
    hbfArtId      = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('KampTilbArtId')
    hbcVareNr     = ihBrowse:GET-BROWSE-COLUMN(1)
    hbfVareNr     = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('VareNr')
/*     hbcPromoType  = ihBrowse:GET-BROWSE-COLUMN(1) */
    hbfPromoType  = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('PromoType')
    hbcVareTekst  = ihBrowse:GET-BROWSE-COLUMN(2)
    hbfVareTekst  = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('VareTekst')
    hbcVarePris   = ihBrowse:GET-BROWSE-COLUMN(3)
    hbfVarePris   = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('VarePris')
    hbcVarePrisH  = ihBrowse:GET-BROWSE-COLUMN(4)
    hbfVarePrisH  = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('VarePrisH')
    hbcMinAnt     = ihBrowse:GET-BROWSE-COLUMN(5)
    hbfMinAnt     = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('KampTilbArtMinAntall')
    hbcBelop      = ihBrowse:GET-BROWSE-COLUMN(6)
    hbfBelop      = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('KampTilbArtBelop')
    hbcRabattType = ihBrowse:GET-BROWSE-COLUMN(7)
    hbfRabattType = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('KampRabattTypeId')
  .

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fillRabattType C-Win 
FUNCTION fillRabattType RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cWhere AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF KampTilbTypeId:SCREEN-VALUE = '1' THEN
    DO:
      hbcBelop:LABEL = 'Beløp'.
      IF KampTilbPropBetalFor:CHECKED THEN 
        cWhere = "WHERE CAN-DO('1,3',STRING(KampRabattTypeId))".
      ELSE
        cWhere = "WHERE CAN-DO('2,3,7,8',STRING(KampRabattTypeId))".
    END.
    ELSE IF KampTilbTypeId:SCREEN-VALUE = '2' THEN
    DO:
      hbcBelop:LABEL = 'Spar'.
      IF KampTilbPropBetalFor:CHECKED THEN 
        cWhere = "WHERE CAN-DO('1,3',STRING(KampRabattTypeId))".
      ELSE
        cWhere = "WHERE CAN-DO('2,3,5',STRING(KampRabattTypeId))".
    END.
    ELSE IF KampTilbTypeId:SCREEN-VALUE = '3' THEN
    DO:
      hbcBelop:LABEL = 'Prosent'.
      IF KampTilbPropBetalFor:CHECKED THEN 
        cWhere = "WHERE CAN-DO('1,3',STRING(KampRabattTypeId))".
      ELSE
        cWhere = "WHERE CAN-DO('2,3,6',STRING(KampRabattTypeId))".
    END.
    ELSE IF KampTilbTypeId:SCREEN-VALUE = '4' THEN
    DO:
      hbcBelop:LABEL = 'Betal for'.
      IF KampTilbPropBetalFor:CHECKED THEN 
        cWhere = "WHERE CAN-DO('1,2,3',STRING(KampRabattTypeId))".
      ELSE
        cWhere = "WHERE CAN-DO('2,3,9',STRING(KampRabattTypeId))".
    END.
    ELSE IF KampTilbTypeId:SCREEN-VALUE = '10' THEN
        cWhere = "WHERE CAN-DO('7',STRING(KampRabattTypeId))".
    
    IF VALID-HANDLE(hcbRabattType) THEN
    ASSIGN 
      hcbRabattType:DELIMITER = "|"
      hcbRabattType:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("getFieldList","KampRabattType;KampRabattTypeNavn;KampRabattTypeId",cWhere)
    . 
    IF KampTilbTypeId:SCREEN-VALUE = '10' THEN 
        hcbRabattType:SCREEN-VALUE = '7'.

  END.
  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setButton C-Win 
FUNCTION setButton RETURNS LOGICAL
  (INPUT ibFlag AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR hWidget AS HANDLE NO-UNDO.
  
  hWidget = DYNAMIC-FUNCTION("getEventWidget",hToolbar2,"newArt","button").
  IF VALID-HANDLE(hWidget) THEN
    hWidget:SENSITIVE = ibFlag.
  hWidget = DYNAMIC-FUNCTION("getEventWidget",hToolbar2,"newFam","button").
  IF VALID-HANDLE(hWidget) THEN
    hWidget:SENSITIVE = ibFlag.
  hWidget = DYNAMIC-FUNCTION("getEventWidget",hToolbar2,"newKupong","button").
  IF VALID-HANDLE(hWidget) THEN
    hWidget:SENSITIVE = ibFlag.
  
  DO WITH FRAME {&FRAME-NAME}:
    KampTilbGrenseAntall:SENSITIVE = KampTilbGrenseAntallBruk:CHECKED.
    IF NOT KampTilbGrenseAntallBruk:CHECKED THEN KampTilbGrenseAntall:SCREEN-VALUE = '0'.
    KampTilbGrenseAntall:SENSITIVE = KampTilbGrenseAntallBruk:CHECKED.
    
/*     KampTilbBelop:SENSITIVE = KampTilbPropBetalFor:CHECKED. */
/*     IF NOT KampTilbPropBetalFor:CHECKED THEN KampTilbBelop:SCREEN-VALUE = '0'. */
    KampTilbPopUpTekst:SENSITIVE = KampTilbPopUpTekstBruk:CHECKED.
  END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

