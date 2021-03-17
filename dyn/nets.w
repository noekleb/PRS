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
DEF VAR hBuffer         AS HANDLE NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.
DEF VAR hSearchField    AS HANDLE NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS fltSendt btnFraDato fltFraDato fltTilDato ~
fltTransactionId btnBlank btnStartUtvalg btnTilDato fltButikkNr ~
btnFraDatoSendt fltFraDatoSendt fltTilDatoSendt btnTilDatoSendt btnButikk ~
fltFraKundeNr btnFraKundenr fltTilKundeNr btnTilKundeNr rectBrowse ~
rectToolBar rectWinToolbar searchField 
&Scoped-Define DISPLAYED-OBJECTS fltSendt fltFraDato fltTilDato ~
fltTransactionId fltButikkNr fltFraDatoSendt fltTilDatoSendt fltFraKundeNr ~
fltTilKundeNr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnBlank 
     LABEL "Blank filter" 
     SIZE 13 BY 1.

DEFINE BUTTON btnButikk  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnFraDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnFraDatoSendt 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnFraKundenr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnStartUtvalg 
     LABEL "Start søk" 
     SIZE 13 BY 1.

DEFINE BUTTON btnTilDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnTilDatoSendt 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnTilKundeNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE fltButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikknr" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE fltFraDato AS DATE FORMAT "99/99/99" 
     LABEL "Dato fra" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE fltFraDatoSendt AS DATE FORMAT "99/99/99" 
     LABEL "Dato fra" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE fltFraKundeNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Kundenr fra" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE fltTilDato AS DATE FORMAT "99/99/99" 
     LABEL "til" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE fltTilDatoSendt AS DATE FORMAT "99/99/99" 
     LABEL "til" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE fltTilKundeNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "til" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE fltTransactionId AS CHARACTER FORMAT "X(30)" 
     LABEL "TransactionId" 
     VIEW-AS FILL-IN 
     SIZE 46.4 BY 1.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 205 BY 27.05.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY .95.

DEFINE VARIABLE fltSendt AS LOGICAL INITIAL no 
     LABEL "Sendt" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.8 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fltSendt AT ROW 4.33 COL 137.2 WIDGET-ID 22
     btnFraDato AT ROW 3.05 COL 98.4 NO-TAB-STOP 
     fltFraDato AT ROW 3.14 COL 83 COLON-ALIGNED
     fltTilDato AT ROW 3.1 COL 103.8 COLON-ALIGNED
     fltTransactionId AT ROW 4.19 COL 14.6 COLON-ALIGNED
     btnBlank AT ROW 3.19 COL 193
     btnStartUtvalg AT ROW 4.24 COL 193
     btnTilDato AT ROW 3.1 COL 119.2 NO-TAB-STOP 
     fltButikkNr AT ROW 3.14 COL 14.6 COLON-ALIGNED
     btnFraDatoSendt AT ROW 4.1 COL 98.4 WIDGET-ID 2 NO-TAB-STOP 
     fltFraDatoSendt AT ROW 4.19 COL 83 COLON-ALIGNED WIDGET-ID 6
     fltTilDatoSendt AT ROW 4.14 COL 103.8 COLON-ALIGNED WIDGET-ID 8
     btnTilDatoSendt AT ROW 4.14 COL 119.2 WIDGET-ID 4 NO-TAB-STOP 
     btnButikk AT ROW 3.14 COL 29.6 WIDGET-ID 12 NO-TAB-STOP 
     fltFraKundeNr AT ROW 3.14 COL 135 COLON-ALIGNED WIDGET-ID 16
     btnFraKundenr AT ROW 3.14 COL 158 WIDGET-ID 14 NO-TAB-STOP 
     fltTilKundeNr AT ROW 3.14 COL 163.2 COLON-ALIGNED WIDGET-ID 20
     btnTilKundeNr AT ROW 3.14 COL 186.2 WIDGET-ID 18 NO-TAB-STOP 
     rectBrowse AT ROW 5.52 COL 1.6
     rectToolBar AT ROW 1.19 COL 22.4
     rectWinToolbar AT ROW 1.19 COL 196.2
     searchField AT ROW 1.19 COL 1.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 206 BY 31.76.


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
         TITLE              = "Nets/MayFlower Transaksjonslogg"
         HEIGHT             = 31.76
         WIDTH              = 206
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 384
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
   FRAME-NAME Custom                                                    */
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
ON END-ERROR OF C-Win /* Nets/MayFlower Transaksjonslogg */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Nets/MayFlower Transaksjonslogg */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Nets/MayFlower Transaksjonslogg */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBlank C-Win
ON CHOOSE OF btnBlank IN FRAME DEFAULT-FRAME /* Blank filter */
DO:
        DO WITH FRAME {&Frame-name}:
            ASSIGN
                fltButikkNr:SCREEN-VALUE      = ''
                fltTransactionId:SCREEN-VALUE       = ''
                fltFraDato:SCREEN-VALUE = ''
                fltTilDato:SCREEN-VALUE = ''
                fltFraDatoSendt:SCREEN-VALUE = ''
                fltTilDatoSendt:SCREEN-VALUE = ''
                .
            DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
            RUN OpenQuery.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnButikk C-Win
ON CHOOSE OF btnButikk IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fltButikkNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "butik;butnamn".

  RUN JBoxDLookup.w ("butiker;butik;butnamn", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN 
      fltButikkNr:SCREEN-VALUE   = ENTRY(1,cLookupValue,"|")
    .
/*       SasBeskr:SCREEN-VALUE = ENTRY(2,cLookupValue,"|"). */
    IF fltButikkNr:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').   
  END.
  ELSE APPLY "entry" TO fltButikkNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFraDato C-Win
ON CHOOSE OF btnFraDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fltFraDato IN FRAME DEFAULT-FRAME
DO:

  RUN Cal.w (fltFraDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFraDatoSendt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFraDatoSendt C-Win
ON CHOOSE OF btnFraDatoSendt IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fltFraDatoSendt IN FRAME DEFAULT-FRAME
DO:

  RUN Cal.w (fltFraDatoSendt:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFraKundenr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFraKundenr C-Win
ON CHOOSE OF btnFraKundenr IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fltFraKundeNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "KundeNr;Navn".

  RUN JBoxDLookup.w ("Kunde;KundeNr;Navn", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN 
      fltFraKundeNr:SCREEN-VALUE   = ENTRY(1,cLookupValue,"|")
    .
/*       SasBeskr:SCREEN-VALUE = ENTRY(2,cLookupValue,"|"). */
    IF fltFraKundeNr:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').   
  END.
  ELSE APPLY "entry" TO fltFraKundeNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStartUtvalg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStartUtvalg C-Win
ON CHOOSE OF btnStartUtvalg IN FRAME DEFAULT-FRAME /* Start søk */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTilDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTilDato C-Win
ON CHOOSE OF btnTilDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fltTilDato IN FRAME DEFAULT-FRAME
    DO:

        RUN Cal.w (fltTilDato:HANDLE).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTilDatoSendt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTilDatoSendt C-Win
ON CHOOSE OF btnTilDatoSendt IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fltTilDatoSendt IN FRAME DEFAULT-FRAME
    DO:

        RUN Cal.w (fltTilDatoSendt:HANDLE).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTilKundeNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTilKundeNr C-Win
ON CHOOSE OF btnTilKundeNr IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fltTilKundeNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "KundeNr;Navn".

  RUN JBoxDLookup.w ("Kunde;KundeNr;Navn", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN 
      fltTilKundeNr:SCREEN-VALUE   = ENTRY(1,cLookupValue,"|")
    .
/*       SasBeskr:SCREEN-VALUE = ENTRY(2,cLookupValue,"|"). */
    IF fltTilKundeNr:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').   
  END.
  ELSE APPLY "entry" TO fltTilKundeNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fltButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fltButikkNr C-Win
ON RETURN OF fltButikkNr IN FRAME DEFAULT-FRAME /* Butikknr */
DO:
  APPLY "CHOOSE" TO btnStartUtvalg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fltFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fltFraDato C-Win
ON RETURN OF fltFraDato IN FRAME DEFAULT-FRAME /* Dato fra */
DO:
  APPLY "CHOOSE" TO btnStartUtvalg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fltFraDatoSendt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fltFraDatoSendt C-Win
ON RETURN OF fltFraDatoSendt IN FRAME DEFAULT-FRAME /* Dato fra */
DO:
  APPLY "CHOOSE" TO btnStartUtvalg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fltFraKundeNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fltFraKundeNr C-Win
ON RETURN OF fltFraKundeNr IN FRAME DEFAULT-FRAME /* Kundenr fra */
DO:
  APPLY "CHOOSE" TO btnStartUtvalg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fltTilDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fltTilDato C-Win
ON RETURN OF fltTilDato IN FRAME DEFAULT-FRAME /* til */
DO:
  APPLY "CHOOSE" TO btnStartUtvalg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fltTilDatoSendt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fltTilDatoSendt C-Win
ON RETURN OF fltTilDatoSendt IN FRAME DEFAULT-FRAME /* til */
DO:
  APPLY "CHOOSE" TO btnStartUtvalg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fltTilKundeNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fltTilKundeNr C-Win
ON RETURN OF fltTilKundeNr IN FRAME DEFAULT-FRAME /* til */
DO:
  APPLY "CHOOSE" TO btnStartUtvalg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fltTransactionId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fltTransactionId C-Win
ON RETURN OF fltTransactionId IN FRAME DEFAULT-FRAME /* TransactionId */
DO:
  APPLY "CHOOSE" TO btnStartUtvalg.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR obOk AS LOG NO-UNDO.
bOK = FALSE.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse AND 
    hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
    DO:
        RUN gviskvittokopi2.w (INT(hBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE)
                               ,1
                               ,INT(hBuffer:BUFFER-FIELD("KasseNr"):BUFFER-VALUE)
                               ,DATE(hBuffer:BUFFER-FIELD("Dato"):BUFFER-VALUE)
                               ,DEC(hBuffer:BUFFER-FIELD("BongNr"):BUFFER-VALUE)
                               ,THIS-PROCEDURE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteRecord C-Win 
PROCEDURE deleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.
DEF VAR cRowIdList  AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Slette linje(r) ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"nets_delete.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"nets_delete.p",'').
END.
ELSE
  LEAVE.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

RUN InvokeMethod (hBrowse,'OpenQuery').
/*
IF iReturn = 1 
    THEN RUN InvokeMethod (hBrowse,'OpenQuery'). 
ELSE 
    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).
*/
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
  /*KundeNr:SENSITIVE IN FRAME {&FRAME-NAME} =  DYNAMIC-FUNCTION('getToolbarState',hToolbar) = 'New'.*/
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
  DISPLAY fltSendt fltFraDato fltTilDato fltTransactionId fltButikkNr 
          fltFraDatoSendt fltTilDatoSendt fltFraKundeNr fltTilKundeNr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fltSendt btnFraDato fltFraDato fltTilDato fltTransactionId btnBlank 
         btnStartUtvalg btnTilDato fltButikkNr btnFraDatoSendt fltFraDatoSendt 
         fltTilDatoSendt btnTilDatoSendt btnButikk fltFraKundeNr btnFraKundenr 
         fltTilKundeNr btnTilKundeNr rectBrowse rectToolBar rectWinToolbar 
         searchField 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetRecord C-Win 
PROCEDURE GetRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cRettning AS CHARACTER  NO-UNDO.

    DEFINE OUTPUT PARAMETER iButikkNr LIKE BongHode.ButikkNr NO-UNDO.
    DEFINE OUTPUT PARAMETER iGruppeNr LIKE BongHode.GruppeNr NO-UNDO.
    DEFINE OUTPUT PARAMETER iKasseNr  LIKE BongHode.KasseNr  NO-UNDO.
    DEFINE OUTPUT PARAMETER dDato     LIKE BongHode.Dato     NO-UNDO.
    DEFINE OUTPUT PARAMETER iBongNr   LIKE BongHode.BongNr   NO-UNDO.
    
    DEFINE VARIABLE cYMD         AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDato        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cVerdier     AS CHARACTER  NO-UNDO.
    DEF VAR cTekst AS CHAR NO-UNDO.

    IF CAN-DO("Prev,Next",cRettning) AND hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN 
    DO:
        RUN PrevNext (cRettning).
        cTekst = DYNAMIC-FUNCTION("getFieldValues",
                                  "BongHode",
                                  "WHERE B_Id = '" + STRING(hBuffer:BUFFER-FIELD("B_Id"):BUFFER-VALUE) + "'","ButikkNr,KasseNr,Dato,BongNr").
        IF cTekst <> ? AND NUM-ENTRIES(cTekst,'|') >= 4 THEN
            ASSIGN 
                iButikkNr = INT(ENTRY(1,cTekst,'|'))
                iGruppeNr = 1
                iKasseNr  = INT(ENTRY(2,cTekst,'|'))
                dDato     = DATE(ENTRY(3,cTekst,'|'))
                iBongNr   = INT(ENTRY(4,cTekst,'|')).
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
DO WITH FRAME {&FRAME-NAME}:
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",          /* Create a browse object */
                    rectBrowse:HANDLE,              /* Rectangle to define coordinates for browse */
                    100,                            /* Rows to batch */
                    "MULTIPLE",                             /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "Nets"
                    + ";iJBoxCompanyId|Firmanr" 
                    + ";ButikkNr|Butikknr" 
                    + ";TransactionId|TransactionId|x(35)"
                    + ";Dato"
                    + ";Sendt|Sendt|*/ "                             
                    + ";SendtDato"
                    + ";!SendtTid"
                    + ";+fSendtTid|CHAR|x(12)|SendtTid_Kl(SendtTid)|Kl"
                    + ",BongHode"
                      + ";B_Id|BongId|>>>>99999999999999999999999"
                      + ";BongNr"
                      + ";KasseNr"
                      + ";KundeNr"
                      + ";MedlemsNr@14"
                    + ",Kunde"
                      + ";Navn|Navn"
                      + ";EksterntKundeNr|Ekst.Kundenr"
                    + ",KundeKort"
                      + ";KortNr"
                    + ",Medlem"
                      + ";Fornavn|Fornavn@15"
                      + ";Etternavn|Etternavn@16"
/*                    ,"WHERE where KundeNr > 0" + */
                   ,"WHERE FALSE" + 
                             ",FIRST BongHode NO-LOCK WHERE BongHode.B_Id = Nets.B_Id OUTER-JOIN" +
                             ",FIRST Kunde NO-LOCK WHERE Kunde.KundeNr = BongHode.KundeNr OUTER-JOIN" +
                             ",FIRST KundeKort NO-LOCK WHERE KundeKort.KundeNr = BongHode.KundeNr OUTER-JOIN" +
                             ",FIRST Medlem NO-LOCK WHERE Medlem.MedlemsNr = BongHode.MedlemsNr OUTER-JOIN"
                    ,"sort|TransactionId").             /* Initial sort column */
  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcfieldproc","nets_brwcalc.p"). 
  /*DYNAMIC-FUNCTION("setAttribute",hBrowse,"nocolumnsearch","Navn,Fornavn,Etternavn"). */
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","WHERE TransactionID >= ''"). 
  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).
  /*
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                    "KundeNr,KortNr",   /* Update columns in buffer */
                      "",                           /* Corresponding input fields (fill-in..). blank if equal to update columns */
                      "",        /* Additional buffer and displ.fields - not updateable*/
                      "",                           /* Corresponding fill-ins */
                    "").                            /* other input widgets and lookup buttons for update fill-ins */

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hBrowse).
  */
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "Delete,Refresh,excel;Eksporter til E&xcel" /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    ,"maxborder").                  /* Misc - enable, maxborder.. */
  
  /*DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).*/
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,          /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "close",
                    "right,enable").                      /* Misc - enable, maxborder.. */

  APPLY "RETURN" TO hSearchField.
  APPLY "value-changed" TO hBrowse.

END.

DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,350,0,350).

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
DEF VAR cWhere        AS CHAR NO-UNDO.
DEF VAR cPrescanKunde AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN fltButikkNr fltFraKundeNr fltTilKundeNr fltFraDato fltTilDato fltFraDatoSendt fltTilDatoSendt.


  DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",''). 


  cWhere = buildFilter(cWhere,fltButikkNr:HANDLE,'ButikkNr','EQ').      
/*   cWhere = cWhere + buildFilter(cWhere,fltNavn:HANDLE,'Navn','BEGINS').  */ /* <- queryFilter kan bare settes på primærtabellen (kundekort) */
  cWhere = cWhere + buildFilter(cWhere,fltTransactionId:HANDLE,'TransactionId','BEGINS').
  cWhere = cWhere + buildFilter(cWhere,fltFraDato:HANDLE,'Dato','GE').
  cWhere = cWhere + buildFilter(cWhere,fltTilDato:HANDLE,'Dato','LE').
  cWhere = cWhere + buildFilter(cWhere,fltFraDatoSendt:HANDLE,'SendtDato','GE').
  cWhere = cWhere + buildFilter(cWhere,fltTilDatoSendt:HANDLE,'SendtDato','LE').
  /*cWhere = cWhere + buildFilter(cWhere,fltFraKundeNr:HANDLE,'KundeNr','GE').*/
  /*cWhere = cWhere + buildFilter(cWhere,fltTilKundeNr:HANDLE,'KundeNr','LE').*/
/*   cWhere = cWhere + buildFilter(cWhere,fltButikkNr:HANDLE,'ButikkNr','EQ'). */
  IF fltSendt:CHECKED THEN 
      cWhere = buildFilter(cWhere,fltSendt:HANDLE,'Sendt','EQ').      

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cWhere). 



  IF DEC(fltFraKundeNr:SCREEN-VALUE) > 0 THEN 
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanqueryfilter",
                     "BongHode WHERE KundeNr >= " + fltFraKundeNr:SCREEN-VALUE +
                     (IF DEC(fltTilKundeNr:SCREEN-VALUE) > 0 THEN 
                         " AND KundeNr <= " + fltTilKundeNr:SCREEN-VALUE
                         ELSE "")
                     ).
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanqueryfilter","").


  ASSIGN
      fltFraDato:MODIFIED       = FALSE 
      fltFraDato:MODIFIED       = FALSE  
      fltFraDatoSendt:MODIFIED  = FALSE 
      fltFraDatoSendt:MODIFIED  = FALSE  
      fltTransactionId:MODIFIED = FALSE 
      fltButikkNr:MODIFIED      = FALSE
      .

END.

RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevNext C-Win 
PROCEDURE PrevNext :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cRettning AS CHARACTER  NO-UNDO.

IF CAN-DO("Prev,Next",cRettning) THEN DO:
  hBrowse:SELECT-FOCUSED-ROW().
  CASE cRettning:
      WHEN "Prev" THEN
          hBrowse:SELECT-PREV-ROW().
      WHEN "Next" THEN
        hBrowse:SELECT-NEXT-ROW().
  END CASE.
  APPLY "value-changed" TO hBrowse.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

