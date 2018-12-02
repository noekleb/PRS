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
DEF VAR hWindow         AS HANDLE NO-UNDO.

DEF VAR cAdgang         AS CHAR   NO-UNDO.
DEF VAR cTekst          AS CHAR   NO-UNDO.
DEF VAR dDec            AS DEC    NO-UNDO.
DEF VAR hSearchField    AS HANDLE NO-UNDO.
DEF VAR hBuffer         AS HANDLE NO-UNDO.
DEF VAR iCL             AS INT    NO-UNDO.
DEF VAR hwgenData       AS HANDLE.

DEF VAR cRowidList        AS CHAR   NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS B-sokButikk rectBrowse rectToolBar ~
rectWinToolbar RECT-64 searchField fiButikkNr T-VisStd fiFraRegDato ~
fiTilRegDato fiBeskrivelse B-sokKonto fiFraEDato fiTilEDato fiKontoNr ~
BUTTON-1 ButikkNr TTId B-sokKonto-2 btnCalFraDato-11 Beskrivelse ~
btnCalFraDato-7 btnCalFraDato-12 btnCalFraDato-8 
&Scoped-Define DISPLAYED-OBJECTS fiButikkNr T-VisStd fiFraRegDato ~
fiTilRegDato fiBeskrivelse fiFraEDato fiTilEDato fiKontoNr ButikkNr TTId ~
Beskrivelse KontoNr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-sokButikk 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-sokKonto 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-sokKonto-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnCalFraDato-11 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraDato-12 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraDato-7 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraDato-8 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON BUTTON-1 
     LABEL "Blank filter" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE Beskrivelse AS CHARACTER FORMAT "X(256)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 40.2 BY 1 TOOLTIP "Merknad".

DEFINE VARIABLE ButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikknr" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE fiBeskrivelse AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 46.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikknr" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE fiFraEDato AS DATE FORMAT "99/99/99" 
     LABEL "Fra/til endret dato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Fra utstedelsesdato for rabattsjekk.".

DEFINE VARIABLE fiFraRegDato AS DATE FORMAT "99/99/99" 
     LABEL "Fra/til registrert dato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Utstedelsesdato for rabattsjekk.".

DEFINE VARIABLE fiKontoNr AS INTEGER FORMAT ">>>9999" INITIAL 0 
     LABEL "Kontonummer" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1.

DEFINE VARIABLE fiTilEDato AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Utstedelsesdato for rabattsjekk.".

DEFINE VARIABLE fiTilRegDato AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Utstedelsesdato for rabattsjekk.".

DEFINE VARIABLE KontoNr AS INTEGER FORMAT ">>>9999" INITIAL 0 
     LABEL "Kontonr" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE TTId AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Trans.type" 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE RECTANGLE RECT-64
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 24.29.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 140.4 BY 24.29.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY .95.

DEFINE VARIABLE T-VisStd AS LOGICAL INITIAL no 
     LABEL "Vis standardoppsett" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-sokButikk AT ROW 2.43 COL 44.8
     fiButikkNr AT ROW 2.43 COL 22.4 COLON-ALIGNED HELP
          "Nr. på butikk som har utstedtsjekken"
     T-VisStd AT ROW 2.48 COL 81
     fiFraRegDato AT ROW 3.38 COL 142 COLON-ALIGNED HELP
          "Dato da sjekken ble utsted"
     fiTilRegDato AT ROW 3.38 COL 164 COLON-ALIGNED HELP
          "Dato da sjekken ble utsted" NO-LABEL
     fiBeskrivelse AT ROW 3.43 COL 22.4 COLON-ALIGNED
     B-sokKonto AT ROW 4.43 COL 36.2
     fiFraEDato AT ROW 4.38 COL 142 COLON-ALIGNED HELP
          "Dato da sjekken ble utsted"
     fiTilEDato AT ROW 4.38 COL 164 COLON-ALIGNED HELP
          "Dato da sjekken ble utsted" NO-LABEL
     fiKontoNr AT ROW 4.43 COL 22.4 COLON-ALIGNED HELP
          "Kontonummer i regnskap"
     BUTTON-1 AT ROW 5.19 COL 189
     ButikkNr AT ROW 7.19 COL 159 COLON-ALIGNED HELP
          "Nr. på butikk som har utstedtsjekken"
     TTId AT ROW 8.38 COL 159 COLON-ALIGNED
     B-sokKonto-2 AT ROW 10.76 COL 177
     btnCalFraDato-11 AT ROW 3.43 COL 182
     Beskrivelse AT ROW 9.57 COL 159 COLON-ALIGNED HELP
          "Merknad"
     KontoNr AT ROW 10.76 COL 159 COLON-ALIGNED HELP
          "Unikt ID for eksport av data for butikk"
     btnCalFraDato-7 AT ROW 3.43 COL 160
     btnCalFraDato-12 AT ROW 4.38 COL 182
     btnCalFraDato-8 AT ROW 4.38 COL 160
     rectBrowse AT ROW 6.71 COL 1.6
     rectToolBar AT ROW 1.14 COL 1.8
     rectWinToolbar AT ROW 1.14 COL 194
     RECT-64 AT ROW 6.71 COL 142
     searchField AT ROW 5.67 COL 1.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 203.6 BY 30.24.


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
         TITLE              = "Kontering av transatskjonstyper for SIE eksport"
         HEIGHT             = 30.19
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN KontoNr IN FRAME DEFAULT-FRAME
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
ON END-ERROR OF C-Win /* Kontering av transatskjonstyper for SIE eksport */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kontering av transatskjonstyper for SIE eksport */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Kontering av transatskjonstyper for SIE eksport */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-sokButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-sokButikk C-Win
ON CHOOSE OF B-sokButikk IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fibutikkNr
DO:
    cTekst = "Butik".                    
    RUN JBoxDLookup.w ("Butiker;Butik;ButNamn", "where Butiker.harButikksystem = true and Butiker.ApningsDato <> ? and Butiker.NedlagtDato = ?", INPUT-OUTPUT cTekst).

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    ELSE DO:
        ASSIGN
            fiButikkNr:SCREEN-VALUE = cTekst
            fiButikkNr:MODIFIED     = TRUE
            .
        APPLY "any-printable" TO fiButikkNr.
    END.
  APPLY "ENTRY" TO fiButikkNr.
  RUN InvokeMethod (hBrowse,'OpenQuery').

  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-sokKonto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-sokKonto C-Win
ON CHOOSE OF B-sokKonto IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiKontoNr
DO:
    cTekst = "KontoNr".                    
    RUN JBoxDLookup.w ("Kontotabell;KontoNr;KontoNavn", "where true", INPUT-OUTPUT cTekst).

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    ELSE DO:
        ASSIGN
            fiKontoNr:SCREEN-VALUE = cTekst
            fiKontoNr:MODIFIED     = TRUE
            .
        APPLY "any-printable" TO fiKontoNr.
    END.
  APPLY "ENTRY" TO fiKontoNr.
  RUN InvokeMethod (hBrowse,'OpenQuery').

  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-sokKonto-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-sokKonto-2 C-Win
ON CHOOSE OF B-sokKonto-2 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF KontoNr
DO:
    cTekst = "KontoNr".                    
    RUN JBoxDLookup.w ("Kontotabell;KontoNr;KontoNavn", "where true", INPUT-OUTPUT cTekst).

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    ELSE DO:
        ASSIGN
            KontoNr:SCREEN-VALUE = cTekst
            KontoNr:MODIFIED     = TRUE
            .
        APPLY "any-printable" TO KontoNr.
    END.
  APPLY "ENTRY" TO KontoNr.

  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-11 C-Win
ON CHOOSE OF btnCalFraDato-11 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiTilRegDato
DO:
  RUN Cal.w (fiTilRegDato:HANDLE).
  RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-12 C-Win
ON CHOOSE OF btnCalFraDato-12 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiTilEDato
DO:
  RUN Cal.w (fiTilEDato:HANDLE).
  RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-7 C-Win
ON CHOOSE OF btnCalFraDato-7 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiFraRegDato
DO:
  RUN Cal.w (fiFraRegDato:HANDLE).
  RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-8 C-Win
ON CHOOSE OF btnCalFraDato-8 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiFraEDato
DO:
  RUN Cal.w (fiFraEDato:HANDLE).
  RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Blank filter */
DO:
    RUN clearFilterRecord.  
    RUN InvokeMethod (hBrowse,'OpenQuery').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBeskrivelse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBeskrivelse C-Win
ON LEAVE OF fiBeskrivelse IN FRAME DEFAULT-FRAME /* Beskrivelse */
DO:
  RUN InvokeMethod (hBrowse,'OpenQuery').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiButikkNr C-Win
ON LEAVE OF fiButikkNr IN FRAME DEFAULT-FRAME /* Butikknr */
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFraEDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFraEDato C-Win
ON LEAVE OF fiFraEDato IN FRAME DEFAULT-FRAME /* Fra/til endret dato */
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFraRegDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFraRegDato C-Win
ON LEAVE OF fiFraRegDato IN FRAME DEFAULT-FRAME /* Fra/til registrert dato */
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiKontoNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiKontoNr C-Win
ON LEAVE OF fiKontoNr IN FRAME DEFAULT-FRAME /* Kontonummer */
DO:
  RUN InvokeMethod (hBrowse,'OpenQuery').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTilEDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTilEDato C-Win
ON LEAVE OF fiTilEDato IN FRAME DEFAULT-FRAME
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTilRegDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTilRegDato C-Win
ON LEAVE OF fiTilRegDato IN FRAME DEFAULT-FRAME
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-VisStd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-VisStd C-Win
ON VALUE-CHANGED OF T-VisStd IN FRAME DEFAULT-FRAME /* Vis standardoppsett */
DO:
  IF SELF:CHECKED THEN
      ASSIGN 
        fiButikkNr:SCREEN-VALUE = ''
        fiButikkNr:SENSITIVE    = FALSE
        B-sokButikk:SENSITIVE  = FALSE
        .
  ELSE 
      ASSIGN 
        fiButikkNr:SENSITIVE    = TRUE 
        B-sokButikk:SENSITIVE  = TRUE
        .
  RUN InvokeMethod (hBrowse,'OpenQuery').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}
       hWindow                       = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF VALID-HANDLE(hwgenData) THEN
    DELETE PROCEDURE hwgenData.

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
  RUN InitWindow.
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearFilterRecord C-Win 
PROCEDURE clearFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          fiButikkNr:SCREEN-VALUE      = ''
          FiBeskrivelse:SCREEN-VALUE   = ''
          FiKontoNr:SCREEN-VALUE       = ''
          fiFraRegDato:SCREEN-VALUE    = ?
          fiTilRegDato:SCREEN-VALUE    = ?
          fiFraEDato:SCREEN-VALUE      = ?
          fiTilEDato:SCREEN-VALUE      = ?
          .
      DYNAMIC-FUNCTION("setSortString",hBrowse,"").
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

  RUN JBoxBrowseMsgUpdateVal.w ("Slette linje(r) ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"sietranstype_delete.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"sietranstype_delete.p",'').
END.
ELSE
  LEAVE.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

RUN InvokeMethod (hBrowse,'OpenQuery').
/*   IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")). */
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
        ButikkNr:SENSITIVE    = FALSE
        TTId:SENSITIVE        = FALSE
        Beskrivelse:SENSITIVE = FALSE
        .
END.

/*
/* Flagger at purretrinnet er i bruk på en eller flere kundereskontroposter. */
IF hFieldMap:BUFFER-FIELD("bIBruk"):BUFFER-VALUE <> "" THEN
    BROWSE rectBrowse:HANDLE hFieldMap:BUFFER-FIELD("Purretrinn"):BGCOLOR = 11.
*/
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
  DISPLAY fiButikkNr T-VisStd fiFraRegDato fiTilRegDato fiBeskrivelse fiFraEDato 
          fiTilEDato fiKontoNr ButikkNr TTId Beskrivelse KontoNr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-sokButikk rectBrowse rectToolBar rectWinToolbar RECT-64 searchField 
         fiButikkNr T-VisStd fiFraRegDato fiTilRegDato fiBeskrivelse B-sokKonto 
         fiFraEDato fiTilEDato fiKontoNr BUTTON-1 ButikkNr TTId B-sokKonto-2 
         btnCalFraDato-11 Beskrivelse btnCalFraDato-7 btnCalFraDato-12 
         btnCalFraDato-8 
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
cAdgang = '0'. /*(DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 16 and SysGr = 45 and ParaNr = 2","Parameter1")).  */

iCL = INT(DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1")).

DO WITH FRAME {&FRAME-NAME}:
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",          
                    rectBrowse:HANDLE,             
                    100,                           
                    "MULTIPLE",
                    "SIETranstype"
                  + ";TTId|TTId"
                  + ";Beskrivelse"
                  + ";TBId|TBId|>>9"
                  + ";ButikkNr|Butikknr"
                  + ";KontoNr|Kontonr"
                  + ";RegistrertDato|Opprettet"
                  + ";+RegistrertTid|character|x(6)|RegistrertTid(ROWID)|Kl"
                  + ";RegistrertAv"
                  + ";EDato|Endret"
                  + ";+EndretTid|character|x(6)|EndretTid(ROWID)|Kl"
                  + ";BrukerId"
                  + ",TransBeskr"
                    + ";Innloser|Innløser@4"
                    ,
                    "WHERE FALSE, FIRST TransBeskr NO-LOCK WHERE TransBeskr.TTId = SIETransType.TTId"
                   ,"SORT|TTId").                    
  hBrowse:NAME = "brwSIETransType". 
  
  /* Henter buffer handle for å kunne lese ut verdier fra feltene i bufferet. */
  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",    
                                                 
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,  
                    "ButikkNr,TTId,Beskrivelse,KontoNr",              
                    "",              
                    "","",                       
                    "").   
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore").
  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).

  DYNAMIC-FUNCTION('setAttribute',hBrowse,'calcfieldproc','sietranstype_brwcalc.p').
  
  DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,            
                    "Fil",                         
                    "undo;Angre,delete;Slett,save;Lagre,Refresh,BrowseConfig;Kolonneoppsett,excel;Eksporter til E&xcel" +
                    ",OppdStdButikk;Oppdater standardoppsett¤ENABLED" +
                    ",kopierOppsett;Kopier oppsett", 
                    
                   "maxborder").                   

  DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,          
                    "Fil",                          
                    "close;Avslutt",
                                                     
                                                    
                    "right,enable").                
  /* Link objects: */

  DYNAMIC-FUNCTION("LinkAllObjects",                
                                                    
                    THIS-PROCEDURE:CURRENT-WINDOW,  
                    TRUE,                           
                    STRING(rectWinToolBar:HANDLE)). 

  ASSIGN T-VisStd:CHECKED = TRUE.
  RUN InvokeMethod (hBrowse,'OpenQuery').
  APPLY "value-changed" TO hBrowse.
END.

DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar").
DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,
                 "ButikkNr,TTId,Beskrivelse,KontoNr").

DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,950,950,0,250).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
APPLY "ENTRY" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kopierOppsettRecord C-Win 
PROCEDURE kopierOppsettRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn    AS INT  NO-UNDO.
DEF VAR cValues    AS CHAR NO-UNDO.

iReturn = 0.
RUN d-settbutikk.w   ("Kopier til butikk",
                            hBrowse:NUM-SELECTED-ROWS,
                            IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                              INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                            ELSE 99999,
                            OUTPUT iReturn,
                            OUTPUT cValues).

IF iReturn = 0 THEN RETURN.

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"sietranstype_kopiertranstype.p",cValues).
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"sietranstype_kopiertranstype.p",cValues).
END.
ELSE
  LEAVE.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

RUN InvokeMethod (hBrowse,'OpenQuery').
/*   IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")). */

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
DEF VAR cWhere   AS CHAR NO-UNDO.
DEF VAR cVPIDato AS CHAR NO-UNDO.

DEF VAR iFraETid AS INT NO-UNDO.
DEF VAR iTilETid AS INT NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    
    IF INT(fiButikkNr:SCREEN-VALUE) > 0 THEN 
        ASSIGN T-VisStd:CHECKED = FALSE.

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",'').

    IF T-VisStd:CHECKED THEN
        cWhere = ' WHERE ButikkNr = 0 '.
    ELSE 
        cWhere = ''.
    cWhere = cWhere + buildFilter(cWhere,fiButikkNr:HANDLE,'ButikkNr','EQ').
    cWhere = cWhere + buildFilter(cWhere,FiBeskrivelse:HANDLE,'Beskrivelse','BEGINS').
    cWhere = cWhere + buildFilter(cWhere,FiKontoNr:HANDLE,'KontoNr','EQ').
    cWhere = cWhere + buildFilter(cWhere,fiFraRegDato:HANDLE,'RegistrertDato','GE').
    cWhere = cWhere + buildFilter(cWhere,fiTilRegDato:HANDLE,'RegistrertDato','LE').
    cWhere = cWhere + buildFilter(cWhere,fiFraEDato:HANDLE,'EDato','GE').
    cWhere = cWhere + buildFilter(cWhere,fiTilEDato:HANDLE,'EDato','LE').
    
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cWhere).
    
    ASSIGN
        fiButikkNr:MODIFIED      = FALSE 
        FiBeskrivelse:MODIFIED   = FALSE 
        FiKontoNr:MODIFIED       = FALSE 
        fiFraRegDato:MODIFIED    = FALSE
        fiTilRegDato:MODIFIED    = FALSE
        fiFraEDato:MODIFIED      = FALSE
        fiTilEDato:MODIFIED      = FALSE
    .
  END.  

  RUN SUPER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdStdButikkRecord C-Win 
PROCEDURE OppdStdButikkRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN wbyggSIETranstype.w PERSISTENT SET hwgenData.
  IF VALID-HANDLE(hwgenData) THEN RUN setParentHandle IN hwgenData (THIS-PROCEDURE:HANDLE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshListe C-Win 
PROCEDURE refreshListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN InvokeMethod (hBrowse,'OpenQuery').
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
    cState = DYNAMIC-FUNCTION('getToolbarState',hToolbar)
    .


    DO WITH FRAME {&FRAME-NAME}:
        IF cState = "NEW" THEN
        DO:
            /*
            cTekst = DYNAMIC-FUNCTION("getFieldValues","RabSjekkType",
                                       "WHERE RabSjekkTypeNr = '" + RabSjekkTypeNr:SCREEN-VALUE + "'","RabSjekkTypeNr").  
            IF cTekst <> ? THEN
            DO:
                MESSAGE 'Det er allerede registrert en rabattsjekktype av denne type.' 
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN.
            END.
            */
        END.
        
        IF DEC(ButikkNr:SCREEN-VALUE) > 0 THEN
        DO:
            cTekst = DYNAMIC-FUNCTION("getFieldValues","Butiker",
                                       "WHERE Butik = '" + ButikkNr:SCREEN-VALUE + "'","Butik").  
            IF cTekst = ? THEN
            DO:
                MESSAGE 'Ugyldig butikk:' ButikkNr:SCREEN-VALUE SKIP
                    'Resultat: ' cTekst
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN.
            END.
        END.
    END.
    RUN SUPER.
    APPLY "ENTRY" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undoRecord C-Win 
PROCEDURE undoRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN SUPER.
    APPLY "ENTRY" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

