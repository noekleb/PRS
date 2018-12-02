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
DEF VAR hwbyggrabsjekk AS HANDLE.

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
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolBar rectWinToolbar ~
RECT-64 searchField Fornavn fiFraKjopsDato fiTilKjopsDato fiMedlemsNr ~
Etternavn fiFraSaldo fiTilSaldo fiButikkNr B-sokButik-2 BUTTON-1 MedlemsNr ~
ButikkNr KjopsBelop TildeltBelop KjopsDato Notat btnCalFraDato-6 ~
btnCalFraDato-10 B-sokMedlem btnCalFraDato B-sokButik B-sokMedlem2 
&Scoped-Define DISPLAYED-OBJECTS Fornavn fiFraKjopsDato fiTilKjopsDato ~
fiMedlemsNr Etternavn fiFraSaldo fiTilSaldo fiButikkNr MedlemsNr ButikkNr ~
KjopsBelop TildeltBelop KjopsDato Notat 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-sokButik 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-sokButik-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-sokMedlem 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-sokMedlem2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnCalFraDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraDato-10 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraDato-6 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON BUTTON-1 
     LABEL "Blank filter" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE Notat AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 42 BY 16.91 NO-UNDO.

DEFINE VARIABLE ButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikknr" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE Etternavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Etternavn" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE fiButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikknr" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE fiFraKjopsDato AS DATE FORMAT "99/99/99" 
     LABEL "Fra/til kjøps dato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE fiFraSaldo AS DECIMAL FORMAT "->>>,>>9":U INITIAL 0 
     LABEL "Fra/Til saldo" 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1 NO-UNDO.

DEFINE VARIABLE fiMedlemsNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Medlemsnr" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 TOOLTIP "Medlemsnummer".

DEFINE VARIABLE fiTilKjopsDato AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE fiTilSaldo AS DECIMAL FORMAT "->>>,>>9":U INITIAL 999999 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1 NO-UNDO.

DEFINE VARIABLE Fornavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fornavn" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE KjopsBelop AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Beløp" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE KjopsDato AS DATE FORMAT "99/99/99" 
     LABEL "Kjøpsdato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE MedlemsNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Medlemsnr" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 TOOLTIP "Medlemsnummer".

DEFINE VARIABLE TildeltBelop AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Tildelt" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE RECTANGLE RECT-64
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45 BY 24.52.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 157 BY 24.52.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Fornavn AT ROW 2.43 COL 74 COLON-ALIGNED
     fiFraKjopsDato AT ROW 2.43 COL 120 COLON-ALIGNED HELP
          "Siste gyldighetsdato på rabattsjekken"
     fiTilKjopsDato AT ROW 2.43 COL 142 COLON-ALIGNED HELP
          "Siste gyldighetsdato på rabattsjekken" NO-LABEL
     fiMedlemsNr AT ROW 2.48 COL 34 COLON-ALIGNED HELP
          "Peker til artikkel"
     Etternavn AT ROW 3.43 COL 74 COLON-ALIGNED
     fiFraSaldo AT ROW 3.43 COL 120 COLON-ALIGNED
     fiTilSaldo AT ROW 3.43 COL 142.2 COLON-ALIGNED NO-LABEL
     fiButikkNr AT ROW 3.48 COL 34.2 COLON-ALIGNED HELP
          "Nr. på butikk som har utstedtsjekken"
     B-sokButik-2 AT ROW 3.48 COL 52.2
     BUTTON-1 AT ROW 5.48 COL 189
     MedlemsNr AT ROW 8.67 COL 175.6 COLON-ALIGNED HELP
          "Peker til artikkel"
     ButikkNr AT ROW 9.67 COL 175.6 COLON-ALIGNED HELP
          "Nr. på butikk som har utstedtsjekken"
     KjopsBelop AT ROW 10.67 COL 175.6 COLON-ALIGNED HELP
          "Beløp det ble handlet for"
     TildeltBelop AT ROW 11.67 COL 175.6 COLON-ALIGNED HELP
          "Beløp det ble handlet for"
     KjopsDato AT ROW 12.67 COL 175.6 COLON-ALIGNED HELP
          "Siste gyldighetsdato på rabattsjekken"
     Notat AT ROW 14.1 COL 161 NO-LABEL
     btnCalFraDato-6 AT ROW 2.43 COL 138
     btnCalFraDato-10 AT ROW 2.43 COL 160
     B-sokMedlem AT ROW 8.67 COL 197.6
     btnCalFraDato AT ROW 12.67 COL 193.6
     B-sokButik AT ROW 9.67 COL 193.4
     B-sokMedlem2 AT ROW 2.48 COL 56
     rectBrowse AT ROW 6.71 COL 2
     rectToolBar AT ROW 1.14 COL 1.8
     rectWinToolbar AT ROW 1.14 COL 194
     RECT-64 AT ROW 6.71 COL 159
     searchField AT ROW 5.67 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 203.4 BY 30.38.


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
         TITLE              = "Vedlikehold medlemskjøp"
         HEIGHT             = 30.33
         WIDTH              = 203.4
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
ASSIGN 
       Notat:RETURN-INSERTED IN FRAME DEFAULT-FRAME  = TRUE.

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
ON END-ERROR OF C-Win /* Vedlikehold medlemskjøp */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vedlikehold medlemskjøp */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Vedlikehold medlemskjøp */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-sokButik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-sokButik C-Win
ON CHOOSE OF B-sokButik IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF B-sokButik
DO:
    cTekst = "Butik".                    
    RUN JBoxDLookup.w ("Butiker;Butik;ButNamn", "where Butiker.harButikksystem = true and Butiker.ApningsDato <> ? and Butiker.NedlagtDato = ?", INPUT-OUTPUT cTekst).

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    ELSE DO:
        ASSIGN
            ButikkNr:SCREEN-VALUE = cTekst
            ButikkNr:MODIFIED     = TRUE
            .
        APPLY "any-printable" TO ButikkNr.
    END.
  APPLY "ENTRY" TO ButikkNr.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-sokButik-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-sokButik-2 C-Win
ON CHOOSE OF B-sokButik-2 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiButikkNr
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


&Scoped-define SELF-NAME B-sokMedlem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-sokMedlem C-Win
ON CHOOSE OF B-sokMedlem IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF B-sokMedlem
DO:
    cTekst = "MedlemsNr".                    
    RUN JBoxDLookup.w ("Medlem;MedlemsNr;Fornavn;Etternavn", "where true", INPUT-OUTPUT cTekst).

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    ELSE DO:
        ASSIGN
            MedlemsNr:SCREEN-VALUE = cTekst
            MedlemsNr:MODIFIED     = TRUE.
        APPLY "any-printable" TO MedlemsNr.
    END.
    APPLY "ENTRY" TO MedlemsNr.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-sokMedlem2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-sokMedlem2 C-Win
ON CHOOSE OF B-sokMedlem2 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF B-sokMedlem2
DO:
    cTekst = "MedlemsNr".                    
    RUN JBoxDLookup.w ("Medlem;MedlemsNr;Fornavn;Etternavn",
                       "where true", INPUT-OUTPUT cTekst).

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    ELSE DO:
        ASSIGN
            fiMedlemsNr:SCREEN-VALUE = cTekst
            fiMedlemsNr:MODIFIED     = TRUE.
        APPLY "any-printable" TO fiMedlemsNr.
    END.
    APPLY "ENTRY" TO fiMedlemsNr.
    RUN InvokeMethod (hBrowse,'OpenQuery').

  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato C-Win
ON CHOOSE OF btnCalFraDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF KjopsDato
DO:
  RUN Cal.w (KjopsDato:HANDLE).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-10 C-Win
ON CHOOSE OF btnCalFraDato-10 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiTilKjopsDato
DO:
  RUN Cal.w (fiTilKjopsDato:HANDLE).
  RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-6 C-Win
ON CHOOSE OF btnCalFraDato-6 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiFraKjopsDato
DO:
  RUN Cal.w (fiFraKjopsDato:HANDLE).
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


&Scoped-define SELF-NAME Etternavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Etternavn C-Win
ON LEAVE OF Etternavn IN FRAME DEFAULT-FRAME /* Etternavn */
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


&Scoped-define SELF-NAME fiFraKjopsDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFraKjopsDato C-Win
ON LEAVE OF fiFraKjopsDato IN FRAME DEFAULT-FRAME /* Fra/til kjøps dato */
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiMedlemsNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiMedlemsNr C-Win
ON LEAVE OF fiMedlemsNr IN FRAME DEFAULT-FRAME /* Medlemsnr */
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTilKjopsDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTilKjopsDato C-Win
ON LEAVE OF fiTilKjopsDato IN FRAME DEFAULT-FRAME
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Fornavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fornavn C-Win
ON LEAVE OF Fornavn IN FRAME DEFAULT-FRAME /* Fornavn */
DO:
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
  IF VALID-HANDLE(hwbyggrabsjekk) THEN
    DELETE PROCEDURE hwbyggrabsjekk.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE byggMedlemskjopRecord C-Win 
PROCEDURE byggMedlemskjopRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN wbyggmedlemskjop.w PERSISTENT SET hwbyggrabsjekk.
  IF VALID-HANDLE(hwbyggrabsjekk) THEN RUN setParentHandle IN hwbyggrabsjekk (THIS-PROCEDURE:HANDLE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearFilterRecord C-Win 
PROCEDURE clearFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          fiMedlemsNr:SCREEN-VALUE      = ''
          fiButikkNr:SCREEN-VALUE       = ''
          fiFraKjopsDato:SCREEN-VALUE   = ?
          fiTilKjopsDato:SCREEN-VALUE   = ?
          fiFraSaldo:SCREEN-VALUE       = ''
          fiTilSaldo:SCREEN-VALUE       = ''
          Fornavn:SCREEN-VALUE          = ''
          Etternavn:SCREEN-VALUE        = ''
          .
      DYNAMIC-FUNCTION("setSortString",hBrowse,"").
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
    IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
      RUN visBongRecord.

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
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"medkjop_delete.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"medkjop_delete.p",'').
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
    /*
    ASSIGN 
        RabSjekkTypeNr:SENSITIVE = FALSE.
    */
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
  DISPLAY Fornavn fiFraKjopsDato fiTilKjopsDato fiMedlemsNr Etternavn fiFraSaldo 
          fiTilSaldo fiButikkNr MedlemsNr ButikkNr KjopsBelop TildeltBelop 
          KjopsDato Notat 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectBrowse rectToolBar rectWinToolbar RECT-64 searchField Fornavn 
         fiFraKjopsDato fiTilKjopsDato fiMedlemsNr Etternavn fiFraSaldo 
         fiTilSaldo fiButikkNr B-sokButik-2 BUTTON-1 MedlemsNr ButikkNr 
         KjopsBelop TildeltBelop KjopsDato Notat btnCalFraDato-6 
         btnCalFraDato-10 B-sokMedlem btnCalFraDato B-sokButik B-sokMedlem2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getRecord C-Win 
PROCEDURE getRecord :
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
                    "MedKjop"
                  + ";ButikkNr"
                  + ";MedlemsNr|Medlemsnr."
                  + ";!+medlemNavn|character|x(30)|medlemNavn(ROWID)|Medlemmets navn"
                  + ";+UtstedtTid|character|x(6)|TidUtstedt(ROWID)|Kl"
                  + ";KjopsDato"
                  + ";!KjopsTid"
                  + ";+medkjopsTid|character|x(6)|medkjopsTid(ROWID)|Kl"
                  + ";KjopsBelop"
                  + ";KjopsGrunnlag"
                  + ";TildeltBelop"
                  + ";Saldo"
                  + ";Notat"
                  + ";EDato"
                  + ";!ETid"
                  + ";+endretTid|character|x(6)|endretTid(ROWID)|Kl"
                  + ";BrukerID"
                  + ";RegistrertDato"
                  + ";!RegistrertTid"
                  + ";+rTid|character|x(6)|rTid(ROWID)|Kl"
                  + ";RegistrertAv"
                  + ";!B_id"
                  + ",Medlem"
                    +   ";Fornavn|Fornavn|x(20)@3"
                    +   ";Etternavn|Etternavn|x(20)@4"
                    ,
                    "WHERE FALSE" 
                  +  ",FIRST Medlem NO-LOCK OF MedKjop"
                    ,"").                    
  hBrowse:NAME = "brwMedKjop". 
  
  /* Henter buffer handle for å kunne lese ut verdier fra feltene i bufferet. */
  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",    
                                                 
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,  
                    "MedlemsNr,ButikkNr,KjopsBelop,TildeltBelop,KjopsDato,Notat",              
                    "",              
                    "","",                       
                    "").   

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).

  DYNAMIC-FUNCTION('setAttribute',hBrowse,'calcfieldproc','medkjop_brwcalc.p').


IF CAN-DO("1",cAdgang) THEN
      hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                        rectToolBar:HANDLE,            
                        "Fil",                         
                        "undo;Angre,save;Lagre,Refresh,excel;Eksporter til E&xcel",
                        "maxborder").                   
  ELSE
      DYNAMIC-FUNCTION("NewToolBar",
                        rectToolBar:HANDLE,            
                        "Fil",                         
                        "undo;Angre,delete;Slett,save;Lagre,Refresh,BrowseConfig|;Kolonneoppsett,flatview,excel;Eksporter til E&xcel" +
                        ",visBong;Vis bong" + 
                        ",sum;Summer kjøp" + 
                        ",byggMedlemskjop;Poster medlemskjøp¤ENABLED",
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

  RUN InvokeMethod (hBrowse,'OpenQuery').
  APPLY "value-changed" TO hBrowse.
END.

DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar").
DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,
                 "MedlemsNr,B-sokMedlem,ButikkNr,b-sokButikk,KjopsBelop,TildeltBelop,KjopsDato,btnCalFraDato").

DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,950,950,0,250).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
APPLY "ENTRY" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE markerBruktRecord C-Win 
PROCEDURE markerBruktRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Markere rabattsjekker som brukt ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"medrabsjekk_markerBrukt.p",'BRUKT').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"medrabsjekk_markerBrukt.p",'BRUKT').
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE markerUBruktRecord C-Win 
PROCEDURE markerUBruktRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Markere rabattsjekker som ubrukt ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"medrabsjekk_markerBrukt.p",'UBRUKT').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"medrabsjekk_markerBrukt.p",'UBRUKT').
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN SUPER.

  /*
  ASSIGN
      RabSjekkTypeNr:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE
      RabSjekkTypeNr:SCREEN-VALUE = '1'
      DatoUtstedt:SCREEN-VALUE    = STRING(TODAY)
      ButikkNr:SCREEN-VALUE       = STRING(iCL)
      .


  APPLY "entry" TO RabSjekkTypeNr.
  */
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
    
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",'').

    cWhere = buildFilter(cWhere,fiMedlemsNr:HANDLE,'MedlemsNr','EQ').
    cWhere = cWhere + buildFilter(cWhere,fiButikkNr:HANDLE,'ButikkNr','EQ').
    cWhere = cWhere + buildFilter(cWhere,fiFraKjopsDato:HANDLE,'DatoGyldig','GE').
    cWhere = cWhere + buildFilter(cWhere,fiTilKjopsDato:HANDLE,'DatoGyldig','LE').
    cWhere = cWhere + buildFilter(cWhere,fiFraSaldo:HANDLE,'Saldo','GE').
    cWhere = cWhere + buildFilter(cWhere,fiTilSaldo:HANDLE,'Saldo','LE').
    
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cWhere).
    
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryFilter",
                     (IF Fornavn:SCREEN-VALUE NE "" OR Etternavn:SCREEN-VALUE NE "" THEN

                        (IF INDEX(Fornavn:SCREEN-VALUE,"*") > 0 THEN
                           "Medlem WHERE Fornavn MATCHES '" + Fornavn:SCREEN-VALUE + "'"
                         ELSE IF Fornavn:SCREEN-VALUE NE "" THEN
                           "Medlem WHERE Fornavn BEGINS '" + Fornavn:SCREEN-VALUE + "'"
                         ELSE "")

                       + (IF Etternavn:SCREEN-VALUE NE "" AND Fornavn:SCREEN-VALUE NE "" THEN " AND " ELSE "")

                       + (IF Etternavn:SCREEN-VALUE NE "" AND Fornavn:SCREEN-VALUE EQ "" THEN
                           (IF INDEX(Etternavn:SCREEN-VALUE,"*") > 0 THEN
                              "Medlem WHERE Etternavn MATCHES '" + Etternavn:SCREEN-VALUE + "'"
                            ELSE IF Etternavn:SCREEN-VALUE NE "" THEN
                              "Medlem WHERE Etternavn BEGINS '" + Etternavn:SCREEN-VALUE + "'"
                            ELSE "")
                          ELSE "")
                       
                       + (IF Etternavn:SCREEN-VALUE NE "" AND Fornavn:SCREEN-VALUE NE "" THEN
                           (IF INDEX(Etternavn:SCREEN-VALUE,"*") > 0 THEN
                              " Etternavn MATCHES '" + Etternavn:SCREEN-VALUE + "'"
                            ELSE IF Etternavn:SCREEN-VALUE NE "" THEN
                              " Etternavn BEGINS '" + Etternavn:SCREEN-VALUE + "'"
                            ELSE "")
                          ELSE "")

                        + ",EACH MedKjop OF Medlem NO-LOCK"
                      ELSE "")
                     ).

    ASSIGN
        fiMedlemsNr:MODIFIED    = FALSE 
        fiButikkNr:MODIFIED     = FALSE
        fiFraSaldo:MODIFIED     = FALSE
        fiTilSaldo:MODIFIED     = FALSE
        fiFraKjopsDato:MODIFIED = FALSE
        fiTilKjopsDato:MODIFIED = FALSE
    .
  END.  

  RUN SUPER.
END PROCEDURE.

/*
                       + (IF Etternavn:SCREEN-VALUE NE "" THEN
                           (IF INDEX(Etternavn:SCREEN-VALUE,"*") > 0 THEN
                              "Medlem WHERE Etternavn MATCHES '" + Etternavn:SCREEN-VALUE + "'"
                            ELSE IF Etternavn:SCREEN-VALUE NE "" THEN
                              "Medlem WHERE Etternavn BEGINS '" + Etternavn:SCREEN-VALUE + "'"
                            ELSE "")
                          ELSE "")
*/

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
        
        IF ButikkNr:SCREEN-VALUE = "0" THEN DO:
            MESSAGE "Butikk ikke angitt: '0'"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "ENTRY" TO ButikkNr.
            RETURN.
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
        IF MedlemsNr:SCREEN-VALUE = "0" THEN DO:
            MESSAGE "Medlem ikke angitt: '0'"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "ENTRY" TO MedlemsNr.
            RETURN.
        END.
        IF DEC(MedlemsNr:SCREEN-VALUE) > 0 THEN
        DO:
            cTekst = DYNAMIC-FUNCTION("getFieldValues","Medlem",
                                       "WHERE MedlemsNr = '" + MedlemsNr:SCREEN-VALUE + "'","MedlemsNr").  
            IF cTekst = ? THEN
            DO:
                MESSAGE 'Ugyldig medlemsnr:' MedlemsNr:SCREEN-VALUE SKIP
                    'Resultat: ' cTekst
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN.
            END.
        END.
        IF DEC(KjopsBelop:SCREEN-VALUE) = 0 THEN DO:
            MESSAGE "Beløp ikke angitt: '0'"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "ENTRY" TO KjopsBelop.
            RETURN.
        END.
    END.
    RUN SUPER.
    APPLY "ENTRY" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sumRecord C-Win 
PROCEDURE sumRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Summere kjop ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

  IF iReturn = 1 THEN
    bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"medkjop_sum.p",'').
  ELSE IF iReturn = 2 THEN
  DO:
    DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
      IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
        cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
    END.
    bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"medkjop_sum.p",'').
  END.
  ELSE LEAVE.

  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Sum kjøp ","").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visBongRecord C-Win 
PROCEDURE visBongRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN    */
  IF NOT hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
        RETURN NO-APPLY.
  ELSE DO:
    cTekst = DYNAMIC-FUNCTION("getFieldValues",
                              "BongHode",
                              "WHERE B_Id = '" + STRING(hBuffer:BUFFER-FIELD("B_Id"):BUFFER-VALUE) + "'","ButikkNr,KasseNr,Dato,BongNr").

    IF cTekst <> ? AND NUM-ENTRIES(cTekst,'|') >= 4 THEN
        RUN gviskvittokopi2.w (INT(ENTRY(1,cTekst,'|')),1,INT(ENTRY(2,cTekst,'|')),DATE(ENTRY(3,cTekst,'|')),INT(ENTRY(4,cTekst,'|')),THIS-PROCEDURE).
  END.
  RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

