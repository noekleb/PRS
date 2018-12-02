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
&Scoped-Define ENABLED-OBJECTS B-sokButik-2 rectBrowse rectToolBar ~
rectWinToolbar RECT-64 searchField fiTGStore_Id fiFraSalgsDato ~
fiTilSalgsDato fiFraExportDate fiTilExportDate fiFraRegDato fiTilRegDato ~
fiTGRemark fiFraEDato fiTilEDato BUTTON-1 TGStore_Id TGRemark TGNote ~
btnCalFraDato-9 btnCalFraSalgsDato btnCalFraDato-6 btnCalFraDato-10 ~
btnCalFraDato-11 btnCalFraDato-7 btnCalFraDato-12 btnCalFraDato-8 ~
B-sokButik 
&Scoped-Define DISPLAYED-OBJECTS fiTGStore_Id fiFraSalgsDato fiTilSalgsDato ~
fiFraExportDate fiTilExportDate fiFraRegDato fiTilRegDato fiTGRemark ~
fiFraEDato fiTilEDato TGExportId TGStore_Id TGRemark TGNote 

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

DEFINE BUTTON btnCalFraDato-10 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraDato-11 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraDato-12 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraDato-6 
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

DEFINE BUTTON btnCalFraDato-9 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraSalgsDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON BUTTON-1 
     LABEL "Blank filter" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE TGNote AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 59.2 BY 18.81 NO-UNDO.

DEFINE VARIABLE fiFraEDato AS DATE FORMAT "99/99/99" 
     LABEL "Fra/til endret dato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Fra utstedelsesdato for rabattsjekk.".

DEFINE VARIABLE fiFraExportDate AS DATE FORMAT "99/99/99" 
     LABEL "Fra/til eksport dato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE fiFraRegDato AS DATE FORMAT "99/99/99" 
     LABEL "Fra/til registrert dato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Utstedelsesdato for rabattsjekk.".

DEFINE VARIABLE fiFraSalgsDato AS DATE FORMAT "99/99/99" 
     LABEL "Fra/til salgs dato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE fiTGRemark AS CHARACTER FORMAT "X(256)":U 
     LABEL "Merknad" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 NO-UNDO.

DEFINE VARIABLE fiTGStore_Id AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikknr" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE fiTilEDato AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Utstedelsesdato for rabattsjekk.".

DEFINE VARIABLE fiTilExportDate AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE fiTilRegDato AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Utstedelsesdato for rabattsjekk.".

DEFINE VARIABLE fiTilSalgsDato AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE TGExportId AS DECIMAL FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Eksport id" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE TGRemark AS CHARACTER FORMAT "X(256)" 
     LABEL "Merknad" 
     VIEW-AS FILL-IN 
     SIZE 40.2 BY 1 TOOLTIP "Merknad".

DEFINE VARIABLE TGStore_Id AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikknr" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-sokButik-2 AT ROW 2.48 COL 44.8
     fiTGStore_Id AT ROW 2.43 COL 22.4 COLON-ALIGNED HELP
          "Nr. på butikk som har utstedtsjekken"
     fiFraSalgsDato AT ROW 2.43 COL 74 COLON-ALIGNED HELP
          "Siste gyldighetsdato på rabattsjekken"
     fiTilSalgsDato AT ROW 2.43 COL 96 COLON-ALIGNED HELP
          "Siste gyldighetsdato på rabattsjekken" NO-LABEL
     fiFraExportDate AT ROW 2.43 COL 142 COLON-ALIGNED HELP
          "Siste gyldighetsdato på rabattsjekken"
     fiTilExportDate AT ROW 2.43 COL 164 COLON-ALIGNED HELP
          "Siste gyldighetsdato på rabattsjekken" NO-LABEL
     fiFraRegDato AT ROW 3.38 COL 142 COLON-ALIGNED HELP
          "Dato da sjekken ble utsted"
     fiTilRegDato AT ROW 3.38 COL 164 COLON-ALIGNED HELP
          "Dato da sjekken ble utsted" NO-LABEL
     fiTGRemark AT ROW 3.43 COL 22.4 COLON-ALIGNED
     fiFraEDato AT ROW 4.38 COL 142 COLON-ALIGNED HELP
          "Dato da sjekken ble utsted"
     fiTilEDato AT ROW 4.38 COL 164 COLON-ALIGNED HELP
          "Dato da sjekken ble utsted" NO-LABEL
     BUTTON-1 AT ROW 5.19 COL 189
     TGExportId AT ROW 8.62 COL 159.8 COLON-ALIGNED HELP
          "Unikt ID for eksport av data for butikk"
     TGStore_Id AT ROW 9.67 COL 159.8 COLON-ALIGNED HELP
          "Nr. på butikk som har utstedtsjekken"
     TGRemark AT ROW 10.71 COL 159.8 COLON-ALIGNED HELP
          "Merknad"
     TGNote AT ROW 11.95 COL 143 NO-LABEL
     btnCalFraDato-9 AT ROW 2.43 COL 92
     btnCalFraSalgsDato AT ROW 2.43 COL 114
     btnCalFraDato-6 AT ROW 2.43 COL 160
     btnCalFraDato-10 AT ROW 2.43 COL 182
     btnCalFraDato-11 AT ROW 3.43 COL 182
     btnCalFraDato-7 AT ROW 3.43 COL 160
     btnCalFraDato-12 AT ROW 4.38 COL 182
     btnCalFraDato-8 AT ROW 4.38 COL 160
     B-sokButik AT ROW 9.67 COL 177.6
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
         TITLE              = "TimeGrip eksportregister"
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
/* SETTINGS FOR FILL-IN TGExportId IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       TGNote:RETURN-INSERTED IN FRAME DEFAULT-FRAME  = TRUE.

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
ON END-ERROR OF C-Win /* TimeGrip eksportregister */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* TimeGrip eksportregister */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* TimeGrip eksportregister */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-sokButik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-sokButik C-Win
ON CHOOSE OF B-sokButik IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF TGStore_Id
DO:
    cTekst = "Butik".                    
    RUN JBoxDLookup.w ("Butiker;Butik;ButNamn", "where Butiker.harButikksystem = true and Butiker.ApningsDato <> ? and Butiker.NedlagtDato = ?", INPUT-OUTPUT cTekst).

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    ELSE DO:
        ASSIGN
            TGStore_Id:SCREEN-VALUE = cTekst
            TGStore_Id:MODIFIED     = TRUE
            .
        APPLY "any-printable" TO TGStore_Id.
    END.
  APPLY "ENTRY" TO TGStore_Id.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-sokButik-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-sokButik-2 C-Win
ON CHOOSE OF B-sokButik-2 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiTGStore_Id
DO:
    cTekst = "Butik".                    
    RUN JBoxDLookup.w ("Butiker;Butik;ButNamn", "where Butiker.harButikksystem = true and Butiker.ApningsDato <> ? and Butiker.NedlagtDato = ?", INPUT-OUTPUT cTekst).

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    ELSE DO:
        ASSIGN
            fiTGStore_Id:SCREEN-VALUE = cTekst
            fiTGStore_Id:MODIFIED     = TRUE
            .
        APPLY "any-printable" TO fiTGStore_Id.
    END.
  APPLY "ENTRY" TO fiTGStore_Id.
  RUN InvokeMethod (hBrowse,'OpenQuery').

  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-10 C-Win
ON CHOOSE OF btnCalFraDato-10 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiTilExportDate
DO:
  RUN Cal.w (fiTilExportDate:HANDLE).
  RUN InvokeMethod (hBrowse,'OpenQuery').

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


&Scoped-define SELF-NAME btnCalFraDato-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-6 C-Win
ON CHOOSE OF btnCalFraDato-6 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiFraExportDate
DO:
  RUN Cal.w (fiFraExportDate:HANDLE).
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


&Scoped-define SELF-NAME btnCalFraDato-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-9 C-Win
ON CHOOSE OF btnCalFraDato-9 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiFraSalgsDato
DO:
  RUN Cal.w (fiFraSalgsDato:HANDLE).
  RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraSalgsDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraSalgsDato C-Win
ON CHOOSE OF btnCalFraSalgsDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiTilSalgsDato
DO:
  RUN Cal.w (fiTilSalgsDato:HANDLE).
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


&Scoped-define SELF-NAME fiFraEDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFraEDato C-Win
ON LEAVE OF fiFraEDato IN FRAME DEFAULT-FRAME /* Fra/til endret dato */
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFraExportDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFraExportDate C-Win
ON LEAVE OF fiFraExportDate IN FRAME DEFAULT-FRAME /* Fra/til eksport dato */
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


&Scoped-define SELF-NAME fiFraSalgsDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFraSalgsDato C-Win
ON LEAVE OF fiFraSalgsDato IN FRAME DEFAULT-FRAME /* Fra/til salgs dato */
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTGRemark
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTGRemark C-Win
ON LEAVE OF fiTGRemark IN FRAME DEFAULT-FRAME /* Merknad */
DO:
  RUN InvokeMethod (hBrowse,'OpenQuery').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTGStore_Id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTGStore_Id C-Win
ON LEAVE OF fiTGStore_Id IN FRAME DEFAULT-FRAME /* Butikknr */
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


&Scoped-define SELF-NAME fiTilExportDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTilExportDate C-Win
ON LEAVE OF fiTilExportDate IN FRAME DEFAULT-FRAME
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


&Scoped-define SELF-NAME fiTilSalgsDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTilSalgsDato C-Win
ON LEAVE OF fiTilSalgsDato IN FRAME DEFAULT-FRAME
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE byggExportRecord C-Win 
PROCEDURE byggExportRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN wbyggtimegripeksport.w PERSISTENT SET hwbyggrabsjekk.
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
          fiTGStore_Id:SCREEN-VALUE    = ''
          fiTGRemark:SCREEN-VALUE      = ''
          fiFraExportDate:SCREEN-VALUE = ?
          fiTilExportDate:SCREEN-VALUE = ?
          fiFraSalgsDato:SCREEN-VALUE  = ?
          fiTilSalgsDato:SCREEN-VALUE  = ?
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
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"tgexport_delete.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"tgexport_delete.p",'').
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
        TGExportId:SENSITIVE = FALSE.
END.

/*
/* Flagger at purretrinnet er i bruk på en eller flere kundereskontroposter. */
IF hFieldMap:BUFFER-FIELD("bIBruk"):BUFFER-VALUE <> "" THEN
    BROWSE rectBrowse:HANDLE hFieldMap:BUFFER-FIELD("Purretrinn"):BGCOLOR = 11.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE eksporterRecord C-Win 
PROCEDURE eksporterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Eksportere linje(r) ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"tgexport_eksporter.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"tgexport_eksporter.p",'').
END.
ELSE
  LEAVE.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). 
ELSE DO: 
    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")). 
    DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
    RUN DisplayRecord.
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
  DISPLAY fiTGStore_Id fiFraSalgsDato fiTilSalgsDato fiFraExportDate 
          fiTilExportDate fiFraRegDato fiTilRegDato fiTGRemark fiFraEDato 
          fiTilEDato TGExportId TGStore_Id TGRemark TGNote 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-sokButik-2 rectBrowse rectToolBar rectWinToolbar RECT-64 searchField 
         fiTGStore_Id fiFraSalgsDato fiTilSalgsDato fiFraExportDate 
         fiTilExportDate fiFraRegDato fiTilRegDato fiTGRemark fiFraEDato 
         fiTilEDato BUTTON-1 TGStore_Id TGRemark TGNote btnCalFraDato-9 
         btnCalFraSalgsDato btnCalFraDato-6 btnCalFraDato-10 btnCalFraDato-11 
         btnCalFraDato-7 btnCalFraDato-12 btnCalFraDato-8 B-sokButik 
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
                    "TGExport"
                  + ";TGExportId"
                  + ";TGStore_Id|Butikknr"
                  + ";TGRemark|Merknad"
                  + ";TGSalesDate|Salgsdato"
                  + ";TGExportDate|Ekportert"
                  + ";+EksportertTid|character|x(6)|EksportertTid(ROWID)|Kl"
                  + ";RegistrertDato|Opprettet"
                  + ";+RegistrertTid|character|x(6)|RegistrertTid(ROWID)|Kl"
                  + ";RegistrertAv"
                  + ";EDato|Endret"
                  + ";+EndretTid|character|x(6)|EndretTid(ROWID)|Kl"
                  + ";BrukerId"
                  + ";TGNote"
                    ,
                    "WHERE FALSE"
                   ,"SORT|TGExportId").                    
  hBrowse:NAME = "brwTGExport". 
  
  /* Henter buffer handle for å kunne lese ut verdier fra feltene i bufferet. */
  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",    
                                                 
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,  
                    "TGExportId,TGStore_Id,TGRemark,TGNote",              
                    "",              
                    "","",                       
                    "").            
  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).

  DYNAMIC-FUNCTION('setAttribute',hBrowse,'calcfieldproc','tgexport_brwcalc.p').


  
  DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,            
                    "Fil",                         
                    "undo;Angre,delete;Slett,save;Lagre,Refresh,BrowseConfig;Kolonneoppsett,flatview,Print;S&kriv,excel;Eksporter til E&xcel" +
                    ",byggExport;Gen. eksport datasett¤ENABLED" +
                    ",Eksporter;Eksporter datasett", 
                    
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
                 "RabSjekkTypeNr,B-sokRabSjekkTypeN,MedlemsNr,B-sokMedlem,ButikkNr,b-sokButikk,Belop,RabSjekkSerienr,DatoGyldig,DatoUtstedt,DatoSkrevet,DatoBrukt,BruktButikkNr,Notat").

DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,950,950,0,250).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
APPLY "ENTRY" TO hBrowse.

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
    
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",'').

    cWhere = buildFilter(cWhere,fiTGStore_Id:HANDLE,'TGStore_Id','EQ').
    cWhere = cWhere + buildFilter(cWhere,fiTGRemark:HANDLE,'TGRemark','BEGINS').
    cWhere = cWhere + buildFilter(cWhere,fiFraExportDate:HANDLE,'TGExportDate','GE').
    cWhere = cWhere + buildFilter(cWhere,fiTilExportDate:HANDLE,'TGExportDate','LE').
    cWhere = cWhere + buildFilter(cWhere,fiFraSalgsDato:HANDLE,'TGSalesDate','GE').
    cWhere = cWhere + buildFilter(cWhere,fiTilSalgsDato:HANDLE,'TGSalesDate','LE').
    cWhere = cWhere + buildFilter(cWhere,fiFraRegDato:HANDLE,'RegistrertDato','GE').
    cWhere = cWhere + buildFilter(cWhere,fiTilRegDato:HANDLE,'RegistrertDato','LE').
    cWhere = cWhere + buildFilter(cWhere,fiFraEDato:HANDLE,'EDato','GE').
    cWhere = cWhere + buildFilter(cWhere,fiTilEDato:HANDLE,'EDato','LE').
    
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cWhere).
    
    ASSIGN
        fiTGStore_Id:MODIFIED    = FALSE 
        fiTGRemark:MODIFIED      = FALSE 
        fiFraExportDate:MODIFIED = FALSE
        fiTilExportDate:MODIFIED = FALSE
        fiFraSalgsDato:MODIFIED  = FALSE
        fiTilSalgsDato:MODIFIED  = FALSE
        fiFraRegDato:MODIFIED    = FALSE
        fiTilRegDato:MODIFIED    = FALSE
        fiFraEDato:MODIFIED      = FALSE
    .
  END.  

  RUN SUPER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord C-Win 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Skrive ut markerte rabattsjekker?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"medrabsjekk_utskrift.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"medrabsjekk_utskrift.p",'').
END.
ELSE
  LEAVE.

IF bOK THEN DO:
    ocValue = DYNAMIC-FUNCTION("getTransactionMessage"). 
    IF SEARCH(ocValue) <> ? THEN
        RUN browse2pdf\viewxmldialog.w (ocValue,"RABATTSJEKK").
    ELSE
        MESSAGE 'Finner ikke PDF filen: ' ocValue
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
END.

/*RUN InvokeMethod (hBrowse,'OpenQuery').*/
IF iReturn = 1 
    THEN RUN InvokeMethod (hBrowse,'OpenQuery'). 
ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).

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
        
        IF DEC(TGStore_Id:SCREEN-VALUE) > 0 THEN
        DO:
            cTekst = DYNAMIC-FUNCTION("getFieldValues","Butiker",
                                       "WHERE Butik = '" + TGStore_Id:SCREEN-VALUE + "'","Butik").  
            IF cTekst = ? THEN
            DO:
                MESSAGE 'Ugyldig butikk:' TGStore_Id:SCREEN-VALUE SKIP
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

