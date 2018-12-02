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

DEF VAR bOK                  AS LOG NO-UNDO.
DEF VAR ix                   AS INT NO-UNDO.

DEF VAR hToolbar             AS HANDLE NO-UNDO.
DEF VAR hUpdToolbar          AS HANDLE NO-UNDO.
DEF VAR hBrowseListe         AS HANDLE NO-UNDO.
DEF VAR hBufferListe         AS HANDLE NO-UNDO.
DEF VAR hFieldMap            AS HANDLE NO-UNDO.
DEF VAR hBrowseButikktilgang AS HANDLE NO-UNDO.
DEF VAR hBufferButikktilgang AS HANDLE NO-UNDO.
DEF VAR hBrowseProgram       AS HANDLE NO-UNDO.
DEF VAR hBufferProgram       AS HANDLE NO-UNDO.

DEF VAR cAdgang              AS CHAR   NO-UNDO.
DEF VAR hIndDet              AS HANDLE NO-UNDO.
DEF VAR hTLdet               AS HANDLE NO-UNDO.

DEF VAR iTab                 AS INT    NO-UNDO.

DEF VAR cState               AS CHAR   NO-UNDO.

DEF VAR cButikerRowIdList    AS CHAR   NO-UNDO.
DEF VAR cButikerIdList       AS CHAR   NO-UNDO.
DEF VAR cMenyRowIdList       AS CHAR   NO-UNDO.
DEF VAR cMenyIdList          AS CHAR   NO-UNDO.
DEF VAR cProgramRowIdList    AS CHAR   NO-UNDO.
DEF VAR cProgramIdList       AS CHAR   NO-UNDO.
DEF VAR cStripeRowIdList     AS CHAR   NO-UNDO.
DEF VAR cStripeIdList        AS CHAR   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectToolBar rectUpdToolbar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBrowseHandle C-Win 
FUNCTION getBrowseHandle RETURNS HANDLE
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetTabStrip C-Win 
FUNCTION SetTabStrip RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TabStripChanged C-Win 
FUNCTION TabStripChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE TabStrip AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chTabStrip AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 7.6 BY 1.19.

DEFINE RECTANGLE rectUpdToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 13 BY 1.19.

DEFINE BUTTON B-Meny 
     LABEL "Velg MODUL meny..." 
     SIZE 28 BY 1.14.

DEFINE BUTTON B-Menystripe 
     LABEL "Velg STRIPE meny..." 
     SIZE 50 BY 1.14.

DEFINE BUTTON B-Program 
     LABEL "Velg programtilgang..." 
     SIZE 52 BY 1.14.

DEFINE BUTTON B-Tilgang 
     LABEL "Velg tilgjengelige butikker..." 
     SIZE 29 BY 1.14.

DEFINE BUTTON BUTTON-Team 
     LABEL "&Velg team..." 
     SIZE 28 BY 1.14.

DEFINE VARIABLE Beskrivelse AS CHARACTER FORMAT "X(40)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1.

DEFINE VARIABLE BrGrpNr AS INTEGER FORMAT "zz9" INITIAL 0 
     LABEL "GruppeNr" 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE VARIABLE KnappeListe AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1.

DEFINE VARIABLE Navn AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1.

DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 2.62.

DEFINE RECTANGLE RECT-63
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 2.62.

DEFINE RECTANGLE rectBrowseButikktilgang
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 29 BY 5.48.

DEFINE RECTANGLE rectBrowseProgram
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 5.48.

DEFINE RECTANGLE rectBrowseListe
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80 BY 10.95.

DEFINE RECTANGLE RectBrowseSearchListe
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY 1.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectToolBar AT ROW 1.14 COL 75.2
     rectUpdToolbar AT ROW 1.14 COL 1.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 83.4 BY 15.38.

DEFINE FRAME frmDetalj
     BrGrpNr AT ROW 1.48 COL 12 COLON-ALIGNED HELP
          "Brukergruppenummer."
     BUTTON-Team AT ROW 2.43 COL 53
     Beskrivelse AT ROW 2.48 COL 12 COLON-ALIGNED HELP
          "Kort beskrivelse/navn på brukergruppen."
     B-Program AT ROW 4.05 COL 1
     B-Tilgang AT ROW 4.05 COL 53
     B-Menystripe AT ROW 11 COL 2
     B-Meny AT ROW 11 COL 53
     Navn AT ROW 12.14 COL 2 HELP
          "Navn på meny som brukere i denne brukergruppe skal ha." NO-LABEL
     KnappeListe AT ROW 12.14 COL 53 NO-LABEL
     RECT-62 AT ROW 1.24 COL 1
     rectBrowseButikktilgang AT ROW 5.33 COL 53
     RECT-63 AT ROW 10.76 COL 1
     rectBrowseProgram AT ROW 5.33 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.62
         SIZE 81 BY 12.38.

DEFINE FRAME frmListe
     rectBrowseListe AT ROW 2.43 COL 1
     RectBrowseSearchListe AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.62
         SIZE 81 BY 12.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window Template
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Butikkgruppe"
         HEIGHT             = 15.33
         WIDTH              = 83.4
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
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME frmDetalj:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME frmListe:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 15.38
       FRAME DEFAULT-FRAME:WIDTH            = 83.4.

/* SETTINGS FOR FRAME frmDetalj
                                                                        */
/* SETTINGS FOR FILL-IN BrGrpNr IN FRAME frmDetalj
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN KnappeListe IN FRAME frmDetalj
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN Navn IN FRAME frmDetalj
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME frmListe
                                                                        */
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

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME TabStrip ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.43
       COLUMN          = 1
       HEIGHT          = 13.81
       WIDTH           = 83
       HIDDEN          = no
       SENSITIVE       = yes.
/* TabStrip OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Butikkgruppe */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Butikkgruppe */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Butikkgruppe */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmDetalj
&Scoped-define SELF-NAME B-Meny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Meny C-Win
ON CHOOSE OF B-Meny IN FRAME frmDetalj /* Velg MODUL meny... */
DO:
    /* Henter liste med koblinger */
    cMenyRowIdList = DYNAMIC-FUNCTION("getRowIdList","Meny","",
                                         "WHERE can-do('" + KnappeListe:SCREEN-VALUE + "',Meny.Navn)").

    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxSelector.w (THIS-PROCEDURE,0,
                        "Meny;Navn",
                        "where true",
                        INPUT-OUTPUT cMenyRowIdList,
                        "Navn",
                        INPUT-OUTPUT cMenyIdList,
                        "","",
                        OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
    DO WHILE INDEX(cMenyIdList,"|") > 0:
        OVERLAY (cMenyIdList , INDEX(cMenyIdList,"|"))  = ",".
    END.
    IF KnappeListe:SCREEN-VALUE <> cMenyIdList THEN
    DO:
        assign
            KnappeListe:SCREEN-VALUE = cMenyIdList
            KnappeListe = cMenyIdList
            .
        APPLY "ANY-PRINTABLE":U TO KnappeListe.
    END.

    /* runProc,<4Gl procedurere>,<Input parametre>,<Handle til temp-tabell - ? hvis ikke brukt.>.                                       */
    /* Returverdi fra prodeduren kan hentes med funksjonen dynamic-function("getTransactionMessage") - gir returverdi = parameterverdi. */
    /*
    bOk = DYNAMIC-FUNCTION("runProc","lagrebutikktilgang.p",BrGrpNr:SCREEN-VALUE + "|" + cMenyRowIdList,?).
    IF bOk THEN
        APPLY "value-changed" TO hBrowseListe.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Menystripe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Menystripe C-Win
ON CHOOSE OF B-Menystripe IN FRAME frmDetalj /* Velg STRIPE meny... */
DO:
    /* Henter liste med koblinger */
    cStripeRowIdList = DYNAMIC-FUNCTION("getRowIdList","Meny","",
                                         "WHERE can-do('" + Navn:SCREEN-VALUE + "',Meny.Navn)").

    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxSelector.w (THIS-PROCEDURE,0,
                        "Meny;!Mdata;Navn",
                        "where true",
                        INPUT-OUTPUT cStripeRowIdList,
                        "Navn",
                        INPUT-OUTPUT cStripeIdList,
                        "","",
                        OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
    DO WHILE INDEX(cStripeIdList,"|") > 0:
        OVERLAY (cStripeIdList , INDEX(cStripeIdList,"|"))  = ",".
    END.
    IF Navn:SCREEN-VALUE <> cStripeIdList THEN
    DO:
        ASSIGN
            Navn:SCREEN-VALUE = cStripeIdList
            Navn = cStripeIdList.
        APPLY "ANY-PRINTABLE":U TO Navn.
    END.
    /* runProc,<4Gl procedurere>,<Input parametre>,<Handle til temp-tabell - ? hvis ikke brukt.>.                                       */
    /* Returverdi fra prodeduren kan hentes med funksjonen dynamic-function("getTransactionMessage") - gir returverdi = parameterverdi. */
    /*
    bOk = DYNAMIC-FUNCTION("runProc","lagrebutikktilgang.p",BrGrpNr:SCREEN-VALUE + "|" + cStripeRowIdList,?).
    IF bOk THEN
        APPLY "value-changed" TO hBrowseListe.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Program
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Program C-Win
ON CHOOSE OF B-Program IN FRAME frmDetalj /* Velg programtilgang... */
DO:
    /* Henter liste med koblinger */
    cProgramRowIdList = DYNAMIC-FUNCTION("getRowIdList","ProgBrGrp,ProgramListe","",
                                         "WHERE BrGrpNr = " + BrGrpNr:SCREEN-VALUE IN FRAME frmDetalj +
                                         ",FIRST ProgramListe OF ProgBrGrp NO-LOCK").

    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxSelector.w (THIS-PROCEDURE,0,
                        "ProgramListe;ProgNavn;Programbeskrivelse",
                        "where true",
                        INPUT-OUTPUT cProgramRowIdList,
                        "ProgNavn",
                        INPUT-OUTPUT cProgramIdList,
                        "","",
                        OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

    /* runProc,<4Gl procedurere>,<Input parametre>,<Handle til temp-tabell - ? hvis ikke brukt.>.                                       */
    /* Returverdi fra prodeduren kan hentes med funksjonen dynamic-function("getTransactionMessage") - gir returverdi = parameterverdi. */
    bOk = DYNAMIC-FUNCTION("runProc","lagreprogramtilgang.p",BrGrpNr:SCREEN-VALUE + "|" + cProgramRowIdList,?).
    IF bOk THEN
        APPLY "value-changed" TO hBrowseListe.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Tilgang
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Tilgang C-Win
ON CHOOSE OF B-Tilgang IN FRAME frmDetalj /* Velg tilgjengelige butikker... */
DO:
    /* Henter liste med koblinger */
    cButikerRowIdList = DYNAMIC-FUNCTION("getRowIdList","ButikkTilgang,Butiker","",
                                         "WHERE BrGrpNr = " + BrGrpNr:SCREEN-VALUE IN FRAME frmDetalj +
                                         ",FIRST Butiker OF ButikkTilgang NO-LOCK").

    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxSelector.w (THIS-PROCEDURE,0,
                        "Butiker;Butik;ButNamn",
                        "where true",
                        INPUT-OUTPUT cButikerRowIdList,
                        "Butik",
                        INPUT-OUTPUT cButikerIdList,
                        "","",
                        OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

    /* runProc,<4Gl procedurere>,<Input parametre>,<Handle til temp-tabell - ? hvis ikke brukt.>.                                       */
    /* Returverdi fra prodeduren kan hentes med funksjonen dynamic-function("getTransactionMessage") - gir returverdi = parameterverdi. */
    bOk = DYNAMIC-FUNCTION("runProc","lagrebutikktilgang.p",BrGrpNr:SCREEN-VALUE + "|" + cButikerRowIdList,?).
    IF bOk THEN
        APPLY "value-changed" TO hBrowseListe.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Team
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Team C-Win
ON CHOOSE OF BUTTON-Team IN FRAME frmDetalj /* Velg team... */
DO:
  run d-bbutikkteam.w (BrGrpNr:SCREEN-VALUE IN FRAME frmDetalj,?,"V").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME TabStrip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TabStrip C-Win OCX.MouseUp
PROCEDURE TabStrip.TabStrip.MouseUp .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Button
    Shift
    x
    y
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER p-Button AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER p-Shift  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER p-x      AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER p-y      AS INTEGER NO-UNDO.

  ASSIGN
      cState = DYNAMIC-FUNCTION('getToolbarState',hUpdToolBar)
      .
  IF CAN-DO('new,modified',cstate) THEN
  DO:
      MESSAGE "Du må lagre eller avbryte før tab kan byttes."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ASSIGN
          iTab = 2 + 10
          .
      TabStripChanged(iTab).
      RETURN NO-APPLY.
  END.

  TabStripChanged(INT(COM-SELF:SelectedItem:Index)).

END PROCEDURE.

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
  IF VALID-HANDLE(hIndDet) THEN APPLY "close" TO hIndDet.
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

ON 'default-action':U OF hBrowseListe
DO:
    ASSIGN
        iTab = 2 + 10
        .
    dynamic-function('TabStripChanged',iTab).

    DO WITH FRAME frmDetalj:
        APPLY "Entry" TO BrGrpNr.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "brukergrp.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chTabStrip = TabStrip:COM-HANDLE
    UIB_S = chTabStrip:LoadControls( OCXFile, "TabStrip":U)
    TabStrip:NAME = "TabStrip":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "brukergrp.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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

  bOk = DYNAMIC-FUNCTION("runProc","slettbutikktilgang.p",BrGrpNr:SCREEN-VALUE IN FRAME frmDetalj,?).
  IF bOk <> TRUE THEN
      RETURN NO-APPLY.

  RUN SUPER.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE editRecord C-Win 
PROCEDURE editRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  CASE iTab:
      WHEN 1 THEN APPLY 'default-action':U TO hBrowseListe.
  END CASE.
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
  RUN control_load.
  ENABLE rectToolBar rectUpdToolbar 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY BrGrpNr Beskrivelse Navn KnappeListe 
      WITH FRAME frmDetalj IN WINDOW C-Win.
  ENABLE RECT-62 rectBrowseButikktilgang RECT-63 rectBrowseProgram BUTTON-Team 
         Beskrivelse B-Program B-Tilgang B-Menystripe B-Meny 
      WITH FRAME frmDetalj IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmDetalj}
  ENABLE rectBrowseListe RectBrowseSearchListe 
      WITH FRAME frmListe IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmListe}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelRecord C-Win 
PROCEDURE ExcelRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  CASE iTab:
      WHEN 1 THEN DYNAMIC-FUNCTION("toExcelViaFile",hBrowseListe,0).         
      WHEN 2 THEN DYNAMIC-FUNCTION("toExcelViaFile",hBrowseListe,0).         
  END CASE.
  
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow C-Win 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>  
  Notes:       
              ;FeilVare|Reklam  
------------------------------------------------------------------------------*/
SetTabStrip().
TabStripChanged(11).

cAdgang = (DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 16 and SysGr = 35 and ParaNr = 1","Parameter1")).  


DO WITH FRAME frmListe:
  hBrowseListe = DYNAMIC-FUNCTION("NewBrowse",         /* Create a browse object */
                    rectBrowseListe:HANDLE IN FRAME frmListe,            /* Rectangle to define coordinates for browse */
                    200,                            /* Rows to batch */
                    "MULTIPLE,NUM-LOCKED-COLUMNS|1",                     /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "BrukerGrp;BrGrpNr;Beskrivelse;!Knappeliste;!Navn",
                    "WHERE true",
                    "sort|BrGrpNr").                            /* Misc - for something I might need in next version.. */
  hBrowseListe:NAME = "brwListe". /* This name is neccessary because the browser is due to special treatment during resize */
  hBufferListe = hBrowseListe:QUERY:GET-BUFFER-HANDLE(1).
  /*
  /* Flytter kolonner */
  hBrowseListe:MOVE-COLUMN(10,3).
  */
  /*
  /* Endring av labler på kolonner */
  hBrowseListe:GET-BROWSE-COLUMN(2):LABEL = "Type".
  */
  /* Endring av bredde på kollonner */
  /*
  hBrowseListe:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 160.
  */

  DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"querywhere","").
  /*DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"basequery","WHERE Individ.individType = 1 ").*/
  /*DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"sortmap","cRegTid;Tid").*/
/*   DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"sortmap","cSerieNrReg;SerieNrReg"). */
/*   DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"NoColumnSearch","cSerieNrReg").     */

END.

DO WITH FRAME frmDetalj:
  /*
  ASSIGN 
      SerieNrReg:DELIMITER     = "|"
      SerieNrReg:RADIO-BUTTONS = "1-Ingen spørsmål|1|2-Tvang på Serienr|2|3-Spørsmål om serienr|3"
      .
  */
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins)
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowseListe:QUERY,
                    FRAME frmDetalj:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                                                    /* Nb: Felt som brukes her må være hentet i Browser først.                        */
                    "BrGrpNr,Beskrivelse,Navn,KnappeListe"
                       ,       
                    "BrGrpNr,Beskrivelse,Navn,KnappeListe"
                       ,       
                    "", /* Additional buffer and displ.fields - not updateable*/
                    "",  
                    "").     /* Input fields other than in buffer */

  hBrowseButikktilgang = DYNAMIC-FUNCTION("NewBrowse",         /* Create a browse object */
                    rectBrowseButikktilgang:HANDLE IN FRAME frmDetalj,            /* Rectangle to define coordinates for browse */
                    200,                            /* Rows to batch */
                    "NUM-LOCKED-COLUMNS|1",                     /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    /*
                    "Gavekort;butnr;IdentType;Dato|Reg.dato;+cRegTid|CHARACTER|x(5)|gavekort_regtid.p|Tid;Gyldigdato;Bruktdato;KasseNr;BongNr;Belop;!Tid," + 
                      "GaveKType;GKTBeskrivelse",                     /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/
                    */
                    "ButikkTilgang;Butik,Butiker;KortNavn;ButNamn",
                    "WHERE false, first Butiker no-lock where Butiker.butik = ButikkTilgang.Butik",
/*                     "WHERE BrGrpNr = '" + BrGrpNr:SCREEN-VALUE IN FRAME frmDetalj + "', first Butiker no-lock where Butiker.butik = ButikkTilgang.Butik", */
                    "sort|Butik").                            /* Misc - for something I might need in next version.. */
  hBrowseButikktilgang:NAME = "brwTilgang". /* This name is neccessary because the browser is due to special treatment during resize */
  hBufferButikktilgang = hBrowseButikktilgang:QUERY:GET-BUFFER-HANDLE(1).
  DYNAMIC-FUNCTION("setAttribute",hBrowseButikktilgang,"querywhere","").


  hBrowseProgram = DYNAMIC-FUNCTION("NewBrowse",         /* Create a browse object */
                    rectBrowseProgram:HANDLE IN FRAME frmDetalj,            /* Rectangle to define coordinates for browse */
                    200,                            /* Rows to batch */
                    "NUM-LOCKED-COLUMNS|1",                     /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    /*
                    "Gavekort;butnr;IdentType;Dato|Reg.dato;+cRegTid|CHARACTER|x(5)|gavekort_regtid.p|Tid;Gyldigdato;Bruktdato;KasseNr;BongNr;Belop;!Tid," + 
                      "GaveKType;GKTBeskrivelse",                     /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/
                    */
                    "ProgBrGrp;!BrGrpNr,ProgramListe;ProgNavn;Programbeskrivelse",
                    "WHERE false, first ProgramListe no-lock where ProgramListe.ProgNavn = ProgBrGrp.ProgNavn",
/*                     "WHERE BrGrpNr = " + BrGrpNr:SCREEN-VALUE IN FRAME frmDetalj + ", first ProgramListe no-lock where ProgramListe.ProgNavn = ProgBrGrp.ProgNavn", */
                    "sort|ProgNavn").                            /* Misc - for something I might need in next version.. */
  hBrowseProgram:NAME = "brwProgram". /* This name is neccessary because the browser is due to special treatment during resize */
  hBufferProgram = hBrowseProgram:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION("setAttribute",hBrowseProgram,"querywhere","").

  /* Ignore = Ingen validering. Ellers skriv navn på program som gjør validering. */
  /* Dynamisk validering er default. Tileggsvalidering +<ProcNavn>.               */
  /* Kun egen validering, uten dynaisk validering =<ProcNavn>.                    */
  /* Dynamisk validering: Fremednøkkel - Felt må være indeksert og må finnes som  */
  /*                      primærnøkkel i en annen tabell.                         */
  /* DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore").   */
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore"). 
  /*DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraFields","Tid,BruktTid").*/
/*   DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore"). */

  /* Felter som skal være disablet. Comma separert. */
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"disabledFields","KnappeListe,Navn").


END.


DO WITH FRAME {&FRAME-NAME}:
    IF CAN-DO("1",cAdgang) THEN
        hUpdToolBar = DYNAMIC-FUNCTION("NewToolBar",
                      rectUpdToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                      "Fil",                          /* Corresponding menu label - no menu if blank */
                      "Undo;Angre,save;Lagre,excel;Eksporter til E&xcel,first;Første,prev;Forrige,next;Neste,last;Siste",
                                                      /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                         Any number of properties accepted (one ok - if predef. action) */
                      "maxborder").                   /* Misc - for something I might need in next version.. */
    ELSE
        hUpdToolBar = DYNAMIC-FUNCTION("NewToolBar",
                      rectUpdToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                      "Fil",                          /* Corresponding menu label - no menu if blank */
                      "New;Ny,Undo;Angre,delete;Slett,save;Lagre,excel;Eksporter til E&xcel,first;Første,prev;Forrige,next;Neste,last;Siste",
                                                      /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                         Any number of properties accepted (one ok - if predef. action) */
                      "maxborder").                   /* Misc - for something I might need in next version.. */


    hToolBar = DYNAMIC-FUNCTION("NewToolBar",
                      rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                      "Fil",                          /* Corresponding menu label - no menu if blank */
                      "Help,Close;Avslutt",
                                                      /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                         Any number of properties accepted (one ok - if predef. action) */
                      "").                            /* Misc - for something I might need in next version.. */
  
  DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearchListe:HANDLE,hBrowseListe,1).

  /* Link objects: */

  /*
  DYNAMIC-FUNCTION("LinkAllObjects",                /* Link all created objects. Linktype is type of "to" object,
                                                      f.ex link from browse to combo-box is combo-box link */
                    THIS-PROCEDURE:CURRENT-WINDOW,  /* Link only objects created for current window */
                    TRUE,                           /* Replace any existing links */
                    STRING(hToolBar)).  /* Except these objects */
  */
  /* Manuell linking når det er to objekter av samme type. */
  DYNAMIC-FUNCTION("createObjectLink",hBrowseListe,hUpdToolbar).
  DYNAMIC-FUNCTION("createObjectLink",hFieldMap,hUpdToolbar).
  DYNAMIC-FUNCTION("createObjectLink",hBrowseListe,hFieldMap).
  DYNAMIC-FUNCTION("createParentLink",hBrowseButikktilgang,hBrowseListe,'BrGrpNr').
  DYNAMIC-FUNCTION("createParentLink",hBrowseProgram,hBrowseListe,'BrGrpNr').

/*   DYNAMIC-FUNCTION("DumpAllLinks",THIS-PROCEDURE:CURRENT-WINDOW,"c:\temp\links.txt").  */

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectUpdToolBar").
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmDetalj:HANDLE, "rectBrowseButikktilgang,brwTilgang").
  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).

  DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  /*DYNAMIC-FUNCTION("SetToolbar",hUpdToolbar,"enable").*/
  DYNAMIC-FUNCTION("SetToolbar",hToolBar,"enable").

END.

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

APPLY "ENTRY" TO hBrowseListe.
APPLY "value-changed" TO hBrowseListe.

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
/* APPLY "entry" TO hBrowse.  */
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

  ASSIGN
      iTab = 2 + 10
      .
  dynamic-function('TabStripChanged',iTab).

  RUN SUPER.

  DO WITH FRAME frmDetalj:
      ASSIGN
          BrGrpNr:SENSITIVE = TRUE
          .
      APPLY "ENTRY" TO BrGrpNr.
      RETURN NO-APPLY.
  END.

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
  DEF VAR hHandle AS HANDLE NO-UNDO.

  ASSIGN
      hHandle = hBrowseListe
      .

  IF iTab = 1 THEN DO WITH FRAME frmListe:
      DYNAMIC-FUNCTION("SetCurrentObject",hBrowseListe).
  END.

  RUN SUPER.

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
    cState = DYNAMIC-FUNCTION('getToolbarState',hUpdToolBar)
    .

  DO WITH FRAME frmDetalj:


    IF cState = "NEW" THEN
    DO:
      IF (BrGrpNr:screen-value) = (DYNAMIC-FUNCTION("getFieldValues","BrukerGrp","WHERE BrGrpNr = '" + BrGrpNr:SCREEN-VALUE + "'","BrGrpNr")) THEN
      DO:
          MESSAGE "Bruker finnes fra før med type " + BrGrpNr:SCREEN-VALUE + "."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK  TITLE "Lagringskontroll".
          APPLY "ENTRY" TO BrGrpNr.
          RETURN NO-APPLY.
      END.
    END.

    ASSIGN
        BrGrpNr:SENSITIVE = FALSE
        .

  END.

  RUN SUPER.

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

  DO WITH FRAME frmDetalj:
      ASSIGN          
          BrGrpNr:SENSITIVE = FALSE
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBrowseHandle C-Win 
FUNCTION getBrowseHandle RETURNS HANDLE
  (  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hBrowseListe.
/*
IF iTab = 1 THEN
  RETURN hBrowseListe.
ELSE IF iTab = 2 THEN
  RETURN hBrowseListe.
*/  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetTabStrip C-Win 
FUNCTION SetTabStrip RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iy            AS INT NO-UNDO.
DEF VAR cTabStripList AS CHAR NO-UNDO.

/* cTabStripList = DYNAMIC-FUNCTION("getFieldList","JBoxGenCode;cCodeValue;cMisc1;cMisc2",               */
/*                                   "WHERE cCodeType = 'JBoxQueryTabs'" +                               */
/*                                   "  AND cLanguage = '" + DYNAMIC-FUNCTION("getLanguageCode") + "'" + */
/*                                   " BY cMisc1") NO-ERROR.                                             */

cTabStripList = "Liste|Detalj".

IF cTabStripList NE "" THEN DO:
  DO ix = 9 TO 1 BY -1:
    chTabStrip:TabStrip:Tabs:Remove(ix) NO-ERROR.
  END.
  DO ix = 1 TO NUM-ENTRIES(cTabStripList,"|"):
    iy = iy + 1.
    chTabStrip:TabStrip:Tabs:ADD(iy).
    chTabStrip:TabStrip:Tabs:Item(iy):Caption = ENTRY(ix,cTabStripList,"|").
  END.
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TabStripChanged C-Win 
FUNCTION TabStripChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFollowXbar   AS CHAR NO-UNDO.
ASSIGN
    cState = DYNAMIC-FUNCTION('getToolbarState',hUpdToolBar)
    .

DO WITH FRAME {&FRAME-NAME}:

  IF iiTab > 10 THEN DO:
    iiTab = iiTab - 10.
    chTabStrip:TabStrip:Tabs:ITEM(iiTab):SELECTED = TRUE.
  END.

  iTab = iiTab.

  IF iiTab = 1 THEN
  DO:
      FRAME frmListe:MOVE-TO-TOP().
  END.
  ELSE IF iiTab = 2 THEN
  DO:
      FRAME frmDetalj:MOVE-TO-TOP().
      IF CAN-DO('new',cstate) THEN
          BrGrpNr:SENSITIVE = TRUE.
      ELSE
          BrGrpNr:SENSITIVE = FALSE.
  END.
  /*
  APPLY "ENTRY":U TO fi-fArtikkelNr.
  */

  RETURN TRUE.   /* Function return value. */
END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

