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
DEF VAR hUpdToolbar     AS HANDLE NO-UNDO.
DEF VAR hBrowseListe    AS HANDLE NO-UNDO.
DEF VAR hBufferListe    AS HANDLE NO-UNDO.
DEF VAR hBrowseRad      AS HANDLE NO-UNDO.
DEF VAR hBufferRad      AS HANDLE NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.
DEF VAR hFieldMapRad    AS HANDLE NO-UNDO.
DEF VAR hWindow         AS HANDLE NO-UNDO.

DEF VAR fArtikkelNr     AS DEC NO-UNDO.
DEF VAR cAdgang         AS CHAR NO-UNDO.
DEF VAR hMixDet         AS HANDLE NO-UNDO.
DEF VAR hMixRad         AS HANDLE NO-UNDO.

DEF VAR iTab            AS INT NO-UNDO.

DEF VAR cState          AS CHAR  NO-UNDO.
DEF VAR cDummy          AS CHAR  NO-UNDO.

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
     SIZE 8.2 BY 1.19.

DEFINE RECTANGLE rectUpdToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 13 BY 1.19.

DEFINE BUTTON B-Stopp 
     LABEL "Stopp kampanje" 
     SIZE 18.2 BY 1.14.

DEFINE BUTTON btnFraDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnTilDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE MixMatchType AS INTEGER FORMAT ">9" INITIAL 1 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "0",1
     DROP-DOWN-LIST
     SIZE 64 BY 1 NO-UNDO.

DEFINE VARIABLE ProfilNr AS INTEGER FORMAT ">>>>>>9" INITIAL 1 
     LABEL "Prisprofil" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "0",1
     DROP-DOWN-LIST
     SIZE 64 BY 1 NO-UNDO.

DEFINE VARIABLE ED-Tekst AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 71 BY 5.95 NO-UNDO.

DEFINE VARIABLE Antall AS DECIMAL FORMAT ">,>>9" INITIAL 0 
     LABEL "Antall" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE FraDato AS DATE FORMAT "99/99/99" 
     LABEL "Fra dato" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE MixNr AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "Mixnr" 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1.

DEFINE VARIABLE MixTekst AS CHARACTER FORMAT "x(30)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 64 BY 1.

DEFINE VARIABLE TilDato AS DATE FORMAT "99/99/99" 
     LABEL "Til dato" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE Utpris AS DECIMAL FORMAT ">>,>>9.99" INITIAL 0 
     LABEL "Utpris" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 1.43.

DEFINE RECTANGLE RECT-63
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 23 BY 1.43.

DEFINE VARIABLE KlarTilAktivering AS LOGICAL INITIAL NO 
     LABEL "Klar til aktivering" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE MixAktiver AS LOGICAL INITIAL NO 
     LABEL "Aktivert i kasse" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.

DEFINE RECTANGLE rectBrowseListe
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 14.29.

DEFINE RECTANGLE RectBrowseSearchListe
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY 1.19.

DEFINE BUTTON B-SokArtikkel 
     IMAGE-UP FILE "icon/select.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Søk" 
     SIZE 4.6 BY 1 TOOLTIP "Starter Alt-S søkefunksjonen".

DEFINE VARIABLE Beskr AS CHARACTER FORMAT "x(250)" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE Kode AS DECIMAL FORMAT "zzzzzzzzzzzz9" INITIAL 0 
     LABEL "Artikkel" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 1.91.

DEFINE RECTANGLE rectBrowseRad
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 12.62.

DEFINE RECTANGLE RectBrowseSearchRad
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY 1.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectToolBar AT ROW 1.14 COL 87.4
     rectUpdToolbar AT ROW 1.14 COL 1.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 95.6 BY 19.

DEFINE FRAME frmRad
     B-SokArtikkel AT ROW 15.52 COL 33.2 NO-TAB-STOP 
     Kode AT ROW 15.52 COL 10 COLON-ALIGNED HELP
          "Strekkode inklusive sjekksiffer."
     Beskr AT ROW 15.52 COL 35.8 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen" NO-LABEL
     Antall AT ROW 15.52 COL 76 COLON-ALIGNED HELP
          "Totalt antall for denne MixMatch varianten"
          LABEL "Antall" FORMAT ">,>>9"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     rectBrowseRad AT ROW 2.43 COL 1
     RectBrowseSearchRad AT ROW 1 COL 1
     RECT-3 AT ROW 15.05 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.62
         SIZE 93 BY 15.95.

DEFINE FRAME frmDetalj
     btnFraDato AT ROW 6.52 COL 40 NO-TAB-STOP 
     B-Stopp AT ROW 1.19 COL 72
     MixAktiver AT ROW 1.33 COL 44 HELP
          "Aktvering av MixMatch kampanje"
     KlarTilAktivering AT ROW 1.38 COL 21 HELP
          "Kampanje ferdig registrert og klar til aktivering"
     MixNr AT ROW 2.52 COL 18 COLON-ALIGNED HELP
          "MixMatch nummer"
     MixMatchType AT ROW 3.52 COL 18 COLON-ALIGNED HELP
          "Bestemmer type av mixmatch."
     ProfilNr AT ROW 4.52 COL 18 COLON-ALIGNED HELP
          "Prisprofil"
     MixTekst AT ROW 5.52 COL 18 COLON-ALIGNED HELP
          "Tekst knyttet til MixMatch varianten"
     FraDato AT ROW 6.52 COL 18 COLON-ALIGNED
     TilDato AT ROW 7.52 COL 18 COLON-ALIGNED
     Utpris AT ROW 8.52 COL 18 COLON-ALIGNED HELP
          "Total utsalgspris for MixMatch varianten"
     Antall AT ROW 9.57 COL 18 COLON-ALIGNED HELP
          "Totalt antall for denne MixMatch varianten"
     ED-Tekst AT ROW 10.76 COL 20 NO-LABEL
     btnTilDato AT ROW 7.48 COL 40 NO-TAB-STOP 
     RECT-62 AT ROW 1 COL 43
     RECT-63 AT ROW 1 COL 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.62
         SCROLLABLE SIZE 93 BY 15.71.

DEFINE FRAME frmListe
     rectBrowseListe AT ROW 2.43 COL 1
     RectBrowseSearchListe AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.62
         SIZE 93 BY 15.95.


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
         TITLE              = "MixMatch kampanjeregister"
         HEIGHT             = 19
         WIDTH              = 95.6
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
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
       FRAME frmListe:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME frmRad:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 19
       FRAME DEFAULT-FRAME:WIDTH            = 95.6.

/* SETTINGS FOR FRAME frmDetalj
                                                                        */
ASSIGN 
       FRAME frmDetalj:HEIGHT           = 15.71
       FRAME frmDetalj:WIDTH            = 93.

/* SETTINGS FOR BUTTON B-Stopp IN FRAME frmDetalj
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR ED-Tekst IN FRAME frmDetalj
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX MixAktiver IN FRAME frmDetalj
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN MixNr IN FRAME frmDetalj
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME frmListe
                                                                        */
/* SETTINGS FOR FRAME frmRad
                                                                        */
/* SETTINGS FOR BUTTON B-SokArtikkel IN FRAME frmRad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Beskr IN FRAME frmRad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Kode IN FRAME frmRad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

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
       HEIGHT          = 17.38
       WIDTH           = 95
       HIDDEN          = NO
       SENSITIVE       = YES.
/* TabStrip OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* MixMatch kampanjeregister */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* MixMatch kampanjeregister */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* MixMatch kampanjeregister */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmRad
&Scoped-define SELF-NAME B-SokArtikkel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokArtikkel C-Win
ON CHOOSE OF B-SokArtikkel IN FRAME frmRad /* Søk */
OR F10 OF Kode
DO:
  APPLY "ALT-S" TO CURRENT-WINDOW.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmDetalj
&Scoped-define SELF-NAME B-Stopp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Stopp C-Win
ON CHOOSE OF B-Stopp IN FRAME frmDetalj /* Stopp kampanje */
DO:
  ASSIGN
      MixAktiver:SCREEN-VALUE = 'no'
      B-Stopp:SENSITIVE = FALSE
      KlarTilAktivering:SCREEN-VALUE = 'no'
      .
  APPLY "ANY-PRINTABLE":U TO MixTekst.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFraDato C-Win
ON CHOOSE OF btnFraDato IN FRAME frmDetalj /* ... */
DO:
  RUN Cal.w (FraDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTilDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTilDato C-Win
ON CHOOSE OF btnTilDato IN FRAME frmDetalj /* ... */
DO:
  RUN Cal.w (TilDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmRad
&Scoped-define SELF-NAME Kode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Kode C-Win
ON RETURN OF Kode IN FRAME frmRad /* Artikkel */
OR "TAB" OF Kode
DO:
    IF SELF:MODIFIED THEN
    DO:
        Beskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","ArtBas",
                                              "WHERE ArtikkelNr = dec(" + Kode:SCREEN-VALUE + ")","Beskr").  
        IF Beskr:SCREEN-VALUE = '?' THEN
        DO:
            MESSAGE "Ukjent artikkel."
                VIEW-AS ALERT-BOX WARNING BUTTONS OK.
            RETURN NO-APPLY.
        END.
    END.
  
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
          iTab = iTab + 10
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
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}
       hWindow                       = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW).
  IF VALID-HANDLE(hMixDet) THEN APPLY "close" TO hMixDet.
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

ON F2,ALT-S OF hWindow ANYWHERE 
DO:
    IF NOT B-SokArtikkel:SENSITIVE IN FRAME frmRad THEN
        RETURN NO-APPLY.
    RUN d-hsok.w (OUTPUT fArtikkelNr,""). 
    IF fArtikkelNr = ? THEN
        RETURN NO-APPLY.
    ELSE DO WITH FRAME frmRad:
        ASSIGN
            Kode:SCREEN-VALUE  = STRING(fArtikkelNr)
            Beskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","ArtBas",
                                        "WHERE ArtikkelNr = dec(" + Kode:SCREEN-VALUE + ")","Beskr").  

            .
        APPLY "ENTRY" TO Antall.
    END.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
  RUN enable_UI.
  RUN InitWindow.
  {lng.i} /* Oversettelse */
  DYNAMIC-FUNCTION("DoLockWindow",?).

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

ON 'default-action':U OF hBrowseListe
DO:
    ASSIGN
        iTab = 2 + 10
        .
    DYNAMIC-FUNCTION('TabStripChanged',iTab).

    DO WITH FRAME frmDetalj:
        APPLY "Entry" TO MixTekst.
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

OCXFile = SEARCH( "mixmatch.wrx":U ).
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
ELSE MESSAGE "mixmatch.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayRecord C-Win 
PROCEDURE displayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN SUPER.

  RUN setMixType (hFieldMap:BUFFER-FIELD('MixMatchType'):BUFFER-VALUE).

  DO WITH FRAME frmDetalj:
      ASSIGN
          B-Stopp:SENSITIVE = INPUT MixAktiver
          KlarTilAktivering:SENSITIVE = NOT INPUT MixAktiver
          MixNr:SENSITIVE = NOT INPUT MixAktiver
          MixTekst:SENSITIVE = NOT INPUT MixAktiver
          FraDato:SENSITIVE = NOT INPUT MixAktiver
          TilDato:SENSITIVE = NOT INPUT MixAktiver
          btnFraDato:SENSITIVE = NOT INPUT MixAktiver
          btnTilDato:SENSITIVE = NOT INPUT MixAktiver
          Utpris:SENSITIVE = NOT INPUT MixAktiver
          Antall:SENSITIVE = NOT INPUT MixAktiver
          .
  END.

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
  DISPLAY MixAktiver KlarTilAktivering MixNr MixMatchType ProfilNr MixTekst 
          FraDato TilDato Utpris Antall ED-Tekst 
      WITH FRAME frmDetalj IN WINDOW C-Win.
  ENABLE RECT-62 btnFraDato RECT-63 KlarTilAktivering MixMatchType ProfilNr 
         MixTekst FraDato TilDato Utpris Antall btnTilDato 
      WITH FRAME frmDetalj IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmDetalj}
  ENABLE rectBrowseListe RectBrowseSearchListe 
      WITH FRAME frmListe IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmListe}
  DISPLAY Kode Beskr Antall 
      WITH FRAME frmRad IN WINDOW C-Win.
  ENABLE rectBrowseRad RectBrowseSearchRad RECT-3 Antall 
      WITH FRAME frmRad IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmRad}
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

cAdgang = (DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 16 and SysGr = 34 and ParaNr = 1","Parameter1")).  


DO WITH FRAME frmListe:
  hBrowseListe = DYNAMIC-FUNCTION("NewBrowse",         /* Create a browse object */
                    rectBrowseListe:HANDLE IN FRAME frmListe,            /* Rectangle to define coordinates for browse */
                    200,                            /* Rows to batch */
                    "MULTIPLE,NUM-LOCKED-COLUMNS|1",                     /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    /*
                    "Gavekort;butnr;IdentType;Dato|Reg.dato;+cRegTid|CHARACTER|x(5)|gavekort_regtid.p|Tid;Gyldigdato;Bruktdato;KasseNr;BongNr;Belop;!Tid," + 
                      "GaveKType;GKTBeskrivelse",                     /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/
                    */
                    "MixMatchHode;MixNr;MixTekst;Antall;Utpris;FraDato;TilDato;ProfilNr|Profilnr|>>>>>>9;MixMatchType;KlarTilAktivering;MixAktiver",
                    "WHERE true",
                    "sort|MixNr").                            /* Misc - for something I might need in next version.. */
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
  /*DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"sortmap","cSerieNrReg;SerieNrReg").*/

END.

DO WITH FRAME frmDetalj:
  /* Oppsett av combo-box'er. */
  ASSIGN 
      MixMatchType:DELIMITER       = "|"
      MixMatchType:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("GetFieldList","SysPara;Parameter1|Beskrivelse;Parameter1","WHERE SysPara.SysHId = 17 and SysPAra.SysGr = 1 BY ParaNr")
      ProfilNr:DELIMITER           = "|"
      ProfilNr:LIST-ITEM-PAIRS     = "|0|" + DYNAMIC-FUNCTION("GetFieldList","Prisprofil;ProfilNr|Beskrivelse;ProfilNr","WHERE TRUE BY ProfilNr")
      .
  RUN setMixType (INPUT 0).

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins)
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowseListe:QUERY,
                    FRAME frmDetalj:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                                                    /* Nb: Felt som brukes her må være hentet i Browser først.                        */
                    "MixTekst,Antall,Utpris,FraDato,TilDato,ProfilNr,MixMatchType,KlarTilAktivering,MixAktiver"
                       ,       
                    "MixTekst,Antall,Utpris,FraDato,TilDato,ProfilNr,MixMatchType,KlarTilAktivering,MixAktiver"
                       ,       
                    "MixNr", /* Additional buffer and displ.fields - not updateable*/
                    "MixNr",  
                    "").     /* Input fields other than in buffer */

  /* Ignore = Ingen validering. Ellers skriv navn på program som gjør validering. */
  /* Dynamisk validering er default. Tileggsvalidering +<ProcNavn>.               */
  /* Kun egen validering, uten dynaisk validering =<ProcNavn>.                    */
  /* Dynamisk validering: Fremednøkkel - Felt må være indeksert og må finnes som  */
  /*                      primærnøkkel i en annen tabell.                         */
  /* DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore").   */
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore"). 
  /*DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraFields","Tid,BruktTid").*/
/*   DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore"). */

  DYNAMIC-FUNCTION('setAttribute',hFieldMap,"fieldnamedeletewarning","MixTekst").

  /* Felter som skal være disablet. Comma separert. */
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"disabledFields","MixAktiver"). 


END.

DO WITH FRAME frmRad:
  hBrowseRad = DYNAMIC-FUNCTION("NewBrowse",         /* Create a browse object */
                 rectBrowseRad:HANDLE IN FRAME frmRad,            /* Rectangle to define coordinates for browse */
                 200,                            /* Rows to batch */
                 "MULTIPLE,NUM-LOCKED-COLUMNS|1",                     /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                 /*
                 "Gavekort;butnr;IdentType;Dato|Reg.dato;+cRegTid|CHARACTER|x(5)|gavekort_regtid.p|Tid;Gyldigdato;Bruktdato;KasseNr;BongNr;Belop;!Tid," + 
                   "GaveKType;GKTBeskrivelse",                     /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/
                 */
                 "MixMatchRad;Kode|ArtikkelNr;Antall|Antall|>>>9,ArtBas;Beskr|Varetekst",
                 "WHERE MixNr = " + MixNr:screen-value IN FRAME frmDetalj + ", first ArtBas no-lock where ArtBas.ArtikkelNr = dec(Kode)",
                 "sort|Kode").                            /* Misc - for something I might need in next version.. */
  hBrowseRad:NAME = "brwRad". /* This name is neccessary because the browser is due to special treatment during resize */
  hBufferRad = hBrowseRad:QUERY:GET-BUFFER-HANDLE(1).
  /* Flytter kolonner */
  hBrowseRad:MOVE-COLUMN(3,2).
  /*
  /* Endring av labler på kolonner */
  hBrowseListe:GET-BROWSE-COLUMN(2):LABEL = "Type".
  */
  /* Endring av bredde på kollonner */
  /*
  hBrowseListe:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 160.
  */

  DYNAMIC-FUNCTION("setAttribute",hBrowseRad,"querywhere","").
  /*DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"basequery","WHERE Individ.individType = 1 ").*/
  /*DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"sortmap","cRegTid;Tid").*/
  /*DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"sortmap","cSerieNrReg;SerieNrReg").*/
  DYNAMIC-FUNCTION("setAttribute",hBrowseRad,"NoColumnSearch","cBeskr").

  hFieldMapRad = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins)
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowseRad:QUERY,
                    FRAME frmRad:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                                                    /* Nb: Felt som brukes her må være hentet i Browser først.                        */
                    "Kode,Antall"
                       ,       
                    "Kode,Antall"
                       ,       
                    "Beskr", /* Additional buffer and displ.fields - not updateable*/
                    "Beskr",  
                    "").     /* Input fields other than in buffer */

  /* Link av nøkkelfelt */
  DYNAMIC-FUNCTION("setAttribute",hFieldMapRad,"bufferExtraFields","MixNr"). 

  /* Ignore = Ingen validering. Ellers skriv navn på program som gjør validering. */
  /* Dynamisk validering er default. Tileggsvalidering +<ProcNavn>.               */
  /* Kun egen validering, uten dynaisk validering =<ProcNavn>.                    */
  /* Dynamisk validering: Fremednøkkel - Felt må være indeksert og må finnes som  */
  /*                      primærnøkkel i en annen tabell.                         */
  /* DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore").   */
    DYNAMIC-FUNCTION("setAttribute",hFieldMapRad,"customUpdateValProc","ignore"). 
  /*DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraFields","Tid,BruktTid").*/
  /*DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore").    */

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
  DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearchRad:HANDLE,hBrowseRad,1).

  /* Link objects: */
  /*
  DYNAMIC-FUNCTION("LinkAllObjects",                /* Link all created objects. Linktype is type of "to" object,
                                                      f.ex link from browse to combo-box is combo-box link */
                    THIS-PROCEDURE:CURRENT-WINDOW,  /* Link only objects created for current window */
                    TRUE,                           /* Replace any existing links */
                    STRING(hToolBar)).  /* Except these objects */
    */

  DYNAMIC-FUNCTION("createObjectLink",hBrowseListe,hUpdToolbar).
  DYNAMIC-FUNCTION("createObjectLink",hFieldMap,hUpdToolbar).
  DYNAMIC-FUNCTION("createObjectLink",hBrowseListe,hFieldMap).
  DYNAMIC-FUNCTION("createObjectLink",hBrowseRad,hFieldMapRad).
  /* Linker sammen to browsere. */
  DYNAMIC-FUNCTION("createParentLink",hBrowseRad,hBrowseListe,'MixNr').


  /* For DEBUG */
  /*DYNAMIC-FUNCTION("DumpAllLinks",THIS-PROCEDURE:CURRENT-WINDOW,"c:\temp\links.txt").  */

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectUpdToolBar").
  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,300,0,0).

  DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  /*DYNAMIC-FUNCTION("SetToolbar",hUpdToolbar,"enable").*/
  DYNAMIC-FUNCTION("SetToolbar",hToolBar,"enable").

END.

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

TabStripChanged(11).
APPLY "ENTRY" TO hBrowseListe.
IF hBrowseListe:NUM-ITERATIONS > 0 THEN
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

  IF iTab = 1 THEN
  DO:
      ASSIGN
          iTab = 2 + 10
          .
      DYNAMIC-FUNCTION('TabStripChanged',iTab).
  END.

  /* Setter fokus igjen etter at linkene er endret. */
  DYNAMIC-FUNCTION("SetCurrentObject",hUpdToolbar).

  RUN SUPER.

  IF iTab < 3 THEN
  DO:
    DO WITH FRAME frmDetalj:
        ASSIGN
            MixNr:SENSITIVE = FALSE
            ProfilNr:SENSITIVE = TRUE
            MixMatchType:SENSITIVE = TRUE
            MixTekst:SENSITIVE = TRUE
            MixMatchType:SCREEN-VALUE = ENTRY(4,MixMatchType:LIST-ITEM-PAIRS,"|")
            ProfilNr:SCREEN-VALUE = ENTRY(4,ProfilNr:LIST-ITEM-PAIRS,"|")
            FraDato:SCREEN-VALUE = STRING(TODAY)
            TilDato:SCREEN-VALUE = STRING(TODAY + 1)
            .
        APPLY "ENTRY" TO MixTekst.
    END.
  END.
  ELSE DO:
    DO WITH FRAME frmRad:
        ASSIGN
            Kode:SENSITIVE          = TRUE
            Antall:SENSITIVE        = FALSE
            Antall:SCREEN-VALUE     = '1'
            B-SokArtikkel:SENSITIVE = TRUE
            .
        APPLY "ENTRY" TO Kode.
    END.
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
  DEF VAR cState     AS CHAR  NO-UNDO.
  DEF VAR pcTekst    AS CHAR  NO-UNDO.

  ASSIGN
    cState = DYNAMIC-FUNCTION('getToolbarState',hUpdToolBar)
    .

  IF iTab < 3 THEN
  MIXHODE:
  DO:
    DO WITH FRAME frmDetalj:
   
   
      IF cState = "NEW" THEN
      DO:
        /*
        IF int(MixNr:screen-value) = int(DYNAMIC-FUNCTION("getFieldValues","IndType","WHERE MixNr = int(" + MixNr:SCREEN-VALUE + ")","MixNr")) THEN
        DO:
            MESSAGE "MixMatch kampanje eksisterer fra før med nummer " + MixNr:SCREEN-VALUE + "."
                VIEW-AS ALERT-BOX WARNING BUTTONS OK  TITLE "Lagringskontroll".
            APPLY "ENTRY" TO MixNr.
            RETURN NO-APPLY.
        END.
        */
      END.

      IF INPUT FraDato = ? OR INPUT TilDAto = ? THEN
      DO:
          MESSAGE "Dato fra og til må angis."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK.
          RETURN NO-APPLY.
      END.
      IF INPUT TilDato < INPUT FraDato THEN
      DO:
          MESSAGE "Til dato kan ikke være mindre enn fra dato."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK.
          RETURN NO-APPLY.
      END.
      IF INPUT FraDato < TODAY THEN
      DO:
          MESSAGE "Fra dato kan ikke være mindre enn dagens dato."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK.
          RETURN NO-APPLY.
      END.
      IF INPUT MixTekst = "" THEN
      DO:
          MESSAGE "Det må legges inn en tekst på kampanjen."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK.
          RETURN NO-APPLY.
      END.
      IF INPUT MixMatchType = 0 THEN
      DO:
          MESSAGE "Udefinert kampanjetype. Kampanjetype må angis."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK.
          RETURN NO-APPLY.
      END.
      IF DYNAMIC-FUNCTION("getFieldValues","PrisProfil","WHERE ProfilNr = " + ProfilNr:SCREEN-VALUE,"ProfilNr") = ? THEN
      DO:
          MESSAGE "Ugyldig profilnummer."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK.
          RETURN NO-APPLY.
      END.
      IF INPUT Utpris = 0 THEN
      DO:
          MESSAGE "Pris må legges inn på kampanjen."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK.
          RETURN NO-APPLY.
      END.
      /* Legg inn test på antallsfeltet her. */
      CASE INPUT MixMatchType:
        WHEN 1 THEN 
            DO:
              /* Kontroll av antall */
              IF INPUT Antall = 0 THEN
              DO:
                MESSAGE "Antall må legges inn på kampanjen."
                    VIEW-AS ALERT-BOX WARNING BUTTONS OK.
                RETURN NO-APPLY.
              END.
              /* Kontroll av at det er registrert rader. */
              IF INPUT Antall > num-entries(DYNAMIC-FUNCTION("getRowIdList","MixMatchRad","",
                                            "WHERE MixNr = " + MixNr:SCREEN-VALUE)) THEN
              DO:
                  MESSAGE "Det må registreres minst like mange varelinjer som det er angitt i antallsfeltet i MixKampanjens hode " + 
                          MixNr:SCREEN-VALUE IN FRAME frmDetalj + ")."
                      VIEW-AS ALERT-BOX WARNING BUTTONS OK.
                  RETURN NO-APPLY.
              END.

            END.
        WHEN 2 THEN 
            DO:
            END.
      END CASE.
   
      ASSIGN
          MixNr:SENSITIVE = FALSE
          ProfilNr:SENSITIVE = FALSE
          MixMatchType:SENSITIVE = FALSE
          .
      APPLY "ENTRY" TO MixTekst.
   
    END.
  END. /* MIXHODE */
  ELSE 
  MIXRAD:
  DO WITH FRAME frmRad:
      pcTekst = DYNAMIC-FUNCTION("getFieldValues","MixMatchRad","WHERE MixNr = '" + string(hFieldMap:BUFFER-FIELD('MixNr'):BUFFER-VALUE) + "' and Kode = '" + Kode:SCREEN-VALUE + "'","Kode").
      IF pcTekst <> ? AND cState = "NEW" THEN
      DO:
          MESSAGE "Artikkelen er allerede lagt opp på MixMatch kampanjen."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK TITLE 'Lagringskontroll'.
          RETURN NO-APPLY.
      END.
      pcTekst = DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = dec(" + Kode:SCREEN-VALUE + ")","ArtikkelNr,OPris,Pakke,Pant,Utgatt").
      IF pcTekst = ? THEN
      DO:
          MESSAGE "Ukjent artikkel."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK TITLE 'Lagringskontroll'.
          RETURN NO-APPLY.
      END.
      IF DEC(Antall:SCREEN-VALUE) = 0 THEN
      DO:
          MESSAGE "Antall må angis på raden."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK TITLE 'Lagringskontroll'.
          RETURN NO-APPLY.
      END.
      IF (ENTRY(2,pcTekst,"|") = 'yes' OR
          ENTRY(3,pcTekst,"|") = 'yes' OR
          ENTRY(4,pcTekst,"|") = 'yes') THEN
      DO:
          MESSAGE "Artikler med åpen pris, pakkevarer og pantartikler kan ikke legges opp i en MixMatch kampanje."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK TITLE 'Lagringskontroll'.
          RETURN NO-APPLY.
      END.
      IF (ENTRY(5,pcTekst,"|") = 'yes') THEN
      DO:
          MESSAGE "Artikkelen er utgått. Utgatt artikler kan legges opp på MixMatch kampanje."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK TITLE 'Lagringskontroll'.
          RETURN NO-APPLY.
      END.
      /* Her legger vi inn nøkkelfeltverdi */
      DYNAMIC-FUNCTION("setAttribute",hFieldMapRad,"bufferExtraValues",STRING(hFieldMap:BUFFER-FIELD('MixNr'):BUFFER-VALUE)). 
      ASSIGN
          Kode:SENSITIVE = FALSE
          B-SokArtikkel:SENSITIVE = FALSE
          .
  END. /* MIXRAD */

  RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setMixType C-Win 
PROCEDURE setMixType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piMixMatchType AS INT NO-UNDO.

  IF iTab < 3 THEN
  DO:
    DO WITH FRAME frmDetalj:
        CASE piMixMatchType:
            WHEN 0 THEN 
                ASSIGN
                ED-Tekst:SCREEN-VALUE = "UDEFINERT KAMPANJETYPE:" + CHR(10) + 
                                        "Velg en kampanjetype"
                Antall:HIDDEN         = FALSE 
                .
            WHEN 1 THEN 
                ASSIGN
                ED-Tekst:SCREEN-VALUE = "Mix Kampanje N av X:" + CHR(10) + 
                                        "En fri kombinasjon av N varer som inngår i kampanjen skal gi Mix rabatt." + CHR(10) +
                                        "Pris angis på kampanjen. Den fordeles forholdsmessig på det antall varelinjer som inngår i kombinasjonen."
                Antall:HIDDEN         = FALSE
                .
            WHEN 2 THEN 
                ASSIGN
                ED-Tekst:SCREEN-VALUE = "KAMPANJETYPE X AV N:" + CHR(10) + 
                                        "Det registreres inn et antall varianter (f.eks 10). Deretter angis kriteriet (F.eks - Antall = 3) for at rabatt skal gis." + CHR(10) +
                                        "Oppfylles kriteriet gis en rabatt lik prisen på produktene - pris på kampanje."
                Antall:HIDDEN         = TRUE
                .
        END CASE.
    END.
  END.
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

  IF iTab < 3 THEN
  DO:
    DO WITH FRAME frmDetalj:
        ASSIGN          
            MixNr:SENSITIVE = FALSE
            ProfilNr:SENSITIVE = FALSE
            MixMatchType:SENSITIVE = FALSE

            .
    END.
  END.
  ELSE DO WITH FRAME frmRad:
      ASSIGN
          Kode:SENSITIVE = FALSE
          B-SokArtikkel:SENSITIVE = FALSE
          .

  END.

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
  DEF INPUT PARAMETER cFieldName AS CHAR NO-UNDO.

  /*IF iTab < 3 THEN*/
  DO:
      CASE cFieldName:
          WHEN 'MixMatchType' THEN
          DO:
            DO WITH FRAME frmDetalj:
                IF MixMatchType:SCREEN-VALUE = "1" THEN
                    Antall:SCREEN-VALUE   = "0".
                RUN setMixType (INPUT int(MixMatchType:SCREEN-VALUE)).
            END.
          END.
      END CASE.
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

cTabStripList = "Liste|Detalj|Rader".

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
      DYNAMIC-FUNCTION("replaceObjectLink",hBrowseListe,hUpdToolbar).
      DYNAMIC-FUNCTION("replaceObjectLink",hFieldMap,hUpdToolbar).
  END.
  ELSE IF iiTab = 2 THEN
  DO:
      FRAME frmDetalj:MOVE-TO-TOP().
      IF CAN-DO('new',cstate) THEN
          ASSIGN
          MixNr:SENSITIVE = TRUE
          ProfilNr:SENSITIVE = TRUE
          MixMatchType:SENSITIVE = TRUE
          .
      ELSE
          ASSIGN
          MixNr:SENSITIVE = FALSE
          ProfilNr:SENSITIVE = FALSE
          MixMatchType:SENSITIVE = FALSE
          .
      DYNAMIC-FUNCTION("replaceObjectLink",hBrowseListe,hUpdToolbar).
      DYNAMIC-FUNCTION("replaceObjectLink",hFieldMap,hUpdToolbar).
  END.
  ELSE IF iiTab = 3 THEN
  DO:
      FRAME frmRad:MOVE-TO-TOP().
      DYNAMIC-FUNCTION("replaceObjectLink",hBrowseRad,hUpdToolbar).
      DYNAMIC-FUNCTION("replaceObjectLink",hFieldMapRad,hUpdToolbar).
  END.
  /*
  APPLY "ENTRY":U TO fi-fArtikkelNr.
  */

  RETURN TRUE.   /* Function return value. */
END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

