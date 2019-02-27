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
DEF VAR iReturn         AS INT NO-UNDO.

DEF VAR hDataToolbar    AS HANDLE NO-UNDO.
DEF VAR hWinToolBar     AS HANDLE NO-UNDO.
DEF VAR hBrowseGK       AS HANDLE NO-UNDO.
DEF VAR hBrowseTL       AS HANDLE NO-UNDO.
DEF VAR hPopupUtgatt    AS HANDLE NO-UNDO.

DEF VAR hGKTLdet        AS HANDLE NO-UNDO.
DEF VAR hTLdet          AS HANDLE NO-UNDO.

DEF VAR iTab            AS INT NO-UNDO.

DEF VAR iCL             AS INT NO-UNDO.

DEF VAR hFakturering    AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectWinToolBar rectDataToolBar 

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
DEFINE RECTANGLE rectDataToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 11 BY 1.19.

DEFINE RECTANGLE rectWinToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16 BY 1.19.

DEFINE BUTTON btnClearFilter 
     LABEL "Blank filter" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btnDfraBruktdato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnDgyldigFra 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnDgyldigTil 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnDregFra 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnDregTil 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnDtilBruktdato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokKunde 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE BruktButNr AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Brukt i butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","0"
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE Bruktkassenr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kasse" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE butnr AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Fra butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","0"
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE cmbBrukt AS CHARACTER FORMAT "X(256)":U INITIAL "Alle" 
     LABEL "Benyttet" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Alle","Ja","Nei","Utgått" 
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE cmbType AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","0"
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE fiEgne AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Egne" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Alle",0,
                     "Ja",1,
                     "Nei",2
     DROP-DOWN-LIST
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiFakturert AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Fakturert" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Alle",0,
                     "Ja",1,
                     "Nei",2
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiUtgatt AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Utgått" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Alle",0,
                     "Ja",1,
                     "Nei",2
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE KasseNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kasse" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE BongNr AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Bongnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.

DEFINE VARIABLE BruktBongNr AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Brukt BongNr" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE fi-cKundenavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kundenavn" 
     VIEW-AS FILL-IN 
     SIZE 31.2 BY 1 NO-UNDO.

DEFINE VARIABLE fi-dBruktFra AS DATE FORMAT "99/99/99" 
     LABEL "Brukt fra dato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE fi-dBruktTil AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE fi-dGyldigFra AS DATE FORMAT "99/99/99":U 
     LABEL "Gyld. fra dato" 
     VIEW-AS FILL-IN 
     SIZE 13.4 BY 1 NO-UNDO.

DEFINE VARIABLE fi-dGyldigTil AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE fi-dRegFra AS DATE FORMAT "99/99/99":U 
     LABEL "Reg. fra dato" 
     VIEW-AS FILL-IN 
     SIZE 13.4 BY 1 NO-UNDO.

DEFINE VARIABLE fi-dRegTil AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FraBelop AS DECIMAL FORMAT "->>,>>>,>>9.99" INITIAL 0 
     LABEL "Fra belop" 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1.

DEFINE VARIABLE IdentNr AS CHARACTER FORMAT "X(20)" 
     LABEL "Identnr" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE TilBelop AS DECIMAL FORMAT "->>,>>>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1.

DEFINE RECTANGLE rectBrowseGK
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 153 BY 17.14.

DEFINE RECTANGLE RectBrowseSearchGK
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY 1.

DEFINE RECTANGLE RectBrowseSearchTL
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY 1.

DEFINE RECTANGLE rectBrowseTL
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 153 BY 17.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectWinToolBar AT ROW 1.19 COL 141
     rectDataToolBar AT ROW 1.19 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 157.4 BY 28.29.

DEFINE FRAME frmGavekort
     rectBrowseGK AT ROW 2.19 COL 1
     RectBrowseSearchGK AT ROW 1 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 10.52
         SCROLLABLE SIZE 153 BY 18.33.

DEFINE FRAME frmTilgode
     rectBrowseTL AT ROW 2.19 COL 1
     RectBrowseSearchTL AT ROW 1 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 10.52
         SIZE 153 BY 18.57.

DEFINE FRAME frmFilter
     fiFakturert AT ROW 6.43 COL 63 COLON-ALIGNED
     fiUtgatt AT ROW 6.43 COL 114.6 COLON-ALIGNED
     btnDfraBruktdato AT ROW 4.24 COL 130
     IdentNr AT ROW 1.1 COL 13.6 COLON-ALIGNED HELP
          "Gavekortnr"
     cmbType AT ROW 2.14 COL 13.6 COLON-ALIGNED HELP
          "Gavekort-type"
     fi-dRegFra AT ROW 3.19 COL 13.6 COLON-ALIGNED
     fi-dRegTil AT ROW 3.19 COL 32.2 COLON-ALIGNED NO-LABEL
     fi-dGyldigFra AT ROW 4.24 COL 13.6 COLON-ALIGNED
     fi-dGyldigTil AT ROW 4.24 COL 32.2 COLON-ALIGNED NO-LABEL
     FraBelop AT ROW 5.33 COL 13.6 COLON-ALIGNED HELP
          "Fra beløp"
     fiEgne AT ROW 6.38 COL 13.6 COLON-ALIGNED
     TilBelop AT ROW 5.33 COL 32.2 COLON-ALIGNED HELP
          "Fra beløp" NO-LABEL
     butnr AT ROW 1.1 COL 63 COLON-ALIGNED HELP
          "Butikk der gavekort er kjøpt"
     KasseNr AT ROW 2.14 COL 63 COLON-ALIGNED HELP
          "Kasse, fra butikk"
     BongNr AT ROW 3.19 COL 63 COLON-ALIGNED HELP
          "Bongnr, kjøp"
     fi-cKundenavn AT ROW 4.24 COL 63 COLON-ALIGNED HELP
          "Kundenavn"
     cmbBrukt AT ROW 5.33 COL 63 COLON-ALIGNED
     btnSokKunde AT ROW 4.24 COL 96.4
     BruktButNr AT ROW 1.1 COL 114.6 COLON-ALIGNED HELP
          "Butikk der gavekort er brukt"
     Bruktkassenr AT ROW 2.14 COL 114.6 COLON-ALIGNED HELP
          "Kasse, brukt i butikk"
     BruktBongNr AT ROW 3.19 COL 114.6 COLON-ALIGNED HELP
          "BongNr på BRUKT bong."
     fi-dBruktFra AT ROW 4.24 COL 114.6 COLON-ALIGNED HELP
          "Dato brukt"
     fi-dBruktTil AT ROW 4.24 COL 133 COLON-ALIGNED HELP
          "Dato brukt" NO-LABEL
     btnClearFilter AT ROW 6.43 COL 135
     btnDregFra AT ROW 3.19 COL 29.2
     btnDregTil AT ROW 3.19 COL 47.4
     btnDtilBruktdato AT ROW 4.24 COL 148.4
     btnDgyldigFra AT ROW 4.24 COL 29.2
     btnDgyldigTil AT ROW 4.24 COL 47.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.91
         SIZE 152.6 BY 6.62.


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
         TITLE              = "Gavekort"
         HEIGHT             = 28.29
         WIDTH              = 157.4
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
ASSIGN FRAME frmFilter:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME frmGavekort:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME frmTilgode:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME frmFilter
   Custom                                                               */
/* SETTINGS FOR FRAME frmGavekort
   Custom                                                               */
ASSIGN 
       FRAME frmGavekort:HEIGHT           = 18.33
       FRAME frmGavekort:WIDTH            = 153.

/* SETTINGS FOR FRAME frmTilgode
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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmFilter
/* Query rebuild information for FRAME frmFilter
     _Query            is NOT OPENED
*/  /* FRAME frmFilter */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmTilgode
/* Query rebuild information for FRAME frmTilgode
     _Query            is NOT OPENED
*/  /* FRAME frmTilgode */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME TabStrip ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.67
       COLUMN          = 2
       HEIGHT          = 26.43
       WIDTH           = 156
       HIDDEN          = no
       SENSITIVE       = yes.
/* TabStrip OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Gavekort */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Gavekort */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Gavekort */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmFilter
&Scoped-define SELF-NAME BongNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BongNr C-Win
ON RETURN OF BongNr IN FRAME frmFilter /* Bongnr */
OR TAB OF BongNr DO:
  ASSIGN BongNr.
  IF BongNr NE 0 THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BruktBongNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BruktBongNr C-Win
ON RETURN OF BruktBongNr IN FRAME frmFilter /* Brukt BongNr */
OR TAB OF BruktBongNr DO:
  ASSIGN BruktBongNr.
  IF BruktBongNr NE 0 THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BruktButNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BruktButNr C-Win
ON VALUE-CHANGED OF BruktButNr IN FRAME frmFilter /* Brukt i butikk */
DO:
  BruktKassenr:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("getFieldList","Kasse;KasseNr|Navn;KasseNr",
                                       "WHERE ButikkNr = " + BruktButNr:SCREEN-VALUE),"|").
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Bruktkassenr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bruktkassenr C-Win
ON VALUE-CHANGED OF Bruktkassenr IN FRAME frmFilter /* Kasse */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearFilter C-Win
ON CHOOSE OF btnClearFilter IN FRAME frmFilter /* Blank filter */
DO:
  ASSIGN BongNr:SCREEN-VALUE = "" 
         BruktBongNr:SCREEN-VALUE = "" 
         BruktButNr:SCREEN-VALUE = "0" 
         Bruktkassenr:SCREEN-VALUE = "0" 
         butnr:SCREEN-VALUE = "0" 
         cmbType:SCREEN-VALUE = "" 
         fi-cKundenavn:SCREEN-VALUE = "" 
         fi-dBruktFra:SCREEN-VALUE = "" 
         fi-dBruktTil:SCREEN-VALUE = "" 
         fi-dGyldigFra:SCREEN-VALUE = "" 
         fi-dGyldigTil:SCREEN-VALUE = "" 
         fi-dRegFra:SCREEN-VALUE = "" 
         fi-dRegTil:SCREEN-VALUE = "" 
         FraBelop:SCREEN-VALUE = "" 
         IdentNr:SCREEN-VALUE = "" 
         KasseNr:SCREEN-VALUE = "0" 
         cmbBrukt:SCREEN-VALUE = "Alle" 
         TilBelop:SCREEN-VALUE = ""
         fiEgne:SCREEN-VALUE = ""
         fiFakturert:SCREEN-VALUE = ""
         fiUtgatt:SCREEN-VALUE = ""
         .
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDfraBruktdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDfraBruktdato C-Win
ON CHOOSE OF btnDfraBruktdato IN FRAME frmFilter /* ... */
DO:
  RUN Cal.w (fi-dBruktFra:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDgyldigFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDgyldigFra C-Win
ON CHOOSE OF btnDgyldigFra IN FRAME frmFilter /* ... */
DO:
  RUN Cal.w (fi-dGyldigFra:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDgyldigTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDgyldigTil C-Win
ON CHOOSE OF btnDgyldigTil IN FRAME frmFilter /* ... */
DO:
  RUN Cal.w (fi-dGyldigTil:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDregFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDregFra C-Win
ON CHOOSE OF btnDregFra IN FRAME frmFilter /* ... */
DO:
  RUN Cal.w (fi-dRegFra:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDregTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDregTil C-Win
ON CHOOSE OF btnDregTil IN FRAME frmFilter /* ... */
DO:
  RUN Cal.w (fi-dRegTil:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDtilBruktdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDtilBruktdato C-Win
ON CHOOSE OF btnDtilBruktdato IN FRAME frmFilter /* ... */
DO:
  RUN Cal.w (fi-dBruktTil:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokKunde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokKunde C-Win
ON CHOOSE OF btnSokKunde IN FRAME frmFilter /* ... */
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "Navn,Adresse1;Postnr".

  RUN JBoxDLookup.w ("Kunde;KundeNr;Navn;Adresse1;Postnr;Telefon;MobilTlf", "where true", INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO: 
    fi-cKundenavn:SCREEN-VALUE = cLookupValue.
    APPLY "return" TO fi-cKundenavn.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME butnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL butnr C-Win
ON VALUE-CHANGED OF butnr IN FRAME frmFilter /* Fra butikk */
DO:
  Kassenr:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("getFieldList","Kasse;KasseNr|Navn;KasseNr",
                                       "WHERE ButikkNr = " + butnr:SCREEN-VALUE),"|").
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbBrukt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbBrukt C-Win
ON VALUE-CHANGED OF cmbBrukt IN FRAME frmFilter /* Benyttet */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbType C-Win
ON VALUE-CHANGED OF cmbType IN FRAME frmFilter /* Type */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cKundenavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cKundenavn C-Win
ON RETURN OF fi-cKundenavn IN FRAME frmFilter /* Kundenavn */
OR TAB OF fi-cKundenavn DO:
  ASSIGN fi-cKundenavn.
  IF fi-cKundenavn NE "" THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dBruktTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dBruktTil C-Win
ON RETURN OF fi-dBruktTil IN FRAME frmFilter
OR TAB OF fi-dBruktTil DO:
  ASSIGN fi-dBruktTil.
  IF fi-dBruktTil NE ? THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dGyldigTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dGyldigTil C-Win
ON RETURN OF fi-dGyldigTil IN FRAME frmFilter
OR TAB OF fi-dGyldigTil DO:
  ASSIGN fi-dGyldigTil.
  IF fi-dGyldigTil NE ? THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dRegTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dRegTil C-Win
ON RETURN OF fi-dRegTil IN FRAME frmFilter
OR TAB OF fi-dRegTil DO:
  ASSIGN fi-dRegTil.
  IF fi-dRegTil NE ? THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiEgne
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiEgne C-Win
ON VALUE-CHANGED OF fiEgne IN FRAME frmFilter /* Egne */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFakturert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFakturert C-Win
ON VALUE-CHANGED OF fiFakturert IN FRAME frmFilter /* Fakturert */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiUtgatt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiUtgatt C-Win
ON VALUE-CHANGED OF fiUtgatt IN FRAME frmFilter /* Utgått */
DO:
  RUN Openquery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IdentNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IdentNr C-Win
ON RETURN OF IdentNr IN FRAME frmFilter /* Identnr */
OR TAB OF IdentNr DO:
  ASSIGN IdentNr.
  IF IdentNr NE "" THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME KasseNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KasseNr C-Win
ON VALUE-CHANGED OF KasseNr IN FRAME frmFilter /* Kasse */
DO:
  RUN OpenQuery.
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

TabStripChanged(INT(COM-SELF:SelectedItem:Index)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmFilter
&Scoped-define SELF-NAME TilBelop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TilBelop C-Win
ON RETURN OF TilBelop IN FRAME frmFilter
OR TAB OF TilBelop DO:
  ASSIGN TilBelop.
  /* IF TilBelop NE 0 THEN */ RUN OpenQuery.
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
  IF VALID-HANDLE(hGKTLdet) THEN APPLY "close" TO hGKTLdet.
  IF VALID-HANDLE(hFakturering) THEN APPLY "close" TO hFakturering.
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
  FRAME {&FRAME-NAME}:HIDDEN = FALSE.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

ON 'default-action':U OF hBrowseGK
DO:
  IF NOT VALID-HANDLE(hGKTLdet) THEN DO:
    RUN GavekortDet.w PERSIST SET hGKTLdet (THIS-PROCEDURE).
    RUN InitFromBrowse IN hGKTLdet (hBrowseGK,hBrowseGK:QUERY:GET-BUFFER-HANDLE(1):NAME).
    DYNAMIC-FUNCTION("SetToolbar",hDataToolbar,"disable").
  END.
  ELSE RUN MoveToTop IN hGKTLdet.
  RETURN.
END.

ON 'default-action':U OF hBrowseTL
DO:
  IF NOT VALID-HANDLE(hGKTLdet) THEN DO:
    RUN GavekortDet.w PERSIST SET hGKTLdet (THIS-PROCEDURE).
    RUN InitFromBrowse IN hGKTLdet (hBrowseTL,hBrowseTL:QUERY:GET-BUFFER-HANDLE(1):NAME).
    DYNAMIC-FUNCTION("SetToolbar",hDataToolbar,"disable").
  END.
  ELSE RUN MoveToTop IN hGKTLdet.
  RETURN.
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

OCXFile = SEARCH( "Gavekort.wrx":U ).
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
ELSE MESSAGE "Gavekort.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
RUN EditRecord.
RUN DeleteRecord IN hGKTLdet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeselectGKrecord C-Win 
PROCEDURE DeselectGKrecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
hBrowseGK:DESELECT-FOCUSED-ROW().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeselectTLrecord C-Win 
PROCEDURE DeselectTLrecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
hBrowseTL:DESELECT-FOCUSED-ROW().
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
IF iTab = 1 AND VALID-HANDLE(hGKTLdet) THEN DO:
  RUN MoveToTop IN hGKTLdet.
  RUN DisplayRecord IN hGKTLdet.
END.
ELSE IF VALID-HANDLE(hGKTLdet) THEN DO:
  RUN MoveToTop IN hGKTLdet.
  RUN DisplayRecord IN hGKTLdet.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditRecord C-Win 
PROCEDURE EditRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF iTab = 1 THEN
  APPLY "default-action" TO hBrowseGK.
ELSE 
  APPLY "default-action" TO hBrowseTL.
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
  ENABLE rectWinToolBar rectDataToolBar 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY fiFakturert fiUtgatt IdentNr cmbType fi-dRegFra fi-dRegTil 
          fi-dGyldigFra fi-dGyldigTil FraBelop fiEgne TilBelop butnr KasseNr 
          BongNr fi-cKundenavn cmbBrukt BruktButNr Bruktkassenr BruktBongNr 
          fi-dBruktFra fi-dBruktTil 
      WITH FRAME frmFilter IN WINDOW C-Win.
  ENABLE fiFakturert fiUtgatt btnDfraBruktdato IdentNr cmbType fi-dRegFra 
         fi-dRegTil fi-dGyldigFra fi-dGyldigTil FraBelop fiEgne TilBelop butnr 
         KasseNr BongNr fi-cKundenavn cmbBrukt btnSokKunde BruktButNr 
         Bruktkassenr BruktBongNr fi-dBruktFra fi-dBruktTil btnClearFilter 
         btnDregFra btnDregTil btnDtilBruktdato btnDgyldigFra btnDgyldigTil 
      WITH FRAME frmFilter IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmFilter}
  ENABLE rectBrowseGK RectBrowseSearchGK 
      WITH FRAME frmGavekort IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmGavekort}
  ENABLE rectBrowseTL RectBrowseSearchTL 
      WITH FRAME frmTilgode IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmTilgode}
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
DEF VAR iNewBatch AS INT NO-UNDO.
DEF VAR iMaxCount AS INT NO-UNDO.
DEF VAR bDesc     AS LOG NO-UNDO.
DEF VAR hBrowse   AS HANDLE NO-UNDO.

hBrowse = IF iTab = 1 THEN hBrowseGK ELSE hBrowseTL.

IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"lastrowid") = "" THEN DO:
  RUN dAskForQueryBatch.w (OUTPUT iNewBatch).
  IF iNewBatch > 0 THEN DO:
    bDesc = IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"querydesc") = "desc" THEN TRUE ELSE FALSE.

    iMaxCount = DYNAMIC-FUNCTION("fillBrowse",hBrowse,
                        iNewBatch,
                        INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"querystart")),
                        DYNAMIC-FUNCTION("getAttribute",hBrowse,"buffersandfields"),
                        DYNAMIC-FUNCTION("getAttribute",hBrowse,"basequery") + 
                          DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryfilter") + 
                          DYNAMIC-FUNCTION("getAttribute",hBrowse,"querywhere") + 
                          DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryjoin"),
                        DYNAMIC-FUNCTION("getAttribute",hBrowse,"querysort"),
                        bDesc).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"querystart",STRING(iMaxCount)).
  END.
  ELSE IF iNewBatch = -1 THEN RETURN.
END.

DYNAMIC-FUNCTION("ToExcelViaFile",hBrowse,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FaktureringRecord C-Win 
PROCEDURE FaktureringRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hFakturering) THEN
  RUN GKfakturering.w PERSIST SET hFakturering.

RUN MoveToTop IN hFakturering.
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
DEF VAR hSearchFieldGK  AS HANDLE NO-UNDO.
DEF VAR hSearchFieldTL  AS HANDLE NO-UNDO.

SetTabStrip().

DO WITH FRAME frmFilter:
  ASSIGN butnr:DELIMITER = "|"
         BruktButNr:DELIMITER = "|"
         KasseNr:DELIMITER = "|"
         Bruktkassenr:DELIMITER = "|"
         cmbType:DELIMITER = "|"
         butnr:LIST-ITEM-PAIRS = "|0|" + 
            DYNAMIC-FUNCTION("GetFieldList","Butiker;Butik|ButNamn;Butik","WHERE TRUE BY ButNamn")
         BruktButNr:LIST-ITEM-PAIRS = butnr:LIST-ITEM-PAIRS
         cmbType:LIST-ITEM-PAIRS = "|0|" + 
            DYNAMIC-FUNCTION("GetFieldList","GaveKType;IdentType|GKTBeskrivelse;IdentType","WHERE TRUE BY GKTBeskrivelse")
         KasseNr:LIST-ITEM-PAIRS = "|0"
         Bruktkassenr:LIST-ITEM-PAIRS = "|0"
         butnr:SCREEN-VALUE = "0"
         BruktButNr:SCREEN-VALUE = "0"
         cmbType:SCREEN-VALUE = "0"         
         iCl = INT(DYNAMIC-FUNCTION("getFieldValues","SysPara",
                  "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1"))
         .
END.

DO WITH FRAME frmGavekort:
  hBrowseGK = DYNAMIC-FUNCTION("NewBrowse",         /* Create a browse object */
                    rectBrowseGK:HANDLE IN FRAME frmGavekort,            /* Rectangle to define coordinates for browse */
                    50,                            /* Rows to batch */
                    "MULTIPLE,NUM-LOCKED-COLUMNS|4",                     /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "Gavekort"
                      + ";butnr"
                      + ";IdentType|Type|>>>9"
                      + ";IdentNr|Gavekort            |x(30)"
                      + ";Belop"
                      + ";RabKr|Rabatt"
                      + ";Dato|Reg.dato|99/99/99"
                      + ";+cRegTid|CHARACTER|x(5)|int_to_hhmm_time.p(Tid)|Tid"
                      + ";Gyldigdato|Gyld.til|99/99/99"
                      + ";Bruktdato|Brukt|99/99/99;+cBruktTid|CHARACTER|x(5)|int_to_hhmm_time.p(BruktTid)|Tid"
                      + ";!Utgatt"
                      + ";UtgattDato|Utgått"
                      + ";!UtgattRegAv"
                      + ";!UtgattTid"
                      + ";!Eget"
                      + ";!Fakturert|Fakt|J/ "
                      + ";FakturertDato|Fakturert"
                      + ";KasseNr"
                      + ";BongNr"
                      + ";Selgernr"
                      + ";kassnr"
                      + ";BruktButNr"
                      + ";BruktSelgerNr"
                      + ";BruktKassNr"
                      + ";Bruktkassenr"
                      + ";BruktBongNr"
                      + ";KNavn"
                      + ";KTelefon"
                      + ";KAdresse1"
                      + ";KPostNr"
                      + ";KundeNr"
                      + ";!Tid"
                      + ";!Brukttid"
                      + ";!MAdresse1"
                      + ";!MedlemsNr"
                      + ";!MEtterNavn"
                      + ";!MForNavn"
                      + ";!MPostNr"
                      + ";!MTelefon"
                      + ",GaveKType"
                      + ";GKTBeskrivelse|Beskr"
                      + ";Brukerid"
                     , 
                    "WHERE true,FIRST GaveKType NO-LOCK OF Gavekort OUTER-JOIN",
                    "SORT|Identnr,getrecordcount").

  hBrowseGK:NAME = "brwGavekort". /* This name is neccessary because the browser is due to special treatment during resize */

  /*hBrowseGK:MOVE-COLUMN(28,3).*/
  hBrowseGK:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 40.
  hBrowseGK:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 25.
  hBrowseGK:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 150.
  hBrowseGK:GET-BROWSE-COLUMN(4):WIDTH-PIXELS = 60.
  hBrowseGK:GET-BROWSE-COLUMN(11):WIDTH-PIXELS = 50.
  hBrowseGK:GET-BROWSE-COLUMN(12):WIDTH-PIXELS = 50.
  hBrowseGK:GET-BROWSE-COLUMN(13):WIDTH-PIXELS = 50.
  hBrowseGK:GET-BROWSE-COLUMN(14):WIDTH-PIXELS = 70.

  DYNAMIC-FUNCTION("setAttribute",hBrowseGK,"querywhere","").
  DYNAMIC-FUNCTION("setAttribute",hBrowseGK,"rowstobatch","100").
  DYNAMIC-FUNCTION("setAttribute",hBrowseGK,"getrecordcount","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowseGK,"sortmap","cRegTid;Tid,cBruktTid;Brukttid").
  DYNAMIC-FUNCTION("setAttribute",hBrowseGK,"NoColumnSearch","cRegTid,Brukttid,GKTBeskrivelse").
          
END.

DO WITH FRAME frmTilgode:
  hBrowseTL = DYNAMIC-FUNCTION("NewBrowse",         /* Create a browse object */
                    rectBrowseTL:HANDLE IN FRAME frmTilgode,            /* Rectangle to define coordinates for browse */
                    100,                            /* Rows to batch */
                    "MULTIPLE,NUM-LOCKED-COLUMNS|4",                     /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "Tilgode"
                     + ";butnr"
                     + ";!IdentType|Type|>>>9"
                     + ";IdentNr|Gavekort            |x(30)"
                     + ";Belop"
                     + ";RabKr|Rabatt"
                     + ";Dato|Reg.dato|99/99/99"
                     + ";+cRegTid|CHARACTER|x(5)|int_to_hhmm_time.p(Tid)|Tid"
                     + ";Gyldigdato|Gyld.til|99/99/99"
                     + ";Bruktdato|Brukt|99/99/99;+cBruktTid|CHARACTER|x(5)|int_to_hhmm_time.p(BruktTid)|Tid"
                     + ";UtgattDato|Utgått"
                     + ";!Utgatt"
                     + ";!UtgattRegAv"
                     + ";!UtgattTid"
                     + ";!Eget"
                     + ";!Fakturert|Fakt|J/ "
                     + ";FakturertDato|Fakturert"
                     + ";KasseNr"
                     + ";BongNr"
                     + ";Selgernr"
                     + ";kassnr"
                     + ";BruktButNr"
                     + ";BruktSelgerNr"
                     + ";BruktKassNr"
                     + ";Bruktkassenr"
                     + ";BruktBongNr"
                     + ";KNavn"
                     + ";KTelefon"
                     + ";KAdresse1"
                     + ";KPostNr"
                     + ";KundeNr"
                     + ";!Tid"
                     + ";!Brukttid"
                     + ";!MAdresse1"
                     + ";!MedlemsNr"
                     + ";!MEtterNavn"
                     + ";!MForNavn"
                     + ";!MPostNr"
                     + ";!MTelefon"
                     + ";Brukerid;RegistrertAv"
                    , 
/*                     "Tilgode;butnr;IdentType|Ty;IdentNr;Eget|Eget;Belop;!RabKr;Dato|Reg.dato|99/99/99;+cRegTid|CHARACTER|x(5)|tilgode_regtid.p|Tid;Gyldigdato|Gyld.til|99/99/99;Bruktdato|Brukt|99/99/99;+cBruktTid|CHARACTER|x(5)|tilgode_brukttid.p|Tid;KasseNr;BongNr;Selgernr;kassnr" + */
/*                        ";Utgatt;KNavn;KTelefon;KAdresse1;KPostNr;KundeNr;BruktButNr;BruktSelgerNr;BruktKassNr;Bruktkassenr;BruktBongNr;!Tid;!Brukttid;!MAdresse1;!MedlemsNr;!MEtterNavn;!MForNavn;!MPostNr;!MTelefon;!UtgattDato;!UtgattTid;!UtgattRegAv" +                                     */
/*                        ",GaveKType;GKTBeskrivelse|Beskr",                     /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/                                                                                                                           */
                    "WHERE false",
                    "SORT|Identnr,getrecordcount").   

  hBrowseTL:NAME = "brwTilgode". /* This name is neccessary because the browser is due to special treatment during resize */

/*   hBrowseTL:MOVE-COLUMN(25,3). */
  hBrowseTL:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 40.
  hBrowseTL:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 50.
  hBrowseTL:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 150.
  hBrowseTL:GET-BROWSE-COLUMN(4):WIDTH-PIXELS = 60.
  hBrowseTL:GET-BROWSE-COLUMN(11):WIDTH-PIXELS = 50.
  hBrowseTL:GET-BROWSE-COLUMN(12):WIDTH-PIXELS = 50.
  hBrowseTL:GET-BROWSE-COLUMN(13):WIDTH-PIXELS = 50.
  hBrowseTL:GET-BROWSE-COLUMN(14):WIDTH-PIXELS = 70.

/*   DYNAMIC-FUNCTION("setAttribute",hBrowseTL,"querywhere","WHERE true").  */
  DYNAMIC-FUNCTION("setAttribute",hBrowseTL,"sortmap","cRegTid;Tid,cBruktTid;Brukttid").
  DYNAMIC-FUNCTION("setAttribute",hBrowseTL,"getrecordcount","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowseTL,"NoColumnSearch","cRegTid,Brukttid,GKTBeskrivelse").

END.

DO WITH FRAME {&FRAME-NAME}:

  hDataToolBar = DYNAMIC-FUNCTION("NewToolBar",
                    rectDataToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                          /* Corresponding menu label - no menu if blank */
                    "new;Ny,edit;Endre,delete;Slett,excel;Eksporter til E&xcel"
                  + ",Fakturering;Fakturering"
                    ,"").                   

  hWinToolBar = DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                          /* Corresponding menu label - no menu if blank */
                    "|Innstillinger,Close;Avslutt,Help|Hjelp;Hjelp",
                                                    /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    "maxborder,right").                        /* Misc - for something I might need in next version.. */

  DYNAMIC-FUNCTION("NewMenuBand",
                    hBrowseGK,  /* parent widget */
                    "DeselectGK;Fjern markering av rad,Utgatt;&Marker valgte poster som utgått,OpphevUtgatt;Opphev markering av poster som utgått",
                    "").     
  DYNAMIC-FUNCTION("NewMenuBand",
                    hBrowseTL,  /* parent widget */
                    "DeselectTL;Fjern markering av rad,Utgatt;&Marker valgte poster som utgått,OpphevUtgatt;Opphev markering av poster som utgått",
                    "").     
END. /* Default (container) frame */

hSearchFieldGK = DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearchGK:HANDLE,hBrowseGK,3).
hSearchFieldTL = DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearchTL:HANDLE,hBrowseTL,2).

/* Link objects: 

DYNAMIC-FUNCTION("LinkAllObjects",                /* Link all created objects. Linktype is type of "to" object,
                                                    f.ex link from browse to combo-box is combo-box link */
                  THIS-PROCEDURE:CURRENT-WINDOW,  /* Link only objects created for current window */
                  TRUE,                           /* Replace any existing links */
                  STRING(hDataToolBar) + "," + 
                  STRING(hWinToolBar) + "," + 
                  STRING(hSearchFieldGK) + "," + 
                  STRING(hSearchFieldTL)).           /* Except these objects */
*/

DYNAMIC-FUNCTION("CreateObjectLink",hDataToolbar,hBrowseGK).
DYNAMIC-FUNCTION("CreateObjectLink",hSearchFieldGK,hBrowseGK).
DYNAMIC-FUNCTION("CreateObjectLink",hSearchFieldTL,hBrowseTL).

/*   DYNAMIC-FUNCTION("DumpAllLinks",THIS-PROCEDURE:CURRENT-WINDOW,"c:\temp\links.txt").  */

DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmTilgode:HANDLE, "RectBrowseSearchTL").
DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmGavekort:HANDLE, "RectBrowseSearchGK").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectWinToolBar").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmFilter:HANDLE, "frmFilter").
DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmFilter:HANDLE, "btnDtilBruktdato").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,720,500,0,500).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

DYNAMIC-FUNCTION("SetToolbar",hDataToolbar,"enable").
DYNAMIC-FUNCTION("SetToolbar",hWinToolbar,"enable").

APPLY "value-changed" TO hBrowseGK.

SUBSCRIBE TO "InvalidateHandle" ANYWHERE.

APPLY "entry" TO hBrowseGK.

TabStripChanged(11).

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
DEF INPUT PARAM ihChild AS HANDLE NO-UNDO.

IF ihChild = hGKTLdet THEN hGKTLdet = ?.

DYNAMIC-FUNCTION("SetToolbar",hDataToolbar,"enable").

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
RUN EditRecord.
RUN NewRecord IN hGKTLdet.

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
DEF VAR cFilter AS CHAR NO-UNDO.

DO WITH FRAME frmFilter:
  ASSIGN fi-dBruktFra fi-dBruktTil fi-dGyldigFra fi-dGyldigTil fi-dRegFra fi-dRegTil FraBelop TilBelop fiEgne fiFakturert fiUtgatt.

  cFilter = "WHERE true " +
           (IF IdentNr:SCREEN-VALUE NE "" THEN
              IF IdentNr BEGINS "*" THEN
                " AND IdentNr MATCHES '" + IdentNr:SCREEN-VALUE + "*'"
              ELSE 
                " AND IdentNr BEGINS '" + IdentNr:SCREEN-VALUE + "'"
            ELSE "") +
           (IF cmbType:SCREEN-VALUE NE "0" AND cmbType:SCREEN-VALUE NE ? THEN
             " AND IdentType = " + cmbType:SCREEN-VALUE
            ELSE "") +

           (IF fi-dRegFra NE ? THEN
            " AND Dato GE DATE('" + fi-dRegFra:SCREEN-VALUE + "')"
            ELSE "") +
           (IF fi-dRegTil NE ? THEN
            " AND Dato LE DATE('" + fi-dRegTil:SCREEN-VALUE + "')"
            ELSE "") +

           (IF fi-dGyldigFra NE ? THEN
            " AND GyldigDato GE DATE('" + fi-dGyldigFra:SCREEN-VALUE + "')"
            ELSE "") +
           (IF fi-dGyldigTil NE ? THEN
            " AND GyldigDato LE DATE('" + fi-dGyldigTil:SCREEN-VALUE + "')"
            ELSE "") +
           (IF FraBelop NE 0 THEN
             " AND Belop GE DEC('" + FraBelop:SCREEN-VALUE + "')"
            ELSE "") +
           (IF TilBelop NE 0 THEN
             " AND Belop LE DEC('" + TilBelop:SCREEN-VALUE + "')"
            ELSE "") +

           (IF butnr:SCREEN-VALUE NE "0" AND butnr:SCREEN-VALUE NE ? THEN
             " AND butnr = " + butnr:SCREEN-VALUE
            ELSE "") +
           (IF Kassenr:SCREEN-VALUE NE "0" AND Kassenr:SCREEN-VALUE NE ? THEN
             " AND Kassenr = " + Kassenr:SCREEN-VALUE
            ELSE "") +
           (IF BongNr:SCREEN-VALUE NE "0" THEN
             " AND BongNr = " + BongNr:SCREEN-VALUE
            ELSE "") +
           (IF fi-cKundenavn:SCREEN-VALUE NE "" THEN
              IF fi-cKundenavn BEGINS "*" THEN
               " AND KNavn MATCHES '" + fi-cKundenavn:SCREEN-VALUE + "*'"
              ELSE
               " AND KNavn BEGINS '" + fi-cKundenavn:SCREEN-VALUE + "'"
            ELSE "") +

           (IF BruktButNr:SCREEN-VALUE NE "0" AND BruktButNr:SCREEN-VALUE NE ? THEN
             " AND BruktButNr = " + BruktButNr:SCREEN-VALUE
            ELSE "") +
           (IF Bruktkassenr:SCREEN-VALUE NE "0" AND Bruktkassenr:SCREEN-VALUE NE ? THEN
             " AND Bruktkassenr = " + Bruktkassenr:SCREEN-VALUE
            ELSE "") +
           (IF BruktBongNr:SCREEN-VALUE NE "0" THEN
             " AND BruktBongNr = " + BruktBongNr:SCREEN-VALUE
            ELSE "") +

           (IF fi-dBruktFra NE ? THEN
            " AND BruktDato GE DATE('" + fi-dBruktFra:SCREEN-VALUE + "')"
            ELSE "") +
           (IF fi-dBruktTil NE ? THEN
            " AND BruktDato LE DATE('" + fi-dBruktTil:SCREEN-VALUE + "')"
            ELSE "") +

           (IF fiEgne = 1 THEN
            " AND Eget = 'yes'"
            ELSE "") +
           (IF fiEgne = 2 THEN
            " AND Eget = 'no'"
             ELSE "") +

           (IF fiFakturert = 1 THEN
            " AND Fakturert = 'yes'"
            ELSE "") +
           (IF fiFakturert = 2 THEN
            " AND Fakturert = 'no'"
             ELSE "") +

           (IF fiUtgatt = 1 THEN
            " AND Utgatt = 'yes'"
            ELSE "") +
           (IF fiUtgatt = 2 THEN
            " AND Utgatt = 'no'"
             ELSE "") +

           (IF LOOKUP(cmbBrukt:SCREEN-VALUE,cmbBrukt:LIST-ITEMS) > 1 THEN
             IF LOOKUP(cmbBrukt:SCREEN-VALUE,cmbBrukt:LIST-ITEMS) = 2 THEN
               " AND Bruktdato NE ?"
             ELSE IF LOOKUP(cmbBrukt:SCREEN-VALUE,cmbBrukt:LIST-ITEMS) = 3 THEN
               " AND Bruktdato = ?"
             ELSE 
               " AND Utgatt"
            ELSE "") 
            .
END.

IF iTab = 1 THEN DO:
  DYNAMIC-FUNCTION("SetAttribute",hBrowseGK,"queryfilter",cFilter).
  DYNAMIC-FUNCTION("SetCurrentObject",hBrowseGK).
END.
ELSE DO:
  DYNAMIC-FUNCTION("SetAttribute",hBrowseTL,"queryfilter",cFilter).
  DYNAMIC-FUNCTION("SetCurrentObject",hBrowseTL).
END.

RUN SUPER.

IF iTab = 1 THEN 
  APPLY "entry" TO hBrowseGK.
ELSE
  APPLY "entry" TO hBrowseTL.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpphevUtgattRecord C-Win 
PROCEDURE OpphevUtgattRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF (iTab = 1 AND hBrowseGK:NUM-SELECTED-ROWS > 0)
   OR  (iTab = 2 AND hBrowseTL:NUM-SELECTED-ROWS > 0) THEN DO:
  iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Opphev merking av valgte poster som utgått?","Valg","").
  IF iReturn = 1 THEN
    RUN SetUtgattGKTL (FALSE).
END.
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen poster er valgt","Behandling","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLookupAttributes C-Win 
PROCEDURE setLookupAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      Gir mulighet for overstyring av query-egenskaper samt f.eks label for filter-felter
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihLookupBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM ihFilterField1 AS HANDLE NO-UNDO.
DEF INPUT PARAM ihFilterField2 AS HANDLE NO-UNDO.

/* IF DYNAMIC-FUNCTION("getAttribute",ihLookupBrowse,"BuffersAndFields") BEGINS "kunde" THEN  */
/*   DYNAMIC-FUNCTION("setAttribute",ihLookupBrowse,"basequery","where navn begins 'w'").     */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setUtgattGKTL C-Win 
PROCEDURE setUtgattGKTL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ibUtgatt AS LOG NO-UNDO.

DEF VAR hTmpBrowse AS HANDLE NO-UNDO.
DEF VAR cRowidList AS CHAR NO-UNDO.

IF VALID-HANDLE(hGKTLdet) THEN APPLY "close" TO hGKTLdet.

hTmpBrowse = IF iTab = 1 THEN hBrowseGK ELSE hBrowseTL.

DO ix = 1 TO hTmpBrowse:NUM-SELECTED-ROWS:
  bOK = hTmpBrowse:FETCH-SELECTED-ROW(ix).
  IF bOK THEN DO:
    iReturn = 6.
    IF ibUtgatt AND hTmpBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Gyldigdato"):BUFFER-VALUE > TODAY THEN
      iReturn = DYNAMIC-FUNCTION("DoMessage",0,3,
              (IF iTab = 1 THEN "Gavekort " ELSE "Tilgodelapp ") + "&1 er gyldig til &2" + CHR(10) +
              "Er du sikker på at det skal settes til utgått?",
              "Annulering",
              hTmpBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("IdentNr"):BUFFER-VALUE + "|" +
                hTmpBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Gyldigdato"):STRING-VALUE
              ).
    IF iReturn = 2 THEN LEAVE.
    ELSE IF iReturn = 7 THEN NEXT.

    bOk = DYNAMIC-FUNCTION("DoUpdate",hTmpBrowse:QUERY:GET-BUFFER-HANDLE(1):NAME,
                  "ignore",
                  "",
                  hTmpBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
                  "Utgatt,UtgattDato,UtgattTid,UtgattRegAv" + 
                    IF ibUtgatt THEN ",BruktButnr,BruktSelgerNr,BruktKassNr,Bruktkassenr" 
                    ELSE "",
                  STRING(ibUtgatt) + 
                    IF ibUtgatt THEN "|" + STRING(TODAY) + "|" + STRING(TIME) + "|" + DYNAMIC-FUNCTION("getASuserId") + "|" + STRING(iCl) + "|0|0|0" 
                    ELSE "|?|0|",
                  FALSE).
    cRowidList = cRowidList + hTmpBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Rowident1"):BUFFER-VALUE + ",".
  END.
END.

IF iReturn NE 2 THEN DO: 
  DYNAMIC-FUNCTION("DoCommit",FALSE).
  DYNAMIC-FUNCTION("RefreshRowids",hTmpBrowse,cRowidList).

/* Alternativ måte, uten å kjøre ny spørring. Raskere, med kalkulerte verdier og joins blir ikke utført: */

/*   DYNAMIC-FUNCTION("DoCommit",TRUE).      /* TRUE: Returner oppdaterte poster */      */
/*                                                                                       */
/*   IF DYNAMIC-FUNCTION("getTransactionMessage") = "" THEN DO:                          */
/*     DYNAMIC-FUNCTION("refreshFromTransRecord",hTmpBrowse:QUERY:GET-BUFFER-HANDLE(1)). */
/*     hTmpBrowse:REFRESH().                                                             */
/*   END.                                                                                */
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtgattRecord C-Win 
PROCEDURE UtgattRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF (iTab = 1 AND hBrowseGK:NUM-SELECTED-ROWS > 0)
   OR  (iTab = 2 AND hBrowseTL:NUM-SELECTED-ROWS > 0) THEN DO:
  iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Sett valgte poster til utgått?","Valg","").
  IF iReturn = 1 THEN
    RUN SetUtgattGKTL (TRUE).
END.
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen poster er valgt","Behandling","").

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
IF iTab = 1 THEN
  RETURN hBrowseGK.
ELSE
  RETURN hBrowseTL.

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
DEFINE VARIABLE cLng AS CHARACTER   NO-UNDO.
/* cTabStripList = DYNAMIC-FUNCTION("getFieldList","JBoxGenCode;cCodeValue;cMisc1;cMisc2",               */
/*                                   "WHERE cCodeType = 'JBoxQueryTabs'" +                               */
/*                                   "  AND cLanguage = '" + DYNAMIC-FUNCTION("getLanguageCode") + "'" + */
/*                                   " BY cMisc1") NO-ERROR.                                             */
cLng = DYNAMIC-FUNCTION("getLanguageCode") NO-ERROR.
IF cLng = "SE" THEN
    cTabStripList = "Presentkort|Tillgodokvitto".
ELSE
    cTabStripList = "Gavekort|Tilgodelapper".

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

DYNAMIC-FUNCTION("DeleteObjectLink",hDataToolbar,hBrowseGK).
DYNAMIC-FUNCTION("DeleteObjectLink",hDataToolbar,hBrowseTL).

DO WITH FRAME {&FRAME-NAME}:

  IF iiTab > 10 THEN DO:
    iTab = iiTab - 10.
    chTabStrip:TabStrip:Tabs:ITEM(iTab):SELECTED = TRUE.
  END.
  ELSE iTab = iiTab.

  IF VALID-HANDLE(hGKTLdet) THEN APPLY "close" TO hGKTLdet.

  IF iTab = 1 THEN DO:
    FRAME frmGavekort:MOVE-TO-TOP().
    DYNAMIC-FUNCTION("CreateObjectLink",hDataToolbar,hBrowseGK).
  END.
  ELSE DO:
    FRAME frmTilgode:MOVE-TO-TOP().
    DYNAMIC-FUNCTION("CreateObjectLink",hDataToolbar,hBrowseTL).
  END.

  FRAME frmFilter:MOVE-TO-TOP().

  IF iiTab < 10 THEN DO:
    APPLY "choose" TO btnClearFilter IN FRAME frmFilter.
    IF iTab = 1 THEN 
      APPLY "entry" TO hBrowseGK.
    ELSE APPLY "entry" TO hBrowseTL.
  END.

  RETURN TRUE.
END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

