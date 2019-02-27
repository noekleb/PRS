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
DEF VAR hEditToolbar    AS HANDLE NO-UNDO.
DEF VAR hBrowseSykkel   AS HANDLE NO-UNDO.
DEF VAR hBufferSykkel   AS HANDLE NO-UNDO.
DEF VAR hBrowseVapen    AS HANDLE NO-UNDO.
DEF VAR hBufferVapen    AS HANDLE NO-UNDO.
DEF VAR hBrowseElektro  AS HANDLE NO-UNDO.
DEF VAR hBufferElektro  AS HANDLE NO-UNDO.
DEF VAR hWindow         AS HANDLE NO-UNDO.

DEF VAR hIndividDet          AS HANDLE NO-UNDO.
DEF VAR hTLdet          AS HANDLE NO-UNDO.

DEF VAR iTab            AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectToolBar rectEditToolbar 

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
DEFINE RECTANGLE rectEditToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 13 BY 1.19.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 9 BY 1.19.

DEFINE RECTANGLE rectBrowseElektro
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 153 BY 15.48.

DEFINE RECTANGLE RectBrowseSearchElektro
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 22 BY 1.19.

DEFINE BUTTON b-Kalk-2 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS
     LABEL "b kalk 4" 
     SIZE 5 BY 1.

DEFINE BUTTON b-Kalk-4 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS
     LABEL "b kalk 4" 
     SIZE 5 BY 1.

DEFINE BUTTON b-Kalk1 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS
     LABEL "b kalk 4" 
     SIZE 5 BY 1.

DEFINE BUTTON b-Kalk3 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS
     LABEL "Button 1" 
     SIZE 5 BY 1.

DEFINE BUTTON B-SokArtikkel 
     IMAGE-UP FILE "icon/select.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Søk" 
     SIZE 4.6 BY 1 TOOLTIP "Starter Alt-S søkefunksjonen".

DEFINE BUTTON B-SokKunde  NO-FOCUS
     LABEL "..." 
     SIZE 4.6 BY 1 TOOLTIP "Starter Alt-S søkefunksjonen".

DEFINE BUTTON btnAktiverFilter 
     LABEL "Aktiver filter" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnBlankFilter 
     LABEL "Blank filter" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnSokVg  NO-FOCUS
     LABEL "..." 
     SIZE 4.6 BY 1.

DEFINE VARIABLE cmbButikk AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","0"
     DROP-DOWN-LIST
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cAdresse1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Adresse" 
     VIEW-AS FILL-IN 
     SIZE 32.8 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cBeskr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varetekst" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cGarantiNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "GarantiNr" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cJegerkort AS CHARACTER FORMAT "X(256)":U 
     LABEL "Jegerkort" 
     VIEW-AS FILL-IN 
     SIZE 32.8 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cMobilTlf AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 16.6 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cNavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kundenavn" 
     VIEW-AS FILL-IN 
     SIZE 32.8 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cPersOrgNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pers/OrgNr" 
     VIEW-AS FILL-IN 
     SIZE 32.8 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cSerieNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "SerieNr" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cTelefon AS CHARACTER FORMAT "X(256)":U 
     LABEL "Telefon/Mobil" 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cVapenKort AS CHARACTER FORMAT "X(256)":U 
     LABEL "Våpenkort" 
     VIEW-AS FILL-IN 
     SIZE 32.8 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cVmBeskrivelse AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varemerke" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fi-dFraKjoptDato AS DATE FORMAT "99/99/99":U 
     LABEL "Kjøpt" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-dFraSalgDato AS DATE FORMAT "99/99/99":U 
     LABEL "Solgt" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-dTilKjoptDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-dTilSalgDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-fArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "ArtikkelNr" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fi-fIndividNr AS DECIMAL FORMAT ">>>>>>>>>>>9":U INITIAL 0 
     LABEL "IndividNr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-iBatchNr AS INTEGER FORMAT "zzzzzzzz9":U INITIAL 0 
     LABEL "BatchNr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-iVg AS INTEGER FORMAT "zzzzz9":U INITIAL 0 
     LABEL "Varegr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tbIFeilVare AS LOGICAL INITIAL no 
     LABEL "Kun feilvarer" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE RECTANGLE RectBrowseSearchSykkel
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 22 BY 1.19.

DEFINE RECTANGLE rectBrowseSykkel
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 153 BY 15.57.

DEFINE RECTANGLE RectBrowseSearchVapen
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 22 BY 1.19.

DEFINE RECTANGLE rectBrowseVapen
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 153 BY 15.48.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectToolBar AT ROW 1.14 COL 148
     rectEditToolbar AT ROW 1.14 COL 1.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 156.4 BY 27.05.

DEFINE FRAME frmFilter
     B-SokArtikkel AT ROW 1.1 COL 33.2 NO-TAB-STOP 
     btnSokVg AT ROW 5.05 COL 124 NO-TAB-STOP 
     B-SokKunde AT ROW 1.1 COL 94 NO-TAB-STOP 
     btnBlankFilter AT ROW 5 COL 139
     fi-iVg AT ROW 5.1 COL 108 COLON-ALIGNED
     b-Kalk-2 AT ROW 2.05 COL 143 NO-TAB-STOP 
     fi-fArtikkelNr AT ROW 1.1 COL 13 COLON-ALIGNED
     fi-cBeskr AT ROW 2.1 COL 13 COLON-ALIGNED
     fi-cVmBeskrivelse AT ROW 3.1 COL 13 COLON-ALIGNED
     fi-fIndividNr AT ROW 4.1 COL 13 COLON-ALIGNED
     fi-cSerieNr AT ROW 5.1 COL 13 COLON-ALIGNED
     fi-cGarantiNr AT ROW 6.1 COL 13 COLON-ALIGNED
     fi-cNavn AT ROW 1.1 COL 59 COLON-ALIGNED
     fi-cTelefon AT ROW 2.1 COL 59 COLON-ALIGNED
     fi-cMobilTlf AT ROW 2.1 COL 75.2 COLON-ALIGNED NO-LABEL
     fi-cAdresse1 AT ROW 3.1 COL 59 COLON-ALIGNED
     fi-cPersOrgNr AT ROW 4.1 COL 59 COLON-ALIGNED
     fi-cVapenKort AT ROW 5.1 COL 59 COLON-ALIGNED
     fi-cJegerkort AT ROW 6.1 COL 59 COLON-ALIGNED
     cmbButikk AT ROW 1.1 COL 108 COLON-ALIGNED
     fi-dFraSalgDato AT ROW 2.1 COL 108 COLON-ALIGNED
     fi-dTilSalgDato AT ROW 2.1 COL 127 COLON-ALIGNED NO-LABEL
     fi-dFraKjoptDato AT ROW 3.1 COL 108 COLON-ALIGNED
     fi-dTilKjoptDato AT ROW 3.1 COL 127 COLON-ALIGNED NO-LABEL
     fi-iBatchNr AT ROW 4.1 COL 108 COLON-ALIGNED
     tbIFeilVare AT ROW 6.24 COL 110
     b-Kalk-4 AT ROW 3 COL 143 NO-TAB-STOP 
     b-Kalk1 AT ROW 2.05 COL 124 NO-TAB-STOP 
     b-Kalk3 AT ROW 3.05 COL 124 NO-TAB-STOP 
     btnAktiverFilter AT ROW 6.24 COL 139
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.62
         SIZE 154 BY 6.67.

DEFINE FRAME frmElektro
     rectBrowseElektro AT ROW 2.33 COL 1
     RectBrowseSearchElektro AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 10.52
         SCROLLABLE SIZE 154 BY 16.95.

DEFINE FRAME frmSykkel
     rectBrowseSykkel AT ROW 2.33 COL 1
     RectBrowseSearchSykkel AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 10.52
         SIZE 154 BY 16.91.

DEFINE FRAME frmVapen
     rectBrowseVapen AT ROW 2.43 COL 1
     RectBrowseSearchVapen AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 10.52
         SCROLLABLE SIZE 154 BY 16.95.


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
         TITLE              = "Individregister"
         HEIGHT             = 27
         WIDTH              = 156.4
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
ASSIGN FRAME frmElektro:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME frmFilter:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME frmSykkel:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME frmVapen:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR FRAME frmElektro
                                                                        */
ASSIGN 
       FRAME frmElektro:HEIGHT           = 16.95
       FRAME frmElektro:WIDTH            = 154.

/* SETTINGS FOR FRAME frmFilter
   Custom                                                               */
/* SETTINGS FOR FRAME frmSykkel
   L-To-R,COLUMNS                                                       */
/* SETTINGS FOR FRAME frmVapen
                                                                        */
ASSIGN 
       FRAME frmVapen:HEIGHT           = 16.95
       FRAME frmVapen:WIDTH            = 154.

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
       HEIGHT          = 25.48
       WIDTH           = 156
       HIDDEN          = no
       SENSITIVE       = yes.
      TabStrip:NAME = "TabStrip":U .
/* TabStrip OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Individregister */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Individregister */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Individregister */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmFilter
&Scoped-define SELF-NAME b-Kalk-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-Kalk-2 C-Win
ON CHOOSE OF b-Kalk-2 IN FRAME frmFilter /* b kalk 4 */
DO:
  RUN Cal.w (fi-dTilSalgDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-Kalk-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-Kalk-4 C-Win
ON CHOOSE OF b-Kalk-4 IN FRAME frmFilter /* b kalk 4 */
DO:
  RUN Cal.w (fi-dTilKjoptDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-Kalk1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-Kalk1 C-Win
ON CHOOSE OF b-Kalk1 IN FRAME frmFilter /* b kalk 4 */
DO:
  RUN Cal.w (fi-dFraSalgDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-Kalk3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-Kalk3 C-Win
ON CHOOSE OF b-Kalk3 IN FRAME frmFilter /* Button 1 */
DO:
  RUN Cal.w (fi-dFraKjoptDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokArtikkel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokArtikkel C-Win
ON CHOOSE OF B-SokArtikkel IN FRAME frmFilter /* Søk */
OR F10 OF fi-fArtikkelNr
DO:
  APPLY "ALT-S" to CURRENT-WINDOW.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokKunde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokKunde C-Win
ON CHOOSE OF B-SokKunde IN FRAME frmFilter /* ... */
OR F10 OF fi-cNavn
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "Navn".
  RUN JBoxDLookup.w ("Kunde;KundeNr;Navn,Individ;","where true,FIRST Individ Where Individ.kkundenr = kunde.kundenr and Individ.IndividType = " + STRING(iTab), INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    fi-cNavn:SCREEN-VALUE = cLookupValue.
/*     fi-cNavn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Kunde",                         */
/*                                           "WHERE KundeNr = dec(" + cLookupValue + ")","Navn"). */
    RUN OpenQuery.
  END.
  RETURN NO-APPLY.
END.

/*   cLookupValue = "Vg".                                                                                            */
/*   RUN JBoxDLookup.w ("VarGr;Vg;VgBeskr,Individ;","where true,FIRST Individ OF VarGr", INPUT-OUTPUT cLookupValue). */
/*                                                                                                                   */
/*   IF cLookupValue NE "" THEN DO:                                                                                  */
/*     fi-iVg:SCREEN-VALUE = cLookupValue.                                                                           */
/*     /*DYNAMIC-FUNCTION("setToolbar",hUpdToolBar,"modified").*/                                                    */
/*     RUN OpenQuery.                                                                                                */
/*   END.                                                                                                            */
/*   RETURN NO-APPLY.                                                                                                */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAktiverFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAktiverFilter C-Win
ON CHOOSE OF btnAktiverFilter IN FRAME frmFilter /* Aktiver filter */
DO:
   RUN OpenQuery.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBlankFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBlankFilter C-Win
ON CHOOSE OF btnBlankFilter IN FRAME frmFilter /* Blank filter */
DO:
  ASSIGN
      fi-fArtikkelNr:SCREEN-VALUE = ""
      fi-cBeskr:SCREEN-VALUE = ""
      fi-cVmBeskrivelse:SCREEN-VALUE = ""
      fi-fIndividNr:SCREEN-VALUE = ""
      fi-cSerieNr:SCREEN-VALUE = ""
      fi-cGarantiNr:SCREEN-VALUE = ""
      fi-cNavn:SCREEN-VALUE = ""
      fi-cTelefon:SCREEN-VALUE = ""
      fi-cMobilTlf:SCREEN-VALUE = ""
      fi-cAdresse1:SCREEN-VALUE = ""
      fi-cPersOrgNr:SCREEN-VALUE = ""
      fi-cVapenkort:SCREEN-VALUE = ""
      fi-cJegerkort:SCREEN-VALUE = ""
      cmbButikk:SCREEN-VALUE = "0"
      fi-dFraSalgDato:SCREEN-VALUE = ""
      fi-dTilSalgDato:SCREEN-VALUE = ""
      fi-dFraKjoptDato:SCREEN-VALUE = ""
      fi-dTilKjoptDato:SCREEN-VALUE = ""
      fi-iBatchNr:SCREEN-VALUE = ""
      fi-iVg:SCREEN-VALUE = ""
      tbIFeilVare:SCREEN-VALUE = "no"
      .
   RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokVg C-Win
ON CHOOSE OF btnSokVg IN FRAME frmFilter /* ... */
DO:
  DEF VAR cLookupValue  AS CHAR NO-UNDO.
  DEF VAR cVarGrValues AS CHAR NO-UNDO.

  cLookupValue = "Vg".
  RUN JBoxDLookup.w ("VarGr;Vg;VgBeskr,Individ;","where true,FIRST Individ OF VarGr where Individ.IndividType = " + STRING(iTab), INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    fi-iVg:SCREEN-VALUE = cLookupValue.
    /*DYNAMIC-FUNCTION("setToolbar",hUpdToolBar,"modified").*/
    RUN OpenQuery.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbButikk C-Win
ON VALUE-CHANGED OF cmbButikk IN FRAME frmFilter /* Butikk */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cAdresse1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cAdresse1 C-Win
ON RETURN OF fi-cAdresse1 IN FRAME frmFilter /* Adresse */
OR "TAB":U OF FI-cAdresse1
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cBeskr C-Win
ON RETURN OF fi-cBeskr IN FRAME frmFilter /* Varetekst */
OR "TAB":U OF FI-cBeskr
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cGarantiNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cGarantiNr C-Win
ON RETURN OF fi-cGarantiNr IN FRAME frmFilter /* GarantiNr */
OR "TAB":U OF fi-cGarantiNr
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cJegerkort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cJegerkort C-Win
ON RETURN OF fi-cJegerkort IN FRAME frmFilter /* Jegerkort */
OR "TAB":U OF FI-cJegerkort
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cMobilTlf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cMobilTlf C-Win
ON RETURN OF fi-cMobilTlf IN FRAME frmFilter
OR "TAB":U OF FI-cMobilTlf
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cNavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cNavn C-Win
ON RETURN OF fi-cNavn IN FRAME frmFilter /* Kundenavn */
OR "TAB":U OF FI-cNavn
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cPersOrgNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cPersOrgNr C-Win
ON RETURN OF fi-cPersOrgNr IN FRAME frmFilter /* Pers/OrgNr */
OR "TAB":U OF FI-cPersOrgNr
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cSerieNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cSerieNr C-Win
ON RETURN OF fi-cSerieNr IN FRAME frmFilter /* SerieNr */
OR "TAB":U OF FI-cSerieNr
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cTelefon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cTelefon C-Win
ON RETURN OF fi-cTelefon IN FRAME frmFilter /* Telefon/Mobil */
OR "TAB":U OF FI-cTelefon
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cVapenKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cVapenKort C-Win
ON RETURN OF fi-cVapenKort IN FRAME frmFilter /* Våpenkort */
OR "TAB":U OF FI-cVapenKort
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cVmBeskrivelse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cVmBeskrivelse C-Win
ON RETURN OF fi-cVmBeskrivelse IN FRAME frmFilter /* Varemerke */
OR "TAB":U OF FI-cVmBeskrivelse
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dFraKjoptDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dFraKjoptDato C-Win
ON RETURN OF fi-dFraKjoptDato IN FRAME frmFilter /* Kjøpt */
OR "TAB":U OF FI-dFraKjoptDato
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dFraSalgDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dFraSalgDato C-Win
ON RETURN OF fi-dFraSalgDato IN FRAME frmFilter /* Solgt */
OR "TAB":U OF FI-dTilSalgDato
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dTilKjoptDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dTilKjoptDato C-Win
ON RETURN OF fi-dTilKjoptDato IN FRAME frmFilter
OR "TAB":U OF FI-dTilKjoptDato
DO:
  RUN OpenQuery.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dTilSalgDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dTilSalgDato C-Win
ON RETURN OF fi-dTilSalgDato IN FRAME frmFilter
OR "TAB":U OF FI-dTilSalgDato
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fArtikkelNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fArtikkelNr C-Win
ON RETURN OF fi-fArtikkelNr IN FRAME frmFilter /* ArtikkelNr */
OR "TAB":U OF FI-fArtikkelNr
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fIndividNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fIndividNr C-Win
ON RETURN OF fi-fIndividNr IN FRAME frmFilter /* IndividNr */
OR "TAB":U OF FI-fIndividNr
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-iBatchNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-iBatchNr C-Win
ON RETURN OF fi-iBatchNr IN FRAME frmFilter /* BatchNr */
OR "TAB":U OF FI-iBatchNr
DO:
  RUN OpenQuery.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-iVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-iVg C-Win
ON RETURN OF fi-iVg IN FRAME frmFilter /* Varegr */
OR "TAB":U OF FI-iVg
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
&Scoped-define SELF-NAME tbIFeilVare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbIFeilVare C-Win
ON RETURN OF tbIFeilVare IN FRAME frmFilter /* Kun feilvarer */
OR "TAB" OF tbIFeilVare 
DO:
    APPLY "ENTRY":U TO fi-fArtikkelNr IN FRAME frmFilter.
    RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbIFeilVare C-Win
ON VALUE-CHANGED OF tbIFeilVare IN FRAME frmFilter /* Kun feilvarer */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
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
  IF VALID-HANDLE(hIndividDet) THEN APPLY "close" TO hIndividDet.
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

on F2,ALT-S of hWindow anywhere 
do:
    run d-hsokindivid.w (output fi-fArtikkelNr,""). 
    if fi-fArtikkelNr = ? then
        RETURN NO-APPLY.
    ELSE DO WITH FRAME frmFilter:
        ASSIGN
            fi-fArtikkelNr:SCREEN-VALUE = STRING(fi-fArtikkelNr)
            .
        RUN OpenQuery.
    END.
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
  {lng.i} /* Oversettelse */
  RUN InitWindow.
  DYNAMIC-FUNCTION("DoLockWindow",?).

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

ON 'default-action':U OF hBrowseSykkel, hBrowseVapen, hBrowseElektro
DO:
    IF NOT VALID-HANDLE(hIndividDet) THEN
    DO:
        RUN IndividDet.w PERSIST SET hIndividDet (THIS-PROCEDURE).
        RUN InitFromBrowse IN hIndividDet (getBrowseHandle(),iTab).
    END.

    RUN MoveToTop IN hIndividDet.
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

OCXFile = SEARCH( "individ.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chTabStrip = TabStrip:COM-HANDLE
    UIB_S = chTabStrip:LoadControls( OCXFile, "TabStrip":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "individ.wrx":U SKIP(1)
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
  Parameters:  <none>         getBrowseHandle()
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR phBrowse AS HANDLE NO-UNDO.

  IF VALID-HANDLE(hIndividDet) THEN
      RETURN. /* Sletting kan gjøres i detaljrutien. */

  ASSIGN
      phBrowse = getBrowseHandle()
      .
                                           
  APPLY 'default-action':U TO phBrowse.
  IF VALID-HANDLE(hIndividDet) THEN
  DO:
      RUN deleteRecord IN hIndividDet.
      APPLY "close" TO hIndividDet.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE editRecord C-Win 
PROCEDURE editRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  CASE iTab:
      WHEN 1 THEN APPLY 'default-action':U TO hBrowseSykkel.
      WHEN 2 THEN APPLY 'default-action':U TO hBrowseVapen.
      WHEN 3 THEN APPLY 'default-action':U TO hBrowseElektro.
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
  ENABLE rectToolBar rectEditToolbar 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY fi-iVg fi-fArtikkelNr fi-cBeskr fi-cVmBeskrivelse fi-fIndividNr 
          fi-cSerieNr fi-cGarantiNr fi-cNavn fi-cTelefon fi-cMobilTlf 
          fi-cAdresse1 fi-cPersOrgNr fi-cVapenKort fi-cJegerkort cmbButikk 
          fi-dFraSalgDato fi-dTilSalgDato fi-dFraKjoptDato fi-dTilKjoptDato 
          fi-iBatchNr tbIFeilVare 
      WITH FRAME frmFilter IN WINDOW C-Win.
  ENABLE B-SokArtikkel btnSokVg B-SokKunde btnBlankFilter fi-iVg b-Kalk-2 
         fi-fArtikkelNr fi-cBeskr fi-cVmBeskrivelse fi-fIndividNr fi-cSerieNr 
         fi-cGarantiNr fi-cNavn fi-cTelefon fi-cMobilTlf fi-cAdresse1 
         fi-cPersOrgNr fi-cVapenKort fi-cJegerkort cmbButikk fi-dFraSalgDato 
         fi-dTilSalgDato fi-dFraKjoptDato fi-dTilKjoptDato fi-iBatchNr 
         tbIFeilVare b-Kalk-4 b-Kalk1 b-Kalk3 btnAktiverFilter 
      WITH FRAME frmFilter IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmFilter}
  ENABLE rectBrowseElektro RectBrowseSearchElektro 
      WITH FRAME frmElektro IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmElektro}
  ENABLE rectBrowseSykkel RectBrowseSearchSykkel 
      WITH FRAME frmSykkel IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmSykkel}
  ENABLE rectBrowseVapen RectBrowseSearchVapen 
      WITH FRAME frmVapen IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmVapen}
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
      WHEN 1 THEN DYNAMIC-FUNCTION("toExcelViaFile",hBrowseSykkel,0).         
      WHEN 2 THEN DYNAMIC-FUNCTION("toExcelViaFile",hBrowseVapen,0).         
      WHEN 3 THEN DYNAMIC-FUNCTION("toExcelViaFile",hBrowseElektro,0).         
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

DO WITH FRAME frmFilter:
    cmbButikk:DELIMITER = "|".
    cmbButikk:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("GetFieldList","Butiker;Butik|ButNamn;Butik","WHERE TRUE BY ButNamn").
    cmbButikk:SCREEN-VALUE = "0".
END.
DO WITH FRAME frmSykkel:
  hBrowseSykkel = DYNAMIC-FUNCTION("NewBrowse",         /* Create a browse object */
                    rectBrowseSykkel:HANDLE IN FRAME frmSykkel,            /* Rectangle to define coordinates for browse */
                    200,                            /* Rows to batch */
                    "SINGLE,NUM-LOCKED-COLUMNS|4",                     /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    /*
                    "Gavekort;butnr;IdentType;Dato|Reg.dato;+cRegTid|CHARACTER|x(5)|gavekort_regtid.p|Tid;Gyldigdato;Bruktdato;KasseNr;BongNr;Belop;!Tid," + 
                      "GaveKType;GKTBeskrivelse",                     /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/
                    */
                    "Individ;individnr|Individ;ArtikkelNr|ArtikkelNr;Beskr|Varetekst;VmBeskrivelse|VareMrk;SerieNr;Garantinummer;ButNr|Butikk" +
                    ";feilvare;KKundeNr;Navn;Telefon;MobilTlf;Adresse1|Adresse;ePostAdresse;PersOrgNr;SalgDato|Solgt|99/99/99;Pris|Pris;KjoptDato|Kjøpt|99/99/99;!individType" + 
                    ";!SeqNr;!NyVare;!StrKode;!Storl;!Kaliber;!Adresse2;!PostNr;!PostSted;!Vapenkort;!Jegerkort;!LevNr;!LevNamn;!DbKr;!Db%;!VareKost;!VVAreKost;!SNAvn;!SAdresse1;!SAdresse2;!SPostNR;!SPostSted;!STelefon;!SMobilTlf" +
                    ";AvdelingNr|Avd;Hg|Hg;Vg|Vg;BatchNr;ForsNr|KassererNr;SelgerNr," + 
                    "Forsalj;ForsNr;FoNamn", /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/
                    "WHERE Individ.individType = 1, FIRST Forsalj NO-LOCK where Forsalj.ForsNr = Individ.ForsNr OUTER-JOIN",
                    "sort|IndividNr").                            /* Misc - for something I might need in next version.. */
  hBrowseSykkel:NAME = "brwSykkel". /* This name is neccessary because the browser is due to special treatment during resize */
  hBufferSykkel = hBrowseSykkel:QUERY:GET-BUFFER-HANDLE(1).
  /*
  /* Flytter kolonner */
  hBrowseSykkel:MOVE-COLUMN(10,3).
  /* Endring av labler på kolonner */
  hBrowseSykkel:GET-BROWSE-COLUMN(2):LABEL = "Type".
  */
  /* Endring av bredde på kollonner */
/*   hBrowseSykkel:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 160. */
/*   hBrowseSykkel:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 60. */

  DYNAMIC-FUNCTION("setAttribute",hBrowseSykkel,"basequery","WHERE Individ.individType = 1").
  DYNAMIC-FUNCTION("setAttribute",hBrowseSykkel,"querywhere","").
  /*DYNAMIC-FUNCTION("setAttribute",hBrowseSykkel,"basequery","WHERE Individ.individType = 1 ").*/
  /*DYNAMIC-FUNCTION("setAttribute",hBrowseSykkel,"sortmap","cRegTid;Tid").*/

END.
DO WITH FRAME frmVapen:
  hBrowseVapen = DYNAMIC-FUNCTION("NewBrowse",         /* Create a browse object */
                    rectBrowseVapen:HANDLE IN FRAME frmVapen,            /* Rectangle to define coordinates for browse */
                    200,                            /* Rows to batch */
                    "SINGLE,NUM-LOCKED-COLUMNS|4",                     /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    /*
                    "Gavekort;butnr;IdentType;Dato|Reg.dato;+cRegTid|CHARACTER|x(5)|gavekort_regtid.p|Tid;Gyldigdato;Bruktdato;KasseNr;BongNr;Belop;!Tid," + 
                      "GaveKType;GKTBeskrivelse",                     /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/
                    */
                    "Individ;individnr;ArtikkelNr|ArtikkelNr;Beskr|Varetekst;VmBeskrivelse|VareMrk;SerieNr;Garantinummer" +
                    ";feilvare;KKundeNr;Navn;Telefon;MobilTlf;Adresse1|Adresse;ePostAdresse;PersOrgNr;VapenKort;Jegerkort;SalgDato|Solgt;Pris|Pris;KjoptDato|Kjøpt;!individType;!ButNr" + 
                    ";!SeqNr;!NyVare;!StrKode;!Storl;!Kaliber;!Adresse2;!PostNr;!PostSted;!Vapenkort;!Jegerkort;!LevNr;!LevNamn;!DbKr;!Db%;!VareKost;!VVAreKost;!SNAvn;!SAdresse1;!SAdresse2;!SPostNR;!SPostSted;!STelefon;!SMobilTlf" +
                    ";AvdelingNr|Avd;Hg|Hg;Vg|Vg;BatchNr;ForsNr|KassererNr;SelgerNr," + 
                    "Forsalj;ForsNr;FoNamn", /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/
                    "WHERE Individ.individType = 2, FIRST Forsalj NO-LOCK where Forsalj.ForsNr = Individ.ForsNr OUTER-JOIN",
                    "sort|IndividNr").                            /* Misc - for something I might need in next version.. */
  hBrowseVapen:NAME = "brwVapen". /* This name is neccessary because the browser is due to special treatment during resize */
  hBufferVapen = hBrowseVapen:QUERY:GET-BUFFER-HANDLE(1).
  /*
  /* Flytter kolonner */
  hBrowseSykkel:MOVE-COLUMN(10,3).
  /* Endring av labler på kolonner */
  hBrowseSykkel:GET-BROWSE-COLUMN(2):LABEL = "Type".
  */
  /* Endring av bredde på kollonner */
/*   hBrowseVapen:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 160. */
/*   hBrowseVapen:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 60.  */

  DYNAMIC-FUNCTION("setAttribute",hBrowseVapen,"basequery","WHERE Individ.individType = 2").
  DYNAMIC-FUNCTION("setAttribute",hBrowseVapen,"querywhere","").
  /*DYNAMIC-FUNCTION("setAttribute",hBrowseSykkel,"basequery","WHERE Individ.individType = 1 ").*/
  /*DYNAMIC-FUNCTION("setAttribute",hBrowseSykkel,"sortmap","cRegTid;Tid").*/

END.
DO WITH FRAME frmElektro:
  hBrowseElektro = DYNAMIC-FUNCTION("NewBrowse",         /* Create a browse object */
                    rectBrowseElektro:HANDLE IN FRAME frmElektro,            /* Rectangle to define coordinates for browse */
                    200,                            /* Rows to batch */
                    "SINGLE,NUM-LOCKED-COLUMNS|4",                     /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    /*
                    "Gavekort;butnr;IdentType;Dato|Reg.dato;+cRegTid|CHARACTER|x(5)|gavekort_regtid.p|Tid;Gyldigdato;Bruktdato;KasseNr;BongNr;Belop;!Tid," + 
                      "GaveKType;GKTBeskrivelse",                     /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/
                    */
                    "Individ;individnr;ArtikkelNr|ArtikkelNr;Beskr|Varetekst;VmBeskrivelse|VareMrk;SerieNr;Garantinummer" +
                    ";feilvare;KKundeNr;Navn;Telefon;MobilTlf;Adresse1|Adresse;ePostAdresse;PersOrgNr;SalgDato|Solgt;Pris|Pris;KjoptDato|Kjøpt;!individType;!ButNr" + 
                    ";!SeqNr;!NyVare;!StrKode;!Storl;!Kaliber;!Adresse2;!PostNr;!PostSted;!Vapenkort;!Jegerkort;!LevNr;!LevNamn;!DbKr;!Db%;!VareKost;!VVAreKost;!SNAvn;!SAdresse1;!SAdresse2;!SPostNR;!SPostSted;!STelefon;!SMobilTlf" +
                    ";AvdelingNr|Avd;Hg|Hg;Vg|Vg;BatchNr;ForsNr|KassererNr;SelgerNr," + 
                    "Forsalj;ForsNr;FoNamn", /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/
                    "WHERE Individ.individType = 3, FIRST Forsalj NO-LOCK where Forsalj.ForsNr = Individ.ForsNr OUTER-JOIN",
                    "sort|IndividNr").                            /* Misc - for something I might need in next version.. */
  hBrowseElektro:NAME = "brwElektro". /* This name is neccessary because the browser is due to special treatment during resize */
  hBufferElektro = hBrowseElektro:QUERY:GET-BUFFER-HANDLE(1).
  /*
  /* Flytter kolonner */
  hBrowseSykkel:MOVE-COLUMN(10,3).
  /* Endring av labler på kolonner */
  hBrowseSykkel:GET-BROWSE-COLUMN(2):LABEL = "Type".
  */
  /* Endring av bredde på kollonner */
/*   hBrowseElektro:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 160. */
/*   hBrowseElektro:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 60.  */

  DYNAMIC-FUNCTION("setAttribute",hBrowseElektro,"basequery","WHERE Individ.individType = 3").
  DYNAMIC-FUNCTION("setAttribute",hBrowseElektro,"querywhere","").
  /*DYNAMIC-FUNCTION("setAttribute",hBrowseSykkel,"basequery","WHERE Individ.individType = 1 ").*/
  /*DYNAMIC-FUNCTION("setAttribute",hBrowseSykkel,"sortmap","cRegTid;Tid").*/

END.

DO WITH FRAME {&FRAME-NAME}:
  
  hEditToolbar = DYNAMIC-FUNCTION("NewToolBar",
                        rectEditToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                        "Fil",                          /* Corresponding menu label - no menu if blank */
                        "New;Ny,Edit;Endre,delete;Slett,excel;Eksporter til E&xcel",
                                                        /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                           Any number of properties accepted (one ok - if predef. action) */
                        "").                   /* Misc - for something I might need in next version.. */
  hToolBar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                          /* Corresponding menu label - no menu if blank */
                    "Help|Hjelp,Close;Avslutt",
                                                    /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    "maxborder").                   /* Misc - for something I might need in next version.. */

  DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearchSykkel:HANDLE,hBrowseSykkel,1).
  DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearchVapen:HANDLE,hBrowseVapen,1).
  DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearchElektro:HANDLE,hBrowseElektro,1).

  /* Link objects: */

  DYNAMIC-FUNCTION("LinkAllObjects",                /* Link all created objects. Linktype is type of "to" object,
                                                      f.ex link from browse to combo-box is combo-box link */
                    THIS-PROCEDURE:CURRENT-WINDOW,  /* Link only objects created for current window */
                    TRUE,                           /* Replace any existing links */
                    STRING(hToolBar) + "," + STRING(hEditToolbar)).  /* Except these objects */

/*   DYNAMIC-FUNCTION("DumpAllLinks",THIS-PROCEDURE:CURRENT-WINDOW,"c:\temp\links.txt").  */

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar").
  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,800,500,0,0).

  DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  DYNAMIC-FUNCTION("SetToolbar",hEditToolBar,"enable").
  DYNAMIC-FUNCTION("SetToolbar",hToolBar,"enable").

END.

TabStripChanged(11).

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
DEF VAR hBrowse AS HANDLE NO-UNDO.

    IF NOT VALID-HANDLE(hIndividDet) THEN
    DO:
        hBrowse = getBrowseHandle().
        APPLY "default-action" TO hBrowse.
        RUN NewRecord IN hIndividDet.
/*         RUN IndividDet.w PERSIST SET hIndividDet (THIS-PROCEDURE). */
/*         RUN InitFromKey IN hIndividDet (iTab,'?'). */
    END.

    RUN MoveToTop IN hIndividDet.

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
      hHandle = (IF iTab = 1 THEN hBrowseSykkel
                 ELSE IF iTab = 2 THEN hBrowseVapen
                 ELSE hBrowseElektro)
      .

  DO WITH FRAME frmFilter:
    DYNAMIC-FUNCTION("SetAttribute",hHandle,"queryfilter",

                     (IF fi-fIndividNr:SCREEN-VALUE NE "0" THEN
                       " AND IndividNr = " + fi-fIndividNr:SCREEN-VALUE
                      ELSE "") +

                     (IF fi-fArtikkelNr:SCREEN-VALUE NE "0" THEN
                       " AND ArtikkelNr = " + fi-fArtikkelNr:SCREEN-VALUE
                      ELSE "") +
                     (IF cmbButikk:SCREEN-VALUE NE "0" THEN
                       " AND butnr = " + cmbButikk:SCREEN-VALUE
                      ELSE "") +
                     (IF fi-iBatchNr:SCREEN-VALUE NE "0" THEN
                       " AND BatchNr = " + fi-iBatchNr:SCREEN-VALUE
                      ELSE "") + 
                     (IF fi-iVg:SCREEN-VALUE NE "0" THEN
                       " AND Vg = " + fi-iVg:SCREEN-VALUE
                      ELSE "") + 
                     (IF INPUT tbIFeilVare THEN
                       " AND FeilVare = yes"
                      ELSE "") +
                     (IF fi-cBeskr:SCREEN-VALUE NE "" THEN
                        IF fi-cBeskr:SCREEN-VALUE BEGINS "*" THEN
                         " AND Beskr MATCHES '" + fi-cBeskr:SCREEN-VALUE + "*'"
                        ELSE
                         " AND Beskr BEGINS '" + fi-cBeskr:SCREEN-VALUE + "'"
                      ELSE "") +
                     (IF fi-cPersOrgNr:SCREEN-VALUE NE "" THEN
                        IF fi-cPersOrgNr:SCREEN-VALUE BEGINS "*" THEN
                         " AND PersOrgNr MATCHES '" + fi-cPersOrgNr:SCREEN-VALUE + "*'"
                        ELSE
                         " AND PersOrgNr BEGINS '" + fi-cPersOrgNr:SCREEN-VALUE + "'"
                      ELSE "") +
                     (IF fi-cVmBeskrivelse:SCREEN-VALUE NE "" THEN
                        IF fi-cVmBeskrivelse:SCREEN-VALUE BEGINS "*" THEN
                         " AND VmBeskrivelse MATCHES '" + fi-cVmBeskrivelse:SCREEN-VALUE + "*'"
                        ELSE
                         " AND VmBeskrivelse BEGINS '" + fi-cVmBeskrivelse:SCREEN-VALUE + "'"
                      ELSE "") +
                     (IF fi-cSerieNr:SCREEN-VALUE NE "" THEN
                        IF fi-cSerieNr:SCREEN-VALUE BEGINS "*" THEN
                         " AND SerieNr MATCHES '" + fi-cSerieNr:SCREEN-VALUE + "*'"
                        ELSE
                         " AND SerieNr BEGINS '" + fi-cSerieNr:SCREEN-VALUE + "'"
                      ELSE "") +
                     (IF fi-cGarantiNr:SCREEN-VALUE NE "" THEN
                        IF fi-cGarantiNr:SCREEN-VALUE BEGINS "*" THEN
                         " AND Garantinummer MATCHES '" + fi-cGarantiNr:SCREEN-VALUE + "*'"
                        ELSE
                         " AND Garantinummer BEGINS '" + fi-cGarantiNr:SCREEN-VALUE + "'"
                      ELSE "") +
                     (IF fi-cNavn:SCREEN-VALUE NE "" THEN
                        IF fi-cNavn:SCREEN-VALUE BEGINS "*" THEN
                         " AND Navn MATCHES '" + fi-cNavn:SCREEN-VALUE + "*'"
                        ELSE
                         " AND Navn BEGINS '" + fi-cNavn:SCREEN-VALUE + "'"
                      ELSE "") +
                     (IF fi-cTelefon:SCREEN-VALUE NE "" THEN
                        IF fi-cTelefon:SCREEN-VALUE BEGINS "*" THEN
                         " AND Telefon MATCHES '" + fi-cTelefon:SCREEN-VALUE + "*'"
                        ELSE
                         " AND Telefon BEGINS '" + fi-cTelefon:SCREEN-VALUE + "'"
                      ELSE "") +
                     (IF fi-cMobilTlf:SCREEN-VALUE NE "" THEN
                        IF fi-cMobilTlf:SCREEN-VALUE BEGINS "*" THEN
                         " AND MobilTlf MATCHES '" + fi-cMobilTlf:SCREEN-VALUE + "*'"
                        ELSE
                         " AND MobilTlf BEGINS '" + fi-cMobilTlf:SCREEN-VALUE + "'"
                      ELSE "") +
                     (IF fi-cAdresse1:SCREEN-VALUE NE "" THEN
                        IF fi-cAdresse1:SCREEN-VALUE BEGINS "*" THEN
                         " AND Adresse1 MATCHES '" + fi-cAdresse1:SCREEN-VALUE + "*'"
                        ELSE
                         " AND Adresse1 BEGINS '" + fi-cAdresse1:SCREEN-VALUE + "'"
                      ELSE "") +
                     (IF fi-cVapenkort:SCREEN-VALUE NE "" THEN
                        IF fi-CVapenkort:SCREEN-VALUE BEGINS "*" THEN
                         " AND Vapenkort MATCHES '" + fi-cVapenkort:SCREEN-VALUE + "*'"
                        ELSE
                         " AND Vapenkort BEGINS '" + fi-cVapenkort:SCREEN-VALUE + "'"
                      ELSE "") +
                     (IF fi-cJegerkort:SCREEN-VALUE NE "" THEN
                        IF fi-cJegerkort:SCREEN-VALUE BEGINS "*" THEN
                         " AND Jegerkort MATCHES '" + fi-cJegerkort:SCREEN-VALUE + "*'"
                        ELSE
                         " AND Jegerkort BEGINS '" + fi-cJegerkort:SCREEN-VALUE + "'"
                      ELSE "") +
                     (IF INPUT fi-dFraSalgDato NE ? THEN
                      " AND SalgDato GE " + STRING(INPUT fi-dFraSalgDato,"99/99/99")
                      ELSE "") +
                     (IF INPUT fi-dTilSalgDato NE ? THEN
                      " AND SalgDato GE " + STRING(INPUT fi-dTilSalgDato,"99/99/99")
                      ELSE "") +
                     (IF INPUT fi-dTilKjoptDato NE ? THEN
                      " AND KjoptDato GE " + STRING(INPUT fi-dTilKjoptDato,"99/99/99")
                      ELSE "") +
                     (IF INPUT fi-dTilKjoptDato NE ? THEN
                      " AND KjoptDato GE " + STRING(INPUT fi-dTilKjoptDato,"99/99/99")
                      ELSE "") 
                     ).
  END.
  IF iTab = 1 THEN DO WITH FRAME frmSykkel:
      DYNAMIC-FUNCTION("SetCurrentObject",hBrowseSykkel).
  END.
  ELSE IF iTab = 2 THEN DO WITH FRAME frmVapen:
      DYNAMIC-FUNCTION("SetCurrentObject",hBrowseVapen).
  END.
  ELSE IF iTab = 3 THEN DO WITH FRAME frmElektro:
      DYNAMIC-FUNCTION("SetCurrentObject",hBrowseElektro).
  END.

  RUN SUPER.

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
  RETURN hBrowseSykkel.
ELSE IF iTab = 2 THEN
  RETURN hBrowseVapen.
ELSE IF iTab = 3 THEN
  RETURN hBrowseElektro.

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

cTabStripList = "Sykkel|Våpen|Elektro".

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

DO WITH FRAME {&FRAME-NAME}:

  IF iiTab > 10 THEN DO:
    iiTab = iiTab - 10.
    chTabStrip:TabStrip:Tabs:ITEM(iiTab):SELECTED = TRUE.
  END.

  iTab = iiTab.

  FRAME frmFilter:MOVE-TO-TOP().
  IF iiTab = 1 THEN
  DO:
      FRAME frmSykkel:MOVE-TO-TOP().
      ASSIGN
          fi-cVapenKort:HIDDEN = TRUE
          fi-cJegerKort:HIDDEN = TRUE
          .
      IF VALID-HANDLE(hIndividDet) THEN
        DELETE PROCEDURE hIndividDet.
  END.
  ELSE IF iiTab = 2 THEN
  DO:
      FRAME frmVapen:MOVE-TO-TOP().
      ASSIGN 
          fi-cVapenKort:HIDDEN = FALSE
          fi-cJegerKort:HIDDEN = FALSE
          .
      IF VALID-HANDLE(hIndividDet) THEN
        DELETE PROCEDURE hIndividDet.
  END.
  IF iiTab = 3 THEN
  DO:
      FRAME frmElektro:MOVE-TO-TOP().
      ASSIGN 
          fi-cVapenKort:HIDDEN = TRUE
          fi-cJegerKort:HIDDEN = TRUE
          .
      IF VALID-HANDLE(hIndividDet) THEN
        DELETE PROCEDURE hIndividDet.
  END.
  APPLY "choose" TO btnBlankFilter in FRAME frmFilter.
  APPLY "ENTRY":U TO fi-fArtikkelNr.

  RETURN TRUE.   /* Function return value. */
END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

