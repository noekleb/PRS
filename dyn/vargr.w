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

DEF VAR bOK              AS LOG NO-UNDO.
DEF VAR ix               AS INT NO-UNDO.
DEF VAR iReturn          AS INT NO-UNDO.

DEF VAR hToolbar         AS HANDLE NO-UNDO.
DEF VAR hUpdToolbar      AS HANDLE NO-UNDO.
DEF VAR hBrowseListe     AS HANDLE NO-UNDO.
DEF VAR hSearchListe     AS HANDLE NO-UNDO.
DEF VAR hBufferListe     AS HANDLE NO-UNDO.
DEF VAR hFieldMap        AS HANDLE NO-UNDO.
DEF VAR hBrowseKategori  AS HANDLE NO-UNDO.
DEF VAR hBufferKategori  AS HANDLE NO-UNDO.
DEF VAR hBrowseAktivitet AS HANDLE NO-UNDO.
DEF VAR hBufferAktivitet AS HANDLE NO-UNDO.
DEF VAR hBrowseKndGrp    AS HANDLE NO-UNDO.
DEF VAR hBufferKndGrp    AS HANDLE NO-UNDO.
DEF VAR cJoinHuvGr       AS CHAR   NO-UNDO.
DEF VAR cJoinAvdeling    AS CHAR   NO-UNDO.
DEF VAR cJoinMoms        AS CHAR   NO-UNDO.
DEF VAR hBrwListeMenu    AS HANDLE NO-UNDO.
DEF VAR hPasteRabattMenuItem   AS HANDLE NO-UNDO.
DEF VAR hPasteKategoriMenuItem AS HANDLE NO-UNDO.
DEF VAR hPasteAktivitetMenuItem AS HANDLE NO-UNDO.

DEF VAR cAdgang         AS CHAR NO-UNDO.
DEF VAR hIndDet         AS HANDLE NO-UNDO.
DEF VAR hTLdet          AS HANDLE NO-UNDO.

DEF VAR iTab            AS INT NO-UNDO.

DEF VAR cState          AS CHAR  NO-UNDO.
DEF VAR cTekst          AS CHAR NO-UNDO.

DEF VAR cKategoriRowIdList  AS CHAR   NO-UNDO.
DEF VAR cKategoriIdList     AS CHAR   NO-UNDO.
DEF VAR cAktivitetRowIdList  AS CHAR   NO-UNDO.
DEF VAR cAktivitetIdList     AS CHAR   NO-UNDO.
DEF VAR cKndGrpRowIdList    AS CHAR   NO-UNDO.
DEF VAR cKndGrpIdList       AS CHAR   NO-UNDO.
DEF VAR hBrwFillIn          AS HANDLE NO-UNDO.

DEF VAR httVgKundeGrpRabatt AS HANDLE NO-UNDO.
DEF TEMP-TABLE ttVgKundeGrRabatt
    FIELD GruppeId AS INT
    FIELD Rabatt%  AS DEC.
httVgKundeGrpRabatt = BUFFER ttVgKundeGrRabatt:HANDLE:TABLE-HANDLE.

DEF VAR httVgAkt AS HANDLE NO-UNDO.
DEF TEMP-TABLE ttVgAkt
    FIELD AktNr    AS INT.
httVgAkt = BUFFER ttVgAkt:HANDLE:TABLE-HANDLE.

DEF VAR httVgKat AS HANDLE NO-UNDO.
DEF TEMP-TABLE ttVgKat
    FIELD KatNr    AS INT.
httVgKat = BUFFER ttVgKat:HANDLE:TABLE-HANDLE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRab%Lst C-Win 
FUNCTION getRab%Lst RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getVgList C-Win 
FUNCTION getVgList RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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

DEFINE BUTTON B-Aktivitet 
     LABEL "Velg aktivitet..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON B-Kategori 
     LABEL "Velg kategorier..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON B-KonvTbl 
     LABEL "Konverteringstabell..." 
     SIZE 22 BY 1.1.

DEFINE BUTTON B-Kundegruppe 
     LABEL "Velg kundegrupper..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON B-Mva_Kalkyle 
     LABEL "Oppdater mva. på kalkyle varegruppens artikler" 
     SIZE 62 BY 1.1.

DEFINE BUTTON B-oppdArtikler 
     LABEL "Oppdater rabattunderlag" 
     SIZE 28 BY 1.1.

DEFINE BUTTON btnKopierAktivitet 
     LABEL "Kopier" 
     SIZE 10 BY 1.14 TOOLTIP "Kopier kategorikobling for varegruppe".

DEFINE BUTTON btnKopierKategori 
     LABEL "Kopier" 
     SIZE 10 BY 1.14 TOOLTIP "Kopier kategorikobling for varegruppe".

DEFINE BUTTON btnKopierRabatt 
     LABEL "Kopier" 
     SIZE 10 BY 1.14 TOOLTIP "Kopier rabatt-oppsett for alle kundegrupper".

DEFINE BUTTON btnLimInnAktivitet 
     LABEL "Lim inn" 
     SIZE 10 BY 1.14 TOOLTIP "Lim inn kategorikobling for alle kundegruppe. NB! Eksiterende kobling slettes!".

DEFINE BUTTON btnLimInnKategori 
     LABEL "Lim inn" 
     SIZE 10 BY 1.14 TOOLTIP "Lim inn kategorikobling for alle kundegruppe. NB! Eksiterende kobling slettes!".

DEFINE BUTTON btnLimInnRabatt 
     LABEL "Lim inn" 
     SIZE 10 BY 1.14 TOOLTIP "Lim inn rabatt-oppsett for alle kundegrupper. NB! Eksisterende oppsett slettes!".

DEFINE BUTTON btnSokHg  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokMoms  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE Beskrivelse AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.

DEFINE VARIABLE FI-AktivitetTekst AS CHARACTER FORMAT "X(256)":U INITIAL "Aktiviteter" 
      VIEW-AS TEXT 
     SIZE 12.8 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Kategoritekst AS CHARACTER FORMAT "X(256)":U INITIAL "Kategorier" 
      VIEW-AS TEXT 
     SIZE 13.6 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-KndRabTekst AS CHARACTER FORMAT "X(256)":U INITIAL "Kunderabatter" 
      VIEW-AS TEXT 
     SIZE 17.6 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE Hg AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "Hovedgruppe" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE HgBeskr AS CHARACTER FORMAT "x(40)" 
     VIEW-AS FILL-IN 
     SIZE 35.4 BY 1.

DEFINE VARIABLE Kost_Proc AS DECIMAL FORMAT "zz9.9" INITIAL 0 
     LABEL "Kostnadsprosent" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE MomsKod AS INTEGER FORMAT "z9" INITIAL 0 
     LABEL "Momskode" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE MomsProc AS DECIMAL FORMAT "z9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE Vg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE VgBeskr AS CHARACTER FORMAT "x(40)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1.

DEFINE RECTANGLE rectAktivitet
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 14.76.

DEFINE RECTANGLE rectBrowseAktivitet
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 12.62.

DEFINE RECTANGLE rectBrowseKategori
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 12.62.

DEFINE RECTANGLE rectBrowseKndGrp
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 12.62.

DEFINE RECTANGLE rectKat
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 14.76.

DEFINE RECTANGLE rectKndRab
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 14.76.

DEFINE RECTANGLE rectVgr
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 143 BY 5.71.

DEFINE VARIABLE Bonus_Givende AS LOGICAL INITIAL yes 
     LABEL "Inngår i rabattunderlag (Medlemsklubb)" 
     VIEW-AS TOGGLE-BOX
     SIZE 66 BY .81 NO-UNDO.

DEFINE VARIABLE TillatLokalePriser AS LOGICAL INITIAL yes 
     LABEL "Tillat lokale priser" 
     VIEW-AS TOGGLE-BOX
     SIZE 65 BY .81 TOOLTIP "Butikker som har egen/annen prisprofil kan opprette lokal pris" NO-UNDO.

DEFINE BUTTON btnAvdeling 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnClearFilter 
     LABEL "Blank filter" 
     SIZE 22 BY 1.14.

DEFINE BUTTON btnHuvGr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE sokAvdelingNavn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE sokAvdelingNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Avdeling" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokHg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Hovedgruppe" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokHgBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE sokVg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokVgBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE RECTANGLE rectBrowseListe
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 175 BY 17.62.

DEFINE RECTANGLE RectBrowseSearchListe
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY 1.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectToolBar AT ROW 1.14 COL 169.2
     rectUpdToolbar AT ROW 1.14 COL 1.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 177.8 BY 25.29.

DEFINE FRAME frmListe
     sokAvdelingNr AT ROW 1.1 COL 20.2 COLON-ALIGNED HELP
          "Varegruppe"
     sokAvdelingNavn AT ROW 1.1 COL 31 COLON-ALIGNED NO-LABEL
     btnAvdeling AT ROW 1.14 COL 65.4
     sokHg AT ROW 2.14 COL 20.2 COLON-ALIGNED HELP
          "Varegruppe"
     sokHgBeskr AT ROW 2.19 COL 31 COLON-ALIGNED NO-LABEL
     btnHuvGr AT ROW 2.24 COL 65.4
     sokVg AT ROW 3.24 COL 20 COLON-ALIGNED HELP
          "Varegruppe"
     sokVgBeskr AT ROW 3.24 COL 31 COLON-ALIGNED NO-LABEL
     btnClearFilter AT ROW 3.24 COL 153.6
     rectBrowseListe AT ROW 5.76 COL 1
     RectBrowseSearchListe AT ROW 4.38 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.62
         SIZE 175 BY 22.38.

DEFINE FRAME frmDetalj
     btnSokHg AT ROW 4.81 COL 31 NO-TAB-STOP 
     B-KonvTbl AT ROW 1.24 COL 31.4
     B-oppdArtikler AT ROW 1.24 COL 53.4
     B-Mva_Kalkyle AT ROW 1.24 COL 82
     Vg AT ROW 2.91 COL 19 COLON-ALIGNED HELP
          "Varegruppe"
     TillatLokalePriser AT ROW 2.91 COL 75 HELP
          "Tillat lokale priser på artikler i varegruppen"
     VgBeskr AT ROW 3.86 COL 19 COLON-ALIGNED HELP
          "Kort beskrivelse av varegruppen"
     Bonus_Givende AT ROW 3.95 COL 75 HELP
          "Kjøp av på denne varegruppen er bonusgivende for medlemmer av medlemsklubb"
     Hg AT ROW 4.81 COL 19 COLON-ALIGNED HELP
          "Hovedgruppe som varegruppen er koblet til"
     HgBeskr AT ROW 4.81 COL 33.6 COLON-ALIGNED HELP
          "Kort beskrivelse av hovedgruppen" NO-LABEL
     MomsKod AT ROW 5.76 COL 19 COLON-ALIGNED HELP
          "Momskode som er koblet til varegruppen"
     MomsProc AT ROW 5.76 COL 33.6 COLON-ALIGNED NO-LABEL
     Beskrivelse AT ROW 5.76 COL 43 COLON-ALIGNED HELP
          "Kort beskrivelse av mva koden" NO-LABEL
     Kost_Proc AT ROW 6.71 COL 19 COLON-ALIGNED HELP
          "Kostnadsprosent"
     btnKopierKategori AT ROW 9.19 COL 2
     btnSokMoms AT ROW 5.76 COL 31 NO-TAB-STOP 
     btnLimInnKategori AT ROW 9.19 COL 12.2
     B-Kategori AT ROW 9.19 COL 26
     btnKopierAktivitet AT ROW 9.19 COL 49.6
     btnLimInnAktivitet AT ROW 9.19 COL 59.8
     B-Aktivitet AT ROW 9.19 COL 73.8
     btnKopierRabatt AT ROW 9.19 COL 97.4
     btnLimInnRabatt AT ROW 9.19 COL 107.6
     B-Kundegruppe AT ROW 9.19 COL 120.4
     FI-Kategoritekst AT ROW 8.24 COL 2.4 NO-LABEL
     FI-AktivitetTekst AT ROW 8.24 COL 51.2 NO-LABEL
     FI-KndRabTekst AT ROW 8.24 COL 97.6 NO-LABEL
     rectVgr AT ROW 2.43 COL 1
     rectKat AT ROW 8.62 COL 1
     rectKndRab AT ROW 8.62 COL 96.2
     rectBrowseKategori AT ROW 10.52 COL 2
     rectBrowseKndGrp AT ROW 10.52 COL 97.2
     rectAktivitet AT ROW 8.62 COL 48.6
     rectBrowseAktivitet AT ROW 10.52 COL 49.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.62
         SIZE 175 BY 22.38.


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
         TITLE              = "Varegrupperegister"
         HEIGHT             = 25.29
         WIDTH              = 177.8
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
/* SETTINGS FOR FRAME frmDetalj
                                                                        */
/* SETTINGS FOR FILL-IN Beskrivelse IN FRAME frmDetalj
   NO-ENABLE                                                            */
ASSIGN 
       btnLimInnAktivitet:HIDDEN IN FRAME frmDetalj           = TRUE.

ASSIGN 
       btnLimInnKategori:HIDDEN IN FRAME frmDetalj           = TRUE.

ASSIGN 
       btnLimInnRabatt:HIDDEN IN FRAME frmDetalj           = TRUE.

/* SETTINGS FOR FILL-IN FI-AktivitetTekst IN FRAME frmDetalj
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FI-AktivitetTekst:READ-ONLY IN FRAME frmDetalj        = TRUE.

/* SETTINGS FOR FILL-IN FI-Kategoritekst IN FRAME frmDetalj
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FI-Kategoritekst:READ-ONLY IN FRAME frmDetalj        = TRUE.

/* SETTINGS FOR FILL-IN FI-KndRabTekst IN FRAME frmDetalj
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FI-KndRabTekst:READ-ONLY IN FRAME frmDetalj        = TRUE.

/* SETTINGS FOR FILL-IN HgBeskr IN FRAME frmDetalj
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN MomsProc IN FRAME frmDetalj
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Vg IN FRAME frmDetalj
   NO-ENABLE                                                            */
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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmDetalj
/* Query rebuild information for FRAME frmDetalj
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frmDetalj */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmListe
/* Query rebuild information for FRAME frmListe
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frmListe */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME TabStrip ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.43
       COLUMN          = 1
       HEIGHT          = 23.81
       WIDTH           = 177
       HIDDEN          = no
       SENSITIVE       = yes.
/* TabStrip OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Varegrupperegister */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Varegrupperegister */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Varegrupperegister */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").

  DEF VAR hWidget AS HANDLE NO-UNDO.
  hWidget = hBrowseKndGrp:GET-BROWSE-COLUMN(3).
  APPLY "end-resize" TO hWidget.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmDetalj
&Scoped-define SELF-NAME B-Aktivitet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Aktivitet C-Win
ON CHOOSE OF B-Aktivitet IN FRAME frmDetalj /* Velg aktivitet... */
DO:
    /* Henter liste med koblinger */
    cAktivitetRowIdList = DYNAMIC-FUNCTION("getRowIdList","VgAkt,Aktivitet","",
                                         "WHERE Vg = " + Vg:SCREEN-VALUE IN FRAME frmDetalj +
                                         ",FIRST Aktivitet OF VgAkt NO-LOCK").

    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxSelector.w (THIS-PROCEDURE,0,
                        "Aktivitet;AktNr;Beskrivelse",
                        "where true",
                        INPUT-OUTPUT cAktivitetRowIdList,
                        "AktNr",
                        INPUT-OUTPUT cAktivitetIdList,
                        "","",
                        OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

    /* runProc,<4Gl procedurere>,<Input parametre>,<Handle til temp-tabell - ? hvis ikke brukt.>.                                       */
    /* Returverdi fra prodeduren kan hentes med funksjonen dynamic-function("getTransactionMessage") - gir returverdi = parameterverdi. */
    bOk = DYNAMIC-FUNCTION("runProc","lagreaktivitetkobling.p",Vg:SCREEN-VALUE + "|" + cAktivitetRowIdList,?).
    IF bOk THEN
        APPLY "value-changed" TO hBrowseListe.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Kategori
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Kategori C-Win
ON CHOOSE OF B-Kategori IN FRAME frmDetalj /* Velg kategorier... */
DO:
    /* Henter liste med koblinger */
    cKategoriRowIdList = DYNAMIC-FUNCTION("getRowIdList","VgKat,Kategori","",
                                         "WHERE Vg = " + Vg:SCREEN-VALUE IN FRAME frmDetalj +
                                         ",FIRST Kategori OF VgKat NO-LOCK").

    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxSelector.w (THIS-PROCEDURE,0,
                        "Kategori;KatNr;Beskrivelse",
                        "where true",
                        INPUT-OUTPUT cKategoriRowIdList,
                        "KatNr",
                        INPUT-OUTPUT cKategoriIdList,
                        "","",
                        OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

    /* runProc,<4Gl procedurere>,<Input parametre>,<Handle til temp-tabell - ? hvis ikke brukt.>.                                       */
    /* Returverdi fra prodeduren kan hentes med funksjonen dynamic-function("getTransactionMessage") - gir returverdi = parameterverdi. */
    bOk = DYNAMIC-FUNCTION("runProc","lagrekategorikobling.p",Vg:SCREEN-VALUE + "|" + cKategoriRowIdList,?).
    IF bOk THEN
        APPLY "value-changed" TO hBrowseListe.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KonvTbl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KonvTbl C-Win
ON CHOOSE OF B-KonvTbl IN FRAME frmDetalj /* Konverteringstabell... */
DO:
  run d-bimpkonv.w (1503,
                    Vg:SCREEN-VALUE,
                    "Kobling av varegrupper"
                    ).
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Kundegruppe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Kundegruppe C-Win
ON CHOOSE OF B-Kundegruppe IN FRAME frmDetalj /* Velg kundegrupper... */
DO:
    /* Henter liste med koblinger */
    cKndGrpRowIdList = DYNAMIC-FUNCTION("getRowIdList","VgKundeGrpRabatt,Kundegruppe","",
                                         "WHERE Vg = " + Vg:SCREEN-VALUE IN FRAME frmDetalj +
                                         ",FIRST Kundegruppe OF VgKundeGrpRabatt NO-LOCK").

    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxSelector.w (THIS-PROCEDURE,0,
                        "Kundegruppe;GruppeId;Beskrivelse",
                        "where true",
                        INPUT-OUTPUT cKndGrpRowIdList,
                        "GruppeId",
                        INPUT-OUTPUT cKndGrpIdList,
                        "","",
                        OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

    /* runProc,<4Gl procedurere>,<Input parametre>,<Handle til temp-tabell - ? hvis ikke brukt.>.                                       */
    /* Returverdi fra prodeduren kan hentes med funksjonen dynamic-function("getTransactionMessage") - gir returverdi = parameterverdi. */
    bOk = DYNAMIC-FUNCTION("runProc","lagrekndgrpkobling.p",Vg:SCREEN-VALUE + "|" + cKndGrpRowIdList,?).
    IF bOk THEN
        APPLY "value-changed" TO hBrowseListe.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Mva_Kalkyle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Mva_Kalkyle C-Win
ON CHOOSE OF B-Mva_Kalkyle IN FRAME frmDetalj /* Oppdater mva. på kalkyle varegruppens artikler */
DO:
  IF INT(Vg:SCREEN-VALUE IN FRAME frmDetalj) = 0 THEN
  DO:
      MESSAGE 'Varegruppe er ikke angitt.'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  bOk = FALSE.
  MESSAGE 'SJekker kalkyle på alle artikler som ligger i varegruppen. Regner om mva. hvis mvakode på varegruppen er endret.'
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
  IF bOk THEN 
  DO:
      IF NOT DYNAMIC-FUNCTION("runProc","vargr_oppdater_kalkyle.p",Vg:SCREEN-VALUE IN FRAME frmDetalj,'') THEN
        DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i koppdatering av kalkyle","").
      ELSE MESSAGE 'Kalkyle på alle artikler i varegruppen er oppdateret med varegruppens mvakode.'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-oppdArtikler
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-oppdArtikler C-Win
ON CHOOSE OF B-oppdArtikler IN FRAME frmDetalj /* Oppdater rabattunderlag */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    bOk = FALSE.
    MESSAGE 'Skal alle artikler i denne varegruppen oppdateres med rabattunderlag?' SKIP(1)
            (IF Bonus_Givende:CHECKED THEN 
                'Alle artikler som inngår i varegruppen vil bli markert med at de inngår i rabattunderlag.'
            ELSE 
                'Alle artikler som inngår i varegruppen blir markert med at de ikke inngår i rabattunderlag.')
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "asdf" UPDATE bOk.

    IF DYNAMIC-FUNCTION("runProc","vargr_rabattunderlag.p",
                      STRING(Bonus_Givende:CHECKED) + ',' + Vg:SCREEN-VALUE
                      ,?) THEN 
        MESSAGE 'Alle artikler i varegruppen er nå satt til ' 
            (IF Bonus_Givende:CHECKED THEN 
                'Bonus givende.'
            ELSE 
                'IKKE Bonus givende.')
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmListe
&Scoped-define SELF-NAME btnAvdeling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAvdeling C-Win
ON CHOOSE OF btnAvdeling IN FRAME frmListe /* ... */
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "AvdelingNr;AvdelingNavn".

  RUN JBoxDLookup.w ("Avdeling;AvdelingNr;AvdelingNavn", "where true", INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN sokAvdelingNr:SCREEN-VALUE   = ENTRY(1,cLookupValue,"|")
           sokAvdelingNavn:SCREEN-VALUE = ENTRY(2,cLookupValue,"|")
           sokHg:SCREEN-VALUE           = "0"
           sokHgBeskr:SCREEN-VALUE      = ""
           sokVG:SCREEN-VALUE           = "0"
           sokVgBeskr:SCREEN-VALUE      = ""
           .
    RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearFilter C-Win
ON CHOOSE OF btnClearFilter IN FRAME frmListe /* Blank filter */
DO:
  ASSIGN sokAvdelingNavn:SCREEN-VALUE = ""
         sokAvdelingNr:SCREEN-VALUE   = "0" 
         sokHg:SCREEN-VALUE           = "0" 
         sokHgBeskr:SCREEN-VALUE      = "" 
         sokVg:SCREEN-VALUE           = "0" 
         sokVgBeskr:SCREEN-VALUE      = ""
         .
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHuvGr C-Win
ON CHOOSE OF btnHuvGr IN FRAME frmListe /* ... */
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "Hg;HgBeskr".

  RUN JBoxDLookup.w ("HuvGr;Hg;HgBeskr", 
                     (IF sokAvdelingNr:SCREEN-VALUE NE "0" THEN 
                        "WHERE AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE
                      ELSE "where true"), 
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN sokHg:SCREEN-VALUE      = ENTRY(1,cLookupValue,"|")
           sokHgBeskr:SCREEN-VALUE = ENTRY(2,cLookupValue,"|")
           sokVG:SCREEN-VALUE      = "0"
           sokVgBeskr:SCREEN-VALUE = ""
           .
    RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmDetalj
&Scoped-define SELF-NAME btnKopierAktivitet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKopierAktivitet C-Win
ON CHOOSE OF btnKopierAktivitet IN FRAME frmDetalj /* Kopier */
DO:
  RUN KopierAktivitet.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKopierKategori
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKopierKategori C-Win
ON CHOOSE OF btnKopierKategori IN FRAME frmDetalj /* Kopier */
DO:
  RUN KopierKategori.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKopierRabatt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKopierRabatt C-Win
ON CHOOSE OF btnKopierRabatt IN FRAME frmDetalj /* Kopier */
DO:
  RUN KopierRabatt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLimInnAktivitet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLimInnAktivitet C-Win
ON CHOOSE OF btnLimInnAktivitet IN FRAME frmDetalj /* Lim inn */
DO:
  bOk = DYNAMIC-FUNCTION("runProc","lagreaktivitetkobling.p",Vg:SCREEN-VALUE,httVgAkt).
  IF bOk THEN
      APPLY "value-changed" TO hBrowseListe.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLimInnKategori
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLimInnKategori C-Win
ON CHOOSE OF btnLimInnKategori IN FRAME frmDetalj /* Lim inn */
DO:
  bOk = DYNAMIC-FUNCTION("runProc","lagrekategorikobling.p",Vg:SCREEN-VALUE,httVgKat).
  IF bOk THEN
      APPLY "value-changed" TO hBrowseListe.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLimInnRabatt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLimInnRabatt C-Win
ON CHOOSE OF btnLimInnRabatt IN FRAME frmDetalj /* Lim inn */
DO:
  bOk = DYNAMIC-FUNCTION("runProc","lagrekndgrpkobling.p",Vg:SCREEN-VALUE,httVgKundeGrpRabatt).
  IF bOk THEN
      APPLY "value-changed" TO hBrowseListe.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokHg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokHg C-Win
ON CHOOSE OF btnSokHg IN FRAME frmDetalj /* ... */
OR F10 OF Hg
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "Hg".
  RUN JBoxDLookup.w ("HuvGr;Hg;HgBeskr","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    Hg:SCREEN-VALUE = cLookupValue.
    APPLY "TAB" TO Hg.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokMoms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokMoms C-Win
ON CHOOSE OF btnSokMoms IN FRAME frmDetalj /* ... */
OR F10 OF MomsKod
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "MomsKod".
  RUN JBoxDLookup.w ("Moms;MomsKod;Beskrivelse;MomsProc","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue <> "" THEN 
  DO:
      ASSIGN
          MomsKod:SCREEN-VALUE = cLookupValue
          .
      APPLY "TAB" TO MomsKod.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Hg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Hg C-Win
ON RETURN OF Hg IN FRAME frmDetalj /* Hovedgruppe */
OR "TAB" OF Hg
DO:
  IF SELF:MODIFIED THEN
  DO:
      HgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","HuvGr",
                                              "WHERE Hg = '" + Hg:SCREEN-VALUE + "'","HgBeskr").  
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MomsKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MomsKod C-Win
ON RETURN OF MomsKod IN FRAME frmDetalj /* Momskode */
OR "TAB" OF MomsKod
DO:
  IF SELF:MODIFIED THEN
  DO:
      ASSIGN
          cTekst = DYNAMIC-FUNCTION("getFieldValues","Moms","WHERE MomsKod = '" + MomsKod:SCREEN-VALUE + "'","Beskrivelse,MomsProc")
          Beskrivelse:SCREEN-VALUE = ENTRY(1,cTekst,"|")  
          MomsProc:SCREEN-VALUE = ENTRY(2,cTekst,"|")  
          .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmListe
&Scoped-define SELF-NAME sokAvdelingNavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAvdelingNavn C-Win
ON RETURN OF sokAvdelingNavn IN FRAME frmListe
OR TAB OF sokAvdelingNavn DO:
  ASSIGN sokAvdelingNavn.
  IF sokAvdelingNavn NE "" THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokAvdelingNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAvdelingNr C-Win
ON RETURN OF sokAvdelingNr IN FRAME frmListe /* Avdeling */
OR TAB OF sokAvdelingNr DO:
  ASSIGN sokAvdelingNr.
  IF sokAvdelingNr NE 0 THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokHg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokHg C-Win
ON RETURN OF sokHg IN FRAME frmListe /* Hovedgruppe */
OR TAB OF sokHg DO:
  ASSIGN sokHg.
  IF sokHg NE 0 THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokHgBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokHgBeskr C-Win
ON RETURN OF sokHgBeskr IN FRAME frmListe
OR TAB OF sokHgBeskr DO:
  ASSIGN sokHgBeskr.
  IF sokHgBeskr NE "" THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVg C-Win
ON RETURN OF sokVg IN FRAME frmListe /* Varegruppe */
OR TAB OF sokVg DO:
  ASSIGN sokVg.
  IF sokVg NE 0 THEN RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokVgBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVgBeskr C-Win
ON RETURN OF sokVgBeskr IN FRAME frmListe
OR TAB OF sokVgBeskr DO:
  ASSIGN sokVgBeskr.
  IF sokVgBeskr NE "" THEN RUN OpenQuery.
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
  {lng.i}
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
        APPLY "Entry" TO VgBeskr.            .
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

OCXFile = SEARCH( "vargr.wrx":U ).
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
ELSE MESSAGE "vargr.wrx":U SKIP(1)
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

/* Dersom det finnes artikler under varegruppen så kan ikke hg endres: */
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseListe THEN DO WITH FRAME frmDetalj:
  IF DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE Vg = " + STRING(hFieldMap:BUFFER-FIELD("Vg"):BUFFER-VALUE),"ArtikkelNr") = ? THEN
    ASSIGN Hg:SENSITIVE       = TRUE
           btnSokHg:SENSITIVE = TRUE
           .
  ELSE
    ASSIGN Hg:SENSITIVE       = TRUE /*FALSE*/
           btnSokHg:SENSITIVE = TRUE /*FALSE*/
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
  DISPLAY Vg TillatLokalePriser VgBeskr Bonus_Givende Hg HgBeskr MomsKod 
          MomsProc Beskrivelse Kost_Proc FI-Kategoritekst FI-AktivitetTekst 
          FI-KndRabTekst 
      WITH FRAME frmDetalj IN WINDOW C-Win.
  ENABLE btnSokHg rectVgr rectKat rectKndRab rectBrowseKategori 
         rectBrowseKndGrp rectAktivitet rectBrowseAktivitet B-KonvTbl 
         B-oppdArtikler B-Mva_Kalkyle TillatLokalePriser VgBeskr Bonus_Givende 
         Hg MomsKod Kost_Proc btnKopierKategori btnSokMoms btnLimInnKategori 
         B-Kategori btnKopierAktivitet btnLimInnAktivitet B-Aktivitet 
         btnKopierRabatt btnLimInnRabatt B-Kundegruppe 
      WITH FRAME frmDetalj IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmDetalj}
  DISPLAY sokAvdelingNr sokAvdelingNavn sokHg sokHgBeskr sokVg sokVgBeskr 
      WITH FRAME frmListe IN WINDOW C-Win.
  ENABLE rectBrowseListe RectBrowseSearchListe sokAvdelingNr sokAvdelingNavn 
         btnAvdeling sokHg sokHgBeskr btnHuvGr sokVg sokVgBeskr btnClearFilter 
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
                            "WHERE SysHId = 16 and SysGr = 5 and ParaNr = 1","Parameter1")).  


DO WITH FRAME frmListe:
  ASSIGN cJoinHuvGr     = ",FIRST HuvGr NO-LOCK where HuvGr.Hg = VarGr.Hg OUTER-JOIN"
         cJoinAvdeling  = ",FIRST Avdeling NO-LOCK where Avdeling.AvdelingNr = HuvGr.AvdelingNr OUTER-JOIN"
         cJoinMoms      = ",FIRST Moms NO-LOCK where Moms.MomsKod = VarGr.MomsKod OUTER-JOIN"
                          .

  hBrowseListe = DYNAMIC-FUNCTION("NewBrowse",         /* Create a browse object */
                    rectBrowseListe:HANDLE IN FRAME frmListe,            /* Rectangle to define coordinates for browse */
                    1000,                            /* Rows to batch */
                    "MULTIPLE,NUM-LOCKED-COLUMNS|1",                     /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    /*
                    "Gavekort;butnr;IdentType;Dato|Reg.dato;+cRegTid|CHARACTER|x(5)|gavekort_regtid.p|Tid;Gyldigdato;Bruktdato;KasseNr;BongNr;Belop;!Tid," + 
                      "GaveKType;GKTBeskrivelse",                     /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/
                    */
                    "VarGr;Vg|VgNr@5;VgBeskr|VgBeskrivelse|x(40)@6;Hg|HgNr@3;MomsKod;Kost_Proc@9;TillatLokalePriser|Lok.pris|*/ @7;Bonus_Givende|rab.underlag|*/ @8;EDato;BrukerId,HuvGr;HgBeskr|HgBeskrivelse@4,Avdeling;AvdelingNr@1;AvdelingNavn|AvdBeskrivelse@2,Moms;MomsKod@9;MomsProc@10;Beskrivelse@11",
                    "WHERE true" + cJoinHuvGr + cJoinAvdeling + cJoinMoms,
                    "sort|Vg").                            /* Misc - for something I might need in next version.. */
  hBrowseListe:NAME = "brwListe". /* This name is neccessary because the browser is due to special treatment during resize */
  hBufferListe = hBrowseListe:QUERY:GET-BUFFER-HANDLE(1).
  
  /* Flytter kolonner */
  /*
  hBrowseListe:MOVE-COLUMN(9,1).
  hBrowseListe:MOVE-COLUMN(10,2).
  hBrowseListe:MOVE-COLUMN(5,3).
  hBrowseListe:MOVE-COLUMN(10,4).
  hBrowseListe:MOVE-COLUMN(11,9).
  hBrowseListe:MOVE-COLUMN(12,10).
  hBrowseListe:MOVE-COLUMN(13,11).
  */

  DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"querywhere","").
  DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"basequery","where true").
/*   DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"rowstobatch","200"). */
  DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"NoColumnSearch","HgBeskr,AvdelingNr,AvdelingNavn,MomsKode,MomsProc,Beskrivelse").

  hSearchListe = DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearchListe:HANDLE,hBrowseListe,5).

  hBrwListeMenu = DYNAMIC-FUNCTION("NewMenuBand",
                    hBrowseListe,  /* parent widget */
                   "PasteKategori;Lim inn kategorier;PasteKategori"
                    + ",PasteAktivitet;Lim inn aktiviteter;PasteAktivitet"
                    + ",PasteRabatt;Lim inn rabatter;PasteRabatt" 
                    + ",OppdaterMva;Oppdater kalkyle på artiklene i varegruppen med ny mva;OppdaterMva" 
                    ,"").

  hPasteKategoriMenuItem = DYNAMIC-FUNCTION("getEventWidget",hBrowseListe,"PasteKategori","menu-item").
  hPasteKategoriMenuItem:SENSITIVE = FALSE.
  hPasteAktivitetMenuItem = DYNAMIC-FUNCTION("getEventWidget",hBrowseListe,"PasteAktivitet","menu-item").
  hPasteAktivitetMenuItem:SENSITIVE = FALSE.
  hPasteRabattMenuItem = DYNAMIC-FUNCTION("getEventWidget",hBrowseListe,"PasteRabatt","menu-item").
  hPasteRabattMenuItem:SENSITIVE = FALSE.
END.

DO WITH FRAME frmDetalj:

  ASSIGN btnLimInnRabatt:HIDDEN = TRUE
         btnLimInnKategori:HIDDEN = TRUE
         btnLimInnAktivitet:HIDDEN = TRUE
         .
 
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins)
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowseListe:QUERY,
                    FRAME frmDetalj:HANDLE,         /* Frame for the input/display fields (might not be the same frame as the browse) */
                                                    /* Nb: Felt som brukes her må være hentet i Browser først.                        */
                    "Vg,VgBeskr,Hg,MomsKod,Kost_Proc,TillatLokalePriser,Bonus_Givende"
                       ,       
                    "Vg,VgBeskr,Hg,MomsKod,Kost_Proc,TillatLokalePriser,Bonus_Givende"
                       ,       
                    "HgBeskr,Beskrivelse,MomsProc", /* Additional buffer and displ.fields - not updateable*/
                    "HgBeskr,Beskrivelse,MomsProc",  
                    "").     /* Input fields other than in buffer */


  hBrowseKategori = DYNAMIC-FUNCTION("NewBrowse",         /* Create a browse object */
                    rectBrowseKategori:HANDLE IN FRAME frmDetalj,            /* Rectangle to define coordinates for browse */
                    200,                            /* Rows to batch */
                    "NUM-LOCKED-COLUMNS|1",                     /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "VgKat;!Vg,Kategori;KatNr|KatNr;Beskrivelse",
                    "WHERE false, first Kategori no-lock where Kategori.KatNr = VgKat.KatNr",
                    "sort|KatNr").                            /* Misc - for something I might need in next version.. */
  hBrowseKategori:NAME = "brwKategori". /* This name is neccessary because the browser is due to special treatment during resize */
  hBufferKategori = hBrowseKategori:QUERY:GET-BUFFER-HANDLE(1).
  DYNAMIC-FUNCTION("setAttribute",hBrowseKategori,"querywhere","").

  hBrowseAktivitet = DYNAMIC-FUNCTION("NewBrowse",                 /* Create a browse object */
                    rectBrowseAktivitet:HANDLE IN FRAME frmDetalj, /* Rectangle to define coordinates for browse */
                    200,                                        /* Rows to batch */
                    "NUM-LOCKED-COLUMNS|1",                     /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "VgAkt;!Vg,Aktivitet;AktNr|Aktivitet;Beskrivelse",
                    "WHERE false, first Aktivitet no-lock OF VgAkt",
                    "sort|AktNr").                            /* Misc - for something I might need in next version.. */
  hBrowseAktivitet:NAME = "brwAktivitet". /* This name is neccessary because the browser is due to special treatment during resize */
  hBufferAktivitet = hBrowseAktivitet:QUERY:GET-BUFFER-HANDLE(1).
  DYNAMIC-FUNCTION("setAttribute",hBrowseAktivitet,"querywhere","").

  hBrowseKndGrp = DYNAMIC-FUNCTION("NewBrowse",                 /* Create a browse object */
                    rectBrowseKndGrp:HANDLE IN FRAME frmDetalj, /* Rectangle to define coordinates for browse */
                    200,                                        /* Rows to batch */
                    "NUM-LOCKED-COLUMNS|1",                     /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "VgKundeGrpRabatt;!Vg;Rabatt%,KundeGruppe;GruppeId|KndGrp;Beskrivelse",
                    "WHERE false, first KundeGruppe no-lock where KundeGruppe.GruppeId = VgKundeGrpRabatt.GruppeId",
                    "sort|GruppeId").                            /* Misc - for something I might need in next version.. */
  hBrowseKndGrp:NAME = "brwKndGrp". /* This name is neccessary because the browser is due to special treatment during resize */
  hBufferKndGrp = hBrowseKndGrp:QUERY:GET-BUFFER-HANDLE(1).
  DYNAMIC-FUNCTION("setAttribute",hBrowseKndGrp,"querywhere","").

  /* Flytter kolonner */
  hBrowseKndGrp:MOVE-COLUMN(2,1).
  hBrowseKndGrp:MOVE-COLUMN(3,2).


  hBrwFillIn = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowseKndGrp,                     /* Handle to browse */
                    "Rabatt%",                         /* Browse column (display) */
                    "Rabatt%",                         /* Buffer column (to update - foreign key. Maps to value - under) */
                    "",                                /* DB buffers and fields for lookup */
/*                     "HuvGr;Hg;HgBeskr",                                /* DB buffers and fields for lookup */ */
                    "where true",                      /* Query for lookup */
                    "Hg",                              /* Lookup attributes (return fields) */
                    "1,99").                           /* Validation: Date,dec and int: range, Char: |-list */
  ASSIGN hBrwFillIn:NAME = "fiRabProsent"
         hBrwFillIn:HELP = "Rabatt% for kombinasjon kundegruppe/varegruppe"
         .
  /* Ignore = Ingen validering. Ellers skriv navn på program som gjør validering. */
  /* Dynamisk validering er default. Tileggsvalidering +<ProcNavn>.               */
  /* Kun egen validering, uten dynaisk validering =<ProcNavn>.                    */
  /* Dynamisk validering: Fremednøkkel - Felt må være indeksert og må finnes som  */
  /*                      primærnøkkel i en annen tabell.                         */
  /* DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore").   */
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","=varegr_update.p"). 
  /*DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraFields","Tid,BruktTid").*/
/*   DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore"). */

  /* Felter som skal være disablet. Comma separert. */
/*     DYNAMIC-FUNCTION("setAttribute",hFieldMap,"disabledFields","Vg"). */


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
  

  /* Link objects: */  
  /* Manuell linking når det er to objekter av samme type. */
  DYNAMIC-FUNCTION("createObjectLink",hBrowseListe,hUpdToolbar).
  DYNAMIC-FUNCTION("createObjectLink",hFieldMap,hUpdToolbar).
  DYNAMIC-FUNCTION("createObjectLink",hBrowseListe,hFieldMap).
  DYNAMIC-FUNCTION("createObjectLink",hBrowseListe,hSearchListe).
  DYNAMIC-FUNCTION("createParentLink",hBrowseKategori,hBrowseListe,'Vg').
  DYNAMIC-FUNCTION("createParentLink",hBrowseAktivitet,hBrowseListe,'Vg').
  DYNAMIC-FUNCTION("createParentLink",hBrowseKndGrp,hBrowseListe,'Vg').
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseKndGrp,hBrwFillIn,"Rabatt%"). /* Link the fill-in to the browse with column-name info */

/*   DYNAMIC-FUNCTION("DumpAllLinks",THIS-PROCEDURE:CURRENT-WINDOW,"c:\temp\links.txt").  */

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectUpdToolBar").
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmListe:HANDLE, "RectBrowseSearchListe").
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmDetalj:HANDLE,"rectVgr").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmDetalj:HANDLE,"rectVgr").
  DYNAMIC-FUNCTION("setResizeXGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmDetalj:HANDLE, 33.33,"brwKategori,rectBrowseKategori,rectKat,brwAktivitet,rectBrowseAktivitet,rectAktivitet,brwKndGrp,rectBrowseKndGrp,rectKndRab").
  DYNAMIC-FUNCTION("setMoveXGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmDetalj:HANDLE, 33.33,"rectAktivitet,brwAktivitet,rectBrowseAktivitet,rectKndRab,B-Kategori,FI-AktivitetTekst,btnKopierAktivitet,btnLimInnAktivitet").
  DYNAMIC-FUNCTION("setMoveXGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmDetalj:HANDLE, 66.66,"rectKndRab,B-Aktivitet,FI-KndRabTekst,btnKopierRabatt,btnLimInnRabatt,brwKndGrp,rectBrowseKndGrp").

  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,400,500,0,0).

  DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  /*DYNAMIC-FUNCTION("SetToolbar",hUpdToolbar,"enable").*/
  DYNAMIC-FUNCTION("SetToolbar",hToolBar,"enable").

END.

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

APPLY "ENTRY" TO hBrowseListe.
IF hBrowseListe:NUM-ITERATIONS > 0 THEN
    APPLY "value-changed" TO hBrowseListe.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierAktivitet C-Win 
PROCEDURE KopierAktivitet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME frmDetalj:
  btnLimInnAktivitet:HIDDEN = FALSE.
  EMPTY TEMP-TABLE ttVgAkt.
  hBrowseAktivitet:QUERY:GET-FIRST().
  REPEAT WHILE NOT hBrowseAktivitet:QUERY:QUERY-OFF-END:
    CREATE ttVgAkt.
    ttVgAkt.AktNr    = hBufferAktivitet:BUFFER-FIELD("AktNr"):BUFFER-VALUE.
    hBrowseAktivitet:QUERY:GET-NEXT().
  END.
  hPasteAktivitetMenuItem:SENSITIVE = TRUE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierKategori C-Win 
PROCEDURE KopierKategori :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME frmDetalj:
  btnLimInnKategori:HIDDEN = FALSE.
  EMPTY TEMP-TABLE ttVgKat.
  hBrowseKategori:QUERY:GET-FIRST().
  REPEAT WHILE NOT hBrowseKategori:QUERY:QUERY-OFF-END:
    CREATE ttVgKat.
    ASSIGN
        ttVgKat.KatNr = hBufferKategori:BUFFER-FIELD("KatNr"):BUFFER-VALUE
        .
    hBrowseKategori:QUERY:GET-NEXT().
  END.
  hPasteKategoriMenuItem:SENSITIVE = TRUE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierRabatt C-Win 
PROCEDURE KopierRabatt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME frmDetalj:
  btnLimInnRabatt:HIDDEN = FALSE.
  EMPTY TEMP-TABLE ttVgKundeGrRabatt.
  hBrowseKndGrp:QUERY:GET-FIRST().
  REPEAT WHILE NOT hBrowseKndGrp:QUERY:QUERY-OFF-END:
    CREATE ttVgKundeGrRabatt.
    ASSIGN ttVgKundeGrRabatt.GruppeId = hBufferKndGrp:BUFFER-FIELD("GruppeId"):BUFFER-VALUE
           ttVgKundeGrRabatt.Rabatt%  = hBufferKndGrp:BUFFER-FIELD("Rabatt%"):BUFFER-VALUE
           .
    hBrowseKndGrp:QUERY:GET-NEXT().
  END.
  hPasteRabattMenuItem:SENSITIVE = TRUE.
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
          Vg:SENSITIVE = TRUE
          MomsKod:SCREEN-VALUE = ?
          .
      APPLY "ENTRY" TO Vg.
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
DEF VAR cTmpJoinHuvGr      AS CHAR   NO-UNDO.
DEF VAR cTmpJoinAvdeling   AS CHAR   NO-UNDO.
DEF VAR cTmpJoinMoms       AS CHAR   NO-UNDO.
DEF VAR cTmpFilter         AS CHAR   NO-UNDO.


IF iTab = 1 THEN DO WITH FRAME frmListe:
  ASSIGN sokAvdelingNavn sokAvdelingNr sokHg sokHgBeskr sokVg sokVgBeskr.
  DYNAMIC-FUNCTION("SetCurrentObject",hBrowseListe).
  IF sokAvdelingNr NE 0 THEN
    cTmpJoinAvdeling = ",FIRST Avdeling NO-LOCK where Avdeling.AvdelingNr = HuvGr.AvdelingNr AND Avdeling.AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE.
  ELSE IF sokAvdelingNavn NE "" THEN
    cTmpJoinAvdeling = ",FIRST Avdeling NO-LOCK where Avdeling.AvdelingNr = HuvGr.AvdelingNr AND " +
                       (IF INDEX(sokAvdelingNavn,"*") > 0 THEN 
                         "AvdelingNavn MATCHES '" + sokAvdelingNavn + "*'"
                        ELSE
                         "AvdelingNavn BEGINS '" + sokAvdelingNavn + "'").
  ELSE cTmpJoinAvdeling = cJoinAvdeling.

  IF sokHg NE 0 THEN
    ASSIGN cTmpFilter    = " AND VarGr.Hg = " + sokHg:SCREEN-VALUE
           cTmpJoinHuvGr = cJoinHuvGr
           .
  ELSE IF sokHgBeskr NE "" THEN
    cTmpJoinHuvGr = ",FIRST HuvGr NO-LOCK where HuvGr.Hg = VarGr.Hg AND " +
                       (IF INDEX(sokHgBeskr,"*") > 0 THEN 
                         "HgBeskr MATCHES '" + sokHgBeskr + "*'"
                        ELSE
                         "HgBeskr BEGINS '" + sokHgBeskr + "'").
  ELSE cTmpJoinHuvGr = cJoinHuvGr.

  IF sokVg NE 0 THEN
    cTmpFilter = cTmpFilter + " AND VarGr.Vg = " + sokVg:SCREEN-VALUE.
  ELSE IF sokVgBeskr NE "" THEN
    cTmpFilter = " AND " +
                       (IF INDEX(sokVgBeskr,"*") > 0 THEN 
                         "VgBeskr MATCHES '" + sokVgBeskr + "*'"
                        ELSE
                         "VgBeskr BEGINS '" + sokVgBeskr + "'").


  DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"queryfilter",cTmpFilter).
  DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"queryjoin",cTmpJoinHuvGr + cTmpJoinAvdeling + cJoinMoms).
END.

RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterMva C-Win 
PROCEDURE OppdaterMva :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO:
  iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft oppdatering av mva i kalkyle på alle artikler i varegruppen","Bekreft",
                             (IF hBrowseListe:NUM-SELECTED-ROWS > 0 THEN 
                                STRING(hBrowseListe:NUM-SELECTED-ROWS)
                              ELSE 
                                DYNAMIC-FUNCTION("getAttribute",hBrowseListe,"Totalcount"))
                              ).
  IF iReturn NE 2 THEN DO:
    IF NOT DYNAMIC-FUNCTION("runProc","vargr_oppdater_kalkyle.p",getVgList(),'') THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i koppdatering av kalkyle","").
    ELSE DO:
        MESSAGE 'Kalkyle på alle artikler i merkede/valgte varegrupper er oppdateret med varegruppens mvakode.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "value-changed" TO hBrowseListe.
    END.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PasteAktivitet C-Win 
PROCEDURE PasteAktivitet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF CAN-FIND(FIRST ttVgAkt) THEN DO:
  iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft kopiering av aktiviteter til &1 varegrupper","Bekreft",
                             (IF hBrowseListe:NUM-SELECTED-ROWS > 0 THEN 
                                STRING(hBrowseListe:NUM-SELECTED-ROWS)
                              ELSE 
                                DYNAMIC-FUNCTION("getAttribute",hBrowseListe,"Totalcount"))
                              ).
  IF iReturn NE 2 THEN DO:
    IF NOT DYNAMIC-FUNCTION("runProc","lagreaktivitetkobling.p",getVgList(),httVgAkt) THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i kopiering av aktiviteter","").
    ELSE APPLY "value-changed" TO hBrowseListe.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PasteKategori C-Win 
PROCEDURE PasteKategori :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF CAN-FIND(FIRST ttVgKat) THEN DO:
  iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft kopiering av kategorier til &1 varegrupper","Bekreft",
                             (IF hBrowseListe:NUM-SELECTED-ROWS > 0 THEN 
                                STRING(hBrowseListe:NUM-SELECTED-ROWS)
                              ELSE 
                                DYNAMIC-FUNCTION("getAttribute",hBrowseListe,"Totalcount"))
                              ).
  IF iReturn NE 2 THEN DO:
    IF NOT DYNAMIC-FUNCTION("runProc","lagrekategorikobling.p",getVgList(),httVgKat) THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i kopiering av kategorier","").
    ELSE APPLY "value-changed" TO hBrowseListe.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PasteRabatt C-Win 
PROCEDURE PasteRabatt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF CAN-FIND(FIRST ttVgKundeGrRabatt) THEN DO:
  iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft kopiering av rabattprosenter til &1 varegrupper","Bekreft",
                             (IF hBrowseListe:NUM-SELECTED-ROWS > 0 THEN 
                                STRING(hBrowseListe:NUM-SELECTED-ROWS)
                              ELSE 
                                DYNAMIC-FUNCTION("getAttribute",hBrowseListe,"Totalcount"))
                              ).
  IF iReturn NE 2 THEN DO:
    IF NOT DYNAMIC-FUNCTION("runProc","lagrekndgrpkobling.p",getVgList(),httVgKundeGrpRabatt) THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i kopiering av rabatter","").
    ELSE APPLY "value-changed" TO hBrowseListe.
  END.
END.

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

    IF int(Vg:screen-value) = 0 THEN
    DO:
        MESSAGE "Varegruppe må angis."
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        APPLY "ENTRY" TO Vg.
        RETURN NO-APPLY.
    END.
    IF int(Hg:screen-value) = 0 THEN
    DO:
        MESSAGE "Huvedgruppe må angis."
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        APPLY "ENTRY" TO Hg.
        RETURN NO-APPLY.
    END.
    IF NOT int(MomsKod:screen-value) > -1 THEN
    DO:
        MESSAGE "Momskode må angis."
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        APPLY "ENTRY" TO MomsKod.
        RETURN NO-APPLY.
    END.
    IF VgBeskr:SCREEN-VALUE = "" THEN
    DO:
        MESSAGE "Varegruppetekst må angis."
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        APPLY "ENTRY" TO VgBeskr.
        RETURN NO-APPLY.
    END.

    IF cState = "NEW" THEN
    NEWPOST:
    DO:
      IF int(Vg:screen-value) = int(DYNAMIC-FUNCTION("getFieldValues","VarGr","WHERE Vg = int(" + Vg:SCREEN-VALUE + ")","Vg")) THEN
      DO:
          MESSAGE "Vg eksisterer fra før med vgnr " + Vg:SCREEN-VALUE + "."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK  TITLE "Lagringskontroll".
          APPLY "ENTRY" TO Vg.
          RETURN NO-APPLY.
      END.
    END. /* NEWPOST */

    IF DYNAMIC-FUNCTION("getFieldValues","HuvGr","WHERE Hg = '" + Hg:SCREEN-VALUE + "'","HgBeskr") = ? THEN
    DO:
        MESSAGE "Ugyldig hovedgruppe."
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        APPLY "ENTRY" TO Hg.
        RETURN NO-APPLY.
    END.

    IF DYNAMIC-FUNCTION("getFieldValues","Moms","WHERE MomsKod = '" + MomsKod:SCREEN-VALUE + "'","Beskrivelse,MomsProc") = ? THEN
    DO:
        MESSAGE "Ugyldig momskode."
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        APPLY "ENTRY" TO MomsKod.
        RETURN NO-APPLY.
    END.

    ASSIGN
        Vg:SENSITIVE = FALSE
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
          Vg:SENSITIVE = FALSE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRab%Lst C-Win 
FUNCTION getRab%Lst RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.
  DEF VAR piLoop  AS INT NO-UNDO.

  DO piLoop = 5 TO 75:
      pcTekst = pcTekst + 
                (IF pcTekst = ""
                   THEN ""
                   ELSE "|") + 
                STRING(piLoop) + "%|" + STRING(piLoop,">9.99").
  END.

  RETURN pcTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getVgList C-Win 
FUNCTION getVgList RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cVgList AS CHAR NO-UNDO.

IF hBrowseListe:NUM-SELECTED-ROWS > 0 THEN 
  DO ix = 1 TO hBrowseListe:NUM-SELECTED-ROWS:
    IF hBrowseListe:FETCH-SELECTED-ROW(ix) THEN 
      cVgList = cVgList + STRING(hBufferListe:BUFFER-FIELD("Vg"):BUFFER-VALUE) + "|".
  END.
ELSE DO:
  hBrowseListe:QUERY:GET-FIRST().
  REPEAT WHILE NOT hBrowseListe:QUERY:QUERY-OFF-END:
    cVgList = cVgList + STRING(hBufferListe:BUFFER-FIELD("Vg"):BUFFER-VALUE) + "|".
    hBrowseListe:QUERY:GET-NEXT().
  END.
  hBrowseListe:QUERY:GET-FIRST().
END.

RETURN TRIM(cVgList,"|").

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
          Vg:SENSITIVE = TRUE.
      ELSE
          Vg:SENSITIVE = FALSE.
  END.
  /*
  APPLY "ENTRY":U TO fi-fArtikkelNr.
  */

  RETURN TRUE.   /* Function return value. */
END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

