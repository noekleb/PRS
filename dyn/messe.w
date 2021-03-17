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

DEF VAR bOK               AS LOG NO-UNDO.
DEF VAR ix                AS INT NO-UNDO.

DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hUpdToolbar       AS HANDLE NO-UNDO.
DEF VAR hBrowseListe      AS HANDLE NO-UNDO.
DEF VAR hBufferListe      AS HANDLE NO-UNDO.
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hBrwOppmerking    AS HANDLE NO-UNDO.
DEF VAR hBuffOppmerking   AS HANDLE NO-UNDO.
DEF VAR hFieldColor       AS HANDLE NO-UNDO.
DEF VAR hColumnOppmerking AS HANDLE NO-UNDO.
DEF VAR hFieldColNum256   AS HANDLE NO-UNDO.

DEF VAR hBrwSortimentkoder    AS HANDLE NO-UNDO.
DEF VAR hBuffSortimentkoder   AS HANDLE NO-UNDO.
DEF VAR hBrwKampanjeuker      AS HANDLE NO-UNDO.
DEF VAR hBuffKampanjeuker     AS HANDLE NO-UNDO.
DEF VAR hBrwKampanjestotte    AS HANDLE NO-UNDO.
DEF VAR hBuffKampanjestotte   AS HANDLE NO-UNDO.
DEF VAR hBrwLagerkoder        AS HANDLE NO-UNDO.
DEF VAR hBuffLagerkoder       AS HANDLE NO-UNDO.
DEF VAR hFieldColorSort       AS HANDLE NO-UNDO.
DEF VAR hColumnOppmerkingSort AS HANDLE NO-UNDO.
DEF VAR hFieldColNum256Sort   AS HANDLE NO-UNDO.

DEF VAR cAdgang           AS CHAR   NO-UNDO.
DEF VAR hIndDet           AS HANDLE NO-UNDO.
DEF VAR hTLdet            AS HANDLE NO-UNDO.

DEF VAR iTab              AS INT    NO-UNDO.

DEF VAR cState            AS CHAR   NO-UNDO.

DEF VAR cOppmerking       AS CHAR   NO-UNDO.
DEF VAR cFargekoder       AS CHAR   NO-UNDO.

DEF VAR cSortimentkoder   AS CHAR   NO-UNDO.
DEF VAR cFargekoderSort   AS CHAR   NO-UNDO.
DEF VAR cKampanjeuker     AS CHAR   NO-UNDO.
DEF VAR cKampanjestotte   AS CHAR   NO-UNDO.
DEF VAR cLagerkoder       AS CHAR   NO-UNDO.

DEF TEMP-TABLE ttSort    
    FIELD cSort AS CHAR.


DEF VAR cKombTegn AS CHAR NO-UNDO.
DEF VAR cTempList AS CHAR NO-UNDO.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKoder C-Win 
FUNCTION getKoder RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMerknadFarge C-Win 
FUNCTION getMerknadFarge RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFieldColor C-Win 
FUNCTION setFieldColor RETURNS INTEGER
  ( INPUT iRGBcolor AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetTabStrip C-Win 
FUNCTION SetTabStrip RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD settOppmerking C-Win 
FUNCTION settOppmerking RETURNS LOGICAL
  ( INPUT icAddText  AS CHAR,
    INPUT icAllText  AS CHAR,
    INPUT iStartPos  AS INT) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TabStripChanged C-Win 
FUNCTION TabStripChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewKampanjestotte C-Win 
FUNCTION ViewKampanjestotte RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewKampanjeuker C-Win 
FUNCTION ViewKampanjeuker RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewLagerkoder C-Win 
FUNCTION ViewLagerkoder RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewOppmerking C-Win 
FUNCTION ViewOppmerking RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewSortimentkoder C-Win 
FUNCTION ViewSortimentkoder RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

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

DEFINE BUTTON btnBrukt 
     LABEL "Koder i bruk?" 
     SIZE 16.6 BY 1.14.

DEFINE BUTTON btnFarge 
     IMAGE-UP FILE "bmp/color.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnFargeSort 
     IMAGE-UP FILE "bmp/color.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "btnfarge 2" 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnFraDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnKampanjestotte 
     IMAGE-UP FILE "bmp/edit16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Rediger sortimentkoder".

DEFINE BUTTON btnKampanjeuker 
     IMAGE-UP FILE "bmp/edit16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Rediger sortimentkoder".

DEFINE BUTTON btnLagerkoder 
     IMAGE-UP FILE "bmp/edit16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Rediger sortimentkoder".

DEFINE BUTTON btnMesseFraDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnMesseTilDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnPostnr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnPublilserStartDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnPubliserStoppDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSetup 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSetup-2 
     IMAGE-UP FILE "bmp/edit16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Rediger tekst for hånd: merk1,merk2|merk1,merk3...".

DEFINE BUTTON btnSortiment 
     IMAGE-UP FILE "bmp/edit16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Rediger sortimentkoder".

DEFINE BUTTON btnTilDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE Adresse1 AS CHARACTER FORMAT "X(30)" 
     LABEL "Adresse1" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE Adresse2 AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE Beskrivelse AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 32.6 BY 1.

DEFINE VARIABLE FraDato AS DATE FORMAT "99/99/99" 
     LABEL "Levering fra" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE KontaktPers AS CHARACTER FORMAT "X(30)" 
     LABEL "Kontaktperson" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1.

DEFINE VARIABLE MesseBeskrivelse AS CHARACTER FORMAT "X(40)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1.

DEFINE VARIABLE MesseFraDato AS DATE FORMAT "99/99/99" 
     LABEL "Messestart" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE MesseNr AS DECIMAL FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "MesseNr" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE MesseTilDato AS DATE FORMAT "99/99/99" 
     LABEL "Messestopp" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE MobilTlf AS CHARACTER FORMAT "X(20)" 
     LABEL "Mobil" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE PostBoks AS CHARACTER FORMAT "X(40)" 
     LABEL "Postboks" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1.

DEFINE VARIABLE PostNr AS CHARACTER FORMAT "X(10)" 
     LABEL "PostNr" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE PubliserStartDato AS DATE FORMAT "99/99/99" 
     LABEL "Start publiser" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE PubliserStoppDato AS DATE FORMAT "99/99/99" 
     LABEL "Publiser stopp" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE Telefaks AS CHARACTER FORMAT "X(20)" 
     LABEL "Telefaks" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE Telefon AS CHARACTER FORMAT "X(20)" 
     LABEL "Telefon" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE TilDato AS DATE FORMAT "99/99/99" 
     LABEL "Levering til" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE RECTANGLE brwKampanjestotte
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 5.24.

DEFINE RECTANGLE brwKampanjeuker
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 5.24.

DEFINE RECTANGLE brwLagerkoder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 5.24.

DEFINE RECTANGLE brwOppmerking
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 15.71.

DEFINE RECTANGLE brwSortimentkoder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 5.24.

DEFINE RECTANGLE rectBrowseListe
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 197.6 BY 28.57.

DEFINE RECTANGLE RectBrowseSearchListe
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY 1.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectToolBar AT ROW 1.14 COL 193.2
     rectUpdToolbar AT ROW 1.14 COL 1.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 200.2 BY 32.71.

DEFINE FRAME frmListe
     rectBrowseListe AT ROW 2.43 COL 1.4
     RectBrowseSearchListe AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.62
         SIZE 198 BY 30.

DEFINE FRAME frmDetalj
     btnFarge AT ROW 29.91 COL 27.8
     MesseNr AT ROW 1.14 COL 17 COLON-ALIGNED HELP
          "Messenummer"
     MesseBeskrivelse AT ROW 2.19 COL 17 COLON-ALIGNED HELP
          "Navn eller kort beskrivelse av messen."
     MesseFraDato AT ROW 3.24 COL 17 COLON-ALIGNED HELP
          "Startdato for messe"
     MesseTilDato AT ROW 3.24 COL 52.2 COLON-ALIGNED HELP
          "Siste messedag"
     FraDato AT ROW 4.29 COL 17 COLON-ALIGNED HELP
          "Første gyldige leveringsdato"
     TilDato AT ROW 4.29 COL 52.2 COLON-ALIGNED HELP
          "Siste gyldige leveringsdato"
     PubliserStartDato AT ROW 5.33 COL 17 COLON-ALIGNED HELP
          "1. dag når messens varebøker til tilgjengelige"
     PubliserStoppDato AT ROW 5.33 COL 52.2 COLON-ALIGNED HELP
          "Siste dag når messens varebøker er tilgjengelige"
     KontaktPers AT ROW 6.43 COL 17 COLON-ALIGNED HELP
          "Kontaktperson"
     Telefon AT ROW 7.48 COL 17 COLON-ALIGNED
     MobilTlf AT ROW 7.48 COL 48 COLON-ALIGNED HELP
          "Mobiltelefon"
     Telefaks AT ROW 8.52 COL 17 COLON-ALIGNED
     Adresse1 AT ROW 9.57 COL 17 COLON-ALIGNED HELP
          "Adresse"
     Adresse2 AT ROW 10.62 COL 17 COLON-ALIGNED HELP
          "Adresse" NO-LABEL
     PostBoks AT ROW 11.67 COL 17 COLON-ALIGNED
     PostNr AT ROW 12.71 COL 17 COLON-ALIGNED HELP
          "Postnummer"
     Beskrivelse AT ROW 12.71 COL 37.4 COLON-ALIGNED HELP
          "Poststed" NO-LABEL
     btnBrukt AT ROW 29.81 COL 32.4
     btnFargeSort AT ROW 6.52 COL 80.8
     btnSetup-2 AT ROW 29.91 COL 23.6
     btnSetup AT ROW 29.91 COL 19
     btnSortiment AT ROW 6.52 COL 76
     btnLagerkoder AT ROW 26.57 COL 76
     btnKampanjeuker AT ROW 13.14 COL 76
     btnKampanjestotte AT ROW 19.86 COL 76
     btnMesseFraDato AT ROW 3.29 COL 32.6 NO-TAB-STOP 
     btnMesseTilDato AT ROW 3.29 COL 67.6 NO-TAB-STOP 
     btnPublilserStartDato AT ROW 5.33 COL 32.6 NO-TAB-STOP 
     btnPubliserStoppDato AT ROW 5.33 COL 67.6 NO-TAB-STOP 
     btnTilDato AT ROW 4.29 COL 67.6 NO-TAB-STOP 
     btnFraDato AT ROW 4.29 COL 32.6 NO-TAB-STOP 
     btnPostnr AT ROW 12.71 COL 34.8 NO-TAB-STOP 
     brwOppmerking AT ROW 13.86 COL 19
     brwSortimentkoder AT ROW 1.24 COL 76
     brwKampanjeuker AT ROW 7.86 COL 76
     brwKampanjestotte AT ROW 14.57 COL 76
     brwLagerkoder AT ROW 21.24 COL 76
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.62
         SIZE 198 BY 30.


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
         TITLE              = "Innkjøpsperioderegister"
         HEIGHT             = 32.71
         WIDTH              = 200.6
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
/* SETTINGS FOR BUTTON btnFargeSort IN FRAME frmDetalj
   NO-ENABLE                                                            */
ASSIGN 
       btnFargeSort:HIDDEN IN FRAME frmDetalj           = TRUE.

/* SETTINGS FOR BUTTON btnSetup IN FRAME frmDetalj
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnSetup-2 IN FRAME frmDetalj
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN MesseNr IN FRAME frmDetalj
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

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME TabStrip ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.43
       COLUMN          = 1
       HEIGHT          = 31.19
       WIDTH           = 200
       HIDDEN          = no
       SENSITIVE       = yes.
/* TabStrip OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Innkjøpsperioderegister */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Innkjøpsperioderegister */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Innkjøpsperioderegister */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmDetalj
&Scoped-define SELF-NAME btnBrukt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBrukt C-Win
ON CHOOSE OF btnBrukt IN FRAME frmDetalj /* Koder i bruk? */
DO:
  DEF VAR cIkkeIbruk AS CHAR NO-UNDO.
  IF DYNAMIC-FUNCTION("runProc","messe_merknad_ibruk.p",STRING(hFieldMap:BUFFER-FIELD("Messenr"):BUFFER-VALUE),?) THEN DO:      
    cIkkeIbruk = DYNAMIC-FUNCTION("getTransactionMessage").
    IF cIkkeIbruk NE "" THEN DO:
      IF DYNAMIC-FUNCTION("DoMessage",0,4,cIkkeIbruk,"Disse merknadskodene er ikke i bruk. Slett fra messeregister?","") = 6 THEN DO:
        IF DYNAMIC-FUNCTION("runProc","messe_merknad_ibruk.p",STRING(hFieldMap:BUFFER-FIELD("Messenr"):BUFFER-VALUE) + ",delete",?) THEN DO:
          DYNAMIC-FUNCTION("RefreshRowids",hBrowseListe,hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).  
          APPLY "value-changed" TO hBrowseListe.
        END.
        ELSE 
          DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
      END.
    END.
  END.
  ELSE 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFarge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFarge C-Win
ON CHOOSE OF btnFarge IN FRAME frmDetalj /* Button 1 */
DO:
  DEF VAR iColor       AS INT    NO-UNDO.
  DEF VAR hQueryMerk   AS HANDLE NO-UNDO.
  DEF VAR hBuffMerk    AS HANDLE NO-UNDO.
  DEF VAR iColNum256   AS INT    NO-UNDO.

  IF NOT hBuffOppmerking:AVAIL THEN RETURN.

  IF hBrwOppmerking:FOCUSED-ROW > 0 AND hBrwOppmerking:NUM-SELECTED-ROWS = 0 THEN
    hBrwOppmerking:SELECT-ROW(hBrwOppmerking:FOCUSED-ROW).
  IF hBrwOppmerking:NUM-SELECTED-ROWS = 0 THEN
    hBrwOppmerking:SELECT-ROW(1).

  iColor = hBuffOppmerking:BUFFER-FIELD("iColor"):BUFFER-VALUE.

  RUN JBoxDSelectColor.w (hBuffOppmerking:BUFFER-FIELD("Oppmerking"):BUFFER-VALUE,INPUT-OUTPUT iColor,OUTPUT iColNum256,OUTPUT bOK).

  IF bOK THEN DO:
    DO ix = 1 TO hBrwOppmerking:NUM-SELECTED-ROWS:
      IF hBrwOppmerking:FETCH-SELECTED-ROW(ix) THEN
        ASSIGN hFieldColor:BUFFER-VALUE     = iColor
               hFieldColNum256:BUFFER-VALUE = iColNum256.
    END.

    DYNAMIC-FUNCTION("setCurrentObject",hBrwOppmerking).
    RUN OpenQuery.

/*     ASSIGN hFieldColor:BUFFER-VALUE     = iColor                                       */
/*            hFieldColNum256:BUFFER-VALUE = iColNum256.                                  */
/*                                                                                        */
/*     CREATE BUFFER hBuffMerk FOR TABLE hBuffOppmerking.                                 */
/*     CREATE QUERY  hQueryMerk.                                                          */
/*     hQueryMerk:SET-BUFFERS(hBuffMerk).                                                 */
/*     hQueryMerk:QUERY-PREPARE("FOR EACH " + hBuffMerk:NAME).                            */
/*     hQueryMerk:QUERY-OPEN().                                                           */
/*                                                                                        */
/*     cFargekoder = "".                                                                  */
/*     hQueryMerk:GET-FIRST().                                                            */
/*     REPEAT WHILE NOT hQueryMerk:QUERY-OFF-END:                                         */
/*       cFargekoder = cFargekoder + hBuffMerk:BUFFER-FIELD("iColor"):STRING-VALUE + ",". */
/*       hQueryMerk:GET-NEXT().                                                           */
/*     END.                                                                               */
/*     cFargekoder = SUBSTR(cFargekoder,1,LENGTH(cFargekoder) - 1).                       */
/*                                                                                        */
/*     DELETE OBJECT hBuffMerk.                                                           */
/*     DELETE OBJECT hQueryMerk.                                                          */

    APPLY "any-printable" TO MesseBeskrivelse.
    MesseBeskrivelse:MODIFIED = TRUE.


/*     hBrwOppmerking:DESELECT-FOCUSED-ROW().  */
/*     hColumnOppmerking:BGCOLOR = iColNum256. */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFargeSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFargeSort C-Win
ON CHOOSE OF btnFargeSort IN FRAME frmDetalj /* btnfarge 2 */
DO:
  DEF VAR iColor       AS INT    NO-UNDO.
  DEF VAR hQueryMerk   AS HANDLE NO-UNDO.
  DEF VAR hBuffMerk    AS HANDLE NO-UNDO.
  DEF VAR iColNum256   AS INT    NO-UNDO.

  IF NOT hBuffSortimentkoder:AVAIL THEN RETURN.

  IF hBrwSortimentkoder:FOCUSED-ROW > 0 AND hBrwSortimentkoder:NUM-SELECTED-ROWS = 0 THEN
    hBrwSortimentkoder:SELECT-ROW(hBrwSortimentkoder:FOCUSED-ROW).
  IF hBrwSortimentkoder:NUM-SELECTED-ROWS = 0 THEN
    hBrwSortimentkoder:SELECT-ROW(1).

  iColor = hBuffSortimentkoder:BUFFER-FIELD("iColor"):BUFFER-VALUE.

  RUN JBoxDSelectColor.w (hBuffSortimentkoder:BUFFER-FIELD("Sortiment"):BUFFER-VALUE,INPUT-OUTPUT iColor,OUTPUT iColNum256,OUTPUT bOK).

  IF bOK THEN DO:
    DO ix = 1 TO hBrwSortimentkoder:NUM-SELECTED-ROWS:
      IF hBrwSortimentkoder:FETCH-SELECTED-ROW(ix) THEN
        ASSIGN hFieldColorSort:BUFFER-VALUE     = iColor
               hFieldColNum256Sort:BUFFER-VALUE = iColNum256.
    END.

    DYNAMIC-FUNCTION("setCurrentObject",hBrwSortimentkoder).
    RUN OpenQuery.

    APPLY "any-printable" TO MesseBeskrivelse.
    MesseBeskrivelse:MODIFIED = TRUE.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFraDato C-Win
ON CHOOSE OF btnFraDato IN FRAME frmDetalj /* ... */
OR "F10" OF FraDato
DO:
  RUN Cal.w (FraDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKampanjestotte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKampanjestotte C-Win
ON CHOOSE OF btnKampanjestotte IN FRAME frmDetalj /* ... */
DO:  
  DEF VAR cText AS CHAR NO-UNDO.

  cText = REPLACE(cKampanjestotte,"|",CHR(10)).
  RUN JBoxDEditor.w (INPUT-OUTPUT cText,no,no,no,"",OUTPUT bOk).

  IF bOk THEN DO:
    cKampanjestotte = REPLACE(RIGHT-TRIM(cText,CHR(10)),CHR(10),"|").
    ViewKampanjestotte().
    APPLY "any-printable" TO MesseBeskrivelse.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKampanjeuker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKampanjeuker C-Win
ON CHOOSE OF btnKampanjeuker IN FRAME frmDetalj /* ... */
DO:  
  DEF VAR cText AS CHAR NO-UNDO.

  cText = REPLACE(cKampanjeuker,"|",CHR(10)).
  RUN JBoxDEditor.w (INPUT-OUTPUT cText,no,no,no,"",OUTPUT bOk).

  IF bOk THEN DO:
    cKampanjeuker = REPLACE(RIGHT-TRIM(cText,CHR(10)),CHR(10),"|").
    ViewKampanjeuker().
    APPLY "any-printable" TO MesseBeskrivelse.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLagerkoder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLagerkoder C-Win
ON CHOOSE OF btnLagerkoder IN FRAME frmDetalj /* ... */
DO:  
  DEF VAR cText AS CHAR NO-UNDO.

  cText = REPLACE(cLagerkoder,"|",CHR(10)).
  RUN JBoxDEditor.w (INPUT-OUTPUT cText,no,no,no,"",OUTPUT bOk).

  IF bOk THEN DO:
    cLagerkoder = REPLACE(RIGHT-TRIM(cText,CHR(10)),CHR(10),"|").
    ViewLagerkoder().
    APPLY "any-printable" TO MesseBeskrivelse.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMesseFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMesseFraDato C-Win
ON CHOOSE OF btnMesseFraDato IN FRAME frmDetalj /* ... */
OR "F10" OF MesseFraDato
DO:
  RUN Cal.w (MesseFraDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMesseTilDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMesseTilDato C-Win
ON CHOOSE OF btnMesseTilDato IN FRAME frmDetalj /* ... */
OR "F10" OF MesseTilDato
DO:
  RUN Cal.w (MesseTilDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPostnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPostnr C-Win
ON CHOOSE OF btnPostnr IN FRAME frmDetalj /* ... */
OR F10 OF PostNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "PostNr".
  RUN JBoxDLookup.w ("Post;PostNr;Beskrivelse","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    PostNr:SCREEN-VALUE = cLookupValue.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPublilserStartDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPublilserStartDato C-Win
ON CHOOSE OF btnPublilserStartDato IN FRAME frmDetalj /* ... */
OR "F10" OF PubliserStartDato
DO:
  RUN Cal.w (PubliserStartDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPubliserStoppDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPubliserStoppDato C-Win
ON CHOOSE OF btnPubliserStoppDato IN FRAME frmDetalj /* ... */
OR "F10" OF PubliserStoppDato
DO:
  RUN Cal.w (PubliserStoppDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSetup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSetup C-Win
ON CHOOSE OF btnSetup IN FRAME frmDetalj /* ... */
DO:  
  /* Ta en kikk i prosedyra setSelectorAttributes for videre behandling */
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.
  DEF VAR iy          AS INT  NO-UNDO.

  DEF VAR cKoder      AS CHAR NO-UNDO.
  DEF VAR cNyeKoder   AS CHAR NO-UNDO.
  DEF VAR cTmp      AS CHAR NO-UNDO.
  DEF VAR iz        AS INT  NO-UNDO.

  DO ix = 1 TO NUM-ENTRIES(cOppmerking,"|"):
    DO iy = 1 TO NUM-ENTRIES(ENTRY(ix,cOppmerking,"|")):
      IF NOT CAN-DO(cKoder,ENTRY(iy,ENTRY(ix,cOppmerking,"|"))) THEN
        cKoder = cKoder + TRIM(ENTRY(iy,ENTRY(ix,cOppmerking,"|"))) + ",".
    END.
  END.

  RUN dMesseOppmerk.w (TRIM(cKoder,","),?,OUTPUT cNyeKoder).
  IF cNyeKoder = "" THEN RETURN NO-APPLY.

  EMPTY TEMP-TABLE ttSort.
  DO ix = 1 TO NUM-ENTRIES(cNyeKoder):
    CREATE ttSort.
    ttSort.cSort = TRIM(ENTRY(ix,cNyeKoder)).
  END.
  cNyeKoder = "".
  FOR EACH ttSort BY cSort:
    cNyeKoder = cNyeKoder + ttSort.cSort + ",".
  END.
  cNyeKoder = TRIM(cNyeKoder,",").

/*   cOppmerking = "".                                   */
/*   DO ix = 1 TO NUM-ENTRIES(cNyeKoder):                */
/*     settOppmerking(ENTRY(ix,cNyeKoder),cNyeKoder,ix). */
/*   END.                                                */
/*   cOppmerking = TRIM(cOppmerking,"|").                */

  cKombTegn = "".
  DO ix = 1 TO NUM-ENTRIES(cNyeKoder):
    cTmp = "".
    DO iz = ix TO NUM-ENTRIES(cNyeKoder):
      cTmp = cTmp + ENTRY(iz,cNyeKoder) + ",".
    END.
    cTmp = RIGHT-TRIM(cTmp,",").
    RUN Bygg(cTmp). 
  END.

  cOppmerking = TRIM(cKombTegn,"|").

  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "_file"                               /* Dummy buffer */
                      + ";!_file-name"                      /* <- must invoke a dummy non-visual field to avoid initial sort since calculated fields normally arn't sortable */
                      + ";+Description|CHARACTER|x(30)||Kombinasjon"
                      ,"where false",
                      INPUT-OUTPUT cRowIdList,
                      "Description",
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).

  IF bOk THEN DO:
    cOppmerking = cIdList.
    ViewOppmerking().
    APPLY "any-printable" TO MesseBeskrivelse.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSetup-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSetup-2 C-Win
ON CHOOSE OF btnSetup-2 IN FRAME frmDetalj /* ... */
DO:  
  DEF VAR cText AS CHAR NO-UNDO.

  cText = REPLACE(cOppmerking,"|",CHR(10)).
  RUN JBoxDEditor.w (INPUT-OUTPUT cText,no,no,no,"",OUTPUT bOk).

  IF bOk THEN DO:
    cOppmerking = REPLACE(RIGHT-TRIM(cText,CHR(10)),CHR(10),"|").
    ViewOppmerking().
    APPLY "any-printable" TO MesseBeskrivelse.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSortiment
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSortiment C-Win
ON CHOOSE OF btnSortiment IN FRAME frmDetalj /* ... */
DO:  
  DEF VAR cText AS CHAR NO-UNDO.

  cText = REPLACE(cSortimentkoder,"|",CHR(10)).
  RUN JBoxDEditor.w (INPUT-OUTPUT cText,no,no,no,"",OUTPUT bOk).

  IF bOk THEN DO:
    cSortimentkoder = REPLACE(RIGHT-TRIM(cText,CHR(10)),CHR(10),"|").
    ViewSortimentkoder().
    APPLY "any-printable" TO MesseBeskrivelse.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTilDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTilDato C-Win
ON CHOOSE OF btnTilDato IN FRAME frmDetalj /* ... */
OR "F10" OF TilDato
DO:
  RUN Cal.w (TilDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PostNr C-Win
ON RETURN OF PostNr IN FRAME frmDetalj /* PostNr */
OR "TAB" OF PostNr
DO:
  IF SELF:MODIFIED THEN
  DO:
      Beskrivelse:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Post",
                                              "WHERE PostNr = '" + PostNr:SCREEN-VALUE + "'","Beskrivelse").  
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY "cancel".
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  IF VALID-HANDLE(hIndDet) THEN APPLY "close" TO hIndDet.
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
  RETURN.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  DYNAMIC-FUNCTION("EmbedMe" IN SOURCE-PROCEDURE,THIS-PROCEDURE) NO-ERROR.

  RUN enable_UI.
  RUN InitWindow.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* ON 'default-action':U OF hBrowseListe                   */
/* DO:                                                     */
/*     ASSIGN                                              */
/*         iTab = 2 + 10                                   */
/*         .                                               */
/*     dynamic-function('TabStripChanged',iTab).           */
/*                                                         */
/*     DO WITH FRAME frmDetalj:                            */
/*         APPLY "Entry" TO MesseBeskrivelse.            . */
/*     END.                                                */
/*                                                         */
/* END.                                                    */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bygg C-Win 
PROCEDURE Bygg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM cList AS CHAR NO-UNDO. 
DEF VAR ix        AS INT  NO-UNDO.
DEF VAR iy        AS INT  NO-UNDO.

IF LOOKUP(cList, cKombTegn,"|") = 0 THEN
  cKombTegn = cKombTegn + cList + "|".

DO ix = 2 TO NUM-ENTRIES(cList):
  cTempList = "".

  DO iy = 1 TO NUM-ENTRIES(cList):
    IF iy NE ix THEN
      cTempList = cTempList + ENTRY(iy,cList) + ",".
  END.
  RUN Bygg(RIGHT-TRIM(cTempList,",")).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

OCXFile = SEARCH( "messe.wrx":U ).
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
ELSE MESSAGE "messe.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwOppmerking THEN
  APPLY "choose" TO btnFarge IN FRAME frmDetalj.
ELSE DO:
 TabStripChanged(12).
  APPLY "Entry" TO MesseBeskrivelse.            .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseListe THEN DO:
  IF hFieldMap:AVAIL THEN 
    ASSIGN cOppmerking = REPLACE(hFieldMap:BUFFER-FIELD("Oppmerking"):BUFFER-VALUE,"¤","|")
           cFargekoder = hFieldMap:BUFFER-FIELD("Fargekoder"):BUFFER-VALUE
           cSortimentkoder = REPLACE(hFieldMap:BUFFER-FIELD("Sortimentkoder"):BUFFER-VALUE,"¤","|")
           cKampanjeuker   = REPLACE(hFieldMap:BUFFER-FIELD("Kampanjeuker"):BUFFER-VALUE,"¤","|")
           cKampanjestotte = REPLACE(hFieldMap:BUFFER-FIELD("Kampanjestotte"):BUFFER-VALUE,"¤","|")
           cLagerkoder     = REPLACE(hFieldMap:BUFFER-FIELD("Lagerkoder"):BUFFER-VALUE,"¤","|")
           .
  ELSE ASSIGN cOppmerking     = ""
              cFargekoder     = ""
              cSortimentkoder = ""
              cKampanjeuker   = ""
              cKampanjestotte = ""
              cLagerkoder     = ""
              .
/*   IF cOppmerking NE "" THEN DO:                         */
/*     ASSIGN brwOppmerking:HIDDEN IN FRAME frmDetalj = NO */
/*            hBrwOppmerking:HIDDEN = NO                   */
/*            btnSetup:HIDDEN = NO                         */
/*            btnSetup-2:HIDDEN = NO                       */
/*            btnFarge:HIDDEN = NO.                        */

    ViewOppmerking().
/*   END. */
/*   ELSE ASSIGN brwOppmerking:HIDDEN IN FRAME frmDetalj = YES  */
/*               hBrwOppmerking:HIDDEN = YES                    */
/*               btnSetup:HIDDEN = YES                          */
/*               btnSetup-2:HIDDEN = YES                        */
/*               btnFarge:HIDDEN = YES.                         */

  ViewSortimentkoder().
  ViewKampanjeuker().
  ViewKampanjestotte().
  ViewLagerkoder().
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
  DISPLAY MesseNr MesseBeskrivelse MesseFraDato MesseTilDato FraDato TilDato 
          PubliserStartDato PubliserStoppDato KontaktPers Telefon MobilTlf 
          Telefaks Adresse1 Adresse2 PostBoks PostNr Beskrivelse 
      WITH FRAME frmDetalj IN WINDOW C-Win.
  ENABLE btnFarge brwOppmerking brwSortimentkoder brwKampanjeuker 
         brwKampanjestotte brwLagerkoder MesseBeskrivelse MesseFraDato 
         MesseTilDato FraDato TilDato PubliserStartDato PubliserStoppDato 
         KontaktPers Telefon MobilTlf Telefaks Adresse1 Adresse2 PostBoks 
         PostNr btnBrukt btnSortiment btnLagerkoder btnKampanjeuker 
         btnKampanjestotte btnMesseFraDato btnMesseTilDato 
         btnPublilserStartDato btnPubliserStoppDato btnTilDato btnFraDato 
         btnPostnr 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitStringTranslation C-Win 
PROCEDURE InitStringTranslation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM        ihWindow    AS HANDLE NO-UNDO.
DEF INPUT-OUTPUT PARAM iocTypeList AS CHAR   NO-UNDO.

DEF VAR cCurrValue AS CHAR NO-UNDO.

IF ihWindow NE THIS-PROCEDURE:CURRENT-WINDOW THEN RETURN.

/*
IF NOT CAN-DO(iocTypeList,"MesseType") THEN
  iocTypeList = iocTypeList + ",MesseType".

ASSIGN cCurrValue = MesseType:SCREEN-VALUE IN FRAME frmDetalj
       MesseType:LIST-ITEM-PAIRS = 
                 DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"MesseType","1 / Forhåndsordre|1|2 / Suppleringsordre|2")
       MesseType:SCREEN-VALUE = cCurrValue.
*/
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
DEF VAR hSearchField AS HANDLE NO-UNDO.

SetTabStrip().
TabStripChanged(11).

cAdgang = (DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 16 and SysGr = 40 and ParaNr = 2","Parameter1")).  

DO WITH FRAME frmListe:
  hBrowseListe = DYNAMIC-FUNCTION("NewBrowse",
                    rectBrowseListe:HANDLE IN FRAME frmListe, 
                    200,                   
                    "NUM-LOCKED-COLUMNS|1",
                    "Messe"
                    + ";MesseNr|Innkj. periode"
                    + ";MesseBeskrivelse|Beskrivelse"
                    + ";MesseFraDato|Fra dato"
                    + ";MesseTilDato|Til dato"
                    + ";FraDato|Tilg. fra dato"
                    + ";TilDato|Tilg. til dato"
                    + ";PubliserStartDato|Publiser fra dato"
                    + ";PubliserStoppDato|Publiser til dato"
                    + ";KontaktPers"
                    + ";Telefon"
                    + ";MobilTlf"
                    + ";Telefaks"
                    + ";Adresse1"
                    + ";Adresse2"
                    + ";PostBoks"
                    + ";PostNr"
                    + ";!Oppmerking"
                    + ";!Fargekoder"
                    + ";!Sortimentkoder"
                    + ";!Kampanjeuker"
                    + ";!Kampanjestotte"
                    + ";!Lagerkoder"
                  + ",Post;Beskrivelse",
                       ",FIRST Post NO-LOCK where Post.PostNr = Messe.PostNr OUTER-JOIN",
                    "sort|MesseNr desc"). 
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
  /*DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"basequery","WHERE Individ.MesseNr = 1 ").*/
  /*DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"sortmap","cRegTid;Tid").*/
/*   DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"sortmap","cPostNr;PostNr"). */
/*   DYNAMIC-FUNCTION("setAttribute",hBrowseListe,"NoColumnSearch","cPostNr").     */

END.

DO WITH FRAME frmDetalj:

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins)
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowseListe:QUERY,
                    FRAME frmDetalj:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                                                    /* Nb: Felt som brukes her må være hentet i Browser først.                        */
                    "MesseBeskrivelse,FraDato,TilDato,MesseFraDato,MesseTilDato,PubliserStartDato,PubliserStoppDato,KontaktPers,Telefon,MobilTlf,Telefaks,Adresse1,Adresse2,PostBoks,PostNr","",       
                    "MesseNr,Beskrivelse",        
                    "",  
                    "btnMesseFraDato,btnMesseTilDato,btnFraDato,btnTilDato,btnPubliserStartDato,btnPubliserStoppDato,btnPostnr,btnSortiment,btnFarge"). 

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowseListe,hFieldMap).
  /* Ignore = Ingen validering. Ellers skriv navn på program som gjør validering. */
  /* Dynamisk validering er default. Tileggsvalidering +<ProcNavn>.               */
  /* Kun egen validering, uten dynaisk validering =<ProcNavn>.                    */
  /* Dynamisk validering: Fremednøkkel - Felt må være indeksert og må finnes som  */
  /*                      primærnøkkel i en annen tabell.                         */
  /* DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore").   */
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore"). 
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraFields","Sortimentkoder,Fargekoder,Kampanjeuker,Kampanjestotte,Lagerkoder").
/*   DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore"). */

  /* Felter som skal være disablet. Comma separert. */
  /*DYNAMIC-FUNCTION("setAttribute",hFieldMap,"disabledFields","MesseNr"). */

  hBrwOppmerking = DYNAMIC-FUNCTION("NewBrowse",
                   brwOppmerking:HANDLE IN FRAME frmDetalj, 
                   200,                   
                   "multiple",
                   "temp-table"
                 + ";+Oppmerking|CHARACTER|X(256)||Oppmerking"
                 + ";!+iColor|INTEGER|>>>>>>>9"
                 + ";!+iColNum256|INTEGER|>>9"
                   ,"WHERE false"
                   ,"").
  hBuffOppmerking = hBrwOppmerking:QUERY:GET-BUFFER-HANDLE(1).
  DYNAMIC-FUNCTION("setAttribute",hBrwOppmerking,"uselocaldata","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrwOppmerking,"querysort","Oppmerking").

  ASSIGN hFieldColor       = hBuffOppmerking:BUFFER-FIELD("iColor")
         hFieldColNum256   = hBuffOppmerking:BUFFER-FIELD("iColNum256")
         hColumnOppmerking = hBrwOppmerking:GET-BROWSE-COLUMN(1).

  hBrwSortimentkoder = DYNAMIC-FUNCTION("NewBrowse",
                   brwSortimentkoder:HANDLE IN FRAME frmDetalj, 
                   200,                   
                   "multiple",
                   "temp-table"
                 + ";+Sortiment|CHARACTER|X(256)"
                 + ";!+iColor|INTEGER|>>>>>>>9"
                 + ";!+iColNum256|INTEGER|>>9"
                   ,"WHERE false"
                   ,"").
  hBuffSortimentkoder = hBrwSortimentkoder:QUERY:GET-BUFFER-HANDLE(1).
  DYNAMIC-FUNCTION("setAttribute",hBrwSortimentkoder,"uselocaldata","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrwSortimentkoder,"querysort","Sortiment").
  ASSIGN hFieldColorSort       = hBuffSortimentkoder:BUFFER-FIELD("iColor")
         hFieldColNum256Sort   = hBuffSortimentkoder:BUFFER-FIELD("iColNum256")
         hColumnOppmerkingSort = hBrwSortimentkoder:GET-BROWSE-COLUMN(1).

  hBrwKampanjeuker = DYNAMIC-FUNCTION("NewBrowse",
                   brwKampanjeuker:HANDLE IN FRAME frmDetalj, 
                   200,                   
                   "multiple",
                   "temp-table"
                 + ";+Kampanjeuke|CHARACTER|X(256)"
                   ,"WHERE false"
                   ,"").
  hBuffKampanjeuker = hBrwKampanjeuker:QUERY:GET-BUFFER-HANDLE(1).
  DYNAMIC-FUNCTION("setAttribute",hBrwKampanjeuker,"uselocaldata","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrwKampanjeuker,"querysort","Kampanjeuke").

  hBrwKampanjestotte = DYNAMIC-FUNCTION("NewBrowse",
                   brwKampanjestotte:HANDLE IN FRAME frmDetalj, 
                   200,                   
                   "multiple",
                   "temp-table"
                 + ";+Kampanjestotte|CHARACTER|X(256)||Kampanjestøtte"
                   ,"WHERE false"
                   ,"").
  hBuffKampanjestotte = hBrwKampanjestotte:QUERY:GET-BUFFER-HANDLE(1).
  DYNAMIC-FUNCTION("setAttribute",hBrwKampanjestotte,"uselocaldata","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrwKampanjestotte,"querysort","Kampanjestotte").

  hBrwLagerkoder = DYNAMIC-FUNCTION("NewBrowse",
                   brwLagerkoder:HANDLE IN FRAME frmDetalj, 
                   200,                   
                   "multiple",
                   "temp-table"
                 + ";+Lagerkode|CHARACTER|X(256)"
                   ,"WHERE false"
                   ,"").
  hBuffLagerkoder = hBrwLagerkoder:QUERY:GET-BUFFER-HANDLE(1).
  DYNAMIC-FUNCTION("setAttribute",hBrwLagerkoder,"uselocaldata","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrwLagerkoder,"querysort","Lagerkode").
END.

DO WITH FRAME {&FRAME-NAME}:
  hUpdToolBar = DYNAMIC-FUNCTION("NewToolBar",
                  rectUpdToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                  "Fil",                          /* Corresponding menu label - no menu if blank */
                  "New;Ny,Undo;Angre,delete;Slett,save;Lagre,excel;Eksporter til E&xcel,first;Første,prev;Forrige,next;Neste,last;Siste"
                + ",rule,MesseButikk;Butikker;Velg butikker som kan se varebok (hvis ingen er valg er messen åpen for alle)"
                                                  /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                     Any number of properties accepted (one ok - if predef. action) */
                  ,"maxborder").                   /* Misc - for something I might need in next version.. */

  DYNAMIC-FUNCTION("CreateObjectLink",hUpdToolbar,hBrowseListe).
  DYNAMIC-FUNCTION("CreateObjectLink",hUpdToolbar,hFieldMap).

  hToolBar = DYNAMIC-FUNCTION("NewToolBar",
                      rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                      "Fil",                          /* Corresponding menu label - no menu if blank */
                      "Help,Close;Avslutt",
                                                      /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                         Any number of properties accepted (one ok - if predef. action) */
                      "").                            /* Misc - for something I might need in next version.. */
  
  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearchListe:HANDLE,hBrowseListe,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowseListe).

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmDetalj:HANDLE, "brwOppmerking," + hBrwOppmerking:NAME).
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frmDetalj:HANDLE, 
                   "brwSortimentkoder," + hBrwSortimentkoder:NAME
                 + ",brwKampanjeuker," + hBrwKampanjeuker:NAME
                 + ",brwKampanjestotte," + hBrwKampanjestotte:NAME
                 + ",brwLagerkoder," + hBrwLagerkoder:NAME
                   ).
  DYNAMIC-FUNCTION("setNoMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"btnLagerkoder").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectUpdToolBar").
  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,450,500,0,0).

  SUBSCRIBE TO "InitStringTranslation" ANYWHERE.
  DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  DYNAMIC-FUNCTION("SetToolbar",hToolBar,"enable").

END.

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

APPLY "ENTRY" TO hBrowseListe.
IF hBrowseListe:NUM-ITERATIONS > 0 THEN
    APPLY "value-changed" TO hBrowseListe.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MesseButikkRecord C-Win 
PROCEDURE MesseButikkRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList  AS CHAR NO-UNDO.
DEF VAR cIdList     AS CHAR NO-UNDO.  
DEF VAR bOk         AS LOG  NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                              "MesseForButikk,Butiker",      /* Buffer(list) for query */
                              "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                              "where Messenr = " + STRING(hFieldMap:BUFFER-FIELD("MesseNr"):BUFFER-VALUE)
                              + ",FIRST Butiker WHERE Butiker.Butik = MesseForButikk.ButikkNr").

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "Butiker"      
                    + ";Butik"  
                    + ";ButNamn"
                    ,"where true",
                    INPUT-OUTPUT cRowIdList,
                    "Butik", /* Primary key */
                    INPUT-OUTPUT cIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  IF NOT DYNAMIC-FUNCTION("runproc","messebutikker.p",STRING(hFieldMap:BUFFER-FIELD("MesseNr"):BUFFER-VALUE) + "," + cIdList,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

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
  TabStripChanged(12).

ASSIGN cOppmerking = ""
       cFargekoder = ""
       cSortimentkoder = ""
       cKampanjeuker = ""  
       cKampanjestotte = ""
       cLagerkoder = ""    
       .
IF hBuffOppmerking:AVAIL THEN
  hFieldColNum256:BUFFER-VALUE = 0.

RUN SUPER.

hBuffOppmerking:EMPTY-TEMP-TABLE().
hBrwOppmerking:QUERY:QUERY-CLOSE().

hBuffSortimentkoder:EMPTY-TEMP-TABLE().
hBrwSortimentkoder:QUERY:QUERY-CLOSE().
hBuffKampanjeuker:EMPTY-TEMP-TABLE().
hBrwKampanjeuker:QUERY:QUERY-CLOSE().
hBuffKampanjestotte:EMPTY-TEMP-TABLE().
hBrwKampanjestotte:QUERY:QUERY-CLOSE().
hBuffLagerkoder:EMPTY-TEMP-TABLE().
hBrwLagerkoder:QUERY:QUERY-CLOSE().

/* ASSIGN brwOppmerking:HIDDEN IN FRAME frmDetalj = YES  */
/*        hBrwOppmerking:HIDDEN = YES                    */
/*        btnSetup:HIDDEN = YES                          */
/*        btnSetup-2:HIDDEN = YES                        */
/*        btnFarge:HIDDEN = YES.                         */

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
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwOppmerking AND hFieldColNum256:BUFFER-VALUE > 0 THEN
  hColumnOppmerking:BGCOLOR = hFieldColNum256:BUFFER-VALUE.
ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwSortimentkoder AND hFieldColNum256Sort:BUFFER-VALUE > 0 THEN
  hColumnOppmerkingSort:BGCOLOR = hFieldColNum256Sort:BUFFER-VALUE.

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


/*     IF cState = "NEW" THEN */
    DO:
        IF TilDato:SCREEN-VALUE = "" OR INPUT FraDato = ? THEN
        DO:
            MESSAGE "Fra og til dato er ikke angitt."
                VIEW-AS ALERT-BOX WARNING BUTTONS OK  TITLE "Lagringskontroll".
            APPLY "ENTRY" TO TilDato.
            RETURN NO-APPLY.
        END.
      IF INPUT TilDato < INPUT FraDato THEN
      DO:
          MESSAGE "Tildato er mindre enn fra dato"
              VIEW-AS ALERT-BOX WARNING BUTTONS OK  TITLE "Lagringskontroll".
          APPLY "ENTRY" TO TilDato.
          RETURN NO-APPLY.
      END.
    END.
    IF (PostNr:SCREEN-VALUE) <> "" THEN
    DO:
        IF (PostNr:SCREEN-VALUE) <> (DYNAMIC-FUNCTION("getFieldValues","Post","WHERE PostNr = '" + PostNr:SCREEN-VALUE + "'","PostNr")) THEN
        DO:
            MESSAGE "Ugyldig postnummer."
                VIEW-AS ALERT-BOX WARNING BUTTONS OK  TITLE "Lagringskontroll".
            APPLY "ENTRY" TO PostNr.
            RETURN NO-APPLY.
        END.
    END.

    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues",getKoder()).
/*     IF cOppmerking NE ? THEN                                                            */
/*       DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues",getMerknadFarge()). */
/*       DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues",REPLACE(cOppmerking,"|","¤") + "|" + cFargekoder). */
  END.
  

  RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSelectorAttributes C-Win 
PROCEDURE setSelectorAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihSourceBrw   AS HANDLE NO-UNDO.
DEF INPUT PARAM ihTargetBrw   AS HANDLE NO-UNDO.

DEF VAR hSourceBuffer  AS HANDLE NO-UNDO.
DEF VAR hTargetBuffer  AS HANDLE NO-UNDO.
DEF VAR iy             AS INT    NO-UNDO.
DEF VAR cSelectedRows  AS CHAR   NO-UNDO.

IF ihSourceBrw:QUERY:GET-BUFFER-HANDLE(1):NAME = "Butiker" THEN RETURN.

hSourceBuffer = ihSourceBrw:QUERY:GET-BUFFER-HANDLE(1).
hTargetBuffer = ihTargetBrw:QUERY:GET-BUFFER-HANDLE(1).

DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"basequery","where true").
DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"querysort","Description").
DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"uselocaldata","yes").
DYNAMIC-FUNCTION("setAttribute",ihTargetBrw,"uselocaldata","yes").

/* Fill temp-table: */
DO ix = 1 TO NUM-ENTRIES(cOppmerking,"|"):
  hSourceBuffer:BUFFER-CREATE().
  ASSIGN hSourceBuffer:BUFFER-FIELD("Description"):BUFFER-VALUE = ENTRY(ix,cOppmerking,"|")
         hSourceBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix)
         .

  /* If any records should be pre-selected: */
  bOk = hBuffOppmerking:FIND-FIRST("WHERE Oppmerking = '" + ENTRY(ix,cOppmerking,"|") + "'") NO-ERROR.
  IF bOk THEN DO:
    hTargetBuffer:BUFFER-CREATE().
    ASSIGN hTargetBuffer:BUFFER-FIELD("Description"):BUFFER-VALUE = ENTRY(ix,cOppmerking,"|")
           hTargetBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix)
           cSelectedRows = cSelectedRows + "rowid" + STRING(ix) + ","
           .
  END.
END.

DYNAMIC-FUNCTION("setSelectedRowids" IN SOURCE-PROCEDURE,cSelectedRows).

DYNAMIC-FUNCTION("setCurrentObject",ihSourceBrw).
RUN OpenQuerySource IN SOURCE-PROCEDURE.

DYNAMIC-FUNCTION("setCurrentObject",ihTargetBrw).
RUN OpenQueryTarget IN SOURCE-PROCEDURE.

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
          MesseNr:SENSITIVE = FALSE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKoder C-Win 
FUNCTION getKoder RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cKodeliste AS CHAR NO-UNDO.
DEF VAR cFarge     AS CHAR NO-UNDO.
DEF VAR cVerdi     AS CHAR NO-UNDO.

IF hBrwSortimentkoder:QUERY:IS-OPEN THEN DO:    
  hBrwSortimentkoder:QUERY:GET-FIRST().
  REPEAT WHILE NOT hBrwSortimentkoder:QUERY:QUERY-OFF-END:
    ASSIGN cKodeliste = cKodeliste + hBuffSortimentkoder:BUFFER-FIELD("Sortiment"):BUFFER-VALUE + "¤"
/*            cFarge     = cFarge + STRING(hBuffSortimentkoder:BUFFER-FIELD("iColor"):BUFFER-VALUE) + "," */
           .
    hBrwSortimentkoder:QUERY:GET-NEXT().
  END.
END.

IF hBrwOppmerking:QUERY:IS-OPEN THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cOppmerking,"|"):
    bOk = hBuffOppmerking:FIND-FIRST("WHERE Oppmerking = '" + ENTRY(ix,cOppmerking,"|") + "'") NO-ERROR.
    IF bOk THEN 
     cFarge   = cFarge + STRING(hBuffOppmerking:BUFFER-FIELD("iColor"):BUFFER-VALUE) + ",".
  END.
/*   hBrwOppmerking:QUERY:GET-FIRST().                                                       */
/*   REPEAT WHILE NOT hBrwOppmerking:QUERY:QUERY-OFF-END:                                    */
/*    cFarge   = cFarge + STRING(hBuffOppmerking:BUFFER-FIELD("iColor"):BUFFER-VALUE) + ",". */
/*    hBrwOppmerking:QUERY:GET-NEXT().                                                       */
/*   END.                                                                                    */
END.
cKodeliste = TRIM(cKodeliste,"¤") + "|" + (IF cFarge NE "" THEN SUBSTR(cFarge,1,LENGTH(cFarge) - 1) ELSE "") + "|".

IF hBrwKampanjeuker:QUERY:IS-OPEN THEN DO:    
  hBrwKampanjeuker:QUERY:GET-FIRST().
  REPEAT WHILE NOT hBrwKampanjeuker:QUERY:QUERY-OFF-END:
    cVerdi = cVerdi + hBuffKampanjeuker:BUFFER-FIELD("Kampanjeuke"):BUFFER-VALUE + "¤".
    hBrwKampanjeuker:QUERY:GET-NEXT().
  END.
END.
ASSIGN cKodeliste = cKodeliste + TRIM(cVerdi,"¤") + "|"
       cVerdi     = "".

IF hBrwKampanjestotte:QUERY:IS-OPEN THEN DO:    
  hBrwKampanjestotte:QUERY:GET-FIRST().
  REPEAT WHILE NOT hBrwKampanjestotte:QUERY:QUERY-OFF-END:
    cVerdi = cVerdi + hBuffKampanjestotte:BUFFER-FIELD("Kampanjestotte"):BUFFER-VALUE + "¤".
    hBrwKampanjestotte:QUERY:GET-NEXT().
  END.
END.
ASSIGN cKodeliste = cKodeliste + TRIM(cVerdi,"¤") + "|"
       cVerdi     = "".

IF hBrwLagerkoder:QUERY:IS-OPEN THEN DO:    
  hBrwLagerkoder:QUERY:GET-FIRST().
  REPEAT WHILE NOT hBrwLagerkoder:QUERY:QUERY-OFF-END:
    cVerdi = cVerdi + hBuffLagerkoder:BUFFER-FIELD("Lagerkode"):BUFFER-VALUE + "¤".
    hBrwLagerkoder:QUERY:GET-NEXT().
  END.
END.
cKodeliste = cKodeliste + TRIM(cVerdi,"¤").


RETURN cKodeliste.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMerknadFarge C-Win 
FUNCTION getMerknadFarge RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cMerknad AS CHAR NO-UNDO.
DEF VAR cFarge   AS CHAR NO-UNDO.

IF NOT hBrwOppmerking:QUERY:IS-OPEN THEN RETURN "|".

hBrwOppmerking:QUERY:GET-FIRST().
REPEAT WHILE NOT hBrwOppmerking:QUERY:QUERY-OFF-END:
  ASSIGN cMerknad = cMerknad + hBuffOppmerking:BUFFER-FIELD("Oppmerking"):BUFFER-VALUE + "¤"
         cFarge   = cFarge + STRING(hBuffOppmerking:BUFFER-FIELD("iColor"):BUFFER-VALUE) + ",".
  hBrwOppmerking:QUERY:GET-NEXT().
END.

RETURN TRIM(cMerknad,"¤") + "|" + (IF cFarge NE "" THEN SUBSTR(cFarge,1,LENGTH(cFarge) - 1) ELSE "").   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFieldColor C-Win 
FUNCTION setFieldColor RETURNS INTEGER
  ( INPUT iRGBcolor AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iy AS INT NO-UNDO.

IF iRGBcolor = 0 THEN RETURN 15.

IF iRGBcolor NE 0 THEN DO iy = 0 TO COLOR-TABLE:NUM-ENTRIES:
  IF COLOR-TABLE:GET-RGB-VALUE(iy) = iRGBcolor THEN RETURN iy.
END.

ASSIGN iy = COLOR-TABLE:NUM-ENTRIES
       COLOR-TABLE:NUM-ENTRIES = iy + 1.

IF iy = 256 THEN
  MESSAGE PROGRAM-NAME(1) SKIP
          256 SKIP
          VIEW-AS ALERT-BOX.

COLOR-TABLE:SET-DYNAMIC(iy, yes).
COLOR-TABLE:SET-RGB-VALUE(iy,iRGBcolor).

RETURN iy.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION settOppmerking C-Win 
FUNCTION settOppmerking RETURNS LOGICAL
  ( INPUT icAddText  AS CHAR,
    INPUT icAllText  AS CHAR,
    INPUT iStartPos  AS INT):

DEF VAR iy AS INT NO-UNDO.
DEF VAR iz AS INT NO-UNDO.
DEF VAR cAddText AS CHAR.

IF LOOKUP(icAddText,cOppmerking,"|") = 0 THEN
  cOppmerking = cOppmerking + icAddText + "|".
DO iy = iStartPos TO NUM-ENTRIES(icAllText):
  iz = iz + 1.
  cAddText = cAddText + ENTRY(iy,icAllText) + ",".
  DO iz = iStartPos TO NUM-ENTRIES(icAllText):
    IF NOT CAN-DO(cAddText,ENTRY(iz,icAllText)) THEN 
      settOppmerking (cAddText + ENTRY(iz,icAllText),icAllText,iz).
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
DO WITH FRAME {&FRAME-NAME}:
  IF iiTab > 10 THEN DO:
    iTab = iiTab - 10.
    chTabStrip:TabStrip:Tabs:ITEM(iTab):SELECTED = TRUE.
  END.
  ELSE iTab = iiTab.

  IF iTab = 1 THEN
    FRAME frmListe:MOVE-TO-TOP().
  ELSE IF iTab = 2 THEN DO:
    FRAME frmDetalj:MOVE-TO-TOP().
    IF iiTab < 10 THEN
      APPLY "value-changed" TO hBrowseListe.
  END.

  RETURN TRUE.
END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewKampanjestotte C-Win 
FUNCTION ViewKampanjestotte RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hBuffKampanjestotte:EMPTY-TEMP-TABLE().
DO ix = 1 TO NUM-ENTRIES(cKampanjestotte,"|"):
  hBuffKampanjestotte:BUFFER-CREATE().
  hBuffKampanjestotte:BUFFER-FIELD("Kampanjestotte"):BUFFER-VALUE = TRIM(ENTRY(ix,cKampanjestotte,"|")).
END.
DYNAMIC-FUNCTION("setCurrentObject",hBrwKampanjestotte).
RUN OpenQuery.
  
IF hBrwKampanjestotte:FOCUSED-ROW NE ? THEN
  hBrwKampanjestotte:DESELECT-FOCUSED-ROW().

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewKampanjeuker C-Win 
FUNCTION ViewKampanjeuker RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hBuffKampanjeuker:EMPTY-TEMP-TABLE().
DO ix = 1 TO NUM-ENTRIES(cKampanjeuker,"|"):
  hBuffKampanjeuker:BUFFER-CREATE().
  hBuffKampanjeuker:BUFFER-FIELD("Kampanjeuke"):BUFFER-VALUE = TRIM(ENTRY(ix,cKampanjeuker,"|")).
END.
DYNAMIC-FUNCTION("setCurrentObject",hBrwKampanjeuker).
RUN OpenQuery.
  
IF hBrwKampanjeuker:FOCUSED-ROW NE ? THEN
  hBrwKampanjeuker:DESELECT-FOCUSED-ROW().

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewLagerkoder C-Win 
FUNCTION ViewLagerkoder RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hBuffLagerkoder:EMPTY-TEMP-TABLE().
DO ix = 1 TO NUM-ENTRIES(cLagerkoder,"|"):
  hBuffLagerkoder:BUFFER-CREATE().
  hBuffLagerkoder:BUFFER-FIELD("Lagerkode"):BUFFER-VALUE = TRIM(ENTRY(ix,cLagerkoder,"|")).
END.
DYNAMIC-FUNCTION("setCurrentObject",hBrwLagerkoder).
RUN OpenQuery.
  
IF hBrwLagerkoder:FOCUSED-ROW NE ? THEN
  hBrwLagerkoder:DESELECT-FOCUSED-ROW().

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewOppmerking C-Win 
FUNCTION ViewOppmerking RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hBuffOppmerking:EMPTY-TEMP-TABLE().
DO ix = 1 TO NUM-ENTRIES(cOppmerking,"|"):
  hBuffOppmerking:BUFFER-CREATE().
  hBuffOppmerking:BUFFER-FIELD("Oppmerking"):BUFFER-VALUE = TRIM(ENTRY(ix,cOppmerking,"|")).
  IF NUM-ENTRIES(cFargekoder) GE ix THEN 
    ASSIGN hFieldColor:BUFFER-VALUE = INT(ENTRY(ix,cFargekoder))
           hFieldColNum256:BUFFER-VALUE = setFieldColor(hFieldColor:BUFFER-VALUE).
END.
DYNAMIC-FUNCTION("setCurrentObject",hBrwOppmerking).
RUN OpenQuery.
  
IF hBrwOppmerking:FOCUSED-ROW NE ? THEN
  hBrwOppmerking:DESELECT-FOCUSED-ROW().

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewSortimentkoder C-Win 
FUNCTION ViewSortimentkoder RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hBuffSortimentkoder:EMPTY-TEMP-TABLE().
DO ix = 1 TO NUM-ENTRIES(cSortimentkoder,"|"):
  hBuffSortimentkoder:BUFFER-CREATE().
  hBuffSortimentkoder:BUFFER-FIELD("Sortiment"):BUFFER-VALUE = TRIM(ENTRY(ix,cSortimentkoder,"|")).
/*   IF NUM-ENTRIES(cFargekoder) GE ix THEN                                                    */
/*     ASSIGN hFieldColorSort:BUFFER-VALUE = INT(ENTRY(ix,cFargekoder))                        */
/*            hFieldColNum256Sort:BUFFER-VALUE = setFieldColor(hFieldColorSort:BUFFER-VALUE).  */
END.
DYNAMIC-FUNCTION("setCurrentObject",hBrwSortimentkoder).
RUN OpenQuery.
  
IF hBrwSortimentkoder:FOCUSED-ROW NE ? THEN
  hBrwSortimentkoder:DESELECT-FOCUSED-ROW().

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

