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
rectWinToolbar RECT-64 searchField fiRabSjekkTypeNr fiBelop fiFraDatoGyldig ~
fiTilDatoGyldig fiMedlemsNr Fornavn fiRabSjekkSerienr fiFraDatoUtstedt ~
fiTilDatoUtstedt fiButikkNr Etternavn fiBruktButikkNr fiFraDatoSkrevet ~
fiTilDatoSkrevet BUTTON-1 fiFraDatoBrukt fiTilDatoBrukt fiBrukt MedlemsNr ~
ButikkNr Belop RabSjekkSerienr DatoGyldig DatoUtstedt DatoSkrevet DatoBrukt ~
BruktButikkNr Notat B-sokGetSerienr B-sokBruktButikk-2 B-sokBruktButikk ~
btnCalFraDato-9 B-sokRabSjekkTypeNr-2 btnCalFraDato-5 btnCalFraDato-6 ~
btnCalFraDato-10 btnCalFraDato-11 btnCalFraDato-7 btnCalFraDato-12 ~
btnCalFraDato-8 btnCalFraDato-4 B-sokMedlem btnCalFraDato B-sokButik ~
B-sokMedlem2 B-sokRabSjekkTypeNr btnCalFraDato-2 btnCalFraDato-3 
&Scoped-Define DISPLAYED-OBJECTS fiRabSjekkTypeNr fiBelop fiFraDatoGyldig ~
fiTilDatoGyldig fiMedlemsNr Fornavn fiRabSjekkSerienr fiFraDatoUtstedt ~
fiTilDatoUtstedt fiButikkNr Etternavn fiBruktButikkNr fiFraDatoSkrevet ~
fiTilDatoSkrevet fiFraDatoBrukt fiTilDatoBrukt fiBrukt RabSjekkTypeNr ~
MedlemsNr ButikkNr Belop RabSjekkSerienr DatoGyldig DatoUtstedt DatoSkrevet ~
DatoBrukt BruktButikkNr Notat 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-sokBruktButikk 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-sokBruktButikk-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-sokButik 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-sokButik-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-sokGetSerienr  NO-FOCUS
     LABEL "Serienr" 
     SIZE 9.6 BY 1.

DEFINE BUTTON B-sokMedlem 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-sokMedlem2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-sokRabSjekkTypeNr 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-sokRabSjekkTypeNr-2 
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

DEFINE BUTTON btnCalFraDato-11 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraDato-12 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraDato-2 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraDato-3 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraDato-4 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraDato-5 
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

DEFINE BUTTON BUTTON-1 
     LABEL "Blank filter" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE Notat AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 42 BY 12.62 NO-UNDO.

DEFINE VARIABLE Belop AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Beløp" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE BruktButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Brukt i butikk" 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1.

DEFINE VARIABLE ButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikknr" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE DatoBrukt AS DATE FORMAT "99/99/99" 
     LABEL "Dato brukt" 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1.

DEFINE VARIABLE DatoGyldig AS DATE FORMAT "99/99/99" 
     LABEL "Gyldig til" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE DatoSkrevet AS DATE FORMAT "99/99/99" 
     LABEL "Utskrevet" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Utstedelsesdato for rabattsjekk.".

DEFINE VARIABLE DatoUtstedt AS DATE FORMAT "99/99/99" 
     LABEL "Utstedt" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Utstedelsesdato for rabattsjekk.".

DEFINE VARIABLE Etternavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Etternavn" 
     VIEW-AS FILL-IN 
     SIZE 20.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiBelop AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Beløp" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE fiBruktButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Brukt i butikk" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE fiButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikknr" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE fiFraDatoBrukt AS DATE FORMAT "99/99/99" 
     LABEL "Fra/til brukt dato" 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1.

DEFINE VARIABLE fiFraDatoGyldig AS DATE FORMAT "99/99/99" 
     LABEL "Fra/til gyldig dato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE fiFraDatoSkrevet AS DATE FORMAT "99/99/99" 
     LABEL "Fra/til skrevet dato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Fra utstedelsesdato for rabattsjekk.".

DEFINE VARIABLE fiFraDatoUtstedt AS DATE FORMAT "99/99/99" 
     LABEL "Fra/til utstedt dato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Utstedelsesdato for rabattsjekk.".

DEFINE VARIABLE fiMedlemsNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Medlemsnr" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 TOOLTIP "Medlemsnummer".

DEFINE VARIABLE fiRabSjekkSerienr AS DECIMAL FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Serienr" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE fiRabSjekkTypeNr AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Rabattsjekk" 
     VIEW-AS FILL-IN 
     SIZE 4.8 BY 1 TOOLTIP "Unikt nr. som identifiserer rabattsjekktypen.".

DEFINE VARIABLE fiTilDatoBrukt AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1.

DEFINE VARIABLE fiTilDatoGyldig AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE fiTilDatoSkrevet AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Utstedelsesdato for rabattsjekk.".

DEFINE VARIABLE fiTilDatoUtstedt AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Utstedelsesdato for rabattsjekk.".

DEFINE VARIABLE Fornavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fornavn" 
     VIEW-AS FILL-IN 
     SIZE 20.6 BY 1 NO-UNDO.

DEFINE VARIABLE MedlemsNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Medlemsnr" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 TOOLTIP "Medlemsnummer".

DEFINE VARIABLE RabSjekkSerienr AS DECIMAL FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Serienr" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE RabSjekkTypeNr AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Rabattsjekk type" 
     VIEW-AS FILL-IN 
     SIZE 4.8 BY 1 TOOLTIP "Unikt nr. som identifiserer rabattsjekktypen.".

DEFINE RECTANGLE RECT-64
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45 BY 24.29.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 157 BY 24.29.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY .95.

DEFINE VARIABLE fiBrukt AS LOGICAL INITIAL yes 
     LABEL "Vis bare ubrukte" 
     VIEW-AS TOGGLE-BOX
     SIZE 38.2 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-sokButik-2 AT ROW 4.43 COL 44.6
     fiRabSjekkTypeNr AT ROW 2.38 COL 22.6 COLON-ALIGNED HELP
          "Bilagstype"
     fiBelop AT ROW 2.38 COL 97.6 COLON-ALIGNED HELP
          "Beløp det ble handlet for"
     fiFraDatoGyldig AT ROW 2.43 COL 142 COLON-ALIGNED HELP
          "Siste gyldighetsdato på rabattsjekken"
     fiTilDatoGyldig AT ROW 2.43 COL 164 COLON-ALIGNED HELP
          "Siste gyldighetsdato på rabattsjekken" NO-LABEL
     fiMedlemsNr AT ROW 3.38 COL 22.6 COLON-ALIGNED HELP
          "Peker til artikkel"
     Fornavn AT ROW 3.38 COL 62.4 COLON-ALIGNED
     fiRabSjekkSerienr AT ROW 3.38 COL 97.6 COLON-ALIGNED HELP
          "Sjekkens serienr (Butikkens nummerserie)"
     fiFraDatoUtstedt AT ROW 3.38 COL 142 COLON-ALIGNED HELP
          "Dato da sjekken ble utsted"
     fiTilDatoUtstedt AT ROW 3.38 COL 164 COLON-ALIGNED HELP
          "Dato da sjekken ble utsted" NO-LABEL
     fiButikkNr AT ROW 4.38 COL 22.6 COLON-ALIGNED HELP
          "Nr. på butikk som har utstedtsjekken"
     Etternavn AT ROW 4.38 COL 62.4 COLON-ALIGNED
     fiBruktButikkNr AT ROW 4.38 COL 97.6 COLON-ALIGNED HELP
          "Rabattsjekk er brukt i butikk"
     fiFraDatoSkrevet AT ROW 4.38 COL 142 COLON-ALIGNED HELP
          "Dato da sjekken ble utsted"
     fiTilDatoSkrevet AT ROW 4.38 COL 164 COLON-ALIGNED HELP
          "Dato da sjekken ble utsted" NO-LABEL
     BUTTON-1 AT ROW 5.19 COL 189
     fiFraDatoBrukt AT ROW 5.38 COL 142 COLON-ALIGNED HELP
          "Dato da rabattsjekken ble brukt"
     fiTilDatoBrukt AT ROW 5.38 COL 164 COLON-ALIGNED HELP
          "Dato da rabattsjekken ble brukt" NO-LABEL
     fiBrukt AT ROW 5.57 COL 24.4
     RabSjekkTypeNr AT ROW 7.67 COL 175.6 COLON-ALIGNED HELP
          "Bilagstype"
     MedlemsNr AT ROW 8.67 COL 175.6 COLON-ALIGNED HELP
          "Peker til artikkel"
     ButikkNr AT ROW 9.67 COL 175.6 COLON-ALIGNED HELP
          "Nr. på butikk som har utstedtsjekken"
     Belop AT ROW 10.67 COL 175.6 COLON-ALIGNED HELP
          "Beløp det ble handlet for"
     RabSjekkSerienr AT ROW 11.67 COL 175.6 COLON-ALIGNED HELP
          "Sjekkens serienr (Butikkens nummerserie)"
     DatoGyldig AT ROW 12.67 COL 175.6 COLON-ALIGNED HELP
          "Siste gyldighetsdato på rabattsjekken"
     DatoUtstedt AT ROW 13.62 COL 175.6 COLON-ALIGNED HELP
          "Dato da sjekken ble utsted"
     DatoSkrevet AT ROW 14.62 COL 175.6 COLON-ALIGNED HELP
          "Dato da sjekken ble utsted"
     DatoBrukt AT ROW 15.62 COL 175.6 COLON-ALIGNED HELP
          "Dato da rabattsjekken ble brukt"
     BruktButikkNr AT ROW 16.62 COL 175.6 COLON-ALIGNED HELP
          "Rabattsjekk er brukt i butikk"
     Notat AT ROW 18.14 COL 160.2 NO-LABEL
     B-sokGetSerienr AT ROW 11.67 COL 193.4
     B-sokBruktButikk-2 AT ROW 4.38 COL 115.6
     B-sokBruktButikk AT ROW 16.62 COL 193.6
     btnCalFraDato-9 AT ROW 5.38 COL 182.2
     B-sokRabSjekkTypeNr-2 AT ROW 2.38 COL 29.4
     btnCalFraDato-5 AT ROW 5.38 COL 160.2
     btnCalFraDato-6 AT ROW 2.43 COL 160
     btnCalFraDato-10 AT ROW 2.43 COL 182
     btnCalFraDato-11 AT ROW 3.43 COL 182
     btnCalFraDato-7 AT ROW 3.43 COL 160
     btnCalFraDato-12 AT ROW 4.38 COL 182
     btnCalFraDato-8 AT ROW 4.38 COL 160
     btnCalFraDato-4 AT ROW 15.62 COL 193.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 203.6 BY 30.24.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     B-sokMedlem AT ROW 8.67 COL 197.6
     btnCalFraDato AT ROW 12.67 COL 193.6
     B-sokButik AT ROW 9.67 COL 193.4
     B-sokMedlem2 AT ROW 3.38 COL 44.6
     B-sokRabSjekkTypeNr AT ROW 7.67 COL 182.4
     btnCalFraDato-2 AT ROW 13.67 COL 193.6
     btnCalFraDato-3 AT ROW 14.62 COL 193.6
     rectBrowse AT ROW 6.71 COL 1.6
     rectToolBar AT ROW 1.14 COL 1.8
     rectWinToolbar AT ROW 1.14 COL 194
     RECT-64 AT ROW 6.71 COL 159
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
         TITLE              = "Vedlikehold rabattsjekk"
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
ASSIGN 
       Notat:RETURN-INSERTED IN FRAME DEFAULT-FRAME  = TRUE.

/* SETTINGS FOR FILL-IN RabSjekkTypeNr IN FRAME DEFAULT-FRAME
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
ON END-ERROR OF C-Win /* Vedlikehold rabattsjekk */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vedlikehold rabattsjekk */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Vedlikehold rabattsjekk */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-sokBruktButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-sokBruktButikk C-Win
ON CHOOSE OF B-sokBruktButikk IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF B-sokBruktButikk
DO:
    cTekst = "Butik".                    
    RUN JBoxDLookup.w ("Butiker;Butik;ButNamn", "where Butiker.harButikksystem = true and Butiker.ApningsDato <> ? and Butiker.NedlagtDato = ?", INPUT-OUTPUT cTekst).

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    ELSE DO:
        ASSIGN
            BruktButikkNr:SCREEN-VALUE = cTekst
            BruktButikkNr:MODIFIED     = TRUE
            .
        APPLY "any-printable" TO BruktButikkNr.
    END.
  APPLY "ENTRY" TO BruktButikkNr.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-sokBruktButikk-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-sokBruktButikk-2 C-Win
ON CHOOSE OF B-sokBruktButikk-2 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiBruktButikkNr
DO:
    cTekst = "Butik".                    
    RUN JBoxDLookup.w ("Butiker;Butik;ButNamn", "where Butiker.harButikksystem = true and Butiker.ApningsDato <> ? and Butiker.NedlagtDato = ?", INPUT-OUTPUT cTekst).

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    ELSE DO:
        ASSIGN
            fiBruktButikkNr:SCREEN-VALUE = cTekst
            fiBruktButikkNr:MODIFIED     = TRUE
            .
        APPLY "any-printable" TO fiBruktButikkNr.
    END.
  APPLY "ENTRY" TO fiBruktButikkNr.
  RUN InvokeMethod (hBrowse,'OpenQuery').

  RETURN NO-APPLY.

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


&Scoped-define SELF-NAME B-sokGetSerienr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-sokGetSerienr C-Win
ON CHOOSE OF B-sokGetSerienr IN FRAME DEFAULT-FRAME /* Serienr */
OR F10 OF RabSjekkSerienr
DO:
    dDec = 0.
    RUN getRabSjekkSerieNr.p (iCL,OUTPUT dDec).
    ASSIGN
      RabSjekkSerienr:SCREEN-VALUE = string(dDec)
            .
  APPLY "any-printable" TO RabSjekkSerienr.
  APPLY "ENTRY" TO RabSjekkSerienr.
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


&Scoped-define SELF-NAME B-sokRabSjekkTypeNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-sokRabSjekkTypeNr C-Win
ON CHOOSE OF B-sokRabSjekkTypeNr IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF B-sokRabSjekkTypeNr
DO:
    cTekst = "RabSjekkTypeNr".                    
    RUN JBoxDLookup.w ("RabSjekkType;RabSjekkTypeNr;RabSjekkTypeBeskrivelse", "where true", INPUT-OUTPUT cTekst).

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    ELSE 
        ASSIGN
            RabSjekkTypeNr:SCREEN-VALUE = cTekst
            RabSjekkTypeNr:MODIFIED     = TRUE
            .
  APPLY "any-printable" TO RabSjekkTypeNr.
  APPLY "ENTRY" TO RabSjekkTypeNr.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-sokRabSjekkTypeNr-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-sokRabSjekkTypeNr-2 C-Win
ON CHOOSE OF B-sokRabSjekkTypeNr-2 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiRabSjekkTypeNr
DO:
    cTekst = "RabSjekkTypeNr".                    
    RUN JBoxDLookup.w ("RabSjekkType;RabSjekkTypeNr;RabSjekkTypeBeskrivelse", "where true", INPUT-OUTPUT cTekst).

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    ELSE 
        ASSIGN
            fiRabSjekkTypeNr:SCREEN-VALUE = cTekst
            fiRabSjekkTypeNr:MODIFIED     = TRUE
            .
  APPLY "any-printable" TO fiRabSjekkTypeNr.
  APPLY "ENTRY" TO fiRabSjekkTypeNr.
  RUN InvokeMethod (hBrowse,'OpenQuery').

  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato C-Win
ON CHOOSE OF btnCalFraDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF DatoGyldig
DO:
  RUN Cal.w (DatoGyldig:HANDLE).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-10 C-Win
ON CHOOSE OF btnCalFraDato-10 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiTilDatoGyldig
DO:
  RUN Cal.w (fiTilDatoGyldig:HANDLE).
  RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-11 C-Win
ON CHOOSE OF btnCalFraDato-11 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiTilDatoUtstedt
DO:
  RUN Cal.w (fiTilDatoUtstedt:HANDLE).
  RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-12 C-Win
ON CHOOSE OF btnCalFraDato-12 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiTilDatoSkrevet
DO:
  RUN Cal.w (fiTilDatoSkrevet:HANDLE).
  RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-2 C-Win
ON CHOOSE OF btnCalFraDato-2 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF DatoUtstedt
DO:
  RUN Cal.w (DatoUtstedt:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-3 C-Win
ON CHOOSE OF btnCalFraDato-3 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF DatoSkrevet
DO:
  RUN Cal.w (DatoSkrevet:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-4 C-Win
ON CHOOSE OF btnCalFraDato-4 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF DatoBrukt
DO:
  RUN Cal.w (DatoBrukt:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-5 C-Win
ON CHOOSE OF btnCalFraDato-5 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiFraDatoBrukt
DO:
  RUN Cal.w (fiFraDatoBrukt:HANDLE).
  RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-6 C-Win
ON CHOOSE OF btnCalFraDato-6 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiFraDatoGyldig
DO:
  RUN Cal.w (fiFraDatoGyldig:HANDLE).
  RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-7 C-Win
ON CHOOSE OF btnCalFraDato-7 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiFraDatoUtstedt
DO:
  RUN Cal.w (fiFraDatoUtstedt:HANDLE).
  RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-8 C-Win
ON CHOOSE OF btnCalFraDato-8 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiFraDatoSkrevet
DO:
  RUN Cal.w (fiFraDatoSkrevet:HANDLE).
  RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-9 C-Win
ON CHOOSE OF btnCalFraDato-9 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fiTilDatoBrukt
DO:
  RUN Cal.w (fiTilDatoBrukt:HANDLE).
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


&Scoped-define SELF-NAME fiBelop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBelop C-Win
ON LEAVE OF fiBelop IN FRAME DEFAULT-FRAME /* Beløp */
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBrukt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBrukt C-Win
ON VALUE-CHANGED OF fiBrukt IN FRAME DEFAULT-FRAME /* Vis bare ubrukte */
DO:
  RUN InvokeMethod (hBrowse,'OpenQuery').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBruktButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBruktButikkNr C-Win
ON LEAVE OF fiBruktButikkNr IN FRAME DEFAULT-FRAME /* Brukt i butikk */
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


&Scoped-define SELF-NAME fiFraDatoBrukt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFraDatoBrukt C-Win
ON LEAVE OF fiFraDatoBrukt IN FRAME DEFAULT-FRAME /* Fra/til brukt dato */
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFraDatoGyldig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFraDatoGyldig C-Win
ON LEAVE OF fiFraDatoGyldig IN FRAME DEFAULT-FRAME /* Fra/til gyldig dato */
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFraDatoSkrevet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFraDatoSkrevet C-Win
ON LEAVE OF fiFraDatoSkrevet IN FRAME DEFAULT-FRAME /* Fra/til skrevet dato */
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFraDatoUtstedt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFraDatoUtstedt C-Win
ON LEAVE OF fiFraDatoUtstedt IN FRAME DEFAULT-FRAME /* Fra/til utstedt dato */
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


&Scoped-define SELF-NAME fiRabSjekkSerienr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiRabSjekkSerienr C-Win
ON LEAVE OF fiRabSjekkSerienr IN FRAME DEFAULT-FRAME /* Serienr */
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiRabSjekkTypeNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiRabSjekkTypeNr C-Win
ON LEAVE OF fiRabSjekkTypeNr IN FRAME DEFAULT-FRAME /* Rabattsjekk */
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTilDatoBrukt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTilDatoBrukt C-Win
ON LEAVE OF fiTilDatoBrukt IN FRAME DEFAULT-FRAME
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTilDatoGyldig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTilDatoGyldig C-Win
ON LEAVE OF fiTilDatoGyldig IN FRAME DEFAULT-FRAME
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTilDatoSkrevet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTilDatoSkrevet C-Win
ON LEAVE OF fiTilDatoSkrevet IN FRAME DEFAULT-FRAME
DO:
    RUN InvokeMethod (hBrowse,'OpenQuery').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTilDatoUtstedt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTilDatoUtstedt C-Win
ON LEAVE OF fiTilDatoUtstedt IN FRAME DEFAULT-FRAME
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE byggMedRabSjekkRecord C-Win 
PROCEDURE byggMedRabSjekkRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN wbyggrabsjekkmedlem.w PERSISTENT SET hwbyggrabsjekk.
  IF VALID-HANDLE(hwbyggrabsjekk) THEN RUN setParentHandle IN hwbyggrabsjekk (THIS-PROCEDURE:HANDLE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE byggRabSjekkRecord C-Win 
PROCEDURE byggRabSjekkRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN wbyggrabsjekk.w PERSISTENT SET hwbyggrabsjekk.
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
          fiRabSjekkTypeNr:SCREEN-VALUE  = ''
          fiMedlemsNr:SCREEN-VALUE       = ''
          fiButikkNr:SCREEN-VALUE        = ''
          fiBelop:SCREEN-VALUE           = ''
          Fornavn:SCREEN-VALUE         = ''
          Etternavn:SCREEN-VALUE       = ''
          fiRabSjekkSerieNr:SCREEN-VALUE = ''
          fiBruktButikkNr:SCREEN-VALUE   = ''
          fiFraDatoGyldig:SCREEN-VALUE   = ?
          fiTilDatoGyldig:SCREEN-VALUE   = ?
          fiFraDatoSkrevet:SCREEN-VALUE  = ?
          fiTilDatoSkrevet:SCREEN-VALUE  = ?
          fiFraDatoUtstedt:SCREEN-VALUE  = ?
          fiTilDatoUtstedt:SCREEN-VALUE  = ?
          fiFraDatoBrukt:SCREEN-VALUE    = ?
          fiTilDatoBrukt:SCREEN-VALUE    = ?
          fiBrukt:CHECKED                = TRUE
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
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"medrabsjekk_delete.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"medrabsjekk_delete.p",'').
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
        RabSjekkTypeNr:SENSITIVE = FALSE.
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
  DISPLAY fiRabSjekkTypeNr fiBelop fiFraDatoGyldig fiTilDatoGyldig fiMedlemsNr 
          Fornavn fiRabSjekkSerienr fiFraDatoUtstedt fiTilDatoUtstedt fiButikkNr 
          Etternavn fiBruktButikkNr fiFraDatoSkrevet fiTilDatoSkrevet 
          fiFraDatoBrukt fiTilDatoBrukt fiBrukt RabSjekkTypeNr MedlemsNr 
          ButikkNr Belop RabSjekkSerienr DatoGyldig DatoUtstedt DatoSkrevet 
          DatoBrukt BruktButikkNr Notat 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-sokButik-2 rectBrowse rectToolBar rectWinToolbar RECT-64 searchField 
         fiRabSjekkTypeNr fiBelop fiFraDatoGyldig fiTilDatoGyldig fiMedlemsNr 
         Fornavn fiRabSjekkSerienr fiFraDatoUtstedt fiTilDatoUtstedt fiButikkNr 
         Etternavn fiBruktButikkNr fiFraDatoSkrevet fiTilDatoSkrevet BUTTON-1 
         fiFraDatoBrukt fiTilDatoBrukt fiBrukt MedlemsNr ButikkNr Belop 
         RabSjekkSerienr DatoGyldig DatoUtstedt DatoSkrevet DatoBrukt 
         BruktButikkNr Notat B-sokGetSerienr B-sokBruktButikk-2 
         B-sokBruktButikk btnCalFraDato-9 B-sokRabSjekkTypeNr-2 btnCalFraDato-5 
         btnCalFraDato-6 btnCalFraDato-10 btnCalFraDato-11 btnCalFraDato-7 
         btnCalFraDato-12 btnCalFraDato-8 btnCalFraDato-4 B-sokMedlem 
         btnCalFraDato B-sokButik B-sokMedlem2 B-sokRabSjekkTypeNr 
         btnCalFraDato-2 btnCalFraDato-3 
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
                    "MedRabSjekk"
                  + ";ButikkNr"
                  + ";RabSjekkSerieNr"
                  + ";!RabSjekkTypeNr|Type"
                  + ";MedlemsNr|Medlemsnr."
                  + ";!+medlemNavn|character|x(30)|medlemNavn(ROWID)|Medlemmets navn"
                  + ";DatoUtstedt"
                  + ";!TidUtstedt"
                  + ";+UtstedtTid|character|x(6)|TidUtstedt(ROWID)|Kl"
                  + ";Belop"
                  + ";DatoSkrevet"
                  + ";DatoGyldig"
                  + ";Brukt"
                  + ";DatoBrukt"
                  + ";BruktButikkNr|Brukt i butikk"
                  + ";RabSjekkId"
                  + ";Notat"
                  + ";EDato"
                  + ";!ETid"
                  + ";+endretTid|character|x(6)|endretTid(ROWID)|Kl"
                  + ";BrukerID"
                  + ";RegistrertDato"
                  + ";!RegistrertTid"
                  + ";+rTid|character|x(6)|rTid(ROWID)|Kl"
                  + ";RegistrertAv"
                  + ";+sjekkType|character|x(20)|sjekkType(ROWID)|Type@1"
                  + ",Medlem"
                  +   ";Fornavn|Fornavn|x(20)@6"
                  +   ";Etternavn|Etternavn|x(20)@7"
                    ,
                    "WHERE FALSE"
                  + ",FIRST Medlem NO-LOCK OF MedRabSjekk"
                   ,"SORT|Fornavn").                    
  hBrowse:NAME = "brwMedRabSjekk". 
  
  /* Henter buffer handle for å kunne lese ut verdier fra feltene i bufferet. */
  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",    
                                                 
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,  
                    "RabSjekkTypeNr,MedlemsNr,ButikkNr,Belop,RabSjekkSerienr,DatoGyldig,DatoUtstedt,DatoSkrevet,DatoBrukt,BruktButikkNr,Notat",              
                    "",              
                    "","",                       
                    "").            
  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).

  DYNAMIC-FUNCTION('setAttribute',hBrowse,'calcfieldproc','MedRabSjekk_brwcalc.p').


IF CAN-DO("1",cAdgang) THEN
      hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                        rectToolBar:HANDLE,            
                        "Fil",                         
                        "undo;Angre,save;Lagre,Refresh,Print;S&kriv,excel;Eksporter til E&xcel",
                        "maxborder").                   
  ELSE
      DYNAMIC-FUNCTION("NewToolBar",
                        rectToolBar:HANDLE,            
                        "Fil",                         
                        "new;Ny,undo;Angre,delete;Slett,save;Lagre,Refresh,BrowseConfig|;Kolonneoppsett,flatview,Print;S&kriv,excel;Eksporter til E&xcel" +
                        ",byggRabSjekk;Gen. rab.sjekker for kjøp¤ENABLED" +
                        ",byggMedRabSjekk;Gen. rab.sjekker aktive medlemmer¤ENABLED" +
                        ",markerBrukt;Marker sjekker som brukt" +
                        ",markerUBrukt;Marker sjekker som ubrukt",
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

  ASSIGN
      RabSjekkTypeNr:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE
      RabSjekkTypeNr:SCREEN-VALUE = '1'
      DatoUtstedt:SCREEN-VALUE    = STRING(TODAY)
      ButikkNr:SCREEN-VALUE       = STRING(iCL)
      .


  APPLY "entry" TO RabSjekkTypeNr.

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
    cWhere = cWhere + buildFilter(cWhere,fiRabSjekkTypeNr:HANDLE,'RabSjekkTypeNr','EQ').
    cWhere = cWhere + buildFilter(cWhere,fiButikkNr:HANDLE,'ButikkNr','EQ').
    cWhere = cWhere + buildFilter(cWhere,fiBruktButikkNr:HANDLE,'BruktButikkNr','EQ').
    cWhere = cWhere + buildFilter(cWhere,fiBelop:HANDLE,'Belop','EQ').
    cWhere = cWhere + buildFilter(cWhere,fiRabSjekkSerieNr:HANDLE,'RabSjekkSerieNr','EQ').
    cWhere = cWhere + buildFilter(cWhere,fiFraDatoGyldig:HANDLE,'DatoGyldig','GE').
    cWhere = cWhere + buildFilter(cWhere,fiTilDatoGyldig:HANDLE,'DatoGyldig','LE').
    cWhere = cWhere + buildFilter(cWhere,fiFraDatoUtstedt:HANDLE,'DatoUtstedt','GE').
    cWhere = cWhere + buildFilter(cWhere,fiTilDatoUtstedt:HANDLE,'DatoUtstedt','LE').
    cWhere = cWhere + buildFilter(cWhere,fiFraDatoSkrevet:HANDLE,'DatoSkrevet','GE').
    cWhere = cWhere + buildFilter(cWhere,fiTilDatoSkrevet:HANDLE,'DatoSkrevet','LE').
    cWhere = cWhere + buildFilter(cWhere,fiFraDatoBrukt:HANDLE,'DatoBrukt','GE').
    cWhere = cWhere + buildFilter(cWhere,fiTilDatoBrukt:HANDLE,'DatoBrukt','LE').
    IF fiBrukt:CHECKED THEN
        cWhere = cWhere + buildFilter(cWhere,fiBrukt:HANDLE,'Brukt','<>').
    
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cWhere).
    
    /*
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryFilter",
                     (IF Fornavn:SCREEN-VALUE NE "" THEN
                        (IF INDEX(Fornavn:SCREEN-VALUE,"*") > 0 THEN
                           "Medlem WHERE Fornavn MATCHES '" + Fornavn:SCREEN-VALUE + "'"
                         ELSE
                           "Medlem WHERE Fornavn BEGINS '" + Fornavn:SCREEN-VALUE + "'")
                      + ",EACH MedRabSjekk OF Medlem NO-LOCK"
                      ELSE "")

                     ).
    */
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryFilter",
                     (IF Fornavn:SCREEN-VALUE NE "" THEN
                        (IF INDEX(Fornavn:SCREEN-VALUE,"*") > 0 
                           THEN "Medlem WHERE Fornavn MATCHES '" + Fornavn:SCREEN-VALUE + "'"
                         ELSE IF Fornavn:SCREEN-VALUE NE "" 
                           THEN "Medlem WHERE Fornavn BEGINS '" + Fornavn:SCREEN-VALUE + "'"
                         ELSE ""
                     )

                   + (IF Fornavn:SCREEN-VALUE EQ "" AND Etternavn:SCREEN-VALUE NE "" 
                         THEN " Medlem WHERE " 
                       ELSE ""
                     ) 

                   + (IF Etternavn:SCREEN-VALUE NE "" AND Fornavn:SCREEN-VALUE NE "" 
                         THEN " AND " 
                       ELSE ""
                     )

                   + (IF Etternavn:SCREEN-VALUE NE "" THEN
                        (IF INDEX(Etternavn:SCREEN-VALUE,"*") > 0 
                            THEN "Etternavn MATCHES '" + Etternavn:SCREEN-VALUE + "'"
                         ELSE IF Etternavn:SCREEN-VALUE NE "" 
                            THEN "Etternavn BEGINS '" + Etternavn:SCREEN-VALUE + "'"
                         ELSE ""
                        )
                      ELSE ""
                     )
                   + ",EACH MedRabSjekk OF Medlem NO-LOCK"
                      ELSE "")
                     ).



    ASSIGN
        fiRabSjekkTypeNr:MODIFIED  = FALSE 
        fiMedlemsNr:MODIFIED       = FALSE 
        fiButikkNr:MODIFIED        = FALSE
        fiBelop:MODIFIED           = FALSE
        fiRabSjekkSerieNr:MODIFIED = FALSE
        fiBruktButikkNr:MODIFIED   = FALSE
        fiFraDatoGyldig:MODIFIED   = FALSE
        fiTilDatoGyldig:MODIFIED   = FALSE
        fiFraDatoSkrevet:MODIFIED  = FALSE
        fiTilDatoSkrevet:MODIFIED  = FALSE
        fiFraDatoUtstedt:MODIFIED  = FALSE
        fiTilDatoUtstedt:MODIFIED  = FALSE
        fiFraDatoBrukt:MODIFIED    = FALSE
        fiTilDatoBrukt:MODIFIED    = FALSE
        Fornavn:MODIFIED           = FALSE
        Etternavn:MODIFIED         = FALSE
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
        
        IF RabSjekkTypeNr:SCREEN-VALUE = "0" THEN DO:
            MESSAGE "Rabattsjekktype ikke angitt: '0'"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "ENTRY" TO RabSjekkTypeNr.
            RETURN.
        END.

        IF DEC(RabSjekkTypeNr:SCREEN-VALUE) > 0 THEN
        DO:
            cTekst = DYNAMIC-FUNCTION("getFieldValues","RabSjekkType",
                                       "WHERE RabSjekkTypeNr = '" + RabSjekkTypeNr:SCREEN-VALUE + "'","RabSjekkTypeNr").  
            IF cTekst = ? THEN
            DO:
                MESSAGE 'Ugyldig rabattsjekktypenr:' RabSjekkTypeNr:SCREEN-VALUE SKIP
                    'Resultat: ' cTekst
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN.
            END.
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
        IF DEC(BruktButikkNr:SCREEN-VALUE) > 0 THEN
        DO:
            cTekst = DYNAMIC-FUNCTION("getFieldValues","Butiker",
                                       "WHERE Butik = '" + BruktButikkNr:SCREEN-VALUE + "'","Butik").  
            IF cTekst = ? THEN
            DO:
                MESSAGE 'Ugyldig bruktbutikk:' BruktButikkNr:SCREEN-VALUE SKIP
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
        IF DEC(Belop:SCREEN-VALUE) = 0 THEN DO:
            MESSAGE "Beløp ikke angitt: '0'"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "ENTRY" TO Belop.
            RETURN.
        END.
        IF DEC(RabSjekkSerieNr:SCREEN-VALUE) = 0 THEN DO:
            MESSAGE "Serienr. ikke angitt: '0'"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "ENTRY" TO RabSjekkSerieNr.
            RETURN.
        END.
        IF DATE(DatoGyldig:SCREEN-VALUE) = ? THEN DO:
            MESSAGE "Gyldighetsdato er ikke angitt: '0'"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "ENTRY" TO DatoGyldig.
            RETURN.
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

