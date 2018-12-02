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
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEF VAR hParent         AS HANDLE NO-UNDO.
&ELSE
  DEF INPUT PARAM hParent AS HANDLE NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEF VAR bOK                AS LOG NO-UNDO.
DEF VAR ix                 AS INT NO-UNDO.

DEF VAR bByKey             AS LOG NO-UNDO.
DEF VAR hUpdToolbar        AS HANDLE NO-UNDO.
DEF VAR hWinToolbar        AS HANDLE NO-UNDO.
DEF VAR hFieldMap          AS HANDLE NO-UNDO.
DEF VAR hBrowse            AS HANDLE NO-UNDO.
DEF VAR hQuery             AS HANDLE NO-UNDO.
DEF VAR iCurrButnr         AS INT NO-UNDO.
DEF VAR iCurrBruktButNr    AS INT NO-UNDO.
DEF VAR cIdentNrParam      AS CHAR NO-UNDO.
DEF VAR bNoNavigate        AS LOG NO-UNDO.
DEF VAR cTable             AS CHAR NO-UNDO.

DEF VAR iCl                AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Utgatt IdentNr IdentType btnDBruktdato Dato ~
cTid Gyldigdato Eget butnr SelgerNr kassnr KasseNr BongNr Belop RabKr ~
KundeNr btnSokKunde KNavn KTelefon KAdresse1 KPostNr BruktButNr ~
BruktSelgerNr BruktKassNr Bruktkassenr BruktBongNr Bruktdato cBruktTid ~
MedlemsNr btnSokMedlem MEtterNavn MForNavn MTelefon MAdresse1 MPostNr ~
UtgattDato UtgattRegAv btnDGyldigTil cUtgattTid btnDRegistrert B-VisTrans1 ~
B-VisTrans2 rectUpdToolBar rectWinToolBar RECT-2 
&Scoped-Define DISPLAYED-OBJECTS Fakturert Utgatt IdentNr IdentType Dato ~
cTid Gyldigdato Eget butnr SelgerNr kassnr KasseNr BongNr Belop RabKr ~
KundeNr KNavn KTelefon KAdresse1 KPostNr BruktButNr BruktSelgerNr ~
BruktKassNr Bruktkassenr BruktBongNr Bruktdato cBruktTid MedlemsNr ~
MEtterNavn MForNavn MTelefon MAdresse1 MPostNr UtgattDato UtgattRegAv ~
cUtgattTid ficTekst2 ficTekst1 ficTekst6 ficTekst3 FakturertDato 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CloseWindow C-Win 
FUNCTION CloseWindow RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKundeInfo C-Win 
FUNCTION getKundeInfo RETURNS LOGICAL
  ( INPUT icKundeNr AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMedlemsInfo C-Win 
FUNCTION getMedlemsInfo RETURNS LOGICAL
  ( INPUT icMedlemsNr AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setBruktButCombos C-Win 
FUNCTION setBruktButCombos RETURNS LOGICAL
  ( INPUT iiButNr AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setButCombos C-Win 
FUNCTION setButCombos RETURNS LOGICAL
  ( INPUT iiButNr AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-VisTrans1 
     IMAGE-UP FILE "icon/e-detail":U
     LABEL "&Vis transaksjon" 
     SIZE 4.4 BY 1.1.

DEFINE BUTTON B-VisTrans2 
     IMAGE-UP FILE "icon/e-detail":U
     LABEL "&Vis transaksjon" 
     SIZE 4.4 BY 1.1.

DEFINE BUTTON btnDBruktdato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnDGyldigTil 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnDRegistrert 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokKunde 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokMedlem 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE BruktButNr AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Brukt i butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","0"
     DROP-DOWN-LIST
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE Bruktkassenr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kasse" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE BruktKassNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kasserer" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE BruktSelgerNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Selger" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE butnr AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Fra butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","0"
     DROP-DOWN-LIST
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE IdentType AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","0"
     DROP-DOWN-LIST
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE KasseNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kasse" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE kassnr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kasserer" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE SelgerNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Selger" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE Belop AS DECIMAL FORMAT "->>,>>>,>>9.99" INITIAL 0 
     LABEL "Belop/Rabatt" 
     VIEW-AS FILL-IN 
     SIZE 21.6 BY 1.

DEFINE VARIABLE BongNr AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Bongnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.

DEFINE VARIABLE BruktBongNr AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Brukt BongNr" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE Bruktdato AS DATE FORMAT "99/99/99" 
     LABEL "Bruktdato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE cBruktTid AS CHARACTER FORMAT "xx:xx":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE cTid AS CHARACTER FORMAT "xx:xx":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE cUtgattTid AS CHARACTER FORMAT "xx:xx":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE Dato AS DATE FORMAT "99/99/99" 
     LABEL "Registrert" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE FakturertDato AS DATE FORMAT "99/99/99" 
     LABEL "Dato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE ficTekst1 AS CHARACTER FORMAT "X(256)":U INITIAL "Kunde" 
      VIEW-AS TEXT 
     SIZE 20 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE ficTekst2 AS CHARACTER FORMAT "X(256)":U INITIAL "Utstedt" 
      VIEW-AS TEXT 
     SIZE 20 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE ficTekst3 AS CHARACTER FORMAT "X(256)":U INITIAL "Medlem" 
      VIEW-AS TEXT 
     SIZE 20 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE ficTekst6 AS CHARACTER FORMAT "X(256)":U INITIAL "Brukt" 
      VIEW-AS TEXT 
     SIZE 20 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE Gyldigdato AS DATE FORMAT "99/99/99" 
     LABEL "Gyldig til" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE IdentNr AS CHARACTER FORMAT "X(40)" 
     LABEL "Identnr" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.

DEFINE VARIABLE KAdresse1 AS CHARACTER FORMAT "X(40)" 
     LABEL "Adresse" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.

DEFINE VARIABLE KNavn AS CHARACTER FORMAT "X(40)" 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.

DEFINE VARIABLE KPostNr AS CHARACTER FORMAT "X(10)" 
     LABEL "PostNr" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE KTelefon AS CHARACTER FORMAT "X(15)" 
     LABEL "Telefon" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE KundeNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Kundenummer" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE MAdresse1 AS CHARACTER FORMAT "X(40)" 
     LABEL "Adresse" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.

DEFINE VARIABLE MedlemsNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Medlemsnummer" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE MEtterNavn AS CHARACTER FORMAT "X(40)" 
     LABEL "Etternavn" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.

DEFINE VARIABLE MForNavn AS CHARACTER FORMAT "X(40)" 
     LABEL "Fornavn" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.

DEFINE VARIABLE MPostNr AS CHARACTER FORMAT "X(10)" 
     LABEL "PostNr" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE MTelefon AS CHARACTER FORMAT "X(15)" 
     LABEL "Telefon" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE RabKr AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE UtgattDato AS DATE FORMAT "99/99/99" 
     LABEL "Dato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE UtgattRegAv AS CHARACTER FORMAT "X(10)" 
     LABEL "Reg.av" 
     VIEW-AS FILL-IN 
     SIZE 22.2 BY 1.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 4.19.

DEFINE RECTANGLE rectUpdToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 5 BY 1.19.

DEFINE RECTANGLE rectWinToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 4.8 BY 1.19.

DEFINE VARIABLE Eget AS LOGICAL INITIAL no 
     LABEL "Eget" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE Fakturert AS LOGICAL INITIAL no 
     LABEL "Fakturert" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE Utgatt AS LOGICAL INITIAL no 
     LABEL "Utgått" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .71 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Fakturert AT ROW 5.05 COL 70.2 HELP
          "Fakturert"
     Utgatt AT ROW 2.76 COL 70.2
     IdentNr AT ROW 2.57 COL 15.4 COLON-ALIGNED HELP
          "Gavekortnr"
     IdentType AT ROW 3.62 COL 15.4 COLON-ALIGNED HELP
          "Gavekort-type"
     btnDBruktdato AT ROW 13.43 COL 87
     Dato AT ROW 4.86 COL 15.4 COLON-ALIGNED HELP
          "Dato"
     cTid AT ROW 4.86 COL 33.8 COLON-ALIGNED HELP
          "Registrert tid" NO-LABEL
     Gyldigdato AT ROW 5.91 COL 15.4 COLON-ALIGNED HELP
          "Dato"
     Eget AT ROW 6 COL 36 HELP
          "Eget eller andres gavekort."
     butnr AT ROW 7.76 COL 15.4 COLON-ALIGNED HELP
          "Butikk der gavekort er kjøpt"
     SelgerNr AT ROW 8.81 COL 15.4 COLON-ALIGNED
     kassnr AT ROW 9.91 COL 15.4 COLON-ALIGNED
     KasseNr AT ROW 10.95 COL 15.4 COLON-ALIGNED HELP
          "Kasse, fra butikk"
     BongNr AT ROW 12.05 COL 15.4 COLON-ALIGNED HELP
          "Bongnr, kjøp"
     Belop AT ROW 13.43 COL 15.4 COLON-ALIGNED
     RabKr AT ROW 13.43 COL 37 COLON-ALIGNED HELP
          "Rabatt" NO-LABEL
     KundeNr AT ROW 15.43 COL 15.4 COLON-ALIGNED HELP
          "Kundenummer"
     btnSokKunde AT ROW 15.43 COL 37.8
     KNavn AT ROW 16.48 COL 15.4 COLON-ALIGNED HELP
          "Navn eller firmanavn"
     KTelefon AT ROW 17.52 COL 15.4 COLON-ALIGNED HELP
          "Telefon"
     KAdresse1 AT ROW 18.57 COL 15.4 COLON-ALIGNED HELP
          "Kundens adresse"
     KPostNr AT ROW 19.57 COL 15.4 COLON-ALIGNED HELP
          "Postnummer"
     BruktButNr AT ROW 7.76 COL 71.4 COLON-ALIGNED HELP
          "Butikk der gavekort er brukt"
     BruktSelgerNr AT ROW 8.81 COL 71.4 COLON-ALIGNED
     BruktKassNr AT ROW 9.91 COL 71.4 COLON-ALIGNED
     Bruktkassenr AT ROW 10.95 COL 71.4 COLON-ALIGNED HELP
          "Kasse, brukt i butikk"
     BruktBongNr AT ROW 12.05 COL 71.4 COLON-ALIGNED HELP
          "BongNr på BRUKT bong."
     Bruktdato AT ROW 13.43 COL 71.4 COLON-ALIGNED HELP
          "Dato brukt"
     cBruktTid AT ROW 13.43 COL 90 COLON-ALIGNED HELP
          "Tidspunkt, brukt" NO-LABEL
     MedlemsNr AT ROW 15.43 COL 71.4 COLON-ALIGNED HELP
          "Medlemsnummer"
     btnSokMedlem AT ROW 15.43 COL 94
     MEtterNavn AT ROW 16.48 COL 71.4 COLON-ALIGNED HELP
          "Medlemmets etternavn"
     MForNavn AT ROW 17.52 COL 71.4 COLON-ALIGNED HELP
          "Medlemmets fornavn og mellomnavn"
     MTelefon AT ROW 18.57 COL 71.4 COLON-ALIGNED HELP
          "Telefon"
     MAdresse1 AT ROW 19.57 COL 71.4 COLON-ALIGNED HELP
          "Medlemets adresse"
     MPostNr AT ROW 20.62 COL 71.4 COLON-ALIGNED HELP
          "Postnummer"
     UtgattDato AT ROW 2.57 COL 88.8 COLON-ALIGNED
     UtgattRegAv AT ROW 3.67 COL 88.8 COLON-ALIGNED
     btnDGyldigTil AT ROW 5.91 COL 30.8
     cUtgattTid AT ROW 2.57 COL 102 COLON-ALIGNED HELP
          "Registrert tid" NO-LABEL
     btnDRegistrert AT ROW 4.86 COL 30.8
     ficTekst2 AT ROW 7.1 COL 15.6 COLON-ALIGNED NO-LABEL
     ficTekst1 AT ROW 14.76 COL 15.6 COLON-ALIGNED NO-LABEL
     ficTekst6 AT ROW 7.1 COL 71.6 COLON-ALIGNED NO-LABEL
     ficTekst3 AT ROW 14.76 COL 71.6 COLON-ALIGNED NO-LABEL
     B-VisTrans1 AT ROW 12 COL 31.4
     B-VisTrans2 AT ROW 12 COL 88.6
     FakturertDato AT ROW 4.86 COL 88.8 COLON-ALIGNED HELP
          "Fakturert dato"
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 114 BY 21.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     rectUpdToolBar AT ROW 1.14 COL 2
     rectWinToolBar AT ROW 1.14 COL 109.4
     RECT-2 AT ROW 2.29 COL 61
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 114 BY 21.


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
         HEIGHT             = 20.95
         WIDTH              = 114.2
         MAX-HEIGHT         = 21
         MAX-WIDTH          = 115.8
         VIRTUAL-HEIGHT     = 21
         VIRTUAL-WIDTH      = 115.8
         MAX-BUTTON         = no
         RESIZE             = no
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 21
       FRAME DEFAULT-FRAME:WIDTH            = 114.

/* SETTINGS FOR TOGGLE-BOX Fakturert IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FakturertDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ficTekst1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ficTekst2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ficTekst3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ficTekst6 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       MAdresse1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       MEtterNavn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       MForNavn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       MPostNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       MTelefon:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

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
/*   DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VisTrans1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VisTrans1 C-Win
ON CHOOSE OF B-VisTrans1 IN FRAME DEFAULT-FRAME /* Vis transaksjon */
DO:
  DEF VAR lB_Id AS DEC NO-UNDO.

  IF INT(BongNr:SCREEN-VALUE) = 0 THEN
      RETURN NO-APPLY.
    
  /* Kall til rutine for visning av transaksjon. */
  RUN gviskvittokopi.w (int(ButNr:screen-Value),1,int(KasseNr:SCREEN-VALUE),date(Dato:SCREEN-VALUE),INT(BongNr:SCREEN-VALUE)).

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VisTrans2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VisTrans2 C-Win
ON CHOOSE OF B-VisTrans2 IN FRAME DEFAULT-FRAME /* Vis transaksjon */
DO:
  IF INT(BruktBongNr:SCREEN-VALUE) = 0 THEN
      RETURN NO-APPLY.
    
  /* Kall til rutine for visning av transaksjon. */
  RUN gviskvittokopi.w (int(BruktButNr:screen-Value),1,int(BruktKasseNr:SCREEN-VALUE),date(BruktDato:SCREEN-VALUE),INT(BruktBongNr:SCREEN-VALUE)).

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDBruktdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDBruktdato C-Win
ON CHOOSE OF btnDBruktdato IN FRAME DEFAULT-FRAME /* ... */
DO:
  RUN Cal.w (Bruktdato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDGyldigTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDGyldigTil C-Win
ON CHOOSE OF btnDGyldigTil IN FRAME DEFAULT-FRAME /* ... */
DO:
  RUN Cal.w (Gyldigdato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDRegistrert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDRegistrert C-Win
ON CHOOSE OF btnDRegistrert IN FRAME DEFAULT-FRAME /* ... */
DO:
  RUN Cal.w (Dato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokKunde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokKunde C-Win
ON CHOOSE OF btnSokKunde IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "KundeNr".

  RUN JBoxDLookup.w ("Kunde;KundeNr;Navn;Adresse1;Telefon;MobilTlf;!PostNr", "where true", INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    KundeNr:SCREEN-VALUE = cLookupValue.
    getKundeInfo(cLookupValue).
    DYNAMIC-FUNCTION("setToolbar",hUpdToolBar,"modified").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokMedlem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokMedlem C-Win
ON CHOOSE OF btnSokMedlem IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cLookupValue  AS CHAR NO-UNDO.

  cLookupValue = "MedlemsNr".

  RUN JBoxDLookup.w ("Medlem;MedlemsNr;EtterNavn;ForNavn;Adresse1;Telefon;MobilTlf", "where true", INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    MedlemsNr:SCREEN-VALUE = cLookupValue.
    getMedlemsInfo(cLookupValue).
    DYNAMIC-FUNCTION("setToolbar",hUpdToolBar,"modified").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IdentType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IdentType C-Win
ON VALUE-CHANGED OF IdentType IN FRAME DEFAULT-FRAME /* Type */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME KundeNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KundeNr C-Win
ON RETURN OF KundeNr IN FRAME DEFAULT-FRAME /* Kundenummer */
OR "TAB" OF KundeNr DO:
  IF SELF:MODIFIED AND SELF:SCREEN-VALUE NE "0"
     AND NOT getKundeInfo(SELF:SCREEN-VALUE) THEN DO:
    APPLY "entry" TO SELF.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MedlemsNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MedlemsNr C-Win
ON RETURN OF MedlemsNr IN FRAME DEFAULT-FRAME /* Medlemsnummer */
OR "TAB" OF MedlemsNr DO:
  IF SELF:MODIFIED THEN DO:
    IF NOT getMedlemsInfo(SELF:SCREEN-VALUE) 
       AND SELF:SCREEN-VALUE NE "0" THEN DO:
      APPLY "entry" TO SELF.
      RETURN NO-APPLY.
    END.
  END.
END.

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
/*   IF NOT DYNAMIC-FUNCTION("DeleteObject",hFieldMap) THEN RETURN NO-APPLY.  */

  DYNAMIC-FUNCTION("DeleteObject",hWinToolbar).
  DYNAMIC-FUNCTION("DeleteObject",hUpdToolbar).
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

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitFromKey ("00001","Tilgode",?).
    RUN UpdateFromKey ("00002").
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setCurrentObject",hUpdToolbar).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF bByKey THEN
  DYNAMIC-FUNCTION("SetCurrentObject",hQuery).
ELSE
  DYNAMIC-FUNCTION("SetCurrentObject",hBrowse).

IF hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("butnr"):BUFFER-VALUE NE iCurrButnr  THEN
  setButCombos (hFieldMap:BUFFER-FIELD("butnr"):BUFFER-VALUE).
ELSE IF NOT hFieldMap:AVAIL  THEN
  setButCombos (0).

IF hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("BruktButNr"):BUFFER-VALUE NE iCurrBruktButNr  THEN
  setBruktButCombos (hFieldMap:BUFFER-FIELD("BruktButNr"):BUFFER-VALUE).
ELSE IF NOT hFieldMap:AVAIL THEN
  setBruktButCombos (0).

RUN SUPER.

IF hFieldMap:AVAIL THEN DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cTid:SCREEN-VALUE       = STRING(hFieldMap:BUFFER-FIELD("Tid"):BUFFER-VALUE,"HH:MM")
         cBruktTid:SCREEN-VALUE  = STRING(hFieldMap:BUFFER-FIELD("BruktTid"):BUFFER-VALUE,"HH:MM")
         cUtgattTid:SCREEN-VALUE = STRING(hFieldMap:BUFFER-FIELD("UtgattTid"):BUFFER-VALUE,"HH:MM")
         iCurrButnr              = hFieldMap:BUFFER-FIELD("butnr"):BUFFER-VALUE
         iCurrBruktButNr         = hFieldMap:BUFFER-FIELD("BruktButNr"):BUFFER-VALUE
         cTid:MODIFIED           = FALSE
         cBruktTid:MODIFIED      = FALSE
         .
END.
ELSE
  ASSIGN cTid:SCREEN-VALUE       = ""
         cBruktTid:SCREEN-VALUE  = ""
         cUtgattTid:SCREEN-VALUE = ""
         iCurrButnr              = 0
         iCurrBruktButNr         = 0
         .
ASSIGN IdentNr:READ-ONLY = TRUE
       cUtgattTid:READ-ONLY = TRUE.

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
  DISPLAY Fakturert Utgatt IdentNr IdentType Dato cTid Gyldigdato Eget butnr 
          SelgerNr kassnr KasseNr BongNr Belop RabKr KundeNr KNavn KTelefon 
          KAdresse1 KPostNr BruktButNr BruktSelgerNr BruktKassNr Bruktkassenr 
          BruktBongNr Bruktdato cBruktTid MedlemsNr MEtterNavn MForNavn MTelefon 
          MAdresse1 MPostNr UtgattDato UtgattRegAv cUtgattTid ficTekst2 
          ficTekst1 ficTekst6 ficTekst3 FakturertDato 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Utgatt IdentNr IdentType btnDBruktdato Dato cTid Gyldigdato Eget butnr 
         SelgerNr kassnr KasseNr BongNr Belop RabKr KundeNr btnSokKunde KNavn 
         KTelefon KAdresse1 KPostNr BruktButNr BruktSelgerNr BruktKassNr 
         Bruktkassenr BruktBongNr Bruktdato cBruktTid MedlemsNr btnSokMedlem 
         MEtterNavn MForNavn MTelefon MAdresse1 MPostNr UtgattDato UtgattRegAv 
         btnDGyldigTil cUtgattTid btnDRegistrert B-VisTrans1 B-VisTrans2 
         rectUpdToolBar rectWinToolBar RECT-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitFromBrowse C-Win 
PROCEDURE InitFromBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM icTable  AS CHAR NO-UNDO.      /* Gavekort / Tilgode */

ASSIGN hBrowse = ihBrowse
       hQuery  = hBrowse:QUERY
       cTable  = icTable
       .

RUN InitWindow.

/* Link objects: */

DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).
DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hUpdToolBar).

RUN DisplayRecord.

/* DYNAMIC-FUNCTION("DumpAllLinks",THIS-PROCEDURE:CURRENT-WINDOW,"c:\temp\links.txt").  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitFromKey C-Win 
PROCEDURE InitFromKey :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icIdentNr        AS CHAR NO-UNDO.
DEF INPUT PARAM icTable          AS CHAR NO-UNDO.      /* 1: Gavekort, 2: Tilgode */
DEF INPUT PARAM ihExternalBrowse AS HANDLE NO-UNDO.

ASSIGN cIdentNrParam = icIdentNr
       cTable        = icTable
       .

hQuery  = DYNAMIC-FUNCTION("NewQuery",1,"",
                             cTable + ",GaveKType",
                             "WHERE IdentNr = '" + icIdentNr + "', FIRST GaveKType NO-LOCK OF " + cTable + " OUTER-JOIN",
                             "").


IF VALID-HANDLE(ihExternalBrowse) THEN
  DYNAMIC-FUNCTION("setAttribute",hQuery,"ExternalBrowse",STRING(ihExternalBrowse)).
ELSE bNoNavigate = TRUE.

RUN InitWindow.

/* Link objects: */

DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hFieldMap).
DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hUpdToolBar).

bByKey = TRUE.
DYNAMIC-FUNCTION("SetCurrentSourceProc",THIS-PROCEDURE).

IF icIdentNr NE "?" THEN
  RUN DisplayRecord.
ELSE 
  RUN NewRecord.

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
DO WITH FRAME {&FRAME-NAME}:

  {&WINDOW-NAME}:TITLE = cTable.
  iCl = INT(DYNAMIC-FUNCTION("getFieldValues","SysPara",
                              "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1")).  

  IF cTable = "Tilgode" THEN ficTekst2:SCREEN-VALUE = "Utstedt".
  IF cTable = "Tilgode" THEN RabKr:HIDDEN = TRUE.

  ASSIGN butnr:DELIMITER         = "|"
         BruktButNr:DELIMITER    = "|"
         IdentType:DELIMITER     = "|"
         SelgerNr:DELIMITER      = "|"
         kassnr:DELIMITER        = "|"
         kassenr:DELIMITER       = "|"
         BruktSelgerNr:DELIMITER = "|"
         BruktKassNr:DELIMITER   = "|"
         Bruktkassenr:DELIMITER  = "|"

         butnr:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("GetFieldList","Butiker;Butik|ButNamn;Butik","WHERE TRUE BY ButNamn")
         BruktButNr:LIST-ITEM-PAIRS = butnr:LIST-ITEM-PAIRS
         IdentType:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("GetFieldList","GaveKType;IdentType|GKTBeskrivelse;IdentType",
                                                      "WHERE " + (IF cTable = "Tilgode" THEN "IdentType = 0" ELSE "IdentType NE 0")
                                                      + " BY GKTBeskrivelse")

         .

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins)
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hQuery,
                    FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                    "Eget,Bruktkassenr,Belop,RabKr,BongNr,Bruktdato,BruktSelgerNr,BruktKassNr,SelgerNr,Utgatt"
                       + ",BruktBongNr,BruktButNr,KNavn,KPostNr" /* Update columns in buffer */
                       + ",KTelefon,KundeNr,MedlemsNr,kassnr,KasseNr,KAdresse1"
                       + ",IdentNr,Gyldigdato,Dato,butnr"
                       + ",MEtterNavn,MForNavn,MAdresse1,MTelefon,MPostNr"
                       + IF cTable NE "Tilgode" THEN ",IdentType" ELSE ""
                    ,"",       
                    "UtgattDato,UtgattRegAv,Fakturert,FakturertDato" + IF cTable = "Tilgode" THEN ",IdentType" ELSE "", 
                    "",  
                    "cTid,cBruktTid,cUtgattTid").     /* Input fields other than in buffer */

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customDeleteValProc","ignore").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraFields","Tid,BruktTid,UtgattTid,UtgattDato,UtgattRegAv").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"FieldNameDeleteWarning","IdentNr").

/*   bNoNavigate = FALSE.  */
  hUpdToolBar = DYNAMIC-FUNCTION("NewToolBar",
                    rectUpdToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                          /* Corresponding menu label - no menu if blank */
                    "New;Ny,Undo;Angre,delete;Slett,save;Lagre" +
                       IF bNoNavigate THEN ""
                       ELSE ",-,First|Naviger;Første,Prev|Naviger;Forrige,Next|Naviger;Neste,Last|Naviger;Siste",
                                                    /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    "maxborder").                   /* Misc - for something I might need in next version.. */


  hWinToolBar = DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                          /* Corresponding menu label - no menu if blank */
                    "|Innstillinger,Close;Avslutt,Help|Hjelp",
                                                    /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    "right").                        /* Misc - for something I might need in next version.. */

  DYNAMIC-FUNCTION("CreateObjectLink",hUpdToolbar,hFieldMap).

  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,500,0,0).

  DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  DYNAMIC-FUNCTION("SetToolbar",hWinToolBar,"enable").

END.

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
/* APPLY "entry" TO FRAME {&FRAME-NAME}.  */
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
DYNAMIC-FUNCTION("setCurrentObject",hUpdToolbar).

RUN SUPER.

IdentNr:READ-ONLY IN FRAME {&FRAME-NAME} = FALSE.
APPLY "entry" TO IdentNr.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cTid cBruktTid cUtgattTid.

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraValues",
                   STRING(INT(SUBSTR(cTid,1,2)) * 3600 + INT(SUBSTR(cTid,3)) * 60) + "|" +
                   STRING(INT(SUBSTR(cBruktTid,1,2)) * 3600 + INT(SUBSTR(cBruktTid,3)) * 60) + "|" +
                   STRING(INT(SUBSTR(cUtgattTid,1,2)) * 3600 + INT(SUBSTR(cUtgattTid,3)) * 60) + "|" +
                   STRING(UtgattDato:SCREEN-VALUE) + "|" +
                   STRING(UtgattRegAv:SCREEN-VALUE)
                   ).

END.

RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UndoRecord C-Win 
PROCEDURE UndoRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF bByKey AND DYNAMIC-FUNCTION("getToolbarState",hUpdToolbar) = "new" AND cIdentNrParam = "?" THEN
  APPLY "close" TO THIS-PROCEDURE.
ELSE RUN SUPER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateFromKey C-Win 
PROCEDURE UpdateFromKey :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icIdentNr  AS CHAR NO-UNDO.

DEF VAR iNumRes   AS INT NO-UNDO.

iNumRes = DYNAMIC-FUNCTION("FillQuery",hQuery,1,-1,
                           "GaveKort,GaveKType",
                           "WHERE IdentNr = '" + icIdentNr + "', FIRST GaveKType NO-LOCK OF Gavekort OUTER-JOIN"
                            ).

DYNAMIC-FUNCTION("SetCurrentSourceProc",THIS-PROCEDURE).


IF icIdentNr NE "?" THEN
  RUN DisplayRecord.
ELSE 
  RUN NewRecord.

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
DEF INPUT PARAM icField AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  CASE icField:
    WHEN "butnr" THEN setButCombos (INT(butnr:SCREEN-VALUE)).
    WHEN "BruktButNr" THEN setBruktButCombos (INT(BruktButNr:SCREEN-VALUE)).
    WHEN "Utgatt" THEN DO:
      ASSIGN Utgatt.
      IF Utgatt THEN DO:
        setBruktButCombos (iCL).
        ASSIGN BruktButNr:SCREEN-VALUE  = STRING(iCl)
               UtgattDato:SCREEN-VALUE  = STRING(TODAY)
               cUtgattTid:SCREEN-VALUE  = STRING(TIME,"HH:MM")
               UtgattRegAv:SCREEN-VALUE = DYNAMIC-FUNCTION("getASuserId")
               .
      END.
      ELSE DO:
        IF hFieldMap:AVAIL THEN
          setBruktButCombos (hFieldMap:BUFFER-FIELD("BruktButNr"):BUFFER-VALUE).
        ASSIGN UtgattDato:SCREEN-VALUE  = ?
               cUtgattTid:SCREEN-VALUE  = ""
               UtgattRegAv:SCREEN-VALUE = ""
               .
      END.
    END.
  END CASE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CloseWindow C-Win 
FUNCTION CloseWindow RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF NOT DYNAMIC-FUNCTION("DeleteObject",hFieldMap) THEN 
  RETURN FALSE.
ELSE 
  APPLY "close" TO THIS-PROCEDURE.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKundeInfo C-Win 
FUNCTION getKundeInfo RETURNS LOGICAL
  ( INPUT icKundeNr AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cKundeValues AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  cKundeValues = DYNAMIC-FUNCTION("getFieldList","Kunde;Navn;Telefon;Adresse1;PostNr","WHERE KundeNr = " + icKundeNr).
  IF cKundeValues NE "" THEN DO:
    KundeNr:SCREEN-VALUE = icKundeNr.
    DO ix = 1 TO NUM-ENTRIES(cKundeValues,"|"):
      CASE ix:
        WHEN 1 THEN KNavn:SCREEN-VALUE = ENTRY(ix,cKundeValues,"|").
        WHEN 2 THEN KTelefon:SCREEN-VALUE = ENTRY(ix,cKundeValues,"|").
        WHEN 3 THEN KAdresse1:SCREEN-VALUE = ENTRY(ix,cKundeValues,"|").
        WHEN 4 THEN KPostNr:SCREEN-VALUE = ENTRY(ix,cKundeValues,"|").
      END CASE.
    END.
  END.
  ELSE DO: 
    DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig kundenr","Advarsel","").
    RETURN FALSE.
  END.
END.
  
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMedlemsInfo C-Win 
FUNCTION getMedlemsInfo RETURNS LOGICAL
  ( INPUT icMedlemsNr AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cMedlemValues AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  cMedlemValues = DYNAMIC-FUNCTION("getFieldList","Medlem;KundeNr;EtterNavn;ForNavn;Telefon;Adresse1;PostNr","WHERE MedlemsNr = " + icMedlemsNr).
  IF cMedlemValues NE "" THEN DO:
    DO ix = 2 TO NUM-ENTRIES(cMedlemValues,"|"):
      CASE ix:
        WHEN 2 THEN MEtterNavn:SCREEN-VALUE = ENTRY(ix,cMedlemValues,"|").
        WHEN 3 THEN MForNavn:SCREEN-VALUE   = ENTRY(ix,cMedlemValues,"|").
        WHEN 4 THEN MTelefon:SCREEN-VALUE   = ENTRY(ix,cMedlemValues,"|").
        WHEN 5 THEN MAdresse1:SCREEN-VALUE  = ENTRY(ix,cMedlemValues,"|").
        WHEN 6 THEN MPostNr:SCREEN-VALUE    = ENTRY(ix,cMedlemValues,"|").
      END CASE.
    END.
    IF ENTRY(1,cMedlemValues,"|") NE "0" THEN 
      getKundeInfo(ENTRY(1,cMedlemValues,"|")).
  END.
  ELSE DO: 
    ASSIGN MEtterNavn:SCREEN-VALUE = ""
           MForNavn:SCREEN-VALUE   = ""
           MTelefon:SCREEN-VALUE   = ""
           MAdresse1:SCREEN-VALUE  = ""
           MPostNr:SCREEN-VALUE    = ""
           .
    IF icMedlemsNr NE "0" THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig medlemsnr","Advarsel","").
    RETURN FALSE.
  END.
END.

RETURN TRUE.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setBruktButCombos C-Win 
FUNCTION setBruktButCombos RETURNS LOGICAL
  ( INPUT iiButNr AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR pcTekst  AS CHAR NO-UNDO.
DEF VAR pc2Tekst AS CHAR NO-UNDO.
DEF VAR piLoop   AS INT  NO-UNDO.
DEF VAR piInt    AS INT  NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN BruktSelgerNr:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("getFieldList","ButikkSelger,Selger;SelgerNr|Navn;SelgerNr",
                                              "WHERE ButikkNr = " + STRING(iiButNr) + 
                                              ", FIRST Selger where Selger.SelgerNr = ButikkSelger.SelgerNr NO-LOCK"),"|")
         pcTekst = DYNAMIC-FUNCTION("getFieldList","butikkforsalj;KassererId,Forsalj;ForsNr;FoNamn",
                                           "WHERE Butik = " + STRING(iiButNr) + 
                                           ", FIRST Forsalj OF butikkforsalj NO-LOCK")
         BruktKassenr:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("getFieldList","Kasse;KasseNr|Navn;KasseNr",
                                              "WHERE ButikkNr = " + STRING(iiButNr)),"|")
         pc2Tekst = "|0"
        .
  IF NUM-ENTRIES(pcTekst,"|") >= 3 THEN
  DO piLoop = 1 TO (NUM-ENTRIES(pcTekst,"|") / 3):
      ASSIGN
          piInt = 3 * piLoop
          pc2Tekst = pc2Tekst + 
                     (IF pc2Tekst = ""
                        THEN ""
                        ELSE "|") +
                     ENTRY(piInt - 1,pcTekst,"|") + " / " + ENTRY(piInt,pcTekst,"|") + "|" + ENTRY(piInt - 2,pcTekst,"|")
      .
  END.
  ASSIGN
      BruktKassNr:LIST-ITEM-PAIRS = pc2Tekst
      .
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setButCombos C-Win 
FUNCTION setButCombos RETURNS LOGICAL
  ( INPUT iiButNr AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR pcTekst  AS CHAR NO-UNDO.
DEF VAR pc2Tekst AS CHAR NO-UNDO.
DEF VAR piLoop   AS INT  NO-UNDO.
DEF VAR piInt    AS INT  NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN SelgerNr:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("getFieldList","ButikkSelger,Selger;SelgerNr|Navn;SelgerNr",
                                              "WHERE ButikkNr = " + STRING(iiButNr) + 
                                              ", FIRST Selger where Selger.SelgerNr = ButikkSelger.SelgerNr NO-LOCK"),"|")
         pcTekst = DYNAMIC-FUNCTION("getFieldList","butikkforsalj;KassererId,Forsalj;ForsNr;FoNamn",
                                              "WHERE Butik = " + STRING(iiButNr) + 
                                              ", FIRST Forsalj OF butikkforsalj NO-LOCK")
         Kassenr:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("getFieldList","Kasse;KasseNr|Navn;KasseNr",
                                              "WHERE ButikkNr = " + STRING(iiButNr)),"|")
         pc2Tekst = "|0"
         .

  IF NUM-ENTRIES(pcTekst,"|") >= 3 THEN
  DO piLoop = 1 TO (NUM-ENTRIES(pcTekst,"|") / 3):
      ASSIGN
          piInt = 3 * piLoop
          pc2Tekst = pc2Tekst + 
                     (IF pc2Tekst = ""
                        THEN ""
                        ELSE "|") +
                     ENTRY(piInt - 1,pcTekst,"|") + " / " + ENTRY(piInt,pcTekst,"|") + "|" + ENTRY(piInt - 2,pcTekst,"|")
      .
  END.
  ASSIGN
      KassNr:LIST-ITEM-PAIRS = pc2Tekst
      .
END.

RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

