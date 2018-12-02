&ANALYZE-SUSPEND _VERSION-NUMBER GUI GUI
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

DEF VAR bOK             AS LOG NO-UNDO.
DEF VAR ix              AS INT NO-UNDO.

DEF VAR hUpdToolbar     AS HANDLE NO-UNDO.
DEF VAR hWinToolbar     AS HANDLE NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR iIndividType    AS INT    NO-UNDO.
DEF VAR bByKey          AS LOG    NO-UNDO.
DEF VAR iCurrButnr      AS INT    NO-UNDO.
DEF VAR iCl             AS INT    NO-UNDO. /* Sentrallager. */
DEF VAR lArtikkelNr     AS DEC    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-SokArtikkel btnKjoptDato btnSalgDato ~
btnSokKunde btnSokPost1 ButNr individnr ArtikkelNr Beskr VmBeskrivelse ~
StrKode Kaliber serienr Garantinummer KKundeNr Navn Adresse1 Adresse2 ~
PostNr Poststed ePostAdresse Telefon MobilTlf PersOrgNr VapenKort Jegerkort ~
BatchNr KjoptDato feilvare levnr Salgdato NyVare Pris DBKr DB% VareKost ~
VVarekost ForsNr SelgerNr SNavn SAdresse1 SAdresse2 SPostNr SPoststed ~
STelefon SMobilTlf btnSokPost2 btnSokPost-3 btnSokStrKode btnSokBatchNr ~
rectUpdToolBar rectWinToolBar RECT-1 
&Scoped-Define DISPLAYED-OBJECTS IndividType ButNr individnr ArtikkelNr ~
Beskr VmBeskrivelse StrKode Storl Kaliber serienr Garantinummer KKundeNr ~
Navn Adresse1 Adresse2 PostNr Poststed ePostAdresse Telefon MobilTlf ~
PersOrgNr VapenKort Jegerkort BatchNr KjoptDato feilvare levnr LevNamn ~
Salgdato NyVare Pris DBKr DB% VareKost VVarekost ForsNr SelgerNr SNavn ~
SAdresse1 SAdresse2 SPostNr SPoststed STelefon SMobilTlf ficTekst2 ~
ficTekst4 ficTekst1 ficTekst5 ficTekst3 ficTekst6 SeqNr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKundeInfo C-Win 
FUNCTION getKundeInfo RETURNS LOGICAL
  ( INPUT icKundeNr AS CHAR )  FORWARD.

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
DEFINE BUTTON B-SokArtikkel 
     IMAGE-UP FILE "icon/select.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Søk" 
     SIZE 4.6 BY 1 TOOLTIP "Starter Alt-S søkefunksjonen".

DEFINE BUTTON btnKjoptDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSalgDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokBatchNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokKunde  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokPost-3  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokPost1  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokPost2  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokStrKode  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE ButNr AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "ButikkNr" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "0",1
     DROP-DOWN-LIST
     SIZE 42.4 BY 1 NO-UNDO.

DEFINE VARIABLE ForsNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Kasserer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "0",1
     DROP-DOWN-LIST
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE IndividType AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "IndividType" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "0",1
     DROP-DOWN-LIST
     SIZE 42.4 BY 1
     FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE VARIABLE SelgerNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "SelgerNr" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "0",1
     DROP-DOWN-LIST
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE Adresse1 AS CHARACTER FORMAT "X(40)" 
     LABEL "Adresse" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE Adresse2 AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE ArtikkelNr AS DECIMAL FORMAT "zzzzzzzzzzzz9" INITIAL 0 
     LABEL "ArtNr/Varetekst" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE BatchNr AS INTEGER FORMAT "zzzzzzzz9" INITIAL 0 
     LABEL "BatchNr" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE Beskr AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1.

DEFINE VARIABLE DB% AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE DBKr AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "DbKr/%" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE ePostAdresse AS CHARACTER FORMAT "X(40)" 
     LABEL "E-Post adresse" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE ficTekst1 AS CHARACTER FORMAT "X(256)":U INITIAL "Kjøper" 
      VIEW-AS TEXT 
     SIZE 20 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE ficTekst2 AS CHARACTER FORMAT "X(256)":U INITIAL "Vare" 
      VIEW-AS TEXT 
     SIZE 20 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE ficTekst3 AS CHARACTER FORMAT "X(256)":U INITIAL "Tidligere eier" 
      VIEW-AS TEXT 
     SIZE 20 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE ficTekst4 AS CHARACTER FORMAT "X(256)":U INITIAL "SerieNr og Garanti" 
      VIEW-AS TEXT 
     SIZE 32 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE ficTekst5 AS CHARACTER FORMAT "X(256)":U INITIAL "Solgt" 
      VIEW-AS TEXT 
     SIZE 20 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE ficTekst6 AS CHARACTER FORMAT "X(256)":U INITIAL "Varemottak" 
      VIEW-AS TEXT 
     SIZE 20 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE Garantinummer AS CHARACTER FORMAT "X(25)" 
     LABEL "Garantinummer" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE individnr AS DECIMAL FORMAT ">>>>>>>>>>>9" INITIAL 0 
     LABEL "Individnr" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE Jegerkort AS CHARACTER FORMAT "X(25)" 
     LABEL "Jegerkort" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE Kaliber AS CHARACTER FORMAT "X(12)" 
     LABEL "Kaliber" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE KjoptDato AS DATE FORMAT "99/99/99" 
     LABEL "KjoptDato" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE KKundeNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Kundenummer" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE LevNamn AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 22.6 BY 1.

DEFINE VARIABLE levnr AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE MobilTlf AS CHARACTER FORMAT "X(15)" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE Navn AS CHARACTER FORMAT "X(40)" 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE PersOrgNr AS CHARACTER FORMAT "X(25)" 
     LABEL "Person/orgNr" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE PostNr AS CHARACTER FORMAT "X(10)" 
     LABEL "PostNr/Sted" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE Poststed AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE Pris AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE SAdresse1 AS CHARACTER FORMAT "X(40)" 
     LABEL "Adresse" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE SAdresse2 AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE Salgdato AS DATE FORMAT "99/99/99" 
     LABEL "Salgsdato" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE SeqNr AS DECIMAL FORMAT ">>>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1.

DEFINE VARIABLE serienr AS CHARACTER FORMAT "X(20)" 
     LABEL "Serienr" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE SMobilTlf AS CHARACTER FORMAT "X(15)" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE SNavn AS CHARACTER FORMAT "X(40)" 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE SPostNr AS CHARACTER FORMAT "X(10)" 
     LABEL "PostNr" 
     VIEW-AS FILL-IN 
     SIZE 16.6 BY 1.

DEFINE VARIABLE SPoststed AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE STelefon AS CHARACTER FORMAT "X(15)" 
     LABEL "Telefon/Mobil" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE Storl AS CHARACTER FORMAT "x(4)" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1.

DEFINE VARIABLE StrKode AS INTEGER FORMAT "999" INITIAL 0 
     LABEL "Størrelse" 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE VARIABLE Telefon AS CHARACTER FORMAT "X(15)" 
     LABEL "Telefon/Mobil" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE VapenKort AS CHARACTER FORMAT "X(25)" 
     LABEL "Våpenkort" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE VareKost AS DECIMAL FORMAT "->,>>>,>>9.99" INITIAL 0 
     LABEL "Kalk/Reel VareKost" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE VmBeskrivelse AS CHARACTER FORMAT "X(30)" 
     LABEL "Varemerke" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE VVarekost AS DECIMAL FORMAT "-z,zzz,zz9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19 BY 1.

DEFINE RECTANGLE rectUpdToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 5 BY 1.19.

DEFINE RECTANGLE rectWinToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 4.8 BY 1.19.

DEFINE VARIABLE feilvare AS LOGICAL INITIAL no 
     LABEL "Feilvare" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE NyVare AS LOGICAL INITIAL yes 
     LABEL "Ny vare" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-SokArtikkel AT ROW 6.81 COL 37.8 NO-TAB-STOP 
     btnKjoptDato AT ROW 5.81 COL 94.4 NO-TAB-STOP 
     btnSalgDato AT ROW 11.48 COL 99.6 NO-TAB-STOP 
     btnSokKunde AT ROW 14.29 COL 37.4 NO-TAB-STOP 
     btnSokPost1 AT ROW 18.29 COL 33 NO-TAB-STOP 
     IndividType AT ROW 3.1 COL 15.4 COLON-ALIGNED HELP
          "Individtype"
     ButNr AT ROW 4.81 COL 15.4 COLON-ALIGNED HELP
          "Butikknummer"
     individnr AT ROW 5.81 COL 15.4 COLON-ALIGNED HELP
          "Individvarenummer"
     ArtikkelNr AT ROW 6.81 COL 15.4 COLON-ALIGNED
     Beskr AT ROW 6.81 COL 40.4 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen" NO-LABEL
     VmBeskrivelse AT ROW 7.81 COL 15.4 COLON-ALIGNED HELP
          "Varemerke"
     StrKode AT ROW 8.81 COL 15.4 COLON-ALIGNED
     Storl AT ROW 8.81 COL 25.8 COLON-ALIGNED HELP
          "Størrelse" NO-LABEL
     Kaliber AT ROW 9.81 COL 15.4 COLON-ALIGNED HELP
          "Kaliber"
     serienr AT ROW 11.43 COL 15.4 COLON-ALIGNED
     Garantinummer AT ROW 12.43 COL 15.4 COLON-ALIGNED HELP
          "Garantinummer"
     KKundeNr AT ROW 14.29 COL 15.4 COLON-ALIGNED HELP
          "Kundenummer"
     Navn AT ROW 15.29 COL 15.4 COLON-ALIGNED HELP
          "Navn eller firmanavn"
     Adresse1 AT ROW 16.29 COL 15.4 COLON-ALIGNED HELP
          "Kundens adresse"
     Adresse2 AT ROW 17.29 COL 15.4 COLON-ALIGNED HELP
          "Kundens adresse" NO-LABEL
     PostNr AT ROW 18.29 COL 15.4 COLON-ALIGNED HELP
          "Postnummer"
     Poststed AT ROW 18.29 COL 35.4 COLON-ALIGNED HELP
          "Poststed" NO-LABEL
     ePostAdresse AT ROW 19.29 COL 15.4 COLON-ALIGNED HELP
          "E-Post adresse"
     Telefon AT ROW 20.33 COL 15.4 COLON-ALIGNED HELP
          "Telefon"
     MobilTlf AT ROW 20.33 COL 36.4 COLON-ALIGNED HELP
          "Mobiltelefon" NO-LABEL
     PersOrgNr AT ROW 21.33 COL 15.4 COLON-ALIGNED HELP
          "Person eller organisasjonsnummer"
     VapenKort AT ROW 22.33 COL 15.4 COLON-ALIGNED HELP
          "Kjøpers våpenkortnummer"
     Jegerkort AT ROW 23.33 COL 15.4 COLON-ALIGNED HELP
          "Kjøpers jegerkort"
     BatchNr AT ROW 4.81 COL 77.4 COLON-ALIGNED HELP
          "Batch nummer som holder sammen transaksjoner"
     KjoptDato AT ROW 5.81 COL 77.4 COLON-ALIGNED
     feilvare AT ROW 3.33 COL 104.4 HELP
          "Feilvare" NO-TAB-STOP 
     levnr AT ROW 6.81 COL 77.4 COLON-ALIGNED HELP
          "Leverandørnummer"
     LevNamn AT ROW 6.81 COL 96.8 COLON-ALIGNED HELP
          "Leverandørens navn" NO-LABEL
     Salgdato AT ROW 11.43 COL 77.4 COLON-ALIGNED HELP
          "Dato når produktet er solgt"
     NyVare AT ROW 7.91 COL 79.4 HELP
          "Ny eller brukt vare (Brukt er innbytte eller innkjøpt brukt)" NO-TAB-STOP 
     Pris AT ROW 12.38 COL 77.4 COLON-ALIGNED HELP
          "Pris (Til kunde inkl. mva)"
     DBKr AT ROW 13.43 COL 77.4 COLON-ALIGNED HELP
          "Dekningsbidrag"
     DB% AT ROW 13.43 COL 97.6 COLON-ALIGNED HELP
          "Dekningsbidrag oppgitt i %" NO-LABEL
     VareKost AT ROW 14.38 COL 77.4 COLON-ALIGNED HELP
          "Varekost (Fra kalkyle)"
     VVarekost AT ROW 14.38 COL 97.4 COLON-ALIGNED HELP
          "Vektet varekost" NO-LABEL
     ForsNr AT ROW 15.38 COL 77.4 COLON-ALIGNED HELP
          "Selgers nummer"
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 127.8 BY 24.19.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     SelgerNr AT ROW 16.38 COL 77.4 COLON-ALIGNED HELP
          "Selgernummer."
     SNavn AT ROW 19.33 COL 77.4 COLON-ALIGNED HELP
          "Selgers navn eller firmanavn"
     SAdresse1 AT ROW 20.33 COL 77.4 COLON-ALIGNED HELP
          "Selgers adresse"
     SAdresse2 AT ROW 21.33 COL 77.4 COLON-ALIGNED HELP
          "Selgers adresse" NO-LABEL
     SPostNr AT ROW 22.33 COL 77.4 COLON-ALIGNED HELP
          "Selgers postnummer"
     SPoststed AT ROW 22.33 COL 98.4 COLON-ALIGNED HELP
          "Selgers poststed" NO-LABEL
     STelefon AT ROW 23.33 COL 77.4 COLON-ALIGNED HELP
          "Selgers telefon"
     SMobilTlf AT ROW 23.33 COL 98.4 COLON-ALIGNED HELP
          "Selgers mobiltelefon" NO-LABEL
     ficTekst2 AT ROW 4.19 COL 15.4 COLON-ALIGNED NO-LABEL
     ficTekst4 AT ROW 10.81 COL 15.4 COLON-ALIGNED NO-LABEL
     ficTekst1 AT ROW 13.62 COL 15.4 COLON-ALIGNED NO-LABEL
     ficTekst5 AT ROW 10.71 COL 77.4 COLON-ALIGNED NO-LABEL
     ficTekst3 AT ROW 18.57 COL 77.8 COLON-ALIGNED NO-LABEL
     ficTekst6 AT ROW 4.14 COL 78.4 COLON-ALIGNED NO-LABEL
     btnSokPost2 AT ROW 22.33 COL 96 NO-TAB-STOP 
     btnSokPost-3 AT ROW 6.76 COL 94.4 NO-TAB-STOP 
     btnSokStrKode AT ROW 8.81 COL 23.4 NO-TAB-STOP 
     btnSokBatchNr AT ROW 4.81 COL 94.4 NO-TAB-STOP 
     SeqNr AT ROW 5.81 COL 35.6 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     rectUpdToolBar AT ROW 1.62 COL 1
     rectWinToolBar AT ROW 1.57 COL 118.4
     RECT-1 AT ROW 3.24 COL 102.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 127.8 BY 24.19.


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
         TITLE              = "Individ"
         HEIGHT             = 24.19
         WIDTH              = 127.8
         MAX-HEIGHT         = 25.05
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 25.05
         VIRTUAL-WIDTH      = 320
         MAX-BUTTON         = no
         TOP-ONLY           = yes
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
   Custom                                                               */
/* SETTINGS FOR FILL-IN ficTekst1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ficTekst2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ficTekst3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ficTekst4 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ficTekst5 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ficTekst6 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX IndividType IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN SeqNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       SeqNr:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN Storl IN FRAME DEFAULT-FRAME
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
ON END-ERROR OF C-Win /* Individ */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Individ */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Individ */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtikkelNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtikkelNr C-Win
ON RETURN OF ArtikkelNr IN FRAME DEFAULT-FRAME /* ArtNr/Varetekst */
OR "TAB" OF ArtikkelNr
DO:
    IF SELF:MODIFIED THEN
    DO:
        Beskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","ArtBas",
                                              "WHERE ArtikkelNr = dec(" + ArtikkelNr:SCREEN-VALUE + ")","Beskr").  
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokArtikkel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokArtikkel C-Win
ON CHOOSE OF B-SokArtikkel IN FRAME DEFAULT-FRAME /* Søk */
OR F10 OF ArtikkelNr
DO:
  APPLY "ALT-S" to CURRENT-WINDOW.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKjoptDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKjoptDato C-Win
ON CHOOSE OF btnKjoptDato IN FRAME DEFAULT-FRAME /* ... */
DO:
  RUN Cal.w (KjoptDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSalgDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSalgDato C-Win
ON CHOOSE OF btnSalgDato IN FRAME DEFAULT-FRAME /* ... */
DO:
  RUN Cal.w (SalgDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokBatchNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokBatchNr C-Win
ON CHOOSE OF btnSokBatchNr IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF BatchNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "BatchNr".
  RUN JBoxDLookup.w ("BatchLogg;BatchNr;Beskrivelse;OppdStatus","where true",INPUT-OUTPUT cLookupValue).
  IF cLookupValue NE "" THEN 
  DO:
    BatchNr:SCREEN-VALUE = cLookupValue.
    DYNAMIC-FUNCTION("setToolbar",hUpdToolBar,"modified").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokKunde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokKunde C-Win
ON CHOOSE OF btnSokKunde IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF KKundeNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "KundeNr".
  RUN JBoxDLookup.w ("Kunde;Navn;KundeNr;Adresse1;Telefon;MobilTlf", "where true", INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    KKundeNr:SCREEN-VALUE = cLookupValue.
    getKundeInfo(cLookupValue).

    DYNAMIC-FUNCTION("setToolbar",hUpdToolBar,"modified").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokPost-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokPost-3 C-Win
ON CHOOSE OF btnSokPost-3 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF LevNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "LevNr".
  RUN JBoxDLookup.w ("LevBas;LevNamn;LevNr","where true", INPUT-OUTPUT cLookupValue).
  IF cLookupValue NE "" THEN 
  DO:
    LevNr:SCREEN-VALUE = cLookupValue.
    cLookupValue = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = '" + LevNr:SCREEN-VALUE + "'").
    LevNamn:SCREEN-VALUE = ENTRY(1,cLookupValue,"|").
    DYNAMIC-FUNCTION("setToolbar",hUpdToolBar,"modified").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokPost1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokPost1 C-Win
ON CHOOSE OF btnSokPost1 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF PostNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "PostNr".
  RUN JBoxDLookup.w ("Post;PostNr;Beskrivelse","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    PostNr:SCREEN-VALUE = cLookupValue.
    cLookupValue = DYNAMIC-FUNCTION("getFieldList","Post;Beskrivelse","WHERE PostNr = '" + PostNr:SCREEN-VALUE + "'").
    PostSted:SCREEN-VALUE = ENTRY(1,cLookupValue,"|").
    DYNAMIC-FUNCTION("setToolbar",hUpdToolBar,"modified").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokPost2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokPost2 C-Win
ON CHOOSE OF btnSokPost2 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF SPostNR
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "PostNr".
  RUN JBoxDLookup.w ("Post;PostNr;Beskrivelse","where true", INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    SPostNr:SCREEN-VALUE = cLookupValue.
    cLookupValue = DYNAMIC-FUNCTION("getFieldList","Post;Beskrivelse","WHERE PostNr = '" + SPostNr:SCREEN-VALUE + "'").
    SPostSted:SCREEN-VALUE = ENTRY(1,cLookupValue,"|").
    DYNAMIC-FUNCTION("setToolbar",hUpdToolBar,"modified").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokStrKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokStrKode C-Win
ON CHOOSE OF btnSokStrKode IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF StrKode
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "StrKode".
  RUN JBoxDLookup.w ("StrKonv;StrKode;Storl;Merknad","where true", INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    StrKode:SCREEN-VALUE = cLookupValue.
    cLookupValue = DYNAMIC-FUNCTION("getFieldList","StrKonv;Storl","WHERE StrKode = " + StrKode:SCREEN-VALUE).
    Storl:SCREEN-VALUE = ENTRY(1,cLookupValue,"|").
    DYNAMIC-FUNCTION("setToolbar",hUpdToolBar,"modified").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ButNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButNr C-Win
ON RETURN OF ButNr IN FRAME DEFAULT-FRAME /* ButikkNr */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButNr C-Win
ON VALUE-CHANGED OF ButNr IN FRAME DEFAULT-FRAME /* ButikkNr */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ForsNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ForsNr C-Win
ON RETURN OF ForsNr IN FRAME DEFAULT-FRAME /* Kasserer */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME KKundeNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KKundeNr C-Win
ON RETURN OF KKundeNr IN FRAME DEFAULT-FRAME /* Kundenummer */
OR "TAB" OF KKundeNr
DO:
  DEF VAR cTekst AS CHAR NO-UNDO.

  IF SELF:MODIFIED THEN
  DO:
      ASSIGN
          cTekst = DYNAMIC-FUNCTION("getFieldValues","Kunde",
                                     "WHERE KundeNr = " + KKundeNr:SCREEN-VALUE,
                                     "Navn,Adresse1,Adresse2,Telefon,MobilTlf,PostNr,ePostAdresse")  
          .
      ASSIGN
          Navn:SCREEN-VALUE         = ENTRY(1,cTekst,"|")
          Adresse1:SCREEN-VALUE     = ENTRY(2,cTekst,"|")
          Adresse2:SCREEN-VALUE     = ENTRY(3,cTekst,"|")
          Telefon:SCREEN-VALUE      = ENTRY(4,cTekst,"|")
          MobilTlf:SCREEN-VALUE     = ENTRY(5,cTekst,"|")
          PostNr:SCREEN-VALUE       = ENTRY(6,cTekst,"|")
          ePostAdresse:SCREEN-VALUE = ENTRY(7,cTekst,"|")
          PostSted:SCREEN-VALUE     = DYNAMIC-FUNCTION("getFieldValues","Post",
                                                       "WHERE PostNr = '" + ENTRY(6,cTekst,"|") + "'","Beskrivelse")  
          .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME levnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL levnr C-Win
ON RETURN OF levnr IN FRAME DEFAULT-FRAME /* Leverandør */
OR "TAB" OF LevNr
DO:
  IF SELF:MODIFIED THEN
  DO:
      LevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","LevBas",
                                              "WHERE LevNr = " + LevNr:SCREEN-VALUE,"LevNamn").  
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PostNr C-Win
ON RETURN OF PostNr IN FRAME DEFAULT-FRAME /* PostNr/Sted */
OR "TAB" OF PostNr
DO:
  IF SELF:MODIFIED THEN
  DO:
      PostSted:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Post",
                                              "WHERE PostNr = '" + PostNr:SCREEN-VALUE + "'","Beskrivelse").  
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SelgerNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SelgerNr C-Win
ON RETURN OF SelgerNr IN FRAME DEFAULT-FRAME /* SelgerNr */
DO:
    APPLY "TAB" TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SMobilTlf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SMobilTlf C-Win
ON RETURN OF SMobilTlf IN FRAME DEFAULT-FRAME
OR "TAB" OF SMobilTlf
DO:
  APPLY "ENTRY" TO ArtikkelNr.  
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SPostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SPostNr C-Win
ON RETURN OF SPostNr IN FRAME DEFAULT-FRAME /* PostNr */
OR "TAB" OF SPostNr
DO:
  IF SELF:MODIFIED THEN
  DO:
      SPostSted:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Post",
                                              "WHERE PostNr = '" + SPostNr:SCREEN-VALUE + "'","Beskrivelse").  
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME StrKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL StrKode C-Win
ON RETURN OF StrKode IN FRAME DEFAULT-FRAME /* Størrelse */
OR "TAB" OF LevNr
DO:
  IF SELF:MODIFIED THEN
  DO:
      Storl:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","StrKonv",
                                              "WHERE StrKode = " + StrKode:SCREEN-VALUE,"Storl").  
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
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("DeleteObject",hFieldMap).
  DYNAMIC-FUNCTION("DeleteObject",hWinToolbar).
  DYNAMIC-FUNCTION("DeleteObject",hUpdToolbar).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

on F2,ALT-S of CURRENT-WINDOW anywhere 
do:
    run d-hsok.w (output ArtikkelNr,""). 
    if ArtikkelNr = ? then
        RETURN NO-APPLY.
    ELSE DO WITH FRAME Default-Frame:
        ASSIGN
            ArtikkelNr:SCREEN-VALUE = STRING(ArtikkelNr)
            Beskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","ArtBas",
                                        "WHERE ArtikkelNr = dec(" + ArtikkelNr:SCREEN-VALUE + ")","Beskr").  

            .
        RUN MoveToTop.
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

  RUN enable_UI.
  {lng.i} /* Oversettelse */

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitFromKey (1,'?').
  &ENDIF
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

/* Initiering av comboer som er avhengige av ButNr */
IF hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("butnr"):BUFFER-VALUE NE iCurrButnr  THEN
    setButCombos (IF hFieldMap:AVAIL THEN hFieldMap:BUFFER-FIELD("butnr"):BUFFER-VALUE ELSE iCl).
ELSE
    setButCombos (iCl).

RUN SUPER.

IF hFieldMap:AVAIL THEN DO WITH FRAME {&FRAME-NAME}:
  ASSIGN iCurrButnr             = hFieldMap:BUFFER-FIELD("butnr"):BUFFER-VALUE
         /*
         cTid:SCREEN-VALUE      = STRING(hFieldMap:BUFFER-FIELD("Tid"):BUFFER-VALUE,"HH:MM")
         cBruktTid:SCREEN-VALUE = STRING(hFieldMap:BUFFER-FIELD("BruktTid"):BUFFER-VALUE,"HH:MM")
         */
         .
END.
ELSE
  ASSIGN iCurrButnr             = 0
         /*
         cTid:SCREEN-VALUE      = ""
         cBruktTid:SCREEN-VALUE = ""
         */
         .
ASSIGN
    IndividNr:READ-ONLY   = TRUE
    IndividType:SENSITIVE = FALSE 
    .
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
  DISPLAY IndividType ButNr individnr ArtikkelNr Beskr VmBeskrivelse StrKode 
          Storl Kaliber serienr Garantinummer KKundeNr Navn Adresse1 Adresse2 
          PostNr Poststed ePostAdresse Telefon MobilTlf PersOrgNr VapenKort 
          Jegerkort BatchNr KjoptDato feilvare levnr LevNamn Salgdato NyVare 
          Pris DBKr DB% VareKost VVarekost ForsNr SelgerNr SNavn SAdresse1 
          SAdresse2 SPostNr SPoststed STelefon SMobilTlf ficTekst2 ficTekst4 
          ficTekst1 ficTekst5 ficTekst3 ficTekst6 SeqNr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-SokArtikkel btnKjoptDato btnSalgDato btnSokKunde btnSokPost1 ButNr 
         individnr ArtikkelNr Beskr VmBeskrivelse StrKode Kaliber serienr 
         Garantinummer KKundeNr Navn Adresse1 Adresse2 PostNr Poststed 
         ePostAdresse Telefon MobilTlf PersOrgNr VapenKort Jegerkort BatchNr 
         KjoptDato feilvare levnr Salgdato NyVare Pris DBKr DB% VareKost 
         VVarekost ForsNr SelgerNr SNavn SAdresse1 SAdresse2 SPostNr SPoststed 
         STelefon SMobilTlf btnSokPost2 btnSokPost-3 btnSokStrKode 
         btnSokBatchNr rectUpdToolBar rectWinToolBar RECT-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE extraSetToolbar C-Win 
PROCEDURE extraSetToolbar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER cToolbarState AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          IndividType:SENSITIVE = FALSE 
          .
  END.

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
  DEF INPUT PARAM ihBrowse      AS HANDLE NO-UNDO.
  DEF INPUT PARAM piIndividType AS INT NO-UNDO.

  ASSIGN hBrowse      = ihBrowse
         hQuery       = hBrowse:QUERY
         iIndividType = piIndividType
      .
  RUN InitWindow.

  /* Link objects: */
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hUpdToolBar).

  RUN DisplayRecord.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitFromKey C-Win 
PROCEDURE InitFromKey :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Denne benyttes ved første gangs kall til IndividDet.
               Deretter benyttes UpdateFromKey.
------------------------------------------------------------------------------*/
DEF INPUT PARAM piIndividType   AS INT    NO-UNDO.
DEF INPUT PARAM icIndividNr     AS CHAR   NO-UNDO. /* ? = Ny post */
DEF INPUT PARAM hExternalBrowse AS HANDLE NO-UNDO.

ASSIGN
    bByKey       = TRUE
    iIndividType = piIndividType
    .
hQuery  = DYNAMIC-FUNCTION("NewQuery",1,"",
                           "Individ",
                           "WHERE IndividNr = '" + icIndividNr + "'",
                           "").
RUN InitWindow.

/* Her linkes denn eksterne Browser/Query til den interne (Erstatter denne). */
/* Denne metoden benyttes for å linke Artikkelkortet til detaljvinduet       */
/* På en slik måte at navigasjonstoolbaren i detalvinduet styrer browser i   */
/* artikkelkortet.                                                           */
DYNAMIC-FUNCTION('setAttribute',hQuery,'ExternalBrowse',STRING(hExternalBrowse)).

/* Link objects: */

DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hFieldMap).
DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hUpdToolBar).

/* bByKey = TRUE. */

DYNAMIC-FUNCTION("SetCurrentSourceProc",THIS-PROCEDURE).

IF icIndividNr = '?' THEN
    RUN newRecord.
ELSE 
    RUN DisplayRecord.

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

/* Henter nummer på sentrallager */
iCl = int(DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1")).  

DO WITH FRAME {&FRAME-NAME}:
 
  /* Oppsett av combo-box'er. */
  ASSIGN butnr:DELIMITER             = "|"
         butnr:LIST-ITEM-PAIRS       = "|0|" + DYNAMIC-FUNCTION("GetFieldList","Butiker;Butik|ButNamn;Butik","WHERE TRUE BY ButNamn")
         IndividType:DELIMITER       = "|"
         IndividType:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("GetFieldList","IndType;IndividBeskr;IndividType","WHERE TRUE BY IndividType")
         ForsNr:DELIMITER            = "|"
         SelgerNr:DELIMITER          = "|"
         .
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins)
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hQuery,
                    FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                                                    /* Nb: Felt som brukes her må være hentet i Browser først.                        */
                    "IndividType,ArtikkelNr,Beskr,VmBeskrivelse,IndividNr,SerieNr,Garantinummer,ButNr" +
                    ",StrKode,Storl,Kaliber,KKundeNr,Navn,Adresse1,Adresse2,PostNr,PostSted,Vapenkort,Jegerkort,BatchNr,LevNr,LevNamn,DbKr,Db%,VareKost,VVAreKost,SNAvn,SAdresse1,SAdresse2,SPostNr,SPostSted,STelefon,SMobilTlf" +
                    ",SeqNr,NyVare,ePostAdresse,Telefon,MobilTlf,PersOrgNr,FeilVare,KjoptDato,SalgDato,Pris,ForsNr,SelgerNr"
                       ,       
                    "IndividType,ArtikkelNr,Beskr,VmBeskrivelse,IndividNr,SerieNr,Garantinummer,ButNr" + /* Corresponding input fields (fill-in..)*/
                    ",StrKode,Storl,Kaliber,KKundeNr,Navn,Adresse1,Adresse2,PostNr,PostSted,Vapenkort,Jegerkort,BatchNr,LevNr,LevNamn,DbKr,Db%,VareKost,VVAreKost,SNAvn,SAdresse1,SAdresse2,SPostNr,SPostSted,STelefon,SMobilTlf" +
                    ",SeqNr,NyVare,ePostAdresse,Telefon,MobilTlf,PersOrgNr,FeilVare,KjoptDato,SalgDato,Pris,ForsNr,SelgerNr"
                       ,       
                    "", /* Additional buffer and displ.fields - not updateable*/
                    "",  
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

  hUpdToolBar = DYNAMIC-FUNCTION("NewToolBar",
                    rectUpdToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                          /* Corresponding menu label - no menu if blank */
                    "New;Ny,Undo;Angre,delete;Slett,save;Lagre,first|Naviger;Første,prev|Naviger;Forrige,next|Naviger;Neste,last|Naviger;Siste,|Innstillinger",
                                                    /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    "maxborder").                   /* Misc - for something I might need in next version.. */

  hWinToolBar = DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                          /* Corresponding menu label - no menu if blank */
                    "Help|Hjelp,Close;Avslutt",
                                                    /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    "").                            /* Misc - for something I might need in next version.. */

  /* Link objects: */
  DYNAMIC-FUNCTION("CreateObjectLink",hUpdToolbar,hFieldMap).
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectUpdToolBar").
  /*
  DYNAMIC-FUNCTION("setResizeXgroup", THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,50, "IndividType,BruktButikkNr,Bruktkassenr,BruktKassNr,BruktSelgerNr,butnr,KAdresse1,KasseNr,kassnr,KNavn,MAdresse1,MEtterNavn,MForNavn,SelgerNr").
  DYNAMIC-FUNCTION("setMoveXgroup", THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,50, "MPostNr,MTelefon,btnDBruktdato,btnSokMedlem,BruktBongNr,Bruktdato,cBruktTid,MedlemsNr,BruktButikkNr,Bruktkassenr,BruktKassNr,BruktSelgerNr,MAdresse1,MEtterNavn,MForNavn").
  */
  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,500,0,0).
  DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("SetToolbar",hWinToolBar,"enable").

  /* Felter som skal være disablet. Comma separert. */
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"disabledFields","IndividType,IndividNr,SeqNr,Beskr,Storl,LevNamn,DbKr,Db%"). 

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
/* APPLY "entry" TO hBrowse.  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newRecord C-Win 
PROCEDURE newRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      ButNr:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = STRING(iCl)
      .

  /* Initiering av comboer som er avhengige av ButNr */
  setButCombos (int(ButNr:SCREEN-VALUE)).

  RUN SUPER.

  ASSIGN
      IndividNr:READ-ONLY IN FRAME {&FRAME-NAME}      = FALSE
      IndividType:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(iIndividType)
      ButNr:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = STRING(iCl)
      .
  /* er det startet fra artikelkortet o.l., er artikkel kjent og skal ikke enres. */
  IF lArtikkelNr <> 0 THEN
      RUN setArtikkelNr (lArtikkelNr).

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

DEF VAR fIndividNr AS DEC FORMAT ">>>>>>>>>>>9" NO-UNDO.
DEF VAR iSeqNr     AS INT                       NO-UNDO.
DEF VAR cState     AS CHAR                      NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  ASSIGN
      cState = DYNAMIC-FUNCTION('getToolbarState',hUpdToolBar)
      .

  IF int(ButNr:SCREEN-VALUE) = 0 THEN
  DO:
      MESSAGE "Butikk må angis."
          VIEW-AS ALERT-BOX WARNING BUTTONS OK  TITLE "Lagringskontroll".
      APPLY "ENTRY" TO ButNr.
      RETURN NO-APPLY.
  END.
  /*IF dec(ArtikkelNr:SCREEN-VALUE) <> 0 THEN*/
  DO:
      IF dec(ArtikkelNr:SCREEN-VALUE) <> int(DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = dec(" + ArtikkelNr:SCREEN-VALUE + ")","ArtikkelNr")) THEN
      DO:
          MESSAGE "Ugyldig artikkelnummer."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK  TITLE "Lagringskontroll".
          APPLY "ENTRY" TO ArtikkelNr.
          RETURN NO-APPLY.
      END.
  END.
  IF INT(StrKode:SCREEN-VALUE) <> 0 THEN
  DO:
      IF int(StrKode:SCREEN-VALUE) <> int(DYNAMIC-FUNCTION("getFieldValues","StrKonv","WHERE StrKonv.StrKode = " + StrKode:SCREEN-VALUE,"StrKode")) THEN
      DO:
          MESSAGE "Ugyldig størrelseskode."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK  TITLE "Lagringskontroll".
          APPLY "ENTRY" TO StrKode.
          RETURN NO-APPLY.
      END.
  END.
  IF INT(KKundeNr:SCREEN-VALUE) <> 0 THEN
  DO:
      IF dec(KKundeNr:SCREEN-VALUE) <> DEC(DYNAMIC-FUNCTION("getFieldValues","Kunde","WHERE KundeNr = " + KKundeNr:SCREEN-VALUE ,"KundeNr")) THEN
      DO:
          MESSAGE "Ugyldig kundenummer."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK  TITLE "Lagringskontroll".
          APPLY "ENTRY" TO KKundeNr.
          RETURN NO-APPLY.
      END.
  END.
  IF INT(LevNr:SCREEN-VALUE) <> 0 THEN
  DO:
      IF int(LevNr:SCREEN-VALUE) <> int(DYNAMIC-FUNCTION("getFieldValues","LevBas","WHERE LevNr = " + LevNr:SCREEN-VALUE,"LevNr")) THEN
      DO:
          MESSAGE "Ugyldig leverandørnummer."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK  TITLE "Lagringskontroll".
          APPLY "ENTRY" TO LevNr.
          RETURN NO-APPLY.
      END.
  END.
  IF INT(BatchNr:SCREEN-VALUE) <> 0 THEN
  DO:
      IF int(BatchNr:SCREEN-VALUE) <> int(DYNAMIC-FUNCTION("getFieldValues","BatchLogg","WHERE BatchNr = " + BatchNr:SCREEN-VALUE,"BatchNr")) THEN
      DO:
          MESSAGE "Ugyldig batchloggnummer."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK  TITLE "Lagringskontroll".
          APPLY "ENTRY" TO BatchNr.
          RETURN NO-APPLY.
      END.
  END.
  IF (PostNr:SCREEN-VALUE) <> "" THEN
  DO:
      IF (PostNr:SCREEN-VALUE) <> (DYNAMIC-FUNCTION("getFieldValues","Post","WHERE PostNr = '" + PostNr:SCREEN-VALUE + "'","PostNr")) THEN
      DO:
          MESSAGE "Ugyldig postnummer (Kjøper)."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK  TITLE "Lagringskontroll".
          APPLY "ENTRY" TO PostNr.
          RETURN NO-APPLY.
      END.
  END.
  IF (SPostNr:SCREEN-VALUE) <> "" THEN
  DO:
      IF (SPostNr:SCREEN-VALUE) <> (DYNAMIC-FUNCTION("getFieldValues","Post","WHERE PostNr = '" + SPostNr:SCREEN-VALUE + "'","PostNr")) THEN
      DO:
          MESSAGE "Ugyldig postnummer (Selger)."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK  TITLE "Lagringskontroll".
          APPLY "ENTRY" TO SPostNr.
          RETURN NO-APPLY.
      END.
  END.


  IF DYNAMIC-FUNCTION("getToolbarState",hUpdToolbar) = "NEW" THEN
  DO:
    /* Via AppServer */
    IF DYNAMIC-FUNCTION("getAppserviceHandle") <> ? THEN
    DO:
        run tildelindividnr.p ON DYNAMIC-FUNCTION("getAppserviceHandle") 
            (int(ButNr:SCREEN-VALUE),OUTPUT fIndividNr, OUTPUT iSeqNr) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO:
            IF DYNAMIC-FUNCTION("reConnectServer") THEN
                run tildelindividnr.p ON DYNAMIC-FUNCTION("getAppserviceHandle") 
                    (int(ButNr:SCREEN-VALUE),OUTPUT fIndividNr, OUTPUT iSeqNr) NO-ERROR.
        END.       
    END.
    /* Direkte */
    ELSE
        run tildelindividnr.p (int(ButNr:SCREEN-VALUE),OUTPUT fIndividNr, OUTPUT iSeqNr) NO-ERROR.

    ASSIGN
        IndividNr:SCREEN-VALUE = STRING(fIndividNr)
        SeqNr:SCREEN-VALUE     = STRING(iSeqNr)
        .
  END.
  /*
  ASSIGN cTid cBruktTid.

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraValues",
                   STRING(INT(SUBSTR(cTid,1,2)) * 3600 + INT(SUBSTR(cTid,3)) * 60) + "|" +
                   STRING(INT(SUBSTR(cBruktTid,1,2)) * 3600 + INT(SUBSTR(cBruktTid,3)) * 60)).
  */
END.

RUN SUPER.

/* Dette gjøre kun når IndividDet kalles fra andre steder. F.eks artikkelkortet. */
IF valid-handle(hParent) THEN 
DO:
   IF cState = 'NEW' THEN
       RUN openQueryBrowse IN hParent NO-ERROR.
   ELSE
       RUN refreshBrowse IN hParent NO-ERROR.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setArtikkelNr C-Win 
PROCEDURE setArtikkelNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM plArtikkelNr AS DEC NO-UNDO.

  DO WITH FRAME default-frame:
      ASSIGN
          ArtikkelNr:SCREEN-VALUE = STRING(plArtikkelNr)
          lArtikkelNr             = plArtikkelNr
          ArtikkelNr:SENSITIVE    = FALSE
          B-SokArtikkel:SENSITIVE = FALSE
          Beskr:SENSITIVE         = FALSE
          .
      APPLY "TAB" TO ArtikkelNr.
      APPLY "ENTRY" TO VmBeskrivelse.
  END.


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
    DEF INPUT PARAM piIndividType AS INT NO-UNDO.
    DEF INPUT PARAM icIndividNr   AS CHAR NO-UNDO. /* ? = Ny post */

    DEF VAR iNumRes   AS INT NO-UNDO.

    ASSIGN
        bByKey       = TRUE
        iIndividType = piIndividType
        .

    iNumRes = DYNAMIC-FUNCTION("FillQuery",hQuery,1,"",
                               "Individ",
                               "WHERE IndividNr = '" + icIndividNr + "'"
                            ).

    DYNAMIC-FUNCTION("SetCurrentSourceProc",THIS-PROCEDURE).

    IF icIndividNr = '?' THEN
        RUN newRecord.
    ELSE 
        RUN DisplayRecord.
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
    /*
    WHEN "BruktButikkNr" THEN setBruktButCombos (INT(BruktButikkNr:SCREEN-VALUE)).
    */
  END CASE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKundeInfo C-Win 
FUNCTION getKundeInfo RETURNS LOGICAL
  ( INPUT icKundeNr AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cKundeValues AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  cKundeValues = DYNAMIC-FUNCTION("getFieldList","Kunde;Navn;Telefon;Adresse1;PostNr;Adresse2;ePostAdresse" + 
                                  ";MobilTlf,Post;Beskrivelse","WHERE KundeNr = " + icKundeNr + ", first Post no-lock where Post.PostNr = Kunde.PostNr OUTER-JOIN").

  IF cKundeValues NE "" THEN DO ix = 1 TO NUM-ENTRIES(cKundeValues,"|"):
    CASE ix:
      WHEN  1 THEN Navn:SCREEN-VALUE = ENTRY(ix,cKundeValues,"|").
      WHEN  2 THEN Telefon:SCREEN-VALUE = ENTRY(ix,cKundeValues,"|").
      WHEN  3 THEN Adresse1:SCREEN-VALUE = ENTRY(ix,cKundeValues,"|").
      WHEN  4 THEN PostNr:SCREEN-VALUE = ENTRY(ix,cKundeValues,"|").
      WHEN  5 THEN Adresse2:SCREEN-VALUE = ENTRY(ix,cKundeValues,"|").
      WHEN  6 THEN ePostAdresse:SCREEN-VALUE = ENTRY(ix,cKundeValues,"|").
      WHEN  7 THEN MobilTlf:SCREEN-VALUE = ENTRY(ix,cKundeValues,"|").
      WHEN  8 THEN PostSted:SCREEN-VALUE = ENTRY(ix,cKundeValues,"|").
    END CASE.
  END.
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
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN SelgerNr:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("getFieldList","ButikkSelger,Selger;SelgerNr|Navn;SelgerNr",
                                              "WHERE ButikkNr = " + STRING(iiButNr) + 
                                              ", FIRST Selger OF ButikkSelger NO-LOCK"),"|")
         ForsNr:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("getFieldList","butikkforsalj,Forsalj;ForsNr|FoNamn;ForsNr",
                                              "WHERE Butik = " + STRING(iiButNr) + 
                                              ", FIRST Forsalj OF butikkforsalj NO-LOCK"),"|")
         .
END.

RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

