&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
DEFINE INPUT  PARAMETER dModellnr AS DECIMAL    NO-UNDO.
/* DEFINE INPUT  PARAMETER ipArtikkelNr LIKE ArtBas.ArtikkelNr    NO-UNDO. */
DEFINE INPUT  PARAMETER cGenEan       AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER wForslagLopNr AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER lOppdater     AS LOGICAL    NO-UNDO.
DEFINE INPUT  PARAMETER hParent       AS HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER hParentWIN    AS HANDLE     NO-UNDO.


/* Local Variable Definitions ---                                       */
DEFINE VARIABLE iAntArt AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAntChecked AS INTEGER    NO-UNDO.
DEFINE VARIABLE dNyHmodell AS DECIMAL    NO-UNDO.
DEFINE TEMP-TABLE TT_ArtBas NO-UNDO LIKE ArtBas
    FIELD Varekost AS DECI
    FIELD Tilbud AS LOG
    FIELD Pris   AS DECI
    FIELD KalkyleAvvik AS LOG
    FIELD PrisKoFinnes AS LOG
    FIELD ProfilNr     AS INTE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME B-Model

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ArtBas

/* Definitions for BROWSE B-Model                                       */
&Scoped-define FIELDS-IN-QUERY-B-Model ArtBas.ArtikkelNr ~
ArtBas.Artikkelnr = Artbas.modellfarge ArtBas.Beskr ArtBas.BongTekst ~
ArtBas.KundeRabatt ArtBas.ManRabIKas ArtBas.LevKod ArtBas.LevFargKod ~
ArtBas.SaSong ArtBas.ProdNr ArtBas.AnbefaltPris ArtBas.forhRab% ~
ArtBas.supRab% 
&Scoped-define ENABLED-FIELDS-IN-QUERY-B-Model 
&Scoped-define QUERY-STRING-B-Model FOR EACH ArtBas ~
      WHERE ArtBas.ModellFarge = dModellNr NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B-Model OPEN QUERY B-Model FOR EACH ArtBas ~
      WHERE ArtBas.ModellFarge = dModellNr NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B-Model ArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-B-Model ArtBas


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-B-Model}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ArtBas.VareFakta 
&Scoped-define ENABLED-TABLES ArtBas
&Scoped-define FIRST-ENABLED-TABLE ArtBas
&Scoped-Define ENABLED-OBJECTS RECT-62 RECT-63 RECT-65 BUTTON-Ok RECT-64 ~
RECT-66 B-LeggTil B-Model B-Endre TG-Beskr TG-BongTekst TG-Kunderab ~
TG-ManRab TG-Sasong TG-ProdNr TG-Anbefaltpris TG-Forhrab TG-Supprab ~
TG-Varefakta B-Oppdater B-Angre CB-Prisprofil RS-Vis FI-VelgMasterTxt ~
FI-VisTxt FI-HovedvareTxt FI-KalkyleTxt FI-OvrigtTxt FI-varafaktatxt 
&Scoped-Define DISPLAYED-FIELDS ArtBas.ArtikkelNr ArtBas.Beskr ~
ArtPris.VareKost[1] ArtBas.BongTekst ArtPris.Pris[1] ArtBas.KundeRabatt ~
ArtPris.Tilbud ArtBas.ManRabIKas ArtBas.SaSong SaSong.SasBeskr ~
ArtBas.ProdNr Produsent.Beskrivelse ArtBas.AnbefaltPris ArtBas.forhRab% ~
ArtBas.supRab% ArtBas.VareFakta 
&Scoped-define DISPLAYED-TABLES ArtBas ArtPris SaSong Produsent
&Scoped-define FIRST-DISPLAYED-TABLE ArtBas
&Scoped-define SECOND-DISPLAYED-TABLE ArtPris
&Scoped-define THIRD-DISPLAYED-TABLE SaSong
&Scoped-define FOURTH-DISPLAYED-TABLE Produsent
&Scoped-Define DISPLAYED-OBJECTS TG-Beskr TG-BongTekst TG-Kunderab ~
TG-ManRab TG-Sasong TG-ProdNr TG-Anbefaltpris TG-Forhrab TG-Supprab ~
TG-Varefakta CB-Prisprofil RS-Vis FI-VelgMasterTxt FI-VisTxt ~
FI-HovedvareTxt FI-KalkyleTxt FI-OvrigtTxt FI-varafaktatxt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Angre 
     IMAGE-UP FILE "icon/e-undo.bmp":U
     LABEL "Angre" 
     SIZE 4.8 BY 1.14.

DEFINE BUTTON B-Endre 
     LABEL "Endre artikkel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON B-LeggTil 
     LABEL "Endre/Legg til" 
     SIZE 21 BY 1.05 TOOLTIP "Legg til".

DEFINE BUTTON B-Oppdater 
     IMAGE-UP FILE "icon/saverec.bmp":U
     LABEL "Lagre" 
     SIZE 4.8 BY 1.14.

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE VARIABLE CB-Prisprofil AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Prisprofil" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "         1" 
     DROP-DOWN-LIST
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FI-HovedvareTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Verdier som kan kopieres" 
      VIEW-AS TEXT 
     SIZE 54.2 BY .76
     FONT 18 NO-UNDO.

DEFINE VARIABLE FI-KalkyleTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Normalkalkyle" 
      VIEW-AS TEXT 
     SIZE 31 BY .76
     FONT 18 NO-UNDO.

DEFINE VARIABLE FI-OvrigtTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Øvrig info" 
      VIEW-AS TEXT 
     SIZE 54.2 BY .76
     FONT 18 NO-UNDO.

DEFINE VARIABLE FI-varafaktatxt AS CHARACTER FORMAT "X(256)":U INITIAL "Varefakta    :" 
      VIEW-AS TEXT 
     SIZE 12.4 BY .62 NO-UNDO.

DEFINE VARIABLE FI-VelgMasterTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Velg vare for kopiering" 
      VIEW-AS TEXT 
     SIZE 38 BY .76
     FONT 18 NO-UNDO.

DEFINE VARIABLE FI-VisTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Vis" 
      VIEW-AS TEXT 
     SIZE 8 BY .76
     FONT 18 NO-UNDO.

DEFINE VARIABLE RS-Vis AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Alle", 1,
"Artinfo", 2,
"Kalkyle", 3
     SIZE 47 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 174 BY 13.86.

DEFINE RECTANGLE RECT-63
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE .4 BY 13.86.

DEFINE RECTANGLE RECT-64
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE .4 BY 12.81.

DEFINE RECTANGLE RECT-65
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 174 BY 1.38.

DEFINE RECTANGLE RECT-66
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 174 BY .1.

DEFINE VARIABLE TG-Anbefaltpris AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Beskr AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE TG-BongTekst AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Forhrab AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Kunderab AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE TG-ManRab AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE TG-ProdNr AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Sasong AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Supprab AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Varefakta AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B-Model FOR 
      ArtBas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B-Model
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B-Model C-Win _STRUCTURED
  QUERY B-Model NO-LOCK DISPLAY
      ArtBas.ArtikkelNr COLUMN-LABEL "Artikkelnr" FORMAT ">>>>>>>>>>>>9":U
      ArtBas.Artikkelnr = Artbas.modellfarge FORMAT "H/":U WIDTH 2.8
      ArtBas.Beskr FORMAT "x(30)":U
      ArtBas.BongTekst FORMAT "X(20)":U
      ArtBas.KundeRabatt FORMAT "J/N":U
      ArtBas.ManRabIKas FORMAT "J/N":U
      ArtBas.LevKod FORMAT "x(20)":U
      ArtBas.LevFargKod FORMAT "X(15)":U
      ArtBas.SaSong FORMAT "zz9":U
      ArtBas.ProdNr FORMAT "zzzzz9":U
      ArtBas.AnbefaltPris FORMAT "->>>,>>9.99":U
      ArtBas.forhRab% FORMAT "->>9.99":U
      ArtBas.supRab% FORMAT "->>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 173 BY 5.81 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-Ok AT ROW 1.14 COL 170.8 NO-TAB-STOP 
     B-LeggTil AT ROW 1.24 COL 44
     B-Model AT ROW 2.48 COL 2
     B-Endre AT ROW 9.57 COL 72
     ArtBas.ArtikkelNr AT ROW 9.62 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     ArtBas.Beskr AT ROW 10.62 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     ArtPris.VareKost[1] AT ROW 10.62 COL 121.8 COLON-ALIGNED
          LABEL "Varekost"
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     TG-Beskr AT ROW 10.71 COL 4
     ArtBas.BongTekst AT ROW 11.62 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     ArtPris.Pris[1] AT ROW 11.62 COL 121.8 COLON-ALIGNED
          LABEL "Pris"
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     TG-BongTekst AT ROW 11.71 COL 4
     ArtBas.KundeRabatt AT ROW 12.62 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     ArtPris.Tilbud AT ROW 12.62 COL 121.8 COLON-ALIGNED
          LABEL "På tilbud" FORMAT "J/N"
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     TG-Kunderab AT ROW 12.71 COL 4
     ArtBas.ManRabIKas AT ROW 13.62 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     TG-ManRab AT ROW 13.71 COL 4
     ArtBas.SaSong AT ROW 14.62 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     SaSong.SasBeskr AT ROW 14.62 COL 37.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     TG-Sasong AT ROW 14.71 COL 4
     ArtBas.ProdNr AT ROW 15.62 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     Produsent.Beskrivelse AT ROW 15.62 COL 37.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     TG-ProdNr AT ROW 15.71 COL 4
     ArtBas.AnbefaltPris AT ROW 16.62 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     TG-Anbefaltpris AT ROW 16.71 COL 4
     ArtBas.forhRab% AT ROW 17.62 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     TG-Forhrab AT ROW 17.71 COL 4
     ArtBas.supRab% AT ROW 18.62 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     TG-Supprab AT ROW 18.71 COL 4
     ArtBas.VareFakta AT ROW 19.62 COL 29 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 71 BY 2.38
     TG-Varefakta AT ROW 19.71 COL 4
     B-Oppdater AT ROW 22.48 COL 80
     B-Angre AT ROW 22.48 COL 86
     CB-Prisprofil AT ROW 22.57 COL 61.6 COLON-ALIGNED
     RS-Vis AT ROW 22.62 COL 2 NO-LABEL
     FI-VelgMasterTxt AT ROW 1.29 COL 3 NO-LABEL
     FI-VisTxt AT ROW 8.52 COL 2.6 NO-LABEL
     FI-HovedvareTxt AT ROW 8.52 COL 12.2 COLON-ALIGNED NO-LABEL
     FI-KalkyleTxt AT ROW 9.62 COL 111 COLON-ALIGNED NO-LABEL
     FI-OvrigtTxt AT ROW 14.71 COL 104 COLON-ALIGNED NO-LABEL
     FI-varafaktatxt AT ROW 19.67 COL 14 COLON-ALIGNED NO-LABEL
     RECT-62 AT ROW 8.38 COL 1.6
     RECT-63 AT ROW 8.33 COL 10.8
     RECT-65 AT ROW 1.05 COL 2
     RECT-64 AT ROW 9.43 COL 103.4
     RECT-66 AT ROW 9.43 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 175.8 BY 30.05.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 29.71
         WIDTH              = 175.4
         MAX-HEIGHT         = 35.19
         MAX-WIDTH          = 176
         VIRTUAL-HEIGHT     = 35.19
         VIRTUAL-WIDTH      = 176
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB B-Model B-LeggTil DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN ArtBas.AnbefaltPris IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.ArtikkelNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       B-Model:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

/* SETTINGS FOR FILL-IN ArtBas.Beskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Produsent.Beskrivelse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.BongTekst IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VelgMasterTxt IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-VisTxt IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN ArtBas.forhRab% IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.KundeRabatt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.ManRabIKas IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtPris.Pris[1] IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ArtBas.ProdNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN SaSong.SasBeskr IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ArtBas.SaSong IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.supRab% IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtPris.Tilbud IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
ASSIGN 
       ArtBas.VareFakta:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN ArtPris.VareKost[1] IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B-Model
/* Query rebuild information for BROWSE B-Model
     _TblList          = "SkoTex.ArtBas"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "SkoTex.ArtBas.ModellFarge = dModellNr"
     _FldNameList[1]   > SkoTex.ArtBas.ArtikkelNr
"ArtBas.ArtikkelNr" "Artikkelnr" ">>>>>>>>>>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"ArtBas.Artikkelnr = Artbas.modellfarge" ? "H/" ? ? ? ? ? ? ? no ? no no "2.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > SkoTex.ArtBas.Beskr
"ArtBas.Beskr" ? "x(30)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > SkoTex.ArtBas.BongTekst
"ArtBas.BongTekst" ? "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > SkoTex.ArtBas.KundeRabatt
"ArtBas.KundeRabatt" ? "J/N" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > SkoTex.ArtBas.ManRabIKas
"ArtBas.ManRabIKas" ? "J/N" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = SkoTex.ArtBas.LevKod
     _FldNameList[8]   = SkoTex.ArtBas.LevFargKod
     _FldNameList[9]   = SkoTex.ArtBas.SaSong
     _FldNameList[10]   = SkoTex.ArtBas.ProdNr
     _FldNameList[11]   = SkoTex.ArtBas.AnbefaltPris
     _FldNameList[12]   = SkoTex.ArtBas.forhRab%
     _FldNameList[13]   = SkoTex.ArtBas.supRab%
     _Query            is OPENED
*/  /* BROWSE B-Model */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 23.67
       COLUMN          = 2
       HEIGHT          = 6.91
       WIDTH           = 173
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {0F026C11-5A66-4c2b-87B5-88DDEBAE72A1} type: VSFlexGrid */
      CtrlFrame:MOVE-AFTER(RS-Vis:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Angre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Angre C-Win
ON CHOOSE OF B-Angre IN FRAME DEFAULT-FRAME /* Angre */
DO:
  RUN UndoChecked.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Endre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Endre C-Win
ON CHOOSE OF B-Endre IN FRAME DEFAULT-FRAME /* Endre artikkel */
DO:
/*     RUN EndreArtModell IN hParent (THIS-PROCEDURE,{&WINDOW-NAME}). */
    {&WINDOW-NAME}:SENSITIVE = FALSE.
    hParentWIN:SENSITIVE = TRUE.
    hParentWIN:MOVE-TO-TOP().
    hParentWIN:TOP-ONLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LeggTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LeggTil C-Win
ON CHOOSE OF B-LeggTil IN FRAME DEFAULT-FRAME /* Endre/Legg til */
DO:
    RUN ModellFarge.
    RETURN "NO-APPLY".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B-Model
&Scoped-define SELF-NAME B-Model
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Model C-Win
ON VALUE-CHANGED OF B-Model IN FRAME DEFAULT-FRAME
DO:
  RUN InitPrisprofil.
  RUN InitArtBasInfo.
  RUN FillGrid.
  DISPLAY {&DISPLAYED-FIELDS} ArtPris.Varekost[1] ArtPris.Pris[1] ArtPris.TilBud WITH FRAME {&FRAME-NAME}.
  ArtPris.Tilbud:BGCOLOR = IF ArtPris.tilbud = FALSE THEN ? ELSE 12.
  Sasong.SasBeskr:SCREEN-VALUE = IF AVAIL Sasong THEN Sasong.sasbeskr ELSE "".
  Produsent.Beskrivelse:SCREEN-VALUE = IF AVAIL Produsent AND Produsent.Prodnr > 0 THEN Produsent.Beskrivelse  ELSE "".

  RUN ByttArtikkel IN hParent (ArtBas.Artikkelnr) NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Oppdater
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Oppdater C-Win
ON CHOOSE OF B-Oppdater IN FRAME DEFAULT-FRAME /* Lagre */
DO:
    RUN Uppdatera.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-Win
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
    IF iAntChecked > 0 THEN DO:
        MESSAGE "Du har ikke oppdaterte endringer." SKIP
                "Endringene blir forkastet."
            VIEW-AS ALERT-BOX INFO BUTTONS OK-CANCEL UPDATE lOK AS LOG.
        IF lOK <> TRUE THEN
            RETURN NO-APPLY.
    END.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Prisprofil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Prisprofil C-Win
ON VALUE-CHANGED OF CB-Prisprofil IN FRAME DEFAULT-FRAME /* Prisprofil */
DO:
    RUN InitArtBasInfo.
    RUN FillGrid.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Click
PROCEDURE CtrlFrame.VSFlexGrid.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRow AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCol AS INTEGER    NO-UNDO.
  ASSIGN iRow = chCtrlFrame:MouseRow
         iCol = chCtrlFrame:MouseCol.
  IF iRow < 1 OR iCol < 1 OR chCtrlFrame:Cell(5,iRow,iCol,iRow,iCol) = 0 THEN
      RETURN NO-APPLY.
  chCtrlFrame:Cell(5,iRow,iCol,iRow,iCol) = STRING(chCtrlFrame:Cell(5,iRow,iCol,iRow,iCol) = 1,"2/1").
  IF chCtrlFrame:Cell(5,iRow,iCol,iRow,iCol) = 1 THEN
      iAntChecked = iAntChecked + 1.
  ELSE
      iAntChecked = iAntChecked - 1.
  RUN EnaDisButtons.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-Vis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Vis C-Win
ON VALUE-CHANGED OF RS-Vis IN FRAME DEFAULT-FRAME
DO:
  CB-Prisprofil:SENSITIVE = NOT SELF:SCREEN-VALUE = "2".
  RUN HideShow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Anbefaltpris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Anbefaltpris C-Win
ON VALUE-CHANGED OF TG-Anbefaltpris IN FRAME DEFAULT-FRAME
DO:
    chCtrlFrame:ColHidden(8) = NOT SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Beskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Beskr C-Win
ON VALUE-CHANGED OF TG-Beskr IN FRAME DEFAULT-FRAME
DO:
  chCtrlFrame:ColHidden(2) = NOT SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-BongTekst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-BongTekst C-Win
ON VALUE-CHANGED OF TG-BongTekst IN FRAME DEFAULT-FRAME
DO:
    chCtrlFrame:ColHidden(3) = NOT SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Forhrab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Forhrab C-Win
ON VALUE-CHANGED OF TG-Forhrab IN FRAME DEFAULT-FRAME
DO:
    chCtrlFrame:ColHidden(10) = NOT SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Kunderab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Kunderab C-Win
ON VALUE-CHANGED OF TG-Kunderab IN FRAME DEFAULT-FRAME
DO:
    chCtrlFrame:ColHidden(4) = NOT SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-ManRab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-ManRab C-Win
ON VALUE-CHANGED OF TG-ManRab IN FRAME DEFAULT-FRAME
DO:
    chCtrlFrame:ColHidden(5) = NOT SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-ProdNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-ProdNr C-Win
ON VALUE-CHANGED OF TG-ProdNr IN FRAME DEFAULT-FRAME
DO:
    chCtrlFrame:ColHidden(7) = NOT SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Sasong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Sasong C-Win
ON VALUE-CHANGED OF TG-Sasong IN FRAME DEFAULT-FRAME
DO:
    chCtrlFrame:ColHidden(6) = NOT SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Supprab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Supprab C-Win
ON VALUE-CHANGED OF TG-Supprab IN FRAME DEFAULT-FRAME
DO:
    chCtrlFrame:ColHidden(11) = NOT SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Varefakta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Varefakta C-Win
ON VALUE-CHANGED OF TG-Varefakta IN FRAME DEFAULT-FRAME
DO:
    chCtrlFrame:ColHidden(9) = NOT SELF:CHECKED.
/*     chCtrlFrame:Cell(5,1,9,chCtrlFrame:rows - 1,9) = 2. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
CURRENT-WINDOW:HIDDEN = TRUE.
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
    RUN disable_UI.
    hParentWIN:SENSITIVE = TRUE.    
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

IF NOT CAN-FIND(FIRST ArtBas WHERE ArtBas.ModellFarge = dModellnr) THEN DO:
    RUN w-ModellFarge.w (dModellnr,cGenEan,wForslagLopNr,OUTPUT lOppdater,OUTPUT dNyHmodell).
    MESSAGE dModellNr SKIP
            dNyHModell
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    IF dNyHModell <> dModellNr AND dNyHModell > 0 THEN
        dModellNr = dNyHModell.
    IF lOppdater THEN
        RUN ByttArtikkel IN hParent (dModellNr) NO-ERROR.
END.

IF NOT CAN-FIND(FIRST ArtBas WHERE ArtBas.ModellFarge = dModellnr) THEN DO:
    hParentWIN:SENSITIVE = TRUE.
    APPLY "CLOSE" TO THIS-PROCEDURE.
    RETURN.
END.

CURRENT-WINDOW:HIDDEN = FALSE.
/*   FIND Artbas WHERE ArtBas.artikkelnr = dModellnr AND ArtBas.modellfarge = dModellnr NO-LOCK NO-ERROR. */
/*   IF NOT AVAIL ArtBas THEN DO:                                                                         */
/*       MESSAGE "Finner ikke hovedmodellartikkel"                                                        */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                           */
/*       RETURN.                                                                                          */
/*   END.                                                                                                 */
/*   RUN InitArtBasInfo. */
  ASSIGN Artbas.Kunderabatt:FORMAT = "  J/  N"
         Artbas.ManRabIKas:FORMAT  = "  J/  N".
  RUN enable_UI.
  B-Oppdater:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.
  B-Angre:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.
  APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
/*   RUN FillGrid. */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
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

OCXFile = SEARCH( "wSynkModell.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "wSynkModell.wrx":U SKIP(1)
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
  DISPLAY TG-Beskr TG-BongTekst TG-Kunderab TG-ManRab TG-Sasong TG-ProdNr 
          TG-Anbefaltpris TG-Forhrab TG-Supprab TG-Varefakta CB-Prisprofil 
          RS-Vis FI-VelgMasterTxt FI-VisTxt FI-HovedvareTxt FI-KalkyleTxt 
          FI-OvrigtTxt FI-varafaktatxt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE ArtBas THEN 
    DISPLAY ArtBas.ArtikkelNr ArtBas.Beskr ArtBas.BongTekst ArtBas.KundeRabatt 
          ArtBas.ManRabIKas ArtBas.SaSong ArtBas.ProdNr ArtBas.AnbefaltPris 
          ArtBas.forhRab% ArtBas.supRab% ArtBas.VareFakta 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE ArtPris THEN 
    DISPLAY ArtPris.VareKost[1] ArtPris.Pris[1] ArtPris.Tilbud 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE Produsent THEN 
    DISPLAY Produsent.Beskrivelse 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE SaSong THEN 
    DISPLAY SaSong.SasBeskr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-62 RECT-63 RECT-65 BUTTON-Ok RECT-64 RECT-66 B-LeggTil B-Model 
         B-Endre TG-Beskr TG-BongTekst TG-Kunderab TG-ManRab TG-Sasong 
         TG-ProdNr TG-Anbefaltpris TG-Forhrab TG-Supprab ArtBas.VareFakta 
         TG-Varefakta B-Oppdater B-Angre CB-Prisprofil RS-Vis FI-VelgMasterTxt 
         FI-VisTxt FI-HovedvareTxt FI-KalkyleTxt FI-OvrigtTxt FI-varafaktatxt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnaDisButtons C-Win 
PROCEDURE EnaDisButtons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN B-Angre:SENSITIVE = iAntChecked > 0
               B-Oppdater:SENSITIVE = iAntChecked > 0
               RS-Vis:SENSITIVE = iAntChecked = 0
               CB-PrisProfil:SENSITIVE = iAntChecked = 0 AND INPUT RS-Vis <> 2
               BROWSE B-Model:SENSITIVE = iAntChecked = 0
               TG-Anbefaltpris:SENSITIVE = iAntChecked = 0
               TG-Beskr:SENSITIVE = iAntChecked = 0
               TG-BongTekst:SENSITIVE = iAntChecked = 0
               TG-Forhrab:SENSITIVE = iAntChecked = 0
               TG-Kunderab:SENSITIVE = iAntChecked = 0
               TG-ManRab:SENSITIVE = iAntChecked = 0
               TG-ProdNr:SENSITIVE = iAntChecked = 0
               TG-Sasong:SENSITIVE = iAntChecked = 0
               TG-Supprab:SENSITIVE = iAntChecked = 0.
/*                TG-Varefakta:SENSITIVE = iAntChecked = 0. */
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndreKlar C-Win 
PROCEDURE EndreKlar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE rRowId AS ROWID      NO-UNDO.
    rRowId = ROWID(ArtBas).
    hParentWIN:SENSITIVE = FALSE.
    hParentWIN:TOP-ONLY.
    {&WINDOW-NAME}:MOVE-TO-TOP().
    {&OPEN-QUERY-{&BROWSE-NAME}}
    {&WINDOW-NAME}:SENSITIVE = TRUE.
    REPOSITION {&BROWSE-NAME} TO ROWID rRowId.
    APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
/*     hParentWIN:TOP-ONLY. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillGrid C-Win 
PROCEDURE FillGrid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        chCtrlFrame:Rows = 1.
        FOR EACH TT_ArtBas:
            chCtrlFrame:Rows = chCtrlFrame:Rows + 1.
            chCtrlFrame:Cell(0,chCtrlFrame:Rows - 1,0,chCtrlFrame:Rows - 1,0) = TT_ArtBas.Artikkelnr.
            chCtrlFrame:Cell(0,chCtrlFrame:Rows - 1,2,chCtrlFrame:Rows - 1,2) = TT_ArtBas.Beskr.
            chCtrlFrame:Cell(5,chCtrlFrame:Rows - 1,2,chCtrlFrame:Rows - 1,2) = STRING(TT_ArtBas.Beskr = ArtBas.Beskr,"0/2").
            chCtrlFrame:Cell(0,chCtrlFrame:Rows - 1,3,chCtrlFrame:Rows - 1,3) = TT_ArtBas.Bongtekst.
            chCtrlFrame:Cell(5,chCtrlFrame:Rows - 1,3,chCtrlFrame:Rows - 1,3) = STRING(TT_ArtBas.Bongtekst = ArtBas.Bongtekst,"0/2").
            chCtrlFrame:Cell(0,chCtrlFrame:Rows - 1,4,chCtrlFrame:Rows - 1,4) = STRING(TT_ArtBas.KundeRabatt,"J/N").
            chCtrlFrame:Cell(5,chCtrlFrame:Rows - 1,4,chCtrlFrame:Rows - 1,4) = STRING(TT_ArtBas.KundeRabatt = ArtBas.KundeRabatt,"0/2").
            chCtrlFrame:Cell(0,chCtrlFrame:Rows - 1,5,chCtrlFrame:Rows - 1,5) = STRING(TT_ArtBas.ManRabIKas,"J/N").
            chCtrlFrame:Cell(5,chCtrlFrame:Rows - 1,5,chCtrlFrame:Rows - 1,5) = STRING(TT_ArtBas.ManRabIKas = ArtBas.ManRabIKas,"0/2").
            chCtrlFrame:Cell(0,chCtrlFrame:Rows - 1,6,chCtrlFrame:Rows - 1,6) = TT_ArtBas.Sasong.
            chCtrlFrame:Cell(5,chCtrlFrame:Rows - 1,6,chCtrlFrame:Rows - 1,6) = STRING(TT_ArtBas.Sasong = ArtBas.Sasong,"0/2").
            chCtrlFrame:Cell(0,chCtrlFrame:Rows - 1,7,chCtrlFrame:Rows - 1,7) = TT_ArtBas.ProdNr.
            chCtrlFrame:Cell(5,chCtrlFrame:Rows - 1,7,chCtrlFrame:Rows - 1,7) = STRING(TT_ArtBas.ProdNr = ArtBas.ProdNr,"0/2").
            chCtrlFrame:Cell(0,chCtrlFrame:Rows - 1,8,chCtrlFrame:Rows - 1,8) = STRING(TT_ArtBas.Anbefaltpris,">>>,>>9.99").
            chCtrlFrame:Cell(5,chCtrlFrame:Rows - 1,8,chCtrlFrame:Rows - 1,8) = STRING(TT_ArtBas.Anbefaltpris = ArtBas.Anbefaltpris,"0/2").
            chCtrlFrame:Cell(0,chCtrlFrame:Rows - 1,9,chCtrlFrame:Rows - 1,9) = SUBSTR(TT_ArtBas.Varefakta,1,30).
            chCtrlFrame:Cell(5,chCtrlFrame:Rows - 1,9,chCtrlFrame:Rows - 1,9) = STRING(TT_ArtBas.Varefakta = ArtBas.Varefakta,"0/2").
            chCtrlFrame:Cell(0,chCtrlFrame:Rows - 1,10,chCtrlFrame:Rows - 1,10) = STRING(TT_ArtBas.forhRab%,">>>9.99").
            chCtrlFrame:Cell(5,chCtrlFrame:Rows - 1,10,chCtrlFrame:Rows - 1,10) = STRING(TT_ArtBas.forhRab% = ArtBas.forhRab%,"0/2").
            chCtrlFrame:Cell(0,chCtrlFrame:Rows - 1,11,chCtrlFrame:Rows - 1,11) = STRING(TT_ArtBas.supRab%,">>>9.99").
            chCtrlFrame:Cell(5,chCtrlFrame:Rows - 1,11,chCtrlFrame:Rows - 1,11) = STRING(TT_ArtBas.supRab% = ArtBas.supRab%,"0/2").

            IF TT_ArtBas.Profilnr <> INPUT CB-Prisprofil THEN
                ASSIGN chCtrlFrame:Cell(0,chCtrlFrame:Rows - 1,12,chCtrlFrame:Rows - 1,12) = "Ingen kalkyle"
                       chCtrlFrame:Cell(5,chCtrlFrame:Rows - 1,12,chCtrlFrame:Rows - 1,12) = "0".
            ELSE IF TT_ArtBas.PriskoFinnes = TRUE THEN
                ASSIGN chCtrlFrame:Cell(0,chCtrlFrame:Rows - 1,12,chCtrlFrame:Rows - 1,12) = "Priskøpost"
                       chCtrlFrame:Cell(5,chCtrlFrame:Rows - 1,12,chCtrlFrame:Rows - 1,12) = "0".
            ELSE 
                chCtrlFrame:Cell(5,chCtrlFrame:Rows - 1,12,chCtrlFrame:Rows - 1,12) = STRING(TT_ArtBas.KalkyleAvvik,"2/0").
            chCtrlFrame:Cell(0,chCtrlFrame:Rows - 1,13,chCtrlFrame:Rows - 1,13) = STRING(TT_ArtBas.Varekost,">>>,>>9.99").
            chCtrlFrame:Cell(0,chCtrlFrame:Rows - 1,14,chCtrlFrame:Rows - 1,14) = STRING(TT_ArtBas.Pris,">>>,>>9.99").
            chCtrlFrame:Cell(0,chCtrlFrame:Rows - 1,15,chCtrlFrame:Rows - 1,15) = STRING(TT_ArtBas.Tilbud,"J/").


        END.
    END.
    RUN HideShow.
    chCtrlFrame:AutoSize(1).
    chCtrlFrame:AutoSize(2).
    chCtrlFrame:AutoSize(3).
    chCtrlFrame:AutoSize(4).
    chCtrlFrame:AutoSize(5).
    chCtrlFrame:AutoSize(6).
    chCtrlFrame:AutoSize(7).
    chCtrlFrame:AutoSize(8).
    chCtrlFrame:AutoSize(9).
    chCtrlFrame:AutoSize(10).
    chCtrlFrame:AutoSize(11).
    chCtrlFrame:AutoSize(12).
    chCtrlFrame:AutoSize(13).
    chCtrlFrame:AutoSize(14).
    chCtrlFrame:AutoSize(15).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HideShow C-Win 
PROCEDURE HideShow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN chCtrlFrame:ColHidden(2) = RS-Vis:SCREEN-VALUE = "3" OR NOT TG-Beskr:CHECKED       
               chCtrlFrame:ColHidden(3) = RS-Vis:SCREEN-VALUE = "3" OR NOT TG-BongTekst:CHECKED   
               chCtrlFrame:ColHidden(4) = RS-Vis:SCREEN-VALUE = "3" OR NOT TG-Kunderab:CHECKED    
               chCtrlFrame:ColHidden(5) = RS-Vis:SCREEN-VALUE = "3" OR NOT TG-ManRab:CHECKED    
               chCtrlFrame:ColHidden(6) = RS-Vis:SCREEN-VALUE = "3" OR NOT TG-Sasong:CHECKED      
               chCtrlFrame:ColHidden(7) = RS-Vis:SCREEN-VALUE = "3" OR NOT TG-ProdNr:CHECKED      
               chCtrlFrame:ColHidden(8) = RS-Vis:SCREEN-VALUE = "3" OR NOT TG-Anbefaltpris:CHECKED
               chCtrlFrame:ColHidden(9) = RS-Vis:SCREEN-VALUE = "3" OR NOT TG-Varefakta:CHECKED.
               chCtrlFrame:ColHidden(10) = RS-Vis:SCREEN-VALUE = "3" OR NOT TG-Forhrab:CHECKED.
               chCtrlFrame:ColHidden(11) = RS-Vis:SCREEN-VALUE = "3" OR NOT TG-Supprab:CHECKED.

        ASSIGN chCtrlFrame:ColHidden(12) = NOT CAN-DO("1,3",RS-Vis:SCREEN-VALUE)
               chCtrlFrame:ColHidden(13) = NOT CAN-DO("1,3",RS-Vis:SCREEN-VALUE)
               chCtrlFrame:ColHidden(14) = NOT CAN-DO("1,3",RS-Vis:SCREEN-VALUE)
               chCtrlFrame:ColHidden(15) = NOT CAN-DO("1,3",RS-Vis:SCREEN-VALUE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitArtBasInfo C-Win 
PROCEDURE InitArtBasInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bArtBas FOR ArtBas.
    DEFINE BUFFER bArtpris FOR Artpris.
    EMPTY TEMP-TABLE TT_Artbas.
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST ArtPris OF ArtBas WHERE ArtPris.ProfilNr = INT(CB-PrisProfil:SCREEN-VALUE) NO-LOCK NO-ERROR.
        FIND Sasong OF ArtBas NO-LOCK NO-ERROR.
        FIND Produsent OF ArtBas NO-LOCK NO-ERROR.
        FOR EACH bArtbas WHERE bArtbas.Modellfarge = dModellNr AND bArtBas.Artikkelnr <> ArtBas.Artikkelnr NO-LOCK.
            RELEASE TT_ArtBas.
            BUFFER-COPY bArtBas TO TT_ArtBas.
            FIND FIRST bArtPris OF bArtBas WHERE bArtPris.Profilnr = INPUT CB-Prisprofil NO-LOCK NO-ERROR.
            IF AVAIL bArtPris THEN DO:
                ASSIGN TT_ArtBas.Varekost = bArtPris.Varekost[1]
                       TT_ArtBas.Tilbud   = bArtPris.Tilbud
                       TT_ArtBas.Pris     = bArtPris.Pris[1]
                       TT_ArtBas.KalkyleAvvik = bArtPris.Varekost[1] <> ArtPris.Varekost[1] OR bArtPris.Pris[1] <> ArtPris.Pris[1]
                       TT_ArtBas.ProfilNr     = bArtPris.ProfilNr.
                IF CAN-FIND(FIRST Prisko OF bArtPris WHERE Prisko.ProfilNr = TT_ArtBas.Profilnr) THEN
                    ASSIGN TT_ArtBas.PriskoFinnes = TRUE.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls C-Win 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   chCtrlFrame = chCtrlFrame:VSFlexGrid.
   chCtrlFrame:Rows = 1.
   chCtrlFrame:Cols = 16.
   chCtrlFrame:Cell(5,0,1,0,1) = 1.
/*    chCtrlFrame:Cell(5,1,1,iAntArt,1) = 2. */
   chCtrlFrame:AllowSelection = FALSE.
   chCtrlFrame:Cell(0,0,0,0,0) = "Artikkelnr".
   chCtrlFrame:Cell(0,0,1,0,1) = "Synk".
   chCtrlFrame:Cell(0,0,2,0,2) = "Varetekst".
   chCtrlFrame:Cell(0,0,3,0,3) = "Bongtekst".
   chCtrlFrame:Cell(0,0,4,0,4) = "Kunderabatt".
   chCtrlFrame:Cell(0,0,5,0,5) = "Man.rabatt".
   chCtrlFrame:Cell(0,0,6,0,6) = "Sesong".
   chCtrlFrame:Cell(0,0,7,0,7) = "Produsent".
   chCtrlFrame:Cell(0,0,8,0,8) = "Anbefalt pris".
   chCtrlFrame:Cell(0,0,9,0,9) = "Varefakta".
   chCtrlFrame:Cell(0,0,10,0,10) = "Forh.rab%".
   chCtrlFrame:Cell(0,0,11,0,11) = "Supp.rab%".


   chCtrlFrame:Cell(0,0,12,0,12) = "Kalkyle".
   chCtrlFrame:Cell(0,0,13,0,13) = "Varekost".
   chCtrlFrame:Cell(0,0,14,0,14) = "Normalpris".
   chCtrlFrame:Cell(0,0,15,0,15) = "På tilbud".
   ASSIGN
/*    chCtrlFrame:ColHidden(2) = TRUE  */
/*    chCtrlFrame:ColHidden(3) = TRUE  */
/*    chCtrlFrame:ColHidden(4) = TRUE  */
/*    chCtrlFrame:ColHidden(5) = TRUE  */
/*    chCtrlFrame:ColHidden(6) = TRUE  */
/*    chCtrlFrame:ColHidden(7) = TRUE. */
/*    chCtrlFrame:ColHidden(8) = TRUE. */
   chCtrlFrame:ColAlignment(8) = 7.
   chCtrlFrame:ColAlignment(10) = 7.
   chCtrlFrame:ColAlignment(11) = 7.
   chCtrlFrame:ColAlignment(13) = 7.
   chCtrlFrame:ColAlignment(14) = 7.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitPrisprofil C-Win 
PROCEDURE InitPrisprofil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cPrisprofil AS CHARACTER  NO-UNDO.
    FOR EACH ArtPris OF ArtBas NO-LOCK.
        cPrisProfil = cPrisProfil + (IF cPrisProfil <> "" THEN "," ELSE "") + STRING(Artpris.ProfilNr).
    END.
    ASSIGN CB-Prisprofil:LIST-ITEMS IN FRAME {&FRAME-NAME} = cPrisProfil
           CB-PrisProfil:SCREEN-VALUE = ENTRY(1,CB-Prisprofil:LIST-ITEMS).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ModellFarge C-Win 
PROCEDURE ModellFarge :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {&WINDOW-NAME}:SENSITIVE = FALSE.
    ASSIGN dNyHmodell = ?
           lOppdater = FALSE.
    RUN w-ModellFarge.w (dModellnr,cGenEan,wForslagLopNr,OUTPUT lOppdater,OUTPUT dNyHmodell).
    {&WINDOW-NAME}:SENSITIVE = TRUE.
    IF lOppdater = TRUE THEN DO:
        ASSIGN dModellNr = IF dNyHmodell <> ? AND dNyHmodell <> 0 THEN dNyHmodell ELSE dModellNr.
        RUN ByttArtikkel IN hParent (dModellNr) NO-ERROR.
        IF NOT CAN-FIND(FIRST Artbas WHERE ArtBas.Modellfarge = dModellnr) THEN
            APPLY "CLOSE" TO THIS-PROCEDURE.
        ELSE DO:
            {&OPEN-QUERY-{&BROWSE-NAME}}
            APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UndoChecked C-Win 
PROCEDURE UndoChecked :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iRow AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iCol AS INTEGER    NO-UNDO.
    DO iRow = 1 TO chCtrlframe:ROWS - 1:
        DO iCol = 1 TO chCtrlframe:Cols - 1:
            IF chCtrlframe:cell(5,iRow,iCol,iRow,iCol)  = 1 THEN
                chCtrlframe:cell(5,iRow,iCol,iRow,iCol) = 2.
        END.
    END.
    iAntChecked = 0.
    RUN EnaDisButtons.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Uppdatera C-Win 
PROCEDURE Uppdatera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dArtikkelnr AS DECIMAL    NO-UNDO.
  DEFINE BUFFER bArtBas FOR Artbas.
  DEFINE BUFFER bArtPris FOR Artpris.
  DO WITH FRAME {&FRAME-NAME}:
  DO ii = 1 TO chCtrlFrame:Rows - 1:
      DO:
          ASSIGN dArtikkelnr = DECI(chCtrlFrame:Cell(0,ii,0,ii,0)).
          FIND bArtBas WHERE bArtbas.Artikkelnr = dArtikkelnr NO-ERROR.
          ASSIGN bArtBas.Beskr        = IF chCtrlFrame:Cell(5,ii,2,ii,2) = "1"  THEN ArtBas.Beskr        ELSE bArtBas.Beskr
                 bArtBas.Bongtekst    = IF chCtrlFrame:Cell(5,ii,3,ii,3) = "1"  THEN ArtBas.Bongtekst    ELSE bArtBas.Bongtekst
                 bArtBas.KundeRabatt  = IF chCtrlFrame:Cell(5,ii,4,ii,4) = "1"  THEN ArtBas.KundeRabatt  ELSE bArtBas.KundeRabatt
                 bArtBas.ManRabIKas   = IF chCtrlFrame:Cell(5,ii,5,ii,5) = "1"  THEN ArtBas.ManRabIKas  ELSE bArtBas.ManRabIKas
                 bArtBas.Sasong       = IF chCtrlFrame:Cell(5,ii,6,ii,6) = "1"  THEN ArtBas.Sasong       ELSE bArtBas.Sasong
                 bArtBas.ProdNr       = IF chCtrlFrame:Cell(5,ii,7,ii,7) = "1"  THEN ArtBas.ProdNr       ELSE bArtBas.ProdNr
                 bArtBas.Anbefaltpris = IF chCtrlFrame:Cell(5,ii,8,ii,8) = "1"  THEN ArtBas.Anbefaltpris ELSE bArtBas.Anbefaltpris
                 bArtBas.Varefakta    = IF chCtrlFrame:Cell(5,ii,9,ii,9) = "1"  THEN ArtBas.Varefakta    ELSE bArtBas.Varefakta.
                 bArtBas.forhRab%    = IF chCtrlFrame:Cell(5,ii,10,ii,10) = "1"  THEN ArtBas.forhRab%    ELSE bArtBas.forhRab%.
                 bArtBas.supRab%    = IF chCtrlFrame:Cell(5,ii,11,ii,11) = "1"  THEN ArtBas.supRab%    ELSE bArtBas.supRab%.
          /* Hantering av kalkyle */
          IF chCtrlFrame:Cell(5,ii,12,ii,12) = "1" THEN DO:
              FIND FIRST bArtPris WHERE bArtPris.Artikkelnr = dArtikkelnr AND bArtPris.ProfilNr = ArtPris.ProfilNr NO-ERROR.
              IF AVAIL bArtPris THEN DO:
                  BUFFER-COPY ArtPris EXCEPT ArtPris.ArtikkelNr TO bArtPris.
                  bArtPris.tilbud = FALSE.
                  RELEASE bArtPris.
              END.
          END.
      END.
  END.
  END.
  ASSIGN lOppdater = TRUE.
  iAntChecked = 0.
  RUN InitArtBasInfo.
  RUN FillGrid.
  RUN EnaDisButtons.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

