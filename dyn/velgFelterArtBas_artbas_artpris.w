&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF OUTPUT PARAM ocExceptList AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocLabelList  AS CHAR NO-UNDO.
DEF OUTPUT PARAM oiReturn     AS INT  NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR hWidget AS HANDLE NO-UNDO.
DEF VAR hParent AS HANDLE NO-UNDO.
DEF VAR bHidden AS LOG    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 RECT-4 RECT-56 RECT-72 tbChooseAll ~
LevKod LinkVareNr KjedeVare Beskr EkstStrTypeNavn Gjennomfaktureres ~
LevFargKod AntIPkn LokPris LevNr Salgsenhet KjedeRab% ProdNr Etikettekst1 ~
KjedeSupRab% VmId BongTekst Sasong Anonseartikkel LevDato1 Vg ~
Grunnsortiment InnkjopsPris VPIBildekode supRab% RAvdNr forhRab% Mengde ~
AnbefaltPris JamforEnhet Pris KInnkjopspris KFrakt% KDivKost% Mva% KRab1% ~
KPris KRab2% KRab3% NyStrekkode KorrStrekkode Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS tbChooseAll LevKod LinkVareNr KjedeVare ~
Beskr EkstStrTypeNavn Gjennomfaktureres LevFargKod AntIPkn LokPris LevNr ~
Salgsenhet KjedeRab% ProdNr Etikettekst1 KjedeSupRab% VmId BongTekst Sasong ~
Anonseartikkel LevDato1 Vg Grunnsortiment InnkjopsPris VPIBildekode supRab% ~
RAvdNr forhRab% Mengde AnbefaltPris JamforEnhet Pris KInnkjopspris KFrakt% ~
KDivKost% Mva% KRab1% KPris KRab2% KRab3% NyStrekkode KorrStrekkode ~
FILL-IN-22 FILL-IN-23 FILL-IN-24 FILL-IN-25 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MultiSelectTb Dialog-Frame 
FUNCTION MultiSelectTb RETURNS LOGICAL
  ( INPUT ibChecked AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-22 AS CHARACTER FORMAT "X(256)":U INITIAL "Artikkelinformasjon" 
      VIEW-AS TEXT 
     SIZE 27 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-23 AS CHARACTER FORMAT "X(256)":U INITIAL "VPI informasjon" 
      VIEW-AS TEXT 
     SIZE 27 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-24 AS CHARACTER FORMAT "X(256)":U INITIAL "Kalkyle" 
      VIEW-AS TEXT 
     SIZE 27 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-25 AS CHARACTER FORMAT "X(256)":U INITIAL "Strekkoder" 
      VIEW-AS TEXT 
     SIZE 27 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 9.05.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 128 BY 2.62.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 4.33.

DEFINE RECTANGLE RECT-72
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 9.05.

DEFINE VARIABLE AnbefaltPris AS LOGICAL INITIAL no 
     LABEL "Veiledende (anbefalt) pris" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE Anonseartikkel AS LOGICAL INITIAL no 
     LABEL "Anonseartikkel" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE AntIPkn AS LOGICAL INITIAL no 
     LABEL "Antall i pakkning" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .81 NO-UNDO.

DEFINE VARIABLE Beskr AS LOGICAL INITIAL no 
     LABEL "Varetekst" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE BongTekst AS LOGICAL INITIAL no 
     LABEL "Bongtekst" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE EkstStrTypeNavn AS LOGICAL INITIAL no 
     LABEL "Størrelsestypebetegnelse" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .81 NO-UNDO.

DEFINE VARIABLE Etikettekst1 AS LOGICAL INITIAL no 
     LABEL "Etikettekst 1" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE forhRab% AS LOGICAL INITIAL no 
     LABEL "Rab.forh" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE Gjennomfaktureres AS LOGICAL INITIAL no 
     LABEL "Gjennomfaktureres" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE Grunnsortiment AS LOGICAL INITIAL no 
     LABEL "Grunnsortiment" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE InnkjopsPris AS LOGICAL INITIAL no 
     LABEL "Engros" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE JamforEnhet AS LOGICAL INITIAL no 
     LABEL "Jamførenhet" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE KDivKost% AS LOGICAL INITIAL no 
     LABEL "DivKost%" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE KFrakt% AS LOGICAL INITIAL no 
     LABEL "Frakt%" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE KInnkjopspris AS LOGICAL INITIAL no 
     LABEL "Innkjøpspris" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE KjedeRab% AS LOGICAL INITIAL no 
     LABEL "Kjede forh.rabatt" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE KjedeSupRab% AS LOGICAL INITIAL no 
     LABEL "Kjede sup.rabatt" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE KjedeVare AS LOGICAL INITIAL no 
     LABEL "Kjedelevert" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE KorrStrekkode AS LOGICAL INITIAL no 
     LABEL "Korriger strekkoder som ligger feil" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE KPris AS LOGICAL INITIAL no 
     LABEL "Pris" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE KRab1% AS LOGICAL INITIAL no 
     LABEL "Rab1%" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE KRab2% AS LOGICAL INITIAL no 
     LABEL "Rab2%" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE KRab3% AS LOGICAL INITIAL no 
     LABEL "Rab3%" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE LevDato1 AS LOGICAL INITIAL no 
     LABEL "LevUke (4 felter)" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE LevFargKod AS LOGICAL INITIAL no 
     LABEL "Lev.fargekode" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE LevKod AS LOGICAL INITIAL no 
     LABEL "Lev.kode" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE LevNr AS LOGICAL INITIAL no 
     LABEL "Leverandør" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE LinkVareNr AS LOGICAL INITIAL no 
     LABEL "Linkvarenr (Pantvare)" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .81 NO-UNDO.

DEFINE VARIABLE LokPris AS LOGICAL INITIAL no 
     LABEL "Lokal pris" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE Mengde AS LOGICAL INITIAL no 
     LABEL "Mengde i salgsenhet" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE Mva% AS LOGICAL INITIAL no 
     LABEL "Mva%" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE NyStrekkode AS LOGICAL INITIAL no 
     LABEL "Legg til nye strekkoder" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE Pris AS LOGICAL INITIAL no 
     LABEL "Markedspris (anbefalt pris)" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE ProdNr AS LOGICAL INITIAL no 
     LABEL "Produsent" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE RAvdNr AS LOGICAL INITIAL no 
     LABEL "Vareområde" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE Salgsenhet AS LOGICAL INITIAL no 
     LABEL "Salgsenhet" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .81 NO-UNDO.

DEFINE VARIABLE Sasong AS LOGICAL INITIAL no 
     LABEL "Sesong" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE supRab% AS LOGICAL INITIAL no 
     LABEL "Rab.sup" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE tbChooseAll AS LOGICAL INITIAL no 
     LABEL "Velg alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE Vg AS LOGICAL INITIAL no 
     LABEL "Varegruppe" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE VmId AS LOGICAL INITIAL no 
     LABEL "Varemerke" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE VPIBildekode AS LOGICAL INITIAL no 
     LABEL "Bildereferanse" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     tbChooseAll AT ROW 1.48 COL 8
     LevKod AT ROW 3.14 COL 8
     LinkVareNr AT ROW 3.14 COL 42
     KjedeVare AT ROW 3.14 COL 82
     Beskr AT ROW 3.86 COL 8
     EkstStrTypeNavn AT ROW 3.86 COL 42
     Gjennomfaktureres AT ROW 3.86 COL 82
     LevFargKod AT ROW 4.57 COL 8
     AntIPkn AT ROW 4.57 COL 42
     LokPris AT ROW 4.62 COL 82
     LevNr AT ROW 5.29 COL 8
     Salgsenhet AT ROW 5.29 COL 42
     KjedeRab% AT ROW 5.29 COL 82
     ProdNr AT ROW 6 COL 8
     Etikettekst1 AT ROW 6 COL 42
     KjedeSupRab% AT ROW 6.1 COL 82
     VmId AT ROW 6.71 COL 8
     BongTekst AT ROW 6.71 COL 42
     Sasong AT ROW 7.43 COL 8 HELP
          "Denne varen skal gjennomfaktureres via kjedekontor."
     Anonseartikkel AT ROW 7.43 COL 42
     LevDato1 AT ROW 7.43 COL 82
     Vg AT ROW 8.14 COL 8
     Grunnsortiment AT ROW 8.14 COL 42
     InnkjopsPris AT ROW 8.14 COL 82
     VPIBildekode AT ROW 8.86 COL 8
     supRab% AT ROW 8.86 COL 82
     RAvdNr AT ROW 9.57 COL 8
     forhRab% AT ROW 9.57 COL 82
     Mengde AT ROW 10.29 COL 8
     AnbefaltPris AT ROW 10.29 COL 82
     JamforEnhet AT ROW 11 COL 8
     Pris AT ROW 11 COL 82
     KInnkjopspris AT ROW 12.67 COL 8
     KFrakt% AT ROW 12.67 COL 42
     KDivKost% AT ROW 13.38 COL 8
     Mva% AT ROW 13.38 COL 42
     KRab1% AT ROW 14.1 COL 8
     KPris AT ROW 14.1 COL 42
     KRab2% AT ROW 14.81 COL 8
     KRab3% AT ROW 15.52 COL 8
     NyStrekkode AT ROW 17.81 COL 8
     KorrStrekkode AT ROW 18.57 COL 8
     Btn_OK AT ROW 20.29 COL 102
     Btn_Cancel AT ROW 20.29 COL 117
     FILL-IN-22 AT ROW 2.43 COL 3 COLON-ALIGNED NO-LABEL
     FILL-IN-23 AT ROW 2.43 COL 80 COLON-ALIGNED NO-LABEL
     FILL-IN-24 AT ROW 12 COL 3 COLON-ALIGNED NO-LABEL
     FILL-IN-25 AT ROW 17.05 COL 3 COLON-ALIGNED NO-LABEL
     RECT-3 AT ROW 2.91 COL 4
     RECT-4 AT ROW 17.43 COL 4
     RECT-56 AT ROW 12.43 COL 4
     RECT-72 AT ROW 2.91 COL 80
     SPACE(0.19) SKIP(9.70)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Velg felter som skal overføres til artikkelkort"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-22 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-23 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-24 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-25 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Velg felter som skal overføres til artikkelkort */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DEF VAR cLabelList AS CHAR NO-UNDO.
  hWidget = FRAME {&FRAME-NAME}:FIRST-CHILD:FIRST-CHILD.
  REPEAT WHILE VALID-HANDLE(hWidget):
    IF hWidget:TYPE = "toggle-box" AND hWidget:CHECKED THEN
      ASSIGN ocLabelList = ocLabelList + hWidget:LABEL + CHR(10)
             ocExceptList = ocExceptList + hWidget:NAME + "|".
    hWidget = hWidget:NEXT-SIBLING.
  END.
  ASSIGN ocExceptList = TRIM(ocExceptList,"|")
         oiReturn = 1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbChooseAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbChooseAll Dialog-Frame
ON VALUE-CHANGED OF tbChooseAll IN FRAME Dialog-Frame /* Velg alle */
DO:
  MultiSelectTb(SELF:CHECKED).
  SELF:LABEL = IF SELF:CHECKED THEN 'Fjern all merking' ELSE 'Merk alle'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{incl/frametrigg.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  hParent = SOURCE-PROCEDURE.
  bHidden = DYNAMIC-FUNCTION("HideVPIfieldSelect" IN hParent) NO-ERROR.

  IF bHidden THEN DO:
    tbChooseAll:CHECKED = YES.
    APPLY "VALUE-CHANGED" TO tbChooseAll.
    APPLY "CHOOSE" TO Btn_OK.
  END.
  ELSE DO:
    RUN InitWindow.
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
  END.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY tbChooseAll LevKod LinkVareNr KjedeVare Beskr EkstStrTypeNavn 
          Gjennomfaktureres LevFargKod AntIPkn LokPris LevNr Salgsenhet 
          KjedeRab% ProdNr Etikettekst1 KjedeSupRab% VmId BongTekst Sasong 
          Anonseartikkel LevDato1 Vg Grunnsortiment InnkjopsPris VPIBildekode 
          supRab% RAvdNr forhRab% Mengde AnbefaltPris JamforEnhet Pris 
          KInnkjopspris KFrakt% KDivKost% Mva% KRab1% KPris KRab2% KRab3% 
          NyStrekkode KorrStrekkode FILL-IN-22 FILL-IN-23 FILL-IN-24 FILL-IN-25 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-3 RECT-4 RECT-56 RECT-72 tbChooseAll LevKod LinkVareNr KjedeVare 
         Beskr EkstStrTypeNavn Gjennomfaktureres LevFargKod AntIPkn LokPris 
         LevNr Salgsenhet KjedeRab% ProdNr Etikettekst1 KjedeSupRab% VmId 
         BongTekst Sasong Anonseartikkel LevDato1 Vg Grunnsortiment 
         InnkjopsPris VPIBildekode supRab% RAvdNr forhRab% Mengde AnbefaltPris 
         JamforEnhet Pris KInnkjopspris KFrakt% KDivKost% Mva% KRab1% KPris 
         KRab2% KRab3% NyStrekkode KorrStrekkode Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow Dialog-Frame 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
END.
  
DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MultiSelectTb Dialog-Frame 
FUNCTION MultiSelectTb RETURNS LOGICAL
  ( INPUT ibChecked AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hWidget = FRAME {&FRAME-NAME}:FIRST-CHILD:FIRST-CHILD.
REPEAT WHILE VALID-HANDLE(hWidget):
  /*Unntak....*/
  /*IF NOT CAN-DO('NyStrekkode,KorrStrekkode',hWidget:NAME) THEN */
  IF hWidget:TYPE = "toggle-box" AND NOT hWidget:HIDDEN THEN
      hWidget:CHECKED = ibChecked.
  hWidget = hWidget:NEXT-SIBLING.
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

