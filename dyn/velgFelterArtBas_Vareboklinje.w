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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 RECT-4 tbChooseAll LevKod ~
InnkjopsPris KjedeRab% Beskr forhRab% KjedeSupRab% LevFargKod ~
KjedeValutaPris supRab% LevNr KjedeProdusent AnbefaltPris ProdNr ~
MerknadsKoder Pris Sasong LevDato1 Mva% Vg Gjennomfaktureres KjedeVare ~
RAvdNr AntIPkn VPIBildekode NyStrekkode KorrStrekkode Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS tbChooseAll LevKod InnkjopsPris KjedeRab% ~
Beskr forhRab% KjedeSupRab% LevFargKod KjedeValutaPris supRab% LevNr ~
KjedeProdusent AnbefaltPris ProdNr MerknadsKoder Pris Sasong LevDato1 Mva% ~
Vg Gjennomfaktureres KjedeVare RAvdNr AntIPkn VPIBildekode NyStrekkode ~
KorrStrekkode 

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

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 103 BY 10.71.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 103 BY 2.62.

DEFINE VARIABLE AnbefaltPris AS LOGICAL INITIAL no 
     LABEL "Veiledende (anbefalt) pris" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE AntIPkn AS LOGICAL INITIAL no 
     LABEL "Antall i pakkning" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.

DEFINE VARIABLE Beskr AS LOGICAL INITIAL no 
     LABEL "Art.navn" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE forhRab% AS LOGICAL INITIAL no 
     LABEL "Rab.forh" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE Gjennomfaktureres AS LOGICAL INITIAL no 
     LABEL "Gjennomfaktureres" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE InnkjopsPris AS LOGICAL INITIAL no 
     LABEL "Engros" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE KjedeProdusent AS LOGICAL INITIAL no 
     LABEL "Kjedeprodusent" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.

DEFINE VARIABLE KjedeRab% AS LOGICAL INITIAL no 
     LABEL "Kjede rabatt" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.

DEFINE VARIABLE KjedeSupRab% AS LOGICAL INITIAL no 
     LABEL "Kjede sup.rabatt" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.

DEFINE VARIABLE KjedeValutaPris AS LOGICAL INITIAL no 
     LABEL "Kjedevalutapris" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.

DEFINE VARIABLE KjedeVare AS LOGICAL INITIAL no 
     LABEL "Kjedelevert" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE KorrStrekkode AS LOGICAL INITIAL no 
     LABEL "Korriger strekkoder som ligger feil" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE LevDato1 AS LOGICAL INITIAL no 
     LABEL "LevUke (4 felter)" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE LevFargKod AS LOGICAL INITIAL no 
     LABEL "Lev.fargekode" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE LevKod AS LOGICAL INITIAL no 
     LABEL "Lev.kode" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE LevNr AS LOGICAL INITIAL no 
     LABEL "Leverandør" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE MerknadsKoder AS LOGICAL INITIAL no 
     LABEL "Merknadskoder (4 felt)" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 TOOLTIP "Sortiment, lagerkoder, kampanjeuke og kampanjestøtte." NO-UNDO.

DEFINE VARIABLE Mva% AS LOGICAL INITIAL no 
     LABEL "Mva%" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE NyStrekkode AS LOGICAL INITIAL no 
     LABEL "Legg til nye strekkoder" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE Pris AS LOGICAL INITIAL no 
     LABEL "Markedspris (hentes fra anbefalt pris)" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81 NO-UNDO.

DEFINE VARIABLE ProdNr AS LOGICAL INITIAL no 
     LABEL "Produsent" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE RAvdNr AS LOGICAL INITIAL no 
     LABEL "Vareområde" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE Sasong AS LOGICAL INITIAL no 
     LABEL "Sesong" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE supRab% AS LOGICAL INITIAL no 
     LABEL "Rab.sup" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE tbChooseAll AS LOGICAL INITIAL no 
     LABEL "Velg alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE Vg AS LOGICAL INITIAL no 
     LABEL "Varegruppe" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE VPIBildekode AS LOGICAL INITIAL no 
     LABEL "Bildereferanse" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     tbChooseAll AT ROW 1.95 COL 8
     LevKod AT ROW 3.38 COL 8
     InnkjopsPris AT ROW 3.38 COL 35.2
     KjedeRab% AT ROW 3.38 COL 78
     Beskr AT ROW 4.24 COL 8
     forhRab% AT ROW 4.29 COL 35.2
     KjedeSupRab% AT ROW 4.29 COL 78
     LevFargKod AT ROW 5.1 COL 8
     KjedeValutaPris AT ROW 5.14 COL 78
     supRab% AT ROW 5.24 COL 35.2
     LevNr AT ROW 5.91 COL 8
     KjedeProdusent AT ROW 6 COL 78
     AnbefaltPris AT ROW 6.1 COL 35.2
     ProdNr AT ROW 6.71 COL 8
     MerknadsKoder AT ROW 6.95 COL 78
     Pris AT ROW 7 COL 35.2
     Sasong AT ROW 7.67 COL 8 HELP
          "Denne varen skal gjennomfaktureres via kjedekontor."
     LevDato1 AT ROW 7.81 COL 78
     Mva% AT ROW 7.86 COL 35.2
     Vg AT ROW 8.43 COL 8
     Gjennomfaktureres AT ROW 8.76 COL 78
     KjedeVare AT ROW 9.71 COL 78
     RAvdNr AT ROW 10.48 COL 8
     AntIPkn AT ROW 11.43 COL 8
     VPIBildekode AT ROW 12.38 COL 8
     NyStrekkode AT ROW 14 COL 8
     KorrStrekkode AT ROW 14.95 COL 8
     Btn_OK AT ROW 16.71 COL 77.2
     Btn_Cancel AT ROW 16.71 COL 92.2
     "Artikkelfelt" VIEW-AS TEXT
          SIZE 26 BY .86 AT ROW 9.57 COL 8
          FONT 6
     RECT-3 AT ROW 2.91 COL 4
     RECT-4 AT ROW 13.62 COL 4
     SPACE(0.79) SKIP(1.70)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Velg felter som skal overføres fra artikkelkort"
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Velg felter som skal overføres fra artikkelkort */
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
  RUN InitWindow.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
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
  DISPLAY tbChooseAll LevKod InnkjopsPris KjedeRab% Beskr forhRab% KjedeSupRab% 
          LevFargKod KjedeValutaPris supRab% LevNr KjedeProdusent AnbefaltPris 
          ProdNr MerknadsKoder Pris Sasong LevDato1 Mva% Vg Gjennomfaktureres 
          KjedeVare RAvdNr AntIPkn VPIBildekode NyStrekkode KorrStrekkode 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-3 RECT-4 tbChooseAll LevKod InnkjopsPris KjedeRab% Beskr forhRab% 
         KjedeSupRab% LevFargKod KjedeValutaPris supRab% LevNr KjedeProdusent 
         AnbefaltPris ProdNr MerknadsKoder Pris Sasong LevDato1 Mva% Vg 
         Gjennomfaktureres KjedeVare RAvdNr AntIPkn VPIBildekode NyStrekkode 
         KorrStrekkode Btn_OK Btn_Cancel 
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

