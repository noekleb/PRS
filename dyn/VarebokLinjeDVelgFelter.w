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
&Scoped-Define ENABLED-OBJECTS btnFjernAlle Beskr InnkjopsPris LevFargKod ~
AnbefaltPris LevKod Pris ProdNr Mva% forhRab% supRab% KjedeInnkPris ~
KjedeRab% LevDato1 KjedeVare Gjennomfaktureres Sasong Btn_OK Btn_Cancel ~
btnVelgAlle 
&Scoped-Define DISPLAYED-OBJECTS Beskr InnkjopsPris LevFargKod AnbefaltPris ~
LevKod Pris ProdNr Mva% forhRab% supRab% KjedeInnkPris KjedeRab% LevDato1 ~
KjedeVare Gjennomfaktureres Sasong FI-Tekst-2 

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
DEFINE BUTTON btnFjernAlle  NO-FOCUS FLAT-BUTTON
     LABEL "Fjern alle" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnVelgAlle  NO-FOCUS FLAT-BUTTON
     LABEL "Velg alle" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-Tekst-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Felt som alltid oppdateres:" 
      VIEW-AS TEXT 
     SIZE 61 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE AnbefaltPris AS LOGICAL INITIAL no 
     LABEL "Veiledende (anbefalt) pris" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE Beskr AS LOGICAL INITIAL no 
     LABEL "Art.navn" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE forhRab% AS LOGICAL INITIAL no 
     LABEL "Rab.forh" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE Gjennomfaktureres AS LOGICAL INITIAL no 
     LABEL "Gj.fakt" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE InnkjopsPris AS LOGICAL INITIAL no 
     LABEL "Engros" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE KjedeInnkPris AS LOGICAL INITIAL no 
     LABEL "Kj.ink.pr" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE KjedeRab% AS LOGICAL INITIAL no 
     LABEL "Kj.bonus" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE KjedeVare AS LOGICAL INITIAL no 
     LABEL "Kjedelevert" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE LevDato1 AS LOGICAL INITIAL no 
     LABEL "LevUke (4 felter)" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE LevFargKod AS LOGICAL INITIAL no 
     LABEL "Lev.fargekode" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE LevKod AS LOGICAL INITIAL no 
     LABEL "Leverandørs modellnr (Lev. art.nr)" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE Mva% AS LOGICAL INITIAL no 
     LABEL "Mva%" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE Pris AS LOGICAL INITIAL no 
     LABEL "Markedspris (hentes fra anbefalt pris)" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81 NO-UNDO.

DEFINE VARIABLE ProdNr AS LOGICAL INITIAL no 
     LABEL "Produsent" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY .81 NO-UNDO.

DEFINE VARIABLE Sasong AS LOGICAL INITIAL no 
     LABEL "Sesong" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE supRab% AS LOGICAL INITIAL no 
     LABEL "Rab.sup" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnFjernAlle AT ROW 1.24 COL 17
     Beskr AT ROW 3.48 COL 10
     InnkjopsPris AT ROW 3.62 COL 62
     LevFargKod AT ROW 4.52 COL 10
     AnbefaltPris AT ROW 4.71 COL 62
     LevKod AT ROW 5.52 COL 10
     Pris AT ROW 5.76 COL 62
     ProdNr AT ROW 6.48 COL 10
     Mva% AT ROW 6.81 COL 62
     forhRab% AT ROW 7.91 COL 62
     supRab% AT ROW 8.95 COL 62
     KjedeInnkPris AT ROW 10 COL 62
     KjedeRab% AT ROW 11.05 COL 62
     LevDato1 AT ROW 12.14 COL 62
     KjedeVare AT ROW 13.19 COL 62 HELP
          "Varen forhandles via kjeden."
     Gjennomfaktureres AT ROW 14.24 COL 62 HELP
          "Denne varen skal gjennomfaktureres via kjedekontor."
     Sasong AT ROW 15.33 COL 62 HELP
          "Denne varen skal gjennomfaktureres via kjedekontor."
     Btn_OK AT ROW 20.76 COL 43
     Btn_Cancel AT ROW 20.76 COL 58
     btnVelgAlle AT ROW 1.24 COL 2
     FI-Tekst-2 AT ROW 18.38 COL 8 COLON-ALIGNED NO-LABEL
     "Leverandør, Avdeling, hovedgruppe og varegruppe" VIEW-AS TEXT
          SIZE 88 BY .62 AT ROW 19.57 COL 10
     SPACE(13.99) SKIP(1.90)
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

/* SETTINGS FOR FILL-IN FI-Tekst-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FI-Tekst-2:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

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


&Scoped-define SELF-NAME btnFjernAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFjernAlle Dialog-Frame
ON CHOOSE OF btnFjernAlle IN FRAME Dialog-Frame /* Fjern alle */
DO:
  MultiSelectTb(NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnVelgAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVelgAlle Dialog-Frame
ON CHOOSE OF btnVelgAlle IN FRAME Dialog-Frame /* Velg alle */
DO:
  MultiSelectTb(YES).
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
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
  DISPLAY Beskr InnkjopsPris LevFargKod AnbefaltPris LevKod Pris ProdNr Mva% 
          forhRab% supRab% KjedeInnkPris KjedeRab% LevDato1 KjedeVare 
          Gjennomfaktureres Sasong FI-Tekst-2 
      WITH FRAME Dialog-Frame.
  ENABLE btnFjernAlle Beskr InnkjopsPris LevFargKod AnbefaltPris LevKod Pris 
         ProdNr Mva% forhRab% supRab% KjedeInnkPris KjedeRab% LevDato1 
         KjedeVare Gjennomfaktureres Sasong Btn_OK Btn_Cancel btnVelgAlle 
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
  IF hWidget:TYPE = "toggle-box" AND NOT hWidget:HIDDEN THEN
    hWidget:CHECKED = ibChecked.
  hWidget = hWidget:NEXT-SIBLING.
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

