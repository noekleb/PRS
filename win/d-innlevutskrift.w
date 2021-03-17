&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEF VAR wDirekteLev         as log NO-UNDO.
  DEF VAR wTvFolgeseddel      AS log NO-UNDO.
  def var wEtiketter          as log no-undo.
  def var wFolgeseddler       as log no-undo.
  def var wInnleveranse       as log no-undo.
  DEF VAR cPrinterValg        AS CHAR NO-UNDO.

  wDirekteLev = false.
&ELSE
  DEF INPUT  PARAMETER wDirekteLev         as log NO-UNDO.
  DEF OUTPUT PARAMETER wTvFolgeseddel      AS log NO-UNDO.
  def INPUT-OUTPUT PARAMETER wEtiketter          as log no-undo.
  def output PARAMETER wFolgeseddler       as log no-undo.
  def output PARAMETER wInnleveranse       as log no-undo.
  DEF OUTPUT PARAMETER cPrinterValg AS CHARACTER  NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */
def var wTekst              as char no-undo.
def var wOk                 as char initial "AVBRYT" no-undo.
DEF VAR wSkrivFolgeseddel   AS LOG  NO-UNDO.

DEFINE VARIABLE cStartEtiPrinter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLAYOUT  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSKRIVER AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-50 T-Etiketter CB-Printer FI-StartEti ~
T-Innleveranse Btn_OK Btn_Cancel FI-Tekst1 
&Scoped-Define DISPLAYED-OBJECTS T-TvungenFolgeseddel T-Etiketter ~
CB-Printer FI-StartEti T-Innleveranse FI-Tekst1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Ingen utskrift" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Utskrift" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE CB-Printer AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE FI-StartEti AS INTEGER FORMAT ">9":U INITIAL 1 
     LABEL "Start etikett (1-24)" 
     VIEW-AS FILL-IN 
     SIZE 5.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Tekst1 AS CHARACTER FORMAT "X(256)":U INITIAL "Utskrift av:" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 6.43.

DEFINE VARIABLE T-Etiketter AS LOGICAL INITIAL no 
     LABEL "&Etiketter" 
     VIEW-AS TOGGLE-BOX
     SIZE 57 BY .81 NO-UNDO.

DEFINE VARIABLE T-Folgeseddler AS LOGICAL INITIAL no 
     LABEL "&Innleveransrapport for butikkene" 
     VIEW-AS TOGGLE-BOX
     SIZE 50 BY .81 NO-UNDO.

DEFINE VARIABLE T-Innleveranse AS LOGICAL INITIAL yes 
     LABEL "&Innleveranserapport" 
     VIEW-AS TOGGLE-BOX
     SIZE 56 BY .81 NO-UNDO.

DEFINE VARIABLE T-TvungenFolgeseddel AS LOGICAL INITIAL yes 
     LABEL "Følgeseddel innleveranse" 
     VIEW-AS TOGGLE-BOX
     SIZE 57 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     T-TvungenFolgeseddel AT ROW 2.91 COL 3.4
     T-Etiketter AT ROW 3.95 COL 3.4
     CB-Printer AT ROW 5.05 COL 5 COLON-ALIGNED NO-LABEL
     FI-StartEti AT ROW 5.05 COL 55 COLON-ALIGNED
     T-Innleveranse AT ROW 6.48 COL 3.4
     T-Folgeseddler AT ROW 7.43 COL 7.4
     Btn_OK AT ROW 9.1 COL 2
     Btn_Cancel AT ROW 9.1 COL 49
     FI-Tekst1 AT ROW 1.71 COL 2 COLON-ALIGNED NO-LABEL
     RECT-50 AT ROW 2.43 COL 2
     SPACE(1.19) SKIP(1.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Utskrift"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX T-Folgeseddler IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       T-Folgeseddler:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR TOGGLE-BOX T-TvungenFolgeseddel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Utskrift */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Ingen utskrift */
DO:
  wEtiketter = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Utskrift */
DO:
  assign
    T-Etiketter 
    T-Folgeseddler 
    T-Innleveranse
    T-TvungenFolgeseddel
    wEtiketter          = T-Etiketter 
    wFolgeSeddler       = T-Folgeseddler 
    wInnleveranse       = T-Innleveranse
    wTvFolgeseddel      = T-TvungenFolgeseddel
    wOK                 = "OK"
    cPrinterValg        = CB-Printer:SCREEN-VALUE + CHR(1) + 
                      (IF INPUT FI-StartEti < 1 OR INPUT FI-StartEti > 24 THEN "1" ELSE FI-StartEti:SCREEN-VALUE).
   RUN SaveSettings.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Printer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Printer Dialog-Frame
ON VALUE-CHANGED OF CB-Printer IN FRAME Dialog-Frame
DO:
  ASSIGN FI-StartEti:SENSITIVE = CAN-DO(cStartEtiPrinter,SELF:SCREEN-VALUE).
  IF FI-StartEti:SENSITIVE THEN
      APPLY "ENTRY" TO FI-StartEti.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Etiketter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Etiketter Dialog-Frame
ON VALUE-CHANGED OF T-Etiketter IN FRAME Dialog-Frame /* Etiketter */
DO:
  ASSIGN CB-Printer:SENSITIVE  = SELF:CHECKED
         FI-StartEti:SENSITIVE = SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Innleveranse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Innleveranse Dialog-Frame
ON VALUE-CHANGED OF T-Innleveranse IN FRAME Dialog-Frame /* Innleveranserapport */
DO:
  assign
    T-Innleveranse.
/*   if T-Innleveranse                       */
/*     then T-FolgeSeddler:sensitive = true. */
/*   else T-FolgeSeddler:sensitive = false.  */
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Skal følgeseddel ved innleveranse skrives ut. */
{syspara.i 5 4 12 wTekst}
IF trim(wTekst) = "1" THEN
    wSkrivFolgeseddel = TRUE.
ELSE
    wSkrivFolgeseddel = FALSE.

{syspara.i 5 4 5 wTekst}
if can-do("JA,Yes,True",wTekst) AND wEtiketter = TRUE THEN /* 16/1-03 */
  T-Etiketter = true.
/* if can-do("JA,Yes,True",wTekst) then */
/*   T-Etiketter = true.                */
/* {syspara.i 5 4 6 wTekst}             */
/* if can-do("JA,Yes,True",wTekst) then */
/*   T-Innleveranse = true.             */
/* {syspara.i 5 4 7 wTekst}             */
/* if can-do("JA,Yes,True",wTekst) then */
/*   T-Folgeseddler = true.             */
  
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN GetLastPrinter.
    RUN InitCombo.
  RUN enable_UI.
  APPLY "VALUE-CHANGED" TO CB-Printer.
  ASSIGN T-Etiketter:SENSITIVE = wEtiketter /* 16/1-03 */
         CB-Printer:SENSITIVE = wEtiketter.
/*   APPLY "VALUE-CHANGED" TO T-Etiketter. */
/*   IF wSkrivFolgeseddel THEN                */
/*       T-TvungenFolgeseddel:HIDDEN = false. */
/*   ELSE                                     */
T-TvungenFolgeseddel:HIDDEN = TRUE.

/*   if T-Innleveranse then */
/*     assign                             */
/*       T-FolgeSeddler:sensitive = true. */
  
  view frame Dialog-Frame.
/*   display                  */
/*     T-FolgeSeddler         */
/*   with frame Dialog-Frame. */
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
return wOk.

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
  DISPLAY T-TvungenFolgeseddel T-Etiketter CB-Printer FI-StartEti T-Innleveranse 
          FI-Tekst1 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-50 T-Etiketter CB-Printer FI-StartEti T-Innleveranse Btn_OK 
         Btn_Cancel FI-Tekst1 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetLastPrinter Dialog-Frame 
PROCEDURE GetLastPrinter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(wLibHandle) THEN
  DO WITH FRAME default-frame:
    RUN HentParametre IN wLibHandle ("ETIKETTER", "LAYOUT",  OUTPUT cLAYOUT).
    RUN HentParametre IN wLibHandle ("ETIKETTER", "SKRIVER", OUTPUT cSKRIVER).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCombo Dialog-Frame 
PROCEDURE InitCombo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bSysPara FOR SysPara.
    ASSIGN cListItemPairs = "".
    FOR EACH SysPara WHERE SysPara.SysHId = 5 AND 
                       SysPara.SysGr = 21 NO-LOCK:
        IF CAN-DO(REPLACE(SysPara.Parameter2,";",","),"AVAIL") THEN DO:
            FIND bSysPara WHERE bSysPara.SysHId = 5 AND 
                                bSysPara.SysGr  = 20 AND
                                bSysPara.ParaNr = SysPara.ParaNr NO-LOCK NO-ERROR.
            IF AVAIL bSysPara THEN DO:
                ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") +
                    (IF bSysPara.Parameter1 = "dummy" THEN bSysPara.Parameter2 ELSE bSysPara.Parameter1) + "," + TRIM(STRING(bSysPara.ParaNr)).
/*                        bSysPara.Parameter2 + "," + TRIM(STRING(bSysPara.ParaNr)). */
                IF CAN-DO(REPLACE(SysPara.Parameter2,";",","),"START") THEN
                    ASSIGN cStartEtiPrinter = cStartEtiPrinter + (IF cStartEtiPrinter = "" THEN "" ELSE ",") + 
                       TRIM(STRING(bSysPara.ParaNr)).
            END.
        END.
    END.
    ASSIGN CB-Printer:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME}= cListItemPairs.
    IF cSKRIVER <> ? AND CAN-DO(cListItemPairs,cSKRIVER) THEN
        CB-Printer = INT(cSKRIVER).
    ELSE
        CB-Printer = INT(ENTRY(2,cListItemPairs)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveSettings Dialog-Frame 
PROCEDURE SaveSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(wLibHandle) THEN
  DO WITH FRAME {&FRAME-NAME}:
    RUN LagreParametre IN wLibHandle ("ETIKETTER", "LAYOUT", ENTRY(LOOKUP(CB-Printer:SCREEN-VALUE,cListItemPairs) - 1,CB-Printer:LIST-ITEM-PAIRS)).
    RUN LagreParametre IN wLibHandle ("ETIKETTER", "SKRIVER", CB-Printer:SCREEN-VALUE).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

