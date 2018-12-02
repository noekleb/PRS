&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
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
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEF VAR iBestNr      LIKE BestHode.Bestnr INIT 17410007 NO-UNDO.
  DEF VAR iLeveringsNr LIKE BestHLev.LeveringsNr          NO-UNDO.
  DEF VAR cPrinterValg AS CHARACTER                       NO-UNDO.
&ELSE
  DEF INPUT  PARAMETER iBestNr      LIKE BestHode.Bestnr      NO-UNDO.
  DEF OUTPUT PARAMETER iLeveringsNr LIKE BestHLev.LeveringsNr NO-UNDO.
  DEF OUTPUT PARAMETER cPrinterValg AS CHARACTER              NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

  DEFINE VARIABLE cStartEtiPrinter AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLAYOUT  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSKRIVER AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.

  {runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES BestHLev

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 BestHLev.LeveringsNr ~
BestHLev.LevertAv BestHLev.LevertDato STRING(Levtidspunkt,"HH:MM:SS") 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH BestHLev ~
      WHERE BestHLev.BestNr = iBestNr NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH BestHLev ~
      WHERE BestHLev.BestNr = iBestNr NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 BestHLev
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 BestHLev


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CB-Printer FI-StartEti BROWSE-2 Btn_OK ~
Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS CB-Printer FI-StartEti 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
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

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      BestHLev SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 Dialog-Frame _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      BestHLev.LeveringsNr FORMAT ">>>>>>9":U
      BestHLev.LevertAv FORMAT "X(15)":U
      BestHLev.LevertDato FORMAT "99/99/9999":U
      STRING(Levtidspunkt,"HH:MM:SS") COLUMN-LABEL "Tid" FORMAT "x(10)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 63 BY 8.33 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     CB-Printer AT ROW 1.62 COL 2.8 NO-LABEL
     FI-StartEti AT ROW 1.62 COL 50.8 COLON-ALIGNED
     BROWSE-2 AT ROW 3.1 COL 2
     Btn_OK AT ROW 3.14 COL 66
     Btn_Cancel AT ROW 4.38 COL 66
     Btn_Help AT ROW 6.38 COL 66
     SPACE(0.59) SKIP(4.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Velg innleveranse / skriver"
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
/* BROWSE-TAB BROWSE-2 FI-StartEti Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX CB-Printer IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "SkoTex.BestHLev"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "BestHLev.BestNr = iBestNr"
     _FldNameList[1]   = SkoTex.BestHLev.LeveringsNr
     _FldNameList[2]   = SkoTex.BestHLev.LevertAv
     _FldNameList[3]   = SkoTex.BestHLev.LevertDato
     _FldNameList[4]   > "_<CALC>"
"STRING(Levtidspunkt,""HH:MM:SS"")" "Tid" "x(10)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Velg innleveranse / skriver */
DO:
  ASSIGN iLeveringsNr = BestHLev.LeveringsNr
         cPrinterValg = CB-Printer:SCREEN-VALUE + CHR(1) + 
                    (IF INPUT FI-StartEti < 1 OR INPUT FI-StartEti > 24 THEN "1" ELSE FI-StartEti:SCREEN-VALUE).
 RUN SaveSettings.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Velg innleveranse / skriver */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
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


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN GetLastPrinter.
  RUN InitCombo.
  RUN enable_UI.
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
  DISPLAY CB-Printer FI-StartEti 
      WITH FRAME Dialog-Frame.
  ENABLE CB-Printer FI-StartEti BROWSE-2 Btn_OK Btn_Cancel Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
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
                       /*^bSysPara.Parameter2 + "," + TRIM(STRING(bSysPara.ParaNr)).  */
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

