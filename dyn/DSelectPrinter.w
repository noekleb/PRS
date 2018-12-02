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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT-OUTPUT PARAM iocPrinter  AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM ioiAntEks   AS INT  NO-UNDO.
DEF INPUT-OUTPUT PARAM ioiFormat   AS INT  NO-UNDO.
DEF INPUT        PARAM icButFields AS CHAR NO-UNDO.
DEF OUTPUT       PARAM obOk        AS LOG  NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cbPrinter cbFormat fiAntEks Btn_OK ~
Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS cbPrinter cbFormat fiAntEks 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.15.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.15.

DEFINE VARIABLE cbFormat AS CHARACTER FORMAT "X(256)":U 
     LABEL "Format" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE cbPrinter AS CHARACTER FORMAT "X(256)":U 
     LABEL "Skriver" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE fiAntEks AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Kopier" 
     VIEW-AS FILL-IN 
     SIZE 5.43 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     cbPrinter AT ROW 2.12 COL 9 COLON-ALIGNED
     cbFormat AT ROW 3.31 COL 9 COLON-ALIGNED
     fiAntEks AT ROW 3.31 COL 56.14 COLON-ALIGNED
     Btn_OK AT ROW 5.69 COL 33
     Btn_Cancel AT ROW 5.69 COL 49
     SPACE(1.19) SKIP(0.29)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Velg".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Velg */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  IF cbPrinter:SCREEN-VALUE NE ? THEN DO:
    IF NOT fiAntEks:HIDDEN AND INT(fiAntEks:SCREEN-VALUE) = 0 THEN RETURN NO-APPLY.
    ASSIGN iocPrinter = cbPrinter:SCREEN-VALUE
           ioiAntEks  = IF fiAntEks:HIDDEN THEN ioiAntEks ELSE INT(fiAntEks:SCREEN-VALUE)
           ioiFormat  = IF cbFormat:HIDDEN THEN ioiFormat ELSE INT(cbFormat:SCREEN-VALUE)
           obOk       = TRUE.
  END.
  ELSE RETURN NO-APPLY.
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
  DISPLAY cbPrinter cbFormat fiAntEks 
      WITH FRAME Dialog-Frame.
  ENABLE cbPrinter cbFormat fiAntEks Btn_OK Btn_Cancel 
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
DEF VAR cButikkNr  AS CHAR  NO-UNDO.
DEF VAR cPrintInfo AS CHAR  NO-UNDO.
DEF VAR cButFormat AS CHAR NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cbPrinter:LIST-ITEMS = SESSION:GET-PRINTERS()
         cbFormat:DELIMITER   = "|"
         .
  IF ioiFormat = 0 THEN
    cbFormat:HIDDEN = TRUE.

  IF icButFields = "" THEN DO:
    IF ioiAntEks = 0 THEN 
      ASSIGN fiAntEks:HIDDEN = TRUE
             ioiAntEks       = 1
             .
    ELSE fiAntEks:SCREEN-VALUE = STRING(ioiAntEks).
  END.
  ELSE DO:
    cButikkNr = DYNAMIC-FUNCTION("getFieldValues","Bruker",
                                 "WHERE BrukerID = '" + DYNAMIC-FUNCTION("getASuserId") + "'",
                                 "ButikkNr").
    IF cButikkNr = ? OR INT(cButikkNr) = 0 THEN 
      cButikkNr = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                                 "WHERE SysHId = '5' AND SysGr = '1' AND ParaNr = '1'",
                                 "Parameter1").
    IF INT(cButikkNr) > 0 THEN DO:
      cPrintInfo = DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE Butik = " + cButikkNr,icButFields).
      IF NUM-ENTRIES(icButFields) = 2 THEN 
        fiAntEks:SCREEN-VALUE = ENTRY(2,cPrintInfo,"|").
      ELSE ASSIGN fiAntEks:HIDDEN = TRUE
                  ioiAntEks       = 1.              
      IF CAN-DO(cbPrinter:LIST-ITEMS,ENTRY(1,cPrintInfo,"|")) THEN
        cbPrinter:SCREEN-VALUE = ENTRY(1,cPrintInfo,"|").
      ELSE
          cbPrinter:SCREEN-VALUE = SESSION:PRINTER-NAME.
    END.
    cButFormat = DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE Butik = " + cButikkNr,"FakturaLayout").
    cbFormat:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                "SysPara;Beskrivelse;ParaNr",
                                                "WHERE SysHId = '19' AND SysGr = '12'").
    cbFormat:SCREEN-VALUE = cButFormat NO-ERROR.
/*     cbFormat:SCREEN-VALUE = cbFormat:ENTRY(1) NO-ERROR. */
  END.

END.

DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

