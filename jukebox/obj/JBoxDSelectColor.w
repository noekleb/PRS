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
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEF VAR ioiRGBColor AS INT  NO-UNDO.
  DEF VAR oiColorIdx  AS INT  NO-UNDO.
  DEF VAR icText      AS CHAR NO-UNDO.
  DEF VAR obOk        AS LOG  NO-UNDO.
&ELSE
  DEF INPUT        PARAM icText      AS CHAR NO-UNDO.
  DEF INPUT-OUTPUT PARAM ioiRGBColor AS INT  NO-UNDO.
  DEF OUTPUT       PARAM oiColorIdx  AS INT  NO-UNDO.
  DEF OUTPUT       PARAM obOK        AS LOG  NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEF VAR ix      AS INT NO-UNDO.
DEF VAR bOk     AS LOG NO-UNDO.
DEF VAR iOrgRGB AS INT NO-UNDO.
DEF VAR hParent AS HANDLE NO-UNDO.
DEF VAR cParentField AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnColor slRed slGreen slBlue fiSample ~
Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS slRed slGreen slBlue fiSample 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LocalTranslation Dialog-Frame 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setColor Dialog-Frame 
FUNCTION setColor RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnColor 
     IMAGE-UP FILE "bmp/color.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ändra" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fiSample AS CHARACTER FORMAT "X(256)":U INITIAL "Tekst-eksempel" 
     VIEW-AS FILL-IN 
     SIZE 45.2 BY 1 NO-UNDO.

DEFINE VARIABLE slBlue AS INTEGER INITIAL 255 
     VIEW-AS SLIDER MIN-VALUE 0 MAX-VALUE 255 HORIZONTAL LARGE-TO-SMALL 
     TIC-MARKS NONE 
     SIZE 54 BY 2.05 NO-UNDO.

DEFINE VARIABLE slGreen AS INTEGER INITIAL 255 
     VIEW-AS SLIDER MIN-VALUE 0 MAX-VALUE 255 HORIZONTAL LARGE-TO-SMALL 
     TIC-MARKS NONE 
     SIZE 54 BY 2.19 NO-UNDO.

DEFINE VARIABLE slRed AS INTEGER INITIAL 255 
     VIEW-AS SLIDER MIN-VALUE 0 MAX-VALUE 255 HORIZONTAL LARGE-TO-SMALL 
     TIC-MARKS NONE 
     SIZE 54 BY 2.05 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnColor AT ROW 8.43 COL 57
     slRed AT ROW 1.19 COL 12 NO-LABEL
     slGreen AT ROW 3.24 COL 12 NO-LABEL
     slBlue AT ROW 5.48 COL 12 NO-LABEL
     fiSample AT ROW 8.38 COL 9.8 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 10.29 COL 40.4
     Btn_Cancel AT ROW 10.29 COL 55.8
     SPACE(0.79) SKIP(0.23)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Velg bakgrunnsfarge".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Velg bakgrunnsfarge */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnColor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnColor Dialog-Frame
ON CHOOSE OF btnColor IN FRAME Dialog-Frame /* Ändra */
DO:
  DEFINE VAR wRGBchar AS CHAR NO-UNDO.

  SYSTEM-DIALOG COLOR ix.
  
  ASSIGN 
/*          wRGBchar = STRING(COLOR-TABLE:GET-RED-VALUE(ix)) + "," +   */
/*                     STRING(COLOR-TABLE:GET-GREEN-VALUE(ix)) + "," + */
/*                     STRING(COLOR-TABLE:GET-BLUE-VALUE(ix))          */
         slRed    = COLOR-TABLE:GET-RED-VALUE(ix)
         slGreen  = COLOR-TABLE:GET-GREEN-VALUE(ix)
         slBlue   = COLOR-TABLE:GET-BLUE-VALUE(ix)
                    .
  DISP slRed slGreen slBlue WITH FRAME {&FRAME-NAME}.
  setColor().

/*   IF wRGBchar <> tStatus.wRGBchar THEN DO:         */
/*       ASSIGN tStatus.wRGBchar = wRGBchar           */
/*              tStatus.wRGBint  = hex2int(wRGBchar). */
/*       BROWSE {&BROWSE-NAME}:REFRESH().             */
/*   END.                                             */
/*   APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:  
  ASSIGN obOK        = TRUE
         ioiRGBColor = RGB-VALUE(slRed,slGreen,slBlue)
         oiColorIdx  = ix.
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
     MESSAGE PROGRAM-NAME(1) SKIP
             ioiRGBColor SKIP
             oiColorIdx
             VIEW-AS ALERT-BOX.
  &ENDIF
  IF CAN-DO(hParent:INTERNAL-ENTRIES,"setRGBvalues") THEN DO:
    DYNAMIC-FUNCTION("setRGBvalues" IN hParent, STRING(slRed) +  "," + STRING(slGreen) + "," + STRING(slBlue),cParentField) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
      DYNAMIC-FUNCTION("setRGBvalues" IN hParent, STRING(slRed) +  "," + STRING(slGreen) + "," + STRING(slBlue)) NO-ERROR.
  END.
  APPLY "go" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME slBlue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL slBlue Dialog-Frame
ON VALUE-CHANGED OF slBlue IN FRAME Dialog-Frame
DO:
  setColor().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME slGreen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL slGreen Dialog-Frame
ON VALUE-CHANGED OF slGreen IN FRAME Dialog-Frame
DO:
  setColor().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME slRed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL slRed Dialog-Frame
ON VALUE-CHANGED OF slRed IN FRAME Dialog-Frame
DO:
  setColor().
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
  hParent =  SOURCE-PROCEDURE.
  DEF VAR fTest AS DEC NO-UNDO.
  fTest = DECIMAL(icText) NO-ERROR.

  IF ERROR-STATUS:ERROR AND CAN-DO(hParent:INTERNAL-ENTRIES,"getRgb") THEN
    ASSIGN cParentField = icText
           icText = DYNAMIC-FUNCTION("getRGB" IN hParent,cParentField).
  RUN InitWindow.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.


IF NOT obOK AND iOrgRGB NE 0 THEN
  COLOR-TABLE:SET-RGB-VALUE(ix,iOrgRGB).

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
  DISPLAY slRed slGreen slBlue fiSample 
      WITH FRAME Dialog-Frame.
  ENABLE btnColor slRed slGreen slBlue fiSample Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
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
  iOrgRGB = ioiRGBColor.

  IF ioiRGBColor NE 0 THEN DO ix = 0 TO COLOR-TABLE:NUM-ENTRIES:
    IF COLOR-TABLE:GET-RGB-VALUE(ix) = ioiRGBColor THEN DO:
      ASSIGN slRed   = COLOR-TABLE:GET-RED-VALUE(ix)
             slGreen = COLOR-TABLE:GET-GREEN-VALUE(ix)
             slBlue  = COLOR-TABLE:GET-BLUE-VALUE(ix)
             bOK     = TRUE
             NO-ERROR.
      DISP slRed slGreen slBlue.
      LEAVE.
    END.
  END.
  ELSE IF NUM-ENTRIES(icText) = 3 THEN
    ASSIGN slRed   = INT(ENTRY(1,icText))
           slGreen = INT(ENTRY(2,icText))
           slBlue  = INT(ENTRY(3,icText))
           bOk     = YES
           .

  IF NOT bOK THEN DO:
    ASSIGN ix = COLOR-TABLE:NUM-ENTRIES
           COLOR-TABLE:NUM-ENTRIES = ix + 1.
    COLOR-TABLE:SET-DYNAMIC(ix, yes).
  END.

  IF icText NE "" THEN
    fiSample:SCREEN-VALUE = icText.

  fiSample:BGCOLOR IN FRAME {&FRAME-NAME} = ix.
  setColor().

  LocalTranslation().

  DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LocalTranslation Dialog-Frame 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Set english labels
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF DYNAMIC-FUNCTION("Scandinavian") THEN RETURN FALSE.
  ELSE 
    ASSIGN Btn_Cancel:LABEL         = "Cancel" 
           fiSample:SCREEN-VALUE    = "Text sample" 
           FRAME Dialog-Frame:TITLE = "Select background color"
           .
  
  RETURN TRUE.  
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setColor Dialog-Frame 
FUNCTION setColor RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN slRed slGreen slBlue.

  COLOR-TABLE:SET-RED-VALUE(ix,slRed) NO-ERROR.
  COLOR-TABLE:SET-GREEN-VALUE(ix,slGreen) NO-ERROR.
  COLOR-TABLE:SET-BLUE-VALUE(ix,slBlue) NO-ERROR.

END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

