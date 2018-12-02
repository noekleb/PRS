&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def stream O.

DEF VAR justPreview         as logical.

def var myReturn            as char no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS myUnit topMargin labelHeight pageOrientation ~
rSens labelWidth labelsOnARow labelsOnACol BUTTON-1 formName Btn_OK ~
leftMargin Btn_Cancel RECT-10 RECT-11 RECT-12 RECT-13 RECT-14 RECT-15 ~
RECT-16 RECT-17 RECT-18 RECT-3 RECT-4 RECT-5 RECT-6 RECT-7 RECT-8 RECT-9 
&Scoped-Define DISPLAYED-OBJECTS myUnit topMargin labelHeight ~
pageOrientation rSens labelWidth labelsOnARow labelsOnACol formName ~
leftMargin 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 24 BY 1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 24 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     LABEL "Show me a PREVIEW" 
     SIZE 24 BY 1.

DEFINE VARIABLE formName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Form name" 
     VIEW-AS FILL-IN 
     SIZE 31 BY .95 TOOLTIP "Name of the ouput file." NO-UNDO.

DEFINE VARIABLE labelHeight AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 50 
     VIEW-AS FILL-IN 
     SIZE 14 BY .95 NO-UNDO.

DEFINE VARIABLE labelsOnACol AS DECIMAL FORMAT ">>9":U INITIAL 5 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 TOOLTIP "# labels in a column" NO-UNDO.

DEFINE VARIABLE labelsOnARow AS INTEGER FORMAT ">9":U INITIAL 2 
     LABEL "# labels in the page" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 TOOLTIP "# labels on a row" NO-UNDO.

DEFINE VARIABLE labelWidth AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 90 
     VIEW-AS FILL-IN 
     SIZE 14 BY .95 NO-UNDO.

DEFINE VARIABLE leftMargin AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 16 
     VIEW-AS FILL-IN 
     SIZE 14 BY .95 NO-UNDO.

DEFINE VARIABLE topMargin AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 22 
     VIEW-AS FILL-IN 
     SIZE 14 BY .95 NO-UNDO.

DEFINE VARIABLE myUnit AS CHARACTER INITIAL "MM" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Inches", "INCHES",
"Millimeters", "MM"
     SIZE 45 BY .95 NO-UNDO.

DEFINE VARIABLE pageOrientation AS CHARACTER INITIAL "Portrait" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Portrait", "Portrait",
"LandScape", "Landscape"
     SIZE 26 BY 2.14
     FONT 6 NO-UNDO.

DEFINE VARIABLE rSens AS CHARACTER INITIAL "H" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Horizontal", "H",
"Vertical", "V"
     SIZE 25 BY 1.91
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 0  
     SIZE 25 BY .95
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 2.14.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 2.14.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 2.14.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 2.14.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 2.14.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 2.14.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 2.14.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 2.14.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 2.14.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 0  
     SIZE 1 BY 1.43
     BGCOLOR 0 .

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 0  
     SIZE 15 BY .1
     BGCOLOR 0 .

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 0  
     SIZE 1 BY 2.62
     BGCOLOR 1 .

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 0  
     SIZE 15 BY .1
     BGCOLOR 1 .

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 0  
     SIZE 3 BY .24
     BGCOLOR 0 .

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 0  
     SIZE .4 BY 6.43
     BGCOLOR 0 .

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 0  
     SIZE 20 BY .24
     BGCOLOR 1 .

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 0  
     SIZE .4 BY 3.33
     BGCOLOR 1 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 2.14.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 2.14.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 2.14.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 2.14.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 2.14.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 2.14.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 2.14.

DEFINE RECTANGLE thePage
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 84 BY 13.57
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     myUnit AT ROW 1.71 COL 65 NO-LABEL
     topMargin AT ROW 3.62 COL 93 COLON-ALIGNED NO-LABEL
     labelHeight AT ROW 6 COL 97 COLON-ALIGNED NO-LABEL
     pageOrientation AT ROW 10.52 COL 91 NO-LABEL
     rSens AT ROW 14.57 COL 92 NO-LABEL
     labelWidth AT ROW 18.14 COL 14 COLON-ALIGNED NO-LABEL
     labelsOnARow AT ROW 18.14 COL 55 COLON-ALIGNED
     labelsOnACol AT ROW 18.14 COL 66 COLON-ALIGNED NO-LABEL
     BUTTON-1 AT ROW 18.14 COL 91
     formName AT ROW 19.33 COL 55 COLON-ALIGNED
     Btn_OK AT ROW 19.33 COL 91
     leftMargin AT ROW 20.29 COL 1 COLON-ALIGNED NO-LABEL
     Btn_Cancel AT ROW 20.52 COL 91
     RECT-22 AT ROW 6.48 COL 83
     RECT-25 AT ROW 13.86 COL 8
     RECT-19 AT ROW 3.38 COL 79
     thePage AT ROW 3.38 COL 5
     RECT-24 AT ROW 13.14 COL 6
     RECT-21 AT ROW 4.81 COL 83
     RECT-20 AT ROW 4.1 COL 79
     RECT-1 AT ROW 1.95 COL 5
     RECT-26 AT ROW 14.1 COL 19.2
     RECT-23 AT ROW 13.14 COL 5
     RECT-10 AT ROW 12.67 COL 28
     RECT-11 AT ROW 12.67 COL 48
     RECT-12 AT ROW 4.81 COL 48
     RECT-13 AT ROW 7.43 COL 48
     RECT-14 AT ROW 10.05 COL 48
     RECT-15 AT ROW 12.67 COL 67
     RECT-16 AT ROW 4.81 COL 67
     RECT-17 AT ROW 7.43 COL 67
     RECT-18 AT ROW 10.05 COL 67
     RECT-3 AT ROW 4.81 COL 8
     RECT-4 AT ROW 4.81 COL 28
     RECT-5 AT ROW 7.43 COL 8
     RECT-6 AT ROW 7.43 COL 28
     RECT-7 AT ROW 10.05 COL 8
     RECT-8 AT ROW 10.05 COL 28
     RECT-9 AT ROW 12.67 COL 8
     "Label height" VIEW-AS TEXT
          SIZE 16 BY .71 AT ROW 5.29 COL 98
     " Top margin" VIEW-AS TEXT
          SIZE 16 BY .71 AT ROW 2.91 COL 94
     " Label width" VIEW-AS TEXT
          SIZE 16 BY .71 AT ROW 17.43 COL 15
     " Left margin" VIEW-AS TEXT
          SIZE 16 BY .71 AT ROW 19.57 COL 2
     "x" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 18.38 COL 66
     " xPrint labels printer" VIEW-AS TEXT
          SIZE 25 BY .95 AT ROW 1.71 COL 4
          BGCOLOR 6 FGCOLOR 14 FONT 6
     SPACE(89.79) SKIP(19.81)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "xPrint demo program"
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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-19 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-20 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-21 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-22 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-23 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-24 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-25 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-26 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE thePage IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* xPrint demo program */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
        ASSIGN frame {&FRAME-NAME} {&DISPLAYED-OBJECTS}.
        
        if formName = "" then do :
                message "Form name is mandatory"
                            view-as alert-box info.
                apply "entry" to formName.
                return no-apply.
                end.
                
        OUTPUT STREAM O to value(formName).
        justPreview = false.
        
        RUN writexPrintForm.
        
        if NOT formName matches "*.i" then
                put stream O skip(1).      /* Import does not read without skip */
        
        Output stream O close.
        
        myReturn = formName + "," + string(labelsOnACol * labelsOnARow) + ',' + pageOrientation.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 Dialog-Frame
ON CHOOSE OF BUTTON-1 IN FRAME Dialog-Frame /* Show me a PREVIEW */
DO:
  Output stream O to tempLabel.xpr.
  justPreview = true.
  
  RUN writexPrintForm.
  
  Output stream O close.
  
  run printFile('tempLabel.xpr').
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */

thePage:move-to-bottom().

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

return myReturn.

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
  DISPLAY myUnit topMargin labelHeight pageOrientation rSens labelWidth 
          labelsOnARow labelsOnACol formName leftMargin 
      WITH FRAME Dialog-Frame.
  ENABLE myUnit topMargin labelHeight pageOrientation rSens labelWidth 
         labelsOnARow labelsOnACol BUTTON-1 formName Btn_OK leftMargin 
         Btn_Cancel RECT-10 RECT-11 RECT-12 RECT-13 RECT-14 RECT-15 RECT-16 
         RECT-17 RECT-18 RECT-3 RECT-4 RECT-5 RECT-6 RECT-7 RECT-8 RECT-9 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE writexPrintForm Dialog-Frame 
PROCEDURE writexPrintForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var frameNumber     as int no-undo.
DEF VAR I               as INT no-undo.
DEF VAR J               as INT no-undo.

ASSIGN frame {&FRAME-NAME} {&DISPLAYED-OBJECTS}.


if justPreview then
    put stream O unformatted '<PREVIEW><O' pageOrientation ">".
    
PUT STREAM o   CONTROL '<UNITS=' myUnit '>'.

session:numeric-Format = "AMERICAN".

If rSens = "H" then
    DO I = 1 to labelsOnACol :
        DO J = 1 to  labelsOnARow  :
                frameNumber = frameNumber + 1.
                PUT STREAM O CONTROL "<AT=" topMargin + (I - 1) * labelHeight
                                            ','
                                     leftMargin + (J - 1) * labelWidth
                                     '><#' frameNumber 
                                     '><AT=+' labelHeight ',+' labelWidth 
                                     '><FRAME#' frameNumber '>'.
                if justPreview then
                        put stream O control "<BGCOLOR=LTGRAY><FILLRECT)#" frameNumber ">".
                END.
        END.
else
    DO J = 1 to labelsOnARow :
        DO I = 1 to  labelsOnACol  :
                frameNumber = frameNumber + 1.
                PUT STREAM O CONTROL "<AT=" topMargin + (I - 1) * labelHeight
                                            ','
                                     leftMargin + (J - 1) * labelWidth
                                     '><#' frameNumber 
                                     '><AT=+' labelHeight ',+' labelWidth 
                                     '><FRAME#' frameNumber '>'.
                if justPreview then
                        put stream O control "<BGCOLOR=LTGRAY><FILLRECT)#" frameNumber ">".
                END.
        END.  
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE xPrint Dialog-Frame 
PROCEDURE xPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

PROCEDURE printFile external "xPrint.dll":
    def input parameter A as char.
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

