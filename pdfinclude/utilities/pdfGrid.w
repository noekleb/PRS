&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
                        Coe Press Equipment Corporation
  File:         pdfRuler.w    
  Author:       Paul Keary                              Created: 07/22/03
  Description:  Generates a PDF containing rulers useful in measuring PDF
                units.

  This .W file was created with the Progress UIB.
--------------------------------------------------------------------------
  Modified   By             Description
  11/18/03   G Campbell     - Updated to use most recent version of PDFinclude
                            - removed some COE Press Specific 'stuff'
                            
  
--------------------------------------------------------------------------
  Input Parameters:
  Output Parameters:
------------------------------------------------------------------------*/
/* Create an unnamed pool to store all the widgets created by this procedure.
   This is a good default which assures that this procedure's triggers and 
   internal procedures will execute in this procedure's storage, and that
   proper cleanup will occur on deletion of the procedure. */
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ pdf_inc.i "THIS-PROCEDURE"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS GridFileName PaperSize LandscapePage ~
tCaption IncludeTextGrid FontName LeftMargin PointSize TopMargin ~
IncludeGraphicGrid btn-makegrid btn-cancel RECT-1 RECT-2 RECT-3 
&Scoped-Define DISPLAYED-OBJECTS GridFileName PaperSize LandscapePage ~
tCaption IncludeTextGrid FontName LeftMargin PointSize TopMargin ~
IncludeGraphicGrid Lines GridRed GraphicPointSize Units GridGreen GridBlue ~
PDFdocument PDFpage 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 GridFileName PaperSize LandscapePage tCaption ~
IncludeTextGrid FontName LeftMargin PointSize TopMargin IncludeGraphicGrid ~
Lines GridRed GraphicPointSize Units GridGreen GridBlue 
&Scoped-define List-2 Lines GridRed GraphicPointSize Units GridGreen ~
GridBlue PDFdocument b_PDFfile PDFpage 
&Scoped-define List-3 FontName LeftMargin PointSize TopMargin 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TextHorizontal C-Win 
FUNCTION TextHorizontal RETURNS CHARACTER
    ( INPUT pdfStream AS CHARACTER,
      INPUT pdfText   AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-makegrid AUTO-GO 
     LABEL "Make Grid" 
     SIZE 15 BY 1.14.

DEFINE BUTTON b_PDFfile 
     LABEL "..." 
     SIZE 4 BY 1 TOOLTIP "Browse for PDF File".

DEFINE VARIABLE FontName AS CHARACTER FORMAT "X(256)":U INITIAL "Courier" 
     LABEL "Font Name" 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "Courier","Helvetica","Times-Roman" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE PaperSize AS CHARACTER FORMAT "X(256)":U INITIAL "LETTER" 
     LABEL "Paper Size" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "LETTER","LEGAL","A0","A1","A2","A3","A4","A5","A6","B5","LEDGER" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE GraphicPointSize AS DECIMAL FORMAT ">9.99":U INITIAL 10 
     LABEL "Point Size" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE GridBlue AS DECIMAL FORMAT "9.99":U INITIAL 0 
     LABEL "Blue" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE GridFileName AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\temp~\gord.pdf" 
     LABEL "'Grid' File Name" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1 NO-UNDO.

DEFINE VARIABLE GridGreen AS DECIMAL FORMAT "9.99":U INITIAL 0 
     LABEL "Green" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE GridRed AS DECIMAL FORMAT "9.99":U INITIAL .8 
     LABEL "Red" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE LeftMargin AS INTEGER FORMAT ">>9":U INITIAL 1 
     LABEL "Left Margin" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Lines AS INTEGER FORMAT ">>9":U INITIAL 100 
     LABEL "Lines" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE PDFdocument AS CHARACTER FORMAT "X(256)":U 
     LABEL "PDF Document" 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE VARIABLE PDFpage AS INTEGER FORMAT ">>>9":U INITIAL 1 
     LABEL "Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE PointSize AS DECIMAL FORMAT ">9.99":U INITIAL 10 
     LABEL "Point Size" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tCaption AS CHARACTER FORMAT "X(256)":U INITIAL "PDF Grid" 
     LABEL "Caption" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1 NO-UNDO.

DEFINE VARIABLE TopMargin AS INTEGER FORMAT ">>9":U INITIAL 1 
     LABEL "Top Margin" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Units AS INTEGER FORMAT ">>9":U INITIAL 10 
     LABEL "Units" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 79 BY .1.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 79 BY .1.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 79 BY .1.

DEFINE VARIABLE IncludeGraphicGrid AS LOGICAL INITIAL no 
     LABEL "Include Graphic Grid" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE IncludeTextGrid AS LOGICAL INITIAL yes 
     LABEL "Include Text Grid" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE LandscapePage AS LOGICAL INITIAL no 
     LABEL "Landscape" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     GridFileName AT ROW 1.48 COL 18 COLON-ALIGNED
     PaperSize AT ROW 2.91 COL 18 COLON-ALIGNED
     LandscapePage AT ROW 3.14 COL 44
     tCaption AT ROW 4.1 COL 18 COLON-ALIGNED
     IncludeTextGrid AT ROW 6 COL 20
     FontName AT ROW 7.19 COL 18 COLON-ALIGNED
     LeftMargin AT ROW 7.19 COL 55 COLON-ALIGNED
     PointSize AT ROW 8.38 COL 18 COLON-ALIGNED
     TopMargin AT ROW 8.38 COL 55 COLON-ALIGNED
     IncludeGraphicGrid AT ROW 10.76 COL 20
     Lines AT ROW 11.71 COL 18 COLON-ALIGNED
     GridRed AT ROW 11.71 COL 35 COLON-ALIGNED
     GraphicPointSize AT ROW 11.71 COL 55 COLON-ALIGNED
     Units AT ROW 12.91 COL 18 COLON-ALIGNED
     GridGreen AT ROW 12.91 COL 35 COLON-ALIGNED
     GridBlue AT ROW 14.1 COL 35 COLON-ALIGNED
     PDFdocument AT ROW 15.29 COL 18 COLON-ALIGNED
     b_PDFfile AT ROW 15.29 COL 72
     PDFpage AT ROW 16.48 COL 18 COLON-ALIGNED
     btn-makegrid AT ROW 18.38 COL 47
     btn-cancel AT ROW 18.38 COL 64
     RECT-1 AT ROW 10.05 COL 2
     RECT-2 AT ROW 5.52 COL 2
     RECT-3 AT ROW 17.67 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 18.91
         DEFAULT-BUTTON btn-makegrid CANCEL-BUTTON btn-cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "PDF Grid Generator"
         HEIGHT             = 18.91
         WIDTH              = 80
         MAX-HEIGHT         = 18.91
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 18.91
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
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
                                                                        */
/* SETTINGS FOR BUTTON b_PDFfile IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
/* SETTINGS FOR COMBO-BOX FontName IN FRAME DEFAULT-FRAME
   1 3                                                                  */
/* SETTINGS FOR FILL-IN GraphicPointSize IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN GridBlue IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN GridFileName IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN GridGreen IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN GridRed IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR TOGGLE-BOX IncludeGraphicGrid IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX IncludeTextGrid IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX LandscapePage IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN LeftMargin IN FRAME DEFAULT-FRAME
   1 3                                                                  */
/* SETTINGS FOR FILL-IN Lines IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR COMBO-BOX PaperSize IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN PDFdocument IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN PDFpage IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN PointSize IN FRAME DEFAULT-FRAME
   1 3                                                                  */
/* SETTINGS FOR FILL-IN tCaption IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN TopMargin IN FRAME DEFAULT-FRAME
   1 3                                                                  */
/* SETTINGS FOR FILL-IN Units IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* PDF Grid Generator */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* PDF Grid Generator */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "CLOSE" TO CURRENT-WINDOW.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-makegrid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-makegrid C-Win
ON CHOOSE OF btn-makegrid IN FRAME DEFAULT-FRAME /* Make Grid */
DO:
  RUN MakePage.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b_PDFfile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_PDFfile C-Win
ON CHOOSE OF b_PDFfile IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEFINE VARIABLE OkPressed AS LOGICAL NO-UNDO INIT TRUE.
  SYSTEM-DIALOG GET-FILE PDFdocument
                FILTERS "PDF Documents" "*.pdf"
                MUST-EXIST 
                TITLE "Get PDF Document"
                UPDATE OkPressed .

  IF OkPressed THEN
    DISPLAY PDFdocument with FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IncludeGraphicGrid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IncludeGraphicGrid C-Win
ON VALUE-CHANGED OF IncludeGraphicGrid IN FRAME DEFAULT-FRAME /* Include Graphic Grid */
DO:
  ASSIGN {&SELF-NAME}.

  IF IncludeGraphicGrid THEN
    ENABLE {&LIST-2} WITH FRAME {&FRAME-NAME}.
  ELSE
    DISABLE {&LIST-2} WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IncludeTextGrid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IncludeTextGrid C-Win
ON VALUE-CHANGED OF IncludeTextGrid IN FRAME DEFAULT-FRAME /* Include Text Grid */
DO:
  ASSIGN {&SELF-NAME}.

  IF IncludeTextGrid THEN
    ENABLE {&LIST-3} WITH FRAME {&FRAME-NAME}.
  ELSE
    DISABLE {&LIST-3} WITH FRAME {&FRAME-NAME}.
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
/*
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.
*/

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
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
  DISPLAY GridFileName PaperSize LandscapePage tCaption IncludeTextGrid FontName 
          LeftMargin PointSize TopMargin IncludeGraphicGrid Lines GridRed 
          GraphicPointSize Units GridGreen GridBlue PDFdocument PDFpage 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE GridFileName PaperSize LandscapePage tCaption IncludeTextGrid FontName 
         LeftMargin PointSize TopMargin IncludeGraphicGrid btn-makegrid 
         btn-cancel RECT-1 RECT-2 RECT-3 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MakeGraphicGrid C-Win 
PROCEDURE MakeGraphicGrid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE X     AS INTEGER NO-UNDO.
  DEFINE VARIABLE Y     AS INTEGER NO-UNDO.
  DEFINE VARIABLE Loop  AS INTEGER NO-UNDO.
  DEFINE VARIABLE Loop2 AS INTEGER NO-UNDO.

  RUN pdf_Stroke_color("Spdf",GridRed,GridGreen,GridBlue).
  RUN pdf_set_font("spdf",FontName,GraphicPointSize).

  /* Draw Vertical Lines */
  IF Lines > 0 THEN DO:
    DO Loop = 1 TO pdf_PageWidth("Spdf"):
      IF Loop MODULO Lines = 0 THEN DO:
        RUN pdf_Line("Spdf", Loop, 0, Loop, pdf_pageHeight("spdf"),0.5).
        
        RUN pdf_text_Color("Spdf",GridRed,GridGreen,GridBlue).
        RUN pdf_text_xy("Spdf", STRING(Loop), Loop - PointSize, 5).
        RUN pdf_text_Color("Spdf",0.0, 0.0, 0.0).
      END.
      ELSE IF Loop MODULO Units = 0 THEN DO:
        DO Loop2 = 1 TO pdf_PageHeight("Spdf"):
          IF Loop2 MODULO Units = 0 THEN
            RUN pdf_Line("Spdf", Loop, Loop2, Loop + 1, Loop2 ,0.5).
        END.
      END.
    END.

    /* Draw Horizontal Lines */
    DO Loop = 1 TO pdf_PageHeight("Spdf"):
      IF Loop MODULO Lines = 0 THEN DO:
        RUN pdf_Line("Spdf", 0, Loop, pdf_PageWidth("Spdf"), Loop, 0.5).

        RUN pdf_text_Color("Spdf",GridRed,GridGreen,GridBlue).
        RUN pdf_text_xy("Spdf", STRING(Loop), 1, Loop).
        RUN pdf_text_Color("Spdf",0.0, 0.0, 0.0).
      END.
    END.
  END.

  ELSE IF Units > 0 THEN DO:
    DO Loop = 1 TO pdf_PageWidth("Spdf"):
      IF Loop MODULO Units = 0 THEN DO:
        DO Loop2 = 1 TO pdf_PageHeight("Spdf"):
          IF Loop2 MODULO Units = 0 THEN
            RUN pdf_Line("Spdf", Loop, Loop2, Loop + 1, Loop2 ,0.5).
        END.
      END.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MakePage C-Win 
PROCEDURE MakePage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE VARIABLE CaptionWidth  AS INTEGER NO-UNDO.

  ASSIGN FRAME {&FRAME-NAME} {&LIST-1}.

  /* Verify the input */
  IF GridFileName = "" THEN DO:
    MESSAGE "You must enter a File Name for the 'Grid'!" 
            VIEW-AS ALERT-BOX.
    APPLY "ENTRY" TO GridFileName.
    RETURN.
  END.

  IF IncludeTextGrid THEN DO:
    IF FontName = "Helvetica" OR FontName = "Times-Roman" THEN DO:
      MESSAGE "You have selected a Proportional Font!" 
              SKIP(1)
              "The Text Grid is not representative of true Text placement." SKIP
              "This is because each character in a Proportional Font has a different width."
              VIEW-AS ALERT-BOX INFORMATION.
    END.

    IF PointSize = 0 THEN DO:
      MESSAGE "You must enter a non-zero value for the Text Point Size'!" 
              VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO GridFileName.
      RETURN.
    END.

    IF LeftMargin = 0 THEN DO:
      MESSAGE "You must enter a non-zero value for the Left Margin'!" 
              VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO LeftMargin.
      RETURN.
    END.

    IF TopMargin = 0 THEN DO:
      MESSAGE "You must enter a non-zero value for the Top Margin'!" 
              VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO TopMargin.
      RETURN.
    END.
  END. /* IncludeTextGrid */

  IF IncludeGraphicGrid THEN DO:
    IF Lines = 0 AND Units = 0 THEN DO:
      MESSAGE "Both Lines and Units cannot be zero - enter a non-zero value for one of them!"
              VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO Lines.
      RETURN.
    END.

    IF GridRed > 1 
    OR GridGreen > 1
    OR GridBlue > 1 THEN DO:
      MESSAGE "Colour values can only be in the range of zero (0) to one (1)!"
              VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO GridRed.
      RETURN.
    END.

    IF GraphicPointSize = 0 THEN DO:
      MESSAGE "You must enter a non-zero value for the Graphic Point Size'!" 
              VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO GraphicPointSize.
      RETURN.
    END.

    IF PDFdocument <> "" 
    AND SEARCH(PDFdocument) = ? THEN DO:
      MESSAGE "Cannot find the PDF document you specified!" 
              VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO PDFdocument.
      RETURN.
    END.

    IF PDFDocument <> "" 
    AND PDFPage = 0 THEN DO:
      MESSAGE "PDF Page cannot be zero!" 
              VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO PDFpage.
      RETURN.
    END.
  END. /* Include Graphic Grid */

  /* The following code defines the PDF Page */
  RUN pdf_reset_all.
  RUN pdf_new("spdf",GridFileName).

  IF PDFDocument <> "" THEN
    RUN pdf_open_pdf("Spdf",PDFDocument,"GRID").

  RUN pdf_set_PaperType("spdf",PaperSize).
  RUN pdf_set_LeftMargin("spdf",LeftMargin).
  RUN pdf_set_TopMargin("spdf",TopMargin).

  IF LandscapePage THEN RUN pdf_set_orientation("spdf","LANDSCAPE").

  RUN pdf_new_page("Spdf").

  IF PDFdocument <> "" THEN
    RUN pdf_use_pdf_page("Spdf","GRID",PDFpage).

  IF tCaption <> "" THEN DO:
    CaptionWidth = INT((LENGTH(tCaption) * 600 / 1000) * 20). 

    RUN pdf_watermark("Spdf",
                      tCaption, 
                      FontName,
                      20.0,
                      0.805,
                      0.805,
                      0.805,
                      (pdf_PageWidth("Spdf") / 2) - (CaptionWidth / 2),
                      INT((pdf_PageHeight("Spdf") / 2) + 10)  ).
  END.

  /* Now Draw the Text Grid - if selected */
  IF IncludeTextGrid THEN
    RUN MakeTextGrid.
  
  /* Now Draw the Graphic Grid - if selected */
  IF IncludeGraphicGrid THEN
    RUN MakeGraphicGrid.

  /* Close the PDF Document and  View it */
  RUN pdf_close("spdf").
  RUN ViewDoc.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MakeTextGrid C-Win 
PROCEDURE MakeTextGrid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE Loop  AS INTEGER NO-UNDO.

  RUN pdf_set_font("spdf",FontName,PointSize).
  RUN pdf_text_color("Spdf",0.0,0.0,0.0).

  /* Place Text Ruler */
  RUN pdf_skip("Spdf").
  DO Loop = 1 TO (pdf_PageHeight("Spdf") - PointSize):
    IF Loop = 1 THEN
      RUN pdf_text("Spdf",TextHorizontal("spdf","0")).

    ELSE IF Loop MODULO 10 = 0 THEN DO:
      RUN pdf_text("Spdf",TextHorizontal("spdf",STRING(Loop ))).
    END.

    ELSE
      RUN pdf_text("Spdf", "-").

    IF (pdf_textY("Spdf") - (PointSize * 2)) <= 0 THEN 
      LEAVE.

    RUN pdf_skip("Spdf").

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewDoc C-Win 
PROCEDURE ViewDoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 RUN viewxmldialog.w (GridFileName,"PDF Grid").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TextHorizontal C-Win 
FUNCTION TextHorizontal RETURNS CHARACTER
    ( INPUT pdfStream AS CHARACTER,
      INPUT pdfText   AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  DEFINE VARIABLE TextHorizontal  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE Loop            AS INTEGER   NO-UNDO.
  DEFINE VARIABLE Increment       AS CHARACTER NO-UNDO.

  DO Loop = 1 TO pdf_PageWidth(pdfStream):

    IF Loop MODULO 10 = 0 THEN DO:
      Increment = STRING(Loop ). 
      TextHorizontal = TextHorizontal 
                     + SUBSTR("---------",LENGTH(Increment))
                     + Increment.
    END.

    IF pdf_text_width("Spdf",TextHorizontal) > pdf_PageWidth(pdfStream) THEN 
      LEAVE.
  END.

  TextHorizontal = SUBSTR(TextHorizontal, LENGTH(pdfText) + 1).

  TextHorizontal = pdfText + TextHorizontal.

  RETURN TextHorizontal.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

