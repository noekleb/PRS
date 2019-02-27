&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        Container for a JukeBox window program

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           18.oct.2006

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

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE bEnableExcel       AS LOGICAL     NO-UNDO INIT YES.
DEFINE VARIABLE bHideLines         AS LOGICAL     NO-UNDO.
DEFINE VARIABLE bHideColHeaders    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE bKeepEmptySpace    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE bViewHeaderInExcel AS LOGICAL     NO-UNDO INIT YES.
DEFINE VARIABLE bOk                AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cEndColumnHeader   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cStartColumnHeader AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hBrowse            AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBuffer            AS HANDLE      NO-UNDO.
DEFINE VARIABLE hToolbar           AS HANDLE      NO-UNDO.
DEFINE VARIABLE iCols              AS INTEGER     NO-UNDO.
DEFINE VARIABLE iFooterCnt         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPageLines         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNumHeaderLines    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iFooterLineNum     AS INTEGER     NO-UNDO.
DEFINE VARIABLE ix                 AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLineCnt           AS INTEGER     NO-UNDO INIT 1.
DEFINE VARIABLE cDropFile          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOrientation       AS CHARACTER   NO-UNDO.

DEFINE VARIABLE chActiveWin        AS COM-HANDLE  NO-UNDO.
DEFINE VARIABLE chExcelApplication AS COM-HANDLE  NO-UNDO.
DEFINE VARIABLE chWorkbook         AS COM-HANDLE  NO-UNDO.
DEFINE VARIABLE chWorksheet        AS COM-HANDLE  NO-UNDO.

DEF STREAM strFile.

DEF TEMP-TABLE ttColumnValues NO-UNDO
    FIELD cTekst AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwPreview tbPreview 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ClearEmptySpace C-Win 
FUNCTION ClearEmptySpace RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CountDashes C-Win 
FUNCTION CountDashes RETURNS CHARACTER
  ( cText AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getColValues C-Win 
FUNCTION getColValues RETURNS CHARACTER
  ( cText AS CHAR, cColWidths AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDataType C-Win 
FUNCTION getDataType RETURNS CHARACTER
  ( cTestValues AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadLine C-Win 
FUNCTION LoadLine RETURNS LOGICAL
  ( INPUT icInput AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadPreviewFromFile C-Win 
FUNCTION LoadPreviewFromFile RETURNS LOGICAL
  ( INPUT icFileName AS CHAR,
    INPUT ibAppend   AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadPreviewFromText C-Win 
FUNCTION LoadPreviewFromText RETURNS LOGICAL
  ( INPUT icText      AS CHAR,
    INPUT icDelimiter AS CHAR,
    INPUT ibAppend    AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadPreviewFromTT C-Win 
FUNCTION LoadPreviewFromTT RETURNS LOGICAL
  ( INPUT ihTT        AS HANDLE,
    INPUT icFieldName AS CHAR,
    INPUT ibAppend    AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setEnableExcel C-Win 
FUNCTION setEnableExcel RETURNS LOGICAL
  ( INPUT ibEnableExcel AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setEndColumnHeader C-Win 
FUNCTION setEndColumnHeader RETURNS LOGICAL
  ( INPUT icEndColumnHeader AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFooterLineNum C-Win 
FUNCTION setFooterLineNum RETURNS LOGICAL
  ( INPUT iiFooterLineNum AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setHeadersHidden C-Win 
FUNCTION setHeadersHidden RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setKeepEmptySpace C-Win 
FUNCTION setKeepEmptySpace RETURNS LOGICAL
  ( INPUT ibKeepEmptySpace AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setNumHeaderLines C-Win 
FUNCTION setNumHeaderLines RETURNS LOGICAL
  ( INPUT iiNumHeaderLines AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setOrientation C-Win 
FUNCTION setOrientation RETURNS LOGICAL
  ( INPUT icOrientation AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setStartColumnHeader C-Win 
FUNCTION setStartColumnHeader RETURNS LOGICAL
  ( INPUT icStartColumnHeader AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setViewHeadersInExcel C-Win 
FUNCTION setViewHeadersInExcel RETURNS LOGICAL
  ( INPUT ibViewHeaderInExcel AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setWindowTitle C-Win 
FUNCTION setWindowTitle RETURNS LOGICAL
  ( INPUT icWinTitle AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE brwPreview
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 127 BY 20.48.

DEFINE RECTANGLE tbPreview
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brwPreview AT ROW 2.43 COL 2
     tbPreview AT ROW 1.24 COL 2.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 129 BY 22.1.


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
         TITLE              = "<Insert window title>"
         HEIGHT             = 22.14
         WIDTH              = 129
         MAX-HEIGHT         = 24.48
         MAX-WIDTH          = 129
         VIRTUAL-HEIGHT     = 24.48
         VIRTUAL-WIDTH      = 129
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <Insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    /* Export to Excel does only apply for column reports with dashes (-) to underline header columns
       To remove the Excel button (must be called before InitializeObject): */
/*     setEnableExcel(NO).  */

    RUN InitializeObject.
    
    /* YES in the last parameter: Append the existing preview: */
    setEndColumnHeader("---------- -------- ").
    /*  For a paged column report you can tell the preview what the last row in the header starts with to 
        suppress headers on pages after the first from preview: */
    LoadPreviewFromFile("c:\temp\rpprev.txt",NO).

/*     setEndColumnHeader("******************").        */
/*     LoadPreviewFromFile("c:\temp\utgifter.txt",NO).  */
    setWindowTitle("Testreport").
    RUN MoveToTop.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyToClipboardRecord C-Win 
PROCEDURE CopyToClipboardRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
CLIPBOARD:MULTIPLE = YES.

DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
   IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
     CLIPBOARD:VALUE = hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE.
END.
CLIPBOARD:MULTIPLE = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeselectRowRecord C-Win 
PROCEDURE DeselectRowRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hSelectCount AS HANDLE NO-UNDO.

hSelectCount = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE:CURRENT-WINDOW,"RecordSelectWidget")) NO-ERROR.

hBrowse:DESELECT-ROWS().

IF VALID-HANDLE(hSelectCount) THEN
  hSelectCount:SCREEN-VALUE = "".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DropFileNotifyBrowse C-Win 
PROCEDURE DropFileNotifyBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ix      AS INT    NO-UNDO.
DEF VAR bAppend AS LOG    NO-UNDO.
DEF VAR hHelp   AS HANDLE NO-UNDO.

hHelp = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE:CURRENT-WINDOW,"HelpTextWidget")) NO-ERROR.


IF hBrowse:NUM-ITERATIONS > 0 THEN
  MESSAGE IF DYNAMIC-FUNCTION("Scandinavian") THEN
            "Legg til eksisterende?"
          ELSE
            "Append to existing?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bAppend.
DO ix = 1 TO hBrowse:NUM-DROPPED-FILES:
  IF hBrowse:NUM-DROPPED-FILES > 1 THEN
    LoadLine(" ").
    LoadLine("/******** " + hBrowse:GET-DROPPED-FILE(ix) + " **************/").
  
  cDropFile = hBrowse:GET-DROPPED-FILE(ix).

  LoadPreviewFromFile(ENTRY(1,hBrowse:GET-DROPPED-FILE(ix)),IF ix = 1 THEN bAppend ELSE YES).

  IF VALID-HANDLE(hHelp) THEN DO:
    IF ix = 1 THEN
      hHelp:SCREEN-VALUE = hBrowse:GET-DROPPED-FILE(ix).
    ELSE 
      hHelp:SCREEN-VALUE = hHelp:SCREEN-VALUE + ", " + ENTRY(NUM-ENTRIES(hBrowse:GET-DROPPED-FILE(ix),"\"),hBrowse:GET-DROPPED-FILE(ix),"\").

    hBrowse:HELP = hHelp:SCREEN-VALUE.
  END.
END.
IF bAppend OR hBrowse:NUM-DROPPED-FILES > 1 THEN
  hBrowse:GET-BROWSE-COLUMN(2):VISIBLE = YES.

APPLY "entry" TO hBrowse.

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
  ENABLE brwPreview tbPreview 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelRecord C-Win 
PROCEDURE ExcelRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR cColumnWidths AS CHAR NO-UNDO.
DEF VAR cLabels       AS CHAR NO-UNDO.
DEF VAR cFirstLabels  AS CHAR NO-UNDO.
DEF VAR cDataTypes    AS CHAR NO-UNDO.
DEF VAR cTestValues   AS CHAR NO-UNDO.
DEF VAR ix            AS INT  NO-UNDO.
DEF VAR iCount        AS INT  NO-UNDO.
DEF VAR cHeader       AS CHAR NO-UNDO.
DEF VAR bColHeadFound AS LOG  NO-UNDO.
DEF VAR cColHeadGuess AS CHAR NO-UNDO.

EMPTY TEMP-TABLE ttColumnValues.
 
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " WHERE NOT bHidden BY iLnr").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN cHeader = cHeader + (IF cHeader NE "" THEN "|" ELSE "") + hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE
         ix      = ix + 1.
  IF cEndColumnHeader NE "" AND hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE BEGINS cEndColumnHeader THEN DO:
    bColHeadFound = YES.
    LEAVE.
  END. 
  IF ix > 100 THEN LEAVE.
  hQuery:GET-NEXT().
END.

IF NOT bColHeadFound THEN DO:
  hQuery:GET-FIRST().
  ASSIGN ix      = 0
         cHeader = "".
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    ASSIGN cHeader = cHeader + (IF cHeader NE "" THEN "|" ELSE "") + hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE
           ix      = ix + 1.
    IF hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE MATCHES "*---- ----*" THEN DO:
      cColHeadGuess = hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE.
      LEAVE.
    END.
    IF ix > 100 THEN LEAVE.
    hQuery:GET-NEXT().
  END.

  IF cColHeadGuess NE "" THEN
    MESSAGE (IF DYNAMIC-FUNCTION("Scandinavian") THEN
               "Er dette siste linje i kolonneoverskrift:" 
             ELSE 
               "Is this the last line in the column header:") SKIP(1)
             cColHeadGuess
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bColHeadFound.
  IF bColHeadFound THEN DO:
    cEndColumnHeader = cColHeadGuess.
    DELETE OBJECT hQuery.
    ASSIGN iPageLines = 0
           bHideLines = NO.
    setHeadersHidden().
    RUN ExcelRecord.
    RETURN.
  END. 
END.

IF bColHeadFound THEN DO:
  cColumnWidths = CountDashes (hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE).
  hQuery:GET-PREV().
  hQuery:GET-PREV().
  cFirstLabels = GetColValues (hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE, cColumnWidths).
  hQuery:GET-NEXT().
  cLabels      = GetColValues (hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE, cColumnWidths).
  hQuery:GET-NEXT(). /*--dashes--*/
  hQuery:GET-NEXT().

  DO ix = 1 TO NUM-ENTRIES(cFirstLabels,";"):
    IF ENTRY(ix,cFirstLabels,";") NE "" THEN 
      ENTRY(ix,cLabels,";") = TRIM(TRIM(ENTRY(ix,cFirstLabels,";"),"-")) + " " + TRIM(ENTRY(ix,cLabels,";")).
  END.
END.

DO ON STOP UNDO, LEAVE:
  REPEAT WHILE hBuffer:AVAIL:
    IF NOT (hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE = "" OR (SUBSTRING(hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE, 1, 20) = ""  AND 
                  (hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE MATCHES "*TOTAL*" OR hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE MATCHES "*---*"))) THEN DO:
      CREATE ttColumnValues.
      ttColumnValues.cTekst = GetColValues (hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE,cColumnWidths).
    END.
    hQuery:GET-NEXT().
  END.

  DO ix = 1 TO NUM-ENTRIES(cLabels, ";") - 1:
    iCount = 1.
    cTestValues = "".
    FOR EACH ttColumnValues:
      cTestValues = cTestValues + ENTRY(ix,ttColumnValues.cTekst, ";") + ";".
      IF iCount = 5 THEN
        LEAVE.
      iCount = iCount + 1.
    END.
    cDataTypes = cDataTypes + GetDataType(cTestValues) + ";".
    FIND FIRST ttColumnValues.
    IF ENTRY(ix, cDataTypes, ";") = "DATE" THEN
      ENTRY(ix, cColumnWidths, ";") =  STRING (INT(ENTRY(ix, cColumnWidths, ";")) + 2).
    ELSE IF (ENTRY(ix, cDataTypes, ";") = "CHARACTER"
          AND SUBSTRING (ENTRY(ix, ttColumnValues.cTekst, ";"),5,1) = "."
          AND LENGTH(TRIM(ENTRY(ix, ttColumnValues.cTekst, ";"))) = 10) THEN DO:
      FOR EACH ttColumnValues:
        ENTRY(ix,ttColumnValues.cTekst, ";") = SUBSTRING(ENTRY(ix, ttColumnValues.cTekst, ";"),1,4) + "-" + SUBSTRING(ENTRY(ix, ttColumnValues.cTekst, ";"),6).
      END.
    END.
  END.
  
  
  DELETE OBJECT hQuery.
  
  RUN OutputToExcel(cColumnWidths,cLabels,cDataTypes,cHeader).
    IF ERROR-STATUS:ERROR THEN
      DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
        MESSAGE ERROR-STATUS:GET-MESSAGE(ix).
      END.
END.

IF KEYFUNCTION(LASTKEY) = "stop" THEN DO:

  RELEASE OBJECT chWorkbook NO-ERROR.
  RELEASE OBJECT chWorksheet NO-ERROR.

END.


SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
DEF VAR cEditMenuLabel AS CHAR NO-UNDO.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?) NO-ERROR.

DO WITH FRAME {&FRAME-NAME}:
  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
          ,brwPreview:HANDLE
          ,100
          ,"multiple"
          ,"temp-table"
           + ";cText|CHARACTER|x(256)||" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Tekst" ELSE "Text")
           + ";cFileName|CHARACTER|x(40)||File name"
           + ";!bHidden|LOGICAL|yes/no"
           + ";!bEmpty|LOGICAL|yes/no"
           + ";!iLnr|INTEGER|>>>>9||"
          ,"WHERE false"
          ,"").

  ASSIGN hBrowse:FONT        = 3
         hBrowse:SEPARATORS  = NO
         hBrowse:HELP        = "Preview"
         hBrowse:DROP-TARGET = YES
         hBuffer             = hBrowse:QUERY:GET-BUFFER-HANDLE(1)
         hBrowse:GET-BROWSE-COLUMN(2):VISIBLE = NO.
         .
  DYNAMIC-FUNCTION("setNoColumnSort",hBrowse,"cText").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"searchDefault","match").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"baseQuery","WHERE NOT bHidden").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"windowsBrowse","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterExcludeFields","cFileName").

  DYNAMIC-FUNCTION("NewMenuBand",
                   hBrowse,                                      /* Parent widget */
                   "CopyToClipboard;"
                    + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Kopier markerte rader" ELSE "Copy seleced rows")
                 + ",DeselectRow;"
                    + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fjern markering av rad" ELSE "Deselect rows")
                   ,"").

  cEditMenuLabel = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Rediger" ELSE "Edit".

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
        ,TbPreview:HANDLE
        ,(IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fil" ELSE "File")
        ,"Filter|" + cEditMenuLabel
       + ",Word"
       + (IF bEnableExcel THEN ",Excel" ELSE "")
       + ",Print|"
       + ",SelectPrinter;Printctrl-p¤menu"
       + ",-¤menu,Close;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Avsluttalt-a" ELSE "Exitalt-x") + "¤menu"
       + "|Edit;" + IF DYNAMIC-FUNCTION("Scandinavian") THEN "Rediger" ELSE "Edit"
        ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
END.

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF (iCols GT 80 OR cOrientation = "landscape") AND {&WINDOW-NAME}:WIDTH-CHARS < 130 THEN DO:
  {&WINDOW-NAME}:WIDTH-CHARS = {&WINDOW-NAME}:WIDTH-CHARS + 45.
  APPLY "window-resized" TO {&WINDOW-NAME}.
END.

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

APPLY "entry" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OutputToExcel C-Win 
PROCEDURE OutputToExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM cColumnWidths AS CHAR NO-UNDO.
DEF INPUT PARAM cLabels       AS CHAR NO-UNDO.
DEF INPUT PARAM cDataTypes    AS CHAR NO-UNDO.
DEF INPUT PARAM icHeader      AS CHAR NO-UNDO.

DEF VAR cAlfabet     AS CHAR NO-UNDO INIT "A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z".
DEF VAR cColumn      AS CHAR NO-UNDO.
DEF VAR cColList     AS CHAR NO-UNDO.
DEF VAR iRow         AS INT  NO-UNDO.
DEF VAR ix           AS INT NO-UNDO.
DEF VAR iz           AS INT NO-UNDO.      
DEF VAR cSessionDate AS CHAR NO-UNDO.
DEF VAR cSessionNum  AS CHAR NO-UNDO.

/* ASSIGN cSessionDate = SESSION:DATE-FORMAT    */
/*        cSessionNum  = SESSION:NUMERIC-FORMAT */
/*        SESSION:DATE-FORMAT    = "mdy"        */
/*        SESSION:NUMERIC-FORMAT = "american".  */

SESSION:SET-WAIT-STATE("General":U).

chActiveWin = chExcelApplication:ActiveWindow NO-ERROR.

/* create a new Excel Application object */
IF NOT VALID-HANDLE(chActiveWin) THEN DO:
  RELEASE OBJECT chExcelApplication NO-ERROR.
  CREATE "Excel.Application" chExcelApplication.
END.

/* launch Excel so it is visible to the user */
chExcelApplication:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

/* set the column names for the Worksheet */
iz = NUM-ENTRIES(cColumnWidths,';') - 1.
DO ix = 1 TO iz:
  IF ix > 26 THEN
    cColumn = "A" + ENTRY(ix - 26,cAlfabet).
  ELSE
    cColumn = ENTRY(ix,cAlfabet).
  cColList = cColList + cColumn + ";".
  chWorkSheet:Columns(cColumn):ColumnWidth = INT(ENTRY(ix, cColumnWidths,';')).
  chWorkSheet:Range(cColumn + "1"):Value = ENTRY(ix,cLabels,";") NO-ERROR.
END.
chWorkSheet:Range("A1:" + cColumn + "1"):Font:Bold = TRUE NO-ERROR.
                                      
/* copy the temp-table values into the Worksheet cells */ 
iRow = 2.
FOR EACH ttColumnValues:
  DO ix = 1 TO iz:
    IF ENTRY(ix,cDataTypes,';') = "DECIMAL" THEN
      chWorkSheet:Range(ENTRY(ix,cColList, ';') + STRING(iRow)):VALUE = DECIMAL(TRIM(ENTRY(ix, ttColumnValues.cTekst, ';'))) NO-ERROR.
    ELSE IF ENTRY(ix,cDataTypes,';') = "DATE" THEN
      chWorkSheet:Range(ENTRY(ix,cColList, ';') + STRING(iRow)):VALUE = DATE(TRIM(ENTRY(ix, ttColumnValues.cTekst, ';'))) NO-ERROR.
    ELSE
      chWorkSheet:Range(ENTRY(ix,cColList, ';') + STRING(iRow)):VALUE = TRIM(ENTRY(ix, ttColumnValues.cTekst, ';')).
  END.
  iRow = iRow + 1.
END.

/* SESSION:DATE-FORMAT    = cSessionDate. */
/* SESSION:NUMERIC-FORMAT = cSessionNum.  */

IF bViewHeaderInExcel THEN DO:
  DO ix = 1 TO NUM-ENTRIES(icHeader,"|") - 1:
    chWorkSheet:Rows("1:1"):INSERT() NO-ERROR.
  END.
  DO ix = 1 TO NUM-ENTRIES(icHeader,"|") - 2:
    chWorkSheet:Range("A" + STRING(ix)):Value = ENTRY(ix,icHeader,"|") NO-ERROR.    
  END.
END.

/* release com-handles */
RELEASE OBJECT chWorkbook NO-ERROR.
RELEASE OBJECT chWorksheet NO-ERROR.

SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord C-Win 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hQuery AS HANDLE NO-UNDO.

IF NOT hBrowse:QUERY:IS-OPEN THEN RETURN.

bOK = SESSION:SET-WAIT-STATE("General").

IF (iCols GT 80 OR cOrientation = "landscape") AND cOrientation NE "portrait" THEN DO:
  If iCols Gt 120 Then Do:
    Output To PRINTER.
    Put Control "~033&l8D".      /* 8 lines pr inch          */
    Put Control "~033(s12H".     /* 12 chars pr inch         */
    Put Control "~033&l1O".      /* Landscape                */
  End.
  Else Do:
    Output To PRINTER.
    Put Control "~033&l1O".      /* Landscape                */
  End.
End.
Else Do:
  Output To PRINTER.
  Put Control "~033&l0O".      /* Portrait                 */
End.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " BY iLnr").
hQuery:QUERY-OPEN().

REPEAT WHILE hQuery:GET-NEXT():
  IF hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE = "" THEN PUT SKIP(1).
  ELSE IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryWhere") = "" AND DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryFilter") = "" AND ASC(SUBSTR(hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE,1,1)) = 12 THEN PAGE.
  ELSE PUT UNFORMATTED hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE SKIP.
END.

OUTPUT CLOSE.

DELETE OBJECT hQuery.

bOK = SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectPrinterRecord C-Win 
PROCEDURE SelectPrinterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
SYSTEM-DIALOG PRINTER-SETUP UPDATE bOk.

IF bOk THEN RUN PrintRecord.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WordRecord C-Win 
PROCEDURE WordRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE chWordApplication AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chDocument        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE cTekst            AS CHAR NO-UNDO.
DEFINE VARIABLE hQuery            AS HANDLE NO-UNDO.

SESSION:SET-WAIT-STATE("General").

CREATE "Word.Application" chWordApplication.
chWordApplication:Visible = True.

chDocument = chWordApplication:Documents:Add().
chDocument:Activate().
chDocument:Range:FONT:NAME = "Courier".
chDocument:Range:FONT:SIZE = 8.

IF iCols LE 80 THEN 
  chDocument:PageSetup:Orientation  = 0.
ELSE DO:
  chDocument:PageSetup:Orientation  = 1.
  chDocument:PageSetup:LeftMargin  = 36.
  chDocument:PageSetup:RightMargin = 19.
END.
  
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " BY iLnr").
hQuery:QUERY-OPEN().

REPEAT WHILE hQuery:GET-NEXT():
  chWordApplication:Selection:InsertAfter(hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE + CHR(13)).
END.
RELEASE OBJECT chDocument.
RELEASE OBJECT chWordApplication.

DELETE OBJECT hQuery.

SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ClearEmptySpace C-Win 
FUNCTION ClearEmptySpace RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Clear empty lines from preview
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hQuery AS HANDLE NO-UNDO.

DEF VAR bPrevLineEmpty AS LOG NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " WHERE NOT bHidden BY iLnr").
hQuery:QUERY-OPEN().
REPEAT WHILE hQuery:GET-NEXT():
  IF hBuffer:BUFFER-FIELD("bEmpty"):BUFFER-VALUE AND bPrevLineEmpty THEN
    hBuffer:BUFFER-FIELD("bHidden"):BUFFER-VALUE = YES.
  bPrevLineEmpty = hBuffer:BUFFER-FIELD("bEmpty"):BUFFER-VALUE.
END.

DELETE OBJECT hQuery.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CountDashes C-Win 
FUNCTION CountDashes RETURNS CHARACTER
  ( cText AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iPos   AS INT NO-UNDO.
DEF VAR iCount AS INT NO-UNDO.
DEF VAR cList  AS CHAR NO-UNDO.

iPos = 1.
iCount = 0.

IF SUBSTRING(cText, iPos, 1) NE "-" THEN
  RETURN ''.

REPEAT WHILE NOT iPos > (LENGTH(cText) + 1):
  IF SUBSTRING(cText, iPos, 1) = "-" THEN DO:
    iCount = iCount + 1.
    iPos = iPos + 1.
  END.
  ELSE DO:
    cList = cList + STRING(iCount) + ";".  
    iPos = iPos + 1.
    iCount = 0.
  END.
END.

RETURN cList. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getColValues C-Win 
FUNCTION getColValues RETURNS CHARACTER
  ( cText AS CHAR, cColWidths AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix         AS INT NO-UNDO.
DEF VAR iPos       AS INT NO-UNDO.
DEF VAR iColW      AS INT NO-UNDO.
DEF VAR cColValues AS CHAR NO-UNDO.

iPos = 1.
DO ix = 1 TO (NUM-ENTRIES(cColWidths, ';':U) - 1):
  iColW = INT(ENTRY(ix, cColWidths, ';':U)).
  cColValues = cColValues + SUBSTRING(cText, iPos, iColW) + ';':U.
  iPos = iPos + 1 + iColW.
END.
  
RETURN cColValues.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDataType C-Win 
FUNCTION getDataType RETURNS CHARACTER
  ( cTestValues AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix     AS INT NO-UNDO.
DEF VAR dDec   AS DECIMAL NO-UNDO.
DEF VAR dtDate AS DATE NO-UNDO.

IF SUBSTRING(ENTRY(1, cTestValues,';'),1,1) = '' THEN DO: 
  DO ix = 1 TO NUM-ENTRIES(cTestValues,';') - 1:
    dDec = DECIMAL(ENTRY(ix, cTestValues,';')) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
      RETURN "CHARACTER".
  END.
  RETURN "DECIMAL".
END.
ELSE DO:
  DO ix = 1 TO NUM-ENTRIES(cTestValues,';') - 1:

    IF ENTRY(ix, cTestValues,';') NE "" THEN DO:
      IF NOT ((LENGTH(TRIM(ENTRY(ix, cTestValues,';'))) = 8 
              OR LENGTH(TRIM(ENTRY(ix, cTestValues,';'))) = 10) 
          AND (ENTRY(ix, cTestValues,';') MATCHES "*/*/*" )) 

         OR (LENGTH(TRIM(ENTRY(ix, cTestValues,';'))) = 10 
             AND SUBSTR(TRIM(ENTRY(ix, cTestValues,';')),5,1) = ".")

        THEN
        RETURN "CHARACTER".

      dDec = DECIMAL(ENTRY(ix, cTestValues,';')) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN RETURN "DECIMAL".
    END.
  END.
  RETURN "DATE".
END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadLine C-Win 
FUNCTION LoadLine RETURNS LOGICAL
  ( INPUT icInput AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Load a text line either from a temp-table or a file into the preview table
    Notes:  
------------------------------------------------------------------------------*/
IF ASC(SUBSTR(icInput,1,1)) = 27 THEN NEXT.

IF ASC(SUBSTR(icInput,1,1)) = 12 THEN DO:
  iPageLines = 0.

  IF cEndColumnHeader NE "" OR iNumHeaderLines NE 0 THEN
    bHideLines = YES.

END.
  
IF cStartColumnHeader NE "" AND icInput BEGINS cStartColumnHeader AND NOT bHideColHeaders THEN
  bHideLines = NO.

hBuffer:BUFFER-CREATE().
ASSIGN iPageLines = iPageLines + 1
       hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE     = icInput
       hBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE = cDropFile
       hBuffer:BUFFER-FIELD("iLnr"):BUFFER-VALUE      = iLineCnt
       hBuffer:BUFFER-FIELD("bEmpty"):BUFFER-VALUE    = icInput = ""  
       hBuffer:BUFFER-FIELD("bHidden"):BUFFER-VALUE   = bHideLines  
                                                        OR (iFooterLineNum > 0 AND iPageLines = iFooterLineNum)
                                                        OR ASC(icInput) = 12   
       iLineCnt   = iLineCnt + 1
       iCols      = MAX(iCols,LENGTH(icInput)) 
       .

IF bHideLines AND iNumHeaderLines NE 0 AND iPageLines = iNumHeaderLines THEN 
  bHideLines = NO.

ELSE IF bHideLines AND cEndColumnHeader NE "" AND icInput BEGINS cEndColumnHeader THEN
  bHideLines = NO.

IF cStartColumnHeader NE "" AND icInput BEGINS cStartColumnHeader THEN
  bHideColHeaders = YES.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadPreviewFromFile C-Win 
FUNCTION LoadPreviewFromFile RETURNS LOGICAL
  ( INPUT icFileName AS CHAR,
    INPUT ibAppend   AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cInput      AS CHAR NO-UNDO.

IF NOT ibAppend THEN DO:
  hBuffer:EMPTY-TEMP-TABLE().
  iLineCnt = 1.
END.

ASSIGN bHideColHeaders = NO
       bHideLines      = NO
       iPageLines      = 0
       .

INPUT STREAM strFile FROM VALUE(icFileName).

REPEAT:
  cInput = "".

  IMPORT STREAM strFile UNFORMATTED cInput.
  
  LoadLine(cInput).
END.

INPUT STREAM strFile CLOSE.

IF NOT bKeepEmptySpace THEN
  ClearEmptySpace().

RUN InvokeMethod(hBrowse,"OpenQuery").

cDropFile = "".

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadPreviewFromText C-Win 
FUNCTION LoadPreviewFromText RETURNS LOGICAL
  ( INPUT icText      AS CHAR,
    INPUT icDelimiter AS CHAR,
    INPUT ibAppend    AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cInput      AS CHAR NO-UNDO.
DEF VAR ix          AS INT  NO-UNDO.

IF NOT ibAppend THEN DO:
  hBuffer:EMPTY-TEMP-TABLE().
  iLineCnt = 1.
END.

ASSIGN bHideColHeaders = NO
       bHideLines      = NO
       iPageLines      = 0
       .

DO ix = 1 TO NUM-ENTRIES(icText,icDelimiter):
  LoadLine(ENTRY(ix,icText,icDelimiter)).
END.


IF NOT bKeepEmptySpace THEN
  ClearEmptySpace().

RUN InvokeMethod(hBrowse,"OpenQuery").

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadPreviewFromTT C-Win 
FUNCTION LoadPreviewFromTT RETURNS LOGICAL
  ( INPUT ihTT        AS HANDLE,
    INPUT icFieldName AS CHAR,
    INPUT ibAppend    AS LOG) :
/*------------------------------------------------------------------------------
  Purpose: Fill the preview table from either a temp-table-handle, buffer-handle or query-handle 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cInput      AS CHAR   NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR httBuff     AS HANDLE NO-UNDO.
DEF VAR bDeleteObj  AS LOG    NO-UNDO.

IF NOT VALID-HANDLE(ihTT) THEN RETURN NO.

IF NOT ibAppend THEN DO:
  hBuffer:EMPTY-TEMP-TABLE().
  iLineCnt = 1.
END.

IF icFieldName = "" THEN icFieldName = "cText".

ASSIGN bHideColHeaders = NO
       bHideLines      = NO
       iPageLines      = 0
       .

IF ihTT:TYPE = "buffer" THEN
  httBuff = ihTT.
ELSE IF ihTT:TYPE = "query" THEN
  httBuff = ihTT:GET-BUFFER-HANDLE(1).
ELSE 
  ASSIGN httBuff    = ihTT:DEFAULT-BUFFER-HANDLE
         bDeleteObj = YES.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(httBuff).
hQuery:QUERY-PREPARE("FOR EACH " + httBuff:NAME).
hQuery:QUERY-OPEN().

REPEAT WHILE hQuery:GET-NEXT():
  cInput = httBuff:BUFFER-FIELD(icFieldName):BUFFER-VALUE.

  LoadLine(cInput).

END.

DELETE OBJECT hQuery.

IF NOT bKeepEmptySpace THEN
  ClearEmptySpace().

RUN InvokeMethod(hBrowse,"OpenQuery").

IF bDeleteObj THEN DELETE OBJECT ihTT.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setEnableExcel C-Win 
FUNCTION setEnableExcel RETURNS LOGICAL
  ( INPUT ibEnableExcel AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bEnableExcel = ibEnableExcel.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setEndColumnHeader C-Win 
FUNCTION setEndColumnHeader RETURNS LOGICAL
  ( INPUT icEndColumnHeader AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Set the start of the column header in a paged report
    Notes: Rows from page-break to column header will be automatically hidden from preview
------------------------------------------------------------------------------*/
cEndColumnHeader = icEndColumnHeader.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFooterLineNum C-Win 
FUNCTION setFooterLineNum RETURNS LOGICAL
  ( INPUT iiFooterLineNum AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
iFooterLineNum = iiFooterLineNum.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setHeadersHidden C-Win 
FUNCTION setHeadersHidden RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hQuery AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " BY iLnr").
hQuery:QUERY-OPEN().
REPEAT WHILE hQuery:GET-NEXT():
  IF ASC(SUBSTR(hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE,1,1)) = 12 THEN 
    ASSIGN iPageLines = 0
           bHideLines = YES.

  ASSIGN iPageLines = iPageLines + 1
         hBuffer:BUFFER-FIELD("bHidden"):BUFFER-VALUE = bHideLines
         .

  IF bHideLines AND cEndColumnHeader NE "" AND hBuffer:BUFFER-FIELD("cText"):BUFFER-VALUE BEGINS cEndColumnHeader THEN
    bHideLines = NO.

END.

DELETE OBJECT hQuery.
RUN InvokeMethod(hBrowse,"OpenQuery").

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setKeepEmptySpace C-Win 
FUNCTION setKeepEmptySpace RETURNS LOGICAL
  ( INPUT ibKeepEmptySpace AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bKeepEmptySpace = ibKeepEmptySpace.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setNumHeaderLines C-Win 
FUNCTION setNumHeaderLines RETURNS LOGICAL
  ( INPUT iiNumHeaderLines AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
iNumHeaderLines = iiNumHeaderLines.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setOrientation C-Win 
FUNCTION setOrientation RETURNS LOGICAL
  ( INPUT icOrientation AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cOrientation = icOrientation.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setStartColumnHeader C-Win 
FUNCTION setStartColumnHeader RETURNS LOGICAL
  ( INPUT icStartColumnHeader AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  If the column headers don't appear on the first page of the report
            this setting is needed to determine when to start hiding them
    Notes:  
------------------------------------------------------------------------------*/
cStartColumnHeader = icStartColumnHeader.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setViewHeadersInExcel C-Win 
FUNCTION setViewHeadersInExcel RETURNS LOGICAL
  ( INPUT ibViewHeaderInExcel AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bViewHeaderInExcel = ibViewHeaderInExcel.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setWindowTitle C-Win 
FUNCTION setWindowTitle RETURNS LOGICAL
  ( INPUT icWinTitle AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
THIS-PROCEDURE:CURRENT-WINDOW:TITLE = icWinTitle.

IF VALID-HANDLE(hBrowse) THEN
  hBrowse:HELP = icWinTitle.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

