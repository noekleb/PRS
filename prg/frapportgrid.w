&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS fFrameWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrfrm.w - ADM2 SmartFrame Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

 
 asdf
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

DEFINE VARIABLE iSortCol         AS INTEGER                  NO-UNDO.
DEFINE VARIABLE iSortOrder       AS INTEGER                  NO-UNDO.

DEFINE VARIABLE iMouseDownRow    AS INTEGER                  NO-UNDO.
DEFINE VARIABLE cSumWhatSave     AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cColLabelSave    AS CHARACTER                NO-UNDO.
DEFINE VARIABLE dCtrlWidth       AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE cXSolgt%         AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cTitle           AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cKundenavn       AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cPolygon         AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cExtraInfo       AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cPreSelectCols   AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cPrintKunCols    AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cPrintNoHiddCols AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cVisSave         AS CHARACTER                NO-UNDO.
DEFINE VARIABLE h_Window         AS HANDLE                   NO-UNDO.
DEFINE VARIABLE iColourSort      AS INTEGER INIT 16777215    NO-UNDO.   /* Vit */
DEFINE VARIABLE cSprak           AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cOldLabels       AS CHARACTER                NO-UNDO.
DEFINE VARIABLE hOversettGrid2SE AS HANDLE                   NO-UNDO.
DEFINE VARIABLE lOversett        AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE cToday           AS CHARACTER FORMAT "X(10)" NO-UNDO.
DEFINE VARIABLE dColPos          AS DECIMAL EXTENT 90        NO-UNDO.
DEFINE VARIABLE dColPos2         AS DECIMAL EXTENT 90        NO-UNDO.
DEFINE VARIABLE iMaxBredd        AS DECIMAL EXTENT 90        NO-UNDO.
DEFINE VARIABLE cHeaderTxt       AS CHARACTER FORMAT "X(35)" NO-UNDO.
DEFINE VARIABLE cSidTxt          AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cRub             AS CHARACTER EXTENT 20      NO-UNDO.
DEFINE VARIABLE iMaxRub          AS INTEGER                  NO-UNDO.
DEFINE VARIABLE dPageWidth       AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE dY               AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE wrk              AS CHARACTER                NO-UNDO.
DEFINE VARIABLE iLastRow         AS INTEGER                  NO-UNDO.
DEFINE VARIABLE iLastRow2        AS INTEGER                  NO-UNDO.
DEFINE VARIABLE dBelopp          AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE iAntal           AS INTEGER FORMAT "->>>>9"  NO-UNDO.
DEFINE VARIABLE cUL              AS CHARACTER                NO-UNDO.
DEFINE VARIABLE iLen             AS INTEGER                  NO-UNDO.
DEFINE VARIABLE cStr             AS CHARACTER                NO-UNDO.
DEFINE VARIABLE iSkoModus        AS INTEGER                  NO-UNDO.
DEFINE VARIABLE lSummarad        AS LOGICAL     NO-UNDO.

DEFINE STREAM Ut.

DEFINE FRAME PageHeader
   HEADER
      "<ALIGN=BASE><FArial><R4><P12><B><C6>" STRING(TODAY)
      "<P12></B><C110><P10>" PAGE-NUMBER FORMAT ">>" SKIP
      "<R5><C6><FROM><R5><C113><LINE>" SKIP
      WITH PAGE-TOP STREAM-IO WIDTH 255.

DEFINE TEMP-TABLE TT_RapportRader NO-UNDO
    FIELD iPageNum AS INTEGER  /* Sidnr */
    FIELD iColPage AS INTEGER  /* Hantering av 'för många cols' */
    FIELD iRadNum  AS INTEGER
    FIELD cRadData AS CHARACTER
    FIELD lLast   AS LOG 
    INDEX RadNum iPageNum iColPage iRadNum.

DEFINE TEMP-TABLE TT_Preselect
    FIELD cProgName  AS CHARACTER
    FIELD iFane      AS INTEGER
    FIELD ColLabels  AS CHARACTER
    FIELD cPreSelect AS CHARACTER
    INDEX ProgFane cProgName iFane.

DEFINE TEMP-TABLE TT_Apotek NO-UNDO
    FIELD cFsgStalle AS CHARACTER
    FIELD cButik     AS CHARACTER
    FIELD cPeriodAr  AS CHARACTER
    FIELD cPeriodMan AS CHARACTER
    FIELD cVersion   AS CHARACTER
    FIELD cArtNr     AS CHARACTER
    FIELD cAntal     AS CHARACTER
    FIELD cBelopp    AS CHARACTER
    INDEX Apoteki cPeriodAr cButik cPeriodMan.

 {windows.i}

  { pdf_inc.i "THIS-PROCEDURE"}.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD bredd Procedure 
FUNCTION bredd RETURNS DECIMAL
  ( INPUT cText AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD bredd fFrameWin 
FUNCTION bredd RETURNS DECIMAL
    ( INPUT cText AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTxtFrame fFrameWin 
FUNCTION getTxtFrame RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SummerCol fFrameWin 
FUNCTION SummerCol RETURNS CHARACTER
  ( INPUT iCol AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE Resultat AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chResultat AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FI-Txt AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 75 BY 1.38
     FGCOLOR 9 FONT 8 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 201.2 BY 21.14.

DEFINE FRAME FRAME-Txt
     FI-Txt AT ROW 1.81 COL 1 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 48.2 ROW 8.29
         SIZE 79 BY 2.86.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW fFrameWin ASSIGN
         HEIGHT             = 21.14
         WIDTH              = 201.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB fFrameWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW fFrameWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-Txt:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME fMain
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME fMain:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-Txt
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-Txt:HIDDEN           = TRUE
       FRAME FRAME-Txt:SENSITIVE        = FALSE.

/* SETTINGS FOR FILL-IN FI-Txt IN FRAME FRAME-Txt
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FI-Txt:HIDDEN IN FRAME FRAME-Txt           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-Txt
/* Query rebuild information for FRAME FRAME-Txt
     _Query            is NOT OPENED
*/  /* FRAME FRAME-Txt */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME Resultat ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 1
       COLUMN          = 1
       HEIGHT          = 20.95
       WIDTH           = 201
       HIDDEN          = no
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      Resultat:NAME = "Resultat":U .
/* Resultat OCXINFO:CREATE-CONTROL from: {0F026C11-5A66-4c2b-87B5-88DDEBAE72A1} type: VSFlexGrid */
      Resultat:MOVE-BEFORE(FRAME FRAME-Txt:HANDLE).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Resultat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Resultat fFrameWin OCX.MouseDown
PROCEDURE Resultat.VSFlexGrid.MouseDown .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Button
    Shift
    X
    Y
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Button AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-Shift  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-X      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER p-Y      AS DECIMAL NO-UNDO.

    ASSIGN iMouseDownRow = chResultat:MouseRow.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Resultat fFrameWin OCX.MouseUp
PROCEDURE Resultat.VSFlexGrid.MouseUp .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Button
    Shift
    X
    Y
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Button AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-Shift  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-X      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER p-Y      AS DECIMAL NO-UNDO.
DEFINE       VARIABLE  r1       AS INTEGER    NO-UNDO.
DEFINE       VARIABLE  r2       AS INTEGER    NO-UNDO.
DEFINE       VARIABLE  iCount   AS INTEGER    NO-UNDO.
  
  IF chResultat:Rows = 1 OR chResultat:MouseCol = -1 THEN
      RETURN.
  IF p-Button = 2 AND chResultat:MouseRow = 0 /* AND TRIM(cSumWhatSave) = "" */ THEN DO:
      MESSAGE 
      SummerCol(chResultat:MouseCol)
          VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Sum col: " + chResultat:TextMatrix(0,chResultat:MouseCol).
      RETURN.
  END.
  IF chResultat:MouseRow = 0 THEN DO:
    IF chResultat:MouseCol = 0 AND chResultat:Cell(5,0,0,0,0) = 1 THEN DO:
        ASSIGN chResultat:Cell(5,0,0,0,0) = 0.
        /* Ta bort sista raden = sumrad */
        IF chResultat:IsSubtotal(chResultat:Rows - 1) = TRUE THEN
        chResultat:RemoveItem(chResultat:Rows - 1).
        /* Återställ rader till det normala */
        DO iCount = 1 TO chResultat:Rows - 1:
            chResultat:RowHidden(iCount) = FALSE.
        END.
        RETURN.
    END.
    /* Återställ sorteringsfärg */
    ASSIGN chResultat:Cell(6,0,iSortCol,0,iSortCol) = 0.
    IF iSortCol   = chResultat:MouseCol THEN
      ASSIGN iSortOrder = IF iSortOrder = 1 THEN 2 ELSE 1.
    ELSE 
      ASSIGN iSortOrder = 1.
    ASSIGN chResultat:ROW = 1
           chResultat:RowSel = 1
           chResultat:COL    = chResultat:MouseCol
           chResultat:ColSel = chResultat:COL
           chResultat:SORT   = iSortOrder
           iSortCol          = chResultat:MouseCol
           chResultat:TopRow = 1.
    /* Sätt sorteringsfärg */
    ASSIGN chResultat:Cell(6,0,iSortCol,0,iSortCol) = iColourSort.
  END.
  ELSE IF chResultat:MouseRow > 0 AND iMouseDownRow <> chResultat:MouseRow AND p-Shift <> 2 /* chResultat:MouseCol = 0 AND chResultat:Cell(5,0,0,0,0) = 0 */ THEN DO:
      /* IF iMouseDownRow <> chResultat:MouseRow THEN */ DO:
          ASSIGN r1 = MIN(iMouseDownRow,chResultat:MouseRow)
                 r2 = MAX(iMouseDownRow,chResultat:MouseRow)
                 r2 = r2 - IF r2 = chResultat:Rows - 1 AND 
              chResultat:IsSubtotal(chResultat:Rows - 1) = TRUE
              THEN 1 ELSE 0.
          IF chResultat:Cell(5,0,0,0,0) = 1 AND 
              chResultat:IsSubtotal(chResultat:Rows - 1) = TRUE
              THEN
              chResultat:RemoveItem(chResultat:Rows - 1).
          ASSIGN chResultat:Cell(5,0,0,0,0) = 1.
          IF r1 > 1 THEN DO iCount = 1 TO r1 - 1:
              chResultat:RowHidden(iCount) = TRUE.
          END.
          DO iCount = r2 + 1 TO chResultat:Rows - 1:
              chResultat:RowHidden(iCount) = TRUE.
          END.
          IF chResultat:IsSubTotal(chResultat:Rows - 1) THEN
              RUN Summer (cSumWhatSave,cColLabelSave).
      END.
  END.
  ELSE IF chResultat:MouseRow > 0 AND p-Shift = 2 /*  */ THEN DO:
      DO:
          ASSIGN r1 = MIN(iMouseDownRow,chResultat:MouseRow)
                 r2 = MAX(iMouseDownRow,chResultat:MouseRow)
                 r2 = r2 - IF r2 = chResultat:Rows - 1 AND
              chResultat:IsSubtotal(chResultat:Rows - 1) = TRUE
              THEN 1 ELSE 0.
          IF chResultat:Cell(5,0,0,0,0) = 1 AND 
             chResultat:IsSubtotal(chResultat:Rows - 1) = TRUE THEN
              chResultat:RemoveItem(chResultat:Rows - 1).
          ASSIGN chResultat:Cell(5,0,0,0,0) = 1.
          DO iCount = r1 TO r2:
              chResultat:RowHidden(iCount) = TRUE.
          END.
          IF chResultat:IsSubtotal(chResultat:Rows - 1) = TRUE THEN
              chResultat:RowHidden(chResultat:Rows - 1) = TRUE.
          IF chResultat:IsSubTotal(chResultat:Rows - 1) THEN
              RUN Summer (cSumWhatSave,cColLabelSave).
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK fFrameWin 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects fFrameWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AlignCol fFrameWin 
PROCEDURE AlignCol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iAlignCol AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER iAlignHow AS INTEGER    NO-UNDO.
    /* vi justerar alignment för col, inte label */
    IF chResultat:Rows < 2 THEN
        RETURN.
/*     ASSIGN chResultat:ColAlignment(iAlignCol) = iAlignHow. */
    ASSIGN chResultat:Cell(2,0,iAlignCol,chResultat:Rows - 1,iAlignCol) = iAlignHow.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearGrid fFrameWin 
PROCEDURE ClearGrid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cLabels   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE        cSELabels AS CHARACTER NO-UNDO.
    DEFINE VARIABLE        iCount    AS INTEGER   NO-UNDO.

/*     IF lOversett AND cOldLabels <> cLabels THEN DO: */
/*     OUTPUT TO c:\temp\ken1\LABELS.txt APPEND. */
/*     PUT UNFORMATTED cLabels SKIP.             */
/*     OUTPUT CLOSE.                             */
/*     IF lOversett THEN */
    DO:
        cOldLabels = cLabels.
        cSELabels = DYNAMIC-FUNCTION('Oversett2SE', INPUT cLabels) NO-ERROR.
        IF NUM-ENTRIES(cLabels) = NUM-ENTRIES(cSELabels) THEN
            cLabels = cSELabels.
    END.
    
    ASSIGN cColLabelSave = "".
    chResultat:CLEAR(2).
    chResultat:Rows = 1.
    chResultat:Cols = NUM-ENTRIES(cLabels) + 1.
    DO iCount = 1 TO NUM-ENTRIES(cLabels):
        ASSIGN chResultat:Cell(0,0,iCount,0,iCount) = ENTRY(iCount,cLabels).
    END.
    /* Om vi har en checkbox tas den bort */
    ASSIGN chResultat:Cell(5,0,0,0,0) = 0
           chResultat:FrozenCols = 0.
    /* Återställ sorteringsfärger */
    ASSIGN chResultat:Cell(6,0,iSortCol,0,iSortCol) = 0 NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ColVerdier fFrameWin 
PROCEDURE ColVerdier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER cColVerdier AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cCol        AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  iCol        AS INTEGER    NO-UNDO.
DEFINE        VARIABLE  iRow        AS INTEGER    NO-UNDO.

ASSIGN iCol = INT(cCol).
DO iRow = 1 TO chResultat:Rows - 2:
    IF chResultat:RowHidden(iRow) THEN
        NEXT.
    ASSIGN cColVerdier = cColVerdier + (IF cColVerdier <> "" THEN CHR(1) ELSE "") + chResultat:Cell(0,iRow,iCol,iRow,iCol).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load fFrameWin  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "frapportgrid.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chResultat = Resultat:COM-HANDLE
    UIB_S = chResultat:LoadControls( OCXFile, "Resultat":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "frapportgrid.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject fFrameWin 
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.
  RUN ClearGrid (",,,,").
  /* Code placed here will execute AFTER standard behavior.    */
  IF VALID-HANDLE(hOversettGrid2SE) THEN
      DELETE PROCEDURE hOversettGrid2SE.
  IF VALID-HANDLE(chResultat) THEN
     RELEASE OBJECT chResultat NO-ERROR.
  IF VALID-HANDLE(Resultat) THEN
     DELETE OBJECT Resultat NO-ERROR.
  ASSIGN chResultat = ?
         Resultat   = ?.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI fFrameWin  _DEFAULT-DISABLE
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
  HIDE FRAME fMain.
  HIDE FRAME FRAME-Txt.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoBigger fFrameWin 
PROCEDURE DoBigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      FRAME fMain:HEIGHT = 1.1 * FRAME fMain:HEIGHT.
      Resultat:HEIGHT = 1.1 * Resultat:HEIGHT.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI fFrameWin  _DEFAULT-ENABLE
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
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Txt}
  FRAME FRAME-Txt:SENSITIVE = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndreSize fFrameWin 
PROCEDURE EndreSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER dHeight AS DECIMAL    NO-UNDO.
   DEFINE INPUT  PARAMETER dWidth  AS DECIMAL    NO-UNDO.
   DEFINE        VARIABLE  dKoeff  AS DECIMAL    NO-UNDO.

   IF dWidth > 0 THEN DO:
       ASSIGN dKoeff             = dHeight / FRAME fMain:HEIGHT
              FRAME fMain:HEIGHT = dHeight
              FRAME fMain:WIDTH  = dWidth.
       ASSIGN Resultat:WIDTH = dWidth - (dCtrlWidth - DYNAMIC-FUNCTION('getMinWidth':U))
              Resultat:HEIGHT = MAX(Resultat:HEIGHT,Resultat:HEIGHT * dKoeff).
   END.
   ELSE DO:
       ASSIGN FRAME fMain:HEIGHT = dHeight
              Resultat:HEIGHT    = dHeight - .2.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndreSize2 fFrameWin 
PROCEDURE EndreSize2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER dHeight AS DECIMAL    NO-UNDO.
   DEFINE INPUT  PARAMETER dWidth  AS DECIMAL    NO-UNDO.
   DEFINE        VARIABLE  dKoeff  AS DECIMAL    NO-UNDO.

   ASSIGN FRAME fMain:HEIGHT = THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT - 2
          Resultat:HEIGHT = FRAME fMain:HEIGHT - 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FeltVerdier fFrameWin 
PROCEDURE FeltVerdier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER cFeltVerdier AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cColNr       AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cRettning    AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  iCount       AS INTEGER    NO-UNDO.

IF NOT (chResultat:ROW > 0 AND chResultat:ROW < chResultat:Rows) THEN
    RETURN.
IF cRettning = "PREV" AND chResultat:ROW > 1 THEN DO:
    ASSIGN chResultat:ROW = chResultat:ROW - 1
           chResultat:RowSel = chResultat:ROW.
END.
ELSE IF cRettning = "NEXT" AND chResultat:ROW < chResultat:Rows - IF cSumWhatSave = "" THEN 1 ELSE 2 THEN DO:
    ASSIGN chResultat:ROW = chResultat:ROW + 1
           chResultat:RowSel = chResultat:ROW.
END.
DO iCount = 1 TO NUM-ENTRIES(cColNr):
    ASSIGN cFeltVerdier = cFeltVerdier + (IF cFeltVerdier = "" THEN "" ELSE ",") 
          + chResultat:Cell(0,chResultat:ROW,INT(ENTRY(iCount,cColNr)),chResultat:ROW,INT(ENTRY(iCount,cColNr))).
END.
    IF cRettning = "PREV" AND NOT chResultat:RowIsVisible(chResultat:ROW) THEN
        chResultat:TopRow = chResultat:ROW.
    ELSE IF cRettning = "NEXT" THEN DO:
        IF chResultat:ROWS = chResultat:ROW + 1 THEN
            chResultat:TopRow = chResultat:TopRow + 2.
        ELSE IF chResultat:ROWS > chResultat:ROW + 1 AND NOT chResultat:RowIsVisible(chResultat:ROW + 1) THEN
            chResultat:TopRow = chResultat:TopRow + 1.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillTT fFrameWin 
PROCEDURE FillTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER hTT_Table    AS HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER cFieldName   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cCol         AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cDataType    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iUrval       AS INTEGER    NO-UNDO.
DEFINE        VARIABLE  iRow         AS INTEGER    NO-UNDO.
DEFINE        VARIABLE  iCol         AS INTEGER    NO-UNDO.

ASSIGN iRow = IF iUrval = 1 THEN chResultat:ROW ELSE 1
       iCol = INT(cCol).
DO iRow = iRow TO IF iUrval = 1 THEN chResultat:ROW ELSE chResultat:Rows - 2:
    IF chResultat:RowHidden(iRow) THEN
        NEXT.
    hTT_Table:BUFFER-CREATE().
    hTT_Table:BUFFER-FIELD(cFieldName):BUFFER-VALUE() = IF cDataType = "DECI" THEN DECI(chResultat:Cell(0,iRow,iCol,iRow,iCol))
                                                   ELSE IF cDataType = "INT" THEN INT(chResultat:Cell(0,iRow,iCol,iRow,iCol))
                                                       ELSE INT(chResultat:Cell(0,iRow,iCol,iRow,iCol)).
END.
hTT_Table:BUFFER-RELEASE().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillTTFelter fFrameWin 
PROCEDURE FillTTFelter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER hTT_Table    AS HANDLE    NO-UNDO.
DEFINE INPUT  PARAMETER cFieldName   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER cCols        AS CHARACTER NO-UNDO.
DEFINE        VARIABLE  iUrval       AS INTEGER   NO-UNDO.
DEFINE        VARIABLE  iRow         AS INTEGER   NO-UNDO.
DEFINE        VARIABLE  iCol         AS INTEGER   NO-UNDO.
DEFINE        VARIABLE  ii           AS INTEGER   NO-UNDO.
DEFINE        VARIABLE  cTxt         AS CHARACTER NO-UNDO.

ASSIGN iRow = IF iUrval = 1 THEN chResultat:ROW ELSE 1.
DO iRow = iRow TO IF iUrval = 1 THEN chResultat:ROW ELSE chResultat:Rows - 2:
    IF chResultat:RowHidden(iRow) THEN
        NEXT.
    cTxt = "".
    DO ii = 1 TO NUM-ENTRIES(cCols):
        cTxt = cTxt + (IF cTxt <> "" THEN CHR(1) ELSE "") + chResultat:Cell(0,iRow,INT(ENTRY(ii,cCols)),iRow,INT(ENTRY(ii,cCols))).
    END.
    hTT_Table:BUFFER-CREATE().
    hTT_Table:BUFFER-FIELD(cFieldName):BUFFER-VALUE() = cTxt.
    hTT_Table:BUFFER-RELEASE(). 
END.
hTT_Table:BUFFER-RELEASE().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getVisibelColValues fFrameWin 
PROCEDURE getVisibelColValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER iCol AS INTEGER     NO-UNDO.
  DEFINE OUTPUT PARAMETER colValues AS LONGCHAR     NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER     NO-UNDO.
  DO iCount = 1 TO chResultat:rows - IF TRIM(cSumWhatSave) = "" THEN 1 ELSE 2:
      IF chResultat:RowHidden(iCount) = TRUE THEN
          NEXT.
      colValues = colValues + (IF colValues <> "" THEN "," ELSE "") + chResultat:TextMatrix(iCount,iCol).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls fFrameWin 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN chResultat = chResultat:VSFlexGrid
           chResultat:FixedCols =  0
           chResultat:GridLines = 0
           chResultat:Rows = 1
           chResultat:Cols = 12
           chResultat:MergeCol(1) = TRUE
           chResultat:BorderStyle = 1
           chResultat:MergeCells = 2
           chResultat:SelectionMode = 1
           chResultat:AutoSearch = 1
           chResultat:RowHeight(0) = chResultat:RowHeight(0) * 1.1
/*            chResultat:ExplorerBar = 5 */
           chResultat:ClipSeparators = "|" + CHR(13).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject fFrameWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN dCtrlWidth = Resultat:WIDTH.
  PUBLISH "GetWindowH" (OUTPUT h_Window ).
/*  RUN  GetBrukerLng IN h_Window (OUTPUT cSprak) NO-ERROR. */ /* GetBrukerLng finns i wRapportGen.w */
  FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
  IF AVAIL bruker THEN
     cSprak = TRIM(Bruker.Lng).

  IF CAN-DO("SE,SVE",cSprak) THEN
      lOversett = TRUE.
/*       RUN oversettGrid2SE.p PERSISTENT SET hOversettGrid2SE. */
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  {syspara.i 1 1 100 cKundenavn}
  {syspara.i 1 1 101 cPolygon}
  {syspara.i 1 1 54 iSkoModus INT}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadGrid fFrameWin 
PROCEDURE LoadGrid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cFileName    AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER iFrozencCols AS INTEGER    NO-UNDO.

    ASSIGN chResultat:Cell(5,0,0,0,0) = 0 /* Om vi har en checkbox tas den bort */
           chResultat:Rows = 1
           chResultat:Cols = 1
           cSumWhatSave    = ""
           cXSolgt%        = "".
    chResultat:LoadGrid(cFileName,5,TRUE).
    IF chResultat:Rows > 1 THEN DO:
      ASSIGN chResultat:FrozenCols = iFrozencCols
             chResultat:ROW        = 1
             chResultat:RowSel     = 1
             chResultat:COL        = 0
             chResultat:ColSel     = chResultat:COL
             chResultat:SORT       = 1
             iSortCol              = chResultat:COL
             iSortOrder            = 1.
    END.
/*     chResultat:RowHeight(0) = chResultat:RowHeight(0) * 2. */

/*         ASSIGN chResultat:ColAlignment(2) = 8. */


    cVisSave = "".
    IF VALID-HANDLE(h_Window) THEN
        RUN VisVisAlleKnapp IN h_Window (FALSE).
    APPLY "ENTRY" TO Resultat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFNumCheck fFrameWin 
PROCEDURE PDFNumCheck :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cText    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER lNumeric AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER lDecimal AS LOGICAL   NO-UNDO.
DEFINE VARIABLE         c1       AS CHARACTER NO-UNDO.
DEFINE VARIABLE         ix       AS INTEGER   NO-UNDO.

ASSIGN lNumeric = TRUE.
ASSIGN lDecimal = FALSE.

DO ix = 1 TO LENGTH(cText):
   ASSIGN c1 = SUBSTRING(cText,ix,1).
   IF c1 > "9"  OR c1 = "/" THEN
     ASSIGN lNumeric = FALSE.
   IF c1 = "." OR c1 = "," THEN
     ASSIGN lDecimal = TRUE.
END.

ASSIGN ix = LENGTH(cText).
  IF ix >= 10 THEN
    ASSIGN lNumeric = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFNumCheck2 fFrameWin 
PROCEDURE PDFNumCheck2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cText     AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER lNumeric  AS LOGICAL     NO-UNDO.
DEFINE OUTPUT PARAMETER lDecimal  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE         dTst      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE         ix        AS INTEGER     NO-UNDO.
DEFINE VARIABLE         c1        AS CHARACTER   NO-UNDO.
IF TRIM(cText) <> "" THEN DO:
    dTst = DEC(cText) NO-ERROR.

    ASSIGN lNumeric = ERROR-STATUS:ERROR.

    ASSIGN lDecimal = FALSE.
    IF lNumeric = FALSE THEN
      ASSIGN cText = TRIM(REPLACE(cText,".",",")).
      DO ix = 1 TO LENGTH(cText):
        ASSIGN c1 = SUBSTRING(cText,ix,1).
        IF c1 = "," THEN
          ASSIGN lDecimal = TRUE.
      END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFPageFooter fFrameWin 
PROCEDURE PDFPageFooter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
/*  RUN pdf_set_dash IN h_PDFinc ("Spdf",1,0).*/
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_BottomMargin ("Spdf"), pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , pdf_BottomMargin ("Spdf"), 0.5).

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).  
  RUN pdf_text_xy_dec ("Spdf",cKundeNavn,pdf_LeftMargin ("Spdf"),pdf_BottomMargin ("Spdf") - 14).
  RUN pdf_text_xy_dec ("Spdf",cPolygon,pdf_Pagewidth ("Spdf") - pdf_LeftMargin ("Spdf") - 500,pdf_BottomMargin ("Spdf") - 14).
  cSidTxt = TRIM("Sida: " + STRING(pdf_page("Spdf")) + " (" + pdf_TotalPages("Spdf") + ")").
  RUN pdf_text_xy_dec ("Spdf",cSidTxt,pdf_Pagewidth ("Spdf") - pdf_LeftMargin ("Spdf") - 50,pdf_BottomMargin ("Spdf") - 14).

END PROCEDURE. /* PDFPageFooter */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFPageHeader fFrameWin 
PROCEDURE PDFPageHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",20).
  RUN pdf_text_xy_dec ("Spdf",cToday,dColPos[1],pdf_PageHeight("Spdf") - 45).
  RUN pdf_text_xy_dec ("Spdf",cHeaderTxt,dColPos[2] + 200,pdf_PageHeight("Spdf") - 45).
  cSidTxt = TRIM(STRING(pdf_page("Spdf"))).
  RUN pdf_text_xy_dec ("Spdf",cSidTxt,800,pdf_PageHeight("Spdf") - 45).
  
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_PageHeight ("Spdf") - 50, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , pdf_PageHeight ("Spdf") - 50, 0.5).

END PROCEDURE. /* PDFPageHeader */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFSetPositioner fFrameWin 
PROCEDURE PDFSetPositioner :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN dColPos[1] = pdf_LeftMargin ("Spdf")  
         dColPos[2] = 100                      
         dColPos[3] = 400                      
         dColPos[4] = 530                      
         dColPos[5] = 600
         dColPos[6] = 122.

END PROCEDURE. /* PDFSetPositioner */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFXprintRapp2 fFrameWin 
PROCEDURE PDFXprintRapp2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER cUrvalsInfo    AS CHARACTER           NO-UNDO.
   DEFINE INPUT PARAMETER cColWidth%     AS CHARACTER           NO-UNDO.
   DEFINE INPUT PARAMETER cPageCols      AS CHARACTER           NO-UNDO. /* vid för bred grid för XPrintrapport */
   DEFINE INPUT PARAMETER cRightCols     AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  pcRappFil      AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  iCount         AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iCount0        AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iCount2        AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  cLabelStr      AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  cLabelStrTMP   AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  cRowStr        AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  cFormatStr     AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  cFormatStrTMP  AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  iColTmp        AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iCount3        AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iFirstSumCol   AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iFirstPageCol  AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iCountPageCols AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  cRight         AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  iSidNr         AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iRadNr         AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  lMindrePG1     AS LOGICAL             NO-UNDO.
   DEFINE       VARIABLE  dColPos3       AS DECIMAL EXTENT 90   NO-UNDO.
   DEFINE       VARIABLE  iRight         AS INTEGER EXTENT 90   NO-UNDO.
   DEFINE       VARIABLE  cPage          AS CHARACTER EXTENT 10 NO-UNDO.
   DEFINE       VARIABLE  iPageLcol      AS INTEGER EXTENT 10   NO-UNDO.  /* Första kolumn per sida */
   DEFINE       VARIABLE  iPageRcol      AS INTEGER EXTENT 10   NO-UNDO.  /* Sista kolumn per sida */
   DEFINE       VARIABLE  iMaxCol        AS INTEGER             NO-UNDO.  /* Totalt antal kolumner alla sidor */
   DEFINE       VARIABLE  iNumCol        AS INTEGER EXTENT 10   NO-UNDO.  /* Antal kolumner per sida */
   DEFINE       VARIABLE  iAddCol        AS INTEGER EXTENT 10   NO-UNDO.  /* Kolumner att addera */
   DEFINE       VARIABLE  lNumeric       AS LOGICAL             NO-UNDO.
   DEFINE       VARIABLE  lDecimal       AS LOGICAL             NO-UNDO.
   DEFINE       VARIABLE  lLast          AS LOGICAL             NO-UNDO.
   DEFINE       VARIABLE  cLastFieldWidth AS CHARACTER          NO-UNDO.
   DEFINE       VARIABLE  iLeftMargin    AS INTEGER             NO-UNDO.

/*    MESSAGE "cPageCols " cPageCols         */
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK. */

   ASSIGN lLast = FALSE.
   IF cExtraInfo <> "" THEN
       ASSIGN lMindrePG1 = TRUE.
   IF NUM-ENTRIES(cRightCols) <> chResultat:Cols - 1 THEN DO:
       ASSIGN cRightCols = FILL(",",chResultat:Cols - 2).
   END.
 DO iCount = 1 TO NUM-ENTRIES(cPageCols,";"):
   ASSIGN cPage[iCount] = ENTRY(iCount,cPageCols,";").
   ASSIGN iPageLcol[iCount] = INT(ENTRY(1,cPage[iCount],",")).
   ASSIGN iPageRcol[iCount] = INT(ENTRY(2,cPage[iCount],",")).
   ASSIGN iNumCol[iCount] = iPageRcol[iCount] - iPageLcol[iCount] + 1.
 END.

 ASSIGN iAddCol[1] = 0.
 IF NUM-ENTRIES(cPageCols,";") > 1 THEN
 DO iCount = 2 TO NUM-ENTRIES(cPageCols,";"):
   ASSIGN iAddCol[iCount] = iAddCol[iCount - 1] + iNumCol[iCount - 1].
 END.

/* MESSAGE "cRightCols" SKIP cRightCols
     VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
   EMPTY TEMP-TABLE TT_RapportRader.
   ASSIGN iFirstSumCol = IF cColLabelSave <> "" THEN INT(ENTRY(1,cColLabelSave)) + 1 ELSE ?.

/*           cTitle = "Statistikk". */
   DO iCount = 1 TO NUM-ENTRIES(cPageCols,";"):
       ASSIGN cFormatStrTMP = "6"
              iColTmp       = 6
              iCount3       = 0.
       DO iCount2 = INT(ENTRY(1,ENTRY(iCount,cPageCols,";"))) TO INT(ENTRY(2,ENTRY(iCount,cPageCols,";"))):
           ASSIGN iCount3    = ROUND(DECI(ENTRY(iCount2,cColWidth%,CHR(1))) * 105 / 100,0)
                  iColTmp    = iColTmp + iCount3
                  cFormatStrTMP = cFormatStrTMP + "," + STRING(iColTmp).
       END.
       cLastFieldWidth = cLastFieldWidth + (IF cLastFieldWidth <> "" THEN "," ELSE "") + STRING(ROUND(iCount3 * 7.26,0)).
/*        MESSAGE "cFormatStr" cFormatStr SKIP       */
/*                "cFormatStrTMP" cFormatStrTMP SKIP */
/*            cPageCOls                              */
/*            SKIP cColWidth%                        */
/*            VIEW-AS ALERT-BOX INFO BUTTONS OK.     */
       ASSIGN cFormatStr = cFormatStr + (IF cFormatStr = "" THEN "" ELSE ";") + cFormatStrTMP.
   END.
   /* Finn lablar */
   ASSIGN iMaxCol = 0.
   DO iCount = 1 TO NUM-ENTRIES(cPageCols,";"):
       ASSIGN cLabelStrTMP = ""
              iFirstPageCol = INT(ENTRY(1,ENTRY(iCount,cPageCols,";"))) - 1.
       DO iCount2 = 1 TO NUM-ENTRIES(ENTRY(iCount,cFormatStr,";")) - 1:
         ASSIGN iMaxCol = iMaxCol + 1.
         ASSIGN dColPos3[iMaxCol] = DEC(ENTRY(iCount2,ENTRY(iCount,cFormatStr,";"))).
           ASSIGN cLabelStrTMP = cLabelStrTMP + chResultat:Cell(0,0,iCount2 + iFirstPageCol,0,iCount2 + iFirstPageCol) + ";".
       END.
       ASSIGN cLabelStr = cLabelStr + cLabelStrTMP.
   END.
   IF lOversett = TRUE THEN
       ASSIGN cToday = STRING(YEAR(TODAY),"9999") + "-"
                     + STRING(MONTH(TODAY),"99") + "-"
                     + STRING(DAY(TODAY),"99").
     ELSE
       ASSIGN cToday = STRING(TODAY).
   ASSIGN iLastRow = 0.
   ASSIGN iSidNr = 1
          iRadNr = 1.
   DO iCount = 1 TO chResultat:Rows - 1:
       IF chResultat:RowHidden(iCount) THEN
           NEXT.
       DO iCountPageCols = 1 TO NUM-ENTRIES(cPageCols,";"):
           ASSIGN iFirstPageCol = INT(ENTRY(1,ENTRY(iCountPageCols,cPageCols,";"))) - 1
                  iCount3 = 0.
           DO iCount2 = INT(ENTRY(1,ENTRY(iCountPageCols,cPageCols,";"))) TO INT(ENTRY(2,ENTRY(iCountPageCols,cPageCols,";"))):
               ASSIGN iCount3 = iCount3 + 1.
               IF ENTRY(iCount2,cRightCols) = "1" THEN
                 ASSIGN iRight[iCount2] = 1.

/*                   cRight = IF ENTRY(iCount2,cRightCols) = "1" THEN "<RIGHT=C+" + 
                STRING(INT(ENTRY(iCount3 + 1,ENTRY(iCountPageCols,cFormatStr,";"))) - INT(ENTRY(iCount3,ENTRY(iCountPageCols,cFormatStr,";"))) - 2) + ">" ELSE ""
                      cRowStr = cRowStr + "<C" + ENTRY(iCount2 - iFirstPageCol,ENTRY(iCountPageCols,cFormatStr,";")) + ">" + cRight + chResultat:Cell(0,iCount,iCount2,iCount,iCount2).*/
/*                ASSIGN cRowStr = cRowStr + chResultat:Cell(0,iCount,iCount2,iCount,iCount2) + ";". */

                                                         /* 19=TextDisplay */
               ASSIGN cRowStr = cRowStr + chResultat:Cell(19,iCount,iCount2,iCount,iCount2) + ";".
           END.
           CREATE TT_RapportRader.
           ASSIGN TT_RapportRader.iPageNum = iSidNr
                  TT_RapportRader.iColPage = iCountPageCols
                  TT_RapportRader.iRadNum  = iRadNr
                  TT_RapportRader.cRadData = IF iCount = chResultat:Rows - 1 AND chResultat:IsSubtotal(chResultat:Rows - 1) THEN "" + cRowStr + "" ELSE cRowStr
                  TT_RapportRader.lLast   = iCount = chResultat:Rows - 1
                  cRowStr = "".
           ASSIGN iLastRow = iLastRow + 1.
           IF iCountPageCols = NUM-ENTRIES(cPageCols,";") THEN DO:
               IF ((lMindrePG1 = TRUE AND iRadNr = 34) OR iRadNr = 36) AND iCount < chResultat:Rows - 1 THEN
/*                IF iRadNr = 36 AND iCount < chResultat:Rows - 1 THEN */
                   ASSIGN iRadNr = 1
                          iSidNr = iSidNr + 1
                          lMindrePG1 = FALSE.
               ELSE
                   ASSIGN iRadNr = iRadNr + 1.
           END.
       END.
   END.
/*    OUTPUT TO "CLIPBOARD".      */
/*    FOR EACH TT_RapportRader:   */
/*        EXPORT TT_RapportRader. */
/*    END.                        */
/*    OUTPUT CLOSE.               */

   ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "rapp.pdf".
          iLeftMargin = 30.
   RUN pdf_new ("Spdf",pcRappfil).
   pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PDFPageFooter").
   RUN pdf_set_PaperType ("Spdf","A4").
   RUN pdf_set_LeftMargin ("Spdf", iLeftMargin).
   RUN pdf_set_BottomMargin ("Spdf", 40).
   RUN pdf_set_VerticalSpace ("Spdf", 13).
   RUN pdf_set_Orientation ("Spdf", "landscape").

   ASSIGN dPageWidth = pdf_PageWidth ("Spdf").
   IF lOversett = TRUE THEN
      ASSIGN cTitle = TRIM(REPLACE(cTitle,"Salg","Försäljning")).

   ASSIGN cHeaderTxt = cTitle.

/*   DO iCount3 = 1 TO iMaxCol:
     MESSAGE iCount3 SKIP dColPos3[iCount3]
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
   MESSAGE "Klart!"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

/*    ASSIGN dColPos[1] = iLeftMargin. */
   DO iCount3 = 1 TO iMaxCol:
     ASSIGN dColPos[iCount3] = dColPos3[iCount3] * 7.26.
   END.
/*    ASSIGN dColPos[7] = dColPos[7] + 10.  /* TRACEDATO 130609 dColPos[7] = dColPos[7] + 20. */ */
   DO iCount3 = 1 TO iMaxCol:
     ASSIGN dColPos2[iCount3] = dColPos[iCount3].
   END.

/*    ASSIGN dColPos2[1] = dColPos2[1] + 20.   /* Ok */                                                                            */
/*    ASSIGN dColPos2[3] = dColPos2[3] + 20.   /* Ok */                                                                            */
/*    ASSIGN dColPos2[4] = dColPos2[4] + 40.   /* Ok */   /* TRACEDATO 130609 dColPos2[4] + 75. */                                 */
/*    ASSIGN dColPos2[5] = dColPos2[5] + 80.   /* Ok */   /* TRACEDATO 130609 dColPos2[5] = dColPos2[5] + 70. */                   */
/*    ASSIGN dColPos2[6] = dColPos2[6] + 80.   /* Ok */   /* TRACEDATO 130609 dColPos2[6] = dColPos2[6] + 40. */                   */
/*    ASSIGN dColPos2[7] = dColPos2[7] + 25.   /* Ok */   /* TRACEDATO 130609 ASSIGN dColPos2[7] = dColPos2[7] + 25.  */           */
/*    ASSIGN dColPos2[8] = dColPos2[8] + 60.   /* Ok */   /* TRACEDATO 130609 ASSIGN dColPos2[8] = dColPos2[8] + 20. /* Ok */ */   */
/*    ASSIGN dColPos2[9] = dColPos2[9] + 57.   /* Ok */   /* TRACEDATO 130609 ASSIGN dColPos2[9] = dColPos2[9] + 30. /* Ok */ */   */
/*    ASSIGN dColPos2[10] = dColPos2[10] + 30. /* Ok */   /* TRACEDATO 130609 ASSIGN dColPos2[10] = dColPos2[10] + 40. /* Ok */ */ */
/*    ASSIGN dColPos2[11] = dColPos2[11] + 30. /* Ok */                                                                            */
/*    ASSIGN dColPos2[12] = dColPos2[12] + 45.                                                                                     */
/*    ASSIGN dColPos2[13] = dColPos2[13] + 20.                                                                                     */
/*    ASSIGN dColPos2[14] = dColPos2[14] + 47.                                                                                     */
/*    ASSIGN dColPos2[15] = dColPos2[15] + 20.                                                                                     */
/*    ASSIGN dColPos2[16] = dColPos2[16] + 47.                                                                                     */
/*    ASSIGN dColPos2[17] = dColPos2[17] + 20.                                                                                     */
/*    ASSIGN dColPos2[18] = dColPos2[18] + 47.                                                                                     */
/*    ASSIGN dColPos2[19] = dColPos2[19] + 20.                                                                                     */
/*    ASSIGN dColPos2[20] = dColPos2[20] + 47.                                                                                     */
/*    ASSIGN dColPos2[21] = dColPos2[21] + 20.                                                                                     */
/*    ASSIGN dColPos2[22] = dColPos2[22] + 47.                                                                                     */
/*    ASSIGN dColPos2[23] = dColPos2[23] + 23.                                                                                     */
/*    ASSIGN dColPos2[24] = dColPos2[24] + 47.                                                                                     */
/*    ASSIGN dColPos2[25] = dColPos2[25] + 20.                                                                                     */
/*    ASSIGN dColPos2[26] = dColPos2[26] + 47.                                                                                     */
/*                                                                                                                                 */
/*    ASSIGN dColPos2[27] = dColPos2[27] + 20.                                                                                     */
/*    ASSIGN dColPos2[28] = dColPos2[28] + 47.                                                                                     */

   IF cExtraInfo <> "" THEN
   DO:
     IF lOversett = TRUE THEN
       ASSIGN cExtraInfo = REPLACE(cExtraInfo,"Butikk","Butik")
              cExtraInfo = REPLACE(cExtraInfo,"AAR","ÅR")
              cExtraInfo = REPLACE(cExtraInfo,"MANED","MÅNAD")
              cExtraInfo = REPLACE(cExtraInfo,"UKE","Vecka")
              cExtraInfo = REPLACE(cExtraInfo,"Dato","Datum")
              cExtraInfo = REPLACE(cExtraInfo,"PeriodeType","Period")
              cExtraInfo = REPLACE(cExtraInfo,"TransType","TransTyp")
              cExtraInfo = REPLACE(cExtraInfo,"Alle","Alla").

   END.
   ASSIGN iLastRow2 = 0.
/*    OUTPUT TO "CLIPBOARD". */
   

   FOR EACH TT_RapportRader BREAK BY TT_RapportRader.iPageNum BY
                                     TT_RapportRader.iColPage BY
                                     TT_RapportRader.iRadNum: 
/*        PUT UNFORMATTED TT_RapportRader.cRadData SKIP. */
       ASSIGN iLastRow2 = iLastRow2 + 1.
       IF FIRST-OF(TT_RapportRader.iColPage) THEN DO:
           IF NOT FIRST(TT_RapportRader.iPageNum) THEN DO:
/*               RUN PFooter.
               PAGE.*/
           END.
           RUN pdf_new_page ("Spdf").
           RUN PDFPageHeader.
           ASSIGN dY = pdf_PageHeight ("Spdf") - 70.
           RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
           IF FIRST(TT_RapportRader.iPageNum) AND cExtraInfo <> "" THEN DO:

             RUN pdf_text_xy_dec ("Spdf",cExtraInfo,dColPos[1],dY).
             ASSIGN cExtraInfo = "".
             ASSIGN dY = dY - 20.
           END.
           DO iCount3 = iPageLcol[TT_RapportRader.iColPage] TO iPageRcol[TT_RapportRader.iColPage]:
             RUN pdf_text_xy_dec ("Spdf",ENTRY(iCount3,cLabelStr,";"),dColPos[iCount3],dY).
             ASSIGN cUL = "".
             DO iLen = 1 TO LENGTH(ENTRY(iCount3,cLabelStr,";")):
               ASSIGN cUL = cUL + "_".
             END.
             RUN pdf_text_xy_dec ("Spdf",cUL,dColPos[iCount3],dY).
           END.
       END.
       RUN pdf_set_font ("Spdf", "Helvetica",10).
       dY = dY - 13.
/*        IF LAST(TT_RapportRader.iColPage) AND LAST (TT_RapportRader.iRadNum) THEN */
       IF TT_RapportRader.lLast = TRUE THEN
       DO:
         IF lSummarad = TRUE THEN DO:
             RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
             dY = dY - 13.
         END.
         ASSIGN lLast = TRUE.
       END.

/*        OUTPUT TO "CLIPBOARD".                */
/*        PUT UNFORMATTED cLastFieldWidth SKIP. */
/*        OUTPUT CLOSE.                         */
       DO iCount3 = 1 TO iNumCol[TT_RapportRader.iColPage]:
         ASSIGN iCount = iCount3 + iAddCol[TT_RapportRader.iColPage].
         ASSIGN wrk = TRIM(ENTRY(iCount3,TT_RapportRader.cRadData,";")).
         IF iRight[iCount3] = 1 THEN DO:
             IF iCount3 < iNumCol[TT_RapportRader.iColPage] THEN
                 RUN pdf_text_xy_dec ("Spdf",wrk,dColPos2[iCount  + 1] - bredd(wrk) - 5,dY).
             ELSE DO:
                 RUN pdf_text_xy_dec ("Spdf",wrk,dColPos2[iCount] + INT(ENTRY(TT_RapportRader.iColPage,cLastFieldWidth)) - bredd(wrk),dY).
/*                      RUN pdf_text_xy_dec ("Spdf",wrk,pdf_PageWidth("Spdf") - bredd(wrk) - 30,dY). */
             END.
/*  xxxxxxx            RUN pdf_text_xy_dec ("Spdf",wrk,dColPos2[iCount] - bredd(wrk),dY). */
/*  !!            RUN pdf_text_xy_dec ("Spdf",wrk,dColPos2[iCount] - bredd(wrk) + iLeftMargin,dY). */
/*            RUN PDFNumCheck2 (wrk,OUTPUT lNumeric, OUTPUT lDecimal).                             */
/* /*            PUT UNFORMATTED wrk " " lNumeric " " lDecimal SKIP. */                            */
/* /*           IF lNumeric = FALSE THEN*/                                                         */
/*            IF lNumeric = TRUE THEN DO:                                                          */
/* /*                PUT UNFORMATTED "--" wrk SKIP. */                                             */
/*                RUN pdf_text_xy_dec ("Spdf",wrk,dColPos[iCount],dY).                             */
/*            END.                                                                                 */
/*            ELSE                                                                                 */
/*            DO:                                                                                  */
/*              ASSIGN dBelopp = DEC(ENTRY(iCount3,TT_RapportRader.cRadData,";")).                 */
/*              IF lDecimal = TRUE THEN                                                            */
/*                  ASSIGN cStr = TRIM(STRING(dBelopp,"->>>>>>>>9.99")).                           */
/*              ELSE                                                                               */
/*                ASSIGN cStr = TRIM(STRING(dBelopp,"->>>>>>>>>>>>9")).                            */
/*              RUN pdf_text_xy_dec ("Spdf",cStr,dColPos2[iCount] - bredd(cStr) + iLeftMargin,dY). */
/*            END.                                                                                 */
         END.
         ELSE DO:
             RUN pdf_text_xy_dec ("Spdf",wrk,dColPos[iCount],dY).
         END.
       END.
   END.
/*    OUTPUT CLOSE. */
   RUN pdf_close ("Spdf").
   RUN browse2pdf\viewxmldialog.w (pcRappFil, 'Polygon Retail Solutions').
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFXprintRapp2org fFrameWin 
PROCEDURE PDFXprintRapp2org :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER cUrvalsInfo    AS CHARACTER           NO-UNDO.
   DEFINE INPUT PARAMETER cColWidth%     AS CHARACTER           NO-UNDO.
   DEFINE INPUT PARAMETER cPageCols      AS CHARACTER           NO-UNDO. /* vid för bred grid för XPrintrapport */
   DEFINE INPUT PARAMETER cRightCols     AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  pcRappFil      AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  iCount         AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iCount0        AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iCount2        AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  cLabelStr      AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  cLabelStrTMP   AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  cRowStr        AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  cFormatStr     AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  cFormatStrTMP  AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  iColTmp        AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iCount3        AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iFirstSumCol   AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iFirstPageCol  AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iCountPageCols AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  cRight         AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  iSidNr         AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iRadNr         AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  lMindrePG1     AS LOGICAL             NO-UNDO.
   DEFINE       VARIABLE  dColPos3       AS DECIMAL EXTENT 90   NO-UNDO.
   DEFINE       VARIABLE  iRight         AS INTEGER EXTENT 90   NO-UNDO.
   DEFINE       VARIABLE  cPage          AS CHARACTER EXTENT 10 NO-UNDO.
   DEFINE       VARIABLE  iPageLcol      AS INTEGER EXTENT 10   NO-UNDO.  /* Första kolumn per sida */
   DEFINE       VARIABLE  iPageRcol      AS INTEGER EXTENT 10   NO-UNDO.  /* Sista kolumn per sida */
   DEFINE       VARIABLE  iMaxCol        AS INTEGER             NO-UNDO.  /* Totalt antal kolumner alla sidor */
   DEFINE       VARIABLE  iNumCol        AS INTEGER EXTENT 10   NO-UNDO.  /* Antal kolumner per sida */
   DEFINE       VARIABLE  iAddCol        AS INTEGER EXTENT 10   NO-UNDO.  /* Kolumner att addera */
   DEFINE       VARIABLE  lNumeric       AS LOGICAL             NO-UNDO.
   DEFINE       VARIABLE  lDecimal       AS LOGICAL             NO-UNDO.
   DEFINE       VARIABLE  lLast          AS LOGICAL             NO-UNDO.

/*    MESSAGE "cPageCols " cPageCols         */
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK. */
   ASSIGN lLast = FALSE.

   IF cExtraInfo <> "" THEN
       ASSIGN lMindrePG1 = TRUE.
   /* skapa en styrsträng for rapportrader */
   IF NUM-ENTRIES(cRightCols) <> chResultat:Cols - 1 THEN DO:
/*        MESSAGE NUM-ENTRIES(cRightCols) chResultat:Cols - 1 SKIP "Feil i 'RIGHT'-string" */
/*            VIEW-AS ALERT-BOX INFO BUTTONS OK.                                           */
       ASSIGN cRightCols = FILL(",",chResultat:Cols - 2).
   END.
 DO iCount = 1 TO NUM-ENTRIES(cPageCols,";"):
   ASSIGN cPage[iCount] = ENTRY(iCount,cPageCols,";").
   ASSIGN iPageLcol[iCount] = INT(ENTRY(1,cPage[iCount],",")).
   ASSIGN iPageRcol[iCount] = INT(ENTRY(2,cPage[iCount],",")).
   ASSIGN iNumCol[iCount] = iPageRcol[iCount] - iPageLcol[iCount] + 1.
 END.

 ASSIGN iAddCol[1] = 0.
 IF NUM-ENTRIES(cPageCols,";") > 1 THEN
 DO iCount = 2 TO NUM-ENTRIES(cPageCols,";"):
   ASSIGN iAddCol[iCount] = iAddCol[iCount - 1] + iNumCol[iCount - 1].
 END.

/* MESSAGE "cRightCols" SKIP cRightCols
     VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
   EMPTY TEMP-TABLE TT_RapportRader.
   ASSIGN iFirstSumCol = IF cColLabelSave <> "" THEN INT(ENTRY(1,cColLabelSave)) + 1 ELSE ?.

/*           cTitle = "Statistikk". */
   DO iCount = 1 TO NUM-ENTRIES(cPageCols,";"):
       ASSIGN cFormatStrTMP = "6"
              iColTmp       = 6
              iCount3       = 0.
       DO iCount2 = INT(ENTRY(1,ENTRY(iCount,cPageCols,";"))) TO INT(ENTRY(2,ENTRY(iCount,cPageCols,";"))):
           ASSIGN iCount3    = ROUND(DECI(ENTRY(iCount2,cColWidth%,CHR(1))) * 105 / 100,0)
                  iColTmp    = iColTmp + iCount3
                  cFormatStrTMP = cFormatStrTMP + "," + STRING(iColTmp).
       END.
       ASSIGN cFormatStr = cFormatStr + (IF cFormatStr = "" THEN "" ELSE ";") + cFormatStrTMP.
   END.

   /* Finn lablar */
   ASSIGN iMaxCol = 0.
   DO iCount = 1 TO NUM-ENTRIES(cPageCols,";"):
       ASSIGN cLabelStrTMP = ""
              iFirstPageCol = INT(ENTRY(1,ENTRY(iCount,cPageCols,";"))) - 1.
       DO iCount2 = 1 TO NUM-ENTRIES(ENTRY(iCount,cFormatStr,";")) - 1:
         ASSIGN iMaxCol = iMaxCol + 1.
         ASSIGN dColPos3[iMaxCol] = DEC(ENTRY(iCount2,ENTRY(iCount,cFormatStr,";"))).
/*           ASSIGN cLabelStrTMP = cLabelStrTMP + "<C" + ENTRY(iCount2,ENTRY(iCount,cFormatStr,";")) + ">" + chResultat:Cell(0,0,iCount2 + iFirstPageCol,0,iCount2 + iFirstPageCol).*/
           ASSIGN cLabelStrTMP = cLabelStrTMP + chResultat:Cell(0,0,iCount2 + iFirstPageCol,0,iCount2 + iFirstPageCol) + ";".
       END.
/*       ASSIGN cLabelStr = cLabelStr + (IF cLabelStr = "" THEN "" ELSE ";") + "<U>" + cLabelStrTMP + "</U>".*/
       ASSIGN cLabelStr = cLabelStr + cLabelStrTMP.
   END.

/*    MESSAGE "iMaxCol " iMaxCol             */
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK. */
   IF lOversett = TRUE THEN
       ASSIGN cToday = STRING(YEAR(TODAY),"9999") + "-"
                     + STRING(MONTH(TODAY),"99") + "-"
                     + STRING(DAY(TODAY),"99").
     ELSE
       ASSIGN cToday = STRING(TODAY).

/*    FIELD iPageNum AS INTEGER   */
/*    FIELD iColPage AS INTEGER   */
/*    FIELD iRadNum  AS INTEGER   */
/*    FIELD cRadData AS CHARACTER */
   ASSIGN iLastRow = 0.
   ASSIGN iSidNr = 1
          iRadNr = 1.
   DO iCount = 1 TO chResultat:Rows - 1:
       IF chResultat:RowHidden(iCount) THEN
           NEXT.
       DO iCountPageCols = 1 TO NUM-ENTRIES(cPageCols,";"):
           ASSIGN iFirstPageCol = INT(ENTRY(1,ENTRY(iCountPageCols,cPageCols,";"))) - 1
                  iCount3 = 0.
           DO iCount2 = INT(ENTRY(1,ENTRY(iCountPageCols,cPageCols,";"))) TO INT(ENTRY(2,ENTRY(iCountPageCols,cPageCols,";"))):
               ASSIGN iCount3 = iCount3 + 1.
               IF ENTRY(iCount2,cRightCols) = "1" THEN
                 ASSIGN iRight[iCount3] = 1.

/*                   cRight = IF ENTRY(iCount2,cRightCols) = "1" THEN "<RIGHT=C+" + 
                STRING(INT(ENTRY(iCount3 + 1,ENTRY(iCountPageCols,cFormatStr,";"))) - INT(ENTRY(iCount3,ENTRY(iCountPageCols,cFormatStr,";"))) - 2) + ">" ELSE ""
                      cRowStr = cRowStr + "<C" + ENTRY(iCount2 - iFirstPageCol,ENTRY(iCountPageCols,cFormatStr,";")) + ">" + cRight + chResultat:Cell(0,iCount,iCount2,iCount,iCount2).*/
               ASSIGN cRowStr = cRowStr + chResultat:Cell(0,iCount,iCount2,iCount,iCount2) + ";".
           END.
/*           MESSAGE "cRowStr " cRowStr
               VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
           CREATE TT_RapportRader.
           ASSIGN TT_RapportRader.iPageNum = iSidNr
                  TT_RapportRader.iColPage = iCountPageCols
                  TT_RapportRader.iRadNum  = iRadNr
                  TT_RapportRader.cRadData = IF iCount = chResultat:Rows - 1 AND chResultat:IsSubtotal(chResultat:Rows - 1) THEN "" + cRowStr + "" ELSE cRowStr
                  cRowStr = "".
           ASSIGN iLastRow = iLastRow + 1.
           IF iCountPageCols = NUM-ENTRIES(cPageCols,";") THEN DO:
               IF ((lMindrePG1 = TRUE AND iRadNr = 34) OR iRadNr = 36) AND iCount < chResultat:Rows - 1 THEN
/*                IF iRadNr = 36 AND iCount < chResultat:Rows - 1 THEN */
                   ASSIGN iRadNr = 1
                          iSidNr = iSidNr + 1
                          lMindrePG1 = FALSE.
               ELSE
                   ASSIGN iRadNr = iRadNr + 1.
           END.
       END.
   END.

   ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "rapp.pdf".
/*   OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(80).
   PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
   PUT CONTROL '<PREVIEW=ZoomToWidth><OLANDSCAPE>'.*/

   RUN pdf_new ("Spdf",pcRappfil).
   pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PDFPageFooter").
   RUN pdf_set_PaperType ("Spdf","A4").
   RUN pdf_set_LeftMargin ("Spdf", 30).
   RUN pdf_set_BottomMargin ("Spdf", 40).
   RUN pdf_set_VerticalSpace ("Spdf", 13).
   RUN pdf_set_Orientation ("Spdf", "landscape").

   ASSIGN dPageWidth = pdf_PageWidth ("Spdf").
   IF lOversett = TRUE THEN
      ASSIGN cTitle = TRIM(REPLACE(cTitle,"Salg","Försäljning")).

   ASSIGN cHeaderTxt = cTitle.

/*   DO iCount3 = 1 TO iMaxCol:
     MESSAGE iCount3 SKIP dColPos3[iCount3]
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
   MESSAGE "Klart!"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

   ASSIGN dColPos[1] = pdf_LeftMargin ("Spdf").
   DO iCount3 = 2 TO iMaxCol:
     ASSIGN dColPos[iCount3] = dColPos3[iCount3] * 7.26.
   END.
   ASSIGN dColPos[7] = dColPos[7] + 10.  /* TRACEDATO 130609 dColPos[7] = dColPos[7] + 20. */
   DO iCount3 = 1 TO iMaxCol:
     ASSIGN dColPos2[iCount3] = dColPos[iCount3].
   END.

   ASSIGN dColPos2[1] = dColPos2[1] + 20.   /* Ok */
   ASSIGN dColPos2[3] = dColPos2[3] + 20.   /* Ok */
   ASSIGN dColPos2[4] = dColPos2[4] + 40.   /* Ok */   /* TRACEDATO 130609 dColPos2[4] + 75. */
   ASSIGN dColPos2[5] = dColPos2[5] + 80.   /* Ok */   /* TRACEDATO 130609 dColPos2[5] = dColPos2[5] + 70. */
   ASSIGN dColPos2[6] = dColPos2[6] + 80.   /* Ok */   /* TRACEDATO 130609 dColPos2[6] = dColPos2[6] + 40. */
   ASSIGN dColPos2[7] = dColPos2[7] + 25.   /* Ok */   /* TRACEDATO 130609 ASSIGN dColPos2[7] = dColPos2[7] + 25.  */
   ASSIGN dColPos2[8] = dColPos2[8] + 60.   /* Ok */   /* TRACEDATO 130609 ASSIGN dColPos2[8] = dColPos2[8] + 20. /* Ok */ */
   ASSIGN dColPos2[9] = dColPos2[9] + 57.   /* Ok */   /* TRACEDATO 130609 ASSIGN dColPos2[9] = dColPos2[9] + 30. /* Ok */ */
   ASSIGN dColPos2[10] = dColPos2[10] + 30. /* Ok */   /* TRACEDATO 130609 ASSIGN dColPos2[10] = dColPos2[10] + 40. /* Ok */ */
   ASSIGN dColPos2[11] = dColPos2[11] + 30. /* Ok */
   ASSIGN dColPos2[12] = dColPos2[12] + 45.
   ASSIGN dColPos2[13] = dColPos2[13] + 20.

   ASSIGN dColPos2[14] = dColPos2[14] + 47.
   ASSIGN dColPos2[15] = dColPos2[15] + 20.
   ASSIGN dColPos2[16] = dColPos2[16] + 47.
   ASSIGN dColPos2[17] = dColPos2[17] + 20.
   ASSIGN dColPos2[18] = dColPos2[18] + 47.
   ASSIGN dColPos2[19] = dColPos2[19] + 20.
   ASSIGN dColPos2[20] = dColPos2[20] + 47.
   ASSIGN dColPos2[21] = dColPos2[21] + 20.
   ASSIGN dColPos2[22] = dColPos2[22] + 47.
   ASSIGN dColPos2[23] = dColPos2[23] + 23.
   ASSIGN dColPos2[24] = dColPos2[24] + 47.
   ASSIGN dColPos2[25] = dColPos2[25] + 20.
   ASSIGN dColPos2[26] = dColPos2[26] + 47.

   ASSIGN dColPos2[27] = dColPos2[27] + 20.
   ASSIGN dColPos2[28] = dColPos2[28] + 47.

   IF cExtraInfo <> "" THEN
   DO:
     IF lOversett = TRUE THEN
       ASSIGN cExtraInfo = REPLACE(cExtraInfo,"Butikk","Butik")
              cExtraInfo = REPLACE(cExtraInfo,"AAR","ÅR")
              cExtraInfo = REPLACE(cExtraInfo,"MANED","MÅNAD")
              cExtraInfo = REPLACE(cExtraInfo,"UKE","Vecka")
              cExtraInfo = REPLACE(cExtraInfo,"Dato","Datum")
              cExtraInfo = REPLACE(cExtraInfo,"PeriodeType","Period")
              cExtraInfo = REPLACE(cExtraInfo,"TransType","TransTyp")
              cExtraInfo = REPLACE(cExtraInfo,"Alle","Alla").

   END.
   ASSIGN iLastRow2 = 0.
   FOR EACH TT_RapportRader BREAK BY TT_RapportRader.iPageNum BY
                                     TT_RapportRader.iColPage BY
                                     TT_RapportRader.iRadNum: 
       ASSIGN iLastRow2 = iLastRow2 + 1.
       IF FIRST-OF(TT_RapportRader.iColPage) THEN DO:
           IF NOT FIRST(TT_RapportRader.iPageNum) THEN DO:
/*               RUN PFooter.
               PAGE.*/
           END.
           RUN pdf_new_page ("Spdf").
           RUN PDFPageHeader.
           ASSIGN dY = pdf_PageHeight ("Spdf") - 70.
           RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
           IF FIRST(TT_RapportRader.iPageNum) AND cExtraInfo <> "" THEN DO:

/*               PUT UNFORMATTED "<C6><P12>" cExtraInfo  "<P10><R+1>" SKIP.*/
             RUN pdf_text_xy_dec ("Spdf",cExtraInfo,dColPos[1],dY).
             ASSIGN cExtraInfo = "".
             ASSIGN dY = dY - 20.
           END.
           DO iCount3 = iPageLcol[TT_RapportRader.iColPage] TO iPageRcol[TT_RapportRader.iColPage]:
             RUN pdf_text_xy_dec ("Spdf",ENTRY(iCount3,cLabelStr,";"),dColPos[iCount3],dY).
             ASSIGN cUL = "".
             DO iLen = 1 TO LENGTH(ENTRY(iCount3,cLabelStr,";")):
               ASSIGN cUL = cUL + "_".
             END.
             RUN pdf_text_xy_dec ("Spdf",cUL,dColPos[iCount3],dY).
           END.
           /*PUT UNFORMATTED ENTRY(TT_RapportRader.iColPage,cLabelStr,";") SKIP.*/
       END.
       RUN pdf_set_font ("Spdf", "Helvetica",10).
       dY = dY - 13.
       IF LAST(TT_RapportRader.iColPage) AND LAST (TT_RapportRader.iRadNum) THEN
       DO:
         RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
         dY = dY - 13.
         ASSIGN lLast = TRUE.
       END.
       DO iCount3 = 1 TO iNumCol[TT_RapportRader.iColPage]:
         ASSIGN iCount = iCount3 + iAddCol[TT_RapportRader.iColPage].
         ASSIGN wrk = TRIM(ENTRY(iCount3,TT_RapportRader.cRadData,";")).
         IF iRight[iCount3] = 1 THEN
/*         IF ENTRY(iCount,cRightCols) = "1" THEN*/
         DO:
/*           RUN PDFNumCheck (wrk,OUTPUT lNumeric, OUTPUT lDecimal).*/
           RUN PDFNumCheck2 (wrk,OUTPUT lNumeric, OUTPUT lDecimal).
/*           IF lNumeric = FALSE THEN*/
           IF lNumeric = TRUE THEN
             RUN pdf_text_xy_dec ("Spdf",wrk,dColPos[iCount],dY).
           ELSE
           DO:
             ASSIGN dBelopp = DEC(ENTRY(iCount3,TT_RapportRader.cRadData,";")).
             IF lDecimal = TRUE THEN
                 ASSIGN cStr = TRIM(STRING(dBelopp,"->>>>>>>9.99")).
/*                ASSIGN cStr = TRIM(STRING(dBelopp,"->>,>>>,>>9.99")). */
             ELSE
               ASSIGN cStr = TRIM(STRING(dBelopp,"->>>>>>9")).
             RUN pdf_text_xy_dec ("Spdf",cStr,dColPos2[iCount] - bredd(cStr),dY).
           END.
         END.
         ELSE
           RUN pdf_text_xy_dec ("Spdf",wrk,dColPos[iCount],dY).
       END.
/*       PUT UNFORMATTED TT_RapportRader.cRadData SKIP.*/
/*        IF LAST-OF(TT_RapportRader.iColPage) AND NOT LAST(TT_RapportRader.iRadNum) THEN */
/*            PAGE.                                                                       */
/*                                                                                        */
   END.

/*   RUN PFooter.*/
/*   IF cUrvalsInfo <> "" THEN DO:
       PAGE.
       PUT UNFORMATTED  "<R4><B><C1><CENTER=C110><P24>" cTitle "</B><P10><R+2>".
       PUT UNFORMATTED "<B><P14><R10><C10>" REPLACE(cUrvalsInfo,CHR(10),"<R+2><C10>") "</B><P10>" SKIP.
       RUN PFooter.
   END.*/

   RUN pdf_close ("Spdf").
   RUN browse2pdf\viewxmldialog.w (pcRappFil, 'Polygon Retail Solutions').
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFXprintRapp2X fFrameWin 
PROCEDURE PDFXprintRapp2X :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER cUrvalsInfo    AS CHARACTER           NO-UNDO.
   DEFINE INPUT PARAMETER cColWidth%     AS CHARACTER           NO-UNDO.
   DEFINE INPUT PARAMETER cPageCols      AS CHARACTER           NO-UNDO. /* vid för bred grid för XPrintrapport */
   DEFINE INPUT PARAMETER cRightCols     AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  pcRappFil      AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  iCount         AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iCount0        AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iCount2        AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  cLabelStr      AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  cLabelStrTMP   AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  cRowStr        AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  cFormatStr     AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  cFormatStrTMP  AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  iColTmp        AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iCount3        AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iFirstSumCol   AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iFirstPageCol  AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iCountPageCols AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  cRight         AS CHARACTER           NO-UNDO.
   DEFINE       VARIABLE  iSidNr         AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  iRadNr         AS INTEGER             NO-UNDO.
   DEFINE       VARIABLE  lMindrePG1     AS LOGICAL             NO-UNDO.
   DEFINE       VARIABLE  dColPos3       AS DECIMAL EXTENT 90   NO-UNDO.
   DEFINE       VARIABLE  iRight         AS INTEGER EXTENT 90   NO-UNDO.
   DEFINE       VARIABLE  cPage          AS CHARACTER EXTENT 10 NO-UNDO.
   DEFINE       VARIABLE  iPageLcol      AS INTEGER EXTENT 10   NO-UNDO.  /* Första kolumn per sida */
   DEFINE       VARIABLE  iPageRcol      AS INTEGER EXTENT 10   NO-UNDO.  /* Sista kolumn per sida */
   DEFINE       VARIABLE  iMaxCol        AS INTEGER             NO-UNDO.  /* Totalt antal kolumner alla sidor */
   DEFINE       VARIABLE  iNumCol        AS INTEGER EXTENT 10   NO-UNDO.  /* Antal kolumner per sida */
   DEFINE       VARIABLE  iAddCol        AS INTEGER EXTENT 10   NO-UNDO.  /* Kolumner att addera */
   DEFINE       VARIABLE  lNumeric       AS LOGICAL             NO-UNDO.
   DEFINE       VARIABLE  lDecimal       AS LOGICAL             NO-UNDO.
   DEFINE       VARIABLE  lLast          AS LOGICAL             NO-UNDO.

/*MESSAGE "FÖR TEST" SKIP
    "cUrvalsInfo"  cUrvalsInfo skip
    "cColWidth% "  cColWidth%  skip
    "cPageCols  "  cPageCols   skip
    "cRightCols "  cRightCols SKIP
    NUM-ENTRIES(cColWidth%,CHR(1))

    VIEW-AS ALERT-BOX INFO BUTTONS OK.
     OUTPUT TO "CLIPBOARD".
     PUT UNFORMATTED cColWidth% SKIP.
     OUTPUT CLOSE.*/

/*    MESSAGE "cPageCols " cPageCols         */
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK. */
   ASSIGN lLast = FALSE.

   IF cExtraInfo <> "" THEN
       ASSIGN lMindrePG1 = TRUE.
   /* skapa en styrsträng for rapportrader */
   IF NUM-ENTRIES(cRightCols) <> chResultat:Cols - 1 THEN DO:
/*        MESSAGE NUM-ENTRIES(cRightCols) chResultat:Cols - 1 SKIP "Feil i 'RIGHT'-string" */
/*            VIEW-AS ALERT-BOX INFO BUTTONS OK.                                           */
       ASSIGN cRightCols = FILL(",",chResultat:Cols - 2).
   END.
 DO iCount = 1 TO NUM-ENTRIES(cPageCols,";"):
   ASSIGN cPage[iCount] = ENTRY(iCount,cPageCols,";").
   ASSIGN iPageLcol[iCount] = INT(ENTRY(1,cPage[iCount],",")).
   ASSIGN iPageRcol[iCount] = INT(ENTRY(2,cPage[iCount],",")).
   ASSIGN iNumCol[iCount] = iPageRcol[iCount] - iPageLcol[iCount] + 1.
 END.

 ASSIGN iAddCol[1] = 0.
 IF NUM-ENTRIES(cPageCols,";") > 1 THEN
 DO iCount = 2 TO NUM-ENTRIES(cPageCols,";"):
   ASSIGN iAddCol[iCount] = iAddCol[iCount - 1] + iNumCol[iCount - 1].
 END.

/* MESSAGE "cRightCols" SKIP cRightCols
     VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
   EMPTY TEMP-TABLE TT_RapportRader.
   ASSIGN iFirstSumCol = IF cColLabelSave <> "" THEN INT(ENTRY(1,cColLabelSave)) + 1 ELSE ?.

/*           cTitle = "Statistikk". */
   DO iCount = 1 TO NUM-ENTRIES(cPageCols,";"):
       ASSIGN cFormatStrTMP = "6"
              iColTmp       = 6
              iCount3       = 0.
       DO iCount2 = INT(ENTRY(1,ENTRY(iCount,cPageCols,";"))) TO INT(ENTRY(2,ENTRY(iCount,cPageCols,";"))):
           ASSIGN iCount3    = ROUND(DECI(ENTRY(iCount2,cColWidth%,CHR(1))) * 105 / 100,0)
                  iColTmp    = iColTmp + iCount3
                  cFormatStrTMP = cFormatStrTMP + "," + STRING(iColTmp).
       END.
       ASSIGN cFormatStr = cFormatStr + (IF cFormatStr = "" THEN "" ELSE ";") + cFormatStrTMP.
   END.

   /* Finn lablar */
   ASSIGN iMaxCol = 0.
   DO iCount = 1 TO NUM-ENTRIES(cPageCols,";"):
       ASSIGN cLabelStrTMP = ""
              iFirstPageCol = INT(ENTRY(1,ENTRY(iCount,cPageCols,";"))) - 1.
       DO iCount2 = 1 TO NUM-ENTRIES(ENTRY(iCount,cFormatStr,";")) - 1:
         ASSIGN iMaxCol = iMaxCol + 1.
         ASSIGN dColPos3[iMaxCol] = DEC(ENTRY(iCount2,ENTRY(iCount,cFormatStr,";"))).
/*           ASSIGN cLabelStrTMP = cLabelStrTMP + "<C" + ENTRY(iCount2,ENTRY(iCount,cFormatStr,";")) + ">" + chResultat:Cell(0,0,iCount2 + iFirstPageCol,0,iCount2 + iFirstPageCol).*/
           ASSIGN cLabelStrTMP = cLabelStrTMP + chResultat:Cell(0,0,iCount2 + iFirstPageCol,0,iCount2 + iFirstPageCol) + ";".
       END.
/*       ASSIGN cLabelStr = cLabelStr + (IF cLabelStr = "" THEN "" ELSE ";") + "<U>" + cLabelStrTMP + "</U>".*/
       ASSIGN cLabelStr = cLabelStr + cLabelStrTMP.
   END.

/*    MESSAGE "iMaxCol " iMaxCol             */
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK. */
   IF lOversett = TRUE THEN
       ASSIGN cToday = STRING(YEAR(TODAY),"9999") + "-"
                     + STRING(MONTH(TODAY),"99") + "-"
                     + STRING(DAY(TODAY),"99").
     ELSE
       ASSIGN cToday = STRING(TODAY).

/*    FIELD iPageNum AS INTEGER   */
/*    FIELD iColPage AS INTEGER   */
/*    FIELD iRadNum  AS INTEGER   */
/*    FIELD cRadData AS CHARACTER */
   ASSIGN iLastRow = 0.
   ASSIGN iSidNr = 1
          iRadNr = 1.
   DO iCount = 1 TO chResultat:Rows - 1:
       IF chResultat:RowHidden(iCount) THEN
           NEXT.
       DO iCountPageCols = 1 TO NUM-ENTRIES(cPageCols,";"):
           ASSIGN iFirstPageCol = INT(ENTRY(1,ENTRY(iCountPageCols,cPageCols,";"))) - 1
                  iCount3 = 0.
           DO iCount2 = INT(ENTRY(1,ENTRY(iCountPageCols,cPageCols,";"))) TO INT(ENTRY(2,ENTRY(iCountPageCols,cPageCols,";"))):
               ASSIGN iCount3 = iCount3 + 1.
               IF ENTRY(iCount2,cRightCols) = "1" THEN
                 ASSIGN iRight[iCount3] = 1.

/*                   cRight = IF ENTRY(iCount2,cRightCols) = "1" THEN "<RIGHT=C+" + 
                STRING(INT(ENTRY(iCount3 + 1,ENTRY(iCountPageCols,cFormatStr,";"))) - INT(ENTRY(iCount3,ENTRY(iCountPageCols,cFormatStr,";"))) - 2) + ">" ELSE ""
                      cRowStr = cRowStr + "<C" + ENTRY(iCount2 - iFirstPageCol,ENTRY(iCountPageCols,cFormatStr,";")) + ">" + cRight + chResultat:Cell(0,iCount,iCount2,iCount,iCount2).*/
               ASSIGN cRowStr = cRowStr + chResultat:Cell(0,iCount,iCount2,iCount,iCount2) + ";".
           END.
/*           MESSAGE "cRowStr " cRowStr
               VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
           CREATE TT_RapportRader.
           ASSIGN TT_RapportRader.iPageNum = iSidNr
                  TT_RapportRader.iColPage = iCountPageCols
                  TT_RapportRader.iRadNum  = iRadNr
                  TT_RapportRader.cRadData = IF iCount = chResultat:Rows - 1 AND chResultat:IsSubtotal(chResultat:Rows - 1) THEN "" + cRowStr + "" ELSE cRowStr
                  cRowStr = "".
           ASSIGN iLastRow = iLastRow + 1.
           IF iCountPageCols = NUM-ENTRIES(cPageCols,";") THEN DO:
               IF ((lMindrePG1 = TRUE AND iRadNr = 34) OR iRadNr = 36) AND iCount < chResultat:Rows - 1 THEN
/*                IF iRadNr = 36 AND iCount < chResultat:Rows - 1 THEN */
                   ASSIGN iRadNr = 1
                          iSidNr = iSidNr + 1
                          lMindrePG1 = FALSE.
               ELSE
                   ASSIGN iRadNr = iRadNr + 1.
           END.
       END.
   END.

   ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "rapp.pdf".
/*   OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(80).
   PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
   PUT CONTROL '<PREVIEW=ZoomToWidth><OLANDSCAPE>'.*/

   RUN pdf_new ("Spdf",pcRappfil).
   pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PDFPageFooter").
   RUN pdf_set_PaperType ("Spdf","A4").
   RUN pdf_set_LeftMargin ("Spdf", 30).
   RUN pdf_set_BottomMargin ("Spdf", 40).
   RUN pdf_set_VerticalSpace ("Spdf", 13).
   RUN pdf_set_Orientation ("Spdf", "landscape").

   ASSIGN dPageWidth = pdf_PageWidth ("Spdf").
   IF lOversett = TRUE THEN
      ASSIGN cTitle = TRIM(REPLACE(cTitle,"Salg","Försäljning")).

   ASSIGN cHeaderTxt = cTitle.

/*   DO iCount3 = 1 TO iMaxCol:
     MESSAGE iCount3 SKIP dColPos3[iCount3]
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
   MESSAGE "Klart!"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

   ASSIGN dColPos[1] = pdf_LeftMargin ("Spdf").
   DO iCount3 = 2 TO iMaxCol:
     ASSIGN dColPos[iCount3] = dColPos3[iCount3] * 7.26.
   END.
   ASSIGN dColPos[7] = dColPos[7] + 10.  /* TRACEDATO 130609 dColPos[7] = dColPos[7] + 20. */
   DO iCount3 = 1 TO iMaxCol:
     ASSIGN dColPos2[iCount3] = dColPos[iCount3].
   END.

   ASSIGN dColPos2[1] = dColPos2[1] + 20.   /* Ok */
   ASSIGN dColPos2[3] = dColPos2[3] + 20.   /* Ok */
   ASSIGN dColPos2[4] = dColPos2[4] + 40.   /* Ok */   /* TRACEDATO 130609 dColPos2[4] + 75. */
   ASSIGN dColPos2[5] = dColPos2[5] + 80.   /* Ok */   /* TRACEDATO 130609 dColPos2[5] = dColPos2[5] + 70. */
   ASSIGN dColPos2[6] = dColPos2[6] + 80.   /* Ok */   /* TRACEDATO 130609 dColPos2[6] = dColPos2[6] + 40. */
   ASSIGN dColPos2[7] = dColPos2[7] + 25.   /* Ok */   /* TRACEDATO 130609 ASSIGN dColPos2[7] = dColPos2[7] + 25.  */
   ASSIGN dColPos2[8] = dColPos2[8] + 60.   /* Ok */   /* TRACEDATO 130609 ASSIGN dColPos2[8] = dColPos2[8] + 20. /* Ok */ */
   ASSIGN dColPos2[9] = dColPos2[9] + 57.   /* Ok */   /* TRACEDATO 130609 ASSIGN dColPos2[9] = dColPos2[9] + 30. /* Ok */ */
   ASSIGN dColPos2[10] = dColPos2[10] + 30. /* Ok */   /* TRACEDATO 130609 ASSIGN dColPos2[10] = dColPos2[10] + 40. /* Ok */ */
   ASSIGN dColPos2[11] = dColPos2[11] + 30. /* Ok */
   ASSIGN dColPos2[12] = dColPos2[12] + 45.
   ASSIGN dColPos2[13] = dColPos2[13] + 20.

   ASSIGN dColPos2[14] = dColPos2[14] + 47.
   ASSIGN dColPos2[15] = dColPos2[15] + 20.
   ASSIGN dColPos2[16] = dColPos2[16] + 47.
   ASSIGN dColPos2[17] = dColPos2[17] + 20.
   ASSIGN dColPos2[18] = dColPos2[18] + 47.
   ASSIGN dColPos2[19] = dColPos2[19] + 20.
   ASSIGN dColPos2[20] = dColPos2[20] + 47.
   ASSIGN dColPos2[21] = dColPos2[21] + 20.
   ASSIGN dColPos2[22] = dColPos2[22] + 47.
   ASSIGN dColPos2[23] = dColPos2[23] + 23.
   ASSIGN dColPos2[24] = dColPos2[24] + 47.
   ASSIGN dColPos2[25] = dColPos2[25] + 20.
   ASSIGN dColPos2[26] = dColPos2[26] + 47.

   ASSIGN dColPos2[27] = dColPos2[27] + 20.
   ASSIGN dColPos2[28] = dColPos2[28] + 47.

   IF cExtraInfo <> "" THEN
   DO:
     IF lOversett = TRUE THEN
       ASSIGN cExtraInfo = REPLACE(cExtraInfo,"Butikk","Butik")
              cExtraInfo = REPLACE(cExtraInfo,"AAR","ÅR")
              cExtraInfo = REPLACE(cExtraInfo,"MANED","MÅNAD")
              cExtraInfo = REPLACE(cExtraInfo,"UKE","Vecka")
              cExtraInfo = REPLACE(cExtraInfo,"Dato","Datum")
              cExtraInfo = REPLACE(cExtraInfo,"PeriodeType","Period")
              cExtraInfo = REPLACE(cExtraInfo,"TransType","TransTyp")
              cExtraInfo = REPLACE(cExtraInfo,"Alle","Alla").

   END.
   ASSIGN iLastRow2 = 0.
   FOR EACH TT_RapportRader BREAK BY TT_RapportRader.iPageNum BY
                                     TT_RapportRader.iColPage BY
                                     TT_RapportRader.iRadNum: 
       ASSIGN iLastRow2 = iLastRow2 + 1.
       IF FIRST-OF(TT_RapportRader.iColPage) THEN DO:
           IF NOT FIRST(TT_RapportRader.iPageNum) THEN DO:
/*               RUN PFooter.
               PAGE.*/
           END.
           RUN pdf_new_page ("Spdf").
           RUN PDFPageHeader.
           ASSIGN dY = pdf_PageHeight ("Spdf") - 70.
           RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
           IF FIRST(TT_RapportRader.iPageNum) AND cExtraInfo <> "" THEN DO:

/*               PUT UNFORMATTED "<C6><P12>" cExtraInfo  "<P10><R+1>" SKIP.*/
             RUN pdf_text_xy_dec ("Spdf",cExtraInfo,dColPos[1],dY).
             ASSIGN cExtraInfo = "".
             ASSIGN dY = dY - 20.
           END.
           DO iCount3 = iPageLcol[TT_RapportRader.iColPage] TO iPageRcol[TT_RapportRader.iColPage]:
             RUN pdf_text_xy_dec ("Spdf",ENTRY(iCount3,cLabelStr,";"),dColPos[iCount3],dY).
             ASSIGN cUL = "".
             DO iLen = 1 TO LENGTH(ENTRY(iCount3,cLabelStr,";")):
               ASSIGN cUL = cUL + "_".
             END.
             RUN pdf_text_xy_dec ("Spdf",cUL,dColPos[iCount3],dY).
           END.
           /*PUT UNFORMATTED ENTRY(TT_RapportRader.iColPage,cLabelStr,";") SKIP.*/
       END.
       RUN pdf_set_font ("Spdf", "Helvetica",10).
       dY = dY - 13.
       IF LAST(TT_RapportRader.iColPage) AND LAST (TT_RapportRader.iRadNum) THEN
       DO:
         RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
         dY = dY - 13.
         ASSIGN lLast = TRUE.
       END.
       DO iCount3 = 1 TO iNumCol[TT_RapportRader.iColPage]:
         ASSIGN iCount = iCount3 + iAddCol[TT_RapportRader.iColPage].
         ASSIGN wrk = TRIM(ENTRY(iCount3,TT_RapportRader.cRadData,";")).
         IF iRight[iCount3] = 1 THEN
/*         IF ENTRY(iCount,cRightCols) = "1" THEN*/
         DO:
/*           RUN PDFNumCheck (wrk,OUTPUT lNumeric, OUTPUT lDecimal).*/
           RUN PDFNumCheck2 (wrk,OUTPUT lNumeric, OUTPUT lDecimal).
/*           IF lNumeric = FALSE THEN*/
           IF lNumeric = TRUE THEN
             RUN pdf_text_xy_dec ("Spdf",wrk,dColPos[iCount],dY).
           ELSE
           DO:
             ASSIGN dBelopp = DEC(ENTRY(iCount3,TT_RapportRader.cRadData,";")).
             IF lDecimal = TRUE THEN
                 ASSIGN cStr = TRIM(STRING(dBelopp,"->>>>>>>9.99")).
/*                ASSIGN cStr = TRIM(STRING(dBelopp,"->>,>>>,>>9.99")). */
             ELSE
               ASSIGN cStr = TRIM(STRING(dBelopp,"->>>>>>>>>>>>9")).
             RUN pdf_text_xy_dec ("Spdf",cStr,dColPos2[iCount] - bredd(cStr),dY).
           END.
         END.
         ELSE
           RUN pdf_text_xy_dec ("Spdf",wrk,dColPos[iCount],dY).
       END.
/*       PUT UNFORMATTED TT_RapportRader.cRadData SKIP.*/
/*        IF LAST-OF(TT_RapportRader.iColPage) AND NOT LAST(TT_RapportRader.iRadNum) THEN */
/*            PAGE.                                                                       */
/*                                                                                        */
   END.

/*   RUN PFooter.*/
/*   IF cUrvalsInfo <> "" THEN DO:
       PAGE.
       PUT UNFORMATTED  "<R4><B><C1><CENTER=C110><P24>" cTitle "</B><P10><R+2>".
       PUT UNFORMATTED "<B><P14><R10><C10>" REPLACE(cUrvalsInfo,CHR(10),"<R+2><C10>") "</B><P10>" SKIP.
       RUN PFooter.
   END.*/

   RUN pdf_close ("Spdf").
   RUN browse2pdf\viewxmldialog.w (pcRappFil, 'Polygon Retail Solutions').
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFXprintRappOms fFrameWin 
PROCEDURE PDFXprintRappOms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cUrvalsInfo    AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER cColWidth%     AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER cPageCols      AS CHARACTER NO-UNDO. /* vid för bred grid för XPrintrapport */
   DEFINE INPUT  PARAMETER cRightCols     AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  pcRappFil      AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  iCount         AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iCount0        AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iCount2        AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  cLabelStr      AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  cLabelStrTMP   AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  cRowStr        AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  cFormatStr     AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  cFormatStrTMP  AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  iColTmp        AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iCount3        AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iFirstSumCol   AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iFirstPageCol  AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iCountPageCols AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  cRight         AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  iSidNr         AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iRadNr         AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  lMindrePG1     AS LOGICAL   NO-UNDO.
   DEFINE        VARIABLE  lNumeric       AS LOGICAL   NO-UNDO.
   DEFINE        VARIABLE  lDecimal       AS LOGICAL   NO-UNDO.

   /* skapa en styrsträng for rapportrader */
   IF cExtraInfo <> "" THEN
       ASSIGN lMindrePG1 = TRUE.  
   IF NUM-ENTRIES(cRightCols) <> NUM-ENTRIES(cPageCols) THEN DO:
/*        MESSAGE "Feil i 'RIGHT'-string"        */
/*            VIEW-AS ALERT-BOX INFO BUTTONS OK. */
       ASSIGN cRightCols = FILL(",",chResultat:Cols - 2).
   END.
   EMPTY TEMP-TABLE TT_RapportRader.
   ASSIGN iFirstSumCol = IF cColLabelSave <> "" THEN INT(ENTRY(1,cColLabelSave)) + 1 ELSE ?.
   IF lOversett = TRUE THEN
     ASSIGN cHeaderTxt = "Omsättningsrapport".
   ELSE
     ASSIGN cHeaderTxt = "Omsettningsrapport".

    IF lOversett = TRUE THEN
        ASSIGN cToday = STRING(YEAR(TODAY),"9999") + "-"
                      + STRING(MONTH(TODAY),"99") + "-"
                      + STRING(DAY(TODAY),"99").
      ELSE
        ASSIGN cToday = STRING(TODAY).

   DO:
       ASSIGN cFormatStrTMP = "6"
              iColTmp       = 6
              iCount3       = 0.

       DO iCount2 = 1 TO NUM-ENTRIES(cPageCols):
           ASSIGN iCount3    = ROUND(DECI(ENTRY(iCount2,cColWidth%,CHR(1))) * 100 / 100,0)
                  iColTmp    = iColTmp + iCount3
                  cFormatStrTMP = cFormatStrTMP + "," + STRING(iColTmp).
       END.
       ASSIGN cFormatStr = cFormatStrTMP.
   END.
   /* Finn lablar */
   DO:
       ASSIGN cLabelStrTMP = "".
       DO iCount2 = 1 TO NUM-ENTRIES(cFormatStr) - 1:
           ASSIGN cRight = IF ENTRY(iCount2,cRightCols) = "1" THEN "<RIGHT=C+" + 
               STRING(INT(ENTRY(iCount2 + 1,cFormatStr)) - INT(ENTRY(iCount2,cFormatStr)) - 2) + ">" ELSE ""
                  cLabelStrTMP = cLabelStrTMP + "<C" + ENTRY(iCount2,cFormatStr) + ">" + cRight + chResultat:Cell(0,0,INT(ENTRY(iCount2,cPageCols)),0,INT(ENTRY(iCount2,cPageCols))).
       END.
       ASSIGN cLabelStr = cLabelStr + (IF cLabelStr = "" THEN "" ELSE ";") + "<U>" + cLabelStrTMP + "</U>".
   END.
   DO iCount3 = 1 TO NUM-ENTRIES(cFormatStr) - 1:
     ASSIGN cRub[iCount3] = chResultat:Cell(0,0,INT(ENTRY(iCount3,cPageCols))).
   END.
   ASSIGN iMaxRub = NUM-ENTRIES(cFormatStr) - 1.
   ASSIGN iLastRow = 0.
   ASSIGN iSidNr = 1
          iRadNr = 1.
   DO iCount = 1 TO chResultat:Rows - 1:
       IF chResultat:RowHidden(iCount) THEN
           NEXT.
       DO:
           ASSIGN iFirstPageCol = INT(ENTRY(1,cPageCols)) - 1
                  iCount3 = 0.
           DO iCount2 = 1 TO NUM-ENTRIES(cPageCols):
               ASSIGN iCount3 = iCount3 + 1
                      cRowStr = cRowStr + chResultat:Cell(0,iCount,INT(ENTRY(iCount2,cPageCols))) + ";".
           END.
           CREATE TT_RapportRader.
           ASSIGN TT_RapportRader.iPageNum = iSidNr
                  TT_RapportRader.iColPage = iCountPageCols
                  TT_RapportRader.iRadNum  = iRadNr
                  TT_RapportRader.cRadData = cRowStr
                  cRowStr = "".
           ASSIGN iLastRow = iLastRow + 1.
               IF ((lMindrePG1 = TRUE AND iRadNr = 34) OR iRadNr = 36) AND iCount < chResultat:Rows - 1 THEN
                   ASSIGN iRadNr = 1
                          iSidNr = iSidNr + 1
                          lMindrePG1 = FALSE.
               ELSE
                   ASSIGN iRadNr = iRadNr + 1.
       END.
   END.

   ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "rapp.pdf".

   RUN pdf_new ("Spdf",pcRappfil).
   pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PDFPageFooter").
   RUN pdf_set_PaperType ("Spdf","A4").
   RUN pdf_set_LeftMargin ("Spdf", 30).
   RUN pdf_set_BottomMargin ("Spdf", 40).
   RUN pdf_set_VerticalSpace ("Spdf", 13).
   RUN pdf_set_Orientation ("Spdf", "landscape").

   ASSIGN dPageWidth = pdf_PageWidth ("Spdf").

   ASSIGN dColPos[1] = 30
          dColPos[2] = 80
          dColPos[3] = 187
          dColPos[4] = 253
          dColPos[5] = 339
          dColPos[6] = 418
          dColPos[7] = 500
          dColPos[8] = 653
          dColPos[9] = 705.
   ASSIGN dColPos2[1] = 50
          dColPos2[2] = 80
          dColPos2[3] = 187
          dColPos2[4] = 273
          dColPos2[5] = 386
          dColPos2[6] = 443
          dColPos2[7] = 559
          dColPos2[8] = 676
          dColPos2[9] = 725.


/*   DO iCount3 = 2 TO NUM-ENTRIES(cColWidth%,CHR(1)):
     ASSIGN dColPos[iCount3] = dColPos[iCount3 - 1] + ((INT(ENTRY(iCount3,cColWidth%,CHR(1))) * dPageWidth) / 100 - 10).
   END.*/

   ASSIGN dColPos[3] = dColPos[3] + 10.
   ASSIGN iLastRow2 = 0.
   FOR EACH TT_RapportRader BREAK BY TT_RapportRader.iPageNum BY
                                     TT_RapportRader.iColPage BY
                                     TT_RapportRader.iRadNum: 
       ASSIGN iLastRow2 = iLastRow2 + 1.
       IF FIRST-OF(TT_RapportRader.iColPage) THEN DO:
           RUN pdf_new_page ("Spdf").
           RUN PDFPageHeader.
           ASSIGN dY = pdf_PageHeight ("Spdf") - 70.
           RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
           IF cExtraInfo <> "" THEN
           DO:
             IF lOversett = TRUE THEN
               ASSIGN cExtraInfo = REPLACE(cExtraInfo,"Butikk","Butik")
                      cExtraInfo = REPLACE(cExtraInfo,"AAR","ÅR")
                      cExtraInfo = REPLACE(cExtraInfo,"MANED","MÅNAD")
                      cExtraInfo = REPLACE(cExtraInfo,"UKE","Vecka")
                      cExtraInfo = REPLACE(cExtraInfo,"PeriodeType","Period").
             RUN pdf_text_xy_dec ("Spdf",cExtraInfo,dColPos[1],dY).
             ASSIGN cExtraInfo = "".
             ASSIGN dY = dY - 20.
           END.
           DO iCount3 = 1 TO iMaxRub:
             RUN pdf_text_xy_dec ("Spdf",cRub[iCount3],dColPos[iCount3],dY).
             ASSIGN cUL = "".
             DO iLen = 1 TO LENGTH(cRub[iCount3]):
               ASSIGN cUL = cUL + "_".
             END.
             RUN pdf_text_xy_dec ("Spdf",cUL,dColPos[iCount3],dY).
           END.
       END.
       IF (dy - 20) < 40 THEN
       DO:
         RUN pdf_new_page ("Spdf").
         RUN PDFPageHeader.
         ASSIGN dY = pdf_PageHeight ("Spdf") - 70.
         RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
           DO iCount3 = 1 TO iMaxRub:
             RUN pdf_text_xy_dec ("Spdf",cRub[iCount3],dColPos[iCount3],dY).
             ASSIGN cUL = "".
             DO iLen = 1 TO LENGTH(cRub[iCount3]):
               ASSIGN cUL = cUL + "_".
             END.
             RUN pdf_text_xy_dec ("Spdf",cUL,dColPos[iCount3],dY).
           END.
       END.

       RUN pdf_set_font ("Spdf", "Helvetica",10).
       dy = dy - 13.
       IF iLastRow = iLastRow2 THEN
         DO:
           RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
           dY = dY - 13.
         END.
       DO iCount3 = 1 TO iMaxRub:
         ASSIGN wrk = ENTRY(iCount3,TT_RapportRader.cRadData,";").
         IF ENTRY(iCount3,cRightCols) = "1" THEN
         DO:
           RUN PDFNumCheck2 (wrk,OUTPUT lNumeric, OUTPUT lDecimal).
           IF lNumeric = TRUE THEN
             RUN pdf_text_xy_dec ("Spdf",wrk,dColPos[iCount3],dY).
           ELSE
           DO:
             ASSIGN dBelopp = DEC(ENTRY(iCount3,TT_RapportRader.cRadData,";")).
             IF lDecimal = TRUE THEN
               ASSIGN cStr = TRIM(STRING(dBelopp,"->>>>>>>>9.99")).
             ELSE
               ASSIGN cStr = TRIM(STRING(dBelopp,"->>>>>>>>>>>>9")).
             RUN pdf_text_xy_dec ("Spdf",cStr,dColPos2[iCount3] - bredd(cStr),dY).
           END.
/*           ASSIGN dBelopp = DEC(ENTRY(iCount3,TT_RapportRader.cRadData,";")).
           IF dBelopp = 0 THEN
             ASSIGN cStr = " ".
           ELSE IF iCount3 > 4 THEN
             ASSIGN cStr = TRIM(STRING(dBelopp,"->>,>>>,>>9.99")).
/*                      cbelopp = FILL(" ",14 -  LENGTH(cBelopp)) + cBelopp.

             ASSIGN cStr = TRIM(STRING(dBelopp),"ZZZ.ZZZ.ZZ9,99").*/
           ELSE
             ASSIGN cStr = TRIM(STRING(dBelopp)).
           RUN pdf_text_xy_dec ("Spdf",cStr,dColPos2[iCount3] - bredd(cStr),dY).*/
         END.
         ELSE
           RUN pdf_text_xy_dec ("Spdf",wrk,dColPos[iCount3],dY).
       END.
   END.
   RUN pdf_close ("Spdf").
   RUN browse2pdf\viewxmldialog.w (pcRappFil, 'Polygon Retail Solutions').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PFooter fFrameWin 
PROCEDURE PFooter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
              PUT UNFORMATTED "<R45><C6><FROM><R45><C113><LINE>" SKIP
                              "<C6>" cKundenavn "<C1><CENTER=C113>" cPolygon "<C105>" STRING(TODAY) SKIP.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PreHiddenExcel fFrameWin 
PROCEDURE PreHiddenExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ii               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cPrintKunColsTmp AS CHARACTER NO-UNDO.

    cPrintKunColsTmp = cPrintKunCols.
    DO ii = 1 TO chResultat:Cols - 1:
        IF chResultat:ColHidden(ii) THEN
            NEXT.
        cPrintNoHiddCols = cPrintNoHiddCols + (IF cPrintNoHiddCols <> "" THEN "," ELSE "") + STRING(ii).
    END.
    cPrintKunCols = cPrintNoHiddCols.
    RUN VisaIExcel.
    cPrintKunCols = cPrintKunColsTmp.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintGrid fFrameWin 
PROCEDURE PrintGrid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cReportType  AS CHARACTER          NO-UNDO.
    DEFINE INPUT  PARAMETER cUrvalsInfo  AS CHARACTER          NO-UNDO.
    DEFINE INPUT  PARAMETER iOrientation AS INTEGER            NO-UNDO.
    DEFINE INPUT  PARAMETER cField#List  AS CHARACTER          NO-UNDO.
    DEFINE INPUT  PARAMETER cRightCols   AS CHARACTER          NO-UNDO.
    DEFINE VARIABLE         iCount       AS INTEGER            NO-UNDO.
    DEFINE VARIABLE         tottwipsMAX  AS INTEGER            NO-UNDO.
    DEFINE VARIABLE         tottwipsTMP  AS INTEGER            NO-UNDO.
    DEFINE VARIABLE         cProcent     AS CHARACTER          NO-UNDO.
    DEFINE VARIABLE         dTotProcent  AS DECIMAL            NO-UNDO.
    DEFINE VARIABLE         iMaxTwips    AS INTEGER INIT 13000 NO-UNDO.
    DEFINE VARIABLE         iFirstCol    AS INTEGER            NO-UNDO.
    DEFINE VARIABLE         cPageCols    AS CHARACTER          NO-UNDO. /* vid för bred grid för XPrintrapport */
    DEFINE VARIABLE         iiCnt        AS INTEGER            NO-UNDO.

    IF chResultat:Rows = 1 THEN
        RETURN.
    ASSIGN cTitle = ENTRY(1,cUrvalsInfo,CHR(1)).
    IF NUM-ENTRIES(cTitle,CHR(2)) = 2 THEN
        ASSIGN cExtraInfo = ENTRY(2,cTitle,CHR(2))
               cTitle     = ENTRY(1,cTitle,CHR(2)).
    ELSE 
        ASSIGN cExtraInfo  = "".
    IF NUM-ENTRIES(cUrvalsInfo,CHR(1)) = 2 THEN
        ASSIGN cUrvalsInfo = ENTRY(2,cUrvalsInfo,CHR(1)).
    ELSE
        ASSIGN cUrvalsInfo = "".
    IF cReportType = "QUICK" THEN DO:
        chResultat:GridLines = 12.
        chResultat:PrintGrid(cUrvalsInfo,1,iOrientation,720).
        chResultat:GridLines = 0.
    END.
    ELSE IF cReportType = "XPRINTOMS" THEN DO:
        /* vi förutsätter att dnna får plats på 1 sida */
        DO iCount = 1 TO NUM-ENTRIES(cField#List):
            ASSIGN tottwipsTMP = tottwipsTMP + chResultat:ColWidth(INT(ENTRY(iCount,cField#List))).
        END.
        DO iCount = 1 TO NUM-ENTRIES(cField#List):
            ASSIGN cProcent = cProcent + (IF cProcent = "" THEN "" ELSE CHR(1)) + 
                    STRING(ROUND(chResultat:ColWidth(INT(ENTRY(iCount,cField#List))) / tottwipsTMP * 100,1)).
        END.
        RUN PDFXPrintRappOms (cUrvalsInfo,cProcent,cField#List,cRightCols).
    END.
    ELSE IF cReportType = "XPRINT" THEN DO:
      ASSIGN iFirstCol = 1.
      DO icount = 1 TO chResultat:Cols - 1:
/*           IF chResultat:ColHidden(icount) = TRUE THEN */
/*               NEXT.                                   */
          iiCnt = iiCnt + 1.
          IF tottwipsTMP + chResultat:ColWidth(icount) < iMaxTwips THEN
              ASSIGN tottwipsTMP = tottwipsTMP + chResultat:ColWidth(icount).
          ELSE DO:
              ASSIGN tottwipsMAX = MAX(tottwipsMAX,tottwipsTMP).
              ASSIGN cPageCols = cPageCols + (IF cPageCols = "" THEN "" ELSE ";") + STRING(iFirstCol) + "," + STRING(iCount - 1). /*  */
              iFirstCol = iCount.
              ASSIGN tottwipsTMP = chResultat:ColWidth(icount).
          END.
      END.
      IF cPageCols = "" THEN
          ASSIGN tottwipsMAX = tottwipsTMP
                 cPageCols = "1" + "," + STRING(chResultat:Cols - 1).
      ELSE DO:
          ASSIGN tottwipsMAX = MAX(tottwipsMAX,tottwipsTMP)
                 cPageCols = cPageCols + ";" + STRING(iFirstCol) + "," + STRING(chResultat:Cols - 1).
      END.
      DO icount = 1 TO chResultat:Cols - 1:
          ASSIGN cProcent = cProcent + (IF cProcent = "" THEN "" ELSE CHR(1)) + 
              STRING(ROUND(chResultat:ColWidth(icount) / tottwipsMAX * 100,1))
              dTotProcent = dTotProcent + ROUND(chResultat:ColWidth(icount) / tottwipsMAX * 100,1).
      END.
/*       RUN XPrintRapp (cUrvalsInfo,cProcent). */

/*       RUN XPrintRapp2 (cUrvalsInfo,cProcent,cPageCols,cRightCols). */
      RUN PDFXPrintRapp2 (cUrvalsInfo,cProcent,cPageCols,cRightCols).
/*       RUN PDFXPrintRapp2X (cUrvalsInfo,cProcent,cPageCols,cRightCols). */
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetDateFormat fFrameWin 
PROCEDURE SetDateFormat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER iCol    AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER cFormat AS CHARACTER NO-UNDO.

/*   chResultat:ColFormat(iCol) = cFormat. */
/*   chResultat:ColDataType(iCol) = 7. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetRowBold fFrameWin 
PROCEDURE SetRowBold :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iCol     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER cScanTxt AS CHARACTER NO-UNDO.
    DEFINE VARIABLE         iWidth   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE         iCount   AS INTEGER   NO-UNDO.

    DO iCount = 1 TO chResultat:Rows - 1:
        IF CAN-DO(cScanTxt,chResultat:Cell(0,iCount,iCol,iCount,iCol)) THEN
           ASSIGN chResultat:Cell(13,iCount,0,iCount,chResultat:Cols - 1) = TRUE.
    END.
    chResultat:AutoSize(iCol,chResultat:Cols - 1).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowBold fFrameWin 
PROCEDURE ShowBold :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cWhatBold AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  iCount    AS INTEGER   NO-UNDO.

   IF NOT chResultat:Rows > 1 THEN
       RETURN.
   IF ENTRY(1,cWhatBold) = "C" THEN DO iCount = 1 TO chResultat:Rows - 1:
       chResultat:Cell(13,iCount,INT(ENTRY(2,cWhatBold)),iCount,INT(ENTRY(2,cWhatBold))) = TRUE
       .
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Summer fFrameWin 
PROCEDURE Summer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cSumWhat    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER cColLabel   AS CHARACTER NO-UNDO.
    DEFINE        VARIABLE  iCount      AS INTEGER   NO-UNDO.
    DEFINE        VARIABLE  iCount2     AS INTEGER   NO-UNDO.
    DEFINE        VARIABLE  dColSum     AS DECIMAL   NO-UNDO.
    DEFINE        VARIABLE  cTmpVerdi   AS CHARACTER NO-UNDO.
    DEFINE        VARIABLE  cSumWhatTmp AS CHARACTER NO-UNDO.
    DEFINE        VARIABLE  cKalk       AS CHARACTER NO-UNDO.
    DEFINE        VARIABLE  cKalkUnder  AS CHARACTER NO-UNDO.
    DEFINE        VARIABLE  iAntDec     AS INTEGER   NO-UNDO.
    DEFINE        VARIABLE  iKalkCol    AS INTEGER   NO-UNDO.
    DEFINE        VARIABLE  cKalkOver   AS CHARACTER NO-UNDO.
    DEFINE        VARIABLE  dKalkOver   AS DECIMAL   NO-UNDO.
    DEFINE        VARIABLE  dKalkUnder  AS DECIMAL   NO-UNDO.
    DEFINE        VARIABLE  dKalk%      AS DECIMAL   NO-UNDO.
    DEFINE        VARIABLE  iColTmp     AS INTEGER   NO-UNDO.
    DEFINE        VARIABLE  iCFcol      AS INTEGER   NO-UNDO.
lSummarad = FALSE.
    IF NOT chResultat:Rows > 1 THEN
        RETURN.
    ASSIGN cColLabelSave   = cColLabel
           cSumWhatSave    = cSumWhat
           cSumWhatTmp     = ENTRY(1,cSumWhat,";")
           chResultat:Rows = chResultat:Rows + 1
           chResultat:Cell(0,chResultat:Rows - 1,INT(ENTRY(1,cColLabel)),chResultat:Rows - 1,INT(ENTRY(1,cColLabel))) = ENTRY(2,cColLabel).
    DO iCount = 1 TO NUM-ENTRIES(cSumWhatTmp):
        ASSIGN dColSum = 0
               cTmpVerdi = chResultat:Cell(0,1,INT(ENTRY(iCount,cSumWhatTmp)),1,INT(ENTRY(iCount,cSumWhatTmp))).
        DO iCount2 = 1 TO chResultat:Rows - 2:
            IF chResultat:RowHidden(iCount2) THEN
                NEXT.
            ASSIGN dColSum = dColSum + DECI(chResultat:Cell(0,iCount2,INT(ENTRY(iCount,cSumWhatTmp)),iCount2,INT(ENTRY(iCount,cSumWhatTmp)))).
        END.
        ASSIGN chResultat:Cell(0,chResultat:Rows - 1,INT(ENTRY(iCount,cSumWhatTmp)),chResultat:Rows - 1,INT(ENTRY(iCount,cSumWhatTmp))) =
            IF NUM-ENTRIES(cTmpVerdi) = 2 AND LENGTH(ENTRY(2,cTmpVerdi)) = 2 THEN TRIM(STRING(dColSum,"->>>>>>>>>9.99"))
                ELSE IF NUM-ENTRIES(cTmpVerdi) = 2 AND LENGTH(ENTRY(2,cTmpVerdi)) = 1 THEN TRIM(STRING(dColSum,"->>>>>>>>>9.9"))
                ELSE TRIM(STRING(dColSum,"->>>>>>>>>9")).
        IF INT(ENTRY(iCount,cSumWhatTmp)) <> iColTmp THEN DO:
            iColTmp = INT(ENTRY(iCount,cSumWhatTmp)).
            chResultat:ColFormat(iColTmp) =
            IF NUM-ENTRIES(cTmpVerdi) = 2 AND LENGTH(ENTRY(2,cTmpVerdi)) = 2 THEN "###,###,###.##"
                ELSE IF NUM-ENTRIES(cTmpVerdi) = 2 AND LENGTH(ENTRY(2,cTmpVerdi)) = 1 THEN "###,###,###.#"
                ELSE "###,###,###".
            lSummarad = TRUE.
        END.
    END.
    iColTmp = 0.
    /* entry 2 är kalkyleringssträngar */
    IF NUM-ENTRIES(cSumWhat,";") >= 2 THEN DO iCount = 2 TO NUM-ENTRIES(cSumWhat,";"):
        IF TRIM(ENTRY(iCount,cSumWhat,";")) = "" THEN
            LEAVE.
        ASSIGN cKalk = ENTRY(iCount,cSumWhat,";")
               iAntDec    = INT(ENTRY(1,cKalk))
               iKalkCol   = INT(ENTRY(2,cKalk))
               cKalkOver  = ENTRY(3,cKalk)
               cKalkUnder = ENTRY(4,cKalk).
        ASSIGN iColTmp = INT(TRIM(TRIM(ENTRY(1,cKalkOver,"|"),"-"),"+"))
               dKalkOver = DECI(chResultat:Cell(0,chResultat:Rows - 1,iColTmp,chResultat:Rows - 1,iColTmp)).
        DO iCount2 = 2 TO NUM-ENTRIES(cKalkOver,"|"):
            ASSIGN iColTmp = INT(TRIM(TRIM(ENTRY(iCount2,cKalkOver,"|"),"-"),"+"))
                   dKalkOver = dKalkOver + (IF ENTRY(iCount2,cKalkOver,"|") BEGINS "-" THEN -1 ELSE 1) * DECI(chResultat:Cell(0,chResultat:Rows - 1,iColTmp,chResultat:Rows - 1,iColTmp)).
        END.
        ASSIGN iColTmp = INT(TRIM(TRIM(ENTRY(1,cKalkUnder,"|"),"-"),"+"))
               dKalkUnder = DECI(chResultat:Cell(0,chResultat:Rows - 1,iColTmp,chResultat:Rows - 1,iColTmp)).
        DO iCount2 = 2 TO NUM-ENTRIES(cKalkUnder,"|"):
            ASSIGN iColTmp = INT(TRIM(TRIM(ENTRY(iCount2,cKalkUnder,"|"),"-"),"+"))
                   dKalkUnder = dKalkUnder + (IF ENTRY(iCount2,cKalkUnder,"|") BEGINS "-" THEN -1 ELSE 1) * DECI(chResultat:Cell(0,chResultat:Rows - 1,iColTmp,chResultat:Rows - 1,iColTmp)).
        END.
        ASSIGN dKalk% = IF dKalkUnder <> 0 THEN ROUND(dKalkOver / dKalkUnder * 100,iAntDec) ELSE 0
               chResultat:Cell(0,chResultat:Rows - 1,iKalkCol,chResultat:Rows - 1,iKalkCol) = IF iAntDec = 1 THEN STRING(dKalk%,"->>>>>9.9") ELSE
                                                                                              IF iAntDec = 2 THEN STRING(dKalk%,"->>>>>9.99") ELSE "->>>>>9".
        IF iKalkCol <> iColTmp THEN DO:
            iColTmp = iKalkCol.
            chResultat:ColFormat(iKalkCol) = IF iAntDec = 2 THEN "###,###,###.##" ELSE
                                             IF iAntDec = 1 THEN "###,###,###.#" ELSE "###,###,###".
        END.
    END.
    iColTmp = 0.
    /* Detta är en specialare för Solgt% när man väljer delar av resultatet */
    /* skall andelens solgt av totalen visas */
    IF cXSolgt% <> "" AND chResultat:Cell(5,0,0,0,0) = 1 THEN DO:
        ASSIGN iAntDec    = INT(ENTRY(1,cXSolgt%))
               iKalkCol   = INT(ENTRY(2,cXSolgt%)) /* resultatcol */
               /* ENTRY(3,cXSolgt%) = den col där data finns      */
               /* idetta fall skall totsolgt för delmängden delas med */
               /* totsolgt för hela mängden  hela mängden finns hidden på raden ovanför */
               iColTmp    = INT(ENTRY(3,cXSolgt%))
               dKalkOver  = DECI(chResultat:Cell(0,chResultat:Rows - 1,iColTmp,chResultat:Rows - 1,iColTmp))
               dKalkunder = DECI(chResultat:Cell(0,chResultat:Rows - 2,iColTmp,chResultat:Rows - 2,iColTmp))
               dKalk% = IF dKalkUnder <> 0 THEN ROUND(dKalkOver / dKalkUnder * 100,iAntDec) ELSE 0
               chResultat:Cell(0,chResultat:Rows - 1,iKalkCol,chResultat:Rows - 1,iKalkCol) = IF iAntDec = 1 THEN STRING(dKalk%,"->>>9.9") ELSE
                                                                                              IF iAntDec = 2 THEN STRING(dKalk%,"->>>9.99") ELSE "->>>9".
         IF iKalkCol <> iColTmp THEN DO:
             iColTmp = iKalkCol.
             chResultat:ColFormat(iKalkCol) = IF iAntDec = 2 THEN "###,###,###.##" ELSE
                                              IF iAntDec = 1 THEN "###,###,###.#" ELSE "###,###,###".
         END.
    END.
/*     IF NUM-ENTRIES(cSumWhat,";") > 1 THEN DO iCount = 2 TO NUM-ENTRIES(cSumWhat,";"):                                                                                            */
/*         ASSIGN cKalk = ENTRY(iCount,cSumWhat,";")                                                                                                                                */
/*                iAntDec    = INT(ENTRY(1,cKalk))                                                                                                                                  */
/*                iKalkCol   = INT(ENTRY(2,cKalk))                                                                                                                                  */
/*                dKalkOver  = DECI(chResultat:Cell(0,chResultat:Rows - 1,INT(ENTRY(3,cKalk)),chResultat:Rows - 1,INT(ENTRY(3,cKalk))))                                             */
/*                cKalkUnder   = ENTRY(4,cKalk)                                                                                                                                       */
/*                dKalkUnder = DECI(chResultat:Cell(0,chResultat:Rows - 1,INT(ENTRY(1,cKalkUnder,"-")),chResultat:Rows - 1,INT(ENTRY(1,cKalkUnder,"-")))).                              */
/*         DO iCount2 = 2 TO NUM-ENTRIES(cKalkUnder,"-"):                                                                                                                             */
/*             ASSIGN dKalkUnder = dKalkUnder - DECI(chResultat:Cell(0,chResultat:Rows - 1,INT(ENTRY(iCount2,cKalkUnder,"-")),chResultat:Rows - 1,INT(ENTRY(iCount2,cKalkUnder,"-")))). */
/*         END.                                                                                                                                                                     */
/*         ASSIGN dKalk% = IF dKalkUnder <> 0 THEN ROUND(dKalkOver / dKalkUnder * 100,iAntDec) ELSE 0                                                                               */
/*                chResultat:Cell(0,chResultat:Rows - 1,iKalkCol,chResultat:Rows - 1,iKalkCol) = IF iAntDec = 1 THEN STRING(dKalk%,"->>>9.9") ELSE                                  */
/*                                                                                               IF iAntDec = 2 THEN STRING(dKalk%,"->>>9.99") ELSE "->>>9".                        */
/*     END.                                                                                                                                                                         */
/*     cSumWhat  = "4,5,6"  */
/*     cColLabel = "3,Sum". */

    chResultat:Cell(13,chResultat:Rows - 1,INT(ENTRY(1,cColLabel)),chResultat:Rows - 1,chResultat:Cols - 1) = TRUE.
    chResultat:IsSubtotal(chResultat:Rows - 1) = TRUE.
/*     chResultat:AutoSize(INT(ENTRY(1,cSumWhatTmp)),INT(ENTRY(NUM-ENTRIES(cSumWhatTmp),cSumWhatTmp))). */
    chResultat:AutoSize(1,chResultat:Cols - 1).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToExcelFil fFrameWin 
PROCEDURE ToExcelFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  cFileName  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER iSumRad    AS INTEGER   NO-UNDO.
    DEFINE       VARIABLE   iCount     AS INTEGER   NO-UNDO.
    DEFINE       VARIABLE   iCount2    AS INTEGER   NO-UNDO.
    DEFINE       VARIABLE   cExportRad AS CHARACTER NO-UNDO.
    DEFINE       VARIABLE   iPerCol    AS INTEGER    NO-UNDO.

    DO iCount = 0 TO chResultat:Cols - 1:
        IF chResultat:Cell(0,0,iCOunt,0,iCount) MATCHES("*period*") THEN DO:
            iPerCol = iCount.
            LEAVE.
        END.
    END.

    OUTPUT TO VALUE(cFileName).
    DO iCount = 0 TO chResultat:Rows - 1:
       IF chResultat:RowHidden(iCount) THEN
           NEXT.
       ASSIGN cExportRad = "".
       DO iCount2 = 0 TO chResultat:Cols - 1:
           IF iCount2 > 0 AND cPrintKunCols <> "" AND 
               NOT CAN-DO(cPrintKunCols,STRING(iCount2)) THEN
               NEXT.
           ASSIGN cExportRad = cExportRad + (IF iCount2 = 0 THEN "" ELSE CHR(9)) +
               (IF chResultat:Cell(0,iCount,iCount2,iCount,iCount2) BEGINS "+" THEN " " ELSE "") +
                (IF iCount > 0 AND iCount2 = iPerCol AND TRIM(chResultat:Cell(0,iCount,iCount2,iCount,iCount2)) <> "" THEN " " ELSE "") +
                 chResultat:Cell(0,iCount,iCount2,iCount,iCount2).
       END.
/*        IF iCount = 1 THEN                                */
/*            MESSAGE NUM-ENTRIES(cExportRad,CHR(160))      */
/*                VIEW-AS ALERT-BOX INFO BUTTONS OK.        */
/*        IF iCount > 0 THEN                                */
/*            cExportRad = REPLACE(cExportRad,CHR(160),""). */
       PUT UNFORMATTED cExportRad SKIP.
       ASSIGN iSumRad = iSumRad + 1.
    END.
    OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisaIExcel fFrameWin 
PROCEDURE VisaIExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFileName          AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE chExcelApplication AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorkbook         AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorksheet        AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE iSumRad            AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iCount             AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cColNum            AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cColForm           AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE hh                 AS HANDLE     NO-UNDO.
    DEFINE VARIABLE iPerCol            AS INTEGER    NO-UNDO.

/*     DEFINE VARIABLE cColChr AS CHARACTER   NO-UNDO.                          */
/*     DO iCount = 0 TO chResultat:Cols - 1:                                    */
/*         IF chResultat:Cell(0,0,iCOunt,0,iCount) MATCHES("*period*") THEN DO: */
/*             iPerCol = iCount.                                                */
/*             LEAVE.                                                           */
/*         END.                                                                 */
/*     END.                                                                     */
/*     IF iPerCol > 0 THEN                                                      */
/*         cColChr = CHR(65 + iPerCol).                                         */
/* HH = DYNAMIC-FUNCTION('getPageNTarget':U IN h_Window). */
/* MESSAGE hh:NAME                                        */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                 */
    IF chResultat:Rows = 1 THEN
        RETURN.
    ASSIGN cFileName = SESSION:TEMP-DIRECTORY + "GridRap" + STRING(TIME) + ".xls".
    DO iCount = 1 TO chResultat:Cols - 1:
        IF NUM-ENTRIES(chResultat:ColFormat(iCount)) > 1 THEN DO:
            cColForm = cColForm + (IF cColForm <> "" THEN ";" ELSE "") + chResultat:ColFormat(iCount).
            cColNum = cColNum + (IF cColNum <> "" THEN ";" ELSE "") + STRING(iCount).
            chResultat:ColFormat(iCount) = REPLACE(chResultat:ColFormat(iCount),",","").
        END.
    END.
    chResultat:REFRESH().
    IF chResultat:Cell(5,0,0,0,0) = 0 AND cPrintKunCols = "" THEN
        chResultat:SaveGrid(cFileName,4,TRUE) NO-ERROR.
    ELSE DO:
    /* SaveGrid Tar med alla rader, æven hidden -> egen rutin før dumpen */
        RUN ToExcelFil(cFileName,OUTPUT iSumRad).
    END.
    IF cColNum <> "" THEN DO iCount = 1 TO NUM-ENTRIES(cColNum,";"):
        chResultat:ColFormat(INT(ENTRY(iCount,cColNum,";"))) = ENTRY(iCount,cColForm,";").
    END.
    chResultat:REFRESH().

    DEFINE VARIABLE cOpenOffice AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOpenOffice AS LOGICAL   NO-UNDO.

    {syspara.i 1 1 210 cOpenOffice}

    lOpenOffice = IF cOpenOffice NE "" THEN TRUE ELSE FALSE.

    IF lOpenOffice THEN
    DO:
       RUN OpenFileInOpenOffice.p (cFilename).
    END.
    ELSE
    DO:
      CREATE "Excel.Application" chExcelApplication.

    /* launch Excel so it is not visible to the user */
    /* create a new Workbook */
        ASSIGN
/*     chWorkbook = chExcelApplication:Workbooks:OpenText(cFileName,2,,,,,TRUE) */
      chWorkbook = chExcelApplication:Workbooks:OPEN(cFileName)
      chWorksheet = chExcelApplication:Sheets:ITEM(1)
      chWorkSheet:PageSetup:Orientation    = 2 /* landscape */
      chWorkSheet:Range("A1:CC1"):Font:Bold = TRUE.
      chWorkSheet:Columns("A:CC"):AutoFit().
/*     IF cColChr <> "" THEN DO:                                  */
/*         chWorkSheet:Columns(cColChr + ":" + cColChr):Select(). */
/*         chExcelApplication:Selection:NumberFormat = "@".       */
/*     END.                                                       */
      IF cSumWhatSave <> "" THEN DO:
        IF chResultat:Cell(5,0,0,0,0) = 0 THEN
            ASSIGN iSumRad = chResultat:Rows.
        chWorkSheet:Range("A" + STRING(iSumRad) + ":CC" + STRING(iSumRad)):Font:Bold = TRUE.
      END.
      chExcelApplication:Visible = TRUE.
      chWorkbook:Saved = TRUE.
      RELEASE OBJECT chWorkSheet NO-ERROR.
      RELEASE OBJECT chWorkbook NO-ERROR.
      RELEASE OBJECT chExcelApplication NO-ERROR.
      ASSIGN chWorksheet        = ?
             chWorkbook         = ?
             chExcelApplication = ?.
/*     chResultat:SaveGrid(cFileName,4,TRUE). */
/*     RUN OpenDoc.p ("EX",cFileName).        */
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisaIExcelKolonnevalg fFrameWin 
PROCEDURE VisaIExcelKolonnevalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcProgram   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFane      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE         iCol         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE         cColLabels   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE         iReturnValue AS INTEGER   NO-UNDO.

    IF chResultat:Rows = 1 THEN
        RETURN.
    DO iCol = 1 TO chResultat:Cols - 1:
        ASSIGN cColLabels = cColLabels + 
                               (IF cColLabels <> "" THEN "," ELSE "") + 
                               chResultat:TextMatrix(0,iCol).
    END.
    FIND TT_Preselect WHERE
        TT_Preselect.cProgName = ipcProgram AND
        TT_Preselect.iFane     = ipiFane NO-LOCK NO-ERROR.
    IF AVAIL TT_Preselect AND TT_Preselect.ColLabels = cColLabels THEN
        cPrintKunCols = TT_Preselect.cPreSelect.
    ELSE IF AVAIL TT_Preselect THEN
        DELETE TT_Preselect.
    RUN JBoxDynFieldChoose.w (THIS-PROCEDURE:CURRENT-WINDOW,?,cColLabels,"ROW",INPUT-OUTPUT cPrintKunCols,OUTPUT iReturnValue).
    IF iReturnValue = 0 OR (iReturnValue = 2 AND cPrintKunCols = "") THEN
        RETURN.
    IF NOT AVAIL TT_Preselect THEN DO:
        CREATE TT_Preselect.
        ASSIGN TT_Preselect.cProgName = ipcProgram
               TT_Preselect.ColLabels = cColLabels
               TT_Preselect.iFane     = ipiFane.
    END.
    ASSIGN TT_Preselect.cPreselect = cPrintKunCols.
    {sww.i}
    RUN VisaIExcel.
    {swn.i}
    ASSIGN cPrintKunCols = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisaNotepad fFrameWin 
PROCEDURE VisaNotepad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iCount      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCount2     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCount3     AS INTEGER     NO-UNDO.
DEFINE VARIABLE cTab        AS CHARACTER EXTENT 8 NO-UNDO.
DEFINE VARIABLE cBelopp     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dBelopp     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iAnt        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iAnt2       AS INTEGER     NO-UNDO.
DEFINE VARIABLE cKundNamn   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cApoteksfil AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lFirsta     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cAr         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMan        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iButik      AS INTEGER     NO-UNDO.
DEFINE VARIABLE cWrk        AS CHARACTER   NO-UNDO.

/*  {syspara.i 1 1 103 cKundNamn}

  IF cKundNamn = "" THEN
  DO:
    MESSAGE "Försäljningsställe saknas! " SKIP "(1/1/103)"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.*/

/*DEFINE TEMP-TABLE TT_Apotek NO-UNDO
    FIELD cFsgStalle AS CHARACTER
    FIELD cButik     AS CHARACTER
    FIELD cPeriodAr  AS CHARACTER
    FIELD cPeriodMan AS CHARACTER
    FIELD cVersion   AS CHARACTER
    FIELD cArtNr     AS CHARACTER
    FIELD cAntal     AS CHARACTER
    FIELD cBelopp    AS CHARACTER
    INDEX Apoteki cPeriodAr cButik cPeriodMan.*/
FOR EACH TT_Apotek:
  DELETE TT_Apotek.
END.

ASSIGN lFirsta = TRUE.
DO iCount = 1 TO chResultat:Rows - 2:
  DO iCount2 = 1 TO 8:
    ASSIGN cTab[iCount2] = chResultat:Cell(0,iCount,iCount2,iCount,iCount2).
  END.
  CREATE TT_Apotek.
  IF lFirsta = TRUE THEN
  DO:
    ASSIGN iButik = INT(cTab[4]).
    ASSIGN lFirsta = FALSE.
    {syspara.i 50 60 INT(cTab[4]) cKundNamn}
    IF cKundNamn = "" THEN
    DO:
      MESSAGE "Försäljningsställe saknas! " SKIP "(50/60/" INT(cTab[4]) ")"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
    END.
  END.
/*  ELSE IF iButik <> INT(cTab[4]) THEN
  DO:
      ASSIGN iButik = INT(cTab[4]).
      ASSIGN lFirsta = FALSE.
      {syspara.i 50 60 INT(cTab[4]) cKundNamn}
      IF cKundNamn = "" THEN
      DO:
        MESSAGE "Försäljningsställe saknas! " SKIP "(50/60/" INT(cTab[4]) ")"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
      END.
  END.*/

  ASSIGN TT_Apotek.cFsgStalle = cKundNamn
/*         iButik               = INT(cTab[4])*/
         TT_Apotek.cButik     = cTab[4]
         TT_Apotek.cPeriodAr  = SUBSTRING(cTab[6],1,4)
         TT_Apotek.cPeriodMan = SUBSTRING(cTab[6],6,2)
         TT_Apotek.cVersion   = "1"
         TT_Apotek.cArtNr     = cTab[1]
         dBelopp              = DEC(cTab[7])
         TT_Apotek.cAntal     = TRIM(STRING(dBelopp,"->>>>9"))
         cBelopp              = cTab[8].
/*         TT_Apotek.cBelopp    = TRIM(STRING(dBelopp,"->>>>9,99")).*/
  ASSIGN iLen = LENGTH(cBelopp)
         cWrk = SUBSTRING(cBelopp,1,(iLen - 3))
         cWrk = cWrk + "."
         cWrk = cWrk + SUBSTRING(cBelopp,(iLen - 1),iLen)
         TT_Apotek.cBelopp = cWrk.
END.

ASSIGN iAnt = 0.
ASSIGN lFirsta = TRUE.
FOR EACH TT_Apotek:
  ASSIGN iAnt = iAnt + 1.
    IF lFirsta = TRUE THEN
    DO:
      ASSIGN lFirsta = FALSE
             cAR = TT_Apotek.cPeriodAr
             cMan = TT_Apotek.cPeriodMan. 
      FIND FIRST Butiker WHERE Butiker.Butik = INT(TT_Apotek.cButik) NO-ERROR.
      IF AVAILABLE Butiker THEN 
        ASSIGN cKundNamn = Butiker.ButNamn.
      ELSE
        ASSIGN cKundNamn = TT_Apotek.cFsgStalle.
/*      IF AVAILABLE Butiker AND Butiker.OrganisasjonsNr <> "" THEN
        ASSIGN TT_Apotek.cButik = Butiker.OrganisasjonsNr + "-" + STRING(Butiker.Butik).*/
    END.
END.
/*MESSAGE "iAnt " iAnt
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
ASSIGN cApoteksfil = SESSION:TEMP-DIR + "Apoteksfil_" + TRIM(STRING(iButik)) + "_" + cAR + "_" + cMan + ".txt".
/*ASSIGN cApoteksfil = SESSION:TEMP-DIR + "Apoteksfil_" + cAR + "_" + cMan + ".txt".*/
/*ASSIGN cApoteksfil = SESSION:TEMP-DIR + "Apoteksfil.txt".*/

ASSIGN iAnt = 0.
ASSIGN iAnt2 = 0.
OUTPUT STREAM Ut TO VALUE(cApoteksfil) NO-ECHO.

FOR EACH TT_Apotek:
  IF DEC(TT_Apotek.cAntal) = 0 AND
     DEC(TT_Apotek.cBelopp) = 0 THEN
  DO:
    ASSIGN iAnt2 = iAnt2 + 1.
    NEXT.
  END.
  ASSIGN iAnt = iAnt + 1.
  PUT STREAM Ut UNFORMATTED cKundNamn + CHR(9) +
                  TT_Apotek.cFsgStalle + CHR(9) +
/*                  TT_Apotek.cButik + CHR(9) +   */
                  TT_Apotek.cPeriodAr + CHR(9) + 
                  TT_Apotek.cPeriodMan + CHR(9) +
                  TT_Apotek.cVersion + CHR(9) +  
                  TT_Apotek.cArtNr + CHR(9) +    
                  TT_Apotek.cAntal + CHR(9) +    
                  TT_Apotek.cBelopp SKIP. /*+ CHR(9) +
                  STRING(iAnt) SKIP.*/
END.

OUTPUT STREAM Ut CLOSE.

/*MESSAGE "iAnt " iAnt SKIP "iAnt2 " iAnt2
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

  IF SEARCH(cApoteksfil) <> ? THEN
  DO:
    DEF VAR hInstance AS INT.

    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH(cApoteksfil),
                                  "",
                                  1,
                                  OUTPUT hInstance).

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisKun fFrameWin 
PROCEDURE VisKun :
/*------------------------------------------------------------------------------
  Purpose: Skjul kolonner som inte finns i liste    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cVisListe AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER cVisType  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE         ii        AS INTEGER   NO-UNDO.

    IF cVisListe <> "" THEN
        IF VALID-HANDLE(h_Window) THEN
    RUN VisVisAlleKnapp IN h_Window (TRUE).

    IF cVisListe <> "" THEN
        cVisSave = cVisListe.
    ELSE
        cVisListe = cVisSave.
    IF cVisType = "SKJUL" AND cVisListe <> "" THEN DO:
        DO ii = 1 TO chResultat:Cols - 1:
             chResultat:ColHidden(ii) = NOT CAN-DO(cVisListe,STRING(ii)).
        END.
    END.
    ELSE DO ii = 1 TO chResultat:Cols - 1:
        chResultat:ColHidden(ii) = FALSE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisTxtBox fFrameWin 
PROCEDURE VisTxtBox :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cTxt AS CHARACTER  NO-UNDO.
  IF cTxt = "" THEN
      FRAME FRAME-Txt:MOVE-TO-BOTTOM().
  ELSE DO:
      IF FRAME FRAME-Txt:HIDDEN THEN DO:
          FRAME FRAME-Txt:HIDDEN = FALSE.
          FI-Txt:HIDDEN = FALSE.
      END.
      ASSIGN FI-Txt:SCREEN-VALUE = cTxt.
      FRAME FRAME-Txt:MOVE-TO-TOP().
  END.
  PROCESS EVENTS.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE X%Solgt fFrameWin 
PROCEDURE X%Solgt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcXSolgt% AS CHARACTER  NO-UNDO.
    ASSIGN cXSolgt% = ipcXSolgt%.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE XPrintRapp fFrameWin 
PROCEDURE XPrintRapp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cUrvalsInfo  AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER cColWidth%   AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE  pcRappFil    AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE  iCount       AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE  iCount2      AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE  cLabelStr    AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE  cRowStr      AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE  cFormatStr   AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE  iColTmp      AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE  iCount3      AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE  iFirstSumCol AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE  cRight       AS CHARACTER  NO-UNDO.

   /* skapa en styrsträng for rapportrader */
   ASSIGN cFormatStr = "6"
          iColTmp    = 6
          iFirstSumCol = IF cColLabelSave <> "" THEN INT(ENTRY(1,cColLabelSave)) + 1 ELSE ?.
   DO iCount = 1 TO NUM-ENTRIES(cColWidth%,CHR(1)): /* -1 */
       ASSIGN iCount3    = ROUND(DECI(ENTRY(iCount,cColWidth%,CHR(1))) * 100 / 100,0)
              iColTmp    = iColTmp + iCount3
              cFormatStr = cFormatStr + "," + STRING(iColTmp).
   END.
   /* Finn lablar */
   DO iCount = 1 TO chResultat:Cols - 1:
       ASSIGN cLabelStr = cLabelStr + "<C" + ENTRY(iCount,cFormatStr) + ">" + chResultat:Cell(0,0,iCount,0,iCount).
   END.
   ASSIGN cLabelStr = "<U>" + cLabelStr + "</U>"
          pcRappFil = SESSION:TEMP-DIRECTORY + "rapp.xpr".
   OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(80).
   PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
   PUT CONTROL '<PREVIEW=ZoomToWidth><OLANDSCAPE>'.

   VIEW FRAME PageHeader.

   PUT UNFORMATTED cLabelStr SKIP.
   /* Print rapporlabels */
   DO iCount = 1 TO chResultat:Rows - 1:
/*        IF chResultat:RowHidden(iCount) THEN */
/*            NEXT.                            */
           ASSIGN cRowStr = "".
       DO iCount2 = 1 TO chResultat:Cols - 1:
           ASSIGN cRight = IF iFirstSumCol <> ? AND iCount2 >= iFirstSumCol THEN "<RIGHT=C+" + 
/*                STRING(INT(ENTRY(iCount2 + 1,cFormatStr))) + ">" ELSE "" */
/*                          STRING(INT(ENTRY(iCount2 + 1,cFormatStr)) - INT(ENTRY(iCount2,cFormatStr)) - 3) + ">" ELSE ""  */
               STRING(INT(ENTRY(iCount2 + 1,cFormatStr)) - INT(ENTRY(iCount2,cFormatStr)) - 2) + ">" ELSE ""
                  cRowStr = cRowStr + "<C" + ENTRY(iCount2,cFormatStr) + ">" + cRight + chResultat:Cell(0,iCount,iCount2,iCount,iCount2).
       END.
       IF LINE-COUNTER >= 40 AND iCount < chResultat:Rows - 2 THEN DO:
           PAGE.
           VIEW FRAME PageHeader.
           PUT UNFORMATTED cLabelStr SKIP.
       END.
       IF iCount < chResultat:Rows - 1 THEN
          PUT UNFORMATTED  cRowStr SKIP.
       ELSE IF chResultat:IsSubtotal(chResultat:Rows - 1) = TRUE THEN
           PUT UNFORMATTED "<B><R+1>" cRowStr "</B>" SKIP.
       ELSE
           PUT UNFORMATTED  cRowStr SKIP.
   END.
   IF cUrvalsInfo <> "" THEN DO:
       PAGE.
       PUT UNFORMATTED "<B><P14><R10><C10>" REPLACE(cUrvalsInfo,CHR(10),"<R+2><C10>") "</B>" SKIP.
   END.
   OUTPUT CLOSE.
/*    RUN w-VisXprint.w ("TST", pcRappFil). */
   RUN VisXprint.p (pcRappFil).
   
/* 
"<C43>" +  entry(1,pcLabel,chr(1)) + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.KontantBeholdning,"->,>>>,>>9.99") SKIP        

  PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
/*   PUT CONTROL '<SILENT=TRUE>'. */
/*   PUT CONTROL '<PRINT=NO>'.    */
  PUT CONTROL '<PREVIEW=ZoomToWidth>'.
/*   PUT CONTROL '<PREVIEW=PDF>'. */
  /*put control "<PrinterSetup>". */

 
 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE XPrintRapp2 fFrameWin 
PROCEDURE XPrintRapp2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cUrvalsInfo    AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER cColWidth%     AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER cPageCols      AS CHARACTER NO-UNDO. /* vid för bred grid för XPrintrapport */
   DEFINE INPUT  PARAMETER cRightCols     AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  pcRappFil      AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  iCount         AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iCount0        AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iCount2        AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  cLabelStr      AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  cLabelStrTMP   AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  cRowStr        AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  cFormatStr     AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  cFormatStrTMP  AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  iColTmp        AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iCount3        AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iFirstSumCol   AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iFirstPageCol  AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iCountPageCols AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  cRight         AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  iSidNr         AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iRadNr         AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  lMindrePG1     AS LOGICAL   NO-UNDO.

   IF cExtraInfo <> "" THEN
       ASSIGN lMindrePG1 = TRUE.
   /* skapa en styrsträng for rapportrader */
   IF NUM-ENTRIES(cRightCols) <> chResultat:Cols - 1 THEN DO:
/*        MESSAGE NUM-ENTRIES(cRightCols) chResultat:Cols - 1 SKIP "Feil i 'RIGHT'-string" */
/*            VIEW-AS ALERT-BOX INFO BUTTONS OK.                                           */
       ASSIGN cRightCols = FILL(",",chResultat:Cols - 2).
   END.
   EMPTY TEMP-TABLE TT_RapportRader.
   ASSIGN iFirstSumCol = IF cColLabelSave <> "" THEN INT(ENTRY(1,cColLabelSave)) + 1 ELSE ?.
/*           cTitle = "Statistikk". */
   DO iCount = 1 TO NUM-ENTRIES(cPageCols,";"):
       ASSIGN cFormatStrTMP = "6"
              iColTmp       = 6
              iCount3       = 0.
       DO iCount2 = INT(ENTRY(1,ENTRY(iCount,cPageCols,";"))) TO INT(ENTRY(2,ENTRY(iCount,cPageCols,";"))):
           ASSIGN iCount3    = ROUND(DECI(ENTRY(iCount2,cColWidth%,CHR(1))) * 105 / 100,0)
                  iColTmp    = iColTmp + iCount3
                  cFormatStrTMP = cFormatStrTMP + "," + STRING(iColTmp).
       END.
       ASSIGN cFormatStr = cFormatStr + (IF cFormatStr = "" THEN "" ELSE ";") + cFormatStrTMP.
   END.
   /* Finn lablar */
   DO iCount = 1 TO NUM-ENTRIES(cPageCols,";"):
       ASSIGN cLabelStrTMP = ""
              iFirstPageCol = INT(ENTRY(1,ENTRY(iCount,cPageCols,";"))) - 1.
/*        DO iCount2 = INT(ENTRY(1,ENTRY(iCount,cPageCols,";"))) TO INT(ENTRY(2,ENTRY(iCount,cPageCols,";"))): */
       DO iCount2 = 1 TO NUM-ENTRIES(ENTRY(iCount,cFormatStr,";")) - 1:
           ASSIGN cLabelStrTMP = cLabelStrTMP + "<C" + ENTRY(iCount2,ENTRY(iCount,cFormatStr,";")) + ">" + chResultat:Cell(0,0,iCount2 + iFirstPageCol,0,iCount2 + iFirstPageCol).
       END.
       ASSIGN cLabelStr = cLabelStr + (IF cLabelStr = "" THEN "" ELSE ";") + "<U>" + cLabelStrTMP + "</U>".
   END.
/*    FIELD iPageNum AS INTEGER   */
/*    FIELD iColPage AS INTEGER   */
/*    FIELD iRadNum  AS INTEGER   */
/*    FIELD cRadData AS CHARACTER */
   ASSIGN iSidNr = 1
          iRadNr = 1.
   DO iCount = 1 TO chResultat:Rows - 1:
       IF chResultat:RowHidden(iCount) THEN
           NEXT.
       DO iCountPageCols = 1 TO NUM-ENTRIES(cPageCols,";"):
           ASSIGN iFirstPageCol = INT(ENTRY(1,ENTRY(iCountPageCols,cPageCols,";"))) - 1
                  iCount3 = 0.
           DO iCount2 = INT(ENTRY(1,ENTRY(iCountPageCols,cPageCols,";"))) TO INT(ENTRY(2,ENTRY(iCountPageCols,cPageCols,";"))):
               ASSIGN iCount3 = iCount3 + 1
                   cRight = IF ENTRY(iCount2,cRightCols) = "1" THEN "<RIGHT=C+" + 
                STRING(INT(ENTRY(iCount3 + 1,ENTRY(iCountPageCols,cFormatStr,";"))) - INT(ENTRY(iCount3,ENTRY(iCountPageCols,cFormatStr,";"))) - 2) + ">" ELSE ""
                      cRowStr = cRowStr + "<C" + ENTRY(iCount2 - iFirstPageCol,ENTRY(iCountPageCols,cFormatStr,";")) + ">" + cRight + chResultat:Cell(0,iCount,iCount2,iCount,iCount2).
           END.
           CREATE TT_RapportRader.
           ASSIGN TT_RapportRader.iPageNum = iSidNr
                  TT_RapportRader.iColPage = iCountPageCols
                  TT_RapportRader.iRadNum  = iRadNr
                  TT_RapportRader.cRadData = IF iCount = chResultat:Rows - 1 AND chResultat:IsSubtotal(chResultat:Rows - 1) THEN "<B><R+1>" + cRowStr + "</B>" ELSE cRowStr
                  cRowStr = "".
           IF iCountPageCols = NUM-ENTRIES(cPageCols,";") THEN DO:
               IF ((lMindrePG1 = TRUE AND iRadNr = 34) OR iRadNr = 36) AND iCount < chResultat:Rows - 1 THEN
/*                IF iRadNr = 36 AND iCount < chResultat:Rows - 1 THEN */
                   ASSIGN iRadNr = 1
                          iSidNr = iSidNr + 1
                          lMindrePG1 = FALSE.
               ELSE
                   ASSIGN iRadNr = iRadNr + 1.
           END.
       END.
   END.
   ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "rapp.xpr".
   OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(80).
   PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
   PUT CONTROL '<PREVIEW=ZoomToWidth><OLANDSCAPE>'.
   FOR EACH TT_RapportRader BREAK BY TT_RapportRader.iPageNum BY
                                     TT_RapportRader.iColPage BY
                                     TT_RapportRader.iRadNum: 
       IF FIRST-OF(TT_RapportRader.iColPage) THEN DO:
           IF NOT FIRST(TT_RapportRader.iPageNum) THEN DO:
               RUN PFooter.
               PAGE.
           END.
           VIEW FRAME PageHeader.
           PUT UNFORMATTED  "<R4><B><C1><CENTER=C110><P24>" cTitle "</B><P10><R+2>".
           IF FIRST(TT_RapportRader.iPageNum) AND cExtraInfo <> "" THEN
               PUT UNFORMATTED "<C6><P12>" cExtraInfo  "<P10><R+1>" SKIP.
           PUT UNFORMATTED ENTRY(TT_RapportRader.iColPage,cLabelStr,";") SKIP.
       END.
       PUT UNFORMATTED TT_RapportRader.cRadData SKIP.
/*        IF LAST-OF(TT_RapportRader.iColPage) AND NOT LAST(TT_RapportRader.iRadNum) THEN */
/*            PAGE.                                                                       */
/*                                                                                        */
   END.

   RUN PFooter.
   IF cUrvalsInfo <> "" THEN DO:
       PAGE.
       PUT UNFORMATTED  "<R4><B><C1><CENTER=C110><P24>" cTitle "</B><P10><R+2>".
       PUT UNFORMATTED "<B><P14><R10><C10>" REPLACE(cUrvalsInfo,CHR(10),"<R+2><C10>") "</B><P10>" SKIP.
       RUN PFooter.
   END.
   OUTPUT CLOSE.
/*                                                          */
/*    OUTPUT TO value(SESSION:TEMP-DIRECTORY + "dump.txt"). */
/*        FOR EACH TT_RapportRader.                         */
/*            EXPORT TT_RapportRader.                       */
/*        END.                                              */
/*    OUTPUT CLOSE.                                         */
/*    RUN w-VisXprint.w ("TST", pcRappFil). */
   RUN VisXprint.p (pcRappFil).
   
/* 
"<C43>" +  entry(1,pcLabel,chr(1)) + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.KontantBeholdning,"->,>>>,>>9.99") SKIP        

  PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
/*   PUT CONTROL '<SILENT=TRUE>'. */
/*   PUT CONTROL '<PRINT=NO>'.    */
  PUT CONTROL '<PREVIEW=ZoomToWidth>'.
/*   PUT CONTROL '<PREVIEW=PDF>'. */
  /*put control "<PrinterSetup>". */

 
 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE XPrintRapp2org fFrameWin 
PROCEDURE XPrintRapp2org :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cUrvalsInfo   AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER cColWidth%    AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER cPageCols     AS CHARACTER NO-UNDO. /* vid för bred grid för XPrintrapport */
   DEFINE        VARIABLE  pcRappFil     AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  iCount        AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iCount0       AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iCount2       AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  cLabelStr     AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  cLabelStrTMP  AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  cRowStr       AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  cFormatStr    AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  cFormatStrTMP AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  iColTmp       AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iCount3       AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iFirstSumCol  AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iFirstPageCol AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  cRight        AS CHARACTER NO-UNDO.

   /* skapa en styrsträng for rapportrader */
/*                                      
   DEFINE TEMP-TABLE TT_RapportRader
    FIELD RadNum  AS INTEGER
    FIELD RadData AS CHARACTER
                                      */
   EMPTY TEMP-TABLE TT_RapportRader.
   ASSIGN iFirstSumCol = IF cColLabelSave <> "" THEN INT(ENTRY(1,cColLabelSave)) + 1 ELSE ?.
   DO iCount = 1 TO NUM-ENTRIES(cPageCols,";"):
       ASSIGN cFormatStrTMP = "6"
              iColTmp       = 6
              iCount3       = 0.
       DO iCount2 = INT(ENTRY(1,ENTRY(iCount,cPageCols,";"))) TO INT(ENTRY(2,ENTRY(iCount,cPageCols,";"))):
           ASSIGN iCount3    = ROUND(DECI(ENTRY(iCount2,cColWidth%,CHR(1))) * 100 / 100,0)
                  iColTmp    = iColTmp + iCount3
                  cFormatStrTMP = cFormatStrTMP + "," + STRING(iColTmp).
       END.
       ASSIGN cFormatStr = cFormatStr + (IF cFormatStr = "" THEN "" ELSE ";") + cFormatStrTMP.
   END.
/*    DO iCount = 1 TO NUM-ENTRIES(cColWidth%,CHR(1)): /* -1 */                          */
/*        ASSIGN iCount3    = ROUND(DECI(ENTRY(iCount,cColWidth%,CHR(1))) * 100 / 100,0) */
/*               iColTmp    = iColTmp + iCount3                                          */
/*               cFormatStr = cFormatStr + "," + STRING(iColTmp).                        */
/*    END.                                                                               */
   /* Finn lablar */
   DO iCount = 1 TO NUM-ENTRIES(cPageCols,";"):
       ASSIGN cLabelStrTMP = ""
              iFirstPageCol = INT(ENTRY(1,ENTRY(iCount,cPageCols,";"))) - 1.
/*        DO iCount2 = INT(ENTRY(1,ENTRY(iCount,cPageCols,";"))) TO INT(ENTRY(2,ENTRY(iCount,cPageCols,";"))): */
       DO iCount2 = 1 TO NUM-ENTRIES(ENTRY(iCount,cFormatStr,";")) - 1:
           ASSIGN cLabelStrTMP = cLabelStrTMP + "<C" + ENTRY(iCount2,ENTRY(iCount,cFormatStr,";")) + ">" + chResultat:Cell(0,0,iCount2 + iFirstPageCol,0,iCount2 + iFirstPageCol).
       END.
       ASSIGN cLabelStr = cLabelStr + (IF cLabelStr = "" THEN "" ELSE ";") + "<U>" + cLabelStrTMP + "</U>".
   END.
/*    DO iCount = 1 TO chResultat:Cols - 1:                                                                                  */
/*        IF iCount <= iNumColsPg1 THEN  /* Betyder att vi har för många kolonner för 1 sida */                              */
/*            ASSIGN cLabelStr = cLabelStr + "<C" + ENTRY(iCount,cFormatStr) + ">" + chResultat:Cell(0,0,iCount,0,iCount).   */
/*        ELSE                                                                                                               */
/*            ASSIGN cLabelStr2 = cLabelStr2 + "<C" + ENTRY(iCount,cFormatStr) + ">" + chResultat:Cell(0,0,iCount,0,iCount). */
/*    END.                                                                                                                   */
   ASSIGN 
/*        cLabelStr = "<U>" + cLabelStr + "</U>" */
          pcRappFil = SESSION:TEMP-DIRECTORY + "rapp.xpr".
   OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(80).
   PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
   PUT CONTROL '<PREVIEW=ZoomToWidth><OLANDSCAPE>'.

   DO iCount0 = 1 TO NUM-ENTRIES(cPageCols,";"):
       ASSIGN iFirstPageCol = INT(ENTRY(1,ENTRY(iCount0,cPageCols,";"))) - 1.
       VIEW FRAME PageHeader.
       PUT UNFORMATTED ENTRY(iCount0,cLabelStr,";") SKIP.

   /* Print rapporlabels */
     DO iCount = 1 TO chResultat:Rows - 1:
       IF chResultat:RowHidden(iCount) THEN
           NEXT.
           ASSIGN cRowStr = "".
       DO iCount2 = 1 TO NUM-ENTRIES(ENTRY(iCount0,cFormatStr,";")) - 1:
           ASSIGN cRight = IF iFirstSumCol <> ? AND iCount2 >= iFirstSumCol THEN "<RIGHT=C+" + 
/*                STRING(INT(ENTRY(iCount2 + 1,cFormatStr))) + ">" ELSE "" */
/*                          STRING(INT(ENTRY(iCount2 + 1,cFormatStr)) - INT(ENTRY(iCount2,cFormatStr)) - 3) + ">" ELSE ""  */
               STRING(INT(ENTRY(iCount2 + 1,cFormatStr)) - INT(ENTRY(iCount2,cFormatStr)) - 2) + ">" ELSE ""
                  cRowStr = cRowStr + "<C" + ENTRY(iCount2,ENTRY(iCount0,cFormatStr,";")) + ">" + cRight + chResultat:Cell(0,iCount,iCount2 + iFirstPageCol,iCount,iCount2 + iFirstPageCol).
       END.
       IF LINE-COUNTER >= 40 AND iCount < chResultat:Rows - 2 THEN DO:
           PAGE.
           VIEW FRAME PageHeader.
           IF NUM-ENTRIES(cPageCols,";") > 1 THEN
               PUT UNFORMATTED ENTRY(IF iCount0 = NUM-ENTRIES(cPageCols,";") THEN 1 ELSE iCount0 + 1,cLabelStr,";").
           ELSE
               PUT UNFORMATTED cLabelStr SKIP.
       END.
       IF iCount < chResultat:Rows - 1 THEN
          PUT UNFORMATTED  cRowStr SKIP.
       ELSE IF chResultat:IsSubtotal(chResultat:Rows - 1) = TRUE THEN
           PUT UNFORMATTED "<B><R+1>" cRowStr "</B>" SKIP.
       ELSE
           PUT UNFORMATTED  cRowStr SKIP.
     END.
   END.
   IF cUrvalsInfo <> "" THEN DO:
       PAGE.
       PUT UNFORMATTED "<B><P14><R10><C10>" REPLACE(cUrvalsInfo,CHR(10),"<R+2><C10>") "</B>" SKIP.
   END.
   OUTPUT CLOSE.
/*    RUN w-VisXprint.w ("TST", pcRappFil). */
   RUN VisXprint.p (pcRappFil).
   
/* 
"<C43>" +  entry(1,pcLabel,chr(1)) + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.KontantBeholdning,"->,>>>,>>9.99") SKIP        

  PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
/*   PUT CONTROL '<SILENT=TRUE>'. */
/*   PUT CONTROL '<PRINT=NO>'.    */
  PUT CONTROL '<PREVIEW=ZoomToWidth>'.
/*   PUT CONTROL '<PREVIEW=PDF>'. */
  /*put control "<PrinterSetup>". */

 
 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE XPrintRappOms fFrameWin 
PROCEDURE XPrintRappOms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cUrvalsInfo    AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER cColWidth%     AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER cPageCols      AS CHARACTER NO-UNDO. /* vid för bred grid för XPrintrapport */
   DEFINE INPUT  PARAMETER cRightCols     AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  pcRappFil      AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  iCount         AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iCount0        AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iCount2        AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  cLabelStr      AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  cLabelStrTMP   AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  cRowStr        AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  cFormatStr     AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  cFormatStrTMP  AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  iColTmp        AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iCount3        AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iFirstSumCol   AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iFirstPageCol  AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iCountPageCols AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  cRight         AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  iSidNr         AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iRadNr         AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  lMindrePG1     AS LOGICAL   NO-UNDO.

   /* skapa en styrsträng for rapportrader */
   IF cExtraInfo <> "" THEN
       ASSIGN lMindrePG1 = TRUE.
   IF NUM-ENTRIES(cRightCols) <> NUM-ENTRIES(cPageCols) THEN DO:
/*        MESSAGE "Feil i 'RIGHT'-string"        */
/*            VIEW-AS ALERT-BOX INFO BUTTONS OK. */
       ASSIGN cRightCols = FILL(",",chResultat:Cols - 2).
   END.
   EMPTY TEMP-TABLE TT_RapportRader.
   ASSIGN iFirstSumCol = IF cColLabelSave <> "" THEN INT(ENTRY(1,cColLabelSave)) + 1 ELSE ?
          cTitle = "Omsettningsrapport".
   DO:
       ASSIGN cFormatStrTMP = "6"
              iColTmp       = 6
              iCount3       = 0.
       DO iCount2 = 1 TO NUM-ENTRIES(cPageCols):
           ASSIGN iCount3    = ROUND(DECI(ENTRY(iCount2,cColWidth%,CHR(1))) * 100 / 100,0)
                  iColTmp    = iColTmp + iCount3
                  cFormatStrTMP = cFormatStrTMP + "," + STRING(iColTmp).
       END.
       ASSIGN cFormatStr = cFormatStrTMP.
   END.
   /* Finn lablar */
   DO:
       ASSIGN cLabelStrTMP = "".
       DO iCount2 = 1 TO NUM-ENTRIES(cFormatStr) - 1:
           ASSIGN cRight = IF ENTRY(iCount2,cRightCols) = "1" THEN "<RIGHT=C+" + 
               STRING(INT(ENTRY(iCount2 + 1,cFormatStr)) - INT(ENTRY(iCount2,cFormatStr)) - 2) + ">" ELSE ""
                  cLabelStrTMP = cLabelStrTMP + "<C" + ENTRY(iCount2,cFormatStr) + ">" + cRight + chResultat:Cell(0,0,INT(ENTRY(iCount2,cPageCols)),0,INT(ENTRY(iCount2,cPageCols))).
       END.
       ASSIGN cLabelStr = cLabelStr + (IF cLabelStr = "" THEN "" ELSE ";") + "<U>" + cLabelStrTMP + "</U>".
   END.

   ASSIGN iSidNr = 1
          iRadNr = 1.
   DO iCount = 1 TO chResultat:Rows - 1:
       IF chResultat:RowHidden(iCount) THEN
           NEXT.
       DO:
           ASSIGN iFirstPageCol = INT(ENTRY(1,cPageCols)) - 1
                  iCount3 = 0.
           DO iCount2 = 1 TO NUM-ENTRIES(cPageCols):
               ASSIGN iCount3 = iCount3 + 1
                   cRight = IF ENTRY(iCount2,cRightCols) = "1" THEN "<RIGHT=C+" + 
                STRING(INT(ENTRY(iCount3 + 1,cFormatStr)) - INT(ENTRY(iCount3,cFormatStr)) - 2) + ">" ELSE ""
                      cRowStr = cRowStr + "<C" + ENTRY(iCount2,cFormatStr) + ">" + cRight + chResultat:Cell(0,iCount,INT(ENTRY(iCount2,cPageCols)),iCount,INT(ENTRY(iCount2,cPageCols))).
           END.
           CREATE TT_RapportRader.
           ASSIGN TT_RapportRader.iPageNum = iSidNr
                  TT_RapportRader.iColPage = iCountPageCols
                  TT_RapportRader.iRadNum  = iRadNr
                  TT_RapportRader.cRadData = IF iCount = chResultat:Rows - 1 AND chResultat:IsSubtotal(chResultat:Rows - 1) THEN "<B><R+1>" + cRowStr + "</B>" ELSE cRowStr
                  cRowStr = "".
               IF ((lMindrePG1 = TRUE AND iRadNr = 34) OR iRadNr = 36) AND iCount < chResultat:Rows - 1 THEN
                   ASSIGN iRadNr = 1
                          iSidNr = iSidNr + 1
                          lMindrePG1 = FALSE.
               ELSE
                   ASSIGN iRadNr = iRadNr + 1.
       END.
   END.
   ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "rapp.xpr".

   
   OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(80).
   PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
/*    PUT CONTROL '<PREVIEW=ZoomToWidth>'. */
   PUT CONTROL '<PREVIEW=ZoomToWidth><OLANDSCAPE>'.
   FOR EACH TT_RapportRader BREAK BY TT_RapportRader.iPageNum BY
                                     TT_RapportRader.iColPage BY
                                     TT_RapportRader.iRadNum: 
       IF FIRST-OF(TT_RapportRader.iColPage) THEN DO:
           IF NOT FIRST(TT_RapportRader.iPageNum) THEN DO:
               RUN PFooter.
               PAGE.
           END.
           VIEW FRAME PageHeader.
           PUT UNFORMATTED  "<R4><B><C1><CENTER=C110><P24>" cTitle "</B><P10><R+2>".
           IF FIRST(TT_RapportRader.iPageNum) AND cExtraInfo <> "" THEN
               PUT UNFORMATTED "<C6><P12>" cExtraInfo  "<P10><R+1>" SKIP. 
           PUT UNFORMATTED cLabelStr SKIP.
       END.
       PUT UNFORMATTED TT_RapportRader.cRadData SKIP.
   END.
   RUN PFooter.
   IF cUrvalsInfo <> "" THEN DO:
       PAGE.
       PUT UNFORMATTED "<B><P14><R10><C10>" REPLACE(cUrvalsInfo,CHR(10),"<R+2><C10>") "</B>" SKIP.
   END.
   OUTPUT CLOSE.
/*    RUN w-VisXprint.w ("TST", pcRappFil). */
   RUN VisXprint.p (pcRappFil).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE XPrintRappOmsOrg fFrameWin 
PROCEDURE XPrintRappOmsOrg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cUrvalsInfo    AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER cColWidth%     AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER cPageCols      AS CHARACTER NO-UNDO. /* vid för bred grid för XPrintrapport */
   DEFINE INPUT  PARAMETER cRightCols     AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  pcRappFil      AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  iCount         AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iCount0        AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iCount2        AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  cLabelStr      AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  cLabelStrTMP   AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  cRowStr        AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  cFormatStr     AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  cFormatStrTMP  AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  iColTmp        AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iCount3        AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iFirstSumCol   AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iFirstPageCol  AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iCountPageCols AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  cRight         AS CHARACTER NO-UNDO.
   DEFINE        VARIABLE  iSidNr         AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  iRadNr         AS INTEGER   NO-UNDO.
   DEFINE        VARIABLE  lMindrePG1     AS LOGICAL   NO-UNDO.

   /* skapa en styrsträng for rapportrader */
   IF cExtraInfo <> "" THEN
       ASSIGN lMindrePG1 = TRUE.
   IF NUM-ENTRIES(cRightCols) <> NUM-ENTRIES(cPageCols) THEN DO:
/*        MESSAGE "Feil i 'RIGHT'-string"        */
/*            VIEW-AS ALERT-BOX INFO BUTTONS OK. */
       ASSIGN cRightCols = FILL(",",chResultat:Cols - 2).
   END.
   EMPTY TEMP-TABLE TT_RapportRader.
   ASSIGN iFirstSumCol = IF cColLabelSave <> "" THEN INT(ENTRY(1,cColLabelSave)) + 1 ELSE ?
          cTitle = "Omsettningsrapport".
   DO:
       ASSIGN cFormatStrTMP = "6"
              iColTmp       = 6
              iCount3       = 0.
       DO iCount2 = 1 TO NUM-ENTRIES(cPageCols):
           ASSIGN iCount3    = ROUND(DECI(ENTRY(iCount2,cColWidth%,CHR(1))) * 100 / 100,0)
                  iColTmp    = iColTmp + iCount3
                  cFormatStrTMP = cFormatStrTMP + "," + STRING(iColTmp).
       END.
       ASSIGN cFormatStr = cFormatStrTMP.
   END.
   /* Finn lablar */
   DO:
       ASSIGN cLabelStrTMP = "".
       DO iCount2 = 1 TO NUM-ENTRIES(cFormatStr) - 1:
           ASSIGN cRight = IF ENTRY(iCount2,cRightCols) = "1" THEN "<RIGHT=C+" + 
               STRING(INT(ENTRY(iCount2 + 1,cFormatStr)) - INT(ENTRY(iCount2,cFormatStr)) - 2) + ">" ELSE ""
                  cLabelStrTMP = cLabelStrTMP + "<C" + ENTRY(iCount2,cFormatStr) + ">" + cRight + chResultat:Cell(0,0,INT(ENTRY(iCount2,cPageCols)),0,INT(ENTRY(iCount2,cPageCols))).
       END.
       ASSIGN cLabelStr = cLabelStr + (IF cLabelStr = "" THEN "" ELSE ";") + "<U>" + cLabelStrTMP + "</U>".
   END.
   ASSIGN iSidNr = 1
          iRadNr = 1.
   DO iCount = 1 TO chResultat:Rows - 1:
       IF chResultat:RowHidden(iCount) THEN
           NEXT.
       DO:
           ASSIGN iFirstPageCol = INT(ENTRY(1,cPageCols)) - 1
                  iCount3 = 0.
           DO iCount2 = 1 TO NUM-ENTRIES(cPageCols):
               ASSIGN iCount3 = iCount3 + 1
                   cRight = IF ENTRY(iCount2,cRightCols) = "1" THEN "<RIGHT=C+" + 
                STRING(INT(ENTRY(iCount3 + 1,cFormatStr)) - INT(ENTRY(iCount3,cFormatStr)) - 2) + ">" ELSE ""
                      cRowStr = cRowStr + "<C" + ENTRY(iCount2,cFormatStr) + ">" + cRight + chResultat:Cell(0,iCount,INT(ENTRY(iCount2,cPageCols)),iCount,INT(ENTRY(iCount2,cPageCols))).
           END.
           CREATE TT_RapportRader.
           ASSIGN TT_RapportRader.iPageNum = iSidNr
                  TT_RapportRader.iColPage = iCountPageCols
                  TT_RapportRader.iRadNum  = iRadNr
                  TT_RapportRader.cRadData = IF iCount = chResultat:Rows - 1 AND chResultat:IsSubtotal(chResultat:Rows - 1) THEN "<B><R+1>" + cRowStr + "</B>" ELSE cRowStr
                  cRowStr = "".
               IF ((lMindrePG1 = TRUE AND iRadNr = 34) OR iRadNr = 36) AND iCount < chResultat:Rows - 1 THEN
                   ASSIGN iRadNr = 1
                          iSidNr = iSidNr + 1
                          lMindrePG1 = FALSE.
               ELSE
                   ASSIGN iRadNr = iRadNr + 1.
       END.
   END.
   ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "rapp.xpr".

   
   OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(80).
   PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
/*    PUT CONTROL '<PREVIEW=ZoomToWidth>'. */
   PUT CONTROL '<PREVIEW=ZoomToWidth><OLANDSCAPE>'.
   FOR EACH TT_RapportRader BREAK BY TT_RapportRader.iPageNum BY
                                     TT_RapportRader.iColPage BY
                                     TT_RapportRader.iRadNum: 
       IF FIRST-OF(TT_RapportRader.iColPage) THEN DO:
           IF NOT FIRST(TT_RapportRader.iPageNum) THEN DO:
               RUN PFooter.
               PAGE.
           END.
           VIEW FRAME PageHeader.
           PUT UNFORMATTED  "<R4><B><C1><CENTER=C110><P24>" cTitle "</B><P10><R+2>".
           IF FIRST(TT_RapportRader.iPageNum) AND cExtraInfo <> "" THEN
               PUT UNFORMATTED "<C6><P12>" cExtraInfo  "<P10><R+1>" SKIP. 
           PUT UNFORMATTED cLabelStr SKIP.
       END.
       PUT UNFORMATTED TT_RapportRader.cRadData SKIP.
   END.
   RUN PFooter.
   IF cUrvalsInfo <> "" THEN DO:
       PAGE.
       PUT UNFORMATTED "<B><P14><R10><C10>" REPLACE(cUrvalsInfo,CHR(10),"<R+2><C10>") "</B>" SKIP.
   END.
   OUTPUT CLOSE.
/*    RUN w-VisXprint.w ("TST", pcRappFil). */
   RUN VisXprint.p (pcRappFil).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION bredd fFrameWin 
FUNCTION bredd RETURNS DECIMAL
    ( INPUT cText AS CHARACTER ):
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/

    RETURN pdf_text_widthdec ("Spdf",cText).   /* Function return value. */

END FUNCTION. /* bredd */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTxtFrame fFrameWin 
FUNCTION getTxtFrame RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN FRAME FRAME-Txt:HANDLE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SummerCol fFrameWin 
FUNCTION SummerCol RETURNS CHARACTER
  ( INPUT iCol AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dSum   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dTmp   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE lError AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cString AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iNumDeci AS INTEGER INIT ?  NO-UNDO.
  DO iCount = 1 TO chResultat:rows - IF TRIM(cSumWhatSave) = "" THEN 1 ELSE 2:
      IF chResultat:RowHidden(iCount) = TRUE THEN
          NEXT.
      dTmp = DECI(chResultat:TextMatrix(iCount,iCol)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
          ASSIGN lError = TRUE.
      ELSE DO:
          ASSIGN dSum = dSum + dTmp.
          IF iNumDeci = ? THEN
              ASSIGN iNumDeci = IF NUM-ENTRIES(chResultat:TextMatrix(iCount,iCol)) = 2 THEN
                  LENGTH(ENTRY(2,chResultat:TextMatrix(iCount,iCol))) ELSE 0.
      END.
  END.
  ASSIGN cString = STRING(dSum,"->>>,>>>,>>9" + IF iNumDeci = 0 THEN "" ELSE "." + FILL("9",iNumDeci)) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
      ASSIGN cString = ""
             lError = TRUE.
  OUTPUT TO "CLIPBOARD".
  PUT UNFORMATTED dSum SKIP.
  OUTPUT CLOSE.
  RETURN cString + (IF lError THEN " " + "(+ ERROR)" ELSE "").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

