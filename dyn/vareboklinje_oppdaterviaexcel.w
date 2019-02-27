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
{incl/Excel_1.3.i}

DEF VAR cTest             AS CHAR   NO-UNDO.
DEF VAR cFileName         AS CHAR   NO-UNDO.
DEF VAR cFullFileName     AS CHAR   NO-UNDO.
DEF VAR cFilePath         AS CHAR   NO-UNDO.
DEF VAR bOk               AS LOG    NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.

DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hBrowseLinje      AS HANDLE NO-UNDO.
DEF VAR hFieldMapLinje    AS HANDLE NO-UNDO.
DEF VAR httRapport        AS HANDLE NO-UNDO.
DEF VAR thVareboklinjeRow AS HANDLE NO-UNDO.
DEF VAR thVareboklinjeCol AS HANDLE NO-UNDO.
DEF VAR thError           AS HANDLE NO-UNDO.
DEF VAR thVareboklinje    AS HANDLE NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.


DEF TEMP-TABLE ttExcelSetup NO-UNDO
      FIELD iSequence    AS INT
      FIELD cFieldName   AS CHAR 
      FIELD cUpdateField AS CHAR 
      FIELD cLabel       AS CHAR
      FIELD cDataType    AS CHAR
      FIELD cColumn      AS CHAR
      FIELD cExcelFormat AS CHAR
      FIELD iWidth       AS INT
      FIELD bReadOnly    AS LOG
      FIELD cFormula     AS CHAR  /*Vurderte å gjøre fomula her, men valgte på utlegget i stedet, inntil videre*/
  INDEX iSequence IS PRIMARY UNIQUE iSequence
  INDEX cFieldName cFieldName
  INDEX cColumn cColumn
  INDEX bReadOnly bReadOnly
  INDEX cUpdateField cUpdateField
  .
DEF VAR iCount     AS INT NO-UNDO.
DEF VAR iNumFields AS INT NO-UNDO.

DEF VAR chExcelApplication      AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook              AS COM-HANDLE NO-UNDO.
DEF VAR chWorksheet             AS COM-HANDLE NO-UNDO.
DEF VAR chInterior              AS COM-HANDLE NO-UNDO.
DEF VAR chProtect               AS COM-HANDLE NO-UNDO.

DEF VAR cColumn                 AS CHAR NO-UNDO.
DEF VAR cUnprotect              AS CHAR NO-UNDO.
DEF VAR cFieldList              AS CHAR NO-UNDO.
DEF VAR cLabelList              AS CHAR NO-UNDO.

DEF TEMP-TABLE ttVareboklinjeRow NO-UNDO
  FIELD rRowId     AS ROWID
  FIELD vareboknr  AS DEC
  FIELD artikkelnr AS DEC
  FIELD iRowNum    AS INT
  FIELD bHasError  AS LOG
  INDEX rRowid IS PRIMARY UNIQUE rRowid
  INDEX iRowNum iRowNum
  INDEX bHasError bHasError
  .
DEF TEMP-TABLE ttVareboklinjeCol NO-UNDO
  FIELD rRowid        AS ROWID
  FIELD iFieldNum     AS INT
  FIELD cFieldName    AS CHAR
  FIELD cUpdateField  AS CHAR
  FIELD cDataType     AS CHAR
  FIELD cValue        AS CHAR
  FIELD bHasError     AS LOG
  INDEX rRowid IS PRIMARY UNIQUE rRowid iFieldNum
  INDEX bHasError bHasError
  .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnSend btnOppdater btnHent 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cleanExcel C-Win 
FUNCTION cleanExcel RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD createTTexcelSetup C-Win 
FUNCTION createTTexcelSetup RETURNS LOGICAL
  (INPUT icFieldName   AS CHAR,
   INPUT icExcelFormat AS CHAR,
   INPUT iiWidth       AS INT,
   INPUT ibReadOnly    AS LOG,
   INPUT icUpdateField AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD doExcelSetup C-Win 
FUNCTION doExcelSetup RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAlphaSeqNo C-Win 
FUNCTION getAlphaSeqNo RETURNS CHARACTER
  ( INPUT iiSeqNo AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDictDataType C-Win 
FUNCTION getDictDataType RETURNS CHARACTER
  (INPUT icFieldName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getExcelForDBupdate C-Win 
FUNCTION getExcelForDBupdate RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getReportFile C-Win 
FUNCTION getReportFile RETURNS CHARACTER
  ( INPUT icClientServerFile AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OpenExcelForUpdate C-Win 
FUNCTION OpenExcelForUpdate RETURNS LOGICAL
  ( INPUT icFileAndCount AS CHAR,
    INPUT ibTestPrint    AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnHent 
     LABEL "Hent fil" 
     SIZE 24 BY 1.62 DROP-TARGET.

DEFINE BUTTON btnOppdater 
     LABEL "Kontroller og oppdater" 
     SIZE 24 BY 1.62.

DEFINE BUTTON btnSend 
     LABEL "Åpne i Excel" 
     SIZE 24 BY 1.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSend AT ROW 1.29 COL 3
     btnOppdater AT ROW 2.91 COL 3
     btnHent AT ROW 4.57 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 28.2 BY 6.62 DROP-TARGET.


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
         TITLE              = "Oppdater via Excel"
         HEIGHT             = 6.62
         WIDTH              = 28
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 114.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 114.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
ON END-ERROR OF C-Win /* Oppdater via Excel */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Oppdater via Excel */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DEFAULT-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME C-Win
ON DROP-FILE-NOTIFY OF FRAME DEFAULT-FRAME
DO:
  IF SELF:NUM-DROPPED-FILES = 1 THEN 
  DO:
    ASSIGN cFileName = SELF:GET-DROPPED-FILE(1).
    IF NOT CAN-DO("xls,xlsx",ENTRY(NUM-ENTRIES(cFileName,"."),cFileName,".")) THEN
      MESSAGE "Tillatte filtyper: '.xls,.xlsx'"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE 
    DO:
      ASSIGN 
        cFilePath = SUBSTR(cFileName,1,R-INDEX(cFileName,'\'))
        cFileName = SUBSTR(cFileName,R-INDEX(cFileName,'\') + 1)
      .

      DYNAMIC-FUNCTION('OpenExcelForUpdate',cFilePath + '|' + cFileName + '|0',FALSE).
    END.
  END.
  ELSE 
  DO:
    MESSAGE "Kun en fil er tillatt!"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
  END.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHent C-Win
ON CHOOSE OF btnHent IN FRAME DEFAULT-FRAME /* Hent fil */
DO:
  
  Main: 
  REPEAT:    
    SYSTEM-DIALOG GET-FILE cFileName        
    TITLE      "Velg EXCEL fil ..."        
    FILTERS    "Kildefil (*.xls)"   "*.xls"
    MUST-EXIST        
    USE-FILENAME        
    UPDATE bOk.          
    
    IF bOk = TRUE THEN    
    DO:

      ASSIGN 
        cFilePath = SUBSTR(cFileName,1,R-INDEX(cFileName,'\'))
        cFileName = SUBSTR(cFileName,R-INDEX(cFileName,'\') + 1)
      .

      DYNAMIC-FUNCTION('OpenExcelForUpdate',cFilePath + '|' + cFileName + '|0',FALSE).
    END.
    ELSE
      LEAVE Main.            
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHent C-Win
ON DROP-FILE-NOTIFY OF btnHent IN FRAME DEFAULT-FRAME /* Hent fil */
DO:
  IF SELF:NUM-DROPPED-FILES = 1 THEN 
  DO:
    ASSIGN cFileName = SELF:GET-DROPPED-FILE(1).
    IF NOT CAN-DO("xls,xlsx",ENTRY(NUM-ENTRIES(cFileName,"."),cFileName,".")) THEN
      MESSAGE "Tillatte filtyper: '.xls,.xlsx'"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE 
    DO:
      ASSIGN 
        cFilePath = SUBSTR(cFileName,1,R-INDEX(cFileName,'\'))
        cFileName = SUBSTR(cFileName,R-INDEX(cFileName,'\') + 1)
      .

      DYNAMIC-FUNCTION('OpenExcelForUpdate',cFilePath + '|' + cFileName + '|0',FALSE).
    END.
  END.
  ELSE 
  DO:
    MESSAGE "Kun en fil er tillatt!"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
  END.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOppdater
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOppdater C-Win
ON CHOOSE OF btnOppdater IN FRAME DEFAULT-FRAME /* Kontroller og oppdater */
DO:
  DYNAMIC-FUNCTION('getExcelForDBupdate').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSend C-Win
ON CHOOSE OF btnSend IN FRAME DEFAULT-FRAME /* Åpne i Excel */
DO:
  DEF VAR iReturn    AS INT  NO-UNDO.
  DEF VAR cRowIdList AS CHAR NO-UNDO.
  DEF VAR iCount     AS INT  NO-UNDO.

  doExcelSetup().

  RUN JBoxBrowseSelectMsg.w ("Send valgte poster?",
            hBrowseLinje:NUM-SELECTED-ROWS,
            IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"rowstobatch")) THEN
               INT(DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"Totalcount"))
            ELSE 99999,
            OUTPUT iReturn).  /*1=Alle,2=Valgte*/
  
  IF iReturn = 1 THEN
    cRowidList = ''.
  ELSE IF iReturn = 2 THEN
  DO ix = 1 TO hBrowseLinje:NUM-SELECTED-ROWS:
    IF hBrowseLinje:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hFieldMapLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + ",".
  END.
  ELSE RETURN.
  

  ASSIGN 
    cRowIdList = TRIM(cRowIdList,",")
    cFieldList = TRIM(cFieldList,",")  /*look in createttExcelSetup*/
    cLabelList = TRIM(cLabelList,",")  /*look in createttExcelSetup*/
  .

  /*   DYNAMIC-FUNCTION("setServerTransInputParam",cFieldList + CHR(1) + cLabelList + CHR(1) + cRowIdList). */
  
  /*Hent data til excel*/
  SESSION:SET-WAIT-STATE("general").
  bOk = DYNAMIC-FUNCTION("runProc","varelinje_via_excel.p",
                      cFieldList 
                      + CHR(1) + cLabelList 
                      + CHR(1) + cRowidList 
                      + CHR(1) + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"BaseQuery") +
                                 DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryFilter") +
                                 IF DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") NE "" THEN
                                   ";" + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QuerySort") + DYNAMIC-FUNCTION("getAttribute",hBrowseLinje,"QueryDesc")
                                 ELSE ";"
                      ,httRapport).
  SESSION:SET-WAIT-STATE("").
  IF bOk THEN
  DO:
    /**** Had to rename the file, so that i could use SaveAs without having to overwrite it**/
    ASSIGN 
      cFileName = ENTRY(1,DYNAMIC-FUNCTION("getTransactionMessage"),'|')
      cFilePath = SUBSTR(cFileName,1,R-INDEX(cFileName,'\'))
      cFileName = SUBSTR(cFileName,R-INDEX(cFileName,'\') + 1)
      iCount    = INT(ENTRY(2,DYNAMIC-FUNCTION("getTransactionMessage"),'|'))
    .
    OS-RENAME VALUE(cFilePath + cFileName) VALUE(cFilePath + '_' + cFileName).
    OpenExcelForUpdate(cFilePath + '|_' + cFileName + '|' + STRING(iCount),FALSE).
  END.
  ELSE 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
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
  
  RELEASE OBJECT chProtect          NO-ERROR.
  RELEASE OBJECT chInterior         NO-ERROR.
  RELEASE OBJECT chWorksheet        NO-ERROR.
  RELEASE OBJECT chWorkbook         NO-ERROR.
  RELEASE OBJECT chExcelApplication NO-ERROR.
  
  DELETE OBJECT thError           NO-ERROR.
  DELETE OBJECT thVareboklinje    NO-ERROR.
  DELETE OBJECT thVareboklinjeRow NO-ERROR.
  DELETE OBJECT thVareboklinjeCol NO-ERROR.
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
  hParent = SOURCE-PROCEDURE.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF

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
  ENABLE btnSend btnOppdater btnHent 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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
DO WITH FRAME {&FRAME-NAME}:
/*   DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?). */
/* STATUS DEFAULT "All Around Sports Order Processing System". */

  ASSIGN 
    thVarebokLinjeRow = TEMP-TABLE ttVareboklinjeRow:HANDLE
    thVareboklinjeCol = TEMP-TABLE ttVareboklinjeCol:HANDLE
  .
  EMPTY TEMP-TABLE ttExcelSetup NO-ERROR.
  cleanExcel().
END.


DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
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
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setParameter C-Win 
PROCEDURE setParameter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ihFieldMapHode  AS HANDLE NO-UNDO.
DEF INPUT PARAMETER ihBrowseLinje   AS HANDLE NO-UNDO.
DEF INPUT PARAMETER ihFieldMapLinje AS HANDLE NO-UNDO.
  
  ASSIGN 
    hFieldMap      = ihFieldMapHode
    hBrowseLinje   = ihBrowseLinje
    hFieldMapLinje = ihFieldMapLinje
  .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cleanExcel C-Win 
FUNCTION cleanExcel RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RELEASE OBJECT chProtect          NO-ERROR.
RELEASE OBJECT chInterior         NO-ERROR.
RELEASE OBJECT chWorksheet        NO-ERROR.
RELEASE OBJECT chWorkbook         NO-ERROR.
RELEASE OBJECT chExcelApplication NO-ERROR.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION createTTexcelSetup C-Win 
FUNCTION createTTexcelSetup RETURNS LOGICAL
  (INPUT icFieldName   AS CHAR,
   INPUT icExcelFormat AS CHAR,
   INPUT iiWidth       AS INT,
   INPUT ibReadOnly    AS LOG,
   INPUT icUpdateField AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cLabel AS CHAR NO-UNDO.

CREATE ttExcelSetup.
cLabel = hBrowseLinje:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(icFieldName):LABEL NO-ERROR.
 
  ASSIGN 
    iNumFields                = iNumFields + 1
    ttExcelSetup.iSequence    = iNumFields
    ttExcelSetup.cFieldName   = icFieldName
    ttExcelSetup.cLabel       = (IF cLabel = ? OR cLabel = '' THEN icFieldName ELSE cLabel)
  .
  ttExcelSetup.cDataType    = getDictDataType(icFieldName).
  ttExcelSetup.cColumn      = getAlphaSeqNo(iNumFields).
  ASSIGN 
    ttExcelSetup.cExcelFormat = icExcelFormat
    ttExcelSetup.iWidth       = iiWidth
    ttExcelSetup.bReadOnly    = ibReadOnly
    ttExcelSetup.cUpdateField = icUpdateField 
    cFieldList                = cFieldList + ttExcelSetup.cFieldName + ','
    cLabelList                = cLabelList + ttExcelSetup.cLabel + ','
  .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION doExcelSetup C-Win 
FUNCTION doExcelSetup RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  EMPTY TEMP-TABLE ttExcelSetup.
  ASSIGN 
    iNumFields = 0
    cFieldList = ''
    cLabelList = ''
  .

  /*   Her skal de kunne arbeide i                                                                               */
/*     Engros, Rab.forh, Rab.sup, Markedspris, Veil pris, Kamp.pris, Levuke, Sort, Kamp.Uke, Støtte, og lager. */
/*                                                                                                             */
/* Kollonnene Nto.innpr forh, Nto.innpr sup skal være stengt men ha formel. De regnes i prosent av engros.     */

  /*Det er ikke tatt hensyn til å kunne oppdatere på kolonner som tilhører annen tabell enn vareboklinje,
  programmene vil feile*/

  /*FieldName,Excel Format,Excel Width,Read Only,Formula*/
  createTTexcelSetup('Rowid','0',10,TRUE,'').
  createTTexcelSetup('vareboknr','0',10,TRUE,'').
  createTTexcelSetup('artikkelnr','0',10,TRUE,'').
  createTTexcelSetup('levnr','0',10,TRUE,'').
  createTTexcelSetup('levnamn','@',10,TRUE,'').                 /*Lev.navn*/
  createTTexcelSetup('beskrivelse','@',30,TRUE,'').             /*Varemerke*/
  createTTexcelSetup('beskr','@',30,TRUE,'').                   /*Art.navn*/
  createTTexcelSetup('levkod','@',10,TRUE,'').                  /*Lev.art.nr*/
  createTTexcelSetup('levfargkod','@',10,TRUE,'').              /*Lev.farge*/
  createTTexcelSetup('innkjopspris','# ##0,00',10,FALSE,'').       /*Engros*/
  createTTexcelSetup('varekost','# ##0,00',10,TRUE,'').         /*Nto.innpr forh*/
  createTTexcelSetup('forhrab%','0,0',10,FALSE,'').               /*Rab.forh*/       
  createTTexcelSetup('forhkalkyle','# ##0,00',10,TRUE,'').      /*Klk.f*/
  createTTexcelSetup('supvarekost','# ##0,00',10,TRUE,'').      /*Nto.innpr sup*/
  createTTexcelSetup('suprab%','0,0',10,FALSE,'').                /*Rab.sup*/
  createTTexcelSetup('supKalkyle','# ##0,00',10,TRUE,'').       /*Klk.s*/
  createTTexcelSetup('pris','# ##0,00',10,FALSE,'').              /*Markedspris*/
  createTTexcelSetup('anbefaltpris','# ##0,00',10,FALSE,'').      /*Veil.pris*/
  createTTexcelSetup('Kampanjepris','# ##0,00',10,FALSE,'').      /*Kamp.pris*/
  createTTexcelSetup('kjederab%','0,0',10,FALSE,'').              /*Kj.rab%*/
  createTTexcelSetup('kjedeinnkpris','# ##0,00',10,TRUE,'').    /*Kj.ink.pr*/
  createTTexcelSetup('kjedesuprab%','0,0',10,FALSE,'').           /*Kj.sup.rab%*/
  createTTexcelSetup('kjedesupinnkpris','# ##0,00',10,TRUE,''). /*Kj.sup.ink.pr*/
/*   createTTexcelSetup('sasong','0',10,FALSE,'').                   /*Sasong*/ */
  createTTexcelSetup('SasBeskr','@',30,FALSE,'sasong|sasong').                 /*Sesong (sasong)*/ /*UpdateField: lookup table | field in vareboklinje*/
  createTTexcelSetup('Kjedevare','@',10,FALSE,'').                /*Kj.vare*/
  createTTexcelSetup('gjennomfaktureres','@',10,FALSE,'').        /*Gj.fakt*/
  createTTexcelSetup('fritttillegg','@',10,TRUE,'').            /*Fritt tillegg (Artbas)*/
  createTTexcelSetup('salgsenhet','@',10,TRUE,'').              /*Salgsenhet (artbas)*/
  createTTexcelSetup('AntIPakn','0',10,TRUE,'').                /*Antipakn (artbas)*/
  createTTexcelSetup('levuke','0',10,FALSE,'').                   /*LevUke*/
/*   createTTexcelSetup('Ravdnr','0',10,FALSE,'').                   /*Vareomr*/ */
  createTTexcelSetup('RavdBeskrivelse','@',30,FALSE,'Regnskapsavdeling|artbas.RavdNr').            /*Regnskapsavdeling*/
  createTTexcelSetup('Sortimentkoder','@',10,FALSE,'LookupValues|' + DYNAMIC-FUNCTION('getFieldValues','messe','WHERE messenr = ' + STRING(hFieldMap:BUFFER-FIELD('messenr'):BUFFER-VALUE),'sortimentkoder')).           /*Sort*/
  createTTexcelSetup('Kampanjeuker','@',10,FALSE,'LookupValues|' + DYNAMIC-FUNCTION('getFieldValues','messe','WHERE messenr = ' + STRING(hFieldMap:BUFFER-FIELD('messenr'):BUFFER-VALUE),'kampanjeuker')).             /*Kamp.uke*/
  createTTexcelSetup('Kampanjestotte','@',10,FALSE,'LookupValues|' + DYNAMIC-FUNCTION('getFieldValues','messe','WHERE messenr = ' + STRING(hFieldMap:BUFFER-FIELD('messenr'):BUFFER-VALUE),'kampanjestotte')).           /*Støtte*/
  createTTexcelSetup('lagerkoder','@',10,FALSE,'LookupValues|' + DYNAMIC-FUNCTION('getFieldValues','messe','WHERE messenr = ' + STRING(hFieldMap:BUFFER-FIELD('messenr'):BUFFER-VALUE),'lagerkoder')).               /*Lager*/
  createTTexcelSetup('linjemerknad','0',10,TRUE,'').            /*Merknad*/
  createTTexcelSetup('artikkelnr','0',10,TRUE,'').              /*Art.nr*/
  createTTexcelSetup('vg','0',10,FALSE,'').                       /*VgNr*/
  createTTexcelSetup('vgbeskr','@',30,TRUE,'').                 /*VgBeskr*/
/*   createTTexcelSetup('vgbeskr','@',10,FALSE,'VarGr|Vg').                 /*VgBeskr*/ */
  createTTexcelSetup('avdelingNr','@',10,TRUE,'').              /*Avdelingnr*/
  createTTexcelSetup('avdelingNavn','@',30,TRUE,'').            /*Avdeling navn*/
  createTTexcelSetup('Hg','@',10,TRUE,'').                      /*hg*/
  createTTexcelSetup('HgBeskr','@',30,TRUE,'').                 /*hgbeskr*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAlphaSeqNo C-Win 
FUNCTION getAlphaSeqNo RETURNS CHARACTER
  ( INPUT iiSeqNo AS INT ) :
/*------------------------------------------------------------------------------
  Purpose: Return corresponding letter for a sequence number 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.

IF iiSeqNo < 0 OR iiSeqNo > 80 THEN RETURN "".
ELSE IF iiSeqNo < 27 THEN
  ocValue = CHR(64 + iiSeqNo).
ELSE IF iiSeqNo < 54 THEN
  ocValue = "A" + CHR(64 - 26 + iiSeqNo).
ELSE IF iiSeqNo < 81 THEN
  ocValue = "B" + CHR(64 - 26 + iiSeqNo).

RETURN ocValue.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDictDataType C-Win 
FUNCTION getDictDataType RETURNS CHARACTER
  (INPUT icFieldName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hBuffer AS HANDLE      NO-UNDO.
    DEFINE VARIABLE i       AS INTEGER     NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL     NO-UNDO.

    DO i = 1 TO NUM-DBS:
        CREATE BUFFER hBuffer FOR TABLE SUBSTITUTE( "&1._Field":U, LDBNAME( i ) ).

        ASSIGN lFound = hBuffer:FIND-FIRST( SUBSTITUTE( "WHERE _Field-Name EQ &1":U, QUOTER(icFieldName ) ) ) NO-ERROR.
        IF lFound THEN
            RETURN hBuffer:BUFFER-FIELD( "_Data-Type":U ):BUFFER-VALUE.
    END.

    RETURN ?.

/*   DEF VAR qh AS HANDLE NO-UNDO.                                            */
/*                                                                            */
/*   DEF VAR bh AS HANDLE NO-UNDO.                                            */
/*   bh = BUFFER _field:HANDLE.                                               */
/*   bh:FIND-FIRST('where _field-name = ' + QUOTER(icFieldName)) NO-ERROR.    */
/*   IF bh:AVAILABLE THEN RETURN bh:BUFFER-FIELD('_Data-Type'):BUFFER-VALUE.  */
/*   ELSE RETURN ''.                                                          */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getExcelForDBupdate C-Win 
FUNCTION getExcelForDBupdate RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
    Burde kanskje benytte unique nøkkel istedet for rowid
    
------------------------------------------------------------------------------*/
  DEF VAR obError AS CHAR   NO-UNDO.
  DEF VAR qh      AS HANDLE NO-UNDO.
  DEF VAR iRow    AS INT    NO-UNDO.
  DEF VAR cError  AS CHAR   NO-UNDO.  

  EMPTY TEMP-TABLE ttVareboklinjeRow.
  EMPTY TEMP-TABLE ttVareboklinjeCol.
  
  IF NOT VALID-HANDLE(chExcelApplication) THEN
  DO:
    MESSAGE 'Åpne Excel via knapp før kontroll kan gjennomføres'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    LEAVE.
  END.
  SESSION:SET-WAIT-STATE("general").
  
  ASSIGN 
    iCount = 1 /*Hopp over header*/
    bOk    = FALSE
    ix     = 0
  .
  DO WHILE NOT bOk:
    chExcelApplication:VISIBLE = FALSE NO-ERROR.
    bOk = NOT ERROR-STATUS:ERROR.
    IF bOk THEN LEAVE.
    bOk = (IF ix GT 2 THEN TRUE ELSE bOk).
    ix = ix + 1.
    PAUSE 1.
  END.
  IF ERROR-STATUS:ERROR THEN 
  DO:
    MESSAGE 'Klarer ikke å oppdatere, husk å trykke enter etter endring av en celle'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    LEAVE.
  END.
  chExcelApplication:ActiveSheet:Unprotect('').
  /*Check if Column A has a value, if so, read the whole line*/
  DO WHILE chWorkSheet:range("A" + STRING(iCount)):VALUE NE ?:
    iCount = iCount + 1.
    CREATE ttVareboklinjeRow.
    ASSIGN 
      ttVareboklinjeRow.iRowNum    = iCount
      ttVareboklinjeRow.rRowid     = TO-ROWID(chWorkSheet:range("A" + STRING(iCount)):VALUE)
      ttVareboklinjeRow.Vareboknr  = DEC(chWorkSheet:Range("B" + STRING(iCount)):VALUE)
      ttVareboklinjeRow.Artikkelnr = DEC(chWorkSheet:Range("C" + STRING(iCount)):VALUE)

    .
    /*For each 10 records, show count in status area*/
    IF iCount MOD 10 = 0 THEN
    DO:
      STATUS DEFAULT 'Antall poster:' + STRING(iCount).
      STATUS INPUT 'Antall poster:' + STRING(iCount).
    END.
    /*For each row that is not read only, set the color to white*/
    FOR EACH ttExcelSetup WHERE NOT ttExcelSetup.bReadOnly:
      CREATE ttVareboklinjeCol.
      ASSIGN 
        ttVareboklinjeCol.rRowid       = ttVareboklinjeRow.rRowid
        ttVareboklinjeCol.iFieldNum    = iSequence
        ttVareboklinjeCol.cFieldName   = ttExcelSetup.cFieldName
        ttVareboklinjeCol.cDataType    = ttExcelSetup.cDataType
        ttVareboklinjeCol.cUpdateField = ttExcelSetup.cUpdateField
        ttVareboklinjeCol.cValue       = chWorkSheet:range(ttExcelSetup.cColumn + STRING(iCount)):VALUE
      .
      chWorkSheet:Range(ttExcelSetup.cColumn + STRING(iCount)):SELECT().
      chInterior            = chExcelApplication:Selection:Interior.
      chInterior:ColorIndex = 0.
      RELEASE OBJECT chInterior.
    END.
  END.
  /*Show the count in status area*/
  STATUS DEFAULT 'Antall poster:' + STRING(iCount - 2).
  STATUS INPUT 'Antall poster:' + STRING(iCount - 2).
  SESSION:SET-WAIT-STATE("").

  /*Check the data for update*/
  RUN sjekk_excelupdate.p (INPUT-OUTPUT TABLE-HANDLE thVareboklinjeRow, INPUT-OUTPUT TABLE-HANDLE thVareboklinjeCol, OUTPUT TABLE-HANDLE thVareboklinje, OUTPUT TABLE-HANDLE thError).
  CREATE QUERY qh.
  qh:SET-BUFFERS(thError:DEFAULT-BUFFER-HANDLE).
  qh:QUERY-PREPARE('FOR EACH ' + thError:DEFAULT-BUFFER-HANDLE:NAME + ' BY iRowNum').
  qh:QUERY-OPEN().
  qh:GET-FIRST().

  /*Kan slå legges på ttVareboklinje, for å slippe ekstra error tabell...*/
  cError = ''.
  ix = 1.
  DO WHILE NOT qh:QUERY-OFF-END:
    ASSIGN 
      ix = ix + 1
      iRow   = thError:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('iRowNum'):BUFFER-VALUE
      cError = cError + 'Rad:' + STRING(ix) + ' - ' + thError:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('cErrorText'):BUFFER-VALUE + CHR(10)
    .
    
    chExcelApplication:ActiveSheet:Range(getAlphaSeqNo(thError:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('iColumnNum'):BUFFER-VALUE) + STRING(iRow) + ':' + getAlphaSeqNo(thError:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('iColumnNum'):BUFFER-VALUE) + STRING(iRow)):SELECT().
    chInterior = chExcelApplication:Selection:Interior.
    chInterior:ColorIndex = 3.
    RELEASE OBJECT chInterior.
    qh:GET-NEXT().
  END.
  DELETE OBJECT qh NO-ERROR.
  
  CREATE QUERY qh.
  qh:SET-BUFFERS(thVareboklinje:DEFAULT-BUFFER-HANDLE).
  qh:QUERY-PREPARE('FOR EACH ' + thVareboklinje:DEFAULT-BUFFER-HANDLE:NAME + ' WHERE NOT bHasError BY iRowNum DESC').
  qh:QUERY-OPEN().
  qh:GET-FIRST().

  DO WHILE NOT qh:QUERY-OFF-END:
    chExcelApplication:ActiveSheet:Rows(thVareboklinje:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('iRowNum'):BUFFER-VALUE):DELETE.  
    qh:GET-NEXT().
  END.

  chExcelApplication:ActiveSheet:Protect('',TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE).
  
  IF cError NE '' THEN 
  DO:
    DYNAMIC-FUNCTION("DoMessage",0,20,cError,'Feilmelding','').
    chExcelApplication:VISIBLE = TRUE.
    chExcelApplication:ActiveWorkbook:Save.
  END.
  ELSE 
  DO:
    chWorkbook:CLOSE(FALSE,FALSE).
    OS-DELETE VALUE(cFilename).
    cleanExcel().
  END.


  RUN OpenQuery IN hParent.

  RETURN TRUE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getReportFile C-Win 
FUNCTION getReportFile RETURNS CHARACTER
  ( INPUT icClientServerFile AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFileName    AS CHAR   NO-UNDO.
DEF VAR httRepTable  AS HANDLE NO-UNDO.
DEF VAR httRepBuffer AS HANDLE NO-UNDO. 
DEF VAR hQuery       AS HANDLE NO-UNDO.

RETURN icClientServerFile. /* <- Fjern denne ved overgang til Appserver */

ASSIGN httRepTable  = DYNAMIC-FUNCTION("getRunProcReturnTable",?)
       httRepBuffer = httRepTable:DEFAULT-BUFFER-HANDLE
       cFileName = SESSION:TEMP-DIR + STRING(hFieldMap:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE) + "_" + DYNAMIC-FUNCTION("getASuserId") + "_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY)) + STRING(TIME) + ".txt".  

OUTPUT TO VALUE(cFileName).

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(httRepBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + httRepBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  PUT UNFORMATTED httRepBuffer:BUFFER-FIELD("cRpLinje"):BUFFER-VALUE.
  hQuery:GET-NEXT().
END.

OUTPUT CLOSE.

DELETE OBJECT hQuery.
DELETE OBJECT httRepTable.

RETURN cFileName.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OpenExcelForUpdate C-Win 
FUNCTION OpenExcelForUpdate RETURNS LOGICAL
  ( INPUT icFileAndCount AS CHAR,
    INPUT ibTestPrint    AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iCntStr                 AS INT  NO-UNDO.
DEF VAR cLookupList             AS CHAR NO-UNDO.
DEF VAR cTmpFileName            AS CHAR NO-UNDO.

ASSIGN 
  cFilePath = ENTRY(1,icFileAndCount,"|")
  cFileName = getReportFile(ENTRY(2,icFileAndCount,"|")) 
  iCount    = INT(ENTRY(3,icFileAndCount,"|"))
.
chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
IF NOT VALID-HANDLE(chExcelApplication) THEN 
DO:
  IF DYNAMIC-FUNCTION("setWebDoc","Open",cFileName) NE "" THEN
    MESSAGE "Could not open file: " cFileName VIEW-AS ALERT-BOX ERROR.
  RETURN TRUE.
END.
  
IF cFileName BEGINS '_' THEN
DO:
  ASSIGN 
    cTmpFileName = cFileName
    cFileName    = TRIM(cFileName,'_')
  .
  cFullFileName = cFilePath + cTmpFileName.
  chExcelApplication:Workbooks:OpenText(cFullFileName,2,,,,,TRUE).
  chExcelApplication:ActiveWorkbook:SaveAs(cFilePath + cFileName,18,,,,,,,,,,).
  OS-DELETE VALUE(cFilePath + cTmpFileName).
END.
ELSE
  chExcelApplication:Workbooks:Open(cFilePath + cFileName,,,,,,TRUE).

ASSIGN 
  chWorkbook  = chExcelApplication:WorkBooks:ITEM(1)
  chWorkSheet = chExcelApplication:Sheets:ITEM(1)
  cUnProtect = ''
  .

chExcelApplication:ActiveSheet:Unprotect('').

/*Clear format*/
FOR EACH ttExcelSetup NO-LOCK:
  cColumn = ttExcelSetup.cColumn + ':' + ttExcelSetup.cColumn.
  chWorkSheet:Range(cColumn):NumberFormat = ttExcelSetup.cExcelFormat.
  chWorkSheet:Range(cColumn):ColumnWidth  = ttExcelSetup.iWidth.

  chWorkSheet:Range(cColumn):SELECT().
  chInterior = chExcelApplication:SELECTION:Interior.
  chInterior:ColorIndex = 15.
  RELEASE OBJECT chInterior NO-ERROR.

  cUnprotect = IF NOT ttExcelSetup.bReadOnly THEN cUnprotect + ttExcelSetup.cColumn + ':' + ttExcelSetup.cColumn + ',' ELSE cUnprotect.
END.

/*Set validation for unprotected columns*/
FOR EACH ttExcelSetup WHERE NOT bReadOnly:

  cColumn = ttExcelSetup.cColumn + ':' + ttExcelSetup.cColumn.
  
  chWorksheet:COLUMNS(cColumn):LOCKED        = FALSE.
  chWorksheet:COLUMNS(cColumn):FormulaHidden = FALSE.

  chWorkSheet:COLUMNS(cColumn):SELECT().
  chInterior = chExcelApplication:SELECTION:Interior.
  chInterior:ColorIndex = 0.
  RELEASE OBJECT chInterior.
 
  /*LOOKUP Fields*/
  IF ttExcelSetup.cUpdateField NE '' THEN
  DO:
    IF ttExcelSetup.cUpdateField BEGINS "LookupValues" THEN
    DO:
      IF ENTRY(2,ttExcelSetup.cUpdateField,'|') = '' THEN NEXT. /*There is no value in the LookupValues|.....*/
      cLookupList = ENTRY(2,ttExcelSetup.cUpdateField,'|').
      cLookupList = REPLACE(cLookupList,'¤',';').
      chWorksheet:COLUMNS(cColumn):SELECT().
      chInterior = chExcelApplication:SELECTION:Validation.
      chInterior:DELETE().
      chInterior:ADD(3,1,1,cLookupList,TRUE) NO-ERROR.
      ttExcelSetup.cUpdateField = ''.

      RELEASE OBJECT chInterior.
    END.
    ELSE
    DO:
      cLookupList = DYNAMIC-FUNCTION("getFieldList",ENTRY(1,ttExcelSetup.cUpdateField,'|') + ';' + ttExcelSetup.cFieldName,"WHERE TRUE").
      cLookupList = REPLACE(cLookupList,'|',';').
      chWorksheet:COLUMNS(cColumn):SELECT().
      chInterior = chExcelApplication:SELECTION:Validation.
      chInterior:DELETE().
      chInterior:ADD(3,1,1,cLookupList,TRUE) NO-ERROR.
  
      RELEASE OBJECT chInterior.
    END.
  END.
  ELSE
  DO:
    CASE ttExcelSetup.cDataType:
      WHEN 'INTEGER' THEN
      DO:
        chWorksheet:COLUMNS(cColumn):SELECT().
        chInterior = chExcelApplication:SELECTION:Validation.
        chInterior:DELETE().
        chInterior:ADD(1,1,7,'0',TRUE).
  
        RELEASE OBJECT chInterior.    
      END.
      WHEN 'DECIMAL' THEN
      DO:
        chWorksheet:COLUMNS(cColumn):SELECT().
        chInterior = chExcelApplication:SELECTION:Validation.
        chInterior:DELETE().
        chInterior:ADD(2,1,7,'0',TRUE).
  
        RELEASE OBJECT chInterior.    
      END.
      WHEN 'DATE' THEN
      DO:
        chWorksheet:COLUMNS(cColumn):SELECT().
        chInterior = chExcelApplication:SELECTION:Validation.
        chInterior:DELETE().
        chInterior:ADD(4,1,7,'1/1/1900',TRUE).

        RELEASE OBJECT chInterior.    
      END.
      WHEN 'LOGICAL' THEN
      DO:
        chWorksheet:COLUMNS(cColumn):SELECT().
        chInterior = chExcelApplication:SELECTION:Validation.
        chInterior:DELETE().
        chInterior:ADD(3,1,1,'Yes;No',TRUE).

        RELEASE OBJECT chInterior.    
      END.
    END CASE.
  END.
END.

/*
   With Selection.Validation
        .Delete
        .Add Type:=xlValidateList, AlertStyle:=xlValidAlertStop, Operator:= _
        xlBetween, Formula1:="A;b;c"
        .IgnoreBlank = True
        .InCellDropdown = True
        .InputTitle = ""
        .ErrorTitle = ""
        .InputMessage = ""
        .ErrorMessage = ""
        .ShowInput = True
        .ShowError = True
    End With
    
chWorksheet:Range("G1"):SELECT().
chInterior = chExcelApplication:SELECTION:Validation.
chInterior:DELETE().
chInterior:ADD(3,1,1,'A;B;C;D',TRUE).

RELEASE OBJECT chInterior.
    

*/
chWorksheet:COLUMNS("A:C"):SELECT().
chExcelApplication:SELECTION:EntireColumn:HIDDEN = TRUE.

chExcelApplication:ActiveSheet:Rows(1):FONT:BOLD = TRUE.
chWorkSheet:Range("D2"):Select().
chExcelApplication:ActiveWindow:FreezePanes = TRUE.


cColumn = "D:" + DYNAMIC-FUNCTION('getAlphaSeqNo',iNumFields).
chExcelApplication:VISIBLE = TRUE.
  
chWorksheet:Range('A' + '1'):SELECT().
chExcelApplication:SELECTION:AutoFilter(1,?,1,?,TRUE).
chWorkSheet:Columns(cColumn):AutoFit().

chExcelApplication:ActiveSheet:Protect("",TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE).
chExcelApplication:ActiveWorkbook:Save.

/* release com-handles */

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

