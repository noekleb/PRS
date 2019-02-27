&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*------------------------------------------------------------------------
    Library     : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF VAR chExcelApplication    AS COM-HANDLE.  
DEF VAR chWorkbooks           AS COM-HANDLE.
DEF VAR chWorksheets          AS COM-HANDLE.
Define variable cColumn         as char no-undo.
Define variable iColumn         as integer no-undo.
Define variable lvResult        as logical no-undo.
Define variable cvValue   as char no-undo.
Define variable cvRange   as char no-undo.
Define variable cvType   as char no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-AddWorksheet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddWorksheet Method-Library 
FUNCTION AddWorksheet RETURNS COM-HANDLE
  ( INPUT chExcelApplication       as COM-HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateExcelAppl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateExcelAppl Method-Library 
FUNCTION CreateExcelAppl RETURNS COM-HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateWorkbook) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateWorkbook Method-Library 
FUNCTION CreateWorkbook RETURNS COM-HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetExcelVersion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetExcelVersion Method-Library 
FUNCTION GetExcelVersion RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetActivtWorksheet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetActivtWorksheet Method-Library 
FUNCTION SetActivtWorksheet RETURNS COM-HANDLE
  ( INPUT chExcelApplication    as COM-HANDLE, 
    INPUT iWorksheet            as integer )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetColumnsFormat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetColumnsFormat Method-Library 
FUNCTION SetColumnsFormat RETURNS LOGICAL
  (INPUT chExcelApplication as COM-HANDLE,
   INPUT chWorkSheets       as COM-HANDLE,
   INPUT cvRange            as char,
   INPUT cvFormat           as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetColumnsValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetColumnsValue Method-Library 
FUNCTION SetColumnsValue RETURNS LOGICAL
  (INPUT chWorkSheets   as COM-HANDLE,
   INPUT cvRange       as char,
   INPUT cvValue        as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetColumnsWidth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetColumnsWidth Method-Library 
FUNCTION SetColumnsWidth RETURNS LOGICAL
  (INPUT chWorkSheets   as COM-HANDLE,
   INPUT cvColumn       as char,
   INPUT ivColumnWidth  as integer)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetExcelApplVisable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetExcelApplVisable Method-Library 
FUNCTION SetExcelApplVisable RETURNS LOGICAL
  ( INPUT chExcelApplication as COM-HANDLE,
    INPUT Visable as Logical )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetPageSetup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetPageSetup Method-Library 
FUNCTION SetPageSetup RETURNS LOGICAL
  ( INPUT chExcelApplication as COM-HANDLE,
    INPUT chWorkSheets       as COM-HANDLE,
    INPUT cvType              as char,
    INPUT cvRange            as char,
    INPUT cvValue            as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Method-Library 
/* ************************* Included-Libraries *********************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Method-Library 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-AddWorksheet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddWorksheet Method-Library 
FUNCTION AddWorksheet RETURNS COM-HANDLE
  ( INPUT chExcelApplication       as COM-HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
    DYNAMIC-FUNCTION('AddWorksheet':U, chExcelApplication)
------------------------------------------------------------------------------*/
  Define variable chWorksheet as COM-HANDLE no-undo.
  
  chWorksheet = chExcelApplication:Sheets:add().
  RETURN chWorkSheet.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateExcelAppl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateExcelAppl Method-Library 
FUNCTION CreateExcelAppl RETURNS COM-HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
Define variable localchExcelApplication as COM-HANDLE.
  CREATE "Excel.Application" localchExcelApplication.  
  RETURN localchExcelApplication.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateWorkbook) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateWorkbook Method-Library 
FUNCTION CreateWorkbook RETURNS COM-HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
Define variable chWorkbooks as COM-HANDLE  no-undo.
  chWorkbooks =   chExcelApplication:Workbooks:Add().
  RETURN chWorkbooks.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetExcelVersion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetExcelVersion Method-Library 
FUNCTION GetExcelVersion RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  DYNAMIC-FUNCTION('GetExcelVersion':U)
------------------------------------------------------------------------------*/
  LOAD "EXCEL.Application" BASE-KEY "HKEY_CLASSES_ROOT" NO-Error. /* Open Registry key */
  If error-status:error 
   THEN Return "95".
   Else
   Do:
     If error-status:error = FALSE then Return "97".
   End.

   Message "Ukjent excel versjon" view-as alert-box INFORMATION.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetActivtWorksheet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetActivtWorksheet Method-Library 
FUNCTION SetActivtWorksheet RETURNS COM-HANDLE
  ( INPUT chExcelApplication    as COM-HANDLE, 
    INPUT iWorksheet            as integer ) :
/*------------------------------------------------------------------------------
  Purpose:  Setter aktivt worksheet
    Notes:  
------------------------------------------------------------------------------*/
Define variable chWorkSheets as COM-HANDLE no-undo.

  Assign chWorkSheets = chExcelApplication:Sheets:Item(iWorksheet).
  RETURN chWorkSheets.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetColumnsFormat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetColumnsFormat Method-Library 
FUNCTION SetColumnsFormat RETURNS LOGICAL
  (INPUT chExcelApplication as COM-HANDLE,
   INPUT chWorkSheets       as COM-HANDLE,
   INPUT cvRange            as char,
   INPUT cvFormat           as char) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
    DYNAMIC-FUNCTION('SetColumnsFormat':U,chExcelApplication,chWorkSheets,cvRange,cvFormat).
------------------------------------------------------------------------------*/
  chWorkSheets:Range(cvRange):Select(). 
/*      Columns("E:E").Select 
    Selection.NumberFormat = "m/d/yyyy"
*/
/*    chWorkSheets:Columns(cvRange).Select(). */
  
  
  Case cvFormat:
    when "Date"     then 
                     Do:
                       /*chExcelApplication:Selection:Category = "2". */
                       chExcelApplication:Selection:NumberFormat = "d/m/yyyy".
                     End.  
    when "Tekst"    then  chExcelApplication:Selection:NumberFormat = "@".
    when "Bold"     then  chExcelApplication:Selection:Font:Bold = TRUE.
    when "Decimal"    then  chExcelApplication:Selection:NumberFormat = "#,0#".
    when "Integer"    then  chExcelApplication:Selection:NumberFormat = "# ##0".

  End Case.
  RETURN TRUE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetColumnsValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetColumnsValue Method-Library 
FUNCTION SetColumnsValue RETURNS LOGICAL
  (INPUT chWorkSheets   as COM-HANDLE,
   INPUT cvRange       as char,
   INPUT cvValue        as char) :
/*------------------------------------------------------------------------------
  Purpose:  Setter bredden på en kollone
    Notes:  
------------------------------------------------------------------------------*/
  chWorkSheets:Range(cvRange):Value = cvValue.
  RETURN TRUE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetColumnsWidth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetColumnsWidth Method-Library 
FUNCTION SetColumnsWidth RETURNS LOGICAL
  (INPUT chWorkSheets   as COM-HANDLE,
   INPUT cvColumn       as char,
   INPUT ivColumnWidth  as integer) :
/*------------------------------------------------------------------------------
  Purpose:  Setter bredden på en kollone
    Notes:  
    DYNAMIC-FUNCTION('SetColumnsWidth':U,chWorkSheets,cvColumn,ivColumnWidth)
------------------------------------------------------------------------------*/
  if ivColumnWidth = 0
   then chWorkSheets:Columns(cvColumn):AutoFit().
   else chWorkSheets:Columns(cvColumn):ColumnWidth = ivColumnWidth.
  
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetExcelApplVisable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetExcelApplVisable Method-Library 
FUNCTION SetExcelApplVisable RETURNS LOGICAL
  ( INPUT chExcelApplication as COM-HANDLE,
    INPUT Visable as Logical ) :
/*------------------------------------------------------------------------------
  Purpose:Setter Excel Visable/Invisable  
    Notes:  
  lvResult = DYNAMIC-FUNCTION('SetExcelApplVisable':U , chExcelApplication , TRUE). 
------------------------------------------------------------------------------*/
/*      chWorkSheets:Range("A1:A1"):Select(). /* Fjerner selection*/*/


    ASSIGN
      chExcelApplication:Visible = Visable.


    RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetPageSetup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetPageSetup Method-Library 
FUNCTION SetPageSetup RETURNS LOGICAL
  ( INPUT chExcelApplication as COM-HANDLE,
    INPUT chWorkSheets       as COM-HANDLE,
    INPUT cvType              as char,
    INPUT cvRange            as char,
    INPUT cvValue            as char) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/


  Case cvType:
/*    When "PrintArea"        then   chWorkSheets:PageSetup:PrintArea      = cvRange.    */
    When "FitToPagesWide"   then   chWorkSheets:PageSetup:FitToPagesWide = 1. 
    When "CenterHeader"     then   chWorkSheets:PageSetup:CenterHeader   = cvValue.  
    When "LeftHeader"       then   chWorkSheets:PageSetup:LeftHeader     = cvValue.  
    When "Orientation"      then   chWorkSheets:PageSetup:Orientation    = cvValue.  
  End Case.

  RETURN TRUE.   /* Function return value. */
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

