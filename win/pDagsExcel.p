&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
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
DEFINE INPUT  PARAMETER iNumCols  AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER iLastRow  AS INTEGER NO-UNDO.

DEFINE VARIABLE chExcelApplication      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
RUN toExcel.

  /* release com-handles */
      
  RELEASE OBJECT chWorksheetRange NO-ERROR.
  RELEASE OBJECT chWorkSheet NO-ERROR.
  RELEASE OBJECT chWorkbook NO-ERROR.
  RELEASE OBJECT chExcelApplication NO-ERROR.
  ASSIGN chWorksheetRange   = ?
         chWorksheet        = ?
         chWorkbook         = ?
         chExcelApplication = ?.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-toExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE toExcel Procedure 
PROCEDURE toExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iCount                  AS INTEGER NO-UNDO.
DEFINE VARIABLE iCount2                 AS INTEGER NO-UNDO.
DEFINE VARIABLE iCountH                 AS INTEGER NO-UNDO.
DEFINE VARIABLE iColumn                 AS INTEGER NO-UNDO.
DEFINE VARIABLE cLastColName            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLabels   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE numrows   AS INTE INIT 6 NO-UNDO.
DEFINE VARIABLE numcols   AS INTE NO-UNDO.
DEFINE VARIABLE cRangeCol AS CHARACTER  NO-UNDO.
{sww.i}

DO:
/*   ASSIGN cSeparator = IF "" = ":" THEN " " */
/*            ELSE "{&DEFAULTSEP}".           */
  /* create a new Excel Application object */
  CREATE "Excel.Application" chExcelApplication.

  /* launch Excel so it is not visible to the user */
  chExcelApplication:Visible = FALSE.
  /* create a new Workbook */
  chWorkbook = chExcelApplication:Workbooks:Add(1).

  /* get the active Worksheet */
  DO:
    ASSIGN cLastColName = CHR(ASC("A") + iNumCols - 1)
           chWorkSheet = chExcelApplication:Sheets:Item(1)
           chWorkSheet:PageSetup:Orientation    = 2. /* landscape */
/*            chWorkSheet:NAME = "Periodjämförelse". */
           chWorkSheet:Activate().
           chWorkSheet:Range("A1:E1"):Font:Bold = TRUE.
           chWorkSheet:Range("A2:" + CAPS(cLastColName) + "2"):Font:Bold = TRUE.
           chWorkSheet:Range("A2:" + CAPS(cLastColName) + "2"):HorizontalAlignment = -4108.
           chWorkSheet:Range("B3:B" + STRING(iLastRow)):HorizontalAlignment = -4108.
           chWorkSheet:Range("A" + STRING(iLastRow + 1) + ":" + CAPS(cLastColName) + STRING(iLastRow + 1)):Font:Bold = TRUE.
           chWorkSheet:Range(CAPS(cLastColName) + "3" + ":" + CAPS(cLastColName) + STRING(iLastRow)):Font:Bold = TRUE.
  END.
  chWorkSheet:PASTE().
  chWorkSheet:Range("A1:" + cLastColName + "1"):Merge().
  chWorkSheet:Columns("A:C"):AutoFit().
/*   chWorkSheet:Range("F:F"):NumberFormat = "0,0_;[Red]-0,0 ". */
  DO iCount = 4 TO iNumCols:
      ASSIGN cRangeCol = CHR(ASC("A") + iCount - 1) + STRING(iLastRow + 1).
      chWorkSheet:Range(cRangeCol):Formula = SUBSTITUTE("=SUM(&1:&2)",CHR(ASC("A") + iCount - 1) + "3",CHR(ASC("A") + iCount - 1) + STRING(iLastRow)).
  END.
  DO iCount = 3 TO iLastRow:
      ASSIGN cRangeCol = cLastColName + STRING(iCount).
      chWorkSheet:Range(cRangeCol):Formula = SUBSTITUTE("=SUM(D&1:&2&3)",STRING(iCount),CHR(ASC(cLastColName) - 1),STRING(iCount)).
  END.
  chWorkSheet:Range("D3"):Select().
  chExcelApplication:ActiveWindow:FreezePanes = True.
  chWorkSheet:Activate().
  chExcelApplication:Visible = TRUE.
  {swn.i}

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

