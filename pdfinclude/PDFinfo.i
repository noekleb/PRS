/******************************************************************************

  Program:      PDFinfo.i
  
  Written By:   Gordon Campbell
  Written On:   January 2004
  
  Description:  Defines the temp-tables that are required in PDFinfo.p and it's
                calling procedure.
  
******************************************************************************/

/* Used to Return Font List */
DEFINE TEMP-TABLE TT_Font NO-UNDO
  FIELD font_name   AS CHARACTER.

/* Used to Return Information Parameters */
DEFINE TEMP-TABLE TT_Info NO-UNDO
  FIELD info_name   AS CHARACTER
  FIELD info_value  AS CHARACTER.
