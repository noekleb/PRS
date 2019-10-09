&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : pdflibrary.p 
    Purpose     : converts a browser to pdf report

    Syntax      :

    Description :

    Author:  Julian Lyndon-Smith
             Dot R Limited
             Released under possenet licence 
             http://www.possenet.org/legal/license.html
             
    Created     :
    Notes       : 1.2 jmls 
                  Changed 10 to {&MaxColumns}
                  Added function callback support for each column
                  Added IGC changes
                  added workarounds for calc columns
                  Fixed label and format assignments
    
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


&SCOPED-DEFINE version $Id: PDFLibrary.p,v 1.2 2003/12/05 18:59:01 gcampbell Exp $
&SCOPED-DEFINE MaxColumns 10

/* use pdf include from www.epro-sys.com */
{ pdf_inc.i "THIS-PROCEDURE"} 

/* temp-table definitions for parameters */
{pdfprint.i}

/* we use a handle to the passed TT here, so that changes to the base temp-table can 
   be made without having to recompile all procedures */

DEF INPUT-OUTPUT PARAMETER TABLE-HANDLE iop_hTTPDF. /* TTPDF temp-table handle */

DEF VAR pv_roTable AS ROWID EXTENT 10 NO-UNDO. /* so we can move back to the original selected row */

DEF VAR pv_i           AS INT NO-UNDO.           /* general counter */
DEF VAR pv_iNumRecords AS INT NO-UNDO.           /* number of records read */
DEF VAR pv_iLineNumber AS INT NO-UNDO.           /* current line number */
DEF VAR pv_iPageNumber AS INT NO-UNDO.           /* current page number */

DEF VAR pv_hPDF         AS HANDLE NO-UNDO.           /* buffer handle of PDF temp-table */

DEF VAR pv_hQuery       AS HANDLE NO-UNDO.           /* handle to query of browse */

DEF VAR pv_loCalcColumn AS LOGICAL NO-UNDO EXTENT {&MaxColumns}. /* is column a calculated field ?  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-GetTempFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetTempFileName Procedure 
FUNCTION GetTempFileName RETURNS CHARACTER
  ( INPUT ip_cDirectory AS CHAR,
    INPUT ip_cFile      AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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

ASSIGN pv_hPDF = iop_hTTPDF:DEFAULT-BUFFER-HANDLE. /* get handle to buffer of PDF temp-table */

pv_hPDF:FIND-FIRST(). /* get the first record */

IF NOT pv_hPDF:AVAIL THEN RETURN. /* no record was found */

CREATE TTPDF.
BUFFER TTPDF:BUFFER-COPY(pv_hPDF).

ASSIGN pv_hQuery = TTPDF.hBrowse:QUERY /* get handle to query of browse */
       TTPDF.deAltColour[1] = TTPDF.deAltColour[1] / 255  /* convert Red value to PDF requirements */
       TTPDF.deAltColour[2] = TTPDF.deAltColour[2] / 255  /* convert Green value to PDF requirements */
       TTPDF.deAltColour[3] = TTPDF.deAltColour[3] / 255  /* convert Blue value to PDF requirements */
       
       TTPDF.deTitleColour[1] = TTPDF.deTitleColour[1] / 255  /* convert Red value to PDF requirements */
       TTPDF.deTitleColour[2] = TTPDF.deTitleColour[2] / 255  /* convert Green value to PDF requirements */
       TTPDF.deTitleColour[3] = TTPDF.deTitleColour[3] / 255  /* convert Blue value to PDF requirements */
       
       TTPDF.deHeaderColour[1] = TTPDF.deHeaderColour[1] / 255  /* convert Red value to PDF requirements */
       TTPDF.deHeaderColour[2] = TTPDF.deHeaderColour[2] / 255  /* convert Green value to PDF requirements */
       TTPDF.deHeaderColour[3] = TTPDF.deHeaderColour[3] / 255  /* convert Blue value to PDF requirements */
    .

/* need to store the rowids of all buffers in the current query so that we can return to the same result set */
DO pv_i = 1 TO pv_hQuery:NUM-BUFFERS:  /* loop through all buffers of the query, storing the rowid */
 ASSIGN pv_roTable[pv_i] = pv_hQuery:GET-BUFFER-HANDLE(pv_i):ROWID.
END.

RUN VALUE(TTPDF.cType) IN THIS-PROCEDURE. /* run the required report */

RUN PrintReport IN THIS-PROCEDURE. /* print the current report */

/*if requested, delete the resultant file */
IF TTPDF.loDelete THEN OS-DELETE VALUE(TTPDF.cFileName).
                 
pv_hQuery:REPOSITION-TO-ROWID(pv_roTable) NO-ERROR. /* move back to the current row */

APPLY "VALUE-CHANGED":U TO TTPDF.hBrowse. /* refresh the browse */

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-EndOfReport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndOfReport Procedure 
PROCEDURE EndOfReport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 /* Display Footer UnderLine and End of Report Tag (Centered) */
 RUN pdf_skip ("Spdf":U).
 RUN pdf_stroke_fill ("Spdf":U,1.0,1.0,1.0).
 RUN pdf_text_xy ("Spdf":U, "** End of Report **", 250, pdf_TextY("Spdf":U) - 20).
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewPage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewPage Procedure 
PROCEDURE NewPage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR lv_i AS INT NO-UNDO.
  
 pv_iPageNumber = pv_iPageNumber + 1.
 
 RUN pdf_set_font ("Spdf":U,"Courier-Bold",12.0).  
 RUN pdf_skip ("Spdf":U).

 RUN pdf_text_at  ("Spdf":U,TTPDF.cReportTitle,1).
 RUN pdf_skip ("Spdf":U).
 RUN pdf_skip ("Spdf":U).
 
 /* Set Header Font Size and Colour */
 RUN pdf_set_font ("Spdf":U,"Courier-Bold",8.0).  
 RUN pdf_text_color ("Spdf":U,TTPDF.deTitleColour[1],TTPDF.deTitleColour[2],TTPDF.deTitleColour[3]).

 /* Put a Rectangle around the Header */
 RUN pdf_stroke_color ("Spdf":U, .0,.0,.0).
 RUN pdf_stroke_fill ("Spdf":U, .9,.9,.9).

 RUN pdf_rect ("Spdf":U, pdf_LeftMargin("Spdf":U), pdf_TextY("Spdf":U) - 3, pdf_PageWidth("Spdf":U) - 30  , 12, 1.0).

 /* Output Report Header */
 DO lv_i = 1 TO EXTENT(TTPDF.cColumns):
  IF TTPDF.iColumns[lv_i] EQ 0 THEN LEAVE.
  RUN pdf_text_at ("Spdf":U,TTPDF.cColumns[lv_i],TTPDF.iColumns[lv_i]).
 END.

 /* Display Header UnderLine */
 RUN pdf_skip ("Spdf":U).
 RUN pdf_set_dash ("Spdf":U,1,0).
 RUN pdf_line  ("Spdf":U, pdf_LeftMargin("Spdf":U), pdf_TextY("Spdf":U) + 5, pdf_PageWidth("Spdf":U) - 20 , pdf_TextY("Spdf":U) + 5, 2).
 RUN pdf_skip ("Spdf":U).
 
 /* Set Detail Font and Colour */
 RUN pdf_text_color ("Spdf":U,.0,.0,.0).
 RUN pdf_set_font ("Spdf":U,"Courier",8.0).
 
 pv_iLineNumber = 3.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PageFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PageFooter Procedure 
PROCEDURE PageFooter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
 RUN pdf_stroke_color ("Spdf":U, .0,.0,.0).
 RUN pdf_skip ("Spdf":U).
 RUN pdf_set_dash ("Spdf":U,1,0).
 RUN pdf_line  ("Spdf":U, pdf_LeftMargin("Spdf":U), pdf_TextY("Spdf":U) - 5, pdf_PageWidth("Spdf":U) - 20 , pdf_TextY("Spdf":U) - 5, 2).
 RUN pdf_skip ("Spdf":U).
 RUN pdf_skip ("Spdf":U).

 RUN pdf_text_at  ("Spdf":U,SUBSTITUTE("Date: &1 Time: &2  Records:&3 Produced By:&4 ",
                                     STRING(TODAY,"99/99/9999":U),
                                     STRING(TIME,"HH:MM:SS"),
                                     pv_iNumRecords,
                                     TTPDF.cUserCode
                                     ),1).
 
 RUN pdf_text_to  ("Spdf":U,SUBSTITUTE("Page: &1",pv_iPageNumber), INT(pdf_PageWidth("Spdf":U) / pdf_PointSize("Spdf":U))).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PageHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PageHeader Procedure 
PROCEDURE PageHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 DEF VAR lv_i AS INT NO-UNDO.
  
 pv_iPageNumber = pv_iPageNumber + 1.
 
 RUN pdf_set_font ("Spdf":U,"Courier-Bold",12.0).  
 RUN pdf_skip ("Spdf":U).

 RUN pdf_text_at  ("Spdf":U,TTPDF.cReportTitle,1).
 RUN pdf_skip ("Spdf":U).
 RUN pdf_skip ("Spdf":U).
 
 /* Set Header Font Size and Colour */
 RUN pdf_set_font ("Spdf":U,"Courier-Bold",8.0).  
 RUN pdf_text_color ("Spdf":U,1.0,.0,.0).

 /* Put a Rectangle around the Header */
 RUN pdf_stroke_color ("Spdf":U, .0,.0,.0).
 RUN pdf_stroke_fill ("Spdf":U, .9,.9,.9).

 RUN pdf_rect ("Spdf":U, pdf_LeftMargin("Spdf":U), pdf_TextY("Spdf":U) - 3, pdf_PageWidth("Spdf":U) - 30  , 12, 1.0).

 /* Output Report Header */
 DO lv_i = 1 TO EXTENT(TTPDF.cColumns):
  IF TTPDF.iColumns[lv_i] EQ 0 THEN LEAVE.
  RUN pdf_text_at ("Spdf":U,TTPDF.cColumns[lv_i],TTPDF.iColumns[lv_i]).
 END.

 /* Display Header UnderLine */
 RUN pdf_skip ("Spdf":U).
 RUN pdf_set_dash ("Spdf":U,1,0).
 RUN pdf_line  ("Spdf":U, pdf_LeftMargin("Spdf":U), pdf_TextY("Spdf":U) + 5, pdf_PageWidth("Spdf":U) - 20 , pdf_TextY("Spdf":U) + 5, 2).
 RUN pdf_skip ("Spdf":U).
 
 /* Set Detail Font Colour */
 RUN pdf_text_color ("Spdf":U,.0,.0,.0).
 RUN pdf_set_font ("Spdf":U,"Courier",8.0).
 
 pv_iLineNumber = 3.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrintReport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintReport Procedure 
PROCEDURE PrintReport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
 RUN viewxmldialog.w (TTPDF.cFileName,TTPDF.cReportTitle).
                                                     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReportListing) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReportListing Procedure 
PROCEDURE ReportListing :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR lv_iWidth    AS INT NO-UNDO.
 DEF VAR lv_i         AS INT NO-UNDO.

 DEF VAR lv_cResult AS CHAR NO-UNDO.

 /* generate a temp file name if one is not passed */
 IF TTPDF.cFileName EQ "":U OR
    TTPDF.cFileName EQ ?    THEN ASSIGN TTPDF.cFileName = GetTempFileName("":U,SUBSTITUTE("&1.pdf":U,ETIME)).

 /* Create stream for new PDF file */
 RUN pdf_new ("Spdf":U,TTPDF.cFileName).

 /* self explanatory */
 RUN pdf_set_PaperType("Spdf":U,TTPDF.cPaperSize).
 RUN pdf_set_Orientation("Spdf":U,TTPDF.cPageLayout).
 RUN pdf_set_TopMargin ("Spdf":U,10).

 RUN pdf_set_BottomMargin("Spdf":U,50).   /* igc - added Aug 19, 2003 */

 pdf_PageHeader("Spdf",
                THIS-PROCEDURE:HANDLE, 
                "PageHeader"). /* set default header function */
 pdf_PageFooter("Spdf",
                THIS-PROCEDURE:HANDLE,
                "PageFooter"). /* set default footer function */

 ASSIGN lv_iWidth      = 2. /* start at column 2 */

 /* run through all columns of browse 
    get defaults of each column from browse if not specified in TTPDF */
 DO lv_i = 1 TO MIN(EXTENT(TTPDF.cFormat),TTPDF.hBrowse:NUM-COLUMNS):
  ASSIGN TTPDF.hColumns[lv_I]     = IF NOT VALID-HANDLE(TTPDF.hColumns[lv_I])        THEN TTPDF.hBrowse:GET-BROWSE-COLUMN(lv_i) ELSE TTPDF.hColumns[lv_I]
         TTPDF.hBufferField[lv_i] = IF NOT VALID-HANDLE(TTPDF.hBufferField[lv_i])    THEN TTPDF.hColumns[lv_I]:BUFFER-FIELD     ELSE TTPDF.hBufferField[lv_i]
         pv_loCalcColumn[lv_i]    = NOT VALID-HANDLE(TTPDF.hBufferField[lv_i])
         TTPDF.iWidth[lv_i]       = IF TTPDF.iWidth[lv_i]  EQ 0                      THEN TTPDF.hColumns[lv_I]:WIDTH            ELSE TTPDF.iWidth[lv_i]
         TTPDF.iExtent[lv_i]      = IF TTPDF.iExtent[lv_i] EQ 0               AND 
                                       VALID-HANDLE(TTPDF.hBufferField[lv_i]) AND 
                                       TTPDF.hBufferField[lv_i]:EXTENT GT 0          THEN 1 ELSE 0.

  IF pv_loCalcColumn[lv_i] THEN ASSIGN TTPDF.hBufferField[lv_i] = TTPDF.hColumns[lv_I].

  ASSIGN TTPDF.cFormat[lv_i]      = IF TTPDF.cFormat[lv_i] EQ "":U 
                                       THEN IF TTPDF.hBufferField[lv_i]:DATA-TYPE EQ "CHARACTER":U
                                               THEN SUBSTITUTE("x(&1)":U,TTPDF.iWidth[lv_i])
                                               ELSE TTPDF.hBufferField[lv_i]:FORMAT       
                                       ELSE TTPDF.cFormat[lv_i]

         TTPDF.cColumns[lv_i]     = IF TTPDF.cColumns[lv_i] EQ "":U 
                                       THEN IF TTPDF.hBufferField[lv_i]:DATA-TYPE EQ "decimal" 
                                               THEN SUBSTITUTE("&1&2":U,FILL(" ":U,LENGTH(TTPDF.cFormat[lv_i]) - LENGTH(TTPDF.hColumns[lv_I]:LABEL)),TTPDF.hColumns[lv_I]:LABEL)
                                               ELSE TTPDF.hColumns[lv_I]:LABEL
                                       ELSE TTPDF.cColumns[lv_i]

         TTPDF.iColumns[lv_i]     = IF TTPDF.iColumns[lv_i] EQ 0 THEN lv_iWidth ELSE TTPDF.iColumns[lv_i]
         lv_iWidth             = lv_iWidth + TTPDF.hColumns[lv_I]:WIDTH + 2.
 END. /* DO lv_i = 1 TO MIN( */

 /* start at the first record of the query */
 pv_hQuery:GET-FIRST.

 /* Instantiate a New Page */
 RUN pdf_new_page("Spdf":U).

 /* loop through all records in query */ 
 DO WHILE NOT pv_hQuery:QUERY-OFF-END :
  ASSIGN pv_iNumRecords = pv_iNumRecords + 1.

  IF pv_iLineNumber MOD 2 EQ 0 AND TTPDF.loAltColours THEN /* alternate colour */
  DO:
   RUN pdf_stroke_fill ("Spdf":U,TTPDF.deAltColour[1],TTPDF.deAltColour[2],TTPDF.deAltColour[3]).
   RUN pdf_stroke_color ("Spdf":U,TTPDF.deAltColour[1],TTPDF.deAltColour[2],TTPDF.deAltColour[3]). /* light grey is default unless changed by user*/
   RUN pdf_rect ("Spdf":U, pdf_LeftMargin("Spdf":U), pdf_TextY("Spdf":U) - 2, pdf_PageWidth("Spdf":U) - 30  , 9, 1.0).
  END. /* IF pv_iLineNumber MOD 2 EQ 0  */
 
  RUN pdf_set_font ("Spdf":U,"Courier",8.0). /* ensure correct font is used */

  DO lv_i = 1 TO MIN(EXTENT(TTPDF.cColumns),TTPDF.hBrowse:NUM-COLUMNS): /* loop through columns printing data */
   IF TTPDF.cFunction[lv_i] NE "":U THEN /* get value as result of a function call instead of a buffer value */
   DO:
    ASSIGN lv_cResult = DYNAMIC-FUNCTION(TTPDF.cFunction[lv_i] IN TTPDF.hFunctionProcedure,pv_hQuery,pv_iNumRecords,lv_i) NO-ERROR.
    IF lv_cResult EQ ? THEN ASSIGN lv_cResult = "":U.
    RUN pdf_text_at ("Spdf":U,SUBSTR(lv_cResult,1,TTPDF.iWidth[lv_i]),TTPDF.iColumns[lv_i]).
   END. /* IF TTPDF.cFunction[lv_i] NE "":U */

   ELSE 
   DO: /* we do not use this ASSIGN lv_cResult = SUBSTR(STRING(IF NOT pv_loCalcColumn[lv_i] 
                                                                  THEN TTPDF.hBufferField[lv_i]:BUFFER-VALUE(TTPDF.iExtent[lv_i])
                                                                  ELSE TTPDF.hBufferField[lv_i]:SCREEN-VALUE,TTPDF.cFormat[lv_i]),1,TTPDF.iWidth[lv_i]).
          as it automatically casts the :BUFFER-VALUE to be a string rather than the native data type - which buggers up the formatting. Ouch !*/                                                                 
                                         
    IF pv_loCalcColumn[lv_i] 
       THEN ASSIGN lv_cResult = SUBSTR(STRING(TTPDF.hBufferField[lv_i]:SCREEN-VALUE,TTPDF.cFormat[lv_i]),1,TTPDF.iWidth[lv_i]).
       ELSE ASSIGN lv_cResult = SUBSTR(STRING(TTPDF.hBufferField[lv_i]:BUFFER-VALUE(TTPDF.iExtent[lv_i]),TTPDF.cFormat[lv_i]),1,TTPDF.iWidth[lv_i]).

    IF lv_cResult EQ ? THEN ASSIGN lv_cResult = "":U. /* stops the ? from causing a problem with the pdf file */

    RUN pdf_text_at ("Spdf":U,lv_cResult,TTPDF.iColumns[lv_i]). /* print the text */
   END. /* else do */

  END. /*  DO lv_i = 1 TO MIN( */

  RUN pdf_skip ("Spdf":U).  /* new line */
  
  pv_iLineNumber = pv_iLineNumber + 1.

  pv_hQuery:GET-NEXT(). /* get next record */
 END. /*  DO WHILE NOT pv_hQuery:QUERY-OFF-END: */

 RUN EndOfReport IN THIS-PROCEDURE.

 RUN pdf_close("Spdf":U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-GetTempFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetTempFileName Procedure 
FUNCTION GetTempFileName RETURNS CHARACTER
  ( INPUT ip_cDirectory AS CHAR,
    INPUT ip_cFile      AS CHAR) :

 /* get temporary filename */
 
 DEF VAR lv_cTempFile AS CHAR NO-UNDO.

 DEF VAR lv_cFileName AS CHAR NO-UNDO.

 DEF VAR lv_i AS INT NO-UNDO.

 IF ip_cDirectory EQ "":U OR ip_cDirectory EQ ? THEN ASSIGN ip_cDirectory = SESSION:TEMP-DIR.

 /* strip out all problems with the file name */
 ASSIGN lv_cFileName = replace(ip_cFile,"//":U,"/":U)
        lv_cFileName = REPLACE(lv_cFileName,"\":U,"/":U)
        lv_cFileName = replace(lv_cFileName,"[":U,"":U)
        lv_cFileName = replace(lv_cFileName,"]":U,"":U)
        lv_cFileName = replace(lv_cFileName," ":U,"-":U)
        lv_cTempFile = RIGHT-TRIM(ip_cDirectory,"/":U) /* strip off trailing slash if present */
        lv_cTempFile = RIGHT-TRIM(ip_cDirectory,"\":U) /* strip off trailing slash if present */.

 IF NUM-ENTRIES(lv_cFileName,"/":U) EQ 1 THEN ASSIGN lv_cTempFile = SUBSTITUTE("&1/&2":U,lv_cTempFile,lv_cFileName).

 ELSE
 DO lv_i = 1 TO NUM-ENTRIES(lv_cFileName,"/":U) - 1:
  ASSIGN lv_cTempFile = SUBSTITUTE("&1/&2":U,lv_cTempFile,ENTRY(lv_i,lv_cFileName,"/":U)).
  OS-CREATE-DIR VALUE(lv_cTempFile).
 END. 

 IF OPSYS EQ "WIN32":U THEN ASSIGN lv_cTempFile = REPLACE(lv_cTempFile,"/":U,"\":U).

 RETURN lv_cTempFile.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

