&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Author:  Julian Lyndon-Smith
             Dot R Limited
             Released under possenet licence 
             http://www.possenet.org/legal/license.html
             
    Created     :
    Notes       : 1.3 jmls 
                  Added version number
                  Added comments
                  Added header details
                  Added footer and header fields
    
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE version $Id: pdfprint.i,v 1.1 2003/11/17 16:32:55 jmls Exp $
&SCOPED-DEFINE MaxColumns 10

&IF DEFINED(PdfPrint) EQ 0 &THEN
 DEF TEMP-TABLE TTPDF NO-UNDO
        FIELD cFileName AS CHAR
        FIELD cReportTitle AS CHAR
        FIELD cData AS CHAR EXTENT 250
        FIELD cColumns AS CHAR EXTENT {&MaxColumns}
        FIELD cFormat AS CHAR EXTENT {&MaxColumns}
        FIELD cUserCode AS CHAR
        FIELD iColumns  AS INT EXTENT {&MaxColumns}
        FIELD iWidth  AS INT EXTENT {&MaxColumns}
        FIELD iExtent AS INT EXTENT {&MaxColumns}
        FIELD loPrint AS LOGICAL INIT YES
        FIELD cPageLayout AS CHAR INIT "Portrait"
        FIELD cPaperSize AS CHAR INIT "A4"
        FIELD cType AS CHAR INIT "ReportListing"
        FIELD loDelete AS LOGICAL INIT YES

        FIELD hBufferField AS HANDLE EXTENT {&MaxColumns}
        FIELD hBrowse   AS HANDLE
        FIELD hColumns  AS HANDLE EXTENT {&MaxColumns}
        FIELD cFunction AS CHAR EXTENT {&MaxColumns}
        FIELD hFunctionProcedure AS HANDLE 
        FIELD Version AS DEC INIT 1.3
        FIELD cPageFooter AS CHAR INIT "fnPageFooter":U
        FIELD cPageHeader AS CHAR INIT "fnPageHeader":U
        FIELD deAltColour AS DEC EXTENT 3 INIT [229,229,229]
        FIELD deTitleColour AS DEC EXTENT 3 INIT [0,0,0]
        FIELD deHeaderColour AS DEC EXTENT 3 INIT [255,0,0]
        FIELD loAltColours AS LOGICAL INIT YES
        
        .
         
  CREATE TTPDF.
  ASSIGN TTPDF.hFunctionProcedure = THIS-PROCEDURE:HANDLE.

  &GLOBAL-DEFINE PdfPrint YES
  
  FUNCTION ClearPdf RETURNS LOGICAL:
   FIND FIRST TTPDF.
   DELETE TTPDF.
   CREATE TTPDF.
  END FUNCTION.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


