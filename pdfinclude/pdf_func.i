/******************************************************************************

    Program:        pdf_func.i
    
    Written By:     Gordon Campbell - PRO-SYS Consultants Ltd.
    Written On:     January 8, 2004
    
    Description:    Contains function declaration.  Copied from pdf_inc.i
                    on January 8, 2004.
                    
                    This was done so that pdftool.p could also declare the
                    functions without having to use pdf_inc.i.
                    
    Arguments:      {1} - handle to the pdf_inc.p procedure

    History:        
    
    01/08/04  G Campbell    Added Function pdf_get_tool_parameter
                    
    01/21/04  G Campbell    Added function pdf_GetNumFittingChars
                            Added function pdf_text_widthdec

    02/27/04  G Campbell    Changed return value from INTEGER to DECIMAL for
                            functions:
                            
                            pdf_GraphicX
                            pdf_GraphicY
                            
    03/04/04  G Campbell    As per Bruno Van Loon (brunovanloon@hotmail.com)
    
                            Missing function declaration for pdf_get_parameter
                    
    06/11/04  G Campbell    Added function pdf_Font_Loaded
    
    06/15/04  G Campbell    Change pdf_VerticalSpace to return DECIMAL
    
    06/15/04  G Campbell    Added function definition for GetXMLNodeValue
    
    09/27/04  G Campbell    Added function definitions for:
        
                            pdf_text_fontwidth
                            pdf_text_fontwidth2

    09/28/04  J Johnson     Added function pdf_get_pdf_info
                            Different than pdf_get_info
                            pdf_get_info gets a value for the pdf that you
                            are CREATING
                            pdf_get_pdf_info gets a value from a pdf file
                            loaded with pdf_open_pdf.

    10/08/04  G Campbell    When adding pdf_text_fontwidth and 
                            pdf_text_fontwidth2 I mispelled the functions names.
                            I had put 'fonwidth'. 

    3/10/05   G Campbell    Added call definition for pdf_text_widthdec2
    
    7/06/05   G Campbell    Included the Zlib function calls.  We now no
                            longer need the zlib include or procedures.
                            
                            Added PDFencrypt - taken from PDFencrypt.p.
                            
******************************************************************************/

FUNCTION pdf_Font RETURNS CHARACTER ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_FontType RETURNS CHARACTER ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_ImageDim RETURNS INTEGER ( INPUT pdfStream AS CHARACTER,
                                       INPUT pdfImage  AS CHARACTER,
                                       INPUT pdfDim    AS CHARACTER) IN {1}.
FUNCTION pdf_TextX RETURNS INTEGER ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_TextY RETURNS INTEGER ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_VerticalSpace RETURNS DECIMAL ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_PointSize RETURNS DECIMAL ( INPUT pdfStream AS CHARACTER ) IN {1}.
FUNCTION pdf_text_width RETURNS INTEGER ( INPUT pdfStream   AS CHARACTER,
                                          INPUT pdfText     AS CHARACTER) IN {1}.
FUNCTION pdf_text_widthdec RETURNS DECIMAL ( INPUT pdfStream   AS CHARACTER,
                                             INPUT pdfText     AS CHARACTER) IN {1}.
FUNCTION pdf_text_widthdec2 RETURNS DECIMAL ( INPUT pdfStream   AS CHARACTER,
                                              INPUT pdfFontTag  AS CHARACTER, 
                                              INPUT pdfFontSize AS DECIMAL,
                                              INPUT pdfText     AS CHARACTER) IN {1}.
FUNCTION pdf_TextRed RETURN DECIMAL ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_TextGreen RETURN DECIMAL ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_TextBlue RETURN DECIMAL ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_FillRed RETURN DECIMAL ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_FillGreen RETURN DECIMAL ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_FillBlue RETURN DECIMAL ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_Page RETURN INTEGER ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_PageWidth RETURN INTEGER ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_Pageheight RETURN INTEGER ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_PageRotate RETURN INTEGER ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_Angle RETURN INTEGER ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_TopMargin RETURN INTEGER ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_BottomMargin RETURN INTEGER ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_GraphicX RETURN DECIMAL ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_GraphicY RETURN DECIMAL ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_get_info RETURNS CHARACTER ( INPUT pdfStream    AS CHARACTER,
                                          INPUT pdfAttribute AS CHARACTER) IN {1}.
FUNCTION pdf_LeftMargin RETURN INTEGER ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_GetNumFittingChars RETURNS INTEGER 
                                ( INPUT pdfStream   AS CHARACTER,
                                  INPUT pdfText     AS CHARACTER,
                                  INPUT pdfFromX    AS INTEGER,
                                  INPUT pdfToX      AS INTEGER ) IN {1}.
FUNCTION pdf_Orientation RETURN CHARACTER ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_PaperType RETURN CHARACTER ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_Render RETURN INTEGER ( INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_get_wrap_length RETURNS INTEGER ( INPUT pdfStream   AS CHARACTER,
                                               INPUT pdfText AS CHARACTER,
                                               INPUT pdfWidth AS INTEGER ) IN {1}.
FUNCTION pdf_TotalPages RETURN CHARACTER (INPUT pdfStream AS CHARACTER) IN {1}.
FUNCTION pdf_PageFooter RETURN LOGICAL (INPUT pdfStream     AS CHARACTER,
                                        INPUT pdfProcHandle AS HANDLE,
                                        INPUT pdfFooterProc AS CHARACTER) IN {1}.
FUNCTION pdf_PageHeader RETURN LOGICAL (INPUT pdfStream     AS CHARACTER,
                                        INPUT pdfProcHandle AS HANDLE,
                                        INPUT pdfHeaderProc AS CHARACTER) IN {1}.
FUNCTION pdf_LastProcedure RETURN LOGICAL (INPUT pdfStream     AS CHARACTER,
                                           INPUT pdfProcHandle AS HANDLE,
                                           INPUT pdfHeaderProc AS CHARACTER) IN {1}.
FUNCTION pdf_get_tool_parameter RETURNS CHARACTER
        (INPUT  pdfStream      AS CHARACTER,
         INPUT  pdfToolName    AS CHARACTER,
         INPUT  pdfToolParam   AS CHARACTER,
         INPUT  pdfToolCol     AS INTEGER) IN {1}.
FUNCTION pdf_get_parameter RETURNS CHARACTER
         (INPUT pdfStream     AS CHARACTER,
          INPUT pdfParameter  AS CHARACTER) IN {1}.
FUNCTION pdf_Font_Loaded RETURN LOGICAL
        ( INPUT pdfStream AS CHARACTER,
          INPUT pdfFont   AS CHARACTER) IN {1}.
FUNCTION GetXMLNodeValue RETURNS CHARACTER
  (INPUT pParent AS CHARACTER,
   INPUT pNode   AS CHARACTER ) IN {1}.
FUNCTION pdf_text_fontwidth RETURNS DECIMAL
  ( INPUT pdfStream   AS CHARACTER,
    INPUT pdfFont     AS CHARACTER,
    INPUT pdfText     AS CHARACTER) IN {1}.
FUNCTION pdf_text_fontwidth2 RETURNS DECIMAL
  ( INPUT pdfStream   AS CHARACTER,
    INPUT pdfFont     AS CHARACTER,
    INPUT pdfFontSize AS DECIMAL,
    INPUT pdfText     AS CHARACTER) IN {1}.
FUNCTION pdf_get_pdf_info RETURNS CHARACTER
        (pdfSTREAM AS CHARACTER,
         pdfID     AS CHARACTER,
         pInfo     AS CHARACTER) in {1}.

/* --- PRIVATE Functions but definitions are required because they are used in 
       both pdf_inc.p and pdfextract.p */
FUNCTION CompressBuffer RETURNS INTEGER
        (INPUT        InputBuffer  AS MEMPTR,
         INPUT-OUTPUT OutputBuffer AS MEMPTR,
         OUTPUT       OutputSize   AS INTEGER) IN {1}.

FUNCTION DeCompressBuffer RETURNS INTEGER
        (INPUT        InputBuffer  AS MEMPTR,
         OUTPUT       OutputBuffer AS MEMPTR,
         OUTPUT       OutputSize   AS INTEGER) IN {1}.

FUNCTION CompressFile RETURNS LOGICAL
        (INPUT  InputFile  AS CHARACTER,
         INPUT  OutputFile AS CHARACTER) IN {1}.

FUNCTION DeCompressFile RETURNS LOGICAL
        (INPUT  InputFile  AS CHARACTER,
         INPUT  OutputFile AS CHARACTER) IN {1}.

FUNCTION PDFendecrypt RETURNS LOGICAL
        (INPUT BufferPtr    AS MEMPTR,
         INPUT PasswordPtr  AS MEMPTR) IN {1}.
