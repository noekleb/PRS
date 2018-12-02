/******************************************************************************

    Program:        persistent-1.p
    
    Description:    This program uses the Persistent instance of PDFinclude.
    
                    Called from persistent.p

******************************************************************************/
DEFINE INPUT PARAMETER p_Handle AS HANDLE NO-UNDO.

RUN pdf_new IN p_Handle ("Spdf","\gord\PDFinclude\samples\persistent\persistent-1.pdf").
RUN pdf_new_page IN p_Handle ("Spdf").

RUN pdf_text IN p_Handle ("Spdf", "Persistent One!").

RUN pdf_close IN p_Handle ("Spdf").

/* end of persistent-1.pdf */
