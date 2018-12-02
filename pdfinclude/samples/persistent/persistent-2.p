/******************************************************************************

    Program:        persistent-2.p
    
    Description:    This program uses the Persistent instance of PDFinclude.
    
                    Called from persistent.p

******************************************************************************/
DEFINE INPUT PARAMETER p_Handle AS HANDLE NO-UNDO.

RUN pdf_new IN p_Handle ("Spdf","\gord\PDFinclude\samples\persistent\persistent-2.pdf").
RUN pdf_new_page IN p_Handle ("Spdf").

RUN pdf_text IN p_Handle ("Spdf", "Persistent Two!").

RUN pdf_close IN p_Handle ("Spdf").

/* end of persistent-2.pdf */
