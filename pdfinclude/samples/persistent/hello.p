/******************************************************************************

    Program:        hello.p
    
    Description:    This program illustrates the very basic elements that are 
                    required when using PDFinclude.

******************************************************************************/

{ pdf_inc.i "NOT SUPER"}

RUN pdf_new IN h_PDFinc ("Spdf","\gord\PDFinclude\samples\persistent\hello.pdf").
RUN pdf_new_page IN h_PDFinc ("Spdf").

RUN pdf_text IN h_PDFinc ("Spdf", "Hello World!").

RUN pdf_close IN h_PDFinc ("Spdf").

/* end of hello.pdf */
