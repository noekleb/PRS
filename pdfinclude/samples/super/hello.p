/******************************************************************************

    Program:        hello.p
    
    Description:    This program illustrates the very basic elements that are 
                    required when using PDFinclude.

******************************************************************************/

{ pdf_inc.i "THIS-PROCEDURE"}

RUN pdf_new ("Spdf","\gord\PDFinclude\samples\super\hello.pdf").
RUN pdf_new_page("Spdf").

RUN pdf_text    ("Spdf", "Hello World!").

RUN pdf_close("Spdf").

/* end of hello.pdf */
