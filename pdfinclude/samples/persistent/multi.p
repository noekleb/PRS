/******************************************************************************

    Program:        multi.p
    
    Description:    This program illustrates the creation of multiple documents
                    within a single procedure.

******************************************************************************/

{ pdf_inc.i "NOT SUPER"}

RUN pdf_new IN h_PDFinc ("Spdf","\gord\PDFinclude\samples\persistent\multiOne.pdf").
RUN pdf_new_page IN h_PDFinc ("Spdf").

RUN pdf_text IN h_PDFinc ("Spdf", "Multiple Document One!").
 
RUN pdf_close IN h_PDFinc ("Spdf").

RUN pdf_new IN h_PDFinc ("Spdf","\gord\PDFinclude\samples\persistent\multiTwo.pdf").
RUN pdf_new_page IN h_PDFinc ("Spdf").

RUN pdf_text IN h_PDFinc ("Spdf", "Multiple Document Two!").

RUN pdf_close IN h_PDFinc ("Spdf").

/* end of multi.p */
