/******************************************************************************

    Program:        rotatetext.p
    
    Description:    Illustrates the rotation of text available via PDFinclude.

******************************************************************************/

{ pdf_inc.i "NOT SUPER"}

RUN pdf_new IN h_PDFinc ("Spdf","\gord\PDFinclude\samples\persistent\rotatetext.pdf").
RUN pdf_new_page IN h_PDFinc ("Spdf").

/* Place some text */
RUN pdf_text IN h_PDFinc ("Spdf", FILL("HORIZONTAL ",2)).

RUN pdf_text_rotate IN h_PDFinc ("Spdf",90).
RUN pdf_text IN h_PDFinc ("Spdf", FILL("VERTICAL ",2)).

RUN pdf_text_rotate IN h_PDFinc ("Spdf",0).
RUN pdf_text IN h_PDFinc ("Spdf", FILL("HORIZON ",2)).

RUN pdf_text_rotate IN h_PDFinc ("Spdf",270).
RUN pdf_text IN h_PDFinc ("Spdf", FILL("VERT ",2)).

RUN pdf_text_rotate IN h_PDFinc ("Spdf",0).
RUN pdf_set_font IN h_PDFinc ("Spdf",pdf_Font("Spdf"),12).
RUN pdf_text_color IN h_PDFinc ("Spdf",1,0,0).
RUN pdf_text IN h_PDFinc ("Spdf", "SNAKEHEAD----8~~ ").

RUN pdf_close IN h_PDFinc ("Spdf").

/* end of rotatetext.p */
