/******************************************************************************

    Program:        text.p
    
    Description:    Illustrates the many possible text rendering and placement
                    options available via PDFinclude.

******************************************************************************/

{ pdf_inc.i "NOT SUPER"}

RUN pdf_new IN h_PDFinc ("Spdf","\gord\PDFinclude\samples\persistent\text.pdf").
RUN pdf_new_page IN h_PDFinc ("Spdf").

/* Place some text */
RUN pdf_text IN h_PDFinc ("Spdf", FILL("123456789 ",8)).
RUN pdf_skip IN h_PDFinc ("Spdf").
RUN pdf_text_at IN h_PDFinc ("Spdf", "Position AT Column 10",10).
RUN pdf_text_to IN h_PDFinc ("Spdf", "Position TO Column 70",70).
RUN pdf_text IN h_PDFinc ("Spdf", "Text placed at last X/Y Coordinate").
RUN pdf_skip IN h_PDFinc ("Spdf").

/* Change the text color to red */
RUN pdf_text_color IN h_PDFinc ("Spdf",1.0,.0,.0).

RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",14.0).
RUN pdf_text_xy IN h_PDFinc ("Spdf","This is larger text placed using XY coordinates",100,100).
RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",10.0).

/* Change the text color back to black */
RUN pdf_text_color IN h_PDFinc ("Spdf",.0,.0,.0).

/* Change the Rectangle border to red and the fill to white */
RUN pdf_stroke_color IN h_PDFinc ("Spdf",1.0,.0,.0).
RUN pdf_stroke_fill IN h_PDFinc ("Spdf",1.0,1.0,1.0).

/* Display a boxed text string */
RUN pdf_text_boxed_xy  IN h_PDFinc 
                      ("Spdf",
                       "This is BOXED text placed using XY coordinates",
                       100,
                       450,
                       pdf_text_width("Spdf", "This is BOXED text placed using XY coordinates"),
                       10,"Left",1).

RUN pdf_close IN h_PDFinc ("Spdf").

/* end of text.p */
