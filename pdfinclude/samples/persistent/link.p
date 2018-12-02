/******************************************************************************

    Program:        link.p
    
    Description:    Illustrates a text object used as a link

******************************************************************************/

{ pdf_inc.i "NOT SUPER"}

RUN pdf_new IN h_PDFinc ("Spdf","\gord\PDFinclude\samples\persistent\link.pdf").
RUN pdf_new_page IN h_PDFinc ("Spdf").

/* Place some text */
RUN pdf_text IN h_PDFinc ("Spdf", FILL("123456789 ",8)). 
RUN pdf_skip IN h_PDFinc ("Spdf").
RUN pdf_text IN h_PDFinc ("Spdf", "Press the big blue rectangle below").
RUN pdf_link IN h_PDFinc ("Spdf", 100,100,300,300,"http://www.epro-sys.com",0.0,0.0,1.0,1,"I").

RUN pdf_close IN h_PDFinc ("Spdf").

/* end of link.p */
