/*******************************************************************************

    Program:        column-list.p
    
    Description:    lllustrates the use of Column placement using a Proportional
                    Font.
                    
*******************************************************************************/

{ pdf_inc.i "NOT SUPER"}

DEFINE VARIABLE j# AS INTEGER   NO-UNDO.
DEFINE VARIABLE i# AS CHARACTER NO-UNDO.

/* Create stream for new PDF file */
RUN pdf_new IN h_PDFinc ("Spdf","\gord\PDFinclude\samples\persistent\column-list.pdf").

/* Load Arial Code Font */
RUN pdf_load_font IN h_PDFinc ("Spdf","Arial","c:\windows\fonts\arial.ttf","\gord\pdfinclude\samples\support\arial.afm","").

/* Start a New Page */
RUN pdf_new_page IN h_PDFinc("Spdf").

/* Set the Font to Arial */
RUN pdf_set_font IN h_PDFinc("Spdf","Arial",10.0).

REPEAT j# = 1 TO 5:
   i# = STRING( EXP( 10, j#) ).

   RUN pdf_text_at IN h_PDFinc("Spdf", i#,10).
   RUN pdf_text_at IN h_PDFinc("Spdf", STRING(j#),70).

   RUN pdf_skip IN h_PDFinc("Spdf").
END.

RUN pdf_close IN h_PDFinc("Spdf").


/* end of column-list.p */
