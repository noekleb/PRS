/******************************************************************************

    Program:        hello.p
    
    Description:    This program illustrates the very basic elements that are 
                    required when using PDFinclude.

******************************************************************************/

{ pdf_inc.i "THIS-PROCEDURE"}

RUN pdf_new ("Spdf","hello-enc.pdf").
RUN pdf_set_parameter ("Spdf","Compress","TRUE").
RUN pdf_set_parameter ("Spdf","Encrypt","TRUE").
RUN pdf_set_parameter ("Spdf","EncryptKey","128").
/*
RUN pdf_set_parameter ("Spdf","MasterPassword","Custom").
RUN pdf_set_parameter ("Spdf","UserPassword","Custom").
*/
RUN pdf_new_page("Spdf").

RUN pdf_text    ("Spdf", "Hello World!").

RUN pdf_close("Spdf").

/* end of hello.pdf */
