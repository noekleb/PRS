/******************************************************************************

    Program:        persistent.p
    
    Description:    This program illustrates how you can run PDFinclude as a
                    Persistent Procedure. This allows you to define 
                    PDFinclude only once and allows you to reuse the functions
                    in many different programs.
                    
******************************************************************************/

{ pdf_inc.i "NOT SUPER"}

RUN persistent-1.p (INPUT h_PDFinc).
RUN persistent-2.p (INPUT h_PDFinc).

IF VALID-HANDLE(h_PDFinc) THEN
  DELETE PROCEDURE h_PDFinc.

/* end of persistent.pdf */
