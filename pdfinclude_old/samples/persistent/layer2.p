/******************************************************************************

    Program:        layer2.p
    
    Description:    Called from Layer.p.  This portion outputs the list of 
                    Customer Orders to the PDF document.

                    NOTE: Must be attached to Sports2000 DB to run this
                          example

******************************************************************************/

DEFINE INPUT PARAMETER h_PDFinc   AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p_CustNum  AS INTEGER NO-UNDO.

RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",10.0).

/* For each Customer - display their Orders */
FOR EACH Order WHERE Order.CustNum = p_CustNum NO-LOCK:
  RUN pdf_text IN h_PDFinc ("Spdf", "Order:" + STRING(OrderNum) ).
  RUN pdf_Skip IN h_PDFinc ("Spdf").

END. /* each Order */

/* end of layer2.p */
