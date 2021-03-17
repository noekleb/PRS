FUNCTION SentrerTekst RETURNS CHARACTER
  ( INPUT wh-Tekst AS WIDGET, INPUT wTekst AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  Sentrerer en tekst
    Notes:  
------------------------------------------------------------------------------*/
  &SCOP Tekst IF wTekst = "" THEN wh-Tekst:SCREEN-VALUE ELSE wTekst
  RETURN
     FILL(" ",INT(  (wh-Tekst:WIDTH-PIXELS
                     - FONT-TABLE:GET-TEXT-WIDTH-PIXELS({&Tekst},wh-Tekst:FONT))
                     / 2
                     / FONT-TABLE:GET-TEXT-WIDTH-PIXELS(" ",wh-Tekst:FONT)
                 ) /* INT */
         ) /* FILL */

         + {&Tekst}.
 
END FUNCTION.

