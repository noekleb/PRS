FUNCTION DialogBoksAktiv RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Finner ut om en dialogboks er aktiv eller ikke. 
    Notes:  
------------------------------------------------------------------------------*/
   DEF VAR wh         AS WIDGET NO-UNDO.
   DEF VAR wDialogBox AS LOGI   NO-UNDO.
    
   IF VALID-HANDLE(FOCUS) THEN DO:

      ASSIGN wh = FOCUS:PARENT.
   
      DO WHILE VALID-HANDLE(wh):
         IF wh:TYPE = "DIALOG-BOX" THEN RETURN YES.
         ASSIGN wh = wh:PARENT.
      END.
   END.

   RETURN NO.

END FUNCTION.
