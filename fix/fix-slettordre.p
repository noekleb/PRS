DEF VAR ocReturn AS CHAR NO-UNDO.
FOR EACH ordre
    WHERE ordre.VareBehNr = 90000003
      AND ordre.RegistrertDato = TODAY
    :
  RUN ordre_delete_all.p ("",STRING(ROWID(ordre)),"",OUTPUT ocReturn).
  IF ocReturn NE "" THEN
    DISP ocReturn FORMAT "x(50)".
END.
