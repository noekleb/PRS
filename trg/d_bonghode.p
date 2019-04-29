TRIGGER PROCEDURE FOR DELETE OF BongHode.

FOR EACH BongLinje EXCLUSIVE-LOCK WHERE 
  BongLinje.b_id = BongHode.b_id:
    DELETE BongLinje.
END.
