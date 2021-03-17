TRIGGER PROCEDURE FOR DELETE OF FakturaHode.

FOR EACH FakturaLinje OF FakturaHode:
  DELETE FakturaLinje.
END.

