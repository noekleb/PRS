TRIGGER PROCEDURE FOR DELETE OF PKSDLHode.

FOR EACH PkSdlLinje OF PkSdlHode:
  DELETE PkSdlLinje.
END.
FOR EACH PkSdlPris OF PkSdlHode:
  DELETE PkSdlPris.
END.
FOR EACH PkSdlMottak OF PkSdlHode:
  DELETE PkSdlMottak.
END.

