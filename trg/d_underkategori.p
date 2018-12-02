TRIGGER PROCEDURE FOR DELETE OF Underkategori.

FOR EACH ArtBasUnderkategori EXCLUSIVE-LOCK WHERE
    ArtBasUnderkategori.UnderKatNr = Underkategori.UnderKatNr:
    DELETE ArtBasUnderkategori.
END.
