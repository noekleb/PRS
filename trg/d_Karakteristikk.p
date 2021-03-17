TRIGGER PROCEDURE FOR DELETE OF Karakteristikk.

FOR EACH ArtBasKarakteristikk EXCLUSIVE-LOCK WHERE
    ArtBasKarakteristikk.KarakteristikkId = Karakteristikk.KarakteristikkId:

    DELETE ArtBasKarakteristikk.

END.

