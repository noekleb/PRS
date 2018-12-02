TRIGGER PROCEDURE FOR CREATE OF ArtBasKarakteristikk.

    assign
      ArtBasKarakteristikk.RegistrertDato = today
      ArtBasKarakteristikk.RegistrertTid  = time
      ArtBasKarakteristikk.BrukerId   = userid("skotex").

