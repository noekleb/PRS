FOR EACH Datasett EXCLUSIVE-LOCK:
  DataSett.Behandlet = 3.
  FOR EACH BongHode OF DataSEtt:
      ASSIGN
          BongHode.BongStatus = 5
          .
  END.
  FOR EACH Filer OF Datasett:
      ASSIGN
          Filer.Overfort = FALSE
          Filer.OverfortDato = ?
          Filer.OverfortTid = 0
          Filer.Overfortav = ''
          .

  END.
END.

