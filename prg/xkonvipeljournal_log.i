
/*  TN 8/8-14 Det skal ikke lenger skrives til denne loggen
RUN bibl_logg.p ('xkonvipeljournal', 'xkonvipeljournal.p: ' +  "{1} Art.nr: " +
  STRING(BongLinje.ArtikkelNr) + '; Butikk: ' +
  STRING(BongLinje.ButikkNr) + '; Kasse: ' +
  STRING(BongLinje.KasseNr) + '; Dato: ' +
  STRING(BongLinje.Dato) + '; BongNr: ' +
  STRING(BongLinje.BongNr) + '; Strekkode: ' +
  STRING(BongLinje.Strekkode) + '; Bongtekst: ' +
  STRING(BongLinje.BongTekst) + '; Rab.type: ' +
  STRING(ABSOLUTE(DEC(ENTRY(18 + piLoop,Bonglinje.Originaldata,";")) / 100)) + ';' + STRING(TIME,"HH:MM:SS") + ' Org.data: ' +
  BongLinje.OriginalData).   
-----*/
  
