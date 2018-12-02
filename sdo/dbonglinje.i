  FIELD Antall LIKE BongLinje.Antall VALIDATE ~
  FIELD fuAntall AS INTEGER FORMAT "->>>9" LABEL "Antall"~
  FIELD ArtikkelNr LIKE BongLinje.ArtikkelNr VALIDATE ~
  FIELD BongNr LIKE BongLinje.BongNr VALIDATE ~
  FIELD fuTransTypeTekst AS CHARACTER FORMAT "x(30)" LABEL "TransTypeTekst"~
  FIELD BongPris LIKE BongLinje.BongPris VALIDATE ~
  FIELD BongTekst LIKE BongLinje.BongTekst VALIDATE ~
  FIELD ButikkNr LIKE BongLinje.ButikkNr VALIDATE ~
  FIELD Dato LIKE BongLinje.Dato VALIDATE ~
  FIELD EAv LIKE BongLinje.EAv VALIDATE ~
  FIELD EDato LIKE BongLinje.EDato VALIDATE ~
  FIELD ETid LIKE BongLinje.ETid VALIDATE ~
  FIELD GruppeNr LIKE BongLinje.GruppeNr VALIDATE ~
  FIELD KasseNr LIKE BongLinje.KasseNr VALIDATE ~
  FIELD LinjeNr LIKE BongLinje.LinjeNr VALIDATE ~
  FIELD LinjeRab LIKE BongLinje.LinjeRab VALIDATE ~
  FIELD LinjeSum LIKE BongLinje.LinjeSum VALIDATE ~
  FIELD MButikkNr LIKE BongLinje.MButikkNr VALIDATE ~
  FIELD Mva% LIKE BongLinje.Mva% VALIDATE ~
  FIELD MvaGr LIKE BongLinje.MvaGr VALIDATE ~
  FIELD MvaGruppeNavn LIKE BongLinje.MvaGruppeNavn VALIDATE ~
  FIELD MvaKr LIKE BongLinje.MvaKr VALIDATE ~
  FIELD OAv LIKE BongLinje.OAv VALIDATE ~
  FIELD ODato LIKE BongLinje.ODato VALIDATE ~
  FIELD OriginalData LIKE BongLinje.OriginalData VALIDATE  FORMAT "X(200)"~
  FIELD OTid LIKE BongLinje.OTid VALIDATE ~
  FIELD Storrelse LIKE BongLinje.Storrelse VALIDATE ~
  FIELD SubtotalRab LIKE BongLinje.SubtotalRab VALIDATE ~
  FIELD TBId LIKE BongLinje.TBId VALIDATE  FORMAT ">>9" LABEL "TBId"~
  FIELD TransDato LIKE BongLinje.TransDato VALIDATE ~
  FIELD TransTid LIKE BongLinje.TransTid VALIDATE ~
  FIELD TTId LIKE BongLinje.TTId VALIDATE ~
  FIELD Type LIKE BongLinje.Type VALIDATE ~
  FIELD VareGr LIKE BongLinje.VareGr VALIDATE ~
  FIELD VareGruppeNavn LIKE BongLinje.VareGruppeNavn VALIDATE ~
  FIELD LopeNr LIKE BongLinje.LopeNr VALIDATE  FORMAT ">>>>>9"~
  FIELD FeilKode LIKE BongLinje.FeilKode VALIDATE ~
  FIELD FeilKodeTekst LIKE BongLinje.FeilKodeTekst VALIDATE ~
  FIELD NotatKode LIKE BongLinje.NotatKode VALIDATE  FORMAT ">>9"~
  FIELD NotatKodeTekst LIKE BongLinje.NotatKodeTekst VALIDATE ~
  FIELD Makulert LIKE BongLinje.Makulert VALIDATE  FORMAT "*/"~
  FIELD HovedGr LIKE BongLinje.HovedGr VALIDATE ~
  FIELD HovedGrBeskrivelse LIKE BongLinje.HovedGrBeskrivelse VALIDATE ~
  FIELD ReturButikk LIKE BongLinje.ReturButikk VALIDATE ~
  FIELD ReturKasserer LIKE BongLinje.ReturKasserer VALIDATE ~
  FIELD ReturKassererNavn LIKE BongLinje.ReturKassererNavn VALIDATE ~
  FIELD fuTransKl AS CHARACTER FORMAT "x(8)" LABEL "Kl"~
  FIELD b_id LIKE BongLinje.b_id VALIDATE  FORMAT "->>>>>>>>>>>>>>>>>>>>9"~
  FIELD RefNr LIKE BongLinje.RefNr VALIDATE  FORMAT "->>>>>>>>9"~
  FIELD RefTekst LIKE BongLinje.RefTekst VALIDATE ~
  FIELD SeqNr LIKE BongLinje.SeqNr VALIDATE ~
  FIELD Strekkode LIKE BongLinje.Strekkode VALIDATE  FORMAT "X(30)"~
  FIELD TransNr LIKE BongLinje.TransNr VALIDATE ~
  FIELD VVarekost LIKE BongLinje.VVarekost VALIDATE ~
  FIELD AaaaMmDd LIKE BongLinje.AaaaMmDd VALIDATE ~
  FIELD GenerellRabatt LIKE BongLinje.GenerellRabatt VALIDATE ~
  FIELD KampanjeId LIKE BongLinje.KampanjeId VALIDATE ~
  FIELD KampEierId LIKE BongLinje.KampEierId VALIDATE ~
  FIELD KampId LIKE BongLinje.KampId VALIDATE ~
  FIELD KampTilbId LIKE BongLinje.KampTilbId VALIDATE ~
  FIELD Kunderabatt LIKE BongLinje.Kunderabatt VALIDATE ~
  FIELD LevNavn LIKE BongLinje.LevNavn VALIDATE ~
  FIELD LevNr LIKE BongLinje.LevNr VALIDATE ~
  FIELD Medlemsrabatt LIKE BongLinje.Medlemsrabatt VALIDATE ~
  FIELD OrgVareGr LIKE BongLinje.OrgVareGr VALIDATE ~
  FIELD Personalrabatt LIKE BongLinje.Personalrabatt VALIDATE ~
  FIELD PrisPrSalgsenhet LIKE BongLinje.PrisPrSalgsenhet VALIDATE ~
  FIELD ProduktType LIKE BongLinje.ProduktType VALIDATE ~
  FIELD SalgsType LIKE BongLinje.SalgsType VALIDATE ~
  FIELD SkiftNr LIKE BongLinje.SkiftNr VALIDATE ~
  FIELD ForKonvertering LIKE BongLinje.ForKonvertering VALIDATE 
