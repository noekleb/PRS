/************************************************************
    Program:  prishist.i
    Created:  TN   19 Mar 100
Description:

Last change:  TN   26 Apr 100    2:00 pm
************************************************************/

FIND first HPrisKo NO-LOCK where
  HPrisKo.ArtikkelNr = LokArtBas.ArtikkelNr and
  HPrisKo.ProfilNr   = FI-ProfilNr NO-ERROR.
IF AVAILABLE HPrisKo then
  wEndringsNr = HPrisKo.EndringsNr + 1.
ELSE
  wEndringsNr = 1.

CREATE HPrisKo.
ASSIGN
  HPrisKo.ArtikkelNr     = LokArtBas.ArtikkelNr
  HPrisKo.ProfilNr       = FI-ProfilNr
  HPrisKo.EndringsNr     = wEndringsNr.
assign
  HPrisKo.LevNr          = LokArtBas.LevNr
  HPrisKo.ValPris        = LokArtPris.ValPris[{&PrisIndex}]
  HPrisKo.InnkjopsPris   = LokArtPris.InnKjopsPris[{&PrisIndex}]
  HPrisKo.Rab1Kr         = LokArtPris.Rab1Kr[{&PrisIndex}]
  HPrisKo.Rab1%          = LokArtPris.Rab1%[{&PrisIndex}]
  HPrisKo.Rab2Kr         = LokArtPris.Rab2Kr[{&PrisIndex}]
  HPrisKo.Rab2%          = LokArtPris.Rab2%[{&PrisIndex}]
  HPrisKo.Frakt          = LokArtPris.Frakt[{&PrisIndex}]
  HPrisKo.Frakt%         = LokArtPris.Frakt%[{&PrisIndex}]
  HPrisKo.DivKostKr      = LokArtPris.DivKostKr[{&PrisIndex}]
  HPrisKo.DivKost%       = LokArtPris.DivKost%[{&PrisIndex}]
  HPrisKo.Rab3Kr         = LokArtPris.Rab3Kr[{&PrisIndex}]
  HPrisKo.Rab3%          = LokArtPris.Rab3%[{&PrisIndex}]
  HPrisKo.DBKr           = LokArtPris.DBKr[{&PrisIndex}]
  HPrisKo.DB%            = LokArtPris.DB%[{&PrisIndex}]
  HPrisKo.Pris           = LokArtPris.Pris[{&PrisIndex}]
  HPrisKo.EuroPris       = LokArtPris.EuroPris[{&PrisIndex}]
  HPrisKo.EuroManuel     = LokArtPris.EuroManuel.

ASSIGN
  HPrisKo.Tilbud         = LokArtPris.Tilbud
  HPrisKo.AktiveresDato  = if wTilbud
                             THEN LokArtPris.TilbudFraDato
                             ELSE LokArtPris.AktivFraDato
  HPrisKo.GyldigTilDato  = if wTilbud
                             THEN LokArtPris.TilbudTilDato
                             ELSE ?
  HPrisKo.AktiveresTid   = if wTilbud
                             then LokArtPris.TilbudFraTid
                             else LokArtPris.AktivFraTid
  HPrisKo.GyldigTilTid   = if wTilbud
                             THEN LokArtPris.TilbudTilTid
                             ELSE 0
  HPrisKo.Timestyrt      = LokArtPris.TilbudTimeStyrt

  HPrisKo.Aktivert       = true
  HPrisKo.Type           = {&wTilbud}
  HPrisKo.VareKost       = LokArtPris.VareKost[{&PrisIndex}]
  HPrisKo.MvaKr          = LokArtPris.MvaKr[{&PrisIndex}]
  HPrisKo.Mva%           = LokArtPris.Mva%[{&PrisIndex}]

  HPrisKo.EDato          = today
  HPrisKo.ETid           = time
  HPrisKo.BrukerID       = USERID("dictdb")

  HPrisKo.RegistrertDato = today
  HPrisKo.RegistrertTid  = time
  HPrisKo.RegistrertAv   = USERID("dictdb").


