/************************************************************
    Program:  prishist.i
    Created:  TN   19 Mar 100
Description:

Last change:  TN   26 Apr 100    2:00 pm
************************************************************/

FIND FIRST HPrisKo NO-LOCK WHERE
  HPrisKo.ArtikkelNr = LokArtBas.ArtikkelNr AND
  HPrisKo.ProfilNr   = FI-ProfilNr NO-ERROR.
IF AVAILABLE HPrisKo THEN
  wEndringsNr = HPrisKo.EndringsNr + 1.
ELSE
  wEndringsNr = 1.

CREATE HPrisKo.
ASSIGN
  HPrisKo.ArtikkelNr     = LokArtBas.ArtikkelNr
  HPrisKo.ProfilNr       = FI-ProfilNr
  HPrisKo.EndringsNr     = wEndringsNr.
ASSIGN
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
  HPrisKo.AktiveresDato  = IF wTilbud
                             THEN LokArtPris.TilbudFraDato
                             ELSE LokArtPris.AktivFraDato
  HPrisKo.GyldigTilDato  = IF wTilbud
                             THEN LokArtPris.TilbudTilDato
                             ELSE ?
  HPrisKo.AktiveresTid   = IF wTilbud
                             THEN LokArtPris.TilbudFraTid
                             ELSE LokArtPris.AktivFraTid
  HPrisKo.AktiveresTid   = IF (HPrisKo.AktiveresTid = 0 OR HPrisKo.AktiveresTid = ?) THEN TIME ELSE HPrisKo.AktiveresTid
  HPrisKo.GyldigTilTid   = IF wTilbud
                             THEN LokArtPris.TilbudTilTid
                             ELSE 0
  HPrisKo.Timestyrt      = LokArtPris.TilbudTimeStyrt

  HPrisKo.Aktivert       = TRUE
  HPrisKo.Type           = {&wTilbud}
  HPrisKo.VareKost       = LokArtPris.VareKost[{&PrisIndex}]
  HPrisKo.MvaKr          = LokArtPris.MvaKr[{&PrisIndex}]
  HPrisKo.Mva%           = LokArtPris.Mva%[{&PrisIndex}]

  HPrisKo.EDato          = TODAY
  HPrisKo.ETid           = TIME
  HPrisKo.BrukerID       = USERID("dictdb")

  HPrisKo.RegistrertDato = TODAY
  HPrisKo.RegistrertTid  = TIME
  HPrisKo.RegistrertAv   = USERID("dictdb").
IF AVAILABLE HPrisKo THEN 
  RELEASE hPrisko.

