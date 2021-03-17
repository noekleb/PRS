/************************************************************
    Program:  kalkyle.i
    Created:  TN   12 Nov 98
Description:  Rutiner og funksjoner for håndtering av kalkyle.


	Last change:  TN    1 May 100   10:17 am
************************************************************/

DEF buffer bufArtBas FOR ArtBas.
DEF buffer bufArtLag FOR ArtLag.
DEF BUFFER bufPrisKo FOR PrisKo.

/* Kalkulasjonsvariabler. */
def var FI-Pris      as DEC NO-UNDO.
def var FI-Mva%      as DEC NO-UNDO.
DEF VAR wWork        as DEC NO-UNDO.

def var FI-Mva       as DEC NO-UNDO.
DEF VAR wKalkModus   as INT NO-UNDO.
def var FI-ValPris   as DEC NO-UNDO.
def var FI-InnPris   as DEC NO-UNDO.
def var FI-Rab1      as DEC NO-UNDO.
def var FI-Rab1%     as DEC NO-UNDO.
def var FI-Rab2      as DEC NO-UNDO.
def var FI-Rab2%     as DEC NO-UNDO.
def var FI-Frakt     as DEC NO-UNDO.
def var FI-Frakt%    as DEC NO-UNDO.
def var FI-DivKost   as DEC NO-UNDO.
def var FI-DivKost%  as DEC NO-UNDO.
def var FI-Rab3      as DEC NO-UNDO.
def var FI-Rab3%     as DEC NO-UNDO.
def var FI-VareKost  as DEC NO-UNDO.
def var FI-DB        as DEC NO-UNDO.
def var FI-DB%       as DEC NO-UNDO.
def var FI-EUPris    as DEC NO-UNDO.
def var FI-EuManuel  as log NO-UNDO.
def var FI-Valkurs   as DEC NO-UNDO.

/*
FUNCTION InnPris RETURNS dec ().
    RETURN (FI-ValPris * FI-ValKurs).
END FUNCTION.   

FUNCTION VareKost RETURNS dec ().
    RETURN (FI-InnPris -
            FI-Rab1    -
            FI-Rab2    +
            FI-Frakt   +
            FI-DivKost -
            FI-Rab3).
END FUNCTION.   

FUNCTION Pris RETURNS dec ().
    RETURN (
            (FI-VareKost + FI-DB + FI-MVA)
           ).
END FUNCTION.   

FUNCTION Mva RETURNS dec ().
    RETURN (
            ((FI-VareKost + FI-DB) * FI-Mva%) / 100
           ).
END FUNCTION.
*/

FUNCTION Mva2 RETURNS dec ().
    wWork = FI-Pris - (FI-Pris / (1 + (FI-Mva% / 100))).
    if wWork = ? THEN wWork = 0.

    RETURN wWork.
END FUNCTION.

/*
FUNCTION EuroPris RETURNS dec ().
    RETURN (FI-Pris * FI-EuroKurs).
END FUNCTION.   

FUNCTION Rab1% RETURNS dec ().
    wWork = (FI-Rab1 * 100) / FI-InnPris.
    if wWork = ? THEN wWork = 0.

    RETURN wWork.
END FUNCTION.   

FUNCTION Rab1 RETURNS dec ().
    RETURN (
            (FI-Innpris * FI-Rab1%) / 100
           ).
END FUNCTION.   

FUNCTION Rab2% RETURNS dec ().
    wWork = (FI-Rab2 * 100) / (FI-InnPris - FI-Rab1).
    if wWork = ? THEN wWork = 0.

    RETURN wWork.
END FUNCTION.

FUNCTION Rab2 RETURNS dec ().
    RETURN (
            ((FI-Innpris - FI-Rab1) * FI-Rab2%) / 100
           ).
END FUNCTION.   

FUNCTION Frakt% RETURNS dec ().
    wWork = (FI-Frakt * 100) / (FI-InnPris - FI-Rab1 - FI-Rab2).
    if wWork = ? THEN wWork = 0.

    RETURN wWork.
END FUNCTION.

FUNCTION Frakt RETURNS dec ().
    RETURN (
            ((FI-Innpris - FI-Rab1 - FI-Rab2) * FI-Frakt%) / 100
           ).
END FUNCTION.   

FUNCTION DivKost% RETURNS dec ().
    wWork = (FI-DivKost * 100) / (FI-InnPris - FI-Rab1 - FI-Rab2 + FI-Frakt).
    if wWork = ? THEN wWork = 0.

    RETURN wWork.
END FUNCTION.

FUNCTION DivKost RETURNS dec ().
    RETURN (
            ((FI-Innpris - FI-Rab1 - FI-Rab2 + FI-Frakt) * FI-DivKost%) / 100
           ).
END FUNCTION.   

FUNCTION Rab3% RETURNS dec ().
    wWork = (FI-Rab3 * 100) / (FI-InnPris - FI-Rab1 - FI-Rab2 + FI-Frakt + FI-DivKost).
    if wWork = ? THEN wWork = 0.

    RETURN wWork.
END FUNCTION.

FUNCTION Rab3 RETURNS dec ().
    wWork = ((FI-Innpris - FI-Rab1 - FI-Rab2 + FI-Frakt + FI-DivKost) * FI-Rab3%) / 100.
    if wWork = ? THEN wWork = 0.

    RETURN wWork.
END FUNCTION.

FUNCTION DB% RETURNS dec ().
    wWork = ROUND((FI-Db * 100) / (FI-VareKost + FI-DB),2).
    if wWork = ? THEN wWork = 0.

    RETURN wWork.
END FUNCTION.

FUNCTION DB1 RETURNS dec ().
    wWork = (FI-Pris - FI-Mva - FI-VareKost).
    if wWork = ? THEN wWork = 0.

    RETURN wWork.
END FUNCTION.

FUNCTION DB RETURNS dec ().
    wWork = (FI-VareKost / (1 - (FI-DB% / 100))) - FI-VareKost.
    if wWork = ? THEN wWork = 0.

    RETURN wWork.
END FUNCTION.

FUNCTION DB2 RETURNS dec ().
    RETURN (
            (FI-Pris - FI-VareKost - FI-Mva)
           ).
END FUNCTION.
*/

/* Kalkulerer mva et beløp. */
PROCEDURE MvaKalk:
  DEF INPUT  PARAMETER wMva%  as DEC NO-UNDO.
  def INPUT  PARAMETER wBelop as DEC NO-UNDO.
  def OUTPUT PARAMETER wMvaKr as DEC NO-UNDO.

  assign
    FI-Mva% = wMva%
    FI-Pris = wBelop
    wMvaKr  = Mva2().

END PROCEDURE.

/*
/* initierer variablene for kalkylen. */
PROCEDURE InitKalkyle:
  /* Interface */
  DEF INPUT        PARAMETER wArtBasRecid as RECID NO-UNDO.
  DEF INPUT        PARAMETER FI-ProfilNr  as int   NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER wSkjerm      as CHAR  NO-UNDO.
  DEF INPUT        PARAMETER wMomsProc    as DEC   NO-UNDO.
  DEF INPUT        PARAMETER wValKurs     as DEC   NO-UNDO.
  DEF INPUT        PARAMETER wFeltNr      as INT   NO-UNDO.
  DEF INPUT        PARAMETER wTilbud      as LOG   NO-UNDO.

  DEF VAR wPrisIndex    as INT   NO-UNDO.
  DEF VAR wArtPrisRecid as RECID NO-UNDO.

  DEF BUFFER LokArtPris FOR ArtPris.
  DEF BUFFER LokArtBas  FOR ArtBas.

  /* Strong scoope */
  do FOR LokArtPris, LokArtBas:

  assign
    wPrisIndex = IF wTilbud THEN 2 ELSE 1.

  /* Henter artikkelen. */
  FIND LokArtBas NO-LOCK where
    RECID(LokArtBas) = wArtBasRecid NO-ERROR.

  /* Henter eller oppretter ArtPris for gjeldende prisprofil. */
  FIND LokArtPris of LokArtBas NO-LOCK where
    LokArtPris.ProfilNr = FI-ProfilNr NO-ERROR.
  IF NOT AVAILABLE LokArtPris THEN
    DO:
      RUN HentArtPris (input LokArtBas.ArtikkelNr, input FI-ProfilNr, input wTilbud, output wArtPrisRecid).
      FIND LokArtPris NO-LOCK where
        RECID(LokArtPris) = wArtPrisRecid NO-ERROR.
    END.

  assign
    wSkjerm = string(LokArtPris.ValPris[wPrisIndex]) + ";" +
              string(LokArtPris.InnKjopsPris[wPrisIndex]) + ";" +
              string(LokArtPris.Rab1Kr[wPrisIndex]) + ";" +
              string(LokArtPris.Rab1%[wPrisIndex]) + ";" +
              string(LokArtPris.Rab2Kr[wPrisIndex]) + ";" +
              string(LokArtPris.Rab2%[wPrisIndex]) + ";" +
              string(LokArtPris.Frakt[wPrisIndex]) + ";" +
              string(LokArtPris.Frakt%[wPrisIndex]) + ";" +
              string(LokArtPris.DivKostKr[wPrisIndex]) + ";" +
              string(LokArtPris.DivKost%[wPrisIndex]) + ";" +
              string(LokArtPris.Rab3Kr[wPrisIndex]) + ";" +
              string(LokArtPris.Rab3%[wPrisIndex]) + ";" +
              string(LokArtPris.VareKost[wPrisIndex]) + ";" +
              string(LokArtPris.MvaKr[wPrisIndex]) + ";" +
              string(LokArtPris.Mva%[wPrisIndex]) + ";" +
              string(LokArtPris.DBKr[wPrisIndex]) + ";" +
              string(LokArtPris.DB%[wPrisIndex]) + ";" +
              string(LokArtPris.Pris[wPrisIndex]) + ";" +
              string(LokArtPris.EuroPris[wPrisIndex]) + ";" +
              STRING(LokArtPris.EuroManuel) + ";".
  IF LokArtPris.Tilbud = false then
    wSkjerm = wSkjerm +
              (if LokArtPris.AktivFraDato <> ?
                 then STRING(LokArtPris.AktivFraDato)
                 else "") + ";" +
              "0;;;0;0;".
  ELSE
    wSkjerm = wSkjerm +
              ";0;" +
              (if LokArtPris.TilbudFraDato <> ?
                 then STRING(LokArtPris.TilbudFraDato)
                 ELSE "") + ";" +                     /* 23 */
              STRING(LokArtPris.TilbudFraTid) + ";" +    /* 25 */
              (if LokArtPris.TilbudTilDato <> ?
                 then STRING(LokArtPris.TilbudTilDato)
                 ELSE "") + ";" +                     /* 24 */
              STRING(LokArtPris.TilbudTilTid) + ";".    /* 26 */
   assign
     wSkjerm = wSkjerm +
              STRING(LokArtPris.TilbudTimeStyrt).        /* 27 */
  END. /* Strong scoope */
END PROCEDURE.

PROCEDURE Omregning:
  /* Interface */
  DEF INPUT        PARAMETER wArtBasRecid as RECID NO-UNDO.
  DEF INPUT        PARAMETER FI-ProfilNr  as int   NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER wSkjerm      as CHAR  NO-UNDO.
  DEF INPUT        PARAMETER wMomsProc    as DEC   NO-UNDO.
  DEF INPUT        PARAMETER wValKurs     as DEC   NO-UNDO.
  DEF INPUT        PARAMETER wFeltNr      as INT   NO-UNDO.
  DEF INPUT        PARAMETER wTilbud      as LOG   NO-UNDO.

  /* Leser av kalkulasjonsmodusen */
  wKalkModus = 0.
  {syspara.i 2 1 2 wKalkModus INT}
  if wKalkModus = 0 then
    wKalkModus = 1. /* Default er utpris fast. */

  assign
    FI-ValKurs  = wValKurs
    FI-Mva%     = wMomsProc.

  /* Initierer variablene */
  assign
    FI-ValPris  = DEC(entry(1,wSkjerm,";"))
    FI-InnPris  = DEC(entry(2,wSkjerm,";"))
    FI-Rab1     = DEC(entry(3,wSkjerm,";"))
    FI-Rab1%    = DEC(entry(4,wSkjerm,";"))
    FI-Rab2     = DEC(entry(5,wSkjerm,";"))
    FI-Rab2%    = DEC(entry(6,wSkjerm,";"))
    FI-Frakt    = DEC(entry(7,wSkjerm,";"))
    FI-Frakt%   = DEC(entry(8,wSkjerm,";"))
    FI-DivKost  = DEC(entry(9,wSkjerm,";"))
    FI-DivKost% = DEC(entry(10,wSkjerm,";"))
    FI-Rab3     = DEC(entry(11,wSkjerm,";"))
    FI-Rab3%    = DEC(entry(12,wSkjerm,";"))
    FI-VareKost = DEC(entry(13,wSkjerm,";"))
    FI-Mva      = DEC(entry(14,wSkjerm,";"))
    /* FI-Mva%     = DEC(entry(15,wSkjerm,";")) */
    FI-DB       = DEC(entry(16,wSkjerm,";"))
    FI-DB%      = DEC(entry(17,wSkjerm,";"))
    FI-Pris     = DEC(entry(18,wSkjerm,";"))
    FI-EUPris   = DEC(entry(19,wSkjerm,";"))
    FI-EuManuel = if CAN-DO("True,Yes",entry(20,wSkjerm,";"))
                    then true
                    else false.

  /* Kalkulasjon av feltene. */
  /* KALKULASJON             */
  CASE wFeltNr:
    WHEN 1 THEN /* ValutaPris */
      DO:
        /* Regne ut Innkj›pspris ut fra valutapris. */
        /* Korrigerer deretter påvirkerde verdier.  */
        assign
          FI-InnPris  = Innpris()
          FI-Rab1%    = Rab1%()
          FI-Rab2%    = Rab2%()    /* Regnes om! */
          FI-Frakt%   = Frakt%()   /* Regnes om! */
          FI-DivKost% = DivKost%() /* Regnes om! */
          FI-Rab3%    = Rab3%()    /* Regnes om! */
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 3 then /* Rabatt 1 Kr */
      DO:
        assign
          FI-Rab1%    = Rab1%()
          FI-Rab2%    = Rab2%()    /* Regnes om! */
          FI-Frakt%   = Frakt%()   /* Regnes om! */
          FI-DivKost% = DivKost%() /* Regnes om! */
          FI-Rab3%    = Rab3%()    /* Regnes om! */
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 4 then /* Rabatt 1 % */
      DO:
        assign
          FI-Rab1     = Rab1()
          FI-Rab2%    = Rab2%()    /* Regnes om! */
          FI-Frakt%   = Frakt%()   /* Regnes om! */
          FI-DivKost% = DivKost%() /* Regnes om! */
          FI-Rab3%    = Rab3%()    /* Regnes om! */
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 5 then /* Rabatt 2 Kr */
      DO:
        assign
          FI-Rab2%    = Rab2%()
          FI-Frakt%   = Frakt%()   /* Regnes om! */
          FI-DivKost% = DivKost%() /* Regnes om! */
          FI-Rab3%    = Rab3%()    /* Regnes om! */
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 6 then /* Rabatt 2 % */
      DO:
        assign
          FI-Rab2     = Rab2()
          FI-Frakt%   = Frakt%()   /* Regnes om! */
          FI-DivKost% = DivKost%() /* Regnes om! */
          FI-Rab3%    = Rab3%()    /* Regnes om! */
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 7 then /* Frakt Kr */
      DO:
        assign
          FI-Frakt%   = Frakt%()
          FI-DivKost% = DivKost%() /* Regnes om! */
          FI-Rab3%    = Rab3%()    /* Regnes om! */
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 8 then /* Frakt % */
      DO:
        assign
          FI-Frakt    = Frakt()
          FI-DivKost% = DivKost%() /* Regnes om! */
          FI-Rab3%    = Rab3%()    /* Regnes om! */
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 9 then /* DivKostKr */
      DO:
        assign
          FI-DivKost% = DivKost%()
          FI-Rab3%    = Rab3%()    /* Regnes om! */
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 10 then /* DivKost % */
      DO:
        assign
          FI-DivKost  = DivKost()
          FI-Rab3%    = Rab3%()    /* Regnes om! */
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 11 then /* Rabatt 3 Kr */
      DO:
        assign
          FI-Rab3%    = Rab3%()
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 12 then /* Rabatt 3 % */
      DO:
        assign
          FI-Rab3     = Rab3()
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 14 then /* DB Kr */
      DO:
        assign
          FI-DB%      = DB%()
          FI-Mva      = Mva()
          FI-Pris     = Pris().
        if FI-EuManuel = FALSE then
          assign
            FI-EUPris = EuroPris().
      END.

    WHEN 15 then /* DB % */
      DO:
        assign
          FI-DB       = DB()
          FI-Mva      = Mva()
          FI-Pris     = Pris().
        if FI-EuManuel = FALSE then
          assign
            FI-EUPris = EuroPris().
      END.

    WHEN 18 then /* Pris */
      DO:
        assign
          FI-Mva      = Mva2()
          FI-DB       = DB2()
          FI-DB%      = DB%().
        if FI-EuManuel = FALSE then
          assign
            FI-EUPris = EuroPris().
      END.

  END case. /* KALKULASJON */

  /* Pakker returstreng */
  assign
    wSkjerm = string(FI-ValPris) + ";" +
              string(FI-InnPris) + ";" +
              string(FI-Rab1) + ";" +
              string(FI-Rab1%) + ";" +
              string(FI-Rab2) + ";" +
              string(FI-Rab2%) + ";" +
              string(FI-Frakt) + ";" +
              string(FI-Frakt%) + ";" +
              string(FI-DivKost) + ";" +
              string(FI-DivKost%) + ";" +
              string(FI-Rab3) + ";" +
              string(FI-Rab3%) + ";" +
              string(FI-VareKost) + ";" +
              string(FI-Mva) + ";" +
              string(FI-Mva%) + ";" +
              string(FI-DB) + ";" +
              string(FI-DB%) + ";" +
              string(FI-Pris) + ";" +
              string(FI-EUPris).
END PROCEDURE.

/* Opprettelse av ArtPris */
PROCEDURE HentArtPris:
  DEF INPUT  PARAMETER wArtikkelNr  like ArtBas.ArtikkelNr NO-UNDO.
  DEF INPUT  PARAMETER FI-ProfilNr   AS INT   NO-UNDO.
  DEF INPUT  PARAMETER wTilbud       AS LOG   NO-UNDO.
  DEF OUTPUT PARAMETER wArtPrisRecid AS RECID NO-UNDO.

  DEF BUFFER LokArtPris FOR ArtPris.

  /* Strong scoope */
  DO FOR LokArtPris:

  FIND LokArtPris NO-LOCK where
    LokArtPris.ArtikkelNr = wArtikkelNr and
    LokArtPris.ProfilNr   = FI-ProfilNr NO-ERROR.

  if AVAILABLE LokArtPris then
    wArtPrisRecid = RECID(LokArtPris).
  else
    do TRANSACTION:

      CREATE LokArtPris.

      /* Nökkelfelt */
      assign
        LokArtPris.ArtikkelNr = wArtikkelNr
        LokArtPris.ProfilNr   = FI-ProfilNr
        wArtPrisRecid      = RECID(LokArtPris).

      /* Øvrig informasjon. */
      assign
        LokArtPris.Tilbud   = wTilbud
        wArtPrisRecid    = RECID(LokArtPris).

      RELEASE LokArtPris.
    END. /* TRANSACTION */

  /* Reduserer til no-lock på recorden. */
  FIND LokArtPris NO-LOCK where
    RECID(LokArtPris) = wArtPrisRecid.

  END. /* Strong scoope */
END PROCEDURE.

/* Lagre ArtPris */
PROCEDURE LagreArtPris:
  DEF INPUT        PARAMETER wArtBasRecid as RECID NO-UNDO.
  DEF INPUT        PARAMETER FI-ProfilNr  as int   NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER wSkjerm      as CHAR  NO-UNDO.
  DEF INPUT        PARAMETER wTilbud      as LOG   NO-UNDO.
  DEF INPUT        PARAMETER wDirekte     as LOG   NO-UNDO.
  DEF INPUT        PARAMETER wPrisKo      as LOG   NO-UNDO.

  DEF VAR wStatus       as CHAR INITIAL "AVBRYT, ERROR" NO-UNDO.
  DEF VAR wPrisIndex    as INT                          NO-UNDO.
  DEF VAR wArtPrisRecid as RECID                        NO-UNDO.

  DEF BUFFER LokArtPris FOR ArtPris.
  DEF BUFFER LokArtBas  FOR ArtBas.
  DEF BUFFER LokArtLag  FOR ArtLag.
  DEF BUFFER LokPrisKo  FOR PrisKo.
  DEF BUFFER buf2ArtLag FOR ArtLag.
  DEF BUFFER buf2ArtBas FOR ArtBas.
  DEF BUFFER buf2PrisKo FOR PrisKo.

/*
MESSAGE "Gurre i p-biblo.p"  wSkjerm skip
wTilbud skip
wDirekte skip
wPrisKo skip
VIEW-AS ALERT-BOX.
*/

  assign
    wPrisIndex = IF wTilbud THEN 2 ELSE 1.

  /* Strong scoope */
  /* do FOR LokArtBas, LokArtPris, LokArtLag, LokPrisKo, buf2ArtLag, buf2ArtBas, buf2PrisKo: */
  do FOR LokArtBas, LokArtPris, LokPrisKo, buf2ArtBas, buf2PrisKo:

  /* Henter artikkelen. */
  FIND LokArtBas NO-LOCK where
    RECID(LokArtBas) = wArtBasRecid NO-ERROR.

  /* En transaksjon rundt all opdatering. */
  TRANS_BLOKK:
  do TRANSACTION:

    FIND CURRENT LokArtBas EXCLUSIVE-LOCK.

    /* Henter eller oppretter ArtPris for gjeldende prisprofil. */
    RUN HentArtPris
            (input LokArtBas.ArtikkelNr,
             input FI-ProfilNr,
             input wTilbud,
             OUTPUT wArtPrisRecid).
    FIND LokArtPris EXCLUSIVE-LOCK where
      RECID(LokArtPris) = wArtPrisRecid.

    /* Hvis bruker har krysset av for direkte oppdatering av pris. */
    /* Ved direkte oppdatering, opprettes også poster i PrisKo.    */
    IF wDirekte THEN
    DIREKTE:
    do:

      /* Er det en normalprisendring, skal ikke Tilbudsflagget r›res. */
      IF wTilbud = FALSE THEN
        DO:
          . /* Gj›r ingenting.  */
        END.

      /* G†r artikkelen p† tilbud, skal flagget oppdateres. */
      else IF wTilbud = TRUE AND LokArtPris.Tilbud = FALSE then
        DO:
          assign
            LokArtPris.Tilbud        = true
            LokArtBas.SattPaKampanje = TODAY.
        END.

      /* G†r artikkelen av tilbud, skal flagget oppdateres. */
      else IF wTilbud = TRUE AND LokArtPris.Tilbud = true then
        DO:
          assign
            LokArtPris.Tilbud = false.
        END.

      /* Ukjent tilstand */
      ELSE DO:

      END.

      /* vrig informasjon. */
      assign
        LokArtPris.ValPris[wPrisIndex]      = DEC(entry( 1,wSkjerm,";"))
        LokArtPris.InnKjopsPris[wPrisIndex] = DEC(entry( 2,wSkjerm,";"))
        LokArtPris.Rab1Kr[wPrisIndex]       = DEC(entry( 3,wSkjerm,";"))
        LokArtPris.Rab1%[wPrisIndex]        = DEC(entry( 4,wSkjerm,";"))
        LokArtPris.Rab2Kr[wPrisIndex]       = DEC(entry( 5,wSkjerm,";"))
        LokArtPris.Rab2%[wPrisIndex]        = DEC(entry( 6,wSkjerm,";"))
        LokArtPris.Frakt[wPrisIndex]        = DEC(entry( 7,wSkjerm,";"))
        LokArtPris.Frakt%[wPrisIndex]       = DEC(entry( 8,wSkjerm,";"))
        LokArtPris.DivKostKr[wPrisIndex]    = DEC(entry( 9,wSkjerm,";"))
        LokArtPris.DivKost%[wPrisIndex]     = DEC(entry(10,wSkjerm,";"))
        LokArtPris.Rab3Kr[wPrisIndex]       = DEC(entry(11,wSkjerm,";"))
        LokArtPris.Rab3%[wPrisIndex]        = DEC(entry(12,wSkjerm,";"))
        LokArtPris.VareKost[wPrisIndex]     = DEC(entry(13,wSkjerm,";"))
        LokArtPris.MvaKr[wPrisIndex]        = DEC(entry(14,wSkjerm,";"))
        LokArtPris.Mva%[wPrisIndex]         = DEC(entry(15,wSkjerm,";"))
        LokArtPris.DBKr[wPrisIndex]         = DEC(entry(16,wSkjerm,";"))
        LokArtPris.DB%[wPrisIndex]          = DEC(entry(17,wSkjerm,";"))
        LokArtPris.Pris[wPrisIndex]         = DEC(entry(18,wSkjerm,";"))
        LokArtPris.EuroPris[wPrisIndex]     = DEC(entry(19,wSkjerm,";")).
        LokArtPris.EuroManuel               = if CAN-DO("True,Yes",entry(20,wSkjerm,";"))
                                                then true
                                                else false.

/*
MESSAGE wSkjerm skip
  "Tilbud:" wTilbud skip
  "Entry  20" entry(20,wSkjerm,";") skip
  "Entry  21" entry(21,wSkjerm,";") skip
  "Entry  22" entry(22,wSkjerm,";") skip
  "Entry  23" entry(23,wSkjerm,";") skip
  "Entry  24" entry(24,wSkjerm,";") skip
  "Entry  25" entry(25,wSkjerm,";") skip
  "Entry  26" entry(26,wSkjerm,";") skip
  "Entry  27" entry(27,wSkjerm,";") skip
VIEW-AS ALERT-BOX.
*/

      /* Dato og tid ordin‘r kalkyle */
      if wTilbud = false then
        assign
          LokArtPris.AktivFraDato           = DATE(entry(21,wSkjerm,";"))
          LokArtPris.AktivFraTid            = INT(entry(22,wSkjerm,";")).

      ELSE
        assign
          LokArtPris.TilbudFraDato          = DATE(entry(23,wSkjerm,";"))
          LokArtPris.TilbudFraTid           = INT(entry(24,wSkjerm,";"))
          LokArtPris.TilbudTilDato          = DATE(entry(25,wSkjerm,";"))
          LokArtPris.TilbudTilTid           = INT(entry(26,wSkjerm,";"))
          LokArtPris.TilbudTimeStyrt        = if CAN-DO("True,Yes",entry(27,wSkjerm,";"))
                                                then TRUE
                                                else FALSE.
      /* Det opprettes alltid prishistorikk ved normalprisendring.           */
      /* Ved tilbud eller endring av tilbud, opprettes det kun historikkpost */
      /* hvis det finnes en p† tilbudspost i prisk›en.                       */
      /* Historikk p† fra tibudsposten, opprettes lenger nede i programmet.  */
      if wTilbud then
        HISTBLOKK:
        DO:
          /* Hent/opprett aktiveringspost. */
          FIND first LokPrisKo EXCLUSIVE-LOCK where
            LokPrisKo.ArtikkelNr    = LokArtBas.ArtikkelNr and
            LokPrisKo.ProfilNr      = FI-ProfilNr and
            LokPrisKo.Tilbud        = wTilbud and
            LokPrisKo.Type          > 1 NO-ERROR.
          if AVAILABLE LokPrisKo THEN
            DO:
              if LokPrisKo.TYPE = 2 then
                DO:
                  {prishist.i
                    &PrisIndex = "wPrisIndex"
                    &wTilbud    = "if wTilbud THEN 2 ELSE 1"
                  }
                END.
            END.
          else DO:
            {prishist.i
              &PrisIndex = "wPrisIndex"
              &wTilbud    = "if wTilbud THEN 2 ELSE 1"
            }
          END.
        END. /* HISTBLOKK */
      ELSE DO:
        {prishist.i
          &PrisIndex = "1"
          &wTilbud   = "1"
        }
      END.
    END. /* DIREKTE */

    /* Oppdaterer prisk›. */
    if wPrisKo then
    OPPDAT_AV_PRISKO:
    DO:
      /* kontroller utf›res her. */
      /* For ordin‘r kalkyle foretas ingen kontroller. Opprettelse eller overskrivning. */
      /* Kontroll av tilbud:                                                            */
      if wTilbud THEN
        DO_SJEKK:
        DO:
          /* Sjekk 1:                                             */
          /*   Sjekker start av tilbud.                           */
          /*   Finnes ingen tilbudspost, oppretter vi nye poster. */
          FIND first buf2PrisKo NO-LOCK where
            buf2PrisKo.ArtikkelNr     = ArtBas.ArtikkelNr and
            buf2PrisKo.ProfilNr       = FI-ProfilNr and
            buf2PrisKo.AktiveresDato >= DATE(entry(23,wSkjerm,";")) and
            buf2PrisKo.Tilbud        = true NO-ERROR.

          if NOT AVAILABLE buf2PrisKo then
            LEAVE DO_SJEKK. /* OK - Ingen tilbud på senere tidspunkt. Både på og av postene kan opprettes.*/

          /* Sjekk 2 */
          /* Finnes en tilbudspost av type 2 (På tilbud), må dato og tid v‘re st›rre en dato og tid */
          /* for avsluttning av det nye tilbudet.                                                   */
          /* For nytt vanlig tilbud og for nytt tilbud i tilbud, blir kontrollen den samme.         */
          if buf2PrisKo.Type = 2 OR
             buf2PrisKo.TYPE = 3 then
            SJEKK2:
            DO:
              /* Ser om tilbudet overlapper neste tilbud eller går utover aktivt tilbud.. */
              if DATE(entry(25,wSkjerm,";")) >= buf2PrisKo.AktiveresDato then
                DO:

                  /* Hvis dato er lik, må også tid sjekkes. */
                  if DATE(entry(24,wSkjerm,";")) = buf2PrisKo.AktiveresDato then
                    DO:

                      /* Tiden overlapper neste tilbud. */
                      if INT(entry(26,wSkjerm,";")) >= buf2PrisKo.AktiveresTid then
                        DO:

                          if buf2PrisKo.Type = 2 then
                            wStatus = "AVBRYT,ERROR-02". /* Tilbud startdato/tid overlapper neste tilbud. */
                          else
                            wStatus = "AVBRYT,ERROR-03". /* Tilbud startdato/tid på tilbud i tilbud ligger utenfor utenforliggende tilbud. */
                          UNDO TRANS_BLOKK, LEAVE TRANS_BLOKK.
                        END.
                      ELSE LEAVE SJEKK2. /* OK */
                    END.
                  ELSE DO:
                    wStatus = "AVBRYT,ERROR-01". /* Tilbud startdato overlapper neste tilbud. */
                    UNDO TRANS_BLOKK, LEAVE TRANS_BLOKK.
                  END.
                END.
              ELSE LEAVE SJEKK2. /* OK */

            END. /* SJEKK2 */
        END. /* DO_SJEKK */

      /* Hent/opprett aktiveringspost. */
      FIND first LokPrisKo EXCLUSIVE-LOCK where
        LokPrisKo.ArtikkelNr    = LokArtBas.ArtikkelNr and
        LokPrisKo.ProfilNr      = FI-ProfilNr and
        LokPrisKo.Tilbud        = wTilbud and
        LokPrisKo.Type          < 3 NO-ERROR.
      /* Ved direkte oppdatering, skal eventuell post i prisk› tas bort. */
      if wDirekte then
        DO:
          if AVAILABLE LokPrisKo then
            DELETE LokPrisKo.
          if wTilbud = FALSE then
            LEAVE OPPDAT_AV_PRISKO.
        END.

      /*-------------
      /* Hent/opprett aktiveringspost. */
      FIND LokPrisKo EXCLUSIVE-LOCK where
        LokPrisKo.ArtikkelNr    = LokArtBas.ArtikkelNr and
        LokPrisKo.ProfilNr      = FI-ProfilNr and
        LokPrisKo.AktiveresDato = DATE(entry(21,wSkjerm,";")) and
        LokPrisKo.AktiveresTid  = INT(entry(23,wSkjerm,";")) and
        LokPrisKo.Tilbud        = wTilbud NO-ERROR.
      ---------------*/
      if NOT AVAILABLE LokPrisKo then
        SKAPELSEN:
        DO:
          /* Hvis det er direkte oppdatering og tilbud, skal ikke ny post opprettes. */
          if wDirekte AND wTilbud then
            LEAVE SKAPELSEN.
          /* Oppretter p† tilbudspost. */
          create LokPrisKo.
          assign
            LokPrisKo.ArtikkelNr    = LokArtBas.ArtikkelNr
            LokPrisKo.ProfilNr      = FI-ProfilNr
            LokPrisKo.AktiveresDato = DATE(entry(23,wSkjerm,";"))
            LokPrisKo.AktiveresTid  = INT(entry(24,wSkjerm,";"))
            LokPrisKo.Tilbud        = wTilbud.
        END. /* SKAPELSEN */

      if AVAILABLE LokPrisKo then
        DO:
          /*----- Ekstra s lenge det bare skal finnes en post. ------ */
          assign
            LokPrisKo.AktiveresDato = if wTilbud
                                        then DATE(entry(23,wSkjerm,";"))
                                        ELSE DATE(entry(21,wSkjerm,";"))
            LokPrisKo.AktiveresTid  = if wTilbud
                                        then INT(entry(24,wSkjerm,";"))
                                        else INT(entry(22,wSkjerm,";")).
          /*--------------------------------------------------------- */

          /* vrig informasjon.                 */
          /* Felles for tilbud og ordin‘r pris. */
          assign
            LokPrisKo.TYPE         = if wTilbud THEN 2 ELSE 1
            LokPrisKo.ValPris      = DEC(entry( 1,wSkjerm,";"))
            LokPrisKo.InnKjopsPris = DEC(entry( 2,wSkjerm,";"))
            LokPrisKo.Rab1Kr       = DEC(entry( 3,wSkjerm,";"))
            LokPrisKo.Rab1%        = DEC(entry( 4,wSkjerm,";"))
            LokPrisKo.Rab2Kr       = DEC(entry( 5,wSkjerm,";"))
            LokPrisKo.Rab2%        = DEC(entry( 6,wSkjerm,";"))
            LokPrisKo.Frakt        = DEC(entry( 7,wSkjerm,";"))
            LokPrisKo.Frakt%       = DEC(entry( 8,wSkjerm,";"))
            LokPrisKo.DivKostKr    = DEC(entry( 9,wSkjerm,";"))
            LokPrisKo.DivKost%     = DEC(entry(10,wSkjerm,";"))
            LokPrisKo.Rab3Kr       = DEC(entry(11,wSkjerm,";"))
            LokPrisKo.Rab3%        = DEC(entry(12,wSkjerm,";"))
            LokPrisKo.VareKost     = DEC(entry(13,wSkjerm,";"))
            LokPrisKo.MvaKr        = DEC(entry(14,wSkjerm,";"))
            LokPrisKo.Mva%         = DEC(entry(15,wSkjerm,";"))
            LokPrisKo.DBKr         = DEC(entry(16,wSkjerm,";"))
            LokPrisKo.DB%          = DEC(entry(17,wSkjerm,";"))
            LokPrisKo.Pris         = DEC(entry(18,wSkjerm,";"))
            LokPrisKo.EuroPris     = DEC(entry(19,wSkjerm,";")).
            LokPrisKo.EuroManuel   = if CAN-DO("True,Yes",entry(20,wSkjerm,";"))
                                                then true
                                                else false.
          /* Dato og tid ordin‘r kalkyle */
          if wTilbud = false then
            assign
              LokPrisKo.GyldigTilDato = ?
              LokPrisKo.GyldigTilTid  = 0
              LokPrisKo.Timestyrt     = false.

          ELSE
            assign
              LokPrisKo.GyldigTilDato    = DATE(entry(25,wSkjerm,";"))
              LokPrisKo.GyldigTilTid     = INT(entry(26,wSkjerm,";"))
              LokPrisKo.TimeStyrt        = if CAN-DO("True,Yes",entry(27,wSkjerm,";"))
                                             then TRUE
                                             else FALSE.
        END.

      /* Hent/opprett slutt på tilbud post. */
      /* *** Viktig **** */
      /* Når et tilbud er slutt, skal prisen settes tilbake til den gjeldende */
      /* ordin‘re kalkyle. Denne hentes fra ArtPris.                          */
      /* Men, men! Ved avsluttning på et tilbud i tilbud, skal prisen settes  */
      /* tilbake til den tilbudsprisen som favner det tilbudet som avsluttes. */
      /* Dette finner man ut ved å se fremover i prisk›en. Er neste post i    */
      /* k›en en avsluttning på et tilbud, skal kalkylen hentes fra denne     */
      /* Posten.                                                              */
      if wTilbud then
        SLUTT_TILBUD:
        DO:
          FIND last LokPrisKo EXCLUSIVE-LOCK where
            LokPrisKo.ArtikkelNr    = LokArtBas.ArtikkelNr and
            LokPrisKo.ProfilNr      = FI-ProfilNr and
            LokPrisKo.Tilbud        = wTilbud and
            LokPrisKo.Type          = 3 NO-ERROR.
          /* Ved direkte oppdatering, skal eventuell post i prisk› tas bort. */
          /*
          if wDirekte then
            DO:
              if AVAILABLE LokPrisKo then
                DELETE LokPrisKo.
              LEAVE SLUTT_TILBUD.
            END.
          */
          /*------------------
          FIND LokPrisKo EXCLUSIVE-LOCK where
            LokPrisKo.ArtikkelNr    = LokArtBas.ArtikkelNr and
            LokPrisKo.ProfilNr      = FI-ProfilNr and
            LokPrisKo.AktiveresDato = DATE(entry(22,wSkjerm,";")) and
            LokPrisKo.AktiveresTid  = INT(entry(24,wSkjerm,";")) + 1 and
            LokPrisKo.Tilbud        = wTilbud NO-ERROR.
          --------------------*/
          if NOT AVAILABLE LokPrisKo then
            DO:
              create LokPrisKo.
              assign
                LokPrisKo.ArtikkelNr    = LokArtBas.ArtikkelNr
                LokPrisKo.ProfilNr      = FI-ProfilNr
                LokPrisKo.AktiveresDato = DATE(entry(25,wSkjerm,";"))
                LokPrisKo.AktiveresTid  = INT(entry(26,wSkjerm,";")) + 1
                LokPrisKo.Tilbud        = wTilbud.
            END.

          /*--- S† lenge det bare skal finnes et sett. --------------*/
          assign
            LokPrisKo.AktiveresDato = DATE(entry(25,wSkjerm,";"))
            LokPrisKo.AktiveresTid  = INT(entry(26,wSkjerm,";")) + 1.
          /*---------------------------------------------------------*/

          /* vrig informasjon.                 */
          assign
            LokPrisKo.TYPE         = 3
            LokPrisKo.ValPris      = DEC(entry( 1,wSkjerm,";"))
            LokPrisKo.InnKjopsPris = DEC(entry( 2,wSkjerm,";"))
            LokPrisKo.Rab1Kr       = DEC(entry( 3,wSkjerm,";"))
            LokPrisKo.Rab1%        = DEC(entry( 4,wSkjerm,";"))
            LokPrisKo.Rab2Kr       = DEC(entry( 5,wSkjerm,";"))
            LokPrisKo.Rab2%        = DEC(entry( 6,wSkjerm,";"))
            LokPrisKo.Frakt        = DEC(entry( 7,wSkjerm,";"))
            LokPrisKo.Frakt%       = DEC(entry( 8,wSkjerm,";"))
            LokPrisKo.DivKostKr    = DEC(entry( 9,wSkjerm,";"))
            LokPrisKo.DivKost%     = DEC(entry(10,wSkjerm,";"))
            LokPrisKo.Rab3Kr       = DEC(entry(11,wSkjerm,";"))
            LokPrisKo.Rab3%        = DEC(entry(12,wSkjerm,";"))
            LokPrisKo.VareKost     = DEC(entry(13,wSkjerm,";"))
            LokPrisKo.MvaKr        = DEC(entry(14,wSkjerm,";"))
            LokPrisKo.Mva%         = DEC(entry(15,wSkjerm,";"))
            LokPrisKo.DBKr         = DEC(entry(16,wSkjerm,";"))
            LokPrisKo.DB%          = DEC(entry(17,wSkjerm,";"))
            LokPrisKo.Pris         = DEC(entry(18,wSkjerm,";"))
            LokPrisKo.EuroPris     = DEC(entry(19,wSkjerm,";")).
            LokPrisKo.EuroManuel   = if CAN-DO("True,Yes",entry(20,wSkjerm,";"))
                                                then true
                                                else false.
          /* Dato og tid. */
          assign
            LokPrisKo.GyldigTilDato = ?
            LokPrisKo.GyldigTilTid  = 0
            LokPrisKo.TimeStyrt     = if CAN-DO("True,Yes",entry(27,wSkjerm,";"))
                                         then TRUE
                                         else FALSE
            LokPrisKo.AktiveresTid  = if LokPrisKo.TimeStyrt = false
                                        then (23 * 60 * 60) + (59 * 60) + 59 /* Den skal vare til og med dato. */
                                        ELSE LokPrisKo.AktiveresTid.

          /* Hvis FraTilbudsposten skal aktiveres idag, oppdateres den direkte. */
          if LokPrisKo.AktiveresDato  <= TODAY then
            DO:
              if AVAILABLE LokArtPris then
                LokArtPris.Tilbud = FALSE.

              /* Setter AV tilbudspost i prisk›en. */
              {prishist.i
                &PrisIndex = "2"
                &wTilbud   = "3"
              }
              /* Normalpris settes i prisk›en. */
              {prishist.i
                &PrisIndex = "1"
                &wTilbud   = "1"
              }
              DELETE LokPrisKo.
            END.

        END. /* SLUTT_TILBUD */

    END. /* OPPDAT_AV_PRISKO */

    /* Allt har gått bra. */
    assign
      wStatus = "OK".
  END. /* TRANS_BLOKK TRANSACTION */

  /* Slipper recordene. no-lock på de oppdaterte recordene. */
  if AVAILABLE buf2ArtBas then
    RELEASE buf2ArtBas.
  if AVAILABLE ArtPris then
    RELEASE LokArtPris.
  if AVAILABLE LokPrisKo then
    RELEASE LokPrisKo.

  /* Avslutter og kvitterer ut jobben. */
  RETURN wStatus.
  END. /* Strong scoope */
END PROCEDURE.

/* Denne proceduren kalles når varekost i Lager.VVarekost = 0. */
PROCEDURE HentVareKost:
  DEF input  parameter wArtikkelNr like ArtBas.ArtikkelNr NO-UNDO.
  DEF INPUT  parameter wButikk     as INT                 NO-UNDO.
  def input  parameter wPris       as dec                 no-undo.
  DEF output parameter wVareKost   as DEC                 NO-UNDO.

  def var wMva%   as dec no-undo.
  def var wdb%    as dec no-undo.
  DEF VAR wTilbud as INT NO-UNDO.

  assign
    wVareKost = 0.

  FIND Butiker NO-LOCK where
    Butiker.Butik = wButikk NO-ERROR.
  if not available Butiker then
    return "AVBRYT".

  FIND ArtBas NO-LOCK where
    ArtBas.ArtikkelNr = wArtikkelNr NO-ERROR.
  if not available ArtBas then
    return "AVBRYT".

  FIND ArtPris NO-LOCK where
    ArtPris.ArtikkelNr = wArtikkelNr and
    ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.

  /* Varekost ved hjelp av kalkyle, hvis denne finnes. */
  if AVAILABLE ArtPris then
    DO:
      /* Flagger om artikkelen er på tilbud. */
      if ArtPris.Tilbud then wTilbud = 2.
      else wTilbud = 1.

      /* Henter kalkyleinformasjon og setter varekost fra kalkulert varekost. */
      assign
        wVareKost = ArtPris.VareKost[wTilbud]  /* Kalkulert varekost. */
        wMva%     = ArtPris.Mva%[wTilbud]      /* Mva prosent         */
        wDb%      = ArtPris.Db%[wTilbud].      /* Db  prosent         */

      /* Henter kostpris ved hjelp av Db% hvis varekost ikke var angitt i kalkyle. */
      if wVareKost = 0 and wDb% <> 0 then
        wVareKost = (wPris / (1 + (wmva% / 100))) / (1 + (wDb% / 100)).
    END.

  /* Henter kostpris ved hjelp av Db% på varegruppen. */
  if wVareKost = 0 then
    VAREGRUPPE:
    DO:
      FIND VarGr of ArtBas NO-LOCK NO-ERROR.
      if not available VarGr then
        LEAVE VAREGRUPPE.

      assign
        wDb%      = VarGr.Kost_Proc
        wVareKost = if wDb% <> 0
                      then (wPris / (1 + (wmva% / 100))) / (1 + (wDb% / 100))
                      else wVareKost.
    END. /* VAREGRUPPE */

  /* Henter kostpris ved hjelp av systemets default Db%. */
  if wVareKost = 0 then
    DO:
      FIND VarGr of ArtBas NO-LOCK NO-ERROR.
      if AVAILABLE VarGr THEN
        FIND Moms of VarGr NO-LOCK NO-ERROR.
      if not available Moms then
        {syspara.i 2 1 4 wMva% dec}
      ELSE wmva% = Moms.MomsProc.

      {syspara.i 2 1 3 wDb% dec}
      if wDb% <> 0 then
        wVareKost = (wPris / (1 + (wmva% / 100))) / (1 + (wDb% / 100)).
      ELSE wVareKost = 0.
    END.

END procedure.

*/           
