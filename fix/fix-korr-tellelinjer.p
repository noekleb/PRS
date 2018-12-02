CURRENT-WINDOW:WIDTH = 200.

DEF BUFFER clButiker FOR Butiker.

DEF VAR iCl        AS INT NO-UNDO.
DEF VAR wVVAreKost AS DEC NO-UNDO.
DEF VAR iTelleNr   AS INT NO-UNDO.

ASSIGN
    iTelleNr = 1
    .

{syspara.i 5 1 1 iCL INT}
FIND clButiker WHERE clButiker.Butik = iCL NO-LOCK NO-ERROR.
IF NOT AVAIL clButiker THEN DO:
  MESSAGE 
  "Finner ikke sentral-lager: " + STRING(iCL) 
  VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RETURN.
END.

FOR EACH TelleLinje WHERE
    TelleLinje.TelleNr = iTelleNr:

    IF NOT AVAILABLE TelleHode THEN
        FIND TelleHode OF TelleLinje.

    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = TelleLinje.ArtikkelNr.
    IF TelleLinje.Beskr = "" THEN
        TelleLinje.Beskr = ArtBas.BongTekst.

    FIND Butiker 
         WHERE Butiker.Butik = INT(TelleHode.ButikkListe)
         NO-LOCK NO-ERROR.

    FIND ArtPris NO-LOCK WHERE
         ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
         ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.

    /* Henter pris fra sentrallageret hvis den ikke finnes for butikken. */
    IF NOT AVAILABLE ArtPris THEN
      FIND ArtPris NO-LOCK WHERE
           ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
           ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
    IF AVAILABLE ArtPris THEN
      wVVAreKost = ArtPris.Varekost[1].

    ASSIGN
          TelleLinje.VVareKost  = wVVAreKost
          TelleLinje.NedSkrevet = wVVAreKost
          TelleLinje.OpprVerdi  = TelleLinje.AntallPar * wVVareKost
          TelleLinje.OpptVerdi  = TelleLinje.AntallTalt * wVVareKost
          TelleLinje.AntallDiff = TelleLinje.AntallPar - TelleLinje.AntallTalt
          TelleLinje.VerdiDiff  = TelleLinje.AntallDiff * TelleLinje.VVareKost
        .
END.
