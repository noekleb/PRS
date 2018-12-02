DEFINE VARIABLE iTelleNr AS INTEGER NO-UNDO.

ASSIGN
  iTelleNr = 1
  .

/* Nullstill antall talt. */
FOR EACH tellelinje WHERE tellelinje.tellenr = iTelleNr:
    ASSIGN
      TelleLinje.AntallTalt = 0
      TelleLinje.AntallDiff = TelleLinje.AntallPar
      TelleLinje.VerdiDiff  = TelleLinje.AntallDiff * TelleLinje.VVareKost
      TelleLinje.OpptVerdi  = 0
      Tellelinje.OpprAntalTalt = 0
      tellelinje.oppdatert = FALSE.
END.

/* Koble fra tellelister - lokasjonslister */
FOR EACH TelleHode NO-LOCK WHERE
  TelleHode.KobletTilTelleNr = iTelleNr:
  FOR EACH tellelinje WHERE tellelinje.tellenr = TelleHode.TelleNr:
    ASSIGN
      TelleLinje.Oppdatert = FALSE.
  END.
END.
FOR EACH TelleHode EXCLUSIVE-LOCK WHERE
  TelleHode.KobletTilTelleNr = iTelleNr:

  ASSIGN
    TelleHode.KobletTilTelleNr = 0
    TelleHode.Oppdatert        = ?
    TelleHode.BatchNr          = 0
    .
END.
FOR EACH TelleHode EXCLUSIVE-LOCK WHERE
  TelleHode.TelleNr = iTelleNr:

  ASSIGN
    TelleHode.KobletTilTelleNr = 0
    TelleHode.Oppdatert        = ?
    TelleHode.BatchNr          = 0
    .
END.