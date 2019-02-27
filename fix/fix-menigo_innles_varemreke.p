CURRENT-WINDOW:WIDTH = 300.

DEF BUFFER bufVaremerke FOR Varemerke.

DEF STREAM Inn.

DEF VAR cLevKod    AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR cBeskr     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR cVaremerke AS CHAR FORMAT "x(30)" NO-UNDO.

DEF VAR cLinje AS CHAR FORMAT "x(100)" NO-UNDO.
DEF VAR cFilNavn AS CHAR NO-UNDO.

cFilNavn = 'C:\Home\Lindbak\ANKOMMET\Artnr _varum.csv'.

INPUT STREAM Inn FROM VALUE(cFilNavn) NO-ECHO.

REPEAT:
  IMPORT STREAM Inn UNFORMATTED cLinje.

  ASSIGN
      cLevKod    = trim(ENTRY(1,cLinje,';'))
      cBeskr     = trim(ENTRY(2,cLinje,';'))
      cVaremerke = trim(ENTRY(3,cLinje,';'))
      .

  IF NOT CAN-FIND(FIRST Varemerke WHERE
    trim(entry(1,Varemerke.Beskrivelse,'(')) = cVaremerke) THEN
  DO:
      FIND LAST Varemerke NO-LOCK.
      CREATE bufVaremerke.
      ASSIGN
          bufVaremerke.VmId        = Varemerke.VmId + 1
          bufVaremerke.Beskrivelse = cVaremerke
          bufVaremerke.KortNavn    = cVaremerke
          .
  END.
  ELSE DO:
      FIND FIRST Varemerke WHERE
    trim(entry(1,Varemerke.Beskrivelse,'(')) = cVaremerke.
  END.

  FIND ArtBas EXCLUSIVE-LOCK WHERE
      ArtBas.LevKod BEGINS cLevKod NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
  DO:
      FIND FIRST Strekkode NO-LOCK WHERE
        Strekkode.Bestillingsnummer BEGINS cLevKod NO-ERROR.
      IF AVAILABLE Strekkode THEN
          FIND ArtBas OF Strekkode EXCLUSIVE-LOCK.
  END.

  IF AVAILABLE ArtBas THEN
      ArtBas.VmId = Varemerke.VmId.
  /*
  DISPLAY
      cLevKod   
      cBeskr    
      cVaremerke
      CAN-FIND(FIRST Varemerke WHERE
               trim(entry(1,Varemerke.Beskrivelse,'(')) = cVaremerke)
      CAN-FIND(FIRST ArtBas WHERE
               ArtBas.LevKod BEGINS cLevKod)
      ArtBas.Beskr WHEN AVAILABLE ArtBAs
      CAN-FIND(FIRST Strekkode WHERE
               Strekkode.Bestillingsnummer BEGINS cLevKod)
      WITH WIDTH 300.
   */
END.

INPUT STREAM Inn CLOSE.
