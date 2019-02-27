DEF VAR cFilNavn      AS CHAR NO-UNDO.
DEF VAR iTilStrTypeId AS INT  NO-UNDO.
DEF VAR iFraStrTypeId AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEFINE BUFFER bStrType FOR StrType.
DEFINE BUFFER bStrTstr FOR StrTstr.
ASSIGN
  cFilNavn = "C:\Appdir\se\fix\fix-sport1-konvstr.sdv"
  .

DEF STREAM InnFil.

INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.

LOOPEN:
REPEAT:
  IMPORT STREAM InnFil UNFORMATTED cLinje.
  ASSIGN
    iTilStrTypeId = int(ENTRY(1,cLinje,";"))
    iFraStrTypeId = int(ENTRY(2,cLinje,";")).

  STATUS DEFAULT  
    "Behandler:  FRA: " + string(iFraStrTypeId) + " Til: " + string(iTilStrTypeId) + ".".

  FOR EACH VPIArtBas WHERE
    VPIArtBas.EkstVPILevNr = 100 AND
    VPIArtBas.StrTypeId = iFraStrTypeId:
    ASSIGN
      VPIArtBas.StrTypeId = iTilStrTypeId
      .
  END.
END. /* LOOPEN */

INPUT STREAM InnFil CLOSE.
