DEF VAR cFilNavn      AS CHAR NO-UNDO.
DEF VAR iTilStrTypeId AS INT  NO-UNDO.
DEF VAR iFraStrTypeId AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEFINE BUFFER bStrType FOR StrType.
DEFINE BUFFER bStrTstr FOR StrTstr.
ASSIGN
  cFilNavn = "C:\Appdir\91d\SkoTex\fix\fix-sport1-konvstr.sdv"
  .

DEF STREAM InnFil.

INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.

LOOPEN:
REPEAT:
  IMPORT STREAM InnFil UNFORMATTED cLinje.

  ASSIGN
    iTilStrTypeId = int(ENTRY(1,cLinje,";"))
    iFraStrTypeId = int(ENTRY(2,cLinje,";"))
    .

  /* Sjekker at det er ledig. */
  IF CAN-FIND(StrType WHERE
              StrType.StrTypeId = iTilStrTypeId) THEN
  DO:
    MESSAGE "Opptatt:  FRA:" iFraStrTypeId "Til:" iTilStrTypeId
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.

  STATUS DEFAULT  
    "Behandler:  FRA: " + string(iFraStrTypeId) + " Til: " + string(iTilStrTypeId) + ".".

  FIND StrType WHERE
    StrType.StrTypeId = iFraStrTypeId NO-ERROR.
  IF NOT AVAILABLE StrType THEN
    NEXT LOOPEN.
/*
>  /* Flytter alle størrelsene */
>  FOR EACH StrTStr OF StrType:
>    ASSIGN
>      StrTStr.StrTypeId = iTilStrTypeId
>      .
>  END.
 */
  /* Flytter alle størrelsene  Kenneth ie skapar ny StrTStr och deletar */
  FOR EACH StrTStr OF StrType:
    RELEASE bStrTStr.
    BUFFER-COPY StrTStr EXCEPT StrTypeId TO bStrTStr.
    ASSIGN
      bStrTStr.StrTypeId = iTilStrTypeId.
    DELETE StrTStr.
  END.
  /* Flytter alle artikler */
  FOR EACH ArtBas WHERE
    ArtBas.StrTypeId = StrType.StrTypeId:
    ASSIGN
      ArtBas.StrTypeId = iTilStrTypeId
      .
  END.

  /* Flytter alle leverandørsortiment */
  FOR EACH LevSort WHERE
    LevSort.StrTypeId = StrType.StrTypeId:
    ASSIGN
      LevSort.StrTypeId = iTilStrTypeId
      .
  END.
/*
>  ASSIGN
>    StrType.StrTypeId = iTilStrTypeId
>     .
 */
  RELEASE bStrType.
  BUFFER-COPY StrType EXCEPT StrTypeId TO bStrType.
  ASSIGN bStrType.StrTypeId = iTilStrTypeId.
  DELETE StrType.

END. /* LOOPEN */

INPUT STREAM InnFil CLOSE.
