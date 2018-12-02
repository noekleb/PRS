
 /*

	Last change:  TN
*/

DEFINE INPUT PARAMETER wQY          AS CHAR     NO-UNDO.
DEFINE INPUT PARAMETER iHt          AS INTEGER  NO-UNDO.
DEFINE INPUT PARAMETER gcTabell     AS CHAR     NO-UNDO.

DEF VAR hField1  AS HANDLE NO-UNDO.
DEF VAR hField2  AS HANDLE NO-UNDO.
DEF VAR hField3  AS HANDLE NO-UNDO.
DEF VAR hQuery   AS HANDLE NO-UNDO.
DEF VAR hBuffer  AS HANDLE NO-UNDO.
DEF VAR cFilnavn AS CHAR   NO-UNDO.
DEF VAR iLoop    AS INT    NO-UNDO.
DEF VAR iLoopVl  AS INTE   NO-UNDO.
DEF VAR cDir     AS CHAR   NO-UNDO.
DEF VAR iCl      AS INT    NO-UNDO.
DEF VAR ltilbud  AS LOG    NO-UNDO.
DEF VAR lcKatalog AS CHAR  NO-UNDO.
DEF VAR pcTekst   AS CHAR  NO-UNDO.
DEF VAR plEan     AS DEC   NO-UNDO.
DEF VAR cBeskr    AS CHAR  NO-UNDO.
DEF VAR cLinje    AS CHAR  NO-UNDO.
DEF VAR cEkstent  AS CHAR  NO-UNDO.
DEF VAR cButListe AS CHAR  NO-UNDO.
DEF VAR cEksportKatalog AS CHAR NO-UNDO.

CREATE QUERY  hQuery.
CREATE BUFFER hBuffer FOR TABLE gcTabell.


{tmp2artbasdef.i}

hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE(wQY).
hQuery:QUERY-OPEN().


STATUS DEFAULT "".
REPEAT:
  hQuery:GET-NEXT() NO-ERROR.
  IF NOT hBuffer:AVAILABLE THEN 
  DO:
      LEAVE.
  END.
  ASSIGN hField1 = hBuffer:BUFFER-FIELD("Artikkelnr").
  FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = hField1:BUFFER-VALUE() NO-ERROR.
  IF AVAILABLE ArtBas THEN
  ERP-LOGG:
  DO TRANSACTION:
      FIND ELogg WHERE
           ELogg.TabellNavn     = "ArtBas" AND
           ELogg.EksterntSystem = "ERP"    AND
           ELogg.Verdier        = STRING(ArtBas.ArtikkelNr) NO-ERROR.
      IF NOT AVAIL Elogg THEN DO:
          CREATE Elogg.
          ASSIGN ELogg.TabellNavn     = "ArtBas"
                 ELogg.EksterntSystem = "ERP"
                 ELogg.Verdier        = STRING(ArtBas.ArtikkelNr).
      END.
      ASSIGN ELogg.EndringsType = 1
             ELogg.Behandlet    = FALSE.
      RELEASE ELogg.
  END. /* TRANSACTION ERP-LOGG */
  ASSIGN iLoop = iLoop + 1.
  IF iLoop MODULO 50 = 0 THEN
    STATUS DEFAULT "Antall loggede produkter " + STRING(iLoop) + ".".
END.
STATUS DEFAULT " ".

MESSAGE "Logget artikler til ERP: " SKIP
        "Antall produkter logget " + STRING(iLoop) + "." SKIP
        "Artiklene legges ut ved neste overføring til ERP"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

