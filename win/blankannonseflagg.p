
DEF INPUT PARAMETER wQY      AS CHAR NO-UNDO.
DEF INPUT PARAMETER gcTabell AS CHAR NO-UNDO.

{tmp2artbasdef.i}

DEF VAR hField1  AS HANDLE NO-UNDO.
DEF VAR hField2  AS HANDLE NO-UNDO.
DEF VAR hField3  AS HANDLE NO-UNDO.
DEF VAR hQuery   AS HANDLE NO-UNDO.
DEF VAR hBuffer  AS HANDLE NO-UNDO.
DEF VAR iLoop    AS INT    NO-UNDO.

DEF BUFFER bArtBas FOR ArtBas.

CREATE QUERY  hQuery.
CREATE BUFFER hBuffer FOR TABLE gcTabell.

hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE(wQY).
hQuery:QUERY-OPEN().
STATUS DEFAULT "".
REPEAT TRANSACTION:
    hQuery:GET-NEXT() NO-ERROR.
    IF NOT hBuffer:AVAILABLE THEN LEAVE.
       ASSIGN hField1 = hBuffer:BUFFER-FIELD("Artikkelnr")
              hField2 = hBuffer:BUFFER-FIELD("Beskr")
              hField3 = hBuffer:BUFFER-FIELD("lager").
      IF hField3:BUFFER-VALUE() = FALSE THEN
          NEXT.

      FIND bArtBas EXCLUSIVE-LOCK WHERE
          bArtBas.ArtikkelNr = DECI(hField1:STRING-VALUE())
          NO-ERROR NO-WAIT.
      IF AVAILABLE bArtBas THEN
          ASSIGN
          bArtBas.AnonseArtikkel = FALSE
          .

   iLoop = iLoop + 1.
   IF iLoop MODULO 50 = 0 THEN
     STATUS DEFAULT "Antall nullstille artikkler " + STRING(iLoop) + ".".
END.
STATUS DEFAULT "".
MESSAGE "Annonseflagg nullstillt på valgte artikler." SKIP
        "Antall valgte artikkler: " + STRING(iLoop) + "."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
