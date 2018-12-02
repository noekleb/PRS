/* Sender plukklsite til fil - pllistehode_send_brukere_til_pda.p
   Parametere:  buffersandfields
                query 
   
   Opprettet: 24.08.05 av BHa 
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR cFilNavn          AS CHAR   NO-UNDO.
DEF VAR iAntLinjer        AS INT    NO-UNDO.
DEF VAR cSendesKatalog    AS CHAR   NO-UNDO.

/* Henter eksportkatalog. */
{syspara.i 1 1 51 cSendesKatalog} /* Mulig dette skal skilles ut som egen parameter */
IF cSendesKatalog = "" THEN
DO:
    {syspara.i 1 1 51 cSendesKatalog} /* Henter standard katalog for eksport */
END.

CREATE QUERY hQuery.
CREATE BUFFER hBuffer FOR TABLE "Bruker".
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("for each Bruker no-lock").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
IF hBuffer:AVAIL THEN DO:

  ASSIGN
      cSendesKatalog = RIGHT-TRIM(cSendesKatalog,"\")
      cFilNavn       = cSendesKatalog + "\users.txt"
      .

  OUTPUT TO VALUE(cFilNavn).
/* Utlegg av header informasjon. */
/*   PUT UNFORMATTED             */
/*       ";" +                   */
/*       ";" + "LevNr" +         */
/*       SKIP                    */
/*       .                       */
END.
ELSE RETURN.
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:

  PUT UNFORMATTED
      STRING(hBuffer:BUFFER-FIELD("BrukerId"):BUFFER-VALUE)
      ";1"
      SKIP
      .
  hQuery:GET-NEXT().
END.

OUTPUT CLOSE.

DELETE OBJECT hBuffer NO-ERROR.
DELETE OBJECT hQuery  NO-ERROR.

IF ocReturn = "" THEN obOk = TRUE.

