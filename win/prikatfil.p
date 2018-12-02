
 /*

	Last change:  TN
*/
DEFINE INPUT PARAMETER wQY          AS CHAR     NO-UNDO.
DEFINE INPUT PARAMETER iKalkyle     AS INTEGER  NO-UNDO. /* 0=Gjeldende kalkyle, 1=Forh. og supl. rabatt. */
DEFINE INPUT PARAMETER gcTabell     AS CHAR     NO-UNDO.

DEFINE VARIABLE rPricat AS CLASS cls.vpi.Pricat NO-UNDO.

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

DEFINE VARIABLE idDate      AS DATE NO-UNDO.
DEFINE VARIABLE icSessionId AS CHAR NO-UNDO.
DEFINE VARIABLE ocValue     AS CHAR NO-UNDO.

DEFINE VARIABLE cLevDato1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLevDato2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLevDato3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLevDato4 AS CHARACTER NO-UNDO.

CREATE QUERY  hQuery.
CREATE BUFFER hBuffer FOR TABLE gcTabell.

rPricat = NEW cls.vpi.Pricat().

{windows.i}
{tmp2artbasdef.i}
{syspara.i 1 1 51 cEksportKatalog}
cEksportKatalog = SESSION:TEMP-DIRECTORY.
IF cEksportKatalog = "" THEN
    cEksportKatalog = "c:\HOME\lindbak\sendes".
ASSIGN 
    cEkstent = "csv"
    cFilnavn = RIGHT-TRIM(RIGHT-TRIM(TRIM(cEksportKatalog), "\"),"/") + "\" + 
                    "pricat" + "." + cEkstent
    cDir = ENTRY(1,cFilnavn,"\") + "\"
    .
DO iLoop = 2 TO NUM-ENTRIES(cFilnavn,"\") - 1:
    cDir = cDir + ENTRY(iLoop,cFilnavn,"\") + "\".
    OS-CREATE-DIR VALUE(cDir).
END.

hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE(wQY). /* For each tmp2ArtBas no-lock by beskr indexed-reposition */
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
   DO:
       rPricat:opprettPricatRecord( INPUT iKalkyle,
                                    INPUT 1,
                                    INPUT ROWID(ArtBas)).
   END.

   ASSIGN iLoop = iLoop + 1.
   IF iLoop MODULO 50 = 0 THEN
     STATUS DEFAULT "Antall eksporterte produkter/varelinjer " + STRING(iLoop) + "/" + STRING(iLoopVL) + ".".
END.

STATUS DEFAULT "Eksporterer til prikatfil...".
rPricat:eksporterPricat(1).
cFilNavn = rPricat:ppcPricatFil.

IF SEARCH(cFilnavn) <> ? THEN
DO:
    DEF VAR hInstance AS INT.

    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH(cFilnavn),
                                  "",
                                  1,
                                  OUTPUT hInstance).

END.

MESSAGE "Prikatfil: " cFilnavn + "." SKIP
        "Antall produkter eksportert " + STRING(iLoop) + "."
        "Antall varelinjer eksportert " + STRING(iLoopVL) + "."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

