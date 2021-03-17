/*
fix-plu-ugr-pressbyran.p

Import rutine som importerer plu nummer pr. varegruppe for Pressbyrån.
Første kollonne inneholder undergruppenummeret. Tredje kollonne inneholder
plu nummeret.
PLU nummeret opprettes som en strekkode på eksisterende plu artikkel for varegruppen.
PLU artikkelene er opprettet med vanlig plu genereringsrutine.
NB: Prefix er benyttet - 99999999. Som sikrer at det ikke blir 
kollisjon med andre eksisterende artikkelnummer. Det er ikke opprettet 
strekkoder på plu artikklene før importen.

*/

DEF VAR cFilNavn  AS CHAR   NO-UNDO.
DEF VAR cLinje    AS CHAR   NO-UNDO.
DEF VAR bOk       AS LOG    NO-UNDO.
DEF VAR iInt      AS INT    FORMAT ">>>>>9"        NO-UNDO.
DEF VAR lDec      AS DEC    FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEF VAR cLog      AS CHAR   NO-UNDO.
DEF VAR hInstance AS INT    NO-UNDO.

DEF STREAM Inn.
DEF STREAM Ut.

{windows.i}

CURRENT-WINDOW:WIDTH = 200.

ASSIGN
    cFilNavn = "plu-ugr-pressbyran.txt"
    cLog     = "plu-ugr-pressbyran.log"
    .

IF SEARCH(cFilNavn) = ? THEN
DO:
    MESSAGE "Ukjent importfil " + cFilNavn + "." SKIP
            "Import avbrutt."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.
ELSE DO:
    ASSIGN
        bOk = FALSE
        .
    MESSAGE "Skal import av " + cFilNavn + " starte?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
    IF bOk <> TRUE THEN
        RETURN.
END.

IF SEARCH(cFilNavn) <> ? THEN
    OS-DELETE VALUE(cLog).

INPUT STREAM Inn FROM VALUE(cFilNavn).
OUTPUT STREAM Ut TO VALUE(cLog).

FORM
    WITH FRAME Gurre DOWN
    .

IMPORT-FIL:
REPEAT:
    IMPORT STREAM Inn UNFORMATTED cLinje.

    /* Feilsjekk */
    ASSIGN
        iInt = INT(ENTRY(1,cLinje,";"))
        lDec = DEC(ENTRY(3,cLinje,";"))
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        NEXT IMPORT-Fil.
    END.

    FIND VarGr NO-LOCK WHERE
        VarGr.Vg = iInt NO-ERROR.
    IF AVAILABLE VarGr THEN
        FIND FIRST ArtBas NO-LOCK WHERE
             ArtBas.ArtikkelNr = dec("9999999" + string(VarGr.Vg,"999999")) AND
             ArtBas.OPris = TRUE NO-ERROR.

    IF NOT AVAILABLE VarGr OR
       NOT AVAILABLE ArtBas
       THEN
    DO:
        DISPLAY
            STREAM Ut
            lDec COLUMN-LABEL "PLU"
            iInt COLUMN-LABEL "Ugr"
            "* Ukjent ugr" WHEN NOT AVAILABLE VarGr FORMAT "x(15)"
            "* Ukjent PLU" WHEN NOT AVAILABLE ArtBas FORMAT "x(15)"
            cLinje FORMAT "x(50)" COLUMN-LABEL "Tekst"
            WITH FRAME Gurre WIDTH 198 DOWN.
        DOWN 1 WITH FRAME Gurre.
    END.

    /* Oppretter strekkode hvis den ikke finnes fra før. */
    IF AVAILABLE ArtBas THEN
    DO:
        FIND Strekkode NO-LOCK WHERE
            Strekkode.Kode = TRIM(ENTRY(3,cLinje,";")) NO-ERROR.
        IF NOT AVAILABLE Strekkode THEN
        DO:
            CREATE Strekkode.
            ASSIGN
                Strekkode.Kode         = TRIM(ENTRY(3,cLinje,";"))
                Strekkode.ArtikkelNr   = ArtBas.ArtikkelNr
                Strekkode.HovedNr      = TRUE
                Strekkode.StrKode      = 1
                Strekkode.KodeType     = 3
                Strekkode.VareId       = 0
                Strekkode.IKasse       = TRUE
                .
        END.
    END.
    
END. /* IMPORT-FIL */

/* Sjekker at alle varegrupper har en PLU artikkel. */
PLU-SJEKK:
FOR EACH VarGr NO-LOCK:
    FIND ArtBas NO-LOCK WHERE
         ArtBas.ArtikkelNr = dec("9999999" + string(VarGr.Vg,"999999")) AND
         ArtBas.OPris = TRUE NO-ERROR.
    IF NOT AVAILABLE ArtBas
       THEN
    DO:
        DISPLAY
            STREAM Ut
            VarGr.Vg FORMAT "zzzzz9" COLUMN-LABEL "Ugr"
            VarGr.VgBeskr COLUMN-LABEL "Beskrivelse"
            "* Ukjent PLU for varegruppe" WHEN NOT AVAILABLE VarGr FORMAT "x(15)"
            WITH FRAME Gurre WIDTH 198 DOWN.
        DOWN 1 WITH FRAME Gurre.
    END.
END. /* PLU-SJEKK */

OUTPUT STREAM Ut CLOSE.
INPUT STREAM Inn CLOSE.

IF SEARCH(cFilNavn) <> ? THEN
  RUN ShellExecute{&A} IN hpApi(0,
                              "open",
                              "notepad.exe",
                              cLog,
                              "",
                              1,
                              OUTPUT hInstance).

PROCEDURE ShellExecute{&A} EXTERNAL "shell32" :
    DEFINE INPUT PARAMETER HWND AS LONG.
    DEFINE INPUT PARAMETER lpOperation AS CHAR.
    DEFINE INPUT PARAMETER lpFile AS CHAR.
    DEFINE INPUT PARAMETER lpParameters AS CHAR.
    DEFINE INPUT PARAMETER lpDirectory AS CHAR.
    DEFINE INPUT PARAMETER nShowCmd AS LONG.
    DEFINE RETURN PARAMETER hInstance AS LONG.
END.

