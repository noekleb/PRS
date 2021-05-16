DEF VAR cLogg AS CHAR NO-UNDO.
DEF VAR cFil AS CHAR NO-UNDO.
DEF VAR bOk AS LOG NO-UNDO.
DEF VAR cReturn AS CHAR NO-UNDO.
DEF VAR cFilListe AS LONGCHAR NO-UNDO.

DEFINE VARIABLE rhentFTP AS CLASS cls.sendFTP.hentFTP NO-UNDO.

{ cls\StdFunk\filliste.i }

ASSIGN 
    cFil = 'C:\NSoft\Polygon\PRS\kom\Ut\EDI\DESADV237368.edi'
    cLogg = 'TEST'
    .

rhentFTP = NEW cls.sendFTP.hentFTP( cLogg ).

IF rhentFTP:HentFileList( OUTPUT cFilListe) THEN 
    rhentFTP:ByggHostFilListe(cFilListe, FALSE, OUTPUT TABLE tmpFiler).

MESSAGE NUM-ENTRIES(cFilListe,CHR(10)) SKIP 1
    STRING(cFilListe)
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

