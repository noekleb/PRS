DEF VAR cFileName AS CHAR NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.

rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner().

cFileName = rStandardFunksjoner:hentSystem( INPUT-OUTPUT cFileName ).

MESSAGE  cFileName
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
