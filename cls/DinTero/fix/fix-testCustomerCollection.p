
DEF VAR iAntall AS INT NO-UNDO.
DEF VAR cReturn AS CHAR NO-UNDO.
DEF VAR cSearch AS CHAR NO-UNDO.

ASSIGN 
    cSearch = '10016'
    cSearch = '+4799999978'
    cSearch = ''
    .

RUN cls\dintero\asCustomercollection.p (cSearch, OUTPUT iAntall, OUTPUT cReturn).

MESSAGE iAntall SKIP
    cReturn
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
