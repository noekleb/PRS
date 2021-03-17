DEF VAR iStatusCode AS INT NO-UNDO.
DEF VAR cStatusReason AS CHAR NO-UNDO.
                       
DEFINE VARIABLE rtokenDintero AS cls.Dintero.tokenDintero NO-UNDO. 

rtokenDintero  = NEW cls.Dintero.tokenDintero() NO-ERROR.


rtokenDintero:getToken(iStatusCode, cStatusReason).

MESSAGE 
    'iStatusCode:'  iStatusCode SKIP
    'cStatusReason:' cStatusReason
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
