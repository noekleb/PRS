DEF VAR iType AS INT NO-UNDO.
DEF VAR cStatusReason AS CHAR NO-UNDO.
DEF VAR cSokTekst AS CHAR NO-UNDO.
DEFINE VARIABLE hServer AS HANDLE NO-UNDO.
DEF VAR iStatus AS INT NO-UNDO.

                       
/*
piType = 1 Ukjent eMail adresse
piType = 2 Ukjent mobilnr. (med landkode)
piType = 3 Ukjent mobilnr. (uten landkode)
*/

ASSIGN 
    iType     = 3
/*    cSokTekst = 'joyhuser@hotmail.com'*/
    cSokTekst = '99999994'
    cSokTekst = '+4799999994'
    .

RUN cls\dintero\asUserExist.p (iType, cSokTekst, OUTPUT cStatusReason, OUTPUT iStatus).

MESSAGE 
    'cStatusReason:' cStatusReason
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

