/*
  Program: asUserExist.p
  
  Søketype:
  iType = 1-eMail, 2-Prefix(landkode)+Mobilnr, 3-mobilnr. 
*/

DEFINE INPUT  PARAMETER iType AS INT NO-UNDO.
DEFINE INPUT  PARAMETER cSokTekst AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER cStatusReason AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER iStatus AS INTEGER NO-UNDO.

DEFINE VARIABLE rcustomerDintero AS cls.Dintero.customerDintero NO-UNDO.

rcustomerDintero  = NEW cls.Dintero.customerDintero() NO-ERROR.

IF itype = 1 THEN
    rcustomerDintero:UserExist(iType, cSokTekst, OUTPUT cStatusReason, OUTPUT iStatus).
ELSE IF itype = 2 THEN
    rcustomerDintero:UserExist(iType, cSokTekst, OUTPUT cStatusReason, OUTPUT iStatus).
ELSE IF itype = 3 THEN
    rcustomerDintero:UserExist(iType, cSokTekst, OUTPUT cStatusReason, OUTPUT iStatus).
ELSE
  cStatusReason = '000 Ukjent søketype ' + STRING(iType) + '.'.
  
RETURN.  
  
