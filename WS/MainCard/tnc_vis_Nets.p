/*
058495 145951              145951 058495 
080997 004708
058319 372546
072797 009908
065414 007937
210253 055633

*/

CURRENT-WINDOW:WIDTH = 300.
DEF VAR cListe AS CHAR NO-UNDO.
FOR EACH PRSTrans.Nets NO-LOCK WHERE
    PRSTrans.Nets.iJBoxCompanyId = 175 AND
    PRSTrans.Nets.TransactionId  BEGINS '065414 007937' 
    USE-INDEX LastRegistrert
    BREAK BY PRSTrans.Nets.iJBoxCompanyId 
          BY PRSTrans.Nets.RegistrertDato DESCENDING
    
    : /* 058495 145951 */
    DISPLAY
    PRSTrans.Nets.iJBoxCompanyId 
    PRSTrans.Nets.TransactionId
    PRSTrans.Nets.BrukerId 
    PRSTrans.Nets.ButikkNr 
    PRSTrans.Nets.B_id 
    PRSTrans.Nets.Dato 
    PRSTrans.Nets.RegistrertDato COLUMN-LABEL 'Registrert'
    STRING(PRSTrans.Nets.RegistrertTid,"HH:MM:SS") 
    PRSTrans.Nets.Sendt 
    PRSTrans.Nets.SendtDato 
    PRSTrans.Nets.SendtTid
    WITH WIDTH 300.
END.
