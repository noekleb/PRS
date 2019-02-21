DEF VAR obOk AS LOG NO-UNDO.

DEF VAR iButNr AS INT NO-UNDO.
DEF VAR cFil AS CHAR NO-UNDO.
DEF VAR iTest AS INT NO-UNDO.

DEFINE VARIABLE rSendEMail AS cls.SendEMail.SendEMail NO-UNDO.
rSendEMail  = NEW cls.SendEMail.SendEMail( ) NO-ERROR.

ASSIGN 
    iTest = 1
    .

rSendEMail:parMailType     = 'PAKKSEDDEL'.
rSendEMail:parSUBJECT      =  'Nyttig SUBJECT ' + STRING(NOW).

IF iTest = 1 THEN /* Message body via windows fil med UTF-8 */
DO:
    rSendEMail:parToADDRESS    = 'noekleb@online.no tom@nsoft.no'.
    rSendEMail:parMessage-Charset = 'UTF-8'. /* Blank eller 'UTF-8' når det går fra fil. */
    rSendEMail:parMessage-File = 'C:\NSoft\Polygon\PRS\cls\SendEMail\messageBody.txt'.
    rSendEMail:parFILE         = 'C:\NSoft\Polygon\PRS\utskrift\But_20_Faktura_20900001-180219-64893.pdf'.  
END.
ELSE IF iTest = 2 THEN 
DO:
    rSendEMail:parMessage-Charset = 'iso-8859-1'. /* Blank eller 'UTF-8' når det går fra fil. */
    rSendEMail:parMESSAGE      = 'Dette er en æøåÆØÅ test æøåÆØÅ asdf'. /* tekst lagt inn i progress program. */
    rSendEMail:parFILE         = 'C:\NSoft\Polygon\PRS\utskrift\But_20_Faktura_20900001-180219-64893.pdf'.  
END.

/* PSAS01 TEST */
ELSE IF iTest = 3 THEN
DO:
    rSendEMail:parMessage-Charset = 'UTF-8'. /* Blank eller 'UTF-8' når det går fra fil. */
    rSendEMail:parMessage-File = 'c:\Polygon\prs\mailBody.txt'.
    rSendEMail:parFILE         = 'C:\Polygon\PRS\utskrift\But_16_Faktura_16700001-111217--2098317287.pdf' +   
                                 ' ' + 
                                 'C:\Polygon\PRS\utskrift\But_2_Faktura_2700002-120517-75522.pdf' + 
                                 ' ' + 
                                 'C:\Polygon\PRS\utskrift\But_2_Faktura_2700001-120517-504599.pdf'.  
END.

/* Hos GANT */
ELSE IF iTest = 4 THEN /* Message body via windows fil med UTF-8 */
DO:
    rSendEMail:parToADDRESS    = "tomn@nsoft.no".
    rSendEMail:parMessage-Charset = 'UTF-8'. /* Blank eller 'UTF-8' når det går fra fil. */
    rSendEMail:parMessage-File = 'C:\Appdir\se\cls\SendEMail\messageBody.txt'.
    rSendEMail:parFILE         = 'C:\Appdir\se\cls\But_3_PkSdl_2900001_200219-63795130.pdf'.  
END.

obOk = rSendEMail:send( ).

MESSAGE obOk
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


