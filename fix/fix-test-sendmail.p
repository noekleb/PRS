DEFINE VARIABLE rSendEMail AS cls.SendEMail.SendEMail NO-UNDO.
rSendeMail  = NEW cls.SendeMail.SendeMail( ) NO-ERROR.
   
rSendEMail:parMessage-Content-Type = 'html'.
rSendEMail:parToADDRESS       = 'tomn@nsoft.no'.
rSendEMail:parMailType        = 'PAKKSEDDEL'.
rSendEMail:parSUBJECT         = 'test test test'.
rSendEMail:parMessage-Charset = 'UTF-8'. /* iso-8859-1 Blank eller 'UTF-8' når det går fra fil. */
/*rSendEMail:parMessage-File    =  'c:\tmp\mailtxt.htm'.*/
rSendEMail:parMessage-File    = 'c:\tmp\53877165.txt'.
rSendEMail:parFILE            = ''.
rSendEMail:parLOGFILE         = ''.  
rSendEMail:send( ).


/*
c:\tmp\mailtxt.htm:
------------------
<html><body>
Here are <i>my øæå ØÆÅ</i> images:
<img src="cid:C:\NSoft\GitHub\consultingwerk\ADE-Sourcecode\src\webspeed\images\buttons.gif">
<img src="cid:C:\NSoft\GitHub\consultingwerk\ADE-Sourcecode\src\webspeed\images\l-help.gif">
</body></html>
*/
