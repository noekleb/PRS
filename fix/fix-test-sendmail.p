DEFINE VARIABLE rSendEMail AS cls.SendEMail.SendEMail NO-UNDO.
rSendeMail  = NEW cls.SendeMail.SendeMail( ) NO-ERROR.
   
rSendEMail:parMessage-Content-Type = 'html'.
rSendEMail:parToADDRESS       = 'tomn@nsoft.no'.
rSendEMail:parMailType        = 'PAKKSEDDEL'.
rSendEMail:parSUBJECT         = 'test test test'.
rSendEMail:parMessage-Charset = 'UTF-8'. /* iso-8859-1 Blank eller 'UTF-8' når det går fra fil. */
/*rSendEMail:parMessage-File    =  'c:\tmp\mailtxt.htm'.*/
rSendEMail:parMessage-File    = 'c:\tmp\mailtxt.htm'.
rSendEMail:parFILE            = 'C:\NSoft\Polygon\PRS\utskrift\But_2_PkSdl_2900001_241019-89121.pdf,C:\NSoft\GitHub\consultingwerk\ADE-Sourcecode\src\webspeed\images\buttons.gif,C:\NSoft\GitHub\consultingwerk\ADE-Sourcecode\src\webspeed\images\l-help.gif'.
rSendEMail:parLOGFILE         = 'C:\NSoft\Polygon\PRS\LOG\asPutFromGib281019.log'.  
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
