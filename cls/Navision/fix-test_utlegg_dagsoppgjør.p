DEF VAR cTekst AS CHAR NO-UNDO.

FOR EACH Butiker NO-LOCK:
    RUN dagsrapp_utskrift.p ('99',Butiker.butik,TODAY,TODAY,FALSE,OUTPUT cTekst).
END.

/*
-> Saml_2_mva dagsrapp_utskrift.p (wrk\dagsrapp_utskrift.p) at line 4678
    PDFSamling dagsrapp_utskrift.p (wrk\dagsrapp_utskrift.p) at line 1979
    dagsrapp_utskrift.p (wrk\dagsrapp_utskrift.p) at line 1134
    c:\tmp\p66884_Untitled1.ped (c:\tmp\p66884_Untitled1.ped) at line 3
*/

