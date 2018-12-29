
/*------------------------------------------------------------------------
    File        : asPostPakkeEtikettUtskrift.p
    Purpose     : Utskriftsfunksjonen for utskrift av postpakke etikett via AppServer.

    Syntax      :

    Description : Bestiller/skriver ut postpakke etikett for kundeordre av speditør.

    Author(s)   : tny
    Created     : Sat Dec 29 10:25:43 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER cKOrdre_IdLst AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn AS CHARACTER NO-UNDO.

DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEF VAR bOk AS LOG NO-UNDO.

DEFINE VARIABLE rKundeordreBehandling AS CLASS cls.Kundeordre.KundeordreBehandling.

/* ***************************  Main Block  *************************** */
rKundeordreBehandling = NEW cls.Kundeordre.KundeordreBehandling( ).

/*IF rKundeordreBehandling:skrivPakkseddel (1180000004, 'utlev',1,'aree') THEN*/
IF rKundeordreBehandling:skrivPostpakkeEtikett (cKOrdre_IdLst) THEN
    ocReturn = 'OK Vellykket utskrift.'.
ELSE 
    ocReturn = 'FEIL Utskrift feilet!.'.
