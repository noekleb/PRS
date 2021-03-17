
/*------------------------------------------------------------------------
    File        : asSetStatusLevertSpeditor.p
    Purpose     : Setter ny status på kundeordre.

    Syntax      :

    Description : Setter ny status på ordre og sender eventuell mail til kunde.

    Author(s)   : tny
    Created     : Sat Dec 29 10:25:43 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER cKOrdre_IdLst AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iLevStatus AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn AS CHARACTER NO-UNDO.

DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEF VAR bOk AS LOG NO-UNDO.

DEFINE VARIABLE rKundeordreBehandling AS CLASS cls.Kundeordre.KundeordreBehandling.

/* ***************************  Main Block  *************************** */
rKundeordreBehandling = NEW cls.Kundeordre.KundeordreBehandling( ).

IF rKundeordreBehandling:setStatusKundeordre (cKOrdre_IdLst, iLevStatus) THEN
    ocReturn = 'OK Vellykket statusendring.'.
ELSE 
    ocReturn = 'FEIL Statusendring feilet!.'.
