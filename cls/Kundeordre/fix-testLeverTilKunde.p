
/*------------------------------------------------------------------------
    File        : fix-testLeverTilKunde.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tny
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.

DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEF VAR bOk AS LOG NO-UNDO.

DEFINE VARIABLE rKundeordreBehandling AS CLASS cls.Kundeordre.KundeordreBehandling.

/* ***************************  Main Block  *************************** */
rKundeordreBehandling = NEW cls.Kundeordre.KundeordreBehandling( ).

IF rKundeordreBehandling:LeverTilKunde ( 1180000005, 'tomn' ) THEN
    ocReturn = 'OK Vellykket innlevering.'.
ELSE 
    ocReturn = 'FEIL Innlevering feilet!.'.
