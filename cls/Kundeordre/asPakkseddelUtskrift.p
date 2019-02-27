
/*------------------------------------------------------------------------
    File        : asPakkseddelUtskrift.p
    Purpose     : For test av utskriftsfunksjonen av pakkseddel.

    Syntax      :

    Description : Skriver ut pakkseddel for en kundeordre.
                  NB: Brukerid som angis som siste parameter styrer hvilken
                      butikk og skriver utskriften kommer på.
                      Brukeren må være knyttet til en butikk hvor butikkens 
                      rapportskriver er den skriveren hvor utskriften skal komme.

    Author(s)   : tny
    Created     : Fri Dec 28 10:25:43 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER cKOrdre_IdLst AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER cUtskriftType AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iAntEks AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER cBruker AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn AS CHARACTER NO-UNDO.

DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEF VAR bOk AS LOG NO-UNDO.

DEFINE VARIABLE rKundeordreBehandling AS CLASS cls.Kundeordre.KundeordreBehandling.

/* ***************************  Main Block  *************************** */
rKundeordreBehandling = NEW cls.Kundeordre.KundeordreBehandling( ).

/*IF rKundeordreBehandling:skrivPakkseddel (1180000004, 'utlev',1,'aree') THEN*/
IF rKundeordreBehandling:skrivFlerePakkseddler (cKOrdre_IdLst, cUtskriftType, iAntEks, cBruker) THEN
    ocReturn = 'OK Vellykket utskrift!'.
ELSE 
    ocReturn = 'FEIL Utskrift feilet.'.
