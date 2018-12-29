 
 /*------------------------------------------------------------------------
    File        : KundeordreBehandling
    Purpose     : Hensikten med klasen er å gjøre metodene tilgjengelig for andre anheter via AppServer. Slik at f.eks også kassen kan utføre funksjonene.
    Syntax      : 
    Description : Klassen inneholder metoder for håndtering av kundeordre.
    Author(s)   : tny
    Created     : Fri Dec 28 10:11:57 CET 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS cls.Kundeordre.KundeordreBehandling: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
    DEFINE VARIABLE bTest AS LOG NO-UNDO.
    DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.

    DEFINE VARIABLE rStandardFunksjoner AS cls.Stdfunk.StandardFunksjoner NO-UNDO.
         
    CONSTRUCTOR PUBLIC KundeordreBehandling (  ):
        SUPER ().
  
        ASSIGN
            bTest = TRUE
            cLogg = 'KundeordreBehandling' + REPLACE(STRING(TODAY),'/','')
            .

        rStandardFunksjoner  = NEW cls.stdfunk.StandardFunksjoner(cLogg) NO-ERROR.
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            'Klasse opprettet.'
            ).
        
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL skrivFlerePakkseddler( INPUT pcKOrdre_IdLst AS CHARACTER,
                                                 INPUT pcUtskriftstype AS CHARACTER,
                                                 INPUT piAnteks AS INTEGER,
                                                 INPUT pcBruker AS CHARACTER ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        
        IF pcKOrdre_IdLst <> '' THEN 
            DO iLoop = 1 TO NUM-ENTRIES(pcKOrdre_IdLst):
                bResult = skrivPakkseddel( INPUT DEC(ENTRY(iLoop,pcKOrdre_IdLst)),
                                               INPUT pcUtskriftstype,
                                               INPUT piAnteks,
                                               INPUT pcBruker ).
            END.
        
        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: forenklet utskrift av pakkseddel.
     Notes: 
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL skrivPakkseddel( INPUT plKOrdre_Id AS DECIMAL,
                                           INPUT pcUtskriftstype AS CHARACTER,
                                           INPUT piAnteks AS INTEGER,
                                           INPUT pcBruker AS CHARACTER ):

        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE bdirekte AS LOG NO-UNDO.
        DEFINE VARIABLE cPrinter AS CHARACTER NO-UNDO.

        IF pcUtskriftstype = '' THEN
            pcUtskriftstype = 'full'.
        bDirekte = TRUE.

        FIND Bruker NO-LOCK WHERE 
          Bruker.BrukerId = pcBruker NO-ERROR.
        IF AVAILABLE Bruker AND Bruker.Butik > 0 THEN 
        DO:
            FIND Butiker NO-LOCK WHERE
              Butiker.Butik = Bruker.Butik NO-ERROR.
            IF AVAILABLE Butiker THEN 
              cPrinter = Butiker.RAPPrinter.    
        END.
        ELSE cPrinter = ''.

/*        RUN skrivkundeordre.p (STRING(plKOrdre_Id) + "|full",  */
/*                         bdirekte, cPrinter, piAntEks, "", "").*/
        bResult = skrivPakkseddel( INPUT plKOrdre_Id,
                                      INPUT pcUtskriftsType,
                                      INPUT bDirekte,
                                      INPUT cPrinter,
                                      INPUT piAntEks,
                                      INPUT "",
                                      INPUT "" ).
        bResult = TRUE.
        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: Gir tilgang til fullt grensesnitt for utskrift av pakkseddel.
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL skrivPakkseddel( INPUT plKOrdre_Id AS DECIMAL,
                                           INPUT pcUtskriftsType AS CHARACTER,
                                           INPUT pbDirekte AS LOG,
                                           INPUT pcPrinter AS CHARACTER,
                                           INPUT piAntEks AS INTEGER,
                                           INPUT pcMailAdress AS CHARACTER,
                                           INPUT pcStatusTxt AS CHARACTER ):

        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

        IF CAN-FIND(FIRST KOrdreHode WHERE
                    KOrdreHode.KOrdre_Id = plKOrdre_Id) THEN
        DO:
            RUN skrivkundeordre.p (STRING(plKOrdre_Id) + "|" + pcUtskriftsType,pbDirekte,pcPrinter,piAntEks,pcMailAdress,pcStatusTxt).
            bResult = TRUE.
        END.
        ELSE
            bResult = FALSE.

        RETURN bResult.

    END METHOD.

    DESTRUCTOR PUBLIC KundeordreBehandling ( ):
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            'Klasse avsluttet.'
            ).

    END DESTRUCTOR.

END CLASS.