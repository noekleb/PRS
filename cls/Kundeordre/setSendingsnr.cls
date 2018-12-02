 
 /*------------------------------------------------------------------------
    File        : setSendingsnr
    Purpose     : Dekker over problemet som kan oppstå når ordrebehandler utleverer et antall ordre uten først å sjekke om varene er på lager. Og de bruker tid på å behandle ordrene.
    Syntax      : 
    Description : Setter sendingsnr på kundeordre som er utlevert hvor dette feltet er blankt. Kundeordren må være utlevert for min. 30 min. siden, før feltet settes automatisk.
    Author(s)   : tny
    Created     : Sun Dec 03 02:45:38 CET 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS cls.Kundeordre.setSendingsnr FINAL: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
    DEFINE VARIABLE bOk AS LOG NO-UNDO.    

    DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
        
        
    CONSTRUCTOR PUBLIC setSendingsnr ( INPUT pcLogg AS CHARACTER,
                                       OUTPUT bOk AS LOG ):
        SUPER ().
        
        ASSIGN 
            cLogg = pcLogg
            .
            
        rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
        bOk = fyllSendingsnr().
            
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PROTECTED LOGICAL fyllSendingsnr(  ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

        FOR EACH KORdreHode EXCLUSIVE-LOCK WHERE  
            KOrdreHode.butikkNr = 15 AND
            KOrdreHode.SendingsNr = '' AND 
            KOrdreHode.RegistrertDato >= DATE(01,01,2017) AND 
            KOrdreHode.LevStatus = '50':

            IF KOrdreHode.RegistrertDato < TODAY THEN 
                ASSIGN 
                    KOrdreHode.SendingsNr = 'UTLEVERT' + ' ' + REPLACE(STRING(TODAY),'/','') + ' ' + REPLACE(STRING(TIME),':','').
            ELSE IF (TIME - KOrdrEHode.RegistrertTid) > 1800 THEN   
                    ASSIGN 
                        KOrdreHode.SendingsNr = 'UTLEVERT' + ' ' + REPLACE(STRING(TODAY),'/','') + ' ' + REPLACE(STRING(TIME),':','').

            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Kundeordre:' + 
                ' Butikk: ' + STRING(KOrdreHode.butikkNr) + 
                ' KOrdre_Id: ' + STRING(KOrdreHode.Kordre_Id) + 
                ' EkstORdrenr: ' + KOrdreHode.ekstOrdreNr +
                ' Sendingsnr: ' + KOrdreHode.SendingsNr +
                ' Registrertdato: ' + (IF KOrdreHode.RegistrertDato <> ? THEN STRING(KOrdreHode.RegistrertDato) ELSE '?') +
                ' RegistrertTid: ' + STRING(KOrdrEHode.RegistrertTid,"HH:MM:SS") + 
                ' Levstatus: ' + STRING(KOrdreHode.LevStatus) + 
                '.'
                ).    
        END.

        bResult = TRUE.
        
        RETURN bResult.

    END METHOD.

    DESTRUCTOR PUBLIC setSendingsnr ( ):

    END DESTRUCTOR.

END CLASS.