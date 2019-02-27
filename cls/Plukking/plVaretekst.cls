 
 /*------------------------------------------------------------------------
    File        : plVaretekst
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tny
    Created     : Sat Feb 03 18:37:05 CET 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS cls.Plukking.plVaretekst: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE iVaretekst AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCl AS INTEGER NO-UNDO.
    DEFINE VARIABLE ix AS INTEGER NO-UNDO.

    DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
        
    CONSTRUCTOR PUBLIC plVaretekst ( INPUT pcLogg AS CHARACTER ):        
        SUPER ().

        ASSIGN 
            cLogg = pcLogg
            .

        rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ).

        /* Innhold i varetekst */
        {syspara.i 11 5 1 iVaretekst INT}
        /* Sentrallager */
        {syspara.i 5 1 1 iCl INT}
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL getVaretekst( INPUT pcKode AS CHARACTER,
                                        INPUT piTilBut AS INTEGER,
                                        OUTPUT pcBeskr AS CHARACTER ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE cMsg AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cLopNr AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cLager AS CHARACTER NO-UNDO.
        
        FIND Strekkode NO-LOCK WHERE 
            Strekkode.Kode = pcKode NO-ERROR.
        IF AVAILABLE Strekkode THEN
        BYGG_VARETEKST:
        DO: 
            FIND FIRST ArtBas NO-LOCK 
                WHERE ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
            IF NOT AVAILABLE ArtBas THEN 
                LEAVE BYGG_VARETEKST.
            FIND StrKonv NO-LOCK WHERE 
                StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
            IF NOT AVAILABLE StrKonv THEN 
                LEAVE BYGG_VARETEKST.

            IF ArtBas.LopNr = ? THEN cLopNr = "?".
            ELSE IF ArtBas.LopNr <= 9999 THEN cLopNr = STRING(ArtBas.LopNr).
            ELSE cLopNr = STRING(ArtBas.LopNr).

            FIND ArtLag NO-LOCK WHERE
                ArtLag.ArtikkelNr = Strekkode.ArtikkelNr AND
                ArtLag.Butik      = iCl AND
                ArtLag.StrKode    = ArtLag.StrKode NO-ERROR.
            IF AVAILABLE ArtLag THEN
                cLager = " (" + STRING(ArtLag.LagAnt) + "-".
            ELSE
                cLager = " (0-".
            /* Tillbutiks lager */
            FIND ArtLag NO-LOCK WHERE
                ArtLag.ArtikkelNr = Strekkode.ArtikkelNr AND
                ArtLag.Butik      = piTilbut AND
                ArtLag.StrKode    = Strekkode.StrKode NO-ERROR.
            IF AVAILABLE ArtLag THEN
                cLager = cLager + STRING(ArtLag.LagAnt) + ")".
            ELSE
                cLager = "".
       
            /* Innhold i varetekst */
            CASE iVaretekst:
                WHEN 0 THEN 
                    pcBeskr = STRING(ArtBas.Vg) + "/" + cLopNr + " " + StrKonv.Storl + cLager.
                WHEN 1 THEN 
                    pcBeskr = ArtBas.Beskr + cLager.
                WHEN 2 THEN 
                    pcBeskr = STRING(ArtBas.LevNr) + "/" + ArtBas.LevKod + cLager.
            END CASE.
            ASSIGN 
                pcBeskr = SUBSTRING(pcBeskr,1,40)
                bResult = TRUE 
                .
        END. /* BYGG_VARETEKST */
        ELSE cMsg = '** Ukjent strekkode ' + pcKode + '.'.

        RETURN bResult.
        CATCH zeroError AS Progress.Lang.AppError:
            DO ix = 1 TO zeroError:NumMessages.
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '** ENDFeil: ' + zeroError:GetMessage(ix) 
                    ).
            END.
        END CATCH.
        CATCH oneError AS Progress.Lang.SysError:
            DO ix = 1 TO oneError:NumMessages.
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '** ENDFeil: ' + oneError:GetMessage(ix) 
                    ).
            END.
        END CATCH.                
        CATCH twoError AS Progress.Lang.ProError:
            DO ix = 1 TO twoError:NumMessages.
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '** ENDFeil: ' + twoError:GetMessage(ix) 
                    ).
            END.
        END CATCH.    

    END METHOD.

    DESTRUCTOR PUBLIC plVaretekst ( ):

    END DESTRUCTOR.

END CLASS.