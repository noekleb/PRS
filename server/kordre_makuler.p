
/*------------------------------------------------------------------------
    File        : kordre_makuler.p
    Purpose     : Når man velger å makulere en utlevert kundeordre, skal hele ordren settes til return request.

    Syntax      :

    Description : Lager return request ved makulering av levert kundeordre fra nettbutikk.

    Author(s)   : Tom Nøkleby
    Created     : Mon Feb 13 15:15:52 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER lKOrdre_Id    AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER cOldLevStatus AS CHARACTER NO-UNDO. 

DEFINE VARIABLE iButikkNr        AS INTEGER NO-UNDO.
DEFINE VARIABLE cKvittotext      AS CHARACTER NO-UNDO.
DEFINE VARIABLE dReturKOrdre_Id  AS DECIMAL NO-UNDO.
DEFINE VARIABLE cEksterntOrdrenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE ocReturn         AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk             AS LOG NO-UNDO.
DEFINE VARIABLE cNettButikkType  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lNekad AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lPs12  AS LOGICAL     NO-UNDO.

{tt_kolinjer.i}

DEFINE BUFFER bufKOrdreHode  FOR KOrdreHode.
DEFINE BUFFER bufKOrdreLinje FOR KORdreLinje.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
obOk = FALSE.

cNettButikkType = (DYNAMIC-FUNCTION("getFieldValues","SysPara",
                        "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 20","Parameter1")).

FIND KOrdreHode NO-LOCK WHERE 
    KOrdreHode.KOrdre_Id = lKOrdre_Id NO-ERROR.
/* Ukjent Ordre */
IF NOT AVAILABLE KOrdreHode THEN
    DO: 
        ocReturn = 'Ukjent kundeordre (' + STRING(lKOrdre_Id) + ').'. 
        RETURN.
    END.
/* Bare makulerte ordre håndteres her. */
IF AVAILABLE KOrdreHode AND KOrdreHode.LevStatus <> '60' THEN
    DO: 
        ocReturn =  'Ukjent kundeordre (' + STRING(lKOrdre_Id) + ').'.
        RETURN.
    END.
    
/* For JF */    
IF AVAILABLE KOrdreHode AND KOrdreHode.Opphav = 10 AND cNettButikkType = "2" /* PRS nettbutikk */ THEN 
DO:
    IF CAN-FIND(FIRST kordrelinje WHERE 
                kordrelinje.kordre_id = KOrdreHode.Kordre_id AND KOrdrelinje.plockstatus > 2) THEN
        lNekad = TRUE.
    IF CAN-FIND(FIRST kordrelinje WHERE 
                kordrelinje.kordre_id = KOrdreHode.Kordre_id AND KOrdrelinje.plockstatus > 0 AND 
                KOrdrelinje.plockstatus < 3) THEN
        lPs12 = TRUE.
    IF lNekad AND lPs12 THEN DO:
        ocReturn = "Behandling av order påbörjad. >> Manuell handtering krävs".
        RETURN.
    END.
END.
    
    
/* Returnerer ordren */
IF AVAILABLE KOrdreHode THEN 
RETURNER:
DO:
    FIND FIRST KOrdreLinje OF KOrdreHode NO-LOCK WHERE 
        KOrdreLinje.PlukkButikk > 0 NO-ERROR.
    IF AVAILABLE KOrdreLinje THEN
    DO: 
        iButikkNr = KOrdreLinje.PlukkButikk.
        RUN opprettReturOrdre.    
        
        RUN returSalgeCom.
    END.
    obOk = TRUE.
END. /* RETURNER */   

/* **********************  Internal Procedures  *********************** */

PROCEDURE opprettReturOrdre:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE plArtikkeNr LIKE ArtBas.ArtikkelNr NO-UNDO. 

    /* Kopierer ordrehode. */    
    BLOKKEN: 
    DO TRANSACTION:
        CREATE bufKOrdreHode.
        BUFFER-COPY KOrdreHode
            EXCEPT KORdre_Id LevStatus Verkstedmerknad Sendingsnr ekstOrdreNr
            TO bufKORdreHode
        ASSIGN
            bufKOrdreHode.RefKOrdre_Id = KOrdreHode.KOrdre_Id
            bufKOrdreHode.LevStatus    = '50'
            bufKOrdreHode.VerkstedMerknad = 'Fra ordre: ' + KORdreHode.EkstOrdreNr + '.' + CHR(10) +
                                            'KordreId : ' + STRING(KORdreHode.Kordre_Id) + '.' + 
                                            'Retur fra butikk: ' + STRING(iButikkNr) + '.'
            bufKOrdreHode.SendingsNr  = 'RETUR'
            bufKOrdreHode.EkstOrdreNr = KOrdreHode.EkstOrdreNr + ' ' + 'RETUR'
            .
        LINJER:
        FOR EACH KORdreLinje NO-LOCK WHERE 
            KOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id:
            ASSIGN plArtikkeNr = DEC(KOrdreLinje.VareNr) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
                 NEXT.
            ELSE 
                DO:
                    CREATE bufKOrdreLinje.
                    BUFFER-COPY KOrdreLinje
                        EXCEPT KOrdre_Id
                        TO bufKOrdreLinje
                        ASSIGN 
                            bufKOrdreLinje.KOrdre_Id   = bufKOrdreHode.KOrdre_Id
                            bufKOrdreLinje.Antall      = KOrdreLinje.Antall * -1
                            bufKOrdreLinje.ReturKodeId = 43
                            .
                END.
        END. /* LINJER */
            
        FIND CURRENT bufKORdreHode NO-LOCK.
            
    END. /* BLOKKEN TRANSACTION */


END PROCEDURE.

PROCEDURE returSalgeCom:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    RUN kordre_kontantPHX.p (iButikkNr,
                             STRING(bufKOrdreHode.KOrdre_Id),
                             ?,
                             '',
                             OUTPUT ocReturn,
                             OUTPUT obOk 
                            ).

END PROCEDURE.

