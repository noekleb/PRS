/* Kodrehode_manko.p
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

{ttKOrdre.i}

/*DEF VAR hQuery        AS HANDLE NO-UNDO.*/
/*CREATE QUERY hQuery.                                                             */
/*hQuery:SET-BUFFERS(ihBuffer).                                                    */
/*hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " BY dDateSent BY iTimeSent").*/
/*hQuery:QUERY-OPEN().                                                             */
/*hQuery:GET-FIRST().                                                              */
/*REPEAT WHILE NOT hQuery:QUERY-OFF-END:                                           */
/*                                                                                 */
/*  /* Customize from here: */                                                     */
/*                                                                                 */
/*/*  ASSIGN dDate      = ihBuffer:BUFFER-FIELD("dDateSent"):BUFFER-VALUE*/        */
/*/*         iTime      = ihBuffer:BUFFER-FIELD("iTimeSent"):BUFFER-VALUE*/        */
/*/*         cFileName  = ihBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE*/        */
/*/*         .                                                           */        */
/*                                                                                 */
/*  hQuery:GET-NEXT().                                                             */
/*END.                                                                             */
/*                                                                                 */
/*DELETE OBJECT hQuery.                                                            */

RUN settMankoTbls.

ihBuffer:COPY-TEMP-TABLE (BUFFER ttKOrdreLinje:HANDLE,NO,NO,YES).

obOK = YES.
obOk = ocReturn = "".

/* **********************  Internal Procedures  *********************** */

PROCEDURE settMankoTbls:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
    DEFINE VARIABLE cManko AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttKORdreHode.
    EMPTY TEMP-TABLE ttKOrdreLinje.
    EMPTY TEMP-TABLE ttArtBas.

    OPPRETTTBL:
    FOR EACH KOrdreHode NO-LOCK WHERE 
        KOrdreHode.LevStatus >= '10' AND 
        KORdrEHode.LevStatus <=  '55',
        EACH KOrdreLinje OF KOrdrEHode
        BREAK BY KOrdrEHode.DatotidOpprettet DESCENDING:
    
        IF KORdreHode.LEvStatus = '50' THEN
            NEXT.
    
        /* Betalingslinjer o.l. */
        ASSIGN 
            lDec = DEC(KORdreLinje.VareNr) NO-ERROR.
        IF ERROR-STATUS:ERROR OR lDec = 0 THEN
            NEXT.
        FIND ArtBas NO-LOCK WHERE 
            ArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr) NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN
            NEXT.
    
        FIND FIRST ttKOrdreHode WHERE 
                   ttKOrdreHode.KOrdre_Id = KOrdreHode.KOrdre_Id NO-ERROR.
        IF NOT AVAILABLE ttKOrdreHode THEN
        DO:
            CREATE ttKOrdreHode.
            BUFFER-COPY KOrdreHode TO ttKORdreHode.
        END.
    
        FIND FIRST ttKORdreLinje WHERE 
                   ttKOrdreLinje.KOrdre_Id     = KORdreLinje.KOrdre_Id AND
                   ttKORdreLinje.KOrdreLinjeNr = KOrdreLinje.KOrdreLinjeNr AND 
                   ttKORdreLinje.ArtikkelNr    = DEC(KOrdreLinje.VareNr) AND
                   ttKOrdreLinje.ButNr         = KOrdrEHode.ButikkNr AND
                   ttKORdreLinje.StrKode       = KOrdreLinje.StrKode NO-ERROR.
        IF NOT AVAILABLE ttKORdreLinje THEN
        DO:
            CREATE ttKOrdreLinje.
            BUFFER-COPY KOrdreLinje 
                TO ttKOrdreLinje
                ASSIGN 
                    ttKORdreLinje.ButNr       = KOrdrEHode.ButikkNr
                    ttKOrdreLinje.EkstOrdreNr = KOrdrEHode.EkstOrdreNr
                    .
            BUFFER-COPY ArtBas 
                TO ttKOrdreLinje.
        END.
    
        FIND FIRST ttArtBas WHERE 
            ttArtBas.ArtikkelNr = ArtBas.ArtikkelNr AND 
            ttArtBas.ButNr = KORdreHode.butikkNr AND
            ttArtBas.StrKode = KORdreLinje.StrKode NO-ERROR.
        IF NOT AVAILABLE ttArtBas THEN
        DO:
            CREATE ttArtBas.
            ASSIGN 
                ttArtBas.ArtikkelNr = ArtBas.ArtikkelNr 
                ttArtBas.ButNr = KORdreHode.butikkNr 
                ttArtBas.StrKode = KORdreLinje.StrKode
                ttArtBas.Beskr   = ArtBas.Beskr
                ttArtBas.LevKod  = ArtBas.LevKod
                .
                
            /* Summerer opp lager for størrelsen */    
            FOR EACH ArtLag NO-LOCK WHERE 
                ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
                ArtLag.Butik     >= 15 AND 
                ArtLag.butik     <= 16 AND 
                artLag.StrKode    = KOrdreLinje.StrKode:
                
                ASSIGN 
                    ttArtBas.Lagant  = ttArtBas.Lagant + (IF ArtLag.lagant > 0 THEN ArtLag.lagant ELSE 0)
                    ttArtBas.Storl   = ArtLag.Storl
                    .
            END.
        END.
    
        ASSIGN 
            ttArtBas.BestAnt    = ttArtBas.BestAnt + ttKORdreLinje.Antall
            ttArtBas.Diff       = ttArtBas.Lagant - ttartBas.BestAnt
            ttKOrdreLinje.Manko = NOT ttArtBas.Lagant >= ttArtBas.Bestant
            ttKORdreHode.Manko  = IF ttKORdreHode.Manko = FALSE THEN ttKOrdreLinje.Manko ELSE ttKORdreHode.Manko  
            .
    END. /* OPPRETTTBL */
    
END PROCEDURE.
