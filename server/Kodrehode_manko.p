/* Kodrehode_manko.p
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cNetButLagerLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.

DEFINE BUFFER bufKOrdreLinje FOR KOrdreLinje.

{ttKOrdre.i}

{syspara.i 150 1 3 cNetButLagerLst}

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
        EACH KOrdreLinje OF KOrdrEHode NO-LOCK
        BREAK BY KOrdrEHode.DatotidOpprettet /*DESCENDING*/:
    
        IF KORdreHode.LevStatus = '50' THEN
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
                
            FIND FIRST ArtLag NO-LOCK WHERE 
                ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
                ArtLag.Butik      = KOrdrEHode.butikkNr AND
                artLag.StrKode    = KOrdreLinje.StrKode NO-ERROR.
            /* Tar inn lager som ligger i nettbutikk ordren. */
            IF AVAILABLE ArtLag THEN
                ASSIGN 
                ttArtBas.Lagant = ArtLag.Lagant
                ttArtBas.Storl   = ArtLag.Storl                
                .
            IF cNetButLagerLst <> '' THEN 
                DO iLoop = 1 TO NUM-ENTRIES(cNetButLagerLst): 
                    FIND FIRST ArtLag NO-LOCK WHERE 
                        ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
                        ArtLag.Butik      = INT(ENTRY(iLoop,cNetButLagerLst)) AND
                        artLag.StrKode    = KOrdreLinje.StrKode NO-ERROR.
                    IF AVAILABLE ArtLag THEN
                        ASSIGN 
                        ttArtBas.Lagant = ttArtBas.Lagant + ArtLag.Lagant
                        .
                END.
        END.
    
        ASSIGN 
            ttArtBas.BestAnt    = ttArtBas.BestAnt + ttKORdreLinje.Antall
            ttArtBas.Diff       = ttArtBas.Lagant - ttartBas.BestAnt
            ttKOrdreLinje.Manko = NOT ttArtBas.Lagant >= ttArtBas.Bestant
            ttKORdreHode.Manko  = IF ttKORdreHode.Manko = FALSE THEN ttKOrdreLinje.Manko ELSE ttKORdreHode.Manko  
            .
        IF KOrdreLinje.Manko <> ttKOrdreLinje.Manko THEN 
        DO:
          FIND bufKOrdreLinje EXCLUSIVE-LOCK WHERE 
            RECID(bufKOrdreLinje) = RECID(KOrdreLinje) NO-WAIT NO-ERROR.
          IF AVAILABLE bufKOrdreLinje THEN 
          DO:
            ASSIGN 
              bufKOrdreLinje.Manko = ttKOrdreLinje.Manko
              .
            RELEASE bufKOrdreLinje.
              
          END.
        END.
        
    END. /* OPPRETTTBL */
    
END PROCEDURE.
