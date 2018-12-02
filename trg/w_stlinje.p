TRIGGER PROCEDURE FOR WRITE OF StLinje OLD BUFFER oldStLinje.

{trg\c_w_trg.i &Fil=SkoTex.StLinje &Type=W} /* TN 3/11-08 */

DEFINE VARIABLE bStatTilHK  AS LOG       NO-UNDO.
DEFINE VARIABLE cTekst      AS CHARACTER NO-UNDO.

/* Skal det sendes artikkelstatistikk? */
{syspara.i 3 4 1 cTekst}
IF (cTekst = "1" OR cTekst = "") THEN
    bStatTilHK = TRUE.
ELSE
    bStatTilHK = FALSE.

IF NEW StLinje OR oldStLinje.AarPerLinNr = 0 THEN
    ASSIGN
    StLinje.AarPerLinNr = int(STRING(StLinje.Aar,"9999") + STRING(StLinje.PerLinNr,"999"))
    .
/* Vi använder CHR(2) i stf CHR(1), Vissa ststistiker innhåller */
/* CHR(1) i DataObjekt ex. SELGER-VG */
/* TN 28/7-09 Endret slik at statistikk legges ut også på lokale artikler. */
IF bStatTilHK THEN 
LOGGSTAT:
DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "StLinje" AND
         ELogg.EksterntSystem = "HK"    AND
         ELogg.Verdier        = StLinje.StTypeId      + CHR(2) + 
                                Stlinje.PerId         + CHR(2) + 
                                StLinje.DataObjekt    + CHR(2) + 
                                StLinje.Diverse       + CHR(2) + 
                                STRING(StLinje.Butik) + CHR(2) + 
                                STRING(StLinje.Aar)   + CHR(2) + 
                                STRING(StLinje.PerLinNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "StLinje"
               ELogg.EksterntSystem = "HK"   
               ELogg.Verdier        = StLinje.StTypeId      + CHR(2) + 
                                      Stlinje.PerId         + CHR(2) + 
                                      StLinje.DataObjekt    + CHR(2) + 
                                      StLinje.Diverse       + CHR(2) + 
                                      STRING(StLinje.Butik) + CHR(2) + 
                                      STRING(StLinje.Aar)   + CHR(2) + 
                                      STRING(StLinje.PerLinNr) NO-ERROR.
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE Elogg.
    
    /* Logger utlegg av artiklene som det sendes artikkelstatistikk på. */
    IF bStatTilHK AND StLinje.StTypeId = 'ARTIKKEL' THEN
    ARTIKKEL:
    DO:
        /* Det skal bare skapes Elogg post en gang pr. dag.                 */
        /* NB: Gamle "TILKORRPOSLOGG" poster slettes via datamottaksserver. */
        IF (DECIMAL(StLinje.DataObjekt) > 0 AND
            DECIMAL(StLinje.DataObjekt) < 8500000) AND
            NOT CAN-FIND(ELogg WHERE 
                         ELogg.TabellNavn     = "ArtBas"  AND
                         ELogg.EksterntSystem = "TILKORRPOSLOGG" AND
                         ELogg.Verdier        = StLinje.DataObjekt AND
                         ELogg.RegistrertDato = TODAY) AND 
            CAN-FIND(FIRST Strekkode WHERE
                     Strekkode.ArtikkelNr = DEC(StLinje.DataObjekt))
            THEN
        RAPPORT-LOKALE-ARTIKLER:
        DO:
            FIND ELogg WHERE 
                 ELogg.TabellNavn     = "ArtBas"  AND
                 ELogg.EksterntSystem = "TILKORRPOS" AND
                 ELogg.Verdier        = StLinje.DataObjekt NO-ERROR.
            IF NOT AVAIL Elogg THEN DO:
                CREATE Elogg.
                ASSIGN ELogg.TabellNavn     = "ArtBas"
                       ELogg.EksterntSystem = "TILKORRPOS"   
                       ELogg.Verdier        = StLinje.DataObjekt.
            END.
            ASSIGN ELogg.EndringsType = 1
                   ELogg.Behandlet    = FALSE.
            RELEASE ELogg.
            
            /* Oppretter loggpost. En artikkel skal bare sendes en gang i løpet av en dag. */
            FIND ELogg WHERE 
                 ELogg.TabellNavn     = "ArtBas"  AND
                 ELogg.EksterntSystem = "TILKORRPOSLOGG" AND
                 ELogg.Verdier        = StLinje.DataObjekt NO-ERROR.
            IF NOT AVAIL Elogg THEN DO:
                CREATE Elogg.
                ASSIGN ELogg.TabellNavn     = "ArtBas"
                       ELogg.EksterntSystem = "TILKORRPOSLOGG"   
                       ELogg.Verdier        = StLinje.DataObjekt.
            END.
            ASSIGN ELogg.EndringsType = 1
                   ELogg.Behandlet    = FALSE.
            RELEASE ELogg.
        END. /* RAPPORT-LOKALE-ARTIKLER */
    END. /* ARTIKKEL */
    
END. /* LOGGSTAT */
RELEASE ELogg.


