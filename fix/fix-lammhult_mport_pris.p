/* 
  Import av priser fra Axfood.
  Kun inn og utpris skal importeres og aktiveres hos Lammhult. 

*/

CURRENT-WINDOW:WIDTH = 200.

DEF VAR cFilNavn AS CHAR NO-UNDO.
DEFINE VARIABLE cErrFil AS CHARACTER NO-UNDO.

DEF VAR cEAN        AS CHAR FORMAT "x(14)"  NO-UNDO.
DEF VAR cBeskr      LIKE ArtBas.Beskr       NO-UNDO.
DEF VAR lInnpris    LIKE PrisKo.Pris        NO-UNDO.
DEF VAR lPris       LIKE PrisKo.Pris        NO-UNDO.
DEF VAR cRecord     AS CHAR FORMAT "x(180)" NO-UNDO.
DEF VAR FI-EuroKurs AS DEC                  NO-UNDO.
DEF VAR iTime       AS   INT                NO-UNDO.
DEFINE VARIABLE iAntFunnet AS INTEGER NO-UNDO.
DEFINE VARIABLE iAntMangler AS INTEGER NO-UNDO.

{syspara.i 2 1 1 FI-EuroKurs DEC}

ASSIGN
    cFilNavn = "C:\Home\Lindbak\ANKOMMET\Prisfil Axfood Göran Polygon.csv"
    cErrFil  = "C:\Home\Lindbak\ANKOMMET\Prisfil Axfood Göran Polygon.err"
    .

DEF STREAM InnFil.
DEF STREAM UtFil.
DEFINE STREAM ErrFil.

INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.

FIND FIRST PrisProfil NO-LOCK WHERE
    PrisProfil.ProfilNr > 0 NO-ERROR.

HOVEDLOOP:
REPEAT:

    IMPORT STREAM InnFil UNFORMATTED cRecord.

    ASSIGN
        cEAN        = TRIM(ENTRY(20,cRecord,";"))
        cBeskr      = ENTRY(4,cRecord,";")
        lInnpris    = DECIMAL(ENTRY(11,cRecord,";"))
        lPris       = DECIMAL(ENTRY(12,cRecord,";"))
        NO-ERROR.

    /* 0 i innpris og pris godtas ikke */
    IF ERROR-STATUS:ERROR  
       OR (lInnPris = 0 AND lPris = 0)  
       OR cEAN = '' THEN
       DO:
           OUTPUT STREAM ErrFil TO VALUE (cErrFil) APPEND.
           PUT STREAM ErrFil UNFORMATTED cRecord SKIP.
           OUTPUT STREAM ErrFil CLOSE.
           NEXT HOVEDLOOP.
       END.
    
    /* Konverterer EAN kode. */
    RUN bibl_chkean.p (INPUT-OUTPUT cEAN).
    
    /* Henter artikkel. */
    FIND Strekkode NO-LOCK WHERE
        Strekkode.Kode = cEAN NO-ERROR.
    IF AVAILABLE Strekkode THEN 
        FIND ArtBas NO-LOCK WHERE
             ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
    IF AVAILABLE ArtBas AND ArtBas.OPris = FALSE THEN
    ARTBAS:
    DO:
        iAntFunnet = iAntFunnet + 1.
        FIND FIRST ArtPris OF ArtBas WHERE
            ArtPris.ProfilNr > 0 NO-ERROR.
        IF AVAILABLE ArtPris THEN
        DO:
            /* 0 i pris godtas ikke.          */
            /* Overstyres med gjeldende pris. */
            IF lPris < 0.1 THEN
                lPris = DEC(ArtPris.Pris[1]).
            IF lInnpris < 0.1 THEN
                lInnpris = DEC(ArtPris.Varekost[1]).
            /* Er det ingen prisendring, skal det ikke legges opp priskøpost. */
            IF (lInnPris = ArtPris.VareKost[1] AND 
                lPris = ArtPris.Pris[1]) THEN 
                LEAVE ARTBAS.
            
            DISPLAY
                ArtBas.ArtikkelNr
                cBeskr
                lInnpris
                lPris
                ArtPris.MvaKr[1]
                ArtPris.Mva%[1]
                lInnPris
                lPris
                ArtPris.Pris[1]
                /*cRecord*/ 
                WITH WIDTH 200.
            
            iTime = TIME.

            FIND FIRST PrisKo EXCLUSIVE-LOCK WHERE
              PrisKo.ArtikkelNr    = ArtBas.ArtikkelNr AND
              PrisKo.ProfilNr      = PrisProfil.ProfilNr AND
              PrisKo.AktiveresDato = TODAY AND
              PrisKo.aktiveresTid  = itime AND
              PrisKo.Tilbud        = FALSE AND
              PrisKo.Type          = 1 NO-ERROR.
            /* En luring */
            IF AVAILABLE PrisKo THEN
                DELETE prisko.

            IF NOT AVAILABLE PrisKo THEN
            DO:
                CREATE PrisKo.
                ASSIGN
                    PrisKo.ArtikkelNr    = ArtBas.ArtikkelNr
                    PrisKo.ProfilNr      = PrisProfil.ProfilNr
                    PrisKo.AktiveresDato = TODAY 
                    PrisKo.aktiveresTid  = iTime 
                    PrisKo.Tilbud        = FALSE
                    PrisKo.Type          = 1 /* Normalprisendring */
                    PrisKo.MomsKod       = ArtPris.MomsKod[1]
                    PrisKo.EuroManuel    = FALSE
                    NO-ERROR.
                /* Kalkyle */
                ASSIGN
                    PrisKo.ValPris      = lInnPris
                    PrisKo.InnkjopsPris = lInnPris
                    PrisKo.VareKost     = lInnPris
                    PrisKo.Mva%         = ArtPris.Mva%[1]
                    PrisKo.Pris         = lPris
                    PrisKo.EndringsType = 1 /* Ny*/
                    /* Beregnes */
                    PrisKo.MvaKr        = PrisKo.Pris * PrisKo.Mva% / (100 + PrisKo.Mva%)
                    PrisKo.DbKr         = PrisKo.Pris - PrisKo.MvaKr - PrisKo.VareKost
                    PrisKo.Db%          = ROUND(PrisKo.DbKr / (PrisKo.Pris - PrisKo.MvaKr) * 100,2)
                    PrisKo.EuroPris     = PrisKo.Pris * FI-EuroKurs
                    .
                IF ERROR-STATUS:ERROR THEN
                    IF AVAILABLE PrisKo THEN
                        DELETE PrisKo.
                
                /* Setter posten klar til behandling hvis den ikke har salg i butikken. */
                IF NOT CAN-FIND(FIRST TransLogg WHERE 
                                      TransLogg.ArtikkelNr = ArtBas.ArtikkelNr) THEN 
                    ASSIGN 
                        PrisKo.EtikettStatus  = 1
                        PrisKo.KlargjorStatus = 1
                        .
                
            END. /* Priskø */
        END.

    END. /* ARTBAS */
    ELSE DO: 
        iAntMangler = iAntMangler + 1.
        OUTPUT STREAM ErrFil TO VALUE (cErrFil) APPEND.
            PUT STREAM Errfil UNFORMATTED cRecord SKIP.
        OUTPUT STREAM ErrFil CLOSE.
    END.


END. /* HOVEDLOOP */

INPUT STREAM InnFil CLOSE.

MESSAGE 'iAntFunnet' iAntFunnet 'iAntMangler' iAntMangler
VIEW-AS ALERT-BOX.