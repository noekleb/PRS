

CURRENT-WINDOW:WIDTH = 300.

DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR lArtikkelNr AS DEC NO-UNDO.
DEF VAR iKorr AS INT NO-UNDO.
DEF VAR cLinje AS CHAR NO-UNDO.
DEF VAR cEAN   AS CHAR NO-UNDO.
DEF VAR lVVareKost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR iAnt AS INT NO-UNDO.
DEF VAR iButNr AS INT NO-UNDO.
DEFINE VARIABLE FI-EuroKurs AS DECIMAL   NO-UNDO.

{syspara.i 2 1 1 FI-EuroKurs DEC}


ASSIGN
    iButNr   = 1
    cFilNavn = "C:\ArkivDokument\Kunder\Stensland\VaretellingSteensland_korr_VVArekost.csv".

DEF STREAM Inn.

INPUT STREAM Inn FROM VALUE(cFilNAvn) NO-ECHO.

REPEAT:
    IMPORT STREAM Inn UNFORMATTED cLinje.
    iAnt = iAnt + 1.
    IF iAnt <= 1 THEN
        NEXT.

    ASSIGN
        iAnt = iAnt + 1
        iKorr       = INT(ENTRY(17,cLinje,';'))
        lArtikkelNr = DEC(ENTRY(2,cLinje,';'))
        lVVareKost = DEC(ENTRY(11,cLinje,';'))
    .

    IF iKorr <> 1 THEN
        NEXT.

    FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
    IF AVAILABLE ArtBas THEN
        FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE ArtBas THEN
        FIND Lager EXCLUSIVE-LOCK WHERE
        Lager.ArtikkelNr = ArtBAs.ArtikkelNr AND
        Lager.Butik      = iButNr NO-ERROR.

    IF AVAILABLE ArtBas THEN
    DO:
        IF NOT AVAILABLE Lager THEN
        DO:
            CREATE Lager.
            ASSIGN
                Lager.ArtikkelNr = ArtBAs.ArtikkelNR
                Lager.Butik      = iButNr
                .
        END.
        ASSIGN Lager.VVareKost = lVVareKost.


        IF AVAILABLE ArtPris THEN
        ARTPRIS:
        DO:
          FIND first PrisKo EXCLUSIVE-LOCK where
            PrisKo.ArtikkelNr    = ArtBas.ArtikkelNr and
            PrisKo.ProfilNr      = ArtPris.ProfilNr and
            PrisKo.AktiveresDato = TODAY AND
            PrisKo.aktiveresTid  = 0 AND
            PrisKo.Tilbud        = TRUE  AND 
            PrisKo.Type          = 1 NO-ERROR.
          /* En luring */
          IF AVAILABLE PrisKo THEN
              DELETE prisko.
         
          IF NOT AVAILABLE PrisKo THEN
          DO:
              CREATE PrisKo.
              ASSIGN
                  PrisKo.ArtikkelNr    = ArtBas.ArtikkelNr
                  PrisKo.ProfilNr      = ArtPris.ProfilNr
                  PrisKo.AktiveresDato = TODAY  
                  PrisKo.AktiveresTid  = 0 
                  PrisKo.Tilbud        = FALSE
                  PrisKo.Type          = 1 /* Normalpris */
                  PrisKo.MomsKod       = ArtPris.MomsKod[1]
                  PrisKo.EuroManuel    = FALSE
                  Prisko.EtikettStatus  = 1
                  PrisKo.Klargjorstatus = 1
                  NO-ERROR.
              /* Kalkyle */
              assign
                  PrisKo.ValPris      = lVVareKost
                  PrisKo.InnkjopsPris = lVVareKost
                  Prisko.Rab1Kr       = 0
                  Prisko.Rab2Kr       = 0
                  Prisko.Rab3Kr       = 0
                  PrisKo.Rab1%        = 0
                  PrisKo.Rab2%        = 0
                  PrisKo.Rab3%        = 0
                  PrisKo.VareKost     = lVVareKost
                  PrisKo.Mva%         = ArtPris.Mva%[1]
                  PrisKo.Pris         = ArtPris.Pris[1]
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
          END.
        END. /* ARTPRIS */


        FOR EACH TransLogg EXCLUSIVE-LOCK WHERE 
            TransLogg.ArtikkelNr = ArtBas.ArtikkelNr:
            ASSIGN
                TransLogg.VVareKost = lVVareKost
                .
        END.
        DISPLAY
            ArtBas.ArtikkelNr
            ArtBas.LevKod
            ArtBas.Beskr
            iKorr
            lVVAreKost
            AVAILABLE Strekkode
            AVAILABLE ArtBas
            AVAILABLE ArtPris
            AVAILABLE Lager
        WITH WIDTH 300.


    END.

END.
INPUT STREAM Inn CLOSE.
