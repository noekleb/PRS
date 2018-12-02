/* 
  Import av priser fr aVisma Global.
  Engangsjobb som måtte gjøres fordi det var endret mye priser i Visma,
  istedenfor at endirngene ble gjort i InfoPOS SE.
  TN 27/5-06
  
  Artikkelnummer;EANNo;AltArtNo;SupplArtNo;Name;Innkjøpspris;Veil.utsalgspris

*/

CURRENT-WINDOW:WIDTH = 200.

DEF VAR cFilNavn AS CHAR NO-UNDO.

DEF VAR lArtikkelNr LIKE ArtBas.ArtikkelNr  NO-UNDO.
def var cArtikkelNr as char format "x(13)"  no-undo.
DEF VAR cBeskr      LIKE ArtBas.Beskr       NO-UNDO.
DEF VAR lInnpris    LIKE PrisKo.Pris        NO-UNDO.
DEF VAR lPris       LIKE PrisKo.Pris        NO-UNDO.
DEF VAR cRecord     AS CHAR FORMAT "x(180)" NO-UNDO.
DEF VAR FI-EuroKurs AS DEC                  NO-UNDO.
DEF VAR iTime       AS   INT                NO-UNDO.

{syspara.i 2 1 1 FI-EuroKurs DEC}

    /* TEST */
/* MESSAGE fi-EuroKurs                    */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/* FOR EACH PrisKo: DELETE prisko. END.   */
    /* TEST */

ASSIGN
    cFilNAvn = "C:\Home\Lindbak\ANKOMMET\060522 Underlag for oppdatering av priser i SE.csv"
    .

DEF STREAM InnFil.
DEF STREAM UtFil.

INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.

FIND FIRST PrisProfil NO-LOCK WHERE
    PrisProfil.ProfilNr > 0 NO-ERROR.

HOVEDLOOP:
REPEAT:

    IMPORT STREAM InnFil UNFORMATTED cRecord.

    ASSIGN
        cArtikkelNr = trim(ENTRY(1,cRecord,";"))
        cArtikkelNr = substring(cArtikkelNr,1,length(cArtikkelNr) - 3)
        lArtikkelNr = dec(cArtikkelNr)
        cBeskr      = ENTRY(5,cRecord,";")
        lInnpris    = dec(ENTRY(6,cRecord,";"))
        lPris       = dec(ENTRY(7,cRecord,";"))
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        lArtikkelNr = 0.

    /* 0 i innpris og pris godtas ikke */
    IF (lInnPris = 0 AND lPris = 0) THEN
        NEXT HOVEDLOOP.

    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
    IF AVAILABLE ArtBas AND ArtBas.OPris = FALSE THEN
    ARTBAS:
    DO:
        FIND FIRST ArtPris OF ArtBas WHERE
            ArtPris.ProfilNr > 0 NO-ERROR.
        IF AVAILABLE ArtPris THEN
        DO:
            /* 0 i pris godtas ikke.          */
            /* Overstyres med gjeldende pris. */
            IF lPris < 0.1 THEN
                lPris = int(ArtPris.Pris[1]).
            
            DISPLAY
                lArtikkelNr
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

            FIND first PrisKo EXCLUSIVE-LOCK where
              PrisKo.ArtikkelNr    = lArtikkelNr and
              PrisKo.ProfilNr      = PrisProfil.ProfilNr and
              PrisKo.AktiveresDato = TODAY AND
              PrisKo.aktiveresTid  = itime AND
              PrisKo.Tilbud        = false and
              PrisKo.Type          = 1 NO-ERROR.
            /* En luring */
            IF AVAILABLE PrisKo THEN
                DELETE prisko.

            IF NOT AVAILABLE PrisKo THEN
            DO:
                CREATE PrisKo.
                ASSIGN
                    PrisKo.ArtikkelNr    = lArtikkelNr
                    PrisKo.ProfilNr      = PrisProfil.ProfilNr
                    PrisKo.AktiveresDato = TODAY 
                    PrisKo.aktiveresTid  = iTime 
                    PrisKo.Tilbud        = false
                    PrisKo.Type          = 1 /* Normalprisendring */
                    PrisKo.MomsKod       = ArtPris.MomsKod[1]
                    PrisKo.EuroManuel    = FALSE
                    NO-ERROR.
                /* Kalkyle */
                assign
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
            END.

        END.

    END. /* ARTBAS */


END. /* HOVEDLOOP */

INPUT STREAM InnFil CLOSE.

