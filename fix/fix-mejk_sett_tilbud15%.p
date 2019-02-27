/* 

*/

CURRENT-WINDOW:WIDTH = 200.

DEFINE VARIABLE FI-EuroKurs AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cEANLst     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVgLst      AS CHARACTER NO-UNDO.
DEFINE VARIABLE bFlagg      AS LOG       NO-UNDO.
DEFINE VARIABLE dFraDato    AS DATE      NO-UNDO.
DEFINE VARIABLE dTilDato    AS DATE      NO-UNDO.
DEFINE VARIABLE iFraTid     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTilTid     AS INTEGER   NO-UNDO.

{syspara.i 2 1 1 FI-EuroKurs DEC}

ASSIGN
  dFraDato = 11/08/2012
  dTilDato = 11/09/2012
  iFraTid  = 0
  iTilTid  = 0
  cEANLst = "4000281695502,7310511216405,7310511210403,7310511212407,7310510001231,7622300147884,7622201056643,7622300725457,7622300661472," +
            "7310511217402,0739413080187,7340005402571,7340005400348,7340005402106,7300400354202,7300400365604,7300400296106,7300400296205," + 
            "7300400366403,7300400362702,7340005402403,7300400362009,7300400295802"
  cVgLst  = "600,701,702,703"
  .
ARTLOOP:
FOR EACH ArtBas NO-LOCK WHERE
  ArtBas.ArtikkelNr = 9042187 AND 
  ArtBas.OPris = FALSE:

  /* Ligger på vargrupper som ikke skal med. */
  IF CAN-DO(cVgLst,STRING(ArtBas.Vg)) THEN 
    NEXT ARTLOOP.

  bFlagg = FALSE.
  STREKKODE:
  FOR EACH Strekkode OF ArtBAs NO-LOCK:
    IF CAN-DO(cEANLst,Strekkode.Kode) THEN
      DO: 
        bFlagg = TRUE.
        LEAVE STREKKODE.
      END.
  END. /* STREKKODE */
  IF bFlagg = TRUE THEN NEXT ARTLOOP.
  ELSE 
  PRISKOEN:
  DO:
    FIND FIRST ArtPris OF ArtBas WHERE
        ArtPris.ProfilNr > 0 NO-LOCK NO-ERROR.
    IF AVAILABLE ArtPris THEN
    ARTPRIS:
    DO:
        DISPLAY
            ArtBas.ArtikkelNr
            ArtBas.Beskr
            ArtPris.MvaKr[1]
            ArtPris.Mva%[1]
            ArtPris.Pris[1]
            WITH WIDTH 200.
        
        FIND first PrisKo EXCLUSIVE-LOCK where
          PrisKo.ArtikkelNr    = ArtBas.ArtikkelNr and
          PrisKo.ProfilNr      = ArtPris.ProfilNr and
          PrisKo.AktiveresDato = dFraDato AND
          PrisKo.aktiveresTid  = iFraTid AND
          PrisKo.Tilbud        = TRUE  AND 
          PrisKo.Type          = 2 NO-ERROR.
        /* En luring */
        IF AVAILABLE PrisKo THEN
            DELETE prisko.

        IF NOT AVAILABLE PrisKo THEN
        DO:
            CREATE PrisKo.
            ASSIGN
                PrisKo.ArtikkelNr    = ArtBas.ArtikkelNr
                PrisKo.ProfilNr      = ArtPris.ProfilNr
                PrisKo.AktiveresDato = dFraDato 
                PrisKo.AktiveresTid  = iFraTid 
                Prisko.GyldigTilDato = dTilDato
                Prisko.GyldigTilTid  = iTilTid
                PrisKo.Tilbud        = TRUE
                PrisKo.Type          = 2 /* PÅ Tilbud */
                PrisKo.MomsKod       = ArtPris.MomsKod[1]
                PrisKo.EuroManuel    = FALSE
                Prisko.EtikettStatus  = 1
                PrisKo.Klargjorstatus = 1
                NO-ERROR.
            /* Kalkyle */
            assign
                PrisKo.ValPris      = ArtPris.ValPris[1]
                PrisKo.InnkjopsPris = ArtPris.InnkjopsPris[1]
                Prisko.Rab1Kr       = ArtPris.Rab1Kr[1]
                Prisko.Rab2Kr       = ArtPris.Rab2Kr[1]
                Prisko.Rab3Kr       = ArtPris.Rab3Kr[1]
                PrisKo.Rab1%        = ArtPris.Rab1%[1]
                PrisKo.Rab2%        = ArtPris.Rab2%[1]
                PrisKo.Rab3%        = ArtPris.Rab3%[1]
                PrisKo.VareKost     = ArtPris.VareKost[1]
                PrisKo.Mva%         = ArtPris.Mva%[1]
                PrisKo.Pris         = ROUND(ArtPris.Pris[1] - ((ArtPris.Pris[1] * 15) / 100),1)
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
  END. /* PRISKOEN */    
END. /* ARTLOOP */

