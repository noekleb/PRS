

CURRENT-WINDOW:WIDTH = 300.

DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR cLinje AS CHAR NO-UNDO.
DEF VAR cEAN   AS CHAR NO-UNDO.
DEF VAR lVVareKost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR iAnt AS INT NO-UNDO.
DEF VAR iButNr AS INT NO-UNDO.
DEF VAR iLevNr AS INT NO-UNDO.
DEF VAR lRab% AS DEC FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR iButikkNr AS INT NO-UNDO.

DEFINE VARIABLE FI-EuroKurs AS DECIMAL   NO-UNDO.

{syspara.i 2 1 1 FI-EuroKurs DEC}

ASSIGN
    iLevNr = 417
    lRab%  = 28.57
    iButikkNr = 406
    .

ARTIKKEL:
FOR EACH ArtBas NO-LOCK WHERE 
    ArtBas.LevNr = iLevNr AND 
    ArtBas.OPris = FALSE,
    FIRST ArtPris OF ArtBas EXCLUSIVE-LOCK:

    FIND FIRST Lager EXCLUSIVE-LOCK WHERE 
      Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
      Lager.Butik      = iButikkNr NO-ERROR.

    IF AVAILABLE Lager THEN
        lVVareKost = Lager.VVarekost - ((Lager.VVareKost * lRab%) / 100).
    ELSE 
        lVVareKost = ArtPris.VareKost[1] - ((ArtPris.VareKost[1] * lRab%) / 100).

    IF lVVareKost = 0 OR
        lVVareKost = ? THEN NEXT.
    FIND LevBas OF ArtBAs NO-LOCK NO-ERROR.

    DISPLAY
        LevBas.LevNamn
        ArtBAs.ArtikkelNr
        ArtBas.LevKod
        ArtBAs.Beskr
        ArtBAs.LevNr
        lVVAreKost
        Lager.VVArekost WHEN AVAILABLE LAger
        ArtPris.VareKost[1]
    WITH WIDTH 300.

    IF AVAILABLE LAger THEN
        ASSIGN Lager.VVareKost = lVVareKost.

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
    
END. /* ARTIKKEL */

