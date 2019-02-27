DEF VAR X        AS INT  NO-UNDO.
DEF VAR cButLst  AS CHAR NO-UNDO.
DEF VAR iBatchNr AS INT  NO-UNDO.
DEF VAR dTst     AS DECI NO-UNDO.
FUNCTION FixStorl RETURNS CHARACTER
  ( pcStorl as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

 assign
    pcStorl = trim(pcStorl)
    pcStorl = caps(pcStorl)
    pcStorl = if (length(pcStorl) = 1 or 
                 length(pcStorl) = 3
                 ) 
                then " " + pcStorl
                else pcStorl.          

  /* Bytter ut eventuelle comma med punkt. */
  if index(pcStorl,",") <> 0 then
    OVERLAY(pcStorl, index(pcStorl,","), 1, "CHARACTER") = ".".

  RETURN pcStorl.   /* Function return value. */

END FUNCTION.

DEF STREAM Bong.
DEF STREAM TRANSlogg.

ASSIGN
    cButLSt  = "278"
    .

/* Batch for TransLogg */
run batchlogg.p (program-name(1),
                 "FIX Slettede artikler " +
                 string(today) +
                 " " +
                 string(time,"HH:MM") +
                 " " +
                 userid("dictdb"),
                 output iBatchNr).

FOR EACH Butiker NO-LOCK WHERE
    can-do(cButLst,string(Butiker.Butik)):

    OUTPUT STREAM TRANSlogg TO VALUE('translogg' + STRING(Butiker.Butik) + '.txt').
/*     OUTPUT STREAM bong TO VALUE('artlogg' + STRING(Butiker.Butik) + '.txt'). */
/*     EXPORT STREAM Bong DELIMITER ";" */
/*         "B_Id"                       */
/*         "LinjeNr"                    */
/*         "ArtBAs.ArtikkelNr"          */
/*         "ArtBas.Beskr"               */
/*         "TTId"                       */
/*         "ArtikkelNr"                 */
/*         "BongTekst"                  */
/*         "LinjeSum"                   */
/*         "LinjeRab"                   */
/*         "SubtotalRab"                */
/*         "Mva%"                       */
/*         "MvaKr"                      */
/*         "ButikkN"                    */
/*         "GruppeNr"                   */
/*         "KasseNr"                    */
/*         "Dato"                       */
/*         "BongNr"                     */
/*         "BongNr"                     */
/*         .                            */

    FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
        BongLinje.ButikkNr = Butiker.Butik AND 
        BongLinje.GruppeNr = 1 AND
        BongLinje.KasseNr  > 0 AND
        BongLinje.Dato >= 1/1/2004 AND
        BongLinje.Dato <= 12/31/2004 AND
    BongLinje.Makulert = FALSE AND
    CAN-DO("001,003,010",STRING(BongLinje.TTId,"999")):

        FIND TransLogg WHERE TransLogg.Dato        = BongLinje.Dato     AND
                             TransLogg.TTId        = BongLinje.TTId     AND
                             TransLogg.Butik       = BongLinje.ButikkNr AND
                             TransLogg.KassaNr     = BongLinje.KasseNr  AND
                             TransLogg.BongId      = BongLinje.BongNr   AND
                             TransLogg.BongLinjeNr = BongLinje.LinjeNr USE-INDEX Dato NO-LOCK NO-ERROR.
        IF AVAIL TransLogg THEN
            NEXT.
        RUN OpprettTransLogg.

    END.
/*     OUTPUT STREAM bong CLOSE. */
    OUTPUT STREAM Translogg CLOSE.
END.

PROCEDURE OpprettTransLogg:
  DEF VAR piTransNr AS INT  NO-UNDO.
  DEF VAR pcTTId     AS CHAR NO-UNDO.
  DEF VAR piTTId    AS INT  NO-UNDO.
  DEF VAR piTBId    AS INT  NO-UNDO.

    ASSIGN
        X = X + 1
        .
    IF X MODULO 100 = 0 THEN
    DO:
        PAUSE 0.
        DISPLAY X WITH FRAME G .
        .
    END.

/*     EXPORT STREAM Bong DELIMITER ";"                                      */
/*         BongLinje.B_Id                                                    */
/*         BongLinje.LinjeNr                                                 */
/*         (IF AVAILABLE ArtBAs THEN ArtBAs.ArtikkelNr ELSE 0)               */
/*         (IF AVAILABLE ArtBAs THEN ArtBAs.Beskr ELSE "")                   */
/*         BongLinje.TTId                                                    */
/*         BongLinje.ArtikkelNr                                              */
/*         BongLinje.BongTekst                                               */
/*         BongLinje.LinjeSum * (IF BongLinje.Antall < 0 THEN -1 ELSE 1)     */
/*         BongLinje.LinjeRab * (IF BongLinje.Antall < 0 THEN -1 ELSE 1)     */
/*         BongLinje.SubtotalRab * (IF BongLinje.Antall < 0 THEN -1 ELSE 1)  */
/*         BongLinje.Mva%                                                    */
/*         BongLinje.MvaKr * (IF BongLinje.Antall < 0 THEN -1 ELSE 1)        */
/*         BongLinje.ButikkN                                                 */
/*         BongLinje.GruppeNr                                                */
/*         BongLinje.KasseNr                                                 */
/*         BongLinje.Dato                                                    */
/*         BongLinje.BongNr                                                  */
/*         BongLinje.BongNr                                                  */
/*         .                                                                 */

    IF AVAILABLE ArtPris THEN
        RELEASE ArtPris.
/*     FIND Butiker NO-LOCK WHERE                       */
/*         Butiker.Butik = BongLinje.ButikkNr NO-ERROR. */
/*     IF NOT AVAILABLE Butiker THEN                    */
/*         RETURN.                                      */

    ASSIGN
        piTTId = BongLinje.TTId
        piTBId = BongLinje.TBId
        pcTTId  = STRING(BongLinje.TTId,"999")
        .

    /* Setter transaksjonsnummer  */
    if piTransNr = 0 then
      DO:
        find last TransLogg where
          TransLogg.Butik = BongLinje.ButikkNr
          use-index TransLogg no-error.
        if available TransLogg then
          piTransNr = TransLogg.TransNr + 1.
        else
          piTransNr = 1.
      END.
    else
      piTransNr = piTransNr + 1.

    /* Henter artbas og setter artikkelnummer. */
      
    /* Först försöker vi hitta artikeln m h a artikkelnr */
    IF DECI(Bonglinje.Artikkelnr) > 0 THEN
        FIND ArtBas WHERE ArtBas.Artikkelnr = DECI(Bonglinje.Artikkelnr) NO-LOCK NO-ERROR.
    /* Sedan m h a strekkode */
    IF NOT AVAIL ArtBas THEN
        FIND StrekKode WHERE StrekKode.Kode = Bonglinje.Strekkode NO-LOCK NO-ERROR.
    /* Sedan med hjälp av interngenererad strekkode */
    IF NOT AVAIL StrekKode AND Bonglinje.StrekKode BEGINS "02" AND LENGTH(TRIM(Bonglinje.Strekkode)) = 13 THEN DO:
        ASSIGN dTst = DECI(SUBSTR(BongLinje.StrekKode,3,7)) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN
            FIND Artbas WHERE ArtBas.Artikkelnr = dTst NO-LOCK NO-ERROR.
    END.
    /* Till sist letar vi upp PLU-artikkel */
    IF NOT AVAIL ArtBas AND Bonglinje.Varegr > 0 AND Bonglinje.Varegr < 1000 THEN
        FIND ArtBas WHERE ArtBas.ArtikkelNr = Bonglinje.Varegr NO-LOCK NO-ERROR.



/*     FIND first ArtBas NO-LOCK where            */
/*       ArtBas.VG    = BongLinje.VareGr and      */
/*       ArtBas.ArtikkelNr = BongLinje.VareGr AND */
/*       ArtBas.OPris = TRUE NO-ERROR.            */
/*     IF NOT AVAILABLE ArtBAs THEN               */
/*         FIND first ArtBas NO-LOCK where        */
/*           ArtBas.VG    = BongLinje.VareGr and  */
/*           ArtBas.OPris = TRUE NO-ERROR.        */

    FIND BongHode NO-LOCK WHERE
        BongHode.B_Id = BongLinje.B_ID NO-ERROR.

    /* Oppretter TransLogg */
    CREATE TransLogg.
    assign TransLogg.Butik        = BongLinje.ButikkNr
           TransLogg.TransNr      = piTransNr
           TransLogg.SeqNr        = 1
           /* Setter inn pekere på transaksjonene */
           BongLinje.TransNr      = piTransNr
           BongLinje.SeqNr        = 1
           .

    assign
           TransLogg.BatchNr      = iBatchNr
           TransLogg.TTId         = BongLinje.TTId
           TransLogg.TBId         = BongLinje.TBId
           TransLogg.ArtikkelNr   = if AVAILABLE ArtBas
                                      THEN ArtBas.ArtikkelNr
                                      ELSE Bonglinje.Varegr
           TransLogg.Vg           = BongLinje.VareGr
           TransLogg.LopNr        = BongLinje.LopeNr
           TransLogg.Antall       = BongLinje.Antall
           TransLogg.Pris         = BongLinje.LinjeSum / ABSOLUTE(BongLinje.Antall)
           TransLogg.Pris         = if TransLogg.Pris = ?
                                      THEN 0
                                      ELSE TransLogg.Pris
           TransLogg.RabKr        = (BongLinje.LinjeRab + BongLinje.SubtotalRab) / absolute(BongLinje.Antall)
           TransLogg.RabKr        = if TransLogg.RabKr = ?
                                      THEN 0
                                      ELSE TransLogg.RabKr
           TransLogg.KundNr       = BongHode.KundeNr

           TransLogg.LevNr        = if AVAILABLE ArtBas
                                      THEN ArtBas.LevNr
                                      ELSE 0
           TransLogg.OvButik      = if BongLinje.TTId = 6 then BongLinje.MButikkNr else 0
           TransLogg.OvTransNr    = if BongLinje.TTId = 6 then TransLogg.TransNr else 0
           TransLogg.BongId       = BongLinje.BongNr
           TransLogg.BongLinjeNr  = BongLinje.LinjeNr
           TransLogg.KassaNr      = BongLinje.KasseNr
           TransLogg.ForsNr       = BongHode.KassererNr
           TransLogg.Plukket      = if BongLinje.TTId = 6 then true ELSE false
           TransLogg.Dato         = BongLinje.TransDato
           TransLogg.Tid          = BongLinje.TransTid
           TransLogg.SelgerNr     = BongHode.SelgerNr
           TransLogg.BestNr       = 0
           TransLogg.Postert      = TRUE
           Translogg.PostertDato  = Translogg.Dato
           TransLogg.KortNr       = (IF BongHode.KortType = 2
                                       THEN BongHode.KundeKort
                                       ELSE BongHode.MedlemsKort)
           TransLogg.KundNr       = BongHode.KundeNr
           TransLogg.MedlemsNr    = BongHode.MedlemsNr
           TransLogg.KortType     = BongHode.KortType
           TransLogg.RefNr        = BongLinje.RefNr
           TransLogg.RefTekst     = BongLinje.RefTekst
           Translogg.Kode         = Bonglinje.Strekkode
           Translogg.BongTekst    = BongLinje.BongTekst
           TransLogg.VVareKost    = BongLinje.VVareKost / ABSOLUTE(BongLinje.Antall)
           Translogg.SattVVAreKost = TRUE
           TransLogg.KalkylePris  = IF AVAILABLE ArtPris
                                      THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                      ELSE Translogg.KalkylePris
           TransLogg.Varekost     = IF AVAILABLE ArtPris
                                      THEN ArtPris.Varekost[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                      ELSE TransLogg.Varekost
           TransLogg.Mva          = BongLinje.MvaKr / ABSOLUTE(BongLinje.Antall)
           TransLogg.Mva%         = (IF AVAILABLE ArtPris
                                       THEN ArtPris.Mva%[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                       ELSE (TransLogg.Mva / (TransLogg.Pris - TransLogg.Mva - TransLogg.RabKr)) * 100)
           .
    assign TransLogg.Storl        = FixStorl(BongLinje.Storrelse)
           TransLogg.TilStorl     = TransLogg.Storl
           .

    EXPORT STREAM TRANSlogg
        TransLogg.

END PROCEDURE.
