DEF VAR gdDato1   AS DATE NO-UNDO.
DEF VAR giBatchNr AS INT  NO-UNDO.
DEF VAR giTransNr AS INT  NO-UNDO.

FUNCTION FixStorl RETURNS CHARACTER
  ( pcStorl as char )  FORWARD.

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

/* Batch for TransLogg */
run batchlogg.p (program-name(1),
                 "Utplukkede transer " +
                 string(today) +
                 " " +
                 string(time,"HH:MM") +
                 " " +
                 userid("dictdb"),
                 output giBatchNr).

/* Flagger batchen åpnet for postering. */
run batchstatus.p (giBatchNr, 1).

CURRENT-WINDOW:WIDTH = 240.
    
FOR EACH Butiker NO-LOCK,
    EACH Gruppe WHERE
         Gruppe.ButikkNr = Butiker.Butik NO-LOCK,
    EACH Kasse OF Gruppe NO-LOCK:

    /* Dato angis i amerikansk format MM/DD/YYYY. */
    DAATOLOOP:
    DO gdDato1 = 11/12/2003 TO TODAY:
        BONGLINJER:
        FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
            BongLinje.ButikkNr = Butiker.Butik AND
            BongLinje.GruppeNr = Gruppe.GruppeNr AND
            BongLinje.KasseNr  = Kasse.KasseNr AND
            BongLinje.Dato     = gdDato1 AND 
            BongLinje.TransNr  = 0: /* Kun bonger som ikke er tidligere oppdatert. */

            /* Legg inn restriksjon på de TTId som er ønsket */
            IF NOT CAN-DO("004",STRING(BongLinje.TTId,"999")) THEN
                NEXT BONGLINJER.

            FIND BongHode NO-LOCK where
                BongHode.B_Id = BongLinje.B_Id NO-ERROR.
            FIND ArtBas NO-LOCK WHERE
                ArtBas.ArtikkelNr = DEC(BongLinje.ArtikkelNr) NO-ERROR.

            PAUSE 0.
            DISPLAY
                BongLinje.ButikkNr
                BongLinje.KasseNr
                BongLinje.Dato
                BongLinje.BongNr
                BongLinje.LinjeNr
                BongLinje.Antall
                BongLinje.LinjeSum
                BongLinje.MvaKr
                BongLinje.LinjeRab
                BongLinje.SubTotalRab
                (BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubTotalRab) COLUMN-LABEL "Netto"
                WITH FRAME Oscar DOWN WIDTH 238.

            /* Setter transaksjonsnummer  */
            if giTransNr = 0 then
            DO:
                find last TransLogg where
                    TransLogg.Butik = BongLinje.ButikkNr
                    use-index TransLogg no-error.
                if available TransLogg then
                    giTransNr = TransLogg.TransNr + 1.
                else
                    giTransNr = 1.
            END.
            else
                giTransNr = giTransNr + 1.

            TRANSLOGG:
            DO:
                /* Oppretter TransLogg */
                CREATE TransLogg.
                assign TransLogg.Butik        = BongLinje.ButikkNr
                       TransLogg.TransNr      = giTransNr
                       TransLogg.SeqNr        = 1
                       /* Setter inn pekere på transaksjonene */
                       BongLinje.TransNr      = giTransNr
                       BongLinje.SeqNr        = 1
                       .
            
                assign
                       TransLogg.BatchNr      = giBatchNr
                       TransLogg.TTId         = BongLinje.TTId
                       TransLogg.TBId         = BongLinje.TBId
                       TransLogg.ArtikkelNr   = if AVAILABLE ArtBas
                                                  THEN ArtBas.ArtikkelNr
                                                  ELSE dec(BongLinje.ArtikkelNr)
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
                       TransLogg.Postert      = FALSE
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
                       TransLogg.VVareKost    = BongLinje.VVareKost
                       .
                ASSIGN TransLogg.Mva          = BongLinje.MvaKr / ABSOLUTE(BongLinje.Antall).
                assign TransLogg.Storl        = FixStorl(BongLinje.Storrelse)
                       TransLogg.TilStorl     = TransLogg.Storl
                       .
            END. /* TRANSLOGG */

        END. /* BONGLINJER */
    END. /* DATOLOOP */
END.

/* Flagger batchen klar for oppdatering. */
run batchstatus.p (giBatchNr, 2).

