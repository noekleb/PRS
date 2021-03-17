/* CONNECT -db infopos -H 10.125.250.27 -S 8010 -N tcp */
DEF VAR piTransNr     AS INT  NO-UNDO.
DEF VAR iBatchNr         AS INT  NO-UNDO.

DEFINE VARIABLE dDatoloop AS DATE    NO-UNDO.

/* vi loopr genom datum  */
MESSAGE "Tänk på att vi har  antal = 0"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
DO dDatoloop = DATE(1,1,2010) TO today /*DATE(1,5,2010)*/:
    RUN batchlogg.p ("Manuell",
                     "Data fra kassene " +
                     string(dDatoLoop) +
                     " " +
                     string(0,"HH:MM") +
                     " " +
                     USERID("dictdb"),
                     OUTPUT iBatchNr).
    FOR EACH infopos.varedag WHERE infopos.varedag.dato = dDatoLoop AND infopos.varedag.antall <> 0 
    and infopos.varedag.butnr = 5 NO-LOCK:
/*         IF butnr <> 404 AND butnr <> 411 THEN next. */
        FIND artbas WHERE artbas.artikkelnr = infopos.varedag.ean NO-LOCK NO-ERROR.
        FIND artpris WHERE artpris.artikkelnr = infopos.varedag.ean AND artpris.profilnr = 1 NO-LOCK NO-ERROR.
        FIND LAST TransLogg WHERE
          TransLogg.Butik = varedag.Butnr
          USE-INDEX TransLogg NO-LOCK NO-ERROR.
        IF AVAILABLE TransLogg THEN
          piTransNr = TransLogg.TransNr + 1.
        ELSE
          piTransNr = 1.
        CREATE Translogg.
        ASSIGN TransLogg.Butik        = infopos.varedag.butnr
               TransLogg.TransNr      = piTransNr
               TransLogg.SeqNr        = 1
               NO-ERROR.
        ASSIGN
               TransLogg.BatchNr      = iBatchNr
               TransLogg.TTId         = IF infopos.varedag.antall > 0 THEN 1 ELSE 10
               TransLogg.TBId         = 1
               TransLogg.ArtikkelNr   = infopos.varedag.ean
               TransLogg.Vg           = IF AVAIL artbas THEN artbas.vg ELSE 9999
               TransLogg.LopNr        = IF AVAIL artbas THEN artbas.LopNr ELSE ?
               TransLogg.Antall       = infopos.varedag.Antall
               TransLogg.Pris         = (infopos.varedag.salgssum + infopos.varedag.medrabkr + infopos.varedag.kunrabkr + infopos.varedag.perrabkr + infopos.varedag.genrabkr)/ ABSOLUTE(translogg.Antall)
               TransLogg.Pris         = IF TransLogg.Pris = ?
                                          THEN 0
                                          ELSE TransLogg.Pris
               TransLogg.RabKr        = (infopos.varedag.medrabkr + infopos.varedag.kunrabkr + infopos.varedag.perrabkr + infopos.varedag.genrabkr) / absolute(Translogg.Antall)
               TransLogg.RabKr        = IF TransLogg.RabKr = ?
                                          THEN 0
                                          ELSE TransLogg.RabKr
/*                TransLogg.KundNr       = BongHode.KundeNr */

               TransLogg.LevNr        = IF AVAILABLE ArtBas
                                          THEN ArtBas.LevNr
                                          ELSE 0
/*                TransLogg.BongId       = BongLinje.BongNr                           */
/*                TransLogg.BongLinjeNr  = BongLinje.LinjeNr                          */
/*                TransLogg.KassaNr      = BongLinje.KasseNr                          */
/*                TransLogg.ForsNr       = BongHode.KassererNr                        */
/*                TransLogg.Plukket      = IF BongLinje.TTId = 6 THEN TRUE ELSE FALSE */
               TransLogg.Dato         = infopos.varedag.dato
/*                TransLogg.Tid          = BongLinje.TransTid */
/*                TransLogg.SelgerNr     = BongHode.SelgerNr */
/*                TransLogg.BestNr       = 0 */
               TransLogg.Postert      = FALSE
/*                TransLogg.KortNr       = (IF BongHode.KortType = 2     */
/*                                            THEN BongHode.KundeKort    */
/*                                            ELSE BongHode.MedlemsKort) */
/*                TransLogg.KundNr       = BongHode.KundeNr    */
/*                TransLogg.MedlemsNr    = BongHode.MedlemsNr  */
/*                TransLogg.KortType     = BongHode.KortType   */
/*                TransLogg.RefNr        = BongLinje.RefNr     */
/*                TransLogg.RefTekst     = BongLinje.RefTekst  */
               Translogg.Kode          = IF varedag.ean > 99999999 THEN FILL("0",13 - LENGTH(STRING(infopos.varedag.ean))) + STRING(infopos.varedag.ean) ELSE STRING(infopos.varedag.ean)
               Translogg.BongTekst     = IF AVAIL artbas THEN artbas.bongtekst ELSE "okänd"
               TransLogg.VVareKost     = infopos.varedag.kostpris / ABS(translogg.Antall)
               TransLogg.VVareKost     = IF TransLogg.VVAreKost = ? THEN 0 ELSE Translogg.VVareKost
               TransLogg.SattVVarekost = TRUE /* Skal ikke regnes om ved opp. av statistikker. */
/*                TransLogg.KalkylePris   = IF AVAILABLE ArtPris                                    */
/*                                           THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]     */
/*                                           ELSE Translogg.KalkylePris                             */
/*                TransLogg.Varekost     = IF AVAILABLE ArtPris                                     */
/*                                           THEN ArtPris.Varekost[IF ArtPris.Tilbud THEN 2 ELSE 1] */
/*                                           ELSE TransLogg.Varekost                                */
               TransLogg.Mva          = dec(infopos.varedag.MvaKr / ABSOLUTE(infopos.varedag.Antall))
               Translogg.Mva          = IF Translogg.Mva = ? THEN 0 ELSE Translogg.Mva
               TransLogg.Mva%         = IF AVAILABLE ArtPris
                                           THEN ArtPris.Mva%[1]
                                           ELSE TransLogg.Mva / (TransLogg.Pris - TransLogg.Mva - TransLogg.RabKr) * 100
               Translogg.Mva%         = (IF Translogg.Mva% = ? THEN 0 ELSE Translogg.Mva%)
               .
    END.
/* Flagger batchen klar for oppdatering. */
    RUN batchstatus.p (iBatchNr, 2).
END.

