DEFINE VARIABLE iMotpostBatchNr AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNyBatchNr AS INTEGER     NO-UNDO.
DEFINE VARIABLE cTitel AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ii         AS INTEGER    NO-UNDO.
DEFINE VARIABLE cStrekkode AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dNyVk      AS DECIMAL INIT 53     NO-UNDO.
DEFINE VARIABLE dOldVk     AS DECIMAL INIT 5338   NO-UNDO.
DEFINE VARIABLE dKorrDB    AS DECIMAL    NO-UNDO.
DEF VAR wTransNr       AS INT INITIAL 0     NO-UNDO.

DEFINE BUFFER finnSistaTrans FOR translogg.
DEFINE BUFFER MotTrans FOR Translogg.
DEFINE BUFFER NyTrans FOR Translogg.

FIND artbas WHERE artbas.artikkelnr =  20008881164 NO-LOCK.
FIND VarGr OF Artbas NO-LOCK.
FIND HuvGr OF VarGr NO-LOCK.

dKorrDB = dOldVk - dNyVk.


/* skapa batch för motpostering */
cTitel = "Motpostering fel varukost ".
RUN batchlogg.p ("Manuell",cTitel,OUTPUT iMotpostBatchNr).

/* skapa batch för motpostering */
cTitel = "Nypostering rättad varukost ".
RUN batchlogg.p ("Manuell",cTitel,OUTPUT iNyBatchNr).

FOR EACH translogg WHERE translogg.artikkelnr = artbas.artikkelnr
                     AND dato > DATE(8,1,2009)
                     AND translogg.vvarekost  = dOldVk AND postert = TRUE NO-LOCK:
    FIND bonglinje WHERE bonglinje.butikknr = translogg.butik AND
                        bonglinje.GruppeNr  = 1                   AND
                        bonglinje.KasseNr   = translogg.KassaNr   AND
                        bonglinje.Dato      = translogg.Dato      AND
                        bonglinje.BongNr    = translogg.bongid    AND
                        bonglinje.linjenr   = translogg.bonglinjenr AND
                        bonglinje.strekkode = translogg.kode      AND 
                        bonglinje.makulert  = FALSE.
        Bonglinje.VVarekost = 
               (IF bonglinje.antall < 0 THEN -1 * bonglinje.antall ELSE bonglinje.antall) * dNyVk.
       FIND prFSData WHERE 
            prFSData.butikknr    = Bonglinje.ButikkNr AND
            prFSData.dato        = Bonglinje.dato     AND 
            prFSData.avdelingsnr = HuvGr.Avdelingnr   AND
            prFSData.hg          = Bonglinje.HovedGr  AND 
            prFSData.vg          = Bonglinje.VareGr.
       prFSData.dbkr = prFSData.dbkr + (Bonglinje.antall * dKorrDB). 
    FIND LAST finnSistaTrans WHERE
      finnSistaTrans.Butik = TransLogg.Butik
      USE-INDEX TransLogg NO-LOCK NO-ERROR.
    IF AVAILABLE finnSistaTrans THEN
      wTransNr = finnSistaTrans.TransNr + 1.
    ELSE
      wTransNr = 1.

      CREATE MotTrans.
      BUFFER-COPY TransLogg TO MotTrans
        ASSIGN
          MotTrans.BatchNr   = iMotpostBatchNr
          MotTrans.TransNr   = wTransNr
          MotTrans.Antall    = TransLogg.Antall * -1
          MotTrans.Postert = FALSE
          MotTrans.PostertDato = ?
          MotTrans.PostertTid  = 0
          .
      FIND LAST finnSistaTrans WHERE
        finnSistaTrans.Butik = TransLogg.Butik
        USE-INDEX TransLogg NO-LOCK NO-ERROR.
      IF AVAILABLE finnSistaTrans THEN
        wTransNr = finnSistaTrans.TransNr + 1.
      ELSE
        wTransNr = 1.

        CREATE NyTrans.
        BUFFER-COPY TransLogg TO NyTrans
          ASSIGN
            NyTrans.BatchNr   = iNyBatchNr
            NyTrans.TransNr   = wTransNr
            NyTrans.vVarekost = dNyVk
            NyTrans.Postert = FALSE
            NyTrans.PostertDato = ?
            NyTrans.PostertTid  = 0
            .

END.
/* /* Flagger batchen klar for oppdatering. */ */
/* run batchstatus.p (iMotpostBatchNr, 2).     */
/* /* Flagger batchen klar for oppdatering. */ */
/* run batchstatus.p (iNyBatchNr, 2).          */

