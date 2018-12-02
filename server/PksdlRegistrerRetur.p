/* Registrer innleveranse fra pakkseddel
   Parameter:  <PkSdlId>;<brukerid>
   Opprettet: 09.08.07 av BHa              
-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipdPkSdlId AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iMottaksId AS INTEGER     NO-UNDO.
DEFINE VARIABLE iBatchNr   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTransNr AS INTEGER     NO-UNDO.
DEFINE BUFFER bufPkSdlLinje FOR PkSdlLinje.
DO ON ERROR UNDO, LEAVE:
  FIND PkSdlHode WHERE PkSdlHode.PkSdlId = ipdPkSdlId NO-LOCK NO-ERROR.
  IF NOT AVAIL PkSdlHode OR PkSdlHode.PkSdlStatus <> 10 THEN
      RETURN.
  FIND LAST PkSdlMottak NO-LOCK WHERE PkSdlMottak.PkSdlId = PkSdlHode.PkSdlId NO-ERROR.
  IF AVAIL PkSdlMottak THEN
    iMottaksId = PkSdlMottak.MottaksId + 1.
  ELSE
      iMottaksId = 1.
  CREATE PkSdlMottak.
  ASSIGN PkSdlMottak.PkSdlId     = PkSdlHode.PkSdlId
         PkSdlMottak.MottaksId   = iMottaksId
         PkSdlMottak.MottattDato = TODAY
         PkSdlMottak.MottattTid  = TIME
         .
  RUN batchlogg.w (PROGRAM-NAME(1), 
                   "Returpacksedel: " + string(PkSdlHode.PkSdlId) +
                   " " + string(TODAY) + " " + 
                   " " + string(TIME,"HH:MM:SS"),
                   OUTPUT iBatchNr).
  IF NOT iBatchNr > 0 THEN
  DO:
      RETURN.
  END.
  /* Behandler og lager liste med bestillingsnr som skal innleveres. */
  FOR EACH PkSdlLinje WHERE PkSdlLinje.PkSdlId = PkSdlHode.PkSdlId:
      IF PkSdlLinje.MottaksId <> 0 THEN
          NEXT.
      FIND artbas WHERE artbas.artikkelnr = PkSdlLinje.artikkelnr NO-LOCK NO-ERROR.
      FIND butiker WHERE butiker.butik = PkSdlLinje.butikknr NO-LOCK NO-ERROR.
      IF AVAIL artbas AND AVAIL butiker THEN DO:
          FIND artpris WHERE artpris.artikkelnr = artbas.artikkelnr AND
                             artpris.profilnr   = butiker.profilnr NO-LOCK NO-ERROR.
          IF NOT AVAIL artpris THEN
              FIND artpris WHERE artpris.artikkelnr = artbas.artikkelnr NO-LOCK NO-ERROR.
      END.
      IF NOT AVAIL artbas OR NOT AVAIL butiker OR NOT AVAIL artpris THEN DO:
          FIND bufPkSdlLinje WHERE ROWID(bufPkSdlLinje) = ROWID(PkSdlLinje).
          bufPkSdlLinje.MottaksId = PkSdlMottak.MottaksId.
          RELEASE bufPkSdlLinje.
          NEXT.
      END.
      FIND strkonv WHERE strkonv.strkode = PkSdlLinje.StrKode NO-LOCK NO-ERROR.
      FIND LAST TransLogg NO-LOCK WHERE
        TransLogg.Butik = PkSdlLinje.Butikknr USE-INDEX TransLogg NO-ERROR.
      IF AVAILABLE TransLogg THEN
        iTransNr = TransLogg.TransNr + 1.
      ELSE 
        iTransNr = 1.
      NESTE_NR:
      DO WHILE TRUE:
        iTransNr = iTransNr + 1.
        IF CAN-FIND(TransLogg WHERE
                    TransLogg.Butik   = PkSdlLinje.Butikknr AND
                    TransLogg.TransNr = iTransNr) THEN
          NEXT NESTE_NR.
        ELSE
          LEAVE NESTE_NR.
      END. /* NESTE_NR */
      CREATE TransLogg.
      ASSIGN TransLogg.Butik        = PkSdlLinje.butikknr
             TransLogg.TransNr      = iTransNr
             TransLogg.SeqNr        = 1.
      ASSIGN TransLogg.BatchNr      = iBatchNr
             TransLogg.KundNr       = 0
             TransLogg.TTId         = 5 /* Varekjøp */
             TransLogg.TBId         = 1
             TransLogg.ArtikkelNr   = Artbas.artikkelnr
             TransLogg.LevNr        = artbas.levnr
             TransLogg.BongId       = 0
             TransLogg.BongLinjeNr  = 0
             TransLogg.KassaNr      = 0
             TransLogg.Vg           = artbas.Vg
             TransLogg.LopNr        = artbas.LopNr
             TransLogg.Antall       = PkSdlLinje.AntLevert
/*              TransLogg.Antall       = IF TG-Negativ:CHECKED IN FRAME FRAME-Inlev THEN -1 * TransLogg.Antall ELSE TransLogg.Antall */
             TransLogg.Pris         = ArtPris.Varekost[1]
             TransLogg.RabKr        = 0
             TransLogg.Mva          = 0
             TransLogg.Plukket      = TRUE /* Skal ikke ut på plukkliste */
             TransLogg.Dato         = TODAY
             TransLogg.Tid          = TIME
             TransLogg.SattVVareKost = FALSE
             TransLogg.BestNr       = 99 /* förslag när vi gör Forenklet varemottak */
             TransLogg.Postert      = FALSE
             TransLogg.IndividNr    = 0
             TransLogg.Storl        = IF AVAIL strkonv THEN strkonv.storl ELSE "1"
             Translogg.Kode         = PkSdlLinje.Kode.

     FIND bufPkSdlLinje WHERE ROWID(bufPkSdlLinje) = ROWID(PkSdlLinje).
     bufPkSdlLinje.MottaksId = PkSdlMottak.MottaksId.
     RELEASE bufPkSdlLinje.
  END.

  FIND CURRENT PkSdlHode EXCLUSIVE-LOCK NO-ERROR.
      PkSdlHode.PkSdlStatus = 20.
  RUN batchstatus.p (iBatchNr, 2).
END.


