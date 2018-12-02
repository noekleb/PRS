/* Postering av bonglinjer som ikke er opprettet i TransLogg. */
CURRENT-WINDOW:WIDTH = 300.

DEF VAR dDato    AS DATE NO-UNDO.
DEF VAR iBatchNr AS INT NO-UNDO.
DEF VAR iBongNr  AS INT NO-UNDO.

DEF BUFFER bufBongHode  FOR BongHode.
DEF BUFFER bufBongLinje FOR BongLinje.


FUNCTION FixStorl RETURNS CHARACTER
  ( pcStorl AS CHAR )  FORWARD.

DATOLOOP:
DO dDato = 10/22/2012 TO 10/25/2012:

    /* Batch for TransLogg. Byttes for hver dato */
    RUN batchlogg.p (PROGRAM-NAME(1),
                     "KORR TN " + STRING(dDato) +
                     " " +
                     USERID("dictdb"),
                     OUTPUT iBatchNr).

FOR EACH Butiker NO-LOCK WHERE
    CAN-DO('2,3,4,5,10',STRING(Butiker.Butik)):
    FOR EACH bufBongHode NO-LOCK WHERE
        bufBongHode.ButikkNr = Butiker.Butik AND
        bufBongHode.Dato     = dDato AND 
        bufBongHode.Makulert = 0,
        EACH bufBongLinje NO-LOCK WHERE
            bufBongLinje.B_Id = bufBongHode.B_Id AND
            bufBongLinje.Makulert = FALSE AND
        NOT CAN-FIND(TransLogg WHERE
                 TransLogg.Butik   = bufBongLinje.ButikkNr AND
                 TransLogg.TransNr = bufBongLinje.TransNr AND
                 TransLogg.SeqNr   = bufBongLinje.SeqNr):

        /* Disse er ikke interessante. */
        IF NOT CAN-DO('001,002,003,004,005,006,007,010,011,012,024,025,109',STRING(bufBongLinje.TTId,"999")) THEN
            NEXT.

        IF iBongNr <> bufBongHode.BongNr THEN
        DO:
          iBongNr = bufBongHode.BongNr.
          /*
          DISPLAY
            bufBongHode.ButikkNR
            bufBongHode.Dato
            STRING(bufBongHode.Tid,"HH:MM:SS")
            bufBongHode.Makulert
            bufBongHode.KasseNr
            bufBongHode.BongNr
            WITH WIDTH 300.
          */
          RUN TransLogg.
        END.
    END.
END.
END. /* DATOLOOP */

PROCEDURE TransLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR plWork        AS DEC  NO-UNDO.
DEF VAR pcTekst       AS CHAR NO-UNDO.
DEF VAR piTransNr     AS INT  NO-UNDO.
DEF VAR pcTTId        AS CHAR NO-UNDO.
DEF VAR piTTId        AS INT  NO-UNDO.
DEF VAR piTBId        AS INT  NO-UNDO.
DEF VAR piButikkNr    AS INT  NO-UNDO.
DEF VAR piMButikkNr   AS INT  NO-UNDO.
DEF VAR piEtikettBut  AS INT  NO-UNDO.
DEF VAR bEtikettKasse AS LOG  NO-UNDO.
DEF VAR bEtiBong      AS LOG  NO-UNDO.
DEFINE VARIABLE lMvaKr AS DECIMAL NO-UNDO.

DEFINE VARIABLE iSeq AS INTEGER    NO-UNDO.

DEF BUFFER ovButiker FOR Butiker.

DEF VAR flVaremottak AS LOG NO-UNDO.

/* KonvReg */
pcTekst = "".
{syspara.i 1 2 2 pcTekst} /* RESTPAR */

FIND BongHode EXCLUSIVE-LOCK WHERE
    BongHode.B_Id = bufBongHode.B_Id NO-ERROR.

/* Legger opp transaksjonene i translogg. Kun salgstransaksjoner. */
/* TRANSNR og SEQNR påførs bonglinjene her.                       */
SKAPELSEN:
FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
    BongLinje.B_Id = bufBongHode.B_Id AND
    BongLinje.Makulert = FALSE AND
    /* 009 - Svinntransaksjoner skal ikke tas med her. De blir lagt inn i en lokasjonliste i varetellingsmodulen */
    CAN-DO("001,002,003,004,005,006,007,010,011,012,024,025,109",STRING(BongLinje.TTId,"999")):

    IF AVAILABLE ArtBas  THEN RELEASE ArtBas. 
    IF AVAILABLE ArtPris THEN RELEASE ArtPris.
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = BongLinje.ButikkNr NO-ERROR.
    IF NOT AVAILABLE Butiker THEN
        NEXT SKAPELSEN.

    ASSIGN
        piTTId      = BongLinje.TTId
        piTBId      = BongLinje.TBId
        pcTTId      = STRING(BongLinje.TTId,"999")
        piButikkNr  = BongLinje.ButikkNr
        piMButikkNr = BongLinje.MButikkNr
        .

    /* Setter transaksjonsnummer  */
    IF piTransNr = 0 THEN
      DO:
        FIND LAST TransLogg WHERE
          TransLogg.Butik = BongLinje.ButikkNr
          USE-INDEX TransLogg NO-ERROR.
        IF AVAILABLE TransLogg THEN
          piTransNr = TransLogg.TransNr + 1.
        ELSE
          piTransNr = 1.
      END.
    ELSE
      piTransNr = piTransNr + 1.

    IF DECIMAL(BongLinje.ArtikkelNr) > 0 THEN 
      FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = DECIMAL(BongLinje.ArtikkelNr) NO-ERROR.
    IF AVAILABLE ArtBas THEN
      ASSIGN
        BongLinje.VareGr = ArtBas.VG
        BongLinje.LopeNr = ArtBas.LopNr.
    
    IF AVAILABLE ArtBas THEN
      DO:
        FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
        FIND ArtPris OF ArtBas NO-LOCK WHERE
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
      END.

    TRANSLOGGEN:
    DO:
        /* Oppretter TransLogg */    
        CREATE TransLogg.
        NYTRANSLOGG:
        DO WHILE TRUE ON ERROR UNDO, RETRY:
            ASSIGN TransLogg.Butik        = piButikkNr
                   TransLogg.TransNr      = piTransNr
                   TransLogg.SeqNr        = 1
                   /* Setter inn pekere på transaksjonene */
                   BongLinje.TransNr      = piTransNr
                   BongLinje.SeqNr        = 1
                   NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                piTransNr = piTransNr + 1.
            ELSE LEAVE NYTRANSLOGG.
        END. /* NYTRANSLOGG */

        ASSIGN
               TransLogg.BatchNr      = iBatchNr
               TransLogg.TTId         = BongLinje.TTId
               TransLogg.TBId         = BongLinje.TBId
               TransLogg.ArtikkelNr   = IF AVAILABLE ArtBas
                                          THEN ArtBas.ArtikkelNr
                                          ELSE 0
               TransLogg.Vg           = BongLinje.VareGr
               TransLogg.LopNr        = BongLinje.LopeNr
               TransLogg.Antall       = BongLinje.Antall
               TransLogg.Pris         = BongLinje.LinjeSum / ABSOLUTE(BongLinje.Antall)
               TransLogg.Pris         = IF TransLogg.Pris = ?
                                          THEN 0
                                          ELSE TransLogg.Pris
               TransLogg.RabKr        = (BongLinje.LinjeRab + BongLinje.SubtotalRab) / absolute(BongLinje.Antall)
               TransLogg.RabKr        = IF TransLogg.RabKr = ?
                                          THEN 0
                                          ELSE TransLogg.RabKr
               TransLogg.KundNr       = BongHode.KundeNr

               TransLogg.LevNr        = IF AVAILABLE ArtBas
                                          THEN ArtBas.LevNr
                                          ELSE 0
               TransLogg.OvButik      = IF BongLinje.TTId = 6 THEN piMButikkNr ELSE 0
               TransLogg.OvTransNr    = IF BongLinje.TTId = 6 THEN TransLogg.TransNr ELSE 0
               TransLogg.BongId       = BongLinje.BongNr
               TransLogg.BongLinjeNr  = BongLinje.LinjeNr
               TransLogg.KassaNr      = BongLinje.KasseNr
               TransLogg.ForsNr       = BongHode.KassererNr
               TransLogg.Plukket      = IF BongLinje.TTId = 6 THEN TRUE ELSE FALSE
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
               TransLogg.VVareKost    = BongLinje.VVareKost / ABS(BongLinje.Antall)
               TransLogg.VVareKost    = IF TransLogg.VVareKost = ? THEN 0 ELSE Translogg.VVareKost
               TransLogg.SattVVarekost = (IF AVAILABLE ArtBas AND ArtBas.Lager = TRUE 
                                            THEN FALSE 
                                          ELSE IF CAN-DO("1,3,10",STRING(Translogg.TTId))
                                            THEN TRUE /* Skal ikke regnes om ved opp. av statistikker. */
                                          ELSE FALSE)
               TransLogg.KalkylePris  = IF AVAILABLE ArtPris
                                          THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                          ELSE Translogg.KalkylePris
               TransLogg.Varekost     = IF AVAILABLE ArtPris
                                          THEN ArtPris.Varekost[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                          ELSE TransLogg.Varekost
               TransLogg.Pris         = (IF BongLinje.TTId = 5 
                                          THEN  TransLogg.Varekost 
                                          ELSE TransLogg.Pris)                                          
               TransLogg.Mva          = (dec(BongLinje.MvaKr / ABSOLUTE(BongLinje.Antall)))
               Translogg.Mva          = (IF Translogg.Mva = ? THEN 0 ELSE Translogg.Mva)
               TransLogg.Mva%         = (IF AVAILABLE ArtPris
                                           THEN ArtPris.Mva%[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                           ELSE (TransLogg.Mva / (TransLogg.Pris - TransLogg.Mva - TransLogg.RabKr)) * 100)
               Translogg.Mva%         = (IF Translogg.Mva% = ? THEN 0 ELSE Translogg.Mva%)
               Translogg.Generellrabatt         = BongLinje.Generellrabatt / absolute(BongLinje.Antall)
               Translogg.Tilbudsrabatt          = BongLinje.Tilbudsrabatt / absolute(BongLinje.Antall)
               Translogg.MixMatchRabatt         = BongLinje.MixMatchRabatt / absolute(BongLinje.Antall)
               Translogg.Medlemsrabatt          = BongLinje.Medlemsrabatt / absolute(BongLinje.Antall)
               Translogg.Kunderabatt            = BongLinje.Kunderabatt / absolute(BongLinje.Antall)
               Translogg.Personalrabatt         = BongLinje.Personalrabatt / absolute(BongLinje.Antall)
               Translogg.AlternativPrisRabatt   = BongLinje.AlternativPrisRabatt / absolute(BongLinje.Antall)
               Translogg.ManuelEndretPrisRabatt = BongLinje.ManuelEndretPrisRabatt / absolute(BongLinje.Antall)
               .
        /* Overstyrer for 002-Brekkasje, 005-Varekjøp, 006-Overføring og 011-Internt forbruk. */
        /* Disse transaksjonene skal håndteres til varekost.                  */
        IF CAN-DO('002,005,006,011',STRING(BongLinje.TTId,"999")) THEN 
        DO:
          ASSIGN
            TransLogg.Pris          = TransLogg.Varekost
            TransLogg.RabKr         = 0
            TransLogg.SubtotalRab   = 0
            TransLogg.VVareKost     = 0
            TransLogg.SattVVarekost = FALSE
            TransLogg.Mva           = 0
            TransLogg.Mva%          = 0
            .
        END.

        ASSIGN TransLogg.Storl        = FixStorl(BongLinje.Storrelse)
               TransLogg.TilStorl     = TransLogg.Storl
               .
    END. /* TRANSLOGGEN */
END. /* SKAPELSEN */

END PROCEDURE.

FUNCTION FixStorl RETURNS CHARACTER
  ( pcStorl AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

 ASSIGN
    pcStorl = TRIM(pcStorl)
    pcStorl = CAPS(pcStorl)
    pcStorl = IF (LENGTH(pcStorl) = 1 OR 
                 LENGTH(pcStorl) = 3
                 ) 
                then " " + pcStorl
                else pcStorl.          

  /* Bytter ut eventuelle comma med punkt. */
  IF INDEX(pcStorl,",") <> 0 THEN
    OVERLAY(pcStorl, INDEX(pcStorl,","), 1, "CHARACTER") = ".".

  RETURN pcStorl.   /* Function return value. */

END FUNCTION.
