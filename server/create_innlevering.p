/*------------------------------------------------------------------------------
  Purpose:     Opprett
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM  ihBuffer      AS HANDLE NO-UNDO.
DEF OUTPUT PARAM oiBatchNr     AS INT NO-UNDO.
DEF OUTPUT PARAM ofAntLevert   AS DEC NO-UNDO.
DEF OUTPUT PARAM ofVerdiLevert AS DEC NO-UNDO.
DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR fArtikkelNr     AS DEC NO-UNDO.
DEF VAR cArtikkelEti    AS CHAR NO-UNDO.
DEF VAR cEtiketter      AS CHAR NO-UNDO.
DEF VAR cAntallEti      AS CHAR NO-UNDO.
DEF VAR cIndividNr      AS CHAR NO-UNDO.

DEF VAR cStorlek        AS CHAR NO-UNDO. /* strl i find, går ej med chvsFlexGrid */
DEF VAR iRow            AS INTE NO-UNDO.
DEF VAR iLevert         AS INTE NO-UNDO.
DEF VAR iTransNr        AS INTE NO-UNDO.
DEF VAR cEtiketterTmp   AS CHARACTER  NO-UNDO.
DEF VAR cAntallEtiTmp   AS CHARACTER  NO-UNDO.
DEF VAR iCount          AS INTEGER    NO-UNDO.
DEF VAR dIndividNr      LIKE Individ.IndividNr NO-UNDO.
DEF VAR cIndividNrTmp   AS CHARACTER  NO-UNDO.
DEF VAR iIndividBatchNr LIKE Batchlogg.BatchNr NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE Bestilt NE 0 NO-LOCK BY ArtikkelNr").
hQuery:QUERY-OPEN().

TRANSBLOKK:
DO TRANSACTION ON ERROR UNDO, LEAVE:                    

  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

    IF ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE NE fArtikkelNr THEN DO:
      FIND ArtBas WHERE ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) NO-LOCK NO-ERROR.
      IF NOT AVAIL ArtBas THEN DO:
        ocReturn = "Finner ikke artikkel " + STRING(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + " ved opprettelse av innleveranse".
        UNDO, LEAVE.
      END.

      ASSIGN cEtiketterTmp = ""
             cAntallEtiTmp = ""
             cIndividNrTmp = "".
    END.

    IF fArtikkelNr = 0 THEN 
      RUN batchlogg.w (program-name(1),"Varemottak: " + STRING(ArtBas.Artikkelnr),output oiBatchNr).
    ELSE IF ArtBas.ArtikkelNr NE fArtikkelNr THEN DO:
      RUN genStrekKode.p(ArtBas.ArtikkelNr,oiBatchNr,"TRANSLOGG").
  
      IF cEtiketterTmp <> "" THEN DO:
        DO iCount = 1 TO NUM-ENTRIES(cEtiketterTmp):
          FIND StrKonv WHERE StrKonv.Storl = ENTRY(iCount,cEtiketterTmp) NO-LOCK NO-ERROR.
          IF AVAIL StrKonv THEN DO:
            FIND FIRST StrekKode OF ArtBas WHERE StrekKode.KodeType = 1 AND 
                                                 StrekKode.StrKode = StrKonv.StrKode AND
                                                 NOT StrekKode.Kode BEGINS "02" NO-LOCK NO-ERROR.
            IF NOT AVAIL StrekKode THEN
                FIND FIRST StrekKode OF ArtBas WHERE StrekKode.KodeType = 1 AND 
                                                     StrekKode.StrKode = StrKonv.StrKode NO-LOCK NO-ERROR.
            ASSIGN ENTRY(iCount,cEtiketterTmp) = IF AVAIL StrekKode THEN StrekKode.Kode ELSE "".
          END.
        END.
        ASSIGN cArtikkelEti = cArtikkelEti + (IF cArtikkelEti <> "" THEN CHR(1) ELSE "") + STRING(ArtBas.ArtikkelNr)
               cEtiketter   = cEtiketter   + (IF cEtiketter   <> "" THEN CHR(1) ELSE "") + cEtiketterTmp
               cAntallEti   = cAntallEti   + (IF cAntallEti   <> "" THEN CHR(1) ELSE "") + cAntallEtiTmp
               cIndividNr   = cIndividNr   + (IF cIndividNr   <> "" THEN CHR(1) ELSE "") + cIndividNrTmp.
      END.
    END.


    IF ArtBas.Inn_Dato = ? THEN DO:
      FIND CURRENT ArtBas EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF LOCKED ArtBas THEN DO:
        ocReturn = "Artikkel " + STRING(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + " er ikke tilgjengelig for oppdatering".
        UNDO, LEAVE.
      END.
      ArtBas.Inn_Dato = TODAY.
    END.
    IF ArtBas.IndividType > 0 THEN DO:
      FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
      FIND HuvGr OF VarGr  NO-LOCK NO-ERROR.
      FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
      FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
      iIndividBatchNr = oiBatchNr.
    END.

    FIND Butiker WHERE Butiker.Butik = INT(ihBuffer:BUFFER-FIELD("BestiltButikkNr"):BUFFER-VALUE) NO-LOCK NO-ERROR.
    FIND ArtPris OF ArtBas WHERE ArtPris.ProfilNr = Butiker.ProfilNr NO-LOCK NO-ERROR.
    IF NOT AVAIL ArtPris THEN DO:
      FIND Butiker WHERE Butiker.Butik = INT(ihBuffer:BUFFER-FIELD("CLButikkNr"):BUFFER-VALUE) NO-LOCK NO-ERROR.
      FIND ArtPris OF ArtBas WHERE ArtPris.ProfilNr = Butiker.ProfilNr NO-LOCK NO-ERROR.
      IF NOT AVAIL ArtPris THEN
        FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
    END.

    /* Transaksjonsnummer for butikken. */
    FIND LAST TransLogg NO-LOCK WHERE
              TransLogg.Butik = INT(ihBuffer:BUFFER-FIELD("BestiltButikkNr"):BUFFER-VALUE) USE-INDEX TransLogg NO-ERROR.

    cStorlek = ihBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE.
    RUN FiksStorl (INPUT-OUTPUT cStorlek).

    ASSIGN iTransNr = IF AVAILABLE TransLogg THEN TransLogg.TransNr + 1 ELSE 1
           iLevert  = INT(ihBuffer:BUFFER-FIELD("Bestilt"):BUFFER-VALUE).

    IF ArtBas.IndividType > 0 THEN
      FIND StrKonv WHERE StrKonv.Storl = cStorlek NO-LOCK NO-ERROR.

    /* Oppretter transaksjon */
    LAG_TRANS:
    DO iCount = 1 TO (IF ArtBas.IndividType > 0 THEN iLevert ELSE 1):
      /* Sjekker at transnr er ledig */
      IF CAN-FIND(TransLogg WHERE
                  TransLogg.Butik   = INT(ihBuffer:BUFFER-FIELD("BestiltButikkNr"):BUFFER-VALUE) AND
                  TransLogg.TransNr = iTransNr) THEN
        NESTE_NR:
        DO WHILE TRUE:
          ASSIGN iTransNr = iTransNr + 1.
          IF CAN-FIND(TransLogg WHERE
                      TransLogg.Butik   = INT(ihBuffer:BUFFER-FIELD("BestiltButikkNr"):BUFFER-VALUE) AND
                      TransLogg.TransNr = iTransNr) THEN
            NEXT NESTE_NR.
          ELSE
            LEAVE NESTE_NR.
        END. /* NESTE_NR */

      IF ArtBas.IndividType > 0 AND AVAIL StrKonv THEN
        RUN SkapaIndivid IN THIS-PROCEDURE (INPUT oiBatchNr,INPUT StrKonv.Strkode,INPUT StrKonv.Storl, OUTPUT dIndividNr).

      CREATE TransLogg.
      ASSIGN TransLogg.Butik         = ihBuffer:BUFFER-FIELD("BestiltButikkNr"):BUFFER-VALUE
             TransLogg.TransNr       = iTransNr
             TransLogg.SeqNr         = 1.
      ASSIGN TransLogg.BatchNr       = oiBatchNr
             TransLogg.KundNr        = 0
             TransLogg.TTId          = 5 /* Varekjøp */
             TransLogg.TBId          = 1
             TransLogg.ArtikkelNr    = ArtBas.ArtikkelNr
             TransLogg.LevNr         = ArtBas.LevNr
             TransLogg.BongId        = 0
             TransLogg.BongLinjeNr   = 0
             TransLogg.KassaNr       = 0
             TransLogg.Vg            = ArtBas.Vg
             TransLogg.LopNr         = ArtBas.LopNr
             TransLogg.Antall        = IF ArtBas.IndividType > 0 THEN 1 ELSE iLevert
             TransLogg.Pris          = ArtPris.VareKost[1]
             TransLogg.RabKr         = 0
             TransLogg.Mva           = 0
             TransLogg.Plukket       = TRUE /* Skal ikke ut på plukkliste */
             TransLogg.Dato          = TODAY
             TransLogg.Tid           = TIME
             TransLogg.SattVVareKost = FALSE /* TRUE */
             TransLogg.BestNr        = 99 /* förslag när vi gör Forenklet varemottak */
             TransLogg.Postert       = FALSE
             TransLogg.IndividNr     = dIndividNr                                             
             TransLogg.Storl         = cStorlek
             /*
             TransLogg.vVareKost     = ArtPris.Varekost[1]
             TransLogg.VareKost      = ArtPris.Varekost[1]
             */
             cEtiketterTmp          = cEtiketterTmp + (IF cEtiketterTmp <> "" THEN "," ELSE "") + TransLogg.Storl
             cAntallEtiTmp          = cAntallEtiTmp + (IF cAntallEtiTmp <> "" THEN "," ELSE "") + STRING(TransLogg.Antall)
             cIndividNrTmp          = cIndividNrTmp + (IF cIndividNrTmp <> "" THEN "," ELSE "") + STRING(TransLogg.IndividNr)

             ofAntLevert            = ofAntLevert + TransLogg.Antall
             ofVerdiLevert          = ofVerdiLevert + TransLogg.Antall * TransLogg.Pris
             .

    /* Här skakall vi fylla en TT med värden
            create TT_Eti.
            artikkelnr =
            butik =
            strkode =
            antal   =
    */       
    END. /* LAG_TRANS */

    RELEASE TransLogg.
    
    RUN batchstatus.p (oiBatchNr, 2).

    fArtikkelNr = ArtBas.ArtikkelNr.
    hQuery:GET-NEXT().
  END.
END. /* TRANSBLOKK TRANSACTION */

RUN genStrekKode.p(fArtikkelNr,oiBatchNr,"TRANSLOGG").

IF cEtiketterTmp <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cEtiketterTmp):
    FIND StrKonv WHERE StrKonv.Storl = ENTRY(iCount,cEtiketterTmp) NO-LOCK NO-ERROR.
    IF AVAIL StrKonv THEN DO:
      FIND FIRST StrekKode OF ArtBas WHERE StrekKode.KodeType = 1 AND 
                                           StrekKode.StrKode = StrKonv.StrKode AND
                                           NOT StrekKode.Kode BEGINS "02" NO-LOCK NO-ERROR.
      IF NOT AVAIL StrekKode THEN
          FIND FIRST StrekKode OF ArtBas WHERE StrekKode.KodeType = 1 AND 
                                               StrekKode.StrKode = StrKonv.StrKode NO-LOCK NO-ERROR.
      ASSIGN ENTRY(iCount,cEtiketterTmp) = IF AVAIL StrekKode THEN StrekKode.Kode ELSE "".
    END.
  END.
  ASSIGN cArtikkelEti = cArtikkelEti + (IF cArtikkelEti <> "" THEN CHR(1) ELSE "") + STRING(fArtikkelNr)
         cEtiketter   = cEtiketter   + (IF cEtiketter   <> "" THEN CHR(1) ELSE "") + cEtiketterTmp
         cAntallEti   = cAntallEti   + (IF cAntallEti   <> "" THEN CHR(1) ELSE "") + cAntallEtiTmp
         cIndividNr   = cIndividNr   + (IF cIndividNr   <> "" THEN CHR(1) ELSE "") + cIndividNrTmp.
END.

IF cArtikkelEti NE "" THEN
  ocReturn = "etikett" + 
             cArtikkelEti + "|" + 
             cEtiketter   + "|" + 
             cAntallEti   + "|" + 
             cIndividNr.


DELETE OBJECT hQuery.

PROCEDURE SkapaIndivid:

DEF INPUT  PARAMETER iBatchNr    AS INTEGER    NO-UNDO.
DEF INPUT  PARAMETER iStrKode   LIKE StrKonv.StrKode   NO-UNDO.
DEF INPUT  PARAMETER cStorl     LIKE StrKonv.Storl     NO-UNDO.
DEF OUTPUT PARAMETER dIndividNr LIKE Individ.individnr NO-UNDO.
DEF        VAR  dSeqNr      AS DECIMAL            NO-UNDO.

FIND LAST Individ WHERE Individ.butnr = Butiker.Butik USE-INDEX SeqNr NO-LOCK NO-ERROR.
ASSIGN dSeqnr = IF NOT AVAIL Individ THEN 1 ELSE Individ.SeqNr + 1.
CREATE Individ.
REPEAT:
    ASSIGN dIndividNr         = DECI(STRING(Butiker.butik) + STRING(dSeqnr))
           Individ.butnr      = Butiker.Butik
           Individ.SeqNr      = dSeqNr
           Individ.ArtikkelNr = ArtBas.ArtikkelNr
           Individ.StrKode    = iStrKode
           Individ.individnr  = dIndividNr NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
        LEAVE.
    ASSIGN dSeqNr = dSeqNr + 1.
END.
ASSIGN Individ.AvdelingNr    = HuvGr.AvdelingNr
       Individ.Beskr         = ArtBas.Beskr
       Individ.Hg            = ArtBas.Hg
       Individ.IndividType   = ArtBas.IndividType
       Individ.LevNamn       = LevBas.Levnamn
       Individ.levnr         = ArtBas.LevNr
       Individ.NyVare        = TRUE
       Individ.Storl         = cStorl
       Individ.StrKode       = iStrKode
       Individ.Vg            = ArtBas.Vg
       Individ.VmBeskrivelse = IF AVAIL Varemerke THEN Varemerke.Beskrivelse ELSE ""
       Individ.VMId          = ArtBas.VMId
       Individ.BatchNr       = iBatchNr.

END PROCEDURE.


PROCEDURE FiksStorl:
  def INPUT-output parameter wStorl as char NO-UNDO.

  DEF VAR wDecimaler as CHAR NO-UNDO.

  {syspara.i 1 1 16 wDecimaler}

  assign
     wStorl = trim(wStorl)
     wStorl = caps(wStorl)
     wStorl = if (length(wStorl) = 1 or
                  length(wStorl) = 3
                  )
                 then " " + wStorl
                 else wStorl.

  /* Bytter ut eventuelle comma med punkt. */
  if index(wStorl,",") <> 0 then
    OVERLAY(wStorl, index(wStorl,","), 1, "CHARACTER") = ".".

  /* Sjekker om det er benyttet gyldige tegn i halvnummer. */
  /* Er det ikke det, tas halvnummeret bort.               */
  if NUM-ENTRIES(wStorl,".") = 2 then
    DO:
      if NOT CAN-DO(wDecimaler,ENTRY(2,wStorl,".")) then
        wStorl = ENTRY(1,wStorl,".").
    END.

  RETURN wStorl.   /* Function return value. */

END PROCEDURE.

