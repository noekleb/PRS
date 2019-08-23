/* Registrer innleveranse fra pakkseddel
   Parameter:  <PkSdlId>;<brukerid>
   Opprettet: 09.08.07 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fPkSdlId        AS DEC    NO-UNDO.
DEF VAR cUserId         AS CHAR   NO-UNDO.
DEF VAR cBestNrListe    AS CHAR   NO-UNDO.
DEF VAR h_PrisKo        AS HANDLE NO-UNDO.
DEF VAR fDbKr           AS DEC    NO-UNDO.
DEF VAR fDb%            AS DEC    NO-UNDO.
DEF VAR fMvaKr          AS DEC    NO-UNDO.
DEF VAR fRab1Kr         AS DEC    NO-UNDO.
DEF VAR fRab1%          AS DEC    NO-UNDO.
DEF VAR fEuroKurs       AS DEC    NO-UNDO.
DEF VAR fFrakt%         AS DEC    NO-UNDO.
DEF VAR fFrakt          AS DEC    NO-UNDO.
DEF VAR fVarekost       AS DEC    NO-UNDO.
DEF VAR fInnkjopsPris   AS DEC    NO-UNDO.
DEF VAR fPris           AS DEC    NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR iCurrBestNr     AS INT    NO-UNDO.
DEF VAR iMottaksId      AS INT    NO-UNDO INIT 1.
DEF VAR cButliste       AS CHAR   NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE bAktiver AS LOG NO-UNDO.
DEFINE VARIABLE cNettButikkLager AS CHARACTER NO-UNDO.

/* ASSIGN fPkSdlId   = DEC(ENTRY(1,icParam,";")) */
/*        cUserId    = ENTRY(2,icParam,";")      */
/*        .                                      */

cUserId = icParam.

{syspara.i 150 1 3 cNettButikkLager}

{syspara.i 2 1 1 fEuroKurs DECIMAL}
IF fEuroKurs = ? OR fEuroKurs = 0 THEN
  fEuroKurs = 0.5.

{syspara.i 150 1 18 cTekst}
IF CAN-DO('1,J,Ja,Y,Yes,True',cTekst) THEN 
  bAktiver = TRUE.
ELSE 
  bAktiver = FALSE.  

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE AntLevert > 0 AND MottaksId = 0 BY BestNr").
hQuery:QUERY-OPEN().

DO ON ERROR UNDO, LEAVE:

  hQuery:GET-FIRST().
  IF NOT ihBuffer:AVAIL THEN DO:
    ocReturn = "Ingen varer med levert antall (og som ikke allerede er mottatt) er valgt".
    UNDO, LEAVE.  
  END. 
  
  FIND LAST PkSdlMottak NO-LOCK
       WHERE PkSdlMottak.PkSdlId = DECIMAL(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE)
       NO-ERROR.
  IF AVAIL PkSdlMottak THEN
    iMottaksId = PkSdlMottak.MottaksId + 1.
  CREATE PkSdlMottak.
  ASSIGN PkSdlMottak.PkSdlId     = ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE
         PkSdlMottak.MottaksId   = iMottaksId
         PkSdlMottak.MottattDato = TODAY
         PkSdlMottak.MottattTid  = TIME
         .

  /* Behandler og lager liste med bestillingsnr som skal innleveres. */
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    /*
    FIND PkSdlLinje EXCLUSIVE-LOCK 
         WHERE ROWID(PkSdlLinje) = TO-ROWID(STRING(ihBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE))
         NO-ERROR.
    */
    FIND PkSdlLinje EXCLUSIVE-LOCK WHERE 
        PkSdlLinje.PkSdlId      = DEC(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) AND 
        PkSdlLinje.PkSdlLinjeId = INT(ihBuffer:BUFFER-FIELD("PkSdlLinjeId"):BUFFER-VALUE)
        NO-ERROR.
         
    IF NOT AVAIL PkSdlLinje THEN DO:
      ocReturn = "Pakkseddel-linje ikke tilgjengelig for oppdatering".
      UNDO, LEAVE.
    END.
    
    IF NOT CAN-DO(cButliste,STRING(PkSdlLinje.ButikkNr)) THEN
      cButliste = cButliste + (IF cButliste NE "" THEN "," ELSE "") + STRING(PkSdlLinje.ButikkNr).
    IF PkSdlLinje.BestNr NE iCurrBestNr THEN DO:
      ASSIGN cBestNrListe = cBestNrListe + STRING(PkSdlLinje.BestNr) + "|"
             fPkSdlId     = ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE.
      RUN setBestPris (PkSdlLinje.BestNr, OUTPUT ocReturn).
      IF ocReturn NE "" THEN UNDO, LEAVE.
    END.
    ASSIGN iCurrBestNr          = PkSdlLinje.BestNr
           PkSdlLinje.MottaksId = PkSdlMottak.MottaksId. 
           
    /* Gjøres varemottak på nettbutikkens lager, skal tilhørende artikkel aktiveres i nettbutikk. PArameterstyrt. */
    IF bAktiver AND CAN-DO(cNettButikkLager,STRING(PkSdlLinje.ButikkNr)) THEN 
      RUN aktiverWebButikkArtikkel.p (PkSdlLinje.ArtikkelNr).
    
    hQuery:GET-NEXT().
  END.
  
  RUN ordre_best_full_innlev.p ("best;" + cUserId + ";" + TRIM(cBestNrListe,"|"),
                                ?,icSessionId,OUTPUT ocReturn,OUTPUT obOk).

  IF obOk THEN DO:
    FIND PkSdlHode EXCLUSIVE-LOCK
         WHERE PkSdlHode.PkSdlId = fPkSdlId
         NO-ERROR.
    IF NOT AVAIL PkSdlHode THEN DO:
      ocReturn = "Finner ikke pakklisteid " + STRING(fPkSdlId) + " - programfeil".
      UNDO,LEAVE.
    END.
    FIND FIRST PkSdlLinje NO-LOCK
         OF PkSdlHode
         WHERE PkSdlLinje.AntLevert > 0
           AND PkSdlLinje.MottaksId = 0
         NO-ERROR.
    IF AVAIL PkSdlLinje THEN
      PkSdlHode.PkSdlStatus = 15.
    ELSE
      PkSdlHode.PkSdlStatus = 20.
    
  END.
  ELSE UNDO, LEAVE.
END.

DELETE OBJECT hQuery NO-ERROR.

/* Finner riktig StrKode fra pakkseddellinjen */
FUNCTION getSTrKode RETURNS INTEGER (INPUT iiBestNr AS INT,INPUT iiButikkNr AS INT,INPUT icStorl AS CHARACTER):
  DEFINE VARIABLE iiStrKode AS INTEGER NO-UNDO.
  
  ASSIGN 
    cTekst    = TRIM(icStorl)
    iiStrKode = 0.

  FIND FIRST StrKonv NO-LOCK WHERE StrKonv.Storl = cTekst NO-ERROR.
  IF AVAILABLE StrKonv THEN 
  DO: 
    FIND FIRST PkSdlLinje NO-LOCK
       WHERE PkSdlLinje.PkSdlId  = fPkSdlId
         AND PkSdlLinje.BestNr   = iiBestNr
         AND PkSdlLinje.ButikkNr = iiButikkNr
         AND PkSdlLinje.StrKode  = StrKonv.StrKode
       NO-ERROR.
    IF AVAILABLE PkSdlLinje THEN 
      iiStrKode = PkSdlLinje.StrKode.
  END.
  
  IF iiStrKode = 0 THEN 
  DO:
    RUN bibl_fixstorl.p (cTekst,?,'',OUTPUT ocReturn,OUTPUT obOk).
    cTekst = ocReturn.
    FIND FIRST StrKonv NO-LOCK WHERE StrKonv.Storl = cTekst NO-ERROR.
    IF AVAILABLE StrKonv THEN 
    DO: 
      FIND FIRST PkSdlLinje NO-LOCK
         WHERE PkSdlLinje.PkSdlId  = fPkSdlId
           AND PkSdlLinje.BestNr   = iiBestNr
           AND PkSdlLinje.ButikkNr = iiButikkNr
           AND PkSdlLinje.StrKode  = StrKonv.StrKode
         NO-ERROR.
      IF AVAILABLE PkSdlLinje THEN 
        iiStrKode = PkSdlLinje.StrKode.
    END.
  END.
      
  RETURN iiStrKode.
END FUNCTION.

FUNCTION LevAntall RETURNS DECIMAL (INPUT iiBestNr AS INT,INPUT iiButikkNr AS INT,INPUT iiStrKode AS INT):
  DEFINE VARIABLE pfiAntall AS DECIMAL NO-UNDO.
  
  pfiAntall = 0.
  FOR EACH PkSdlLinje NO-LOCK
       WHERE PkSdlLinje.PkSdlId  = fPkSdlId
         AND PkSdlLinje.BestNr   = iiBestNr
         AND PkSdlLinje.ButikkNr = iiButikkNr
         AND PkSdlLinje.StrKode  = iiStrKode:
    pfiAntall = pfiAntall + PkSdlLinje.AntLevert.
  END.
       
  IF pfiAntall > 0 THEN
    RETURN pfiAntall.
  ELSE DO:
    RETURN ?.
  END.
END FUNCTION.

FUNCTION InnlevertButikk RETURNS LOGICAL (INPUT iiButikkNr AS INT):
  IF cButliste NE "" AND NOT CAN-DO(cButliste,STRING(iiButikkNr)) THEN
    RETURN NO.
  ELSE RETURN YES.
END FUNCTION.

PROCEDURE setBestPris: /*PrisOppdatering:*/
  /* Prosedyren kalles fra ordre_best_full_innlev.p (hvis den finnes i kallende prosedyre) og erstatter prisoppdateringen der */

  DEF INPUT PARAM iiBestNr  AS INT NO-UNDO.
  DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO.

  DEF VAR wArtBasRecid AS RECID NO-UNDO.
  DEF VAR wSkjerm      AS CHAR  NO-UNDO.
  DEFINE VARIABLE iOrdreType AS INTEGER NO-UNDO.

  FIND FIRST BestHode NO-LOCK
       WHERE BestHode.BestNr = iiBestNr
       NO-ERROR.
  IF NOT AVAIL BestHode THEN DO:
    ocReturn = "Finner ikke bestillingshode ved prisoppdatering i varemottak. Programfeil: " + PROGRAM-NAME(1).
    RETURN.
  END.
      
  FIND FIRST ArtBas OF BestHode NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN DO:
    ocReturn = "Finner ikke artikkel ved prisoppdatering i varemottak. Programfeil: " + PROGRAM-NAME(1).
    RETURN.
  END.
  wArtBasRecid = RECID(ArtBas).

  /* Oppdaterer priser på bestillingen med priser fra pakkseddelen. */
  FOR EACH BestPris EXCLUSIVE-LOCK 
      WHERE BestPris.BestNr   = BestHode.BestNr 
        AND BestPris.BestStat = BestHode.BestStat:

    FIND FIRST PkSdlPris EXCLUSIVE-LOCK
         WHERE PkSdlPris.PkSdlId    = fPkSdlId
           AND PkSdlPris.ArtikkelNr = BestPris.ArtikkelNr
         NO-ERROR.
    IF NOT AVAIL PkSdlPris THEN DO:
      ocReturn = "Finner ikke prispost ved prisoppdatering i varemottak. Programfeil: " + PROGRAM-NAME(1).
      RETURN.
    END.
    /* TN 21/12-17 Henter pakkseddellhode for å kunne teste på ordretype. */
    FIND PkSdlHode OF PkSdlPris NO-LOCK NO-ERROR.
    IF AVAILABLE PkSdlHode THEN 
    DO:
        ASSIGN 
            iOrdreType = INT(TRIM(ENTRY(1,PkSdlHode.MeldingFraLev,CHR(10)))) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            iOrdretype = 0. 
    END.
    ELSE 
        iOrdretype = 0.
    
    /* Pakkseddelpris er redigert og skal oppdateres inn på bestpris før varemottak.          */
    /*  TN 17/10-08 Det er alltid denne prisen som skal benyttes. Hvis ikke den er overstyrt, */
    /* viser den det samme som ligger i bestpris, men alikevel er det riktig å ta de          */
    /* prisfelt som bruker ser i skjermen og benytte dem.                                     */
    /* IF PkSdlPris.OverstyrPris THEN */
    OVERSTYRPRIS:
    DO: 
        IF PkSdlPris.VareKost = 0 THEN
            PkSdlPris.VareKost = PkSdlPris.NyVareKost.

        ASSIGN 
            fInnkjopsPris = PkSdlPris.NyInnkjopsPris
            fFrakt        = PkSdlPris.NyFrakt
            fRab1%        = PkSdlPris.NyRab1%
            fVarekost     = PkSdlPris.NyVarekost
            fPris         = PkSdlPris.NyPris
            fDb%          = PkSdlPris.NyDB%               
            fMvaKr        = PkSdlPris.NyPris - PkSdlPris.NyPris / (1 + BestPris.Mva% / 100)
            fDbKr         = PkSdlPris.NyPris - fMvaKr - PkSdlPris.NyVarekost
            fRab1Kr       = fRab1% * PkSdlPris.NyInnkjopsPris / 100
            fFrakt%       = PkSdlPris.NyFrakt / (PkSdlPris.NyInnkjopsPris - fRab1Kr) * 100.
        
        /* Oppdaterer Bestpris */
        ASSIGN
             BestPris.ValPris      = fInnkjopsPris
             BestPris.InnkjopsPris = fInnkjopsPris
             BestPris.Rab1Kr       = fRab1Kr
             BestPris.Rab1%        = fRab1% 
             BestPris.Frakt%       = fFrakt%
             BestPris.Frakt        = fFrakt
             BestPris.Varekost     = fVarekost
             BestPris.DbKr         = fDbKr  
             BestPris.Db%          = fDb%   
             BestPris.MvaKr        = fMvaKr 
             BestPris.Pris         = fPris.
    END. /* OVERSTYRPRIS */
  END. /* PRISOPPDATERING */
END PROCEDURE.
