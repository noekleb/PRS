
/*------------------------------------------------------------------------
    File        : asPkSdl_AngreOverforing.p
    Purpose     : 

    Syntax      :

    Description : Bare pakksedler som er et resultat av en overføring kan angres.
                  Disse er merket med opphav = 4.

    Author(s)   : 
    Created     : Mon Feb 27 18:31:18 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER lPkSdlId AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER iButNr   AS INTEGER   NO-UNDO. /* Opprinnelig overført fra.               */
DEFINE INPUT  PARAMETER iTilBut  AS INTEGER   NO-UNDO. /* Butikk som skal ha overføringen.        */
DEFINE OUTPUT PARAMETER bOk      AS LOG INITIAL TRUE NO-UNDO.
DEFINE OUTPUT PARAMETER cReturn  AS CHARACTER NO-UNDO.

DEFINE VARIABLE iFeilbut AS INTEGER  NO-UNDO. /* Butikk som feilaktig mottok overføring. */
DEFINE VARIABLE iSentralLager AS INTEGER NO-UNDO.
DEFINE VARIABLE iBatchNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE lKundeNr LIKE Kunde.KundeNr NO-UNDO.
DEFINE VARIABLE ihBuffer AS HANDLE NO-UNDO.
DEFINE VARIABLE iDummy AS INTEGER NO-UNDO.
DEFINE VARIABLE iOrgOvbut AS INTEGER NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE TT_OvBuffer NO-UNDO LIKE OvBuffer.
DEFINE            TEMP-TABLE tmpPksdlLinje NO-UNDO LIKE PkSdlLinje.

DEFINE BUFFER bufOvBunt FOR OvBunt.
DEFINE BUFFER feilButiker FOR Butiker.
DEFINE BUFFER bufFakturaHode FOR FakturaHode.

{syspara.i 22 20 1 iSentrallager INT}

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/* Henter pakkseddel, pakkseddellinje og feil butikk. */
FIND LAST PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlId = lPkSdlId NO-ERROR.
IF AVAILABLE PkSdlHode THEN 
    FIND FIRST PkSdlLinje OF PkSdlHode NO-ERROR.
IF AVAILABLE PkSdlLinje THEN 
    FIND feilButiker NO-LOCK WHERE 
        feilButiker.Butik = PkSdlLinje.ButikkNr NO-ERROR.

/* Setter iFeilBut */
IF AVAILABLE PkSdlLinje THEN 
    ASSIGN 
        iFeilbut = PkSdlLinje.ButikkNr
        .
ELSE 
    iFeilBut = 0.

/* Sjekker om pakkseddel er gyldig og ikke mottatt. */
RUN PkSdlValiderAngre.p (lPkSdlId, iButNr, OUTPUT iOrgOvbut, OUTPUT bOk, OUTPUT cReturn).
IF bOk = FALSE THEN
    RETURN cReturn. 

/* Sjekker at parametre er gyldige. */
RUN validerParametre(OUTPUT bOk, OUTPUT cReturn).
IF bOk = FALSE THEN
    RETURN cReturn.
         
/* Krediterer faktura */
RUN krediterFaktura(OUTPUT bOk, OUTPUT cReturn).         
IF bOk = FALSE THEN
    RETURN cReturn.

/* Overfør varene tilbake til der de kom fra. */
RUN overfortilbake(OUTPUT bOk, OUTPUT cReturn).
IF bOk = FALSE THEN
    RETURN cReturn.

/* Makulerer pakkseddel som var feil. */
RUN makulerPakkseddel(OUTPUT bOk, OUTPUT cReturn). 
IF bOk = FALSE THEN
    RETURN cReturn.

/* Opprett ny overføring til riktig butikk, opprett pakkseddel og og fakturer. */
RUN overforTilbut (OUTPUT bOk, OUTPUT cReturn).
IF bOk = FALSE THEN
    RETURN cReturn.

RETURN cReturn.
    
/* **********************  Internal Procedures  *********************** */

PROCEDURE krediterFaktura:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER pbOk AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER pcReturn AS CHARACTER NO-UNDO.

    /* --------------- 1. Faktura skal krediteres. -------------------- */
    FIND FIRST FakturaHode NO-LOCK WHERE 
        FakturaHode.ButikkNr  = iButNr AND 
        FakturaHode.FakturaNr = DEC(PkSdlHode.PkSdlNr) NO-ERROR.
    IF AVAILABLE FakturaHode THEN
    FAKTURA_BEHANDLING:
    DO: 
        ASSIGN 
            lKundeNr = FakturaHode.KundeNr
            .
        FIND FIRST KundeReskontr NO-LOCK WHERE
            kundeReskontr.Bilagstype = 1 AND
            KundeReskontr.FakturaNr  = FakturaHode.FakturaNr NO-ERROR.
        IF NOT AVAILABLE Kundereskontr THEN
        DO:
            ASSIGN
                pbOk    = FALSE 
                cReturn = 'Finner ikke pakkseddelens fakturareskontropost. Anmodning avvist.' 
                .
            RETURN.
        END.  
        
        /* Er den kreditert tidligere ? */
        FIND FIRST KundeResKobling NO-LOCK WHERE 
            KundeResKobling.DReskontro_Id = Kundereskontr.Reskontro_Id NO-ERROR. 
        IF AVAILABLE KundeResKobling THEN 
        DO:
            ASSIGN
                pbOk    = FALSE 
                cReturn = 'Pakkseddel er angret tildigere. Anmodning avvist.' + chr(10) + 
                          'Faktura fra overføringen er kreditert tidligere.'
                .
            RETURN.
        END.        
               
        /* Faktura krediteres. */
        RUN kunderes_krednota.p(
            STRING(Kundereskontr.KundeNr) + '|' + 
            STRING(Kundereskontr.Reskontro_id) + '|' + 
            STRING(FakturaHode.Totalt) + '|' + 
            'Angret overføring.',
            ?,
            '',
            OUTPUT pcReturn,
            OUTPUT pbOk
            ).
         IF pbOk THEN 
         SKRIV_FAKTURA:
         DO:
            FIND FIRST bufFakturaHode NO-LOCK WHERE 
                bufFakturaHode.bilagsType = 2 AND 
                bufFakturaHode.FakturaNr  = DECIMAL(pcReturn) NO-ERROR.
            IF NOT AVAILABLE bufFakturaHode THEN 
                LEAVE SKRIV_FAKTURA.
            RUN faktura_fakturaskriver.p (STRING(bufFakturaHode.ButikkNr) + "|1|",
                                              ?,
                                              "",
                                              OUTPUT pcReturn,
                                              OUTPUT pbOk).  
            IF pbOk THEN 
                  DO:
                      cTekst = pcReturn.
                      IF cTekst <> "" THEN 
                      DO:
                          RUN skrivfaktura.p (STRING(bufFakturaHode.Faktura_Id) + "|",ENTRY(1,cTekst,"|"),ENTRY(2,cTekst,"|"),ENTRY(3,cTekst,"|"),ENTRY(4,cTekst,"|"),ENTRY(5,cTekst,"|")). 
                          /* Ekstra kopi til butikk? */
                          IF feilButiker.FaktKopiRappskriver AND feilButiker.RapPrinter <> "" THEN
                              RUN skrivfaktura.p (STRING(bufFakturaHode.Faktura_Id) + "|",ENTRY(1,cTekst,"|"),feilButiker.RapPrinter,"1",ENTRY(4,cTekst,"|"),ENTRY(5,cTekst,"|")).
                      END.
                  END.                                              
         END. /* SKRIV_FAKTURA */
         ELSE RETURN.
         
    END. /* FAKTURA_BEHANDLING */
    ELSE DO:
        ASSIGN
            pbOk    = FALSE 
            cReturn = 'Finner ikke pakkseddelens faktura. Anmodning avvist.' 
            .
       RETURN.
    END.  

    ASSIGN 
        pbOk = TRUE.

END PROCEDURE.

PROCEDURE overfortilbake:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER pbOk AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER pcReturn AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iBatchNr AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTransNr AS INTEGER NO-UNDO.

    /* Batch for TransLogg */
    RUN batchlogg.w (PROGRAM-NAME(1),
                     "Angre overføring " +
                     string(TODAY) +
                     " " +
                     string(TIME,"HH:MM") +
                     " " +
                     USERID("dictdb"),
                     OUTPUT iBatchNr).
                     
    /* Setter transaksjonsnummer  */
    IF iTransNr = 0 THEN
      DO:
        FIND LAST TransLogg WHERE
          TransLogg.Butik = iButNr
          USE-INDEX TransLogg NO-ERROR.
        IF AVAILABLE TransLogg THEN
          iTransNr = TransLogg.TransNr + 1.
        ELSE
          iTransNr = 1.
      END.
    ELSE
      iTransNr = iTransNr + 1.

    EMPTY TEMP-TABLE tt_OvBuffer.
    OVERFORTILBAKE:
    FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
        FIND ArtBas NO-LOCK WHERE 
            ArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN 
            NEXT.
        FIND StrKonv NO-LOCK WHERE 
            StrKonv.StrKode = PkSdlLinje.StrKode NO-ERROR.
        FIND PkSdlPris NO-LOCK WHERE 
            PkSdlPris.PkSdlId    = PkSdlLinje.PkSdlId AND 
            PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
        IF AVAILABLE ArtBas AND AVAILABLE PkSdlPris THEN 
        OPPRETT_TRANSLOGG:
        DO:
            /* Oppretter TransLogg */    
            CREATE TransLogg.
            NYTRANSLOGG:
            DO WHILE TRUE ON ERROR UNDO, RETRY:
                ASSIGN TransLogg.Butik        = iButNr
                       TransLogg.TransNr      = iTransNr
                       TransLogg.SeqNr        = 1
                       NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    iTransNr = iTransNr + 1.
                ELSE LEAVE NYTRANSLOGG.
            END. /* NYTRANSLOGG */
            ASSIGN
               TransLogg.BatchNr      = iBatchNr
               TransLogg.TTId         = 6
               TransLogg.TBId         = 2
               TransLogg.ArtikkelNr   = ArtBas.ArtikkelNr
               TransLogg.Vg           = ArtBas.Vg
               TransLogg.LopNr        = ArtBas.LopNr
               TransLogg.Antall       = PkSdlLinje.Antall * -1
               TransLogg.Pris         = PkSdlPris.NyVarekost / ABSOLUTE(PkSdlLinje.Antall)
               TransLogg.KundNr       = lKundeNr
               TransLogg.LevNr        = ArtBas.LevNr
               TransLogg.OvButik      = PkSdlLinje.butikkNr
               TransLogg.OvTransNr    = iButNr
               TransLogg.Plukket      = TRUE
               TransLogg.Dato         = TODAY
               TransLogg.Tid          = TIME 
               TransLogg.BestNr       = 0
               TransLogg.Postert      = FALSE
               TransLogg.RefNr        = 7
               TransLogg.RefTekst     = 'Fra AngreOverføring ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS")
               Translogg.Kode         = PkSdlLinje.Kode
               TransLogg.Storl        = (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')
               TransLogg.TilStorl     = (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')
               Translogg.BongTekst    = ArtBas.Beskr               
               TransLogg.VVareKost    = PkSdlPris.NyVarekost
               TransLogg.SattVVarekost = TRUE /* Skal ikke regnes om ved opp. av statistikker. */
               TransLogg.KalkylePris  = IF AVAILABLE ArtPris
                                          THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                          ELSE Translogg.KalkylePris
               TransLogg.Varekost     = PkSdlPris.NyVarekost
               TransLogg.Pris         = PkSdlPris.NyVarekost                                          
               TransLogg.Mva          = 0
               Translogg.Mva          = 0
               TransLogg.Mva%         = 0
               .
        END. /* OPPRETT_TRANSLOGG */
    END. /* OVERFORTILBAKE */

    /* Flagger batchen klar for oppdatering. */
    RUN batchstatus.p (iBatchNr, 2).
    
    ASSIGN 
        pbOk = TRUE.
END PROCEDURE.

PROCEDURE makulerPakkseddel:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER pbOk AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER pcReturn AS CHARACTER NO-UNDO.

    DO TRANSACTION:
        FIND CURRENT PkSdlHode EXCLUSIVE-LOCK.
        ASSIGN 
            PkSdlHode.PkSdlStatus = 60
            PkSdlHode.Merknad     = 'Makulert fra AngreOverføring ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS") + chr(10) + 
                                    PkSdlHode.Merknad
            .
        FIND CURRENT PkSdlHode NO-LOCK.
    END. /* TRANSACTION */

    ASSIGN 
        pbOk = TRUE.
END PROCEDURE.

PROCEDURE overforTilbut:
/*------------------------------------------------------------------------------
 Purpose: Opprett ny overføring til riktig butikk, og fakturer.
            - Opprett overføring og poster denne. TBId = 2.
            - Skriv ut faktura i butikken det overføres til.
 Notes:
------------------------------------------------------------------------------*/
    
    DEFINE OUTPUT PARAMETER pbOk AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER pcReturn AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE tmpPksdlLinje.
    FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
        CREATE tmpPkSdlLinje.
        BUFFER-COPY PkSdlLinje
            TO tmpPkSdlLinje.
    END.

    ihBuffer = BUFFER tmpPkSdlLinje:HANDLE. 

    RUN pksdl_overfor.p (STRING(iButNr) + '|' + STRING(iTilBut),
                         ihBuffer,
                         '',
                         OUTPUT pcReturn,
                         OUTPUT pbOk
                        ).

END PROCEDURE.

PROCEDURE validerParametre:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER pbOk AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER pcReturn AS CHARACTER NO-UNDO.
    
    IF lPkSdlId = 0 THEN 
    DO:
        ASSIGN 
            pcReturn = '** Pakkseddelnr. er ikke angitt.'
            pbOk     = FALSE 
        .
        RETURN.
    END. 
    
    IF (iButNr = 0) THEN 
    DO:
        ASSIGN 
            pcReturn = '** Fra butikk ikke angitt.'
            pbOk     = FALSE 
        .
        RETURN.
    END.
    
    IF (iFeilBut = 0 OR iTilBut = 0) THEN 
    DO:
        ASSIGN 
            pcReturn = '** Feil butikk og ny til butikk nå angis.'
            pbOk     = FALSE 
        .
        RETURN.
    END.
    
    IF NOT AVAILABLE feilButiker THEN  
    DO:
        ASSIGN 
            pcReturn = "** Ugyldig 'Feil' butikk angitt."
            pbOk     = FALSE 
        .
        RETURN.
    END.
    
    IF NOT CAN-FIND(Butiker WHERE 
                    Butiker.Butik = iTilBut) THEN  
    DO:
        ASSIGN 
            pcReturn = "** Ugyldig 'Til' butikk angitt."
            pbOk     = FALSE 
        .
        RETURN.
    END.
    
    ASSIGN 
        pbOk = TRUE.
    
END PROCEDURE.


