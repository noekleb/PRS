
/*------------------------------------------------------------------------
    File        : PkSdl_motposter.p
    Purpose     : 

    Syntax      :

    Description : Bare pakksedler som er mottatt kan motposteres.

    Author(s)   : 
    Created     : Mon Feb 27 18:31:18 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER lPkSdlId AS DECIMAL   NO-UNDO.
DEFINE OUTPUT PARAMETER bOk      AS LOG INITIAL TRUE NO-UNDO.
DEFINE OUTPUT PARAMETER cReturn  AS CHARACTER NO-UNDO.

DEFINE VARIABLE iSentralLager AS INTEGER NO-UNDO.
DEFINE VARIABLE iBatchNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE lKundeNr LIKE Kunde.KundeNr NO-UNDO.
DEFINE VARIABLE ihBuffer AS HANDLE NO-UNDO.
DEFINE VARIABLE iDummy AS INTEGER NO-UNDO.
DEFINE VARIABLE iOrgOvbut AS INTEGER NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.

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
IF NOT AVAILABLE PkSdlHode THEN
DO: 
    cReturn = '** Ukjent pakkseddelId (' + STRING(lPkSdlId) + ').'.
    RETURN cReturn.
END.    
/* Sjekker om pakkseddel er mottatt. */
IF PkSdlHode.PkSdlStatus <= 10 OR PkSdlHode.Merknad BEGINS 'MOTPOSTERT' THEN
DO: 
    cReturn = '** Pakkseddel ikke mottatt. Kan ikke motposteres (' + STRING(lPkSdlId) + ').'.
    RETURN cReturn.
END.    
ELSE bOk = TRUE.
FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK NO-ERROR.
IF NOT AVAILABLE PkSdlLinje THEN 
    RETURN.
ELSE 
    iButNr = PkSdlLinje.ButikkNr. 

/* Overfør varene tilbake til der de kom fra. */
RUN motposter(OUTPUT bOk, OUTPUT cReturn).
IF bOk = FALSE THEN
DO:
    cReturn = '** Feil ved motpostering.'.
    RETURN cReturn.
END.

RETURN cReturn.
    
/* **********************  Internal Procedures  *********************** */

PROCEDURE motposter:
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
                     "Motposter PksdlId " +
                     STRING(PkSdlHode.PkSdlId) + 
                     ' ' +
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

    MOTPOSTERER:
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
                ASSIGN TransLogg.Butik        = PkSdlLinje.ButikkNr
                       TransLogg.TransNr      = iTransNr
                       TransLogg.SeqNr        = 1
                       NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    iTransNr = iTransNr + 1.
                ELSE LEAVE NYTRANSLOGG.
            END. /* NYTRANSLOGG */
            ASSIGN
               TransLogg.BatchNr      = iBatchNr
               TransLogg.TTId         = 5
               TransLogg.TBId         = 1
               TransLogg.ArtikkelNr   = ArtBas.ArtikkelNr
               TransLogg.Vg           = ArtBas.Vg
               TransLogg.LopNr        = ArtBas.LopNr
               TransLogg.Antall       = PkSdlLinje.Antall * -1
               TransLogg.Pris         = PkSdlPris.NyVarekost / ABSOLUTE(PkSdlLinje.Antall)
               TransLogg.KundNr       = 0
               TransLogg.LevNr        = ArtBas.LevNr
               TransLogg.OvButik      = PkSdlLinje.butikkNr
               TransLogg.OvTransNr    = 0
               TransLogg.Plukket      = TRUE
               TransLogg.Dato         = TODAY
               TransLogg.Tid          = TIME 
               TransLogg.BestNr       = 0
               TransLogg.Postert      = FALSE
               TransLogg.RefNr        = 8
               TransLogg.RefTekst     = 'Fra motposter pakkseddel ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS")
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
    END. /* MOTPOSTERER */

    /* Flagger batchen klar for oppdatering. */
    RUN batchstatus.p (iBatchNr, 2).
    
    DO TRANSACTION:
        FIND CURRENT PkSdlHode EXCLUSIVE-LOCK.
        ASSIGN 
            PkSdlHode.Merknad = 'MOTPOSTERT ' + STRING(TODAY) + ' av ' + USERID('skotex') + '.' + chr(10) +  
                                PkSdlHode.Merknad.   
        FIND CURRENT PkSdlHode EXCLUSIVE-LOCK.
    END.
    
    ASSIGN 
        pbOk = TRUE.
END PROCEDURE.


