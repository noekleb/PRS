/* ---------------------------------------- (c) 2013 CHSO --------------- */
/* ---- Program Name : WSGetNetsData.p                               ---- */
/* ---- Description  :                                               ---- */
/* ----                                                              ---- */
/* ---- Author       : Curt H. Oldenborg                             ---- */
/* ---- Date Started : 2013-04-2            
------------------------------------------------------------------------- */

DEFINE TEMP-TABLE RowData NO-UNDO
    FIELD ButikkNr AS INT 
    FIELD ButNavn AS CHAR  
    FIELD EkstKundeNr AS CHAR  
    FIELD ArtikkelNr AS CHAR  
    FIELD StrKode AS CHAR  
    FIELD VareTekst AS CHAR  
    FIELD VareGr AS INT  
    FIELD VareGrTekst AS CHAR  
    FIELD HovedGr AS INT  
    FIELD HovedGrTekst AS CHAR  
    FIELD Avdeling AS CHAR  
    FIELD AvdelingTekst AS CHAR  
    FIELD Dato AS DATE  
    FIELD Antall AS DEC  
    FIELD LevNr AS INT  
    FIELD LevNavn AS CHAR  
    FIELD RabKr AS DEC  
    FIELD InnVerdiKr AS DEC  
    FIELD UtVerdiKr AS DEC  
    FIELD MvaKr AS DEC.


DEFINE INPUT PARAMETER LoginSessionId AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER CompanyId AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER TransactionID AS CHAR NO-UNDO. 
DEFINE OUTPUT PARAMETER TABLE FOR RowData.
DEFINE OUTPUT PARAMETER RequestStatus AS LOGICAL INIT FALSE NO-UNDO. 

DEFINE VARIABLE lok AS LOGICAL NO-UNDO. 
DISABLE TRIGGERS FOR LOAD OF Nets. 
DISABLE TRIGGERS FOR DUMP OF Nets. 


    RUN checkSessionId.p (LoginSessionId,Companyid, OUTPUT loK). 
    
    LOG-MANAGER:WRITE-MESSAGE("SessionId:" + QUOTER(LoginSessionId) + " Status:" + QUOTER(loK),"WSGetNetsData").
    IF NOT lok THEN RETURN. 

    FIND FIRST nets WHERE nets.transactionid  = Transactionid AND 
                          nets.iJBoxCompanyId = companyid NO-LOCK NO-ERROR. 

    LOG-MANAGER:WRITE-MESSAGE("TransactionId:" + QUOTER(Transactionid) + " Status:" + QUOTER(AVAIL nets),"WSGetNetsData").

    IF NOT AVAIL nets THEN RETURN . 
    
    DO TRANSACTION : 
        FIND CURRENT nets EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL nets THEN
        DO:
            ASSIGN 
            nets.Sendt = TRUE.
            nets.sendtDato = TODAY. 
            nets.sendtTid = TIME. 
        END.
    END. 
    

    FIND FIRST bonghode WHERE bonghode.b_id = nets.b_id NO-LOCK NO-ERROR. 
    FOR EACH  bonglinje WHERE bonglinje.b_id = nets.b_id   AND 
                       (bonglinje.ttid = 1   OR 
                        bonglinje.ttid = 10  OR 
                        bonglinje.ttid = 3 ) NO-LOCK :

            CREATE RowData. 
            ASSIGN 
             RowData.ButikkNr      = bonghode.butikknr 
             RowData.ButNavn       = "" 
             RowData.EkstKundeNr   = ""
             RowData.ArtikkelNr    = bonglinje.ArtikkelNr
             RowData.StrKode       = bonglinje.strekkode
             RowData.VareTekst     = BongTekst 
             RowData.VareGr        = bonglinje.varegr 
             RowData.VareGrTekst   = bonglinje.VareGruppeNavn
             RowData.HovedGr       = bonglinje.hovedgr 
             RowData.HovedGrTekst  = bonglinje.HovedGrBeskrivelse
             RowData.Antall        = bonglinje.Antall
             RowData.LevNr         = bonglinje.levnr  
             RowData.LevNavn       = bonglinje.levnavn
             RowData.MvaKr         = bonglinje.mvakr 
             RowData.Dato          = bonglinje.dato
             RowData.RabKr         = BongLinje.LinjeRab + BongLinje.SubtotalRab   
             RowData.InnVerdiKr    = BongLinje.LinjeSum - (BongLinje.LinjeRab + BongLinje.SubtotalRab) - BongLinje.MvaKr
             RowData.UtVerdiKr     = BongLinje.LinjeSum - (BongLinje.LinjeRab + BongLinje.SubtotalRab)
             RowData.Avdeling      = ""   
             RowData.AvdelingTekst = "" 
             .
    END.

    RequestStatus = TRUE. 

 
