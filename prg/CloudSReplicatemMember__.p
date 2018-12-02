

/* ***************************  Main Block  *************************** */

/* ---------------------------------------- (c) 2014 CHSO --------------- */
/* ---- Program Name : putmMember.p                                  ---- */
/* ---- Description  :                                               ---- */
/* ----                                                              ---- */
/* ---- Author       : Curt H. Oldenborg                             ---- */
/* ---- Date Started : 2013-04-2            
------------------------------------------------------------------------- */

DEFINE VARIABLE dDato AS DATE INIT TODAY NO-UNDO. 
                       
DEFINE VARIABLE hDset AS HANDLE NO-UNDO.
DEFINE VARIABLE lDebug AS LOGICAL INIT FALSE NO-UNDO. 

DEFINE TEMP-TABLE tt_medlem LIKE medlem. 

DEFINE VARIABLE iCnt AS INT NO-UNDO.
DEFINE VARIABLE hServer AS HANDLE NO-UNDO. 
DEFINE VARIABLE lConnected AS LOG NO-UNDO. 
DEFINE VARIABLE cConnectionString AS CHAR NO-UNDO.
DEFINE VARIABLE cSessionid AS CHAR NO-UNDO. 
DEFINE VARIABLE cCompanyId AS CHAR NO-UNDO. 
DEFINE VARIABLE cUserid AS CHAR NO-UNDO. 
DEFINE VARIABLE cPassword AS CHAR NO-UNDO. 
DEFINE VARIABLE lOK AS LOGICAL NO-UNDO.

DEFINE VARIABLE LastRegisterDateTime AS DATETIME NO-UNDO. 

DEFINE VARIABLE LastSendtDato AS DATE NO-UNDO. 
DEFINE VARIABLE LastSendtTid  AS INT NO-UNDO. 
DEFINE VARIABLE CurrentTime AS DATETIME NO-UNDO. 
DEFINE VARIABLE RemoteSystemId AS CHAR NO-UNDO. 
DEFINE VARIABLE RemoteTableId AS CHAR NO-UNDO. 
DEFINE VARIABLE cKortNr AS CHAR NO-UNDO. 
DEFINE VARIABLE iKortNr AS INT NO-UNDO.
DEFINE VARIABLE lMayFlowerMember AS LOGICAL NO-UNDO. 


/*
{syspara.i 5 1 1 cStoreNumber}

 {syspara.i 5 1 1 cConnectionString}
   {syspara.i 5 1 1 cUserid}
   {syspara.i 5 1 1 cPassword}
\                                      */



DEFINE TEMP-TABLE RecordChanges 
    FIELD updateDateTime AS DATETIME 
    FIELD updateNumber AS INT 
    FIELD updateRowId AS ROWID. 


FUNCTION  getLocalUpdatedRecords   RETURNS LOGICAL (INPUT readFromDate AS DATETIME ):
    DEFINE VARIABLE iCnt AS INT NO-UNDO. 

    /* Endringer i skotex medlem --->  WS Database mMember */
    FOR EACH medlem NO-LOCK WHERE 
        medlem.edato GE DATE(readFromDate)  :

        /* added check for datetime ONLY GE RECORDS INCLUDED */
        IF DATETIME(medlem.edato,medlem.etid * 1000) LT readFromDate THEN NEXT. 

        IF medlem.edato = ? THEN NEXT . 

        /* nytt medlem på bakrom - og ikke på WS   transf all records 
        IF medlem.EksterntMedlemsNr = "" OR 
           medlem.EksterntMedlemsNr = ?  THEN    */
        DO:
            CREATE RecordChanges. 
            iCnt = iCnt + 1. 
            RecordChanges.UpdateDateTime = DATETIME(medlem.edato,medlem.etid * 1000 ). 
            RecordChanges.UpdateNumber   = iCnt. 
            RecordChanges.updateRowid    = ROWID(medlem). 
        END. 
    END.

END.


FUNCTION  copyLocalUpdatedRecords   RETURNS LOGICAL ():

    DEFINE VARIABLE iCnt AS INT NO-UNDO. 
    FOR EACH RecordChanges NO-LOCK: 
        
        FIND medlem WHERE ROWID(medlem) = recordChanges.updateRowid NO-LOCK NO-ERROR. 
        IF AVAIL medlem THEN
        DO:
            iCnt = iCnt + 1. 
            CREATE tt_medlem. 
            BUFFER-COPY medlem TO tt_medlem. 
        END. 
    END.
    EMPTY TEMP-TABLE RecordChanges. 
    
    LOG-MANAGER:WRITE-MESSAGE("copyLocalUpdatedRecords Num#:" + STRING(iCnt), "BATCH").

    IF lDebug THEN TEMP-TABLE tt_medlem:WRITE-XML("file","c:\tmp\tt_medlem.xml",TRUE,?,?,FALSE,TRUE).

END.

FUNCTION isMayFlowerMember RETURNS LOGICAL 
    (INPUT EksterntMedlemsNr AS CHAR , OUTPUT iMayFlowerid AS INT ):
          
    lMayFlowerMember = FALSE. 
    DEF VAR cMayFlowerId AS CHAR NO-UNDO. 
    /* test for mayflower member */
          IF Medlem.EksterntMedlemsNr BEGINS "M-" THEN
          DO:
              lMayFlowerMember = TRUE. 
              cMayFlowerId     = LEFT-TRIM(REPLACE(TRIM(EksterntMedlemsNr),'M-',''),'0').
              iMayFlowerId     = INTEGER(cMayFlowerid) NO-ERROR. 
              IF iMayFlowerid  =  0 OR iMayFlowerid = ?  THEN
              lMayflowerMember  = FALSE. 
          END. 
          ELSE lMayFlowerMember = FALSE. 

   RETURN lMayFlowerMember. 
END. 


FUNCTION isRegisteredFromEcom RETURNS LOGICAL 
    (INPUT mobiltlf AS CHAR , INPUT  email AS CHAR ):
          
    DEFINE BUFFER bfMedlem FOR medlem. 
    DEFINE BUFFER bfMedlemskort FOR medlemskort. 

    FIND FIRST bfmedlem WHERE 
               bfmedlem.mobiltlf = mobiltlf AND 
               bfmedlem.MKlubbId = 1    NO-LOCK NO-ERROR. 

    IF AVAIL bfmedlem THEN
    FIND FIRST  bfMedlemskort WHERE 
                bfMedlemsKort.medlemsnr  = bfmedlem.medlemsnr AND 
                bfMedlemsKort.KortType   = 1 /* fra ecom */ NO-LOCK NO-ERROR.
     
    IF AVAIL bfMedlemskort  THEN RETURN TRUE. 

    FIND FIRST bfmedlem WHERE 
               bfmedlem.epost = email AND 
               bfmedlem.MKlubbId = 1    NO-LOCK NO-ERROR. 

    IF AVAIL bfmedlem THEN
    FIND FIRST  bfMedlemskort WHERE 
                bfMedlemsKort.medlemsnr  = bfmedlem.medlemsnr AND 
                bfMedlemsKort.KortType   = 1 /* fra ecom */ NO-LOCK NO-ERROR.

    IF AVAIL bfMedlemskort  THEN RETURN TRUE. 

    RETURN FALSE. 
END. 




/* default if not defined */ 
IF TRIM(cConnectionString) = ""  THEN
   cConnectionString = "-URL http://appfarm.netextend.no/aia/Aia?AppService=PRSTrans -sessionModel session-free".

cUserId        = "PRS". 
cPassword      = "PRSTest". 
cCompanyid     = "200".
RemoteTableId  = "mMember" .
RemoteSystemId = "PRS".

CREATE SERVER hServer. 
lConnected = hServer:CONNECT(cConnectionString) NO-ERROR .

IF lConnected THEN
DO:
  RUN WSLogin.p ON hServer (cUserid, cPassword, 
                            OUTPUT cSessionId,
                            OUTPUT lok) NO-ERROR. 

  IF NOT lok THEN
  DO:
      hServer:DISCONNECT(). 
      DELETE OBJECT hServer NO-ERROR. 
      DELETE OBJECT hDset NO-ERROR. 
      RETURN. 
  END.


  RUN getTableLastCreatedDateTime.p ON hServer 
      (cSessionId,
       cCompanyId, 
       RemoteTableId, 
       RemoteSystemId,
       FALSE, /* false = lastupdateddatefromRemoteSystem */
       OUTPUT LastRegisterDateTime, 
       OUTPUT lok). 
  
 
  getLocalUpdatedRecords   (LastRegisterDateTime).
  copyLocalUpdatedRecords ().

  /* Call General update routine based on origion Table RemoteSystemid */
  RUN putClientUpdatedRecords.p ON hServer (cSessionId,RemoteTableId,RemoteSystemId,TABLE tt_medlem,OUTPUT lok).  
  EMPTY TEMP-TABLE tt_medlem.
  
  RUN getServerUpdatedRecords.p ON hServer (cSessionId,RemoteTableId,RemoteSystemId,OUTPUT TABLE tt_medlem,OUTPUT lok).

  DEFINE VARIABLE lCreateNewMedlem AS LOGICAL NO-UNDO. 
  DEFINE BUFFER MedlemsGruppe FOR medlemsgruppe. 
  DEFINE BUFFER MedlemsType   FOR medlemstype. 
  DEFINE BUFFER bfMedlem      FOR medlem . 

  DEFINE VARIABLE iMedGruppeDefault AS INTEGER INIT 1 NO-UNDO. /* GANT - Medlem */
  DEFINE VARIABLE iMedTypeDefault   AS INTEGER INIT 1 NO-UNDO. /* GANT - Medlem */
  DEFINE VARIABLE iMKlubbIdDefault  AS INTEGER INIT 2 NO-UNDO. /* 1 - ecom 2 - GantExclusive */

  DEFINE VARIABLE cMayFlowerid      AS CHARACTER NO-UNDO. 
  DEFINE VARIABLE iMayFlowerid      AS INTEGER   NO-UNDO. 
  DEFINE VARIABLE lRegisteredFromEcom AS LOGICAL NO-UNDO. 

  DISABLE TRIGGERS FOR DUMP OF medlem. 
  DISABLE TRIGGERS FOR LOAD OF medlem. 
  DISABLE TRIGGERS FOR LOAD OF medlemskort. 
  DISABLE TRIGGERS FOR DUMP OF medlemskort. 

  /* Update local records with updated server/remote records */
  FOR EACH tt_medlem NO-LOCK BY tt_Medlem.eDato BY tt_Medlem.eTid : 
       

      lCreateNewMedlem = IF  tt_medlem.medlemsnr = ? THEN TRUE ELSE FALSE.  


      IF NOT lCreateNewMedlem THEN
      DO:
          FIND FIRST medlem WHERE medlem.medlemsnr = tt_medlem.medlemsnr NO-ERROR. 
          
          IF AVAIL medlem THEN 
          DO:
              ASSIGN 
                  Medlem.PersonNr          = tt_Medlem.PersonNr
                  Medlem.ForNavn           = tt_Medlem.ForNavn
                  Medlem.EtterNavn         = tt_Medlem.EtterNavn
                  Medlem.PostNr            = tt_Medlem.PostNr
                  Medlem.Adresse1          = tt_Medlem.Adresse1
                  Medlem.Kjonn             = tt_medlem.kjonn
                  Medlem.EksterntMedlemsNr = tt_Medlem.EksterntMedlemsNr
                  Medlem.ePostAdresse      = tt_Medlem.ePostAdresse
                  Medlem.MobilTlf          = tt_Medlem.MobilTlf
                  Medlem.Telefon           = tt_Medlem.Telefon  
                  Medlem.FodselsDato       = tt_Medlem.FodselsDato. 
                  Medlem.MedlemsNr         = tt_Medlem.MedlemsNr.
                  Medlem.Bonus_Berettiget  = FALSE. 
                  Medlem.Kilde             = 'CloudS'.
                  Medlem.Edato             = tt_Medlem.edato. 
                  Medlem.eTid              = tt_medlem.etid. 
    
                  /* default 1 hvis ikke butikk 0 eller ? */ 
                  Medlem.ButikkNr         =  IF tt_Medlem.butikknr = 0 OR 
                                                tt_Medlem.butikknr = ?     
                                                THEN 1 ELSE tt_Medlem.butikknr.
    
              lMayflowerMember     = isMayFlowerMember(Medlem.EksterntMedlemsNr,OUTPUT iMayflowerid).
              lRegisteredFromEcom  = isRegisteredFromEcom(Medlem.mobiltlf,Medlem.epost). 
              
              FIND FIRST MedlemsKlubb  WHERE MedlemsKlubb.MKlubbId   = iMKlubbIdDefault   NO-LOCK NO-ERROR.
              Medlem.MKlubbId  = IF AVAILABLE MedlemsKlubb  THEN MedlemsKlubb.MKlubbId   ELSE 0.

              LOG-MANAGER:WRITE-MESSAGE("isMayFlowerMember:"    + STRING(lMayflowerMember)   ,"BATCH").
              LOG-MANAGER:WRITE-MESSAGE("isRegisteredFromEcom:" + STRING(lRegisteredFromEcom),"BATCH").
    
              iKortNr =  INT( Medlem.MobilTlf) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN
                iKortNr =  INT(Medlem.MedlemsNr).
    
              FIND FIRST  Medlemskort WHERE 
                          MedlemsKort.KortNr  = STRING(iKortNr)  
                          NO-ERROR.
             
              IF NOT AVAIL MedlemsKort THEN
              DO:
                  CREATE MedlemsKort.
                  ASSIGN
                      MedlemsKort.MedlemsNr      = Medlem.MedlemsNr
                      MedlemsKort.KortNr         = STRING(iKortNr)
                      MedlemsKort.AktivertDato   = TODAY 
                      MedlemsKort.UtgarDato      = TODAY + 999
                      MedlemsKort.Innehaver      = Medlem.Fornavn + " " + Medlem.EtterNavn
                      MedlemsKort.KortType       = 2  /* type 2 for access from Kasse */  
                      MedlemsKort.Merknad        = "Registered from CloudS:" + Medlem.EksterntMedlemsNr
                      MedlemsKort.Sperret        = FALSE
                      MedlemsKort.InterntKKortId = IF lMayFlowerMember THEN iMayFlowerid ELSE 0 
                      .
    
                  LOG-MANAGER:WRITE-MESSAGE("Created MedlemsKort:" + STRING( MedlemsKort.MedlemsNr),"BATCH").
              END. 
              ELSE 
              DO:
                  ASSIGN 
                  MedlemsKort.Innehaver      = Medlem.Fornavn + " " + Medlem.EtterNavn
                  MedlemsKort.KortType       = 2  /* type 2 for access from Kasse */  
                  MedlemsKort.InterntKKortId = IF lMayFlowerMember THEN iMayFlowerid ELSE 0 .
                  LOG-MANAGER:WRITE-MESSAGE("Endret MedlemsKort:" + STRING( MedlemsKort.MedlemsNr),"BATCH").
              END. 


          END.
          /* ikke avail */ 
      END. 
      

      LastRegisterDateTime = DATETIME(tt_medlem.edato,tt_medlem.etid * 1000). 
  END.
  
  LOG-MANAGER:WRITE-MESSAGE("setTableLastCreatedDateTime:" + STRING(LastRegisterDateTime),"BATCH").

  RUN setTableLastCreatedDateTime.p ON hServer (cSessionId, cCompanyId,RemoteTableId,RemoteSystemId,TRUE, /* TRUE = LastUpdatedDateToRemoteSystem */
       INPUT LastRegisterDateTime, 
       OUTPUT lok). 


  hServer:DISCONNECT(). 
  DELETE OBJECT hServer NO-ERROR. 

END. 

QUIT.


     /*

       /* FIND FIRST Post WHERE 
             Post.PostNr = TRIM(getMemberResponseReturn.zip) NO-LOCK NO-ERROR.
             IF NOT AVAILABLE Post THEN 
             DO:
                 CREATE Post.
                 ASSIGN
                     Post.PostNr      = TRIM(getMemberResponseReturn.zip)
                     Post.Beskrivelse = TRIM(getMemberResponseReturn.postarea).
                 FIND CURRENT Post NO-LOCK.
             END. */
    
              ASSIGN 
                  Medlem.MedlemsNr         = tt_Medlem.MedlemsNr
                  Medlem.PersonNr          = tt_Medlem.PersonNr
                  Medlem.ForNavn           = tt_Medlem.ForNavn
                  Medlem.EtterNavn         = tt_Medlem.EtterNavn
                  Medlem.PostNr            = tt_Medlem.PostNr
                  Medlem.Adresse1          = tt_Medlem.Adresse1
                  Medlem.Kjonn             = tt_medlem.kjonn
                  Medlem.EksterntMedlemsNr = tt_Medlem.EksterntMedlemsNr
                  Medlem.ePostAdresse      = tt_Medlem.ePostAdresse
                  Medlem.MobilTlf          = tt_Medlem.MobilTlf
                  Medlem.Telefon           = tt_Medlem.Telefon  
                  Medlem.FodselsDato       = tt_Medlem.FodselsDato. 
                  Medlem.MedlemsNr         = tt_Medlem.MedlemsNr.
                  Medlem.Bonus_Berettiget  = FALSE. 
                  Medlem.Kilde             = 'CloudS'.
    
                  /* default 1 hvis ikke butikk 0 eller ? */ 
                  Medlem.ButikkNr         =  IF tt_Medlem.butikknr = 0 OR 
                                             tt_Medlem.butikknr = ?     
                                             THEN 1 ELSE tt_Medlem.butikknr.
    
                  FIND FIRST Medlemsgruppe WHERE MedlemsGruppe.MedGruppe = iMedGruppeDefault  NO-LOCK NO-ERROR. 
                  FIND FIRST MedlemsType   WHERE MedlemsType.MedType     = iMedTypeDefault    NO-LOCK NO-ERROR. 
                  FIND FIRST MedlemsKlubb  WHERE MedlemsKlubb.MKlubbId   = iMKlubbIdDefault   NO-LOCK NO-ERROR.
    
                  Medlem.MedGruppe = IF AVAILABLE MedlemsGruppe THEN MedlemsGruppe.MedGruppe ELSE 0.
                  Medlem.MedType   = IF AVAILABLE MedlemsType   THEN MedlemsType.MedType     ELSE 0.
                  Medlem.MKlubbId  = IF AVAILABLE MedlemsKlubb  THEN MedlemsKlubb.MKlubbId   ELSE 0.
                  
    
              lMayflowerMember     = isMayFlowerMember(Medlem.EksterntMedlemsNr,OUTPUT iMayflowerid).
              lRegisteredFromEcom  = isRegisteredFromEcom(Medlem.mobiltlf,Medlem.epost). 
    
              LOG-MANAGER:WRITE-MESSAGE("isMayFlowerMember:" + STRING(lMayflowerMember),"BATCH").
              LOG-MANAGER:WRITE-MESSAGE("isRegisteredFromEcom:" + STRING(lRegisteredFromEcom),"BATCH").
    
              iKortNr =  INT( Medlem.MobilTlf) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN
                cKortNr =  STRING(Medlem.MedlemsNr).
    
              FIND FIRST  Medlemskort WHERE 
                          MedlemsKort.KortNr  = cKortNr 
                          NO-LOCK NO-ERROR.
    
              IF NOT AVAIL MedlemsKort THEN
              DO:
                  CREATE MedlemsKort.
                  ASSIGN
                      MedlemsKort.MedlemsNr      = Medlem.MedlemsNr
                      MedlemsKort.KortNr         = cKortNr
                      MedlemsKort.AktivertDato   = TODAY 
                      MedlemsKort.UtgarDato      = TODAY + 999
                      MedlemsKort.Innehaver      = Medlem.Fornavn + " " + Medlem.EtterNavn
                      MedlemsKort.KortType       = 2  /* type 2 for access from Kasse */  
                      MedlemsKort.Merknad        = "Registered from CloudS:" + Medlem.EksterntMedlemsNr
                      MedlemsKort.Sperret        = FALSE
                      MedlemsKort.InterntKKortId = IF lMayFlowerMember THEN iMayFlowerid ELSE 0 
                      NO-ERROR.
    
                  LOG-MANAGER:WRITE-MESSAGE("Created MedlemsKort:" + STRING( MedlemsKort.MedlemsNr),"BATCH").
              END. 
   */
