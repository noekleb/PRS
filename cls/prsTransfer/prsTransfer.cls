 
 /*------------------------------------------------------------------------
    File        : prsTransfer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tomn
    Created     : Tue Oct 15 09:44:26 CEST 2019
    Notes       : 

  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS cls.prsTransfer.prsTransfer: 
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE PROPERTY bConnected AS LOG  NO-UNDO GET. SET.

  DEFINE VARIABLE bTest AS LOG NO-UNDO.
  DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCmd AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDbName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iAntDg AS INTEGER NO-UNDO.

  DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.

  DEFINE BUFFER bufKOrdrEHode FOR KOrdreHode.
        
  /* Endringer her skal ikke utløse ny ELogg post og resending av ordre. */    
  ON CREATE OF KOrdreHode OVERRIDE DO: END.
  ON WRITE  OF KOrdreHode OVERRIDE DO: END.
  ON CREATE OF KOrdreHode OVERRIDE DO: END.
      
  CONSTRUCTOR PUBLIC prsTransfer ( INPUT pcLogg AS CHARACTER  ):
    SUPER ().

    rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner().
    
    ASSIGN 
      cLogg  = pcLogg
      bTest  = FALSE
      iAntDg = 3
      .
      
    IF cLogg = '' THEN 
      cLogg = 'prsTransfer' + REPLACE(STRING(TODAY),'/','') /*+ '_' + REPLACE(STRING(TIME),':','')*/
      .
  END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID getOverfStatus( INPUT lKOrdre_Id AS DEC, 
                                     INPUT cEkstOrdreNr AS CHARACTER,
                                     INPUT cType AS CHARACTER ):

    KOMLOOP:
    FOR EACH sendtowebtrans NO-LOCK WHERE 
        sendtowebtrans.batchtype = cType AND
        sendtowebtrans.extraparam = ENTRY(1,cEkstOrdreNr,' ') 
        BREAK
        BY sendtowebtrans.batchtype 
        BY sendtowebtrans.extraparam
        BY sendtowebtrans.sent:
        
        
        
        IF LAST-OF(sendtowebtrans.extraparam) THEN 
        DO TRANSACTION:
          FIND bufKOrdreHode EXCLUSIVE-LOCK WHERE 
            bufKOrdreHode.KOrdre_Id = lKORdre_Id NO-ERROR NO-WAIT.
          IF AVAILABLE bufKORdreHode AND NOT LOCKED bufKOrdreHode AND sendtowebtrans.sent <> ? THEN 
          DO:
            ASSIGN 
              bufKORdreHode.webPayload       = sendtowebtrans.payload 
              bufKORdreHode.webSendt         = sendtowebtrans.sent 
              bufKORdreHode.webcurrentstatus = sendtowebtrans.currentstatus
              bufKOrdreHode.WebBatchNr       = sendtowebtrans.batchnr
              bufKOrdreHode.WebKode          = STRING(sendtowebtrans.TransferStatus)
              NO-ERROR.
            
            /* Skal alltid skrives til loggen */            
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '      Ov.status satt på type/Ordre/OrdreId: ' + cType + ' ' + 
                bufKOrdreHode.EkstOrdreNr + '/' + 
                STRING(bufKOrdrEHode.KOrdre_Id) + ' ' + 
                (IF bufKORdreHode.webcurrentstatus = ? THEN '?' ELSE bufKORdreHode.webcurrentstatus) + ' ' +
                (IF STRING(bufKORdreHode.webSendt,"99/99/9999 HH:MM:SS") = ? THEN '?' ELSE STRING(bufKORdreHode.webSendt,"99/99/9999 HH:MM:SS"))   
                ).    
            
            RELEASE bufKOrdreHode.
          END.
          ELSE DO:
            IF bTest THEN 
              rStandardFunksjoner:SkrivTilLogg(cLogg,
                  '      Fant ikke ordre med type/ordreid : ' + cType + '/' + 
                  STRING(bufKOrdrEHode.KOrdre_Id) 
                  ).    
          END.
          
          LEAVE KOMLOOP.
        END. /* TRANSACTION */

    END. /* KOMLOOP */
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID lesSendteOrdre( INPUT cEkstOrdreNr AS CHARACTER, INPUT piAntDg AS INTEGER ):
    DEFINE VARIABLE cType AS CHARACTER NO-UNDO.
    
    ASSIGN 
      iAntDg = (IF piantDg > 0 THEN piAntDg ELSE iAntDg)
      cType = 'SHIP'
      .
    
    IF bTest THEN 
    DO:
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    cType: ' + cType 
          ).    
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    cEkstOrdreNr: ' + cEkstOrdreNr 
          ).    
    END.
    FOR EACH KOrdreHode NO-LOCK WHERE 
      KOrdreHode.LevStatus = '50' AND 
      KOrdreHode.SendingsNr >= '' AND 
      NOT KOrdreHode.SendingsNr MATCHES '*RETUR*' AND
      (IF cEkstOrdreNr <> '' THEN KOrdreHode.EkstOrdreNr = cEkstOrdreNr ELSE TRUE) AND 
      DATE(KOrdreHode.ShipmentSendt) >= TODAY - iAntDg AND 
      NOT KOrdreHode.webcurrentstatus MATCHES '*completed*':

      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    Ordre/OrdreId: ' + KOrdreHode.EkstOrdreNr + '/' + STRING(KOrdrEHode.KOrdre_Id)  
            ).    
        
      getOverfStatus( KOrdreHode.KOrdre_Id, 
                      KOrdreHode.EkstOrdreNr,
                      cType).        
    END.
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID lesSendteReturer( INPUT cEkstOrdreNr AS CHARACTER, INPUT piAntDg AS INTEGER  ):
    DEFINE VARIABLE cType AS CHARACTER NO-UNDO.
    
    ASSIGN 
      iAntDg = (IF piantDg > 0 THEN piAntDg ELSE iAntDg)    
      cType = 'RETUR'
      .

    IF bTest THEN 
    DO:
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    cType: ' + cType 
          ).    
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    cEkstOrdreNr: ' + cEkstOrdreNr 
          ).    
    END.
    
    FOR EACH KOrdreHode NO-LOCK WHERE 
      KOrdreHode.LevStatus = '50' AND 
      KOrdreHode.SendingsNr = cType AND 
      (IF cEkstOrdreNr <> '' THEN ENTRY(1,KOrdreHode.EkstOrdreNr,' ') = cEkstOrdreNr ELSE TRUE) AND 
      DATE(KOrdreHode.ShipmentSendt) >= TODAY - iAntDg AND 
      NOT KOrdreHode.webcurrentstatus MATCHES '*completed*':

      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    Ordre/OrdreId: ' + KOrdreHode.EkstOrdreNr + '/' + STRING(KOrdrEHode.KOrdre_Id)  
            ).    
        
      getOverfStatus( KOrdreHode.KOrdre_Id, 
                      KOrdreHode.EkstOrdreNr,
                      cType + 'N').        
    END.
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID setResendPHX(  ):
    
    DEFINE BUFFER bufKOrdreHode FOR KOrdreHode.
    DEFINE BUFFER bufsendtowebtrans FOR sendtowebtrans.
    
    DO FOR bufKOrdrEHode, bufsendtowebtrans:
      FOR EACH bufKOrdreHode NO-LOCK WHERE WebKode = '1' AND 
        bufKOrdreHode.WebBatchNr > 0:
        FIND bufsendtowebtrans EXCLUSIVE-LOCK WHERE
          bufsendtowebtrans.BatchNr = DEC(bufKOrdrEHode.WebBatchNr) NO-ERROR NO-WAIT.
        IF AVAILABLE bufsendtowebtrans AND NOT LOCKED bufsendtowebtrans THEN
        DO:
          FIND KOrdreHode EXCLUSIVE-LOCK WHERE 
            KOrdreHode.KOrdre_Id = bufKOrdrEHode.KORdre_Id NO-ERROR.
          IF AVAILABLE KOrdreHode AND NOT LOCKED KOrdreHode THEN 
            ASSIGN 
              KOrdreHode.WebKode            = '2'
              bufsendtowebtrans.transferstatus = 1
              bufsendtowebtrans.payload        = ?
              bufsendtowebtrans.sent           = ?
              bufsendtowebtrans.transactionid  = ""
              bufsendtowebtrans.currentstatus  = ""
            .
          IF AVAILABLE KORdreHode THEN 
            RELEASE KOrdreHode.
          IF AVAILABLE bufsendtowebtrans THEN 
            RELEASE bufsendtowebtrans.
        END.        
      END.
    END.
    
    RETURN.

  END METHOD.

  DESTRUCTOR PUBLIC prsTransfer ( ):

  END DESTRUCTOR.

END CLASS.