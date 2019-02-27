
/*  
Field Name                  Data Type  Flg Format
--------------------------- ---------- --- --------------------------------
B_id                        deci-2     i   ->>>>>>>>>>>>>>>>>>>>>9
iJBoxCompanyId              I64        i   ->,>>>,>>9
ButikkNr                    inte       i   >>>>>9
Dato                        date           99/99/99
TransactionId               char       i   X(20)
Sendt                       logi       i   yes/no
SendtDato                   date           99/99/99
SendtTid                    inte       i   ->,>>>,>>9
EDato                       date       i   99/99/9999
ETid                        inte       i   ->,>>>,>>9
BrukerID                    char           X(10)
RegistrertDato              date       i   99/99/9999
RegistrertTid               inte       i   ->,>>>,>>9
RegistrertAv                char           X(10)
DatoSendt                   datetm     i   99/99/9999 HH:MM:SS.SSS
*/  
  
  
DEFINE TEMP-TABLE Transaction_  
    FIELD MEMBER_ID AS CHAR 
    FIELD AMOUNT AS DEC 
    FIELD PURCHASE_DATE AS DATE
    FIELD PURCHASE_TIME AS INTE
    FIELD RECEIPT_NBR AS CHAR  
    FIELD TERMINAL_NBR AS INT
    FIELD WORKPLACE_NBR AS INT
    FIELD CURRENCY AS CHAR INIT "NOR" . 
  
DEFINE TEMP-TABLE transaction_row 
    FIELD row_AMOUNT AS DEC
    FIELD row_AMOUNT_COST AS DEC 
    FIELD row_BONUS_BASED AS CHAR
/*     FIELD row_BONUS_BASED AS LOGICAL FORMAT "true/false" */
    FIELD row_PIECES AS INT 
    FIELD row_PRODUCT_CODE AS CHAR 
    FIELD row_PRODUCT_DESC AS CHAR 
    FIELD row_PRODUCT_GROUP_CODE AS CHAR
    FIELD row_PRODUCT_GROUP_DESC  AS CHAR
    FIELD row_DISCOUNT AS DEC . 

DEFINE TEMP-TABLE transaction_check 
    FIELD check_BAR_CODE AS CHAR
    FIELD check_WORKPLACE_NBR AS INT 
    FIELD check_REDEEM_DATE AS DATE.

DEFINE DATASET InsertTransaction NAMESPACE-URI "http://abalon.se/memberclub/MemberService" 
    FOR TRANSACTION_, transaction_row,TRANSACTION_check.


DEFINE TEMP-TABLE MemberServiceFault NO-UNDO
    NAMESPACE-URI "http://abalon.se/memberclub/MemberService"
    XML-NODE-NAME "MemberServiceFault" 
    FIELD FaultMessage AS CHARACTER XML-NODE-NAME "message"
    FIELD UserFaultMessage AS CHAR .

DEFINE DATASET detail 
    FOR MemberServiceFault.


DEFINE TEMP-TABLE insertTransactionResponse_result NO-UNDO
    XML-NODE-NAME "result" 
    FIELD REFERENCE_NBR AS CHARACTER XML-NODE-NAME "REFERENCE_NBR".

DEFINE DATASET insertTransactionResponse 
    FOR insertTransactionResponse_result.

DEFINE VARIABLE gcMemberid AS CHAR NO-UNDO. 

SESSION:SET-NUMERIC-FORMAT(",",".").

PROCEDURE buildInsertTransaction :
    DEFINE INPUT PARAMETER iBonghodeId AS DEC NO-UNDO. 
    
    DEFINE BUFFER BongLinje FOR BongLinje . 
    DEFINE BUFFER bonghode  FOR bonghode . 
    DEFINE BUFFER artbas    FOR artbas. 
    DEFINE BUFFER medlem    FOR medlem. 
    DEFINE VARIABLE cMemberid AS CHAR NO-UNDO. 
    DEFINE VARIABLE iKoeff    AS INTEGER     NO-UNDO.

    DEFINE VARIABLE cStrekkode AS CHARACTER   NO-UNDO.

    EMPTY TEMP-TABLE Transaction_.
    EMPTY TEMP-TABLE transaction_row.
    EMPTY TEMP-TABLE transaction_check.

    FIND FIRST bonghode WHERE bonghode.b_id = iBonghodeId NO-LOCK NO-ERROR. 
    FIND FIRST medlem WHERE medlem.medlemsnr = bonghode.medlemsnr NO-LOCK NO-ERROR. 

    IF AVAIL medlem AND Medlem.MKlubbId = 1 THEN
    DO:
        cMemberid = medlem.EksterntMedlemsNr. 
        gcMemberid = cMemberid. 
    END. 
    ELSE DO:
        cMemberid = "". 
        gcMemberid = "". 
    END.
/*     ELSE RETURN. */

    CREATE Transaction_.
    ASSIGN 
        Transaction_.member_id     = cMemberid
        Transaction_.AMOUNT        = BongHode.Belop
        Transaction_.PURCHASE_DATE = BongHode.Dato
        Transaction_.PURCHASE_TIME = BongHode.Tid
        Transaction_.RECEIPT_NBR   = STRING(BongHode.BongNr)
        Transaction_.TERMINAL_NBR  = BongHode.KasseNr
        Transaction_.WORKPLACE_NBR = BongHode.ButikkNr
        Transaction_.CURRENCY      = "NOK".

    FOR EACH BongLinje NO-LOCK WHERE
          BongLinje.B_Id = BongHode.B_Id  
           AND BongLinje.Makulert = FALSE AND           
            CAN-DO('1,3,10',STRING(BongLinje.TTId))  :
          iKoeff = IF BongLinje.TTId = 1 THEN 1 ELSE -1.

          FIND ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = DECIMAL(BongLinje.ArtikkelNr) NO-ERROR.
          IF AVAIL ArtBas THEN DO:
              cStrekKode = TRIM(BongLinje.Strekkode).
              IF cStrekkode = "" THEN DO:
                  FIND strkonv WHERE strkonv.storl = bonglinje.storrelse NO-LOCK NO-ERROR.
                  IF AVAIL strkonv THEN DO:
                      FIND FIRST strekkode WHERE strekkode.artikkelnr = artbas.artikkelnr
                          AND strekkode.strkode = strkonv.strkode NO-LOCK NO-ERROR.
                      IF AVAIL strekkode THEN
                          cStrekkode = strekkode.kode.
                  END.
              END.
              CREATE transaction_row.
              ASSIGN 
                  transaction_row.row_AMOUNT             = iKoeff * (BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab) 
                  transaction_row.row_AMOUNT_COST        = iKoeff * (BongLinje.VVareKost * ABS(BongLinje.Antall))
                  transaction_row.row_BONUS_BASED        = (IF AVAILABLE ArtBas THEN (IF ArtBas.Bonus_Givende THEN "true" ELSE "false") ELSE "false")  
                  transaction_row.row_PIECES             = BongLinje.Antall
                  transaction_row.row_PRODUCT_CODE       = cStrekkode /* BongLinje.Strekkode*/
                  transaction_row.row_PRODUCT_DESC       = BongLinje.BongTekst 
                  transaction_row.row_PRODUCT_GROUP_CODE = STRING(BongLinje.VareGr) 
                  transaction_row.row_PRODUCT_GROUP_DESC = BongLinje.VareGruppeNavn
                  transaction_row.row_DISCOUNT           = -1 * iKoeff * (BongLinje.LinjeRab + BongLinje.SubtotalRab). /* vid retur rekl positiv annars negativ */
          END.
    END. 

    FOR EACH BongLinje NO-LOCK WHERE
        BongLinje.B_Id     = BongHode.B_Id  AND 
        BongLinje.Makulert = FALSE          AND
        CAN-DO('98',STRING(BongLinje.TTId)) AND
        BongLinje.BongTekst BEGINS 'MayFlower Sjeck': 

        CREATE transaction_check.
        ASSIGN 
            transaction_check.check_BAR_CODE      =  BongLinje.Strekkode 
            transaction_check.check_WORKPLACE_NBR =  BongLinje.ButikkNr 
            transaction_check.check_REDEEM_DATE   =  bongLinje.Dato. 

    END. 
END. 




FUNCTION getInsertTransactionRequest RETURN LONGCHAR (INPUT DATASET InsertTransaction,INPUT icUserName AS CHAR, INPUT icPassword AS CHAR ):

    DEFINE VARIABLE hWriter AS HANDLE NO-UNDO.
    DEFINE VARIABLE lcxml AS LONGCHAR NO-UNDO. 
    DEFINE VARIABLE cPrefix AS CHAR INIT "ns0" NO-UNDO.
    DEFINE VARIABLE cReq AS LONGCHAR NO-UNDO. 
    
    CREATE SAX-WRITER hWriter.
    hWriter:FRAGMENT = TRUE. 
    hWriter:FORMATTED = TRUE. 
    
    hWriter:SET-OUTPUT-DESTINATION("LONGCHAR",lcxml).
    hWriter:START-DOCUMENT().
    
    hWriter:START-ELEMENT(cPrefix + ":" + 'insertTransaction').
    hWriter:DECLARE-NAMESPACE("http://abalon.se/mfService",cPrefix).
   

    FOR EACH TRANSACTION_ NO-LOCK : 
        hWriter:START-ELEMENT("Transaction").
        
        IF Transaction_.member_id NE '' THEN
        DO:
            hWriter:START-ELEMENT("MEMBER_ID").
            hWriter:WRITE-CHARACTERS(Transaction_.member_id).
            hWriter:END-ELEMENT("MEMBER_ID").
        END. 
        
           
        hWriter:START-ELEMENT("AMOUNT").
        hWriter:WRITE-CHARACTERS(TRIM(STRING(Transaction_.AMOUNT,"->>>>>>>>>>9.99"))).
        hWriter:END-ELEMENT("AMOUNT").

        hWriter:START-ELEMENT("PURCHASE_DATE").
        hWriter:WRITE-CHARACTERS(      STRING(YEAR(Transaction_.PURCHASE_DATE),"9999") + 
                                 "-" + STRING(MONTH(Transaction_.PURCHASE_DATE),"99") + 
                                 "-" + STRING(DAY(Transaction_.PURCHASE_DATE),"99")   + 
                                 "T" + STRING(Transaction_.PURCHASE_TIME,"HH:MM:SS")).
                
        hWriter:END-ELEMENT("PURCHASE_DATE").

        hWriter:START-ELEMENT("RECEIPT_NBR").
        hWriter:WRITE-CHARACTERS(STRING (Transaction_.RECEIPT_NBR)).
        hWriter:END-ELEMENT("RECEIPT_NBR").

        hWriter:START-ELEMENT("TERMINAL_NBR").
        hWriter:WRITE-CHARACTERS(STRING(Transaction_.TERMINAL_NBR)).
        hWriter:END-ELEMENT("TERMINAL_NBR").
        
        hWriter:START-ELEMENT("WORKPLACE_NBR").
        hWriter:WRITE-CHARACTERS(STRING(transaction_.WORKPLACE_NBR)).
        hWriter:END-ELEMENT("WORKPLACE_NBR").

        hWriter:START-ELEMENT("CURRENCY").
        hWriter:WRITE-CHARACTERS(Transaction_.CURRENCY).
        hWriter:END-ELEMENT("CURRENCY").

/*         hWriter:START-ELEMENT("rows"). */

        FOR EACH transaction_row NO-LOCK:
            hWriter:START-ELEMENT("rows").
                hWriter:START-ELEMENT("AMOUNT").
                hWriter:WRITE-CHARACTERS(TRIM(STRING(Transaction_row.row_AMOUNT,"->>>>>>>>>>9.99"))).
                hWriter:END-ELEMENT("AMOUNT").
                
                hWriter:START-ELEMENT("AMOUNT_COST").
                hWriter:WRITE-CHARACTERS(TRIM(STRING(Transaction_row.row_AMOUNT_COST,"->>>>>>>>>>9.99"))).
                hWriter:END-ELEMENT("AMOUNT_COST").
    
                hWriter:START-ELEMENT("BONUS_BASED").
                hWriter:WRITE-CHARACTERS(STRING(transaction_row.row_BONUS_BASED)).
                hWriter:END-ELEMENT("BONUS_BASED").
                
                hWriter:START-ELEMENT("PIECES").
                hWriter:WRITE-CHARACTERS(STRING(transaction_row.row_PIECES)).
                hWriter:END-ELEMENT("PIECES").
            
                hWriter:START-ELEMENT("PRODUCT").
                    hWriter:START-ELEMENT("CODE").
                    hWriter:WRITE-CHARACTERS(transaction_row.row_PRODUCT_CODE).
                    hWriter:END-ELEMENT("CODE").
                    
                    hWriter:START-ELEMENT("DESC").
                    hWriter:WRITE-CHARACTERS(transaction_row.row_PRODUCT_DESC).
                    hWriter:END-ELEMENT("DESC").
                hWriter:END-ELEMENT("PRODUCT").
    
                hWriter:START-ELEMENT("PRODUCT_GROUP").
                    hWriter:START-ELEMENT("CODE").
                    hWriter:WRITE-CHARACTERS(transaction_row.row_PRODUCT_GROUP_CODE).
                    hWriter:END-ELEMENT("CODE").
                    
                    hWriter:START-ELEMENT("DESC").
                    hWriter:WRITE-CHARACTERS(transaction_row.row_PRODUCT_GROUP_DESC).
                    hWriter:END-ELEMENT("DESC").
                hWriter:END-ELEMENT("PRODUCT_GROUP").
    
    
                hWriter:START-ELEMENT("DISCOUNT").
                hWriter:WRITE-CHARACTERS(TRIM(STRING(Transaction_row.row_DISCOUNT,"->>>>>>>>>>9.99"))).
                hWriter:END-ELEMENT("DISCOUNT").

            hWriter:END-ELEMENT("rows").
        END.
/*         hWriter:END-ELEMENT("rows"). */
        
        hWriter:END-ELEMENT("Transaction").
    END. 


    /* User */ 
    hWriter:START-ELEMENT("User").
        hWriter:START-ELEMENT("username").
        hWriter:WRITE-CHARACTERS(icUserName).
        hWriter:END-ELEMENT("username").
    
        hWriter:START-ELEMENT("password").
        hWriter:WRITE-CHARACTERS(icPassword).
        hWriter:END-ELEMENT("password").
    
        hWriter:START-ELEMENT("roleCode").
        hWriter:WRITE-CHARACTERS('wsuser').
        hWriter:END-ELEMENT("roleCode").
    hWriter:END-ELEMENT("User").
    
    hWriter:END-ELEMENT(cPrefix + ":" + 'insertTransaction').
    hWriter:END-DOCUMENT().
    
    DELETE OBJECT hWriter. 
/* OUTPUT TO "c:\tmp\insert.xml" APPEND. */
/* PUT UNFORMATTED STRING(lcxml) SKIP.   */
/* OUTPUT CLOSE.                         */
    RETURN lcxml.
END. 


PROCEDURE UpdateNetsTable : 

    DEFINE BUFFER nets FOR skotex.nets. 

    FOR EACH nets WHERE nets.dato < TODAY - 4:
        IF nets.transactionid = ? THEN
            NEXT.
        DELETE nets.
    END.

    FOR EACH butiker NO-LOCK : 
         FOR EACH bonghode WHERE bonghode.dato GE TODAY - 2 
                             AND bonghode.butikknr  = butiker.butik  NO-LOCK: 
             IF Bonghode.makulert = 2 THEN
                 NEXT.
             IF NOT CAN-FIND(FIRST bonglinje WHERE bonglinje.b_id = bonghode.b_id AND (bonglinje.ttid = 1 OR bonglinje.ttid = 3 OR bonglinje.ttid = 10)) THEN
                 NEXT.
/*              IF bonghode.medlemskort NE "" AND bonghode.medlemsnr NE 0 AND bonghode.belop NE 0  THEN */
             DO:
                 FIND nets WHERE nets.b_id = bonghode.b_id NO-LOCK NO-ERROR.   
                 IF NOT AVAIL nets THEN
                 DO:
                     CREATE nets. 
                     nets.butikknr         = bonghode.butikknr.
                     nets.b_id             = bonghode.b_id. 
                     nets.transactionid    = ?.
                     nets.dato             = bonghode.dato.
                     nets.edato            = TODAY.
                     nets.etid             = TIME. 
                     nets.Registrertdato   = TODAY.
                     nets.registrerttid    = TIME.                 
                 END.
             END. 
         END.
    END.
END. 

/*
    LOG-MANAGER:LOGFILE-NAME    = SESSION:TEMP-DIR + "MayFlowerTransaction.log".
   /* LOG-MANAGER:LOG-ENTRY-TYPES = "4gltrace:4".*/
    LOG-MANAGER:LOGGING-LEVEL   = 3.
    
    FILE-INFO:FILE-NAME = LOG-MANAGER:LOGFILE-NAME.
    IF FILE-INFO:FILE-SIZE GE 10000000 THEN LOG-MANAGER:CLEAR-LOG(). 
*/

  LOG-MANAGER:WRITE-MESSAGE("MayFlowerTransaction -- Starting --","BATCH").

 DEFINE VARIABLE cInput AS LONGCHAR NO-UNDO. 
 DEFINE VARIABLE cUsername AS CHARACTER INIT "polygon"NO-UNDO.
 DEFINE VARIABLE cPassword AS CHARACTER INIT "polygon09PRS" NO-UNDO.
 DEFINE VARIABLE hWebService AS HANDLE NO-UNDO. 
 DEFINE VARIABLE cWsdlFil AS CHAR NO-UNDO. 
 DEFINE VARIABLE WsdlParam AS CHAR NO-UNDO.
 DEFINE VARIABLE hFunctionService AS HANDLE NO-UNDO. 
 DEFINE VARIABLE lok AS LOG INIT TRUE NO-UNDO. 
 DEFINE BUFFER nets   FOR skotex.Nets. 
 DEFINE BUFFER bfNets FOR skotex.nets. 


 cWsdlFil = "c:\tmp\Transactionservice.wsdl". 
 cWsdlFil = "WS\MayFlower\WSTransaction\wsdl\Transactionservice.wsdl". 

 DEFINE VARIABLE hSoapFaultDetail AS HANDLE   NO-UNDO. 
 DEFINE VARIABLE lcSoapFault      AS LONGCHAR NO-UNDO. 
 DEFINE VARIABLE lSoapFault       AS LOG      NO-UNDO. 
 DEFINE VARIABLE gcErrorMessage   AS CHAR     NO-UNDO. 
 DEFINE VARIABLE cMemberid        AS CHAR NO-UNDO. 
 DEF VAR lcResponse AS LONGCHAR NO-UNDO. 

 RUN UpdateNetsTable.

 CREATE SERVER hWebService.
 hWebService:CONNECT("-WSDL " + cWsdlFil ) NO-ERROR.

 IF NOT hWebService:CONNECTED() THEN 
        MESSAGE "feil ved connect" VIEW-AS ALERT-BOX. 
     ELSE 
 OPPKOBLET:
 DO:

     RUN Transactionservice SET hFunctionService ON hWebService NO-ERROR.

     IF ERROR-STATUS:ERROR THEN
     DO:
        MESSAGE "feil ved memberservice " VIEW-AS ALERT-BOX. 
         LEAVE OPPKOBLET.
     END.

     loop:
     FOR EACH Nets WHERE nets.sendt = FALSE AND DatoSendt = ? NO-LOCK: 

         RUN buildInsertTransaction(nets.b_id). 

         cInput = getInsertTransactionRequest ( INPUT DATASET InsertTransaction,cUsername,cPassword).
/*          COPY-LOB cInput TO FILE "c:\tmp\WS_MayFlower_" + "Request_getInsertTransactionRequest.xml". */
         RUN insertTransaction IN hFunctionService(INPUT cInput,OUTPUT lcresponse) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN 
         DO:
             IF ERROR-STATUS:ERROR-OBJECT-DETAIL NE ? THEN
             DO:  
                 hSoapFaultDetail = ERROR-STATUS:ERROR-OBJECT-DETAIL:SOAP-FAULT-DETAIL.
                 IF VALID-HANDLE(hSoapFaultDetail) THEN
                     lcSoapFault = hSoapFaultDetail:GET-SERIALIZED().
                 lSoapFault = TRUE.
    
                 DATASET detail:READ-XML("longchar",lcSoapFault,"empty","", ?, ?, ?) NO-ERROR.
                 FIND FIRST memberservicefault NO-LOCK NO-ERROR. 
                 IF AVAIL memberservicefault THEN 
                 DO:
                     MemberServiceFault.userfaultmessage = LEFT-TRIM(TRIM(ENTRY(2,MemberServiceFault.faultmessage,":"))). 
                     gcErrorMessage =  MemberServiceFault.userfaultmessage. 
                 END. 
                 
                 
                 IF LOG-MANAGER:LOGGING-LEVEL GE 3 THEN 
                     LOG-MANAGER:WRITE-MESSAGE(STRING(lcSoapFault),"WSERROR").
             END.
             lOk = FALSE. 
         END.              
    
    
         IF NOT lok THEN 
         DO:
/*              MESSAGE STRING(lcResponse) VIEW-AS ALERT-BOX.                          */
/*              MESSAGE "errror" gcErrorMessage string(lcSoapFault) VIEW-AS ALERT-BOX. */
            LOG-MANAGER:WRITE-MESSAGE(gcErrorMessage,"ERROR").

            FIND bfNets WHERE ROWID(bfnets) = ROWID(nets) EXCLUSIVE-LOCK NO-ERROR. 
            bfNets.DatoSendt     = NOW. 
            bfNets.Sendt         = ?. 
            bfNets.SendtDato     = TODAY.
            bfNets.SendtTId      = TIME. 
         END. 
         ELSE
         DO:

            DATASET insertTransactionResponse:READ-XML("longchar",lcResponse,"empty","", ?, ?, ?) NO-ERROR.
            FIND FIRST  insertTransactionResponse_result NO-LOCK NO-ERROR. 
            IF AVAIL  insertTransactionResponse_result THEN 
            DO:
                FIND bfNets WHERE ROWID(bfnets) = ROWID(nets) EXCLUSIVE-LOCK NO-ERROR. 
                bfNets.Transactionid = insertTransactionResponse_result.REFERENCE_NBR.
                bfNets.DatoSendt     = NOW. 
                bfNets.Sendt         = TRUE. 
                bfNets.SendtDato     = TODAY.
                bfNets.SendtTId      = TIME. 
                LOG-MANAGER:WRITE-MESSAGE("OK:" + bfNets.Transactionid + " Memberid:" + gcMemberid,"INFO").
            END. 

         END. 
         
     END. 

     hWebService:DISCONNECT().

     DELETE OBJECT hWebService NO-ERROR. 
     DELETE OBJECT hFunctionService NO-ERROR.

  END. 
  LOG-MANAGER:WRITE-MESSAGE("MayFlowerTransaction -- Completed --","BATCH").

 


