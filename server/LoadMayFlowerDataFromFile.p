
DEFINE TEMP-TABLE getMemberResponseReturn NO-UNDO
        NAMESPACE-URI "" 
        XML-NODE-NAME "return" 
        FIELD memberId AS CHARACTER FORMAT "x(20)"
        FIELD accountNumber AS CHAR FORMAT "x(20)"
        FIELD cardNumber AS CHARACTER 
        FIELD password AS CHARACTER 
        FIELD webcode AS CHARACTER 
        FIELD firstName AS CHAR
        FIELD lastName AS CHAR 
        FIELD gender AS CHAR 
        FIELD mobile AS CHAR     
        FIELD phone AS CHAR 
        FIELD socialnr AS CHAR
        FIELD email  AS CHAR 
        FIELD zip  AS CHAR 
        FIELD postarea AS CHAR
        FIELD countryCode AS CHAR
        FIELD address AS CHAR
        FIELD birthdate AS CHAR. 



DEF STREAM out. 

    DEF VAR icnt AS INT. 

INPUT  STREAM out FROM VALUE ("c:\tmp\member-mayflower.d").
REPEAT: 
    CREATE getMemberResponseReturn.
    IMPORT STREAM out getMemberResponseReturn. 
END.
OUTPUT STREAM out CLOSE. 


FOR EACH getMemberResponseReturn : 
    icnt = icnt + 1. 
END.
MESSAGE icnt VIEW-AS ALERT-BOX. 

DEFINE VARIABLE gButikknr AS INT INIT 1 NO-UNDO.
    DEFINE VARIABLE gGruppeNr AS INT NO-UNDO. 
DEFINE VARIABLE gKasseNr  AS INT NO-UNDO. 

DEFINE VARIABLE hbf AS HANDLE NO-UNDO. 
DEFINE VARIABLE wslOk AS LOG NO-UNDO. 
DEFINE VARIABLE wsRet AS CHAR NO-UNDO. 
DEFINE VARIABLE lMedlemsNr AS INT NO-UNDO.
DEFINE VARIABLE cMKlubbId AS CHAR NO-UNDO. 
DEFINE VARIABLE cKortNr AS CHAR NO-UNDO. 
DEFINE VARIABLE cPostSted AS CHAR NO-UNDO. 

{syspara.i 14 1  7 cMKlubbId}

                                
 FOR EACH getMemberResponseReturn NO-LOCK 
     TRANSACTION:
            
         ASSIGN
              cKortNr = LEFT-TRIM(REPLACE(TRIM(getMemberResponseReturn.accountNumber),'M-',''),'0').
              
        FIND FIRST MedlemsGruppe NO-LOCK NO-ERROR.
        FIND FIRST MedlemsType   NO-LOCK NO-ERROR.
        
        ASSIGN lMedlemsNr = DEC(cKortNr) NO-ERROR.
        
         IF lMedlemsNr = 0 OR CAN-FIND(Medlem WHERE Medlem.MedlemsNr = lMedlemsNr) OR lMedlemsNr = ? THEN 
         DO:
              FIND LAST medlem WHERE medlemsnr NE ? NO-LOCK NO-ERROR.
              IF AVAILABLE Medlem THEN 
                lMedlemsNr = Medlem.MedlemsNr + 1.
              ELSE 
                lMedlemsNr = 1.
         END. 

         FIND medlem WHERE medlem.medlemsnr = lMedlemsNr NO-ERROR. 
         IF NOT AVAIL  medlem THEN 
         DO:
             FIND FIRST medlem WHERE Medlem.EksterntMedlemsNr = getMemberResponseReturn.accountNumber NO-ERROR.
             IF NOT AVAIL medlem THEN
                 CREATE medlem. 
         END. 

         FIND FIRST Post WHERE 
           Post.PostNr = TRIM(getMemberResponseReturn.zip) NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Post THEN 
         DO:
             CREATE Post.
             ASSIGN
                 Post.PostNr      = TRIM(getMemberResponseReturn.zip)
                 Post.Beskrivelse = TRIM(getMemberResponseReturn.postarea).
             FIND CURRENT Post NO-LOCK.
         END.
        

         DISP icnt lMedlemsNr . icnt = icnt + 1.  PAUSE 0. 
         ASSIGN 
             Medlem.MedlemsNr = lMedlemsNr
             Medlem.PersonNr  = getMemberResponseReturn.socialnr
             Medlem.ForNavn   = TRIM(getMemberResponseReturn.firstname)
             Medlem.EtterNavn = TRIM(getMemberResponseReturn.lastname)
             Medlem.PostNr    = TRIM(getMemberResponseReturn.zip)
             Medlem.Adresse2  = ""
             Medlem.Adresse1  = getMemberResponseReturn.address
             Medlem.MedGruppe = IF AVAILABLE MedlemsGruppe THEN MedlemsGruppe.MedGruppe ELSE 1
             Medlem.MedType   = IF AVAILABLE MedlemsType   THEN MedlemsType.MedType ELSE 1
             Medlem.MKlubbId  = INT(cMKlubbId)
             Medlem.ButikkNr  = gButikknr
             Medlem.Kjonn     = CAN-DO('male,man,m',TRIM(getMemberResponseReturn.gender))
             Medlem.EksterntMedlemsNr = getMemberResponseReturn.accountNumber
             Medlem.Kilde        = 'MayFlower'
             Medlem.ePostAdresse = getMemberResponseReturn.email
             Medlem.MobilTlf     = 
                 IF LENGTH(getMemberResponseReturn.mobile) = 12 THEN
                 SUBSTRING(getMemberResponseReturn.mobile,5) ELSE getMemberResponseReturn.mobile
                                   
             Medlem.Telefon      = getMemberResponseReturn.phone . 
             Medlem.Bonus_Berettiget = FALSE. 
             /* Medlem.FodselsDato  = hParameterTable::FodselsDato.*/ 
        
         FIND FIRST  medlemskort WHERE 
              MedlemsKort.KortNr = cKortNr 
              NO-LOCK NO-ERROR.

         IF NOT AVAIL MedlemsKort THEN
         DO:

         CREATE MedlemsKort.
         ASSIGN
             MedlemsKort.MedlemsNr    = Medlem.MedlemsNr
             MedlemsKort.KortNr       = cKortNr
             MedlemsKort.AktivertDato = TODAY 
             MedlemsKort.UtgarDato    = TODAY + 999
             MedlemsKort.Innehaver    = Medlem.Fornavn + Medlem.EtterNavn
             MedlemsKort.KortType     = 1 NO-ERROR.
         END. 
        
         cPostSted = "". 
     FIND FIRST post WHERE 
                Post.PostNr      = Medlem.PostNr NO-LOCK NO-ERROR. 
     IF AVAIL post THEN
     cPostSted = post.beskrivelse. 

     
        IF AVAILABLE Medlem      THEN RELEASE medlem.
        IF AVAILABLE Medlemskort THEN RELEASE MedlemsKort.
        
     END. /* TRANSACTION */

     
     
 

