
/*------------------------------------------------------------------------
    File        : customerToMedlem.p
    Purpose     : 

    Syntax      :

    Description : Oppdaterer medlem fra datasett dsCustCustomer.

    Author(s)   : Tom Nøkleby
    Created     : Wed Dec 09 11:27:01 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
BLOCK-LEVEL ON ERROR UNDO, THROW.

{cls\dintero\ttCustomerObj.i}
{cls\dintero\dsCustomerObj.i}

DEFINE INPUT PARAMETER DATASET FOR dsCustCustomer.
DEFINE OUTPUT PARAMETER cStatus AS CHARACTER NO-UNDO.

DEFINE VARIABLE lMedlemsNr AS DECIMAL NO-UNDO.

/* Sikrer at ikke medlemmet logges for overføring tilbake til Dintero. */
ON WRITE  OF Medlem OVERRIDE 
  DO: 
  END.
ON WRITE  OF Medlemskort OVERRIDE 
  DO: 
  END.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FIND FIRST ttCustCustomer NO-ERROR.
IF NOT AVAILABLE ttCustCustomer THEN 
DO:
  cStatus = 'Medlem UKJENT.'.
  RETURN.
END.

ASSIGN 
  lMedlemsNr = DEC(ttCustCustomer.customer_id)
  NO-ERROR.
IF ERROR-STATUS:ERROR THEN 
  DO:
    cStatus = 'Ugyldig medlemsid: ' + ttCustCustomer.customer_id + '.'.
    RETURN.
  END.
  
UPDATEBLOKK:  
DO ON ERROR UNDO, LEAVE TRANSACTION:
  FIND FIRST Medlem EXCLUSIVE-LOCK WHERE
    Medlem.EksterntMedlemsNr = ttCustCustomer.customer_id NO-ERROR NO-WAIT.
  IF AVAILABLE Medlem AND LOCKED Medlem THEN 
    DO:
      cStatus = 'Medlem LÅST.'.
      LEAVE UPDATEBLOKK.
    END.  
  IF NOT AVAILABLE Medlem THEN 
  DO:
    /* NB: Medlemsnr. settes i create trigger */
    CREATE Medlem.
    ASSIGN 
      Medlem.EksterntMedlemsNr = ttCustCustomer.customer_id
    cStatus = 'Medlem OPPRETTET.'.
  END.
  ELSE 
    cStatus = 'Medlem OPPDATERT.'.

  ASSIGN   
    Medlem.PersonNr     = ''
    Medlem.MedType      = IF ttCustCustomer.custType = 'customer' THEN 3
                       ELSE IF ttCustCustomer.custType = 'company' THEN 2
                       ELSE 1 
    Medlem.ForNavn      = ttCustCustomer.first_name
    Medlem.EtterNavn    = ttCustCustomer.last_name
    Medlem.ePostAdresse = ttCustCustomer.email 
    Medlem.MobilTlf     = ttCustCustomer.phone_number
    Medlem.ButikkNr     = INT(ttCustCustomer.favorite_store)  
    Medlem.Aktiv        = CAN-DO('Aktiv,TRUE,YES',ttCustCustomer.custStatus) 
    Medlem.MKlubbId     = 1 /* GANT Exclusive */
    Medlem.MedGruppe    = 1
    Medlem.Kjonn        = IF ttCustCustomer.gender = 'male' THEN TRUE ELSE FALSE
    Medlem.Kilde        = 'Dintero'
    Medlem.EDato        = TODAY
    Medlem.ETid         = TIME 
    Medlem.BrukerID     = USERID('skotex')
    .

  IF LENGTH(ttCustCustomer.date_of_birth) = 8 THEN 
    Medlem.FodselsDato = DATE(INT(SUBSTRING(ttCustCustomer.date_of_birth,5,2)),
      INT(SUBSTRING(ttCustCustomer.date_of_birth,1,4)),
      INT(SUBSTRING(ttCustCustomer.date_of_birth,7,2))
      ).
  ELSE 
    Medlem.FodselsDato = ?.  

  FIND FIRST ttCustAddresses WHERE 
    ttCustAddresses.addrType = 'home' NO-ERROR.      
  IF AVAILABLE ttCustAddresses THEN 
  DO:
    ASSIGN 
      Medlem.PostNr   = ttCustAddresses.postal_code
      Medlem.Adresse1 = ttCustAddresses.address_line
      Medlem.Adresse2 = ttCustAddresses.address_line_2 
      Medlem.Land     = ttCustAddresses.country 
      .
  END.

  FIND FIRST ttCustMarketing_consent WHERE 
    ttCustMarketing_consent.customer_id = ttCustCustomer.Customer_id AND 
    ttCustMarketing_consent.consent_id = 'sms' NO-ERROR.
  IF AVAILABLE ttCustMarketing_consent THEN
    Medlem.MottaeSMSUtsendelser = IF ttCustMarketing_consent.consent = TRUE THEN TRUE ELSE FALSE.

  FIND FIRST ttCustMarketing_consent WHERE 
    ttCustMarketing_consent.customer_id = ttCustCustomer.Customer_id AND 
    ttCustMarketing_consent.consent_id = 'email' NO-ERROR.
  IF AVAILABLE ttCustMarketing_consent THEN 
    Medlem.MottaeMailUtsendelser = IF ttCustMarketing_consent.consent = TRUE THEN TRUE ELSE FALSE.
     
  FIND MedlemsKort EXCLUSIVE-LOCK WHERE 
    MedlemsKort.KortNr = STRING(Medlem.MedlemsNr) + STRING(Medlem.MKlubbId) NO-ERROR.
  IF NOT AVAILABLE MedlemsKort THEN 
  DO:    
    CREATE MedlemsKort.
    ASSIGN
      MedlemsKort.MedlemsNr = Medlem.MedlemsNr
      MedlemsKort.KortNr    = STRING(Medlem.MedlemsNr) + STRING(Medlem.MKlubbId).
  END.
  ASSIGN    
    MedlemsKort.AktivertDato = TODAY 
    MedlemsKort.UtgarDato    = ?
    MedlemsKort.Innehaver    = Medlem.Fornavn + Medlem.EtterNavn
    MedlemsKort.KortType     = 1
    .
  FIND CURRENT MedlemsKort NO-LOCK.
  RELEASE MedlemsKort.
  FIND CURRENT Medlem NO-LOCK.

  RELEASE Medlem.
  
END. /* UPDATEBLOKK TRANSACTION */

