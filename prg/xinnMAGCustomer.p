&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xinnBITSCustomer.p
    Purpose     :

    Syntax      :

    Description : Leser inn kunde fra BITS nettbutikk ePages.

    Author(s)   : Tom Nøkleby
    Created     : 13/5-09
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.
DEF VAR ctmpKatalog   AS CHAR NO-UNDO.
DEF VAR pcLinje       AS CHAR NO-UNDO.
DEFINE VARIABLE cReturnStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOk AS LOGICAL     NO-UNDO.

DEF STREAM InnFil.
DEF STREAM UtVPI.
DEF STREAM UtVre.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR.


DEFINE TEMP-TABLE headerInf SERIALIZE-NAME "header" 
    FIELD messageID AS CHAR 
    FIELD SendTime  AS DATETIME.

DEFINE TEMP-TABLE Customer SERIALIZE-NAME "CustomerRow" 
    FIELD customerID AS CHAR FORMAT "X(20)"
    FIELD extcustomerID  AS CHAR
    FIELD operation AS CHAR 
    FIELD ContactTitle AS CHAR SERIALIZE-NAME "title"
    FIELD ContactFirstName AS CHAR SERIALIZE-NAME "firstname" FORMAT "X(20)"
    FIELD contactLastName AS CHAR SERIALIZE-NAME "lastname"  FORMAT "X(20)"
    FIELD dob AS CHAR
    FIELD email AS CHAR FORMAT "X(40)".

DEFINE TEMP-TABLE shippingaddress SERIALIZE-NAME "shippingaddress" 
    FIELD customerID AS CHAR FORMAT "X(20)"
    FIELD firstname  AS CHAR FORMAT "X(20)"
    FIELD lastname AS CHAR FORMAT "X(20)"
    FIELD city AS CHAR FORMAT "X(30)"
    FIELD postcode AS CHAR FORMAT "X(10)"
    FIELD street AS CHAR FORMAT "X(40)"
    FIELD telephone AS CHAR FORMAT "X(15)"
    FIELD is_default_shipping AS CHARACTER 
    FIELD is_default_billing AS CHARACTER 
    FIELD customer_address_id AS CHAR.

DEFINE TEMP-TABLE billingaddress SERIALIZE-NAME "billingaddress" 
    FIELD customerID AS CHAR FORMAT "X(20)"
    FIELD firstname  AS CHAR FORMAT "X(20)"
    FIELD lastname AS CHAR FORMAT "X(20)"
    FIELD city AS CHAR FORMAT "X(30)"
    FIELD postcode AS CHAR FORMAT "X(10)"
    FIELD street AS CHAR FORMAT "X(40)"
    FIELD telephone AS CHAR FORMAT "X(15)"
    FIELD is_default_shipping AS CHARACTER 
    FIELD is_default_billing AS CHARACTER 
    FIELD customer_address_id AS CHAR.

DEFINE TEMP-TABLE CustomerNew NO-UNDO
    FIELD customerID AS CHAR FORMAT "X(20)"
    FIELD extcustomerID AS CHAR FORMAT "X(20)"
    FIELD operation AS CHAR
    FIELD ctitle AS CHAR
    FIELD firstname1 AS CHAR FORMAT "X(20)"
    FIELD lastname1 AS CHAR FORMAT "X(20)"
    FIELD dob AS CHAR
    FIELD email AS CHAR FORMAT "X(40)"
    FIELD firstname2 AS CHAR FORMAT "X(20)"
    FIELD lastname2 AS CHAR FORMAT "X(20)"
    FIELD city2 AS CHAR FORMAT "X(30)"
    FIELD postcode2 AS CHAR FORMAT "X(10)"
    FIELD street2 AS CHAR FORMAT "X(40)"
    FIELD telephone2 AS CHAR FORMAT "X(15)"
    FIELD is_default_shipping2 AS CHARACTER 
    FIELD is_default_billing2 AS CHARACTER 
    FIELD customer_address_id2 AS CHAR
    FIELD firstname3 AS CHAR FORMAT "X(20)"
    FIELD lastname3 AS CHAR FORMAT "X(20)"
    FIELD city3 AS CHAR FORMAT "X(30)"
    FIELD postcode3 AS CHAR FORMAT "X(10)"
    FIELD street3 AS CHAR FORMAT "X(40)"
    FIELD telephone3 AS CHAR FORMAT "X(15)"
    FIELD is_default_shipping3 AS CHARACTER 
    FIELD is_default_billing3 AS CHARACTER 
    FIELD customer_address_id3 AS CHAR.

{windows.i}

/*{xinnBITSCustomer.i}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 19.05
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
FOR EACH tt_Error:
  DELETE tt_Error.
END.

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

RUN LesInnFil.

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CustomerUpdate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustomerUpdate Procedure 
PROCEDURE CustomerUpdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO TRANSACTION:
       /* Henter eksisterende eller oppretter ny kunde. */
       FIND FIRST Kunde EXCLUSIVE-LOCK WHERE
           Kunde.EksterntKundeNr =  STRING(CustomerNew.customerID) NO-ERROR.
  
       /* NB: Kundenummer og kundekort opprettes automatisk av db trigger c_kunde.p */
       /* Her settes de felt som skal initieres ved ny kunde fra nettbutikk.        */
       IF NOT AVAILABLE Kunde THEN
       DO:
           FIND FIRST KundeType NO-LOCK NO-ERROR.
           FIND FIRST KundeGruppe NO-LOCK NO-ERROR.
           CREATE Kunde.                                     
           ASSIGN
               Kunde.EksterntKundeNr = STRING(CustomerNew.customerID)
               Kunde.Kilde           = "ePages"
/*               Kunde.TilgKilde           = tt_billingAddress.displayName*/
               Kunde.WebKunde        = TRUE 
               Kunde.Aktiv           = TRUE
               Kunde.TypeId          = IF AVAILABLE KundeType THEN KundeType.TypeId ELSE 0
               Kunde.GruppeId        = IF AVAILABLE KundeGruppe THEN KundeGruppe.GruppeId ELSE 0
               Kunde.Navn            = CustomerNew.firstname1 + " " + 
                                       CustomerNew.lastname1
               Kunde.KontNavn        = CustomerNew.firstName1 + " " +
                                       CustomerNew.lastname1                                  
               Kunde.Adresse1        = CustomerNew.street2
               Kunde.Adresse2        = " "            
               Kunde.PostNr          = CustomerNew.postcode2
               Kunde.Land            = (IF AVAILABLE NumLandKode THEN NumLandKode.Land ELSE Kunde.Land)
               Kunde.ByNavn          = CustomerNew.city2
               Kunde.Telefon         = CustomerNew.telephone2
               Kunde.MobilTlf        = CustomerNew.telephone2
               Kunde.ePostAdresse    = CustomerNew.email
               Kunde.RegistrertDato  = TODAY
               Kunde.RegistrertTid   = TIME
               Kunde.LevAdresse1     = CustomerNew.street2
               Kunde.LevAdresse2     = " "            
               Kunde.LevPostNr       = CustomerNew.postcode2.
               IF CustomerNew.ctitle = "M" THEN
                  Kunde.Kjon = 1.
               ELSE IF CustomerNew.ctitle = "F" THEN
                  Kunde.Kjon = 2.
               ELSE
                  Kunde.Kjon = 0.
           ASSIGN
               Kunde.FaktAdresse1    = CustomerNew.street3
               Kunde.FaktAdresse2    = " "           
               Kunde.FaktPostNr      = CustomerNew.postcode3
               Kunde.FodtDato        = DATE(CustomerNew.dob)
               Kunde.Alder           = YEAR(Kunde.FodtDato) - YEAR(TODAY) + 1 NO-ERROR.
       END.                                             
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ErrorLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ErrorLogg Procedure 
PROCEDURE ErrorLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF CAN-FIND(FIRST tt_error) THEN 
    DO:
    OUTPUT TO VALUE("Error.Txt").
      PUT UNFORMATTED
        "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
        "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn SKIP
        .
      FOR EACH tt_Error:
        PUT UNFORMATTED tt_Error.Tekst SKIP.
      END.
    OUTPUT CLOSE.
    IF SEARCH("Error.Txt") <> ? THEN
    DO:
      DEF VAR hInstance AS INT.

      RUN ShellExecute{&A} IN hpApi(0,
                                    "open",
                                    "notepad.exe",
                                    SEARCH("Error.Txt"),
                                    "",
                                    1,
                                    OUTPUT hInstance).

    END.
  
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       

------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr AS INT  NO-UNDO.
  DEF VAR piAntFeil AS INT  NO-UNDO.
  DEF VAR pcBkuFil  AS CHAR NO-UNDO.
  DEF VAR piLoop AS INT NO-UNDO.
  DEFINE VARIABLE cFilNavn_xsd AS CHARACTER   NO-UNDO.
  
  ASSIGN
      piLinjeNr  = 1.
      iAntLinjer = 0
      .
      
  /* Leser inn hele xml filen DOM. */
/*  RUN xmlReadBITSCustomer.p (INPUT cFilNavn).*/
ASSIGN cFilNavn_xsd = ENTRY(1,cFilNavn,"_") + ".xsd".

/* Skapa *.xsd om förändringa har skett i TEMP-TABLE.*/
/*TEMP-TABLE customerRow:WRITE-XMLSCHEMA('file', cFilNavn_xsd, TRUE,  ?, TRUE).
  lOk = TEMP-TABLE customerRow:READ-XML('file',cFilNavn,"empty",?,?,?,?) .*/

DEFINE VARIABLE hDset AS HANDLE NO-UNDO.
CREATE DATASET hDset .
hDset:NAME = "PublishEComCustomer". 

hDset:ADD-BUFFER(BUFFER headerInf:HANDLE).
hDset:ADD-BUFFER(BUFFER Customer:HANDLE).
hDset:ADD-BUFFER(BUFFER shippingaddress:HANDLE).      
hDset:ADD-BUFFER(BUFFER billingaddress:HANDLE).      


DEFINE VARIABLE hRelation AS HANDLE EXTENT 2 NO-UNDO.

hRelation[1] = hDset:ADD-RELATION(BUFFER Customer:HANDLE,BUFFER  shippingaddress:HANDLE,"customerID,customerID",
                              TRUE /* REPOSITION */,
                              TRUE /*nested*/ ,
                              TRUE /* not-active*/ ,
                              FALSE /* recursive */ ,
                              TRUE /* foreign key hidden */ ).
hRelation[2] = hDset:ADD-RELATION(BUFFER Customer:HANDLE,BUFFER  billingaddress:HANDLE,"customerID,customerID",
                              TRUE /* REPOSITION */,
                              TRUE /*nested*/ ,
                              TRUE /* not-active*/ ,
                              FALSE /* recursive */ ,
                              TRUE /* foreign key hidden */ ).


hDset:WRITE-XMLSCHEMA("file",cFilNavn_xsd,true,?,false,true).
lOk = hDset:READ-XML ('file',cFilNavn,?,?,?,?,?) .


  IF NOT lOk THEN
/*    ASSIGN cReturnStatus = RETURN-VALUE.*/
    ASSIGN cReturnStatus = "ERROR".
      
  IF cReturnStatus = 'ERROR' THEN
    DO:
      CREATE tt_Error.
      ASSIGN
        piAntFeil = piAntFeil + 1
        tt_Error.LinjeNr = piAntFeil
        tt_Error.Tekst   = "** Feil ved innlesning av fil " + cFilNavn + ".".
     RETURN.
    END.

    FOR EACH customer:
        CREATE customernew.
        ASSIGN customerNew.customerID    = customer.customerID
               customerNew.extcustomerID = customer.extcustomerID
               customerNew.operation     = customer.operation
               customerNew.ctitle        = customer.ContactTitle
               customerNew.FirstName1    = customer.ContactFirstName
               customerNew.lastName1     = customer.contactLastName
               customerNew.dob           = customer.dob
               customerNew.email         = customer.email.
        FIND FIRST shippingaddress WHERE shippingaddress.customerID = customer.customerID.
          IF AVAILABLE shippingaddress THEN
          DO:
            ASSIGN customerNew.firstname2           = shippingaddress.firstname
                   customerNew.lastname2            = shippingaddress.lastname
                   customerNew.city2                = shippingaddress.city
                   customerNew.postcode2            = shippingaddress.postcode
                   customerNew.street2              = shippingaddress.street
                   customerNew.telephone2           = shippingaddress.telephone
                   customerNew.is_default_shipping2 = STRING(shippingaddress.is_default_shipping)
                   customerNew.is_default_billing2  = STRING(shippingaddress.is_default_billing)
                   customerNew.customer_address_id2 = shippingaddress.customer_address_id.
          END.
        FIND FIRST billingaddress WHERE billingaddress.customerID = customer.customerID.
          IF AVAILABLE billingaddress THEN
          DO:
            ASSIGN customerNew.firstname3           = billingaddress.firstname 
                   customerNew.lastname3            = billingaddress.lastname
               customerNew.city3                = billingaddress.city
               customerNew.postcode3            = billingaddress.postcode
               customerNew.street3              = billingaddress.street
               customerNew.telephone3           = billingaddress.telephone
               customerNew.is_default_shipping3 = STRING(billingaddress.is_default_shipping)
               customerNew.is_default_billing3  = STRING(billingaddress.is_default_billing)
               customerNew.customer_address_id3 = billingaddress.customer_address_id.
        END.
    END.


  FOR EACH CustomerNew:
     RUN CustomerUpdate.
  END.

OS-COPY VALUE(cFilNavn) VALUE("c:\home\lindbak\ankommet\bku").
OS-DELETE VALUE(cFilNavn).


  /* Stempler posten som innlest. */
  DO TRANSACTION:
      FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
      ASSIGN
          VPIFilHode.VPIFilStatus = 5
          .
  END.
  IF AVAILABLE VPIFilHode THEN
      FIND CURRENT VPIFilHode    NO-LOCK.

  IF CAN-FIND(FIRST tt_Error) THEN
    RUN ErrorLogg.

  ASSIGN
  pcBkuFil = VPIFilHode.Katalog + "~\bku" + "\" + 
             VPIFilHode.FilNavn
  .

  /* Sikrer at backup katalog finnes. */
  OS-CREATE-DIR value(VPIFilHode.Katalog + "~\bku").
  /* Flytter filen til backup katalog. */
  OS-COPY value(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) 
          value(pcBkuFil).
  /* Renser bort fil */
  IF SEARCH(pcBkuFil) <> ? THEN
  DO:
      /* Filen tas bort fra katalogen. */
      IF SEARCH(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) <> ? THEN
          OS-DELETE VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrepTempTabell) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrepTempTabell Procedure 
PROCEDURE PrepTempTabell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
        /*------------------------------------------------------------------------------
                        Purpose: Tømmer temp-tabell                                                                                                                                       
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
    /* Det kommer ALLTID bare en kunde pr. fil.         */
    /* Renser opp i temptabell før innlesning begynner. */
/*    FOR EACH TT_Customer:           DELETE TT_Customer.           END.
    FOR EACH TT_BillingAddress:     DELETE TT_BillingAddress.     END.
    FOR EACH TT_ShippingAdress:     DELETE TT_ShippingAdress.     END.
    FOR EACH TT_User:               DELETE TT_User.               END.
    FOR EACH TT_UserBillingAddress: DELETE TT_UserBillingAddress. END.
    FOR EACH TT_UserShippingAdress: DELETE TT_UserShippingAdress. END.*/
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

