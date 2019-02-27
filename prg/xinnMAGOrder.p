&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xinnMAGOrder.p
    Purpose     :

    Syntax      :

    Description : Leser inn ordre fra BITS nettbutikk ePages.

    Author(s)   : Tom Nøkleby
    Created     : 19/5-09
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEFINE VARIABLE iCL AS INTEGER NO-UNDO.
DEFINE VARIABLE iNettbutikk AS INTEGER NO-UNDO.
DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.
DEF VAR cUtFilNavn    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBkuFil AS CHARACTER NO-UNDO.
DEF VAR ctmpKatalog   AS CHAR NO-UNDO.
DEF VAR pcLinje       AS CHAR NO-UNDO.
DEFINE VARIABLE cReturnStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFraktVareNr  AS CHARACTER NO-UNDO.
DEF STREAM InnFil.
DEF STREAM UtVPI.
DEF STREAM UtVre.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR.

/* Nytt */
DEFINE TEMP-TABLE orderHeader NO-UNDO SERIALIZE-NAME "header"
    FIELD id AS CHAR INIT ?
    FIELD orderID AS CHAR 
    FIELD orderDate AS DATETIME SERIALIZE-NAME "date"
    FIELD customerId AS CHAR 
    FIELD amount AS DEC
    FIELD discountamount AS DEC
    FIELD vatamount AS DECIMAL
    FIELD shippingAmount AS DECIMAL 
    FIELD shippingVat AS DECIMAL 
    FIELD comment AS CHARACTER  
    INDEX Idx1 AS PRIMARY UNIQUE orderID.

DEFINE TEMP-TABLE orderPaymentInfo NO-UNDO SERIALIZE-NAME "paymentInfo"
    FIELD orderID AS CHAR  INIT ?
    FIELD paymentMethod AS CHAR 
    FIELD amount AS DEC 
    FIELD currency AS CHAR
    INDEX Idx1 AS PRIMARY  orderID.

DEFINE TEMP-TABLE OrdershippingInfo NO-UNDO SERIALIZE-NAME "shippingInfo"
    FIELD orderID AS CHAR  INIT ? 
    FIELD region AS CHAR 
    FIELD street AS CHAR 
    FIELD country AS CHAR 
    FIELD lastname AS CHAR 
    FIELD firstname AS CHAR 
    FIELD postcode AS CHAR 
    FIELD city AS CHAR 
    FIELD telephone AS CHAR 
    FIELD shippingDescription AS CHARACTER 
    FIELD shippingMethod AS CHARACTER 
    INDEX Idx1 AS PRIMARY  orderID.

DEFINE TEMP-TABLE orderItemRows NO-UNDO SERIALIZE-NAME "orderItemRows"
    FIELD orderID AS CHAR INIT ?
    INDEX Idx1 AS PRIMARY UNIQUE orderID.

DEFINE TEMP-TABLE orderItem NO-UNDO SERIALIZE-NAME "orderItem"
    FIELD orderID AS CHAR INIT ?
    FIELD rowID AS INT 
    FIELD itemID AS CHAR 
    FIELD sku AS CHAR 
    FIELD itemtext AS CHAR 
    FIELD base_price AS DEC 
    FIELD qty AS DEC 
    FIELD vatpercentage AS CHAR 
    FIELD sum AS DEC 
    FIELD currency AS CHAR 
    FIELD configurable AS CHARACTER  
    INDEX Idx1 AS PRIMARY orderID.


DEFINE VARIABLE hDset AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE OrdHuvud NO-UNDO
    FIELD orderID AS CHAR 
    FIELD orderDate AS DATE
    FIELD orderTid AS INTEGER  
    FIELD customerId AS CHAR 
    FIELD amount1 AS DEC
    FIELD disccountamount AS DEC
    FIELD vatamount AS DEC
    FIELD paymentMethod AS CHAR 
    FIELD amount2 AS DEC
    FIELD currency AS CHAR
    FIELD region AS CHAR 
    FIELD street AS CHAR 
    FIELD country AS CHAR 
    FIELD lastname AS CHAR 
    FIELD firstname AS CHAR 
    FIELD postcode AS CHAR 
    FIELD city AS CHAR 
    FIELD telephone AS CHARACTER
    FIELD shippingAmount AS DECIMAL 
    FIELD shippingVat AS DECIMAL 
    FIELD shippingDescription AS CHARACTER 
    FIELD shippingMethod AS CHARACTER 
    FIELD comment AS CHARACTER  
    INDEX OrderId OrderId.
    
DEFINE VARIABLE cWrkDate AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDate AS DATE        NO-UNDO.
DEFINE VARIABLE hh AS INTEGER     NO-UNDO.
DEFINE VARIABLE mm AS INTEGER     NO-UNDO.
DEFINE VARIABLE ss AS INTEGER     NO-UNDO.
DEFINE VARIABLE cTime AS INTEGER     NO-UNDO.
DEFINE VARIABLE cFraktMetode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKOrdre_Id_Lst AS CHARACTER NO-UNDO.

DEFINE BUFFER bNettbutikk FOR Butiker.
DEFINE BUFFER bSentrallager FOR Butiker.

DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.

/* Nytt till hit*/

{windows.i}

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


{syspara.i 5 1 1 iCL int}
{syspara.i 150 1 2 iNettbutikk int}

FIND FIRST SysPara NO-LOCK WHERE
    SysPara.SysHId       = 150 AND  
    SysPara.SysGr        = 10 AND  
    SysPara.Beskrivelse  = "Posten" NO-ERROR.
IF AVAILABLE SysPara THEN
  cFraktVareNr = Syspara.Parameter1.
  
FOR EACH tt_Error:
  DELETE tt_Error.
END.
FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.

IF NOT CAN-FIND(Butiker WHERE
                Butiker.Butik = iNettbutikk) THEN
  RETURN '** Feil - Nettbutikknr. ikke satt, eller satt til ukjent butikk.'.

ASSIGN
    cUtFilNavn = 'Magento_Ordreimport_mangler_kunde'  + REPLACE(STRING(TODAY),'/','')
    cFilNavn   = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn
    cBkuFil    = VPIFilHode.Katalog + "~\bku~\" + VPIFilHode.FilNavn.

RUN LesInnFil.

IF cKOrdre_Id_Lst <> '' THEN 
  DO:
    /* Setter plukkbutikk på kundeordrelinjene. */
    RUN sett_plukkbutikk_kundeordre.p (cKOrdre_Id_Lst).
    /* Opprettelse av overføringsordre og sending av email. En ordre pr. fra/til butikkrelasjon. */
    RUN opprett_Overforingsordre.p (cKOrdre_Id_Lst,FALSE).
  END.

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ErrorLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ErrorLogg Procedure 
PROCEDURE ErrorLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF CAN-FIND(FIRST tt_Error) THEN 
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
  
  ASSIGN
      piLinjeNr  = 1
      iAntLinjer = 0.
      
  /* Leser inn hele xml filen DOM. */
/*  RUN xmlReadBITSOrder.p (INPUT cFilNavn).*/


  RUN xmlReadMagOrder.

  ASSIGN cReturnStatus = RETURN-VALUE.
      
  IF cReturnStatus = 'ERROR' THEN
    DO:
      CREATE tt_Error.
      ASSIGN
        piAntFeil = piAntFeil + 1
        tt_Error.LinjeNr = piAntFeil
        tt_Error.Tekst   = "** Feil ved innlesning av fil " + cFilNavn + ".".
    END.

  /* Stempler posten som innlest. */
  DO TRANSACTION:
      FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
      ASSIGN
          VPIFilHode.VPIFilStatus = 5.
  END.
  IF AVAILABLE VPIFilHode THEN
      FIND CURRENT VPIFilHode    NO-LOCK.

  IF CAN-FIND(FIRST tt_Error) THEN
    RUN ErrorLogg.

  ASSIGN
  pcBkuFil = VPIFilHode.Katalog + "~\bku" + "\" + 
             VPIFilHode.FilNavn.

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

&IF DEFINED(EXCLUDE-ProcessOrderDataSet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessOrderDataSet Procedure 
PROCEDURE ProcessOrderDataSet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcXml AS LONGCHAR NO-UNDO. 
    
    hDset:READ-XML('LONGCHAR',ipcXML,?,?,?,?,?).

    /* Populate orderid to other tables */ 
    FIND FIRST orderHeader WHERE id = ?  NO-LOCK.
    orderHeader.id = "".
    FOR EACH OrderItemRows WHERE OrderItemRows.OrderId = ?: 
        OrderItemRows.Orderid = orderHeader.OrderId.
    END.
    FOR EACH OrderItem WHERE orderItem.OrderId = ?: 
        orderItem.Orderid = orderHeader.OrderId.
    END.
    FOR EACH OrdershippingInfo WHERE OrdershippingInfo.OrderId = ?: 
        OrdershippingInfo.Orderid = orderHeader.OrderId.
    END.
    FOR EACH orderPaymentInfo WHERE orderPaymentInfo.OrderId = ?: 
        orderPaymentInfo.Orderid = orderHeader.OrderId.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-xmlReadMagOrder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE xmlReadMagOrder Procedure 
PROCEDURE xmlReadMagOrder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

CREATE DATASET hDset.
hDset:NAME = "ProcessEComSalesOrder". 

hDset:ADD-BUFFER(BUFFER orderHeader:HANDLE).
hDset:ADD-BUFFER(BUFFER orderPaymentInfo:HANDLE).
hDset:ADD-BUFFER(BUFFER OrdershippingInfo:HANDLE).      
hDset:ADD-BUFFER(BUFFER orderItemRows:HANDLE).      
hDset:ADD-BUFFER(BUFFER orderItem:HANDLE).      


/*RUN ProcessOrderDataSet.*/

/*PROCEDURE ProcessOrderDataSet: 
END. */

DEFINE VARIABLE icnt       AS INTEGER  NO-UNDO. 
DEFINE VARIABLE hDocMine   AS HANDLE   NO-UNDO.
DEFINE VARIABLE hRootMine  AS HANDLE   NO-UNDO.
DEFINE VARIABLE hChNode    AS HANDLE   NO-UNDO.
DEFINE VARIABLE hDocNew    AS HANDLE   NO-UNDO. 
DEFINE VARIABLE hNode      AS HANDLE   NO-UNDO.
DEFINE VARIABLE cXML       AS LONGCHAR NO-UNDO. 
/*DEFINE VARIABLE cFilNavn AS CHARACTER   NO-UNDO.*/
DEFINE VARIABLE iaar AS INTEGER     NO-UNDO.
DEFINE VARIABLE iman AS INTEGER     NO-UNDO.
DEFINE VARIABLE idag AS INTEGER     NO-UNDO.

CREATE X-DOCUMENT hDocMine.
CREATE X-NODEREF hRootMine.
CREATE X-NODEREF hChNode.

DEFINE VARIABLE cKode AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDec  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iLevFNr AS INTEGER NO-UNDO.

/*ASSIGN cFilNavn = "c:\home\lindbak\ankommet\ProcessEComSalesOrder.xml".*/

/*hDocMine:LOAD("FILE","ProcessEComSalesOrder.xml",FALSE).*/
/*MESSAGE "cFil4 " cFilNavn
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
hDocMine:LOAD("FILE",cFilNavn,FALSE).
hDocMine:GET-DOCUMENT-ELEMENT(hRootMine).

DO icnt = 1 TO hRootMine:NUM-CHILDREN:
  hRootMine:GET-CHILD(hChNode, icnt).

  IF hChNode:LOCAL-NAME = "Order" THEN 
  DO:
      CREATE X-DOCUMENT hDocNew.
      CREATE X-NODEREF  hNode.
      hDocNew:GET-DOCUMENT-ELEMENT(hNode).
      hDocNew:IMPORT-NODE(hNode,hChNode,TRUE).
      hDocnew:APPEND-CHILD(hNode).
      hDocNew:SAVE("LONGCHAR",cXML).
      
      RUN ProcessOrderDataSet(cXML). 
      
      DELETE OBJECT hDocNew. 
      DELETE OBJECT hNode. 
  END. 
END. 
DELETE OBJECT hDocMine.
DELETE OBJECT hRootMine.
DELETE OBJECT hChNode. 

FOR EACH orderitemrows: 
   CREATE OrdHuvud.
   ASSIGN OrdHuvud.orderID = orderitemrows.orderId.
   FOR EACH orderHeader WHERE orderHeader.orderID = orderitemrows.orderId:
      ASSIGN
       OrdHuvud.orderDate       = DATE(orderHeader.orderDate)
       OrdHuvud.orderTid        = MTIME(orderHeader.orderDate)
       OrdHuvud.customerId      = orderHeader.customerId 
       OrdHuvud.amount1         = orderHeader.amount
       OrdHuvud.disccountamount = orderHeader.discountamount
       OrdHuvud.vatamount       = orderHeader.vatamount
       OrdHuvud.shippingAmount  = orderHeader.shippingAmount 
       OrdHuvud.shippingVat     = orderHeader.shippingVat 
       OrdHuvud.Comment         = orderHeader.comment
       NO-ERROR.
   END.
   FOR EACH orderPaymentInfo WHERE orderPaymentInfo.orderID = orderitemrows.orderId:
      ASSIGN
       OrdHuvud.paymentMethod   = orderPaymentInfo.paymentMethod
       OrdHuvud.amount2         = orderPaymentInfo.amount
       OrdHuvud.currency        = orderPaymentInfo.currency
       NO-ERROR.
   END.
   FOR EACH OrdershippingInfo WHERE OrdershippingInfo.orderID = orderitemrows.orderId:
      ASSIGN
       OrdHuvud.region              = OrdershippingInfo.region
       OrdHuvud.street              = OrdershippingInfo.street
       OrdHuvud.country             = OrdershippingInfo.country
       OrdHuvud.lastname            = OrdershippingInfo.lastname
       OrdHuvud.firstname           = OrdershippingInfo.firstname
       OrdHuvud.postcode            = OrdershippingInfo.postcode
       OrdHuvud.city                = OrdershippingInfo.city
       OrdHuvud.telephone           = OrdershippingInfo.telephone
       OrdHuvud.shippingDescription = OrdershippingInfo.shippingDescription 
       OrdHuvud.shippingMethod      = OrdershippingInfo.shippingMethod
       
       NO-ERROR.
   END.
   
END.

  FIND LAST Kasse NO-LOCK WHERE
            Kasse.ButikkNr = iNettbutikk NO-ERROR.
  FIND FIRST Forsalj NO-LOCK WHERE
           Forsalj.Brukerid2 = " " NO-ERROR.
  FIND Kunde NO-LOCK WHERE
       Kunde.EksterntKundeNr = OrdHuvud.customerId NO-ERROR.
  IF NOT AVAILABLE Kunde THEN
  OPPRETT_KUNDE: 
  DO:
      FIND FIRST KundeType NO-LOCK NO-ERROR.
      FIND FIRST KundeGruppe NO-LOCK NO-ERROR.
      CREATE Kunde.
      ASSIGN 
          Kunde.EksterntKundeNr = STRING(OrdHuvud.customerId)
          Kunde.Kilde           = "ePages"
          Kunde.WebKunde        = TRUE 
          Kunde.Aktiv           = TRUE
          Kunde.Navn            = 'Ukjent - Mag.OrdreNr ' + STRING(OrdHuvud.orderID)
          Kunde.TypeId          = IF AVAILABLE KundeType THEN KundeType.TypeId ELSE 0
          Kunde.GruppeId        = IF AVAILABLE KundeGruppe THEN KundeGruppe.GruppeId ELSE 0
          Kunde.RegistrertDato  = TODAY
          Kunde.RegistrertTid   = TIME
      .

      IF AVAILABLE kunde THEN 
        FIND CURRENT Kunde NO-LOCK.

      /* Logger at kunde ikke finnes. */
      IF NOT AVAILABLE Kunde THEN 
      DO:
          RUN bibl_loggDbFri.p (cUtfilNavn, 
              'Ordre ' + STRING(OrdHuvud.orderID) + ' ' + 
              ' har ukjent kunde ' + STRING(OrdHuvud.customerId) + '.'  
              ). 
      END.
  END. /* OPPRETT_KUNDE */

  FOR EACH OrdHuvud:
    
      /* <date>2012-06-15 15:13:00</date> */
      /*   
      ASSIGN idag = INT(SUBSTRING(OrdHuvud.orderdate,9,2)).
      ASSIGN iman = INT(SUBSTRING(OrdHuvud.orderdate,6,2)).
      ASSIGN iaar = INT(SUBSTRING(OrdHuvud.orderdate,1,4)).

      ASSIGN cDate = DATE(iman,idag,iaar).
      ASSIGN hh = INT(SUBSTRING(OrdHuvud.orderdate,12,2)).
      ASSIGN mm = INT(SUBSTRING(OrdHuvud.orderdate,15,2)).
      ASSIGN ss = INT(SUBSTRING(OrdHuvud.orderdate,18,2)).

      ASSIGN cTime = (hh * 3600) + (mm * 60) + ss.
      */
      
    FIND FIRST Leveringsform EXCLUSIVE-LOCK WHERE
      Leveringsform.LevFormMetode = OrdHuvud.shippingMethod NO-ERROR.
    IF NOT AVAILABLE Leveringsform THEN 
    DO:
      FIND LAST Leveringsform NO-LOCK.
      IF AVAILABLE Leveringsform THEN 
        iLevFnr = Leveringsform.LevFnr + 1.
      ELSE 
        iLevFNr = 1.
      CREATE Leveringsform.
      ASSIGN
      Leveringsform.LevFNr             = iLevFNr
      Leveringsform.LevFormMetode      = OrdHuvud.shippingMethod
      Leveringsform.LevFormBeskrivelse = OrdHuvud.shippingDescription
      .
      FIND CURRENT Leveringsform NO-LOCK.
    END. 
    ELSE iLevFNr = LeveringsForm.LevFNr.

    /* Bare nye ordre skal innleses */
    IF NOT CAN-FIND(FIRST KOrdreHode WHERE 
                          KOrdreHode.EkstOrdreNr = OrdHuvud.orderID) THEN 
    OPPRETT_KORDRE:
    DO:
        CREATE KOrdreHode.
        ASSIGN
            KOrdreHode.VerkstedMerknad   = 'Innlest ' + STRING(TODAY) + ' ' + STRING(TIME,'HH:MM:SS') 
            KOrdreHode.Opphav            = 10 /* Nettbutikk */
            KOrdreHode.EkstOrdreNr       = OrdHuvud.orderID
            KOrdreHode.internMerknad     = " " 
            KOrdreHode.KundeMerknad      = " " 
            KOrdreHode.RegistrertDato    = OrdHuvud.orderdate
            KOrdreHode.RegistrertTid     = OrdHuvud.orderTid
            KOrdreHode.RegistrertAv      = " "  /* /Shops/DemoShop/Users/magbyr*/
            KOrdreHode.LevFNr            = iLevFNr 
            KOrdreHode.KundeMerknad      = OrdHuvud.Comment
            NO-ERROR.
        ASSIGN                   
            KOrdreHode.KundeNr           = (IF AVAILABLE Kunde THEN Kunde.KundeNr ELSE DEC(OrdHuvud.customerId))
            KOrdreHode.ButikkNr          = iNettbutikk
            KOrdreHode.KasseNr           = (IF AVAILABLE Kasse THEN Kasse.KasseNr ELSE 0)
            KOrdreHode.ForsNr            = (IF AVAILABLE Forsalj THEN Forsalj.ForsNr ELSE 0)
            KOrdreHode.SelgerNr          = 0
            NO-ERROR.
        ASSIGN     
            KOrdreHode.Navn              = OrdHuvud.firstname + " " + OrdHuvud.lastname
            KOrdreHode.KontNavn          = (IF AVAILABLE Kunde THEN Kunde.KontNavn ELSE " ")
            NO-ERROR.
        ASSIGN 
            KOrdreHode.Adresse1          = OrdHuvud.street
            KOrdreHode.Adresse2          = " "
            KOrdreHode.PostNr            = OrdHuvud.postcode
            KOrdreHode.Poststed          = OrdHuvud.city
            KOrdreHode.ePostAdresse      = (IF AVAILABLE Kunde THEN Kunde.ePostAdresse ELSE KOrdreHode.ePostAdresse)               
            KOrdreHode.Telefon           = OrdHuvud.telephone
            KOrdreHode.MobilTlf          = OrdHuvud.telephone
            KOrdreHode.Telefaks          = " "
            NO-ERROR.
        ASSIGN      
            KOrdreHode.DeresRef          = (IF AVAILABLE Kunde THEN Kunde.DeresRef ELSE " ")
            KOrdreHode.VaarRef           = " "
            NO-ERROR.
        ASSIGN
            KOrdreHode.Leveringsdato     = KOrdreHode.RegistrertDato + 7
            KOrdreHode.BetaltDato        = KOrdreHode.RegistrertDato
            KOrdreHode.TotalRabatt%      = 0.0
            KOrdreHode.BetBet            = Kunde.BetBet
            NO-ERROR.
        ASSIGN 
            KOrdreHode.LevStatus         = "30" /* Bekreftet */
            KOrdreHode.DeresRef          = OrdHuvud.firstname + " " + 
                                           OrdHuvud.lastName
            KOrdreHode.FaktAdresse1      = OrdHuvud.street
            KOrdreHode.FaktAdresse2      = " "
            KOrdreHode.FaktPostNr        = OrdHuvud.postcode
            KOrdreHode.FaktPoststed      = OrdHuvud.city
            KOrdreHode.Telefon           = OrdHuvud.telephone
            KOrdreHode.Telefaks          = " "
            KOrdreHode.FirmaNavn         = " "
            KOrdreHode.FirmaAdresse1     = OrdHuvud.firstname
            KOrdreHode.FirmaAdresse2     = OrdHuvud.lastname
            KOrdreHode.FaktLand          = OrdHuvud.country
            KOrdreHode.LevAdresse1       = OrdHuvud.street
            KOrdreHode.LevAdresse2       = " "
            KOrdreHode.LevPostNr         = OrdHuvud.postcode
            KOrdreHode.LevPoststed       = OrdHuvud.city
            KOrdreHode.LevLand           = OrdHuvud.country
            KOrdreHode.ValKod            = OrdHuvud.currency
            NO-ERROR.
        ASSIGN
          cKOrdre_Id_Lst = cKOrdre_Id_Lst + (IF cKOrdre_Id_Lst = '' THEN '' ELSE ',') + STRING(KOrdreHode.KORdre_Id).
        
        ORDREHODE:
        FOR EACH orderItem WHERE 
          OrdHuvud.orderID = orderItem.orderID NO-LOCK:
            
          IF NOT orderItem.configurable MATCHES '*' + 'TRUE' + '*' THEN 
            NEXT ORDREHODE.
          
          ASSIGN lDec = DECIMAL(orderItem.sku) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN NEXT.
          
          cKode = TRIM(STRING(orderItem.sku)).
          RUN bibl_chkean.p (INPUT-OUTPUT cKode).
          FIND Strekkode NO-LOCK WHERE 
            Strekkode.Kode = cKode NO-ERROR.
          IF AVAILABLE Strekkode THEN
          DO: 
            FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
            FIND StrKonv NO-LOCK WHERE
                 StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
          END.  
          
          IF AVAILABLE ArtBas THEN 
            FIND ArtPris OF ArtBas NO-LOCK WHERE
               ArtPris.ProfilNr = bNettButikk.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris AND AVAILABLE ArtBas THEN 
            FIND ArtPris OF ArtBas NO-LOCK WHERE
               ArtPris.ProfilNr = bSentrallager.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
            FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
           
          FIND FIRST Moms NO-LOCK WHERE
             Moms.MomsProc = ROUND(DECIMAL(orderItem.vatpercentage) / 10000,2) NO-ERROR.
          IF AVAILABLE ArtBas THEN 
            FIND Lager NO-LOCK WHERE
               Lager.ArtikkelNr = DECIMAL(orderItem.sku) AND 
               Lager.Butik      = bNettButikk.Butik NO-ERROR.
           
          FIND LAST KOrdrelinje NO-LOCK WHERE
             KOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id NO-ERROR.
          IF AVAILABLE KOrdreLinje THEN 
             iLinjeNr = KOrdreLinje.KOrdreLinjeNr + 1.
          ELSE
             iLinjeNr = 1.
               
          CREATE KOrdreLinje.
          ASSIGN iAntLinjer = iAntLinjer + 1.
          ASSIGN
               KOrdreLinje.KOrdre_ID         = KOrdreHode.KOrdre_Id
               KOrdreLinje.KOrdreLinjeNr     = iLinjeNr 
               KOrdreLinje.VareNr            = IF AVAILABLE ArtBas THEN STRING(ArtBas.ArtikkelNr) ELSE '' /*TRIM(orderItem.sku)*/
               KOrdreLinje.Varetekst         = (IF AVAILABLE ArtBAs THEN ArtBas.Beskr ELSE IF orderItem.itemtext <> "" THEN orderItem.itemtext ELSE '** Ukjent ' + KOrdreLinje.VareNr)
               KOrdreLinje.Mva%              = ROUND(DECIMAL(orderItem.vatpercentage) / 10000,2)
               KOrdreLinje.Antall            = orderItem.qty
               KOrdreLinje.NettoPris         = ROUND(orderItem.base_price,2) + ROUND(ROUND(orderItem.base_price,2) * KOrdreLinje.Mva% / 100,2)
               KOrdreLinje.MvaKr             = ROUND(ROUND(orderItem.base_price,2) * KOrdreLinje.Mva% / 100,2)
               KOrdreLinje.NettoLinjesum     = (KOrdreLinje.NettoPris * KOrdreLinje.Antall)      
               KOrdreLinje.LinjeRabattKr     = 0
               KOrdreLinje.LinjeRab%         = 0  
               KOrdreLinje.LinjeRab%         = 0              
               KOrdreLinje.MomsKod           = (IF AVAILABLE Moms THEN Moms.MomsKod ELSE 0)
               KOrdreLinje.Storl             = (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')
               KOrdreLinje.StrKode           = (IF AVAILABLE StrKonv THEN StrKonv.StrKode ELSE 0)
               KOrdreLinje.Storl             = (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')
               KOrdreLinje.VareKost          = (IF AVAILABLE Lager THEN Lager.VVareKost ELSE 0)
               
               KOrdreLinje.VareKost          = IF (KOrdreLinje.VareKost = ? OR KOrdreLinje.VareKost <= 0) THEN 0 ELSE KOrdreLinje.VareKost
               KOrdreLinje.VareKost          = IF (KOrdreLinje.VareKost = 0 AND AVAILABLE ArtPris) THEN  ArtPris.VareKost[1] ELSE KOrdreLinje.VareKost
                
               KOrdreLinje.BruttoPris        = KOrdreLinje.NettoPris  
               KOrdreLinje.Pris              = KOrdreLinje.NettoPris  
               KOrdreLinje.Linjesum          = KOrdreLinje.NettoLinjesum 
               
               KOrdreLinje.DbKr              = KOrdreLinje.NettoLinjesum - (KOrdreLinje.VareKost * KOrdreLinje.Antall)
               KOrdreLinje.Db%               = (KOrdreLinje.DbKr / KOrdreLinje.NettoLinjesum) * 100
               KOrdreLinje.Db%               = (IF KOrdreLinje.Db% = ? THEN 0 ELSE KOrdreLinje.Db%) 
               KOrdreLinje.RefNr             = 0
               KOrdreLinje.RefTekst          = ''
               KOrdreLinje.Bestillingsnummer = IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE ''
               KOrdreLinje.LevFargKod        = IF AVAILABLE ArtBas THEN ArtBas.LevFargKod ELSE ''
               KOrdreLinje.ValKod            = KOrdreHode.ValKod           
               NO-ERROR.
        END. /* ORDREHODE */
        FIND LAST KOrdrelinje NO-LOCK WHERE
          KOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id NO-ERROR.
        IF AVAILABLE KOrdreLinje THEN 
          iLinjeNr = KOrdreLinje.KOrdreLinjeNr + 1.
        ELSE
          iLinjeNr = 1.
    
        CREATE KOrdreLinje.
        ASSIGN iAntLinjer = iAntLinjer + 1.
        ASSIGN
          KOrdreLinje.KOrdre_ID     = KOrdreHode.KOrdre_Id
          KOrdreLinje.KOrdreLinjeNr = iLinjeNr 
            /* Betaling */
/*          KOrdreLinje.MomsKod       = IF AVAILABLE Moms THEN Moms.MomsKod ELSE 0*/
          KOrdreLinje.Antall        = 1
          KOrdreLinje.NettoPris     = OrdHuvud.amount2 * -1
/*          KOrdreLinje.MvaKr         = OrdHuvud.vatamount*/
          KOrdreLinje.NettoLinjesum = KOrdreLinje.NettoPris
          KOrdreLinje.BruttoPris    = KOrdreLinje.NettoPris  
          KOrdreLinje.Pris          = KOrdreLinje.NettoPris 
          KOrdreLinje.Linjesum      = KOrdreLinje.NettoPris
          KORdreLinje.Varetekst     = "Forhåndsbetalt"
          KORdreLinje.VareNr        = "BETALT"
          NO-ERROR.          
        IF NOT ERROR-STATUS:ERROR THEN 
        DO:
          CASE OrdHuvud.paymentMethod:
            WHEN 'dibs'             THEN KORdreLinje.Varetekst = 'Betalt med kreditkort'.
            WHEN 'kreditor_invoice' THEN KORdreLinje.Varetekst = 'FAKTURA tilsendes'.
          END CASE. 
        END.
        IF OrdHuvud.disccountamount <> 0 THEN 
        RABATT:
        DO:
            CREATE KOrdreLinje.
            ASSIGN 
                iAntLinjer = iAntLinjer + 1
                iLinjeNr   = iLinjeNr + 1.
            ASSIGN
                KOrdreLinje.KOrdre_ID     = KOrdreHode.KOrdre_Id
                KOrdreLinje.KOrdreLinjeNr = iLinjeNr 
                /* Rabatt fra ordrehode */
                KOrdreLinje.Antall        = 1
                KOrdreLinje.NettoPris     = OrdHuvud.disccountamount
                KOrdreLinje.NettoLinjesum = KOrdreLinje.NettoPris
                KOrdreLinje.BruttoPris    = KOrdreLinje.NettoPris  
                KOrdreLinje.Pris          = KOrdreLinje.NettoPris 
                KOrdreLinje.Linjesum      = KOrdreLinje.NettoPris
                KORdreLinje.Varetekst     = "KUPONG"
                KORdreLinje.VareNr        = "KUPONG"
            NO-ERROR.          
        END. /* RABATT */
        IF OrdHuvud.shippingAmount > 0 THEN 
        FRAKT:
        DO:
          FIND LAST KOrdrelinje NO-LOCK WHERE
            KOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id NO-ERROR.
          IF AVAILABLE KOrdreLinje THEN 
            iLinjeNr = KOrdreLinje.KOrdreLinjeNr + 1.
          ELSE
            iLinjeNr = 1.
        
          CREATE KOrdreLinje.
          ASSIGN iAntLinjer = iAntLinjer + 1.
          ASSIGN
            KOrdreLinje.KOrdre_ID     = KOrdreHode.KOrdre_Id
            KOrdreLinje.KOrdreLinjeNr = iLinjeNr 
              /* Betaling */
            KOrdreLinje.MomsKod       = IF AVAILABLE Moms THEN Moms.MomsKod ELSE 0
            KOrdreLinje.Antall        = 1
            KOrdreLinje.NettoPris     = OrdHuvud.shippingAmount + OrdHuvud.shippingVat            
            KOrdreLinje.MvaKr         = OrdHuvud.shippingVat
            KOrdreLinje.NettoLinjesum = KOrdreLinje.NettoPris
            KOrdreLinje.BruttoPris    = KOrdreLinje.NettoPris  
            KOrdreLinje.Pris          = KOrdreLinje.NettoPris 
            KOrdreLinje.Linjesum      = KOrdreLinje.NettoPris
            KORdreLinje.Varetekst     = "FRAKT"
            KORdreLinje.VareNr        = cFraktVareNr
            NO-ERROR.
        END. /* FRAKT */
    END. /* OPPRETT_KORDRE */
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

