&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEFINE VARIABLE piLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cSourceType             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReadMode               AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFile                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSchemaLocation         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldTypeMapping       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVerifySchemaMode       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRetOK                  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hDSetHead               AS HANDLE    NO-UNDO.
DEFINE VARIABLE hDSetItem               AS HANDLE    NO-UNDO.
DEFINE VARIABLE cValKod                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE h_prisko    AS HANDLE  NO-UNDO.
DEFINE VARIABLE h_dvpifilhode AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvpiartbas  AS HANDLE NO-UNDO.
DEFINE VARIABLE lbufFilId        AS DECIMAL NO-UNDO.
DEFINE VARIABLE piAntLinjer AS INTEGER NO-UNDO.
DEFINE VARIABLE pbOk        AS LOG     NO-UNDO.
DEFINE VARIABLE cEDB-System AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLength AS INTEGER NO-UNDO.
DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
DEFINE VARIABLE cEkstVPILevLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE bUndertrykkErrLogg      AS LOG NO-UNDO.
DEFINE VARIABLE cFilPrefix AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
 
DEF VAR hReIPriceCostPrice AS HANDLE NO-UNDO.
DEF VAR hReIPriceRetailPrice AS HANDLE NO-UNDO.
DEF VAR hReITradeItemPriceHeader AS HANDLE NO-UNDO.
DEF VAR hReItemSalesRestriction AS HANDLE NO-UNDO.
DEF VAR hReItemCharacteristic AS HANDLE NO-UNDO.
DEF VAR hRelItemDepositItem AS HANDLE NO-UNDO.
DEF VAR hRelItemSupplierItem AS HANDLE NO-UNDO.
DEF VAR hRelItemProperty AS HANDLE NO-UNDO.
DEF VAR hRelItemPrice AS HANDLE NO-UNDO.
DEF VAR hRelItemAdditionalGTIN AS HANDLE NO-UNDO.

DEFINE VARIABLE chMode                AS CHARACTER NO-UNDO.
DEFINE VARIABLE chFile                AS CHARACTER NO-UNDO.
DEFINE VARIABLE chFormatted           AS LOG NO-UNDO.
DEFINE VARIABLE chOmit-Initial-Values AS CHAR  NO-UNDO.

DEF VAR iCL           AS INT FORMAT ">>>>>9" NO-UNDO.
DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.
DEF VAR cBkuFil       AS CHARACTER NO-UNDO.
DEF VAR ctmpKatalog   AS CHAR NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cReturnStatus AS CHARACTER NO-UNDO.
DEF VAR hInstance     AS INT NO-UNDO.
DEF VAR cRet_val      AS CHAR NO-UNDO.
DEF VAR cLoggKatalog  AS CHAR NO-UNDO.
DEF VAR lOk           AS LOG NO-UNDO.
DEFINE VARIABLE cKatalog      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPriCatfil    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iEkstVPILevNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iLevNr        AS INTEGER NO-UNDO.

DEFINE VARIABLE cFieldLst1    AS CHAR     NO-UNDO.
DEFINE VARIABLE cFieldLst2    AS CHAR     NO-UNDO.
DEFINE VARIABLE cFieldLst3    AS CHAR     NO-UNDO.
DEFINE VARIABLE cFieldLst4    AS CHAR     NO-UNDO.
DEFINE VARIABLE cFieldLst5    AS CHAR     NO-UNDO.
DEFINE VARIABLE cFieldLst6    AS CHAR     NO-UNDO.
DEFINE VARIABLE cFieldLst7    AS CHAR     NO-UNDO.
DEFINE VARIABLE cFieldLst8    AS CHAR     NO-UNDO.
DEFINE VARIABLE cFieldLst9    AS CHAR     NO-UNDO.

DEFINE STREAM InnFil.
DEFINE STREAM UtFil.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr   AS INT
  FIELD Tekst     AS CHAR
  FIELD ErrNr     AS INTEGER
  FIELD ButikkNr  AS INTEGER
  FIELD Gradering AS INTEGER 
  INDEX Feil ErrNr
  INDEX Linje LinjeNr. 

DEFINE TEMP-TABLE VPIHeader NO-UNDO SERIALIZE-NAME "Header"
    FIELD TimeStamp AS CHAR INIT ? 
    FIELD SellerGLN AS CHAR
    FIELD BrokerGLN AS CHAR
    FIELD CustomerGLN AS CHAR
    FIELD CustomerID AS CHAR
    INDEX Idx1 AS PRIMARY TimeStamp.

/* Item fil */          
DEFINE TEMP-TABLE VPIItem NO-UNDO SERIALIZE-NAME "Item"
    FIELD itemID AS CHAR INITIAL ?
    FIELD ArticleID AS CHAR 
    FIELD Description AS CHAR   
    FIELD Brand AS CHAR   
    FIELD DescriptionShort AS CHAR   
    FIELD WeightVolume AS CHAR   
    FIELD PackageTypeCode AS CHAR   
    FIELD PackageTypeDescription AS CHAR   
    FIELD Label1Name AS CHAR   
    FIELD Label1Text1 AS CHAR   
    FIELD Label1Text2 AS CHAR   
    FIELD Label2Name AS CHAR   
    FIELD LabelPriceCompareUnit AS CHAR   
    FIELD ReplacedItemID AS CHAR   
    FIELD UOM AS CHAR   
    FIELD SeasonCategory AS CHAR   
    FIELD HierarchyLevel AS INT   
    FIELD ArticleHierarchy AS CHAR   
    FIELD StartAvailable AS CHAR   
    FIELD EndAvailability AS CHAR   
    FIELD Warranty AS CHAR   
    FIELD WeightItem AS CHAR   
    FIELD NetContent AS DEC   
    FIELD NetContentUnit AS CHAR   
    FIELD ComparePriceFactor AS DEC   
    FIELD ExposureCode AS CHAR   
    FIELD CountryOfOrigin AS CHAR   
    FIELD GrossWeight AS DEC   
    FIELD AgeLimit AS INT   
    FIELD NetWeight AS DEC   
    FIELD WeightUnit AS CHAR   
    FIELD Volume AS DEC   
    FIELD VolumeUnit AS CHAR       
    FIELD Length AS DEC   
    FIELD Width AS DEC   
    FIELD Height AS DEC   
    FIELD MeasurementUnit AS CHAR   
    FIELD TaxCode AS CHAR   
    FIELD TaxRate AS DEC   
    INDEX Idx1 AS PRIMARY itemID.

/* Salgsrestriksjoner som henger på iTem */             
DEFINE TEMP-TABLE VPISalesRestriction NO-UNDO SERIALIZE-NAME "SalesRestriction"
    FIELD itemID      AS CHAR INITIAL ? 
    FIELD ID          AS CHAR INITIAL ?
    FIELD Description AS CHAR   
    INDEX Idx1 AS PRIMARY itemID ID.

/* Karrakteristikk som henger på iTem */                
DEFINE TEMP-TABLE VPICharacteristic NO-UNDO SERIALIZE-NAME "Characteristic"
    FIELD itemID      AS CHAR INITIAL ? 
    FIELD ID          AS CHAR INITIAL ?
    FIELD Description AS CHAR   
    INDEX Idx1 AS PRIMARY itemID ID.

/* DepositItem som henger på iTem */            
DEFINE TEMP-TABLE VPIDepositItem NO-UNDO SERIALIZE-NAME "DepositItem"
    FIELD itemID      AS CHAR INITIAL ? 
    FIELD DepItemID   AS CHAR INITIAL ? SERIALIZE-NAME "ItemId"
    FIELD Description AS CHAR 
    FIELD Price       AS DEC   
    FIELD Quantity    AS INT 
    INDEX Idx1 AS PRIMARY itemID DepItemId.

/* Supplier som henger på iTem */               
DEFINE TEMP-TABLE VPISupplierItem NO-UNDO SERIALIZE-NAME "SupplierItem"
    FIELD itemID         AS CHAR INITIAL ? 
    FIELD SupplierID     AS CHAR INITIAL ?
    FIELD SupplierItemID AS CHAR 
    FIELD SupplierName   AS CHAR 
    FIELD GTIN           AS CHAR 
    FIELD UOM            AS CHAR 
    FIELD Preffered      AS CHAR 
    FIELD ExpirationDate AS CHAR 
    FIELD RemovalFlag    AS CHAR 
    FIELD StockItemCount AS INT 
    FIELD PalletType     AS CHAR 
    FIELD PalletSize     AS DEC 
    INDEX Idx1 AS PRIMARY itemID SupplierID SupplierItemID GTIN UOM.

/* Property som henger på iTem og Supplier */           
DEFINE TEMP-TABLE VPIProperty NO-UNDO SERIALIZE-NAME "Property"
    FIELD itemID         AS CHAR INITIAL ? 
    FIELD SupplierID     AS CHAR INITIAL ?
    FIELD SupplierItemID AS CHAR INITIAL ?
    FIELD GTIN           AS CHAR INITIAL ?
    FIELD PropValue      AS CHAR INITIAL ? SERIALIZE-NAME "Value"
    INDEX Idx1 AS PRIMARY itemID SupplierID SupplierItemID GTIN PropValue.

DEFINE TEMP-TABLE VPIPrice NO-UNDO SERIALIZE-NAME "Price"
    FIELD itemID  AS CHAR INITIAL ? 
    FIELD Dummy   AS CHAR INITIAL ?
    INDEX Idx1 AS PRIMARY Dummy.

/* Kostpriser som henger på iTem */             
DEFINE TEMP-TABLE VPICostPrice NO-UNDO SERIALIZE-NAME "CostPrice"
    FIELD itemID    AS CHAR INITIAL ? 
    FIELD StartDate AS CHAR INITIAL ?
    FIELD Amount    AS DEC  
    INDEX Idx1 AS PRIMARY ItemId StartDate.

/* Kostpriser som henger på iTem */             
DEFINE TEMP-TABLE VPIRetailPrice NO-UNDO SERIALIZE-NAME "RetailPrice"
    FIELD itemID    AS CHAR INITIAL ? 
    FIELD StartDate AS CHAR INITIAL ?
    FIELD Amount    AS DEC  
    INDEX Idx1 AS PRIMARY ItemID StartDate.

/* Strekkoder som henger på iTem */             
DEFINE TEMP-TABLE VPIAdditionalGTIN NO-UNDO SERIALIZE-NAME "AdditionalGTIN"
    FIELD itemID AS CHAR INITIAL ? 
    FIELD GTIN   AS CHAR INITIAL ?
    INDEX Idx1 AS PRIMARY itemID GTIN.

{windows.i}

DEFINE BUFFER clButiker FOR Butiker.
DEFINE BUFFER HKArtPris FOR ArtPris.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getDateDmy) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDateDmy Procedure 
FUNCTION getDateDmy RETURNS CHARACTER
        (cDato AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VareIdent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD VareIdent Procedure 
FUNCTION VareIdent RETURNS CHARACTER
        (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VareItemIdent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD VareItemIdent Procedure 
FUNCTION VareItemIdent RETURNS CHARACTER
        (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

bTest = FALSE.

{syspara.i 102 1 1 cFilPrefix}
IF cFilPrefix = '' THEN cFilPrefix = "VPILog_".
cFilPrefix = cFilPrefix + REPLACE(STRING(TODAY,"99/99/99"),'/','') + '_'.

{syspara.i 50 15 39 cTekst}
IF cTekst = '' OR CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bUndertrykkErrLogg = TRUE.
ELSE
  bUndertrykkErrLogg = FALSE.

{syspara.i 1 1 59 cLoggKatalog}
IF cLoggKatalog <> '' THEN DO:
    /* Sikrer at katalog finnes. */
    OS-CREATE-DIR VALUE(RIGHT-TRIM(cLoggKatalog,'\')).    
    cLoggKatalog = RIGHT-TRIM(cLoggKatalog,'\') + '\'.
END.

{syspara.i 1 1 52 cKatalog}
IF TRIM(cKatalog) <> '' THEN 
  cKatalog = RIGHT-TRIM(cKatalog,'\').


{syspara.i 50 15 46 cValKod}

{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE clButiker THEN 
DO:
  MESSAGE 'Ukjent butiknr satt for sentrallager.'
  VIEW-AS ALERT-BOX.
  RETURN.
END.

FOR EACH tt_Error:
  DELETE tt_Error.
END.

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ELSE iEkstVPILevNr = VPIFilHode.EkstVPILevNr.

FIND EkstVPILev NO-LOCK WHERE 
    EkstVPILEv.EkstVPILevNr = iEkstVPILevNr NO-ERROR.
IF AVAILABLE EkstVPILev THEN
  ASSIGN  
    iLevNr      = EkstVPILev.LevNr 
    cEDB-System = EkstVPILev.EDB-System.

/* Temp tabell */
{ttpricat.i &NEW=" " &SHARED=" "}
DEFINE BUFFER bufttPriKat FOR ttPriKat.

ASSIGN
    cPriCatfil = RIGHT-TRIM(TRIM(cKatalog),"\") + "\" + "GVPI" + STRING(VPIFilHode.EkstVPILevNr,"999") + REPLACE(STRING(TODAY,"99/99/99"),'/','') + "-" + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + ".csv"
    cFilNavn   = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn
    cBkuFil    = VPIFilHode.Katalog + "~\bku~\" + VPIFilHode.FilNavn.

/* Starter program for lasting av VPI mottak */
IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN prisko.p PERSISTENT SET h_PrisKo.

/* Setter opp datesett med tabeller og relasjoner. */
RUN ByggDatasett.

/* Leser inn data i datasettet. */
RUN LesInnFil.

/* Bygger opp PriCat temp tabellen. */
RUN ByggPriCat.
IF CAN-FIND(FIRST tt_Error) THEN  
    RUN ErrorLogg.

/* Eksporterer pricat filen. */
IF CAN-FIND(FIRST ttPriKat) THEN 
    RUN EksportPriCat.

/* Testing :) */
/*
hDsetHead:WRITE-XML("file",                         
                 "wrk\TradeItemPrice_testHead.xml", 
                 TRUE,  
                 ?).
hDsetItem:WRITE-XML("file",                         
                 "wrk\TradeItemPrice_testItem.xml", 
                 TRUE,  
                 ?).
*/                 
/* Testing slutt :( */

IF VALID-HANDLE(h_dvpifilhode) THEN
    DELETE PROCEDURE h_dvpifilhode.
IF VALID-HANDLE(h_dvpiartbas) THEN
    DELETE PROCEDURE h_dvpiartbas.
IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_PrisKo.

IF cRet_val = "OK" THEN 
  DO TRANSACTION:
    FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
    ASSIGN VPIFilHode.VPIFilStatus = 5.
    FIND CURRENT VPIFilHode NO-LOCK.
    RUN bibl_logg.p (RIGHT-TRIM(cFilPrefix,"_"), 'xinnTradeItemPrice - Innlesning avsluttet med returmelding: ' + cRet_Val).
    RETURN "OK".
  END.
ELSE DO:
      DO  TRANSACTION:
          FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
          ASSIGN VPIFilHode.VPIFilStatus = 9.
          FIND CURRENT VPIFilHode NO-LOCK.
      END.
      RUN bibl_logg.p (RIGHT-TRIM(cFilPrefix,"_"), 'xinnTradeItemPrice - LesInnFil returnerer feil: ' + cRet_Val).
      RETURN 'ERROR'.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggDatasett) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggDatasett Procedure 
PROCEDURE ByggDatasett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
CREATE DATASET hDsetHead.
hDsetHead:NAME           = "NodeHeader". 
hDsetHead:SERIALIZE-NAME = "Header". 

hDsetHead:ADD-BUFFER(BUFFER VPIHeader:HANDLE).

CREATE DATASET hDsetItem.
hDsetItem:NAME           = "NodeItem". 
hDsetItem:SERIALIZE-NAME = "Item". 

hDsetItem:ADD-BUFFER(BUFFER VPIItem:HANDLE).
hDsetItem:ADD-BUFFER(BUFFER VPISalesRestriction:HANDLE).
hDsetItem:ADD-BUFFER(BUFFER VPICharacteristic:HANDLE).
hDsetItem:ADD-BUFFER(BUFFER VPIDepositItem:HANDLE).
hDsetItem:ADD-BUFFER(BUFFER VPISupplierItem:HANDLE).
hDsetItem:ADD-BUFFER(BUFFER VPIProperty:HANDLE).
hDsetItem:ADD-BUFFER(BUFFER VPICostPrice:HANDLE).
hDsetItem:ADD-BUFFER(BUFFER VPIRetailPrice:HANDLE).
hDsetItem:ADD-BUFFER(BUFFER VPIAdditionalGTIN:HANDLE).

hReItemSalesRestriction = hDsetItem:ADD-RELATION(BUFFER VPIItem:HANDLE,BUFFER VPISalesRestriction:HANDLE,"itemID,itemID",                      TRUE,TRUE,TRUE,FALSE,TRUE).
hReItemCharacteristic   = hDsetItem:ADD-RELATION(BUFFER VPIItem:HANDLE,BUFFER VPICharacteristic:HANDLE,  "itemID,itemID",                      TRUE,TRUE,TRUE,FALSE,TRUE). 
hRelItemDepositItem     = hDsetItem:ADD-RELATION(BUFFER VPIItem:HANDLE,BUFFER VPIDepositItem:HANDLE,     "itemID,itemID",                      TRUE,TRUE,TRUE,FALSE,TRUE).
hRelItemSupplierItem    = hDsetItem:ADD-RELATION(BUFFER VPIItem:HANDLE,BUFFER VPISupplierItem:HANDLE,    "itemID,itemID",                      TRUE,TRUE,TRUE,FALSE,TRUE). 
hRelItemProperty        = hDsetItem:ADD-RELATION(BUFFER VPISupplierItem:HANDLE,BUFFER VPIProperty:HANDLE,"itemID,itemID,SupplierID,SupplierID",TRUE,TRUE,TRUE,FALSE,TRUE).
hReIPriceCostPrice      = hDsetItem:ADD-RELATION(BUFFER VPIItem:HANDLE,BUFFER VPICostPrice:HANDLE,       "itemID,itemID",                      TRUE,TRUE,TRUE,FALSE,TRUE).
hReIPriceRetailPrice    = hDsetItem:ADD-RELATION(BUFFER VPIItem:HANDLE,BUFFER VPIRetailPrice:HANDLE,     "itemID,itemID",                      TRUE,TRUE,TRUE,FALSE,TRUE).
hRelItemAdditionalGTIN  = hDsetItem:ADD-RELATION(BUFFER VPIItem:HANDLE,BUFFER VPIAdditionalGTIN:HANDLE,  "itemID,itemID",                      TRUE,TRUE,TRUE,FALSE,TRUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggPriCat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggPriCat Procedure 
PROCEDURE ByggPriCat :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEFINE VARIABLE piLoop    AS INTEGER NO-UNDO.

piLinjeNr = 1.

/* Oppretter eventuelle nye karakteristikk koder */
RUN OpprettKarakteristikk.

FIND FIRST VPIHeader.

ARTIKKEL_LOOP:
FOR EACH VPIItem:  
  piLinjeNr = piLinjeNr  + 1.
  IF AVAILABLE ttPriKat THEN RELEASE ttPriKat.

  FIND FIRST VPISupplierItem     WHERE VPISupplierItem.ItemId     = VPIItem.ItemId AND 
                                       VPISupplierItem.Preffered  = 'true' NO-ERROR.
  IF NOT AVAILABLE VPISupplierItem THEN 
    FIND FIRST VPISupplierItem WHERE VPISupplierItem.ItemId = VPIItem.ItemId NO-ERROR.

  cTekst = TRIM(STRING(VPIItem.ItemID)).
  IF LENGTH(cTekst) > 13 THEN 
  SKAL_IKKE_MED:
  DO:
      CREATE tt_Error.
      ASSIGN
        tt_Error.LinjeNr   = piLinjeNr
        tt_Error.ErrNr     = 1
        tt_Error.ButikkNr  = INT(VPIHeader.CustomerID)
        tt_Error.Tekst = VareItemIdent() + "* Strekkoder med lengde > 13 importeres ikke (" + VPIItem.ItemID + "). ".
      NEXT ARTIKKEL_LOOP.
  END. /* SKAL_IKKE_MED */

  FIND FIRST VPICostPrice WHERE VPICostPrice.ItemId = VPIItem.ItemId NO-ERROR.  
  /*
  IF NOT AVAILABLE VPICostPrice THEN
  DO: 
      CREATE tt_Error.
      ASSIGN
          tt_Error.LinjeNr   = piLinjeNr
          tt_Error.ErrNr     = 1
          tt_Error.ButikkNr  = INT(VPIHeader.CustomerID)
          tt_Error.Tekst = VareItemIdent() + "* GTIN (" + VPIItem.ItemID + ") mangler CostPrice. Ikke importert.".
      NEXT ARTIKKEL_LOOP.
  END.    
  */
  FIND FIRST VPIRetailPrice WHERE VPIRetailPrice.ItemId = VPIItem.ItemId NO-ERROR.  
  IF NOT AVAILABLE VPIRetailPrice THEN
  DO: 
      CREATE tt_Error.
      ASSIGN
          tt_Error.LinjeNr   = piLinjeNr
          tt_Error.ErrNr     = 1
          tt_Error.ButikkNr  = INT(VPIHeader.CustomerID)
          tt_Error.Tekst = VareItemIdent() + "* GTIN (" + VPIItem.ItemID + ") mangler RetailPrice. Ikke importert.".
      NEXT ARTIKKEL_LOOP.
  END.

  IF AVAILABLE VPICostPrice AND  VPICostPrice.Amount = 0 
  THEN DO: 
      CREATE tt_Error.
      ASSIGN
          tt_Error.LinjeNr   = piLinjeNr
          tt_Error.ErrNr     = 1
          tt_Error.ButikkNr  = INT(VPIHeader.CustomerID)
          tt_Error.Tekst = VareItemIdent() + "* GTIN (" + VPIItem.ItemID + ") CostPrice er = 0 . Ikke importert.".
      NEXT ARTIKKEL_LOOP.
  END.
  IF AVAILABLE VPIRetailPrice AND VPIRetailPrice.Amount = 0 
  THEN DO: 
      CREATE tt_Error.
      ASSIGN
          tt_Error.LinjeNr   = piLinjeNr
          tt_Error.ErrNr     = 1
          tt_Error.ButikkNr  = INT(VPIHeader.CustomerID)
          tt_Error.Tekst = VareItemIdent() + "* GTIN (" + VPIItem.ItemID + ") RetailPrice er = 0 . Ikke importert.".
      NEXT ARTIKKEL_LOOP.
  END.

  FIND FIRST VPISalesRestriction WHERE VPISalesRestriction.ItemId = VPIItem.ItemId NO-ERROR.
  FIND FIRST VPICharacteristic   WHERE VPICharacteristic.ItemId   = VPIItem.ItemId NO-ERROR.
  FIND FIRST VPIAdditionalGTIN   WHERE VPIAdditionalGTIN.ItemId   = VPIItem.ItemId NO-ERROR.

  FIND FIRST VPIProperty         WHERE 
             VPIProperty.ItemId         = VPIItem.ItemId AND    
             VPIProperty.SupplierID     = VPISupplierItem.SupplierID AND 
             VPIProperty.SupplierItemID = VPISupplierItem.SupplierItemID NO-ERROR.

  /* Konverterer dato fra YYY-MM-DD til DD/MM/YYY */
  IF AVAILABLE VPIRetailPrice THEN 
    ASSIGN 
      VPIRetailPrice.StartDate       = getDateDmy(VPIRetailPrice.StartDate).    
  IF AVAILABLE VPICostPrice THEN 
    ASSIGN 
      VPICostPrice.StartDate         = getDateDmy(VPICostPrice.StartDate).
      
  IF AVAILABLE VPIItem THEN 
    ASSIGN     
      VPIItem.StartAvailabl          = getDateDmy(VPIItem.StartAvailabl)
      VPIItem.EndAvailability        = getDateDmy(VPIItem.EndAvailability).
  IF AVAILABLE VPISupplierItem THEN 
    ASSIGN 
      VPISupplierItem.ExpirationDate = getDateDmy(VPISupplierItem.ExpirationDate).

    CREATE ttPriKat.
    ASSIGN
        ttPriKat.EkstVPILevNr     = iEkstVPILevNr
        ttPriKat.LinjeNr          = piLinjeNr
/*  1 */ ttPriKat.R1              = "R1"
/*  2 */ ttPriKat.LevNr           = STRING(iLevNr) 
/*  3 */ ttPriKat.LevModellNr     = IF AVAILABLE VPISupplierItem THEN TRIM(STRING(VPISupplierItem.SupplierItemID)) ELSE ''
/*  4 */ ttPriKat.EANnr           = IF AVAILABLE VPIItem THEN TRIM(STRING(VPIItem.ItemID)) ELSE ''
/*  5 */ ttPriKat.VareTekst       = IF AVAILABLE VPIItem THEN REPLACE(TRIM(VPIItem.Description),","," ") ELSE ''
         ttPriKat.Varetekst       = REPLACE(ttPriKat.VareTekst,';',',')
/*  6 */ ttPriKat.FargeKode       = "" /* Farge er ikke i bruk */
/*  7 */ ttPriKat.FargeTekst      = "" /* Settes blank */
/*  8 */ ttPriKat.Str             = " 1" /* Størrelse 1 */
/*  9 */ ttPriKat.StrTab          = "2" /* Størrelsestype 2 */
/* 10 */ ttPriKat.Varemerke       = "" /*IF AVAILABLE VPIItem THEN TRIM(REPLACE(TRIM(SUBSTR(VPIItem.Brand,1,30)),","," "),'"') ELSE ''*/ 
/* 11 */ ttPriKat.Enh             = 'Stk' /*IF AVAILABLE VPIItem THEN TRIM(STRING(VPIItem.UOM)) ELSE ''*/
         ttPriKat.Enh             = REPLACE(ttPriKat.Enh,';',',')
/* 12 */ ttPriKat.AntIEnh         = IF AVAILABLE VPISupplierItem THEN REPLACE(TRIM(TRIM(STRING(VPISupplierItem.StockItemCount,">>>9")),'"'),'.',',') ELSE ''
/* 13 */ ttPriKat.LevPrisEngros   = IF AVAILABLE VPICostPrice THEN REPLACE(TRIM(REPLACE(STRING(VPICostPrice.Amount),",","."),'"'),'.',',') ELSE '' 
/* 14 */ ttPriKat.ValKod          = cValKod 
/* 15 */ ttPriKat.forhRab%        = "" 
/* 16 */ ttPriKat.suppRab%        = "" 
/* 17 */ ttPriKat.VeilPris        = IF AVAILABLE VPIRetailPrice THEN REPLACE(TRIM(REPLACE(STRING(VPIRetailPrice.Amount),",","."),'"'),'.',',') ELSE '' 
/* 18 */ ttPriKat.PAKstru         = "" 
/* 19 */ ttPriKat.LevUke1         = "" 
/* 20 */ ttPriKat.LevUke2         = "" 
/* 21 */ ttPriKat.LevUke3         = "" 
/* 22 */ ttPriKat.LevUke4         = "" 
/* 23 */ ttPriKat.VareGruppe      = IF AVAILABLE VPIItem THEN TRIM(STRING(VPIItem.ArticleHierarchy)) ELSE '' 
/* 24 */ ttPriKat.LevNavn         = "" 
/* 25 */ ttPriKat.LevKod          = '' /*IF AVAILABLE VPIItem THEN TRIM(STRING(VPIItem.ArticleID)) ELSE ''*/
/* 26 */ ttPriKat.nettoForh       = "" 
/* 27 */ ttPriKat.kalkForh        = "" 
/* 28 */ ttPriKat.BFforh          = "" 
/* 29 */ ttPriKat.nettoSupp       = "" 
/* 30 */ ttPriKat.kalkSupp        = "" 
/* 31 */ ttPriKat.BFsupp          = "" 
/* 32 */ ttPriKat.MarkedsPris     = IF AVAILABLE VPIRetailPrice THEN REPLACE(TRIM(REPLACE(STRING(VPIRetailPrice.Amount),",","."),'"'),'.',',')  ELSE ''
/* 33 */ ttPriKat.Sortiment       = "" 
/* 34 */ ttPriKat.Sesong          = "" 
/* 35 */ ttPriKat.VPIBildeKode    = "" 
/* 36 */ ttPriKat.Merknad         = IF AVAILABLE VPIProperty THEN TRIM(VPIProperty.PropValue) ELSE ''
/* 37 */ ttPriKat.KjedeValutaPris = "" 
/* 38 */ ttPriKat.KjedeProdusent  = ""  
/* 39 */ ttPriKat.ERPNr           = IF AVAILABLE VPIItem THEN TRIM(STRING(VPIItem.ArticleId)) ELSE ''
/* 40 */ ttPriKat.SalgsEnhetsType = "1"
/* 41 */ ttPriKat.AktivFraDato    = IF AVAILABLE VPICostPrice AND VPICostPrice.StartDate <> ? THEN  STRING(VPICostPrice.StartDate) ELSE ''
/* 42 */ ttPriKat.AktivTilDato    = ""
/* 43 */ ttPriKat.Bongtekst       = IF AVAILABLE VPIItem THEN REPLACE(TRIM(SUBSTR(VPIItem.DescriptionShort,1,20)),","," ") ELSE ''
         ttPriKat.Bongtekst       = REPLACE(ttPriKat.Bongtekst,';',',')
/* 44 */ ttPriKat.Etikettekst1    = IF AVAILABLE VPIItem THEN REPLACE(TRIM(SUBSTR(VPIItem.Label1Name,1,30)),","," ") ELSE ''
         ttPriKat.Etikettekst1    = REPLACE(ttPriKat.Etikettekst1,';',',')
         ttPriKat.Etikettekst1    = (IF ttPriKat.Etikettekst1 = '' THEN ttPriKat.Varetekst ELSE ttPriKat.Etikettekst1)
/* 45 */ ttPriKat.Funksjonskode   = "N"
/* 46 */ ttPriKat.Mva_Proc        = IF AVAILABLE VPIItem THEN STRING(DEC(REPLACE(TRIM(STRING(VPIItem.TaxRate),'"'),'.',',')) * 100) ELSE ''
/* 47 */ ttPriKat.LinkVare        = "" 
/* 48 */ ttPriKat.PantBelop       = "" 
/* 49 */ ttPriKat.Filial          = IF AVAILABLE VPIHeader THEN TRIM(VPIHeader.CustomerID) ELSE ''
/* 50 */ ttPriKat.Produsent       = '' /*IF AVAILABLE VPISupplierItem THEN TRIM(STRING(VPISupplierItem.SupplierID)) ELSE ''*/
/* 51 */ ttPriKat.Mengde          = REPLACE(TRIM(IF VPIItem.NetContent = 1 THEN "1" ELSE REPLACE(STRING(VPIItem.NetContent),",","."),'"'),'.',',') /* Konv. faktor jamførpris */

/* 52 */ ttPriKat.JamforEnhet     = IF AVAILABLE VPIItem THEN TRIM(STRING(VPIItem.NetContentUnit)) ELSE ''
/*/* 53 */ ttPriKat.Kontrolleres    = */
/* 65 */ ttPriKat.Alder           = IF AVAILABLE VPIItem THEN TRIM(STRING(VPIItem.AgeLimit)) ELSE ''
/* 98 */ ttPriKat.Grunnsortiment  = IF CAN-DO('P',TRIM(STRING(VPIItem.ExposureCode))) THEN 'J' ELSE 'N'
/* 99 */ ttPriKat.Opphav          = '6' /* XML VPI import. */                                                                                                             

/* 55  ttPriKat.OpprettArtikkel */
         ttPriKat.Lager           = NO 
/* 76 */ ttPriKat.AlfaKode2       = IF AVAILABLE VPIItem THEN TRIM(STRING(VPIItem.CountryOfOrigin)) ELSE ''

/* 90 */ ttPriKat.Etikett         = 2
/*103 */ ttPriKat.Etikettekst2    = IF AVAILABLE VPIItem THEN REPLACE(TRIM(VPIItem.Label1Text1),","," ") ELSE ''
         ttPriKat.Etikettekst2    = REPLACE(ttPriKat.Etikettekst2,';',',')
/*104 */ ttPriKat.ArtSlag         = '0'

/*117 */ ttPriKat.GarantiKl       = "" /*IF AVAILABLE VPIItem THEN TRIM(VPIItem.Warranty) ELSE ''*/
/*118 */ ttPriKat.PostBredde      = IF AVAILABLE VPIItem THEN REPLACE(TRIM(STRING(VPIItem.Width)),'.',',') ELSE ''
/*119 */ ttPriKat.PostHoyde       = IF AVAILABLE VPIItem THEN REPLACE(TRIM(STRING(VPIItem.Height)),'.',',') ELSE ''
/*120 */ ttPriKat.PostLengde      = IF AVAILABLE VPIItem THEN REPLACE(TRIM(STRING(VPIItem.Length)),'.',',') ELSE ''
/*121 */ ttPriKat.PostVekt        = IF AVAILABLE VPIItem THEN REPLACE(TRIM(STRING(VPIItem.GrossWeight)),'.',',') ELSE ''
         .
    
    /* Blank EAN importeres ikke */ 
    IF ttPriKat.EANnr = '' THEN 
    DO:
        CREATE tt_Error.
        ASSIGN
          ttPriKat.ErrFlag   = TRUE
          tt_Error.LinjeNr   = iAntLinjer
          tt_Error.Tekst     = "** Blank GTIN (" + ttPriKat.EANNr + "). Linje IKKE innlest: " + string(iAntLinjer) + "."
          tt_Error.Gradering = 12.
    END.    

    /* Hvis det skulle være noe rusk, så gjør vidette her :) */
    ASSIGN 
    /* 54 */ ttPriKat.ArtikkelNr = DEC(ttPriKat.EANNr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
        CREATE tt_Error.
        ASSIGN
          ttPriKat.ErrFlag   = TRUE
          tt_Error.LinjeNr   = iAntLinjer
          tt_Error.Tekst     = "** GTIN (" + ttPriKat.EANNr + ") med ugyldige tegn. Linje IKKE innlest: " + string(iAntLinjer) + "."
          tt_Error.Gradering = 12
          ttPriKat.EANNr     = ''.
    END.
        
    /* Fikser postenhet og vekt. */
    IF INT(ttPriKat.PostBredde) > 0 THEN ttPriKat.PostBredde = STRING(INT(ttPriKat.PostBredde) / 10).
    IF INT(ttPriKat.PostHoyde)  > 0 THEN ttPriKat.PostHoyde  = STRING(INT(ttPriKat.PostHoyde) / 10).
    IF INT(ttPriKat.PostLengde) > 0 THEN ttPriKat.PostLengde = STRING(INT(ttPriKat.PostLengde) / 10).
    IF DEC(ttPriKat.PostVekt)   > 0 THEN ttPriKat.PostVekt   = STRING(ROUND(DEC(ttPriKat.PostVekt) / 1000,3)).
    
    IF ttPriKat.JamforEnhet = 'ST' THEN ttPriKat.JamforEnhet = 'STK'. 
    /*IF DECIMAL(ttPriKat.Produsent) > 1000000 THEN ttPriKat.Produsent = STRING(DEC(ttPriKat.Produsent) - 1000000).*/    
            
    /* Konvertering av filial til butikknummer */
    IF cEDB-System <> '' AND 
        CAN-FIND(FIRST ImpKonv NO-LOCK WHERE
          ImpKonv.EDB-System = cEDB-System AND
          ImpKonv.Tabell     = 'Butiker') THEN 
    DO:
        FIND FIRST ImpKonv NO-LOCK WHERE
          ImpKonv.EDB-System = cEDB-System AND
          ImpKonv.Tabell     = 'Butiker' AND
          ImpKonv.EksterntId = TRIM(ttPriKat.Filial) NO-ERROR. 
        IF AVAILABLE ImpKonv THEN ttPriKat.Filial = ImpKonv.InterntId.
        ELSE DO:
            CREATE tt_Error.
            ASSIGN
              ttPriKat.ErrFlag   = TRUE
              tt_Error.LinjeNr   = iAntLinjer
              tt_Error.Tekst     = "** Butikk (" + ttPriKat.Filial + ") uten mapping. Linje IKKE innlest: " + string(iAntLinjer) + "."
              tt_Error.Gradering = 12.
        END.
    END.
   
   IF AVAILABLE hkArtPris  THEN RELEASE hkartPris.
   IF AVAILABLE Strekkode  THEN RELEASE Strekkode.
   
   /* Henter artikkelnr og ArtPris for profilene. */
   FIND Strekkode NO-LOCK WHERE
        Strekkode.Kode = ttPriKat.EANnr NO-ERROR.
   FIND VPIStrekkode NO-LOCK WHERE 
        VPIStrekkode.EkstVPILevNr = VPIFilHode.EkstVPILevNr AND 
        VPIStrekkode.Kode = ttPriKat.EANnr NO-ERROR.          
   /* Henter HK's kalkyle hvis den finnes.      */

   /* Mva skal ikke endres ved VPI import uansett. */
   IF AVAILABLE Strekkode THEN 
   DO:
     FIND hkArtPris NO-LOCK WHERE 
       hkArtPris.ArtikkelNr = Strekkode.ArtikkelNr AND 
       hkArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
     FIND ArtBas NO-LOCK WHERE 
       ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
     IF AVAILABLE ArtBas THEN 
       FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
     IF AVAILABLE VarGr THEN
       FIND Moms OF VarGr NO-LOCK NO-ERROR. 
       IF AVAILABLE Moms THEN 
           ASSIGN
               ttPriKat.Mva_Proc = STRING(Moms.MomsProc).    
   END. 
   /* NB: Filial er mappet om til PRS butikknr eller til HK's VPILevNr lenger oppe.               */
   /* Er filial angitt og denne matcher med et butikknummer, skal butikknr settes i prikat filen. */
   /* Det skal legges opp en post i butikkens VPI lomme, og nye poster legges også i hk's lomme.  */
   /* Posten som legges i hk's VPI lomme, skal ha hk's utpris og den nye innprisen. Og posten som */
   /* legges i butikkens VPI lomme, skal ha den nye utpris og hk's innpris.                       */
   lDec = 0.
   ASSIGN lDec = DECIMAL(ttPriKat.Filial) NO-ERROR.
   IF lDec > 0 AND 
      CAN-FIND(Butiker WHERE Butiker.Butik = INTEGER(lDec)) THEN 
   DO:       
       /* Setter priser i butikkens VPI lomme. */ 
       IF INTEGER(lDec) <> clButiker.Butik THEN 
       ASSIGN
         ttPriKat.LevPrisEngros = (IF AVAILABLE hkArtPris 
                                     THEN STRING(hkArtPris.InnkjopsPris[1]) 
                                     ELSE ttPriKat.LevPrisEngros)
         ttPriKat.ButikkNr = INTEGER(lDec).
       /* Setter butikknr */
       ELSE 
       ASSIGN 
         ttPriKat.Filial   = ''
         ttPriKat.ButikkNr = 0.
         
       IF NOT CAN-DO(cEkstVPILevLst,STRING(lDec)) THEN 
         cEkstVPILevLst = cEkstVPILevLst + (IF cEkstVPILevLst = '' THEN '' ELSE ',') + STRING(lDec).
       RUN opprettEkstVPILev (INTEGER(lDec)).
       
   END.
   ELSE IF ttPriKat.Filial <> '' THEN 
       DO:
         CREATE tt_Error.
         ASSIGN
           ttPriKat.ErrFlag   = TRUE 
           tt_Error.LinjeNr   = iAntLinjer
           tt_Error.Tekst     = VareIdent() + "* Ukjent butikknummer (Fillal: " + ttPriKat.Filial + ") på linje: " + string(iAntLinjer) + "."
           tt_Error.ButikkNr  = INT(ttPriKat.Filial)           
           tt_Error.Gradering = 12.
       END.
    
    /* Konverteringstabell - Varegruppe */
    IF LENGTH(ttPriKat.Varegruppe) > 8 THEN 
        ttPriKat.Varegruppe = SUBSTRING(ttPriKat.Varegruppe,1,8).
    IF CAN-FIND(FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = 'VarGr') THEN 
    DO:
      FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = 'VarGr' AND 
        ImpKonv.EksterntId = ttPriKat.Varegruppe NO-ERROR.
      IF NOT AVAILABLE ImpKonv AND LENGTH(ttPriKat.Varegruppe) = 8 THEN
      DO:
        IF LENGTH(ttPriKat.Varegruppe) > 6 THEN 
            ttPriKat.Varegruppe = SUBSTRING(ttPriKat.Varegruppe,1,6).
      END. 
    END.
  
    /* ArtikkelNr skal være lik EAN koden og ny artikkel skal legges opp. */
    IF NOT CAN-FIND(FIRST ArtBas NO-LOCK WHERE 
                    ArtBas.ArtikkelNr = DEC(ttPriKat.EANNr)) AND 
       NOT CAN-FIND(FIRST Strekkode WHERE 
                    Strekkode.Kode = ttPriKat.EANNr) THEN 
      ttPriKat.ArtikkelNr = DEC(ttPriKat.EANNr).
    ELSE DO:
        FIND FIRST Strekkode NO-LOCK WHERE 
            Strekkode.Kode = ttPriKat.EANNr NO-ERROR.
        IF AVAILABLE Strekkode THEN
        BLOKKEN: 
        DO:
           FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
           IF NOT AVAILABLE ArtBas THEN 
             LEAVE BLOKKEN.
           IF ArtBas.ArtikkelNr >= 900000 AND 
              ArtBas.ArtikkelNr <= 900199 THEN.
           ELSE ttPriKat.ArtikkelNr = ArtBas.ArtikkelNr.
        END. /* BLOKKEN */
        ELSE DO:
            FIND ArtBas NO-LOCK WHERE 
                ArtBas.ArtikkelNr = DEC(ttPriKat.EANNr) NO-ERROR.
            IF AVAILABLE ArtBas THEN 
                ttPriKat.ArtikkelNr = ArtBas.ArtikkelNr.
        END.
    END.
       
    /* Det skal alltid bare kom en slik record. */
    FOR EACH VPIDepositItem WHERE VPIDepositItem.ItemId = VPIItem.ItemId:
        ASSIGN 
            ttPriKat.LinkVare    = TRIM(STRING(VPIDepositItem.DepItemID))
            ttPriKat.AntLinkvare = TRIM(STRING(VPIDepositItem.Quantity))
            . 
    END.
    
    /* Legger på Karakteristikk */
    FOR EACH VPICharacteristic WHERE VPICharacteristic.ItemId = VPIItem.ItemId:
        ASSIGN
          ttPriKat.Karakteristikk = ttPriKat.Karakteristikk + 
                                    (IF ttPriKat.Karakteristikk <> '' THEN ',' ELSE '') + 
                                    TRIM(VPICharacteristic.ID).  
    END.
    
    /* Kontroll av PantLink. Kommer som EAN kode.*/
    IF ttPriKat.LinkVare <> '' AND NOT CAN-FIND(Strekkode WHERE Strekkode.Kode = ttPriKat.LinkVare) THEN 
    DO:
        CREATE tt_Error.
        ASSIGN
          ttPriKat.ErrFlag   = TRUE
          ttPriKat.BehStatus = 30 
          tt_Error.LinjeNr   = ttPriKat.LinjeNr
          tt_Error.ErrNr     = 1
          tt_Error.Tekst     = VareIdent() + "* GTIN (" + ttPriKat.EANNr + ") har ukjent PantLink " + ttPriKat.LinkVare + " . Ikke importert.".
          tt_Error.ButikkNr  = ttPriKat.ButikkNr.
    END.

    /* Kontroll av posten. */
    IF ttPriKat.EANNr <> '' THEN 
    SJEKK_EAN:
    DO:
      IF NOT CAN-DO('7,8,11,12,13',STRING(LENGTH(ttPriKat.EANNr)))
      THEN DO:
        CREATE tt_Error.
        ASSIGN
          ttPriKat.ErrFlag   = TRUE 
          tt_Error.LinjeNr   = ttPriKat.LinjeNr
          tt_Error.ErrNr     = 1
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
          tt_Error.Tekst = VareIdent() + "* KUN strekkoder med lengde 7,8,11,12 og 13 importeres. (" + ttPriKat.EANNr + ") ikke importert. ".
      END.
      ELSE DO:
          cTekst = ttPriKat.EANnr.
          IF LENGTH(cTekst) < 13 THEN
              cTekst = FILL('0',13 - LENGTH(cTekst)) + cTekst.          
          RUN bibl_chkean.p (INPUT-OUTPUT cTekst).
          cTekst2 = RETURN-VALUE.
          IF cTekst2 <> '' THEN
          KONTROLLEAN: 
          DO:
            CREATE tt_Error.
            ASSIGN 
              ttPriKat.ErrFlag   = TRUE 
              tt_Error.LinjeNr   = ttPriKat.LinjeNr
              tt_Error.ErrNr     = 1
              tt_Error.ButikkNr  = ttPriKat.ButikkNr
              tt_Error.Tekst = VareIdent() + "* GTIN (" + ttPriKat.EANnr + ") feil i GTIN. Ikke importert. Feil: " + cTekst2.
          END.
      END.
    END. /* SJEKK_EAN */ 
        
    IF NOT CAN-FIND(FIRST Moms WHERE Moms.MomsProc = DEC(ttPriKat.Mva_Proc)) 
    THEN DO: 
        CREATE tt_Error.
        ASSIGN
          ttPriKat.ErrFlag   = TRUE 
          tt_Error.LinjeNr   = ttPriKat.LinjeNr
          tt_Error.ErrNr     = 1
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
          tt_Error.Tekst = VareIdent() + "* GTIN (" + ttPriKat.EANNr + ") ukjent mva% " + ttPriKat.Mva_Proc +  ". Ikke importert.".
    END.

    /* Sprekkekontroll på varegruppe. */
    SPERRE_PA_VG:
    DO:
      FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = 'NO_VarGr' AND 
        ImpKonv.EksterntId BEGINS ttPriKat.Varegruppe AND 
        ImpKonv.InterntId  = 'NO_IMP' NO-ERROR.
      IF AVAILABLE ImpKonv THEN
      DO:
        CREATE tt_Error.
        ASSIGN
          ttPriKat.ErrFlag   = TRUE 
          tt_Error.LinjeNr   = ttPriKat.LinjeNr
          tt_Error.ErrNr     = 1
          tt_Error.ButikkNr  = ttPriKat.ButikkNr
          tt_Error.Tekst     = VareIdent() + "* GTIN (" + ttPriKat.EANNr + ") i sperret varegruppe (" + ttPriKat.Varegruppe + "). Ikke importert.".
      END. 
    END. /* SPERRE_PA_VG */
        
    piLoop = 1.
    IF ttPriKat.ErrFlag = FALSE THEN 
    FOR EACH VPISupplierItem WHERE VPISupplierItem.ItemId = VPIItem.ItemId:
      IF piLoop > 1 THEN 
      DO:
        FIND FIRST VPIProperty         WHERE 
                   VPIProperty.ItemId         = VPIItem.ItemId AND    
                   VPIProperty.SupplierID     = VPISupplierItem.SupplierID AND 
                   VPIProperty.SupplierItemID = VPISupplierItem.SupplierItemID NO-ERROR.
        CREATE bufttPriKat.
        BUFFER-COPY ttPriKat
          EXCEPT EANnr
          TO bufttPriKat
        ASSIGN 
          bufttPriKat.LevNr       = TRIM(STRING(VPISupplierItem.SupplierID))
          bufttPriKat.LevModellNr = TRIM(STRING(VPISupplierItem.SupplierItemID))
          bufttPriKat.AntIEnh     = REPLACE(TRIM(TRIM(STRING(VPISupplierItem.StockItemCount,">>>9")),'"'),'.',',')
          bufttPriKat.Merknad     = IF AVAILABLE VPIProperty THEN TRIM(VPIPRoperty.PropValue) ELSE ''
          .
      END.
    END.

    /* Disse legges som ekstra EAN koder på samme artikkel. */  
    FOR EACH VPIAdditionalGTIN WHERE VPIAdditionalGTIN.ItemId = VPIItem.ItemId:
      CREATE bufttPriKat.
      BUFFER-COPY ttPriKat
        EXCEPT EANnr
        TO bufttPriKat
      ASSIGN 
        bufttPriKat.EANnr = VPIAdditionalGTIN.GTIN
        .
    END.
  END. /* ITEM */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksportPriCat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportPriCat Procedure 
PROCEDURE EksportPriCat :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEFINE BUFFER bVPIFilHode FOR VPIFilHode.
/*     OUTPUT TO "C:\home\lindbak\sendes\tradefil.txt". */
/*     FOR EACH ttPriKat:                               */
/*         EXPORT ttprikat.                             */
/*     END.                                             */
/*     OUTPUT CLOSE.                                    */

IF NOT CAN-FIND(FIRST ttPriKat WHERE ttPriKat.ErrFlag = FALSE) THEN 
  RETURN.

  
OUTPUT STREAM UtFil TO VALUE(cPriCatfil).
EKSPORTFIL:                    
FOR EACH ttPriKat
    BREAK BY ttPriKat.LevModellNr
          BY ttPriKat.VareTekst     
          BY ttPriKat.FargeTekst    
          BY ttPriKat.SeqNrStr           
          BY ttPriKat.MarkedsPris:
    IF ttPriKat.ErrFlag THEN NEXT.
          
    piAntLinjer = piAntLinjer + 1.      
          
    PUT STREAM UtFil UNFORMATTED  
    /*  1 */ ttPriKat.R1 ";"           
    /*  2 */ ttPriKat.LevNr ";"        
    /*  3 */ ttPriKat.LevModellNr ";"  
    /*  4 */ ttPriKat.EANnr ";"        
    /*  5 */ ttPriKat.VareTekst ";"     
    /*  6 */ ttPriKat.FargeKode ";"    
    /*  7 */ ttPriKat.FargeTekst ";"   
    /*  8 */ ttPriKat.Str ";"          
    /*  9 */ ttPriKat.StrTab ";"       
    /* 10 */ ttPriKat.Varemerke ";"    
    /* 11 */ ttPriKat.Enh ";"          
    /* 12 */ ttPriKat.AntIEnh ";"      
    /* 13 */ ttPriKat.LevPrisEngros ";"
    /* 14 */ ttPriKat.ValKod ";"       
    /* 15 */ ttPriKat.forhRab% ";"     
    /* 16 */ ttPriKat.suppRab% ";"     
    /* 17 */ ttPriKat.VeilPris ";"     
    /* 18 */ ttPriKat.PAKstru ";"      
    /* 19 */ ttPriKat.LevUke1 ";"       
    /* 20 */ ttPriKat.LevUke2 ";"      
    /* 21 */ ttPriKat.LevUke3 ";"      
    /* 22 */ ttPriKat.LevUke4 ";"      
    /* 23 */ ttPriKat.VareGruppe ";"   
    /* 24 */ ttPriKat.LevNavn ";"      
    /* 25 */ ttPriKat.LevKod ";" 
    /* 26 */ ttPriKat.nettoForh ";"    
    /* 27 */ ttPriKat.kalkForh ";"     
    /* 28 */ ttPriKat.BFforh ";"       
    /* 29 */ ttPriKat.nettoSupp ";"    
    /* 30 */ ttPriKat.kalkSupp ";"     
    /* 31 */ ttPriKat.BFsupp ";"       
    /* 32 */ ttPriKat.MarkedsPris ";"  
    /* 33 */ ttPriKat.Sortiment ";"    
    /* 34 */ ttPriKat.Sesong ";"       
    /* 35 */ ttPriKat.VPIBildeKode ";"
    /* 36 */ ttPriKat.Merknad ";"    
    /* 37 */ ttPriKat.KjedeValutaPris ";" 
    /* 38 */ ttPriKat.KjedeProdusent ";"    
    /* 39 */ ttPriKat.ERPNr ";"          
    /* 40 */ ttPriKat.SalgsEnhetsType ";"
    /* 41 */ ttPriKat.AktivFraDato ";"    
    /* 42 */ ttPriKat.AktivTilDato ";"    
    /* 43 */ ttPriKat.Bongtekst ";"       
    /* 44 */ ttPriKat.Etikettekst1 ";"    
    /* 45 */ ttPriKat.Funksjonskode ";"   
    /* 46 */ ttPriKat.Mva_Proc ";"        
    /* 47 */ ttPriKat.LinkVare ";"         
    /* 48 */ ttPriKat.PantBelop ";"       
    /* 49 */ ttPriKat.Filial ";"          
    /* 50 */ ttPriKat.Produsent ";"       
    /* 51 */ ttPriKat.Mengde ";"          
    /* 52 */ ttPriKat.JamforEnhet ";"     
    /* 53 */ ttPriKat.Kontrolleres ";"
    /* 54 */ ttPriKat.ArtikkelNr ";"
    /* 55 */ ttPriKat.OpprettArtikkel ";" 
    /* 56 */ ttPriKat.PosterPrisending ";" 
    /* 57 */ ";" 
    /* 58 */ ";" 
    /* 59 */ ";" 
    /* 60 */ ";" 
    /* 61 */ ";" 
    /* 62 */ ttPriKat.Karakteristikk ";" 
    /* 63 */ ";" 
    /* 64 */ ";" 
    /* 65 */ ttPriKat.Alder ";" 
    /* 66 */ ";" 
    /* 67 */ ";" 
    /* 68 */ ";" 
    /* 69 */ ";" 
    /* 70 */ ";" 
    /* 71 */ ";" 
    /* 72 */ ";" 
    /* 73 */ ";" 
    /* 74 */ ";" 
    /* 75 */ ";" 
    /* 76 */ ttPriKat.AlfaKode2 ";" 
    /* 77 */ ";" 
    /* 78 */ ";" 
    /* 79 */ ";" 
    /* 80 */ ";" 
    /* 81 */ ";" 
    /* 82 */ ";" 
    /* 83 */ ";" 
    /* 84 */ ";" 
    /* 85 */ ";" 
    /* 86 */ ";" 
    /* 87 */ ";" 
    /* 88 */ ";" 
    /* 89 */ ";" 
    /* 90 */ ttPriKat.Etikett ";"        
    /* 91 */ ";" 
    /* 92 */ ";" 
    /* 93 */ ";" 
    /* 94 */ ";" 
    /* 95 */ ";" 
    /* 96 */ ";" 
    /* 97 */ ttPriKat.BehStatus ";" 
    /* 98 */ ttPriKat.Grunnsortiment ";" 
    /* 99 */ ttPriKat.Opphav ";" 
    /*100 */ ";" 
    /*101 */ ";" 
    /*102 */ ";" 
    /*103 */ ttPriKat.Etikettekst2 ";" 
    /*104 */ ttPriKat.ArtSlag ";" 
    /*105 */ ";" 
    /*106 */ ";" 
    /*107 */ ";" 
    /*108 */ ";" 
    /*109 */ ";" 
    /*110 */ ";" 
    /*111 */ ";" 
    /*112 */ ";" 
    /*113 */ ";" 
    /*114 */ ";" 
    /*115 */ ";" 
    /*116 */ ";" 
    /*117 */ ttPriKat.GarantiKl ";"
    /*118 */ ttPriKat.PostBredde ";" 
    /*119 */ ttPriKat.PostHoyde ";" 
    /*120 */ ttPriKat.PostLengde ";" 
    /*121 */ ttPriKat.PostVekt ";"
    /*122 */ ttPriKat.AntLinkVare
    SKIP.
END. /* ttPriKat EKSPORTFIL */
OUTPUT STREAM UtFil CLOSE.

iAntLinjer = piAntLinjer.

/* Finner FilId */
IF SEARCH(cPriCatfil) <> ? THEN 
DO FOR bVPIFilHode:
    FILE-INFO:FILE-NAME = cPriCatfil.

    FIND LAST bVPIFilHode NO-LOCK NO-ERROR.
    IF AVAILABLE bVPIFilHode THEN
      lbufFilId = bVPIFilHode.FilId + 1.
    ELSE
      lbufFilId = 1.
    DO TRANSACTION:
        CREATE bVPIFilHode.
        ASSIGN
          bVPIFilHode.FilId        = lbufFilId
          bVPIFilHode.FilNavn      = ENTRY(NUM-ENTRIES(cPricatFil,'\'),cPriCatfil,'\')
          bVPIFilHode.Katalog      = RIGHT-TRIM(TRIM(cKatalog),"\")
          bVPIFilHode.Dato         = FILE-INFO:FILE-MOD-DATE
          bVPIFilHode.Kl           = STRING(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS")
          bVPIFilHode.Storrelse    = FILE-INFO:FILE-SIZE
          bVPIFilHode.AntLinjer    = piAntLinjer
          bVPIFilHode.VPIFilType   = 1 /* VPI */
          bVPIFilHode.VPIFilStatus = 1
          bVPIFilHode.EkstVPILevNr = (IF ttPriKat.ButikkNr = iCL THEN iEkstVPILevNr
                                            ELSE IF ttPriKat.ButikkNr = 0 THEN iEkstVPILevNr
                                            ELSE (1000000 + ttPriKat.ButikkNr))
          .
        RELEASE bVPIFilHode.
    END. /* TRANSACTION */
END. /* bVPIFilHode*/

IF lbufFilId > 0 THEN 
DO:
    pbOk = FALSE.    
    IF NOT VALID-HANDLE(h_dvpifilhode) THEN
        RUN dvpifilhode.w PERSISTENT SET h_dvpifilhode.
    
    IF NOT VALID-HANDLE(h_dvpiartbas) THEN
    DO:
        RUN dvpiartbas.w PERSISTENT SET h_dvpiartbas.
        RUN SettAutoImport IN h_dvpiartbas (INPUT TRUE).
    END.
    /* Leser inn filen. */
    RUN LesInnFil IN h_dvpifilhode (INPUT STRING(lbufFilId), 
                               OUTPUT pbOk, 
                               OUTPUT piAntLinjer).
    /* Pakker ut fil. */
    RUN PakkUtFil IN h_dvpifilhode (INPUT STRING(lbufFilId)).
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
  
  IF CAN-FIND(FIRST tt_error) AND bUndertrykkErrLogg = FALSE THEN  
  UTSKRIFT_AV_ERR_Logg:
  DO:
    RUN bibl_logg.p (RIGHT-TRIM(cFilPrefix,'_'), 'xinnTradeItemPrice - ' + "Innlesning av fil").
    IF AVAILABLE VPIFilHode 
      THEN cTekst = "  Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn. 
    ELSE cTekst = "  Ukjent/slettet VPI fil (xinnTradeItemPrice).".    
    RUN bibl_logg.p (RIGHT-TRIM(cFilPrefix,'_'), 'xinnTradeItemPrice - ' + cTekst).

    FOR EACH tt_Error 
      BREAK BY ErrNr:
      RUN bibl_logg.p (RIGHT-TRIM(cFilPrefix,'_'), 'xinnTradeItemPrice - ' + tt_Error.Tekst).
      DELETE tt_Error.
    END.
  END. /* UTSKRIFT_AV_ERR_Logg */
  
  /* Sender eMail hvis det skapes error fil ved import. */
  /*
  IF bSendEMail AND SEARCH(cErrorFil) <> ? THEN
    RUN sendEMail(SEARCH(cErrorFil)).
  */

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
DEFINE VARIABLE iCnt       AS INTEGER  NO-UNDO. 
DEFINE VARIABLE hDocMine   AS HANDLE   NO-UNDO.
DEFINE VARIABLE hRootMine  AS HANDLE   NO-UNDO.
DEFINE VARIABLE hChNode    AS HANDLE   NO-UNDO.
DEFINE VARIABLE hDocNew    AS HANDLE   NO-UNDO. 
DEFINE VARIABLE hNode      AS HANDLE   NO-UNDO.
DEFINE VARIABLE cXML       AS LONGCHAR NO-UNDO. 
DEFINE VARIABLE cNodeList  AS CHAR     NO-UNDO. 

DEFINE VARIABLE iAar AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMan AS INTEGER     NO-UNDO.
DEFINE VARIABLE iDag AS INTEGER     NO-UNDO.

CREATE X-DOCUMENT hDocMine.
CREATE X-NODEREF  hRootMine.
CREATE X-NODEREF  hChNode.


/* ------------------- */

/* ------------------- */

ASSIGN
    cNodeList = "Header,Item".

hDocMine:LOAD("FILE",cFilNavn,FALSE).
hDocMine:GET-DOCUMENT-ELEMENT(hRootMine).

DO iCnt = 1 TO hRootMine:NUM-CHILDREN:
  hRootMine:GET-CHILD(hChNode, iCnt).

  /* Her leses hver node - Header og Item, legges inn i et eget dokument og sendes til hver sin import rutine. */
  IF LOOKUP(hChNode:LOCAL-NAME,cNodeList) <> 0 THEN 
  DO:
      CREATE X-DOCUMENT hDocNew.
      CREATE X-NODEREF  hNode.
      hDocNew:GET-DOCUMENT-ELEMENT(hNode).
      hDocNew:IMPORT-NODE(hNode,hChNode,TRUE).
      hDocnew:APPEND-CHILD(hNode).
      hDocNew:SAVE("LONGCHAR",cXML).
      
      CASE LOOKUP(hChNode:LOCAL-NAME,cNodeList):
          WHEN 1 THEN RUN ProcessHeaderDataSet(cXML). 
          WHEN 2 THEN 
              DO: 
                  RUN ProcessItemDataSet(cXML).
              END. 
      END CASE.
      
      DELETE OBJECT hDocNew. 
      DELETE OBJECT hNode. 
  END.
END. 

DELETE OBJECT hDocMine.
DELETE OBJECT hRootMine.
DELETE OBJECT hChNode. 

/* INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO. */
/* REPEAT:                                           */
/*     IMPORT STREAM InnFil UNFORMATTED cLinje.      */
/*     /*                                            */
/*     MESSAGE cLinje                                */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.        */
/*     */                                            */
/* END.                                              */
/*                                                   */
/* INPUT STREAM InnFil CLOSE.                        */

cRet_val = "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-opprettEkstVPILev) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprettEkstVPILev Procedure 
PROCEDURE opprettEkstVPILev :
/*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER piEkstVPILevNr LIKE EkstVPILev.EkstVPILevNr NO-UNDO.
  
    RUN oppdaterEkstVPILev.p (piEkstVPILevNr, cEDB-System, iLevNr, VPIFilHode.Katalog).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettKarakteristikk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettKarakteristikk Procedure 
PROCEDURE OpprettKarakteristikk :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  FOR EACH VPICharacteristic
    BREAK BY VPICharacteristic.ItemId
          BY VPICharacteristic.Id:
    IF FIRST-OF(VPICharacteristic.Id) THEN 
    DO:
        IF NOT CAN-FIND(Karakteristikk WHERE  
                        Karakteristikk.KarakteristikkId = VPICharacteristic.Id) THEN 
          DO:
            CREATE Karakteristikk.
            ASSIGN
              Karakteristikk.KarakteristikkId = VPICharacteristic.Id
              Karakteristikk.KBeskrivelse = VPICharacteristic.Description
              .  
            RELEASE Karakteristikk.                
          END.
    END.          
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProcessHeaderDataSet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessHeaderDataSet Procedure 
PROCEDURE ProcessHeaderDataSet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcXml AS LONGCHAR NO-UNDO. 
    
    DEFINE VARIABLE iLoop1        AS INTEGER  NO-UNDO. 
    DEFINE VARIABLE hDoc1         AS HANDLE   NO-UNDO.
    DEFINE VARIABLE hN1           AS HANDLE   NO-UNDO.
    DEFINE VARIABLE hN1Field      AS HANDLE   NO-UNDO.
    DEFINE VARIABLE hN1FieldValue AS HANDLE   NO-UNDO.
    DEFINE VARIABLE hDocNew       AS HANDLE   NO-UNDO. 
    DEFINE VARIABLE hNode         AS HANDLE   NO-UNDO.
    DEFINE VARIABLE cXML          AS LONGCHAR NO-UNDO. 
    DEFINE VARIABLE cFieldLst1    AS CHAR     NO-UNDO.

    CREATE X-DOCUMENT hDoc1.
    CREATE X-NODEREF  hN1.
    CREATE X-NODEREF  hN1Field.
    CREATE X-NODEREF  hN1FieldValue.

    DEF VAR lOk AS LOG NO-UNDO.

    ASSIGN
        cFieldLst1 = 'TimeStamp,SellerGLN,BrokerGLN,CustomerGLN,CustomerID'.

    /* Bare for Header */
    EMPTY TEMP-TABLE VPIHeader.

    CREATE VPIHeader.

    hDoc1:LOAD("LONGCHAR",ipcXML,FALSE).
    hDoc1:GET-DOCUMENT-ELEMENT(hN1).

    DO iLoop1 = 1 TO hN1:NUM-CHILDREN:
      lOk = hN1:GET-CHILD(hN1Field, iLoop1) NO-ERROR.
      IF NOT lOk THEN NEXT.

      IF hN1Field:NUM-CHILDREN = 1 THEN
      NIVA1:
      DO:
          IF CAN-DO(cFieldLst1,hN1Field:NAME) THEN 
          DO:
            lOK = hN1Field:GET-CHILD (hN1FieldValue,1) NO-ERROR.
            IF lOk THEN
            DO:                
                CASE LOOKUP(hN1Field:NAME,cFieldLst1):
                    WHEN 1 THEN VPIHeader.TimeStamp   = hN1FieldValue:NODE-VALUE.
                    WHEN 2 THEN VPIHeader.SellerGLN   = hN1FieldValue:NODE-VALUE.
                    WHEN 3 THEN VPIHeader.BrokerGLN   = hN1FieldValue:NODE-VALUE.
                    WHEN 4 THEN VPIHeader.CustomerGLN = hN1FieldValue:NODE-VALUE.
                    WHEN 5 THEN VPIHeader.CustomerID  = hN1FieldValue:NODE-VALUE.
                END CASE.
            END.
          END.
      END. /* NIVA1 */
    END. 
    /*
    MESSAGE 
        VPIHeader.TimeStamp   SKIP 
        VPIHeader.SellerGLN   SKIP
        VPIHeader.BrokerGLN   SKIP 
        VPIHeader.CustomerGLN SKIP 
        VPIHeader.CustomerID
    VIEW-AS ALERT-BOX.
    */
    DELETE OBJECT hDoc1.
    DELETE OBJECT hN1.
    DELETE OBJECT hN1Field.
    DELETE OBJECT hN1FieldValue. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProcessItemDataSet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessItemDataSet Procedure 
PROCEDURE ProcessItemDataSet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcXml AS LONGCHAR NO-UNDO. 
    
    DEFINE VARIABLE iLoop1        AS INTEGER  NO-UNDO. 
    DEFINE VARIABLE iLoop2        AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iLoop3        AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iLoop4        AS INTEGER  NO-UNDO.
    DEFINE VARIABLE hDoc1         AS HANDLE   NO-UNDO.
    DEFINE VARIABLE hN1           AS HANDLE   NO-UNDO.
    DEFINE VARIABLE hN1Field      AS HANDLE   NO-UNDO.
    DEFINE VARIABLE hN1FieldValue AS HANDLE   NO-UNDO.
    DEFINE VARIABLE hN2Field      AS HANDLE   NO-UNDO.
    DEFINE VARIABLE hN2FieldValue AS HANDLE   NO-UNDO.
    DEFINE VARIABLE hN3Field      AS HANDLE   NO-UNDO.
    DEFINE VARIABLE hN3FieldValue AS HANDLE   NO-UNDO.
    DEFINE VARIABLE hDocNew       AS HANDLE   NO-UNDO. 
    DEFINE VARIABLE hNode         AS HANDLE   NO-UNDO.
    DEFINE VARIABLE cXML          AS LONGCHAR NO-UNDO. 

    DEFINE BUFFER bufVPIItem FOR VPIItem.

    CREATE X-DOCUMENT hDoc1.
    CREATE X-NODEREF  hN1.
    CREATE X-NODEREF  hN1Field.
    CREATE X-NODEREF  hN1FieldValue.
    CREATE X-NODEREF  hN2Field.
    CREATE X-NODEREF  hN2FieldValue.
    CREATE X-NODEREF  hN3Field.
    CREATE X-NODEREF  hN3FieldValue.

    RUN SetFieldLsts.
    CREATE VPIItem. 
    hDoc1:LOAD("LONGCHAR",ipcXML,FALSE).
    hDoc1:GET-DOCUMENT-ELEMENT(hN1).

    DO iLoop1 = 1 TO hN1:NUM-CHILDREN:
      lOk = hN1:GET-CHILD(hN1Field, iLoop1) NO-ERROR.
      IF NOT lOk THEN NEXT.

      IF hN1Field:NUM-CHILDREN = 1 THEN
      NIVA1:
      DO:
          IF CAN-DO(cFieldLst1,hN1Field:NAME) THEN 
          DO:
            lOK = hN1Field:GET-CHILD (hN1FieldValue,1) NO-ERROR.
            IF lOk THEN
            DO:                
                CASE LOOKUP(hN1Field:NAME,cFieldLst1):
                    WHEN  1 THEN VPIItem.itemID                 = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN  2 THEN VPIItem.ArticleID              = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN  3 THEN VPIItem.Description            = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN  4 THEN VPIItem.Brand                  = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN  5 THEN VPIItem.DescriptionShort       = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN  6 THEN VPIItem.WeightVolume           = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN  7 THEN VPIItem.PackageTypeCode        = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN  8 THEN VPIItem.PackageTypeDescription = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN  9 THEN VPIItem.Label1Name             = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN 10 THEN VPIItem.Label1Text1            = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN 11 THEN VPIItem.Label1Text2            = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN 12 THEN VPIItem.Label2Name             = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN 13 THEN VPIItem.LabelPriceCompareUnit  = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN 14 THEN VPIItem.ReplacedItemID         = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN 15 THEN VPIItem.UOM                    = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN 16 THEN VPIItem.SeasonCategory         = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN 17 THEN VPIItem.HierarchyLevel         = INT(hN1FieldValue:NODE-VALUE).
                    WHEN 18 THEN VPIItem.ArticleHierarchy       = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN 19 THEN VPIItem.StartAvailable         = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN 20 THEN VPIItem.EndAvailability        = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN 21 THEN VPIItem.Warranty               = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN 22 THEN VPIItem.WeightItem             = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN 23 THEN VPIItem.NetContent             = DEC(REPLACE(STRING(hN1FieldValue:NODE-VALUE),'.',',')).
                    WHEN 24 THEN VPIItem.NetContentUnit         = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN 25 THEN VPIItem.ComparePriceFactor     = DEC(REPLACE(STRING(hN1FieldValue:NODE-VALUE),'.',',')).
                    WHEN 26 THEN VPIItem.ExposureCode           = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN 27 THEN VPIItem.CountryOfOrigin        = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN 28 THEN VPIItem.GrossWeight            = DEC(REPLACE(STRING(hN1FieldValue:NODE-VALUE),'.',',')).
                    WHEN 29 THEN VPIItem.AgeLimit               = INT(hN1FieldValue:NODE-VALUE).
                    WHEN 30 THEN VPIItem.NetWeight              = DEC(REPLACE(STRING(hN1FieldValue:NODE-VALUE),'.',',')).
                    WHEN 31 THEN VPIItem.WeightUnit             = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN 32 THEN VPIItem.Volume                 = DEC(REPLACE(STRING(hN1FieldValue:NODE-VALUE),'.',',')).
                    WHEN 33 THEN VPIItem.VolumeUnit             = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN 34 THEN VPIItem.Length                 = DEC(REPLACE(STRING(hN1FieldValue:NODE-VALUE),'.',',')).
                    WHEN 35 THEN VPIItem.Width                  = DEC(REPLACE(STRING(hN1FieldValue:NODE-VALUE),'.',',')).
                    WHEN 36 THEN VPIItem.Height                 = DEC(REPLACE(STRING(hN1FieldValue:NODE-VALUE),'.',',')).
                    WHEN 37 THEN VPIItem.MeasurementUnit        = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN 38 THEN VPIItem.TaxCode                = STRING(hN1FieldValue:NODE-VALUE).
                    WHEN 39 THEN VPIItem.TaxRate                = DEC(REPLACE(STRING(hN1FieldValue:NODE-VALUE),'.',',')).
                END CASE.
            END.
          END.
      END. /* NIVA1 */
      ELSE IF hN1Field:NUM-CHILDREN > 1 THEN
      BARNEBARN:
      DO:
          /* Sjekker om artikkelen har vært lest inn en gang før i samme fil. Er den det */
          /* skal den først innleste artikkelen slettes.                                 */
          IF CAN-FIND(FIRST bufVPIItem WHERE 
                            bufVPIItem.ItemId = VPIItem.ItemId AND 
                            RECID(bufVPIItem) <> RECID(VPIItem)) THEN 
            DO:
                FIND FIRST bufVPIItem WHERE 
                           bufVPIItem.ItemId = VPIItem.ItemId AND
                           RECID(bufVPIItem) <> RECID(VPIItem) NO-ERROR.
                IF AVAILABLE bufVPIItem THEN 
                DO:
                    CREATE tt_Error.
                    ASSIGN
                      /*tt_Error.LinjeNr   = ttPriKat.LinjeNr*/
                      tt_Error.ErrNr     = 1
                      /*tt_Error.ButikkNr  = ttPriKat.ButikkNr*/
                      tt_Error.Tekst     = "* GTIN (" + bufVPIItem.ItemId + " " + VPIItem.Description + ") kommer flere ganger i filen. Kun siste forekomst er importert. ".
                    
                    FOR EACH VPISalesRestriction WHERE
                        VPISalesRestriction.ItemId = bufVPIItem.ItemId:
                        DELETE VPISalesRestriction.
                    END.
                    FOR EACH VPICharacteristic WHERE
                        VPICharacteristic.ItemId = bufVPIItem.ItemId:
                        DELETE VPICharacteristic.
                    END.
                    FOR EACH VPIDepositItem WHERE
                        VPIDepositItem.ItemId = bufVPIItem.ItemId:
                        DELETE VPIDepositItem.
                    END.
                    FOR EACH VPISupplierItem WHERE
                        VPISupplierItem.ItemId = bufVPIItem.ItemId:
                        DELETE VPISupplierItem.
                    END.
                    FOR EACH VPICostPrice WHERE
                        VPICostPrice.ItemId = bufVPIItem.ItemId:
                        DELETE VPICostPrice.
                    END.
                    FOR EACH VPIProperty WHERE
                        VPIProperty.ItemId = bufVPIItem.ItemId:
                        DELETE VPIProperty.
                    END.
                    FOR EACH VPIRetailPrice WHERE
                        VPIRetailPrice.ItemId = bufVPIItem.ItemId:
                        DELETE VPIRetailPrice.
                    END.
                    FOR EACH VPIAdditionalGTIN WHERE
                        VPIAdditionalGTIN.ItemId = bufVPIItem.ItemId:
                        DELETE VPIAdditionalGTIN.
                    END.
                    DELETE bufVPIItem.
                END. 
            END.
              
          IF LOOKUP(hN1Field:NAME,cFieldLst2)> 0 THEN 
          DO:
              CASE LOOKUP(hN1Field:NAME,cFieldLst2):
                  WHEN 1 THEN /* SalesRestriction */
                  DO:
                      CREATE VPISalesRestriction.
                      ASSIGN
                          VPISalesRestriction.ItemId = VPIItem.ItemId.
                      DO iLoop2 = 1 TO hN1Field:NUM-CHILDREN:
                          lOk = hN1Field:GET-CHILD(hN2Field, iLoop2) NO-ERROR.
                          IF NOT lOk THEN NEXT.
                          IF hN2Field:NUM-CHILDREN = 1 THEN
                          NIVA2:
                          DO:
                              lOK = hN2Field:GET-CHILD (hN2FieldValue,1) NO-ERROR.
                              IF lOk THEN
                              DO:
                                  CASE LOOKUP(hN2Field:NAME,cFieldLst3):
                                      WHEN  1 THEN VPISalesRestriction.ID          = STRING(hN2FieldValue:NODE-VALUE).
                                      WHEN  2 THEN VPISalesRestriction.Description = STRING(hN2FieldValue:NODE-VALUE).
                                  END CASE.
                              END.
                          END. /* NIVA2 */
                      END.
                      IF VPISalesRestriction.ID = ? THEN DELETE VPISalesRestriction. 
                  END.

                  WHEN 2 THEN  /* Characteristic */
                  DO:
                      CREATE VPICharacteristic.
                      ASSIGN
                          VPICharacteristic.ItemId = VPIItem.ItemId.
                      DO iLoop2 = 1 TO hN1Field:NUM-CHILDREN:
                          lOk = hN1Field:GET-CHILD(hN2Field, iLoop2) NO-ERROR.
                          IF NOT lOk THEN NEXT.
                          IF hN2Field:NUM-CHILDREN = 1 THEN
                          NIVA2:
                          DO:
                              lOK = hN2Field:GET-CHILD (hN2FieldValue,1) NO-ERROR.
                              IF lOk THEN
                              DO:
                                  CASE LOOKUP(hN2Field:NAME,cFieldLst3):
                                      WHEN  1 THEN VPICharacteristic.ID          = STRING(hN2FieldValue:NODE-VALUE).
                                      WHEN  2 THEN VPICharacteristic.Description = STRING(hN2FieldValue:NODE-VALUE).
                                  END CASE.
                              END.
                          END. /* NIVA2 */
                      END.
                      IF VPICharacteristic.ID = ? THEN DELETE VPICharacteristic. 
                  END.

                  WHEN 3 THEN /* DepositItem */
                  DO:
                      CREATE VPIDepositItem.
                      ASSIGN
                          VPIDepositItem.ItemId = VPIItem.ItemId.
                      DO iLoop2 = 1 TO hN1Field:NUM-CHILDREN:
                          lOk = hN1Field:GET-CHILD(hN2Field, iLoop2) NO-ERROR.
                          IF NOT lOk THEN NEXT.
                          IF hN2Field:NUM-CHILDREN = 1 THEN
                          NIVA2:
                          DO:
                              lOK = hN2Field:GET-CHILD (hN2FieldValue,1) NO-ERROR.
                              IF lOk THEN
                              DO:
                                  CASE LOOKUP(hN2Field:NAME,cFieldLst4):
                                      WHEN  1 THEN VPIDepositItem.DepItemId   = STRING(hN2FieldValue:NODE-VALUE).
                                      WHEN  2 THEN VPIDepositItem.Description = STRING(hN2FieldValue:NODE-VALUE).
                                      WHEN  3 THEN VPIDepositItem.Price       = DEC(REPLACE(STRING(hN2FieldValue:NODE-VALUE),'.',',')).
                                      WHEN  4 THEN VPIDepositItem.Quantity    = INT(hN2FieldValue:NODE-VALUE).
                                  END CASE.
                              END.
                          END. /* NIVA2 */
                      END.
                      IF VPIDepositItem.DepItemId = ? THEN DELETE VPIDepositItem. 
                  END.

                  WHEN 4 THEN /* SupplierItem */
                  DO:
                      CREATE VPISupplierItem.
                      ASSIGN
                          VPISupplierItem.ItemId = VPIItem.ItemId.
                      DO iLoop2 = 1 TO hN1Field:NUM-CHILDREN:
                          lOk = hN1Field:GET-CHILD(hN2Field, iLoop2) NO-ERROR.
                          IF NOT lOk THEN NEXT.
                          IF hN2Field:NUM-CHILDREN = 1 THEN
                          NIVA2:
                          DO:
                              lOK = hN2Field:GET-CHILD (hN2FieldValue,1) NO-ERROR.
                              IF lOk THEN
                              DO:
                                  CASE LOOKUP(hN2Field:NAME,cFieldLst5):
                                      WHEN  1 THEN VPISupplierItem.SupplierID     = STRING(hN2FieldValue:NODE-VALUE).
                                      WHEN  2 THEN VPISupplierItem.SupplierItemID = STRING(hN2FieldValue:NODE-VALUE).
                                      WHEN  3 THEN VPISupplierItem.SupplierName   = STRING(hN2FieldValue:NODE-VALUE).
                                      WHEN  4 THEN VPISupplierItem.GTIN           = STRING(hN2FieldValue:NODE-VALUE).
                                      WHEN  5 THEN VPISupplierItem.UOM            = STRING(hN2FieldValue:NODE-VALUE).
                                      WHEN  6 THEN VPISupplierItem.Preffered      = STRING(hN2FieldValue:NODE-VALUE).
                                      WHEN  7 THEN VPISupplierItem.ExpirationDate = STRING(hN2FieldValue:NODE-VALUE).
                                      WHEN  8 THEN VPISupplierItem.RemovalFlag    = STRING(hN2FieldValue:NODE-VALUE).
                                      WHEN  9 THEN VPISupplierItem.StockItemCount = INT(hN2FieldValue:NODE-VALUE).
                                      WHEN 10 THEN VPISupplierItem.PalletType     = STRING(hN2FieldValue:NODE-VALUE).
                                      WHEN 11 THEN VPISupplierItem.PalletSize     = DEC(REPLACE(STRING(hN2FieldValue:NODE-VALUE),'.',',')).
                                  END CASE.
                              END.
                          END. /* NIVA2 */
                          ELSE DO: /* Property */
                              DO iLoop3 = 1 TO hN2Field:NUM-CHILDREN:
                                  lOk = hN2Field:GET-CHILD(hN3Field, iLoop3) NO-ERROR.
                                  IF NOT lOk THEN NEXT.
                                  CASE LOOKUP(hN3Field:NAME,cFieldLst9):
                                      WHEN  1 THEN 
                                      DO:
                                          CREATE VPIProperty.
                                          ASSIGN                                              
                                              VPIProperty.ItemId         = VPIItem.ItemId
                                              VPIProperty.SupplierID     = VPISupplierItem.SupplierID
                                              VPIProperty.SupplierItemID = VPISupplierItem.SupplierItemID
                                              VPIProperty.GTIN           = VPISupplierItem.GTIN
                                          lOk = hN3Field:GET-CHILD(hN3FieldValue, 1) NO-ERROR.
                                          IF lOk THEN VPIProperty.PropValue = STRING(hN3FieldValue:NODE-VALUE).
                                          IF VPIProperty.PropValue = ? THEN DELETE VPIProperty. 
                                      END.
                                  END CASE.
                              END.
                          END.
                      END.
                      IF VPISupplierItem.SupplierID = ? THEN DELETE VPISupplierItem. 
                  END.

                  WHEN 5 THEN /* Price */
                  DO:
                      DO iLoop2 = 1 TO hN1Field:NUM-CHILDREN: /* Denne har CostPrice og RetailPrice under seg */
                          lOk = hN1Field:GET-CHILD(hN2Field, iLoop2) NO-ERROR.
                          IF NOT lOk THEN NEXT.
                          IF hN2Field:NUM-CHILDREN > 1 THEN
                          NIVA2:
                          DO:
                              CASE LOOKUP(hN2Field:NAME,cFieldLst7):
                                  WHEN 1 THEN /* CostPrice */
                                  DO:
                                      CREATE VPICostPrice.
                                      ASSIGN
                                          VPICostPrice.ItemId = VPIItem.ItemId.
                                      DO iLoop3 = 1 TO hN2Field:NUM-CHILDREN:
                                          lOk = hN2Field:GET-CHILD(hN3Field, iLoop3) NO-ERROR.
                                          IF NOT lOk THEN NEXT.
                                         lOK = hN3Field:GET-CHILD (hN3FieldValue,1) NO-ERROR.
                                         IF lOk THEN
                                         DO:
                                             CASE LOOKUP(hN3Field:NAME,cFieldLst8):
                                                 WHEN  1 THEN VPICostPrice.StartDate = STRING(hN3FieldValue:NODE-VALUE).
                                                 WHEN  2 THEN VPICostPrice.Amount    = DEC(REPLACE(STRING(hN3FieldValue:NODE-VALUE),'.',',')).
                                             END CASE.
                                         END.
                                      END.
                                      IF VPICostPrice.StartDate = ? THEN DELETE VPICostPrice.
                                  END.
                                  WHEN 2 THEN /* RetailPrice */
                                  DO:
                                      CREATE VPIRetailPrice.
                                      ASSIGN
                                          VPIRetailPrice.ItemId = VPIItem.ItemId.
                                      DO iLoop3 = 1 TO hN2Field:NUM-CHILDREN:
                                          lOk = hN2Field:GET-CHILD(hN3Field, iLoop3) NO-ERROR.
                                          IF NOT lOk THEN NEXT.
                                         lOK = hN3Field:GET-CHILD (hN3FieldValue,1) NO-ERROR.
                                         IF lOk THEN
                                         DO:
                                             CASE LOOKUP(hN3Field:NAME,cFieldLst8):
                                                 WHEN  1 THEN VPIRetailPrice.StartDate = STRING(hN3FieldValue:NODE-VALUE).
                                                 WHEN  2 THEN VPIRetailPrice.Amount    = DEC(REPLACE(STRING(hN3FieldValue:NODE-VALUE),'.',',')).
                                             END CASE.
                                         END.
                                      END.
                                      IF VPIRetailPrice.StartDate = ? THEN DELETE VPIRetailPrice.
                                  END.
                              END CASE.
                          END. /* NIVA2 */
                      END.
                  END.

                  WHEN 6 THEN /* AdditionalGTIN - Her kan det komme flere GTIN under sammen node ????*/
                  DO:
                      DO iLoop2 = 1 TO hN1Field:NUM-CHILDREN:
                          lOk = hN1Field:GET-CHILD(hN2Field, iLoop2) NO-ERROR.
                          IF NOT lOk THEN NEXT.

                          IF hN2Field:NUM-CHILDREN = 1 THEN
                          NIVA2:
                          DO:
                              lOK = hN2Field:GET-CHILD (hN2FieldValue,1) NO-ERROR.
                              IF lOk THEN
                              DO:
                                  CASE LOOKUP(hN2Field:NAME,cFieldLst6):
                                      WHEN  1 THEN 
                                      DO:
                                          CREATE VPIAdditionalGTIN.
                                          ASSIGN
                                              VPIAdditionalGTIN.ItemId = VPIItem.ItemId
                                              VPIAdditionalGTIN.GTIN   = STRING(hN2FieldValue:NODE-VALUE) NO-ERROR.
                                          IF ERROR-STATUS:ERROR THEN
                                              IF AVAILABLE VPIAdditionalGTIN THEN DELETE VPIAdditionalGTIN.

                                          IF AVAILABLE VPIAdditionalGTIN AND VPIAdditionalGTIN.GTIN = ? THEN DELETE VPIAdditionalGTIN. 
                                      END.
                                  END CASE.
                              END.
                          END. /* NIVA2 */
                      END.
                  END.
              END CASE.
          END.

      END. /* BARNEBARN */
    END. 

    DELETE OBJECT hDoc1.
    DELETE OBJECT hN1.
    DELETE OBJECT hN1Field.
    DELETE OBJECT hN1FieldValue. 
    DELETE OBJECT hN2Field.
    DELETE OBJECT hN2FieldValue. 
    DELETE OBJECT hN3Field.
    DELETE OBJECT hN3FieldValue. 

    IF VPIItem.itemID = ? THEN DELETE VPIItem.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetFieldLsts) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetFieldLsts Procedure 
PROCEDURE SetFieldLsts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
        cFieldLst1 = 'itemID,' +                    
                     'ArticleID,' +                 
                     'Description,' +               
                     'Brand,' +                     
                     'DescriptionShort,' +          
                     'WeightVolume,' +              
                     'PackageTypeCode,' +           
                     'PackageTypeDescription,' +    
                     'Label1Name,' +                
                     'Label1Text1,' +               
                     'Label1Text2,' +               
                     'Label2Name,' +                
                     'LabelPriceCompareUnit,' +     
                     'ReplacedItemID,' +            
                     'UOM,' +                       
                     'SeasonCategory,' +            
                     'HierarchyLevel,' +            
                     'ArticleHierarchy,' +          
                     'StartAvailable,' +            
                     'EndAvailability,' +           
                     'Warranty,' +                  
                     'WeightItem,' +                
                     'NetContent,' +                
                     'NetContentUnit,' +            
                     'ComparePriceFactor,' +        
                     'ExposureCode,' +              
                     'CountryOfOrigin,' +           
                     'GrossWeight,' +               
                     'AgeLimit,' +                  
                     'NetWeight,' +                 
                     'WeightUnit,' +                
                     'Volume,' +                    
                     'VolumeUnit,' +                
                     'Length,' +                    
                     'Width,' +                     
                     'Height,' +                    
                     'MeasurementUnit,' +           
                     'TaxCode,' +                   
                     'TaxRate'                      
        cFieldLst2 = 'SalesRestriction,Characteristic,DepositItem,SupplierItem,Price,AdditionalGTIN'
        cFieldLst3 = 'ID,Description'
        cFieldLst4 = 'ItemId,Description,Price,Quantity'
        cFieldLst5 = 'SupplierID,SupplierItemID,SupplierName,GTIN,UOM,Preffered,ExpirationDate,RemovalFlag,StockItemCount,PalletType,PalletSize'     
        cFieldLst6 = 'GTIN'
        cFieldLst7 = 'CostPrice,RetailPrice'
        cFieldLst8 = 'StartDate,Amount'
        cFieldLst9 = 'Value'
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getDateDmy) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDateDmy Procedure 
FUNCTION getDateDmy RETURNS CHARACTER
        (cDato AS CHARACTER):
        /*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDatoOut AS CHARACTER NO-UNDO.
    
    IF NUM-ENTRIES(cDato,'-') = 3 THEN 
      ASSIGN
        cDatoOut = ENTRY(3,cDato,'-') + '/' + ENTRY(2,cDato,'-') + '/' + ENTRY(1,cDato,'-').  
        
                RETURN cDatoOut.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VareIdent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION VareIdent Procedure 
FUNCTION VareIdent RETURNS CHARACTER
        (  ):
        /*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

        DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

        cTekst = " EkstVpiLev: " + STRING(ttPriKat.EkstVPILevNr,"zzzzzz9") + " Linje: " + STRING(ttPriKat.LinjeNr,">>zzzzzz9") + ' '  
                 + (IF LENGTH(ttPriKat.EANnr)       < 13 THEN FILL(' ',13 - LENGTH(ttPriKat.EANnr))       ELSE '') + ttPriKat.EANnr       + ' ' 
                 + (IF LENGTH(ttPriKat.LevModellNr) < 10 THEN FILL(' ',10 - LENGTH(ttPriKat.LevModellNr)) ELSE '') + ttPriKat.LevModellNr + ' ' 
                 + (IF LENGTH(ttPriKat.LevKod)      < 10 THEN FILL(' ',10 - LENGTH(ttPriKat.LevKod))      ELSE '') + ttPriKat.LevKod      + ' ' 
                 + (IF LENGTH(ttPriKat.Varetekst)   < 30 THEN FILL(' ',30 - LENGTH(ttPriKat.Varetekst))   ELSE '') + ttPriKat.Varetekst   + ': ' 
                 . 

        RETURN cTekst.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VareItemIdent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION VareItemIdent Procedure 
FUNCTION VareItemIdent RETURNS CHARACTER
        (  ):
        /*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

        DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

        
        cTekst = " EkstVpiLev: " + STRING(iEkstVPILevNr,"zzzzzz9") + " Linje: " + STRING(piLinjeNr,">>zzzzzz9") + ' '  
                 + (
                    IF LENGTH(STRING(VPIItem.ItemID)) < 13 
                      THEN FILL(' ',13 - LENGTH(STRING(VPIItem.ItemID))) 
                      ELSE ''
                    ) 
                    + STRING(VPIItem.ItemID)  + ' '
                 + (IF LENGTH(STRING(VPISupplierItem.SupplierItemID)) < 10 THEN FILL(' ',10 - LENGTH(STRING(VPISupplierItem.SupplierItemID))) ELSE '') + STRING(VPISupplierItem.SupplierItemID) + ' ' 
                 + '           '   
                 + (
                    IF LENGTH(VPIItem.Description) < 30 
                      THEN FILL(' ',30 - LENGTH(VPIItem.Description))   
                      ELSE ''
                    ) 
                    + VPIItem.Description
                  .

        RETURN cTekst.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

