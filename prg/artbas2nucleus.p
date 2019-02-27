&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
/* Connected Databases 
          data             PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_ELogg NO-UNDO LIKE ELogg.



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
DEFINE INPUT  PARAMETER hTT_N9Butiker AS HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER cFtpButiker   AS CHARACTER  NO-UNDO. /* Blank eller "PROFIL" */
DEFINE INPUT  PARAMETER cOutputDir    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER dKampId       AS DECIMAL    NO-UNDO. /* Specialingång vid kampanj */
DEFINE INPUT  PARAMETER lSendArtiklar AS LOGICAL    NO-UNDO.  /* Om KampId > 0 och denna ær TRUE, Så skall vi sænda de artiklar som berørs av kampanjen */
                                                              /* Først anropas programmet før att lægga ut artiklar och sedan før sjælva kampanjen */ 
DEFINE INPUT  PARAMETER cEDBSystem    AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cVareFiler      AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cMixFiler       AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntVarer       AS INTEGER    NO-UNDO.
DEFINE OUTPUT PARAMETER iAntPakkeLinjer AS INTEGER    NO-UNDO.

DEFINE VARIABLE hButikker AS HANDLE     NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
DEFINE VARIABLE cExportVareFil    AS CHARACTER INIT "c:\home\lindbak\kasse\vare." NO-UNDO.
DEFINE VARIABLE cExportMixFil     AS CHARACTER INIT "c:\home\lindbak\kasse\mix." NO-UNDO.
DEFINE VARIABLE dMaxPris          AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cBonus            AS CHARACTER  NO-UNDO. /* Hämtas för tillfället från syspara */
DEFINE VARIABLE lBonus            AS LOGICAL    NO-UNDO. 
DEFINE VARIABLE cFeilFil AS CHARACTER  INIT "FeilVareEksport.txt" NO-UNDO.
DEFINE VARIABLE cDateString AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTimeString AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDateTid AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cLogSeqFil AS CHARACTER  NO-UNDO. /* tillfällig logfil av seqnr när vi har en kamapanj */  

DEFINE VARIABLE iCampaignOwnerId   AS INTEGER    NO-UNDO.
DEFINE VARIABLE cCampaignOwnerName AS CHARACTER  NO-UNDO.

DEFINE VARIABLE iCLbutik    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCLprofilnr AS INTEGER     NO-UNDO.
DEFINE STREAM xmlfile.
DEFINE BUFFER   bTT_Elogg FOR TT_Elogg.

DEFINE TEMP-TABLE TT_Master NO-UNDO
    FIELD butik AS INTE
    FIELD verdier AS CHAR
    INDEX ba IS PRIMARY butik verdier.
DEFINE TEMP-TABLE TT_MasterButik NO-UNDO
    FIELD butik AS INTE
    FIELD profilnr AS INTE
    INDEX bp IS PRIMARY UNIQUE butik.

/* DEFINE TEMP-TABLE TT_ArtTilEksport NO-UNDO    */
/*     FIELD ArtikkelNr AS DECIMAL               */
/*     FIELD Slette     AS LOGICAL               */
/*     FIELD Strekkoder AS CHAR                  */
/*     INDEX Artnr IS PRIMARY Artikkelnr Slette. */

DEFINE TEMP-TABLE TT_ArtPris NO-UNDO
    FIELD ArtikkelNr AS DECIMAL
    FIELD Profilnr   AS INTE
    FIELD Tilbud     AS LOGICAL
    FIELD TilbudFra  AS DATE
    FIELD TilbudFraTid  AS INTE
    FIELD TilbudTil  AS DATE
    FIELD TilbudTilTid  AS INTE
    FIELD NPris      AS DECI
    FIELD TPris      AS DECI
    INDEX Artnr IS PRIMARY UNIQUE Artikkelnr profilnr.


DEFINE TEMP-TABLE TT_Item NO-UNDO
    FIELD itemId              AS DECIMAL   /* Item-attribute */
    FIELD action              AS INTEGER      /* Item-attribute skall vara update/delete i xml vi använder 0=delete,1=update*/
    FIELD Name                AS CHARACTER /* x(40) */
    FIELD itemType            AS CHAR /* sku,nonsku,weight */
    FIELD AllowPriceOverride  AS LOGICAL /* true, false lägg bara ut vid true */
    FIELD IsDeposit           AS LOGICAL /* true if ex. deposit */
    FIELD MDallowed           AS LOGICAL /* ManualDiscount-attribute @allowed */
    FIELD MDdiscountOverride  AS LOGICAL /* ManualDiscount-attribute @discountOverride true if chashier can override recommended discount */
    FIELD MDunitDiscount      AS DECI /* ManualDiscount-attribute @unitDiscount, default discount per unit*/
    FIELD MDpercentage        AS DECI /* ManualDiscount-attribute @allowed, default discountpercentage */
    FIELD SCuom               AS CHAR /* SalesContent-attribute @uom="cl">33</SalesContent>*/
    FIELD SRCuom              AS CHAR /* SalesReferenceContent-attribute @uom="l">1</SalesReferenceContent>  */
    FIELD ProductOwnerId      AS INTEGER /* Reference to the Id-field of a PriceBook-ProductOwner */
    FIELD AGsalesType         AS CHAR    /* AccountGroup-attribute @salesType takeaway,eatin */
    FIELD AGaction            AS CHAR    /* AccountGroup-attribute @action */
    FIELD AccountId           AS INTE
    FIELD TaxClass            AS INTE
    FIELD EnterpriseId        AS INTE
    FIELD UserDefinedId       AS INTE
    FIELD EFTCodeId           AS INTE
    FIELD OPris               AS LOG
    FIELD itemidExport        AS DECIMAL
    FIELD depositlink         AS DECI    
    /* MainPrice i egen tabell - profilhantering */
/*     FIELD MPstartDate         AS DATE  /* MainPrice-attribute @startDate */   */
/*     FIELD MPstartTime         AS DATE  /* MainPrice-attribute @startTime */   */
/*     FIELD MPaction            AS DATE  /* MainPrice-attribute @startAction */ */
/*     FIELD UnitPrice           AS DECI  /* The unit price of the product */    */
    
    /* BarCode - egen tabell */
/*     FIELD BCbarCode           AS DECI  /* BarCode-attribute @barCode */ */
/*     FIELD BCaction            AS DECI  /* BarCode-attribute @action */  */

/*                                                  */
/*     FIELD ProductType               AS INTEGER   */
/*     FIELD ArticleType               AS INTEGER   */
/*     FIELD Price                     AS DECIMAL   */
/*     FIELD FuelNumber                AS INTEGER   */
/*     FIELD TaxClass                  AS INTEGER   */
/*     FIELD LastChanged               AS CHARACTER */
/*     FIELD ProductLinkPrintAll       AS INTEGER   */
/*     FIELD ProductLinkUseParentPrice AS INTEGER   */
/*     FIELD SalesReportLevel          AS INTEGER   */
/*     FIELD DiscountAllowed           AS INTEGER   */
/*     FIELD PreferredSupplier         AS CHARACTER */
/*     FIELD ProductGroup              AS INTEGER   */
/*     FIELD BlockedDateTime           AS CHARACTER */
/*     FIELD SupplierProductCode       AS CHARACTER */
/*     FIELD ReferenceUnit             AS CHARACTER */
/*     FIELD ReferenceContent          AS CHARACTER */
/*     FIELD SalesUnit                 AS INTEGER   */
/*     FIELD SalesContent              AS CHARACTER */
    INDEX ProductCode IS PRIMARY UNIQUE itemId
    INDEX ItemAction  itemId action.

DEFINE TEMP-TABLE TT_BarCode NO-UNDO
    FIELD BarCode     AS DECIMAL
    FIELD Quantity    AS INTEGER
/*     FIELD Price       AS CHARACTER */
    FIELD itemId       AS DECIMAL
    FIELD Action       AS INTE /* 0=delete,1=update */
    FIELD itemIdExport AS DECIMAL
    INDEX itemIdBar IS PRIMARY UNIQUE itemId BarCode action.


DEFINE TEMP-TABLE TT_Ftpbutiker NO-UNDO
    FIELD ButikkNr AS INTEGER.

DEFINE TEMP-TABLE TT_Campaign NO-UNDO
    FIELD campaignId      AS DECIMAL
    FIELD action          AS CHARACTER
    FIELD Name            AS CHARACTER
    FIELD CampaignOwnerId AS CHARACTER
    FIELD StartDate       AS CHARACTER
    FIELD StartTime       AS CHARACTER
    FIELD EndDate         AS CHARACTER
    FIELD EndTime         AS CHARACTER
    INDEX campaignid IS PRIMARY UNIQUE campaignid.

DEFINE TEMP-TABLE TT_Promotion NO-UNDO
    FIELD campaignId      AS DECIMAL
    FIELD promotionId        LIKE KampanjeTilbud.KampTilbId
    FIELD action             AS CHARACTER
    FIELD HappyHourId        AS CHARACTER
    FIELD Type               AS CHARACTER
    FIELD ReceiptText        AS CHARACTER
    FIELD PopUpText          AS CHARACTER
    FIELD LimitCount         AS CHARACTER
    FIELD Amount             AS CHARACTER
    FIELD PercentIncrement   AS CHARACTER
    FIELD ProportionalPayFor AS CHARACTER
    INDEX campaignprom IS PRIMARY UNIQUE  campaignId promotionId.

DEFINE TEMP-TABLE TT_PromotionItem NO-UNDO
    FIELD campaignId      AS DECIMAL
    FIELD promotionId        LIKE KampanjeTilbArtikkel.KampTilbId
    FIELD promotionItemId    LIKE KampanjeTilbArtikkel.KampTilbArtId
    FIELD action             AS CHAR
    FIELD ItemTrigger        AS CHAR
    FIELD Amount             AS CHAR
    FIELD SaveItemFree       AS CHAR
    FIELD SuppressDiscount   AS CHAR
    FIELD ItemFamilyId       AS CHAR
    INDEX campaignpromid IS PRIMARY UNIQUE  campaignId promotionId promotionitemid.

DEFINE TEMP-TABLE TT_HappyHourHode NO-UNDO
    FIELD happyhourId LIKE HappyHourHode.HapHourId
    FIELD action      AS CHAR
    FIELD NAME        AS CHAR
    INDEX happyhourId IS PRIMARY UNIQUE happyhourId.

DEFINE TEMP-TABLE TT_HappyHourPeriode NO-UNDO
    FIELD happyhourId       LIKE HappyHourHode.HapHourId
    FIELD happyhourPeriodId LIKE HappyHourPeriode.HapHourPerId
    FIELD action            AS CHAR
    FIELD DayOfWeek         AS CHAR
    FIELD StartTime         AS CHAR
    FIELD EndTime           AS CHAR
    INDEX happyhourId happyhourID.

DEFINE TEMP-TABLE TT_ItemFamily NO-UNDO
    FIELD itemFamilyId LIKE KampanjeTilbArtikkel.ProdFamId
    FIELD action       AS CHAR
    FIELD NAME         AS CHAR
    INDEX itemFamilyId IS PRIMARY UNIQUE itemFamilyId.

DEFINE TEMP-TABLE TT_FamilyMember NO-UNDO
    FIELD itemFamilyId LIKE KampanjeTilbArtikkel.ProdFamId
    FIELD action       AS CHAR
    FIELD ItemId       AS CHAR
    FIELD itemIdExport AS CHAR
    INDEX itemFamilyId IS PRIMARY UNIQUE itemFamilyId ItemId.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getDrivstoffArtnr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDrivstoffArtnr Procedure 
FUNCTION getDrivstoffArtnr RETURNS DECIMAL
  ( INPUT ipArtikkelnr AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getItemIdexport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getItemIdexport Procedure 
FUNCTION getItemIdexport RETURNS DECIMAL
  ( INPUT dItemId AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLinkNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLinkNr Procedure 
FUNCTION getLinkNr RETURNS DECIMAL
  ( INPUT dLinkNr AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPriceBookfilnamn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPriceBookfilnamn Procedure 
FUNCTION getPriceBookfilnamn RETURNS LOGICAL
  ( INPUT cDir AS CHARACTER,INPUT iBut AS INTEGER,OUTPUT cPBfil AS CHARACTER,OUTPUT iSeqNr AS INTEGER,OUTPUT cPBBkufil AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWeekDay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWeekDay Procedure 
FUNCTION getWeekDay RETURNS CHARACTER
  ( INPUT iWeekDay AS INTEGER)  FORWARD.

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
   Temp-Tables and Buffers:
      TABLE: TT_ELogg T "?" NO-UNDO data ELogg
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 17.48
         WIDTH              = 90.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

IF TRIM(cEDBSystem) = '' THEN 
  cEDBSystem = 'POS'.

cFtpButiker = TRIM(cFtpButiker).

{syspara.i 2 1 11 dMaxPris DECI}
IF dMaxPris = 0 OR dMaxPris = ? THEN
    ASSIGN dMaxPris = 99999.
{syspara.i 2 4 15 cBonus}
ASSIGN lBonus = CAN-DO("1,ja,yes,true",TRIM(cBonus)).

{syspara.i 2 1 11 iCLbutik INT}
FIND butiker WHERE butiker.butik = iCLbutik NO-LOCK NO-ERROR.
IF AVAIL butiker THEN 
    ASSIGN iCLprofilnr = butiker.profilnr.
ELSE DO:
    FIND FIRST prisprofil NO-LOCK.
    iCLprofilnr = prisprofil.profilnr.
END.

/* Kanske vi skall hämta iformation om kassor och filer först. */
/* VI VÄNTAR EN STUND */
/* PAUSE 5 NO-MESSAGE. */
IF dKampId = 0 THEN DO:
    RUN bibl_logg.p ('klargjor_elogg', 'ArtBas2Nucleus.p: Utlegg av priser. ' + string(TIME,"HH:MM:SS")).
    RUN KopierEloggMaster.
/*     RUN KopierElogg. */
/*     IF CAN-FIND(TT_ELogg WHERE TT_ELogg.TabellNavn = "ArtBas" AND            */
/*          TT_ELogg.EksterntSystem = "POS" AND TT_ELogg.Verdier = "ALLE") THEN */
/*         RUN SkapaEloggAlle.                                                  */
/*     /* fixa lite med butikslistan */                                         */
/*     RUN ByggTT_FtpButiker.                                                   */
/*     RUN SlettTTmedALLE.                                                      */
END.
ELSE DO:
    RUN bibl_logg.p ('klargjor_elogg', 'ArtBas2Nucleus.p: Utlegg av kampanje. ' + string(TIME,"HH:MM:SS")).
    ASSIGN cLogSeqFil = "KampanjSeq_" + STRING(TODAY,"99999999") + ".txt".
    RUN ByggTT_FtpButiker.
    IF lSendArtiklar THEN
        RUN SkapaEloggKampanj.
    ELSE
        RUN UpdateCampaign.
END.

IF dKampId = 0 THEN DO:
    FOR EACH TT_MasterButik:
        EMPTY TEMP-TABLE TT_ArtPris.
        EMPTY TEMP-TABLE TT_Item.
        EMPTY TEMP-TABLE TT_BarCode.
        EMPTY TEMP-TABLE TT_Ftpbutiker.
        EMPTY TEMP-TABLE TT_Elogg.
        RUN SkapaTT_ELoggButik. /* skapar också TT_Ftpbutik */
        RUN FixArtBasEndringer.
        RUN UpdateItemBarCode.
        
        IF CAN-FIND(FIRST TT_Item) THEN DO:
            RUN ExportButiker IN THIS-PROCEDURE ("Artbas").
        END. 
    END.
END.
ELSE IF lSendArtiklar = TRUE THEN DO:
    RUN FixArtBasEndringer. /*  */
    RUN UpdateItemBarCode.
    IF CAN-FIND(FIRST TT_Item) THEN
        RUN ExportButiker IN THIS-PROCEDURE ("Artbas"). 
END.
ELSE DO:
    IF CAN-FIND(FIRST TT_Campaign) THEN
        RUN ExportButiker IN THIS-PROCEDURE ("Kampanj").

END.

/* RUN SlettTT_ELoggVare. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggTT_FtpButiker) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTT_FtpButiker Procedure 
PROCEDURE ByggTT_FtpButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
/*     IF dKampId = 0 THEN DO ii = 1 TO NUM-ENTRIES(cFtpButiker): /* normal elogghantering för artiklar */ */
/*         FIND Butiker WHERE butiker.butik = INT(ENTRY(ii,cFtpButiker)) NO-LOCK NO-ERROR.                 */
/*         IF NOT AVAIL Butiker OR Butiker.VPI <> 1 THEN                                                   */
/*             NEXT.                                                                                       */
/*         CREATE TT_FtpButiker.                                                                           */
/*         ASSIGN TT_FtpButiker.ButikkNr = INT(ENTRY(ii,cFtpButiker)) NO-ERROR.                            */
/*         IF ERROR-STATUS:ERROR THEN                                                                      */
/*             DELETE TT_FtpButiker.                                                                       */
/*     END.                                                                                                */
    IF dKampId = 0 THEN DO: /* normal elogghantering för artiklar */
        IF cFtpButiker = '' THEN
        DO: 
            FOR EACH butiker NO-LOCK WHERE Butiker.harButikksystem = TRUE AND Butiker.NedlagtDato = ?:
                IF CAN-FIND(FIRST kasse WHERE kasse.butik    = butiker.butik AND
                                              kasse.modellnr = 51            AND
                                              kasse.kassenr  < 11            AND
                                              kasse.aktiv    = TRUE) THEN DO:
                    CREATE TT_FtpButiker.
                    ASSIGN TT_FtpButiker.ButikkNr = Butiker.Butik NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN
                        DELETE TT_FtpButiker.
                END.
            END.
        END.
        ELSE DO:
            FOR EACH butiker NO-LOCK WHERE CAN-DO(cFtpButiker,STRING(Butiker.Butik)) AND 
                                           Butiker.harButikksystem = TRUE AND Butiker.NedlagtDato = ?:
                IF CAN-FIND(FIRST kasse WHERE kasse.butik    = butiker.butik AND
                                              kasse.modellnr = 51            AND
                                              kasse.kassenr  < 11            AND
                                              kasse.aktiv    = TRUE) THEN DO:
                    CREATE TT_FtpButiker.
                    ASSIGN TT_FtpButiker.ButikkNr = Butiker.Butik NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN
                        DELETE TT_FtpButiker.
                END.
            END.
        END.
    END.
    ELSE DO: /* vid kampanj */
        FOR EACH KampanjeButikker WHERE KampanjeButikker.KampId = dKampId AND 
                                        KampanjeButikker.SendtDato = ? NO-LOCK.
            FIND butiker OF KampanjeButikker NO-LOCK NO-ERROR.
/*             IF AVAIL butiker AND Butiker.VPI = 1 AND Butiker.Kampanje = 1 THEN DO: */
            IF AVAIL butiker AND Butiker.Kampanje = 1 THEN DO:
                CREATE TT_FtpButiker.
                ASSIGN TT_FtpButiker.ButikkNr = Butiker.butik NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    DELETE TT_FtpButiker.
            END.
        END.
    END.
    IF CAN-FIND(FIRST TT_FtpButiker) THEN
        ASSIGN hButikker = BUFFER TT_FtpButiker:HANDLE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateBarCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateBarCode Procedure 
PROCEDURE CreateBarCode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ditemId    AS DECIMAL    NO-UNDO.
DEFINE INPUT  PARAMETER cStrekKode AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iSlette    AS INTEGER    NO-UNDO.
DEFINE VARIABLE dTest AS DECIMAL    NO-UNDO.
    ASSIGN dTest = DECI(cStrekKode) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN.
    IF (cStrekKode BEGINS "7388" OR cStrekKode BEGINS "20") AND LENGTH(cStrekKode) = 13 THEN DO:
        ASSIGN cStrekKode = SUBSTR(cStrekKode,1,8) + "99999".
        dTest = DECI(cStrekkode).
    END.
    IF dTest = ABS(dTest) THEN DO:
        IF NOT CAN-FIND(TT_BarCode WHERE TT_BarCode.itemId = dItemId AND TT_BarCode.BarCode = dTest) THEN DO:
            CREATE TT_BarCode.
            ASSIGN TT_BarCode.BarCode   = dTest
                   TT_BarCode.Quantity  = 1
                   TT_BarCode.itemId    = dItemId
                   TT_BarCode.Action    = IF iSlette = 3 THEN 0 ELSE iSlette.
            ASSIGN TT_BarCode.itemIdExport = getItemIdexport(dItemId).
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExportButiker) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportButiker Procedure 
PROCEDURE ExportButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cType AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cPriceBookFil AS CHARACTER  NO-UNDO.
    ASSIGN cDateString = STRING(YEAR(TODAY),"9999") + "-" + STRING(MONTH(TODAY),"99") + "-" +
                            STRING(DAY(TODAY),"99")
           cTimeString = STRING(TIME,"HH:MM:SS")
           cOutputDir  = RIGHT-TRIM(cOutputDir,"\") + "\"
           cDateTid    =  cDateString + cTimeString.

        FOR EACH TT_FtpButiker:
            IF cType = "ArtBas" THEN
                RUN ExportVareNucleus (TT_FtpButiker.ButikkNr).
            ELSE IF cType = "Kampanj" THEN
                RUN ExportKampNucleus (TT_FtpButiker.ButikkNr).
        END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExportKampNucleus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportKampNucleus Procedure 
PROCEDURE ExportKampNucleus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iButikkNr AS INTEGER    NO-UNDO.

    DEFINE VARIABLE hDoc AS HANDLE.
    DEFINE VARIABLE hRoot AS HANDLE.
    DEFINE VARIABLE hRow AS HANDLE.
    DEFINE VARIABLE hField AS HANDLE.
    DEFINE VARIABLE hText AS HANDLE.
    DEFINE VARIABLE hDBFld AS HANDLE.
    DEFINE VARIABLE hCampaign AS HANDLE.
    DEFINE VARIABLE hCampaignOwner AS HANDLE.
    DEFINE VARIABLE hPromotionItems AS HANDLE.
    DEFINE VARIABLE hPromotionItem  AS HANDLE.
    DEFINE VARIABLE hPromotions AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hPromotion AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hPriceBook AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hHappyHour AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hPeriod AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hItemFamilyConfig   AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hItemFamily          AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hFamilyMembers        AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hFamilyMember         AS HANDLE     NO-UNDO.
    DEFINE VARIABLE cPriceBookFil AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cBkuPriceBookFil AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE i AS INTEGER.
    DEFINE VARIABLE iSeqNr AS INTEGER    NO-UNDO.

    IF NOT getPriceBookfilnamn(cOutputDir,iButikkNr,OUTPUT cPriceBookFil,OUTPUT iSeqnr, OUTPUT cBkuPriceBookFil) THEN
        RETURN "ERROR".
/*     ASSIGN cPriceBookFil = cOutputDir + "Pricebook_" + REPLACE(REPLACE(cDateTid,"-",""),":","") + "_" + STRING(iButikkNr) + ".xml". */
    IF dKampid > 0 AND cLogSeqFil <> "" THEN DO:
        OUTPUT TO VALUE(cOutputDir + cLogSeqFil) APPEND.
        PUT UNFORMATTED iButikkNr "," dKampId "," "Kampanj" "," iSeqNr SKIP.
        OUTPUT CLOSE.
    END.
    
    /* Create the objects we need. */
    CREATE X-DOCUMENT hDoc.
    hdoc:ENCODING = 'utf-8'.
    CREATE X-NODEREF hPriceBook.
    CREATE X-NODEREF hCampaign.
    CREATE X-NODEREF hCampaignOwner.
    CREATE X-NODEREF hPromotionItems.
    CREATE X-NODEREF hPromotionItem.
    CREATE X-NODEREF hRoot.
    CREATE X-NODEREF hRow.
    CREATE X-NODEREF hField.
    CREATE X-NODEREF hText.
    CREATE X-NODEREF hPromotions.
    CREATE X-NODEREF hPromotion.
    CREATE X-NODEREF hItemFamilyConfig.
    CREATE X-NODEREF hItemFamily.
    CREATE X-NODEREF hFamilyMembers.
    CREATE X-NODEREF hFamilyMember.
    CREATE X-NODEREF hHappyHour.
    CREATE X-NODEREF hPeriod.

    /* Set up a root node. */
    hDoc:CREATE-NODE (hRoot, "POSBOFile", "ELEMENT").
/*     hDoc:SET-ATTRIBUTE ("encoding","utf-8"). */
    hDoc:APPEND-CHILD (hRoot).
    hRoot:SET-ATTRIBUTE ("version","1.1").
/*     hRoot:SET-ATTRIBUTE ("encoding","utf-8"). */
    hRoot:SET-ATTRIBUTE ("xmlns","http://www.wayne.se/POS-BO/2005-10-26").
    hDoc:CREATE-NODE (hPriceBook, "PriceBook", "ELEMENT").
    hRoot:APPEND-CHILD (hPriceBook).
    hPriceBook:SET-ATTRIBUTE ("sequenceNumber","1").
    hPriceBook:SET-ATTRIBUTE ("date",cDateString).
    hPriceBook:SET-ATTRIBUTE ("time",cTimeString).

    /* !!! */
    FIND FIRST tt_itemfamily NO-ERROR.
    IF AVAIL tt_itemfamily THEN DO:
        hDoc:CREATE-NODE (hItemFamilyConfig, "ItemFamilyConfig", "ELEMENT").
        hPriceBook:APPEND-CHILD (hItemFamilyConfig).
        FOR EACH tt_itemfamily:
            hDoc:CREATE-NODE (hItemFamily, "ItemFamily", "ELEMENT").
            hItemFamilyConfig:APPEND-CHILD (hItemFamily).
            hItemFamily:SET-ATTRIBUTE ("itemFamilyId",STRING(tt_itemfamily.itemfamilyid)).
            hItemFamily:SET-ATTRIBUTE ("action","update").
            hDoc:CREATE-NODE (hField, "Name", "ELEMENT").
            hItemFamily:APPEND-CHILD(hField).
            hDoc:CREATE-NODE (hText, "", "TEXT").
            hField:APPEND-CHILD (hText).
            hText:NODE-VALUE = tt_itemfamily.NAME.
            hDoc:CREATE-NODE (hFamilyMembers, "FamilyMembers", "ELEMENT").
            hItemFamily:APPEND-CHILD(hFamilyMembers).
            FOR EACH TT_FamilyMember OF TT_ItemFamily:
                hDoc:CREATE-NODE (hFamilyMember, "FamilyMember", "ELEMENT").
                hFamilyMembers:APPEND-CHILD(hFamilyMember).
                hFamilyMember:SET-ATTRIBUTE ("action","update").
                hDoc:CREATE-NODE (hField, "ItemId", "ELEMENT").
                hFamilyMember:APPEND-CHILD(hField).
                hDoc:CREATE-NODE (hText, "", "TEXT").
                hField:APPEND-CHILD (hText).
/*                 hText:NODE-VALUE = TT_FamilyMember.itemid. */
                hText:NODE-VALUE = TT_FamilyMember.itemidExport.
            END.
        END.
    END.
    FIND FIRST TT_Campaign.
    hDoc:CREATE-NODE (hCampaign, "Campaign", "ELEMENT").
    hPriceBook:APPEND-CHILD (hCampaign).
    hCampaign:SET-ATTRIBUTE ("campaignId",STRING(TT_Campaign.campaignId)).
    hCampaign:SET-ATTRIBUTE ("action","update"). /* ? start om den item är sänd sedan tidigare ?? */
    /* Name */
    hDoc:CREATE-NODE (hField, "Name", "ELEMENT").
    hCampaign:APPEND-CHILD(hField).
    hDoc:CREATE-NODE (hText, "", "TEXT").
    hField:APPEND-CHILD (hText).
    hText:NODE-VALUE = TT_Campaign.NAME.
    /* CampaignOwnerId */
    hDoc:CREATE-NODE (hField, "CampaignOwnerId", "ELEMENT").
    hCampaign:APPEND-CHILD(hField).
    hDoc:CREATE-NODE (hText, "", "TEXT").
    hField:APPEND-CHILD (hText).
    hText:NODE-VALUE = TT_Campaign.CampaignOwnerId. /* campaiagnownerid*/
    /* StartDate */
    IF TT_Campaign.StartDate <> "" THEN DO:
        hDoc:CREATE-NODE (hField, "StartDate", "ELEMENT").
        hCampaign:APPEND-CHILD(hField).
        hDoc:CREATE-NODE (hText, "", "TEXT").
        hField:APPEND-CHILD (hText).
        hText:NODE-VALUE = TT_Campaign.StartDate. /* campaiagnownerid*/
    END.
    /* StartTime */
    IF TT_Campaign.StartTime <> "" THEN DO:
        hDoc:CREATE-NODE (hField, "StartTime", "ELEMENT").
        hCampaign:APPEND-CHILD(hField).
        hDoc:CREATE-NODE (hText, "", "TEXT").
        hField:APPEND-CHILD (hText).
        hText:NODE-VALUE = TT_Campaign.StartTime. /* campaiagnownerid*/
    END.
    /* EndDate */
    IF TT_Campaign.EndDate <> "" THEN DO:
        hDoc:CREATE-NODE (hField, "EndDate", "ELEMENT").
        hCampaign:APPEND-CHILD(hField).
        hDoc:CREATE-NODE (hText, "", "TEXT").
        hField:APPEND-CHILD (hText).
        hText:NODE-VALUE = TT_Campaign.EndDate. /* campaiagnownerid*/
    END.
    /* EndTime */
    IF TT_Campaign.EndTime <> "" THEN DO:
        hDoc:CREATE-NODE (hField, "EndTime", "ELEMENT").
        hCampaign:APPEND-CHILD(hField).
        hDoc:CREATE-NODE (hText, "", "TEXT").
        hField:APPEND-CHILD (hText).
        hText:NODE-VALUE = TT_Campaign.EndTime. /* campaiagnownerid*/
    END.
    hDoc:CREATE-NODE (hPromotions, "Promotions", "ELEMENT").
    hCampaign:APPEND-CHILD(hPromotions).
    FOR EACH TT_Promotion OF TT_Campaign.
        hDoc:CREATE-NODE (hPromotion, "Promotion", "ELEMENT").
        hPromotions:APPEND-CHILD(hPromotion).
        hPromotion:SET-ATTRIBUTE ("promotionId",STRING(TT_Promotion.promotionId)).
        hPromotion:SET-ATTRIBUTE ("action","update"). /* delete */

        hPromotion:SET-ATTRIBUTE ("promotionId",STRING(TT_Promotion.promotionId)).
        hPromotion:SET-ATTRIBUTE ("action","update"). /* delete */
        /* om HappyHour */
        IF TT_Promotion.HappyHourId <> "" THEN DO:
            hDoc:CREATE-NODE (hField, "HappyHourId", "ELEMENT").
            hPromotion:APPEND-CHILD(hField).
            hDoc:CREATE-NODE (hText, "", "TEXT").
            hField:APPEND-CHILD (hText).
            hText:NODE-VALUE = TT_Promotion.HappyHourId. /* HappyHour */
        END.
        /* Type */
        hDoc:CREATE-NODE (hField, "Type", "ELEMENT").
        hPromotion:APPEND-CHILD(hField).
        hDoc:CREATE-NODE (hText, "", "TEXT").
        hField:APPEND-CHILD (hText).
        hText:NODE-VALUE = TT_Promotion.TYPE. /* Type*/

        /* Receipttext */
        hDoc:CREATE-NODE (hField, "ReceiptText", "ELEMENT").
        hPromotion:APPEND-CHILD(hField).
        hDoc:CREATE-NODE (hText, "", "TEXT").
        hField:APPEND-CHILD (hText).
        hText:NODE-VALUE = SUBSTR(TT_Promotion.Receipttext,1,14). /* ReceiptText*/
        /* PopUpText */
        IF TT_Promotion.PopUpText <> "" THEN DO: /* se dok eventuellt alltid ut men med blank */
            hDoc:CREATE-NODE (hField, "PopUpText", "ELEMENT").
            hPromotion:APPEND-CHILD(hField).
            hDoc:CREATE-NODE (hText, "", "TEXT").
            hField:APPEND-CHILD (hText).
            hText:NODE-VALUE = SUBSTR(TT_Promotion.PopUpText,1,30). /* Popuptext */
        END.
        /* LimitCount */
        IF TT_Promotion.LimitCount <> "" THEN DO:
            hDoc:CREATE-NODE (hField, "LimitCount", "ELEMENT").
            hPromotion:APPEND-CHILD(hField).
            hDoc:CREATE-NODE (hText, "", "TEXT").
            hField:APPEND-CHILD (hText).
            hText:NODE-VALUE = TT_Promotion.LimitCount. /* LimitCount*/
        END.
        /* Amount */ 
        IF TT_Promotion.Amount <> "" THEN DO:
            hDoc:CREATE-NODE (hField, "Amount", "ELEMENT").
            hPromotion:APPEND-CHILD(hField).
            hDoc:CREATE-NODE (hText, "", "TEXT").
            hField:APPEND-CHILD (hText).
            hText:NODE-VALUE = TT_Promotion.Amount. /* Amount */
        END.
        /* PercentIncrement */ 
        IF TT_Promotion.PercentIncrement <> "" THEN DO: /* "percent" */
            hDoc:CREATE-NODE (hField, "PercentIncrement", "ELEMENT").
            hPromotion:APPEND-CHILD(hField).
            hDoc:CREATE-NODE (hText, "", "TEXT").
            hField:APPEND-CHILD (hText).
            hText:NODE-VALUE = TT_Promotion.PercentIncrement. /* PercentIncrement */
        END.
        /* PropotionalPayFor */ 
        IF TT_Promotion.ProportionalPayFor <> "" THEN DO:  /* 4 = payfor */
            hDoc:CREATE-NODE (hField, "ProportionalPayFor", "ELEMENT").
            hPromotion:APPEND-CHILD(hField).
            hDoc:CREATE-NODE (hText, "", "TEXT").
            hField:APPEND-CHILD (hText).
            hText:NODE-VALUE = TT_Promotion.ProportionalPayFor. /* PercentIncrement */
        END.
        /* PromotionItems */
        hDoc:CREATE-NODE (hPromotionItems, "PromotionItems", "ELEMENT").
        hPromotion:APPEND-CHILD(hPromotionItems).
        FOR EACH TT_PromotionItem OF TT_Promotion:
            hDoc:CREATE-NODE (hPromotionItem, "PromotionItem", "ELEMENT").
            hPromotionItems:APPEND-CHILD(hPromotionItem).
            hPromotionItem:SET-ATTRIBUTE ("promotionItemId",STRING(TT_PromotionItem.promotionItemId)).
            hPromotionItem:SET-ATTRIBUTE ("action",TT_PromotionItem.action).
            hDoc:CREATE-NODE (hField, "ItemTrigger", "ELEMENT").
            hPromotionItem:APPEND-CHILD(hField).
            hDoc:CREATE-NODE (hText, "", "TEXT").
            hField:APPEND-CHILD (hText).
            hText:NODE-VALUE = TT_PromotionItem.ItemTrigger. /* ItemTrigger number of units*/
            /* Amount */ 
           IF TT_PromotionItem.Amount <> "" THEN DO:
                hDoc:CREATE-NODE (hField, "Amount", "ELEMENT").
                hPromotionItem:APPEND-CHILD(hField).
                hDoc:CREATE-NODE (hText, "", "TEXT").
                hField:APPEND-CHILD (hText).
                hText:NODE-VALUE = TT_PromotionItem.Amount. /* LimitCount*/
            END.
            /* SaveItemFree */ 
/*             IF TT_PromotionItem.SaveItemFree <> "" THEN */
            DO:
                hDoc:CREATE-NODE (hField, "SaveItemFree", "ELEMENT").
                hPromotionItem:APPEND-CHILD(hField).
                hDoc:CREATE-NODE (hText, "", "TEXT").
                hField:APPEND-CHILD (hText).
                hText:NODE-VALUE = TT_PromotionItem.SaveItemFree. /* LimitCount*/
            END.
            /* SuppressDiscount */ 
/*             IF TT_PromotionItem.SuppressDiscount <> "" THEN */
            DO:
                hDoc:CREATE-NODE (hField, "SuppressDiscount", "ELEMENT").
                hPromotionItem:APPEND-CHILD(hField).
                hDoc:CREATE-NODE (hText, "", "TEXT").
                hField:APPEND-CHILD (hText).
                hText:NODE-VALUE = TT_PromotionItem.SuppressDiscount. /* LimitCount*/
            END.
            /* ItemFamilyId */ 
            hDoc:CREATE-NODE (hField, "ItemFamilyId", "ELEMENT").
            hPromotionItem:APPEND-CHILD(hField).
            hDoc:CREATE-NODE (hText, "", "TEXT").
            hField:APPEND-CHILD (hText).
            hText:NODE-VALUE = TT_PromotionItem.ItemFamilyId. /* LimitCount*/
        END.
    END.
    FOR EACH TT_HappyHourHode:
        hDoc:CREATE-NODE (hHappyHour, "HappyHour", "ELEMENT").
        hPriceBook:APPEND-CHILD (hHappyHour).
        hHappyHour:SET-ATTRIBUTE ("happyhourId",STRING(HappyHourHode.HapHourId)).
        hHappyHour:SET-ATTRIBUTE ("action",TT_HappyHourHode.action).
        hDoc:CREATE-NODE (hField, "Name", "ELEMENT").
        hHappyHour:APPEND-CHILD(hField).
        hDoc:CREATE-NODE (hText, "", "TEXT").
        hField:APPEND-CHILD (hText).
        hText:NODE-VALUE = SUBSTR(HappyHourHode.HapHourNavn,1,20).
        FOR EACH TT_HappyHourPeriode OF TT_HappyHourHode:
            hDoc:CREATE-NODE (hPeriod, "Period", "ELEMENT").
            hHappyHour:APPEND-CHILD(hPeriod).

            hPeriod:SET-ATTRIBUTE ("happyhourPeriodId",STRING(TT_HappyHourPeriode.happyhourPeriodId)).
            hPeriod:SET-ATTRIBUTE ("action",TT_HappyHourPeriode.action).
            IF TT_HappyHourPeriode.DayOfWeek <> "" THEN DO:
                hDoc:CREATE-NODE (hField, "DayOfWeek", "ELEMENT").
                hPeriod:APPEND-CHILD(hField).
                hDoc:CREATE-NODE (hText, "", "TEXT").
                hField:APPEND-CHILD (hText).
                hText:NODE-VALUE = TT_HappyHourPeriode.DayOfWeek.
            END.
            hDoc:CREATE-NODE (hField, "StartTime", "ELEMENT").
            hPeriod:APPEND-CHILD(hField).
            hDoc:CREATE-NODE (hText, "", "TEXT").
            hField:APPEND-CHILD (hText).
            hText:NODE-VALUE = TT_HappyHourPeriode.StartTime.

            hDoc:CREATE-NODE (hField, "EndTime", "ELEMENT").
            hPeriod:APPEND-CHILD(hField).
            hDoc:CREATE-NODE (hText, "", "TEXT").
            hField:APPEND-CHILD (hText).
            hText:NODE-VALUE = TT_HappyHourPeriode.EndTime.
        END.

    END.
    hDoc:CREATE-NODE (hCampaignOwner, "CampaignOwner", "ELEMENT").
    hPriceBook:APPEND-CHILD (hCampaignOwner).
    hCampaignOwner:SET-ATTRIBUTE ("campaignOwnerId",STRING(iCampaignOwnerId)).
    hCampaignOwner:SET-ATTRIBUTE ("action","update").
    hDoc:CREATE-NODE (hField, "Name", "ELEMENT").
    hCampaignOwner:APPEND-CHILD(hField).
    hDoc:CREATE-NODE (hText, "", "TEXT").
    hField:APPEND-CHILD (hText).
    hText:NODE-VALUE = SUBSTR(cCampaignOwnerName,1,50).

    /* Write the XML node tree to an xml file. */
    OUTPUT STREAM xmlfile to VALUE(cPriceBookFil) NO-ECHO.
    hDoc:SAVE ("stream", "xmlfile").
    OUTPUT STREAM xmlfile CLOSE.
    /* Write the XML node tree to an xml file. */
    OUTPUT STREAM xmlfile to VALUE(cBkuPriceBookFil) NO-ECHO.
    hDoc:SAVE ("stream", "xmlfile").
    OUTPUT STREAM xmlfile CLOSE.
/*     hDoc:SAVE ("file", "c:\tmp\tmp.xml"). */
/*     OS-COMMAND SILENT VALUE("move c:\tmp\tmp.xml " + cPriceBookFil). */
    /* Delete the objects. Note that deleting the document
    object deletes the DOM structure under it also. */
    DELETE OBJECT hDoc.
    DELETE OBJECT hRoot.
    DELETE OBJECT hRow.
    DELETE OBJECT hField.
    DELETE OBJECT hText.
    DELETE OBJECT hPriceBook.
    DELETE OBJECT hCampaign.
    DELETE OBJECT hCampaignOwner.
    DELETE OBJECT hPromotionItems.
    DELETE OBJECT hPromotionItem.
    DELETE OBJECT hPromotions.
    DELETE OBJECT hPromotion.
    DELETE OBJECT hItemFamilyConfig.
    DELETE OBJECT hItemFamily.
    DELETE OBJECT hFamilyMembers.
    DELETE OBJECT hFamilyMember.
    DELETE OBJECT hHappyHour.
    DELETE OBJECT hPeriod.
    FIND KampanjeButikker WHERE KampanjeButikker.KampId = dKampId AND KampanjeButikker.Butik = iButikkNr NO-ERROR.
    IF AVAIL KampanjeButikker THEN DO:
        ASSIGN KampanjeButikker.SendtDato = TODAY
               KampanjeButikker.SendtTid  = TIME
               KampanjeButikker.MottattDato = ?
               KampanjeButikker.MottattKl = ""
               KampanjeButikker.Resultat   = "".
        CREATE KampanjeButMottak.
        ASSIGN KampanjeButMottak.Butik  = iButikkNr
               KampanjeButMottak.KampId = dKampId
               KampanjeButMottak.SekvNr = iSeqNr
               KampanjeButMottak.Filnavn = "Kampanj" NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE KampanjeButMottak.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExportVareNucleus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportVareNucleus Procedure 
PROCEDURE ExportVareNucleus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iButikkNr AS INTEGER    NO-UNDO.
    DEFINE VARIABLE hDoc AS HANDLE.
    DEFINE VARIABLE hRoot AS HANDLE.
    DEFINE VARIABLE hRow AS HANDLE.
    DEFINE VARIABLE hField AS HANDLE.
    DEFINE VARIABLE hText AS HANDLE.
    DEFINE VARIABLE hDBFld AS HANDLE.
    DEFINE VARIABLE hItemConfig AS HANDLE.
    DEFINE VARIABLE hItem AS HANDLE.
    DEFINE VARIABLE hItemGroups AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hItemGroup AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hPriceBook AS HANDLE     NO-UNDO.

    DEFINE VARIABLE cPriceBookFil AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cBkuPriceBookFil AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE i      AS INTEGER.
    DEFINE VARIABLE iSeqNr AS INTEGER    NO-UNDO.
    
    FIND Butiker WHERE butiker.butik = iButikkNr NO-LOCK NO-ERROR.
    IF NOT AVAIL butiker THEN
        RETURN.
    IF NOT getPriceBookfilnamn(cOutputDir,iButikkNr,OUTPUT cPriceBookFil,OUTPUT iSeqNr, OUTPUT cBkuPriceBookFil) THEN
        RETURN "ERROR".
/*     ASSIGN cPriceBookFil = cOutputDir + "Pricebook_" + REPLACE(REPLACE(cDateTid,"-",""),":","") + "_" + STRING(iButikkNr) + ".xml". */
    
    IF dKampid > 0 AND cLogSeqFil <> "" THEN DO:
        OUTPUT TO VALUE(cOutputDir + cLogSeqFil) APPEND.
        PUT UNFORMATTED iButikkNr "," dKampId "," "Artikkel" "," iSeqNr SKIP.
        OUTPUT CLOSE.
    END.

    RUN bibl_logg.p ('klargjor_elogg', 'ArtBas2Nucleus.p: Utlegg av priser til fil: ' + cPriceBookFil + '. ' + string(TIME,"HH:MM:SS")).

    /* Create the objects we need. */
    CREATE X-DOCUMENT hDoc.
    hdoc:ENCODING = 'utf-8'.
    CREATE X-NODEREF hPriceBook.
    CREATE X-NODEREF hItemConfig.
    CREATE X-NODEREF hItem.
    CREATE X-NODEREF hRoot.
    CREATE X-NODEREF hRow.
    CREATE X-NODEREF hField.
    CREATE X-NODEREF hText.
    CREATE X-NODEREF hItemGroups.
    CREATE X-NODEREF hItemGroup.
    /* sätt datum och tidsstränggar */
/*     MESSAGE "Ingen från avdelning 1"       */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */

/*     FIND Butiker WHERE butiker.butik = iButikkNr NO-LOCK NO-ERROR. */
/*     IF NOT AVAIL butiker THEN                                      */
/*         RETURN.                                                    */
    /* Get a buffer for the Customer table. */
    /* Set up a root node. */
    hDoc:CREATE-NODE (hRoot, "POSBOFile", "ELEMENT").
/*     hDoc:SET-ATTRIBUTE ("encoding","utf-8"). */
    hDoc:APPEND-CHILD (hRoot).
    hRoot:SET-ATTRIBUTE ("version","1.1").
/*     hRoot:SET-ATTRIBUTE ("encoding","utf-8"). */
    hRoot:SET-ATTRIBUTE ("xmlns","http://www.wayne.se/POS-BO/2005-10-26").
    hDoc:CREATE-NODE (hPriceBook, "PriceBook", "ELEMENT").
    hRoot:APPEND-CHILD (hPriceBook).
    hPriceBook:SET-ATTRIBUTE ("sequenceNumber","1").
    hPriceBook:SET-ATTRIBUTE ("date",cDateString).
    hPriceBook:SET-ATTRIBUTE ("time",cTimeString).
    hDoc:CREATE-NODE (hItemConfig, "ItemConfig", "ELEMENT").
    hPriceBook:APPEND-CHILD (hItemConfig).


    FOR EACH TT_Item WHERE TT_Item.Itemid > 0:
        IF TT_Item.action <> 1 THEN /* De kan inte ta med sletteposter */
            NEXT.
        IF NOT CAN-FIND(FIRST TT_Barcode WHERE TT_Barcode.ItemId = TT_Item.ItemId) THEN
            NEXT.
        hDoc:CREATE-NODE (hItem, "Item", "ELEMENT").
        hItemConfig:APPEND-CHILD (hItem).
/*         hItem:SET-ATTRIBUTE ("itemId",STRING(TT_Item.itemId)). */
        hItem:SET-ATTRIBUTE ("itemId",STRING(TT_Item.itemIdExport)).
        hItem:SET-ATTRIBUTE ("action",STRING(TT_Item.action = 1,"update/delete")).
        hDoc:CREATE-NODE (hField, "Name", "ELEMENT").
        hItem:APPEND-CHILD(hField).
        hDoc:CREATE-NODE (hText, "", "TEXT").
        /* Node to hold value. */
        hField:APPEND-CHILD (hText).
        /* Attach text to field */
        hText:NODE-VALUE = TT_Item.NAME.

        /* ItemType */
        hDoc:CREATE-NODE (hField, "ItemType", "ELEMENT").
        hItem:APPEND-CHILD(hField)                          .
        hDoc:CREATE-NODE (hText, "", "TEXT").
        /* Node to hold value. */
        hField:APPEND-CHILD (hText).
        /* Attach text to field */
        hText:NODE-VALUE = TT_Item.itemType.

        /* ManualDiscount */
        hDoc:CREATE-NODE (hField, "ManualDiscount", "ELEMENT").
        hItem:APPEND-CHILD(hField).
        hField:SET-ATTRIBUTE("allowed","true").
        hField:SET-ATTRIBUTE("discountOverride","true").
        hField:SET-ATTRIBUTE("unitDiscount","0").
        hField:SET-ATTRIBUTE("percentage","0").

        /* SalesContent */
        hDoc:CREATE-NODE (hField, "SalesContent", "ELEMENT").
        hItem:APPEND-CHILD(hField).
        hField:SET-ATTRIBUTE("uom","").
        hDoc:CREATE-NODE (hText, "", "TEXT").
        /* Node to hold value. */
        hField:APPEND-CHILD (hText).
        /* Attach text to field */
        hText:NODE-VALUE = TT_Item.SCuom.

        /* SalesReferenceContent */
        hDoc:CREATE-NODE (hField, "SalesReferenceContent", "ELEMENT").
        hItem:APPEND-CHILD(hField).
        hField:SET-ATTRIBUTE("uom","").
        hDoc:CREATE-NODE (hText, "", "TEXT").
        /* Node to hold value. */
        hField:APPEND-CHILD (hText).
        /* Attach text to field */
        hText:NODE-VALUE = TT_Item.SRCuom.

        /* ProductOwnderId */
        hDoc:CREATE-NODE (hField, "ProductOwnerId", "ELEMENT").
        hItem:APPEND-CHILD(hField)                          .
        hDoc:CREATE-NODE (hText, "", "TEXT").
        /* Node to hold value. */
        hField:APPEND-CHILD (hText).
        /* Attach text to field */
        hText:NODE-VALUE = STRING(TT_Item.ProductOwnerId).

        /* AccountGroups */
        hDoc:CREATE-NODE (hItemGroups, "AccountGroups", "ELEMENT").
        hItem:APPEND-CHILD (hItemGroups).
            /* AccountGroups */
            hDoc:CREATE-NODE (hItemGroup, "AccountGroup", "ELEMENT").
            hItemGroups:APPEND-CHILD (hItemGroup).
            hItemGroup:SET-ATTRIBUTE("salesType","takeaway").
            hItemGroup:SET-ATTRIBUTE("action",STRING(TT_Item.action = 1,"update/delete")).
                hDoc:CREATE-NODE (hField, "AccountId", "ELEMENT").
                hItemGroup:APPEND-CHILD(hField).
                hDoc:CREATE-NODE (hText, "", "TEXT").
                /* Node to hold value. */
                hField:APPEND-CHILD (hText).
                /* Attach text to field */
                hText:NODE-VALUE = STRING(TT_Item.AccountId).

                hDoc:CREATE-NODE (hField, "TaxClassId", "ELEMENT").
                hItemGroup:APPEND-CHILD(hField).
                hDoc:CREATE-NODE (hText, "", "TEXT").
                /* Node to hold value. */
                hField:APPEND-CHILD (hText).
                /* Attach text to field */
                hText:NODE-VALUE = STRING(TT_Item.TaxClass).
         IF dKampid > 0 THEN DO:
             FIND TT_ArtPris WHERE TT_ArtPris.Artikkelnr = TT_Item.itemId AND 
                                   TT_ArtPris.profilnr   = iCLprofilnr NO-ERROR.
             IF NOT AVAIL TT_ArtPris THEN
                 FIND TT_ArtPris WHERE TT_ArtPris.Artikkelnr = TT_Item.itemId AND 
                                       TT_ArtPris.profilnr   = butiker.profilnr NO-ERROR.
         END.
         ELSE DO:
             FIND TT_ArtPris WHERE TT_ArtPris.Artikkelnr = TT_Item.itemId AND 
                                   TT_ArtPris.profilnr   = butiker.profilnr NO-ERROR.
             IF NOT AVAIL TT_ArtPris THEN
                 FIND TT_ArtPris WHERE TT_ArtPris.Artikkelnr = TT_Item.itemId AND 
                                       TT_ArtPris.profilnr   = iCLprofilnr NO-ERROR.
         END.
         IF NOT AVAIL TT_artpris THEN
             FIND FIRST TT_artpris WHERE TT_ArtPris.Artikkelnr = TT_Item.itemId NO-ERROR.
         IF AVAIL TT_Artpris THEN DO:
             /* Main Prices */
             hDoc:CREATE-NODE (hItemGroups, "MainPrices", "ELEMENT").
             hItem:APPEND-CHILD (hItemGroups).
                 /* MainPrice */
                 hDoc:CREATE-NODE (hItemGroup, "MainPrice", "ELEMENT").
                 hItemGroups:APPEND-CHILD (hItemGroup).
                 hItemGroup:SET-ATTRIBUTE("startDate",cDateString).
                 hItemGroup:SET-ATTRIBUTE("startTime",cTimeString).
                 hItemGroup:SET-ATTRIBUTE("action",STRING(TT_Item.action = 1,"update/delete")).
                     hDoc:CREATE-NODE (hField, "UnitPrice", "ELEMENT").
                     hItemGroup:APPEND-CHILD(hField).
                     hDoc:CREATE-NODE (hText, "", "TEXT").
                     /* Node to hold value. */
                     hField:APPEND-CHILD (hText).
                     /* Attach text to field */
                     hText:NODE-VALUE = REPLACE(STRING(TT_Artpris.NPris,"99999.99"),",",".").
         END.
         IF CAN-FIND(FIRST TT_Barcode WHERE TT_Barcode.ItemId = TT_Item.ItemId AND
                                            TT_Barcode.action = 1) THEN DO:
             /* BarCodes */
             hDoc:CREATE-NODE (hItemGroups, "BarCodes", "ELEMENT").
             hItem:APPEND-CHILD (hItemGroups).
             /* BarCode */
             FOR EACH TT_Barcode WHERE TT_Barcode.itemId = TT_Item.itemId:

                 IF TT_BarCode.action <> 1 THEN /* sletteposter skall inte ut */
                     NEXT.

                 hDoc:CREATE-NODE (hItemGroup, "BarCode", "ELEMENT").
                 hItemGroups:APPEND-CHILD (hItemGroup).
                 hItemGroup:SET-ATTRIBUTE("barCode",STRING(TT_Barcode.BarCode,"9999999999999")).
                 hItemGroup:SET-ATTRIBUTE("action",STRING(TT_BarCode.action = 1,"update/delete")).
                     hDoc:CREATE-NODE (hField, "Quantity", "ELEMENT").
                     hItemGroup:APPEND-CHILD(hField).
                     hDoc:CREATE-NODE (hText, "", "TEXT").
                     /* Node to hold value. */
                     hField:APPEND-CHILD (hText).
                     /* Attach text to field */
                     hText:NODE-VALUE = STRING(TT_Barcode.Quantity).
             END.
         END.
         /*
         IF TT_Item.depositlink > 0 THEN DO: /* pant */
             hDoc:CREATE-NODE (hItemGroups, "DepositLinks", "ELEMENT").
             hItem:APPEND-CHILD (hItemGroups).
             hDoc:CREATE-NODE (hItemGroup, "DepositLink", "ELEMENT").
             hItemGroups:APPEND-CHILD (hItemGroup).
             hItemGroup:SET-ATTRIBUTE("linkedItemId",STRING(TT_Item.depositlink)).
             hItemGroup:SET-ATTRIBUTE("action","update").
                 hDoc:CREATE-NODE (hField, "Quantity", "ELEMENT").
                 hItemGroup:APPEND-CHILD(hField).
                 hDoc:CREATE-NODE (hText, "", "TEXT").
                 /* Node to hold value. */
                 hField:APPEND-CHILD (hText).
                 /* Attach text to field */
                 hText:NODE-VALUE = "1".
         END.
         */
    END.



    /* FOR EACH Customer WHERE cust-num < 5:                              */
    /*     /* Create a customer row node. */                              */
    /*     hDoc:CREATE-NODE (hRow, "Customer", "ELEMENT").                */
    /*     hRoot:APPEND-CHILD (hRow).                                     */
    /*     /* Put the row in the tree. Cust-num and Name are attributes   */
    /*     of this element. The remaining fields are elements. */         */
    /*     hRow:SET-ATTRIBUTE ("Cust-num", STRING (cust-num)).            */
    /*     hRow:SET-ATTRIBUTE ("Name", NAME).                             */
    /*     /* Add the other fields as elements. */                        */
    /*     REPEAT i = 1 TO hBuf:NUM-FIELDS:                               */
    /*         hDBFld = hBuf:BUFFER-FIELD (i).                            */
    /*         /* We already did Cust-num and Name above so skip them. */ */
    /*         IF hDBFld:NAME = "Cust-num" OR hDBFld:NAME = "NAME"        */
    /*             THEN NEXT.                                             */
    /*         /* Create an element with the field name as the tag.       */
    /*         Note that the field name is the same as the element        */
    /*         name. The rules for allowed names in XML are less          */
    /*         stringent than the rules for Progress column names. */     */
    /*         hDoc:CREATE-NODE (hField, hDBFld:NAME, "ELEMENT").         */
    /*         hRow:APPEND-CHILD (hField).                                */
    /*         /* Make new field next row child. */                       */
    /*         hDoc:CREATE-NODE (hText, "", "TEXT").                      */
    /*         /* Node to hold value. */                                  */
    /*         hField:APPEND-CHILD (hText).                               */
    /*         /* Attach text to field */                                 */
    /*         hText:NODE-VALUE = STRING (hDBFld:BUFFER-VALUE).           */
    /*     END.                                                           */
    /* END.                                                               */
    /* Write the XML node tree to an xml file. */
    OUTPUT STREAM xmlfile to VALUE(cPriceBookFil) NO-ECHO.
    hDoc:SAVE ("stream", "xmlfile").
    OUTPUT STREAM xmlfile CLOSE.
    /* Write the XML node tree to an xml file. */
    OUTPUT STREAM xmlfile to VALUE(cBkuPriceBookFil) NO-ECHO.
    hDoc:SAVE ("stream", "xmlfile").
    OUTPUT STREAM xmlfile CLOSE.
/*     hDoc:SAVE ("file", "c:\tmp\tmp.xml"). */
/*     OS-COMMAND SILENT VALUE("move c:\tmp\tmp.xml " + cPriceBookFil). */
    /* Delete the objects. Note that deleting the document
    object deletes the DOM structure under it also. */
    DELETE OBJECT hDoc.
    DELETE OBJECT hRoot.
    DELETE OBJECT hRow.
    DELETE OBJECT hField.
    DELETE OBJECT hText.
    DELETE OBJECT hPriceBook.
    DELETE OBJECT hItemConfig.
    DELETE OBJECT hItem.
    DELETE OBJECT hItemGroups.
    DELETE OBJECT hItemGroup.
    IF dKampid > 0 THEN DO:
        CREATE KampanjeButMottak.
        ASSIGN KampanjeButMottak.Butik  = iButikkNr
               KampanjeButMottak.KampId = dKampId
               KampanjeButMottak.SekvNr = iSeqNr 
               KampanjeButMottak.FilNavn = "Artiklar" NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE KampanjeButMottak.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixArtBasEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixArtBasEndringer Procedure 
PROCEDURE FixArtBasEndringer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE lPakkeBort AS LOGICAL    NO-UNDO.
DEFINE VARIABLE dTest      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cTekst     AS CHAR       NO-UNDO.
DEFINE VARIABLE dArtnr AS DECIMAL     NO-UNDO.
DEF VAR piLoop AS INT NO-UNDO.

DEFINE BUFFER bTT_ELogg FOR TT_ELogg.

/* Först samlar vi samman alla Streckkoder som tillhör en record till en behandlingspost */
/* Därefter ser vi om vi har en ändringspost på själva ArtBas. Om den inte finns ser vi på ArtBas */
/* om flaggan för 'iKassa' är satt. Om den inte är det kan vi ignorera sletting av strekkoderna */

    /* Ändring i blocket för att kunna lägga ut pant */
    FOR EACH TT_ELogg WHERE 
        TT_ELogg.TabellNavn     = "ArtBas" AND 
        TT_ELogg.EksterntSystem = cEDBSystem:

        dArtnr = DECI(ENTRY(1,TT_ELogg.Verdier,CHR(1))).

        IF NOT CAN-FIND(FIRST TT_Item WHERE TT_Item.ItemId = dArtnr) THEN 
        DO:
            IF (dArtnr > 900000 AND dArtnr < 900200) OR 
                dArtnr = 0 THEN
                NEXT.
            FIND Artbas WHERE 
                 Artbas.artikkelnr = dArtnr NO-LOCK NO-ERROR.
                 
            IF AVAIL Artbas AND Artbas.LinkVareNr > 0 THEN 
            DO:
                /*
                IF NOT CAN-FIND(FIRST TT_Item WHERE TT_Item.ItemId = Artbas.LinkVareNr) THEN                
                DO:
                    CREATE TT_Item.
                    ASSIGN TT_Item.ItemId       = Artbas.LinkVareNr.
                    ASSIGN TT_Item.itemIdExport = getItemIdexport(TT_Item.ItemId).
                END.
                */
            END.
            
            CREATE TT_Item.
            ASSIGN TT_Item.ItemId       = dArtnr.
            ASSIGN TT_Item.itemIdExport = getItemIdexport(TT_Item.ItemId).
        END.
        IF TT_ELogg.EndringsType = 3 AND NUM-ENTRIES(TT_ELogg.Verdier,CHR(1)) = 2 THEN DO:
            RUN CreateBarCode (DECI(ENTRY(1,TT_ELogg.Verdier,CHR(1))),ENTRY(2,TT_ELogg.Verdier,CHR(1)),TT_ELogg.EndringsType).
        END.
    END.

/*FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = "ArtBas" AND TT_ELogg.EksterntSystem = cEDBSystem:                          */
/*    IF NOT CAN-FIND(FIRST TT_Item WHERE TT_Item.ItemId = DECI(ENTRY(1,TT_ELogg.Verdier,CHR(1)))) THEN DO:                 */
/*        IF (DECI(ENTRY(1,TT_ELogg.Verdier,CHR(1))) > 900000 AND DECI(ENTRY(1,TT_ELogg.Verdier,CHR(1))) < 900200) OR       */
/*            DECI(ENTRY(1,TT_ELogg.Verdier,CHR(1))) = 0 THEN                                                               */
/*            NEXT.                                                                                                         */
/*        CREATE TT_Item.                                                                                                   */
/*        ASSIGN TT_Item.ItemId = DECI(ENTRY(1,TT_ELogg.Verdier,CHR(1))).                                                   */
/*        ASSIGN TT_Item.itemIdExport = getItemIdexport(TT_Item.ItemId).                                                    */
/*    END.                                                                                                                  */
/*    IF TT_ELogg.EndringsType = 3 AND NUM-ENTRIES(TT_ELogg.Verdier,CHR(1)) = 2 THEN DO:                                    */
/*        RUN CreateBarCode (DECI(ENTRY(1,TT_ELogg.Verdier,CHR(1))),ENTRY(2,TT_ELogg.Verdier,CHR(1)),TT_ELogg.EndringsType).*/
/*    END.                                                                                                                  */
/*END.                                                                                                                      */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KopierElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierElogg Procedure 
PROCEDURE KopierElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bElogg FOR Elogg.
    IF CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "ArtBas" AND
                            ELogg.EksterntSystem = cEDBSystem    AND
                            ELogg.Verdier        = "KLARGJOR") THEN DO:
        FIND ELogg WHERE ELogg.TabellNavn     = "ArtBas" AND
                         ELogg.EksterntSystem = cEDBSystem    AND
                         ELogg.Verdier        = "KLARGJOR" NO-LOCK NO-ERROR.
        IF AVAIL ELogg THEN DO:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            ASSIGN TT_ELogg.Verdier = "ALLE".
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
    END.
    ELSE DO:
        FOR EACH ELogg WHERE ELogg.TabellNavn = "ArtBas" AND
                             ELogg.EksterntSystem = cEDBSystem NO-LOCK:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
    END.
  
    /* Tar hånd om ArtPris poster. MIDLERTIDIG */
    ARTPRIS:
    DO:
        FOR EACH ELogg WHERE ELogg.TabellNavn = "ArtPris" AND
                             ELogg.EksterntSystem = cEDBSystem NO-LOCK:
            IF NOT CAN-FIND(TT_Elogg WHERE
                            TT_ELogg.TabellNavn     = 'ArtBas' AND
                            TT_ELogg.EksterntSystem = cEDBSystem AND 
                            TT_ELogg.Verdier        = ENTRY(1,ELogg.Verdier,CHR(1))) THEN 
            DO:
              BUFFER-COPY ELogg 
                EXCEPT Verdier TabellNavn
                TO TT_ELogg 
                ASSIGN 
                   TT_ELogg.TabellNavn = 'ArtBas'
                   TT_ELogg.Verdier    = ENTRY(1,ELogg.Verdier,CHR(1))
                NO-ERROR.
            END.  
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
    END. /* ARTPRIS */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KopierEloggMaster) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierEloggMaster Procedure 
PROCEDURE KopierEloggMaster :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bElogg FOR Elogg.
    ARTPRIS:
    DO:
        FOR EACH ELogg WHERE ELogg.TabellNavn = "ArtPris" AND
                             ELogg.EksterntSystem = cEDBSystem NO-LOCK:
            IF NOT NUM-ENTRIES(ELogg.Verdier,CHR(1)) > 1 THEN
                NEXT.

            /*FIND butiker WHERE butiker.profilnr = INT(ENTRY(2,Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.*/
            BUTIKKER:
            FOR EACH Butiker NO-LOCK WHERE
              Butiker.ProfilNr = INT(ENTRY(2,Elogg.Verdier,CHR(1))):
                  
                IF cFtpButiker <> '' THEN 
                  IF NOT CAN-DO(cFtpButiker,STRING(Butiker.Butik)) THEN
                      NEXT BUTIKKER.
                  
                IF AVAIL butiker THEN DO:
                    FIND artpris WHERE artpris.artikkelnr = DECI(ENTRY(1,Elogg.Verdier,CHR(1))) AND artpris.profilnr = butiker.profilnr NO-LOCK NO-ERROR.
                    IF NOT AVAIL artpris THEN
                        FIND artpris WHERE artpris.artikkelnr = DECI(ENTRY(1,Elogg.Verdier,CHR(1))) AND artpris.profilnr = iCLprofilnr NO-LOCK NO-ERROR.
            
                    IF AVAIL artpris THEN DO:
                        FIND TT_MasterButik WHERE TT_MasterButik.butik = butiker.butik NO-ERROR.
                        IF NOT AVAIL TT_MasterButik THEN DO:
                            CREATE TT_MasterButik.
                            ASSIGN TT_MasterButik.butik = butiker.butik
                                   TT_MasterButik.profilnr = butiker.profilnr.
                        END.
                        IF NOT CAN-FIND(TT_Master WHERE
                                        TT_Master.butik      = butiker.butik AND
                                        TT_Master.Verdier = ENTRY(1,ELogg.Verdier,CHR(1))) THEN 
                        DO:
                            CREATE TT_Master.
                            ASSIGN TT_Master.butik      = Butiker.butik
                                   TT_Master.Verdier = ENTRY(1,ELogg.Verdier,CHR(1))
                            NO-ERROR.
                        END.  
                    END.
            
                END.
            END. /* BUTIKKER */
            
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Master THEN
                RELEASE TT_Master.
        END.
    END. /* ARTPRIS */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaEloggAlle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaEloggAlle Procedure 
PROCEDURE SkapaEloggAlle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH ArtBas WHERE ArtBas.IKasse = TRUE NO-LOCK:
      FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
      IF NOT AVAIL VarGr THEN
          NEXT.
      FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
      IF NOT AVAIL HuvGr OR HuvGr.AvdelingNr = 1 THEN
          NEXT.
      FIND TT_ELogg WHERE 
           TT_ELogg.TabellNavn     = "ArtBas" AND
           TT_ELogg.EksterntSystem = cEDBSystem    AND
           TT_ELogg.Verdier        = STRING(ArtBas.ArtikkelNr) NO-ERROR.
      IF NOT AVAIL TT_ELogg THEN DO:
          CREATE TT_ELogg.
          ASSIGN TT_ELogg.TabellNavn     = "ArtBas"
                 TT_ELogg.EksterntSystem = cEDBSystem   
                 TT_ELogg.Verdier        = STRING(ArtBas.ArtikkelNr).
      END.
      ASSIGN TT_ELogg.EndringsType = 1 
             TT_ELogg.Behandlet    = FALSE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaEloggKampanj) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaEloggKampanj Procedure 
PROCEDURE SkapaEloggKampanj :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND KampanjeMixMatch WHERE KampanjeMixMatch.KampId = dKampId NO-LOCK NO-ERROR.
    IF NOT AVAIL KampanjeMixMatch THEN
        RETURN.
    FOR EACH KampanjeTilbud OF KampanjeMixMatch NO-LOCK:
        FOR EACH KampanjeTilbArtikkel OF Kampanjetilbud NO-LOCK:
            FIND Produktfamilie OF KampanjeTilbArtikkel NO-LOCK NO-ERROR.
            IF NOT AVAIL Produktfamilie THEN
                NEXT.
            IF KampanjeTilbArtikkel.KampTilbArtId <> 0 THEN DO:
                FIND TT_ELogg WHERE 
                     TT_ELogg.TabellNavn     = "ArtBas" AND
                     TT_ELogg.EksterntSystem = cEDBSystem    AND
                     TT_ELogg.Verdier        = STRING(KampanjeTilbArtikkel.KampTilbArtId) NO-ERROR.
                IF NOT AVAIL TT_ELogg THEN DO:
                    CREATE TT_ELogg.
                    ASSIGN TT_ELogg.TabellNavn     = "ArtBas"
                           TT_ELogg.EksterntSystem = cEDBSystem   
                           TT_ELogg.Verdier        = STRING(KampanjeTilbArtikkel.KampTilbArtId).
                END.
                ASSIGN TT_ELogg.EndringsType = 1
                       TT_ELogg.Behandlet    = FALSE.
            END.
            ELSE DO:
                FOR EACH ProduktFamMedlem OF Produktfamilie NO-LOCK:
                    FIND TT_ELogg WHERE 
                         TT_ELogg.TabellNavn     = "ArtBas" AND
                         TT_ELogg.EksterntSystem = cEDBSystem    AND
                         TT_ELogg.Verdier        = STRING(ProduktFamMedlem.ProdFamArtikkelNr) NO-ERROR.
                    IF NOT AVAIL TT_ELogg THEN DO:
                        CREATE TT_ELogg.
                        ASSIGN TT_ELogg.TabellNavn     = "ArtBas"
                               TT_ELogg.EksterntSystem = cEDBSystem   
                               TT_ELogg.Verdier        = STRING(ProduktFamMedlem.ProdFamArtikkelNr).
                    END.
                    ASSIGN TT_ELogg.EndringsType = 1
                           TT_ELogg.Behandlet    = FALSE.

                END.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaTT_ELoggButik) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTT_ELoggButik Procedure 
PROCEDURE SkapaTT_ELoggButik :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CREATE TT_FtpButiker.
    ASSIGN TT_FtpButiker.ButikkNr = TT_MasterButik.butik.
    ASSIGN hButikker = BUFFER TT_FtpButiker:HANDLE.

    FOR EACH TT_Master WHERE TT_Master.butik = TT_MasterButik.butik:
        CREATE TT_Elogg.
        ASSIGN TT_ELogg.TabellNavn     = 'ArtBas'
               TT_ELogg.EksterntSystem = cEDBSystem
               TT_ELogg.Verdier        = ENTRY(1,TT_Master.Verdier,CHR(1))
               TT_ELogg.EndringsType   = 1.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaTT_HappyHour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTT_HappyHour Procedure 
PROCEDURE SkapaTT_HappyHour :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipHappyHourId LIKE HappyHourHode.HapHourId NO-UNDO.
    DEFINE        VARIABLE ii     AS INTEGER    NO-UNDO.
    DEFINE        VARIABLE iAddId AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cWeekDay AS CHARACTER  NO-UNDO.
    IF CAN-FIND(TT_HappyHourHode WHERE TT_HappyHourHode.happyhourId = ipHappyHourId) THEN
        NEXT.
    FIND HappyHourHode WHERE HappyHourHode.HapHourId = ipHappyHourId NO-LOCK NO-ERROR.
    IF AVAIL HappyHourHode AND CAN-FIND(FIRST HappyHourPeriode OF HappyHourHode) THEN DO:
        CREATE TT_HappyHourHode.
        ASSIGN TT_HappyHourHode.happyhourId = HappyHourHode.HapHourId
               TT_HappyHourHode.action      = "update"
               TT_HappyHourHode.NAME        = HappyHourHode.HapHourNavn.
        FOR EACH HappyHourPeriode OF HappyHourHode NO-LOCK:
            DO ii = 1 TO NUM-ENTRIES(HappyHourPeriode.HapHourPerUkedagListe):
                iAddId = IF NUM-ENTRIES(HappyHourPeriode.HapHourPerUkedagListe) = 7 THEN 0 ELSE INT(ENTRY(ii,HappyHourPeriode.HapHourPerUkedagListe)).
                cWeekDay = getWeekDay(iAddId).
                CREATE TT_HappyHourPeriode.
                ASSIGN TT_HappyHourPeriode.happyhourId       = HappyHourPeriode.HapHourId
                       TT_HappyHourPeriode.happyhourPeriodId = HappyHourPeriode.HapHourPerId + iAddId
                       TT_HappyHourPeriode.action            = "update"
                       TT_HappyHourPeriode.DayOfWeek         = cWeekDay /* HappyHourPeriode.HapHourPerUkedagListe */
                       TT_HappyHourPeriode.StartTime         = STRING(HappyHourPeriode.HapHourPerStartTid,"HH:MM:SS")
                       TT_HappyHourPeriode.EndTime           = STRING(HappyHourPeriode.HapHourPerSluttTid,"HH:MM:SS").
                IF iAddId = 0 THEN /* här har vi alla dagar och då lägger vi bara ut 1 TT_H utan DayOfWeek */
                    LEAVE.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaTT_ItemFamily) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTT_ItemFamily Procedure 
PROCEDURE SkapaTT_ItemFamily :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipProdFamId LIKE KampanjeTilbArtikkel.ProdFamId  NO-UNDO.
    DEFINE INPUT  PARAMETER ipArtikkelnr                       AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dUtleggArtnr                               AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dUtleggArtNrTMP                            AS DECIMAL    NO-UNDO.
    IF CAN-FIND(TT_ItemFamily WHERE TT_ItemFamily.itemFamilyId = ipProdFamId) THEN
        RETURN.
    
    FIND ProduktFamilie WHERE ProduktFamilie.Prodfamid = ipprodfamid NO-LOCK NO-ERROR.
    IF NOT AVAIL produktfamilie THEN
        RETURN.
    CREATE tt_itemfamily.
    ASSIGN tt_itemfamily.itemfamilyid = ipProdfamid
           tt_itemfamily.action = "update"
           tt_itemfamily.NAME   = SUBSTR(ProduktFamilie.ProdFamNavn,1,50) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        DELETE tt_itemfamily.
        RETURN.
    END.
    IF ipArtikkelnr <> 0 THEN DO:
        IF AVAIL TT_itemfamily AND tt_itemfamily.NAME = "" THEN DO:
            FIND artbas WHERE artbas.artikkelnr = ipArtikkelnr NO-LOCK NO-ERROR.
            IF AVAIL Artbas THEN
                tt_itemfamily.NAME = IF artbas.bongtekst <> "" THEN SUBSTR(Artbas.bongtekst,1,50) ELSE
                                                                    SUBSTR(Artbas.beskr,1,50).
        END.
        dUtleggArtnr = ipArtikkelnr.
        dUtleggArtNrTMP = getDrivstoffArtnr(dUtleggArtnr).
        IF dUtleggArtNrTMP > 0 THEN
            dUtleggArtnr = dUtleggArtNrTMP.
        CREATE TT_FamilyMember.
        ASSIGN TT_FamilyMember.itemfamilyid = ipProdFamid
               TT_FamilyMember.action = "update"
               TT_FamilyMember.itemid = STRING(dUtleggArtnr) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_FamilyMember.
        IF AVAIL TT_FamilyMember THEN
            ASSIGN TT_FamilyMember.itemidExport = STRING(getItemIdexport(DECI(TT_FamilyMember.itemid))).
    END.
    ELSE DO:
        FOR EACH produktfammedlem OF produktfamilie NO-LOCK.
            dUtleggArtnr = ProduktFamMedlem.ProdFamArtikkelNr.
            dUtleggArtNrTMP = getDrivstoffArtnr(dUtleggArtnr).
            IF dUtleggArtNrTMP > 0 THEN
                dUtleggArtnr = dUtleggArtNrTMP.
            CREATE TT_FamilyMember.
            ASSIGN TT_FamilyMember.itemfamilyid = ipProdFamid
                   TT_FamilyMember.action = "update"
                   TT_FamilyMember.itemid = STRING(dUtleggArtnr) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE TT_FamilyMember.
            IF AVAIL TT_FamilyMember THEN
                ASSIGN TT_FamilyMember.itemidExport = STRING(getItemIdexport(DECI(TT_FamilyMember.itemid))).
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettTTmedALLE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTTmedALLE Procedure 
PROCEDURE SlettTTmedALLE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH TT_ELogg WHERE TT_ELogg.Verdier = "ALLE":
      DELETE TT_ELogg.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettTT_ELoggVare) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTT_ELoggVare Procedure 
PROCEDURE SlettTT_ELoggVare :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH TT_ELogg WHERE 
              TT_ELogg.TabellNavn     = "ArtBas" AND
              TT_ELogg.EksterntSystem = cEDBSystem:
      DELETE TT_Elogg.
  END.
  FOR EACH TT_ELogg WHERE 
              TT_ELogg.TabellNavn     = "Pakkelinje" AND
              TT_ELogg.EksterntSystem = cEDBSystem:
      DELETE TT_Elogg.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UpdateCampaign) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateCampaign Procedure 
PROCEDURE UpdateCampaign :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND KampanjeMixMatch WHERE KampanjeMixMatch.KampId = dKampId NO-LOCK NO-ERROR.
    IF NOT AVAIL KampanjeMixMatch THEN
        RETURN.
    
    /* Skapa TT_CampaignOwner */
    FIND KampanjeEier OF KampanjeMixMatch NO-LOCK.
    iCampaignOwnerId   = KampanjeEier.KampEierId.
    cCampaignOwnerName = SUBSTR(KampanjeEier.KampEierNavn,1,50).

    /* Skapa TT_Campaign */
    CREATE TT_Campaign.
    ASSIGN TT_Campaign.campaignId      = dKampId
           TT_Campaign.action          = "update"
           TT_Campaign.Name            = SUBSTR(KampanjeMixMatch.KampNavn,1,50)
           TT_Campaign.CampaignOwnerId = STRING(iCamPaignOwnerId)
           TT_Campaign.StartDate       = IF KampanjeMixMatch.KampStartDato <> ? THEN
                               STRING(YEAR(KampanjeMixMatch.KampStartDato),"9999") + "-" +     
                               STRING(MONTH(KampanjeMixMatch.KampStartDato),"99")  + "-" +     
                               STRING(DAY(KampanjeMixMatch.KampStartDato),"99") ELSE "" /* StartDate*/
           TT_Campaign.StartTime       = IF KampanjeMixMatch.KampStartDato <> ? THEN 
                                                  STRING(KampanjeMixMatch.KampStartTid,"HH:MM:SS") ELSE ""
           TT_Campaign.EndDate         = IF KampanjeMixMatch.KampSluttDato <> ? THEN
                                              STRING(YEAR(KampanjeMixMatch.KampSluttDato),"9999") + "-" +   
                                              STRING(MONTH(KampanjeMixMatch.KampSluttDato),"99")  + "-" +   
                                              STRING(DAY(KampanjeMixMatch.KampSluttDato),"99") ELSE "" /* EndDate*/ 
           TT_Campaign.EndTime         = IF KampanjeMixMatch.KampSluttDato <> ? THEN
                                 STRING(KampanjeMixMatch.KampSluttTid,"HH:MM:SS") ELSE "". /* EndTime*/

    FOR EACH KampanjeTilbud OF KampanjeMixMatch NO-LOCK:
        IF NOT CAN-FIND(FIRST KampanjeTilbArtikkel OF KampanjeTilbud) THEN
            NEXT.
        CREATE TT_Promotion.
        ASSIGN TT_Promotion.campaignId         = TT_Campaign.campaignId
               TT_Promotion.promotionId        = KampanjeTilbud.KampTilbId
               TT_Promotion.action             = "update"
               TT_Promotion.HappyHourId        = IF KampanjeTilbud.HapHourId > 0 THEN STRING(KampanjeTilbud.HapHourId) ELSE ""
               TT_Promotion.Type               = ENTRY(KampanjeTilbud.KampTilbTypeId,"pay,save,percent,payfor")
               TT_Promotion.ReceiptText        = IF KampanjeTilbud.KampTilbKvitteringstekst = "" THEN "-" ELSE SUBSTR(KampanjeTilbud.KampTilbKvitteringstekst,1,14)
               TT_Promotion.PopUpText          = IF KampanjeTilbud.KamptilbPopUpTekstBruk THEN SUBSTR(KampanjeTilbud.KamptilbPopUpTekst,1,30) ELSE ""
               TT_Promotion.LimitCount         = IF KampanjeTilbud.KamptilbGrenseAntall > 0 THEN STRING(KampanjeTilbud.KamptilbGrenseAntall) ELSE ""
               TT_Promotion.Amount             = IF KampanjeTilbud.KampTilbBelop > 0 THEN REPLACE(STRING(KampanjeTilbud.KampTilbBelop,">>>>>9.9999"),",",".") ELSE ""
               TT_Promotion.PercentIncrement   = IF TT_Promotion.TYPE = "percent" OR TT_Promotion.TYPE = "save"
                                                        THEN STRING(KampanjeTilbud.KampTilbOkning,"1/0") ELSE ""
               TT_Promotion.ProportionalPayFor = IF TT_Promotion.TYPE = "payfor" THEN STRING(KampanjeTilbud.KampTilbPropBetalFor,"1/0") ELSE "".
        IF KampanjeTilbud.HapHourId <> 0 AND CAN-FIND(HappyHourHode OF KampanjeTilbud) THEN DO:
            RUN SkapaTT_HappyHour (KampanjeTilbud.HapHourId).
        END.
        FOR EACH KampanjeTilbArtikkel OF Kampanjetilbud NO-LOCK:
            FIND Produktfamilie OF KampanjeTilbArtikkel NO-LOCK NO-ERROR.
            IF NOT AVAIL Produktfamilie THEN
                NEXT.
            RUN SkapaTT_ItemFamily (KampanjeTilbArtikkel.ProdFamId,KampanjeTilbArtikkel.KampTilbArtId).
            CREATE TT_PromotionItem.
            ASSIGN TT_PromotionItem.campaignId         = TT_Promotion.campaignId
                   TT_PromotionItem.promotionId        = KampanjeTilbArtikkel.KampTilbId
                   TT_PromotionItem.promotionItemId    = KampanjeTilbArtikkel.KampTilbArtSeq
                   TT_PromotionItem.action             = "update"
                   TT_PromotionItem.ItemTrigger        = REPLACE(STRING(KampanjeTilbArtikkel.KampTilbArtMinAntall,">>>>>>>9.9999"),",",".")
                   TT_PromotionItem.Amount             = REPLACE(STRING(KampanjeTilbArtikkel.KampTilbArtBelop,">>>>>9.9999"),",",".")
                   TT_PromotionItem.SaveItemFree       = STRING(KampanjeTilbArtikkel.KampRabattTypeId = 2,"1/0")
                   TT_PromotionItem.SuppressDiscount   = STRING(KampanjeTilbArtikkel.KampRabattTypeId = 3,"1/0")
                   TT_PromotionItem.ItemFamilyId       = STRING(KampanjeTilbArtikkel.ProdFamId).
        END.
    END.

    /* Skapa TT_HappyHour     */
    
    
    /* Skapa TT_ItemFamilyConfig */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UpdateItemBarCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateItemBarCode Procedure 
PROCEDURE UpdateItemBarCode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lSlettes AS LOGICAL    NO-UNDO.
    DEFINE BUFFER bufArt FOR Artbas.
    FOR EACH TT_Item:
        FIND ArtBas WHERE ArtBas.ArtikkelNr = TT_Item.itemId NO-LOCK NO-ERROR.
        IF NOT AVAIL ArtBas THEN 
            ASSIGN TT_Item.action = 0.
        ELSE FINNES: DO:
            FIND varGr OF ArtBas NO-LOCK NO-ERROR.
            IF NOT AVAIL VarGr THEN DO:
                FOR EACH TT_BarCode WHERE TT_BarCode.itemId = TT_Item.itemId:
                    DELETE TT_BarCode.
                END.
                DELETE TT_Item.
                LEAVE FINNES.
            END.
            FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
            IF NOT AVAIL HuvGr THEN DO:
                FOR EACH TT_BarCode WHERE TT_BarCode.itemId = TT_Item.itemId:
                    DELETE TT_BarCode.
                END.
                DELETE TT_Item.
                LEAVE FINNES.
            END.
            IF HuvGr.Avdelingnr = 1 THEN DO:
                FOR EACH TT_BarCode WHERE TT_BarCode.itemId = TT_Item.itemId:
                    DELETE TT_BarCode.
                END.
                DELETE TT_Item.
                LEAVE FINNES.
            END.
            IF ArtBas.Utgatt = TRUE OR ArtBas.IKasse = FALSE THEN DO:
                /* deaktivera alla streckkkode */
                FOR EACH StrekKode OF artbas NO-LOCK:
                    RUN CreateBarCode (TT_Item.itemId,StrekKode.Kode,0).
                END.
                ASSIGN TT_Item.action = 0.
            END.
            ELSE DO:
                FIND Moms OF VarGr NO-LOCK NO-ERROR.
                IF NOT AVAIL Moms THEN DO:
                    FOR EACH TT_BarCode WHERE TT_BarCode.itemId = TT_Item.itemId:
                        DELETE TT_BarCode.
                    END.
                    DELETE TT_Item.
                    LEAVE FINNES.
                END.
                /* Här skall vi uppdatera TT_Item */
                ASSIGN TT_Item.action              = 1
                       TT_Item.Name                = artbas.beskr
                       TT_Item.itemType            = IF artbas.lager = TRUE THEN "sku" ELSE "nonsku" /* weight ? */
/*                        TT_Item.AllowPriceOverride  = */
                       TT_Item.IsDeposit           = artbas.pant
                       TT_Item.MDallowed           = ArtBas.ManRabIKas
                       TT_Item.MDdiscountOverride  = FALSE
                       TT_Item.MDunitDiscount      = 0
                       TT_Item.MDpercentage        = 0
                       TT_Item.SCuom               = "0.00"
                       TT_Item.SRCuom              = "0.00"
                       TT_Item.ProductOwnerId      = 0
                       TT_Item.AGsalesType         = "takeaway" /* eatin */
                       TT_Item.AGaction            = "update"
                       TT_Item.AccountId           = VarGr.Hg
                       TT_Item.TaxClass            = IF Moms.MomsProc = 0 THEN 10 ELSE moms.momskod
/*                        TT_Item.TaxClass            = moms.momskod */
                       TT_Item.EnterpriseId        = 0 /* läggs inte ut */
                       TT_Item.UserDefinedId       = 0 /* läggs inte ut */
                       TT_Item.EFTCodeId           = 0 /* läggs inte ut */
                       TT_Item.OPris               = Artbas.OPris /*    */
                       TT_Item.depositlink         = 0.
                /*       
                IF Artbas.LinkVareNr > 0 AND CAN-FIND(bufArt WHERE bufArt.Artikkelnr = Artbas.LinkVareNr AND Artbas.pant = TRUE) THEN
                       TT_Item.depositlink         = getItemIdexport(Artbas.LinkVareNr).
                */
                /* Här skall vi skapa TT_Artpris */
                IF NOT AVAIL TT_MasterButik THEN DO:
                    FOR EACH artpris WHERE artpris.artikkelnr = TT_Item.itemId NO-LOCK:
                        CREATE TT_ArtPris.
                        ASSIGN TT_ArtPris.ArtikkelNr   = artbas.artikkelnr
                               TT_ArtPris.Profilnr     = artpris.profilnr
                               TT_ArtPris.Tilbud       = artpris.tilbud
                               TT_ArtPris.TilbudFra    = IF artpris.tilbud THEN ArtPris.TilbudFraDato ELSE ?
                               TT_ArtPris.TilbudFraTid = IF artpris.tilbud THEN ArtPris.TilbudFraTid ELSE 0
                               TT_ArtPris.TilbudTil    = IF artpris.tilbud THEN ArtPris.TilbudTilDato ELSE ?
                               TT_ArtPris.TilbudTilTid = IF artpris.tilbud THEN ArtPris.TilbudTilTid ELSE 0
                               TT_ArtPris.NPris        = IF TT_Item.OPris THEN 0 ELSE ArtPris.Pris[1]
                               TT_ArtPris.TPris        = IF TT_Item.OPris THEN 0 ELSE ArtPris.Pris[2].
                    END.
                END.
                ELSE DO:
                    FIND artpris WHERE artpris.artikkelnr = TT_Item.itemId AND artpris.profilnr = TT_Masterbutik.profilnr NO-LOCK NO-ERROR.
                    IF NOT AVAIL artpris THEN
                        FIND artpris WHERE artpris.artikkelnr = TT_Item.itemId AND artpris.profilnr = iCLprofilnr NO-LOCK. /* testat om den finns tidigare */
                    CREATE TT_ArtPris.
                    ASSIGN TT_ArtPris.ArtikkelNr   = artbas.artikkelnr
                           TT_ArtPris.Profilnr     = artpris.profilnr
                           TT_ArtPris.Tilbud       = artpris.tilbud
                           TT_ArtPris.TilbudFra    = IF artpris.tilbud THEN ArtPris.TilbudFraDato ELSE ?
                           TT_ArtPris.TilbudFraTid = IF artpris.tilbud THEN ArtPris.TilbudFraTid ELSE 0
                           TT_ArtPris.TilbudTil    = IF artpris.tilbud THEN ArtPris.TilbudTilDato ELSE ?
                           TT_ArtPris.TilbudTilTid = IF artpris.tilbud THEN ArtPris.TilbudTilTid ELSE 0
                           TT_ArtPris.NPris        = IF TT_Item.OPris THEN 0 ELSE ArtPris.Pris[1]
                           TT_ArtPris.TPris        = IF TT_Item.OPris THEN 0 ELSE ArtPris.Pris[2].
                END.
                /* Här skall vi skapa BarCode, tänk på att vi kan ha ikasse = false även här */
                FOR EACH StrekKode OF ArtBas NO-LOCK.
                    RUN CreateBarCode (TT_Item.itemId,StrekKode.kode,IF StrekKode.IKasse THEN 1 ELSE 0).
                END.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getDrivstoffArtnr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDrivstoffArtnr Procedure 
FUNCTION getDrivstoffArtnr RETURNS DECIMAL
  ( INPUT ipArtikkelnr AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dTmp AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dRetArtNr AS DECIMAL    NO-UNDO.
        FIND Artbas WHERE artbas.artikkelnr = ipArtikkelnr NO-LOCK NO-ERROR.
        IF AVAIL Artbas THEN DO:
            FIND Vargr OF Artbas NO-LOCK NO-ERROR.
            IF AVAIL VarGr THEN DO:
                FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
                IF AVAIL HuvGr AND HuvGr.Avdelingnr = 1 THEN DO:
                    FOR EACH StrekKode OF ArtBas NO-LOCK:
                        IF LENGTH(TRIM(StrekKode.Bestillingsnummer)) > 0 THEN DO:
                            dTmp = DECI(StrekKode.Bestillingsnummer) NO-ERROR.
                            IF NOT ERROR-STATUS:ERROR THEN DO:
                                dRetArtNr = dTmp.
                                LEAVE.
                            END.
                        END.
                    END.
                END.
            END.
        END.

  RETURN dRetArtNr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getItemIdexport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getItemIdexport Procedure 
FUNCTION getItemIdexport RETURNS DECIMAL
  ( INPUT dItemId AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cItem AS CHARACTER  NO-UNDO.
  cItem = STRING(dItemId).
  IF cItem BEGINS "2000" AND LENGTH(cItem) = 11 THEN
     cItem = "2" + SUBSTR(cItem,3).
  ELSE IF LENGTH(cItem) = 11 AND SUBSTR(cItem,4,2) = "00" THEN
      cItem = SUBSTR(cItem,1,3) + SUBSTR(cItem,6).
  RETURN DECI(cItem).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLinkNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLinkNr Procedure 
FUNCTION getLinkNr RETURNS DECIMAL
  ( INPUT dLinkNr AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER bStrekKode FOR StrekKode.
  FIND FIRST bStrekkode WHERE bStrekKode.ArtikkelNr = dLinkNr AND
                              bStrekKode.HovedNr    = TRUE NO-LOCK NO-ERROR.
  IF NOT AVAIL bStrekKode THEN
      FIND FIRST bStrekkode WHERE bStrekKode.ArtikkelNr = dLinkNr AND
                                  bStrekKode.KodeType = 0 NO-LOCK NO-ERROR.
  IF NOT AVAIL bStrekKode THEN
      FIND FIRST bStrekkode WHERE bStrekKode.ArtikkelNr = dLinkNr NO-LOCK NO-ERROR.
  
  RETURN IF AVAIL bStrekKode THEN DECI(bStrekKode.kode) ELSE 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPriceBookfilnamn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPriceBookfilnamn Procedure 
FUNCTION getPriceBookfilnamn RETURNS LOGICAL
  ( INPUT cDir AS CHARACTER,INPUT iBut AS INTEGER,OUTPUT cPBfil AS CHARACTER,OUTPUT iSeqNr AS INTEGER,OUTPUT cPBBkufil AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cButDir AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cSeqFil AS CHARACTER  NO-UNDO.
/*     DEFINE VARIABLE iSeqNr  AS INTEGER    NO-UNDO. */
    DEFINE VARIABLE cTxt AS CHARACTER  NO-UNDO.

    ASSIGN cButDir = cDir + STRING(iBut)
           cSeqFil = cButDir + "\" + "seqnr.txt".
    IF SEARCH(cSeqFil) = ? THEN DO:
        OS-COMMAND SILENT VALUE("md " + cButDir).
        iSeqNr = 1.
        OUTPUT TO VALUE(cSeqFil).
        OUTPUT CLOSE.
    END.
    ELSE DO:
        INPUT FROM value(cSeqFil) NO-ECHO.
        IMPORT UNFORMATTED cTxt.
        iSeqNr = INT(cTxt) + 1.
        INPUT CLOSE.
    END.
    IF SEARCH(cSeqFil) = ? THEN DO:
        RETURN FALSE.
    END.
    /* Sikrer at backup katalog finnes. */
    OS-CREATE-DIR value(cButDir + "\bku").
    
    ASSIGN 
      cPBfil    = cButDir + "\" + "Pricebook_" + STRING(iSeqNr) + ".xml"
      cPBBkufil = cButDir + "\bku\" + "Pricebook_" + STRING(iSeqNr) + ".xml"
      .
    OUTPUT TO VALUE(cSeqFil).
    PUT UNFORMATTED iSeqNr SKIP.
    OUTPUT CLOSE.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWeekDay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWeekDay Procedure 
FUNCTION getWeekDay RETURNS CHARACTER
  ( INPUT iWeekDay AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  RETURN IF iWeekDay = 0 THEN "" ELSE ENTRY(iWeekDay,"sunday,monday,tuesday,wednesday,thursday,friday,saturday").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

