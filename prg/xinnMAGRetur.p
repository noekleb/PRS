&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xinnMAGRetur.p
    Purpose     :

    Syntax      :

    Description : Leser inn returordre fra nettbutikk.

    Author(s)   : Tom Nøkleby
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
DEF VAR cFilXsdNavn   AS CHAR NO-UNDO.
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
DEFINE TEMP-TABLE orderHeader NO-UNDO SERIALIZE-NAME "returnRow"
    FIELD id AS CHAR INIT ?
    FIELD orderID AS CHAR 
    FIELD returnID AS CHARACTER 
    FIELD note AS CHARACTER 
    INDEX Idx1 AS PRIMARY UNIQUE orderID.

DEFINE TEMP-TABLE orderItem NO-UNDO SERIALIZE-NAME "itemRow"
    FIELD orderID AS CHAR INIT ?
    FIELD itemID AS CHAR 
    FIELD sku AS CHAR 
    FIELD qty AS DEC 
    INDEX Idx1 AS PRIMARY orderID.


DEFINE VARIABLE hDset AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE OrdHuvud NO-UNDO
    FIELD orderID AS CHAR 
    FIELD returnID AS CHARACTER 
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

DEFINE BUFFER bNettbutikk FOR Butiker.
DEFINE BUFFER bSentrallager FOR Butiker.
DEFINE BUFFER bKOrdreHode FOR KORdreHode.
DEFINE BUFFER bKOrdreLinje FOR KORdreLinje.

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
    cFilNavn    = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn
    cFilXsdNavn = VPIFilHode.Katalog + "~\" + 'ProcessEComReturnRequest.xsd'
    cBkuFil     = VPIFilHode.Katalog + "~\bku~\" + VPIFilHode.FilNavn.

RUN LesInnFil.

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


  RUN xmlReadMagRetur.

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

&IF DEFINED(EXCLUDE-xmlReadMagRetur) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE xmlReadMagRetur Procedure 
PROCEDURE xmlReadMagRetur :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE icnt       AS INTEGER  NO-UNDO. 
DEFINE VARIABLE iaar AS INTEGER     NO-UNDO.
DEFINE VARIABLE iman AS INTEGER     NO-UNDO.
DEFINE VARIABLE idag AS INTEGER     NO-UNDO.
DEFINE VARIABLE cKode AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDec  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iLevFNr AS INTEGER NO-UNDO.
DEFINE VARIABLE hRelation AS HANDLE NO-UNDO. 
DEFINE VARIABLE iKOrdreLinjeNr LIKE KOrdreLinje.KOrdreLinjeNr NO-UNDO.
DEFINE VARIABLE lSum AS DECIMAL NO-UNDO.
DEFINE VARIABLE lMvaKr AS DECIMAL NO-UNDO.

CREATE DATASET hDset.
hDset:NAME = "ProcessEComReturnRequest". 

hDset:ADD-BUFFER(BUFFER orderHeader:HANDLE).
hDset:ADD-BUFFER(BUFFER orderItem:HANDLE).      

hRelation = hDset:ADD-RELATION(BUFFER OrderHeader:HANDLE,BUFFER  orderItem:HANDLE,
          "orderID,orderID",
            TRUE   /* REPOSITION */,
            TRUE   /*nested*/ ,
            TRUE  /* not-active*/ ,
            FALSE  /* recursive */ ,
            TRUE   /* foreign key hidden */ ).

hDset:WRITE-XMLSCHEMA("file",cFilXsdNavn,TRUE,?,FALSE,TRUE). 
hDset:READ-XML       ('file',cFilNavn,?,?,?,?,?) .

/* ---- for testing --- */
FOR EACH OrderHeader: 
   CREATE OrdHuvud.
   ASSIGN 
   OrdHuvud.orderID        = orderHeader.orderId
   OrdHuvud.returnID       = orderHeader.returnID 
   OrdHuvud.shippingMethod = 'Postpakke'
   .       
END. 


/*
FOR EACH orderHeader: 
   CREATE OrdHuvud.
   ASSIGN 
   OrdHuvud.orderID  = orderHeader.orderId
   OrdHuvud.returnID = orderHeader.returnID 
   .
END.
*/
ORDREHODE:
FOR EACH OrdHuvud WHERE OrdHuvud.OrderId <> '':
    ASSIGN
      iKOrdreLinjeNr = 0
      lSum           = 0
      lMvaKr         = 0.
    FIND FIRST bKordreHode NO-LOCK WHERE 
      bKOrdreHode.EkstORdreNr = OrdHuvud.OrderId NO-ERROR.
    IF AVAILABLE bKOrdreHode THEN 
    DO:
        IF NOT CAN-FIND(FIRST KOrdreHode WHERE 
                              KOrdreHode.EkstOrdreNr = 'RETUR ' + OrdHuvud.returnID) THEN 
        OPPRETT_RETUR:
        DO:
            CREATE  KORdreHode.
            BUFFER-COPY bKOrdreHode 
                  EXCEPT bKOrdreHode.KOrdre_Id
                  TO KOrdreHode
                  ASSIGN KOrdreHode.EkstOrdreNr       = 'RETUR ' + OrdHuvud.returnID
                         KOrdreHode.VerkstedMerknad   = 'Retur innlest ' + STRING(TODAY) + ' ' + STRING(TIME,'HH:MM:SS') 
                         KOrdreHode.RegistrertDato    = OrdHuvud.orderdate
                         KOrdreHode.RegistrertTid     = OrdHuvud.orderTid
                         KOrdreHode.RegistrertAv      = " "  /* /Shops/DemoShop/Users/magbyr*/
                         KOrdreHode.LevStatus         = "30" /* Bekreftet */   
                         KORdreHode.SendingsNr        = ''              
                  .
            END.
            ELSE DO: /* Oppretter returordre som ikke har noe opprinnelig ordre */
                CREATE KORdreHode.
                ASSIGN KOrdreHode.EkstOrdreNr       = 'RETUR ' + OrdHuvud.returnID
                       KOrdreHode.VerkstedMerknad   = 'Retur innlest ' + STRING(TODAY) + ' ' + STRING(TIME,'HH:MM:SS') 
                       KOrdreHode.RegistrertDato    = OrdHuvud.orderdate
                       KOrdreHode.RegistrertTid     = OrdHuvud.orderTid
                       KOrdreHode.RegistrertAv      = " "  /* /Shops/DemoShop/Users/magbyr*/
                       KOrdreHode.Opphav            = 10 /* Nettbutikk */
                       KOrdreHode.ButikkNr          = iNettbutikk
                       KOrdreHode.KasseNr           = (IF AVAILABLE Kasse THEN Kasse.KasseNr ELSE 0)
                       KOrdreHode.ForsNr            = (IF AVAILABLE Forsalj THEN Forsalj.ForsNr ELSE 0)
                       KOrdreHode.SelgerNr          = 0
                       KOrdreHode.LevStatus         = "30" /* Bekreftet */
                       .
            END.
            
            ORDREHODE:
            FOR EACH orderItem WHERE 
              OrdHuvud.orderID = orderItem.orderID NO-LOCK:
              
              ASSIGN 
                  iAntLinjer     = iAntLinjer + 1
                  iKOrdreLinjeNr = iKOrdreLinjeNr + 1.  
                  .
              ASSIGN lDec = DECIMAL(orderItem.sku) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN NEXT.
              
              cKode = STRING(orderItem.sku).
              RUN bibl_chkean.p (INPUT-OUTPUT cKode).
              FIND Strekkode NO-LOCK WHERE 
                Strekkode.Kode = cKode NO-ERROR.
              IF AVAILABLE Strekkode THEN 
              DO:
                FIND StrKonv NO-LOCK WHERE 
                    StrKonv.StrKode = Strekkode.StrKode NO-ERROR. 
                FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
              END.         
               IF AVAILABLE ArtBas THEN 
                 FIND ArtPris OF ArtBas NO-LOCK WHERE
                   ArtPris.ProfilNr = bNettButikk.ProfilNr NO-ERROR.
               IF NOT AVAILABLE ArtPris AND AVAILABLE ArtBas THEN 
                 FIND ArtPris OF ArtBas NO-LOCK WHERE
                   ArtPris.ProfilNr = bSentrallager.ProfilNr NO-ERROR.
               IF AVAILABLE bKOrdreHode AND AVAILABLE ArtBas THEN 
               DO:
                 IF AVAILABLE StrKonv THEN 
                   FIND FIRST bKOrdreLinje OF bKORdreHode NO-LOCK WHERE 
                     bKORdreLinje.VareNr = STRING(ArtBas.ArtikkelNr) AND 
                     bKORdreLinje.Storl  = StrKonv.Storl NO-ERROR.
                 ELSE 
                   FIND FIRST bKOrdreLinje OF bKORdreHode NO-LOCK WHERE 
                     bKORdreLinje.VareNr = STRING(ArtBas.ArtikkelNr) NO-ERROR.
                 IF AVAILABLE bKOrdreLinje THEN 
                 LINJEOPPRETTELSE:
                 DO:
                   CREATE KORdreLinje.
                   BUFFER-COPY bKORdreLinje
                     EXCEPT Kordre_ID KOrdreLinjeNr
                     TO KORdreLinje
                     ASSIGN 
                       KOrdreLinje.KORdre_Id     = KOrdreHode.KOrdre_Id
                       KORdreLinje.KOrdreLinjeNr = iKOrdreLinjeNr
                       .
                     ASSIGN
                       KOrdreLinje.Antall            = orderItem.qty * -1
                       KOrdreLinje.NettoLinjesum     = (KOrdreLinje.NettoPris * KOrdreLinje.Antall) + (KOrdreLinje.NettoPris * KOrdreLinje.Antall * (KORdreLinje.Mva% / 100))   
                       KOrdreLinje.BruttoPris        = KOrdreLinje.NettoPris + (KOrdreLinje.LinjeRabattKr / KOrdreLinje.Antall)  
                       KOrdreLinje.Pris              = KOrdreLinje.NettoPris + (KOrdreLinje.LinjeRabattKr / KOrdreLinje.Antall) 
                       KOrdreLinje.Linjesum          = KOrdreLinje.NettoLinjesum + KOrdreLinje.LinjeRabattKr 
                       KOrdreLinje.DbKr              = KOrdreLinje.NettoLinjesum - (KOrdreLinje.VareKost * KOrdreLinje.Antall)
                       KOrdreLinje.Db%               = (KOrdreLinje.DbKr / KOrdreLinje.NettoLinjesum) * 100
                       KOrdreLinje.Db%               = (IF KOrdreLinje.Db% = ? THEN 0 ELSE KOrdreLinje.Db%) 
                       lSum                          = lSum + KOrdreLinje.Linjesum
                       lMvaKr                        = lMvaKr + (KOrdreLinje.NettoPris * KOrdreLinje.Antall * (KORdreLinje.Mva% / 100)) 
                       .
                 END. /* LINJEOPPRETTELSE */
               END.
            END. /* ORDREHODE */
        END. /* OPPRETT_RETUR */      
END. /* ORDREHODE */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

