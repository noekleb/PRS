&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
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
DEFINE INPUT  PARAMETER hTTArt         AS HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER cQRY           AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cArtikkelNr    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cSourceButiker AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cButNamn        AS CHARACTER  FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE cColLabelString AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cPrintString    AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cTitle          AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cKundenavn      AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cPolygon        AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE iCL             AS INTEGER                   NO-UNDO.
DEFINE VARIABLE iCLProfilnr     AS INTEGER                   NO-UNDO.
DEFINE VARIABLE cButikkListe    AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE lNullposter     AS LOGICAL                   NO-UNDO.
DEFINE VARIABLE lNullager       AS LOGICAL                   NO-UNDO.
DEFINE VARIABLE iSortering      AS INTEGER                   NO-UNDO.
DEFINE VARIABLE lVisStr         AS LOGICAL                   NO-UNDO.
DEFINE VARIABLE dColPos         AS DECIMAL EXTENT 15         NO-UNDO.
DEFINE VARIABLE dColPos2        AS DECIMAL EXTENT 15         NO-UNDO.
DEFINE VARIABLE dColAdd         AS DECIMAL EXTENT 30         NO-UNDO.
DEFINE VARIABLE cRub            AS CHARACTER EXTENT 15       NO-UNDO.
DEFINE VARIABLE cSprak          AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cToday          AS CHARACTER FORMAT "X(10)"  NO-UNDO.
DEFINE VARIABLE cSidTxt         AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE dY              AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE iCount          AS INTEGER                   NO-UNDO.
DEFINE VARIABLE cUL             AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE iLen            AS INTEGER                   NO-UNDO.
DEFINE VARIABLE cTmpData        AS CHARACTER EXTENT 15       NO-UNDO.
DEFINE VARIABLE iCount2         AS INTEGER                   NO-UNDO.
DEFINE VARIABLE iCount3         AS INTEGER INIT 12           NO-UNDO.
DEFINE VARIABLE cTxt            AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cStrl           AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE pcRappFil       AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE lArtAnalys      AS LOGICAL     NO-UNDO.

/*    ["VG/Lopnr","Beskrivelse","Farge","Lev","Lev.art.nr","Ses","Varekost","Pris","Tilbud","Solgt",
    "Solgt verdi","Kjøpt","Kjøpt verdi","Lager","Lagerverdi"] NO-UNDO.*/
DEFINE VARIABLE iCols  AS INTEGER  EXTENT 15 INITIAL
    [6,13,27,36,39,46,49,56,63,70,74,85,90,100,105] NO-UNDO.
DEFINE VARIABLE iRight AS INTEGER EXTENT 15 INITIAL
    [0,0,0,0,0,0,1,1,1,1,1,1,1,1,1] NO-UNDO.
DEFINE VARIABLE c AS CHARACTER  NO-UNDO.
DEFINE VAR      dFraDato AS DATE NO-UNDO.
DEFINE VAR      dTilDato AS DATE NO-UNDO.
DEFINE VAR      lPeriode AS LOG  NO-UNDO.
/* vi skall använda denna för att se om anropande programmet har */
/* begränsningar i butiker genom RUN GetButiker IN SOURCE-PROCEDURE */


DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR hQuery  AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE TT_ArtLager
    FIELD ArtikkelNr  LIKE ArtBas.ArtikkelNr
    FIELD Vg          LIKE ArtBas.Vg
    FIELD Lopnr       LIKE ArtBas.Lopnr
    FIELD LevNr       LIKE ArtBas.LevNr
    FIELD Beskr       LIKE ArtBas.Beskr
    FIELD LevArtNr    LIKE ArtBas.LevKod
    FIELD AntSolgt    LIKE Lager.AntSolgt   
    FIELD VerdiSolgt  LIKE Lager.VerdiSolgt 
    FIELD KjopAnt     LIKE Lager.KjopAnt    
    FIELD KjopVerdi   LIKE Lager.KjopVerdi  
    FIELD LagAnt      LIKE Lager.LagAnt     
    FIELD AntRab      LIKE Lager.AntRab     
    FIELD VerdiRabatt LIKE Lager.VerdiRabatt
    FIELD VVarekost   LIKE Lager.VVarekost
    FIELD PrisOrdi    AS DECI DECIMALS 2
    FIELD PrisTilb    AS DECI DECIMALS 2
    INDEX LevNr IS PRIMARY LevNr Vg LopNr
    INDEX VgLopnr Vg Lopnr.

DEFINE TEMP-TABLE tmpTT_ArtLager LIKE TT_ArtLager
             FIELD Lagerverdi AS DECIMAL.
DEFINE TEMP-TABLE tmpTT_ArtLagerTOT LIKE TT_ArtLager
             FIELD Lagerverdi AS DECIMAL.

DEFINE TEMP-TABLE TT_KjopSalgLager
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    FIELD Butik      LIKE Butiker.Butik
    FIELD Storl      AS CHAR
    FIELD Antsolgt   AS CHAR
    FIELD KjopAnt    AS CHAR
    FIELD Lagant     AS CHAR
    FIELD VisEntry   AS CHAR
    FIELD iAntsolgt  AS INTE
    FIELD iKjopAnt   AS INTE
    FIELD iLagant    AS INTE
    INDEX ArtButTyp  IS PRIMARY UNIQUE ArtikkelNr Butik.

DEFINE VARIABLE cPerTxt   AS CHAR FORMAT "x(70)" NO-UNDO.

DEFINE FRAME PageHeader
   HEADER
      "<ALIGN=BASE><FArial><R3><P12><B><C6>" STRING(TODAY)
      "<R4><P12><C108><P9> Side: " PAGE-NUMBER FORMAT ">>" SKIP
      "<R4><C6><B>Butikk:" cButNamn "<C70>" cPerTxt "</B>" SKIP
      "<R5><C6><FROM><R5><C113><LINE>" SKIP
      WITH PAGE-TOP STREAM-IO WIDTH 255.

  { pdf_inc.i "THIS-PROCEDURE"}.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD bredd Procedure 
FUNCTION bredd RETURNS DECIMAL
  ( INPUT cText AS CHARACTER )  FORWARD.

/* {xPrint.i} */
{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-bredd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD bredd Procedure 
FUNCTION bredd RETURNS DECIMAL
    ( INPUT cText AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAntall) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAntall Procedure 
FUNCTION getAntall RETURNS DECIMAL
  ( INPUT lArtikkelNr AS DEC,
    INPUT iButikkNr   AS INT,
    INPUT cStorl      AS CHAR,
    INPUT iTTId       AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAntVerdi) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAntVerdi Procedure 
FUNCTION getAntVerdi RETURNS CHARACTER
    ( INPUT lArtikkelNr AS DEC,
      INPUT iButikkNr   AS INT,
      INPUT iTTId       AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDataLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDataLinje Procedure 
FUNCTION getDataLinje RETURNS CHARACTER
  ( INPUT iTyp AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKjopSolgtLagerLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKjopSolgtLagerLinje Procedure 
FUNCTION getKjopSolgtLagerLinje RETURNS CHARACTER
  ( INPUT iRad AS INTEGER )  FORWARD.

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
         HEIGHT             = 13.52
         WIDTH              = 59.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{syspara.i 1 1 100 cKundenavn}
{syspara.i 1 1 101 cPolygon}
{syspara.i 5 1 1 iCl INT}

DEFINE VARIABLE iAntal AS INTEGER     NO-UNDO.
/*ASSIGN cTitle = "Lagerliste".*/
/* Getbutiker frågar om det finns begränsningar i vilka butiker vi får se */
/* För att möjligöra Jbox-selecten har vi ett window
   RUN d-AvgrensLagerliste.w (OUTPUT lNullposter,OUTPUT iSortering,OUTPUT cButikkliste,INPUT cSourceButiker).
 */

FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
IF AVAIL bruker THEN
   cSprak = TRIM(Bruker.Lng).

IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
  ASSIGN cToday = STRING(YEAR(TODAY),"9999") + "-"
                + STRING(MONTH(TODAY),"99") + "-"
                + STRING(DAY(TODAY),"99").
ELSE
  ASSIGN cToday = STRING(TODAY).

IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
  ASSIGN cTitle = "Lagerlista"
         cRub[1] = "VG/Löpnr"
         cRub[2] = "Beskrivning"
         cRub[3] = "Färg"
         cRub[4] = "Lev"
         cRub[5] = "Lev.art.nr"
         cRub[6] = "Säs"
         cRub[7] = "Varukost"
         cRub[8] = "Pris"
         cRub[9] = "Kampanj"
         cRub[10] = "Sålt"
         cRub[11] = "Sålt värde"
         cRub[12] = "Köpt"
         cRub[13] = "Köpt värde"
         cRub[14] = "Lager"
         cRub[15] = "Lagervärde".
ELSE
  ASSIGN cTitle = "Lagerliste"
         cRub[1] = "VG/Lopnr"
         cRub[2] = "Beskrivelse"
         cRub[3] = "Farge"
         cRub[4] = "Lev"
         cRub[5] = "Lev.art.nr"
         cRub[6] = "Ses"
         cRub[7] = "Varekost"
         cRub[8] = "Pris"
         cRub[9] = "Tilbud"
         cRub[10] = "Solgt"
         cRub[11] = "Solgt verdi"
         cRub[12] = "Kjøpt"
         cRub[13] = "Kjøpt verdi"
         cRub[14] = "Lager"
         cRub[15] = "Lagerverdi".


RUN d-AvgrensLagerliste.w (OUTPUT lNullposter,
                           OUTPUT iSortering,
                           OUTPUT cButikkliste,
                           INPUT cSourceButiker,
                           OUTPUT dFraDato,
                           OUTPUT dTilDato,
                           OUTPUT lPeriode,
                           OUTPUT lVisStr,
                           OUTPUT lNullager,
                           OUTPUT lArtAnalys).

/*MESSAGE "lArtAnalys " lArtAnalys
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
RETURN.*/
IF RETURN-VALUE = "AVBRYT" THEN
    RETURN.
/*MESSAGE "hTTArt " hTTArt
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
MESSAGE "cQRY " cQRY
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
MESSAGE "cArtikkelNr " cArtikkelNr
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
MESSAGE "cSourceButiker " cSourceButiker
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

IF dFraDato = ? OR dTilDato = ? THEN
    lPeriode = FALSE.

FIND Butiker WHERE Butiker.Butik = iCL NO-LOCK NO-ERROR.
ASSIGN iCLProfilNr = Butiker.ProfilNr.
{sww.i}
IF lArtAnalys = TRUE THEN
  RUN ArtikelAnalys.p (INPUT hTTArt,
                             cQRY,
                             cArtikkelNr,
                             cSourceButiker,
                             lNullposter,
                             iSortering,
                             cButikkListe,
                             dFraDato,
                             dTilDato,
                             lPeriode,
                             lVisStr,
                             lNullager).

ELSE DO:
  RUN ByggTmpLager.
  RUN PDFSkrivLagerlisteX.
     END.
{swn.i}   

/* 
 DYNAMIC-FUNCTION('closeQuery':U).
 bufTTh:BUFFER-RELEASE().
 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggTmpLager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpLager Procedure 
PROCEDURE ByggTmpLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hTTArtBuf    AS HANDLE         NO-UNDO.
  DEFINE VARIABLE hBufferField AS HANDLE         NO-UNDO.
  DEFINE VARIABLE cStorlArray  AS CHARACTER      NO-UNDO.
  DEFINE VARIABLE iEntry       AS INTEGER        NO-UNDO.
  DEFINE VARIABLE iLagAnt      LIKE Lager.LagAnt NO-UNDO.
  DEFINE VARIABLE dVVarekost   AS DECIMAL        NO-UNDO.
  DEFINE VARIABLE cVisEntry    AS CHARACTER      NO-UNDO.
  DEFINE VARIABLE lSumSolgt    AS DECIMAL        NO-UNDO.
  DEFINE VARIABLE lSumKjopt    AS DECIMAL        NO-UNDO.
  DEFINE VARIABLE cRetur       AS CHARACTER      NO-UNDO.

  hTTArtBuf = hTTArt:DEFAULT-BUFFER-HANDLE.
  CREATE QUERY  hQuery.
  hQuery:SET-BUFFERS(hTTArtBuf).
  hQuery:QUERY-PREPARE("FOR EACH TTArt").
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
      hBufferField = hTTArtBuf:BUFFER-FIELD("ArtikkelNr").
      FIND ArtBas WHERE ArtBas.ArtikkelNr = hBufferField:BUFFER-VALUE() NO-LOCK NO-ERROR.
      IF AVAIL Artbas AND NOT (ArtBas.Lager = FALSE OR ArtBas.OPris = TRUE) THEN 
      DO:
          IF CAN-FIND(FIRST Lager OF ArtBas 
                      WHERE CAN-DO(cButikkListe,STRING(Lager.Butik))) THEN 
          DO:
              FIND Artpris OF ArtBas WHERE ArtPris.ProfilNr = iCLProfilNr NO-LOCK NO-ERROR.
              IF NOT AVAIL Artpris THEN
                  FIND FIRST Artpris OF ArtBas NO-LOCK NO-ERROR.
              CREATE TT_ArtLager.
              ASSIGN TT_ArtLager.ArtikkelNr = ArtBas.ArtikkelNr 
                     TT_ArtLager.Vg         = ArtBas.Vg         
                     TT_ArtLager.Lopnr      = ArtBas.Lopnr      
                     TT_ArtLager.LevNr      = ArtBas.LevNr      
                     TT_ArtLager.Beskr      = ArtBas.Beskr      
                     TT_ArtLager.LevArtNr   = IF LENGTH(TRIM(ArtBas.LevKod)) > 9 THEN SUBSTR(TRIM(ArtBas.LevKod),1,8) + ">" ELSE ArtBas.LevKod
                     TT_Artlager.PrisOrdi   = ArtPris.Pris[1]
                     TT_Artlager.PrisTilb   = IF Artpris.Tilbud THEN ArtPris.Pris[2] ELSE 0
                     iLagAnt                = 0
                     dVVarekost             = 0.
              FOR EACH Lager OF ArtBas NO-LOCK WHERE CAN-DO(cButikkListe,STRING(Lager.Butik)):
                  /* Ikke periodisert */
                  IF lPeriode = FALSE THEN
                  DO:
                      ASSIGN 
                          TT_ArtLager.AntSolgt    = TT_ArtLager.AntSolgt    + Lager.AntSolgt   
                          TT_ArtLager.VerdiSolgt  = TT_ArtLager.VerdiSolgt  + Lager.VerdiSolgt 
                          TT_ArtLager.KjopAnt     = TT_ArtLager.KjopAnt     + Lager.KjopAnt    
                          TT_ArtLager.KjopVerdi   = TT_ArtLager.KjopVerdi   + Lager.KjopVerdi  
                          TT_ArtLager.AntRab      = TT_ArtLager.AntRab      + Lager.AntRab     
                          TT_ArtLager.VerdiRabatt = TT_ArtLager.VerdiRabatt + Lager.VerdiRabatt.
                  END.

                  /* Periodisert */
                  ELSE DO:
                      ASSIGN cRetur = getAntVerdi(Lager.ArtikkelNr,Lager.Butik, 1) /* Salg */.
                      ASSIGN 
                          TT_ArtLager.AntSolgt    = TT_ArtLager.AntSolgt    + dec(ENTRY(1,cRetur,'|'))   
                          TT_ArtLager.VerdiSolgt  = TT_ArtLager.VerdiSolgt  + dec(ENTRY(2,cRetur,'|')) 
                          TT_ArtLager.AntRab      = TT_ArtLager.AntRab      + dec(ENTRY(3,cRetur,'|'))     
                          TT_ArtLager.VerdiRabatt = TT_ArtLager.VerdiRabatt + dec(ENTRY(4,cRetur,'|')).

                      ASSIGN cRetur = getAntVerdi(Lager.ArtikkelNr,Lager.Butik, 3) /* Reklamasjon */.
                      ASSIGN 
                          TT_ArtLager.AntSolgt    = TT_ArtLager.AntSolgt    - dec(ENTRY(1,cRetur,'|'))   
                          TT_ArtLager.VerdiSolgt  = TT_ArtLager.VerdiSolgt  - dec(ENTRY(2,cRetur,'|')) 
                          TT_ArtLager.AntRab      = TT_ArtLager.AntRab      - dec(ENTRY(3,cRetur,'|'))     
                          TT_ArtLager.VerdiRabatt = TT_ArtLager.VerdiRabatt - dec(ENTRY(4,cRetur,'|')).

                      ASSIGN cRetur = getAntVerdi(Lager.ArtikkelNr,Lager.Butik, 10) /* Retur */. 
                      ASSIGN 
                          TT_ArtLager.AntSolgt    = TT_ArtLager.AntSolgt    - dec(ENTRY(1,cRetur,'|'))   
                          TT_ArtLager.VerdiSolgt  = TT_ArtLager.VerdiSolgt  - dec(ENTRY(2,cRetur,'|')) 
                          TT_ArtLager.AntRab      = TT_ArtLager.AntRab      - dec(ENTRY(3,cRetur,'|'))     
                          TT_ArtLager.VerdiRabatt = TT_ArtLager.VerdiRabatt - dec(ENTRY(4,cRetur,'|')).
                          
                      ASSIGN cRetur = getAntVerdi(Lager.ArtikkelNr,Lager.Butik,  5) /* Kjøpt */. 
                      ASSIGN 
                          TT_ArtLager.KjopAnt     = TT_ArtLager.KjopAnt     + dec(ENTRY(1,cRetur,'|'))    
                          TT_ArtLager.KjopVerdi   = TT_ArtLager.KjopVerdi   + dec(ENTRY(2,cRetur,'|')).  
                  END.

                  /* Lagersum */
                  ASSIGN TT_ArtLager.LagAnt      = TT_ArtLager.LagAnt      + Lager.LagAnt     
                         iLagAnt                 = iLagAnt + IF Lager.LagAnt > 0 THEN Lager.LagAnt ELSE 0
                         dVVarekost              = dVVarekost + IF Lager.LagAnt > 0 THEN Lager.LagAnt * Lager.VVarekost ELSE 0.

              END.
              
              ASSIGN 
                  TT_ArtLager.VVarekost = IF iLagAnt > 0 
                                            THEN dVVarekost / iLagAnt 
                                            ELSE 0.
              /* Bygger liste med størrelser */
              ASSIGN cStorlArray = "".
              FIND StrType OF ArtBas NO-LOCK NO-ERROR.
              IF AVAIL StrType THEN 
                FOR EACH StrTstr OF StrType NO-LOCK,
                  FIRST ArtLag NO-LOCK WHERE
                    ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND 
                    ArtLag.Storl      = StrTStr.SoStorl.
                  ASSIGN cStorlArray = cStorlArray + (IF cStorlArray <> "" THEN "," ELSE "") + TRIM(StrTStr.SoStorl).
                END.

              /* Leser lager pr. størrelse og legger på størrelser som ikke ligger i størrelsestypen. */
              FOR EACH ArtLag WHERE 
                  ArtLag.ArtikkelNr = ArtBas.Artikkelnr NO-LOCK.
                  IF NOT CAN-DO(cStorlArray,TRIM(ArtLag.Storl)) THEN
                      ASSIGN cStorlArray = cStorlArray + (IF cStorlArray <> "" THEN "," ELSE "") + TRIM(ArtLag.Storl).
              END.
              /* Leser lager pr. størrelse og henter antall og verdier. */
              ASSIGN cVisEntry = "".
              FOR EACH ArtLag NO-LOCK WHERE 
                  ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND 
                  CAN-DO(cButikkListe,STRING(Artlag.Butik)):

                  FIND TT_KjopSalgLager WHERE TT_KjopSalgLager.ArtikkelNr = ArtLag.ArtikkelNr AND
                                              TT_KjopSalgLager.Butik      = ArtLag.Butik NO-ERROR.
                  /* Oppretter tmpRecord og initierer feltene med kommalister. */
                  IF NOT AVAIL TT_KjopSalgLager THEN DO:
                      CREATE TT_KjopSalgLager.
                      ASSIGN TT_KjopSalgLager.ArtikkelNr = ArtLag.ArtikkelNr
                             TT_KjopSalgLager.Butik      = ArtLag.Butik
                             TT_KjopSalgLager.Storl      = cStorlArray
                             TT_KjopSalgLager.Antsolgt   = FILL(",",NUM-ENTRIES(cStorlArray) - 1)
                             TT_KjopSalgLager.Kjopant    = TT_KjopSalgLager.Antsolgt
                             TT_KjopSalgLager.Lagant     = TT_KjopSalgLager.Antsolgt
                             cVisEntry                   = IF cVisEntry <> "" THEN cVisEntry ELSE TT_KjopSalgLager.Antsolgt.
                  END.

                  /* Leser ikke periodisert informasjon */
                  IF lPeriode = FALSE THEN
                  DO:
                      ASSIGN iEntry = LOOKUP(TRIM(ArtLag.Storl),TT_KjopSalgLager.Storl)
                             ENTRY(iEntry,TT_KjopSalgLager.Antsolgt) = STRING(INT(ENTRY(iEntry,TT_KjopSalgLager.Antsolgt)) + ArtLag.AntSolgt) 
                             ENTRY(iEntry,TT_KjopSalgLager.Kjopant) = STRING(INT(ENTRY(iEntry,TT_KjopSalgLager.Kjopant)) + ArtLag.KjopAnt)
                             ENTRY(iEntry,TT_KjopSalgLager.Lagant) = STRING(INT(ENTRY(iEntry,TT_KjopSalgLager.Lagant)) + ArtLag.lagant)
                             ENTRY(iEntry,cVisEntry) = "1"
                             TT_KjopSalgLager.iAntsolgt = TT_KjopSalgLager.iAntsolgt + ArtLag.AntSolgt
                             TT_KjopSalgLager.iKjopAnt  = TT_KjopSalgLager.iKjopAnt  + ArtLag.KjopAnt
                             TT_KjopSalgLager.iLagant   = TT_KjopSalgLager.iLagant   + ArtLag.lagant.
                  END.
                  /* Leser periodisert informasjon */
                  ELSE DO:
                      ASSIGN
                          lSumSolgt = 0
                          lSumKjopt = 0
                          lSumSolgt = getAntall(ArtLag.ArtikkelNr,ArtLag.Butik,ArtLag.Storl, 1) /* Salg */
                                      - getAntall(ArtLag.ArtikkelNr,ArtLag.Butik,ArtLag.Storl, 3) /* Reklamasjon */
                                      - getAntall(ArtLag.ArtikkelNr,ArtLag.Butik,Artlag.Storl, 10) /* Retur */
                          lSumKjopt = getAntall(ArtLag.ArtikkelNr,ArtLag.Butik,ArtLag.Storl, 5)
                          .
                      ASSIGN iEntry = LOOKUP(TRIM(ArtLag.Storl),TT_KjopSalgLager.Storl)
                             ENTRY(iEntry,TT_KjopSalgLager.Antsolgt) = STRING(INT(ENTRY(iEntry,TT_KjopSalgLager.Antsolgt)) + lSumSolgt) 
                             ENTRY(iEntry,TT_KjopSalgLager.Kjopant) = STRING(INT(ENTRY(iEntry,TT_KjopSalgLager.Kjopant)) + lSumKjopt)
                             ENTRY(iEntry,TT_KjopSalgLager.Lagant) = STRING(INT(ENTRY(iEntry,TT_KjopSalgLager.Lagant)) + ArtLag.lagant)
                             ENTRY(iEntry,cVisEntry) = "1"
                             TT_KjopSalgLager.iAntsolgt = TT_KjopSalgLager.iAntsolgt + lSumSolgt
                             TT_KjopSalgLager.iKjopAnt  = TT_KjopSalgLager.iKjopAnt  + lSumKjopt
                             TT_KjopSalgLager.iLagant   = TT_KjopSalgLager.iLagant   + ArtLag.lagant.
                  END.

              END.
              FOR EACH TT_KjopSalgLager WHERE TT_KjopSalgLager.ArtikkelNr = ArtBas.ArtikkelNr:
                  ASSIGN TT_KjopSalgLager.VisEntry = cVisEntry.
              END.
          END.
      END.
      hQuery:GET-NEXT().
  END.
  hQuery:QUERY-CLOSE().    
  hTTArtBuf:BUFFER-RELEASE().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitPrintString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitPrintString Procedure 
PROCEDURE InitPrintString :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cRight AS CHARACTER  NO-UNDO.
    DO iCount = 1 TO EXTENT(iCols):
        ASSIGN cRight = IF iRight[iCount] = 0 THEN "" ELSE 
                 "<RIGHT=C+" + (IF iCount = EXTENT(iCols) THEN STRING(114 - iCols[iCount] - 1) 
                                ELSE STRING(iCols[iCount + 1] - iCols[iCount] - 1)) + ">".
        ASSIGN cPrintString = cPrintString + "<C" + STRING(iCols[iCount]) + ">" + cRight + "&" + STRING(iCount,"99").
    END.
    ASSIGN cColLabelString = "<U>" + cPrintString + "</U>".
    DO iCount = 1 TO EXTENT(iCols):
        ASSIGN cColLabelString = REPLACE(cColLabelString,"&" + STRING(iCount,"99"),cRub[iCount]).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFPageFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFPageFooter Procedure 
PROCEDURE PDFPageFooter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
/*  RUN pdf_set_dash IN h_PDFinc ("Spdf",1,0).*/
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_BottomMargin ("Spdf"), pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , pdf_BottomMargin ("Spdf"), 0.5).

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).  
  RUN pdf_text_xy_dec ("Spdf",cKundeNavn,pdf_LeftMargin ("Spdf"),pdf_BottomMargin ("Spdf") - 14).
  RUN pdf_text_xy_dec ("Spdf",cPolygon,pdf_Pagewidth ("Spdf") - pdf_LeftMargin ("Spdf") - 500,pdf_BottomMargin ("Spdf") - 14).
  cSidTxt = TRIM("Sida: " + STRING(pdf_page("Spdf")) + " (" + pdf_TotalPages("Spdf") + ")").
  RUN pdf_text_xy_dec ("Spdf",cSidTxt,pdf_Pagewidth ("Spdf") - pdf_LeftMargin ("Spdf") - 50,pdf_BottomMargin ("Spdf") - 14).


END PROCEDURE. /* PDFPageFooter */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFPageHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFPageHeader Procedure 
PROCEDURE PDFPageHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",cToday,dColPos[1],pdf_PageHeight("Spdf") - 35).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",30).
  RUN pdf_text_xy_dec ("Spdf",cTitle,dColPos[2] + 250,pdf_PageHeight("Spdf") - 40).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
    RUN pdf_text_xy_dec ("Spdf","Butik: " + cButNamn,dColPos[1],pdf_PageHeight("Spdf") - 45).
  ELSE
    RUN pdf_text_xy_dec ("Spdf","Butikk: " + cButNamn,dColPos[1],pdf_PageHeight("Spdf") - 45).
  RUN pdf_text_xy_dec ("Spdf",cPerTxt,dColPos[10],pdf_PageHeight("Spdf") - 45).
  IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
    cSidTxt = "Sida: " + TRIM(STRING(pdf_page("Spdf"))).
  ELSE
    cSidTxt = "Side: " + TRIM(STRING(pdf_page("Spdf"))).
  RUN pdf_text_xy_dec ("Spdf",cSidTxt,dColPos[15] + 35,pdf_PageHeight("Spdf") - 45).
  

/*  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
  RUN pdf_text_xy_dec ("Spdf",cKundenavn,pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 61).*/
  
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
/*  RUN pdf_text_xy_dec ("Spdf",cToday,pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 72).*/
  
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_PageHeight ("Spdf") - 50, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , pdf_PageHeight ("Spdf") - 50, 0.5).

  ASSIGN dY = pdf_PageHeight ("Spdf") - 70.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).

  DO iCount = 1 TO 15:
    RUN pdf_text_xy_dec ("Spdf",cRub[iCount],dColPos[iCount],dY).
    ASSIGN cUL = "".
    DO iLen = 1 TO LENGTH(cRub[iCount]):
      ASSIGN cUL = cUL + "_".
    END.
    RUN pdf_text_xy_dec ("Spdf",cUL,dColPos[iCount],dY).    
  END.

END PROCEDURE. /* PDFPageHeader */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFPositioner) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFPositioner Procedure 
PROCEDURE PDFPositioner :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN dColPos[1] = pdf_LeftMargin ("Spdf")  
         dColPos[2] = 80                      
         dColPos[3] = 170                      
         dColPos[4] = 210                      
         dColPos[5] = 240
         dColPos[6] = 300
         dColPos[7] = 330
         dColPos[8] = 420
         dColPos[9] = 450
         dColPos[10] = 500
         dColPos[11] = 530
         dColPos[12] = 600
         dColPos[13] = 630
         dColPos[14] = 700
         dColPos[15] = 750.
      
  ASSIGN dColPos2[1] = pdf_LeftMargin ("Spdf")  
         dColPos2[2] = 80                      
         dColPos2[3] = 170                      
         dColPos2[4] = 210                      
         dColPos2[5] = 240
         dColPos2[6] = 300
         dColPos2[7] = 335
         dColPos2[8] = 400
         dColPos2[9] = 450
         dColPos2[10] = 505
         dColPos2[11] = 530
         dColPos2[12] = 610
         dColPos2[13] = 640
         dColPos2[14] = 720
         dColPos2[15] = 765.
      
END PROCEDURE. /* PDSPositioner */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFSkrivlagerlisteX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFSkrivlagerlisteX Procedure 
PROCEDURE PDFSkrivlagerlisteX :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cQRY1     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQRY2     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQRY3     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iOldLevNr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cLevStr   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lFirst    AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cDatFrom  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDatTom   AS CHARACTER  NO-UNDO.

  IF NUM-ENTRIES(cButikkListe) = 1 THEN DO:
      FIND butiker WHERE butiker.butik = INT(cButikkListe) NO-LOCK.
      ASSIGN cButnamn = butiker.butnamn.
  END.
  ELSE
      ASSIGN cButnamn = cButikkListe.

  IF lPeriode THEN
    DO:
      IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
      DO:
        IF dFraDato = ? THEN
          ASSIGN cDatFrom = "?".
        ELSE
          ASSIGN cDatFrom = STRING(YEAR(dFraDato),"9999") + 
                            STRING(MONTH(dFraDato),"99") +
                            STRING(DAY(dFraDato),"99").
        IF dTilDato = ? THEN
          ASSIGN cDatTom = "?".
        ELSE
          ASSIGN cDatTom = STRING(YEAR(dTilDato),"9999") + 
                           STRING(MONTH(dTilDato),"99") + 
                           STRING(DAY(dTilDato),"99").
        ASSIGN cPerTxt = "Köp och försäljning periodicerat från/till datum " + 
                         cDatFrom + 
                          " - " + 
                         cDatTom.
      END.
      ELSE  
        ASSIGN cPerTxt = "Kjøp og salg periodisert - fra/til dato " + 
                         (IF dFraDato = ? THEN "?" ELSE STRING(dFraDato)) + 
                          " - " + 
                         (IF dTilDato = ? THEN "?" ELSE STRING(dTilDato)).
    END.
  ELSE 
      IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
        ASSIGN cPerTxt = "Ej periodicerat".
      ELSE
        ASSIGN cPerTxt = "Ikke periodisert".

  IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
    ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "Lagerlista.pdf".
  ELSE
    ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "Lagerliste.pdf".

  RUN pdf_new ("Spdf",pcRappFil).
  pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PDFPageFooter").
  RUN pdf_set_PaperType ("Spdf","A4").
  RUN pdf_set_LeftMargin ("Spdf", 30).
  RUN pdf_set_BottomMargin ("Spdf", 40).
  RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf", 13).
  RUN pdf_set_Orientation ("Spdf", "landscape").

  RUN PDFPositioner.

  CREATE tmpTT_ArtLagerTOT.

  FOR EACH TT_ArtLager USE-INDEX VgLopnr NO-LOCK:
          IF lNullposter = FALSE AND TT_ArtLager.AntSolgt = 0 AND TT_ArtLager.KjopAnt = 0 THEN DO:
/*              MESSAGE "get next 1"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
              NEXT.
          END.
          IF lNullager = FALSE AND TT_ArtLager.LagAnt = 0 THEN DO:
/*              MESSAGE "get next 2"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
              NEXT.
          END.
          IF iSortering = 2 AND iOldLevNr <> TT_ArtLager.LevNr THEN DO:
/*            MESSAGE "isort 2 - 1"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
              IF lFirst = TRUE THEN DO:
                  ASSIGN dY = dY - 13.
                  DELETE tmpTT_ArtLager.
              END.
              CREATE tmpTT_ArtLager.
              BUFFER-COPY TT_ArtLager EXCEPT ArtikkelNr TO tmpTT_ArtLager
              ASSIGN tmpTT_ArtLager.LagerVerdi = TT_ArtLager.LagAnt * TT_ArtLager.VVarekost.
                  /* Visa LevTot */
              FIND LevBas WHERE LevBas.LevNr = TT_ArtLager.LevNr NO-LOCK NO-ERROR.
              ASSIGN iOldLevNr = TT_ArtLager.LevNr
                     cLevStr   = "<C6><B>Leverandør: " + LevBas.Levnamn + "</B>".
              IF lFirst = FALSE THEN
                  ASSIGN lFirst = TRUE.
              ELSE DO:
              END.
          END.
          ELSE IF iSortering = 2 THEN DO:
/*              MESSAGE "isort 2 - 2"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
              ASSIGN tmpTT_ArtLager.AntSolgt   = tmpTT_ArtLager.AntSolgt   + TT_ArtLager.AntSolgt  
                     tmpTT_ArtLager.VerdiSolgt = tmpTT_ArtLager.VerdiSolgt + TT_ArtLager.VerdiSolgt
                     tmpTT_ArtLager.KjopAnt    = tmpTT_ArtLager.KjopAnt    + TT_ArtLager.KjopAnt   
                     tmpTT_ArtLager.KjopVerdi  = tmpTT_ArtLager.KjopVerdi  + TT_ArtLager.KjopVerdi 
                     tmpTT_ArtLager.LagAnt     = tmpTT_ArtLager.LagAnt     + TT_ArtLager.LagAnt    
                     tmpTT_ArtLager.AntRab     = tmpTT_ArtLager.AntRab     + TT_ArtLager.AntRab
                     tmpTT_ArtLager.LagerVerdi = tmpTT_ArtLager.LagerVerd  + TT_ArtLager.LagAnt * TT_ArtLager.VVarekost.    
          END.
          ASSIGN tmpTT_ArtLagerTOT.AntSolgt   = tmpTT_ArtLagerTOT.AntSolgt   + TT_ArtLager.AntSolgt  
                 tmpTT_ArtLagerTOT.VerdiSolgt = tmpTT_ArtLagerTOT.VerdiSolgt + TT_ArtLager.VerdiSolgt
                 tmpTT_ArtLagerTOT.KjopAnt    = tmpTT_ArtLagerTOT.KjopAnt    + TT_ArtLager.KjopAnt   
                 tmpTT_ArtLagerTOT.KjopVerdi  = tmpTT_ArtLagerTOT.KjopVerdi  + TT_ArtLager.KjopVerdi 
                 tmpTT_ArtLagerTOT.LagAnt     = tmpTT_ArtLagerTOT.LagAnt     + TT_ArtLager.LagAnt    
                 tmpTT_ArtLagerTOT.AntRab     = tmpTT_ArtLagerTOT.AntRab     + TT_ArtLager.AntRab
                 tmpTT_ArtLagerTOT.LagerVerdi = tmpTT_ArtLagerTOT.LagerVerd  + TT_ArtLager.LagAnt * TT_ArtLager.VVarekost.    
          
          /* Legger ut linje */
          RUN pdf_new_page ("Spdf").
          RUN PDFPageHeader.
          RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).

          getDataLinje(1).
          ASSIGN dY = dY - 13.
          DO iCount2 = 1 TO EXTENT(cTmpData):
            RUN pdf_text_xy_dec ("Spdf",cTmpData[iCount2],dColPos2[iCount2],dY).
          END.
          ASSIGN dY = dY - 13.

          /* Legger ut størrelsesmatrise */
          IF lVisStr = TRUE THEN DO:
              FOR EACH TT_KjopSalgLager WHERE TT_KjopSalgLager.ArtikkelNr = TT_ArtLager.ArtikkelNr AND
                  (TT_KjopSalgLager.iAntsolgt <> 0 OR TT_KjopSalgLager.iKjopAnt <> 0 OR TT_KjopSalgLager.iLagant <> 0):
                  IF (dY - 20) < 40 THEN
                  DO:
                    RUN pdf_new_page ("Spdf").
                    RUN PDFPageHeader.
                    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
                    getDataLinje(1).
                    ASSIGN dY = dY - 13.
                    DO iCount2 = 1 TO EXTENT(cTmpData):
                      RUN pdf_text_xy_dec ("Spdf",cTmpData[iCount2],dColPos2[iCount2],dY).
                    END.
                    ASSIGN dY = dY - 13.

                  END.
                  ELSE
                  DO:    
                    dY = dY - 13.                 /* Butiksrad*/
                    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
                    IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
                    DO:
                      ASSIGN cTxt = "Butik: " + STRING(TT_KjopSalgLager.Butik).
                      RUN pdf_text_xy_dec ("Spdf",cTxt,dColPos2[1] + 20,dY).
                    END.
                    ELSE
                    DO:
                      ASSIGN cTxt = "Butikk: " + STRING(TT_KjopSalgLager.Butik).
                      RUN pdf_text_xy_dec ("Spdf",cTxT,dColPos2[1] + 20,dY).
                    END.
                    ASSIGN cUL = "".
                    DO iLen = 1 TO LENGTH(cTxt):
                      ASSIGN cUL = cUL + "_".
                    END.
                    RUN pdf_text_xy_dec ("Spdf",cUL,dColPos2[1] + 20,dY).

                    DO iCount3 = 1 TO NUM-ENTRIES(TT_KjopSalgLager.Storl):
                      IF ENTRY(iCount3,TT_KjopSalgLager.VisEntry) = "1" THEN
                      DO:
                        ASSIGN cStrl = ENTRY(iCount3,TT_KjopSalgLager.Storl)
                               cStrl = REPLACE(cStrl,",",".").
                        RUN pdf_text_xy_dec ("Spdf",cStrl,dColPos2[2] + (30 * iCount3),dY).
                        ASSIGN cUL = "".
                        DO iLen = 1 TO LENGTH(cStrl):
                          ASSIGN cUL = cUL + "_".
                        END.
                        RUN pdf_text_xy_dec ("Spdf",cUL,dColPos2[2] + (30 * iCount3),dY).
                        ASSIGN dColAdd[iCount3] = LENGTH(cStrl) - 1.
                      END.
                    END.

                    dY = dY - 13.                  /* Sålt-rad*/
                    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
                    IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
                      RUN pdf_text_xy_dec ("Spdf","Sålt:",dColPos2[1] + 20,dY).
                    ELSE
                      RUN pdf_text_xy_dec ("Spdf","Solgt:",dColPos2[1] + 20,dY).

                    DO iCount3 = 1 TO NUM-ENTRIES(TT_KjopSalgLager.AntSolgt):
                      IF ENTRY(iCount3,TT_KjopSalgLager.VisEntry) = "1" THEN
                      DO:
                        IF ENTRY(iCount3,TT_KjopSalgLager.AntSolgt) = "0" THEN
                          ASSIGN cStrl = " ".
                        ELSE
                          ASSIGN cStrl = ENTRY(iCount3,TT_KjopSalgLager.AntSolgt).
                        RUN pdf_text_xy_dec ("Spdf",cStrl,dColPos2[2] + (30 * iCount3) + (5 * dColAdd[iCount3]),dY).
                      END.
                    END.

                    dY = dY - 13.                  /* Köpt-rad*/
                    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
                    IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
                      RUN pdf_text_xy_dec ("Spdf","Köpt:",dColPos2[1] + 20,dY).
                    ELSE
                      RUN pdf_text_xy_dec ("Spdf","Kjøpt:",dColPos2[1] + 20,dY).

                    DO iCount3 = 1 TO NUM-ENTRIES(TT_KjopSalgLager.Kjopant):
                      IF ENTRY(iCount3,TT_KjopSalgLager.VisEntry) = "1" THEN
                      DO:
                        IF ENTRY(iCount3,TT_KjopSalgLager.Kjopant) = "0" THEN
                          ASSIGN cStrl = " ".
                        ELSE
                          ASSIGN cStrl = ENTRY(iCount3,TT_KjopSalgLager.Kjopant).
                        RUN pdf_text_xy_dec ("Spdf",cStrl,dColPos2[2] + (30 * iCount3) + (5 * dColAdd[iCount3]),dY).
                      END.
                    END.

                    dY = dY - 13.                  /* Lager-rad*/
                    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
                    IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
                      RUN pdf_text_xy_dec ("Spdf","Lager:",dColPos2[1] + 20,dY).
                    ELSE
                      RUN pdf_text_xy_dec ("Spdf","Lager:",dColPos2[1] + 20,dY).

                    DO iCount3 = 1 TO NUM-ENTRIES(TT_KjopSalgLager.Lagant):
                      IF ENTRY(iCount3,TT_KjopSalgLager.VisEntry) = "1" THEN
                      DO:
                        IF ENTRY(iCount3,TT_KjopSalgLager.Lagant) = "0" THEN
                          ASSIGN cStrl = " ".
                        ELSE
                          ASSIGN cStrl = ENTRY(iCount3,TT_KjopSalgLager.Lagant).
                        RUN pdf_text_xy_dec ("Spdf",cStrl,dColPos2[2] + (30 * iCount3) + (5 * dColAdd[iCount3]),dY).
                      END.
                    END.
                    dY = dY - 13.         
                  END.

              END.
          END. 

  END.

  IF (dY - 20) < 40 THEN
  DO:
    RUN pdf_new_page ("Spdf").
    RUN PDFPageHeader.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
    getDataLinje(1).
    ASSIGN dY = dY - 13.
    DO iCount2 = 1 TO EXTENT(cTmpData):
      RUN pdf_text_xy_dec ("Spdf",cTmpData[iCount2],dColPos2[iCount2],dY).
    END.
    ASSIGN dY = dY - 13.
  END.
  getDataLinje(3).
  ASSIGN dY = dY - 20.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",15).
  DO iCount2 = 1 TO EXTENT(cTmpData):
    IF iCount2 > 14 THEN
      RUN pdf_text_xy_dec ("Spdf",cTmpData[iCount2],dColPos2[iCount2] - 20,dY).
    ELSE
      RUN pdf_text_xy_dec ("Spdf",cTmpData[iCount2],dColPos2[iCount2],dY).
  END.
/*  ASSIGN tmpTT_ArtLagerTOT.AntSolgt   = 0
         tmpTT_ArtLagerTOT.VerdiSolgt = 0
         tmpTT_ArtLagerTOT.KjopAnt    = 0
         tmpTT_ArtLagerTOT.KjopVerdi  = 0
         tmpTT_ArtLagerTOT.LagAnt     = 0
         tmpTT_ArtLagerTOT.LagerVerdi = 0.*/

  IF iSortering = 2 THEN DO:
/*      PUT UNFORMATTED "<B>" getDataLinje(2) "</B>"SKIP.*/
  END.

  RUN pdf_close ("Spdf").

/*  MESSAGE "Visa Lista"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
 
  RUN browse2pdf\viewxmldialog.w (pcRappFil,"Polygon Retail Solutions").
/*  RUN visPDF(pcRappFil, "Polygon Retail Solutions").*/

END PROCEDURE. /* PDFSkrivLagerlisteX */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PFooter Procedure 
PROCEDURE PFooter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
              PUT UNFORMATTED "<R45><C6><FROM><R45><C113><LINE>" SKIP
                              "<C6>" cKundenavn "<C1><CENTER=C113>" cPolygon SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivLagerlisteX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivLagerlisteX Procedure 
PROCEDURE SkrivLagerlisteX :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcRappFil AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cQRY1     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cQRY2     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cQRY3     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iOldLevNr AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cLevStr   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lFirst    AS LOGICAL   NO-UNDO.

  RUN InitPrintString.
  ASSIGN cQRY1 = "FOR EACH TT_ArtLager USE-INDEX VgLopnr"
         cQRY2 = "FOR EACH TT_ArtLager USE-INDEX LevNr".

  IF NUM-ENTRIES(cButikkListe) = 1 THEN DO:
      FIND butiker WHERE butiker.butik = INT(cButikkListe) NO-LOCK.
      ASSIGN cButnamn = butiker.butnamn.
  END.
  ELSE
      ASSIGN cButnamn = cButikkListe.

  IF lPeriode THEN
      ASSIGN
      cPerTxt = "Kjøp og salg periodisert - fra/til dato " + 
                (IF dFraDato = ? THEN "?" ELSE STRING(dFraDato)) + 
                " - " + 
                (IF dTilDato = ? THEN "?" ELSE STRING(dTilDato)).
  ELSE 
      ASSIGN
          cPerTxt = "Ikke periodisert".

  hQuery:SET-BUFFERS(BUFFER TT_ArtLager:HANDLE).
  hQuery:QUERY-PREPARE(IF iSortering = 1 THEN cQRY1 ELSE cQRY2).
/*   hQuery:QUERY-PREPARE("FOR EACH TT_ArtLager"). */
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "Lagerliste.xpr".
  OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(65).
  PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
  PUT CONTROL '<PREVIEW=ZoomToWidth><OLANDSCAPE>'.
  VIEW FRAME PageHeader.
  PUT UNFORMATTED  "<R3.5><B><C1><CENTER=C110><P24>" cTitle "</B><P10><R+2.5>".
  PUT UNFORMATTED "<B>" cColLabelString "</B>" SKIP.

  CREATE tmpTT_ArtLagerTOT.
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
          IF lNullposter = FALSE AND TT_ArtLager.AntSolgt = 0 AND TT_ArtLager.KjopAnt = 0 THEN DO:
              hQuery:GET-NEXT().
              NEXT.
          END.
          IF lNullager = FALSE AND TT_ArtLager.LagAnt = 0 THEN DO:
              hQuery:GET-NEXT().
              NEXT.
          END.
          IF iSortering = 2 AND iOldLevNr <> TT_ArtLager.LevNr THEN DO:
              IF lFirst = TRUE THEN DO:
                  PUT UNFORMATTED "<B>" getDataLinje(2) "</B>"SKIP.
                  DELETE tmpTT_ArtLager.
              END.
              CREATE tmpTT_ArtLager.
              BUFFER-COPY TT_ArtLager EXCEPT ArtikkelNr TO tmpTT_ArtLager
              ASSIGN tmpTT_ArtLager.LagerVerdi = TT_ArtLager.LagAnt * TT_ArtLager.VVarekost.
                  /* Visa LevTot */
              FIND LevBas WHERE LevBas.LevNr = TT_ArtLager.LevNr NO-LOCK NO-ERROR.
              ASSIGN iOldLevNr = TT_ArtLager.LevNr
                     cLevStr   = "<C6><B>Leverandør: " + LevBas.Levnamn + "</B>".
              IF lFirst = FALSE THEN
                  ASSIGN lFirst = TRUE.
              ELSE DO:
                  RUN PFooter.
                  PAGE.
                  VIEW FRAME PageHeader.
                  PUT UNFORMATTED  "<R3.5><B><C1><CENTER=C110><P24>" cTitle "</B><P10><R+2.5>".
                  PUT UNFORMATTED "<B>" cColLabelString "</B>" SKIP.
              END.
              PUT UNFORMATTED cLevStr SKIP.
          END.
          ELSE IF iSortering = 2 THEN DO:
              ASSIGN tmpTT_ArtLager.AntSolgt   = tmpTT_ArtLager.AntSolgt   + TT_ArtLager.AntSolgt  
                     tmpTT_ArtLager.VerdiSolgt = tmpTT_ArtLager.VerdiSolgt + TT_ArtLager.VerdiSolgt
                     tmpTT_ArtLager.KjopAnt    = tmpTT_ArtLager.KjopAnt    + TT_ArtLager.KjopAnt   
                     tmpTT_ArtLager.KjopVerdi  = tmpTT_ArtLager.KjopVerdi  + TT_ArtLager.KjopVerdi 
                     tmpTT_ArtLager.LagAnt     = tmpTT_ArtLager.LagAnt     + TT_ArtLager.LagAnt    
                     tmpTT_ArtLager.AntRab     = tmpTT_ArtLager.AntRab     + TT_ArtLager.AntRab
                     tmpTT_ArtLager.LagerVerdi = tmpTT_ArtLager.LagerVerd  + TT_ArtLager.LagAnt * TT_ArtLager.VVarekost.    
          END.
          ASSIGN tmpTT_ArtLagerTOT.AntSolgt   = tmpTT_ArtLagerTOT.AntSolgt   + TT_ArtLager.AntSolgt  
                 tmpTT_ArtLagerTOT.VerdiSolgt = tmpTT_ArtLagerTOT.VerdiSolgt + TT_ArtLager.VerdiSolgt
                 tmpTT_ArtLagerTOT.KjopAnt    = tmpTT_ArtLagerTOT.KjopAnt    + TT_ArtLager.KjopAnt   
                 tmpTT_ArtLagerTOT.KjopVerdi  = tmpTT_ArtLagerTOT.KjopVerdi  + TT_ArtLager.KjopVerdi 
                 tmpTT_ArtLagerTOT.LagAnt     = tmpTT_ArtLagerTOT.LagAnt     + TT_ArtLager.LagAnt    
                 tmpTT_ArtLagerTOT.AntRab     = tmpTT_ArtLagerTOT.AntRab     + TT_ArtLager.AntRab
                 tmpTT_ArtLagerTOT.LagerVerdi = tmpTT_ArtLagerTOT.LagerVerd  + TT_ArtLager.LagAnt * TT_ArtLager.VVarekost.    
          
          /* Legger ut linje */
          PUT UNFORMATTED getDataLinje(1) SKIP.

          /* Legger ut størrelsesmatrise */
          IF lVisStr = TRUE THEN DO:
              FOR EACH TT_KjopSalgLager WHERE TT_KjopSalgLager.ArtikkelNr = TT_ArtLager.ArtikkelNr AND
                  (TT_KjopSalgLager.iAntsolgt <> 0 OR TT_KjopSalgLager.iKjopAnt <> 0 OR TT_KjopSalgLager.iLagant <> 0):
                  IF LINE-COUNTER > 38 THEN DO:
                      RUN PFooter.
                      PAGE.
                      VIEW FRAME PageHeader.
                      PUT UNFORMATTED  "<R3.5><B><C1><CENTER=C110><P24>" cTitle "</B><P10><R+2.5>".
                      PUT UNFORMATTED "<B>" cColLabelString "</B>" SKIP.
                  END.
                  ELSE
                      PUT SKIP.
                  PUT UNFORMATTED "<P8><B>" /* "<C2>" + STRING(LINE-COUNTER) + */ getKjopSolgtLagerLinje(1) "</B>" SKIP.
                  PUT UNFORMATTED /* "<C2>" + STRING(LINE-COUNTER) + */ getKjopSolgtLagerLinje(2) SKIP.
                  PUT UNFORMATTED /* "<C2>" + STRING(LINE-COUNTER) + */ getKjopSolgtLagerLinje(3) SKIP.
                  PUT UNFORMATTED /* "<C2>" + STRING(LINE-COUNTER) + */ getKjopSolgtLagerLinje(4) "<P10>" SKIP(1).
    /*               PUT UNFORMATTED "Butik" " " TT_KjopSalgLager.Butik " " TT_KjopSalgLager.Storl SKIP */
    /*                               "Solgt" " " "   " " " TT_KjopSalgLager.Antsolgt SKIP               */
    /*                               "Kjøpt" " " "   " " " TT_KjopSalgLager.Kjopant  SKIP               */
    /*                               "Lager" " " "   " " " TT_KjopSalgLager.Lagant   SKIP(1).           */
              END.
          END.
          hQuery:GET-NEXT().
          IF NOT hQuery:QUERY-OFF-END AND LINE-COUNTER > 38 THEN DO:
              RUN PFooter.
              PAGE.
              VIEW FRAME PageHeader.
              PUT UNFORMATTED  "<R3.5><B><C1><CENTER=C110><P24>" cTitle "</B><P10><R+2.5>".
              PUT UNFORMATTED "<B>" cColLabelString "</B>" SKIP(1).
          END.
  END.
  IF iSortering = 2 THEN DO:
      PUT UNFORMATTED "<B>" getDataLinje(2) "</B>"SKIP.
  END.
  PUT UNFORMATTED "<R+2><B>" getDataLinje(3) "</B>"SKIP.
  OUTPUT CLOSE.
/*   OUTPUT TO "CLIPBOARD".                */
/*   <U><C6>VG/Lopnr<C13>Beskrivelse<C27>Farge<C32>Lev<C37>Lev.art.nr<C45>Ses<C50><RIGHT=C+8>Varekost<C59><RIGHT=C+6>Pris<C66><RIGHT=C+7>Tilbud<C74><RIGHT=C+3>Solgt<C78><RIGHT=C+5>Solgt verdi<C84><RIGHT=C+9>Kjøpt<C94><RIGHT=C+8>Kjøpt verdi<C103><RIGHT=C+1>Lager<C105><RIGHT=C+9>Lagerverdi</U> */

/*   PUT UNFORMATTED ccollabelstring SKIP. */
  OUTPUT CLOSE.
  OUTPUT TO TERMINAL.

  /* Klargjør rapportfilnavnet */
  ASSIGN FILE-INFO:FILE-NAME = pcRappFil.
    
  /* Sender filen til visning og utskrift. */
  RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-bredd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION bredd Procedure 
FUNCTION bredd RETURNS DECIMAL
    ( INPUT cText AS CHARACTER ):
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/

    RETURN pdf_text_widthdec ("Spdf",cText).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAntall) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAntall Procedure 
FUNCTION getAntall RETURNS DECIMAL
  ( INPUT lArtikkelNr AS DEC,
    INPUT iButikkNr   AS INT,
    INPUT cStorl      AS CHAR,
    INPUT iTTId       AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR lSumAntall AS DEC  NO-UNDO.
  DEF VAR dDato      AS DATE NO-UNDO.

  IF dFraDato = ? OR 
      dTilDato = ? THEN. /* Gør ingenting */
  ELSE
  DO:
      FOR EACH Translogg NO-LOCK WHERE
          TransLogg.ArtikkelNr = lArtikkelNr AND
          TransLogg.Dato      >= dFraDato AND
          TransLogg.Dato      <= dTilDato AND
          Translogg.Tid       >= 0 AND
          Translogg.Butik      = iButikkNr AND
          TransLogg.TTId       = iTTId AND
          TransLogg.Storl      = cStorl:

          ASSIGN
              lSumAntall = lSumantall + Translogg.Antall
              .

      END.
  END.

  RETURN lSumAntall.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAntVerdi) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAntVerdi Procedure 
FUNCTION getAntVerdi RETURNS CHARACTER
    ( INPUT lArtikkelNr AS DEC,
      INPUT iButikkNr   AS INT,
      INPUT iTTId       AS INT ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
    DEF VAR lSumAntall   AS DEC  NO-UNDO.
    DEF VAR lSumVerdi    AS DEC  NO-UNDO.
    DEF VAR lSumRabAnt   AS DEC  NO-UNDO.
    DEF VAR lsumRabVerdi AS DEC  NO-UNDO.
    DEF VAR dDato        AS DATE NO-UNDO.

    IF dFraDato = ? OR 
        dTilDato = ? THEN. /* Gør ingenting */
    ELSE
    DO:
        FOR EACH Translogg NO-LOCK WHERE
            TransLogg.ArtikkelNr = lArtikkelNr AND
            TransLogg.Dato      >= dFraDato AND
            TransLogg.Dato      <= dTilDato AND
            Translogg.Tid       >= 0 AND
            Translogg.Butik      = iButikkNr AND
            TransLogg.TTId       = iTTId:

            ASSIGN
                lSumAntall   = lSumantall + Translogg.Antall
                lSumVerdi    = lSumVerdi  + (TransLogg.Antall * (Translogg.Pris - TransLogg.RabKr - TransLogg.Mva)) 
                lSumRabAnt   = IF TransLogg.RabKr <> 0 
                                 THEN lSumRabAnt + TransLogg.Antall
                                 ELSE lSumRabAnt
                lSumRabVerdi = IF TransLogg.RabKr <> 0 
                               THEN lSumRabVerdi + (TransLogg.RabKr * TransLogg.Antall)
                               ELSE lSumRabVerdi
                .

        END.
    END.

    RETURN STRING(lSumantall) + '|' + 
           STRING(lSumVerdi)  + '|' +
           STRING(lSumRabAnt) + '|' +
           STRING(lSumRabVerdi)
           .  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDataLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDataLinje Procedure 
FUNCTION getDataLinje RETURNS CHARACTER
  ( INPUT iTyp AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cColLabels AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCount  AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iCount2 AS INTEGER    NO-UNDO.
      
    /*
    ["VG/Lopnr","Beskrivelse","Farge","Lev","Lev.art.nr","Ses","Varekost","Pris","Tilbud","Solgt",
    "Solgt verdi","Kjøpt","Kjøpt verdi","Lager","Lagerverdi"] NO-UNDO.
     */
    IF iTyp = 1 THEN DO: /* TT_ArtLager */
        FIND ArtBas WHERE ArtBas.ArtikkelNr = TT_ArtLager.ArtikkelNr NO-LOCK NO-ERROR.
        FIND Farg OF ArtBas NO-LOCK NO-ERROR.
        FIND Sasong OF ArtBas NO-LOCK NO-ERROR.
        IF AVAILABLE ArtBas THEN 
          ASSIGN cTmpData[1] = STRING(Artbas.Vg) + "/" + STRING(ArtBas.LopNr)
                 cTmpData[2] = SUBSTR(ArtBas.Bongtekst,1,17)
                 cTmpData[3] = IF AVAIL Farg THEN Farg.FarBeskr ELSE " "
                 cTmpData[4] = STRING(ArtBas.LevNr)
                 cTmpData[5] = IF LENGTH(TRIM(ArtBas.LevKod)) > 9 THEN SUBSTR(TRIM(ArtBas.LevKod),1,8) + ">" ELSE ArtBas.LevKod
                 cTmpData[6] = IF AVAIL Sasong AND Sasong.Sasong > 0 THEN STRING(Sasong.Sasong) ELSE " "
                 cTmpData[7] = STRING(TT_ArtLager.VVarekost,">>,>>9.99")
                 cTmpData[8] = STRING(TT_ArtLager.PrisOrdi,">>,>>9.99")
                 cTmpData[9] = IF TT_ArtLager.PrisTilb > 0 THEN STRING(TT_ArtLager.PrisTilb,">>,>>9.99") ELSE " "
                 cTmpData[10] = STRING(TT_ArtLager.AntSolgt)
                 cTmpData[11] = STRING(TT_ArtLager.VerdiSolgt,"->>,>>>,>>9")
                 cTmpData[12] = STRING(TT_ArtLager.KjopAnt)
                 cTmpData[13] = STRING(TT_ArtLager.KjopVerdi,"->>,>>>,>>9")
                 cTmpData[14] = STRING(TT_ArtLager.LagAnt)                                         
                 cTmpData[15] = STRING(TT_ArtLager.LagAnt * TT_ArtLager.VVarekost,"->>,>>>,>>9").

    END.
    ELSE IF iTyp = 2 THEN DO: /* tmpTT_ArtLager */
          ASSIGN cTmpData[1] = " "
                 cTmpData[2] = "Totalt leverandør"
                 cTmpData[3] = " "
                 cTmpData[4] = " "
                 cTmpData[5] = " "
                 cTmpData[6] = " "
                 cTmpData[7] = " "
                 cTmpData[8] = " "
                 cTmpData[9] = " "
                 cTmpData[10] = STRING(tmpTT_ArtLager.AntSolgt)
                 cTmpData[11] = STRING(tmpTT_ArtLager.VerdiSolgt,"->>>,>>>,>>9")
                 cTmpData[12] = STRING(tmpTT_ArtLager.KjopAnt)
                 cTmpData[13] = STRING(tmpTT_ArtLager.KjopVerdi,"->>>,>>>,>>9")
                 cTmpData[14] = STRING(tmpTT_ArtLager.LagAnt)                                         
                 cTmpData[15] = STRING(tmpTT_ArtLager.LagerVerdi,"->>>,>>>,>>9").
    END.
    ELSE IF iTyp = 3 THEN DO: /* tmpTT_ArtLager */
          ASSIGN cTmpData[1] = " "
                 cTmpData[2] = "Totalt rapport"
                 cTmpData[3] = " "
                 cTmpData[4] = " "
                 cTmpData[5] = " "
                 cTmpData[6] = " "
                 cTmpData[7] = " "
                 cTmpData[8] = " "
                 cTmpData[9] = " "
                 cTmpData[10] = STRING(tmpTT_ArtLagerTOT.AntSolgt)
                 cTmpData[11] = STRING(tmpTT_ArtLagerTOT.VerdiSolgt,"->>>,>>>,>>9")
                 cTmpData[12] = STRING(tmpTT_ArtLagerTOT.KjopAnt)
                 cTmpData[13] = STRING(tmpTT_ArtLagerTOT.KjopVerdi,"->>>,>>>,>>9")
                 cTmpData[14] = STRING(tmpTT_ArtLagerTOT.LagAnt)                                         
                 cTmpData[15] = STRING(tmpTT_ArtLagerTOT.LagerVerdi,"->>>,>>>,>>9").
    END.

    ASSIGN cColLabels = cPrintString.

    DO iCount2 = 1 TO EXTENT(cTmpData):
        ASSIGN cColLabels = REPLACE(cColLabels,"&" + STRING(iCount2,"99"),cTmpData[iCount2]).
    END.
  RETURN cColLabels.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKjopSolgtLagerLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKjopSolgtLagerLinje Procedure 
FUNCTION getKjopSolgtLagerLinje RETURNS CHARACTER
  ( INPUT iRad AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cReturRad AS CHARACTER       NO-UNDO.
  DEFINE VARIABLE iCount    AS INTEGER         NO-UNDO.
  DEFINE VARIABLE iCount2   AS INTEGER INIT 12 NO-UNDO.
  ASSIGN cReturRad = "<C8>".
  CASE iRad:
      WHEN 1 THEN DO:
          ASSIGN cReturRad = cReturRad + "<U>" + "Butikk: " + STRING(TT_KjopSalgLager.Butik).
          DO iCount = 1 TO NUM-ENTRIES(TT_KjopSalgLager.Storl):
              IF ENTRY(iCount,TT_KjopSalgLager.VisEntry) = "1" THEN
              ASSIGN iCount2 = iCount2 + 5
                     cReturRad = cReturRad + "<C" + STRING(iCount2) + "><RIGHT=C+5>" + ENTRY(iCount,TT_KjopSalgLager.Storl).
          END.
          ASSIGN cReturRad = cReturRad + "</U>".
      END.
      WHEN 2 THEN DO:
          ASSIGN cReturRad = cReturRad + "Solgt: ".
          DO iCount = 1 TO NUM-ENTRIES(TT_KjopSalgLager.AntSolgt):
              IF ENTRY(iCount,TT_KjopSalgLager.VisEntry) = "1" THEN
              ASSIGN iCount2 = iCount2 + 5
                     cReturRad = cReturRad + "<C" + STRING(iCount2) + "><RIGHT=C+5>" + ENTRY(iCount,TT_KjopSalgLager.AntSolgt).
          END.
      END.
      WHEN 3 THEN DO:
          ASSIGN cReturRad = cReturRad + "Kjøpt: ".
          DO iCount = 1 TO NUM-ENTRIES(TT_KjopSalgLager.Kjopant):
              IF ENTRY(iCount,TT_KjopSalgLager.VisEntry) = "1" THEN
              ASSIGN iCount2 = iCount2 + 5
                     cReturRad = cReturRad + "<C" + STRING(iCount2) + "><RIGHT=C+5>" + ENTRY(iCount,TT_KjopSalgLager.Kjopant).
          END.
      END.
      WHEN 4 THEN DO:
          ASSIGN cReturRad = cReturRad + "Lager: ".
          DO iCount = 1 TO NUM-ENTRIES(TT_KjopSalgLager.Lagant):
              IF ENTRY(iCount,TT_KjopSalgLager.VisEntry) = "1" THEN
              ASSIGN iCount2 = iCount2 + 5
                     cReturRad = cReturRad + "<C" + STRING(iCount2) + "><RIGHT=C+5>" + ENTRY(iCount,TT_KjopSalgLager.Lagant).
          END.
      END.
  END CASE.
  RETURN cReturRad + " ".   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

