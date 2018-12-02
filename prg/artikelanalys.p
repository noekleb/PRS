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
DEFINE INPUT  PARAMETER hTTArt         AS HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER cQRY           AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cArtikkelNr    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cSourceButiker AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER lNullposter    AS LOGICAL    NO-UNDO.
DEFINE INPUT  PARAMETER iSortering     AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER cButikkListe   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER dFraDato       AS DATE       NO-UNDO.
DEFINE INPUT  PARAMETER dTilDato       AS DATE       NO-UNDO.
DEFINE INPUT  PARAMETER lPeriode       AS LOG        NO-UNDO.
DEFINE INPUT  PARAMETER lVisStr        AS LOGICAL    NO-UNDO.
DEFINE INPUT  PARAMETER lNullager      AS LOGICAL    NO-UNDO.

DEFINE VARIABLE cButNamn        AS CHARACTER  FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE cColLabelString AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cPrintString    AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cTitle          AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cKundenavn      AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cPolygon        AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE iCL             AS INTEGER                   NO-UNDO.
DEFINE VARIABLE iCLProfilnr     AS INTEGER                   NO-UNDO.
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
DEFINE VARIABLE cStrl2          AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE pcRappFil       AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cVisEntry       AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cWrk            AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE iTotRadS        AS INTEGER                   NO-UNDO.
DEFINE VARIABLE iTotRadL        AS INTEGER                   NO-UNDO.
DEFINE VARIABLE dForsBel        AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE dVerk%          AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE dKalk%          AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE dTotSalg        AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE dTotKost        AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE cTabStrl        AS CHARACTER EXTENT 31       NO-UNDO.
DEFINE VARIABLE cTabSalg        AS CHARACTER EXTENT 31       NO-UNDO.
DEFINE VARIABLE cTabLag         AS CHARACTER EXTENT 31       NO-UNDO.
DEFINE VARIABLE iTabTotSalg     AS INTEGER   EXTENT 31       NO-UNDO.
DEFINE VARIABLE iTabTotLag      AS INTEGER   EXTENT 31       NO-UNDO.
DEFINE VARIABLE VgSpar          AS INTEGER                   NO-UNDO.
DEFINE VARIABLE iUtf%           AS INTEGER                   NO-UNDO.
DEFINE VARIABLE VgFirst         AS LOGICAL                   NO-UNDO.
DEFINE VARIABLE iAntStrl        AS INTEGER                   NO-UNDO.
DEFINE VARIABLE cAntStrl        AS CHARACTER EXTENT 2        NO-UNDO.
DEFINE VARIABLE iTestSnr        AS INTEGER                   NO-UNDO.
DEFINE VARIABLE iTest           AS INTEGER                   NO-UNDO.
DEFINE VARIABLE iButAnt         AS INTEGER                   NO-UNDO.



DEFINE VARIABLE iCols  AS INTEGER  EXTENT 15 INITIAL
    [6,13,27,36,39,46,49,56,63,70,74,85,90,100,105] NO-UNDO.
DEFINE VARIABLE iRight AS INTEGER EXTENT 15 INITIAL
    [0,0,0,0,0,0,1,1,1,1,1,1,1,1,1] NO-UNDO.
DEFINE VARIABLE c AS CHARACTER  NO-UNDO.

DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR hQuery  AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE TT_ArtLager
    FIELD ArtikkelNr  LIKE ArtBas.ArtikkelNr
    FIELD AvdelingNr  LIKE ArtBas.AvdelingNr
    FIELD Hg          LIKE ArtBas.Hg
    FIELD Vg          LIKE ArtBas.Vg
    FIELD Lopnr       LIKE ArtBas.Lopnr
    FIELD LevNr       LIKE ArtBas.LevNr
    FIELD Farg        LIKE ArtBas.Farg
    FIELD MatKod      LIKE ArtBas.MatKod
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
    FIELD PrisOrdi    AS DECIMAL DECIMALS 2
    FIELD PrisTilb    AS DECIMAL DECIMALS 2 /* VareKost */
    FIELD ValPris     AS DECIMAL DECIMALS 2
    FIELD AktPris     AS DECIMAL DECIMALS 2
    FIELD Bildefil    AS CHARACTER
    FIELD FirstInlev AS CHARACTER
    FIELD BestDato   AS CHARACTER
    INDEX LevNr IS PRIMARY LevNr AvdelingNr Hg Vg LopNr
    INDEX VgLopnr Vg Lopnr.

DEFINE TEMP-TABLE tmpTT_ArtLager LIKE TT_ArtLager
             FIELD Lagerverdi AS DECIMAL.
DEFINE TEMP-TABLE tmpTT_ArtLagerTOT LIKE TT_ArtLager
             FIELD Lagerverdi AS DECIMAL.

DEFINE TEMP-TABLE TT_KjopSalgLager
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    FIELD Butik      LIKE Butiker.Butik
    FIELD Storl      AS CHARACTER
    FIELD Antsolgt   AS CHARACTER
    FIELD KjopAnt    AS CHARACTER
    FIELD Lagant     AS CHARACTER
    FIELD VisEntry   AS CHARACTER
    FIELD iAntsolgt  AS INTEGER
    FIELD iKjopAnt   AS INTEGER
    FIELD iLagant    AS INTEGER
    FIELD dAntRab    AS DECIMAL
    FIELD VerdiSolgt LIKE Lager.VerdiSolgt
    FIELD SVK        LIKE Lager.SVK
    INDEX ArtButTyp  IS PRIMARY UNIQUE ArtikkelNr Butik.

DEFINE TEMP-TABLE tmpTT_Kjop
    FIELD Vg         LIKE ArtBas.Vg
    FIELD Butik      LIKE Butiker.Butik
    FIELD iAntsolgt  AS INTEGER
    FIELD iKjopAnt   AS INTEGER
    FIELD iLagant    AS INTEGER
    FIELD dAntRab    AS DECIMAL
    FIELD VerdiSolgt LIKE Lager.VerdiSolgt
    FIELD SVK        LIKE Lager.SVK
    FIELD dKalkFbel  AS DECIMAL
    FIELD dKalkIbel  AS DECIMAL
    INDEX VgBut     IS PRIMARY UNIQUE Vg Butik.

DEFINE TEMP-TABLE TT_Hg
    FIELD Hg        AS INTEGER
    INDEX Hg IS PRIMARY UNIQUE Hg.

DEFINE TEMP-TABLE tmpTT_Hg
    FIELD Hg         LIKE ArtBas.Vg
    FIELD Butik      LIKE Butiker.Butik
    FIELD iAntsolgt  AS INTEGER
    FIELD iKjopAnt   AS INTEGER
    FIELD iLagant    AS INTEGER
    FIELD dAntRab    AS DECIMAL
    FIELD VerdiSolgt LIKE Lager.VerdiSolgt
    FIELD SVK        LIKE Lager.SVK
    FIELD dKalkFbel  AS DECIMAL
    FIELD dKalkIbel  AS DECIMAL
    INDEX HgBut     IS PRIMARY UNIQUE Hg Butik.

DEFINE TEMP-TABLE TT_AvdNr
    FIELD AvdNr      AS INTEGER
    INDEX AvdNr IS PRIMARY UNIQUE AvdNr.

DEFINE TEMP-TABLE tmpTT_AvdNr
    FIELD AvdNr      LIKE ArtBas.AvdelingNr
    FIELD Butik      LIKE Butiker.Butik
    FIELD iAntsolgt  AS INTEGER
    FIELD iKjopAnt   AS INTEGER
    FIELD iLagant    AS INTEGER
    FIELD dAntRab    AS DECIMAL
    FIELD VerdiSolgt LIKE Lager.VerdiSolgt
    FIELD SVK        LIKE Lager.SVK
    FIELD dKalkFbel  AS DECIMAL
    FIELD dKalkIbel  AS DECIMAL
    INDEX AvdBut     IS PRIMARY UNIQUE AvdNr Butik.

DEFINE TEMP-TABLE tmpTT_Tot
    FIELD Butik      LIKE Butiker.Butik
    FIELD iAntsolgt  AS INTEGER
    FIELD iKjopAnt   AS INTEGER
    FIELD iLagant    AS INTEGER
    FIELD dAntRab    AS DECIMAL
    FIELD VerdiSolgt LIKE Lager.VerdiSolgt
    FIELD SVK        LIKE Lager.SVK
    FIELD dKalkFbel  AS DECIMAL
    FIELD dKalkIbel  AS DECIMAL
    INDEX But        IS PRIMARY UNIQUE Butik.
DEFINE TEMP-TABLE tmpTT_VgTot
    FIELD Vg        AS INTEGER
    FIELD Butik     LIKE Butiker.Butik
    FIELD Storlek   AS CHARACTER
    FIELD iAntSolgt AS INTEGER
    FIELD iKjopAnt  AS INTEGER
    FIELD iLagAnt   AS INTEGER
    FIELD cVisEntry AS CHARACTER
    INDEX VgButik IS PRIMARY UNIQUE Vg Butik Storlek
    INDEX Istrl Storlek.

DEFINE TEMP-TABLE TT_image NO-UNDO
    FIELD image_name    AS CHARACTER
INDEX obj_name AS PRIMARY
      image_name.

DEFINE VARIABLE cPerTxt   AS CHAR FORMAT "x(70)" NO-UNDO.

/*DEFINE FRAME PageHeader
   HEADER
      "<ALIGN=BASE><FArial><R3><P12><B><C6>" STRING(TODAY)
      "<R4><P12><C108><P9> Side: " PAGE-NUMBER FORMAT ">>" SKIP
      "<R4><C6><B>Butikk:" cButNamn "<C70>" cPerTxt "</B>" SKIP
      "<R5><C6><FROM><R5><C113><LINE>" SKIP
      WITH PAGE-TOP STREAM-IO WIDTH 255.*/

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

&IF DEFINED(EXCLUDE-getBildeFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBildeFil Procedure 
FUNCTION getBildeFil RETURNS CHARACTER
    ( INPUT ipBildNr AS INTEGER )  FORWARD.

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

{syspara.i 1 1 100 cKundenavn}
{syspara.i 1 1 101 cPolygon}
{syspara.i 5 1 1 iCl INT}

DEFINE VARIABLE iAntal   AS INTEGER     NO-UNDO.

FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
IF AVAIL bruker THEN
   cSprak = TRIM(Bruker.Lng).

IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
  ASSIGN cToday = STRING(YEAR(TODAY),"9999") + "-"
                + STRING(MONTH(TODAY),"99") + "-"
                + STRING(DAY(TODAY),"99").
ELSE
  ASSIGN cToday = STRING(TODAY).

IF dFraDato = ? OR dTilDato = ? THEN
    lPeriode = FALSE.
/*IF iSortering = 2 THEN
  ASSIGN iSortering = 1.*/

FIND Butiker WHERE Butiker.Butik = iCL NO-LOCK NO-ERROR.
ASSIGN iCLProfilNr = Butiker.ProfilNr.
{sww.i}

RUN ByggTmpLager.

/*ASSIGN VgFirst = TRUE
       iAntStrl = 0.
FOR EACH tmpTT_VgTot USE-INDEX Istrl NO-LOCK:
    IF VgFirst = TRUE THEN
    DO:
      MESSAGE tmpTT_VgTot.Vg tmpTT_VgTot.Butik tmpTT_VgTot.Storlek SKIP
              "Sålt " tmpTT_VgTot.iAntSolgt SKIP
              "Köpt " tmpTT_VgTot.iKjopAnt SKIP
              "Lager " tmpTT_VgTot.iLagAnt
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ASSIGN VgFirst = FALSE
           cStrl2 = tmpTT_VgTot.Storlek
           iAntStrl = 1
           cAntStrl = cStrl2.
    END.
    ELSE IF cStrl2 <> tmpTT_VgTot.Storlek THEN
    DO:
      MESSAGE tmpTT_VgTot.Vg tmpTT_VgTot.Butik tmpTT_VgTot.Storlek SKIP
              "Sålt " tmpTT_VgTot.iAntSolgt SKIP
              "Köpt " tmpTT_VgTot.iKjopAnt SKIP
              "Lager " tmpTT_VgTot.iLagAnt
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ASSIGN cStrl2 = tmpTT_VgTot.Storlek
             iAntStrl = iAntStrl + 1
             cAntStrl = cAntStrl + "," + cStrl2.
    END.
END.
MESSAGE "Antal: " iAntStrl SKIP
         cAntStrl
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

/* DEFINE TEMP-TABLE tmpTT_VgTot
   FIELD Vg        AS INTEGER
   FIELD Butik     LIKE Butiker.Butik
   FIELD Storlek   AS CHARACTER
   FIELD iAntSolgt AS INTEGER
   FIELD iKjopAnt  AS INTEGER
   FIELD iLagAnt   AS INTEGER
   FIELD cVisEntry AS CHARACTER
   INDEX VgButik IS PRIMARY UNIQUE Vg Butik.*/


RUN PDFArtikelAnalys.
{swn.i}

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
  DEFINE VARIABLE lSumSolgt    AS DECIMAL        NO-UNDO.
  DEFINE VARIABLE lSumKjopt    AS DECIMAL        NO-UNDO.
  DEFINE VARIABLE cRetur       AS CHARACTER      NO-UNDO.
  DEFINE VARIABLE cFirstInlev  AS CHARACTER      NO-UNDO.
  DEFINE VARIABLE cBestDato    AS CHARACTER      NO-UNDO.
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
/* cFirstInlev  + cBestDato */
FIND FIRST translogg WHERE translogg.artikkelnr = artbas.artikkelnr AND ttid = 5 NO-LOCK NO-ERROR.
cFirstInlev = IF AVAIL translogg THEN "F.Inlev: " + STRING(translogg.dato) ELSE "".
FIND FIRST BestHode WHERE BestHode.ArtikkelNr = artbas.artikkelnr NO-LOCK NO-ERROR.
cBestDato = IF AVAIL BestHode THEN "B.LevDat: " + STRING(BestHode.LevDato) ELSE "".
/*  */
              CREATE TT_ArtLager.
              ASSIGN TT_ArtLager.ArtikkelNr = ArtBas.ArtikkelNr 
                     TT_ArtLager.AvdelingNr = ArtBas.AvdelingNr
                     TT_ArtLager.Hg         = ArtBas.Hg
                     TT_ArtLager.Vg         = ArtBas.Vg         
                     TT_ArtLager.Lopnr      = ArtBas.Lopnr      
                     TT_ArtLager.LevNr      = ArtBas.LevNr      
                     TT_ArtLager.Farg       = ArtBas.Farg      
                     TT_ArtLager.MatKod     = ArtBas.MatKod
                     TT_ArtLager.Beskr      = IF LENGTH(TRIM(ArtBas.Beskr)) > 10 THEN SUBSTRING(TRIM(ArtBas.Beskr),1,10) ELSE ArtBas.Beskr     
                     TT_ArtLager.LevArtNr   = IF LENGTH(TRIM(ArtBas.LevKod)) > 9 THEN SUBSTR(TRIM(ArtBas.LevKod),1,8) + ">" ELSE ArtBas.LevKod
                     TT_Artlager.PrisOrdi   = ArtPris.Pris[1]
                     TT_ArtLager.PrisTilb   = ArtPris.VareKost[1]
                     TT_ArtLager.ValPris    = ArtPris.ValPris[1]
                     iLagAnt                = 0
                     dVVarekost             = 0
                     TT_ArtLager.FirstInlev = cFirstInlev
                     TT_ArtLager.Bestdato = cBestDato
                     TT_ArtLager.Bildefil = getBildeFil(ArtBas.Bildnr).
                     IF Artpris.Tilbud = TRUE THEN
                       ASSIGN TT_ArtLager.AktPris = ArtPris.Pris[2].
                     ELSE
                       ASSIGN TT_ArtLager.AktPris = ArtPris.Pris[1].
/*                     TT_Artlager.PrisTilb   = IF Artpris.Tilbud THEN ArtPris.Pris[2] ELSE 0 */

              FOR EACH Lager OF ArtBas NO-LOCK WHERE CAN-DO(cButikkListe,STRING(Lager.Butik)):  
                  /* Ikke periodisert */
                  IF lPeriode = FALSE THEN
                  DO:
                      ASSIGN 
                          TT_ArtLager.AntSolgt    = TT_ArtLager.AntSolgt    + Lager.AntSolgt   
                          TT_ArtLager.VerdiSolgt  = TT_ArtLager.VerdiSolgt  + Lager.VerdiSolgt 
                          TT_ArtLager.KjopAnt     = TT_ArtLager.KjopAnt     + Lager.KjopAnt 
                          TT_ArtLager.KjopAnt     = TT_ArtLager.KjopAnt     + Lager.OvAnt
                          TT_ArtLager.KjopVerdi   = TT_ArtLager.KjopVerdi   + Lager.KjopVerdi 
                          TT_ArtLager.KjopVerdi   = TT_ArtLager.KjopVerdi   + Lager.OvVerdi
                          TT_ArtLager.AntRab      = TT_ArtLager.AntRab      + Lager.AntRab     
                          TT_ArtLager.VerdiRabatt = TT_ArtLager.VerdiRabatt + Lager.VerdiRabatt.
                  END.

                  /* Periodisert */
                  ELSE DO:
                      ASSIGN cRetur = getAntVerdi(Lager.ArtikkelNr,Lager.Butik, 1) /* Salg */.
                      ASSIGN 
                          TT_ArtLager.AntSolgt    = TT_ArtLager.AntSolgt    + dec(entry(1,cRetur,'|'))   
                          TT_ArtLager.VerdiSolgt  = TT_ArtLager.VerdiSolgt  + dec(entry(2,cRetur,'|')) 
                          TT_ArtLager.AntRab      = TT_ArtLager.AntRab      + dec(entry(3,cRetur,'|'))     
                          TT_ArtLager.VerdiRabatt = TT_ArtLager.VerdiRabatt + dec(entry(4,cRetur,'|')).

                      ASSIGN cRetur = getAntVerdi(Lager.ArtikkelNr,Lager.Butik, 3) /* Reklamasjon */.
                      ASSIGN 
                          TT_ArtLager.AntSolgt    = TT_ArtLager.AntSolgt    - dec(entry(1,cRetur,'|'))   
                          TT_ArtLager.VerdiSolgt  = TT_ArtLager.VerdiSolgt  - dec(entry(2,cRetur,'|')) 
                          TT_ArtLager.AntRab      = TT_ArtLager.AntRab      - dec(entry(3,cRetur,'|'))     
                          TT_ArtLager.VerdiRabatt = TT_ArtLager.VerdiRabatt - dec(entry(4,cRetur,'|')).

                      ASSIGN cRetur = getAntVerdi(Lager.ArtikkelNr,Lager.Butik, 10) /* Retur */. 
                      ASSIGN 
                          TT_ArtLager.AntSolgt    = TT_ArtLager.AntSolgt    - dec(entry(1,cRetur,'|'))   
                          TT_ArtLager.VerdiSolgt  = TT_ArtLager.VerdiSolgt  - dec(entry(2,cRetur,'|')) 
                          TT_ArtLager.AntRab      = TT_ArtLager.AntRab      - dec(entry(3,cRetur,'|'))     
                          TT_ArtLager.VerdiRabatt = TT_ArtLager.VerdiRabatt - dec(entry(4,cRetur,'|')).
                          
                      ASSIGN cRetur = getAntVerdi(Lager.ArtikkelNr,Lager.Butik,  5) /* Kjøpt */. 
                      ASSIGN 
                          TT_ArtLager.KjopAnt     = TT_ArtLager.KjopAnt     + dec(entry(1,cRetur,'|'))    
                          TT_ArtLager.KjopVerdi   = TT_ArtLager.KjopVerdi   + dec(entry(2,cRetur,'|')).  
                  END.

                  /* Lagersum */
                  ASSIGN TT_ArtLager.LagAnt      = TT_ArtLager.LagAnt      + Lager.LagAnt     
                         iLagAnt                 = iLagAnt + IF Lager.LagAnt > 0 THEN Lager.LagAnt ELSE 0
                         dVVarekost              = dVVarekost + IF Lager.LagAnt > 0 THEN Lager.LagAnt * Lager.VVarekost ELSE 0.

              END.
              
              ASSIGN 
                  TT_ArtLager.VVarekost = ArtPris.Varekost[1]. /*IF iLagAnt > 0 
                                            THEN dVVarekost / iLagAnt 
                                            ELSE ArtPris.Varekost[1]. */ /* ghg/20150819 */
                                            /*ELSE 0. ghg/20150819*/
              /* Bygger liste med størrelser */
              ASSIGN cStorlArray = "".
              FIND StrType OF ArtBas NO-LOCK NO-ERROR.
              IF AVAIL StrType THEN 
                FOR EACH StrTstr OF StrType NO-LOCK.
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
                      FIND FIRST Lager WHERE Lager.ArtikkelNr = ArtLag.ArtikkelNr AND
                                             Lager.Butik = ArtLag.Butik NO-ERROR.
                      IF AVAILABLE Lager THEN
                      ASSIGN TT_KjopSalgLager.VerdiSolgt = Lager.VerdiSolgt
                             TT_KjopSalgLager.SVK        = Lager.SVK.
                      ASSIGN TT_KjopSalgLager.ArtikkelNr = ArtLag.ArtikkelNr
                             TT_KjopSalgLager.Butik      = ArtLag.Butik
                             TT_KjopSalgLager.Storl      = cStorlArray
                             TT_KjopSalgLager.Antsolgt   = FILL(",",NUM-ENTRIES(cStorlArray) - 1)
                             TT_KjopSalgLager.Kjopant    = TT_KjopSalgLager.Antsolgt
                             TT_KjopSalgLager.Lagant     = TT_KjopSalgLager.Antsolgt
                             TT_KjopSalgLager.dAntRab    = ArtLag.AntRab
                             cVisEntry                   = IF cVisEntry <> "" THEN cVisEntry ELSE TT_KjopSalgLager.Antsolgt.
                  END.
/*                FIND tmpTT_VgTot WHERE  tmpTT_VgTot.Vg = TT_ArtLager.Vg AND tmpTT_VgTot.Butik = ArtLag.Butik NO-ERROR.
                IF NOT AVAILABLE tmpTT_VgTot THEN
                DO:
                  CREATE tmpTT_VgTot.
                  ASSIGN tmpTT_VgTot.Vg = TT_ArtLager.Vg
                         tmpTT_VgTot.Butik = ArtLag.Butik
                         tmpTT_VgTot.Storlek = cStorlArray
                         tmpTT_VgTot.cVisEntry = cVisEntry.
/*                  MESSAGE tmpTT_VgTot.cVisEntry
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
/* DEFINE TEMP-TABLE tmpTT_VgTot
   FIELD Vg        AS INTEGER
   FIELD Butik     LIKE Butiker.Butik
   FIELD Storlek   AS CHARACTER
   FIELD iAntSolgt AS INTEGER
   FIELD iKjopAnt  AS INTEGER
   FIELD iLagAnt   AS INTEGER
   FIELD cVisEntry AS CHARACTER
   INDEX VgButik IS PRIMARY UNIQUE Vg Butik.*/
                END.*/

                

                  /* Leser ikke periodisert informasjon */
                  IF lPeriode = FALSE THEN
                  DO:
                      ASSIGN iEntry = LOOKUP(TRIM(ArtLag.Storl),TT_KjopSalgLager.Storl)
                             ENTRY(iEntry,TT_KjopSalgLager.Antsolgt) = STRING(INT(ENTRY(iEntry,TT_KjopSalgLager.Antsolgt)) + ArtLag.AntSolgt) 
                             ENTRY(iEntry,TT_KjopSalgLager.Kjopant) = STRING(INT(ENTRY(iEntry,TT_KjopSalgLager.Kjopant)) + ArtLag.KjopAnt)
                             ENTRY(iEntry,TT_KjopSalgLager.Lagant) = STRING(INT(ENTRY(iEntry,TT_KjopSalgLager.Lagant)) + ArtLag.lagant)
                             ENTRY(iEntry,cVisEntry) = "1"
                             TT_KjopSalgLager.iAntsolgt = TT_KjopSalgLager.iAntsolgt + ArtLag.AntSolgt
                             TT_KjopSalgLager.iKjopAnt  = TT_KjopSalgLager.iKjopAnt  + ArtLag.KjopAnt + ArtLag.OvAnt
                             TT_KjopSalgLager.iLagant   = TT_KjopSalgLager.iLagant   + ArtLag.lagant.
/*                             ENTRY(iEntry,tmpTT_VgTot.cVisEntry) = "1"
                             tmpTT_VgTot.iAntSolgt = tmpTT_VgTot.iAntSolgt + ArtLag.AntSolgt
                             tmpTT_VgTot.iKjopAnt = tmpTT_VgTot.iKjopAnt + ArtLag.KjopAnt + ArtLag.OvAnt
                             tmpTT_VgTot.iLagAnt = tmpTT_VgTot.iLagAnt + ArtLag.lagant.*/

                      IF LENGTH(ENTRY(iEntry,TT_KjopSalgLager.Storl)) = 1 OR LENGTH(ENTRY(iEntry,TT_KjopSalgLager.Storl)) = 3 THEN
                        ASSIGN cStrl2 = " " + ENTRY(iEntry,TT_KjopSalgLager.Storl).
                      ELSE
                        ASSIGN cStrl2 = ENTRY(iEntry,TT_KjopSalgLager.Storl).
                      FIND tmpTT_VgTot WHERE  tmpTT_VgTot.Vg = TT_ArtLager.Vg AND 
                                              tmpTT_VgTot.Butik = ArtLag.Butik AND 
                                              tmpTT_VgTot.Storlek = cStrl2 NO-ERROR.
                      IF NOT AVAILABLE tmpTT_VgTot THEN
                      DO:
                        CREATE tmpTT_VgTot.
                        ASSIGN tmpTT_VgTot.Vg = TT_ArtLager.Vg
                               tmpTT_VgTot.Butik = ArtLag.Butik
                               tmpTT_VgTot.Storlek = cStrl2
                               tmpTT_VgTot.iAntSolgt = tmpTT_VgTot.iAntSolgt + ArtLag.AntSolgt
                               tmpTT_VgTot.iKjopAnt = tmpTT_VgTot.iKjopAnt + ArtLag.KjopAnt + ArtLag.OvAnt
                               tmpTT_VgTot.iLagAnt = tmpTT_VgTot.iLagAnt + ArtLag.lagant.

/*                               tmpTT_VgTot.cVisEntry = cVisEntry.*/
                      END.
                      ELSE
                        ASSIGN tmpTT_VgTot.iAntSolgt = tmpTT_VgTot.iAntSolgt + ArtLag.AntSolgt
                               tmpTT_VgTot.iKjopAnt = tmpTT_VgTot.iKjopAnt + ArtLag.KjopAnt + ArtLag.OvAnt
                               tmpTT_VgTot.iLagAnt = tmpTT_VgTot.iLagAnt + ArtLag.lagant.
/* Totalsummor butik 999 */
                      FIND tmpTT_VgTot WHERE  tmpTT_VgTot.Vg = TT_ArtLager.Vg AND 
                                              tmpTT_VgTot.Butik = 999 AND 
                                              tmpTT_VgTot.Storlek = cStrl2 NO-ERROR.
                      IF NOT AVAILABLE tmpTT_VgTot THEN
                      DO:
                        CREATE tmpTT_VgTot.
                        ASSIGN tmpTT_VgTot.Vg = TT_ArtLager.Vg
                               tmpTT_VgTot.Butik = 999
                               tmpTT_VgTot.Storlek = cStrl2
                               tmpTT_VgTot.iAntSolgt = tmpTT_VgTot.iAntSolgt + ArtLag.AntSolgt
                               tmpTT_VgTot.iKjopAnt = tmpTT_VgTot.iKjopAnt + ArtLag.KjopAnt + ArtLag.OvAnt
                               tmpTT_VgTot.iLagAnt = tmpTT_VgTot.iLagAnt + ArtLag.lagant.
                      END.
                      ELSE
                        ASSIGN tmpTT_VgTot.iAntSolgt = tmpTT_VgTot.iAntSolgt + ArtLag.AntSolgt
                               tmpTT_VgTot.iKjopAnt = tmpTT_VgTot.iKjopAnt + ArtLag.KjopAnt + ArtLag.OvAnt
                               tmpTT_VgTot.iLagAnt = tmpTT_VgTot.iLagAnt + ArtLag.lagant.

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
/*                             ENTRY(iEntry,tmpTT_VgTot.cVisEntry) = "1"
                             tmpTT_VgTot.iAntSolgt = tmpTT_VgTot.iAntSolgt + ArtLag.AntSolgt
                             tmpTT_VgTot.iKjopAnt = tmpTT_VgTot.iKjopAnt + ArtLag.KjopAnt + ArtLag.OvAnt
                             tmpTT_VgTot.iLagAnt = tmpTT_VgTot.iLagAnt + ArtLag.lagant.*/
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

&IF DEFINED(EXCLUDE-PDFAddAvd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFAddAvd Procedure 
PROCEDURE PDFAddAvd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST TT_AvdNr WHERE TT_AvdNr.AvdNr = TT_ArtLager.AvdelingNr NO-LOCK NO-ERROR.
IF NOT AVAILABLE TT_AvdNr THEN
DO:
  CREATE TT_AvdNr.
  ASSIGN TT_AvdNr.AvdNr = TT_ArtLager.AvdelingNr.
END.
FIND FIRST tmpTT_AvdNr WHERE tmpTT_AvdNr.AvdNr = TT_ArtLager.AvdelingNr AND tmpTT_AvdNr.Butik = TT_KjopSalgLager.Butik NO-LOCK NO-ERROR.
IF AVAILABLE tmpTT_AvdNr THEN
  ASSIGN tmpTT_AvdNr.iKjopAnt   = tmpTT_AvdNr.iKjopAnt + TT_KjopSalgLager.iKjopAnt
         tmpTT_AvdNr.iAntsolgt  = tmpTT_AvdNr.iAntsolgt + TT_KjopSalgLager.iAntsolgt
         tmpTT_AvdNr.iLagant    = tmpTT_AvdNr.iLagant + TT_KjopSalgLager.iLagant
         tmpTT_AvdNr.VerdiSolgt = tmpTT_AvdNr.Verdisolgt + TT_KjopSalgLager.Verdisolgt
         tmpTT_AvdNr.SVK        = tmpTT_AvdNr.SVK + TT_KjopSalgLager.SVK
         tmpTT_AvdNr.dKalkFbel  = tmpTT_AvdNr.dKalkFbel + ((TT_ArtLager.PrisOrdi * 0.8) * TT_KjopSalgLager.iAntSolgt)
         tmpTT_AvdNr.dKalkIbel  = tmpTT_AvdNr.dKalkIbel + (TT_ArtLager.VVarekost * TT_KjopSalgLager.iAntSolgt).
ELSE
  DO:
   CREATE tmpTT_AvdNr.
   ASSIGN tmpTT_AvdNr.AvdNr         = TT_ArtLager.AvdelingNr
          tmpTT_AvdNr.Butik      = TT_KjopSalgLager.Butik
          tmpTT_AvdNr.iKjopAnt   = TT_KjopSalgLager.iKjopAnt
          tmpTT_AvdNr.iAntsolgt  = TT_KjopSalgLager.iAntsolgt
          tmpTT_AvdNr.iLagant    = TT_KjopSalgLager.iLagant
          tmpTT_AvdNr.VerdiSolgt = TT_KjopSalgLager.Verdisolgt
          tmpTT_AvdNr.SVK        = TT_KjopSalgLager.SVK
          tmpTT_AvdNr.dKalkFbel  = ((TT_ArtLager.PrisOrdi * 0.8) * TT_KjopSalgLager.iAntSolgt)
          tmpTT_AvdNr.dKalkIbel  = (TT_ArtLager.VVarekost * TT_KjopSalgLager.iAntSolgt).
  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFAddHg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFAddHg Procedure 
PROCEDURE PDFAddHg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST TT_Hg WHERE TT_Hg.Hg = TT_ArtLager.Hg NO-LOCK NO-ERROR.
IF NOT AVAILABLE TT_Hg THEN
DO:
  CREATE TT_Hg.
  ASSIGN TT_Hg.Hg = TT_ArtLager.Hg.
END.
FIND FIRST tmpTT_Hg WHERE tmpTT_Hg.Hg = TT_ArtLager.Hg AND tmpTT_Hg.Butik = TT_KjopSalgLager.Butik NO-LOCK NO-ERROR.
IF AVAILABLE tmpTT_Hg THEN
  ASSIGN tmpTT_Hg.iKjopAnt   = tmpTT_Hg.iKjopAnt + TT_KjopSalgLager.iKjopAnt
         tmpTT_Hg.iAntsolgt  = tmpTT_Hg.iAntsolgt + TT_KjopSalgLager.iAntsolgt
         tmpTT_Hg.iLagant    = tmpTT_Hg.iLagant + TT_KjopSalgLager.iLagant
         tmpTT_Hg.VerdiSolgt = tmpTT_Hg.Verdisolgt + TT_KjopSalgLager.Verdisolgt
         tmpTT_Hg.SVK        = tmpTT_Hg.SVK + TT_KjopSalgLager.SVK
         tmpTT_Hg.dKalkFbel  = tmpTT_Hg.dKalkFbel + ((TT_ArtLager.PrisOrdi * 0.8) * TT_KjopSalgLager.iAntSolgt)
         tmpTT_Hg.dKalkIbel  = tmpTT_Hg.dKalkIbel + (TT_ArtLager.VVarekost * TT_KjopSalgLager.iAntSolgt).
ELSE
  DO:
   CREATE tmpTT_Hg.
   ASSIGN tmpTT_Hg.Hg         = TT_ArtLager.Hg
          tmpTT_Hg.Butik      = TT_KjopSalgLager.Butik
          tmpTT_Hg.iKjopAnt   = TT_KjopSalgLager.iKjopAnt
          tmpTT_Hg.iAntsolgt  = TT_KjopSalgLager.iAntsolgt
          tmpTT_Hg.iLagant    = TT_KjopSalgLager.iLagant
          tmpTT_Hg.VerdiSolgt = TT_KjopSalgLager.Verdisolgt
          tmpTT_Hg.SVK        = TT_KjopSalgLager.SVK
          tmpTT_Hg.dKalkFbel  = ((TT_ArtLager.PrisOrdi * 0.8) * TT_KjopSalgLager.iAntSolgt)
          tmpTT_Hg.dKalkIbel  = (TT_ArtLager.VVarekost * TT_KjopSalgLager.iAntSolgt).
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFAddTot) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFAddTot Procedure 
PROCEDURE PDFAddTot :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST tmpTT_Tot WHERE tmpTT_Tot.Butik = TT_KjopSalgLager.Butik NO-LOCK NO-ERROR.
IF AVAILABLE tmpTT_Tot THEN
  ASSIGN tmpTT_Tot.iKjopAnt   = tmpTT_Tot.iKjopAnt + TT_KjopSalgLager.iKjopAnt
         tmpTT_Tot.iAntsolgt  = tmpTT_Tot.iAntsolgt + TT_KjopSalgLager.iAntsolgt
         tmpTT_Tot.iLagant    = tmpTT_Tot.iLagant + TT_KjopSalgLager.iLagant
         tmpTT_Tot.VerdiSolgt = tmpTT_Tot.Verdisolgt + TT_KjopSalgLager.Verdisolgt
         tmpTT_Tot.SVK        = tmpTT_Tot.SVK + TT_KjopSalgLager.SVK
         tmpTT_Tot.dKalkFbel  = tmpTT_Tot.dKalkFbel + ((TT_ArtLager.PrisOrdi * 0.8) * TT_KjopSalgLager.iAntSolgt)
         tmpTT_Tot.dKalkIbel  = tmpTT_Tot.dKalkIbel + (TT_ArtLager.VVarekost * TT_KjopSalgLager.iAntSolgt).
ELSE
  DO:
   CREATE tmpTT_Tot.
   ASSIGN tmpTT_Tot.Butik      = TT_KjopSalgLager.Butik
          tmpTT_Tot.iKjopAnt   = TT_KjopSalgLager.iKjopAnt
          tmpTT_Tot.iAntsolgt  = TT_KjopSalgLager.iAntsolgt
          tmpTT_Tot.iLagant    = TT_KjopSalgLager.iLagant
          tmpTT_Tot.VerdiSolgt = TT_KjopSalgLager.Verdisolgt
          tmpTT_Tot.SVK        = TT_KjopSalgLager.SVK
          tmpTT_Tot.dKalkFbel  = ((TT_ArtLager.PrisOrdi * 0.8) * TT_KjopSalgLager.iAntSolgt)
          tmpTT_Tot.dKalkIbel  = (TT_ArtLager.VVarekost * TT_KjopSalgLager.iAntSolgt).
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFAddVg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFAddVg Procedure 
PROCEDURE PDFAddVg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST tmpTT_Kjop WHERE tmpTT_Kjop.Vg = TT_ArtLager.Vg AND tmpTT_Kjop.Butik = TT_KjopSalgLager.Butik NO-LOCK NO-ERROR.
IF AVAILABLE tmpTT_Kjop THEN
  ASSIGN tmpTT_Kjop.iKjopAnt   = tmpTT_Kjop.iKjopAnt + TT_KjopSalgLager.iKjopAnt
         tmpTT_Kjop.iAntsolgt  = tmpTT_Kjop.iAntsolgt + TT_KjopSalgLager.iAntsolgt
         tmpTT_Kjop.iLagant    = tmpTT_Kjop.iLagant + TT_KjopSalgLager.iLagant
         tmpTT_Kjop.VerdiSolgt = tmpTT_Kjop.Verdisolgt + TT_KjopSalgLager.Verdisolgt
         tmpTT_Kjop.SVK        = tmpTT_Kjop.SVK + TT_KjopSalgLager.SVK
         tmpTT_Kjop.dKalkFbel  = tmpTT_Kjop.dKalkFbel + ((TT_ArtLager.PrisOrdi * 0.8) * TT_KjopSalgLager.iAntSolgt)
         tmpTT_Kjop.dKalkIbel  = tmpTT_Kjop.dKalkIbel + (TT_ArtLager.VVarekost * TT_KjopSalgLager.iAntSolgt).
ELSE
  DO:
   CREATE tmpTT_Kjop.
   ASSIGN tmpTT_Kjop.Vg         = TT_ArtLager.Vg
          tmpTT_Kjop.Butik      = TT_KjopSalgLager.Butik
          tmpTT_Kjop.iKjopAnt   = TT_KjopSalgLager.iKjopAnt
          tmpTT_Kjop.iAntsolgt  = TT_KjopSalgLager.iAntsolgt
          tmpTT_Kjop.iLagant    = TT_KjopSalgLager.iLagant
          tmpTT_Kjop.VerdiSolgt = TT_KjopSalgLager.Verdisolgt
          tmpTT_Kjop.SVK        = TT_KjopSalgLager.SVK
          tmpTT_Kjop.dKalkFbel  = ((TT_ArtLager.PrisOrdi * 0.8) * TT_KjopSalgLager.iAntSolgt)
          tmpTT_Kjop.dKalkIbel  = (TT_ArtLager.VVarekost * TT_KjopSalgLager.iAntSolgt).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFArtikelAnalys) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFArtikelAnalys Procedure 
PROCEDURE PDFArtikelAnalys :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cQRY1        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQRY2        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQRY3        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iOldLevNr    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lFirst       AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lFirst2      AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cDatFrom     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDatTom      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lFirstAvd    AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lFirstHg     AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE iLM          AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLM2         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLM3         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLM4         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE doRad2       AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE doRad        AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cBildNamn    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iImageHeight AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iImageWidth  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iind         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cIndex       AS CHARACTER   NO-UNDO.

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
                         (IF dFraDato = ? THEN "?" ELSE string(dFraDato)) + 
                          " - " + 
                         (IF dTilDato = ? THEN "?" ELSE string(dTilDato)).
    END.
  ELSE 
      IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
        ASSIGN cPerTxt = "Ej periodicerat".
      ELSE
        ASSIGN cPerTxt = "Ikke periodisert".

  ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "ArtikelAnalys.pdf".

  RUN pdf_new ("Spdf",pcRappFil).
  pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PDFPageFooter").
  RUN pdf_set_PaperType ("Spdf","A4").
  RUN pdf_set_LeftMargin ("Spdf", 30).
  RUN pdf_set_BottomMargin ("Spdf", 40).
  RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf", 13).
/*  RUN pdf_set_Orientation ("Spdf", "landscape").*/
  RUN pdf_set_Orientation ("Spdf","Portrait").
  ASSIGN iLM = pdf_LeftMargin ("Spdf").

  ASSIGN lFirstAvd = TRUE
         lFirstHg = TRUE
         lFirst = TRUE.

  IF iSortering = 1 THEN
  DO:
    FOR EACH TT_ArtLager USE-INDEX VgLopnr NO-LOCK:
     {Artikelanalys.i}
  END.

  IF iSortering = 2 THEN
  DO:
    FOR EACH TT_ArtLager USE-INDEX LevNr NO-LOCK:
     {Artikelanalys.i}
  END.

/*  FOR EACH TT_AvdNr NO-LOCK:
      MESSAGE TT_AvdNr.AvdNr
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.*/
  RUN PDFPrintVg (INPUT-OUTPUT doRad,INPUT iLM).
  RUN PDFPrintHg (INPUT-OUTPUT doRad,INPUT iLM).
  RUN PDFPrintAvd (INPUT-OUTPUT doRad,INPUT iLM).
  RUN PDFPrintTot (INPUT-OUTPUT doRad,INPUT iLM). 

  RUN pdf_close ("Spdf").

  RUN browse2pdf\viewxmldialog.w (pcRappFil,"Polygon Retail Solutions").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFNysida) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFNysida Procedure 
PROCEDURE PDFNysida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*DEFINE INPUT PARAMETER iTest AS INTEGER NO-UNDO.*/
DEFINE INPUT-OUTPUT PARAMETER doRad AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER iLM AS INTEGER NO-UNDO.
DEFINE VARIABLE        iLM2 AS INTEGER     NO-UNDO.
  RUN pdf_new_page ("Spdf").
  ASSIGN doRad = pdf_PageHeight ("Spdf") - 35.
  RUN pdf_stroke_fill ("Spdf",.85,.85,.85).  
  RUN pdf_rect ("Spdf", iLM - 5, doRad - 3, iLM + 510, 15,0.5).      /* Left,Bottom,Length,Height*/
  RUN pdf_stroke_fill ("Spdf",1.0,1.0,1.0).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
  RUN pdf_text_xy_dec ("Spdf",cToday,iLM,doRad).
  RUN pdf_text_xy_dec ("Spdf","ArtikelAnalys",iLM + 200,doRad).
  ASSIGN iLM2 = (iLM + 530) - bredd(cKundenavn).
  RUN pdf_text_xy_dec ("Spdf",cKundenavn,iLM2,doRad).
  ASSIGN doRad = doRad - 20.
/*  MESSAGE "iTestSnr: " iTest ": " iTestSnr
      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

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
  RUN pdf_text_xy_dec ("Spdf",cPolygon,pdf_Pagewidth ("Spdf") - pdf_LeftMargin ("Spdf") - 350,pdf_BottomMargin ("Spdf") - 14).
  cSidTxt = TRIM("Sida: " + STRING(pdf_page("Spdf")) + " (" + pdf_TotalPages("Spdf") + ")").
  ASSIGN iTestSnr = pdf_page("Spdf").
  RUN pdf_text_xy_dec ("Spdf",cSidTxt,pdf_Pagewidth ("Spdf") - pdf_LeftMargin ("Spdf") - 50,pdf_BottomMargin ("Spdf") - 14).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFPrintAvd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFPrintAvd Procedure 
PROCEDURE PDFPrintAvd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER doRad AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER iLM   AS INTEGER         NO-UNDO.
DEFINE VARIABLE        iLM2  AS INTEGER         NO-UNDO.
DEFINE VARIABLE  iAntsolgtT  AS INTEGER         NO-UNDO.
DEFINE VARIABLE  iKjopAntT   AS INTEGER         NO-UNDO.
DEFINE VARIABLE  iLagantT    AS INTEGER         NO-UNDO.
DEFINE VARIABLE  dAntRabT    AS DECIMAL         NO-UNDO.
DEFINE VARIABLE  VerdiSolgtT AS DECIMAL         NO-UNDO.
DEFINE VARIABLE  SVKT        AS DECIMAL         NO-UNDO.
DEFINE VARIABLE  dKalkFbelT  AS DECIMAL         NO-UNDO.
DEFINE VARIABLE  dKalkIbelT  AS DECIMAL         NO-UNDO.

FOR EACH TT_AvdNr USE-INDEX AvdNr NO-LOCK:
  ASSIGN doRad = doRad - 15.
  IF doRad < 90 THEN
  DO:
    ASSIGN iTest = 11.
    RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
  END.
  ELSE
  DO:
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    RUN pdf_line IN h_PDFinc  ("Spdf", iLM, doRad + 10, pdf_PageWidth("Spdf") - iLM, doRad + 10, 0.5).
  END.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
  RUN pdf_text_xy_dec ("Spdf","Totalt för Avd " + STRING(TT_AvdNr.AvdNr) + ":",iLM,doRad).
  RUN pdf_text_xy_dec ("Spdf","But",iLM + 100,doRad).
  RUN pdf_text_xy_dec ("Spdf","Inlev",iLM + 220,doRad).
  RUN pdf_text_xy_dec ("Spdf","Sålt",iLM + 260,doRad).
  RUN pdf_text_xy_dec ("Spdf","Lag",iLM + 300,doRad).
  RUN pdf_text_xy_dec ("Spdf","Utf %",iLM + 340,doRad).
  RUN pdf_text_xy_dec ("Spdf","Förs.Bel",iLM + 400,doRad).
  RUN pdf_text_xy_dec ("Spdf","Verk%",iLM + 460,doRad).
  RUN pdf_text_xy_dec ("Spdf","Kalk%",iLM + 505,doRad).

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", iLM + 100, doRad - 3, pdf_PageWidth("Spdf") - iLM, doRad - 3, 0.5).

  ASSIGN dy = doRad.

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).

  ASSIGN iAntsolgtT  = 0
         iKjopAntT   = 0
         iLagantT    = 0
         dAntRabT    = 0
         VerdiSolgtT = 0
         SVKT        = 0
         dKalkFbelT  = 0
         dKalkIbelT  = 0.

 
  FOR EACH tmpTT_AvdNr WHERE tmpTT_AvdNr.AvdNr = TT_AvdNr.AvdNr USE-INDEX AvdBut NO-LOCK:
      ASSIGN iAntsolgtT  = iAntsolgtT  + tmpTT_AvdNr.iAntsolgt
             iKjopAntT   = iKjopAntT   + tmpTT_AvdNr.iKjopAnt
             iLagantT    = iLagantT    + tmpTT_AvdNr.iLagant
             dAntRabT    = dAntRabT    + tmpTT_AvdNr.dAntRab
             VerdiSolgtT = VerdiSolgtT + tmpTT_AvdNr.VerdiSolgt
             SVKT        = SVKT        + tmpTT_AvdNr.SVK
             dKalkFbelT  = dKalkFbelT  + tmpTT_AvdNr.dKalkFbel
             dKalkIbelT  = dKalkIbelT  + tmpTT_AvdNr.dKalkIbel.

      IF dY < 60 THEN
      DO:
        ASSIGN iTest = 12.
        RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
        ASSIGN dY = doRad.
      END.
      ASSIGN dY = dY - 13
             cWrk = STRING(tmpTT_AvdNr.Butik,"z9")
             iLM2 = iLM + 115.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).             
      ASSIGN cWrk = STRING(tmpTT_AvdNr.iKjopAnt,"-zzzz9")
             iLM2 = iLM + 240.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      ASSIGN cWrk = STRING(tmpTT_AvdNr.iAntsolgt,"-zzzz9")
             iLM2 = iLM + 280.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).  
      ASSIGN cWrk = STRING(tmpTT_AvdNr.iLagant,"-zzzz9")
             iLM2 = iLM + 320.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      ASSIGN iUtf% = (tmpTT_AvdNr.iAntsolgt / tmpTT_AvdNr.iKjopAnt) * 100
             iLM2 = iLM + 355.
      IF tmpTT_AvdNr.iKjopAnt <> 0 THEN
      DO:
        ASSIGN cWrk = STRING(iUtf%,"-zzzz9").
        RUN pdf_text_xy_dec ("Spdf",cWrk + " %",iLM2 - bredd(cWrk),dY).
      END.
      ELSE
      DO:
        ASSIGN iLM2 = iLM + 345.
        RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
      END.
      ASSIGN dForsBel = tmpTT_AvdNr.VerdiSolgt 
             dTotSalg = dTotSalg + dForsBel
             dTotKost = dTotKost + tmpTT_AvdNr.SVK 
             iLM2 = iLM + 440
             cWrk = STRING(dForsBel,"-zzz,zz9.99").
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      IF tmpTT_AvdNr.iAntsolgt <> 0 THEN
      DO:
        ASSIGN dVerk% = ((dForsBel - tmpTT_AvdNr.SVK ) / (dForsBel )) * 100
               cWrk = STRING(dVerk%,"-zzz9.99")
               iLM2 = iLM + 490.
        RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      END.
      ELSE
      DO:
        ASSIGN iLM2 = iLM + 475.
        RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
      END.
      ASSIGN dKalk%   =  ((tmpTT_AvdNr.dKalkFbel - (tmpTT_AvdNr.dKalkIbel)) / tmpTT_AvdNr.dKalkFbel) * 100
             iLM2 = iLM + 535
             cWrk = STRING(dKalk%,"-zzz9.99").
      IF tmpTT_AvdNr.dKalkFbel <> 0 THEN
      DO:
        RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      END.
      ELSE
      DO:
        ASSIGN iLM2 = iLM + 520.
        RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
      END.      
  END.

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", iLM + 100, dY - 3, pdf_PageWidth("Spdf") - iLM, dY - 3, 0.5).

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).

  IF dY < 60 THEN
  DO:
    ASSIGN iTest = 13.
    RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
    ASSIGN dY = doRad.
  END.
  ASSIGN dY = dY - 13
       iLM2 = iLM + 105.
  RUN pdf_text_xy_dec ("Spdf","Tot:",iLM2,dY).             
  ASSIGN cWrk = STRING(iKjopAntT,"-zzzz9")
         iLM2 = iLM + 240.
  RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
  ASSIGN cWrk = STRING(iAntsolgtT,"-zzzz9")
       iLM2 = iLM + 280.
  RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).  
  ASSIGN cWrk = STRING(iLagantT,"-zzzz9")
         iLM2 = iLM + 320.
  RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
  ASSIGN iUtf% = (iAntsolgtT / iKjopAntT) * 100
         iLM2 = iLM + 355.
  IF iKjopAntT <> 0 THEN
  DO:
    ASSIGN cWrk = STRING(iUtf%,"-zzzz9").
    RUN pdf_text_xy_dec ("Spdf",cWrk + " %",iLM2 - bredd(cWrk),dY).
  END.
  ELSE
  DO:
    ASSIGN iLM2 = iLM + 345.
    RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
  END.
  ASSIGN dForsBel = VerdiSolgtT
         iLM2 = iLM + 440
         cWrk = STRING(dForsBel,"-zzz,zzz,zz9.99").
  RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
  IF iAntsolgtT <> 0 THEN
  DO:
    ASSIGN dVerk% = ((dForsBel - SVKT ) / (dForsBel )) * 100
           cWrk = STRING(dVerk%,"-zzz9.99")
           iLM2 = iLM + 490.
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
  END.
  ELSE
  DO:
    ASSIGN iLM2 = iLM + 475.
    RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
  END.
  ASSIGN dKalk%   =  ((dKalkFbelT - (dKalkIbelT)) / dKalkFbelT) * 100
         iLM2 = iLM + 535
         cWrk = STRING(dKalk%,"-zzz9.99").
  IF dKalkFbelT <> 0 THEN
  DO:
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
  END.
  ELSE
  DO:
    ASSIGN iLM2 = iLM + 520.
    RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
  END.

  ASSIGN doRad = dY.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFPrintHg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFPrintHg Procedure 
PROCEDURE PDFPrintHg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER doRad AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER iLM   AS INTEGER         NO-UNDO.
DEFINE VARIABLE        iLM2  AS INTEGER         NO-UNDO.
DEFINE VARIABLE  iAntsolgtT  AS INTEGER         NO-UNDO.
DEFINE VARIABLE  iKjopAntT   AS INTEGER         NO-UNDO.
DEFINE VARIABLE  iLagantT    AS INTEGER         NO-UNDO.
DEFINE VARIABLE  dAntRabT    AS DECIMAL         NO-UNDO.
DEFINE VARIABLE  VerdiSolgtT AS DECIMAL         NO-UNDO.
DEFINE VARIABLE  SVKT        AS DECIMAL         NO-UNDO.
DEFINE VARIABLE  dKalkFbelT  AS DECIMAL         NO-UNDO.
DEFINE VARIABLE  dKalkIbelT  AS DECIMAL         NO-UNDO.

FOR EACH TT_Hg USE-INDEX Hg NO-LOCK:
  ASSIGN doRad = doRad - 15.
  IF doRad < 90 THEN
  DO:
    ASSIGN iTest = 14.
    RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
  END.
  ELSE
  DO:
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    RUN pdf_line IN h_PDFinc  ("Spdf", iLM, doRad + 10, pdf_PageWidth("Spdf") - iLM, doRad + 10, 0.5).
  END.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
  RUN pdf_text_xy_dec ("Spdf","Totalt för HGr " + STRING(TT_Hg.Hg) + ":",iLM,doRad).
  RUN pdf_text_xy_dec ("Spdf","But",iLM + 100,doRad).
  RUN pdf_text_xy_dec ("Spdf","Inlev",iLM + 220,doRad).
  RUN pdf_text_xy_dec ("Spdf","Sålt",iLM + 260,doRad).
  RUN pdf_text_xy_dec ("Spdf","Lag",iLM + 300,doRad).
  RUN pdf_text_xy_dec ("Spdf","Utf %",iLM + 340,doRad).
  RUN pdf_text_xy_dec ("Spdf","Förs.Bel",iLM + 400,doRad).
  RUN pdf_text_xy_dec ("Spdf","Verk%",iLM + 460,doRad).
  RUN pdf_text_xy_dec ("Spdf","Kalk%",iLM + 505,doRad).

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", iLM + 100, doRad - 3, pdf_PageWidth("Spdf") - iLM, doRad - 3, 0.5).

  ASSIGN dy = doRad.

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).

  ASSIGN iAntsolgtT  = 0
         iKjopAntT   = 0
         iLagantT    = 0
         dAntRabT    = 0
         VerdiSolgtT = 0
         SVKT        = 0
         dKalkFbelT  = 0
         dKalkIbelT  = 0.

 
  FOR EACH tmpTT_Hg WHERE tmpTT_Hg.Hg = TT_Hg.Hg USE-INDEX HgBut NO-LOCK:
      ASSIGN iAntsolgtT  = iAntsolgtT  + tmpTT_Hg.iAntsolgt
             iKjopAntT   = iKjopAntT   + tmpTT_Hg.iKjopAnt
             iLagantT    = iLagantT    + tmpTT_Hg.iLagant
             dAntRabT    = dAntRabT    + tmpTT_Hg.dAntRab
             VerdiSolgtT = VerdiSolgtT + tmpTT_Hg.VerdiSolgt
             SVKT        = SVKT        + tmpTT_Hg.SVK
             dKalkFbelT  = dKalkFbelT  + tmpTT_Hg.dKalkFbel
             dKalkIbelT  = dKalkIbelT  + tmpTT_Hg.dKalkIbel.

      IF dY < 60 THEN
      DO:
        ASSIGN iTest = 15.
        RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
        ASSIGN dY = doRad.
      END.
      ASSIGN dY = dY - 13
             cWrk = STRING(tmpTT_Hg.Butik,"z9")
             iLM2 = iLM + 115.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).             
      ASSIGN cWrk = STRING(tmpTT_Hg.iKjopAnt,"-zzzz9")
             iLM2 = iLM + 240.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      ASSIGN cWrk = STRING(tmpTT_Hg.iAntsolgt,"-zzzz9")
             iLM2 = iLM + 280.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).  
      ASSIGN cWrk = STRING(tmpTT_Hg.iLagant,"-zzzz9")
             iLM2 = iLM + 320.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      ASSIGN iUtf% = (tmpTT_Hg.iAntsolgt / tmpTT_Hg.iKjopAnt) * 100
             iLM2 = iLM + 355.
      IF tmpTT_Hg.iKjopAnt <> 0 THEN
      DO:
        ASSIGN cWrk = STRING(iUtf%,"-zzzz9").
        RUN pdf_text_xy_dec ("Spdf",cWrk + " %",iLM2 - bredd(cWrk),dY).
      END.
      ELSE
      DO:
        ASSIGN iLM2 = iLM + 345.
        RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
      END.
      ASSIGN dForsBel = tmpTT_Hg.VerdiSolgt 
             dTotSalg = dTotSalg + dForsBel
             dTotKost = dTotKost + tmpTT_Hg.SVK 
             iLM2 = iLM + 440
             cWrk = STRING(dForsBel,"-zzz,zz9.99").
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      IF tmpTT_Hg.iAntsolgt <> 0 THEN
      DO:
        ASSIGN dVerk% = ((dForsBel - tmpTT_Hg.SVK ) / (dForsBel )) * 100
               cWrk = STRING(dVerk%,"-zzz9.99")
               iLM2 = iLM + 490.
        RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      END.
      ELSE
      DO:
        ASSIGN iLM2 = iLM + 475.
        RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
      END.
      ASSIGN dKalk%   =  ((tmpTT_Hg.dKalkFbel - (tmpTT_Hg.dKalkIbel)) / tmpTT_Hg.dKalkFbel) * 100
             iLM2 = iLM + 535
             cWrk = STRING(dKalk%,"-zzz9.99").
      IF tmpTT_Hg.dKalkFbel <> 0 THEN
      DO:
        RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      END.
      ELSE
      DO:
        ASSIGN iLM2 = iLM + 520.
        RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
      END.      
  END.

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", iLM + 100, dY - 3, pdf_PageWidth("Spdf") - iLM, dY - 3, 0.5).

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).

  IF dY < 60 THEN
  DO:
    ASSIGN iTest = 16.
    RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
    ASSIGN dY = doRad.
  END.
  ASSIGN dY = dY - 13
       iLM2 = iLM + 105.
  RUN pdf_text_xy_dec ("Spdf","Tot:",iLM2,dY).             
  ASSIGN cWrk = STRING(iKjopAntT,"-zzzz9")
         iLM2 = iLM + 240.
  RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
  ASSIGN cWrk = STRING(iAntsolgtT,"-zzzz9")
       iLM2 = iLM + 280.
  RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).  
  ASSIGN cWrk = STRING(iLagantT,"-zzzz9")
         iLM2 = iLM + 320.
  RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
  ASSIGN iUtf% = (iAntsolgtT / iKjopAntT) * 100
         iLM2 = iLM + 355.
  IF iKjopAntT <> 0 THEN
  DO:
    ASSIGN cWrk = STRING(iUtf%,"-zzzz9").
    RUN pdf_text_xy_dec ("Spdf",cWrk + " %",iLM2 - bredd(cWrk),dY).
  END.
  ELSE
  DO:
    ASSIGN iLM2 = iLM + 345.
    RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
  END.
  ASSIGN dForsBel = VerdiSolgtT
         iLM2 = iLM + 440
         cWrk = STRING(dForsBel,"-zzz,zzz,zz9.99").
  RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
  IF iAntsolgtT <> 0 THEN
  DO:
    ASSIGN dVerk% = ((dForsBel - SVKT ) / (dForsBel )) * 100
           cWrk = STRING(dVerk%,"-zzz9.99")
           iLM2 = iLM + 490.
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
  END.
  ELSE
  DO:
    ASSIGN iLM2 = iLM + 475.
    RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
  END.
  ASSIGN dKalk%   =  ((dKalkFbelT - (dKalkIbelT)) / dKalkFbelT) * 100
         iLM2 = iLM + 535
         cWrk = STRING(dKalk%,"-zzz9.99").
  IF dKalkFbelT <> 0 THEN
  DO:
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
  END.
  ELSE
  DO:
    ASSIGN iLM2 = iLM + 520.
    RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
  END.

  ASSIGN doRad = dY.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFPrintTot) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFPrintTot Procedure 
PROCEDURE PDFPrintTot :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER doRad AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER iLM AS INTEGER NO-UNDO.
DEFINE VARIABLE        iLM2 AS INTEGER     NO-UNDO.
DEFINE VARIABLE  iAntsolgtT  AS INTEGER         NO-UNDO.
DEFINE VARIABLE  iKjopAntT   AS INTEGER         NO-UNDO.
DEFINE VARIABLE  iLagantT    AS INTEGER         NO-UNDO.
DEFINE VARIABLE  dAntRabT    AS DECIMAL         NO-UNDO.
DEFINE VARIABLE  VerdiSolgtT AS DECIMAL         NO-UNDO.
DEFINE VARIABLE  SVKT        AS DECIMAL         NO-UNDO.
DEFINE VARIABLE  dKalkFbelT  AS DECIMAL         NO-UNDO.
DEFINE VARIABLE  dKalkIbelT  AS DECIMAL         NO-UNDO.

  ASSIGN doRad = doRad - 15.
  IF doRad < 90 THEN
  DO:
    ASSIGN iTest = 17.
    RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
  END.
  ELSE
  DO:
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    RUN pdf_line IN h_PDFinc  ("Spdf", iLM, doRad + 10, pdf_PageWidth("Spdf") - iLM, doRad + 10, 0.5).
  END.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).

  RUN pdf_text_xy_dec ("Spdf","Totalt Alla:",iLM,doRad).
  RUN pdf_text_xy_dec ("Spdf","But",iLM + 100,doRad).
  RUN pdf_text_xy_dec ("Spdf","Inlev",iLM + 220,doRad).
  RUN pdf_text_xy_dec ("Spdf","Sålt",iLM + 260,doRad).
  RUN pdf_text_xy_dec ("Spdf","Lag",iLM + 300,doRad).
  RUN pdf_text_xy_dec ("Spdf","Utf %",iLM + 340,doRad).
  RUN pdf_text_xy_dec ("Spdf","Förs.Bel",iLM + 400,doRad).
  RUN pdf_text_xy_dec ("Spdf","Verk%",iLM + 460,doRad).
  RUN pdf_text_xy_dec ("Spdf","Kalk%",iLM + 505,doRad).

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", iLM + 100, doRad - 3, pdf_PageWidth("Spdf") - iLM, doRad - 3, 0.5).

  ASSIGN dy = doRad.

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
 
  FOR EACH tmpTT_Tot USE-INDEX But NO-LOCK:
      ASSIGN iAntsolgtT  = iAntsolgtT  + tmpTT_Tot.iAntsolgt
             iKjopAntT   = iKjopAntT   + tmpTT_Tot.iKjopAnt
             iLagantT    = iLagantT    + tmpTT_Tot.iLagant
             dAntRabT    = dAntRabT    + tmpTT_Tot.dAntRab
             VerdiSolgtT = VerdiSolgtT + tmpTT_Tot.VerdiSolgt
             SVKT        = SVKT        + tmpTT_Tot.SVK
             dKalkFbelT  = dKalkFbelT  + tmpTT_Tot.dKalkFbel
             dKalkIbelT  = dKalkIbelT  + tmpTT_Tot.dKalkIbel.

      IF dY < 60 THEN
      DO:
        ASSIGN iTest = 18.
        RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
        ASSIGN dY = doRad.
      END.
      ASSIGN dY = dY - 13
             cWrk = STRING(tmpTT_Tot.Butik,"z9")
             iLM2 = iLM + 115.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).             
      ASSIGN cWrk = STRING(tmpTT_Tot.iKjopAnt,"-zzzz9")
             iLM2 = iLM + 240.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      ASSIGN cWrk = STRING(tmpTT_Tot.iAntsolgt,"-zzzz9")
             iLM2 = iLM + 280.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).  
      ASSIGN cWrk = STRING(tmpTT_Tot.iLagant,"-zzzz9")
             iLM2 = iLM + 320.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      ASSIGN iUtf% = (tmpTT_Tot.iAntsolgt / tmpTT_Tot.iKjopAnt) * 100
             iLM2 = iLM + 355.
      IF tmpTT_Tot.iKjopAnt <> 0 THEN
      DO:
        ASSIGN cWrk = STRING(iUtf%,"-zzzz9").
        RUN pdf_text_xy_dec ("Spdf",cWrk + " %",iLM2 - bredd(cWrk),dY).
      END.
      ELSE
      DO:
        ASSIGN iLM2 = iLM + 345.
        RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
      END.
      ASSIGN dForsBel = tmpTT_Tot.VerdiSolgt 
             dTotSalg = dTotSalg + dForsBel
             dTotKost = dTotKost + tmpTT_Tot.SVK 
             iLM2 = iLM + 440
             cWrk = STRING(dForsBel,"-zzz,zz9.99").
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      IF tmpTT_Tot.iAntsolgt <> 0 THEN
      DO:
        ASSIGN dVerk% = ((dForsBel - tmpTT_Tot.SVK ) / (dForsBel )) * 100
               cWrk = STRING(dVerk%,"-zzz9.99")
               iLM2 = iLM + 490.
        RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      END.
      ELSE
      DO:
        ASSIGN iLM2 = iLM + 475.
        RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
      END.

      ASSIGN dKalk%   =  ((tmpTT_Tot.dKalkFbel - (tmpTT_Tot.dKalkIbel)) / tmpTT_Tot.dKalkFbel) * 100
             iLM2 = iLM + 535
             cWrk = STRING(dKalk%,"-zzz9.99").
      IF tmpTT_Tot.dKalkFbel <> 0 THEN
        RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      ELSE
      DO:
        ASSIGN iLM2 = iLM + 520.
        RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
      END.
  END.              

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", iLM + 100, dY - 3, pdf_PageWidth("Spdf") - iLM, dY - 3, 0.5).

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).

  IF dY < 60 THEN
  DO:
    ASSIGN iTest = 19.
    RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
    ASSIGN dY = doRad.
  END.
  ASSIGN dY = dY - 13
       iLM2 = iLM + 105.
  RUN pdf_text_xy_dec ("Spdf","Tot:",iLM2,dY).             
  ASSIGN cWrk = STRING(iKjopAntT,"-zzzz9")
         iLM2 = iLM + 240.
  RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
  ASSIGN cWrk = STRING(iAntsolgtT,"-zzzz9")
       iLM2 = iLM + 280.
  RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).  
  ASSIGN cWrk = STRING(iLagantT,"-zzzz9")
         iLM2 = iLM + 320.
  RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
  ASSIGN iUtf% = (iAntsolgtT / iKjopAntT) * 100
         iLM2 = iLM + 355.
  IF iKjopAntT <> 0 THEN
  DO:
    ASSIGN cWrk = STRING(iUtf%,"-zzzz9").
    RUN pdf_text_xy_dec ("Spdf",cWrk + " %",iLM2 - bredd(cWrk),dY).
  END.
  ELSE
  DO:
    ASSIGN iLM2 = iLM + 345.
    RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
  END.
  ASSIGN dForsBel = VerdiSolgtT
         iLM2 = iLM + 440
         cWrk = STRING(dForsBel,"-zzz,zzz,zz9.99").
  RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
  IF iAntsolgtT <> 0 THEN
  DO:
    ASSIGN dVerk% = ((dForsBel - SVKT ) / (dForsBel )) * 100
           cWrk = STRING(dVerk%,"-zzz9.99")
           iLM2 = iLM + 490.
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
  END.
  ELSE
  DO:
    ASSIGN iLM2 = iLM + 475.
    RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
  END.
  ASSIGN dKalk%   =  ((dKalkFbelT - (dKalkIbelT)) / dKalkFbelT) * 100
         iLM2 = iLM + 535
         cWrk = STRING(dKalk%,"-zzz9.99").
  IF dKalkFbelT <> 0 THEN
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
  ELSE
  DO:
    ASSIGN iLM2 = iLM + 520.
    RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFPrintVg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFPrintVg Procedure 
PROCEDURE PDFPrintVg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER doRad AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER iLM   AS INTEGER         NO-UNDO.
DEFINE VARIABLE  iLM2        AS INTEGER         NO-UNDO.
DEFINE VARIABLE  iLM3        AS INTEGER         NO-UNDO.
DEFINE VARIABLE  iAntsolgtT  AS INTEGER         NO-UNDO.
DEFINE VARIABLE  iKjopAntT   AS INTEGER         NO-UNDO.
DEFINE VARIABLE  iLagantT    AS INTEGER         NO-UNDO.
DEFINE VARIABLE  dAntRabT    AS DECIMAL         NO-UNDO.
DEFINE VARIABLE  VerdiSolgtT AS DECIMAL         NO-UNDO.
DEFINE VARIABLE  SVKT        AS DECIMAL         NO-UNDO.
DEFINE VARIABLE  dKalkFbelT  AS DECIMAL         NO-UNDO.
DEFINE VARIABLE  dKalkIbelT  AS DECIMAL         NO-UNDO.
DEFINE VARIABLE  iind2       AS INTEGER         NO-UNDO.
DEFINE VARIABLE  iind3       AS INTEGER         NO-UNDO.
DEFINE VARIABLE  iind4       AS INTEGER         NO-UNDO.
DEFINE VARIABLE  iind5       AS INTEGER         NO-UNDO.
DEFINE VARIABLE  lButFirst   AS LOGICAL         NO-UNDO.
DEFINE VARIABLE  iButSave    AS INTEGER         NO-UNDO.

  ASSIGN doRad = doRad - 15.
  IF doRad < 90 THEN
  DO:
    ASSIGN iTest = 20.
    RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
  END.

  ELSE
  DO:
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    RUN pdf_line IN h_PDFinc  ("Spdf", iLM, doRad + 15, pdf_PageWidth("Spdf") - iLM, doRad + 15, 0.5).
  END.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
  /* Här ska tot storlekar per butik in */
IF lVisStr = TRUE THEN
DO:
  ASSIGN VgFirst = TRUE
         iAntStrl = 0
         iind2 = 1
         cAntStrl[1] = ""
         cAntStrl[2] = "".
  FOR EACH tmpTT_VgTot USE-INDEX Istrl WHERE tmpTT_VgTot.Vg = VgSpar NO-LOCK:
    IF VgFirst = TRUE THEN
    DO:
/*      MESSAGE tmpTT_VgTot.Vg tmpTT_VgTot.Butik tmpTT_VgTot.Storlek SKIP
              "Sålt " tmpTT_VgTot.iAntSolgt SKIP
              "Köpt " tmpTT_VgTot.iKjopAnt SKIP
              "Lager " tmpTT_VgTot.iLagAnt
          VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
      ASSIGN VgFirst = FALSE
             cStrl2 = tmpTT_VgTot.Storlek
             iAntStrl = 1.
             IF cAntStrl[iind2] = "" THEN
               ASSIGN cAntStrl[iind2] = cStrl2.
             ELSE
               ASSIGN cAntStrl[iind2] = cAntStrl[iind2] + "," + cStrl2.
    END.
    ELSE IF cStrl2 <> tmpTT_VgTot.Storlek THEN
    DO:
/*      MESSAGE tmpTT_VgTot.Vg tmpTT_VgTot.Butik tmpTT_VgTot.Storlek SKIP
              "Sålt " tmpTT_VgTot.iAntSolgt SKIP
              "Köpt " tmpTT_VgTot.iKjopAnt SKIP
              "Lager " tmpTT_VgTot.iLagAnt
          VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
      ASSIGN cStrl2 = tmpTT_VgTot.Storlek
             iAntStrl = iAntStrl + 1.
      IF cAntStrl[iind2] = "" THEN
        ASSIGN cAntStrl[iind2] = cStrl2.
      ELSE
        ASSIGN cAntStrl[iind2] = cAntStrl[iind2] + "," + cStrl2.
      IF iAntStrl = 20 THEN
        ASSIGN iind2 = 2.
    END.
  END.
/*MESSAGE "Antal: " iAntStrl SKIP
         "1: " cAntStrl[1] SKIP
         "2: " cAntStrl[2]
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
/*MESSAGE "lNullposter " lNullposter SKIP
        "lNullager " lNullager
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
  ASSIGN iind2 = 1.

/* DEFINE TEMP-TABLE tmpTT_VgTot
   FIELD Vg        AS INTEGER
   FIELD Butik     LIKE Butiker.Butik
   FIELD Storlek   AS CHARACTER
   FIELD iAntSolgt AS INTEGER
   FIELD iKjopAnt  AS INTEGER
   FIELD iLagAnt   AS INTEGER
   FIELD cVisEntry AS CHARACTER
   INDEX VgButik IS PRIMARY UNIQUE Vg Butik.*/

  IF cAntStrl[2] <> "" THEN
    ASSIGN iind5 = 2.
  ELSE
    ASSIGN iind5 = 1.
    DO iind2 = 1 TO iind5:
    ASSIGN lButFirst = TRUE.
    ASSIGN iAntsolgtT  = 0
           iKjopAntT   = 0
           iLagantT    = 0.
    FIND FIRST tmpTT_VgTot WHERE tmpTT_VgTot.Vg = 0 NO-ERROR.

    ASSIGN iLM2 = 70.
    RUN pdf_text_xy_dec ("Spdf","VGR " + STRING(VgSpar),iLM,doRad).
    DO iind4 = 1 TO NUM-ENTRIES(cAntStrl[iind2],","):
       ASSIGN iLM2 = iLM2 + 22.
       RUN pdf_text_xy_dec ("Spdf",ENTRY(iind4,cAntStrl[iind2],","),iLM2,doRad).
    END.
    ASSIGN doRad = doRad - 13.

    FOR EACH tmpTT_VgTot WHERE tmpTT_VgTot.Vg = VgSpar NO-LOCK:
      IF NOT CAN-DO(cAntStrl[iind2],tmpTT_VgTot.Storlek) THEN
        NEXT.
      ASSIGN iind3 = LOOKUP(tmpTT_VgTot.Storlek,cAntStrl[iind2]).
      IF iind3 = 0 THEN
        NEXT.
      IF lNullager = FALSE AND tmpTT_VgTot.iLagAnt = 0 THEN
        NEXT.
      IF lButFirst = TRUE THEN
      DO:
        ASSIGN lButFirst = FALSE
               iButSave = tmpTT_VgTot.Butik.
        RUN pdf_text_xy_dec ("Spdf","But: " + STRING(iButSave),iLM,doRad).
        RUN pdf_text_xy_dec ("Spdf","Sålt: ",iLM + 35,doRad).
        RUN pdf_text_xy_dec ("Spdf","Köpt: ",iLM + 35,doRad - 13).
        RUN pdf_text_xy_dec ("Spdf","Lag.: ",iLM + 35,doRad - 26).
      END.
      ELSE IF iButSave <> tmpTT_VgTot.Butik THEN
      DO:
        RUN pdf_text_xy_dec ("Spdf","= " + STRING(iAntsolgtT) ,532,doRad).
        RUN pdf_text_xy_dec ("Spdf","= " + STRING(iKjopAntT),532,doRad - 13).
        RUN pdf_text_xy_dec ("Spdf","= " + STRING(iLagantT),532,doRad - 26).
        ASSIGN doRad = doRad - 39.
        IF doRad < 90 THEN
        DO:
          ASSIGN iTest = 21.
          RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
          ASSIGN iLM2 = 72.
          RUN pdf_text_xy_dec ("Spdf","VGR " + STRING(VgSpar),iLM,doRad).
          DO iind4 = 1 TO NUM-ENTRIES(cAntStrl[iind2],","):
            ASSIGN iLM2 = iLM2 + 22.
            RUN pdf_text_xy_dec ("Spdf",ENTRY(iind4,cAntStrl[iind2],","),iLM2,doRad).
          END.
          ASSIGN doRad = doRad - 13.
        END.
        ASSIGN iButSave = tmpTT_VgTot.Butik.
        IF iButSave = 999 THEN
        DO:
          RUN pdf_text_xy_dec ("Spdf","Totalt:",iLM,doRad).
        END.
        ELSE
        DO:
          RUN pdf_text_xy_dec ("Spdf","But: " + STRING(iButSave),iLM,doRad).
        END.
        RUN pdf_text_xy_dec ("Spdf","Sålt: ",iLM + 35,doRad).
        RUN pdf_text_xy_dec ("Spdf","Köpt: ",iLM + 35,doRad - 13).
        RUN pdf_text_xy_dec ("Spdf","Lag.: ",iLM + 35,doRad - 26).
        ASSIGN iAntsolgtT  = 0
               iKjopAntT   = 0
               iLagantT    = 0.
      END.
      ASSIGN iLM3 = 72 + (iind3 * 22).
/*      IF iLM3 <= 0 THEN
          MESSAGE "iLM3: " iLM3
              VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
      ASSIGN cWrk = STRING(tmpTT_VgTot.iAntSolgt).
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM3,doRad).               
      ASSIGN cWrk = STRING(tmpTT_VgTot.iKjopAnt).
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM3,doRad - 13).               
      ASSIGN cWrk = STRING(tmpTT_VgTot.iLagAnt).
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM3,doRad - 26).               
      ASSIGN iAntsolgtT = iAntsolgtT + iAntsolgt
             iKjopAntT = iKjopAntT + iKjopAnt
             iLagantT = iLagantT + iLagant.
    END.
    IF iAntsolgtT <> 0 OR iKjopAntT <> 0 OR iLagantT <> 0 THEN
    DO:
      RUN pdf_text_xy_dec ("Spdf","= " + STRING(iAntsolgtT) ,532,doRad).
      RUN pdf_text_xy_dec ("Spdf","= " + STRING(iKjopAntT),532,doRad - 13).
      RUN pdf_text_xy_dec ("Spdf","= " + STRING(iLagantT),532,doRad - 26).
    END.
    doRad = doRad - 39.
    IF iind2 = 1 THEN
    DO:
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
      RUN pdf_line IN h_PDFinc  ("Spdf", iLM, doRad + 5, pdf_PageWidth("Spdf") - iLM, doRad + 5, 0.5).
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
      ASSIGN doRad = doRad - 13.
    END.
  END.
  DO:
    ASSIGN iTest = 22.
    RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
  END.
END.


  /* Hit */
  RUN pdf_text_xy_dec ("Spdf","Totalt för grupp " + STRING(VgSpar) + ":",iLM,doRad).
  RUN pdf_text_xy_dec ("Spdf","But",iLM + 100,doRad).
  RUN pdf_text_xy_dec ("Spdf","Inlev",iLM + 220,doRad).
  RUN pdf_text_xy_dec ("Spdf","Sålt",iLM + 260,doRad).
  RUN pdf_text_xy_dec ("Spdf","Lag",iLM + 300,doRad).
  RUN pdf_text_xy_dec ("Spdf","Utf %",iLM + 340,doRad).
  RUN pdf_text_xy_dec ("Spdf","Förs.Bel",iLM + 400,doRad).
  RUN pdf_text_xy_dec ("Spdf","Verk%",iLM + 460,doRad).
  RUN pdf_text_xy_dec ("Spdf","Kalk%",iLM + 505,doRad).

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", iLM + 100, doRad - 3, pdf_PageWidth("Spdf") - iLM, doRad - 3, 0.5).

  ASSIGN dy = doRad.

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).

  ASSIGN iAntsolgtT  = 0
         iKjopAntT   = 0
         iLagantT    = 0
         dAntRabT    = 0
         VerdiSolgtT = 0
         SVKT        = 0
         dKalkFbelT  = 0
         dKalkIbelT  = 0.

 
  FOR EACH tmpTT_Kjop WHERE tmpTT_Kjop.Vg = VgSpar USE-INDEX VgBut NO-LOCK:
      ASSIGN iAntsolgtT  = iAntsolgtT  + tmpTT_Kjop.iAntsolgt
             iKjopAntT   = iKjopAntT   + tmpTT_Kjop.iKjopAnt
             iLagantT    = iLagantT    + tmpTT_Kjop.iLagant
             dAntRabT    = dAntRabT    + tmpTT_Kjop.dAntRab
             VerdiSolgtT = VerdiSolgtT + tmpTT_Kjop.VerdiSolgt
             SVKT        = SVKT        + tmpTT_Kjop.SVK
             dKalkFbelT  = dKalkFbelT  + tmpTT_Kjop.dKalkFbel
             dKalkIbelT  = dKalkIbelT  + tmpTT_Kjop.dKalkIbel.

      IF dY < 60 THEN
      DO:
        ASSIGN iTest = 23.
        RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
        ASSIGN dY = doRad.
      END.
      ASSIGN dY = dY - 13
             cWrk = STRING(tmpTT_Kjop.Butik,"z9")
             iLM2 = iLM + 115.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).             
      ASSIGN cWrk = STRING(tmpTT_Kjop.iKjopAnt,"-zzzz9")
             iLM2 = iLM + 240.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      ASSIGN cWrk = STRING(tmpTT_Kjop.iAntsolgt,"-zzzz9")
             iLM2 = iLM + 280.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).  
      ASSIGN cWrk = STRING(tmpTT_Kjop.iLagant,"-zzzz9")
             iLM2 = iLM + 320.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      ASSIGN iUtf% = (tmpTT_Kjop.iAntsolgt / tmpTT_Kjop.iKjopAnt) * 100
             iLM2 = iLM + 355.
      IF tmpTT_Kjop.iKjopAnt <> 0 THEN
      DO:
        ASSIGN cWrk = STRING(iUtf%,"-zzzz9").
        RUN pdf_text_xy_dec ("Spdf",cWrk + " %",iLM2 - bredd(cWrk),dY).
      END.
      ELSE
      DO:
        ASSIGN iLM2 = iLM + 345.
        RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
      END.
      ASSIGN dForsBel = tmpTT_Kjop.VerdiSolgt 
             dTotSalg = dTotSalg + dForsBel
             dTotKost = dTotKost + tmpTT_Kjop.SVK 
             iLM2 = iLM + 440
             cWrk = STRING(dForsBel,"-zzz,zz9.99").
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      IF tmpTT_Kjop.iAntsolgt <> 0 THEN
      DO:
        ASSIGN dVerk% = ((dForsBel - tmpTT_Kjop.SVK ) / (dForsBel )) * 100
               cWrk = STRING(dVerk%,"-zzz9.99")
               iLM2 = iLM + 490.
        RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      END.
      ELSE
      DO:
        ASSIGN iLM2 = iLM + 475.
        RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
      END.
      ASSIGN dKalk%   =  ((tmpTT_Kjop.dKalkFbel - (tmpTT_Kjop.dKalkIbel)) / tmpTT_Kjop.dKalkFbel) * 100
             iLM2 = iLM + 535
             cWrk = STRING(dKalk%,"-zzz9.99").
      IF tmpTT_Kjop.dKalkFbel <> 0 THEN
        RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      ELSE
      DO:
        ASSIGN iLM2 = iLM + 520.
        RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
      END.      
  END.

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", iLM + 100, dY - 3, pdf_PageWidth("Spdf") - iLM, dY - 3, 0.5).

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).

  IF dY < 60 THEN
  DO:
    ASSIGN iTest = 24.
    RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
    ASSIGN dY = doRad.
  END.
  ASSIGN dY = dY - 13
       iLM2 = iLM + 105.
  RUN pdf_text_xy_dec ("Spdf","Tot:",iLM2,dY).             
  ASSIGN cWrk = STRING(iKjopAntT,"-zzzz9")
         iLM2 = iLM + 240.
  RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
  ASSIGN cWrk = STRING(iAntsolgtT,"-zzzz9")
       iLM2 = iLM + 280.
  RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).  
  ASSIGN cWrk = STRING(iLagantT,"-zzzz9")
         iLM2 = iLM + 320.
  RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
  ASSIGN iUtf% = (iAntsolgtT / iKjopAntT) * 100
         iLM2 = iLM + 355.
  IF iKjopAntT <> 0 THEN
  DO:
    ASSIGN cWrk = STRING(iUtf%,"-zzzz9").
    RUN pdf_text_xy_dec ("Spdf",cWrk + " %",iLM2 - bredd(cWrk),dY).
  END.
  ELSE
  DO:
    ASSIGN iLM2 = iLM + 345.
    RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
  END.
  ASSIGN dForsBel = VerdiSolgtT
         iLM2 = iLM + 440
         cWrk = STRING(dForsBel,"-zzz,zzz,zz9.99").
  RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
  IF iAntsolgtT <> 0 THEN
  DO:
    ASSIGN dVerk% = ((dForsBel - SVKT ) / (dForsBel )) * 100
           cWrk = STRING(dVerk%,"-zzz9.99")
           iLM2 = iLM + 490.
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
  END.
  ELSE
  DO:
    ASSIGN iLM2 = iLM + 475.
    RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
  END.
  ASSIGN dKalk%   =  ((dKalkFbelT - (dKalkIbelT)) / dKalkFbelT) * 100
         iLM2 = iLM + 535
         cWrk = STRING(dKalk%,"-zzz9.99").
  IF dKalkFbelT <> 0 THEN
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
  ELSE
  DO:
    ASSIGN iLM2 = iLM + 520.
    RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
  END.

  ASSIGN doRad = dY.

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

        ASSIGN lSumAntall = lSumantall + Translogg.Antall.

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
                               ELSE lSumRabVerdi.

        END.
    END.

    RETURN string(lSumantall) + '|' + 
           STRING(lSumVerdi)  + '|' +
           STRING(lSumRabAnt) + '|' +
           STRING(lSumRabVerdi).  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBildeFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBildeFil Procedure 
FUNCTION getBildeFil RETURNS CHARACTER
    ( INPUT ipBildNr AS INTEGER ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cBildeFil AS CHARACTER  NO-UNDO.
    FIND BildeRegister NO-LOCK WHERE
      BildeRegister.BildNr = ipBildNr NO-ERROR.
    IF AVAIL BildeRegister AND TRIM(BildeRegister.FilNavn) <> "" THEN DO:
      IF VALID-HANDLE(wLibHandle) THEN
        RUN HentBildePeker IN wLibHandle (INPUT ipBildNr, 1, BildeRegister.FilNavn, OUTPUT cBildeFil).
    END.
    /* cBlanktBilde */
    RETURN cBildeFil.   /* Function return value. */

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

