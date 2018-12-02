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
DEFINE INPUT  PARAMETER hTTArt      AS HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER cQRY        AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cArtikkelNr AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cSourceButiker AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButNamn        AS CHARACTER  FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE cColLabelString AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cColLabelStrTOT AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cColLabelVgTOT AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrintString    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrintStringTOT AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrintStringVg    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrintStringVgTOT AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTitle          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKundenavn      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPolygon        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCL             AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCLProfilnr     AS INTEGER    NO-UNDO.
DEFINE VARIABLE cButikkListe    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lNullposter     AS LOGICAL    NO-UNDO.
DEFINE VARIABLE iSortering      AS INTEGER    NO-UNDO.
DEFINE VARIABLE cBildeFil       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iSideBrytRad AS INTEGER  NO-UNDO.
DEFINE VARIABLE iTotButik AS INTEGER INIT 9999999   NO-UNDO.
DEFINE VARIABLE cTotalRub AS CHARACTER EXTENT 10 NO-UNDO.
/*    ["","Utpris","Inkjøp","Sålt","Lager","Utf%","Fsgbeløp",""] NO-UNDO.*/
DEFINE VARIABLE iTotCols  AS INTEGER  EXTENT 8 INITIAL
    [18,23,29,35,41,47,53,64] NO-UNDO.
DEFINE VARIABLE iTotRight AS INTEGER EXTENT 8 INITIAL
    [0,1,1,1,1,1,1,0] NO-UNDO.
DEFINE VARIABLE cRub            AS CHARACTER EXTENT 15 NO-UNDO.
/*    ["VG/Lopnr","Beskrivelse","Farge","Lev","Lev.art.nr","Ses","Varekost","Pris","Tilbud","Solgt",
    "Solgt verdi","Kjøpt","Kjøpt verdi","Lager","Lagerverdi"] NO-UNDO.*/
DEFINE VARIABLE iCols  AS INTEGER  EXTENT 15 INITIAL
    [6,13,25,45,64,74,84,94,104,120,121,122,123,124,125] NO-UNDO.
DEFINE VARIABLE iRight AS INTEGER EXTENT 15 INITIAL
    [0,0,0,0,0,1,1,1,1,1,1,1,1,1,1] NO-UNDO.

DEFINE VARIABLE cVgTotalRub AS CHARACTER EXTENT 8 NO-UNDO.
/*    ["But","Inkjøp","Sålt","Lager","Utf%","Fsgbeløp",""] NO-UNDO.*/
DEFINE VARIABLE iVgTotCols  AS INTEGER  EXTENT 7 INITIAL
    [34,40,46,52,58,64,70] NO-UNDO.
DEFINE VARIABLE iVgTotRight AS INTEGER EXTENT 7 INITIAL
    [1,1,1,1,1,1,0] NO-UNDO.

DEFINE VARIABLE dColPos         AS DECIMAL EXTENT 15         NO-UNDO.
DEFINE VARIABLE dColPos2        AS DECIMAL EXTENT 15         NO-UNDO.
DEFINE VARIABLE dColPos3        AS DECIMAL EXTENT 15         NO-UNDO.
DEFINE VARIABLE dColPos4        AS DECIMAL EXTENT 15         NO-UNDO.
DEFINE VARIABLE dColPos5        AS DECIMAL EXTENT 15         NO-UNDO.
DEFINE VARIABLE dColPos6        AS DECIMAL EXTENT 15         NO-UNDO.
DEFINE VARIABLE cSprak          AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cToday          AS CHARACTER FORMAT "X(10)"  NO-UNDO.
DEFINE VARIABLE cSidTxt         AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE pcRappFil       AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE dY              AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE iCount          AS INTEGER                   NO-UNDO.
DEFINE VARIABLE iCount2         AS INTEGER                   NO-UNDO.
DEFINE VARIABLE cUL             AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE iLen            AS INTEGER                   NO-UNDO.
DEFINE VARIABLE cTmpData        AS CHARACTER EXTENT 15       NO-UNDO.
DEFINE VARIABLE cBildNamn       AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE iImageHeight    AS INTEGER                   NO-UNDO.
DEFINE VARIABLE iImageWidth     AS INTEGER                   NO-UNDO.
DEFINE VARIABLE cRub2           AS CHARACTER EXTENT 3        NO-UNDO.


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
    FIELD SVK         LIKE Lager.SVK
    FIELD PrisOrdi    AS DECI DECIMALS 2
    FIELD PrisTilb    AS DECI DECIMALS 2
    FIELD InPris      AS DECI DECIMALS 2
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
    FIELD iLagant    LIKE Lager.Lagant 
    INDEX ArtButTyp  IS PRIMARY UNIQUE ArtikkelNr Butik.

DEFINE TEMP-TABLE TT_VG_KSL
    FIELD Vg         LIKE ArtBas.Vg
    FIELD Butik      LIKE Butiker.Butik
    FIELD iAntsolgt  AS DECIMAL FORMAT "->>>,>>9.999"
    FIELD iKjopAnt   AS DECIMAL FORMAT "->>>,>>9.999" 
    FIELD iLagant    AS DECIMAL FORMAT "->>>,>>9.999"
    FIELD dTotSalgBelop AS DECIMAL
    FIELD SVK         LIKE Lager.SVK
    INDEX VgBut  IS PRIMARY UNIQUE Vg Butik.

DEFINE TEMP-TABLE TT_image NO-UNDO
    FIELD image_name    AS CHARACTER
INDEX obj_name AS PRIMARY
      image_name.

DEFINE BUFFER BUF_VG_KSL FOR TT_VG_KSL.

DEFINE BUFFER TT_KSL FOR TT_KjopSalgLager.

DEFINE VARIABLE cPerTxt   AS CHAR FORMAT "x(70)" NO-UNDO.

DEFINE FRAME PageHeader
   HEADER
      "<ALIGN=BASE><FArial><R3><P12><B><C6>" STRING(TODAY)
      "<R4><P12><C74><P9> Side: " PAGE-NUMBER FORMAT ">>" SKIP
      "<R4><C6><B>Butikk:" cButNamn "</B>" SKIP
      "<R5><C6><FROM><R5><C80><LINE>" SKIP
      WITH PAGE-TOP STREAM-IO WIDTH 255.

/* DEFINE FRAME PageHeader                                         */
/*    HEADER                                                       */
/*       "<ALIGN=BASE><FArial><R3><P12><B><C6>" STRING(TODAY)      */
/*       "<R4><P12><C108><P9> Side: " PAGE-NUMBER FORMAT ">>" SKIP */
/*       "<R4><C6><B>Butikk:" cButNamn "<C70>" cPerTxt "</B>" SKIP */
/*       "<R5><C6><FROM><R5><C113><LINE>" SKIP                     */
/*       WITH PAGE-TOP STREAM-IO WIDTH 255.                        */

{ pdf_inc.i "THIS-PROCEDURE"}

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

&IF DEFINED(EXCLUDE-getKjopSolgtLagerLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKjopSolgtLagerLinje Procedure 
FUNCTION getKjopSolgtLagerLinje RETURNS CHARACTER
  ( INPUT iRad AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFgetDataLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PDFgetDataLinje Procedure 
FUNCTION PDFgetDataLinje RETURNS CHARACTER
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
     ASSIGN cTitle = "Säsongsanalys"
           cRub[1] = "VG/Löpnr"
           cRub[2] = "Beskrivning"
           cRub[3] = "Färg"
           cRub[4] = "Lev"
           cRub[5] = "Lev.art.nr"
           cRub[6] = "Säs"
           cRub[7] = "Varukost"
           cRub[8] = "Pris"
           cRub[9] = "ReaPris"
           cRub[10] = "Sålt"
           cRub[11] = "Sålg värde"
           cRub[12] = "Kjöpt"
           cRub[13] = "Kjöpt värde"
           cRub[14] = "Lager"
           cRub[15] = "Lagervärde".
   ELSE
     ASSIGN cTitle = "Sesonganalyse"
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
     IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
       ASSIGN cRub2[1] = "Sålt:"
              cRub2[2] = "Köpt:"
              cRub2[3] = "Lager:".
     ELSE
       ASSIGN cRub2[1] = "Solgt:"
              cRub2[2] = "Kjøpt:"
              cRub2[3] = "Lager:".
     IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
       ASSIGN cTotalRub[1] = ""
              cTotalRub[2] = "Inpris"
              cTotalRub[3] = "Utpris"
              cTotalRub[4] = "Inköp"
              cTotalRub[5] = "Sålt"
              cTotalRub[6] = "Lager"
              cTotalRub[7] = "Utf%"
              cTotalRub[8] = "TB"
              cTotalRub[9] = "TG%"
              cTotalRub[10] = "Fsgbelopp".
     ELSE
       ASSIGN cTotalRub[1] = ""
              cTotalRub[3] = "Innpris"
              cTotalRub[3] = "Utpris"
              cTotalRub[4] = "Inkjøp"
              cTotalRub[5] = "Sålt"
              cTotalRub[6] = "Lager"
              cTotalRub[7] = "Utf%"
              cTotalRub[8] = "DB"
              cTotalRub[9] = "DB%"
              cTotalRub[10] = "Fsgbeløp".

     IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
       ASSIGN cVgTotalRub[1] = "But"
              cVgTotalRub[2] = "Inköp"
              cVgTotalRub[3] = "Sålt"
              cVgTotalRub[4] = "Lager"
              cVgTotalRub[5] = "Utf%"
              cVgTotalRub[6] = "TB"
              cVgTotalRub[7] = "TG%"
              cVgTotalRub[8] = "Fsgbelopp".
     ELSE
       ASSIGN cVgTotalRub[1] = "But"
              cVgTotalRub[2] = "Inkjøp"
              cVgTotalRub[3] = "Sålt"
              cVgTotalRub[4] = "Lager"
              cVgTotalRub[5] = "Utf%"
              cVgTotalRub[6] = "DB"
              cVgTotalRub[7] = "DB%"
              cVgTotalRub[8] = "Fsgbeløp".


/* Getbutiker frågar om det finns begränsningar i vilka butiker vi får se */
/* För att möjligöra Jbox-selecten har vi ett window
   RUN d-AvgrensLagerliste.w (OUTPUT lNullposter,OUTPUT iSortering,OUTPUT cButikkliste,INPUT cSourceButiker).
 */
/* RUN d-AvgrensLagerliste.w (OUTPUT lNullposter,   */
/*                            OUTPUT iSortering,    */
/*                            OUTPUT cButikkliste,  */
/*                            INPUT cSourceButiker, */
/*                            OUTPUT dFraDato,      */
/*                            OUTPUT dTilDato,      */
/*                            OUTPUT lPeriode).     */
cButikkListe = cSourceButiker.
IF RETURN-VALUE = "AVBRYT" THEN
    RETURN.

IF dFraDato = ? OR dTilDato = ? THEN
    lPeriode = FALSE.

FIND Butiker WHERE Butiker.Butik = iCL NO-LOCK NO-ERROR.
ASSIGN iCLProfilNr = Butiker.ProfilNr.
{sww.i}
RUN ByggTmpLager.

RUN ByggSummaLager.
/*RUN SkrivLagerlisteX.*/
RUN PDFSkrivLagerliste.
{swn.i}
/* 
 DYNAMIC-FUNCTION('closeQuery':U).
 bufTTh:BUFFER-RELEASE().
 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggSummaLager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggSummaLager Procedure 
PROCEDURE ByggSummaLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iBut AS INTEGER     NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cNyButikkListe AS CHARACTER   NO-UNDO.
    cNyButikkListe = cButikkListe + "," + string(iTotButik).
    DO ii = 1 TO NUM-ENTRIES(cNyButikkListe):
        iBut = INT(ENTRY(ii,cNyButikkListe)).
        IF CAN-FIND(FIRST TT_VG_KSL WHERE TT_VG_KSL.butik = iBut) THEN DO:
            CREATE BUF_VG_KSL.
            ASSIGN BUF_VG_KSL.butik = iBut
                   BUF_VG_KSL.vg    = 0.
            FOR EACH TT_VG_KSL WHERE TT_VG_KSL.butik = iBut:
                ASSIGN BUF_VG_KSL.iAntsolgt     = BUF_VG_KSL.iAntsolgt     + TT_VG_KSL.iAntsolgt    
                       BUF_VG_KSL.iKjopAnt      = BUF_VG_KSL.iKjopAnt      + TT_VG_KSL.iKjopAnt     
                       BUF_VG_KSL.iLagant       = BUF_VG_KSL.iLagant       + TT_VG_KSL.iLagant      
                       BUF_VG_KSL.dTotSalgBelop = BUF_VG_KSL.dTotSalgBelop + TT_VG_KSL.dTotSalgBelop
                       BUF_VG_KSL.SVK           = BUF_VG_KSL.SVK           + TT_VG_KSL.SVK.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpLager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpLager Procedure 
PROCEDURE ByggTmpLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hTTArtBuf AS HANDLE  NO-UNDO.
  DEFINE        VARIABLE  hBufferField AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cStorlArray AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iEntry AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLagAnt    LIKE Lager.LagAnt NO-UNDO.
  DEFINE VARIABLE dVVarekost AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cVisEntry AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lSumSolgt AS DEC NO-UNDO.
  DEFINE VARIABLE lSumKjopt AS DEC NO-UNDO.
  DEFINE VARIABLE cRetur    AS CHAR NO-UNDO.

/*   hTTArtBuf = hTTArt:DEFAULT-BUFFER-HANDLE. */
  hTTArtBuf = hTTArt.
  CREATE QUERY  hQuery.
  hQuery:SET-BUFFERS(hTTArtBuf).
  hQuery:QUERY-PREPARE("FOR EACH TT_Artiklar").
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
                     TT_ArtLager.LevArtNr   = ArtBas.LevKod
                     TT_Artlager.PrisOrdi   = ArtPris.Pris[1]
                     TT_ArtLager.InPris     = ArtPris.VareKost[1]
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
                          TT_ArtLager.VerdiRabatt = TT_ArtLager.VerdiRabatt + Lager.VerdiRabatt
                          TT_ArtLager.SVK         = TT_ArtLager.SVK         + Lager.SVK.
                      
                      FIND TT_VG_KSL WHERE TT_VG_KSL.Vg = Artbas.Vg AND TT_VG_KSL.butik = Lager.butik NO-ERROR.
                      IF NOT AVAIL TT_VG_KSL THEN DO:
                          CREATE TT_VG_KSL.
                          ASSIGN TT_VG_KSL.Vg    = Artbas.vg
                                 TT_VG_KSL.butik = lager.butik.
                      END.
                      ASSIGN TT_VG_KSL.iAntsolgt = TT_VG_KSL.iAntsolgt + Lager.AntSolgt
                             TT_VG_KSL.iKjopAnt  = TT_VG_KSL.iKjopAnt  + Lager.KjopAnt
                             TT_VG_KSL.iLagant   = TT_VG_KSL.iLagant   + Lager.lagant
                             TT_VG_KSL.dTotSalgBelop = TT_VG_KSL.dTotSalgBelop + Lager.VerdiSolgt
                             TT_VG_KSL.SVK = TT_VG_KSL.SVK + Lager.SVK.
                      /* TOTALT ALLA BUTIKER PÅ VG */
                      FIND BUF_VG_KSL WHERE BUF_VG_KSL.Vg = Artbas.Vg AND BUF_VG_KSL.butik = iTotButik NO-ERROR.
                      IF NOT AVAIL BUF_VG_KSL THEN DO:
                          CREATE BUF_VG_KSL.
                          ASSIGN BUF_VG_KSL.Vg    = Artbas.vg
                                 BUF_VG_KSL.butik = iTotButik.
                      END.
                      ASSIGN BUF_VG_KSL.iAntsolgt = BUF_VG_KSL.iAntsolgt + Lager.AntSolgt
                             BUF_VG_KSL.iKjopAnt  = BUF_VG_KSL.iKjopAnt  + Lager.KjopAnt
                             BUF_VG_KSL.iLagant   = BUF_VG_KSL.iLagant   + Lager.lagant
                             BUF_VG_KSL.dTotSalgBelop = BUF_VG_KSL.dTotSalgBelop + Lager.VerdiSolgt
                             BUF_VG_KSL.SVK = BUF_VG_KSL.SVK + Lager.SVK.

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
                                              TT_KjopSalgLager.Butik      = 0 NO-ERROR. /* ArtLag.Butik NO-ERROR. */
                  /* Oppretter tmpRecord og initierer feltene med kommalister. */
                  IF NOT AVAIL TT_KjopSalgLager THEN DO:
                      CREATE TT_KjopSalgLager.
                      ASSIGN TT_KjopSalgLager.ArtikkelNr = ArtLag.ArtikkelNr
                             TT_KjopSalgLager.Butik      = 0 /* ArtLag.Butik */
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
                             ENTRY(iEntry,TT_KjopSalgLager.Antsolgt) = STRING(DEC(ENTRY(iEntry,TT_KjopSalgLager.Antsolgt)) + ArtLag.AntSolgt) 
                             ENTRY(iEntry,TT_KjopSalgLager.Kjopant) = STRING(DEC(ENTRY(iEntry,TT_KjopSalgLager.Kjopant)) + ArtLag.KjopAnt)
                             ENTRY(iEntry,TT_KjopSalgLager.Lagant) = STRING(DEC(ENTRY(iEntry,TT_KjopSalgLager.Lagant)) + ArtLag.lagant)
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
                             ENTRY(iEntry,TT_KjopSalgLager.Antsolgt) = STRING(DEC(ENTRY(iEntry,TT_KjopSalgLager.Antsolgt)) + lSumSolgt) 
                             ENTRY(iEntry,TT_KjopSalgLager.Kjopant) = STRING(DEC(ENTRY(iEntry,TT_KjopSalgLager.Kjopant)) + lSumKjopt)
                             ENTRY(iEntry,TT_KjopSalgLager.Lagant) = STRING(DEC(ENTRY(iEntry,TT_KjopSalgLager.Lagant)) + ArtLag.lagant)
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

    DO iCount = 1 TO EXTENT(iTotCols):
        ASSIGN cRight = IF iTotRight[iCount] = 0 THEN "" ELSE 
                 "<RIGHT=C+" + (IF iCount = EXTENT(iTotCols) THEN STRING(114 - iTotCols[iCount] - 1) 
                                ELSE STRING(iTotCols[iCount + 1] - iTotCols[iCount] - 1)) + ">".
        ASSIGN cPrintStringTOT = cPrintStringTOT + "<C" + STRING(iTotCols[iCount]) + ">" + cRight + "&" + STRING(iCount,"99").
    END.
    ASSIGN cColLabelStrTOT = "<U>" + cPrintStringTOT + "</U>".
    DO iCount = 1 TO EXTENT(iTotCols):
        ASSIGN cColLabelStrTOT = REPLACE(cColLabelStrTOT,"&" + STRING(iCount,"99"),cRub[iCount]).
    END.
    DO iCount = 1 TO EXTENT(iVgTotCols):
        ASSIGN cRight = IF iVgTotRight[iCount] = 0 THEN "" ELSE 
                 "<RIGHT=C+" + (IF iCount = EXTENT(iVgTotCols) THEN STRING(80 - iVgTotCols[iCount] - 1) 
                                ELSE STRING(iVgTotCols[iCount + 1] - iVgTotCols[iCount] - 1)) + ">".
        ASSIGN cPrintStringVgTOT = cPrintStringVgTOT + "<C" + STRING(iVgTotCols[iCount]) + ">" + cRight + "&" + STRING(iCount,"99").
    END.
    ASSIGN cColLabelVgTOT = "<U>" + cPrintStringVgTOT + "</U>".
    DO iCount = 1 TO EXTENT(iVgTotCols):
        ASSIGN cColLabelVgTOT = REPLACE(cColLabelVgTOT,"&" + STRING(iCount,"99"),cRub[iCount]).
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
  RUN pdf_text_xy_dec ("Spdf",cPolygon,pdf_Pagewidth ("Spdf") - pdf_LeftMargin ("Spdf") - 300,pdf_BottomMargin ("Spdf") - 14).
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
  RUN pdf_text_xy_dec ("Spdf",cTitle,dColPos[2] + 150,pdf_PageHeight("Spdf") - 40).
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

/*  DO iCount = 1 TO 15:
    RUN pdf_text_xy_dec ("Spdf",cRub[iCount],dColPos[iCount],dY).
    ASSIGN cUL = "".
    DO iLen = 1 TO LENGTH(cRub[iCount]):
      ASSIGN cUL = cUL + "_".
    END.
    RUN pdf_text_xy_dec ("Spdf",cUL,dColPos[iCount],dY).    
  END.*/

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
         dColPos2[3] = 270                      
         dColPos2[4] = 350                      
         dColPos2[5] = 500
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
      
  ASSIGN dColPos3[1] = 150
         dColPos3[2] = 180
         dColPos3[3] = 220                      
         dColPos3[4] = 270                      
         dColPos3[5] = 320
         dColPos3[6] = 350
         dColPos3[7] = 380
         dColPos3[8] = 435
         dColPos3[9] = 470
         dColPos3[10] = 500.
  ASSIGN dColPos4[1] = 130
         dColPos4[2] = 175
         dColPos4[3] = 215                      
         dColPos4[4] = 282                      
         dColPos4[5] = 327
         dColPos4[6] = 362
         dColPos4[7] = 380
         dColPos4[8] = 405
         dColPos4[9] = 465
         dColPos4[10] = 490.
  ASSIGN dColPos5[1] = 150
         dColPos5[2] = 200
         dColPos5[3] = 250
         dColPos5[4] = 295
         dColPos5[5] = 340
         dColPos5[6] = 420
         dColPos5[7] = 450
         dColPos5[8] = 500.
  ASSIGN dColPos6[1] = 165
         dColPos6[2] = 225
         dColPos6[3] = 270
         dColPos6[4] = 320
         dColPos6[5] = 362
         dColPos6[6] = 433
         dColPos6[7] = 472
         dColPos6[8] = 550.
END PROCEDURE. /* PDSPositioner */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFSkrivLagerliste) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFSkrivLagerliste Procedure 
PROCEDURE PDFSkrivLagerliste :
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
  DEFINE VARIABLE iRad         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iVg          AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lFlag        AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE iFirstRad    AS INTEGER INIT 6   NO-UNDO.
  DEFINE VARIABLE cTitleString AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cBoldStart   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cBoldEnd     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDatFrom  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDatTom   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cWrk      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE doRad2    AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dY2       AS DECIMAL     NO-UNDO.
  cTitlestring = cTitle.
/*  RUN InitPrintString.*/
  ASSIGN cQRY1 = "FOR EACH TT_ArtLager USE-INDEX VgLopnr"
         cQRY2 = "FOR EACH TT_ArtLager USE-INDEX LevNr".

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

  hQuery:SET-BUFFERS(BUFFER TT_ArtLager:HANDLE).
  hQuery:QUERY-PREPARE(IF iSortering = 1 THEN cQRY1 ELSE cQRY2).
/*   hQuery:QUERY-PREPARE("FOR EACH TT_ArtLager"). */
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "Sesonganalys.pdf".

RUN pdf_new ("Spdf",pcRappFil).
pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PDFPageFooter").
RUN pdf_set_PaperType ("Spdf","A4").
RUN pdf_set_LeftMargin ("Spdf", 30).
RUN pdf_set_BottomMargin ("Spdf", 40).
RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf", 13).
/*RUN pdf_set_Orientation ("Spdf", "landscape")*/
RUN pdf_set_Orientation ("Spdf","Portrait").

RUN PDFPositioner.
RUN pdf_new_page ("Spdf").
RUN PDFPageHeader.

  CREATE tmpTT_ArtLagerTOT.
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
          IF lNullposter = FALSE AND TT_ArtLager.AntSolgt = 0 AND TT_ArtLager.KjopAnt = 0 AND TT_ArtLager.LagAnt = 0 THEN DO:
              hQuery:GET-NEXT().
              NEXT.
          END.
          IF iSortering = 2 AND iOldLevNr <> TT_ArtLager.LevNr THEN DO:
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
                RUN pdf_new_page ("Spdf").
                RUN PDFPageHeader.      
                RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
                  PAGE.
/*                   PUT UNFORMATTED "<B>" cColLabelString "</B>" SKIP. */
                  iRad = iFirstRad.
              END.
/*              PUT UNFORMATTED "<R" iRad ">" cLevStr SKIP.*/
              iRad = iRad + 1.
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
          IF iVg <> TT_Artlager.vg THEN DO:
              IF iVg <> 0 THEN DO:
                  lFlag = FALSE.
                  FOR EACH TT_VG_KSL WHERE TT_VG_KSL.Vg = iVg:
                      IF (dY - 20) < 40 THEN 
                      DO:
                        RUN pdf_new_page ("Spdf").
                        RUN PDFPageHeader.
                        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
                      END.
                      cBoldStart = IF TT_VG_KSL.butik = iTotButik THEN "<B>" ELSE "".
                      cBoldEnd   = IF TT_VG_KSL.butik = iTotButik THEN "</B>" ELSE "".
/*                      PUT UNFORMATTED "<R" iRad ">" cBoldStart getdatalinje(IF lFlag = TRUE THEN 4 ELSE 41) cBoldEnd SKIP.*/
                      iRad = iRad + IF lFlag = FALSE THEN 2 ELSE 1.
                      lFlag = TRUE.
                  END.
              END.
              ASSIGN dY = dY - 13.
              RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).

              /*              FIND vargr WHERE vargr.vg = tt_artlager.vg NO-LOCK NO-ERROR.
              cWrk = STRING(tt_artlager.vg) + " " + (IF AVAIL vargr THEN vargr.vgbeskr ELSE "--").*/
/*              PDFgetdatalinje(0).*/
              RUN pdf_text_xy_dec ("Spdf",PDFgetdatalinje(0),dColpos[1],dY).
              iVg = TT_ArtLager.vg.
              iRad = iRad + 2.
          END.
/*          IF (dY - 20) < 40 THEN */
          IF dY < 162 THEN 
          DO:
            RUN pdf_new_page ("Spdf").
            RUN PDFPageHeader.
            RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
/*                   PUT UNFORMATTED "<B>" cColLabelString "</B>" SKIP. */
          END.
          RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
          PDFgetDataLinje(1).
          ASSIGN dY = dY - 13.
          DO iCount2 = 1 TO 5:
            IF iCount2 = 3 AND LENGTH(cTmpData[3]) > 12 THEN
              RUN pdf_text_xy_dec ("Spdf",SUBSTRING(cTmpData[iCount2],1,12),dColPos2[iCount2],dY).
            ELSE IF iCount2 = 4 AND LENGTH(cTmpData[4]) > 30 THEN
              RUN pdf_text_xy_dec ("Spdf",SUBSTRING(cTmpData[iCount2],1,30),dColPos2[iCount2],dY).
            ELSE
              RUN pdf_text_xy_dec ("Spdf",cTmpData[iCount2],dColPos2[iCount2],dY).
          END.

          ASSIGN dY2 = dY - 68.
          ASSIGN dY = dY - 26.
          DO iCount2 = 1 TO 10:
            RUN pdf_text_xy_dec ("Spdf",cTotalRub[iCount2],dColPos3[iCount2],dY).
            ASSIGN cUL = "".
            DO iLen = 1 TO LENGTH(cTotalRub[iCount2]):
              ASSIGN cUL = cUL + "_".
            END.
            RUN pdf_text_xy_dec ("Spdf",cUL,dColPos3[iCount2],dY).    

          END.
          ASSIGN dY = dY - 13.
          PDFgetDataLinje(11).
          DO iCount2 = 1 TO 10:
            RUN pdf_text_xy_dec ("Spdf",cTmpData[iCount2],dColPos4[iCount2],dY).
          END.

     IF ArtBas.BildNr > 0 THEN
       cBildeFil = getBildeFil(ArtBas.BildNr).

    ASSIGN cBildeFil = REPLACE(cBildeFil,".bmp",".jpg").
    IF cBildeFil <> "" AND SEARCH(cBildeFil) <> ? THEN
    DO:
      IF dY2 < 95 THEN
      DO:
        RUN pdf_new_page ("Spdf").
        RUN PDFPageHeader.
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
      END.
      ASSIGN cBildNamn = "mini" + STRING(TT_ArtLager.ArtikkelNr).
      IF NOT CAN-FIND(FIRST TT_image WHERE TT_image.image_name = cBildNamn NO-LOCK) THEN
      DO:
        RUN pdf_load_image ("Spdf",cBildNamn,cBildeFil).
        CREATE TT_image.
        ASSIGN TT_image.IMAGE_name = cBildNamn.
      END.
      ASSIGN iImageHeight = pdf_imageDim ("Spdf",cBildNamn,"HEIGHT").
      ASSIGN iImageWidth = pdf_imageDim ("Spdf",cBildNamn,"WIDTH").

      IF iImageWidth >= iImageHeight THEN
        
          ASSIGN iImageWidth = 50 * iImageWidth / iImageHeight
                 iImageHeight = 50.
      ELSE
          ASSIGN iImageWidth = 50 * (iImageWidth / iImageHeight)   /* 60 */
                 iImageHeight = 50.
      ASSIGN dY2 = dY - 20
             dY2 = pdf_PageHeight ("Spdf") - dY + 15. /* 55,50 */ 
      RUN pdf_place_image ("Spdf",cBildNamn,dColpos[1],dY2,iImageWidth,iImageHeight).
/*      ASSIGN dY = dY - 40. 
      ASSIGN doRad2 = dY - DECIMAL(iImageHeight).*/
    END.
/*    ELSE
      dY = dY - 60.*/
          
          FIND TT_KjopSalgLager WHERE TT_KjopSalgLager.ArtikkelNr = TT_ArtLager.ArtikkelNr AND
              (TT_KjopSalgLager.iAntsolgt <> 0 OR TT_KjopSalgLager.iKjopAnt <> 0 OR TT_KjopSalgLager.iLagant <> 0) NO-ERROR.
          IF AVAIL TT_KjopSalgLager THEN DO:
              ASSIGN dY = dY - 26.
              IF (dY - 20) < 40 THEN 
              DO:
                RUN pdf_new_page ("Spdf").
                RUN PDFPageHeader.
                RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
/*                   PUT UNFORMATTED "<B>" cColLabelString "</B>" SKIP. 
                  iRad = iFirstRad.*/
              END.
              ASSIGN iRad = 80.
              DO iCount = 1 TO NUM-ENTRIES(TT_KjopSalgLager.Storl):
                IF ENTRY(iCount,TT_KjopSalgLager.VisEntry) = "1" THEN
                DO:
                   ASSIGN iRad = iRad + 20.
                   RUN pdf_text_xy_dec ("Spdf",ENTRY(iCount,TT_KjopSalgLager.Storl),iRad,dY).
                   ASSIGN cUL = "".
                   DO iLen = 1 TO LENGTH(ENTRY(iCount,TT_KjopSalgLager.Storl)):
                     ASSIGN cUL = cUL + "_".
                   END.
                   RUN pdf_text_xy_dec ("Spdf",cUL,iRad,dY).    
                END.
              END.
              ASSIGN iRad = 80.
              ASSIGN dY = dY - 13.
              RUN pdf_text_xy_dec ("Spdf",cRub2[2],dColPos[1],dY).
              DO iCount = 1 TO NUM-ENTRIES(TT_KjopSalgLager.AntSolgt):
                  IF ENTRY(iCount,TT_KjopSalgLager.VisEntry) = "1" THEN
                  DO:
                    ASSIGN iRad = iRad + 20.
                    RUN pdf_text_xy_dec ("Spdf",ENTRY(iCount,TT_KjopSalgLager.Kjopant),iRad,dY).
                  END.
              END.
              ASSIGN iRad = 80.
              ASSIGN dY = dY - 13.
              RUN pdf_text_xy_dec ("Spdf",cRub2[1],dColPos[1],dY).
              DO iCount = 1 TO NUM-ENTRIES(TT_KjopSalgLager.AntSolgt):
                  IF ENTRY(iCount,TT_KjopSalgLager.VisEntry) = "1" THEN
                  DO:
                    ASSIGN iRad = iRad + 20.
                    RUN pdf_text_xy_dec ("Spdf",ENTRY(iCount,TT_KjopSalgLager.AntSolgt),iRad,dY).
                  END.
              END.

              ASSIGN iRad = 80.
              ASSIGN dY = dY - 13.
              RUN pdf_text_xy_dec ("Spdf",cRub2[3],dColPos[1],dY).
              DO iCount = 1 TO NUM-ENTRIES(TT_KjopSalgLager.AntSolgt):
                  IF ENTRY(iCount,TT_KjopSalgLager.VisEntry) = "1" THEN
                  DO:
                    ASSIGN iRad = iRad + 20.
                    RUN pdf_text_xy_dec ("Spdf",ENTRY(iCount,TT_KjopSalgLager.LagAnt),iRad,dY).
                  END.
              END.
/*gh                  PUT SKIP.
              PUT UNFORMATTED "<R" iRad "><P8><B>" /* "<C2>" + STRING(LINE-COUNTER) + */ getKjopSolgtLagerLinje(1) "</B>" SKIP.
              PUT UNFORMATTED "<R" iRad + 1 ">" getKjopSolgtLagerLinje(2) SKIP.
              PUT UNFORMATTED "<R" iRad + 2 ">"  getKjopSolgtLagerLinje(3) SKIP.
              PUT UNFORMATTED "<R" iRad + 3 ">"  getKjopSolgtLagerLinje(4) "<P8>" SKIP(1).*/
              dY = dY - 13.
              iRad = iRad + 5.
          END.
          ELSE 
              iRad = iRad + 2.
          hQuery:GET-NEXT().
          IF NOT hQuery:QUERY-OFF-END AND iRad >= iSideBrytRad THEN DO:
/*              RUN PFooter.
              PAGE.
              VIEW FRAME PageHeader.
              PUT UNFORMATTED cTitlestring.*/
              iRad = iFirstRad.
          END.
  END.
  DO:
      iRad = iRad + 1.
      lFlag = FALSE.
      FOR EACH TT_VG_KSL WHERE TT_VG_KSL.Vg = 0:
/*       FOR EACH TT_VG_KSL WHERE TT_VG_KSL.Vg = iVg: */
/*          cBoldStart = IF TT_VG_KSL.butik = iTotButik THEN "<B>" ELSE "".
          cBoldEnd   = IF TT_VG_KSL.butik = iTotButik THEN "</B>" ELSE "".*/
      ASSIGN dY = dY - 13.
      IF (dY - 20) < 40 THEN 
      DO:
        RUN pdf_new_page ("Spdf").
        RUN PDFPageHeader.
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
        ASSIGN lFlag = FALSE.
      END.
      IF lFlag = FALSE THEN
      DO:
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
        DO iCount2 = 1 TO 8:
          RUN pdf_text_xy_dec ("Spdf",cVgTotalrub[iCount2],dColPos5[iCount2],dY).
          ASSIGN cUL = "".
          DO iLen = 1 TO LENGTH(cVgTotalRub[iCount2]):
            ASSIGN cUL = cUL + "_".
          END.
          RUN pdf_text_xy_dec ("Spdf",cUL,dColPos5[iCount2],dY).    
        END.
        ASSIGN lFlag = TRUE.
        ASSIGN dY = dY - 13.
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
      END.
      PDFgetdatalinje(4).
/*      MESSAGE "1 " cTmpData[1] SKIP
              "6 " cTmpData[6] SKIP
              "7 " cTmpData[7] SKIP
              "8 " cTmpData[8]
          VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
      IF cTmpData[1] = "TOT" THEN
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
      ELSE
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
      DO iCount2 = 1 TO 8:
        RUN pdf_text_xy_dec ("Spdf",cTmpData[iCount2],dColPos6[iCount2] - bredd(cTmpData[iCount2]),dy).
      END.
/*          PUT UNFORMATTED "<R" iRad ">" cBoldStart getdatalinje(IF lFlag = TRUE THEN 4 ELSE 41) cBoldEnd SKIP.*/
          iRad = iRad + IF lFlag = FALSE THEN 2 ELSE 1.
          IF iRad >= iSideBrytRad THEN DO:
/*              RUN PFooter.
              PAGE.
              VIEW FRAME PageHeader.
              PUT UNFORMATTED cTitlestring.*/
/*                   PUT UNFORMATTED "<B>" cColLabelString "</B>" SKIP. */
              iRad = iFirstRad.
          END.
          lFlag = TRUE.
      END.
  END.
  
  RUN pdf_close ("Spdf").

/*  MESSAGE "Visa Lista"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

RUN browse2pdf\viewxmldialog.w (pcRappFil,"Polygon Retail Solutions").

END PROCEDURE.

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
              PUT UNFORMATTED "<R64><C6><FROM><C80><LINE>" SKIP
                              "<C6>" cKundenavn "<C1><CENTER=C80>" cPolygon SKIP.

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
  DEF VAR pcRappFil         AS CHAR NO-UNDO.
  DEFINE VARIABLE cQRY1     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQRY2     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQRY3     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iOldLevNr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cLevStr   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lFirst    AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE iRad         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iVg          AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lFlag        AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE iFirstRad    AS INTEGER INIT 6   NO-UNDO.
  DEFINE VARIABLE cTitleString AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cBoldStart   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cBoldEnd     AS CHARACTER  NO-UNDO.
  cTitlestring = "<R3.5><B><C1><CENTER=C80><P16>" + cTitle + "</B><P8><R+2.5>".
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

  ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "Sesonganalys.xpr".
  OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(65).
  PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
  PUT CONTROL '<PREVIEW=ZoomToWidth>'.
/*   PUT CONTROL '<PREVIEW=ZoomToWidth><OLANDSCAPE>'. */
  VIEW FRAME PageHeader.
  PUT UNFORMATTED cTitlestring.
/*   PUT UNFORMATTED "<B>" cColLabelString "</B>" SKIP. */

  iRad = iFirstRad.
  iSideBrytRad = PAGE-SIZE - 3.

  CREATE tmpTT_ArtLagerTOT.
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
          IF lNullposter = FALSE AND TT_ArtLager.AntSolgt = 0 AND TT_ArtLager.KjopAnt = 0 AND TT_ArtLager.LagAnt = 0 THEN DO:
              hQuery:GET-NEXT().
              NEXT.
          END.
          IF iSortering = 2 AND iOldLevNr <> TT_ArtLager.LevNr THEN DO:
              IF lFirst = TRUE THEN DO:
                  PUT UNFORMATTED "<R" iRad "><B>" getDataLinje(2) "</B>"SKIP.
                  iRad = iRad + 1.
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
                  PUT UNFORMATTED cTitlestring.
/*                   PUT UNFORMATTED "<B>" cColLabelString "</B>" SKIP. */
                  iRad = iFirstRad.
              END.
              PUT UNFORMATTED "<R" iRad ">" cLevStr SKIP.
              iRad = iRad + 1.
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
          IF iVg <> TT_Artlager.vg THEN DO:
              IF iVg <> 0 THEN DO:
                  iRad = iRad + 1.
                  lFlag = FALSE.
                  FOR EACH TT_VG_KSL WHERE TT_VG_KSL.Vg = iVg:
                      IF iRad >= iSideBrytRad THEN DO:
                          RUN PFooter.
                          PAGE.
                          VIEW FRAME PageHeader.
                          PUT UNFORMATTED cTitlestring.
        /*                   PUT UNFORMATTED "<B>" cColLabelString "</B>" SKIP. */
                          iRad = iFirstRad.
                      END.
                      cBoldStart = IF TT_VG_KSL.butik = iTotButik THEN "<B>" ELSE "".
                      cBoldEnd   = IF TT_VG_KSL.butik = iTotButik THEN "</B>" ELSE "".
                      PUT UNFORMATTED "<R" iRad ">" cBoldStart getdatalinje(IF lFlag = TRUE THEN 4 ELSE 41) cBoldEnd SKIP.
                      iRad = iRad + IF lFlag = FALSE THEN 2 ELSE 1.
                      lFlag = TRUE.
                  END.
              END.
              PUT UNFORMATTED "<R" iRad ">" getdatalinje(0) SKIP.
              iVg = TT_ArtLager.vg.
              iRad = iRad + 2.
          END.
          IF iRad >= iSideBrytRad - 4 THEN DO:
              RUN PFooter.
              PAGE.
              VIEW FRAME PageHeader.
              PUT UNFORMATTED cTitlestring.
/*                   PUT UNFORMATTED "<B>" cColLabelString "</B>" SKIP. */
              iRad = iFirstRad.
          END.
          PUT UNFORMATTED "<R" iRad ">" getDataLinje(1) SKIP.
          iRad = iRad + 1.
          PUT UNFORMATTED "<R" iRad + 1 ">" getDataLinje(11) SKIP.
          cBildeFil = "".
          IF ArtBas.BildNr > 0 THEN
              cBildeFil = getBildeFil(ArtBas.BildNr).
          IF cBildeFil <> "" THEN DO:
              PUT UNFORMATTED
                 "<TRANSPARENT=false><R" iRad + 4 ",5><C14><#3><R" iRad ",25><C6><IMAGE#3=" cBildeFil  ">".
              iRad = iRad + 3.
          END.
          ELSE
              iRad = iRad + 1.
          iRad = iRad + 2.

          FIND TT_KjopSalgLager WHERE TT_KjopSalgLager.ArtikkelNr = TT_ArtLager.ArtikkelNr AND
              (TT_KjopSalgLager.iAntsolgt <> 0 OR TT_KjopSalgLager.iKjopAnt <> 0 OR TT_KjopSalgLager.iLagant <> 0) NO-ERROR.
          IF AVAIL TT_KjopSalgLager THEN DO:
/*               IF LINE-COUNTER > 38 THEN DO: */
              IF iRad >= iSideBrytRad THEN DO:
                  RUN PFooter.
                  PAGE.
                  VIEW FRAME PageHeader.
                  PUT UNFORMATTED cTitlestring.
/*                   PUT UNFORMATTED "<B>" cColLabelString "</B>" SKIP. */
                  iRad = iFirstRad.
              END.
              ELSE
                  PUT SKIP.
              PUT UNFORMATTED "<R" iRad "><P8><B>" /* "<C2>" + STRING(LINE-COUNTER) + */ getKjopSolgtLagerLinje(1) "</B>" SKIP.
              PUT UNFORMATTED "<R" iRad + 1 ">" getKjopSolgtLagerLinje(2) SKIP.
              PUT UNFORMATTED "<R" iRad + 2 ">"  getKjopSolgtLagerLinje(3) SKIP.
              PUT UNFORMATTED "<R" iRad + 3 ">"  getKjopSolgtLagerLinje(4) "<P8>" SKIP(1).
/*               PUT UNFORMATTED "Butik" " " TT_KjopSalgLager.Butik " " TT_KjopSalgLager.Storl SKIP */
/*                               "Solgt" " " "   " " " TT_KjopSalgLager.Antsolgt SKIP               */
/*                               "Kjøpt" " " "   " " " TT_KjopSalgLager.Kjopant  SKIP               */
/*                               "Lager" " " "   " " " TT_KjopSalgLager.Lagant   SKIP(1).           */
              iRad = iRad + 5.
          END.
          ELSE 
              iRad = iRad + 2.
          hQuery:GET-NEXT().
          IF NOT hQuery:QUERY-OFF-END AND iRad >= iSideBrytRad THEN DO:
              RUN PFooter.
              PAGE.
              VIEW FRAME PageHeader.
              PUT UNFORMATTED cTitlestring.
/*               PUT UNFORMATTED "<B>" cColLabelString "</B>" SKIP(1). */
              iRad = iFirstRad.
          END.
  END.
  DO:
      iRad = iRad + 1.
      lFlag = FALSE.
      FOR EACH TT_VG_KSL WHERE TT_VG_KSL.Vg = iVg:
          cBoldStart = IF TT_VG_KSL.butik = iTotButik THEN "<B>" ELSE "".
          cBoldEnd   = IF TT_VG_KSL.butik = iTotButik THEN "</B>" ELSE "".
          PUT UNFORMATTED "<R" iRad ">" cBoldStart getdatalinje(IF lFlag = TRUE THEN 4 ELSE 41) cBoldEnd SKIP.
          iRad = iRad + IF lFlag = FALSE THEN 2 ELSE 1.
          IF iRad >= iSideBrytRad THEN DO:
              RUN PFooter.
              PAGE.
              VIEW FRAME PageHeader.
              PUT UNFORMATTED cTitlestring.
/*                   PUT UNFORMATTED "<B>" cColLabelString "</B>" SKIP. */
              iRad = iFirstRad.
          END.
          lFlag = TRUE.
      END.
  END.
/*   IF iSortering = 2 THEN DO:                                       */
/*       PUT UNFORMATTED "<R" iRad "><B>" getDataLinje(2) "</B>"SKIP. */
/*   END.                                                             */
/*   PUT UNFORMATTED "<R+2><B>" getDataLinje(3) "</B>"SKIP. */
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

&IF DEFINED(EXCLUDE-getBildeFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBildeFil Procedure 
FUNCTION getBildeFil RETURNS CHARACTER
  ( INPUT ipBildNr AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ocBildeFil AS CHARACTER  NO-UNDO.
  FIND BildeRegister NO-LOCK WHERE
    BildeRegister.BildNr = ipBildNr NO-ERROR.
  IF AVAIL BildeRegister AND TRIM(BildeRegister.FilNavn) <> "" THEN DO:
    IF VALID-HANDLE(wLibHandle) THEN
      RUN HentBildePeker IN wLibHandle (INPUT ipBildNr, 1, BildeRegister.FilNavn, OUTPUT ocBildeFil).
  END.
  /* cBlanktBilde */
  RETURN ocBildeFil.   /* Function return value. */

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
    DEFINE VARIABLE cTmpData   AS CHARACTER EXTENT 15 NO-UNDO.
    DEFINE VARIABLE cColLabels AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCount     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iCount2    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cRub AS CHARACTER  NO-UNDO.
    /*
    ["VG/Lopnr","Beskrivelse","Farge","Lev","Lev.art.nr","Ses","Varekost","Pris","Tilbud","Solgt",
    "Solgt verdi","Kjøpt","Kjøpt verdi","Lager","Lagerverdi"] NO-UNDO.
     */
    IF iTyp = 0 THEN DO: /* Varegruppetext  */
        FIND vargr WHERE vargr.vg = tt_artlager.vg NO-LOCK NO-ERROR.
        cColLabels = "<C6><B><P10>" + STRING(tt_artlager.vg) + " " + (IF AVAIL vargr THEN vargr.vgbeskr ELSE "--") + 
                                             "</B><P8>".

    END.
    ELSE IF iTyp = 1 THEN DO: /* TT_ArtLager */
        FIND ArtBas WHERE ArtBas.ArtikkelNr = TT_ArtLager.ArtikkelNr NO-LOCK NO-ERROR.
        FIND Farg OF ArtBas NO-LOCK NO-ERROR.
        FIND Sasong OF ArtBas NO-LOCK NO-ERROR.
        FIND LevBas OF artbas NO-LOCK NO-ERROR.
        ASSIGN cTmpData[1] = STRING(Artbas.Vg) + "/" + (IF ArtBas.Lopnr = ? THEN "" ELSE STRING(ArtBas.LopNr))
               cTmpData[2] = ArtBas.LevKod
               cTmpData[3] = SUBSTR(ArtBas.Bongtekst,1,17)
               cTmpData[4] = IF AVAIL Levbas THEN levbas.levnamn ELSE " "
               cTmpData[5] = IF AVAIL Farg THEN Farg.FarBeskr ELSE " ".
/*                cTmpData[6] = STRING(TT_ArtLager.VVarekost,">>,>>9.99"). */
/*           ASSIGN cTmpData[1] = STRING(Artbas.Vg) + "/" + STRING(ArtBas.LopNr)                                     */
/*                  cTmpData[2] = SUBSTR(ArtBas.Bongtekst,1,17)                                                      */
/*                  cTmpData[3] = IF AVAIL Farg THEN Farg.FarBeskr ELSE " "                                          */
/*                  cTmpData[4] = STRING(ArtBas.LevNr)                                                               */
/*                  cTmpData[5] = ArtBas.LevKod                                                                      */
/*                  cTmpData[6] = IF AVAIL Sasong AND Sasong.Sasong > 0 THEN STRING(Sasong.Sasong) ELSE " "          */
/*                  cTmpData[7] = STRING(TT_ArtLager.VVarekost,">>,>>9.99")                                          */
/*                  cTmpData[8] = STRING(TT_ArtLager.PrisOrdi,">>,>>9.99")                                           */
/*                  cTmpData[9] = IF TT_ArtLager.PrisTilb > 0 THEN STRING(TT_ArtLager.PrisTilb,">>,>>9.99") ELSE " " */
/*                  cTmpData[10] = STRING(TT_ArtLager.AntSolgt)                                                      */
/*                  cTmpData[11] = STRING(TT_ArtLager.VerdiSolgt,"->>,>>>,>>9")                                      */
/*                  cTmpData[12] = STRING(TT_ArtLager.KjopAnt)                                                       */
/*                  cTmpData[13] = STRING(TT_ArtLager.KjopVerdi,"->>,>>>,>>9")                                       */
/*                  cTmpData[14] = STRING(TT_ArtLager.LagAnt)                                                        */
/*                  cTmpData[15] = STRING(TT_ArtLager.LagAnt * TT_ArtLager.VVarekost,"->>,>>>,>>9").                 */

    END.
    ELSE IF iTyp = 11 THEN DO: /* TT_ArtLager */
        IF TT_ArtLager.KjopAnt = 0 AND TT_ArtLager.AntSolgt = 0 AND TT_ArtLager.LagAnt = 0 THEN
            RETURN "".
        ASSIGN   cTmpData[1] = "Total:"
                 cTmpData[2] = STRING(TT_ArtLager.PrisOrdi,">>,>>9.99")
                 cTmpData[3] = STRING(TT_ArtLager.KjopAnt)
                 cTmpData[4] = STRING(TT_ArtLager.AntSolgt)
                 cTmpData[5] = STRING(TT_ArtLager.LagAnt)
                 cTmpData[6] = IF TT_ArtLager.KjopAnt > 0 AND TT_ArtLager.AntSolgt > 0 THEN
                                              STRING(TT_ArtLager.AntSolgt / TT_ArtLager.KjopAnt * 100,">>9") + " %" ELSE " "
                 cTmpData[7] = STRING(TT_ArtLager.VerdiSolgt,"->>>,>>>,>>9.99").

        ASSIGN cRub = "<U>" + cPrintStringTOT.
        DO iCount2 = 1 TO 8:
            ASSIGN cRub = REPLACE(cRub,"&" + STRING(iCount2,"99"),cTotalRub[iCount2]).
        END.
        cRub = cRub + "</U><R+1>".
        ASSIGN cColLabels = cPrintStringTOT.
        DO iCount2 = 1 TO 8:
            ASSIGN cColLabels = REPLACE(cColLabels,"&" + STRING(iCount2,"99"),cTmpData[iCount2]).
        END.
        cCollabels = cRub + cColLabels.
        RETURN cColLabels.
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
    ELSE IF iTyp = 4 OR iTyp = 41 THEN DO:
        ASSIGN   cTmpData[1] = IF TT_VG_KSL.butik = iTotButik THEN "TOT" ELSE STRING(TT_VG_KSL.butik)
                 cTmpData[2] = STRING(TT_VG_KSL.iKjopAnt)
                 cTmpData[3] = STRING(TT_VG_KSL.iAntsolgt)
                 cTmpData[4] = STRING(TT_VG_KSL.iLagant)
                 cTmpData[5] = IF TT_VG_KSL.iKjopAnt > 0 AND TT_VG_KSL.iAntsolgt > 0 THEN
                                              STRING(TT_VG_KSL.iAntsolgt / TT_VG_KSL.iKjopAnt * 100,">>>>9") + " %" ELSE "..."
                 cTmpData[6] = STRING(TT_VG_KSL.dTotSalgBelop,"->>>,>>>,>>9.99")
                 cTmpData[7] = " ".

        IF iTyp = 41 THEN DO:
            ASSIGN cRub = "<U>" + cPrintStringVgTOT.
            DO iCount2 = 1 TO 7:
                ASSIGN cRub = REPLACE(cRub,"&" + STRING(iCount2,"99"),cVgTotalRub[iCount2]).
            END.
            cRub = cRub + "</U><R+1>".
        END.
        ASSIGN cColLabels = cPrintStringVgTOT.
        DO iCount2 = 1 TO 8:
            ASSIGN cColLabels = REPLACE(cColLabels,"&" + STRING(iCount2,"99"),cTmpData[iCount2]).
        END.
        cCollabels = cRub + cColLabels.
        RETURN cColLabels.
    END.
    IF iTyp <> 0 THEN DO:
        ASSIGN cColLabels = cPrintString.
        DO iCount2 = 1 TO EXTENT(cTmpData):
            ASSIGN cColLabels = REPLACE(cColLabels,"&" + STRING(iCount2,"99"),cTmpData[iCount2]).
        END.
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
  DEFINE VARIABLE iCount2   AS INTEGER INIT 8 NO-UNDO.
  ASSIGN cReturRad = "<C6>".
  CASE iRad:
      WHEN 1 THEN DO:
          ASSIGN cReturRad = cReturRad + " " + "<U>". /* + "Butikk: " + STRING(TT_KjopSalgLager.Butik). */
          DO iCount = 1 TO NUM-ENTRIES(TT_KjopSalgLager.Storl):
              IF ENTRY(iCount,TT_KjopSalgLager.VisEntry) = "1" THEN
              ASSIGN iCount2 = iCount2 + 3
                     cReturRad = cReturRad + "<C" + STRING(iCount2) + "><RIGHT=C+3>" + ENTRY(iCount,TT_KjopSalgLager.Storl).
          END.
          ASSIGN cReturRad = cReturRad + "</U>".
      END.
      WHEN 2 THEN DO:
          ASSIGN cReturRad = cReturRad + "Solgt: ".
          DO iCount = 1 TO NUM-ENTRIES(TT_KjopSalgLager.AntSolgt):
              IF ENTRY(iCount,TT_KjopSalgLager.VisEntry) = "1" THEN
              ASSIGN iCount2 = iCount2 + 3
                     cReturRad = cReturRad + "<C" + STRING(iCount2) + "><RIGHT=C+3>" + ENTRY(iCount,TT_KjopSalgLager.AntSolgt).
          END.
          cReturRad = cReturRad + "<C" + STRING(iCount2 + 3) + "><RIGHT=C+3>" + STRING(TT_KjopSalgLager.iAntSolgt).
      END.
      WHEN 3 THEN DO:
          ASSIGN cReturRad = cReturRad + "Kjøpt: ".
          DO iCount = 1 TO NUM-ENTRIES(TT_KjopSalgLager.Kjopant):
              IF ENTRY(iCount,TT_KjopSalgLager.VisEntry) = "1" THEN
              ASSIGN iCount2 = iCount2 + 3
                     cReturRad = cReturRad + "<C" + STRING(iCount2) + "><RIGHT=C+3>" + ENTRY(iCount,TT_KjopSalgLager.Kjopant).
          END.
          cReturRad = cReturRad + "<C" + STRING(iCount2 + 3) + "><RIGHT=C+3>" + STRING(TT_KjopSalgLager.iKjopant).
      END.
      WHEN 4 THEN DO:
          ASSIGN cReturRad = cReturRad + "Lager: ".
          DO iCount = 1 TO NUM-ENTRIES(TT_KjopSalgLager.Lagant):
              IF ENTRY(iCount,TT_KjopSalgLager.VisEntry) = "1" THEN
              ASSIGN iCount2 = iCount2 + 3
                     cReturRad = cReturRad + "<C" + STRING(iCount2) + "><RIGHT=C+3>" + ENTRY(iCount,TT_KjopSalgLager.Lagant).
          END.
          cReturRad = cReturRad + "<C" + STRING(iCount2 + 3) + "><RIGHT=C+3>" + STRING(TT_KjopSalgLager.iLagAnt).
      END.
  END CASE.
  RETURN cReturRad + " ".   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFgetDataLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PDFgetDataLinje Procedure 
FUNCTION PDFgetDataLinje RETURNS CHARACTER
    ( INPUT iTyp AS INTEGER ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
      DEFINE VARIABLE cColLabels AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE cRub  AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE dWrk  AS DECIMAL FORMAT "->>>,>>9.99"    NO-UNDO.
      DEFINE VARIABLE dWrk2 AS DECIMAL FORMAT "->>>,>>9.99"    NO-UNDO.
      /*
      ["VG/Lopnr","Beskrivelse","Farge","Lev","Lev.art.nr","Ses","Varekost","Pris","Tilbud","Solgt",
      "Solgt verdi","Kjøpt","Kjøpt verdi","Lager","Lagerverdi"] NO-UNDO.
       */
      IF iTyp = 0 THEN DO: /* Varegruppetext  */
          FIND vargr WHERE vargr.vg = tt_artlager.vg NO-LOCK NO-ERROR.
          cColLabels = STRING(tt_artlager.vg) + " " + (IF AVAIL vargr THEN vargr.vgbeskr ELSE "--").
/*          ASSIGN cTmpData[1] = cColLabels.*/

      END.
      ELSE IF iTyp = 1 THEN DO: /* TT_ArtLager */
          FIND ArtBas WHERE ArtBas.ArtikkelNr = TT_ArtLager.ArtikkelNr NO-LOCK NO-ERROR.
          FIND Farg OF ArtBas NO-LOCK NO-ERROR.
          FIND Sasong OF ArtBas NO-LOCK NO-ERROR.
          FIND LevBas OF artbas NO-LOCK NO-ERROR.
          ASSIGN cTmpData[1] = STRING(Artbas.Vg) + "/" + (IF ArtBas.Lopnr = ? THEN "" ELSE STRING(ArtBas.LopNr))
                 cTmpData[2] = ArtBas.LevKod
                 cTmpData[3] = SUBSTR(ArtBas.Bongtekst,1,17)
                 cTmpData[4] = IF AVAIL Levbas THEN levbas.levnamn ELSE " "
                 cTmpData[5] = IF AVAIL Farg THEN Farg.FarBeskr ELSE " ".
  /*                cTmpData[6] = STRING(TT_ArtLager.VVarekost,">>,>>9.99"). */
  /*           ASSIGN cTmpData[1] = STRING(Artbas.Vg) + "/" + STRING(ArtBas.LopNr)                                     */
  /*                  cTmpData[2] = SUBSTR(ArtBas.Bongtekst,1,17)                                                      */
  /*                  cTmpData[3] = IF AVAIL Farg THEN Farg.FarBeskr ELSE " "                                          */
  /*                  cTmpData[4] = STRING(ArtBas.LevNr)                                                               */
  /*                  cTmpData[5] = ArtBas.LevKod                                                                      */
  /*                  cTmpData[6] = IF AVAIL Sasong AND Sasong.Sasong > 0 THEN STRING(Sasong.Sasong) ELSE " "          */
  /*                  cTmpData[7] = STRING(TT_ArtLager.VVarekost,">>,>>9.99")                                          */
  /*                  cTmpData[8] = STRING(TT_ArtLager.PrisOrdi,">>,>>9.99")                                           */
  /*                  cTmpData[9] = IF TT_ArtLager.PrisTilb > 0 THEN STRING(TT_ArtLager.PrisTilb,">>,>>9.99") ELSE " " */
  /*                  cTmpData[10] = STRING(TT_ArtLager.AntSolgt)                                                      */
  /*                  cTmpData[11] = STRING(TT_ArtLager.VerdiSolgt,"->>,>>>,>>9")                                      */
  /*                  cTmpData[12] = STRING(TT_ArtLager.KjopAnt)                                                       */
  /*                  cTmpData[13] = STRING(TT_ArtLager.KjopVerdi,"->>,>>>,>>9")                                       */
  /*                  cTmpData[14] = STRING(TT_ArtLager.LagAnt)                                                        */
  /*                  cTmpData[15] = STRING(TT_ArtLager.LagAnt * TT_ArtLager.VVarekost,"->>,>>>,>>9").                 */

      END.
      ELSE IF iTyp = 11 THEN DO: /* TT_ArtLager */
          IF TT_ArtLager.KjopAnt = 0 AND TT_ArtLager.AntSolgt = 0 AND TT_ArtLager.LagAnt = 0 THEN
              RETURN "".
          ASSIGN dWrk = TT_ArtLager.VerdiSolgt - TT_ArtLager.SVK.
          ASSIGN   cTmpData[1] = "Total:"
                   cTmpData[3] = STRING(TT_ArtLager.PrisOrdi,">>,>>9.99")
                   cTmpData[4] = STRING(TT_ArtLager.KjopAnt)
                   cTmpData[5] = STRING(TT_ArtLager.AntSolgt)
                   cTmpData[6] = STRING(TT_ArtLager.LagAnt)
                   cTmpData[7] = IF TT_ArtLager.KjopAnt > 0 AND TT_ArtLager.AntSolgt > 0 THEN
                                                STRING(TT_ArtLager.AntSolgt / TT_ArtLager.KjopAnt * 100,">>9") + " %" ELSE " "
                   cTmpData[8] = STRING(dWrk,"->>>,>>9.99")                                                  /* TB% */
                   cTmpData[10] = STRING(TT_ArtLager.VerdiSolgt,"->>>,>>>,>>9.99").
          ASSIGN dWrk2 = dWrk * 100.
          ASSIGN dWrk2 = dWrk2 / TT_ArtLager.VerdiSolgt.
          ASSIGN cTmpData[9] = STRING(dWrk2,"->9.99").
          ASSIGN cTmpData[2] = STRING(TT_ArtLager.InPris,">>,>>9.99").

          ASSIGN cRub = cPrintStringTOT.
          DO iCount2 = 1 TO 8:
              ASSIGN cRub = REPLACE(cRub,"&" + STRING(iCount2,"99"),cTotalRub[iCount2]).
          END.
          cRub = cRub.
          ASSIGN cColLabels = cPrintStringTOT.
          DO iCount2 = 1 TO 8:
              ASSIGN cColLabels = REPLACE(cColLabels,"&" + STRING(iCount2,"99"),cTmpData[iCount2]).
          END.
          cCollabels = cRub + cColLabels.
          RETURN cColLabels.
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
/*      ELSE IF iTyp = 4 OR iTyp = 41 THEN DO:*/
      ELSE IF iTyp = 4 THEN DO:
          ASSIGN   cTmpData[1] = IF TT_VG_KSL.butik = iTotButik THEN "TOT" ELSE STRING(TT_VG_KSL.butik)
                   cTmpData[2] = STRING(TT_VG_KSL.iKjopAnt)
                   cTmpData[3] = STRING(TT_VG_KSL.iAntsolgt)
                   cTmpData[4] = STRING(TT_VG_KSL.iLagant)
                   cTmpData[5] = IF TT_VG_KSL.iKjopAnt > 0 AND TT_VG_KSL.iAntsolgt > 0 THEN
                                                STRING(TT_VG_KSL.iAntsolgt / TT_VG_KSL.iKjopAnt * 100,">>>>9") + " %" ELSE "..."
                   cTmpData[8] = STRING(TT_VG_KSL.dTotSalgBelop,"->>>,>>>,>>9.99").
          ASSIGN dWrk = TT_VG_KSL.dTotSalgBelop - TT_VG_KSL.SVK.
          ASSIGN cTmpData[6] = STRING(dWrk,"->>>,>>>,>>9.99").
          IF TT_VG_KSL.dTotSalgBelop = 0 OR TT_VG_KSL.SVK = 0 THEN
          DO:
            ASSIGN dWrk2 = 0.
            ASSIGN cTmpData[7] = STRING(dWrk2,"->9.99").
          END.
          ELSE
          DO:
            ASSIGN dWrk2 = dWrk * 100.
            ASSIGN dWrk2 = dWrk2 / TT_VG_KSL.dTotSalgBelop.
            ASSIGN cTmpData[7] = STRING(dWrk2,"->9.99").
          END.

/*          IF iTyp = 41 THEN 
          DO:
              ASSIGN cRub = cPrintStringVgTOT.
              DO iCount2 = 1 TO 7:
                  ASSIGN cRub = REPLACE(cRub,"&" + STRING(iCount2,"99"),cVgTotalRub[iCount2]).
              END.
              cRub = cRub.
          END.
          ASSIGN cColLabels = cPrintStringVgTOT.
          DO iCount2 = 1 TO 8:
              ASSIGN cColLabels = REPLACE(cColLabels,"&" + STRING(iCount2,"99"),cTmpData[iCount2]).
          END.
          cCollabels = cRub + cColLabels.
          RETURN cColLabels.*/
      END.
/*      IF iTyp <> 0 THEN DO:
          ASSIGN cColLabels = cPrintString.
          DO iCount2 = 1 TO EXTENT(cTmpData):
              ASSIGN cColLabels = REPLACE(cColLabels,"&" + STRING(iCount2,"99"),cTmpData[iCount2]).
          END.
      END.*/
    RETURN cColLabels.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

