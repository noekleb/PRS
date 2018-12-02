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

/* DEF INPUT  PARAM icParam     AS CHAR  NO-UNDO.  */
/* DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO. */
/* DEF INPUT  PARAM icSessionId AS CHAR  NO-UNDO.  */
/* DEF OUTPUT PARAM ocReturn    AS CHAR  NO-UNDO.  */
/* DEF OUTPUT PARAM obOk        AS LOG NO-UNDO.    */
DEFINE INPUT  PARAMETER cKOrdreID AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cFilnavn AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dKOrdreID AS DECIMAL     NO-UNDO.
DEF VAR icParam     AS CHAR  NO-UNDO. 
DEF VAR ihBuffer    AS HANDLE NO-UNDO.
DEF VAR icSessionId AS CHAR  NO-UNDO.
DEF VAR ocReturn    AS CHAR  NO-UNDO.
DEF VAR obOk        AS LOG NO-UNDO. 

DEFIN BUFFER bufKOL FOR KOrdrelinje.


DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR fReklamasjonsNr AS INT    NO-UNDO.
DEF VAR iStatus         AS INT    NO-UNDO.

DEFINE VARIABLE iCols          AS INTEGER EXTENT 7  NO-UNDO.
DEFINE VARIABLE cColLabels     AS CHARACTER EXTENT 7  NO-UNDO.
DEFINE VARIABLE iToRight       AS INTEGER EXTENT 7  NO-UNDO.
DEFINE VARIABLE iLeftMargin    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iRMarginPos    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPageHeight    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPageWidth     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCL            AS INTEGER     NO-UNDO.
DEFINE VARIABLE cFirmanavn     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAdress        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPostadress AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iStartRow AS INTEGER         NO-UNDO.
DEFINE VARIABLE iMittenR    AS INTEGER     NO-UNDO.
DEFINE VARIABLE cImageFile AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cInstagram AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFacebook AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iTblCols          AS INTEGER EXTENT 6  NO-UNDO.
DEFINE VARIABLE cTblColLabels     AS CHARACTER EXTENT 6  NO-UNDO.
DEFINE VARIABLE cOrdreNr   AS CHARACTER   NO-UNDO.
/* DEFINE VARIABLE dOrdreNr   AS DECIMAL   NO-UNDO. */
DEFINE VARIABLE dDato      AS DATE      NO-UNDO.
DEFINE VARIABLE cKundRad_1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKundRad_2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKundRad_3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKundRad_4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKundRad_5 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKundRad_6 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLevRad_1  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLevRad_2  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLevRad_3  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLevRad_4  AS CHARACTER NO-UNDO.

DEFINE VARIABLE cRightFtxt1 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRightFtxt2 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRightFtxt3 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBarcode    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dTst AS DECIMAL     NO-UNDO.

DEFINE TEMP-TABLE tt_Artbas NO-UNDO LIKE Artbas.
DEFINE VARIABLE cMKlubbId AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cJohanssonMKlubbNR AS CHAR     NO-UNDO.
{ pdf_inc.i "THIS-PROCEDURE"}
/* { pdf_inc.i "NOT SUPER"} */
DEFINE VARIABLE cKO_LFtxt AS CHARACTER EXTENT 4  NO-UNDO.
DEFINE VARIABLE cKO_RFtxt AS CHARACTER EXTENT 2  NO-UNDO.
DEFINE VARIABLE iLMp2 AS INTEGER     NO-UNDO.
  

  ASSIGN iCols[1] = 0
         iCols[2] = 39
         iCols[3] = 110
         iCols[4] = 231
         iCols[5] = 260
         iCols[6] = 295
         iCols[7] = 350.

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
  ( INPUT cText AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EAN13BC) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EAN13BC Procedure 
FUNCTION EAN13BC RETURNS CHARACTER
  ( INPUT icStrekKode AS CHAR )  FORWARD.

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
         HEIGHT             = 27
         WIDTH              = 69.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* RUN ByggTT. */

/* IF CAN-FIND(FIRST tt_artbas) THEN DO: */
/*     {syspara.i 5 1 1 iCL INT}         */
/*     {syspara.i 1 1 100 cFirmaNavn}    */
{syspara.i 19 13 2 cImageFile}
cInstagram = ".\icon\instagram-logo_1.jpg".
cFacebook  = ".\icon\facebook-logo_1.jpg".
IF SEARCH(cInstagram) = ? THEN
    cInstagram = "".

dKOrdreID = DECI(cKOrdreID) NO-ERROR.    
IF ERROR-STATUS:ERROR THEN
    RETURN.
FIND KOrdreHode WHERE KOrdreHode.KOrdre_Id = dKOrdreID NO-LOCK NO-ERROR.
IF NOT AVAIL KOrdreHode THEN
    RETURN.
/* IF KOrdreHode.Butikknr = 25 THEN   */
/*     {syspar2.i 19 13 2 cImageFile} */
RUN getMKlubb (OUTPUT cMKlubbId).
FIND kunde WHERE kunde.kundenr = KOrdrehode.kundenr NO-LOCK NO-ERROR.
IF cMKlubbId <> "" THEN DO:
    FIND FIRST medlem WHERE medlem.kundenr = KOrdrehode.kundenr NO-LOCK NO-ERROR.
    IF AVAIL medlem AND STRING(Medlem.MKlubbId) = cMKlubbId THEN
        cJohanssonMKlubbNR = STRING(Medlem.Medlemsnr).
END.
/* från ordre */
IF LENGTH(TRIM(KOrdreHode.Embalage)) = 22 THEN DO:
    dTst = DECI(KOrdreHode.Embalage) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
        cBarcode  = TRIM(KOrdreHode.Embalage).
END.
dDato    = KOrdreHode.BetaltDato.
cOrdreNr = KOrdreHode.EkstOrdreNr.
/* Hæmta kund o butik */
cKundRad_1 = KOrdreHode.Navn.
cKundRad_2 = KOrdreHode.LevAdresse1.
cKundRad_3 = KOrdreHode.LevPostNr  + " " + KOrdreHode.LevPostSted.
cKundRad_4 = "".
cKundRad_5 = REPLACE(Kordrehode.MobilTlf," ","").
cKundRad_6 = TRIM(Kordrehode.ePostAdresse).
/* cKundRad_5 = IF AVAIL kunde THEN TRIM(Kunde.MobilTlf) ELSE "".     */
/* cKundRad_6 = IF AVAIL kunde THEN TRIM(Kunde.ePostAdresse) ELSE "". */

/*                                   cKundRad_1 = "Kenneth Olausson".  KOrdreHode.Navn                                      */
/*                                   cKundRad_2 = "Tuterudkroken 16".  KOrdreHode.LevAdresse1                               */
/*                                   cKundRad_3 = "2007 Kjeller".      KOrdreHode.LevPostNr  + " " + KOrdreHode.LevPostSted */
/*                                   cKundRad_4 = "".                                                                       */
/* IF KOrdrehode.butikknr = 24 THEN DO: */
DO:
    cLevRad_1  = "AB Aug. Johansson Skomodehus".
    cLevRad_2  = "Vaksalagatan 3".
    cLevRad_3  = "753 20 Uppsala".
    cLevRad_4  = "".
    cRightFtxt1 = "Om du önskar att returnera varor från din order".
    cRightFtxt2 = "river du av och fyller i 'Returschema' på vänster sida.".
    cRightFtxt3 = "AB Aug. Johansson Skomodehus, Vaksalagatan 3, 753 20 Uppsala - Org.nr: 556210-4983".
END.
/* ELSE IF KOrdrehode.butikknr = 25 THEN DO:                                                                           */
/*     cLevRad_1  = "Jedviks/JF Johanssons AB".                                                                        */
/*     cLevRad_2  = "Hulda Mellgrens Gata 5".                                                                          */
/*     cLevRad_3  = "421 32 Västra Frölunda".                                                                          */
/*     cLevRad_4  = "".                                                                                                */
/*     cRightFtxt1 = "Om du önskar att returnera varor från din order".                                                */
/*     cRightFtxt2 = "river du av och fyller i 'Returschema' på vänster sida.".                                        */
/*     cRightFtxt3 = "Jedviks/JF Johanssons AB, Hulda Mellgrens Gata 5, 421 32 Västra Frölunda - Org.nr: 556027-7963". */
/* END.                                                                                                                */


RUN initFooterTxt.

    RUN RapportPDF.
    ASSIGN 
      ocReturn = cFilnavn
      obOk     = TRUE
    .
IF VALID-HANDLE(h_PDFinc) THEN DO:
        DELETE PROCEDURE h_PDFinc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ColLabels) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ColLabels Procedure 
PROCEDURE ColLabels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   DEFINE VARIABLE iX AS INTEGER     NO-UNDO.        */
/*   MESSAGE pdf_PointSize ("Spdf")                    */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.            */
/*   RUN pdf_text_at ("Spdf","",iCols[1]). */
/*   iX = pdf_TextX("Spdf").                           */
/* DEFINE VARIABLE dY AS INTEGER     NO-UNDO. */
DEFINE INPUT  PARAMETER dY AS INTEGER     NO-UNDO.

/*  iPageHeight = 612. 
    iPageWidth  = 842 */
/*   dY = iStartRow. */

  ASSIGN cColLabels[1]  = "Artnr" 
         cColLabels[2]  = "Färg"
         cColLabels[3]  = "Varumärke"
         cColLabels[4]  = "Str"
         cColLabels[5]  = "Ant"
         cColLabels[6] = "Rabatt"
         cColLabels[7] = "Pris".

  DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
  RUN pdf_set_font ("Spdf", "GantModern-Bold",7).
    RUN pdf_skip    ("Spdf").
    DO ii = 1 TO EXTENT(iCols):
/*         RUN pdf_text_xy_dec ("Spdf",cColLabels[ii],iLMp2 + iCols[ii],dY - 142). */
        RUN pdf_text_xy_dec ("Spdf",cColLabels[ii],iLMp2 + iCols[ii],dY).
    END.
  
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DataEndRight) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DataEndRight Procedure 
PROCEDURE DataEndRight :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER dY AS INTEGER     NO-UNDO.

   DEFINE VARIABLE cLevmetodeLbl   AS CHARACTER INIT "Leveringssätt"  NO-UNDO.
   DEFINE VARIABLE cRabattkodeLbl  AS CHARACTER INIT "Rabattkod"   NO-UNDO.
   DEFINE VARIABLE cBruttoLbl       AS CHARACTER INIT "Brutto"       NO-UNDO.
   DEFINE VARIABLE cRabattLbl      AS CHARACTER INIT "Rabatt"      NO-UNDO.
   DEFINE VARIABLE cMomsLbl      AS CHARACTER INIT   "Varav moms"  NO-UNDO.
   DEFINE VARIABLE cFraktLbl       AS CHARACTER INIT "Frakt"       NO-UNDO.
   DEFINE VARIABLE cSumLbl       AS CHARACTER INIT "Summa"       NO-UNDO.
   DEFINE VARIABLE cLevmetode      AS CHARACTER     NO-UNDO.
   DEFINE VARIABLE cRabattkode     AS CHARACTER     NO-UNDO.
   DEFINE VARIABLE dNetto          AS DECIMAL NO-UNDO.
   DEFINE VARIABLE dRabatt         AS DECIMAL NO-UNDO.
   DEFINE VARIABLE dBrutto         AS DECIMAL NO-UNDO.
   DEFINE VARIABLE dMoms         AS DECIMAL NO-UNDO.
   DEFINE VARIABLE dFrakt          AS DECIMAL NO-UNDO.
   DEFINE VARIABLE dSum          AS DECIMAL NO-UNDO.

   FIND LeveringsForm WHERE LeveringsForm.LevFNr = KOrdreHode.LevFNr NO-LOCK NO-ERROR.
   IF AVAIL LeveringsForm THEN 
       cLevmetode = LeveringsForm.LevFormBeskrivelse.
/*    FIND FIRST Kordrelinje OF KOrdrehode WHERE KOrdreLinje.Varetekst = "FRAKT" NO-LOCK NO-ERROR. */
/*    IF AVAIL Kordrelinje THEN                                                                    */
/*        dFrakt = Kordrelinje.Nettolinjesum.                                                      */
/*    dRabatt = KOrdreHode.TotalRabattKr.                                                          */
/*    dTotal  = KOrdreHode.Totalt.                                                                 */
/*    dNetto  = KOrdreHode.Totalt - KOrdreHode.TotalRabattKr.                                      */

   RUN GetDataEndRight (OUTPUT dBrutto,OUTPUT dRabatt,OUTPUT dMoms,OUTPUT dFrakt,OUTPUT dNetto).
/*    MESSAGE "dBrutto" dBrutto SKIP         */
/*            "dRabatt" dRabatt SKIP         */
/*            "dMoms  " dMoms   SKIP         */
/*            "dFrakt " dFrakt  SKIP         */
/*            "dNetto " dNetto               */
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK. */
   dSum = dFrakt + dNetto.

/*        dSum = dFrakt + dNetto.            */
/*        MESSAGE "dBrutto" dBrutto skip     */
/*                "dRabatt" dRabatt skip     */
/*                "dMoms  " dMoms   skip     */
/*                "dFrakt " dFrakt  skip     */
/*                "dNetto " dNetto  skip     */
/*                "dsum   " dsum             */
/*                                           */
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*                                           */

      RUN pdf_set_font ("Spdf", "GantModern-Bold",6).
/*       RUN pdf_text_xy_dec ("Spdf",cLevmetodeLbl,iLMp2,dY). */
      RUN pdf_text_xy_dec ("Spdf",cBruttoLbl,730,dY).
/*       RUN pdf_text_xy_dec ("Spdf",cRabattkodeLbl,iLMp2,dY - 12). */
      RUN pdf_text_xy_dec ("Spdf",cRabattLbl,730,dY - 12).
      RUN pdf_text_xy_dec ("Spdf",cFraktLbl,730,dY - 24).
      RUN pdf_set_font ("Spdf", "GantModern-Bold",8).
      RUN pdf_text_xy_dec ("Spdf",cSumLbl,730,dY - 36).
      RUN pdf_set_font ("Spdf", "GantModern-Bold",6).
      RUN pdf_text_xy_dec ("Spdf",cMomsLbl,730,dY - 48).
      RUN pdf_set_font ("Spdf", "GantModern-Regular",8).
      RUN pdf_text_xy_dec ("Spdf",cLevmetode,iLMp2 + 80,dY).
      
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dBrutto,"->>,>>9.99")),iPageWidth - iLeftMargin - bredd(TRIM(STRING(dBrutto,"->>,>>9.99"))),dY).
/*       RUN pdf_text_xy_dec ("Spdf",cRabattkode,iLMp2 + 80,dY - 12). */
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dRabatt,"->>,>>9.99")),iPageWidth - iLeftMargin - bredd(TRIM(STRING(dRabatt,"->>,>>9.99"))),dY - 12).
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dFrakt,"->>,>>9.99")),iPageWidth - iLeftMargin - bredd(TRIM(STRING(dFrakt,"->>,>>9.99"))),dY - 24).
      RUN pdf_set_font ("Spdf", "GantModern-Bold",8).
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dSum,"->>,>>9.99")),iPageWidth - iLeftMargin - bredd(TRIM(STRING(dSum,"->>,>>9.99"))),dY - 36).
      RUN pdf_set_font ("Spdf", "GantModern-Regular",8).
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dMoms,"->>,>>9.99")),iPageWidth - iLeftMargin - bredd(TRIM(STRING(dMoms,"->>,>>9.99"))),dY - 48).
/* 
      RUN pdf_set_font ("Spdf", "GantModern-Regular",7).

      RUN pdf_text_xy_dec ("Spdf",KOrdreLinje.VareNr,iLMp2 + iCols[1],dY).
      RUN pdf_text_xy_dec ("Spdf",cFarge,iLMp2 + iCols[2],dY).
      RUN pdf_text_xy_dec ("Spdf",KOrdreLinje.Varetekst,iLMp2 + iCols[3],dY).
      RUN pdf_text_xy_dec ("Spdf",KOrdreLinje.Storl,iLMp2 + iCols[4] + 5 - bredd(KOrdreLinje.Storl) / 2,dY).
      RUN pdf_text_xy_dec ("Spdf",KOrdreLinje.Antall,iLMp2 + iCols[5] + 13 - bredd(STRING(KOrdreLinje.Antall)),dY).
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(KOrdreLinje.LinjeRabattKr,"->>,>>9.99")),iLMp2 + iCols[6] + bredd(cColLabels[6]) - bredd(TRIM(STRING(KOrdreLinje.LinjeRabattKr,"->>,>>9.99"))),dY).
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(KOrdreLinje.NettoLinjesum,"->>,>>9.99")),iPageWidth - iLeftmargin - bredd(TRIM(STRING(KOrdreLinje.NettoLinjesum,"->>,>>9.99"))),dY).
 
 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetDataEndRight) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetDataEndRight Procedure 
PROCEDURE GetDataEndRight :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE OUTPUT PARAMETER dBrutto AS DECIMAL     NO-UNDO.
   DEFINE OUTPUT PARAMETER dRabatt AS DECIMAL     NO-UNDO.
   DEFINE OUTPUT PARAMETER dMoms   AS DECIMAL     NO-UNDO.
   DEFINE OUTPUT PARAMETER dFrakt  AS DECIMAL     NO-UNDO.
   DEFINE OUTPUT PARAMETER dNetto  AS DECIMAL     NO-UNDO.
   
   FOR EACH bufKOL OF KOrdrehode NO-LOCK:
       IF bufKOL.antall = 0 OR bufKOL.Varenr MATCHES "*BETALT*" THEN
           NEXT.
       IF bufKOL.Varetekst MATCHES "*FRAKT*" THEN
           ASSIGN dFrakt  = dFrakt  + bufKOL.nettolinjesum
                  dMoms   = dMoms   + bufKOL.MVaKr.
       ELSE
           ASSIGN dBrutto = dBrutto + bufKOL.antall * bufKOL.bruttopris
                  dRabatt = dRabatt + (bufKOL.Linjerabatt * -1)
                  dMoms   = dMoms   + (bufKOL.MVaKr * (IF bufKOL.antall < 1 THEN -1 ELSE 1))
                  dNetto  = dNetto  + (bufKOL.nettolinjesum * (IF bufKOL.antall < 1 THEN -1 ELSE 1)).
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMKlubb) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getMKlubb Procedure 
PROCEDURE getMKlubb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER cMKlubbId AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.

DO ii = 31 TO 39:
    FIND syspara WHERE SysPara.SysHId = 14 AND
                       SysPara.SysGr  =  1 AND
                       SysPara.ParaNr = ii AND 
                       SysPara.parameter1 <> "" NO-LOCK NO-ERROR.
    IF AVAIL syspara AND CAN-DO(SysPara.parameter1,STRING(KOrdrehode.butikknr)) THEN DO:
        cMKlubbId = TRIM(syspara.parameter2).
        LEAVE.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initFooterTxt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initFooterTxt Procedure 
PROCEDURE initFooterTxt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    cKO_LFtxt[1] = "Om du inte fyller i formuläret korrekt kan din returbehandling ta längre tid.".
    cKO_LFtxt[2] = "Retur kan ske inom 14 dagar från mottagsdatumet.".
    cKO_LFtxt[3] = "Det tillkommer en fraktskostnad om du vill göra retur med vår returfraktssedel.".
    cKO_LFtxt[4] = "Se Allmänna villkor/Vanliga frågor på våran hemsida, www.skoaugust.com.".
    cKO_RFtxt[1] = "Tack för att du valt att handla hos oss!".
    cKO_RFtxt[2] = "Osäker hur du skall vårda dina skor? Gå in och läs på vår hemsida under fliken skötsel.".

/*     RUN pdf_text_xy_dec ("Spdf","Tack för att du valt att handla hos oss!",iLeftmargin,d2 - 42).                                                */
/*     RUN pdf_text_xy_dec ("Spdf","Osäker hur du skall vårda dina skor? Gå in och läs på vår hemsida under fliken skovård.",iLeftmargin,d2 - 52). */
/*     IF cInstagram <> "" THEN DO:                                                                                                                */
/*         RUN pdf_text_xy_dec ("Spdf","Följ oss på",iLeftmargin,d2 - 65).                                                                         */
/*         RUN pdf_place_image2 IN h_PDFinc ("Spdf",                                                                                               */
/*                                          "INSTAGRAM",                                                                                           */
/*                                          iLeftMargin,                                                                                           */
/*                                          478,                                                                                                   */
/*                                          pdf_ImageDim ("Spdf","HEADERLOGO","WIDTH") * .20,                                                      */
/*                                          pdf_ImageDim ("Spdf","HEADERLOGO","HEIGHT") * .20).                                                    */
/*         RUN pdf_set_font ("Spdf", "GantModern-Bold",8).                                                                                         */
/*         RUN pdf_text_xy_dec ("Spdf","@JOHANSSONSSKOR",88,d2 - 79).                                                                              */
/*     END.                                                                                                                                        */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LeftFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeftFooter Procedure 
PROCEDURE LeftFooter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE dOrdreNr AS DECIMAL INIT 30000583599    NO-UNDO.
DEFINE VARIABLE dDato    AS DATE  INIT TODAY      NO-UNDO.
DEFINE VARIABLE dY AS INTEGER     NO-UNDO.
DEFINE VARIABLE d2 AS INTEGER     NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DEFINE VARIABLE i2 AS INTEGER     NO-UNDO.
DEFINE VARIABLE iColPos     AS INTEGER   EXTENT 4  NO-UNDO.
DEFINE VARIABLE cOrsaksKod  AS CHARACTER EXTENT 5  NO-UNDO.
DEFINE VARIABLE cOrsaksTxt  AS CHARACTER EXTENT 5  NO-UNDO.
DEFINE VARIABLE cTxt        AS CHARACTER   NO-UNDO.
  ASSIGN iColPos[1] = 0
         iColPos[2] = 80
         iColPos[3] = 170
         iColPos[4] = 265.
  ASSIGN cOrsaksKod[1] = "10,20,30,40"
         cOrsaksKod[2] = "11,21,31,41"
         cOrsaksKod[3] = "12,22,,42"
         cOrsaksKod[4] = ",,,43"
/*          cOrsaksKod[4] = "13,,,43" */
         cOrsaksKod[5] = ",,,44".
  ASSIGN cOrsaksTxt[1] = "För liten,Fel vara levererad,Fel på varan,Färg inte som förväntad"
         cOrsaksTxt[2] = "För stor,Fel storlek levererad,Avvik från beskrivning,Materialet var inte som förväntat"
         cOrsaksTxt[3] = "Fel passform,För sent levererad,,Matchade inte"
         cOrsaksTxt[4] = ",,,Ändrat mening"

/*          cOrsaksTxt[4] = "För kort,,,Ändrat mening" */
         cOrsaksTxt[5] = ",,,Gåva som inte passade".

/*  iPageHeight = 612. 
    iPageWidth  = 842 */

  dY = iStartRow. /* 584 */
  RUN pdf_set_font ("Spdf", "Helvetica-Bold",24).


  DO:
    RUN pdf_set_font ("Spdf", "GantModern-Bold",7).
/*     d2 = 190. */


    d2 = 175.
    RUN pdf_text_xy_dec ("Spdf","OBS!",iLeftmargin,d2 - 10).
    RUN pdf_set_font ("Spdf", "GantModern-Regular",8).
    RUN pdf_text_xy_dec ("Spdf",cKO_LFtxt[1],iLeftmargin,d2 - 20).
    RUN pdf_text_xy_dec ("Spdf",cKO_LFtxt[2],iLeftmargin,d2 - 30).
    RUN pdf_text_xy_dec ("Spdf",cKO_LFtxt[3],iLeftmargin,d2 - 40).
    RUN pdf_text_xy_dec ("Spdf",cKO_LFtxt[4],iLeftmargin,d2 - 50).
/*     RUN pdf_text_xy_dec ("Spdf","Varor som är förseglade kan inte returneras om förseglingen är bruten",iLeftmargin,d2 - 30). */
    
    
    d2 = 100.
    RUN pdf_line ("Spdf",iLeftMargin,d2 + 15,pdf_PageWidth("Spdf") / 2 - iLeftmargin,d2 + 15 ,0.5).
    RUN pdf_set_font ("Spdf", "GantModern-Bold",8).
    RUN pdf_text_xy_dec ("Spdf","ORSAKSNUMMER",iLeftmargin,d2).
    RUN pdf_set_font ("Spdf", "GantModern-Regular",8).
    RUN pdf_text_xy_dec ("Spdf","STORLEK",iLeftmargin + iColPos[1],d2 - 13).
    RUN pdf_text_xy_dec ("Spdf","SERVICE",iLeftmargin + iColPos[2],d2 - 13).
    RUN pdf_text_xy_dec ("Spdf","KVALITET",iLeftmargin + iColPos[3],d2 - 13).
    RUN pdf_text_xy_dec ("Spdf","ANNAT",iLeftmargin + iColPos[4],d2 - 13).

    DO:
        d2 = 85.
        RUN pdf_set_font ("Spdf", "GantModern-Bold",7).
        DO ii = 1 TO 5:
            DO i2 = 1 TO 4:
                RUN pdf_text_xy_dec ("Spdf",ENTRY(i2,cOrsaksKod[ii]),iLeftmargin + iColPos[i2],d2 - (ii * 10)).
            END.
        END.
        d2 = 85.
        RUN pdf_set_font ("Spdf", "GantModern-Regular",7).
        DO ii = 1 TO 5:
            DO i2 = 1 TO 4:
                RUN pdf_text_xy_dec ("Spdf",ENTRY(i2,cOrsaksTxt[ii]),iLeftmargin + iColPos[i2] + 11,d2 - (ii * 10)).
            END.
        END.
        RUN pdf_line ("Spdf",iLeftMargin,23,pdf_PageWidth("Spdf") / 2 - iLeftmargin,23,0.5).
/*         RUN pdf_set_font ("Spdf", "GantModern-Bold",9).                                        */
/*         cTxt = "Dersom du ønsker å bytte eller returnere varer på din ordre".                  */
/*         RUN pdf_text_xy_dec ("Spdf",cTxt,iLMp2 - iLeftMargin + iMittenR - bredd(cTxt) / 2,50). */
/*         cTxt = "river du av og fyller ut 'Bytte- og Returskjema' på siste side.".              */
/*         RUN pdf_text_xy_dec ("Spdf",cTxt,iLMp2 - iLeftMargin + iMittenR - bredd(cTxt) / 2,37). */
/*         RUN pdf_set_font ("Spdf", "GantModern-Regular",7).                                     */
/*         cTxt = "GANT Retail AS, Nesset 5, 3470 Slemmestad - Org.nr: 985 383 383".              */
/*         RUN pdf_text_xy_dec ("Spdf",cTxt,iLMp2 - iLeftMargin + iMittenR - bredd(cTxt) / 2,22). */
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LeftHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeftHeader Procedure 
PROCEDURE LeftHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE dY AS INTEGER     NO-UNDO.
DEFINE VARIABLE d2 AS INTEGER     NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DEFINE VARIABLE i2 AS INTEGER     NO-UNDO.
DEFINE VARIABLE iColPos     AS INTEGER   EXTENT 4  NO-UNDO.
DEFINE VARIABLE cOrsaksKod  AS CHARACTER EXTENT 5  NO-UNDO.
DEFINE VARIABLE cOrsaksTxt  AS CHARACTER EXTENT 5  NO-UNDO.
DEFINE VARIABLE cBarcode AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTxt        AS CHARACTER   NO-UNDO.
/*  iPageHeight = 612. 
    iPageWidth  = 842 */

  dY = iStartRow. /* 584 */
  RUN pdf_set_font ("Spdf", "Helvetica-Bold",24).
  DO:
      RUN pdf_place_image2 IN h_PDFinc ("Spdf",
                                       "HEADERLOGO",
                                       iLeftMargin, 
                                       43,
                                       pdf_ImageDim ("Spdf","HEADERLOGO","WIDTH") * .50,
                                       pdf_ImageDim ("Spdf","HEADERLOGO","HEIGHT") * .50).
    RUN pdf_set_font ("Spdf", "GantModern-Bold",9).
    RUN pdf_text_xy_dec ("Spdf","RETURSCHEMA",iLeftMargin,dY - 24).

    RUN pdf_set_font ("Spdf", "GantModern-Medium",9).
    RUN pdf_text_xy_dec ("Spdf",cLevRad_1,iLeftMargin,dY - 35).
    RUN pdf_text_xy_dec ("Spdf",cLevRad_2,iLeftMargin,dY - 45).
    RUN pdf_text_xy_dec ("Spdf",cLevRad_3,iLeftMargin,dY - 55).
    RUN pdf_text_xy_dec ("Spdf",cLevRad_4,iLeftMargin,dY - 65).

    RUN pdf_set_font ("Spdf", "GantModern-Regular",9).
    RUN pdf_text_xy_dec ("Spdf","Ordernummer:",260,dY).
    RUN pdf_text_xy_dec ("Spdf","Orderdatum:",260,dY - 12).
    RUN pdf_set_font ("Spdf", "GantModern-Bold",9).
    RUN pdf_text_xy_dec ("Spdf",TRIM(cOrdreNr),iRMarginPos - 420 - bredd(TRIM(cOrdreNr)),dY).
    RUN pdf_text_xy_dec ("Spdf",STRING(dDato,"99.99.99"),iRMarginPos - 420 - bredd(STRING(dDato,"99.99.99")),dY - 12).

    DO:
      RUN  pdf_set_font ("Spdf","Code39",16.0).
      RUN pdf_set_parameter("Spdf","ScaleY","2.0").
      cBarcode = "*" + "KO" + cKOrdreID + "*".
      RUN  pdf_text_xy ("Spdf",cbarcode,iRMarginPos - 470 - bredd(cKordreID), dY - 45).
/*       RUN  pdf_text_xy ("Spdf","*" + "KO" + cKOrdreID + "*",iRMarginPos - 480 - bredd(cKOrdreID), dY - 45). */
/*       RUN  pdf_text_xy ("Spdf",cBarcode,iRMarginPos - 420 - bredd(cBarCode), dY - 40). */
      RUN pdf_set_parameter("Spdf","ScaleY","1").
      RUN pdf_set_font ("Spdf", "GantModern-Regular",9).
      RUN  pdf_text_xy ("Spdf",cbarcode,iRMarginPos - 470 - bredd(cbarcode), dY - 55).
    END.
/*     IF cJohanssonMKlubbNR <> "" THEN DO:                                                                                                  */
/*         cJohanssonMKlubbNR = (IF KOrdreHode.butikknr = 24 THEN "Johanssons Medlemsnr: " ELSE "Jedviks Medlemsnr: ") + cJohanssonMKlubbNR. */
/*         RUN pdf_set_font ("Spdf", "GantModern-Regular",9).                                                                                */
/*         RUN pdf_text_xy_dec ("Spdf",cJohanssonMKlubbNR,iRMarginPos - 420 - bredd(cJohanssonMKlubbNR),dY - 70).                            */
/*     END.                                                                                                                                  */

    RUN pdf_line ("Spdf",iLeftMargin,dY - 75,pdf_PageWidth("Spdf") / 2 - iLeftmargin,dy - 75 ,0.5).
    RUN pdf_set_font ("Spdf", "GantModern-Bold",9).
    RUN pdf_text_xy_dec ("Spdf","Jag önskar att returnera följande",iLeftmargin,dY - 93).
    RUN pdf_set_font ("Spdf", "GantModern-Regular",9).
    RUN pdf_text_xy_dec ("Spdf","Returschema rivs av och läggs med din retur.",iLeftmargin,dY - 106).
/*     KOrdreHode.Embalage = B_ID */
/*     DO ii = 10 TO 410 BY 20:                                                            */
/*         RUN pdf_text_xy_dec ("Spdf",STRING(iLeftmargin + ii),iLeftmargin + ii,d2 - 20). */
/*     END.                                                                                */
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-new_page) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new_page Procedure 
PROCEDURE new_page :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN pdf_new_page ("Spdf").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PageFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PageFooter Procedure 
PROCEDURE PageFooter :
/*------------------------------------------------------------------------------
  Purpose:  Procedure to Print Page Footer -- on all pages.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Display a Sample Watermark on every page */
/*   RUN pdf_watermark ("Spdf","Reservasjon Nettbutikk","Courier-Bold",34,.87,.87,.87,80,500). */

  RUN pdf_skip ("Spdf").
  RUN pdf_set_dash ("Spdf",1,0).
  RUN pdf_line  ("Spdf", 20, pdf_BottomMargin ("Spdf") - 4, pdf_PageWidth("Spdf") - 20 , pdf_BottomMargin ("Spdf") - 4, 1).
/*   RUN pdf_line  ("Spdf", 0, pdf_TextY("Spdf") - 5, pdf_PageWidth("Spdf") - 20 , pdf_TextY("Spdf") - 5, 1). */
  RUN pdf_skip ("Spdf").
  RUN pdf_skip ("Spdf").

  RUN pdf_text_to ("Spdf",TODAY,15).
  RUN pdf_text_to  ("Spdf", cFirmanavn, 60).

  RUN pdf_text_to  ("Spdf",  "Side: "
                           + STRING(pdf_page("Spdf"))
                           + " (" + pdf_TotalPages("Spdf") + ")", 135).

/*   vlines = 1. /* Restart our count for the linking */ */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RapportPDF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportPDF Procedure 
PROCEDURE RapportPDF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
Font      Storlek Sidlayout Första TO_Vänsterjusterat Sista_TO Sista_AT
Helvetika      10 Landscape      6                    121      285
               11                6                    110      259
               12                5                    100      237
               10 Portrait       6                     82      192
               11                6                     75      174
               12                5                     68      160
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cOrdrenr AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iAntall AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iColLabelPage AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cLoadedFont AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iWhat  AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iWidth AS INTEGER     NO-UNDO.
  DEFINE VARIABLE dDato AS DATE    NO-UNDO.
  DEFINE VARIABLE cTxt AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dYR AS INTEGER     NO-UNDO.
  DEFINE VARIABLE dYL AS INTEGER     NO-UNDO.
  DEFINE VARIABLE d2 AS INTEGER     NO-UNDO.
/*   cFilNavn = SESSION:TEMP-DIR + "Leveranseseddel" + "_" + STRING(1) + ".pdf". */

    /* skapa ett utlägg pr butik */
  RUN pdf_new ("Spdf",cFilNavn).
  RUN pdf_set_PaperType ("Spdf","A4").
  RUN pdf_set_Orientation ("Spdf","Landscape").
  
  iLeftMargin = 25.
  iPageHeight = pdf_PageHeight ("Spdf").
  iPageWidth  = pdf_PageWidth ("Spdf").
  iRMarginPos =  iPageWidth - iLeftMargin.
  iMittenR = iPageWidth / 2 + iPageWidth / 4.
  iStartRow = iPageHeight - 28.
  RUN pdf_set_BottomMargin ("Spdf", 60).
  iLMp2 = iPageWidth / 2 + iLeftMargin.
/*   RUN pdf_text_xy_dec ("Spdf","XXXX",iLMp2,100). */
  
/*   pdf_PageHeader ("Spdf",THIS-PROCEDURE:HANDLE,"PageHeader"). */
/*   pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PageFooter"). */
  
  RUN pdf_set_VerticalSpace ("Spdf",13).
/*   RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13). */
  /* Ladda Image */
  
  RUN pdf_load_image IN h_PDFinc ("Spdf","HEADERLOGO",cImageFile).
  IF cInstagram <> "" THEN
      RUN pdf_load_image IN h_PDFinc ("Spdf","INSTAGRAM",cInstagram).
  IF cFacebook <> "" THEN
      RUN pdf_load_image IN h_PDFinc ("Spdf","FACEBOOK",cFacebook).

  /*   RUN LoadFonts. */
  RUN pdf_load_font ("Spdf","GantModern-Regular","pdfinclude\GantModern-Regular.TTF","pdfinclude\GantModern-Regular.AFM","").
  RUN pdf_load_font ("Spdf","GantModern-Medium","pdfinclude\GantModern-Medium.TTF","pdfinclude\GantModern-Medium.AFM","").
  RUN pdf_load_font ("Spdf","GantModern-Bold","pdfinclude\GantModern-Bold.TTF","pdfinclude\GantModern-Bold.AFM","").
  RUN pdf_load_font ("Spdf","Wingding","pdfinclude\Wingding.TTF","pdfinclude\Wingding.AFM","").
  cBarCode = "1234567891234567890123".

  IF cBarcode <> "" THEN
      RUN pdf_load_font IN h_PDFinc ("Spdf","Code39",".\PDFinclude\samples\support\code39.ttf",".\PDFinclude\samples\support\code39.afm",""). 

  RUN new_page.
  iColLabelPage = 1.

iColLabelPage = 1.
  DO:
/*       RUN Refline. */

/*       RUN PageHeader.  */
/*       RUN ColLabels. */
/*       dYR = iStartRow - 140. */
      dYL = 438.
      dYR = 438.
      RUN Splitter.
      RUN RightHeader.
      RUN RightFooter.
      RUN LeftHeader.
      RUN ReturTabell(dYL).

      RUN LeftFooter.
      RUN ColLabels (dYR).
      FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK:
          IF KOrdreLinje.VareNr = "BETALT" THEN
              NEXT.
          dYR = dYR - 15.
          RUN SkrivDataRight (dYR).
          IF KOrdreLinje.Varetekst MATCHES "*FRAKT*" THEN
              NEXT.

          IF KOrdreLinje.Nettolinjesum > 0 THEN DO:
              DO ii = 1 TO KOrdreLinje.Antall:
                  dYL = dYL - 15.
                  RUN SkrivDataLeft (dYL).
              END.
          END.
      END.
      RUN VertLines (436,dYL - 5).
      RUN DataEndRight(dYR - 30).
END.
 RUN pdf_close ("Spdf").
/*  RUN SendEmail IN THIS-PROCEDURE. */
/*  RUN browse2pdf\viewxmldialog.w (cFilNavn,"PDF Template"). */
  /* Sender filen til visning og utskrift. */
/*   RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Refline) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Refline Procedure 
PROCEDURE Refline :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE d2 AS INTEGER     NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
   d2 = iStartRow.
  RUN pdf_set_font ("Spdf", "GantModern-Regular",8).
   DO ii = d2 TO 10 BY -10:
       
       RUN pdf_text_xy_dec ("Spdf",STRING(ii),ileftmargin - 10,ii).
       RUN pdf_text_xy_dec ("Spdf",STRING(ii),245,ii).

       RUN pdf_text_xy_dec ("Spdf",STRING(ii),iLMp2 - 18,ii).
       RUN pdf_text_xy_dec ("Spdf",STRING(ii),665,ii).

   END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReturTabell) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReturTabell Procedure 
PROCEDURE ReturTabell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER dY AS INTEGER     NO-UNDO.

/*   ASSIGN iCols[1] = 0    */
/*          iCols[2] = 39   */
/*          iCols[3] = 110  */
/*          iCols[4] = 231  */
/*          iCols[5] = 260  */
/*          iCols[6] = 295  */
/*          iCols[7] = 350. */

    ASSIGN iTblCols[1] = 2
           iTblCols[2] = 44
           iTblCols[3] = 115
           iTblCols[4] = 230
           iTblCols[5] = 275
           iTblCols[6] = 339
           cTblColLabels [1] = "Artnr"
           cTblColLabels [2] = "Färg"
           cTblColLabels [3] = "Varumärke"
           cTblColLabels [4] = "Str"
           cTblColLabels [5] = "Retur (Kryssa av)"
           cTblColLabels [6] = "Orsaknr".

      RUN pdf_set_font ("Spdf", "GantModern-Regular",7).
      RUN pdf_rect ("Spdf", iLeftMargin,dY - 5,(iPageWidth / 2) - iLeftmargin - iLeftMargin,16,0.5).
      RUN pdf_text_color  ("Spdf",1.0,1.0,1.0).
      RUN pdf_text_xy_dec ("Spdf",cTblColLabels [1],iLeftmargin + iTblCols[1],dY).
      RUN pdf_text_xy_dec ("Spdf",cTblColLabels [2],iLeftmargin + iTblCols[2],dY).
      RUN pdf_text_xy_dec ("Spdf",cTblColLabels [3],iLeftmargin + iTblCols[3],dY).
      RUN pdf_text_xy_dec ("Spdf",cTblColLabels [4],iLeftmargin + iTblCols[4],dY).
      RUN pdf_text_xy_dec ("Spdf",cTblColLabels [5],iLeftmargin + iTblCols[5],dY).
      RUN pdf_text_xy_dec ("Spdf",cTblColLabels [6],iLeftmargin + iTblCols[6],dY).
      RUN pdf_text_color  ("Spdf",0.0,0.0,0.0).
      
/*       RUN pdf_text_xy_dec ("Spdf","SOLID LAMBSWOOL SCARF",iLeftmargin,dY - 100). */

/*       RUN pdf_text_xy_dec ("Spdf","91470",iLMp2 + iCols[1],dY - 170).                                          */
/*       RUN pdf_text_xy_dec ("Spdf","91",iLMp2 + iCols[2],dY - 170).                                             */
/*       RUN pdf_text_xy_dec ("Spdf","SOLID LAMBSWOOL SCARF",iLMp2 + iCols[3],dY - 170).                          */
/*       RUN pdf_text_xy_dec ("Spdf","ONSESIZE",iLMp2 + iCols[4],dY - 170).                                       */
/*       RUN pdf_text_xy_dec ("Spdf","1",iLMp2 + iCols[5] + 10 - bredd("1"),dY - 170).                            */
/*       RUN pdf_text_xy_dec ("Spdf","0,00",iLMp2 + iCols[6] + bredd("Rabatt") - bredd("0,00"),dY - 170).         */
/*       RUN pdf_text_xy_dec ("Spdf","6.270,00",iPageWidth - iLeftmargin - bredd("6.270,00"),dY - 170).           */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RightFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RightFooter Procedure 
PROCEDURE RightFooter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*     cKO_RFtxt[1] = "Tack för att du valt att handla hos oss!".                                                */
/*     cKO_RFtxt[2] = "Osäker hur du skall vårda dina skor? Gå in och läs på vår hemsida under fliken skovård.". */

/*     RUN pdf_text_xy_dec ("Spdf","Tack för att du valt att handla hos oss!",iLeftmargin,d2 - 42).                                                */
/*     RUN pdf_text_xy_dec ("Spdf","Osäker hur du skall vårda dina skor? Gå in och läs på vår hemsida under fliken skovård.",iLeftmargin,d2 - 52). */
/*     IF cInstagram <> "" THEN DO:                                                                                                                */
/*         RUN pdf_text_xy_dec ("Spdf","Följ oss på",iLeftmargin,d2 - 65).                                                                         */
/*         RUN pdf_place_image2 IN h_PDFinc ("Spdf",                                                                                               */
/*                                          "INSTAGRAM",                                                                                           */
/*                                          iLeftMargin,                                                                                           */
/*                                          478,                                                                                                   */
/*                                          pdf_ImageDim ("Spdf","HEADERLOGO","WIDTH") * .20,                                                      */
/*                                          pdf_ImageDim ("Spdf","HEADERLOGO","HEIGHT") * .20).                                                    */
/*         RUN pdf_set_font ("Spdf", "GantModern-Bold",8).                                                                                         */
/*         RUN pdf_text_xy_dec ("Spdf","@JOHANSSONSSKOR",88,d2 - 79).                                                                              */
/*     END.                                                                                                                                        */
DEFINE VARIABLE dDim AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dPlace AS DECIMAL     NO-UNDO.

RUN pdf_set_font ("Spdf", "GantModern-Regular",9).
RUN pdf_text_xy_dec ("Spdf",cKO_RFtxt[1],iLMp2 + iCols[1],93).
RUN pdf_text_xy_dec ("Spdf",cKO_RFtxt[2],iLMp2 + iCols[1],83).

IF cInstagram <> "" THEN DO:
    RUN pdf_text_xy_dec ("Spdf","Följ oss på",iLMp2,70).
    RUN pdf_place_image2 IN h_PDFinc ("Spdf",
                                     "INSTAGRAM",
                                     iLMp2 + 50,
                                     528,
                                     pdf_ImageDim ("Spdf","INSTAGRAM","WIDTH") * .10,
                                     pdf_ImageDim ("Spdf","INSTAGRAM","HEIGHT") * .10).
/*     RUN pdf_place_image2 IN h_PDFinc ("Spdf",                                            */
/*                                      "INSTAGRAM",                                        */
/*                                      iLMp2 + 50,                                         */
/*                                      528,                                                */
/*                                      pdf_ImageDim ("Spdf","HEADERLOGO","WIDTH") * .20,   */
/*                                      pdf_ImageDim ("Spdf","HEADERLOGO","HEIGHT") * .20). */
    dDim = pdf_ImageDim ("Spdf","INSTAGRAM","WIDTH") * .10.
    RUN pdf_set_font ("Spdf", "GantModern-Bold",8).
    RUN pdf_text_xy_dec ("Spdf","www.instagram.com/skoaugust",iLMp2 + dDim + 52,69).
/*     RUN pdf_text_xy_dec ("Spdf","@skoaugust",iLMp2 + 111,71). */
END.
IF cFacebook <> "" THEN DO:
/*     RUN pdf_text_xy_dec ("Spdf","Följ oss på",iLMp2,70). */
    RUN pdf_place_image2 IN h_PDFinc ("Spdf",
                                     "FACEBOOK",
                                     iLMp2 + 195,
                                     528,
                                     pdf_ImageDim ("Spdf","FACEBOOK","WIDTH") * .10,
                                     pdf_ImageDim ("Spdf","FACEBOOK","HEIGHT") * .10).
/*     RUN pdf_place_image2 IN h_PDFinc ("Spdf",                                            */
/*                                      "INSTAGRAM",                                        */
/*                                      iLMp2 + 50,                                         */
/*                                      528,                                                */
/*                                      pdf_ImageDim ("Spdf","HEADERLOGO","WIDTH") * .20,   */
/*                                      pdf_ImageDim ("Spdf","HEADERLOGO","HEIGHT") * .20). */
    dDim = pdf_ImageDim ("Spdf","FACEBOOK","WIDTH") * .10.
    RUN pdf_set_font ("Spdf", "GantModern-Bold",8).
    RUN pdf_text_xy_dec ("Spdf","www.facebook.com/skoaugust",iLMp2 + dDim + 200,69).
/*     RUN pdf_text_xy_dec ("Spdf","@skoaugust",iLMp2 + 111,71). */
END.

RUN pdf_set_font ("Spdf", "GantModern-Bold",9).

RUN pdf_text_xy_dec ("Spdf",cRightFtxt1,iMittenR - bredd(cRightFtxt1) / 2,50).
RUN pdf_text_xy_dec ("Spdf",cRightFtxt2,iMittenR - bredd(cRightFtxt2) / 2,37).

RUN pdf_set_font ("Spdf", "GantModern-Regular",7).
RUN pdf_text_xy_dec ("Spdf",cRightFtxt3,iMittenR - bredd(cRightFtxt3) / 2,22).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RightHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RightHeader Procedure 
PROCEDURE RightHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE dY AS INTEGER     NO-UNDO.
DEFINE VARIABLE d2 AS INTEGER     NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DEFINE VARIABLE i2 AS INTEGER     NO-UNDO.
DEFINE VARIABLE iColPos     AS INTEGER   EXTENT 4  NO-UNDO.
DEFINE VARIABLE cOrsaksKod  AS CHARACTER EXTENT 5  NO-UNDO.
DEFINE VARIABLE cOrsaksTxt  AS CHARACTER EXTENT 5  NO-UNDO.
DEFINE VARIABLE cTxt        AS CHARACTER   NO-UNDO.

  dY = iStartRow. /* 584 */
  /* rigth side */
  DO:
      RUN pdf_place_image2 IN h_PDFinc ("Spdf",
                                       "HEADERLOGO",
                                       iLMp2, 
                                       43,
                                       pdf_ImageDim ("Spdf","HEADERLOGO","WIDTH") * .50, /* .40 på båda */
                                       pdf_ImageDim ("Spdf","HEADERLOGO","HEIGHT") * .50).

      RUN pdf_set_font ("Spdf", "GantModern-Bold",9).
      RUN pdf_text_xy_dec ("Spdf","ORDERÖVERSIKT",iLMp2,dY - 24).
      
      RUN pdf_set_font ("Spdf", "GantModern-Medium",9).
      RUN pdf_text_xy_dec ("Spdf",cKundRad_1,iLMp2,dY - 35).
      RUN pdf_text_xy_dec ("Spdf",cKundRad_2,iLMp2,dY - 45).
      RUN pdf_text_xy_dec ("Spdf",cKundRad_3,iLMp2,dY - 55).
      RUN pdf_text_xy_dec ("Spdf",cKundRad_4,iLMp2,dY - 65).
      
      RUN pdf_set_font ("Spdf", "GantModern-Medium",7).
      RUN pdf_text_xy_dec ("Spdf","Mobil",iLMp2,dY - 75).
      RUN pdf_text_xy_dec ("Spdf","E-post",iLMp2,dY - 85).

      RUN pdf_text_xy_dec ("Spdf",cKundRad_5,iLMp2 + 40,dY - 75).
      RUN pdf_text_xy_dec ("Spdf",cKundRad_6,iLMp2 + 40,dY - 85).

      RUN pdf_set_font ("Spdf", "GantModern-Regular",9).
      RUN pdf_text_xy_dec ("Spdf","Ordernummer:",680,dY).
      RUN pdf_text_xy_dec ("Spdf","Orderdatum:",680,dY - 12).
      RUN pdf_set_font ("Spdf", "GantModern-Bold",9).
      RUN pdf_text_xy_dec ("Spdf",TRIM(cOrdreNr),iRMarginPos - bredd(TRIM(cOrdreNr)),dY).
      RUN pdf_text_xy_dec ("Spdf",STRING(dDato,"99.99.99"),iRMarginPos - bredd(STRING(dDato,"99.99.99")),dY - 12).
      

  END.

  RUN pdf_set_font ("Spdf", "GantModern-Bold",8).
  RUN pdf_text_xy_dec ("Spdf","HAR DU FRÅGOR?",680,dY - 35).
/*   RUN pdf_text_xy_dec ("Spdf","JF Kundservice",680,dY - 45). */
  RUN pdf_text_xy_dec ("Spdf","Maila till",680,dY - 45).
  
  RUN pdf_set_font ("Spdf", "GantModern-Medium",7).
  
/*   RUN pdf_text_xy_dec ("Spdf","Öppettider",680,dY - 55). */
/*   RUN pdf_text_xy_dec ("Spdf","E-post",680,dY - 65). */
/*   RUN pdf_text_xy_dec ("Spdf","Telefon",680,dY - 75). */

  RUN pdf_set_font ("Spdf", "GantModern-Regular",7).

/*   RUN pdf_text_xy_dec ("Spdf","Mån-fre, 10:00-16:00",740,dY - 55). */
  RUN pdf_text_xy_dec ("Spdf","info@skoaugust.com",iRMarginPos - bredd("info@skoaugust.com"),dY - 45).
/*   RUN pdf_text_xy_dec ("Spdf","kundtjanst@johanssons.se",740,dY - 65). */
/*   RUN pdf_text_xy_dec ("Spdf","0300 - 56 45 06",740,dY - 75). */

  RUN pdf_set_font ("Spdf", "GantModern-Bold",7).
  RUN pdf_text_xy_dec ("Spdf","Betalning",iLMp2,dY - 100).
  FIND FIRST KOrdrelinje OF KOrdreHode WHERE KOrdreLinje.VareNr = "BETALT"  NO-LOCK NO-ERROR.
  IF AVAIL KOrdreLinje THEN DO:
      RUN pdf_set_font ("Spdf", "wingding",10).
      RUN pdf_text_xy_dec ("Spdf","x",iLMp2,dY - 113).
      RUN pdf_set_font ("Spdf", "GantModern-Regular",8).
      RUN pdf_text_xy_dec ("Spdf",KOrdreLinje.Varetekst,iLMp2 + 10,dY - 113).
  END.
/*   IF lJohanssonMKlubb THEN DO:                                             */
/*       RUN pdf_set_font ("Spdf", "wingding",10).                            */
/*       RUN pdf_text_xy_dec ("Spdf","x",iLMp2,dY - 124).                     */
/*       RUN pdf_set_font ("Spdf", "GantModern-Regular",8).                   */
/*       RUN pdf_text_xy_dec ("Spdf","Exclusive medlem",iLMp2 + 10,dY - 124). */
/*   END.                                                                     */
/*   RUN pdf_set_font ("Spdf", "wingding",10).                                               */
/*   RUN pdf_text_xy_dec ("Spdf","x",iLMp2 + 70,dY - 118).                                   */
/*                                                                                           */
/*   RUN pdf_set_font ("Spdf", "GantModern-Regular",8).                                      */
/*   RUN pdf_text_xy_dec ("Spdf","Faktura tilsendt monafuru@gmail.com",iLMp2 + 82,dY - 118). */
  

  RUN pdf_skip ("Spdf").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivDataLeft) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivDataLeft Procedure 
PROCEDURE SkrivDataLeft :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      DEFINE INPUT  PARAMETER dY AS INTEGER     NO-UNDO.
      DEFINE VARIABLE cFarge AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE cVaretekst AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE cVarenr AS CHARACTER   NO-UNDO.
      cFarge = KOrdreLinje.LevFargKod.
      cVaretekst = KOrdreLinje.Varetekst.
/*       IF NUM-ENTRIES(cFarge,"/") > 1 THEN DO: */
          FIND artbas WHERE artbas.artikkelnr = DECI(KOrdreLinje.VareNr) NO-LOCK NO-ERROR.
          IF AVAIL artbas THEN DO:
              FIND farg WHERE farg.farg = artbas.farg NO-LOCK NO-ERROR.
              IF AVAIL farg THEN
                  cFarge = STRING(farg.farbeskr).
              cVarenr = STRING(artbas.vg) + "/" + IF artbas.lopnr <> ? THEN STRING(artbas.lopnr) ELSE "".
              IF artbas.vmid > 0 THEN DO:
                  FIND varemerke OF artbas NO-LOCK NO-ERROR.
                  IF AVAIL varemerke AND TRIM(Varemerke.Beskrivelse) <> "" THEN
                      cVaretekst = TRIM(Varemerke.Beskrivelse).
              END.
          END.
          ELSE DO:
              cVarenr = KOrdreLinje.VareNr.
          END.
/*       END. */
      RUN pdf_set_font ("Spdf", "GantModern-Regular",7).
/*       RUN pdf_text_xy_dec ("Spdf",KOrdreLinje.VareNr,iLeftmargin + iTblCols[1],dY). */
      RUN pdf_text_xy_dec ("Spdf",cVareNr,iLeftmargin + iTblCols[1],dY).
      RUN pdf_text_xy_dec ("Spdf",cFarge,iLeftmargin + iTblCols[2],dY).
      RUN pdf_text_xy_dec ("Spdf",cVaretekst,iLeftmargin + iTblCols[3],dY).
      RUN pdf_text_xy_dec ("Spdf",KOrdreLinje.Storl,iLeftmargin + ((iTblCols[4] + iTblCols[5]) / 2) - bredd(KOrdreLinje.Storl) / 2 - 2,dY).
      RUN pdf_set_font ("Spdf", "wingding",10).
      RUN pdf_text_xy_dec ("Spdf","o",iLeftmargin + iTblCols[5],dY).
      RUN pdf_set_font ("Spdf", "GantModern-Regular",6).
      RUN pdf_text_xy_dec ("Spdf","Retur",iLeftmargin + iTblCols[5] + 10 ,dY).
/*       RUN pdf_set_font ("Spdf", "wingding",10).                                        */
/*       RUN pdf_text_xy_dec ("Spdf","o",iLeftmargin + iTblCols[5] + 28,dY).              */
/*       RUN pdf_set_font ("Spdf", "GantModern-Regular",6).                               */
/*       RUN pdf_text_xy_dec ("Spdf","Byte mot str:",iLeftmargin + iTblCols[5] + 38 ,dY). */


/*       RUN pdf_rect ("Spdf", iLeftMargin - 2,dY - 5,(iPageWidth / 2) - iLeftmargin - iLeftMargin,16,0.5). */
      RUN pdf_line ("Spdf",iLeftMargin,dY - 5,(iPageWidth / 2) - iLeftmargin,dY - 5,0.5).
      /* 
       RUN pdf_set_font ("Spdf", "GantModern-Regular",7).
      RUN pdf_rect ("Spdf", iLeftMargin - 2,dY - 5,(iPageWidth / 2) - iLeftmargin - iLeftMargin,16,0.5).
      RUN pdf_text_color  ("Spdf",1.0,1.0,1.0).
      RUN pdf_text_xy_dec ("Spdf",cTblColLabels [1],iLeftmargin + iTblCols[1],dY).
      RUN pdf_text_xy_dec ("Spdf",cTblColLabels [2],iLeftmargin + iTblCols[2],dY).
      RUN pdf_text_xy_dec ("Spdf",cTblColLabels [3],iLeftmargin + iTblCols[3],dY).
      RUN pdf_text_xy_dec ("Spdf",cTblColLabels [4],iLeftmargin + iTblCols[4],dY).
      RUN pdf_text_xy_dec ("Spdf",cTblColLabels [5],iLeftmargin + iTblCols[5],dY).
      RUN pdf_text_xy_dec ("Spdf",cTblColLabels [6],iLeftmargin + iTblCols[6],dY).
      RUN pdf_text_color  ("Spdf",0.0,0.0,0.0).
*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivDataRight) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivDataRight Procedure 
PROCEDURE SkrivDataRight :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      DEFINE INPUT  PARAMETER dY AS INTEGER     NO-UNDO.
      DEFINE VARIABLE cVaretekst AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE cFarge AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE cVarenr AS CHARACTER   NO-UNDO.
      cFarge = KOrdreLinje.LevFargKod.
      cVaretekst = KOrdreLinje.Varetekst.
/*       IF NUM-ENTRIES(cFarge,"/") > 1 THEN DO: */
          FIND artbas WHERE artbas.artikkelnr = DECI(KOrdreLinje.VareNr) NO-LOCK NO-ERROR.
          IF AVAIL artbas THEN DO:
              FIND farg WHERE farg.farg = artbas.farg NO-LOCK NO-ERROR.
              IF AVAIL farg THEN
                  cFarge = STRING(farg.farbeskr).
              cVarenr = STRING(artbas.vg) + "/" + IF artbas.lopnr <> ? THEN STRING(artbas.lopnr) ELSE "".
              IF artbas.vmid > 0 THEN DO:
                  FIND varemerke OF artbas NO-LOCK NO-ERROR.
                  IF AVAIL varemerke AND TRIM(Varemerke.Beskrivelse) <> "" THEN
                      cVaretekst = TRIM(Varemerke.Beskrivelse).
              END.
          END.
          ELSE DO:
              cVarenr = KOrdreLinje.VareNr.
          END.
/*       END. */


      RUN pdf_set_font ("Spdf", "GantModern-Regular",7).

/*       RUN pdf_text_xy_dec ("Spdf",KOrdreLinje.VareNr,iLMp2 + iCols[1],dY). */
      RUN pdf_text_xy_dec ("Spdf",cVareNr,iLMp2 + iCols[1],dY).
      RUN pdf_text_xy_dec ("Spdf",cFarge,iLMp2 + iCols[2],dY).
      RUN pdf_text_xy_dec ("Spdf",cVaretekst,iLMp2 + iCols[3],dY).
/*       RUN pdf_text_xy_dec ("Spdf",KOrdreLinje.Varetekst,iLMp2 + iCols[3],dY). */
      RUN pdf_text_xy_dec ("Spdf",KOrdreLinje.Storl,iLMp2 + iCols[4] + 5 - bredd(KOrdreLinje.Storl) / 2,dY).
      RUN pdf_text_xy_dec ("Spdf",KOrdreLinje.Antall,iLMp2 + iCols[5] + 13 - bredd(STRING(KOrdreLinje.Antall)),dY).
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(KOrdreLinje.LinjeRabattKr,"->>,>>9.99")),iLMp2 + iCols[6] + bredd(cColLabels[6]) - bredd(TRIM(STRING(KOrdreLinje.LinjeRabattKr,"->>,>>9.99"))),dY).
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(KOrdreLinje.NettoLinjesum,"->>,>>9.99")),iPageWidth - iLeftmargin - bredd(TRIM(STRING(KOrdreLinje.NettoLinjesum,"->>,>>9.99"))),dY).

      RUN pdf_line ("Spdf",iLMp2,dY - 5,iPageWidth - iLeftmargin,dY - 5,0.5).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Splitter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Splitter Procedure 
PROCEDURE Splitter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      RUN pdf_set_dash ("Spdf", 4,6).
      RUN pdf_line ("Spdf",pdf_PageWidth("Spdf") / 2,1,pdf_PageWidth("Spdf") / 2,650,0.5).
      RUN pdf_set_dash ("Spdf", 1,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VertLines) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VertLines Procedure 
PROCEDURE VertLines :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iStartRow AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER iEndRow AS INTEGER     NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
      DO ii = 1 TO EXTENT(iTblCols):
          RUN pdf_line ("Spdf",iLeftMargin + iTblCols[ii] - 2,iStartRow,iLeftMargin + iTblCols[ii] - 2,iEndRow,0.5).
      END.
      RUN pdf_line ("Spdf",(iPageWidth / 2) - iLeftmargin,iStartRow,(iPageWidth / 2) - iLeftmargin,iEndRow,0.5).

/*     ASSIGN iTblCols[1] = 0                                  */
/*            iTblCols[2] = 42                                 */
/*            iTblCols[3] = 68                                 */
/*            iTblCols[4] = 210                                */
/*            iTblCols[5] = 235                                */
/*            iTblCols[6] = 337                                */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-bredd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION bredd Procedure 
FUNCTION bredd RETURNS DECIMAL
  ( INPUT cText AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN pdf_text_widthdec ("Spdf",cText).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EAN13BC) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EAN13BC Procedure 
FUNCTION EAN13BC RETURNS CHARACTER
  ( INPUT icStrekKode AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE cBarCode AS CHARACTER EXTENT 14 NO-UNDO.
DEFINE VARIABLE cBCString AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.

IF LENGTH(icStrekKode) < 13 THEN
  icStrekKode = FILL("0",13 - LENGTH(icStrekKode)) + icStrekKode.
ELSE IF LENGTH(icStrekKode) > 13 THEN
  RETURN "".

ASSIGN cBarCode[1] = CHR(ASC(SUBSTR(icStrekKode,1,1)) - 15) 
       cBarCode[2] = CHR(ASC(SUBSTR(icStrekKode,2,1)) + 48)
       cBarCode[8] = CHR(124).

ASSIGN cBarcode[3] = SUBSTR(icStrekKode,3,1)
       cBarcode[4] = SUBSTR(icStrekKode,4,1)
       cBarcode[5] = SUBSTR(icStrekKode,5,1)
       cBarcode[6] = SUBSTR(icStrekKode,6,1)
       cBarcode[7] = SUBSTR(icStrekKode,7,1).

CASE SUBSTR(icStrekKode,1,1):
    WHEN "1" THEN
        ASSIGN cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[6] = CHR(ASC(cBarcode[6]) + 16)
               cBarcode[7] = CHR(ASC(cBarcode[7]) + 16).
    WHEN "2" THEN
        ASSIGN cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[5] = CHR(ASC(cBarcode[5]) + 16)
               cBarcode[7] = CHR(ASC(cBarcode[7]) + 16).
    WHEN "3" THEN
        ASSIGN cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[5] = CHR(ASC(cBarcode[5]) + 16)
               cBarcode[6] = CHR(ASC(cBarcode[6]) + 16).
    WHEN "4" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[6] = CHR(ASC(cBarcode[6]) + 16)
               cBarcode[7] = CHR(ASC(cBarcode[7]) + 16).
    WHEN "5" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[7] = CHR(ASC(cBarcode[7]) + 16).
    WHEN "6" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[5] = CHR(ASC(cBarcode[5]) + 16).
    WHEN "7" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[5] = CHR(ASC(cBarcode[5]) + 16)
               cBarcode[7] = CHR(ASC(cBarcode[7]) + 16).
    WHEN "8" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[5] = CHR(ASC(cBarcode[5]) + 16)
               cBarcode[6] = CHR(ASC(cBarcode[6]) + 16).
    WHEN "9" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[6] = CHR(ASC(cBarcode[6]) + 16).
END CASE.
ASSIGN cBarcode[9] = CHR(ASC(SUBSTR(icStrekKode,8,1)) + 32) 
       cBarcode[10] = CHR(ASC(SUBSTR(icStrekKode,9,1)) + 32) 
       cBarcode[11] = CHR(ASC(SUBSTR(icStrekKode,10,1)) + 32) 
       cBarcode[12] = CHR(ASC(SUBSTR(icStrekKode,11,1)) + 32) 
       cBarcode[13] = CHR(ASC(SUBSTR(icStrekKode,12,1)) + 32).
       

cBarcode[14] = CHR(ASC(SUBSTR(icStrekKode,13,1)) + 64).
DO iCount = 1 TO 14:
    cBCString = cBCString + cBarCode[iCount] .
END.

IF cBCstring = ? THEN cBCstring = "".

RETURN cBCstring. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

