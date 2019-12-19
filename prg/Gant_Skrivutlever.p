&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :  ï¿½ Dette er karrakterene som settes inn isteden for øæå.

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
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.

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
DEFINE VARIABLE iTblCols          AS INTEGER EXTENT 6  NO-UNDO.
DEFINE VARIABLE cTblColLabels     AS CHARACTER EXTENT 6  NO-UNDO.
DEFINE VARIABLE cOrdreNr   AS CHARACTER   NO-UNDO.
/* DEFINE VARIABLE dOrdreNr   AS DECIMAL   NO-UNDO.  */
DEFINE VARIABLE dDato      AS DATE      NO-UNDO.
DEFINE VARIABLE cKundRad_1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKundRad_2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKundRad_3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKundRad_4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKundFakt_1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKundFakt_2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLevRad_1  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLevRad_2  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLevRad_3  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLevRad_4  AS CHARACTER NO-UNDO.

DEFINE VARIABLE cRightFtxt1 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRightFtxt2 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRightFtxt3 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBarcode    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dTst AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iMaxrader AS INTEGER     NO-UNDO.
DEFINE VARIABLE iAntrader AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTmpRad AS INTEGER     NO-UNDO.
DEFINE VARIABLE lFler   AS LOGICAL     NO-UNDO.
DEFINE TEMP-TABLE tt_Artbas NO-UNDO LIKE Artbas.
DEFINE VARIABLE cMKlubbId AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lExclusiveMember AS LOGICAL     NO-UNDO.
DEFINE BUFFER bufKOL FOR KOrdrelinje.
DEFINE BUFFER bufKOrdreLinje FOR KOrdreLinje.

{ pdf_inc.i "THIS-PROCEDURE"}
/* { pdf_inc.i "NOT SUPER"} */

DEFINE VARIABLE iLMp2 AS INTEGER     NO-UNDO.

DEFINE TEMP-TABLE tt_KLinje NO-UNDO LIKE KOrdreLinje
    FIELD cVareNr AS CHAR.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

ASSIGN
  bTest = TRUE  
  cLogg = 'skrivkundeordre' + REPLACE(STRING(TODAY),'/','')
  .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

  ASSIGN iCols[1] = 0
         iCols[2] = 43
         iCols[3] = 70
         iCols[4] = 231
         iCols[5] = 260
         iCols[6] = 295
         iCols[7] = 350.

IF bTest THEN
DO: 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Start (Gant_Skrivutlever.p)' 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Filnavn: ' + cFilNavn + '.'  
      ).    
END.

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
         HEIGHT             = 28.48
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
{syspara.i 14 1  7 cMKlubbId} /* for Mayflower */
{syspara.i 19 13 2 cImageFile}

dKOrdreID = DECI(cKOrdreID) NO-ERROR.    
IF ERROR-STATUS:ERROR THEN
    RETURN.
FIND KOrdreHode WHERE KOrdreHode.KOrdre_Id = dKOrdreID NO-LOCK NO-ERROR.
IF NOT AVAIL KOrdreHode THEN
  DO:
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    Finner ikke KOrdreId: ' + cKOrdreID + '.'  
        ).    
    RETURN.
  END.
  
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    KOrdreId/Ekstordrenr: ' + cKOrdreID + '/' + KOrdreHode.EkstOrdreNr + '.'  
      ).    


iMaxrader = 17.
FOR EACH KOrdrelinje OF KOrdrehode NO-LOCK WHERE KOrdreLinje.Aktiv = TRUE:
    IF KOrdreLinje.VareNr = "BETALT" THEN
        NEXT.
    IF KOrdreLinje.VareNr = "FRAKT" THEN
        NEXT.
    iAntrader = iAntrader + 1.
END.
IF iAntRader > iMaxRader THEN
    lFler = TRUE.

FOR EACH KOrdrelinje OF KOrdrehode NO-LOCK:
  /* På vanlige ordre skal bare aktive linjer skrives ut. */
  IF KOrdreHode.SendingsNr <> 'RETUR' THEN 
  DO: 
    IF KOrdreLinje.Aktiv = FALSE THEN 
      NEXT.
  END.
  /* På varelinjer hvor det er byttet vare på returordre. */
  ELSE IF KOrdreHode.SendingsNr = 'RETUR' AND KOrdreLinje.ByttetKOrdreLinjeNr > 0 THEN
  DO:
    /* Skal være med. Begge radene */    
  END. 
  /* På returordre, skal linjer hvor vare er endret på returordre, men ikke på opprinnelig ordrelinje. */
  ELSE IF KOrdreHode.SendingsNr = 'RETUR' AND KOrdreLinje.KopiKOrdreLinjeNr > 0 THEN 
  DO:
    FIND bufKOrdreLinje NO-LOCK WHERE 
      bufKOrdreLinje.KOrdre_Id = KOrdreHode.RefKOrdre_Id AND 
      bufKOrdreLinje.KOrdreLinjeNr = KOrdreLinje.KOrdreLinjeNr NO-ERROR.
    IF AVAILABLE bufKORdreLinje AND bufKOrdreLinje.KopiKOrdreLinjeNr > 0 THEN 
     NEXT.
  END.
  ELSE IF KOrdreHode.SendingsNr = 'RETUR' AND KOrdreLinje.KopiKOrdreLinjeNr = 0 AND KORdreLinje.aktiv = TRUE THEN
  DO:
    /* Skal være med.  Dvs. ikke noe Next her. :) */.
  END.
  /* Skal ikke med. Er nå passive linjer på returordre hvor vare ike er byttet. */
  ELSE DO:
    NEXT.
  END.
  
  CREATE tt_KLinje.
  BUFFER-COPY KOrdrelinje TO tt_KLinje.
  IF tt_KLinje.VareNr MATCHES "*BETALT*" OR tt_KLinje.VareNr MATCHES "*FRAKT*" THEN
      NEXT.
  FIND artbas WHERE artbas.artikkelnr = DECI(tt_KLinje.VareNr) NO-LOCK NO-ERROR.
  cVarenr = IF AVAIL artbas AND TRIM(artbas.levkod) <> "" THEN artbas.levkod ELSE "I" + STRING(tt_KLinje.VareNr).

END.

/* FIND FIRST medlem WHERE medlem.kundenr = KOrdrehode.kundenr NO-LOCK NO-ERROR. */
/* IF AVAIL medlem AND STRING(Medlem.MKlubbId) = cMKlubbId THEN                  */
/*     lExclusiveMember = TRUE.                                                  */
/* fra ordre */
FIND butiker WHERE butiker.butik = kordrehode.butikknr NO-LOCK NO-ERROR.

IF LENGTH(TRIM(KOrdreHode.Embalage)) = 22 THEN DO:
    dTst = DECI(KOrdreHode.Embalage) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
        cBarcode  = TRIM(KOrdreHode.Embalage).
END.
dDato    = KOrdreHode.BetaltDato.
cOrdreNr = KOrdreHode.EkstOrdreNr.
/* Hemt kund o butik */
cKundRad_1 = KOrdreHode.Navn.
cKundRad_2 = KOrdreHode.LevAdresse1.
cKundRad_3 = KOrdreHode.LevPostNr  + " " + KOrdreHode.LevPostSted.
cKundRad_4 = "".
cKundFakt_1 = KOrdreHode.FaktAdresse1 + (IF KOrdreHode.FaktAdresse2 <> "" THEN "," ELSE "") + KOrdreHode.FaktAdresse2.
cKundFakt_2 = KOrdreHode.FaktPostNr + " " + KOrdreHode.FaktPoststed.
/*                                   cKundRad_1 = "Kenneth Olausson".  KOrdreHode.Navn                                      */
/*                                   cKundRad_2 = "Tuterudkroken 16".  KOrdreHode.LevAdresse1                               */
/*                                   cKundRad_3 = "2007 Kjeller".      KOrdreHode.LevPostNr  + " " + KOrdreHode.LevPostSted */
/*                                   cKundRad_4 = "".                                                                       */
FIND post WHERE pos.postnr = butiker.levpostnr NO-LOCK NO-ERROR.
cLevRad_1  = Butiker.ButFirmanavn.
cLevRad_2  = Butiker.LevAdresse1.
cLevRad_3  = butiker.levpostnr + " " + IF AVAIL post THEN Post.Beskrivelse ELSE "".
cLevRad_4  = "".
/* cLevRad_1  = "GANT Retail AS". */
/* cLevRad_2  = "Tomtegata 80".   */
/* cLevRad_3  = "3012 Drammen".   */
/* cLevRad_4  = "".               */

FIND post WHERE pos.postnr = butiker.buponr NO-LOCK NO-ERROR.

cRightFtxt1 = "Dersom du ønsker å bytte eller returnere varer på din ordre".
cRightFtxt2 = "river du av og fyller ut 'Bytte- og Returskjema' på siste side.".
cRightFtxt3 = "GANT Retail AS" + "," + Butiker.BuAdr + "," + butiker.buponr + " " + (IF AVAIL post THEN post.beskrivelse ELSE "") + " - Org.nr: " + Butiker.OrganisasjonsNr + 
                                 " Foretaksregisteret".

/* cRightFtxt3 = "GANT Retail AS, Nesset 5, 3470 Slemmestad - Org.nr: 985 383 383". */

    RUN RapportPDF.
    ASSIGN 
      ocReturn = cFilnavn
      obOk     = TRUE
    .
IF VALID-HANDLE(h_PDFinc) THEN DO:
        DELETE PROCEDURE h_PDFinc.
END.

IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Fil generert: ' + ocReturn + '.' 
      ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    obOk: ' + STRING(obOk) + '.' 
      ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Slutt (Gant_Skrivutlever.p)' 
      ).
END.    

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTT Procedure 
PROCEDURE ByggTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iLevNr AS INTEGER     NO-UNDO.
    FIND FIRST Artbas WHERE Artbas.levnr = 1 NO-LOCK.
    CREATE tt_artbas.
    BUFFER-COPY Artbas TO TT_artbas.
    FIND FIRST Artbas WHERE Artbas.levnr = 10 NO-LOCK.
    CREATE tt_artbas.
    BUFFER-COPY Artbas TO TT_artbas.
    FIND FIRST Artbas WHERE Artbas.levnr = 38 NO-LOCK.
    CREATE tt_artbas.
    BUFFER-COPY Artbas TO TT_artbas.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

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
DEFINE VARIABLE dY AS INTEGER     NO-UNDO.

/*  iPageHeight = 612. 
    iPageWidth  = 842 */
  dY = iStartRow.

  ASSIGN cColLabels[1]  = "Artnr" 
         cColLabels[2]  = "Farge"
         cColLabels[3]  = "Artnavn"
         cColLabels[4]  = "Str"
         cColLabels[5]  = "Ant"
         cColLabels[6] = "Rabatt"
         cColLabels[7] = "Pris".

  DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
  RUN pdf_set_font ("Spdf", "GantModern-Bold",7).
    RUN pdf_skip    ("Spdf").
    DO ii = 1 TO EXTENT(iCols):
        RUN pdf_text_xy_dec ("Spdf",cColLabels[ii],iLMp2 + iCols[ii],dY - 142).
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

   DEFINE VARIABLE   dY_Gave AS INTEGER     NO-UNDO.

   DEFINE VARIABLE cLevmetodeLbl   AS CHARACTER INIT "Leveringsmetode"  NO-UNDO.
   DEFINE VARIABLE cRabattkodeLbl  AS CHARACTER INIT "Rabattkode"  NO-UNDO.
   DEFINE VARIABLE cNettoLbl       AS CHARACTER INIT "Netto"  NO-UNDO.
   DEFINE VARIABLE cRabattLbl      AS CHARACTER INIT "Rabatt"  NO-UNDO.
   DEFINE VARIABLE c25%mvaLbl      AS CHARACTER INIT "25% mva"  NO-UNDO.
   DEFINE VARIABLE cFraktLbl       AS CHARACTER INIT "Frakt"  NO-UNDO.
   DEFINE VARIABLE cTotalLbl       AS CHARACTER INIT "Total"  NO-UNDO.
   DEFINE VARIABLE cBruttoLbl       AS CHARACTER INIT "Brutto"       NO-UNDO.
   DEFINE VARIABLE cSumLbl       AS CHARACTER INIT "Sum"       NO-UNDO.
   DEFINE VARIABLE cMomsLbl      AS CHARACTER INIT   "Sum mva"  NO-UNDO.

   DEFINE VARIABLE cLevmetode      AS CHARACTER     NO-UNDO.
   DEFINE VARIABLE cRabattkode     AS CHARACTER     NO-UNDO.
   DEFINE VARIABLE dNetto          AS DECIMAL NO-UNDO.
   DEFINE VARIABLE dRabatt         AS DECIMAL NO-UNDO.
   DEFINE VARIABLE d25%mva         AS DECIMAL NO-UNDO.
   DEFINE VARIABLE dFrakt          AS DECIMAL NO-UNDO.
   DEFINE VARIABLE dTotal          AS DECIMAL NO-UNDO.
   DEFINE VARIABLE dBrutto         AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE dSum            AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE iTst AS INTEGER     NO-UNDO.
   FIND LeveringsForm WHERE LeveringsForm.LevFNr = KOrdreHode.LevFNr NO-LOCK NO-ERROR.
   IF AVAIL LeveringsForm THEN 
       cLevmetode = LeveringsForm.LevFormBeskrivelse.
RUN GetDataEndRight (OUTPUT dBrutto,OUTPUT dRabatt,OUTPUT d25%mva,OUTPUT dFrakt,OUTPUT dNetto).
dSum = dFrakt + dNetto.

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
      RUN pdf_set_font ("Spdf", "GantModern-Bold",8).
      RUN pdf_text_xy_dec ("Spdf","Leveringsmetode",iLMp2,dY).
      RUN pdf_set_font ("Spdf", "GantModern-Regular",8).
      RUN pdf_text_xy_dec ("Spdf",cLevmetode,iLMp2 + 80,dY).

      IF TRIM(Kordrehode.cOpt1) <> "" THEN DO:
          dy_Gave = dY.
/*           RUN pdf_set_font ("Spdf", "wingding",10).             */
/*           RUN pdf_text_xy_dec ("Spdf","x",iLMp2,dy_Gave - 126). */
          RUN pdf_set_font ("Spdf", "GantModern-Bold",8).
          RUN pdf_text_xy_dec ("Spdf","Gave",iLMp2,dy_Gave - 12).
          RUN pdf_set_font ("Spdf", "GantModern-Regular",8).
          RUN pdf_text_xy_dec ("Spdf","JA",iLMp2 + 80,dy_Gave - 12).
          IF NOT cOpt1 BEGINS "INGEN_TEKST" THEN DO:
              RUN pdf_set_font ("Spdf", "GantModern-Bold",8).
              RUN pdf_text_xy_dec ("Spdf","Meddelande",iLMp2,dy_Gave - 24).
              RUN pdf_set_font ("Spdf", "GantModern-Regular",8).
/*               IF NOT cOpt1 BEGINS "INGEN_TEKST" THEN */
                  RUN SkrivGave(Kordrehode.cOpt1,dy_Gave - 24 /* rad */,iLMp2 + 80 /* leftcol */,715 /* rightcol */).
          END.
      END.

      
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dBrutto,"->>,>>9.99")),iPageWidth - iLeftMargin - bredd(TRIM(STRING(dBrutto,"->>,>>9.99"))),dY).
/*       RUN pdf_text_xy_dec ("Spdf",cRabattkode,iLMp2 + 80,dY - 12). */
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dRabatt,"->>,>>9.99")),iPageWidth - iLeftMargin - bredd(TRIM(STRING(dRabatt,"->>,>>9.99"))),dY - 12).
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dFrakt,"->>,>>9.99")),iPageWidth - iLeftMargin - bredd(TRIM(STRING(dFrakt,"->>,>>9.99"))),dY - 24).
      RUN pdf_set_font ("Spdf", "GantModern-Bold",8).
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dSum,"->>,>>9.99")),iPageWidth - iLeftMargin - bredd(TRIM(STRING(dSum,"->>,>>9.99"))),dY - 36).
      RUN pdf_set_font ("Spdf", "GantModern-Regular",8).
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(d25%mva,"->>,>>9.99")),iPageWidth - iLeftMargin - bredd(TRIM(STRING(d25%mva,"->>,>>9.99"))),dY - 48).

/*       RUN pdf_set_font ("Spdf", "GantModern-Bold",6).                                                                                               */
/*       RUN pdf_text_xy_dec ("Spdf",cLevmetodeLbl,iLMp2,dY).                                                                                          */
/*       RUN pdf_text_xy_dec ("Spdf",cNettoLbl,730,dY).                                                                                                */
/*       RUN pdf_text_xy_dec ("Spdf",cRabattkodeLbl,iLMp2,dY - 12).                                                                                    */
/*       RUN pdf_text_xy_dec ("Spdf",cRabattLbl,730,dY - 12).                                                                                          */
/*       RUN pdf_text_xy_dec ("Spdf",c25%mvaLbl,730,dY - 24).                                                                                          */
/*       RUN pdf_text_xy_dec ("Spdf",cFraktLbl,730,dY - 36).                                                                                           */
/*       RUN pdf_text_xy_dec ("Spdf",cTotalLbl,730,dY - 48).                                                                                           */
/*       RUN pdf_set_font ("Spdf", "GantModern-Regular",8).                                                                                            */
/*       RUN pdf_text_xy_dec ("Spdf",cLevmetode,iLMp2 + 80,dY).                                                                                        */
/*                                                                                                                                                     */
/*       RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dNetto,"->>,>>9.99")),iPageWidth - iLeftMargin - bredd(TRIM(STRING(dNetto,"->>,>>9.99"))),dY).        */
/*       RUN pdf_text_xy_dec ("Spdf",cRabattkode,iLMp2 + 80,dY - 12).                                                                                  */
/*       RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dRabatt,"->>,>>9.99")),iPageWidth - iLeftMargin - bredd(TRIM(STRING(dRabatt,"->>,>>9.99"))),dY - 12). */
/*       RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(d25%mva,"->>,>>9.99")),iPageWidth - iLeftMargin - bredd(TRIM(STRING(d25%mva,"->>,>>9.99"))),dY - 24). */
/*       RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dFrakt,"->>,>>9.99")),iPageWidth - iLeftMargin - bredd(TRIM(STRING(dFrakt,"->>,>>9.99"))),dY - 36).   */
/*       RUN pdf_set_font ("Spdf", "GantModern-Bold",8).                                                                                               */
/*       RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dTotal,"->>,>>9.99")),iPageWidth - iLeftMargin - bredd(TRIM(STRING(dTotal,"->>,>>9.99"))),dY - 48).   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DataEndRightOrg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DataEndRightOrg Procedure 
PROCEDURE DataEndRightOrg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER dY AS INTEGER     NO-UNDO.

   DEFINE VARIABLE cLevmetodeLbl   AS CHARACTER INIT "Leveringsmetode"  NO-UNDO.
   DEFINE VARIABLE cRabattkodeLbl  AS CHARACTER INIT "Rabattkode"  NO-UNDO.
   DEFINE VARIABLE cNettoLbl       AS CHARACTER INIT "Netto"  NO-UNDO.
   DEFINE VARIABLE cRabattLbl      AS CHARACTER INIT "Rabatt"  NO-UNDO.
   DEFINE VARIABLE c25%mvaLbl      AS CHARACTER INIT "25% mva"  NO-UNDO.
   DEFINE VARIABLE cFraktLbl       AS CHARACTER INIT "Frakt"  NO-UNDO.
   DEFINE VARIABLE cTotalLbl       AS CHARACTER INIT "Total"  NO-UNDO.
   DEFINE VARIABLE cLevmetode      AS CHARACTER     NO-UNDO.
   DEFINE VARIABLE cRabattkode     AS CHARACTER     NO-UNDO.
   DEFINE VARIABLE dNetto          AS DECIMAL NO-UNDO.
   DEFINE VARIABLE dRabatt         AS DECIMAL NO-UNDO.
   DEFINE VARIABLE d25%mva         AS DECIMAL NO-UNDO.
   DEFINE VARIABLE dFrakt          AS DECIMAL NO-UNDO.
   DEFINE VARIABLE dTotal          AS DECIMAL NO-UNDO.
   DEFINE VARIABLE dBrutto         AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE dSum            AS DECIMAL     NO-UNDO.
   FIND LeveringsForm WHERE LeveringsForm.LevFNr = KOrdreHode.LevFNr NO-LOCK NO-ERROR.
   IF AVAIL LeveringsForm THEN 
       cLevmetode = LeveringsForm.LevFormBeskrivelse.
RUN GetDataEndRight (OUTPUT dBrutto,OUTPUT dRabatt,OUTPUT d25%mva,OUTPUT dFrakt,OUTPUT dNetto).
/*    FIND FIRST Kordrelinje OF KOrdrehode WHERE Kordrelinje.Varenr = "FRAKT" NO-LOCK NO-ERROR. */
/*    IF AVAIL Kordrelinje THEN                                                                 */
/*        dFrakt = Kordrelinje.Nettolinjesum.                                                   */
/*    dRabatt = KOrdreHode.TotalRabattKr.                                                       */
/*    dTotal  = KOrdreHode.Totalt.                                                              */
/*    dNetto  = KOrdreHode.Totalt - KOrdreHode.TotalRabattKr.                                   */

      RUN pdf_set_font ("Spdf", "GantModern-Bold",6).
      RUN pdf_text_xy_dec ("Spdf",cLevmetodeLbl,iLMp2,dY).
      RUN pdf_text_xy_dec ("Spdf",cNettoLbl,730,dY).
      RUN pdf_text_xy_dec ("Spdf",cRabattkodeLbl,iLMp2,dY - 12).
      RUN pdf_text_xy_dec ("Spdf",cRabattLbl,730,dY - 12).
      RUN pdf_text_xy_dec ("Spdf",c25%mvaLbl,730,dY - 24).
      RUN pdf_text_xy_dec ("Spdf",cFraktLbl,730,dY - 36).
      RUN pdf_text_xy_dec ("Spdf",cTotalLbl,730,dY - 48).
      RUN pdf_set_font ("Spdf", "GantModern-Regular",8).
      RUN pdf_text_xy_dec ("Spdf",cLevmetode,iLMp2 + 80,dY).
      
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dNetto,"->>,>>9.99")),iPageWidth - iLeftMargin - bredd(TRIM(STRING(dNetto,"->>,>>9.99"))),dY).
      RUN pdf_text_xy_dec ("Spdf",cRabattkode,iLMp2 + 80,dY - 12).
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dRabatt,"->>,>>9.99")),iPageWidth - iLeftMargin - bredd(TRIM(STRING(dRabatt,"->>,>>9.99"))),dY - 12).
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(d25%mva,"->>,>>9.99")),iPageWidth - iLeftMargin - bredd(TRIM(STRING(d25%mva,"->>,>>9.99"))),dY - 24).
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dFrakt,"->>,>>9.99")),iPageWidth - iLeftMargin - bredd(TRIM(STRING(dFrakt,"->>,>>9.99"))),dY - 36).
      RUN pdf_set_font ("Spdf", "GantModern-Bold",8).
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dTotal,"->>,>>9.99")),iPageWidth - iLeftMargin - bredd(TRIM(STRING(dTotal,"->>,>>9.99"))),dY - 48).
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
    /* På vanlige ordre skal bare aktive linjer summeres. */
    IF KOrdreHode.SendingsNr <> 'RETUR' THEN
    DO:
      IF bufKOL.Aktiv = FALSE THEN
        NEXT.
    END.
    /* På returordre hvor vare er byttet på varelinje, skal begge linjene med. */
    ELSE IF KOrdreHode.SendingsNr = 'RETUR' AND bufKOL.ByttetKOrdreLinjeNr > 0 THEN 
    DO:
      /* Begge linjene skal med */
    END.
    /* På returordre, skal linjer hvor vare er endret på returordre, men ikke på opprinnelig ordre med. */
    ELSE IF KOrdreHode.SendingsNr = 'RETUR' AND bufKOL.KopiKOrdreLinjeNr > 0 THEN
    DO:
      FIND bufKOrdreLinje NO-LOCK WHERE
        bufKOrdreLinje.KOrdre_Id = KOrdreHode.RefKOrdre_Id AND
        bufKOrdreLinje.KOrdreLinjeNr = bufKOL.KOrdreLinjeNr NO-ERROR.
      IF AVAILABLE bufKORdreLinje AND bufKOrdreLinje.KopiKOrdreLinjeNr > 0 THEN
       NEXT.
    END.
    ELSE IF KOrdreHode.SendingsNr = 'RETUR' AND bufKOL.KopiKOrdreLinjeNr = 0 AND bufKOL.aktiv = TRUE THEN
    DO:
      /* Skal være med.  Dvs. ikke noe Next her. :) */.
    END.
    /* Skal ikke med. Er nå passive linjer på returordre hvor vare ikke er byttet. */
    ELSE DO:
      NEXT.
    END.
     
    IF bufKOL.antall = 0 OR bufKOL.Varenr MATCHES "*BETALT*" THEN
        NEXT.
    IF bufKOL.Varetekst MATCHES "*FRAKT*" THEN
        ASSIGN dFrakt  = dFrakt  + bufKOL.nettolinjesum
               dMoms   = dMoms   + bufKOL.MVaKr.
    ELSE
        ASSIGN dBrutto = dBrutto + (bufKOL.antall * ABS(bufKOL.bruttopris))
               dRabatt = dRabatt + (bufKOL.Linjerabatt * -1)
               dMoms   = dMoms   + bufKOL.MVaKr 
               dNetto  = dNetto  + bufKOL.nettolinjesum.
               
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Varenr: ' + bufKOL.VareNr + '.'  
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    dBrutto: ' + STRING(dBrutto) + ' Antall: ' + STRING(bufKOL.antall) + ' Bruttopris: ' + STRING(bufKOL.bruttopris) + ' Produkt: ' + STRING(bufKOL.antall * bufKOL.bruttopris)   
      ).    
               
  END.
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
         cOrsaksKod[4] = "13,,,43"
         cOrsaksKod[5] = ",,,44".
  ASSIGN cOrsaksTxt[1] = "For liten,Feil vare levert,Feil på varen,Farge ikke som forventet"
         cOrsaksTxt[2] = "For stor,Feil størrelse levert,Avvik fra beskrivelse,Stoffet var ikke som forventet"
         cOrsaksTxt[3] = "Feil passform,For sent levert,,Matchet ikke"
         cOrsaksTxt[4] = "For kort,,,Skiftet mening"
         cOrsaksTxt[5] = ",,,Gave som ikke passet".

/*  iPageHeight = 612. 
    iPageWidth  = 842 */

  dY = iStartRow. /* 584 */
  RUN pdf_set_font ("Spdf", "Helvetica-Bold",24).


  DO:
    RUN pdf_set_font ("Spdf", "GantModern-Bold",7).
    d2 = 167.
    RUN pdf_text_xy_dec ("Spdf","Vær oppmerksom:",iLeftmargin,d2).
    RUN pdf_set_font ("Spdf", "GantModern-Regular",8).
    RUN pdf_text_xy_dec ("Spdf","Dersom du ikke fyller ut skjema korrekt kan din ordrebehandling ta lenger tid.",iLeftmargin,d2 - 10).
    RUN pdf_text_xy_dec ("Spdf","Varer som returneres etter 14 dager fra mottaksdato kan ikke returneres.",iLeftmargin,d2 - 20).
    RUN pdf_text_xy_dec ("Spdf","Varer som er forseglet kan ikke returneres dersom forsegling er brutt",iLeftmargin,d2 - 30).
    RUN pdf_line ("Spdf",iLeftMargin,d2 - 40,pdf_PageWidth("Spdf") / 2 - iLeftmargin,d2 - 40 ,0.5).
    RUN pdf_set_font ("Spdf", "GantModern-Bold",8).
    d2 = 100.
    RUN pdf_text_xy_dec ("Spdf","ÅRSAKSNUMMER",iLeftmargin,d2).
    RUN pdf_set_font ("Spdf", "GantModern-Regular",8).
    RUN pdf_text_xy_dec ("Spdf","STØRRELSE",iLeftmargin + iColPos[1],d2 - 13).
    RUN pdf_text_xy_dec ("Spdf","SERVICE",iLeftmargin + iColPos[2],d2 - 13).
    RUN pdf_text_xy_dec ("Spdf","KVALITET",iLeftmargin + iColPos[3],d2 - 13).
    RUN pdf_text_xy_dec ("Spdf","ANNET",iLeftmargin + iColPos[4],d2 - 13).

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
/*         cTxt = "Dersom du ønsker å bytte eller returnere varer pï¿½ din ordre".                  */
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
DEFINE VARIABLE cDatoKlokkeslett AS CHARACTER   NO-UNDO.
/*  iPageHeight = 612. 
    iPageWidth  = 842 */

  dY = iStartRow. /* 584 */
  RUN pdf_set_font ("Spdf", "Helvetica-Bold",24).
  DO:
      RUN pdf_place_image2 IN h_PDFinc ("Spdf",
                                       "HEADERLOGO",
                                       iLeftMargin, 
                                       30,
                                       pdf_ImageDim ("Spdf","HEADERLOGO","WIDTH") * .25,
                                       pdf_ImageDim ("Spdf","HEADERLOGO","HEIGHT") * .25).
    RUN pdf_set_font ("Spdf", "GantModern-Bold",9).
    RUN pdf_text_xy_dec ("Spdf","BYTTE- OG RETURSKJEMA",iLeftMargin,dY - 20).

    RUN pdf_set_font ("Spdf", "GantModern-Medium",9).
    RUN pdf_text_xy_dec ("Spdf",cLevRad_1,iLeftMargin,dY - 35).
    RUN pdf_text_xy_dec ("Spdf",cLevRad_2,iLeftMargin,dY - 45).
    RUN pdf_text_xy_dec ("Spdf",cLevRad_3,iLeftMargin,dY - 55).
    RUN pdf_text_xy_dec ("Spdf",cLevRad_4,iLeftMargin,dY - 65).

    RUN pdf_set_font ("Spdf", "GantModern-Regular",9).
    RUN pdf_text_xy_dec ("Spdf","Fakturanummer:",255,dY).
    RUN pdf_text_xy_dec ("Spdf","Dato, klokkeslett:",255,dY - 12).
    RUN pdf_text_xy_dec ("Spdf","Kunde:",255,dY - 24).
    RUN pdf_set_font ("Spdf", "GantModern-Bold",8).
    RUN pdf_text_xy_dec ("Spdf",TRIM(cOrdreNr),iRMarginPos - 420 - bredd(TRIM(cOrdreNr)),dY).
    cDatoKlokkeslett = STRING(dDato,"99.99.99") + ", " + STRING(KOrdrehode.RegistrertTid,"HH:MM").
    RUN pdf_text_xy_dec ("Spdf",cDatoKlokkeslett,iRMarginPos - 420 - bredd(cDatoKlokkeslett),dY - 12).
    RUN pdf_text_xy_dec ("Spdf",cKundRad_1,iRMarginPos - 420 - bredd(cKundRad_1),dY - 24).

/*     RUN pdf_text_xy_dec ("Spdf",STRING(dDato,"99.99.99"),iRMarginPos - 420 - bredd(STRING(dDato,"99.99.99")),dY - 12). */

    DO:
      RUN  pdf_set_font ("Spdf","Code39",16.0).
      RUN pdf_set_parameter("Spdf","ScaleY","2.0").
      cBarcode = "*" + "KO" + cKOrdreID + "*".
      RUN  pdf_text_xy ("Spdf",cBarcode,iRMarginPos - 470 - bredd(cKordreID), dY - 57).
/*       RUN  pdf_text_xy ("Spdf","*" + "KO" + cKOrdreID + "*",iRMarginPos - 480 - bredd(cKOrdreID), dY - 45). */
/*       RUN  pdf_text_xy ("Spdf",cBarcode,iRMarginPos - 420 - bredd(cBarCode), dY - 40). */
      RUN pdf_set_parameter("Spdf","ScaleY","1").
      RUN pdf_set_font ("Spdf", "GantModern-Regular",9).
      RUN  pdf_text_xy ("Spdf",cbarcode,iRMarginPos - 470 - bredd(cbarcode), dY - 67).
    END.

    RUN pdf_line ("Spdf",iLeftMargin,dY - 75,pdf_PageWidth("Spdf") / 2 - iLeftmargin,dy - 75 ,0.5).
    RUN pdf_set_font ("Spdf", "GantModern-Bold",9).
    RUN pdf_text_xy_dec ("Spdf","Jeg ønsker å bytte eller returnere følgende",iLeftmargin,dY - 93).
    RUN pdf_set_font ("Spdf", "GantModern-Regular",9).
    RUN pdf_text_xy_dec ("Spdf","Bytte- og returskjema rives av og legges ved din retur.",iLeftmargin,dY - 106).
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

&IF DEFINED(EXCLUDE-PageHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PageHeader Procedure 
PROCEDURE PageHeader :
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
         cOrsaksKod[4] = "13,,,43"
         cOrsaksKod[5] = ",,,44".
  ASSIGN cOrsaksTxt[1] = "For liten,Feil vare levert,Feil på varen,Farge ikke som forventet"
         cOrsaksTxt[2] = "For stor,Feil størrelse levert,Avvik fra beskrivelse,Stoffet var ikke som forventet"
         cOrsaksTxt[3] = "Feil passform,For sent levert,,Matchet ikke"
         cOrsaksTxt[4] = "For kort,,,Skiftet mening"
         cOrsaksTxt[5] = ",,,Gave som ikke passet".

/*  iPageHeight = 612. 
    iPageWidth  = 842 */

  dY = iStartRow. /* 584 */
/*   RUN pdf_set_font ("Spdf", "Helvetica-Bold",24). */

/*     PUT UNFORMATTED "Weborder reservasjon" SKIP   */
/*         "Webbutikk  :" cWebbutikknavn        SKIP */
/*         "Lagerbutikk:" tt_reserver.navn SKIP      */
/*         "Notat      :" cOrdrenrTekst SKIP         */
/*         .                                         */

    /* Collabels */
/*   RUN pdf_text_align ("Spdf","Reservasjon nettbutikk","CENTER", iLeftMargin, iRMarginPos). */
  /* left side */
  DO:
      RUN pdf_place_image2 IN h_PDFinc ("Spdf",
                                       "HEADERLOGO",
                                       iLeftMargin, 
                                       30,
                                       pdf_ImageDim ("Spdf","HEADERLOGO","WIDTH") * .25,
                                       pdf_ImageDim ("Spdf","HEADERLOGO","HEIGHT") * .25).
    RUN pdf_set_font ("Spdf", "GantModern-Bold",9).
    RUN pdf_text_xy_dec ("Spdf","BYTTE- OG RETURSKJEMA",iLeftMargin,dY - 20).

    RUN pdf_set_font ("Spdf", "GantModern-Medium",9).
    RUN pdf_text_xy_dec ("Spdf",cLevRad_1,iLeftMargin,dY - 35).
    RUN pdf_text_xy_dec ("Spdf",cLevRad_2,iLeftMargin,dY - 45).
    RUN pdf_text_xy_dec ("Spdf",cLevRad_3,iLeftMargin,dY - 55).
    RUN pdf_text_xy_dec ("Spdf",cLevRad_4,iLeftMargin,dY - 65).

    RUN pdf_set_font ("Spdf", "GantModern-Regular",9).
    RUN pdf_text_xy_dec ("Spdf","Ordrenummer:",260,dY).
    RUN pdf_text_xy_dec ("Spdf","Ordredato:",260,dY - 12).
    RUN pdf_set_font ("Spdf", "GantModern-Bold",9).
    RUN pdf_text_xy_dec ("Spdf",STRING(dOrdreNr),iRMarginPos - 420 - bredd(STRING(dOrdreNr)),dY).
    RUN pdf_text_xy_dec ("Spdf",STRING(dDato,"99.99.99"),iRMarginPos - 420 - bredd(STRING(dDato,"99.99.99")),dY - 12).
    RUN pdf_line ("Spdf",iLeftMargin,dY - 75,pdf_PageWidth("Spdf") / 2 - iLeftmargin,dy - 75 ,0.5).
    RUN pdf_set_font ("Spdf", "GantModern-Bold",9).
    RUN pdf_text_xy_dec ("Spdf","Jeg ønsker å bytte eller returnere følgende",iLeftmargin,dY - 93).
    RUN pdf_set_font ("Spdf", "GantModern-Regular",9).
    RUN pdf_text_xy_dec ("Spdf","Bytte- og returskjema rives av og legges ved din retur.",iLeftmargin,dY - 106).
    
    RUN pdf_set_font ("Spdf", "GantModern-Bold",7).
    d2 = 167.
    RUN pdf_text_xy_dec ("Spdf","Vær oppmerksom:",iLeftmargin,d2).
    RUN pdf_set_font ("Spdf", "GantModern-Regular",8).
    RUN pdf_text_xy_dec ("Spdf","Dersom du ikke fyller ut skjema korrekt kan din ordrebehandling ta lenger tid.",iLeftmargin,d2 - 10).
    RUN pdf_text_xy_dec ("Spdf","Varer som returneres etter 14 dager fra mottaksdato kan ikke returneres.",iLeftmargin,d2 - 20).
    RUN pdf_text_xy_dec ("Spdf","Varer som er forseglet kan ikke returneres dersom forsegling er brutt",iLeftmargin,d2 - 30).
    RUN pdf_line ("Spdf",iLeftMargin,d2 - 40,pdf_PageWidth("Spdf") / 2 - iLeftmargin,d2 - 40 ,0.5).
    RUN pdf_set_font ("Spdf", "GantModern-Bold",8).
    d2 = 100.
    RUN pdf_text_xy_dec ("Spdf","ÅRSAKSNUMMER",iLeftmargin,d2).
    RUN pdf_set_font ("Spdf", "GantModern-Regular",8).
    RUN pdf_text_xy_dec ("Spdf","STØRRELSE",iLeftmargin + iColPos[1],d2 - 13).
    RUN pdf_text_xy_dec ("Spdf","SERVICE",iLeftmargin + iColPos[2],d2 - 13).
    RUN pdf_text_xy_dec ("Spdf","KVALITET",iLeftmargin + iColPos[3],d2 - 13).
    RUN pdf_text_xy_dec ("Spdf","ANNET",iLeftmargin + iColPos[4],d2 - 13).

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
        RUN pdf_set_font ("Spdf", "GantModern-Bold",9).
        cTxt = "Dersom du ønsker å bytte eller returnere varer på din ordre".
        RUN pdf_text_xy_dec ("Spdf",cTxt,iLMp2 - iLeftMargin + iMittenR - bredd(cTxt) / 2,50).
        cTxt = "river du av og fyller ut 'Bytte- og Returskjema' på siste side.".
        RUN pdf_text_xy_dec ("Spdf",cTxt,iLMp2 - iLeftMargin + iMittenR - bredd(cTxt) / 2,37).
/*         "river du av og fyller ut "Bytte- og Returskjema" på siste side." */
/*                                                                           */

/* RUN pdf_markup("Spdf","My Markup Text","My Markup Title","Highlight",10.0,10.0,20,0,30.0,10.0,100.0,20.0,100.01,0.0,0.0). */

        RUN pdf_set_font ("Spdf", "GantModern-Regular",7).
        cTxt = "GANT Retail AS, Nesset 5, 3470 Slemmestad - Org.nr: 985 383 383".
        RUN pdf_text_xy_dec ("Spdf",cTxt,iLMp2 - iLeftMargin + iMittenR - bredd(cTxt) / 2,22).
    END.
    DO:
        RUN ReturTabell.
    END.
/*     DO ii = 10 TO 410 BY 20:                                                            */
/*         RUN pdf_text_xy_dec ("Spdf",STRING(iLeftmargin + ii),iLeftmargin + ii,d2 - 20). */
/*     END.                                                                                */
  END.
  /* rigth side */
  DO:
      RUN pdf_place_image2 IN h_PDFinc ("Spdf",
                                       "HEADERLOGO",
                                       iLMp2, 
                                       30,
                                       pdf_ImageDim ("Spdf","HEADERLOGO","WIDTH") * .25,
                                       pdf_ImageDim ("Spdf","HEADERLOGO","HEIGHT") * .25).

      RUN pdf_set_font ("Spdf", "GantModern-Bold",9).
      RUN pdf_text_xy_dec ("Spdf","ORDREOVERSIKT",iLMp2,dY - 20).
      
      RUN pdf_set_font ("Spdf", "GantModern-Medium",9).
      RUN pdf_text_xy_dec ("Spdf",cKundRad_1,iLMp2,dY - 35).
      RUN pdf_text_xy_dec ("Spdf",cKundRad_2,iLMp2,dY - 45).
      RUN pdf_text_xy_dec ("Spdf",cKundRad_3,iLMp2,dY - 55).
      RUN pdf_text_xy_dec ("Spdf",cKundRad_4,iLMp2,dY - 65).

      RUN pdf_set_font ("Spdf", "GantModern-Regular",9).
      RUN pdf_text_xy_dec ("Spdf","Ordrenummer:",680,dY).
      RUN pdf_text_xy_dec ("Spdf","Ordredato:",680,dY - 12).
      RUN pdf_set_font ("Spdf", "GantModern-Bold",9).
      RUN pdf_text_xy_dec ("Spdf",STRING(dOrdreNr),iRMarginPos - bredd(STRING(dOrdreNr)),dY).
      RUN pdf_text_xy_dec ("Spdf",STRING(dDato,"99.99.99"),iRMarginPos - bredd(STRING(dDato,"99.99.99")),dY - 12).
      

  END.

  RUN pdf_set_font ("Spdf", "GantModern-Bold",8).
  RUN pdf_text_xy_dec ("Spdf","HAR DU SPØRSMÅL?",680,dY - 35).
  RUN pdf_text_xy_dec ("Spdf","GANT Kundeservice",680,dY - 45).
  
  RUN pdf_set_font ("Spdf", "GantModern-Medium",7).
  
  RUN pdf_text_xy_dec ("Spdf","Åpningstider",680,dY - 55).
  RUN pdf_text_xy_dec ("Spdf","E-post",680,dY - 65).
  RUN pdf_text_xy_dec ("Spdf","Telefon",680,dY - 75).

  RUN pdf_set_font ("Spdf", "GantModern-Regular",7).

  RUN pdf_text_xy_dec ("Spdf","Man-fre, 10:00-16:00",740,dY - 55).
  RUN pdf_text_xy_dec ("Spdf","kundeservice@gant.no",740,dY - 65).
  RUN pdf_text_xy_dec ("Spdf","31 29 23 40",740,dY - 75).

  RUN pdf_set_font ("Spdf", "GantModern-Bold",7).
  RUN pdf_text_xy_dec ("Spdf","Betaling",iLMp2,dY - 105).
  
  RUN pdf_set_font ("Spdf", "wingding",10).
  RUN pdf_text_xy_dec ("Spdf","o",iLMp2,dY - 118).

  RUN pdf_set_font ("Spdf", "GantModern-Regular",8).
  RUN pdf_text_xy_dec ("Spdf","Kredittkort",iLMp2 + 10,dY - 118).
  RUN pdf_set_font ("Spdf", "wingding",10).
  RUN pdf_text_xy_dec ("Spdf","x",iLMp2 + 70,dY - 118).

  RUN pdf_set_font ("Spdf", "GantModern-Regular",8).
  RUN pdf_text_xy_dec ("Spdf","Faktura tilsendt monafuru@gmail.com",iLMp2 + 82,dY - 118).
  

  RUN pdf_skip ("Spdf").


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
  
Font      Storlek Sidlayout Førsta TO_Vænsterjusterat Sista_TO Sista_AT
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

    /* skapa ett utlï¿½gg pr butik */
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

  /*   RUN LoadFonts. */
  RUN pdf_load_font ("Spdf","GantModern-Regular",SEARCH("pdfinclude\GantModern-Regular.TTF"),SEARCH("pdfinclude\GantModern-Regular.AFM"),"").
  RUN pdf_load_font ("Spdf","GantModern-Medium",SEARCH("pdfinclude\GantModern-Medium.TTF"),SEARCH("pdfinclude\GantModern-Medium.AFM"),"").
  RUN pdf_load_font ("Spdf","GantModern-Bold",SEARCH("pdfinclude\GantModern-Bold.TTF"),SEARCH("pdfinclude\GantModern-Bold.AFM"),"").
  RUN pdf_load_font ("Spdf","Wingding",SEARCH("pdfinclude\Wingding.TTF"),SEARCH("pdfinclude\Wingding.AFM"),"").
  cBarCode = "1234567891234567890123".

  IF cBarcode <> "" THEN
      RUN pdf_load_font IN h_PDFinc ("Spdf","Code39",SEARCH("PDFinclude\samples\support\code39.ttf"),SEARCH("PDFinclude\samples\support\code39.afm"),""). 

  RUN new_page.
  iColLabelPage = 1.

iColLabelPage = 1.
  DO:
/*       RUN Refline. */

/*       RUN PageHeader.  */
/*       RUN ColLabels. */
      dYR = iStartRow - 140.
      dYL = 438.
      RUN Splitter.
      RUN RightHeader.
      RUN RightFooter.
      RUN LeftHeader.
      RUN ReturTabell(dYL).
      RUN LeftFooter.
      RUN ColLabels.
      FOR EACH tt_KLinje OF KOrdreHode 
          BY tt_KLinje.cVareNr 
          BY tt_KLinje.LevFargKod:
            
          IF tt_KLinje.VareNr = "BETALT" THEN
              NEXT.
          IF tt_KLinje.VareNr = "FRAKT" THEN
              NEXT.
          
          dYR = dYR - 15.
          RUN SkrivDataRight (dYR).
          IF tt_KLinje.Nettolinjesum > 0 THEN DO:
              DO ii = 1 TO tt_KLinje.Antall:
                  dYL = dYL - 15.
                  RUN SkrivDataLeft (dYL).
              END.
          END.
          iTmpRad = iTmpRad + 1.
          IF lFler AND iTmpRad = iMaxRader THEN DO:
              RUN VertLines (436,dYL - 5).
              iTmpRad = 0.
              RUN new_page.
              dYR = iStartRow - 140.
              dYL = 438.
              RUN Splitter.
              RUN RightHeader.
              RUN RightFooter.
              RUN LeftHeader.
              RUN ReturTabell(dYL).
        
              RUN LeftFooter.
              RUN ColLabels.
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
    ASSIGN iTblCols[1] = 2
/*            iTblCols[2] = 44 */
           iTblCols[2] = 46
           iTblCols[3] = 70
           iTblCols[4] = 208
           iTblCols[5] = 242
           iTblCols[6] = 339
           cTblColLabels [1] = "Artnr"
           cTblColLabels [2] = "Farge"
           cTblColLabels [3] = "Artnavn"
           cTblColLabels [4] = "Str"
           cTblColLabels [5] = "Retur/Bytte (Kryss av en)"
           cTblColLabels [6] = "Årsaknr".

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
DEFINE VARIABLE iColPos          AS INTEGER   EXTENT 4  NO-UNDO.
DEFINE VARIABLE cOrsaksKod       AS CHARACTER EXTENT 5  NO-UNDO.
DEFINE VARIABLE cOrsaksTxt       AS CHARACTER EXTENT 5  NO-UNDO.
DEFINE VARIABLE cTxt             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDatoKlokkeslett AS CHARACTER   NO-UNDO.

  dY = iStartRow. /* 584 */
  /* rigth side */
  DO:
      RUN pdf_place_image2 IN h_PDFinc ("Spdf",
                                       "HEADERLOGO",
                                       iLMp2, 
                                       30,
                                       pdf_ImageDim ("Spdf","HEADERLOGO","WIDTH") * .25,
                                       pdf_ImageDim ("Spdf","HEADERLOGO","HEIGHT") * .25).

      RUN pdf_set_font ("Spdf", "GantModern-Bold",9).
      RUN pdf_text_xy_dec ("Spdf","KONTANTFAKTURA",iLMp2,dY - 20).
      
      RUN pdf_set_font ("Spdf", "GantModern-Medium",9).

      RUN pdf_text_xy_dec ("Spdf","Leveransadresse:",iLMp2,dY - 31).
      RUN pdf_text_xy_dec ("Spdf",cKundRad_1,iLMp2,dY - 42).
      RUN pdf_text_xy_dec ("Spdf",cKundRad_2,iLMp2,dY - 52).
      RUN pdf_text_xy_dec ("Spdf",cKundRad_3,iLMp2,dY - 62).
/*       RUN pdf_text_xy_dec ("Spdf",cKundRad_4,iLMp2,dY - 72). */
      RUN pdf_text_xy_dec ("Spdf","Fakturaadresse:",iLMp2,dY - 75).
      RUN pdf_text_xy_dec ("Spdf",cKundFakt_1,iLMp2,dY - 86).
      RUN pdf_text_xy_dec ("Spdf",cKundFakt_2,iLMp2,dY - 96).

      RUN pdf_set_font ("Spdf", "GantModern-Regular",9).
/*       RUN pdf_text_xy_dec ("Spdf","Ordrenummer:",680,dY). */
      RUN pdf_text_xy_dec ("Spdf","Fakturanummer:",680,dY).
      RUN pdf_text_xy_dec ("Spdf","Dato, klokkeslett:",680,dY - 12).
      RUN pdf_set_font ("Spdf", "GantModern-Bold",8).
      RUN pdf_text_xy_dec ("Spdf",TRIM(cOrdreNr),iRMarginPos - bredd(TRIM(cOrdreNr)),dY).
      cDatoKlokkeslett = STRING(dDato,"99.99.99") + ", " + STRING(KOrdrehode.RegistrertTid,"HH:MM").
      RUN pdf_text_xy_dec ("Spdf",cDatoKlokkeslett,iRMarginPos - bredd(cDatoKlokkeslett),dY - 12).
      

  END.

  RUN pdf_set_font ("Spdf", "GantModern-Bold",8).
  RUN pdf_text_xy_dec ("Spdf","HAR DU SPØRSMÅL?",680,dY - 35).
  RUN pdf_text_xy_dec ("Spdf","GANT Kundeservice",680,dY - 45).
  
  RUN pdf_set_font ("Spdf", "GantModern-Medium",7).
  
  RUN pdf_text_xy_dec ("Spdf","Åpningstider",680,dY - 55).
  RUN pdf_text_xy_dec ("Spdf","E-post",680,dY - 65).
  RUN pdf_text_xy_dec ("Spdf","Telefon",680,dY - 75).
/*   RUN pdf_text_xy_dec ("Spdf","Klokkeslett",680,dY - 85). */

  RUN pdf_set_font ("Spdf", "GantModern-Regular",7).

  RUN pdf_text_xy_dec ("Spdf","Man-fre, 10:00-16:00",740,dY - 55).
  RUN pdf_text_xy_dec ("Spdf","kundeservice@gant.no",740,dY - 65).
  RUN pdf_text_xy_dec ("Spdf",Butiker.Telefaks,740,dY - 75).
/*   RUN pdf_text_xy_dec ("Spdf",STRING(KOrdrehode.RegistrertTid,"HH:MM"),740,dY - 85). */

  RUN pdf_set_font ("Spdf", "GantModern-Bold",7).
  RUN pdf_text_xy_dec ("Spdf","Betaling",iLMp2,dY - 107).
  FIND FIRST tt_KLinje OF KOrdreHode WHERE tt_KLinje.VareNr = "BETALT"  NO-LOCK NO-ERROR.
  IF AVAIL tt_KLinje THEN DO:
      RUN pdf_set_font ("Spdf", "wingding",10).
      RUN pdf_text_xy_dec ("Spdf","x",iLMp2,dY - 117).
      RUN pdf_set_font ("Spdf", "GantModern-Regular",8).
      RUN pdf_text_xy_dec ("Spdf",tt_KLinje.Varetekst + (IF tt_KLinje.Varetekst MATCHES "*klarna*" THEN " Faktura tilsendt " + KOrdreHode.ePostAdresse ELSE ""),iLMp2 + 10,dY - 117).
  END.
/*   IF Kordrehode.cOpt1 <> "" THEN DO:                           */
/*       RUN pdf_set_font ("Spdf", "wingding",10).                */
/*       RUN pdf_text_xy_dec ("Spdf","x",iLMp2,dY - 126).         */
/*       RUN pdf_set_font ("Spdf", "GantModern-Regular",8).       */
/*       RUN pdf_text_xy_dec ("Spdf","Gave",iLMp2 + 10,dY - 126). */
/*   END.                                                         */
/*   IF lExclusiveMember THEN DO:                                             */
/*       RUN pdf_set_font ("Spdf", "wingding",10).                            */
/*       RUN pdf_text_xy_dec ("Spdf","x",iLMp2,dY - 124).                     */
/*       RUN pdf_set_font ("Spdf", "GantModern-Regular",8).                   */
/*       RUN pdf_text_xy_dec ("Spdf","Exclusive medlem",iLMp2 + 10,dY - 124). */
/*   END.                                                                     */

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
/*       DEFINE VARIABLE cVareNr AS CHARACTER   NO-UNDO. */
/*     ASSIGN iTblCols[1] = 0                                  */
/*            iTblCols[2] = 42                                 */
/*            iTblCols[3] = 68                                 */
/*            iTblCols[4] = 208                                */
/*            iTblCols[5] = 242                                */
/*            iTblCols[6] = 337                                */
/*            cTblColLabels [1] = "Artnr"                      */
/*            cTblColLabels [2] = "Farge"                      */
/*            cTblColLabels [3] = "Artnavn"                    */
/*            cTblColLabels [4] = "Str"                        */
/*            cTblColLabels [5] = "Retur/Bytte (Kryss av \en)" */
/*            cTblColLabels [6] = "Årsaknr".                   */

      cFarge = tt_KLinje.LevFargKod.
      IF NUM-ENTRIES(cFarge,"/") > 1 THEN DO:
          FIND artbas WHERE artbas.artikkelnr = DECI(tt_KLinje.VareNr) NO-LOCK NO-ERROR.
          IF AVAIL artbas THEN
              cFarge = STRING(artbas.farg).
          ELSE
              cFarge = ENTRY(1,cFarge,"/").
      END.
      /* numer har vi en temptabell och cVareNr assignas i main */
/*       FIND artbas WHERE artbas.artikkelnr = DECI(tt_KLinje.VareNr) NO-LOCK NO-ERROR.                                  */
/*       cVarenr = IF AVAIL artbas AND TRIM(artbas.levkod) <> "" THEN artbas.levkod ELSE "I" + STRING(tt_KLinje.VareNr). */

      RUN pdf_set_font ("Spdf", "GantModern-Regular",7).


/*       RUN pdf_text_xy_dec ("Spdf",KOrdreLinje.VareNr,iLeftmargin + iTblCols[1],dY). */
      RUN pdf_text_xy_dec ("Spdf",tt_KLinje.cVareNr,iLeftmargin + iTblCols[1],dY).
      RUN pdf_text_xy_dec ("Spdf",cFarge,iLeftmargin + iTblCols[2],dY).
      RUN pdf_text_xy_dec ("Spdf",tt_KLinje.Varetekst,iLeftmargin + iTblCols[3],dY).
      RUN pdf_text_xy_dec ("Spdf",tt_KLinje.Storl,iLeftmargin + ((iTblCols[4] + iTblCols[5]) / 2) - bredd(tt_KLinje.Storl) / 2 - 2,dY).
      RUN pdf_set_font ("Spdf", "wingding",10).
      RUN pdf_text_xy_dec ("Spdf","o",iLeftmargin + iTblCols[5],dY).
      RUN pdf_set_font ("Spdf", "GantModern-Regular",6).
      RUN pdf_text_xy_dec ("Spdf","Retur",iLeftmargin + iTblCols[5] + 10 ,dY).
      RUN pdf_set_font ("Spdf", "wingding",10).
      RUN pdf_text_xy_dec ("Spdf","o",iLeftmargin + iTblCols[5] + 28,dY).
      RUN pdf_set_font ("Spdf", "GantModern-Regular",6).
      RUN pdf_text_xy_dec ("Spdf","Bytte mot str:",iLeftmargin + iTblCols[5] + 38 ,dY).


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
      DEFINE VARIABLE cFarge AS CHARACTER   NO-UNDO.
/*       DEFINE VARIABLE cVarenr AS CHARACTER   NO-UNDO. */
      cFarge = tt_KLinje.LevFargKod.
      IF NUM-ENTRIES(cFarge,"/") > 1 THEN DO:
          FIND artbas WHERE artbas.artikkelnr = DECI(tt_KLinje.VareNr) NO-LOCK NO-ERROR.
          IF AVAIL artbas THEN
              cFarge = STRING(artbas.farg).
          ELSE
              cFarge = ENTRY(1,cFarge,"/").
      END.
      /* numer har vi en temptabell och cVareNr assignas i main */
/*       FIND artbas WHERE artbas.artikkelnr = DECI(tt_KLinje.VareNr) NO-LOCK NO-ERROR.                                  */
/*       cVarenr = IF AVAIL artbas AND TRIM(artbas.levkod) <> "" THEN artbas.levkod ELSE "I" + STRING(tt_KLinje.VareNr). */


      RUN pdf_set_font ("Spdf", "GantModern-Regular",7).

/*       RUN pdf_text_xy_dec ("Spdf",KOrdreLinje.VareNr,iLMp2 + iCols[1],dY). */
      RUN pdf_text_xy_dec ("Spdf",tt_KLinje.cVarenr,iLMp2 + iCols[1],dY).
      RUN pdf_text_xy_dec ("Spdf",cFarge,iLMp2 + iCols[2],dY).
      RUN pdf_text_xy_dec ("Spdf",tt_KLinje.Varetekst,iLMp2 + iCols[3],dY).
      RUN pdf_text_xy_dec ("Spdf",tt_KLinje.Storl,iLMp2 + iCols[4] + 5 - bredd(tt_KLinje.Storl) / 2,dY).
      RUN pdf_text_xy_dec ("Spdf",tt_KLinje.Antall,iLMp2 + iCols[5] + 13 - bredd(STRING(tt_KLinje.Antall)),dY).
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(tt_KLinje.LinjeRabattKr,"->>,>>9.99")),iLMp2 + iCols[6] + bredd(cColLabels[6]) - bredd(TRIM(STRING(tt_KLinje.LinjeRabattKr,"->>,>>9.99"))),dY).
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(tt_KLinje.NettoLinjesum,"->>,>>9.99")),iPageWidth - iLeftmargin - bredd(TRIM(STRING(tt_KLinje.NettoLinjesum,"->>,>>9.99"))),dY).

      RUN pdf_line ("Spdf",iLMp2,dY - 5,iPageWidth - iLeftmargin,dY - 5,0.5).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivGave) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivGave Procedure 
PROCEDURE SkrivGave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cGaveTxt AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER dYL AS DECIMAL     NO-UNDO. 
DEFINE INPUT  PARAMETER dLeftCol  AS DECIMAL     NO-UNDO.
DEFINE INPUT  PARAMETER dRightCol AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iRadCount AS INTEGER     NO-UNDO.
DEFINE VARIABLE cc AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNy AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c2 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DEFINE VARIABLE iGodkand AS INTEGER     NO-UNDO.
DEFINE VARIABLE i2 AS INTEGER     NO-UNDO.
iGodkand = dRightCol - dLeftCol.
DO ii = 1 TO NUM-ENTRIES(cGaveTxt,CHR(10)):
    cc = ENTRY(ii,cGaveTxt,CHR(10)).
    cc = TRIM(REPLACE(cc,"~\r","")).
    IF bredd(cc) < iGodkand THEN DO:
        RUN pdf_text_xy_dec ("Spdf",cc,dLeftCol,dYL - iRadCount).
        iRadCount = iRadCount + 12.
    END.
    ELSE DO:
         c2 = ENTRY(1,cc," ").
         DO i2 = 2 TO NUM-ENTRIES(cc," "):
             IF bredd(c2 + " " + ENTRY(i2,cc," ")) < iGodkand THEN DO:
                 c2 = c2 + " " + ENTRY(i2,cc," ").
                 IF i2 = NUM-ENTRIES(cc," ") THEN DO:
                     RUN pdf_text_xy_dec ("Spdf",c2,dLeftCol,dYL - iRadCount).
                     iRadCount = iRadCount + 12.
                 END.
             END.
             ELSE DO:
                 RUN pdf_text_xy_dec ("Spdf",c2,dLeftCol,dYL - iRadCount).
                 iRadCount = iRadCount + 12.
                 c2 = ENTRY(i2,cc," ").
                 IF i2 = NUM-ENTRIES(cc," ") THEN DO:
                     RUN pdf_text_xy_dec ("Spdf",c2,dLeftCol,dYL - iRadCount).
                     iRadCount = iRadCount + 12.
                 END.
             END.
         END.
    END.
END.

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

