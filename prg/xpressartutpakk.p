&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xpressarttpakk.p
    Purpose     : Utpakking av artikkelinformasjon fra MEgaDisk.

    Syntax      :

    Description : Fra MegaDisk leveres en fil som inneholder:

    Author(s)   : Tom Nøkleby
    Created     : 8/4-03
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT PARAMETER lFilId AS DEC NO-UNDO.

DEF VAR iTotAntLinjer AS INT NO-UNDO.
DEF VAR iCL           AS INT NO-UNDO.
DEF VAR lLapTop       AS LOG NO-UNDO.
DEF VAR iProfilNr     AS INT NO-UNDO.
DEF VAR iAntNya       AS INT NO-UNDO.
DEF VAR iAntUppdat    AS INT NO-UNDO.
DEF VAR iAntNyaAvd    AS INT NO-UNDO.
DEF VAR iAntNyaHg     AS INT NO-UNDO.
DEF VAR iAntFel       AS INT NO-UNDO.
DEF VAR iStdLevNr     AS INT NO-UNDO.

DEF VAR h_PrisKo      AS HANDLE NO-UNDO.
DEF VAR rArtBasRecid  AS RECID  NO-UNDO.
DEF VAR cEndelse      AS CHAR   NO-UNDO.
DEF VAR cTekst        AS CHAR   NO-UNDO.

DEF VAR dcValPris     AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcInnPris     AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcUtpris      AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcRabatt      AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcFrakt       AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.

DEF VAR cEDB-System        LIKE ImpKonv.EDB-System NO-UNDO.
DEF VAR cImpTabell         LIKE ImpKonv.Tabell     NO-UNDO.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .
{windows.i}

DEF VAR piAntFeil   AS INT  NO-UNDO.
DEF VAR bProfitBase AS LOG  NO-UNDO.

DEFINE TEMP-TABLE TT_Artikel
  /*  1  PRODUCT.PRODUCT_ID        */ FIELD Product_id      AS CHAR LABEL "Product_id"                                 
  /*  2  PRODUCT.DESCRIPT          */ FIELD Beskrivning     AS CHAR LABEL "Beskrivning"                                 
  /*  3  PRODUCT.BARCODE           */ FIELD ProdStrekkode   AS CHAR LABEL "ProdStrekkode"                                   
  /*  4  PRODUCT.VAT_CODE          */ FIELD Momskod         AS CHAR LABEL "Momskod"                                     
  /*  5  PRODUCT.PRODCAT_ID        */ FIELD Kategori        AS CHAR LABEL "Kategori"                                    
  /*  6  PRODUCT.PRODGR_ID         */ FIELD Varegruppe      AS CHAR LABEL "Varegruppe"                                
  /*  7  PRODUCT.STKUNIT_ID        */ FIELD Salgsenhet      AS CHAR LABEL "Salgsenhet"                             
  /*  8  P_SUPPL.MANUFACTID        */ FIELD Tillverkare     AS CHAR LABEL "Tillverkare" 
  /*  9  PRICEPERPR.PRICE          */ FIELD KedjansPris     AS CHAR LABEL "Kedjans pris" 
  /* 10  P_SUPPL.SUPP_ID           */ FIELD LevID           AS CHAR LABEL "Leverantörs-ID per (per leverantör)"
  /* 11  P_SUPPL.SUPP_PRICE        */ FIELD InprisLev       AS CHAR LABEL "Inpris per (per leverantör)"        
  /* 12  P_SUPPL.PRODUCT_ID        */ FIELD ArtNrLev        AS CHAR LABEL "Artikelnr leverantör"     
  /* 13  P_SUPPL.PRIORITY          */ FIELD Levprioritet    AS CHAR LABEL "Prioritet leverantör"     
  /* 14  PRODUCT.PRODUCT_NO        */ FIELD ArtikkelNr      AS CHAR LABEL "ArtikkelNr"
  /* 15  P_BCODE.BARCODE           */ FIELD Strekkode       AS CHAR LABEL "Strekkode"
  /* 16  PRODUCT.PANT_P_NO         */ FIELD PantArtikkelNr  AS CHAR LABEL "PantArtikkelNr"
  /* 17  PRICEPERPR.PRICELEVELID   */ FIELD PrisProfil      AS CHAR LABEL "Prisprofil"
  /* 18  PRODUCT.VOLUME            */ FIELD Volym           AS CHAR LABEL "Volym"
  FIELD intPrioritet    AS INTE LABEL "Konverterad til integer"
  FIELD LevNr           AS INTE LABEL "InterntLevnr"
  FIELD OrsakDel        AS CHAR LABEL "Orsak ej inläst"
  FIELD SkapaEAN        AS LOG  LABEL "Skapa EAN"
  FIELD Radnr           AS INTE LABEL "Ordningsföljd inläsning"
  FIELD lKod7388        AS logi    
      INDEX ArtNrPrio IS PRIMARY Artikkelnr intPrioritet ASCENDING
      INDEX OrsakDel   ArtikkelNr OrsakDel
      INDEX ProdStrekkode  ProdStrekkode
      INDEX Strekode Strekkode
      INDEX Radnr Radnr ASCENDING.


DEFINE TEMP-TABLE TT_ImportArtikel  LIKE TT_Artikel.
DEFINE TEMP-TABLE tmpStrekkode      LIKE Strekkode.

DEFINE TEMP-TABLE TT_Artpris
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    FIELD PrisProfil AS INTE
    FIELD Inpris AS CHAR
    FIELD Pris AS CHAR
    INDEX ArtProf ArtikkelNr PrisProfil.

DEFINE BUFFER bTT_ImportArtikel FOR TT_ImportArtikel.
/*       DE GAMLA FÄLTEN                                                                                                    */
/*   /*  1  PRODUCT.PRODUCT_ID        */ FIELD Artikkelnr2     AS CHAR LABEL "Artikkelnr2"                                  */
/*   /*  2  PRODUCT.DESCRIPT          */ FIELD Beskrivning     AS CHAR LABEL "Beskrivning"                                  */
/*   /*  3  PRODUCT.BARCODE           */ FIELD Streckkod       AS CHAR LABEL "Streckkod"                                    */
/*   /*  4  PRODUCT.ExternalProductNo */ FIELD ExtProdNo       AS CHAR LABEL "ExternalProductNo"                            */
/*   /*  5  PRODUCT.NumericProductId  */ FIELD NumProdId       AS CHAR LABEL "NumericProductId"                             */
/*   /*  6  PRODUCT.BC_TYPE_ID        */ FIELD Streckkodstyp   AS CHAR LABEL "Streckkodstyp"                                */
/*   /*  7  PRODUCT.VAT_CODE          */ FIELD Momskod         AS CHAR LABEL "Momskod"                                      */
/*   /*  8  PRODUCT.IS_EXT_CAT        */ FIELD Anskaffvara     AS CHAR LABEL "Anskaffningsvara"                             */
/*   /*  9  PRODUCT.FORMAT_ID         */ FIELD cFormat         AS CHAR LABEL "Format"                                       */
/*   /* 10  PRODUCT.PRODCAT_ID        */ FIELD Kategori        AS CHAR LABEL "Kategori"                                     */
/*   /* 11  PRODUCT.PRODCAT2ID        */ FIELD Farve           AS CHAR LABEL "Färg"                                         */
/*   /* 12  PRODUCT.PRODGR_ID         */ FIELD Prodgrupp       AS CHAR LABEL "Produktgrupp"                                 */
/*   /* 13  PRODUCT.FAMILY_ID         */ FIELD Familj          AS CHAR LABEL "Familj"                                       */
/*   /* 14  PRODUCT.IsTillUnitStock   */ FIELD Saljarlager     AS CHAR LABEL "Säljarlager"                                  */
/*   /* 15  PRODUCT.IsTillUnitSold    */ FIELD FsgRedovSlager  AS CHAR /* LABEL "Försäljningsredovisning av säljarlager" */ */
/*   /* 16  PRODUCT.COMMISS_P         */ FIELD Kommission%     AS CHAR LABEL "Kommission %"                                 */
/*   /* 17  PRODUCT.IS_COMMISS        */ FIELD KommissionTF    AS CHAR LABEL "Kommission (T/F)"                             */
/*   /* 18  PRODUCT.PACK_TYPE         */ FIELD TypAvPaket      AS CHAR LABEL "Typ av paket"                                 */
/*   /* 19  PRODUCT.PACK_QTY          */ FIELD AntIforp        AS CHAR LABEL "Antal i förpackning"                          */
/*   /* 20  PRODUCT.STKUNIT_ID        */ FIELD Lagerfordenhet  AS CHAR LABEL "Lagerförd enhet"                              */
/*   /* 21  PRODUCT.CONV_FACT         */ FIELD FsgEnh          AS CHAR LABEL "Försälj.enh."                                 */
/*   /* 22  PRODUCT.MANUFACTID        */ FIELD Tillverkare     AS CHAR LABEL "Tillverkare"                                  */
/*   /* 23  P_ONLINE.LOC_PRICE        */ FIELD LokaltPris      AS CHAR LABEL "Lokalt pris"                                  */
/*   /* 24  PRODUCT.OUR_PRICE         */ FIELD KedjansPris     AS CHAR LABEL "Kedjans pris"                                 */
/*   /* 25  P_ONLINE.ACT_PRICE        */ FIELD Kampanjpris     AS CHAR LABEL "Kamppris"                                     */
/*   /* 26  PRODUCT.REG_PRICE         */ FIELD Marknadspris    AS CHAR LABEL "Marknpris"                                    */
/*   /* 27  P_ONLINE.OUTPRICE         */ FIELD BerUtpPerBut    AS CHAR LABEL "Beräknat utpris per butik"                    */
/*   /* 28  PRODUCT.OUR_AUTOP         */ FIELD BerKolVartPr    AS CHAR LABEL "Beräkningskolumn för vårt pris"               */
/*   /* 29  P_ONLINE.ACT_AUTOP        */ FIELD BerKolForKamp   AS CHAR LABEL "Beräkningskolumn för kampanjpris"             */
/*   /* 30  PRODUCT.REG_AUTOP         */ FIELD BerKolOrdPris   AS CHAR LABEL "Beräkningskolumn för ordinarie pris"          */
/*   /* 31  PRODUCT.MAX_DISC_P        */ FIELD MaxRab%         AS CHAR LABEL "Max. rabatt %"                                */
/*   /* 32  P_ONLINE.ACT_START        */ FIELD KampStart       AS CHAR LABEL "Kamp.start"                                   */
/*   /* 33  P_ONLINE.ACT_STARTT       */ FIELD StartTid        AS CHAR LABEL "Starttid"                                     */
/*   /* 34  P_ONLINE.ACT_CLOSE        */ FIELD KampStopp       AS CHAR LABEL "Kamp.stopp"                                   */
/*   /* 35  P_ONLINE.ACT_CLOSET       */ FIELD Sluttid         AS CHAR LABEL "Sluttid"                                      */
/*   /* 36  P_SUPPL.SUPP_ID           */ FIELD LevID           AS CHAR LABEL "Leverantörs-ID per (per leverantör)"          */
/*   /* 37  P_SUPPL.SUPP_PRICE        */ FIELD InprisLev       AS CHAR LABEL "Inpris per (per leverantör)"                  */
/*   /* 38  P_SUPPL.PRODUCT_ID        */ FIELD ArtNrLev        AS CHAR LABEL "Artikelnr per (per leverantör)"               */
/*   /* 39  P_SUPPL.BARCODE           */ FIELD StreckkodLev    AS CHAR LABEL "Streckkod per (per leverantör)"               */
/*   /* 40  P_SUPPL.PRIORITY          */ FIELD Levprioritet    AS CHAR LABEL "Prioritet per (per leverantör)"               */
/*   /* 41  Product_Id                */ FIELD ArtikkelNr      AS CHAR LABEL "ArtikkelNr"                                   */
/*   /* 42  P_BCode.Barcode           */ FIELD Strekkode2      AS CHAR LABEL "Strekkode2"                                   */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-SetLopeNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetLopeNr Procedure 
FUNCTION SetLopeNr RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* Centrallager */
{syspara.i 5 1 1 iCl INT}

FOR EACH tt_Error:
  DELETE tt_Error.
END.

/* Nullstiller loggfil. */
OUTPUT TO VALUE("ImpLogg.txt").
OUTPUT CLOSE.

SUBSCRIBE TO 'PBR' ANYWHERE.
{syspara.i 50 200 1 cTekst}
IF TRIM(cTekst) = "1" THEN
  bProfitBase = TRUE.
ELSE 
  bProfitBase = FALSE.

/* Start av procedurebibliotek */
IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN prisko.p PERSISTENT SET h_PrisKo.

/* Sjekker om det kjøres på en LapTop */
if valid-handle(h_dproclib) then
  run SjekkLapTop in h_dproclib (output lLapTop).
ELSE 
  lLapTop = FALSE.

/* Profilnr for sentrallager. */
FIND Butiker NO-LOCK WHERE
    Butiker.butik = iCl NO-ERROR.
IF NOT AVAILABLE Butiker THEN
DO:
    MESSAGE "Sentrallager " + STRING(iCL) + " er ikke lagt inn i butikkregister."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
END.
ASSIGN
    iProfilNr = Butiker.ProfilNr
    .

/* Filhode. */
FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    MESSAGE "Ingen VPIFilHode tilgjengelig"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

/* Henter ID for konvertering */
FIND EkstVPILev NO-LOCK WHERE
    EkstVPILev.EkstVPILevNr = VPIFilHode.EkstVPILevNr NO-ERROR.
IF NOT AVAILABLE EkstVPILev THEN
DO:
    MESSAGE "Ingen ekstern VPI leverandør tilgjengelig." SKIP
            "Id: " + STRING(VPIFilHode.EkstVPILevNr) + "."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
ASSIGN
    cEDB-System = EkstVPILev.KortNavn
    .

/* Datasett statuspost. */
FIND VPIDatasett NO-LOCK WHERE
    VPIDatasett.EkstVPILevNr = VPIFilHode.EkstVPILevNr NO-ERROR.
                                                
/* Er datasettet ferdig oppdatert? */
IF AVAILABLE VPIDatasett THEN
DO:
    IF VPIDatasett.DatasettStatus >= 3 AND
       VPIDatasett.DatasettStatus <= 4 THEN
    DO:
        MESSAGE "Forrige datasett er ikke oppdatert." SKIP
                "Det må være ferdig oppdatert før ny import av VPI kan gjøres."                
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
END.

/* Utpakking av Gruppeinndelinger.                       */
/* Her kommer også avdleinger, hovedgrupper og mvakoder. */
RUN UtpakkArt.

/* /* Kjører oppdatering av faste registre til ProfitBase. */ */
/* IF bProfitBase THEN                                        */
/* PROFITBASE:                                                */
/* DO:                                                        */
/*   RUN pfxoppdatfastereg.p.                                 */
/* END. /* PROFITBASE */                                      */

ON CLOSE OF THIS-PROCEDURE 
DO:
  IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_Prisko.
  RUN disable_UI.
END.

IF CAN-FIND(FIRST tt_Error) THEN
  RUN ErrorLogg.

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
  OUTPUT TO VALUE("Error.Txt").
    PUT UNFORMATTED
      "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn skip
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixKalkyle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixKalkyle Procedure 
PROCEDURE FixKalkyle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER iPrisprofil   AS INTEGER    NO-UNDO.
   DEFINE INPUT  PARAMETER cInprisLev    AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER cBerUtpPerBut AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE wSkjerm               AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE wFeltListe            AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE wFeltNr               AS INTEGER    NO-UNDO.
   ASSIGN wFeltListe = "ValPris,InnPris,Rab1,Rab1%,Rab2,Rab2%,Frakt,Frakt%," + 
                 "DivKost,DivKost%,Rab3,Rab3%,VareKost,DB,DB%," +
                 "FI-Mva,FI-Mva%,Pris,EU-Pris".
   IF NOT CAN-FIND(PrisProfil WHERE Prisprofil.ProfilNr = iPrisprofil) THEN DO:
       CREATE Prisprofil.
       ASSIGN Prisprofil.ProfilNr = iPrisprofil
              Prisprofil.Beskrivelse = "Prisprofil " + STRING(iPrisprofil)
              Prisprofil.KortNavn    = "PP" + STRING(iPrisprofil).
       RELEASE Prisprofil.
   END.
   FIND VarGr WHERE VarGr.Vg = ArtBas.Vg NO-LOCK.
   FIND Moms OF VarGr NO-LOCK.
   FIND ArtPris OF ArtBas WHERE ArtPris.Profilnr = iPrisProfil NO-LOCK NO-ERROR.
   IF AVAIL ArtPris THEN DO:
       IF ArtPris.InnkjopsPris[1] = DECI(cInprisLev) AND 
          ArtPris.Mva%[1]         = Moms.MomsProc    AND
          ArtPris.Pris[1]         = DECI(cBerUtpPerBut) THEN
           RETURN.
   if valid-handle(h_PrisKo) then
     run InitKalkyle in h_PrisKo
          (RECID(ArtBas), 
           iPrisprofil, /* prisprofil */
           input-output wSkjerm,
           Moms.MomsProc,
           1, /* Valuta.ValKurs */
           wFeltNr,
           FALSE).
       ASSIGN wSkjerm = wSkjerm + "0;;;0;;;0;0;".
   END.
   ELSE
       ASSIGN wSkjerm = "0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;;;0;;;0;0;". 
/* 760;760;0;0;0;0;0;0;0;0;0;0;760;380;25;760;50;1900;285;no;11/10/01;0;;;0;0;no */

   ASSIGN wFeltNr = LOOKUP("ValPris",wFeltListe) /* fält 1 */
          ENTRY(1,wSkjerm,";")  = cInprisLev
          ENTRY(17,wSkjerm,";") = STRING(Moms.MomsProc)
          ENTRY(18,wSkjerm,";") = cBerUtpPerBut.

   RUN Omregning IN h_PrisKo
        (RECID(ArtBas), 
         iPrisprofil, /* PrisProfil.ProfilNr */
         input-output wSkjerm,
         Moms.MomsProc,
         1, /* Valuta.ValKurs */
         wFeltNr,
         FALSE).
   ASSIGN wFeltNr = LOOKUP("Pris",wFeltListe) /* fält 1 */
         wSkjerm = wSkjerm + "0;;;0;;;0;0;".
   RUN Omregning IN h_PrisKo
        (RECID(ArtBas), 
         iPrisprofil, /* PrisProfil.ProfilNr */
         input-output wSkjerm,
         Moms.MomsProc,
         1, /* Valuta.ValKurs */
         wFeltNr,
         FALSE).
   ASSIGN wSkjerm = wSkjerm + "0;;;0;;;0;0;"
          ENTRY(20,wSkjerm,";") = "no"
          ENTRY(21,wSkjerm,";") = STRING(TODAY)
          ENTRY(27,wSkjerm,";") = "no".

   run LagreArtPris in h_PrisKo
       (RECID(ArtBas), 
        iPrisprofil, /* Profilnr*/
        INPUT-OUTPUT wSkjerm,
        FALSE, /* tilbud */
        TRUE,  /* plDirekte */
        TRUE,
        ?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Konvertering) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Konvertering Procedure 
PROCEDURE Konvertering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Konverterer EAN/Artikkelnummer  INTE FÖR 7Eleven*/
/*     KONVPLU:                                                                                           */
/*     DO:                                                                                                */
/*         FIND ImpKonv WHERE ImpKonv.EDB-System = cEDB-System   AND                                      */
/*                            ImpKonv.Tabell     = "Strekkode" AND                                        */
/*                            ImpKonv.EksterntID = TRIM(TT_ImportArtikel.ArtikkelNr) NO-LOCK NO-ERROR.    */
/*         IF AVAILABLE ImpKonv THEN                                                                      */
/*             TT_ImportArtikel.ArtikkelNr = ImpKonv.InterntId.                                           */
/*                                                                                                        */
/*         FIND ImpKonv WHERE ImpKonv.EDB-System = cEDB-System   AND                                      */
/*                            ImpKonv.Tabell     = "Strekkode" AND                                        */
/*                            ImpKonv.EksterntID = TRIM(TT_ImportArtikel.Product_id) NO-LOCK NO-ERROR.   */
/*         IF AVAILABLE ImpKonv THEN                                                                      */
/*             TT_ImportArtikel.Product_id = ImpKonv.InterntId.                                          */
/*                                                                                                        */
/*         FIND ImpKonv WHERE ImpKonv.EDB-System = cEDB-System   AND                                      */
/*                            ImpKonv.Tabell     = "Strekkode" AND                                        */
/*                            ImpKonv.EksterntID = TRIM(TT_ImportArtikel.ProdStreckkod) NO-LOCK NO-ERROR. */
/*         IF AVAILABLE ImpKonv THEN                                                                      */
/*             TT_ImportArtikel.ProdStreckkod = ImpKonv.InterntId.                                        */
/*                                                                                                        */
/*         FIND ImpKonv WHERE ImpKonv.EDB-System = cEDB-System   AND                                      */
/*                            ImpKonv.Tabell     = "Strekkode" AND                                        */
/*                            ImpKonv.EksterntID = TRIM(TT_ImportArtikel.Strekkode) NO-LOCK NO-ERROR.     */
/*         IF AVAILABLE ImpKonv THEN                                                                      */
/*             TT_ImportArtikel.Strekkode = ImpKonv.InterntId.                                            */
/*     END. /* KONVPLU */ */
    /* Konverterer varegruppe */
    KONVVG:
    DO:
        FIND ImpKonv WHERE ImpKonv.EDB-System = cEDB-System   AND
                           ImpKonv.Tabell     = "VarGr" AND
                           ImpKonv.EksterntID = TRIM(TT_ImportArtikel.Varegruppe) NO-LOCK NO-ERROR.
        IF AVAILABLE ImpKonv THEN
            TT_ImportArtikel.Varegruppe = ImpKonv.InterntId.
    END. /* KONVVG */

    /* Konverterer LevBas */
    KONVLEVBAS:
    DO:
        /* Konverterer leverandørnummer */
        FIND ImpKonv WHERE ImpKonv.EDB-System = cEDB-System   AND
                           ImpKonv.Tabell     = cImpTabell AND
                           ImpKonv.EksterntID = TRIM(TT_ImportArtikel.LevID) NO-LOCK NO-ERROR.
        /* För enkelhetens skull ändrar vi LevId till vårt interna levnr */
        /* och lägger det på ett extra fält LevNr */
        IF AVAIL ImpKonv THEN
            ASSIGN TT_ImportArtikel.LevNr = INT(ImpKonv.InterntId).
        ELSE DO:
            ASSIGN
                TT_ImportArtikel.LevNr    = iStdLevNr
                .
        END.
    END. /* KONVLEVBAS */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PBR) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PBR Procedure 
PROCEDURE PBR :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcTekst AS CHAR NO-UNDO.

  PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaPostNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaPostNr Procedure 
PROCEDURE SkapaPostNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cPostNr     AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cPostadress AS CHARACTER  NO-UNDO.
  CREATE Post.
  ASSIGN Post.FylkesNr    = "1"
         Post.KommNr      = "1"
         Post.PostNr      = cPostnr
         Post.Beskrivelse = cPostadress
         Post.Merknad     = "Skapad vid import".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaUppdaterArtikel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaUppdaterArtikel Procedure 
PROCEDURE SkapaUppdaterArtikel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Oppretter/oppdaterer poster i registrene */
    DEFINE VARIABLE dLinkvare   AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dMengde     AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE cInpris     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cUtPris     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cHovedStrek AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iLoop       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lAktivert   AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE lOpris      AS LOGICAL    NO-UNDO.
  FOR EACH TT_Artikel:
    ASSIGN dLinkvare = DECI(TT_Artikel.PantArtikkelNr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        ASSIGN dLinkVare = 0.
    ASSIGN dMengde = DECI(TT_Artikel.Volym) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        ASSIGN dMengde = 0.
    ASSIGN lAktivert = TT_Artikel.Kategori <> "U"
           lOpris    = TT_Artikel.lKod7388 = TRUE OR 
                              NOT CAN-FIND(FIRST TT_Artpris WHERE TT_Artpris.Artikkelnr = DECI(TT_Artikel.ArtikkelNr) AND DECI(TT_Artpris.Pris) > 0).
    REGISTEROPPDAT:
    DO:
        IF TT_Artikel.ArtikkelNr <> "" THEN
        DO:
            FIND ArtBas WHERE ArtBas.ArtikkelNr = DEC(TT_Artikel.ArtikkelNr) NO-ERROR.
        END.
        ELSE
            NEXT.
        FIND VarGr WHERE VarGr.Vg = INT(TT_Artikel.Varegruppe) NO-LOCK.
        IF NOT AVAIL ArtBas THEN 
        DO:
            /* Vi har skippat poster som inte har koppling till riktig VarGr tidigare */
            CREATE ArtBas.
            ASSIGN ArtBas.ArtikkelNr = dec(TT_Artikel.Artikkelnr)
                   ArtBas.Hg         = VarGr.Hg
                   ArtBas.Vg         = VarGr.Vg
                   ArtBas.LopNr      = ?
                   ArtBas.Beskr      = TT_Artikel.Beskrivning                   
                   ArtBas.BongTekst  = TT_Artikel.Beskrivning
                   ArtBas.LevNr      = TT_Artikel.LevNr
                   ArtBas.LevKod     = TT_Artikel.ArtNrLev
                   ArtBas.VgKat      = 1
                   ArtBas.StrTypeId  = 2
                   ArtBas.Valkod     = "SEK"
                   ArtBas.LinkVareNr = dLinkVare
                   ArtBas.Mengde     = dMengde
                   ArtBas.Aktivert   = lAktivert
                   ArtBas.Salgsenhet = TT_Artikel.Salgsenhet
                   ArtBas.Opris      = lOpris
                   ArtBas.Lager      = NOT ArtBas.Opris
                   iAntNya           = iAntNya + 1
                   .
            ASSIGN ArtBas.LopNr = SetLopeNr().
        END.
        ELSE DO:
            ASSIGN
                ArtBas.Beskr      = IF ArtBas.Beskr      <> TT_Artikel.Beskrivning THEN TT_Artikel.Beskrivning ELSE ArtBas.Beskr     
                ArtBas.Bongtekst  = IF ArtBas.Bongtekst  <> TT_Artikel.Beskrivning THEN TT_Artikel.Beskrivning ELSE ArtBas.Bongtekst 
                ArtBas.LevNr      = IF ArtBas.LevNr      <> TT_Artikel.LevNr       THEN TT_Artikel.LevNr       ELSE ArtBas.LevNr     
                ArtBas.LevKod     = IF ArtBas.LevKod     <> TT_Artikel.ArtNrLev    THEN TT_Artikel.ArtNrLev    ELSE ArtBas.LevKod    
                ArtBas.LinkVareNr = IF ArtBas.LinkVareNr <> dLinkVare              THEN dLinkVare              ELSE ArtBas.LinkVareNr
                ArtBas.Mengde     = IF ArtBas.Mengde     <> dMengde                THEN dMengde                ELSE ArtBas.Mengde    
                ArtBas.Aktivert   = IF Artbas.Aktivert <> lAktivert THEN lAktivert ELSE ArtBas.Aktivert
                ArtBas.Salgsenhet = IF ArtBas.Salgsenhet <> TT_Artikel.Salgsenhet THEN TT_Artikel.Salgsenhet ELSE ArtBas.Salgsenhet
                ArtBas.Opris      = IF ArtBas.Opris <> lOpris THEN lOpris ELSE ArtBas.Opris
                ArtBas.Lager      = IF ArtBas.Lager = lOpris THEN NOT lOpris ELSE ArtBas.Lager
                iAntUppdat        = iAntUppdat + 1
                .
        END.
        ASSIGN cInpris = ""
               cUtpris = "".
        IF ArtBas.Opris THEN DO:
            /* Vargr är avail */
            FIND Moms WHERE Moms.MomsKod = VarGr.MomsKod NO-LOCK.
            ASSIGN cInpris = STRING(VarGr.Kost_Proc,">9.99")
                   cUtPris = STRING(100 + Moms.MomsProc,">>9.99").
        END.
        FOR EACH TT_ArtPris WHERE TT_Artpris.ArtikkelNr = ArtBas.Artikkelnr:
            IF ArtBas.Opris = FALSE AND DECI(TRIM(TT_ArtPris.InPris)) = 0 THEN DO:
                FIND Moms WHERE Moms.MomsKod = VarGr.MomsKod NO-LOCK.
                ASSIGN cInpris = STRING(ROUND(DECI(TRIM(TT_ArtPris.Pris)) / (100 + Moms.MomsProc) * Vargr.Kost_proc,2)).
            END.
            ASSIGN cInpris = IF cInpris = "" THEN TRIM(TT_ArtPris.Inpris) ELSE cInpris
                   cUtPris = IF cUtpris = "" THEN TRIM(TT_ArtPris.Pris) ELSE cUtpris.
            RUN FixKalkyle (TT_Artpris.Prisprofil,cInpris,cUtPris).
            IF ArtBas.Opris = FALSE THEN
                ASSIGN cUtpris = "".
        END.  
        FIND CURRENT ArtBas NO-LOCK.
        IF CAN-FIND(FIRST tmpStrekKode OF ArtBas) THEN DO:
            ASSIGN iLoop = 0
                   cHovedStrek = "".
            /* Här skall vi för säkerhets skull sätta 1 och bara 1 strekkod som huvudnr */
            FOR EACH tmpStrekKode OF ArtBas WHERE tmpStrekKode.HovedNr = TRUE:
                ASSIGN iLoop = iLoop + 1.
                IF iLoop = 1 THEN
                    ASSIGN cHovedStrek = tmpStrekKode.Kode.
                IF iLoop > 1 THEN
                    ASSIGN tmpStrekKode.HovedNr = FALSE.
            END.
            IF iLoop = 0 AND NOT CAN-FIND(FIRST StrekKode OF ArtBas WHERE StrekKode.HovedNr = TRUE) THEN DO:
                FIND FIRST tmpStrekkode OF ArtBas WHERE tmpStrekKode.KodeType = 1 NO-ERROR.
                IF NOT AVAIL tmpStrekKode THEN
                    FIND FIRST tmpStrekkode OF ArtBas.
                ASSIGN tmpStrekKode.HovedNr = TRUE
                       cHovedStrek = tmpStrekKode.Kode.
            END.
            IF cHovedStrek <> "" THEN DO:
                FIND FIRST tmpStrekkode OF ArtBas WHERE tmpStrekKode.HovedNr = TRUE.
                    ASSIGN cHovedStrek = tmpStrekKode.Kode.
                FOR EACH StrekKode OF ArtBas.
                    IF Strekkode.Kode = tmpStrekKode.Kode THEN
                        ASSIGN StrekKode.HovedNr = TRUE.
                    ELSE
                        ASSIGN StrekKode.HovedNr = FALSE.
                END.
            END.
            /* STREKKODER opprettes */
            STREKKODER:
            FOR EACH tmpStrekkode OF ArtBas:
                IF NOT CAN-FIND(FIRST Strekkode OF Artbas WHERE Strekkode.Kode = tmpStrekkode.Kode) THEN
                DO:
                    CREATE StrekKode.
                    ASSIGN StrekKode.ArtikkelNr = tmpStrekkode.ArtikkelNr
                           StrekKode.HovedNr    = tmpStrekKode.HovedNr
                           StrekKode.Kode       = tmpStrekkode.Kode
                           StrekKode.KodeType   = tmpStrekKode.KodeType
                           StrekKode.StrKode    = tmpStrekKode.StrKode.
                    RELEASE StrekKode.
                END.
            END. /* STREKKODER */
        END.
    END. /* REGISTEROPPDAT */
  END.

    /* Här måste vi väl ta bort alla streckkoder */
    FOR EACH tmpStrekkode:
        DELETE tmpStrekKode.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StrekKode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StrekKode Procedure 
PROCEDURE StrekKode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER  pcArtikkelNr AS CHAR NO-UNDO.
  DEF INPUT PARAMETER  pcKode       AS CHAR NO-UNDO.
  DEF INPUT PARAMETER  lHovedNr      AS LOGICAL    NO-UNDO.

  DEF VAR dDeci AS DECI NO-UNDO.

  IF pcKode = "" THEN
      RETURN.

  IF NOT CAN-FIND(tmpStrekkode WHERE
                  tmpStrekkode.Kode = pcKode) THEN
  STREK:
  DO:
      IF lHovedNr = TRUE AND CAN-FIND(FIRST tmpStrekkode WHERE tmpStrekkode.ArtikkelNr = dec(pcArtikkelNr) and
                                      tmpStrekKode.HovedNr = TRUE) THEN
          ASSIGN lHovedNr = FALSE.
      ASSIGN dDeci = DECI(pcKode) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
          LEAVE STREK.
      CREATE tmpStrekkode.
      ASSIGN
          tmpStrekkode.ArtikkelNr = dec(pcArtikkelNr)
          tmpStrekkode.Kode       = pcKode
          tmpStrekKode.HovedNr    = lHovedNr
          tmpStrekKode.KodeType   = IF LENGTH(tmpStrekkode.Kode) > 5 THEN 1 ELSE 0
          tmpStrekKode.StrKode    = tmpStrekKode.KodeType
          .
  END. /* STREK */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UtPakkArt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtPakkArt Procedure 
PROCEDURE UtPakkArt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iSaknasLevNr AS INTEGER    NO-UNDO.
DEF VAR cDummy       AS CHARACTER  NO-UNDO.
DEF VAR dArtikkelNr  AS DECIMAL    NO-UNDO.
DEF VAR cArtikkelNr  AS CHARACTER  NO-UNDO.
DEF VAR dSteckkod    AS DECIMAL    NO-UNDO.
DEF VAR dStrekkode   AS DECIMAL    NO-UNDO.
DEF VAR iPriotest    AS INTEGER    NO-UNDO.
DEF VAR iRadNr       AS INTEGER    NO-UNDO.
DEF VAR pcLev        AS CHAR NO-UNDO.
DEF VAR piTid        AS INT  NO-UNDO.
DEF VAR iTotAnt      AS INT  NO-UNDO.
DEF VAR dProdIdTst   AS DECIMAL    NO-UNDO.
DEF VAR dStrekTst    AS DECIMAL    NO-UNDO.
DEF VAR cPantListe   AS CHARACTER  NO-UNDO.
DEF VAR iIntPrioritet AS INTE NO-UNDO.
DEFINE BUFFER bTT_Artikel FOR TT_Artikel.

DEF VAR piLev         AS INT  NO-UNDO.
DEF VAR pbStatus      AS LOG  NO-UNDO.
DEF VAR piAntKoblet   AS INT  NO-UNDO.
DEF VAR piAntOppdat   AS INT  NO-UNDO.
DEF VAR iTstNr        AS INT  NO-UNDO.
DEF VAR iVgTest       AS INTE NO-UNDO.
DEF VAR cNumericFormat AS CHAR NO-UNDO.
DEF VAR piLoop1       AS INT  NO-UNDO.

ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
       SESSION:NUMERIC-FORMAT = "American".

ASSIGN
    piTid     = TIME
    cTekst    = "Starter utpakking av artikkler."
    iStdLevNr = 0
    .
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

/* Aldri tilbud */
ASSIGN pbStatus = FALSE.

/* Oppretter VPIDatasett hvis det ikke finnes. */
IF NOT AVAILABLE VPIDatasett THEN
DO TRANSACTION:
    CREATE VPIDatasett.
    ASSIGN
        VPIDatasett.EkstVPILevNr   = VPIFilHode.EkstVPILevNr
        VPIDatasett.DatasettStatus = 1 /* Opprettet. */
        .
    FIND CURRENT VPIDatasett NO-LOCK.
END. /* TRANSACTION */

/* Tømmer temp-table */
FOR EACH TT_Artikel:
  DELETE TT_Artikel.
END.
FOR EACH bTT_Artikel:
  DELETE bTT_Artikel.
END.

/* Setter datasettets status. */
DO TRANSACTION:
  FIND CURRENT VPIDatasett EXCLUSIVE-LOCK.
  ASSIGN
      VPIDatasett.DatasettStatus = 2 /* VPI importeres */
      VPIDatasett.ImportDato     = TODAY
      VPIDatasett.ImportKl       = TIME
      VPIDatasett.FilId          = VPIFilHode.FilId
      .
  FIND CURRENT VPIDatasett NO-LOCK.
END. /* TRANSACTION */

ASSIGN
    iAntNya     = 0   iAntUppdat  = 0
    iAntNyaHg   = 0   iAntNyaAvd  = 0 iAntFel     = 0
/*     cErrorFil   = ENTRY(NUM-ENTRIES(cFilNavn,".") - 1,cFilNavn,".") + ".fel" */
    .

/* för koppling av varor som inte har någon lev satt. */
/* ----- Konverteringstabell - leverandør ----- */
{getvpitabell.i &LevNr = VPIFilHode.EkstVPILevNr
                &Nr = 1500
                &Felt = cImpTabell}
FIND FIRST ImpKonv NO-LOCK WHERE
    ImpKonv.EDB-System = cEDB-System AND
    ImpKonv.Tabell     = cImpTabell AND
    ImpKonv.EksterntId = "SAKNAS" NO-ERROR.
DO TRANSACTION:
IF NOT AVAIL ImpKonv THEN DO:
    FIND LAST LevBas NO-LOCK NO-ERROR.
    ASSIGN iSaknasLevNr = IF AVAIL LevBas THEN LevBas.LevNr + 1 ELSE 1.
    CREATE LevBas.
    ASSIGN LevBas.LevNr   = iSaknasLevNr
           LevBas.LevNamn = "SAKNAS - Arktikler med ukjent levnr."        
           LevBas.Notat   = 
        "Vissa i artiklar som inte har Levnr satt men" + CHR(10) +
        "i övrigt är godkända kopplas hit.".
    RELEASE LevBas.
    CREATE ImpKonv.
    ASSIGN 
        ImpKonv.EDB-System = cEDB-System
        ImpKonv.Tabell     = cImpTabell
        ImpKonv.EksterntID = "SAKNAS"
        ImpKonv.InterntID  = STRING(iSaknasLevNr)
        ImpKonv.Merknad    = STRING(TODAY)
        iStdLevNr          = iSaknasLevNr
        .
END.
ELSE
    ASSIGN 
        iSaknasLevNr = INT(ImpKonv.InterntId)
        iStdLevNr    = INT(ImpKonv.InterntId)
        .                                    
END.                                         
RELEASE ImpKonv.                             
                                             
/* Teller antall poster */                   
FOR EACH VPIFilLinje OF VPIFilHode NO-LOCK:  
  ASSIGN                                     
      iTotAnt = iTotAnt + 1                  
      .                                      
END.                                         
                                             
/* Behandler fillinjene */                   
VPIFILLINJE:                                 
FOR EACH VPIFilLinje OF VPIFilHode: 
    RELEASE TT_ImportArtikel.
    IF LENGTH(trim(ENTRY(15,VPIFilLinje.StorTekst,";"),'"')) > 13 THEN
        NEXT.
    IF trim(ENTRY(15,VPIFilLinje.StorTekst,";"),'"') = "" AND
       LENGTH(trim(ENTRY(1,VPIFilLinje.StorTekst,";"),'"')) > 13 THEN
    NEXT.

    /* Melding til brukeren. */                 
/*     IF AVAILABLE TT_ImportArtikel THEN */
/*         DELETE TT_ImportArtikel.       */
    ASSIGN dProdIdTst = DECI(trim(ENTRY( 1,VPIFilLinje.StorTekst,";"),'"')) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        ASSIGN dProdIdTst = 0.
    ASSIGN iVgTest = DECI(trim(ENTRY( 6,VPIFilLinje.StorTekst,";"),'"')) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        ASSIGN iVgTest = 0.
    ASSIGN dStrekTst = DECI(trim(ENTRY(15,VPIFilLinje.StorTekst,";"),'"')) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        ASSIGN dStrekTst = 0.
    ASSIGN iRadNr = iRadNr + 1.
    CREATE TT_ImportArtikel.
    ASSIGN
        cEndelse                       = ""
        piLoop1                        = piLoop1 + 1
        iTotAntLinjer                  = iTotAntLinjer + 1
        TT_ImportArtikel.Product_id      = IF dProdIdTst = 0 THEN "" ELSE trim(ENTRY( 1,VPIFilLinje.StorTekst,";"),'"') 
        TT_ImportArtikel.Beskrivning     = trim(ENTRY( 2,VPIFilLinje.StorTekst,";"),'"') 
        TT_ImportArtikel.ProdStrekkode   = trim(ENTRY( 3,VPIFilLinje.StorTekst,";"),'"') 
        TT_ImportArtikel.Momskod         = trim(ENTRY( 4,VPIFilLinje.StorTekst,";"),'"') 
        TT_ImportArtikel.Kategori        = trim(ENTRY( 5,VPIFilLinje.StorTekst,";"),'"') 
        TT_ImportArtikel.Varegruppe      = IF iVgTest = 0 THEN "" ELSE trim(ENTRY( 6,VPIFilLinje.StorTekst,";"),'"') 
        TT_ImportArtikel.Salgsenhet      = trim(ENTRY( 7,VPIFilLinje.StorTekst,";"),'"')
        TT_ImportArtikel.Salgsenhet      = IF TT_ImportArtikel.Salgsenhet = "St" THEN "Stk" ELSE IF TT_ImportArtikel.Salgsenhet = "Kilo" THEN
                                              "Kg" ELSE TT_ImportArtikel.Salgsenhet
        TT_ImportArtikel.Tillverkare     = trim(ENTRY( 8,VPIFilLinje.StorTekst,";"),'"') 
        TT_ImportArtikel.KedjansPris     = trim(ENTRY( 9,VPIFilLinje.StorTekst,";"),'"') 
        TT_ImportArtikel.LevID           = trim(ENTRY(10,VPIFilLinje.StorTekst,";"),'"') 
        TT_ImportArtikel.InprisLev       = trim(ENTRY(11,VPIFilLinje.StorTekst,";"),'"') 
        TT_ImportArtikel.ArtNrLev        = trim(ENTRY(12,VPIFilLinje.StorTekst,";"),'"') 
        TT_ImportArtikel.Levprioritet    = trim(ENTRY(13,VPIFilLinje.StorTekst,";"),'"') 
        TT_ImportArtikel.Levprioritet    = IF TT_ImportArtikel.Levprioritet = "0" THEN "9" ELSE TT_ImportArtikel.Levprioritet
        TT_ImportArtikel.ArtikkelNr      = trim(ENTRY(14,VPIFilLinje.StorTekst,";"),'"') 
        TT_ImportArtikel.Strekkode       = IF dStrekTst = 0 THEN "" ELSE trim(ENTRY(15,VPIFilLinje.StorTekst,";"),'"') 
        TT_ImportArtikel.Strekkode       = IF TT_ImportArtikel.Strekkode = "" THEN TT_ImportArtikel.Product_id ELSE TT_ImportArtikel.Strekkode
        TT_ImportArtikel.PantArtikkelNr  = trim(ENTRY(16,VPIFilLinje.StorTekst,";"),'"') 
        TT_ImportArtikel.PrisProfil      = trim(ENTRY(17,VPIFilLinje.StorTekst,";"),'"')  
        TT_ImportArtikel.Volym           = trim(ENTRY(18,VPIFilLinje.StorTekst,";"),'"') 
        TT_ImportArtikel.intPrioritet   = INT(TT_ImportArtikel.Levprioritet) 
        TT_ImportArtikel.LevNr          = 0 
        TT_ImportArtikel.Radnr          = iRadNr
        TT_ImportArtikel.SkapaEAN       = TRUE
        TT_ImportArtikel.OrsakDel       = 
                                         IF TT_ImportArtikel.Artikkelnr = "0" OR TT_ImportArtikel.Artikkelnr = "" THEN "Fel Artnr"
                                         ELSE IF TT_ImportArtikel.Beskrivning = "" THEN "Beskrivning saknas"
                                         ELSE IF LENGTH(TT_ImportArtikel.Artikkelnr) > 13 THEN "Artikelnr > 13 tkn"
                                         ELSE IF TT_ImportArtikel.Strekkode = "" THEN "Streckkod saknas"
                                         ELSE IF TT_ImportArtikel.Varegruppe = "" THEN "Fel varugruppe"
                                         ELSE IF (TT_ImportArtikel.StrekKode BEGINS "7388" AND LENGTH(TT_ImportArtikel.StrekKode) = 13
                                                  AND SUBSTR(TT_ImportArtikel.StrekKode,9,4) <> "0000") THEN
                                                "Fel tidningskod " + TT_ImportArtikel.StrekKode
                                         ELSE IF TT_ImportArtikel.StrekKode BEGINS "7388" AND LENGTH(TT_ImportArtikel.StrekKode) <> 13 AND LENGTH(TT_ImportArtikel.StrekKode) <> 8 THEN
                                             "Fel tidningskod " + TT_ImportArtikel.StrekKode
                                         ELSE "".

    IF TT_ImportArtikel.Orsakdel = "" THEN DO:
        IF TT_ImportArtikel.Strekkode BEGINS "02100" THEN DO:
            IF NOT TT_ImportArtikel.Product_id BEGINS "02100" AND TT_ImportArtikel.Product_id <> "" THEN
                ASSIGN TT_ImportArtikel.Strekkode = TT_ImportArtikel.Product_id.
            ELSE
                ASSIGN TT_ImportArtikel.OrsakDel = "Borttagen '02100'-kod".
        END.
        ELSE IF TT_ImportArtikel.StrekKode BEGINS("7388") AND LENGTH(TT_ImportArtikel.StrekKode) = 8 THEN
            ASSIGN TT_ImportArtikel.StrekKode = DYNAMIC-FUNCTION('fixChkEAN':U IN h_dproclib,INPUT TT_ImportArtikel.StrekKode + "0000").
        IF TT_ImportArtikel.OrsakDel = "" AND LENGTH(TT_ImportArtikel.StrekKode) > 5 THEN DO:
        /* Test om checksiffra är OK */
            ASSIGN TT_ImportArtikel.StrekKode = FILL("0",13 - LENGTH(TT_ImportArtikel.StrekKode)) + TT_ImportArtikel.StrekKode.
            IF TT_ImportArtikel.StrekKode <>
                        DYNAMIC-FUNCTION('fixChkEAN':U IN h_dproclib,INPUT SUBSTR(TT_ImportArtikel.StrekKode,1,12)) THEN
                   TT_ImportArtikel.Orsakdel  = "Fel checksiffra streckkode".
             ASSIGN TT_ImportArtikel.SkapaEan = IF TT_ImportArtikel.Orsakdel = "" AND TT_ImportArtikel.SkapaEan = TRUE THEN TRUE ELSE FALSE.
        END.
        IF TT_ImportArtikel.Orsakdel = "" AND TT_ImportArtikel.StrekKode BEGINS("2") AND LENGTH(TT_ImportArtikel.StrekKode) = 13 AND
                CAN-DO("20,21,22,23,24,25,26,27,28",SUBSTR(TT_ImportArtikel.StrekKode,1,2)) THEN DO:
            IF SUBSTR(TT_ImportArtikel.StrekKode,9,4) <> "0000" THEN
                ASSIGN TT_ImportArtikel.StrekKode = DYNAMIC-FUNCTION('fixChkEAN':U IN h_dproclib,INPUT SUBSTR(TT_ImportArtikel.StrekKode,1,8) + "0000").
            ASSIGN TT_ImportArtikel.Salgsenhet = "Stk".
        END.
    END.
    ASSIGN TT_ImportArtikel.SkapaEAN = TT_ImportArtikel.Orsakdel = "".
/*     IF piLoop1 MODULO 10 = 0 THEN */
        STATUS DEFAULT "Behandler linje " + STRING(iTotAntLinjer) + " av " +
                       STRING(iTotAnt) + ".".

    /* Konverterer */
    RUN Konvertering.

    
    IF TT_ImportArtikel.OrsakDel = "" THEN FEILKONTROLL: DO:
        /* Kontrollerer om artikkelnummer inneholder ugyldige tegn. */
        ASSIGN dArtikkelNr = DECI(TT_ImportArtikel.Artikkelnr) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN TT_ImportArtikel.Orsakdel = "Felaktigt artnr " + TT_ImportArtikel.Artikkelnr
                   TT_ImportArtikel.SkapaEAN = FALSE.
            LEAVE FEILKONTROLL.
        END.
        IF NOT CAN-FIND(VarGr WHERE VarGr.Vg = INT(TT_ImportArtikel.Varegruppe)) THEN DO:
            ASSIGN TT_ImportArtikel.Orsakdel = "Prod-/varugrupp saknas"
                   TT_ImportArtikel.SkapaEAN = FALSE.
            LEAVE FEILKONTROLL.
        END.
    END. /*  */
        /* test dublett streckkod, ev byt till nya om prioritet är lägre */
END. /* VPIFILLINJE */
/* Ta bort lägre prioritet */
FOR EACH TT_ImportArtikel WHERE TT_ImportArtikel.OrsakDel = "" BREAK BY TT_ImportArtikel.ArtikkelNr BY TT_ImportArtikel.IntPrioritet:
    IF FIRST-OF(TT_ImportArtikel.Artikkelnr) THEN
        ASSIGN iIntPrioritet = TT_ImportArtikel.IntPrioritet.
    ELSE IF TT_ImportArtikel.IntPrioritet > iIntPrioritet THEN
        ASSIGN TT_ImportArtikel.OrsakDel = "Lägre prioritet".
END.
/* Se om vi har strekkoden på annan artikel i db */
FOR EACH TT_ImportArtikel WHERE TT_ImportArtikel.OrsakDel = "":
    ASSIGN dArtikkelNr = DECI(TT_ImportArtikel.Artikkelnr).
    IF CAN-FIND(FIRST StrekKode WHERE StrekKode.Kode = TT_ImportArtikel.StrekKode AND StrekKode.ArtikkelNr <> dArtikkelNr) THEN
        ASSIGN TT_ImportArtikel.OrsakDel = "Streckkod på annan DB-artikel".
END.
/* Ta bort dubeltter i import */
FOR EACH TT_ImportArtikel WHERE TT_ImportArtikel.OrsakDel = "" BREAK BY TT_ImportArtikel.StrekKode BY TT_ImportArtikel.ArtikkelNr:
    IF FIRST-OF(TT_ImportArtikel.StrekKode) THEN
        ASSIGN cArtikkelNr = TT_ImportArtikel.ArtikkelNr.
    ELSE IF TT_ImportArtikel.ArtikkelNr <> cArtikkelNr THEN
        ASSIGN TT_ImportArtikel.OrsakDel = "Streckkod på annan importart".
END.
/* FOR EACH TT_ImportArtikel WHERE TT_ImportArtikel.OrsakDel = "" BREAK BY TT_ImportArtikel.ArtikkelNr BY TT_ImportArtikel.StrekKode    */
/*                                                                      BY TT_ImportArtikel.Prisprofil BY TT_ImportArtikel.KedjansPris: */

/* vi antar att førsta posten ær riktig vid inlæasning, dærfør sortering på radnr før att skapa en artprisport */
/* længre ned hade vi tidigare logik før att ændra till høgsta pris */
FOR EACH TT_ImportArtikel WHERE TT_ImportArtikel.OrsakDel = "" BREAK BY TT_ImportArtikel.ArtikkelNr BY TT_ImportArtikel.StrekKode
                                                                     BY TT_ImportArtikel.Prisprofil BY TT_ImportArtikel.RadNr:
    IF FIRST-OF(TT_ImportArtikel.ArtikkelNr) THEN DO:
        RELEASE TT_Artikel.
        BUFFER-COPY TT_ImportArtikel TO TT_Artikel.
        IF TT_Artikel.StrekKode = "7388" OR TT_Artikel.ProdStrekkode BEGINS "7388"
                                         OR TT_Artikel.Product_id BEGINS "7388" THEN
            ASSIGN TT_Artikel.lKod7388 = TRUE.
    END.
    IF FIRST-OF(TT_ImportArtikel.StrekKode) THEN
        RUN StrekKode (TT_ImportArtikel.ArtikkelNr, TT_ImportArtikel.Strekkode,TT_ImportArtikel.Strekkode = TT_ImportArtikel.Product_id).
    ELSE
        ASSIGN TT_ImportArtikel.OrsakDel = "Streckkod dublett ".
    FIND FIRST TT_ArtPris WHERE TT_ArtPris.ArtikkelNr = DECI(TT_ImportArtikel.ArtikkelNr) AND
                          TT_ArtPris.PrisProfil = INT(TT_ImportArtikel.Prisprofil) + 1 NO-ERROR.
    IF NOT AVAIL TT_ArtPris THEN DO:
        CREATE TT_ArtPris.
        ASSIGN TT_ArtPris.ArtikkelNr = DECI(TT_ImportArtikel.ArtikkelNr)
               TT_ArtPris.PrisProfil = INT(TT_ImportArtikel.Prisprofil) + 1
               TT_ArtPris.Inpris     = TT_ImportArtikel.InprisLev
               TT_ArtPris.Pris       = TT_ImportArtikel.KedjansPris.
        RELEASE TT_ArtPris.
    END.
    /* detta førsvinner før att vi har antagit att den først skapade artprisposten ær den riktiga */
/*     ELSE DO:                                                                                 */
/*         IF DECI(TRIM(TT_ImportArtikel.KedjansPris)) > DECI(TRIM(TT_ImportArtikel.Pris)) THEN */
/*             ASSIGN TT_ArtPris.Pris       = TT_ImportArtikel.KedjansPris.                     */
/*     END.                                                                                     */
END.
/* Är detta en 7388 */
FOR EACH TT_Artikel WHERE TT_Artikel.lKod7388 = FALSE:
    FOR EACH tmpStrekKode WHERE tmpStrekKode.Artikkelnr = DECI(TT_Artikel.Artikkelnr) AND
        tmpStrekKode.Kode BEGINS "7388".
        ASSIGN TT_artikel.lKod7388 = TRUE.
        LEAVE.
    END.
    IF NOT TT_Artikel.lKod7388 THEN DO:
        FOR EACH TT_ImportArtikel WHERE TT_ImportArtikel.ArtikkelNr = TT_Artikel.Artikkelnr:
            IF TT_ImportArtikel.StrekKode = "7388" OR TT_ImportArtikel.ProdStrekkode BEGINS "7388"
                                                   OR TT_ImportArtikel.Product_id BEGINS "7388" THEN DO:
                ASSIGN TT_Artikel.lKod7388 = TRUE.
                LEAVE.
            END.
        END.
    END.
END.
OUTPUT TO VALUE("ImpLogg.txt").
FOR EACH TT_ImportArtikel WHERE TT_ImportArtikel.OrsakDel <> "" BY TT_ImportArtikel.ArtikkelNr BY TT_ImportArtikel.StrekKode BY TT_ImportArtikel.Prisprofil:
       EXPORT DELIMITER ";"    
           iTotAntLinjer
           TODAY
           STRING(TIME,"HH:MM:SS")
           TT_ImportArtikel.ArtikkelNr
           TT_ImportArtikel.Beskrivning
           TT_ImportArtikel.Product_id
           TT_ImportArtikel.Strekkode
           TT_ImportArtikel.OrsakDel.
END.
OUTPUT CLOSE.
/* OUTPUT TO VALUE("kalkyl.txt").                */
/* FOR EACH TT_Artpris BY TT_Artpris.Artikkelnr: */
/*        EXPORT DELIMITER ";"    TT_artpris.    */
/* END.                                          */
/* OUTPUT CLOSE.                                 */

RUN SkapaUppdaterArtikel.

ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
       pbStatus = TRUE.

IF pbStatus = TRUE THEN
DO TRANSACTION:
  FIND CURRENT VPIDatasett EXCLUSIVE-LOCK.
  ASSIGN
/*       VPIDatasett.DatasettStatus = 3 /* Gruppeinfo mottatt og oppdatert lokalt */ */
      VPIDatasett.DatasettStatus = 5 /* Her MÅ vi ferdigstille med en gang     */
      .
  FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
  ASSIGN
      VPIFilHode.VPIFilStatus = 5
      .
  FIND CURRENT VPIDatasett NO-LOCK.
  FIND CURRENT VPIFilHode  NO-LOCK.

END. /* TRANSACTION */

STATUS DEFAULT "".

ASSIGN
    cTekst = "Behandlet " + STRING(iTotAntLinjer) + " VPI poster. " + 
             "Antall linjer med ukjente koder " + STRING(piAntFeil) + ".".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").
ASSIGN
    cTekst = "Utpakking av VPI ferdig.Tidsbruk " + STRING(TIME - piTid,"HH:MM:SS") + ".".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-SetLopeNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetLopeNr Procedure 
FUNCTION SetLopeNr RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var wLoop as int no-undo.
      
  DEF BUFFER bufArtBas FOR ArtBas.

  FINN-NESTE:  
  repeat wLoop = 1 to 10000:
  
    if wLoop = 0 then
      next FINN-NESTE.
      
    if can-find(bufArtBas no-lock where
      bufArtBas.Vg    = ArtBas.Vg and
      bufArtBas.LopNr = wLoop) then
      do:
        next FINN-NESTE.
      end.
    else
      leave FINN-NESTE.          
  end. /* FINN-NESTE */
  
  if wLoop > 9999 then
      RETURN ?.
  ELSE
      RETURN wLoop.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

