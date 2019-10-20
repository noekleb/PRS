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


&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEFINE VAR cParaString AS CHARACTER INIT "1050000001|" NO-UNDO.
    DEFINE VAR lDirekte    AS LOGICAL    NO-UNDO.
    DEFINE VAR cPrinter    AS CHARACTER  NO-UNDO.
    DEFINE VAR iAntEks     AS INTEGER    NO-UNDO.
    DEFINE VAR cMailAdress AS CHARACTER  NO-UNDO.
    DEFINE VAR iFormatKod  AS INTEGER    NO-UNDO.
&ELSE
    DEFINE INPUT PARAMETER cParaString AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER lDirekte    AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER cPrinter    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iAntEks     AS INTEGER    NO-UNDO.
    DEFINE INPUT PARAMETER cMailAdress AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER iFormatKod  AS INTEGER    NO-UNDO.
&ENDIF
 

DEFINE VARIABLE cFirma AS CHARACTER FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE hHodeTH      AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLinjeTH     AS HANDLE     NO-UNDO.
DEFINE VARIABLE hTTHodeBuff  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hTTLinjeBuff AS HANDLE     NO-UNDO.
DEFINE VARIABLE cUtskrift    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cCmd         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPWD         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE iGant AS INTEGER NO-UNDO.
DEFINE VARIABLE cRubrik AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iPageHeight AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPageWidth  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLeftCol    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iColLbl AS INTEGER    EXTENT 12  NO-UNDO. /* sätts i SkrivRapportPDF */
DEFINE VARIABLE cColLbl AS CHARACTER    EXTENT 12  NO-UNDO. /* sätts i SkrivRapportPDF */
DEFINE VARIABLE dY AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNettButLager AS INTEGER NO-UNDO.
DEFINE VARIABLE bOverstyr AS LOG NO-UNDO.

DEFINE TEMP-TABLE TT_RapportRader NO-UNDO
    FIELD iPageNum AS INTEGER  /* Sidnr */
    FIELD iColPage AS INTEGER  /* Hantering av 'för många cols' */
    FIELD iRadNum  AS INTEGER
    FIELD cRadData AS CHARACTER
    INDEX RadNum iPageNum iColPage iRadNum.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

{runlib.i}

{ pdf_inc.i "THIS-PROCEDURE"}

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

&IF DEFINED(EXCLUDE-getRapPrinter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRapPrinter Procedure 
FUNCTION getRapPrinter RETURNS CHARACTER
  ( INPUT ipcPrinter AS CHARACTER )  FORWARD.

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

{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
{syspara.i 210 100 8 iGant INT}
{syspara.i 150 1 3 iNettButLager INT}

ASSIGN 
  bTest     = TRUE
  cLogg     = 'skrivpakkseddel' + REPLACE(STRING(TODAY),'/','')
  bOverstyr = IF NUM-ENTRIES(cPrinter,'|') > 1 THEN bOverstyr = FALSE ELSE TRUE
  cPrinter  = ENTRY(1,cPrinter,'|') 
  .
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Start' 
      ).    

FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.

{syspara.i 1 1 7 cPWD}
cPWD = IF cPWD MATCHES "*skotex*" THEN "" ELSE RIGHT-TRIM(cPWD,"\") + '\'.

{syspara.i 1 1 8 cUtskrift}
IF cUtskrift = '' THEN 
  cUtskrift = ".\utskrift".
ELSE 
  cUtskrift = cPWD + cUtskrift.  

{syspar2.i 1 1 8 cCmd}
IF (cCmd = '' AND cPWD = '') THEN 
  cCmd = ".\cmd\FoxitReader.exe /t".
ELSE IF cCmd = '' THEN
    cCmd = cPWD + "\cmd\FoxitReader.exe /t".
ELSE 
    cCmd = cPWD + cCmd.
IF bTest THEN 
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Bruker: ' + IF AVAILABLE Bruker THEN Bruker.Brukerid ELSE "** Ukjent bruker" 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Utskriftskatalog: ' + cUtskrift 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Cmd: ' + cCmd 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Overstyr skriver:' 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    bOverstyr:' + STRING(bOverstyr) 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    iGant    :' + STRING(iGant) 
        ).    
  END.
IF lDirekte AND NOT CAN-DO(SESSION:GET-PRINTERS(),cPrinter) THEN
  DO:
    IF bTest THEN
      DO: 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  ** Ikke Direkte og kan ikke finne sesjons skriver (' + cPrinter + '). Utskrift avbrutt.' 
            ).    
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            'Slutt.' 
            ).
      END.    
    RETURN.
  END.
    
OS-CREATE-DIR VALUE(RIGHT-TRIM(cUtskrift,'\')).    
  cUtskrift = RIGHT-TRIM(cUtskrift,'\') + '\'.

FIND PkSdlHode WHERE PkSdlHode.PkSdlId = INT(ENTRY(1,cParaString,"|")) NO-ERROR.
IF AVAILABLE PkSdlHode THEN 
    FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK NO-ERROR.
IF NOT AVAILABLE PkSdlHode OR NOT AVAILABLE PkSdlLinje THEN
  DO: 
    IF bTest THEN
      DO: 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  ** Ukjent pksdlid (' + ENTRY(1,cParaString,"|") + '). Utskrift avbrutt.' 
            ).    
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            'Slutt.' 
            ).
      END.    
    RETURN.
  END.
  
IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    PkSdl butikk:' + STRING(PkSdlLinje.ButikkNr) 
        ).    
/* TN 11/10-19 GANT eCom skal ha pakkseddel skrevet ut på fakturaskriver. */
IF iGant = 1 AND bOverstyr = TRUE AND AVAILABLE PkSdlLinje AND PkSdlLinje.ButikkNr = iNettButLager THEN 
DO:
    FIND Butiker NO-LOCK WHERE
      Butiker.Butik = PkSdlLinje.ButikkNr NO-ERROR.
    IF AVAILABLE Butiker THEN
    DO: 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  GANT eCom utskrift styrt om til fakturaskriver. Fra : ' + cPrinter + ' til: ' + Butiker.Fakturaskriver + '.' 
          ).
      cPrinter = Butiker.Fakturaskriver.
    END.    
END.
        
RUN PopulateTT.

RUN SkrivRapportPDF.

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt' 
      ).    

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-PopulateTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopulateTT Procedure 
PROCEDURE PopulateTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    hHodeTH  = DYNAMIC-FUNCTION("getTempTable","get_PkSdlHode.p",cParaString,?).
    hLinjeTH = DYNAMIC-FUNCTION("getTempTable","get_PkSdlLinje.p",cParaString,?).
    hTTHodeBuff  = hHodeTH:DEFAULT-BUFFER-HANDLE.
    hTTLinjeBuff = hLinjeTH:DEFAULT-BUFFER-HANDLE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivColLabelsPDF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivColLabelsPDF Procedure 
PROCEDURE SkrivColLabelsPDF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   DEFINE INPUT  PARAMETER iColLbl AS INTEGER EXTENT 8    NO-UNDO. */
  DEFINE VARIABLE iPage AS INTEGER     NO-UNDO.
/*   ASSIGN iColLbl[1] = iLeftCol */
/*          iColLbl[2] = 130      */
/*          iColLbl[3] = 317      */
/*          iColLbl[4] = 320      */
/*          iColLbl[5] = 430      */
/*          iColLbl[6] = 474      */
/*          iColLbl[7] = 545.     */


  iPage = pdf_Page ("Spdf").
 
 DO:
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",7).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[1],iColLbl[1] - bredd(cColLbl[1]),dY).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[2],iColLbl[2],dY).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[3],iColLbl[3],dY).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[4],iColLbl[4],dY).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[5],iColLbl[5],dY).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[6],iColLbl[6],dY).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[7],iColLbl[7] - bredd(cColLbl[7]),dY).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[8],iColLbl[8] - bredd(cColLbl[8]),dY).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[9],iColLbl[9] - bredd(cColLbl[9]),dY).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[10],iColLbl[10] - bredd(cColLbl[10]),dY).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[11],iColLbl[11] - bredd(cColLbl[11]),dY).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[12],iColLbl[12] - bredd(cColLbl[12]),dY).
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivHeader Procedure 
PROCEDURE SkrivHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iSidnr       AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER cFaktNr AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cPkSdlType AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER lKopi        AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  cKopiStr     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cBarCodeFaktNr AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iLength AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cLevLbl AS CHARACTER INIT "Levmelding"  NO-UNDO. /* om melding går över flera rader och ingen levmelding så blir denna blank. */
    DEFINE VARIABLE cMelding AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cLevMeld AS CHARACTER   NO-UNDO.
  
    iLength = LENGTH(cFaktNr).
    IF ROUND(iLength / 2,0) = iLength / 2 THEN
        cBarCodeFaktNr = cFaktNr.
    ELSE
        cBarCodeFaktNr = "0" + cFaktNr.
/*            hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE */
    ASSIGN
/*          cFaktNr = " " + STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlNr"):BUFFER-VALUE) */
/*            cFaktNr = IF cFaktNr = ? THEN "" ELSE cFaktNr                            */
           cKopiStr = IF lKopi THEN "<C40>K O P I" ELSE "".
    cMelding = TRIM(hTTHodeBuff:BUFFER-FIELD("Merknad"):BUFFER-VALUE).
    cLevMeld = TRIM(hTTHodeBuff:BUFFER-FIELD("MeldingFraLev"):BUFFER-VALUE).
    IF cLevMeld = "" AND NUM-ENTRIES(cMelding,CHR(13)) > 1 THEN DO:
        cLevLbl = "".
        cLevMeld = ENTRY(2,cMelding,CHR(13)).
        cMelding = ENTRY(1,cMelding,CHR(13)).
    END.
    PUT UNFORMATTED
        "<AT=5,> "   /* "<P12></B><C77><P10>" PAGE-NUMBER FORMAT ">>" SKIP */
      "<R+.8,><C57><P30><B><RIGHT=C+20>" cPkSdlType " " cFaktNr "</B>"   /* "<P12></B><C77><P10>" PAGE-NUMBER FORMAT ">>" SKIP */
      "<AT=16,160><#2><AT=+10,+38>" "<BARCODE#2,TYPE=2_5_interleaved,CHECKSUM=none,VALUE=" cBarCodeFaktNr ">"
      "<R6><C6><P7>" "PkSdlId      " "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE)       "<C26>Status<C32>: " STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlStatus"):BUFFER-VALUE)
                                                                                                                 + " / " + STRING(hTTHodeBuff:BUFFER-FIELD("Status"):BUFFER-VALUE)
      "<R6.7><C6><P7>" "Pakkseddel   " "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlNr"):BUFFER-VALUE)     "<C26>Endret<C32>: " STRING(hTTHodeBuff:BUFFER-FIELD("EDato"):BUFFER-VALUE,"99/99/99")
      "<R+.7><C6><P7>" "Opphav       " "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlOpphav"):BUFFER-VALUE) "<C26>Tid<C32>: " STRING(hTTHodeBuff:BUFFER-FIELD("ETid"):BUFFER-VALUE,"HH:MM:SS")
      "<R+.7><C6><P7>" "CL           " "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("CL"):BUFFER-VALUE)          "<C26>Bruker<C32>: " /* TRIM(hTTHodeBuff:BUFFER-FIELD("BrukerID"):BUFFER-VALUE) */
      "<R+.7><C6><P7>" "SendtDato    " "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("SendtDato"):BUFFER-VALUE,"99/99/99") "<C26>Regdato<C32>: " STRING(hTTHodeBuff:BUFFER-FIELD("RegistrertDato"):BUFFER-VALUE,"99/99/99")
    /* Kundeadress */
    /* Faktura header info */
    "<R6><C47><P7>"    "Side      "     "<C53><P7>: "  STRING(iSidNr) SKIP
    "<R6.7><C47><P7>"  "Registrert"     "<C53><P7>: "  STRING(hTTHodeBuff:BUFFER-FIELD("RegistrertTid"):BUFFER-VALUE,"HH:MM:SS") SKIP
    "<R7.4><C47><P7>"  "Registrert av"  "<C53><P7>: "  SKIP
    "<R8.1><C47><P7>"  "Merknad"        "<C53><P7>: "  REPLACE(cMelding,CHR(10),' ') SKIP
    "<R8.8><C47><P7>"   cLevLbl         "<C53><P7>: "  REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(cLevMeld,CHR(10),' '),'OrdreType','OT'),'Sesongkode','Ses'),'LandedCost','LC'),'Fil:',''),'Katalog:','') SKIP
    "<R9.5><C47><P7>"  "EkstId"         "<C53><P7>: "  TRIM(hTTHodeBuff:BUFFER-FIELD("EkstId"):BUFFER-VALUE) SKIP
    "<R10.2><C47><P7>" "Sum frakt"      "<C53><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("SumFrakt"):BUFFER-VALUE <> 0 THEN
                                                           STRING(hTTHodeBuff:BUFFER-FIELD("SumFrakt"):BUFFER-VALUE,"->>,>>>,>>9.99") ELSE " ") SKIP
    "<R12><C6><P7>" "Leverandør" SKIP
    "<R13.5><C6><P10><B>" hTTHodeBuff:BUFFER-FIELD("Levnamn"):BUFFER-VALUE SKIP.

/*     PUT UNFORMATTED "<USE#1>" hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE "</USE>". */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivHeaderPDF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivHeaderPDF Procedure 
PROCEDURE SkrivHeaderPDF :
/*------------------------------------------------------------------------------
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iSidnr       AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER cFaktNr AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cPkSdlType AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER lKopi        AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  cKopiStr     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cBarCodeFaktNr AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iLength AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cLevLbl AS CHARACTER INIT "Levmelding"  NO-UNDO. /* om melding går över flera rader och ingen levmelding så blir denna blank. */
    DEFINE VARIABLE cMelding AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cLevMeld AS CHARACTER   NO-UNDO.
  
    iLength = LENGTH(cFaktNr).
    IF ROUND(iLength / 2,0) = iLength / 2 THEN
        cBarCodeFaktNr = cFaktNr.
    ELSE
        cBarCodeFaktNr = "0" + cFaktNr.
/*            hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE */
    ASSIGN
/*          cFaktNr = " " + STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlNr"):BUFFER-VALUE) */
/*            cFaktNr = IF cFaktNr = ? THEN "" ELSE cFaktNr                            */
           cKopiStr = IF lKopi THEN "<C40>K O P I" ELSE "".
    cMelding = TRIM(hTTHodeBuff:BUFFER-FIELD("Merknad"):BUFFER-VALUE).
    cLevMeld = TRIM(hTTHodeBuff:BUFFER-FIELD("MeldingFraLev"):BUFFER-VALUE).
    IF cLevMeld = "" AND NUM-ENTRIES(cMelding,CHR(13)) > 1 THEN DO:
        cLevLbl = "".
        cLevMeld = ENTRY(2,cMelding,CHR(13)).
        cMelding = ENTRY(1,cMelding,CHR(13)).
    END.
    PUT UNFORMATTED
        "<AT=5,> "   /* "<P12></B><C77><P10>" PAGE-NUMBER FORMAT ">>" SKIP */
      "<R+.8,><C57><P30><B><RIGHT=C+20>" cPkSdlType " " cFaktNr "</B>"   /* "<P12></B><C77><P10>" PAGE-NUMBER FORMAT ">>" SKIP */
      "<AT=16,160><#2><AT=+10,+38>" "<BARCODE#2,TYPE=2_5_interleaved,CHECKSUM=none,VALUE=" cBarCodeFaktNr ">"
      "<R6><C6><P7>" "PkSdlId      " "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE)       "<C26>Status<C32>: " STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlStatus"):BUFFER-VALUE)
                                                                                                                 + " / " + STRING(hTTHodeBuff:BUFFER-FIELD("Status"):BUFFER-VALUE)
      "<R6.7><C6><P7>" "Pakkseddel   " "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlNr"):BUFFER-VALUE)     "<C26>Endret<C32>: " STRING(hTTHodeBuff:BUFFER-FIELD("EDato"):BUFFER-VALUE,"99/99/99")
      "<R+.7><C6><P7>" "Opphav       " "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlOpphav"):BUFFER-VALUE) "<C26>Tid<C32>: " STRING(hTTHodeBuff:BUFFER-FIELD("ETid"):BUFFER-VALUE,"HH:MM:SS")
      "<R+.7><C6><P7>" "CL           " "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("CL"):BUFFER-VALUE)          "<C26>Bruker<C32>: " /* TRIM(hTTHodeBuff:BUFFER-FIELD("BrukerID"):BUFFER-VALUE) */
      "<R+.7><C6><P7>" "SendtDato    " "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("SendtDato"):BUFFER-VALUE,"99/99/99") "<C26>Regdato<C32>: " STRING(hTTHodeBuff:BUFFER-FIELD("RegistrertDato"):BUFFER-VALUE,"99/99/99")
    /* Kundeadress */
    /* Faktura header info */
    "<R6><C47><P7>"    "Side      "     "<C53><P7>: "  STRING(iSidNr) SKIP
    "<R6.7><C47><P7>"  "Registrert"     "<C53><P7>: "  STRING(hTTHodeBuff:BUFFER-FIELD("RegistrertTid"):BUFFER-VALUE,"HH:MM:SS") SKIP
    "<R7.4><C47><P7>"  "Registrert av"  "<C53><P7>: "  SKIP
    "<R8.1><C47><P7>"  "Merknad"        "<C53><P7>: "  REPLACE(cMelding,CHR(10),' ') SKIP
    "<R8.8><C47><P7>"   cLevLbl         "<C53><P7>: "  REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(cLevMeld,CHR(10),' '),'OrdreType','OT'),'Sesongkode','Ses'),'LandedCost','LC'),'Fil:',''),'Katalog:','') SKIP
    "<R9.5><C47><P7>"  "EkstId"         "<C53><P7>: "  TRIM(hTTHodeBuff:BUFFER-FIELD("EkstId"):BUFFER-VALUE) SKIP
    "<R10.2><C47><P7>" "Sum frakt"      "<C53><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("SumFrakt"):BUFFER-VALUE <> 0 THEN
                                                           STRING(hTTHodeBuff:BUFFER-FIELD("SumFrakt"):BUFFER-VALUE,"->>,>>>,>>9.99") ELSE " ") SKIP
    "<R12><C6><P7>" "Leverandør" SKIP
    "<R13.5><C6><P10><B>" hTTHodeBuff:BUFFER-FIELD("Levnamn"):BUFFER-VALUE SKIP.

/*     PUT UNFORMATTED "<USE#1>" hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE "</USE>". */       
          
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iSidnr       AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER cFaktNr AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cPkSdlType AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER lKopi        AS LOGICAL    NO-UNDO.

    DEFINE        VARIABLE  cKopiStr     AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  dDato    AS DATE        NO-UNDO.
    DEFINE        VARIABLE  cStr         AS CHARACTER   NO-UNDO.
    DEFINE        VARIABLE  iMittCol     AS INTEGER  INIT 305   NO-UNDO.
    DEFINE VARIABLE iPlusCol AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iY AS INTEGER     NO-UNDO.
/*            hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE */

    DEFINE VARIABLE cButNamn  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cButikkNr AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dKundenr  AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE cHeaderLbl AS CHARACTER EXTENT 7  NO-UNDO.
    
    DEFINE VARIABLE cHbl1 AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cHbl2 AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cHbl3 AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cHbl4 AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cHbl5 AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cHbl6 AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cHbl7 AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cMelding AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cLevMeld AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cLevLbl AS CHARACTER INIT "Levmelding"  NO-UNDO. /* om melding går över flera rader och ingen levmelding så blir denna blank. */

    DEFINE VARIABLE cTxt AS CHARACTER   NO-UNDO.

    IF bruker.lng = "SE" THEN DO:
        ASSIGN cHbl1 = "PkSdlId,Status,Sida"
               cHbl2 = "Packsedel,Ändrad,Registrad"
               cHbl3 = "Ursprung,Tid,Registrad av"
               cHbl4 = "CL,Användare,Anm"
               cHbl5 = "Sänt Datum,Regdatum,LevMeddelande"
               cHbl6 = ",,ExtId"
               cHbl7 = ",,Sum frakt".
    END.
    ELSE DO:
        ASSIGN cHbl1 = "PkSdlId,Status,Side"
               cHbl2 = "Pakkseddel,Endret,Registrert"
               cHbl3 = "Opphav,Tid,Registrert av"
               cHbl4 = "CL,Bruker,Merknad"
               cHbl5 = "SendtDato,Regdato,LevMelding"
               cHbl6 = ",,EkstId"
               cHbl7 = ",,Sum frakt".
    END.


    iLeftCol    = 50.
    iMittCol    = 200.
    iPlusCol = 45.
    iY = iPageHeight - 40.
    
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",30).
    RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",26).
    RUN pdf_set_TextY("Spdf",iPageHeight - 38).
    
    RUN pdf_text_xy_dec ("Spdf",cPkSdlType + " " + cFaktNr,iPageWidth - 30 - bredd(cPkSdlType + " " + cFaktNr),iY).

    iY = iY - 50.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",7).

    /* Rad 1 */
    cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE).
    RUN pdf_text_xy_dec ("Spdf",ENTRY(1,cHbl1),50,iY).
    RUN pdf_text_xy_dec ("Spdf",cTxt,90,iY).
    cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlStatus"):BUFFER-VALUE) + " / " + STRING(hTTHodeBuff:BUFFER-FIELD("Status"):BUFFER-VALUE).
    RUN pdf_text_xy_dec ("Spdf",ENTRY(2,cHbl1),180,iY).
    RUN pdf_text_xy_dec ("Spdf",cTxt,230,iY).
    RUN pdf_text_xy_dec ("Spdf",ENTRY(3,cHbl1),330,iY).
    RUN pdf_text_xy_dec ("Spdf",STRING(iSidNr),380,iY).

    /* Rad 2 */
    iY = iY - 13.
    cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlNr"):BUFFER-VALUE).
    RUN pdf_text_xy_dec ("Spdf",ENTRY(1,cHbl2),50,iY).
    RUN pdf_text_xy_dec ("Spdf",cTxt,90,iY).
    cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("EDato"):BUFFER-VALUE,"99/99/99").
    RUN pdf_text_xy_dec ("Spdf",ENTRY(2,cHbl2),180,iY).
    RUN pdf_text_xy_dec ("Spdf",cTxt,230,iY).
    cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("RegistrertTid"):BUFFER-VALUE,"HH:MM:SS").
    RUN pdf_text_xy_dec ("Spdf",ENTRY(3,cHbl2),330,iY).
    RUN pdf_text_xy_dec ("Spdf",cTxt,380,iY).

    /* Rad 3 */
    iY = iY - 13.
    cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlOpphav"):BUFFER-VALUE).
    RUN pdf_text_xy_dec ("Spdf",ENTRY(1,cHbl3),50,iY).
    RUN pdf_text_xy_dec ("Spdf",cTxt,90,iY).
    cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("ETid"):BUFFER-VALUE,"HH:MM:SS").
    RUN pdf_text_xy_dec ("Spdf",ENTRY(2,cHbl3),180,iY).
    RUN pdf_text_xy_dec ("Spdf",cTxt,230,iY).
/*     cTxt = */
    RUN pdf_text_xy_dec ("Spdf",ENTRY(3,cHbl3),330,iY).
/*     RUN pdf_text_xy_dec ("Spdf",cTxt,380,iY). */


    cMelding = TRIM(hTTHodeBuff:BUFFER-FIELD("Merknad"):BUFFER-VALUE).
    cLevMeld = TRIM(hTTHodeBuff:BUFFER-FIELD("MeldingFraLev"):BUFFER-VALUE).
    IF cLevMeld = "" AND NUM-ENTRIES(cMelding,CHR(13)) > 1 THEN DO:
        cLevLbl = "".
        cLevMeld = ENTRY(2,cMelding,CHR(13)).
        cMelding = ENTRY(1,cMelding,CHR(13)).
    END.

    /* Rad 4 */
    iY = iY - 13.
    cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("CL"):BUFFER-VALUE).
    RUN pdf_text_xy_dec ("Spdf",ENTRY(1,cHbl4),50,iY).
    RUN pdf_text_xy_dec ("Spdf",cTxt,90,iY).
/*     cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("EDato"):BUFFER-VALUE,"99/99/99"). */
    RUN pdf_text_xy_dec ("Spdf",ENTRY(2,cHbl4),180,iY).
/*     RUN pdf_text_xy_dec ("Spdf",cTxt,230,iY). */
    RUN pdf_text_xy_dec ("Spdf",ENTRY(3,cHbl4),330,iY).
    RUN pdf_text_xy_dec ("Spdf",REPLACE(cMelding,CHR(10),' '),380,iY).


    /* Rad 5 */
    iY = iY - 13.
    cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("SendtDato"):BUFFER-VALUE,"99/99/99").
    RUN pdf_text_xy_dec ("Spdf",ENTRY(1,cHbl5),50,iY).
    RUN pdf_text_xy_dec ("Spdf",cTxt,90,iY).
    cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("RegistrertDato"):BUFFER-VALUE,"99/99/99").
    RUN pdf_text_xy_dec ("Spdf",ENTRY(2,cHbl5),180,iY).
    RUN pdf_text_xy_dec ("Spdf",cTxt,230,iY).
    cTxt = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(cLevMeld,CHR(10),' '),'OrdreType','OT'),'Sesongkode','Ses'),'LandedCost','LC'),'Fil:',''),'Katalog:','').
    RUN pdf_text_xy_dec ("Spdf",cLevLbl,330,iY).
    RUN pdf_text_xy_dec ("Spdf",cTxt,380,iY).

    /* Rad 6 */
    iY = iY - 13.
    cTxt = TRIM(hTTHodeBuff:BUFFER-FIELD("EkstId"):BUFFER-VALUE).
    RUN pdf_text_xy_dec ("Spdf",ENTRY(3,cHbl6),330,iY).
    RUN pdf_text_xy_dec ("Spdf",cTxt,380,iY).

    /* Rad 7 */
    iY = iY - 13.
    cTxt = (IF hTTHodeBuff:BUFFER-FIELD("SumFrakt"):BUFFER-VALUE <> 0 THEN
               STRING(hTTHodeBuff:BUFFER-FIELD("SumFrakt"):BUFFER-VALUE,"->>,>>>,>>9.99") ELSE " ").
    RUN pdf_text_xy_dec ("Spdf",ENTRY(3,cHbl7),330,iY).
    RUN pdf_text_xy_dec ("Spdf",cTxt,380,iY).

    iY = iY - 13.
    RUN pdf_text_xy_dec ("Spdf",STRING(bruker.lng = "SE","Leverantör/Leverandør"),50,iY).
    iY = iY - 13.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
    RUN pdf_text_xy_dec ("Spdf",hTTHodeBuff:BUFFER-FIELD("Levnamn"):BUFFER-VALUE,50,iY).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-SkrivRapportPDF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivRapportPDF Procedure 
PROCEDURE SkrivRapportPDF PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFilNavn     AS CHARACTER NO-UNDO.
DEFINE VARIABLE dMva AS DECIMAL     NO-UNDO.
DEFINE        VARIABLE qH             AS HANDLE     NO-UNDO.
DEFINE        VARIABLE qL             AS HANDLE     NO-UNDO.
DEFINE VARIABLE lMomsSkrivet AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cTmpLabel AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTxt AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMvaTxt AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iTTId AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSidNr AS INTEGER     NO-UNDO.
DEFINE VARIABLE cPkSdlType AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER     NO-UNDO.
DEFINE VARIABLE cPkSdlNr AS CHARACTER   NO-UNDO.

DEFINE VARIABLE dAntal    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dRest     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dLevert   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dInnkjSum AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dSalgSum  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dTmpInnkj AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dTmpSalg  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cSumlbl AS CHARACTER   NO-UNDO.

DEFINE VARIABLE iBilagsType    AS INTEGER    NO-UNDO.

    ASSIGN iLeftCol    = 40
           iColLbl[1] = 65    /* R */
           iColLbl[2] = 70    /* 130 */
           iColLbl[3] = 110   /* 317 */
           iColLbl[4] = 210   /* 320 */
           iColLbl[5] = 280   /* 415 */
           iColLbl[6] = 310   /* 474 */
           iColLbl[7] = 375.  /* R */
           iColLbl[8] = 410.  /* R */
           iColLbl[9] = 450.  /* R */
           iColLbl[10] = 480. /* R */
           iColLbl[11] = 520. /* R */
           iColLbl[12] = 560. /* R */

    ASSIGN cMvaTxt = IF bruker.lng = "SE" THEN "Moms" ELSE "Mva".
    ASSIGN cTmpLabel = IF bruker.lng = "SE" THEN "Art.nr,Lev.art,Artnamn,Färg,Str,Enh,Utpris,Enh.pris,Rab,Antal,Rest,Levererat" ELSE "Art.nr,Lev.art,Artnavn,Farge,Str,Enh,Utpris,Enh.pris,Rab,Antall,Rest,Levert".
    ASSIGN cColLbl[1] = ENTRY(1,cTmpLabel)
           cColLbl[2] = ENTRY(2,cTmpLabel)
           cColLbl[3] = ENTRY(3,cTmpLabel)
           cColLbl[4] = ENTRY(4,cTmpLabel)
           cColLbl[5] = ENTRY(5,cTmpLabel)
           cColLbl[6] = ENTRY(6,cTmpLabel)
           cColLbl[7] = ENTRY(7,cTmpLabel)
           cColLbl[8] = ENTRY(8,cTmpLabel)
           cColLbl[9] = ENTRY(9,cTmpLabel).
           cColLbl[10] = ENTRY(10,cTmpLabel).
           cColLbl[11] = ENTRY(11,cTmpLabel).
           cColLbl[12] = ENTRY(12,cTmpLabel).


    CREATE QUERY qH.
    CREATE QUERY qL.
    qL:SET-BUFFERS(hTTLinjeBuff).
    qH:SET-BUFFERS(hTTHodeBuff).
    qH:QUERY-PREPARE("FOR EACH " + hTTHodeBuff:NAME).
    qH:QUERY-OPEN().
    qH:GET-FIRST().

    iBilagstype = 1.
    CASE iBilagstype:
        WHEN 1 THEN ASSIGN cPkSdlNr = IF hTTHodeBuff:BUFFER-FIELD("PkSdlNr"):BUFFER-VALUE > 0 THEN 
                                           STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlNr"):BUFFER-VALUE) ELSE ""
                           cPkSdlType = STRING(cPkSdlNr <> "","Pakkseddel"). 
        OTHERWISE ASSIGN cPkSdlNr   = ""
                         cPkSdlType = "". 
    END CASE.
    
    cFilNavn = cUtskrift + "But_" + STRING(PkSdlLinje.ButikkNr) + '_' +  "PkSdl" + "_" + STRING(PkSdlHode.PkSdlNr) + '_' +  REPLACE(STRING(TODAY),'/','') + '-' + STRING(ETIME) + ".pdf".

   RUN pdf_new ("Spdf",cFilNavn).
   RUN pdf_set_BottomMargin ("Spdf", 20).
   RUN pdf_set_PaperType ("Spdf","A4").
/*    RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13). */
   RUN pdf_set_Orientation ("Spdf","portrait").
   iPageHeight = pdf_PageHeight ("Spdf").
   iPageWidth  = pdf_PageWidth ("Spdf").

   REPEAT WHILE NOT qH:QUERY-OFF-END:
       RUN pdf_new_page ("Spdf").
       iSidNr = 1.
       RUN SkrivHeaderPDF (iSidNr,cPkSdlNr,cPkSdlType,iCount > 1).
       dY = iPageHeight - 220.
       RUN SkrivColLabelsPDF.
       qL:QUERY-PREPARE("FOR EACH " + hTTLinjeBuff:NAME).
       qL:QUERY-OPEN().
       qL:GET-FIRST().

       REPEAT WHILE NOT qL:QUERY-OFF-END:
           ASSIGN dAntal  = dAntal  + hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE
                  dRest   = dRest   + hTTLinjeBuff:BUFFER-FIELD("AntRest"):BUFFER-VALUE
                  dLevert = dLevert + hTTLinjeBuff:BUFFER-FIELD("AntLevert"):BUFFER-VALUE.

           dTmpInnkj = hTTLinjeBuff:BUFFER-FIELD("AntLevert"):BUFFER-VALUE * hTTLinjeBuff:BUFFER-FIELD("NyVarekost"):BUFFER-VALUE.
           dTmpSalg  = hTTLinjeBuff:BUFFER-FIELD("AntLevert"):BUFFER-VALUE * hTTLinjeBuff:BUFFER-FIELD("NyPris"):BUFFER-VALUE.

           dInnkjSum = dInnkjSum + IF dTmpInnkj <> ? THEN dTmpInnkj ELSE 0.
           dSalgsum  = dSalgsum  + IF dTmpSalg <> ? THEN dTmpSalg ELSE 0.
           
           FIND StrKonv NO-LOCK WHERE StrKonv.StrKode = INTEGER(STRING(hTTLinjeBuff:BUFFER-FIELD("StrKode"):BUFFER-VALUE)) NO-ERROR. 
           dY = dY - 13.
           ASSIGN cTxt = " ".
           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",6).
           cTxt = STRING(hTTLinjeBuff:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[1] - bredd(cTxt),dY).
           cTxt = substr(hTTLinjeBuff:BUFFER-FIELD("LevKod"):BUFFER-VALUE,1,20).
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[2],dY).
           cTxt = SUBSTR(hTTLinjeBuff:BUFFER-FIELD("Beskr"):BUFFER-VALUE,1,30).
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[3],dY).
           cTxt = hTTLinjeBuff:BUFFER-FIELD("Farge"):BUFFER-VALUE.
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[4],dY).
           cTxt = (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '').
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[5],dY).
           cTxt = STRING(hTTLinjeBuff:BUFFER-FIELD("Salgsenhet"):BUFFER-VALUE).
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[6],dY).
           cTxt = STRING(hTTLinjeBuff:BUFFER-FIELD("NyPris"):BUFFER-VALUE,"->,>>>,>>9.99").
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[7] - bredd(cTxt),dY).
           cTxt = STRING(hTTLinjeBuff:BUFFER-FIELD("NyInnkjopsPris"):BUFFER-VALUE,"->,>>>,>>9.99").
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[8] - bredd(cTxt),dY).
           STRING(hTTLinjeBuff:BUFFER-FIELD("NyRab1%"):BUFFER-VALUE,"->>9.99").
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[9] - bredd(cTxt),dY).
           cTxt = hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE.
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[10] - bredd(cTxt),dY).
           cTxt = hTTLinjeBuff:BUFFER-FIELD("AntRest"):BUFFER-VALUE.
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[11] - bredd(cTxt),dY).
           cTxt = hTTLinjeBuff:BUFFER-FIELD("AntLevert"):BUFFER-VALUE.
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[12] - bredd(cTxt),dY).
           /* Skall vi göra sidbryt? */
           qL:GET-NEXT().
           IF dY < 80 AND NOT qL:QUERY-OFF-END THEN DO:
               RUN pdf_new_page ("Spdf").
               iSidNr = iSidNr + 1.
               RUN SkrivHeaderPDF (iSidNr,cPkSdlNr,cPkSdlType,iCount > 1).
               dY = iPageHeight - 220.
               RUN SkrivColLabelsPDF.
           END.
       END.
/*        IF NOT lMomsSkrivet THEN DO:                                       */
/*            dY = dY - 13.                                                  */
/*            RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).          */
/*            RUN pdf_text_xy_dec ("Spdf",cMvaTxt,iColLbl[2],dY).            */
/*            cTxt = STRING(dMva,"->,>>>,>>9.99").                           */
/*            RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).     */
/*            RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[9] - bredd(cTxt),dY). */
/*            RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).          */
/*        END.                                                               */
       qH:GET-NEXT().
   END.

   dY = 55.

   
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
   ASSIGN cSumlbl = IF bruker.lng = "SE" THEN "Antal,Rest,Levererat,Netto lev kr,Brutto fsgvärde kr" ELSE "Antall,Rest,Levert,Netto levert kr,Brutto salgsverdi kr".
   RUN pdf_text_xy_dec ("Spdf",ENTRY(1,cSumlbl),90  - bredd(ENTRY(1,cSumlbl)),dY).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(2,cSumlbl),150 - bredd(ENTRY(2,cSumlbl)),dY).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(3,cSumlbl),210 - bredd(ENTRY(3,cSumlbl)),dY).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(4,cSumlbl),310 - bredd(ENTRY(4,cSumlbl)),dY).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(5,cSumlbl),400 - bredd(ENTRY(5,cSumlbl)),dY).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).

   dY = dY - 13.
   RUN pdf_text_xy_dec ("Spdf",STRING(dAntal,"->,>>>,>>9.99"),90  - bredd(STRING(dAntal,"->,>>>,>>9.99")),dY).
   RUN pdf_text_xy_dec ("Spdf",STRING(dRest,"->,>>>,>>9.99"),150 - bredd(STRING(dRest,"->,>>>,>>9.99")),dY).
   RUN pdf_text_xy_dec ("Spdf",STRING(dLevert,"->,>>>,>>9.99"),210 - bredd(STRING(dLevert,"->,>>>,>>9.99")),dY).
   RUN pdf_text_xy_dec ("Spdf",STRING(dInnkjSum,"->,>>>,>>9.99"),310 - bredd(STRING(dInnkjSum,"->,>>>,>>9.99")),dY).
   RUN pdf_text_xy_dec ("Spdf",STRING(dSalgsum,"->,>>>,>>9.99"),400 - bredd(STRING(dSalgsum,"->,>>>,>>9.99")),dY).

   RUN pdf_close ("Spdf").
   qH:QUERY-CLOSE().
   qL:QUERY-CLOSE().
   DELETE OBJECT qH.
   DELETE OBJECT qL.

   IF lDirekte = FALSE THEN
   DO:
      IF bTest THEN
        DO: 
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  Pakkseddel type ' + cPkSdlType + '.'  
              ).    
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  Utskrift til skjerm av fil ' + cFilNavn + '.'  
              ).    
        END.    
       RUN browse2pdf\viewxmldialog.w (cFilNavn,cPkSdlType).
   END.
   ELSE DO:
      IF bTest THEN
        DO: 
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  Utskrift av fil.'  
              ).    
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  Kommando: ' + cCmd + ' ' + cFilnavn + ' "' + cPrinter + '"'  
              ).    
        END.    
      OS-COMMAND SILENT VALUE(cCmd + ' ' + cFilnavn + ' "' + cPrinter + '"').
   END.
       
/*   OS-DELETE VALUE(cFilNavn).*/
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

&IF DEFINED(EXCLUDE-getRapPrinter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRapPrinter Procedure 
FUNCTION getRapPrinter RETURNS CHARACTER
  ( INPUT ipcPrinter AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  RETURN IF ipcPrinter <> "" THEN ipcPrinter ELSE
      IF DYNAMIC-FUNCTION("getAttribute",SESSION,"SE_PRINTER") <> "" THEN
          DYNAMIC-FUNCTION("getAttribute",SESSION,"SE_PRINTER") ELSE SESSION:PRINTER-NAME.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

