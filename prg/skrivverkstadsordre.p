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
    DEFINE VAR cStatusTxt  AS CHARACTER  NO-UNDO.
&ELSE
    DEFINE INPUT PARAMETER cParaString AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER lDirekte    AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER cPrinter    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iAntEks     AS INTEGER    NO-UNDO.
    DEFINE INPUT PARAMETER cMailAdress AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER cStatusTxt  AS CHARACTER  NO-UNDO.
&ENDIF


DEFINE VARIABLE cFirma AS CHARACTER FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE hHodeTH      AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLinjeTH     AS HANDLE     NO-UNDO.
DEFINE VARIABLE hTTHodeBuff  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hTTLinjeBuff AS HANDLE     NO-UNDO.

DEFINE VAR iFormatKod   AS INTEGER    NO-UNDO. /* formatteras bort */
DEFINE VAR lFullRapport AS LOGICAL    NO-UNDO.

DEFINE TEMP-TABLE TT_RapportRader NO-UNDO
    FIELD iPageNum AS INTEGER  /* Sidnr */
    FIELD iColPage AS INTEGER  /* Hantering av 'för många cols' */
    FIELD iRadNum  AS INTEGER
    FIELD cRadData AS CHARACTER
    INDEX RadNum iPageNum iColPage iRadNum.

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

&IF DEFINED(EXCLUDE-fixChkEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fixChkEAN Procedure 
FUNCTION fixChkEAN RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER )  FORWARD.

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
IF NUM-ENTRIES(cParaString,"|") = 1 THEN
    ASSIGN cParaString = cParaString + "|".
IF ENTRY(2,cParaString,"|") = "FULL" THEN
    ASSIGN lFullRapport = TRUE
           ENTRY(2,cParaString,"|") = "".
/* RETURN. */
RUN PopulateTT.

RUN SkrivRapport.

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
    hHodeTH  = DYNAMIC-FUNCTION("getTempTable","get_kordrehode.p",cParaString,?).
    hLinjeTH = DYNAMIC-FUNCTION("getTempTable","get_kordrelinje.p",cParaString,?).
    hTTHodeBuff  = hHodeTH:DEFAULT-BUFFER-HANDLE.
    hTTLinjeBuff = hLinjeTH:DEFAULT-BUFFER-HANDLE.

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
    DEFINE INPUT  PARAMETER cStatus      AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER lKopi        AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  cRubrik      AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  cKopiStr     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cAdresse1    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cAdresse2    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cPostNr      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cPostSted AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cLand AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cKOidEAN AS CHARACTER  NO-UNDO.
/*            hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE */
    ASSIGN cKopiStr = IF lKopi THEN "<C40>K O P I" ELSE ""
           cRubrik  = "O R D R E".

    cAdresse1    = TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse1"):BUFFER-VALUE).
    cAdresse2    = TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE).
    cPostNr      = TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPostNr"):BUFFER-VALUE).
    cPostSted = TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPostSted"):BUFFER-VALUE).
    cLand        = TRIM(hTTHodeBuff:BUFFER-FIELD("FaktLand"):BUFFER-VALUE).
    IF cAdresse1 = "" THEN DO:
        cAdresse1    = TRIM(hTTHodeBuff:BUFFER-FIELD("Adresse1"):BUFFER-VALUE).
        cAdresse2    = TRIM(hTTHodeBuff:BUFFER-FIELD("Adresse2"):BUFFER-VALUE).
        cPostNr      = TRIM(hTTHodeBuff:BUFFER-FIELD("PostNr"):BUFFER-VALUE).
        cPostSted = TRIM(hTTHodeBuff:BUFFER-FIELD("PostSted"):BUFFER-VALUE).
        cLand        = TRIM(hTTHodeBuff:BUFFER-FIELD("FaktLand"):BUFFER-VALUE).
    END.
    ASSIGN cKOidEAN = STRING(hTTHodeBuff:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE).
/*            cKOidEAN = FixChkEAN(FILL("",12 - LENGTH(cKOidEAN)) + cKOidEAN). */
    PUT UNFORMATTED
/*         "<R1><C60>1<R2><C60>2<R3><C60>3<R4><C60>4<R5><C60>5" */
        "<AT=5,> "   /* "<P12></B><C77><P10>" PAGE-NUMBER FORMAT ">>" SKIP */
/*       "<R+.8,><C57><P12><B><RIGHT=C+10>" cRubrik  "<P10></B>"   /* "<P12></B><C77><P10>" PAGE-NUMBER FORMAT ">>" SKIP */ */
        "<R+.8,><C" (STRING(60 - ROUND(LENGTH(cRubrik) / 2,0))) "><P12><B>" cRubrik  "<C" STRING(111 - LENGTH(cStatus)) ">" cStatus "<P10></B>"   /* "<P12></B><C77><P10>" PAGE-NUMBER FORMAT ">>" SKIP */
    /*  "<R+.8> */ "<P10><B><C6>" TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaNavn"):BUFFER-VALUE) cKopiStr
      "<R+.7><C6><P7>"     TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaAdresse1"):BUFFER-VALUE)
      "<R+.7><C6><P7>"     TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostNr"):BUFFER-VALUE) " " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPoststed"):BUFFER-VALUE)
      "<R+.7><C6><P7>" "Telefon"  "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaTelefon"):BUFFER-VALUE)  "<C26>Bank <C32>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaBankKonto"):BUFFER-VALUE)
      "<R+.7><C6><P7>" "Telefaks" "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaTelefaks"):BUFFER-VALUE) "<C26>Postgiro <C32>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostgiro"):BUFFER-VALUE)
      "<R+.7><C6><P7>" "E-post:"  "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaEPost"):BUFFER-VALUE)
      "<R+.7><C6><P7>"   "URL:"     "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaURLAdresse"):BUFFER-VALUE)
      "<R+.7><C6><P7>" "Org.nr"   "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaOrganisasjonsNr"):BUFFER-VALUE)
    /* Kundeadress */
      "<AT=40,><C8><P10>" TRIM(hTTHodeBuff:BUFFER-FIELD("Navn"):BUFFER-VALUE)
      "<R+1><C8><P10>" cAdresse1
      (IF cAdresse2 <> "" THEN "<R+1><C8><P10>" + cAdresse2 ELSE "")
      "<R+1><C8><P10>" cPostNr " " cPoststed
      "<R+1><C8><P10>" TRIM(hTTHodeBuff:BUFFER-FIELD("FaktLand"):BUFFER-VALUE)
      "<R+1><C6><P7>LevAdresse<C12>" TRIM(hTTHodeBuff:BUFFER-FIELD("LevAdresse1"):BUFFER-VALUE)
      (IF TRIM(hTTHodeBuff:BUFFER-FIELD("LevAdresse2"):BUFFER-VALUE) <> "" THEN "<R+.7><C12><P7>" + TRIM(hTTHodeBuff:BUFFER-FIELD("LevAdresse2"):BUFFER-VALUE) ELSE "")
      "<R+.7><C12><P7>" TRIM(hTTHodeBuff:BUFFER-FIELD("LevPostNr"):BUFFER-VALUE) " " TRIM(hTTHodeBuff:BUFFER-FIELD("LevPostSted"):BUFFER-VALUE)
      "<R+.7><C12><P7>" TRIM(hTTHodeBuff:BUFFER-FIELD("LevLand"):BUFFER-VALUE)
    /* Referenser */
      "<AT=70,><C6><P7>Vår ref<C12>: "     TRIM(hTTHodeBuff:BUFFER-FIELD("VaarRef"):BUFFER-VALUE) 
      "<R+.7><C6><P7>Deres ref<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("DeresRef"):BUFFER-VALUE)
      "<R+.7><C6><P7>Referanse<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("Referanse"):BUFFER-VALUE)
    "<AT=100><C.1> " SKIP
    /* Faktura header info */
    "<R4><C50><P7>" "Side"               "<C60><P7>: "  STRING(iSidNr) SKIP
    "<R4.7><C50><P7>" "Ordrenr"          "<C60><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE > 0 THEN STRING(hTTHodeBuff:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE) ELSE "") SKIP
    "<R5.4><C50><P7>" "Kundenr"          "<C60><P7>: "  STRING(hTTHodeBuff:BUFFER-FIELD("KundeNr"):BUFFER-VALUE) SKIP
    "<R6.1><C50><P7>" "Prosjekt"         "<C60><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("KProsjektNr"):BUFFER-VALUE > 0 THEN STRING(hTTHodeBuff:BUFFER-FIELD("KProsjektNr"):BUFFER-VALUE) ELSE "") SKIP
    "<R6.8><C50><P7>" "Leveringsform"   "<C60><P7>: "  /* hTTHodeBuff:BUFFER-FIELD("LevFormMetode"):BUFFER-VALUE  */ SKIP
    "<R7.5><C50><P7>" "Lev.betingelser" "<C60><P7>: "  /* hTTHodeBuff:BUFFER-FIELD("LevFormBeskrivelse"):BUFFER-VALUE  */ SKIP
    "<R8.2><C50><P7>" "Valuta"          "<C60><P7>: "  hTTHodeBuff:BUFFER-FIELD("ValKod"):BUFFER-VALUE SKIP
"<R8.9><C50><P7>" "Bet.betingelser"   "<C60><P7>: "  /* hTTHodeBuff:BUFFER-FIELD("BetTekst"):BUFFER-VALUE */ SKIP
"<R9.6><C50><P7>" "Forfallsdato"    "<C60><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE = ? THEN "" ELSE STRING(hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE)) SKIP.
/*     "<R10.9><C50><P7>" "KID"             "<C60><P7>: "  /* IF hTTHodeBuff:BUFFER-FIELD("KID"):BUFFER-VALUE > 0 THEN hTTHodeBuff:BUFFER-FIELD("KID"):BUFFER-VALUE ELSE "" */ SKIP */
/*     "<R11.6><C50><P7>" "Fakturadato"     "<C60><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("FakturertDato"):BUFFER-VALUE = ? THEN "" ELSE STRING(hTTHodeBuff:BUFFER-FIELD("FakturertDato"):BUFFER-VALUE)) SKIP */
/*     "<R12.3><C50><P7>" "Bet.betingelser"   "<C60><P7>: "  /* hTTHodeBuff:BUFFER-FIELD("BetTekst"):BUFFER-VALUE */ SKIP                                                                                */
/*     "<R13><C50><P7>" "Forfallsdato"    "<C60><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE = ? THEN "" ELSE STRING(hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE)) SKIP. */
    PUT UNFORMATTED "<USE#1>" IF lFullRapport THEN hTTHodeBuff:BUFFER-FIELD("VerkstedMerknad"):BUFFER-VALUE ELSE ""  ".</USE>".
 put UNFORMATTED
   "<AT=" STRING(16) "," STRING(250) "><#2><AT=+12,+38>"
     "<BARCODE#2,TYPE=39,CHECKSUM=none,VALUE=" cKOidEAN ">"
       .
    PUT UNFORMATTED "<R11><C50><FROM><R19><C111><RECT>" SKIP.
    PUT UNFORMATTED "<R2><C5><FROM><R2><C113><LINE>" SKIP
                    "<R46><C5><FROM><R46><C113><LINE>" SKIP
                    "<R2><C5><FROM><R46><C5><LINE>" SKIP
                    "<R2><C113><FROM><R46><C113><LINE>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivHeaderOld) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivHeaderOld Procedure 
PROCEDURE SkrivHeaderOld :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iSidnr       AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER cFaktNr AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cFakturaType AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER lKopi        AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  cKopiStr     AS CHARACTER  NO-UNDO.
/*            hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE */
    ASSIGN
/*          cFaktNr = " " + STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) */
/*            cFaktNr = IF cFaktNr = ? THEN "" ELSE cFaktNr                            */
           cKopiStr = IF lKopi THEN "<C40>K O P I" ELSE "".
    PUT UNFORMATTED
      "<R1><C57><P10><B><RIGHT=C+20>" cFakturatype " " cFaktNr "</B>"   /* "<P12></B><C77><P10>" PAGE-NUMBER FORMAT ">>" SKIP */
      "<R1.8><P10><B><C6>" TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaNavn"):BUFFER-VALUE) cKopiStr
      "<R2.5><C6><P7>"     TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaAdresse1"):BUFFER-VALUE) SKIP
      "<R3.2><C6><P7>"     TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostNr"):BUFFER-VALUE) " " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPoststed"):BUFFER-VALUE) SKIP
      "<R3.9><C6><P7>" "Telefon"  "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaTelefon"):BUFFER-VALUE)  "<C26>Bank <C32>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaBankKonto"):BUFFER-VALUE) SKIP
      "<R4.6><C6><P7>" "Telefaks" "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaTelefaks"):BUFFER-VALUE) "<C26>Postgiro <C32>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostgiro"):BUFFER-VALUE) SKIP
      "<R5.3><C6><P7>" "E-post:"  "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaEPost"):BUFFER-VALUE) SKIP
      "<R6><C6><P7>"   "URL:"     "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaURLAdresse"):BUFFER-VALUE) SKIP
      "<R6.7><C6><P7>" "Org.nr"   "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaOrganisasjonsNr"):BUFFER-VALUE) SKIP
    /* Kundeadress */
      "<R10><C8><P10>" TRIM(hTTHodeBuff:BUFFER-FIELD("Navn"):BUFFER-VALUE)
      "<R+1><C8><P10>" TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse1"):BUFFER-VALUE)
      (IF TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE) <> "" THEN "<R+1><C8><P10>" + TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE) ELSE "")
      "<R+1><C8><P10>" TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPostNr"):BUFFER-VALUE) " " TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPoststed"):BUFFER-VALUE)
      "<R+1><C8><P10>" TRIM(hTTHodeBuff:BUFFER-FIELD("FaktLand"):BUFFER-VALUE)
    /* Referenser */
      "<R17><C6><P7>Vår ref<C12>: "     TRIM(hTTHodeBuff:BUFFER-FIELD("VaarRef"):BUFFER-VALUE)   SKIP
      "<R17.7><C6><P7>Deres ref<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("DeresRef"):BUFFER-VALUE)  SKIP
      "<R18.4><C6><P7>Referanse<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("Referanse"):BUFFER-VALUE) SKIP
    "<R23><C.1>___" SKIP
    /* Faktura header info */
    "<R6><C50><P7>" "Side"               "<C60><P7>: "  STRING(iSidNr) SKIP
    "<R6.7><C50><P7>" "Ordrenr"          "<C60><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE > 0 THEN STRING(hTTHodeBuff:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE) ELSE "") SKIP
    "<R7.4><C50><P7>" "Kundenr"          "<C60><P7>: "  STRING(hTTHodeBuff:BUFFER-FIELD("KundeNr"):BUFFER-VALUE) SKIP
    "<R8.1><C50><P7>" "Prosjekt"         "<C60><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("KProsjektNr"):BUFFER-VALUE > 0 THEN STRING(hTTHodeBuff:BUFFER-FIELD("KProsjektNr"):BUFFER-VALUE) ELSE "") SKIP
    "<R8.8><C50><P7>" "Leveringsform"   "<C60><P7>: "  hTTHodeBuff:BUFFER-FIELD("LevFormMetode"):BUFFER-VALUE SKIP
    "<R9.5><C50><P7>" "Lev.betingelser" "<C60><P7>: "  hTTHodeBuff:BUFFER-FIELD("LevFormBeskrivelse"):BUFFER-VALUE SKIP
    "<R10.2><C50><P7>" "Valuta"          "<C60><P7>: "  hTTHodeBuff:BUFFER-FIELD("ValKod"):BUFFER-VALUE SKIP
    "<R10.9><C50><P7>" "KID"             "<C60><P7>: "  IF hTTHodeBuff:BUFFER-FIELD("KID"):BUFFER-VALUE > 0 THEN hTTHodeBuff:BUFFER-FIELD("KID"):BUFFER-VALUE ELSE "" SKIP
    "<R11.6><C50><P7>" "Fakturadato"     "<C60><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("FakturertDato"):BUFFER-VALUE = ? THEN "" ELSE STRING(hTTHodeBuff:BUFFER-FIELD("FakturertDato"):BUFFER-VALUE)) SKIP
    "<R12.3><C50><P7>" "Bet.betingelser"   "<C60><P7>: "  hTTHodeBuff:BUFFER-FIELD("BetTekst"):BUFFER-VALUE SKIP
    "<R13><C50><P7>" "Forfallsdato"    "<C60><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE = ? THEN "" ELSE STRING(hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE)) SKIP.
    PUT UNFORMATTED "<USE#1>" hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE "</USE>".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivPostGiro) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivPostGiro Procedure 
PROCEDURE SkrivPostGiro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     DEFINE INPUT  PARAMETER iSidnr       AS INTEGER    NO-UNDO. */
/*     DEFINE INPUT  PARAMETER cFaktNr AS CHARACTER  NO-UNDO.      */
/*     DEFINE INPUT  PARAMETER cFakturaType AS CHARACTER  NO-UNDO. */
/*     DEFINE INPUT  PARAMETER lKopi        AS LOGICAL    NO-UNDO. */
    DEFINE        VARIABLE  cKopiStr       AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  dTotalt        AS DECIMAL    NO-UNDO.
    DEFINE        VARIABLE  cBankPG        AS CHARACTER  NO-UNDO.
    ASSIGN dTotalt = DECI(hTTHodeBuff:BUFFER-FIELD("Totalt"):STRING-VALUE)
           cBankPG = TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaBankKonto"):BUFFER-VALUE).
    IF cBankPG = "" THEN
        ASSIGN cBankPG = TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostgiro"):BUFFER-VALUE).
    PUT UNFORMATTED
/*       "<R44.4><P8><C8><P10><B>" TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostgiro"):BUFFER-VALUE) */
/*        "<AT=180,15> " */
/*        "<R44.4><P8><C8><P10><B>" TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostgiro"):BUFFER-VALUE) */
        "<AT=188,22><P8><C8><P10><B>" TRIM(cBankPG)
        "<AT=,90><RIGHT=C+8>" STRING(hTTHodeBuff:BUFFER-FIELD("Totalt"):BUFFER-VALUE,"->,>>>,>>9.99")
/*        "<R44.4><P8><C8><P10><B>" TRIM(cBankPG)                                                    */
/*        "<C35><RIGHT=C+8>" STRING(hTTHodeBuff:BUFFER-FIELD("Totalt"):BUFFER-VALUE,"->,>>>,>>9.99") */
       "<AT=200,175>" TRIM(hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE)
/*         "<R47.4><C68>" TRIM(hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE) */
       "<AT=207,22>" "Kundenr"    "<C17>: "  STRING(hTTHodeBuff:BUFFER-FIELD("KundeNr"):BUFFER-VALUE)
       "<R+1><AT=,22>"   "Faktura"    "<C17>: "  STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE)
       "<R+1><AT=,22>"   "Fakturadato" "<C17>: " STRING(hTTHodeBuff:BUFFER-FIELD("FakturertDato"):BUFFER-VALUE)
       "<AT=236,22>" TRIM(hTTHodeBuff:BUFFER-FIELD("Navn"):BUFFER-VALUE)
       "<R+1><AT=,22>" TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse1"):BUFFER-VALUE)
       (IF TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE) <> "" THEN "<R+1><AT=,22><P10>" + TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE) ELSE " ")
       "<R+1><AT=,22>" TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPostNr"):BUFFER-VALUE) " " TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPoststed"):BUFFER-VALUE)
       "<R+1><AT=,22>" TRIM(hTTHodeBuff:BUFFER-FIELD("FaktLand"):BUFFER-VALUE)
       "<AT=236,120>" TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaNavn"):BUFFER-VALUE) 
       "<R+1><AT=,120>"     TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaAdresse1"):BUFFER-VALUE)
       "<R+2><AT=,120>"     TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostNr"):BUFFER-VALUE) " " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPoststed"):BUFFER-VALUE)
      "<AT=273,82><RIGHT=C+8>" STRING(TRUNC(dTotalt,0),"->,>>>,>>9")
/*       "<AT=275,22><C32.3><RIGHT=C+8>" STRING(TRUNC(dTotalt,0),"->,>>>,>>9") */
      "<AT=,108>" STRING(100 * (dTotalt - TRUNC(dTotalt,0)),"99")
/*         "<C+3.5>" STRING(100 * (dTotalt - TRUNC(dTotalt,0)),"99") */
      "<AT=,133>" cBankPG "</B>".
/*       "<C50>" cBankPG "</B>". */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivRapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivRapport Procedure 
PROCEDURE SkrivRapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE        VARIABLE pcRappFil      AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE iSidNr         AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE iRadNr         AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE cDetaljRad     AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE cDetaljRad1    AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE cDetaljRad2    AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE cSumRad        AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE qH             AS HANDLE     NO-UNDO.
   DEFINE        VARIABLE qL             AS HANDLE     NO-UNDO.
   DEFINE        VARIABLE iAntLinjer     AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE iAntNotatRader AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE iCount         AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE cFakturaType   AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE cFakturaNr     AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE iBilagsType    AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE cRefTxt        AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE iKontrollRad   AS INTEGER    NO-UNDO.
   
   DEFINE        VARIABLE dAvgFri        AS DECIMAL    NO-UNDO.
   DEFINE        VARIABLE dAvgPlikt      AS DECIMAL    NO-UNDO.
   DEFINE        VARIABLE dNetto         AS DECIMAL    NO-UNDO.
   DEFINE        VARIABLE dAvrund        AS DECIMAL    NO-UNDO.
   DEFINE        VARIABLE dRabatt        AS DECIMAL    NO-UNDO.
   DEFINE        VARIABLE dMvakr         AS DECIMAL    NO-UNDO.
   DEFINE        VARIABLE dForskudd      AS DECIMAL    NO-UNDO.
   DEFINE        VARIABLE dRabatt%       AS DECIMAL    NO-UNDO.
   DEFINE        VARIABLE dBruttoSum     AS DECIMAL    NO-UNDO.
   DEFINE        VARIABLE dTotSum        AS DECIMAL    NO-UNDO.

   DEFINE VARIABLE cLevstatus AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE iTst AS INTEGER    NO-UNDO.

   DEFINE VARIABLE iExtraRad AS INTEGER    NO-UNDO. /* Om vi har iFormatKod = 2 skall vi lägga till vid summarad */
   DEFINE VARIABLE iSumRad AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cHlbl1 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl2 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl3 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl4 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl5 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl6 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl7 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl8 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl9 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl10 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl11 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl12 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl13 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl1 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl2 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl3 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl4 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl5 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl6 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl7 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl8 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl9 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl10 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl11 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl12 AS CHARACTER  NO-UNDO.
   cDetaljRad1 = "><P8><C6><RIGHT=C+6>&1<C13>&2<C32>&3<C41>&4<C45><RIGHT=C+4>&5<C50><RIGHT=C+4>&6<C58><RIGHT=C+4>&7<C66><RIGHT=C+4>&8<C74><RIGHT=C+4>&9".
   cDetaljRad2 = "<C80><RIGHT=C+7>&1<C89><RIGHT=C+6>&2<C94><RIGHT=C+7>&3<C103><RIGHT=C+7>&4".
/*    cDetaljRad2 = "<C82><RIGHT=C+6>&1<C90><RIGHT=C+7>&2<C103><RIGHT=C+7>&3". */
   cSumRad    = "<R@1.7><P8><C7><RIGHT=C+7>&1<C17><RIGHT=C+7>&2<C27><RIGHT=C+7>&3<C37><RIGHT=C+7>&4<C47><RIGHT=C+7>&5<C57><RIGHT=C+7>&6<C67><RIGHT=C+7>&7<C103><RIGHT=C+7>&8".
/*    cSumRad    = "<R@1.7><P8><C7><RIGHT=C+7>&1<C15><RIGHT=C+7>&2<C23><RIGHT=C+7>&3<C31><RIGHT=C+7>&4<C39><RIGHT=C+7>&5<C45><RIGHT=C+7>&6<C52><RIGHT=C+7>&7<C60><RIGHT=C+7>&8<C70><RIGHT=C+7>&9". */

/*    iFormatkod = 2. */
   /* Hantering av rader för olika layouter */
   /* 1 = Internationell, 2 = Postgiro */
   iFormatKod = 2.
   ASSIGN iKontrollrad = IF iFormatKod = 1 THEN 62 ELSE IF iFormatKod = 2 THEN 41 ELSE 62  /* 34 */
          iExtraRad    = IF iFormatKod = 2 THEN 3 ELSE 0
          iSumRad = 42. 

   RUN GetTempFileName in wLibHandle ("fakt", "xpr", output pcRappFil).

   ASSIGN cHlbl1 = "Art.nr"    
          cHlbl2 = "Beskr"     
          cHlbl3 = "Farve"    
          cHlbl4 = "Levert"    
          cHlbl5 = "Str"       
          cHlbl6 = "Antall"     
          cHlbl7 = "Enhetspris"
          cHlbl8 = "Bruttopris"
          cHlbl9 = "Depositum" 
          cHlbl10 = "Faktura_Id"
          cHlbl11 = "Rabatt"   
          cHlbl12 = "Mva%"     
          cHlbl13 = "Sum".     

   ASSIGN cSlbl1 = "Avgfri"
          cSlbl2 = "Avgpl"
          cSlbl3 = "Netto"
          cSlbl4 = "Rabatt"
          cSlbl5 = "Rabatt%"
          cSlbl6 = "Mva"
          cSlbl7 = "Forskudd"
          cSlbl8 = "Å betale".

   CREATE QUERY qH.
   CREATE QUERY qL.
   qL:SET-BUFFERS(hTTLinjeBuff).
   qH:SET-BUFFERS(hTTHodeBuff).
   qH:QUERY-PREPARE("FOR EACH " + hTTHodeBuff:NAME).
   qH:QUERY-OPEN().
/*    ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "rapp.xpr". */
   OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(80).
   IF NOT lDirekte THEN DO:
       PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
       PUT CONTROL '<PRINTER' + DYNAMIC-FUNCTION('getRapPrinter':U,cPrinter) + '>'.
       PUT CONTROL '<PREVIEW=ZoomToWidth><OLANDSCAPE>'.
   END.
   ELSE DO:
       ASSIGN cMailAdress = "".
       PUT CONTROL '<PRINTER' + DYNAMIC-FUNCTION('getRapPrinter':U,cPrinter) + '><OLANDSCAPE>'.
   END.
   PUT UNFORMATTED "<ALIGN=BASE><FArial><UNITS=MM>" SKIP.
/*    PUT UNFORMATTED "<TOP=10mm><R1>" SKIP. */
   PUT UNFORMATTED "<R12><C51><#1><R18><C110><FRAME#1>" SKIP. /* <15>..<19>*/
/*    PUT UNFORMATTED "<R16><C40><#1><R21><C75><FRAME#1>" SKIP. */
   qH:GET-FIRST().
   REPEAT WHILE NOT qH:QUERY-OFF-END:
       qL:QUERY-PREPARE("FOR EACH " + hTTLinjeBuff:NAME + " WHERE KOrdre_Id = " +
                        STRING(hTTHodeBuff:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE + 
                               " BY KOrdreLinje.Leveringsdato BY KOrdreLinje.KOrdreLinjeNr")).

/*        ASSIGN cLevstatus = hTTHodeBuff:BUFFER-FIELD("LevStatus"):BUFFER-VALUE. */
       
       DO iCount = 1 TO iAntEks:
           iSidNr = 1.
           RUN SkrivHeader (iSidNr,cStatusTxt,iCount > 1).
           ASSIGN iRadNr     = 20. /* 22 */
           PUT UNFORMATTED                                     /* 168,255,168 */
           SUBSTITUTE("<R&1><C6><FROM><R&2><C112><RECT><BGCOLOR=220,220,220><FILLRECT>",STRING(iRadNr),STRING(iRadNr + 1)) SKIP.
/*            PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(cDetaljRad,"Prodnr","Beskr","Levert","Str","Fakturert","Enhetspris","Rabatt","Mva","Sum") "</B>" SKIP. */
           ASSIGN cDetaljRad = SUBSTITUTE(cDetaljRad1,cHlbl1,cHlbl2,cHlbl3,cHlbl4,cHlbl5,cHlbl6,cHlbl7,cHlbl8,cHlbl9) + 
                               SUBSTITUTE(cDetaljRad2,cHlbl10,cHlbl11,cHlbl12,cHlbl13).
           PUT UNFORMATTED "<B><R" STRING(iRadNr) cDetaljRad "</B>" SKIP.
           IF iCount = 1 THEN
               qL:QUERY-OPEN().
           qL:GET-FIRST().
           ASSIGN iAntLinjer = 0.
           REPEAT WHILE NOT qL:QUERY-OFF-END:
               ASSIGN iAntLinjer = iAntLinjer + 1.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Varespesifikasjon"):BUFFER-VALUE) <> "" THEN
                   ASSIGN iAntLinjer = iAntLinjer + 1.
/*                IF TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE) <> "" OR TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE) <> "" THEN */
/*                    ASSIGN iAntLinjer = iAntLinjer + 1.                                                                                                       */
               qL:GET-NEXT().
           END.
           qL:GET-FIRST().
           REPEAT WHILE NOT qL:QUERY-OFF-END:
               ASSIGN iRadNr     = iRadNr + 1
                      iAntLinjer = iAntLinjer - 1
                      dAvgFri    = dAvgFri   + IF hTTLinjeBuff:BUFFER-FIELD("MvaKr"):BUFFER-VALUE = 0 THEN
                                                  ROUND(hTTLinjeBuff:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE,2) ELSE 0
                      dAvgPlikt  = dAvgPlikt + IF hTTLinjeBuff:BUFFER-FIELD("MvaKr"):BUFFER-VALUE <> 0 THEN
                                                  ROUND(hTTLinjeBuff:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE,2) - ROUND(hTTLinjeBuff:BUFFER-FIELD("MvaKr"):BUFFER-VALUE,2) ELSE 0
                      dNetto     = dNetto    + ROUND(hTTLinjeBuff:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE,2) - ROUND(hTTLinjeBuff:BUFFER-FIELD("MvaKr"):BUFFER-VALUE,2)
                      dAvrund    = dAvrund   + 0
                      dRabatt    = dRabatt   + (hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE * ROUND(hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE,2)) - ROUND(hTTLinjeBuff:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE,2)
                      dMvakr     = dMvakr    + ROUND(hTTLinjeBuff:BUFFER-FIELD("MvaKr"):BUFFER-VALUE,2)
                      dForskudd  = dForskudd + hTTLinjeBuff:BUFFER-FIELD("Depositum"):BUFFER-VALUE
                      dBruttoSum = dBruttoSum + hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE * hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE
                      dTotSum    = dTotSum   + ROUND(hTTLinjeBuff:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE,2).

         /*            PUT UNFORMATTED SUBSTITUTE(cDetaljRad,STRING(iRadNr), */
               cDetaljRad = SUBSTITUTE(cDetaljRad1,
                               IF STRING(hTTLinjeBuff:BUFFER-FIELD("VareNr"):BUFFER-VALUE) = "" THEN " " ELSE STRING(hTTLinjeBuff:BUFFER-FIELD("VareNr"):BUFFER-VALUE),
                               "<B>" + STRING(hTTLinjeBuff:BUFFER-FIELD("Varetekst"):BUFFER-VALUE) + "</B>",
                               hTTLinjeBuff:BUFFER-FIELD("LevFargKod"):BUFFER-VALUE,
                               IF hTTLinjeBuff:BUFFER-FIELD("Leveringsdato"):BUFFER-VALUE <> ? THEN STRING(hTTLinjeBuff:BUFFER-FIELD("Leveringsdato"):BUFFER-VALUE) ELSE " ",
                               STRING(hTTLinjeBuff:BUFFER-FIELD("Storl"):BUFFER-VALUE),
                               STRING(hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE),
                               STRING(hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE,"->,>>>,>>9.99"),
                               STRING(hTTLinjeBuff:BUFFER-FIELD("BruttoPris"):BUFFER-VALUE,"->,>>>,>>9.99"),
                               IF hTTLinjeBuff:BUFFER-FIELD("Depositum"):BUFFER-VALUE = 0 THEN " " ELSE STRING(hTTLinjeBuff:BUFFER-FIELD("Depositum"):BUFFER-VALUE,"->,>>>,>>9.99")) +

                          SUBSTITUTE(cDetaljRad2, IF hTTLinjeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE = 0 THEN " " ELSE STRING(hTTLinjeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE),
                                     STRING(hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE * 
                                                        hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE - hTTLinjeBuff:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE,"->>>,>>9.99"),
                               STRING(hTTLinjeBuff:BUFFER-FIELD("Mva%"):BUFFER-VALUE,"->9.99"),
                               STRING(hTTLinjeBuff:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE,"->,>>>,>>9.99")).

               PUT UNFORMATTED "<R+.9" cDetaljRad.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Varespesifikasjon"):BUFFER-VALUE) <> "" THEN DO:
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   PUT UNFORMATTED "<I><R+.9" SUBSTITUTE(cDetaljRad1 + "</B>"," ",TRIM(STRING(hTTLinjeBuff:BUFFER-FIELD("Varespesifikasjon"):BUFFER-VALUE))) "</I>".
               END.
/*                IF TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE) <> "" THEN DO:                                                                          */
/* /*                IF TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE) <> "" OR TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE) <> "" THEN DO: */ */
/* /*                    ASSIGN cRefTxt = TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE)                                                                    */ */
/* /*                           cRefTxt = cRefTxt + (IF cReftxt <> "" THEN " " ELSE "") + TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE).                */ */
/* /*                    ASSIGN iRadNr     = iRadNr + 1                                                                                                                */ */
/* /*                           iAntLinjer = iAntLinjer - 1.                                                                                                           */ */
/* /*                    PUT UNFORMATTED "<I><R+.9" SUBSTITUTE(cDetaljRad + "</B>"," ",cRefTxt) "</I>".                                                                */ */
/*                END.                                                                                                                                                    */
                   
               /* Skall vi göra sidbryt? */
               IF iRadNr > iKontrollrad AND iAntLinjer > 2  THEN DO:
                   IF iCount = 1 AND iBilagsType = 1 AND iFormatKod = 2 AND iSidNr = 1 THEN DO:
                       RUN SkrivPostGiro.
                   END.
                   PAGE.
                   ASSIGN iSidNr = iSidNr + 1
                          iRadNr = 21. /* 22 */
                   RUN SkrivHeader (iSidNr,cStatusTxt,iCount > 1).
/*                    RUN SkrivHeader (iSidNr,cFakturaNr,cFakturaType,iCount > 1). */
                   PUT UNFORMATTED                                     /* 168,255,168 */
                   SUBSTITUTE("<R&1><P8><C6><FROM><R&2><C112><RECT><BGCOLOR=220,220,220><FILLRECT>",STRING(iRadNr),STRING(iRadNr + 1)) SKIP.
/*                    PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(cDetaljRad,"Prodnr","Beskr","Levert","Str","Fakturert","Enhetspris","Rabatt","Mva","Sum") "</B>" SKIP. */
/*                    PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(cDetaljRad,cHlbl1,cHlbl2,cHlbl3,cHlbl4,cHlbl5,cHlbl6,cHlbl7,cHlbl8,cHlbl9) "</B>" SKIP. */
                   ASSIGN cDetaljRad = SUBSTITUTE(cDetaljRad1,cHlbl1,cHlbl2,cHlbl3,cHlbl4,cHlbl5,cHlbl6,cHlbl7,cHlbl8,cHlbl9) + 
                                       SUBSTITUTE(cDetaljRad2,cHlbl10,cHlbl11,cHlbl12).
                   PUT UNFORMATTED "<B><R" STRING(iRadNr) cDetaljRad "</B>" SKIP.
               END.
               qL:GET-NEXT().
           END.
           PUT UNFORMATTED SUBSTITUTE("<R&1.7><P8><C6><FROM><R&2.7><C112><RECT><BGCOLOR=220,220,220><FILLRECT>",STRING(iSumRad),STRING(iSumRad + 1)) SKIP.
/*            PUT UNFORMATTED SUBSTITUTE("<R&1.7><P8><C6><FROM><R&2.7><C112><RECT><BGCOLOR=220,220,220><FILLRECT>",STRING(iKontrollrad + iExtraRad + 1),STRING(iKontrollrad + iExtraRad + 2)) SKIP. */
/*            PUT UNFORMATTED "<B>" SUBSTITUTE(cSumRad,STRING(65),"Avgfri","Avgpl","Netto","Avrunding","Rabatt","Rabatt%","Mva","Sum inkl. mva") "</B>" SKIP. */
           PUT UNFORMATTED "<B>" REPLACE(SUBSTITUTE(cSumRad,cSlbl1,cSlbl2,cSlbl3,cSlbl4,cSlbl5,cSlbl6,cSlbl7,cSlbl8,cSlbl9),"@1",STRING(iSumRad)) "</B>" SKIP.
           PUT UNFORMATTED REPLACE(SUBSTITUTE(cSumRad,
                                      STRING(dAvgFri,"->,>>>,>>9.99"),
                                      STRING(dAvgPlikt,"->,>>>,>>9.99"),
                                      STRING(dNetto,"->,>>>,>>9.99"),
/*                                       STRING(dAvrund,"->,>>>,>>9.99"), */
                                      STRING(dRabatt,"->,>>>,>>9.99"),
                                      STRING(ROUND(dRabatt / dBruttosum * 100,2),"->>9.99"),
                                      STRING(dMvakr,"->,>>>,>>9.99"),
                                      STRING(dForskudd,"->,>>>,>>9.99"),"<B>" +
                                      STRING(dTotSum,"->,>>>,>>9.99")),"@1",STRING(iSumRad + 1)) + "</B>" SKIP.
           /* Här skrives postgiroblankett ut */
           IF iCount = 1 AND iBilagsType = 1 AND iFormatKod = 2 AND iSidNr = 1 THEN
               RUN SkrivPostGiro.
           IF iCount < iAntEks THEN
               PAGE.
       END.
       qH:GET-NEXT().
       IF NOT qH:QUERY-OFF-END THEN
           PAGE.
   END.
   qH:QUERY-CLOSE().
   DELETE OBJECT qH.
   qL:QUERY-CLOSE().
   DELETE OBJECT qL.
/*        PUT UNFORMATTED                                                                                                    */
/* "<R24><C6><P6><RIGHT=C+4>9801234<C13>Stiga bortennisracket<C43><C52><RIGHT=C+5>0<C59><RIGHT=C+10>0<C71><RIGHT=C+6>" SKIP  */
/* "<R25><C6><RIGHT=C+4>9801234<C13>Stiga bordtennisballer<C43><C52><RIGHT=C+5>0<C59><RIGHT=C+10>0<C71><RIGHT=C+6>"     SKIP */
/* "<R26><C6><RIGHT=C+4>9801234<C13>Craft shorts<C43><C52><RIGHT=C+5>0<C59><RIGHT=C+10>0<C71>"                SKIP.          */
   IF TRIM(cMailAdress) <> "" THEN DO:
       PUT UNFORMATTED "<MAILTO=TO:" + cMailAdress + ">". 
   END.
   OUTPUT CLOSE.
   RUN VisXprint.p (pcRappFil).
   OS-DELETE VALUE(pcRappFil).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fixChkEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fixChkEAN Procedure 
FUNCTION fixChkEAN RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  Räknar ut checksiffra för ean EAN-kod - parameter utan chksiffra
              i.e 12 lång
      Notes:  
  ------------------------------------------------------------------------------*/
      
  cKode = cKode + '0'.
  RUN bibl_chkean.p(INPUT-OUTPUT cKode).
  RETURN cKode.
/*
      DEF VAR iCount1 AS INTE NO-UNDO.
      DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
      DEF VAR iSum AS INTE NO-UNDO.
      DO iCount1 = LENGTH(cKode) TO 1 BY -1:  
          ASSIGN iMulti = IF iMulti = 1 THEN 3 ELSE 1
                 iSum = iSum + INT(SUBSTR(cKode,iCount1,1)) * iMulti.
      END.
      RETURN cKode + string((10 - iSum MODULO 10) MODULO 10).
*/      
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

