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
IF lDirekte AND NOT CAN-DO(SESSION:GET-PRINTERS(),cPrinter) THEN
    RETURN.
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
    hHodeTH  = DYNAMIC-FUNCTION("getTempTable","get_fakturahode.p",cParaString,?).
    hLinjeTH = DYNAMIC-FUNCTION("getTempTable","get_fakturalinje.p",cParaString,?).
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
        "<AT=5,> "   /* "<P12></B><C77><P10>" PAGE-NUMBER FORMAT ">>" SKIP */
      "<R+.8,><C57><P10><B><RIGHT=C+20>" cFakturatype " " cFaktNr "</B>"   /* "<P12></B><C77><P10>" PAGE-NUMBER FORMAT ">>" SKIP */
    /*  "<R+.8> */ "<P10><B><C6>" TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaNavn"):BUFFER-VALUE) cKopiStr
      "<R+.7><C6><P7>"     TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaAdresse1"):BUFFER-VALUE)
      "<R+.7><C6><P7>"     TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostNr"):BUFFER-VALUE) " " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPoststed"):BUFFER-VALUE)
      "<R+.7><C6><P7>" "Telefon"  "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaTelefon"):BUFFER-VALUE) /* "<C26>Bank <C32>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaBankKonto"):BUFFER-VALUE)     */
      "<R+.7><C6><P7>" "Telefaks" "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaTelefaks"):BUFFER-VALUE) /* "<C26>Postgiro <C32>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostgiro"):BUFFER-VALUE) */
      "<R+.7><C6><P7>" "E-post:"  "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaEPost"):BUFFER-VALUE)
      "<R+.7><C6><P7>"   "URL:"     "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaURLAdresse"):BUFFER-VALUE)
      "<R+.7><C6><P7>" "Org.nr"   "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaOrganisasjonsNr"):BUFFER-VALUE)
    /* Kundeadress */
      "<AT=40,><C8><P10>" TRIM(hTTHodeBuff:BUFFER-FIELD("Navn"):BUFFER-VALUE)
      "<R+1><C8><P10>" TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse1"):BUFFER-VALUE)
      (IF TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE) <> "" THEN "<R+1><C8><P10>" + TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE) ELSE "")
      "<R+1><C8><P10>" TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPostNr"):BUFFER-VALUE) " " TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPoststed"):BUFFER-VALUE)
      "<R+1><C8><P10>" TRIM(hTTHodeBuff:BUFFER-FIELD("FaktLand"):BUFFER-VALUE)
    /* Referenser */
      "<AT=70,><C6><P7>Vår ref<C12>: "     TRIM(hTTHodeBuff:BUFFER-FIELD("VaarRef"):BUFFER-VALUE) 
      "<R+.7><C6><P7>Deres ref<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("DeresRef"):BUFFER-VALUE)
      "<R+.7><C6><P7>Referanse<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("Referanse"):BUFFER-VALUE)
    "<AT=100><C.1>___" SKIP
    /* Faktura header info */
    "<R4><C50><P7>" "Side"               "<C62><P7>: "  STRING(iSidNr) SKIP
    "<R4.7><C50><P7>" "Ordrenr"          "<C62><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE > 0 THEN STRING(hTTHodeBuff:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE) ELSE "") SKIP
    "<R5.4><C50><P7>" "Kundenr/Kort/Medlem"          "<C62><P7>: "  STRING(hTTHodeBuff:BUFFER-FIELD("KundeNr"):BUFFER-VALUE) 
        
        (IF hTTHodeBuff:BUFFER-FIELD("Kundekort"):BUFFER-VALUE <> "" THEN " / " + hTTHodeBuff:BUFFER-FIELD("Kundekort"):BUFFER-VALUE ELSE " ")
        (IF hTTHodeBuff:BUFFER-FIELD("Medlemskort"):BUFFER-VALUE <> "" THEN  " / " + hTTHodeBuff:BUFFER-FIELD("Medlemskort"):BUFFER-VALUE ELSE "") SKIP
    "<R6.1><C50><P7>" "Prosjekt"         "<C62><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("KProsjektNr"):BUFFER-VALUE > 0 THEN STRING(hTTHodeBuff:BUFFER-FIELD("KProsjektNr"):BUFFER-VALUE) ELSE "") SKIP
    "<R6.8><C50><P7>" "Leveringsform"   "<C62><P7>: "  hTTHodeBuff:BUFFER-FIELD("LevFormMetode"):BUFFER-VALUE SKIP
    "<R7.5><C50><P7>" "Lev.betingelser" "<C62><P7>: "  hTTHodeBuff:BUFFER-FIELD("LevFormBeskrivelse"):BUFFER-VALUE SKIP
    "<R8.2><C50><P7>" "Valuta"          "<C62><P7>: "  hTTHodeBuff:BUFFER-FIELD("ValKod"):BUFFER-VALUE SKIP
    "<R8.9><C50><P7>" "KID"             "<C62><P7>: "  IF hTTHodeBuff:BUFFER-FIELD("KID"):BUFFER-VALUE > 0 THEN hTTHodeBuff:BUFFER-FIELD("KID"):BUFFER-VALUE ELSE "" SKIP
    "<R9.6><C50><P7>" "Fakturadato"     "<C62><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("FakturertDato"):BUFFER-VALUE = ? THEN "" ELSE STRING(hTTHodeBuff:BUFFER-FIELD("FakturertDato"):BUFFER-VALUE)) SKIP
    "<R10.3><C50><P7>" "Bet.betingelser" "<C62><P7>: "  hTTHodeBuff:BUFFER-FIELD("BetTekst"):BUFFER-VALUE SKIP
    "<R11><C50><P7>" "Forfallsdato"     "<C62><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE = ? THEN "" ELSE STRING(hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE)) SKIP
    "<R11.7><C50><P7>" "Bank"           "<C62><P7>: "  TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaBankKonto"):BUFFER-VALUE) SKIP
    "<R12.4><C50><P7>" "Postgiro"       "<C62><P7>: "  TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostgiro"):BUFFER-VALUE) .
    PUT UNFORMATTED "<USE#1>" hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE "</USE>".
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
   DEFINE VARIABLE pcRappFil      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE iSidNr         AS INTEGER    NO-UNDO.
   DEFINE VARIABLE iRadNr         AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cDetaljRad     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSumRad        AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE qH             AS HANDLE     NO-UNDO.
   DEFINE VARIABLE qL             AS HANDLE     NO-UNDO.
   DEFINE VARIABLE iAntLinjer     AS INTEGER    NO-UNDO.
   DEFINE VARIABLE iAntNotatRader AS INTEGER    NO-UNDO.
   DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cFakturaType   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cFakturaNr     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE iBilagsType    AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cRefTxt        AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE iKontrollRad   AS INTEGER    NO-UNDO.
   DEFINE VARIABLE dBruttoPris    AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dRabKr         AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dAntal         AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE iExtraRad AS INTEGER    NO-UNDO. /* Om vi har iFormatKod = 2 skall vi lägga till vid summarad */
   DEFINE VARIABLE cHlbl1 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl2 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl3 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl4 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl5 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl6 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl7 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl8 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl9 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlblX1 AS CHARACTER  NO-UNDO. /* substitute tar bara 9 parametrar och vi har 110, vi gör en replace mha denna */
   DEFINE VARIABLE cNettoPrisX1 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl1 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl2 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl3 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl4 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl5 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl6 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl7 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl8 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl9 AS CHARACTER  NO-UNDO.

   cDetaljRad = "><P8><C6><RIGHT=C+6>&1<C13>&2<C30>&3<C34><RIGHT=C+4>&4<C38><RIGHT=C+4>&5<C46><RIGHT=C+4>&6<C53><RIGHT=C+4>&10<C60><RIGHT=C+4>&7<C65><RIGHT=C+4>&8<C70><RIGHT=C+7>&9".
   cSumRad    = "<R@1.7><P8><C7><RIGHT=C+7>&1<C15><RIGHT=C+7>&2<C23><RIGHT=C+7>&3<C31><RIGHT=C+7>&4<C39><RIGHT=C+7>&5<C45><RIGHT=C+7>&6<C52><RIGHT=C+7>&7<C60><RIGHT=C+7>&8<C70><RIGHT=C+7>&9".

/*    iFormatkod = 2. */
   /* Hantering av rader för olika layouter */
   /* 1 = Internationell, 2 = Postgiro */
   ASSIGN iKontrollrad = IF iFormatKod = 1 THEN 61 ELSE IF iFormatKod = 2 THEN 34 ELSE 61  /* 34 */
          iExtraRad    = IF iFormatKod = 2 THEN 3 ELSE 0. 

   RUN GetTempFileName in wLibHandle ("fakt", "xpr", output pcRappFil).

   ASSIGN cHlbl1 = "Art.nr"
          cHlbl2 = "Beskr"
          cHlbl3 = "Levert"
          cHlbl4 = "Str"
          cHlbl5 = "Antall"
          cHlbl6 = "Enhetspris"
          cHlbl7 = "Rabatt%"
          cHlbl8 = "Mva%"
          cHlbl9 = "Sum"
          cHlblX1 = "Netto".
   ASSIGN cSlbl1 = "Avgfri"
          cSlbl2 = "Avgpl"
          cSlbl3 = "Netto"
          cSlbl4 = "Avrunding"
          cSlbl5 = "Rabatt"
          cSlbl6 = "Rabatt%"
          cSlbl7 = "Mva"
          cSlbl8 = "Forskudd"
          cSlbl9 = "Å betale".

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
       PUT CONTROL '<PREVIEW=ZoomToWidth>'.
   END.
   ELSE DO:
       ASSIGN cMailAdress = "".
       PUT CONTROL '<PRINTER' + DYNAMIC-FUNCTION('getRapPrinter':U,cPrinter) + '>'.
   END.
   PUT UNFORMATTED "<ALIGN=BASE><FArial><UNITS=MM>" SKIP.
/*    PUT UNFORMATTED "<TOP=10mm><R1>" SKIP. */
   PUT UNFORMATTED "<R14><C40><#1><R18><C75><FRAME#1>" SKIP. /* <15>..<19>*/
/*    PUT UNFORMATTED "<R16><C40><#1><R21><C75><FRAME#1>" SKIP. */
   qH:GET-FIRST().
   REPEAT WHILE NOT qH:QUERY-OFF-END:
       qL:QUERY-PREPARE("FOR EACH " + hTTLinjeBuff:NAME + " WHERE Faktura_id = " +
                        STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE)).

       ASSIGN iBilagstype = hTTHodeBuff:BUFFER-FIELD("BilagsType"):BUFFER-VALUE.
       CASE iBilagstype:
           WHEN 1 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE ""
                              cFakturaType = STRING(cFakturaNr <> "","Faktura/Proformafaktura"). 
           WHEN 2 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE ""
                              cFakturaType = STRING(cFakturaNr <> "","Kreditnota"). 
           WHEN 5 THEN ASSIGN cFakturaNr   = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE ""
                              cFakturaType = "Utbetaling". 
           WHEN 10 THEN ASSIGN cFakturaNr   = ""
                               cFakturaType = "Betalingspåminnelse". 
           OTHERWISE ASSIGN cFakturaNr   = ""
                            cFakturaType = "". 
       END CASE.
       
       DO iCount = 1 TO iAntEks:
           iSidNr = 1.
           RUN SkrivHeader (iSidNr,cFakturaNr,cFakturaType,iCount > 1).
/* PUT UNFORMATTED "<AT=5,160>+5160<AT=10,160>+10160<AT=15,160>+15160<AT=20,160>+20160" SKIP. */
           ASSIGN iRadNr     = 20. /* 22 */
           PUT UNFORMATTED                                     /* 168,255,168 */
           SUBSTITUTE("<R&1><C6><FROM><R&2><C78><RECT><BGCOLOR=220,220,220><FILLRECT>",STRING(iRadNr),STRING(iRadNr + 1)) SKIP.
/*            PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(cDetaljRad,"Prodnr","Beskr","Levert","Str","Antall","Enhetspris","Rabatt","Mva","Sum") "</B>" SKIP. */
           PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(REPLACE(cDetaljRad,"&10",cHlblX1),cHlbl1,cHlbl2,cHlbl3,cHlbl4,cHlbl5,cHlbl6,cHlbl7,cHlbl8,cHlbl9) "</B>" SKIP.
           IF iCount = 1 THEN
               qL:QUERY-OPEN().
           qL:GET-FIRST().
           ASSIGN iAntLinjer = 0.
           REPEAT WHILE NOT qL:QUERY-OFF-END:
               ASSIGN iAntLinjer = iAntLinjer + 1.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE) <> "" THEN
                   ASSIGN iAntLinjer = iAntLinjer + 1.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE) <> "" OR TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE) <> "" THEN
                   ASSIGN iAntLinjer = iAntLinjer + 1.
               qL:GET-NEXT().
           END.
           qL:GET-FIRST().
           REPEAT WHILE NOT qL:QUERY-OFF-END:

               ASSIGN iRadNr     = iRadNr + 1
                      iAntLinjer = iAntLinjer - 1.
               ASSIGN dBruttoPris = ABS(hTTLinjeBuff:BUFFER-FIELD("Linjesum"):BUFFER-VALUE)
                      dRabKr      = ABS(hTTLinjeBuff:BUFFER-FIELD("TotalRabattKr"):BUFFER-VALUE)
                      dAntal      = ABS(hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE)
                      dBruttoPris = (dRabKr + dBruttoPris) / dAntal
/*                       dBruttoPris = ((dAntal * dRabKr) + dBruttoPris) / dAntal */
                      /* TN 6/4-07 Endret slik at Pris feltet benyttes hvis det er utfyllt. Pris eks mva og eks rabatter. */
                      dBruttoPris = IF hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE > 0
                                      THEN hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE
                                      ELSE dBruttoPris
                      dBruttoPris = IF dBruttoPris = ? THEN 0 ELSE dBruttoPris.

         /*            PUT UNFORMATTED SUBSTITUTE(cDetaljRad,STRING(iRadNr), */
               cNettoPrisX1 = STRING(hTTLinjeBuff:BUFFER-FIELD("NettoPris"):BUFFER-VALUE,"->,>>>,>>9.99").
               PUT UNFORMATTED "<R+.9" SUBSTITUTE(replace(cDetaljRad,"&10",cNettoPrisX1),
                   IF STRING(hTTLinjeBuff:BUFFER-FIELD("VareNr"):BUFFER-VALUE) = "" THEN " " ELSE STRING(hTTLinjeBuff:BUFFER-FIELD("VareNr"):BUFFER-VALUE),
                   "<B>" + STRING(hTTLinjeBuff:BUFFER-FIELD("Varetekst"):BUFFER-VALUE) + "</B>",
                   IF hTTLinjeBuff:BUFFER-FIELD("Leveringsdato"):BUFFER-VALUE <> ? THEN STRING(hTTLinjeBuff:BUFFER-FIELD("Leveringsdato"):BUFFER-VALUE) ELSE " ",
                   STRING(hTTLinjeBuff:BUFFER-FIELD("Storl"):BUFFER-VALUE),
                   STRING(hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE),
                   STRING(dBruttoPris,"->,>>>,>>9.99"),
/*                    STRING(hTTLinjeBuff:BUFFER-FIELD("NettoPris"):BUFFER-VALUE,"->,>>>,>>9.99"), */
                   STRING(hTTLinjeBuff:BUFFER-FIELD("LinjeRab%"):BUFFER-VALUE,"->>9.99"),
                   STRING(hTTLinjeBuff:BUFFER-FIELD("Mva%"):BUFFER-VALUE,"->9.99"),
                   STRING(hTTLinjeBuff:BUFFER-FIELD("Linjesum"):BUFFER-VALUE,"->,>>>,>>9.99")).
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE) <> "" THEN DO:
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   PUT UNFORMATTED "<I><R+.9" SUBSTITUTE(REPLACE(cDetaljRad,"&10","") + "</B>"," ",TRIM(STRING(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE))) "</I>".
               END.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE) <> "" OR TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE) <> "" THEN DO:
                   ASSIGN cRefTxt = TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE)
                          cRefTxt = cRefTxt + (IF cReftxt <> "" THEN " " ELSE "") + TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE).
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   PUT UNFORMATTED "<I><R+.9" SUBSTITUTE(REPLACE(cDetaljRad,"&10","") + "</B>"," ",cRefTxt) "</I>".
               END.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("ArbeidsBeskr"):BUFFER-VALUE) <> "" THEN DO:
                   ASSIGN cRefTxt = TRIM(hTTLinjeBuff:BUFFER-FIELD("ArbeidsBeskr"):BUFFER-VALUE).
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   PUT UNFORMATTED "<I><R+.9" SUBSTITUTE(REPLACE(cDetaljRad,"&10","") + "</B>"," ",cRefTxt) "</I>".
               END.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Varespesifikasjon"):BUFFER-VALUE) <> "" THEN DO:
                   ASSIGN cRefTxt = TRIM(hTTLinjeBuff:BUFFER-FIELD("Varespesifikasjon"):BUFFER-VALUE).
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   PUT UNFORMATTED "<I><R+.9" SUBSTITUTE(REPLACE(cDetaljRad,"&10","") + "</B>"," ",cRefTxt) "</I>".
               END.
                   
               /* Skall vi göra sidbryt? */
               IF iRadNr > iKontrollrad AND iAntLinjer > 2  THEN DO:
                   IF iCount = 1 AND iBilagsType = 1 AND iFormatKod = 2 AND iSidNr = 1 THEN DO:
                       RUN SkrivPostGiro.
                   END.
                   PAGE.
                   ASSIGN iSidNr = iSidNr + 1
                          iRadNr = 21. /* 22 */
                   RUN SkrivHeader (iSidNr,cFakturaNr,cFakturaType,iCount > 1).
                   PUT UNFORMATTED                                     /* 168,255,168 */
                   SUBSTITUTE("<R&1><P8><C6><FROM><R&2><C78><RECT><BGCOLOR=220,220,220><FILLRECT>",STRING(iRadNr),STRING(iRadNr + 1)) SKIP.
/*                    PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(cDetaljRad,"Prodnr","Beskr","Levert","Str","Antall","Enhetspris","Rabatt","Mva","Sum") "</B>" SKIP. */
                   PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(REPLACE(cDetaljRad,"&10",cHlblX1),cHlbl1,cHlbl2,cHlbl3,cHlbl4,cHlbl5,cHlbl6,cHlbl7,cHlbl8,cHlbl9) "</B>" SKIP.
               END.
               qL:GET-NEXT().
           END.
           PUT UNFORMATTED SUBSTITUTE("<R&1.7><P8><C6><FROM><R&2.7><C78><RECT><BGCOLOR=220,220,220><FILLRECT>",STRING(iKontrollrad + iExtraRad + 1),STRING(iKontrollrad + iExtraRad + 2)) SKIP.
/*            PUT UNFORMATTED "<B>" SUBSTITUTE(cSumRad,STRING(65),"Avgfri","Avgpl","Netto","Avrunding","Rabatt","Rabatt%","Mva","Sum inkl. mva") "</B>" SKIP. */
           PUT UNFORMATTED "<B>" REPLACE(SUBSTITUTE(cSumRad,cSlbl1,cSlbl2,cSlbl3,cSlbl4,cSlbl5,cSlbl6,cSlbl7,cSlbl8,cSlbl9),"@1",STRING(iKontrollrad + iExtraRad + 1)) "</B>" SKIP.
           PUT UNFORMATTED REPLACE(SUBSTITUTE(cSumRad,
                                      STRING(hTTHodeBuff:BUFFER-FIELD("AvgFriSalg"):BUFFER-VALUE,"->,>>>,>>9.99"),
                                      STRING(hTTHodeBuff:BUFFER-FIELD("AvgPlSalg"):BUFFER-VALUE,"->,>>>,>>9.99"),
                                      STRING(hTTHodeBuff:BUFFER-FIELD("NettoPris"):BUFFER-VALUE,"->,>>>,>>9.99"),
                                      STRING(hTTHodeBuff:BUFFER-FIELD("AvrundingKr"):BUFFER-VALUE,"->,>>>,>>9.99"),
                                      STRING(hTTHodeBuff:BUFFER-FIELD("TotalRabattKr"):BUFFER-VALUE,"->,>>>,>>9.99"),
                                      STRING(hTTHodeBuff:BUFFER-FIELD("TotalRabatt%"):BUFFER-VALUE,"->>9.99"),
                                      STRING(hTTHodeBuff:BUFFER-FIELD("MvaKr"):BUFFER-VALUE,"->,>>>,>>9.99"),
                                      STRING(hTTHodeBuff:BUFFER-FIELD("Forskuddsbetalt"):BUFFER-VALUE,"->,>>>,>>9.99"),"<B>" +
                                      STRING(hTTHodeBuff:BUFFER-FIELD("Totalt"):BUFFER-VALUE,"->,>>>,>>9.99")),"@1",STRING(iKontrollrad + iExtraRad + 2)) + "</B>" SKIP.
/*            PUT UNFORMATTED REPLACE(SUBSTITUTE(cSumRad,                                                               */
/*                                       STRING(1000000,"->,>>>,>>9.99"),                                               */
/*                                       STRING(1000000,"->,>>>,>>9.99"),                                               */
/*                                       STRING(1000000,"->,>>>,>>9.99"),                                               */
/*                                       STRING(1000000,"->,>>>,>>9.99"), /* avrund */                                  */
/*                                       STRING(1000000,"->,>>>,>>9.99"),                                               */
/*                                       STRING(hTTHodeBuff:BUFFER-FIELD("TotalRabatt%"):BUFFER-VALUE,"->>9.99"),       */
/*                                       STRING(1000000,"->,>>>,>>9.99"),                                               */
/*                                       STRING(1000000,"->,>>>,>>9.99"),                                               */
/*                                       STRING(1000000,"->,>>>,>>9.99")),"@1",STRING(iKontrollrad + 3)) + "</B>" SKIP. */
           
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

