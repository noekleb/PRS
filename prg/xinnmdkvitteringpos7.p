&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        :  xinnmdkvitteringpos.p.p
    Purpose     :  Innlesning av kvitteringsfil fra MegaDisc kasse 1.0.

    Syntax      :
                    RUN VALUE(pcInnKvittering + ".p") 
                        (INPUT  piButikkNr,
                         INPUT  piGruppeNr,
                         INPUT  piKasseNr,
                         INPUT  Filer.Katalog + "~\" + Filer.FilNavn,
                         INPUT  plFilId,
                         OUTPUT piAntLinjer
                        ).

    Description :  

    Author(s)   :  Tom Nøkleby
    Created     :  14/02-02
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF INPUT  PARAMETER h_Telleverk AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR cFilError       AS CHAR  NO-UNDO.
DEF VAR cError          AS CHAR  NO-UNDO.
DEF VAR piLoop1         AS INT   NO-UNDO.
DEF VAR cLinje          AS CHAR  NO-UNDO.
DEF VAR cFilNavn        AS CHAR  NO-UNDO.
DEF VAR cOFilNavn       AS CHAR  NO-UNDO.
DEF VAR cFilId          AS CHAR  NO-UNDO.
DEF VAR rKvittoRadRecid AS RECID NO-UNDO.
DEF VAR iAnalyse         AS INT   NO-UNDO.
DEF VAR bOkStatus       AS LOG   NO-UNDO.
DEF VAR iAntISett       AS INT   NO-UNDO.

DEF VAR iButikkNr       AS INT   NO-UNDO.
DEF VAR iGruppeNr       AS INT   NO-UNDO.
DEF VAR iKasseNr        AS INT   NO-UNDO.
DEF VAR cInnKvittering  AS CHAR  NO-UNDO.
DEF VAR iTotAntLinjer   AS INT   NO-UNDO.
DEF VAR iAntBonger      AS INT   NO-UNDO.
DEF VAR cDatoListe      AS CHAR  NO-UNDO.
DEF VAR iRadNr          AS INT   NO-UNDO.
DEF VAR ikvitto_seq     AS INT   NO-UNDO.
DEF VAR cBkuFil         AS CHAR  NO-UNDO.
DEF VAR cPOSKoder       AS CHAR  NO-UNDO.
DEF VAR cTTIdKoder      AS CHAR  NO-UNDO.
DEF VAR cArtikkelNr     AS CHAR  NO-UNDO.
DEF VAR iLopeNr         AS INT   NO-UNDO.
DEF VAR h_PrisKo        AS HANDLE NO-UNDO.
DEF VAR bPengerTilbake  AS LOG   NO-UNDO.
DEF VAR lMDNyFormat     AS LOG   NO-UNDO.
DEF VAR cTekst          AS CHAR  NO-UNDO.
DEF VAR bLoggPrisAvvik  AS LOG   NO-UNDO.
DEF VAR cVinst          AS CHAR  NO-UNDO.
DEF VAR cKontrolltabell AS CHARACTER  NO-UNDO. /* MottaksKontroll av vilken data vi skall testa mot */
DEF VAR cNumFormat      AS CHAR  NO-UNDO.
{megadisc10.i}
 

DEF TEMP-TABLE btt_Rader NO-UNDO LIKE tt_Rader
    FIELD cACT_Date  AS CHAR
    FIELD cInserted  AS CHAR 
    FIELD cLogicDate AS CHAR 
    FIELD cUpdated   AS CHAR.

DEF STREAM InnFil.

/* SKAL SLETTES */ DEF BUFFER bttKvittoRad FOR ttKvittoRad.

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
         HEIGHT             = 23.43
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* Henter liste med varegrupper som inneholder artikkler som er vinstutbetalinger. */
/* {syspara.i 210 1 1 cVinst} */
IF cVinst = "" THEN
    cVinst = "2120".

ASSIGN
                   /* Artikler med disse varegruppene er utbetaling av vinst. */
                   /* Konverteres i "Varesalg".                               */
  cPOSKoder      = "SLG,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,CAS,BAB,CRE,xxx," + 
                   "CHE,REQ,COU,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx," + 
                   "xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx," +
                   "xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx" +
                   "xxx,xxx,xxx,xxx"

  cTTIdKoder     = "001,002,003,004,005,006,007,008,009,010,011,012,013,014,015,016,017,018,019,022,023,050,058,052,053," + 
                   "054,055,056,057,058,059,060,061,062,063,064,065,066,067,068,069," + 
                   "070,071,072,073,080,081,082,096,097,098,099," +
                   "100,101,102,103,104,105,106,107,108,120,121,122,123,130,131,140,141,142,143" +
                   "200,201,202,203"
  .

IF NOT VALID-HANDLE(h_Prisko) THEN
    RUN Prisko.p PERSISTENT SET h_Prisko.

RUN NyFilLogg IN h_Parent (INPUT lFilId, STRING(TODAY) + " " + 
          STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
         " - xinnmdkvitteringpos.p startet.").

FIND Filer NO-LOCK WHERE
    Filer.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE Filer THEN
DO:
    RUN NyFilLogg IN h_Parent (INPUT lFilId, STRING(TODAY) + " " + 
              STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
             " -  Ukjent post 'filer' (" + STRING(lFilId) + ")." + 
             CHR(1) + "1") NO-ERROR.

    RETURN " ** Ukjent Filer post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilId    = STRING(Filer.FilId)
    cFilNavn  = Filer.Katalog + "~\" + Filer.FilNavn
    cOFilNavn = cFilNavn
    cBkuFil   = Filer.Katalog + "~\" + "bku" + "~\" + 
                string(today,"99.99.9999") + "-" +
                entry(1,STRING(TIME,"HH:MM:SS"),":") +   
                entry(2,STRING(TIME,"HH:MM:SS"),":") +   
                entry(3,STRING(TIME,"HH:MM:SS"),":") + "." +   
                Filer.FilNavn
    .
   
/* Tømmer temp-tabellene. */
EMPTY TEMP-TABLE tt_Rader.
EMPTY TEMP-TABLE tt_Hode.

/* Leser første linjen i filen. */
INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  IMPORT STREAM InnFil UNFORMATTED cLinje NO-ERROR.
INPUT STREAM InnFil CLOSE.
ASSIGN lMDNyFormat = NUM-ENTRIES(cLinje,";") > 50.
RUN koblekasse.p (INPUT lFilId,
                  INPUT h_Parent,
                  INPUT 0,
                  INPUT Filer.FilType,
                  INPUT cLinje,
                  OUTPUT iButikkNr,
                  OUTPUT iGruppeNr,
                  OUTPUT iKasseNr
                 ).
IF (iButikkNr = 0 AND iGruppeNr = 0 AND iKasseNr = 0) THEN
    RETURN "** Kobling av kasse misslykkes.".
/* Konverterer butikknummer */
FIND ImpKonv WHERE ImpKonv.EDB-System = "MegaDisk" AND
     ImpKonv.Tabell     = "Butiker" AND
     ImpKonv.EksterntID = TRIM(string(iButikkNr)) NO-LOCK NO-ERROR.
IF AVAILABLE ImpKonv THEN
    iButikkNr = int(ImpKonv.InterntId).

{syspara.i 1 5 1 cTekst}
IF CAN-DO("Ja,yes,true,1",cTekst) THEN
    bLoggPrisAvvik = TRUE. 
ELSE
    bLoggPrisAvvik = false. 
{syspara.i 1 1 25 cKontrolltabell}
IF NOT CAN-DO("1,2",cKontrolltabell) THEN
    ASSIGN cKontrolltabell = "1".

/* Teller opp antall linjer i filen */
RUN Telleverk IN h_Telleverk 
    ("Fil: " + string(Filer.FilId) + " " + 
     Filer.FilNavn + " Teller opp linjer i filen. ") NO-ERROR.
RUN TellOppLinjer.
IF bOkStatus = TRUE THEN
  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Antall linjer i filen: " + STRING(iTotAntLinjer) + "." + 
               CHR(1) + "0").
ELSE DO:
  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Klarte ikk å telle opp linjene i filen." + 
               CHR(1) + "3").
  RETURN "- Klarte ikk å telle opp linjene i filen.".
END.

/* Leser inn filen i temp-table. */
IF lMDNyFormat = TRUE THEN
    RUN InnLesFilNyFormat.
ELSE DO:
        /* NB: Numeric format overstyres ned i InnlesFil. */
        ASSIGN
            cNumFormat = SESSION:NUMERIC-FORMAT
            SESSION:NUMERIC-FORMAT = "AMERICAN"
            .
        RUN InnLesFil.
        ASSIGN
            SESSION:NUMERIC-FORMAT = cNumFormat
            .
    END.
IF bOkStatus = TRUE THEN
  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Lest inn fil i temp-table: " + cOFilNavn + " (" + STRING(iTotAntLinjer) + ")." + 
               CHR(1) + "0").
ELSE DO:
  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Klarte ikke å lese inn fil i temp-table: " + cOFilNavn + 
               CHR(1) + "3").
  RETURN "- Klarte ikke å lese inn fil i temp-table: " + cOFilNavn.
END.

/* Lagrer bongene i dtabasen */
IF CAN-FIND(FIRST ttKvitto) THEN
DO:
    RUN LagreBong.
    IF bOkStatus = TRUE THEN
      RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                    STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                   " - Bongene lagret i databasen: " + STRING(iAntISett) + " bonger." + 
                   CHR(1) + "0").
    ELSE DO:
      RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                    STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                   " - Klarte ikke å lagre bongene i databasen. " + 
                   CHR(1) + "3").
      RETURN "- Klarte ikke å lagre bongene i databasen.".
    END.
END.
RUN Telleverk IN h_Parent ("Kontrollerer at alle datasett er mottatt. Vent litt... ") NO-ERROR.
RUN sjekkdatasett.p (INPUT lFilId, INPUT cDatoListe).
RUN Telleverk IN h_Parent (" ") NO-ERROR.

/* /* Renser bort temporær fil */                                        */
/* IF SEARCH(cFilNavn) <> ? THEN                                         */
/* DO:                                                                   */
/*   RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " +      */
/*                 STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") +    */
/*                " - Filen " + cFilNavn + " kopiert til : " + cBkuFil + */
/*                CHR(1) + "0").                                         */
/*   OS-CREATE-DIR value(Filer.Katalog + "~\bku").                       */
/*   OS-COPY value(cFilNavn) value(cBkuFil).                             */
/*   IF SEARCH(cBkuFil) <> ? THEN                                        */
/*       OS-DELETE VALUE(cFilNavn).                                      */
/* END.                                                                  */

/* Stempler filen som oppdatert */
IF SEARCH(cFilNavn) = ? THEN
DO:
  /*RUN SettFilOppdatert IN h_Parent (INPUT string(cFilId)) NO-ERROR.*/
  DO TRANSACTION:
    FIND CURRENT filer EXCLUSIVE-LOCK.
    ASSIGN
      /* Dette er gjort sammtidig. */
      Filer.Innlest       = TRUE
      Filer.InnlestDato   = TODAY
      Filer.InnlestKl     = TIME
      Filer.InnlestAv     = USERID("SkoTex")
      /* Ferdig så langt */
      Filer.Oppdatert     = TRUE
      Filer.OppdatertDato = TODAY
      Filer.OppdatertKl   = TIME
      Filer.OppdatertAv   = USERID("SkoTex")
      .
  END. /* TRANSACTION */
  FIND CURRENT filer NO-LOCK.
END.

IF VALID-HANDLE(h_Prisko) THEN
    DELETE PROCEDURE h_prisko.

RETURN "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-AssignNyFormat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AssignNyFormat Procedure 
PROCEDURE AssignNyFormat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cRad      AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cBongDato AS CHARACTER  NO-UNDO. /* från "H" */
  DEFINE INPUT  PARAMETER cBongTid  AS CHARACTER  NO-UNDO. /* från "H" */
       ASSIGN
/*  1  A */ TT_Rader.cType          = ENTRY(1,cRad,";")
/*  2  B */ TT_Rader.iRow           = INT(ENTRY(2,cRad,";"))
/*  3  C */ TT_Rader.RECEIPT_NO     = ENTRY(3,cRad,";")         /* joweRECEIPT.RECEIPT_NO     */
/*  4  D */ TT_Rader.ACT_NO         = ENTRY(4,cRad,";")         /* joweRECEIPT.ACT_NO         */
/*  5  E */ TT_Rader.ACT_DATE       = DATE(INT(ENTRY(2,cBongDato,"-")),INT(ENTRY(3,cBongDato,"-")),INT(ENTRY(1,cBongDato,"-"))) /* joweRECEIPT.ACT_DATE       */
/*  6  F */ TT_Rader.LogicDate      = TT_Rader.ACT_DATE         /* joweRECEIPT.LogicDate      */
/*  7  G */ TT_Rader.ACT_TIME       = cBongTid                  /* joweRECEIPT.ACT_TIME       */
/*  8  H */ TT_Rader.OUR_ID         = ENTRY(8,cRad,";")         /* joweRECEIPT.OUR_ID         */
/*  9  I */ TT_Rader.OUR_NO         = ENTRY(9,cRad,";")         /* joweRECEIPT.OUR_NO         */
/* 10  J */ TT_Rader.INDIVID_NO     = ENTRY(10,cRad,";")        /* joweRECEIPT.INDIVID_NO     */
/* 11  K */ TT_Rader.INDIVID_ID     = ENTRY(11,cRad,";")        /* joweRECEIPT.INDIVID_ID     */
/* 12  L */ TT_Rader.NAME           = ENTRY(12,cRad,";")        /* joweRECEIPT.NAME           */
/* 13  M */ TT_Rader.SALESP_ID      = ENTRY(13,cRad,";")        /* joweRECEIPT.SALESP_ID      */
/* 14  N */ TT_Rader.USER_ID        = ENTRY(14,cRad,";")        /* joweRECEIPT.USER_ID        */
/* 15  O */ TT_Rader.EME_NO         = ENTRY(15,cRad,";")        /* joweRECEIPT.EME_NO         */
/* 16  P */ TT_Rader.ACCOUNT_2      = ENTRY(16,cRad,";")        /* joweRECEIPT.ACCOUNT_2      */
/* 17  Q */ TT_Rader.PROJECT_ID     = ENTRY(17,cRad,";")        /* joweRECEIPT.PROJECT_ID     */
/* 18  R */ TT_Rader.NOTES          = ENTRY(18,cRad,";")        /* joweRECEIPT.NOTES          */
/* 19  S */ TT_Rader.INSERTED       = TT_Rader.ACT_DATE         /* joweRECEIPT.INSERTED       */
/* 20  T */ TT_Rader.UPDATED        = TT_Rader.ACT_DATE         /* joweRECEIPT.UPDATED        */
/* 21  U */ TT_Rader.I_USER_NO      = ENTRY(21,cRad,";")        /* joweRECEIPT.I_USER_NO      */
/* 22  V */ TT_Rader.USER_NO        = ENTRY(22,cRad,";")        /* joweRECEIPT.USER_NO        */
/* 23  W */ TT_Rader.CAMP_NO        = ENTRY(23,cRad,";")        /* joweRECEIPT.CAMP_NO        */
/* 24  X */ TT_Rader.CMP_ACT_NO     = ENTRY(24,cRad,";")        /* joweRECEIPT.CMP_ACT_NO     */
/* 25  Y */ TT_Rader.IS_INTERN      = ENTRY(25,cRad,";")        /* joweRECEIPT.IS_INTERN      */
/* 26  Z */ TT_Rader.IS_DONE        = ENTRY(26,cRad,";")        /* joweRECEIPT.IS_DONE        */
/* 27 AA */ TT_Rader.STOCKVALUE     = DECI(ENTRY(27,cRad,";"))  /* joweRECEIPT.STOCKVALUE     */
/* 28 AB */ TT_Rader.FREIGHT        = DECI(ENTRY(28,cRad,";"))  /* joweRECEIPT.FREIGHT        */
/* 29 AC */ TT_Rader.INV_FEE        = DECI(ENTRY(29,cRad,";"))  /* joweRECEIPT.INV_FEE        */
/* 30 AD */ TT_Rader.VAT            = DECI(ENTRY(30,cRad,";"))  /* joweRECEIPT.VAT            */
/* 31 AE */ TT_Rader.ROWSUM         = DECI(ENTRY(31,cRad,";"))  /* joweRECEIPT.ROWSUM         */
/* 32 AF */ TT_Rader.DISCOUNT       = DECI(ENTRY(32,cRad,";"))  /* joweRECEIPT.DISCOUNT       */
/* 33 AG */ TT_Rader.DISCOUNT_P     = DECI(ENTRY(33,cRad,";"))  /* joweRECEIPT.DISCOUNT_P     */
/* 34 AH */ TT_Rader.ROUND_OFF      = DECI(ENTRY(34,cRad,";"))  /* joweRECEIPT.ROUND_OFF      */
/* 35 AI */ TT_Rader.TOTAL          = DECI(ENTRY(35,cRad,";"))  /* joweRECEIPT.TOTAL          */
/* 36 AJ */ TT_Rader.CURR_RATE      = DECI(ENTRY(36,cRad,";"))  /* joweRECEIPT.CURR_RATE      */
/* 37 AK */ TT_Rader.CURRENCYID     = ENTRY(37,cRad,";")        /* joweRECEIPT.CURRENCYID     */
/* 38 AL */ TT_Rader.IS_APPROVE     = ENTRY(38,cRad,";")        /* joweRECEIPT.IS_APPROVE     */
/* 39 AM */ TT_Rader.IS_SENT        = ENTRY(39,cRad,";")        /* joweRECEIPT.IS_SENT        */
/* 40 AN */ TT_Rader.BUYER_TYPE     = ENTRY(40,cRad,";")        /* joweRECEIPT.BUYER_TYPE     */
/* 41 AO */ TT_Rader.BALANCE_NO     = ENTRY(41,cRad,";")        /* joweRECEIPT.BALANCE_NO     */
/* 42 AP */ TT_Rader.TillType       = ENTRY(42,cRad,";")        /* joweRECEIPT.TillType       */
/* 43 AQ */ TT_Rader.TillUnitId     = ENTRY(43,cRad,";")        /* joweRECEIPT.TillUnitId     */
/* 44 AR */ TT_Rader.S_CHANGE       = DECI(ENTRY(44,cRad,";"))  /* joweRECEIPT.S_CHANGE       */
/* 45 AS */ TT_Rader.PRINT_SAP      = ENTRY(45,cRad,";")        /* joweRECEIPT.PRINT_SAP      */
/* 46 AT */ TT_Rader.ORIGIN_NO      = ENTRY(46,cRad,";")        /* joweRECEIPT.ORIGIN_NO      */
/* 47 AU */ TT_Rader.OFFLINE_NO     = ENTRY(47,cRad,";")        /* joweRECEIPT.OFFLINE_NO     */
/* 48 AV */ TT_Rader.CUSTGRP_ID     = ENTRY(48,cRad,";")        /* joweRECEIPT.CUSTGRP_ID     */
/* 49 AW */ TT_Rader.SHOP_ID        = ENTRY(49,cRad,";")        /* joweRECEIPT.SHOP_ID        */
/* 50 AX */ TT_Rader.CASHREG_NO     = INT(ENTRY(50,cRad,";"))   /* joweRECEIPT.CASHREG_NO     */
/* 51 AY */ TT_Rader.SIGNATURE      = ENTRY(51,cRad,";")        /* joweRECEIPT.SIGNATURE      */
/* 52 AZ */ TT_Rader.SUPP_PRICE     = DECI(ENTRY(52,cRad,";"))  /* joweRECEIPT.SUPP_PRICE     */
/* 53 BA */ TT_Rader.CARTYPEID      = ENTRY(53,cRad,";")        /* joweRECEIPT.CARTYPEID      */
/* 54 BB */ TT_Rader.CREDCARDID     = ENTRY(54,cRad,";")        /* joweRECEIPT.CREDCARDID     */
/* 55 BC */ TT_Rader.LEGETIMAT      = ENTRY(55,cRad,";")        /* joweRECEIPT.LEGETIMAT      */
/* 56 BD */ TT_Rader.THEIR_ID       = ENTRY(56,cRad,";")        /* joweRECEIPT.THEIR_ID       */
/* 57 BE */ TT_Rader.EME_NAME       = ENTRY(57,cRad,";")        /* joweRECEIPT.EME_NAME       */
/* 58 BF */ TT_Rader.IS_STAT        = ENTRY(58,cRad,";")        /* joweRECEIPT.IS_STAT        */
/* 59 BG */ TT_Rader.IS_REPORT      = ENTRY(59,cRad,";")        /* joweRECEIPT.IS_REPORT      */
/* 60 BH */ TT_Rader.IS_STAFF       = ENTRY(60,cRad,";").        /* joweRECEIPT.IS_STAFF       */
       ASSIGN
/* 61 BI */ TT_Rader.IS_LOCSOLD     = ENTRY(61,cRad,";")        /* joweRECEIPT.IS_LOCSOLD     */
/* 62 BJ */ TT_Rader.PRODUCT_NO     = ENTRY(62,cRad,";")        /* joweREC_ROW.PRODUCT_NO     */ 
/* 63 BK */ TT_Rader.UNIT_ID        = ENTRY(63,cRad,";")        /* joweREC_ROW.UNIT_ID        */ 
/* 64 BL */ TT_Rader.PriceTypeId    = ENTRY(64,cRad,";")        /* joweREC_ROW.PriceTypeId    */ 
/* 65 BM */ TT_Rader.QUANTITY       = DECI(ENTRY(65,cRad,";"))  /* joweREC_ROW.QUANTITY       */ 
/* 66 BN */ TT_Rader.STK_CONVF      = DECI(ENTRY(66,cRad,";"))  /* joweREC_ROW.STK_CONVF      */ 
/* 67 BO */ TT_Rader.L_SUPP_PRICE   = DECI(ENTRY(67,cRad,";"))  /* joweREC_ROW.SUPP_PRICE     */ 
/* 68 BP */ TT_Rader.UNIT_PRICE     = DECI(ENTRY(68,cRad,";"))  /* joweREC_ROW.UNIT_PRICE     */ 
/* 69 BQ */ TT_Rader.AMOUNT         = DECI(ENTRY(69,cRad,";"))  /* joweREC_ROW.AMOUNT         */ 
/* 70 BR */ TT_Rader.L_DISCOUNT_P   = DECI(ENTRY(70,cRad,";"))  /* joweREC_ROW.DISCOUNT_P     */ 
/* 71 BS */ TT_Rader.L_DISCOUNT     = DECI(ENTRY(71,cRad,";"))  /* joweREC_ROW.DISCOUNT       */ 
/* 72 BT */ TT_Rader.VAT_P          = DECI(ENTRY(72,cRad,";"))  /* joweREC_ROW.VAT_P          */ 
/* 73 BU */ TT_Rader.L_VAT          = DECI(ENTRY(73,cRad,";"))  /* joweREC_ROW.VAT            */ 
/* 74 BV */ TT_Rader.CONTRACTNO     = ENTRY(74,cRad,";")        /* joweREC_ROW.CONTRACTNO     */ 
/* 75 BW */ TT_Rader.GROUP_NO       = ENTRY(75,cRad,";")        /* joweREC_ROW.GROUP_NO       */ 
/* 76 BX */ TT_Rader.ACCOUNT_4      = ENTRY(76,cRad,";")        /* joweREC_ROW.ACCOUNT_4      */ 
/* 77 BY */ TT_Rader.ACCOUNT_5      = ENTRY(77,cRad,";")        /* joweREC_ROW.ACCOUNT_5      */ 
/* 78 BZ */ TT_Rader.IS_MAIN_ROW    = ENTRY(78,cRad,";")        /* joweREC_ROW.IS_MAIN_ROW    */ 
/* 79 CA */ TT_Rader.IS_TEXT        = ENTRY(79,cRad,";")        /* joweREC_ROW.IS_TEXT        */ 
/* 80 CB */ TT_Rader.IS_MANDISC     = ENTRY(80,cRad,";")        /* joweREC_ROW.IS_MANDISC     */ 
/* 81 CC */ TT_Rader.IS_DISC_P      = ENTRY(81,cRad,";")        /* joweREC_ROW.IS_DISC_P      */ 
/* 82 CD */ TT_Rader.IS_N_PRICE     = ENTRY(82,cRad,";")        /* joweREC_ROW.IS_N_PRICE     */ 
/* 83 CE */ TT_Rader.PRODUCT_ID     = ENTRY(83,cRad,";")        /* joweREC_ROW.PRODUCT_ID     */ 
/* 84 CF */ TT_Rader.DESCRIPT       = ENTRY(84,cRad,";")        /* joweREC_ROW.DESCRIPT       */ 
/* 85 CG */ TT_Rader.PRODGR_ID      = ENTRY(85,cRad,";")        /* joweREC_ROW.PRODGR_ID      */ 
/* 86 CH */ TT_Rader.ORIGIN_ID      = ENTRY(86,cRad,";")        /* joweREC_ROW.ORIGIN_ID      */ 
/* 87 CI */ TT_Rader.PROD_CLASS     = ENTRY(87,cRad,";")        /* joweREC_ROW.PROD_CLASS     */ 
/* 88 CJ */ TT_Rader.DISC_CUST      = DECI(ENTRY(88,cRad,";"))  /* joweREC_ROW.DISC_CUST      */ 
/* 89 CK */ TT_Rader.DISC_EMPL      = DECI(ENTRY(89,cRad,";"))  /* joweREC_ROW.DISC_EMPL      */ 
/* 90 CL */ TT_Rader.DISC_ACT       = DECI(ENTRY(90,cRad,";"))  /* joweREC_ROW.DISC_ACT       */ 
/* 91 CM */ TT_Rader.CRED_QTY       = INT(ENTRY(91,cRad,";"))   /* joweREC_ROW.CRED_QTY       */
/* 92 CN */ TT_Rader.CUSTORD_NO     = ENTRY(92,cRad,";")        /* joweREC_ROW.CUSTORD_NO     */
/* 93 CO */ TT_Rader.CORDROW_NO     = ENTRY(93,cRad,";")        /* joweREC_ROW.CORDROW_NO     */ 
/* 94 CP */ TT_Rader.REPORT_CODE    = ENTRY(94,cRad,";")        /* joweREC_ROW.REPORT_CODE    */ 
/* 95 CQ */ TT_Rader.STAT_CODE      = ENTRY(95,cRad,";")        /* joweREC_ROW.STAT_CODE      */ 
/* 96 CR */ TT_Rader.IS_PRESOLD     = ENTRY(96,cRad,";")        /* joweREC_ROW.IS_PRESOLD     */ 
/* 97 CS */ TT_Rader.L_OUR_NO       = ENTRY(97,cRad,";")        /* joweREC_ROW.OUR_NO         */ 
/* 98 CT */ TT_Rader.UnitPriceWoAct = DECI(ENTRY(98,cRad,";"))  /* joweREC_ROW.UnitPriceWoAct */ 
/* 99 CU */ TT_Rader.DiscTypeId     = ENTRY(99,cRad,";")        /* joweREC_ROW.DiscTypeId     */ 
/*100 CV */ TT_Rader.PAYTYPE_ID     = ENTRY(100,cRad,";")       /* joweREC_PAY.PAYTYPE_ID     */ 
/*101 CW */ TT_Rader.PAYMENT_ID     = ENTRY(101,cRad,";")       /* joweREC_PAY.PAYMENT_ID     */ 
/*102 CX */ TT_Rader.RATE_OUT       = DECI(ENTRY(102,cRad,";")) /* joweREC_PAY.RATE_OUT       */ 
/*103 CY */ TT_Rader.P_AMOUNT       = DECI(ENTRY(103,cRad,";")) /* joweREC_PAY.AMOUNT         */ 
/*104 CZ */ TT_Rader.AMOUNT_ORG     = DECI(ENTRY(104,cRad,";")) /* joweREC_PAY.AMOUNT_ORG     */ 
/*105 DA */ TT_Rader.PAIDAMOUNT     = DECI(ENTRY(105,cRad,";")) /* joweREC_PAY.PAIDAMOUNT     */ 
/*106 DB */ TT_Rader.AMOUNT_RET     = DECI(ENTRY(106,cRad,";")). /* joweREC_PAY.AMOUNT_RET     */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Avrunding) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Avrunding Procedure 
PROCEDURE Avrunding :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr     = ttKvitto.ButikkNr
       ttKvittoRad.GruppeNr     = ttKvitto.GruppeNr
       ttKvittoRad.KasseNr      = ttKvitto.KasseNr
       ttKvittoRad.TransDato    = ttKvitto.Dato
       ttKvittoRad.Tid          = tt_Rader.Act_Time
       ttKvittoRad.BongNr       = ttKvitto.BongNr

       ttKvittoRad.LinjeNr      = tt_Rader.iRow + 10 /* Sikre at linjenummer ikke smeller */
       ttKvittoRad.OriginalData = tt_Rader.OriginalData
       ttKvittoRad.TTId         = 78
       ttKvittoRad.TBId         = 1
       ttKvittoRad.StrekKode    = ""
       
       ttKvittoRad.TEXT_1       = ""
       ttKvittoRad.BongPris     = ttKvitto.Avrunding
       ttKvittoRad.LinjeSum     = ttKvitto.Avrunding
       ttKvittoRad.Antall       = 0
       .
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Betalingskort) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Betalingskort Procedure 
PROCEDURE Betalingskort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr     = ttKvitto.ButikkNr
       ttKvittoRad.GruppeNr     = ttKvitto.GruppeNr
       ttKvittoRad.KasseNr      = ttKvitto.KasseNr
       ttKvittoRad.TransDato    = ttKvitto.Dato
       ttKvittoRad.Tid          = tt_Rader.Act_Time
       ttKvittoRad.BongNr       = ttKvitto.BongNr

       ttKvittoRad.LinjeNr      = tt_Rader.iRow
       ttKvittoRad.OriginalData = tt_Rader.OriginalData
       ttKvittoRad.TTId         = tt_Rader.TTId
       ttKvittoRad.TBId         = tt_Rader.TBId
       ttKvittoRad.StrekKode    = ""
       
       ttKvittoRad.TEXT_1       = tt_Rader.PayType_Id /* Card name */
       ttKvittoRad.BongPris     = tt_Rader.Amount_Org
       ttKvittoRad.LinjeSum     = tt_Rader.Amount_Org
       /*ttKvittoRad.kpsxKvant  = DEC(ldPost.amount2) / 100*/
       ttKvittoRad.Antall       = 0
       /* Kvittohode. */
       ttKvitto.flBankKort      = TRUE 
       ttKvitto.flBetalingskort = TRUE 
       ttKvitto.flKreditkort    = TRUE 
       .
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BongHode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BongHode Procedure 
PROCEDURE BongHode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND ttKvitto NO-LOCK WHERE
      RECID(ttKvitto) = tt_Rader.BongRecid NO-ERROR.
  ASSIGN
      bPengerTilbake = FALSE
      .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doKvittoHode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doKvittoHode Procedure 
PROCEDURE doKvittoHode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/
  CREATE ttKvitto.
  ASSIGN
      ttKvitto.kvitto_seq  = iKvitto_seq
      ttKvitto.ButikkNr    = iButikkNr
      ttKvitto.GruppeNr    = iGruppeNr
      ttKvitto.KasseNr     = iKasseNr
      ttKvitto.Dato        = tt_Rader.Act_Date
      ttKvitto.Tid         = int(entry(1,tt_Rader.Act_Time,":")) * 3600 + 
                             int(entry(2,tt_Rader.Act_Time,":")) * 60
      ttKvitto.BongNr      = dec(substring(tt_Rader.Receipt_No,8))
      ttKvitto.KassererNr  = dec(tt_Rader.Salesp_Id)
      ttKvitto.Belop       = tt_Rader.RowSum
      ttKvitto.BruttoBelop = tt_Rader.TOTAL
      ttKvitto.TotalExkl   = tt_Rader.RowSum - tt_Rader.Vat
      ttKvitto.Avrunding   = tt_Rader.ROUND_Off
      ttKvitto.BongStatus  = 1 /* Under klargjøring */
      ttKvitto.OpdKvit     = FALSE
      ttKvitto.GruppeNr    = 1
      ttKvitto.KortType    = 0
      ttKvitto.Moms1       = tt_Rader.Vat
      ttKvitto.MomsTot     = tt_Rader.Vat
      ttKvitto.RabKr       = tt_Rader.Discount
      ttKvitto.Rab%        = tt_Rader.Discount_P
      tt_Rader.BongRecid   = RECID(ttKvitto)
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FordelMva) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FordelMva Procedure 
PROCEDURE FordelMva :
/*------------------------------------------------------------------------------
  Purpose:     Fordeling av Mva på de varelinjer som skal ha Mva.
               Eventuell avrudning/resthåndtering legges på den rad som 
               har størst salgsverdi.
               
               Rutinen kjøres fra doLogId50. Dvs ved slutten av hver bong.
               
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bufttKvittoRad  FOR ttKvittoRad.
  DEF BUFFER buf2ttKvittoRad FOR ttKvittoRad.

  DEF VAR pRadRecid    AS RECID NO-UNDO.
  DEF VAR plMaksBelop  AS DEC   NO-UNDO.
  DEF VAR plTildeltMva AS DEC   NO-UNDO.
  DEF VAR plAndel%     AS DEC   NO-UNDO.

  /* Leser Mva spesifikasjonspostene. */
  LES-MVA-POSTER:
  FOR EACH bufttKvittoRad WHERE
      bufttKvittoRad.kvitto_seq = ttKvitto.Kvitto_seq AND
      bufttKvittoRad.LogId = "9" AND
      bufttKvittoRad.LinjeSum <> 0 AND
      bufttKvittoRad.Makulert = FALSE:

      ASSIGN
          plTildeltMva = 0
          .
      /* Leser alle logid 4 - varesalgsposter for den aktuelle mvakoden. */
      KVITTORADEN:
      FOR EACH buf2ttKvittoRad WHERE
          buf2ttKvittoRad.kvitto_seq = ttKvitto.Kvitto_Seq AND
          buf2ttKvittoRad.LogId      = "4" AND
          buf2ttKvittoRad.Mva%       = bufttKvittoRad.Mva% AND
          buf2ttKvittoRad.Makulert   = FALSE
          BREAK BY buf2ttKvittoRad.LinjeSum:

          ASSIGN
              /* Beregner % andel og posterer andel av Mva. */
              plAndel%              = (buf2ttKvittoRad.LinjeSum / bufttKvittoRad.BongPris) * 100
              /* Posterer andel av Mva beløp. */
              buf2ttKvittoRad.MvaKr   = ROUND((bufttKvittoRad.LinjeSum * plAndel%) / 100,2)
              buf2ttKvittoRad.MvaKode = bufttKvittoRad.MvaKode
              plTildeltMva            = plTildeltMva + buf2ttKvittoRad.MvaKr
              .
          /* Håndterer eventuel avrundingsfeil. */
          IF LAST(buf2ttKvittoRad.LinjeSum) THEN
          DO:
              ASSIGN
                  buf2ttKvittoRad.MvaKr = buf2ttKvittoRad.MvaKr + 
                                          (bufttKvittoRad.LinjeSum - plTildeltMva)
                  plTildeltMva          = 0
                  .
          END.

      END. /* KVITTORADEN */
  END. /* LES-MVA-POSTER */
               
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-HentVareLinjeInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentVareLinjeInfo Procedure 
PROCEDURE HentVareLinjeInfo :
/*------------------------------------------------------------------------------
  Purpose:     Hent varelinjeinfo
  Parameters:  <none>
  Notes:       

------------------------------------------------------------------------------*/
  /* Henter mva koden.                                      */
  /* Forutsetter at det ikke ligger to koder med samme mva% */
  FIND FIRST Moms NO-LOCK WHERE
      Moms.MomsProc = BongLinje.Mva% NO-ERROR.
  IF AVAILABLE Moms THEN
      ASSIGN
      BongLinje.MvaGr         = Moms.MomsKod
      BongLinje.MvaGruppeNavn = Moms.Beskrivelse
      .
  ELSE DO:
      ASSIGN
      BongLinje.MvaGr         = 0
      BongLinje.MvaGruppeNavn = "** Ukjent momskode **"
      cError = cError + 
               (IF cError = ""
                  THEN ""
                  ELSE chr(10)) + 
               "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
               " Ukjent MvaKode på transaksjonen " + string(BongLinje.MvaGr) + 
               "."
      cFilError = cFilError + 
               (IF cFilError = ""
                  THEN ""
                  ELSE "|") + 
               " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
               " Ukjent MvaKode på transaksjonen " + string(BongLinje.MvaGr) + 
               "." + CHR(1) + "2"
      .
  END.

  /* Henter feilkodeteksten */
  FIND FIRST FeilKode NO-LOCK WHERE
      FeilKode.FeilKode = BongLinje.FeilKode NO-ERROR.
  IF AVAILABLE FeilKode THEN
      BongLinje.FeilKodeTekst = FeilKode.Beskrivelse.
  ELSE DO:
      ASSIGN  
          cError = cError + 
                   (IF cError = ""
                      THEN ""
                      ELSE chr(10)) + 
                   "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                   " Ukjent feilkode på transaksjonen " + string(BongLinje.FeilKode) + 
                   "."
          cFilError = cFilError + 
                   (IF cFilError = ""
                      THEN ""
                      ELSE "|") + 
                   " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                   " Ukjent feilkode på transaksjonen " + string(BongLinje.FeilKode) + 
                   "." + CHR(1) + "2"
          BongLinje.FeilKodeTekst = "** Ukjent feilkode **".
  END.

  /* Henter tiltakskode */
  FIND FIRST KravKode NO-LOCK WHERE
      KravKode.KravKode = BongLinje.NotatKode NO-ERROR.
  IF AVAILABLE KravKode THEN
      BongLinje.NotatKodeTekst = KravKode.Beskrivelse.
  ELSE DO:
      ASSIGN  
          cError = cError + 
                   (IF cError = ""
                      THEN ""
                      ELSE chr(10)) + 
                   "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                   " Ukjent tiltakskode på transaksjonen " + string(BongLinje.FeilKode) + 
                   "."
          cFilError = cFilError + 
                   (IF cFilError = ""
                      THEN ""
                      ELSE "|") + 
                   " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                   " Ukjent tiltakskode på transaksjonen " + string(BongLinje.FeilKode) + 
                   "." + CHR(1) + "2"
          BongLinje.NotatKodeTekst = "** Ukjent tiltakskode **"
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InnLesFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InnLesFil Procedure 
PROCEDURE InnLesFil :
DEF VAR piLinjeNr    AS INT   NO-UNDO.
  DEF VAR pcLinje      AS CHAR  NO-UNDO.
  DEF VAR pcOLinje     AS CHAR  NO-UNDO.
  DEF VAR piSettNr     AS INT   NO-UNDO.
  DEF VAR pdDato       AS DATE  NO-UNDO.
  DEF VAR pcDato       AS CHAR  NO-UNDO.
  DEF VAR plDataSettId AS DEC   NO-UNDO.
  DEF VAR pcSokMaske   AS CHAR  NO-UNDO.
  DEF VAR pcOSokMaske  AS CHAR  NO-UNDO.
  DEF VAR pbKoblet     AS LOG   NO-UNDO.
  DEF VAR prRowId      AS ROWID NO-UNDO.
  DEF VAR piAntISett   AS INT   NO-UNDO.
  DEF VAR piLoop       AS INT   NO-UNDO.
  DEF VAR pcTekst      AS CHAR  NO-UNDO.
  DEF VAR piEntry      AS INT   NO-UNDO.
  DEF VAR pcPOS        AS CHAR  NO-UNDO.
  DEF VAR piAr         AS INT   NO-UNDO.
  DEF VAR pc2Dato      AS CHAR  NO-UNDO.

  DEF VAR cNumericFormat AS CHAR NO-UNDO.

  /* Sjekker om det kommer . eller , */
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  NUMERICFORMAT:
  REPEAT:
      IMPORT STREAM InnFil UNFORMATTED cLinje NO-ERROR.
      IF ENTRY(1,cLinje," ") = '"L"' THEN
      DO:
          /* Amerikans format i filen */
          IF (NUM-ENTRIES(ENTRY(67,cLinje," "),".") > 1) OR 
             (NUM-ENTRIES(ENTRY(69,cLinje," "),".") > 1) OR 
             (NUM-ENTRIES(ENTRY(71,cLinje," "),".") > 1) THEN
          DO:
              SESSION:NUMERIC-FORMAT = "American".
              LEAVE NUMERICFORMAT. 
          END.
          /* Europeisk format i filen */
          IF (NUM-ENTRIES(ENTRY(67,cLinje," "),",") > 1) OR 
             (NUM-ENTRIES(ENTRY(69,cLinje," "),",") > 1) OR 
             (NUM-ENTRIES(ENTRY(71,cLinje," "),",") > 1) THEN
          DO:
              SESSION:NUMERIC-FORMAT = "European".
              LEAVE NUMERICFORMAT. 
          END.
      END.
  END. /* NUMERICFORMAT */
  INPUT STREAM InnFil CLOSE.

  ASSIGN
      iantLinjer  = 0
      pcSokMaske  = ""
      pcOSokMaske = ""
      cDatoListe  = ""
      prRowId     = ?
      pcOLinje    = ""
      iAntBonger  = 0
      iKvitto_seq = 0
      bOkStatus   = FALSE
      .

  FIND LAST FilLinjer OF Filer NO-LOCK NO-ERROR.
  IF AVAILABLE FilLinjer THEN
      piLinjeNr = FilLinjer.LinjeNr + 1.
  ELSE
      piLinjeNr = 1.

  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Leser inn i temp-table." + 
               CHR(1) + "0").

  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  /* INPUT STREAM InnFil THROUGH VALUE("./ldparse " + cOFilNavn) NO-ECHO. */
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    CREATE TT_Rader.
    IMPORT STREAM InnFil /*DELIMITER ";"*/ TT_Rader.

    /* Ligger i gammelt datoformat.                                               */
    /* I nytt format skal det legges ut i format dmy med dd.mm.åååå fra Øisten.   */
    /* Det nye formatet glir riktig inn når det er satt opp med norsk datoformat. */
    IF NUM-ENTRIES(string(TT_Rader.ACT_Date),'/') = 3 THEN
    KONVERTER-DATO:
    DO:
        IF TT_Rader.ACT_Date <> ? THEN
        DO:
            ASSIGN
            pc2Dato             = STRING(TT_Rader.ACT_Date)
            TT_Rader.ACT_Date   = ?
            piAr                = IF INT(entry(1,pc2Dato,"/")) < 50
                                    THEN 2000
                                    ELSE 1900
            TT_Rader.ACT_Date   = date(int(entry(2,pc2Dato,"/")),
                                       int(entry(3,pc2Dato,"/")),
                                       piAr + int(entry(1,pc2Dato,"/")))
            .
        END.
        IF TT_Rader.Inserted <> ? THEN
        DO:
            ASSIGN
            pc2Dato             = STRING(TT_Rader.Inserted)
            TT_Rader.Inserted   = ?
            piAr                = IF INT(entry(1,pc2Dato,"/")) < 50
                                    THEN 2000
                                    ELSE 1900
            TT_Rader.Inserted   = date(int(entry(2,pc2Dato,"/")),
                                       int(entry(3,pc2Dato,"/")),
                                       piAr + int(entry(1,pc2Dato,"/")))
            .
        END.
        IF TT_Rader.LogicDate <> ? THEN
        DO:
            ASSIGN
            pc2Dato             = STRING(TT_Rader.LogicDate)
            TT_Rader.LogicDate  = ?
            piAr                = IF INT(entry(1,pc2Dato,"/")) < 50
                                    THEN 2000
                                    ELSE 1900
            TT_Rader.LogicDate  = date(int(entry(2,pc2Dato,"/")),
                                       int(entry(3,pc2Dato,"/")),
                                       piAr + int(entry(1,pc2Dato,"/")))
            .
        END.
        IF TT_Rader.Updated <> ? THEN
        DO:
            ASSIGN
            pc2Dato             = STRING(TT_Rader.Updated)
            TT_Rader.Updated    = ?
            piAr                = IF INT(entry(1,pc2Dato,"/")) < 50
                                    THEN 2000
                                    ELSE 1900
            TT_Rader.Updated    = date(int(entry(2,pc2Dato,"/")),
                                       int(entry(3,pc2Dato,"/")),
                                       piAr + int(entry(1,pc2Dato,"/")))
            .
        END.
    END. /* KONVERTER-DATO */
    ASSIGN
        iantLinjer  = iantLinjer + 1
        .
    /* Setter transaksjonskoden */
    CASE tt_Rader.cType:
        WHEN "H" THEN
        DO:
            ASSIGN
                tt_Rader.POS = "H"
                .
        END.
        WHEN "L" THEN
        DO:
            ASSIGN
                tt_Rader.POS = "SLG"
                .
        END.
        WHEN "P" THEN
        DO:
            ASSIGN
                tt_Rader.POS =      IF tt_Rader.PayType_Id = "CASH"        THEN "CAS"
                               ELSE IF tt_Rader.PayType_Id = "RIKSCOUPON"  THEN "COU"
                               ELSE IF tt_Rader.PayType_Id = "RIKSKOUPONG" THEN "COU"
                               ELSE IF tt_Rader.PayType_Id = "RIKSKUPONG"  THEN "COU"
                               ELSE IF tt_Rader.PayType_Id = "COUPON"      THEN "COU"
                               ELSE IF tt_Rader.PayType_Id = "BABS"        THEN "BAB"
                               ELSE IF tt_Rader.PayType_Id = "AMEX"        THEN "BAB"
                               ELSE IF tt_Rader.PayType_Id = "FINAX"       THEN "BAB"
                               ELSE IF tt_Rader.PayType_Id = "CREDCARD"    THEN "CRE"
                               ELSE IF tt_Rader.PayType_Id = "CASHCARD"    THEN "CRE" /* Behandles som kreditt */
                               ELSE IF tt_Rader.PayType_Id = "CHECKS"      THEN "CHE"
                               ELSE IF tt_Rader.PayType_Id = "REQUISIT"    THEN "REQ"
                               ELSE tt_Rader.PayType_Id
                .                                
        END.

    END CASE.

    /* Setter originaldata */
/*     RUN Originaldata. */

    /* Skaper peker til bonger i TEMP-FILE.                                  */
    /* Flagger også om bongen inneholder totaler eller om det kun er en info */
    /* kvittering.                                                           */
    IF can-do("H",tt_Rader.cType) THEN 
    KVITTERING:
    DO:
      IF NOT CAN-FIND(tt_Hode WHERE 
          tt_Hode.Act_Date   = tt_Rader.Act_Date   AND
          tt_Hode.Act_Time   = tt_Rader.Act_Time   AND
          tt_Hode.Receipt_no = tt_Rader.Receipt_no) THEN 
      OPPSTANDELSEN:
      DO:
        CREATE tt_Hode.
        ASSIGN
            tt_Hode.Act_Date   = tt_Rader.Act_Date   
            tt_Hode.Act_Time   = tt_Rader.Act_Time   
            tt_Hode.Receipt_no = tt_Rader.Receipt_no 
            iAntBonger         = iAntBonger + 1
            .
        ASSIGN
            iKvitto_seq         = iKvitto_seq + 1
            tt_Hode.iKvitto_Seq = iKvitto_seq
            .
        /* Oppretter bongHode. */
        RUN doKvittoHode. 
      END. /* OPPSTANDELSEN */
    END. /* KVITTERING */

    /* Flagger SL kort */
    IF CAN-DO("L",tt_Rader.cType) THEN
    DO:
        /*
        IF CAN-FIND(SlKort WHERE
                    SlKort.KortId = tt_Rader.Product_Id) THEN
        */
/*         IF CAN-FIND(FIRST AnalyseArtikkel WHERE                               */
/*                     AnalyseArtikkel.ArtikkelNr = dec(tt_Rader.Product_Id) AND */
/*                     AnalyseArtikkel.Aktiv      = TRUE AND                     */
/*                     AnalyseArtikkel.StartDato <= tt_Rader.Act_Date AND        */
/*                     AnalyseArtikkel.SluttDato >= tt_Rader.Act_Date) THEN      */
/*             ASSIGN                                                            */
/*               tt_Hode.iSlKort = 1.                                            */
    END.

    IF iAntLinjer MODULO 100 = 0 THEN
    DO:
      RUN Telleverk IN h_Telleverk 
          ("Fil: " + string(Filer.FilId) + " " + 
           Filer.FilNavn + " Leser record " + STRING(iAntLinjer) +
           " av " + STRING(iTotAntLinjer) + ".") NO-ERROR.
    END.

  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  /* Peller bort bonger som ikke er flagget. */
  /*
  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Tar bort kvitteringer som ikke er flagget." + 
               CHR(1) + "0").

  FOR EACH tt_Hode WHERE 
      tt_Hode.iAnalyse = 0:
      FIND ttKvitto WHERE
          ttKvitto.Kvitto_Seq = tt_Hode.iKvitto_Seq NO-ERROR.
      IF AVAILABLE ttKvitto THEN
          DELETE ttKvitto.
      DELETE tt_Hode.
  END.
  */

  ASSIGN
      piLoop = 0
      .

  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Konverterer kvittering." + 
               CHR(1) + "0").

  /* Går igjennom alle poster, kvittering for kvittering.       */
  /* MD-Postene er sortert på Act_Date, Act_Time og Receipt_No. */
  KVITTO:
  FOR EACH tt_Hode /*WHERE
      tt_Hode.iAnalyse = 1  Tar med allt salg */
      BREAK
      BY tt_Hode.Act_Date
      BY tt_Hode.Act_time
      BY tt_Hode.Receipt_No:

      ASSIGN
          piLoop  = piLoop + 1
          iAnalyse = 0
          .
      IF piLoop MODULO 100 = 0 THEN
      DO:
        RUN Telleverk IN h_Telleverk 
            ("Fil: " + string(Filer.FilId) + " " + 
             Filer.FilNavn + " Oppretter bong " + STRING(piLoop) +
             " av " + STRING(iAntBonger) + " (FilId: " + 
             STRING(Filer.FilId) + ").")
            NO-ERROR.
      END.

      ASSIGN
          iRadNr   = 0
          rKvittoRadRecid = ?
          .
      KVITTOLINJER:
      FOR EACH tt_Rader WHERE 
          tt_Rader.Act_Date   = tt_Hode.Act_Date   AND
          tt_Rader.Act_Time   = tt_Hode.Act_Time   AND
          tt_Rader.Receipt_no = tt_Hode.Receipt_no
          BREAK
          BY tt_Rader.Act_Date
          BY tt_Rader.Act_Time
          BY tt_Rader.Receipt_no
          BY tt_Rader.iRow:

          /* Konverterer transaksjonskoden */
          KONVTRANSKODE:
          DO:
              ASSIGN
                pcPOS          = tt_Rader.POS
                piEntry        = LOOKUP(pcPOS,cPOSKoder)
                TT_Rader.TTId  = 0
                TT_Rader.TbId  = 1
                .
              IF piEntry = 0 THEN
                  ASSIGN
                  TT_Rader.TTId = 0
                  TT_Rader.TbId = 1
                  .
              ELSE
                  ASSIGN
                    TT_Rader.TTId = int(ENTRY(piEntry,cTTIdKoder))
                    TT_Rader.TbId = 1
                  NO-ERROR.
          END. /* KONVTRANSKODE */

          ASSIGN
              iRadNr = iRadNr + 1
              .
          /* Behandler linjene */
          CASE tt_Rader.POS:
              WHEN "H"   THEN RUN BongHode.
              WHEN "SLG" THEN
                         DO:
                             RUN VareSalg.
                         END.
              WHEN "CAS" THEN
                         DO:
                             RUN Kontant.
                             RUN Veksel.
                         END.
              WHEN "CRE" THEN 
                         DO:
                           RUN Kredit.
                           RUN Veksel.
                         END.
              WHEN "COU" THEN
                         DO:
                             RUN Kupong.
                             RUN Veksel.
                         END.
              WHEN "BAB" THEN
                         DO:
                             RUN Betalingskort.
                             RUN Veksel.
                         END.
              WHEN "CHE" THEN
                         DO:
                             RUN Sjekk.
                             RUN Veksel.
                         END.
              WHEN "REQ" THEN
                         DO:
                             RUN Rekvisisjon.
                             RUN Veksel.
                         END.
              WHEN "CAC" THEN
                         DO:
                             RUN KontantKort.
                             RUN Veksel.
                         END.
              OTHERWISE DO:
                /* Ukjente logid logges som gravenrede feil. */
                RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                               " - Ukjent LogId på Bong/Linje " + tt_Rader.Receipt_No + "/" + STRING(iRadNr + 1) + 
                               ". (" + tt_Rader.cType + ")." + 
                               CHR(1) + "3").
              END.
          END CASE.

          /* Posterer avrundng. */
          IF LAST-OF(tt_Rader.Receipt_no) AND ttKvitto.Avrunding <> 0 THEN
              RUN Avrunding.

          /* "Husker" siste behandlede post. */
          ASSIGN
              rKvittoRadRecid = RECID(tt_Rader)
              .
      END. /* KVITTOLINJER */

      DELETE tt_Hode.
  END. /* KVITTO */

  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Ferdig med opprettelse av kvittering." + 
               CHR(1) + "0").

  RUN Telleverk IN h_Telleverk (" ") NO-ERROR.

  /* Stempler posten som innlest. */
  IF AVAILABLE DataSett THEN
  DO TRANSACTION:
      FIND CURRENT DataSett EXCLUSIVE-LOCK.
      ASSIGN
          DataSett.AntallLinjer = DataSett.AntallLinjer + piAntISett
          DataSett.SettStatus   = (IF DataSett.SettNr > 1
                                    THEN 3 /* Ekstra  */
                                    ELSE 2 /* Mottatt */)
          DataSett.SettStatus   = (IF DataSett.ButikkNr <> 0
                                    THEN DataSett.SettStatus
                                    ELSE 9  /* Ikke koblet */)
          .
      FIND CURRENT Filer EXCLUSIVE-LOCK.
  END.

  /* Flagger ok på innlesning */
  IF CAN-FIND(FIRST ttKvitto) THEN
      ASSIGN
      bOkStatus     = TRUE
      .
  ELSE DO:
      RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                    STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                   " - Filen inneholdt ingen kvitteringer." + 
                   CHR(1) + "0").
      ASSIGN
          bOkStatus = TRUE
          .
  END.
  IF AVAILABLE DataSett THEN
      FIND CURRENT DataSett NO-LOCK.
  IF AVAILABLE Filer THEN
      FIND CURRENT Filer    NO-LOCK.

  RUN Telleverk IN h_Parent ("Kontrollerer at alle datasett er mottatt. Vent litt... ") NO-ERROR.
  RUN sjekkdatasett.p (INPUT lFilId, INPUT cDatoListe).
  RUN Telleverk IN h_Parent (" ") NO-ERROR.

  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Ferdig med innlesning av fil." + 
               CHR(1) + "0").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InnLesFilNyFormat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InnLesFilNyFormat Procedure 
PROCEDURE InnLesFilNyFormat :
DEF VAR piLinjeNr    AS INT   NO-UNDO.
  DEF VAR pcLinje      AS CHAR  NO-UNDO.
  DEF VAR pcOLinje     AS CHAR  NO-UNDO.
  DEF VAR piSettNr     AS INT   NO-UNDO.
  DEF VAR pdDato       AS DATE  NO-UNDO.
  DEF VAR pcDato       AS CHAR  NO-UNDO.
  DEF VAR plDataSettId AS DEC   NO-UNDO.
  DEF VAR pcSokMaske   AS CHAR  NO-UNDO.
  DEF VAR pcOSokMaske  AS CHAR  NO-UNDO.
  DEF VAR pbKoblet     AS LOG   NO-UNDO.
  DEF VAR prRowId      AS ROWID NO-UNDO.
  DEF VAR piAntISett   AS INT   NO-UNDO.
  DEF VAR piLoop       AS INT   NO-UNDO.
  DEF VAR pcTekst      AS CHAR  NO-UNDO.
  DEF VAR piEntry      AS INT   NO-UNDO.
  DEF VAR pcPOS        AS CHAR  NO-UNDO.
  DEF VAR piAr         AS INT   NO-UNDO.
  DEF VAR pc2Dato      AS CHAR  NO-UNDO.
  DEF VAR cFilRad      AS CHAR  NO-UNDO.
  DEF VAR cBongDato    AS CHARACTER  NO-UNDO.
  DEF VAR cBongTid     AS CHARACTER  NO-UNDO.
  ASSIGN
      iantLinjer  = 0
      pcSokMaske  = ""
      pcOSokMaske = ""
      cDatoListe  = ""
      prRowId     = ?
      pcOLinje    = ""
      iAntBonger  = 0
      iKvitto_seq = 0
      bOkStatus   = FALSE
      .

  FIND LAST FilLinjer OF Filer NO-LOCK NO-ERROR.
  IF AVAILABLE FilLinjer THEN
      piLinjeNr = FilLinjer.LinjeNr + 1.
  ELSE
      piLinjeNr = 1.

  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Leser inn i temp-table." + 
               CHR(1) + "0").

  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  /* INPUT STREAM InnFil THROUGH VALUE("./ldparse " + cOFilNavn) NO-ECHO. */
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED cFilRad.
    IF ENTRY(1,cFilRad,";") = "H" THEN
        ASSIGN cBongDato = ENTRY(5,cFilRad,";")
               cBongTid  = ENTRY(7,cFilRad,";").
    CREATE TT_Rader.
    RUN AssignNyFormat (INPUT cFilRad,cBongDato,cBongTid).
    ASSIGN
        iantLinjer  = iantLinjer + 1
        .
    /* Setter transaksjonskoden */
    CASE tt_Rader.cType:
        WHEN "H" THEN
        DO:
            ASSIGN
                tt_Rader.POS = "H"
                .
        END.
        WHEN "L" THEN
        DO:
            ASSIGN
                tt_Rader.POS = "SLG"
                .
        END.
        WHEN "P" THEN
        DO:
            ASSIGN
                tt_Rader.POS =      IF tt_Rader.PayType_Id = "CASH"        THEN "CAS"
                               ELSE IF tt_Rader.PayType_Id = "RIKSCOUPON"  THEN "COU"
                               ELSE IF tt_Rader.PayType_Id = "RIKSKOUPONG" THEN "COU"
                               ELSE IF tt_Rader.PayType_Id = "RIKSKUPONG"  THEN "COU"
                               ELSE IF tt_Rader.PayType_Id = "COUPON"      THEN "COU"
                               ELSE IF tt_Rader.PayType_Id = "BABS"        THEN "BAB"
                               ELSE IF tt_Rader.PayType_Id = "FINAX"       THEN "BAB"
                               ELSE IF tt_Rader.PayType_Id = "CREDCARD"    THEN "CRE"
                               ELSE IF tt_Rader.PayType_Id = "CASHCARD"    THEN "CRE" /* Behandles som kreditt */
                               ELSE IF tt_Rader.PayType_Id = "CHECKS"      THEN "CHE"
                               ELSE IF tt_Rader.PayType_Id = "REQUISIT"    THEN "REQ"
                               ELSE tt_Rader.PayType_Id
                .                                
        END.

    END CASE.

    /* Setter originaldata */
/*     RUN Originaldata. */

    /* Skaper peker til bonger i TEMP-FILE.                                  */
    /* Flagger også om bongen inneholder totaler eller om det kun er en info */
    /* kvittering.                                                           */
    IF can-do("H",tt_Rader.cType) THEN 
    KVITTERING:
    DO:
      IF NOT CAN-FIND(tt_Hode WHERE 
          tt_Hode.Act_Date   = tt_Rader.Act_Date   AND
          tt_Hode.Act_Time   = tt_Rader.Act_Time   AND
          tt_Hode.Receipt_no = tt_Rader.Receipt_no) THEN 
      OPPSTANDELSEN:
      DO:
        CREATE tt_Hode.
        ASSIGN
            tt_Hode.Act_Date   = tt_Rader.Act_Date   
            tt_Hode.Act_Time   = tt_Rader.Act_Time   
            tt_Hode.Receipt_no = tt_Rader.Receipt_no 
            iAntBonger         = iAntBonger + 1
            .
        ASSIGN
            iKvitto_seq         = iKvitto_seq + 1
            tt_Hode.iKvitto_Seq = iKvitto_seq
            .
        /* Oppretter bongHode. */
        RUN doKvittoHode. 
      END. /* OPPSTANDELSEN */
    END. /* KVITTERING */

    /* Flagger SL kort */
    IF CAN-DO("L",tt_Rader.cType) THEN
    DO:
        /*
        IF CAN-FIND(SlKort WHERE
                    SlKort.KortId = tt_Rader.Product_Id) THEN
        */
/*         IF CAN-FIND(FIRST AnalyseArtikkel WHERE                               */
/*                     AnalyseArtikkel.ArtikkelNr = dec(tt_Rader.Product_Id) AND */
/*                     AnalyseArtikkel.Aktiv      = TRUE AND                     */
/*                     AnalyseArtikkel.StartDato <= tt_Rader.Act_Date AND        */
/*                     AnalyseArtikkel.SluttDato >= tt_Rader.Act_Date) THEN      */
/*             ASSIGN                                                            */
/*               tt_Hode.iSlKort = 1.                                            */
    END.

    IF iAntLinjer MODULO 100 = 0 THEN
    DO:
      RUN Telleverk IN h_Telleverk 
          ("Fil: " + string(Filer.FilId) + " " + 
           Filer.FilNavn + " Leser record " + STRING(iAntLinjer) +
           " av " + STRING(iTotAntLinjer) + ".") NO-ERROR.
    END.

  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  /* Peller bort bonger som ikke er flagget. */
  /*
  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Tar bort kvitteringer som ikke er flagget." + 
               CHR(1) + "0").

  FOR EACH tt_Hode WHERE 
      tt_Hode.iAnalyse = 0:
      FIND ttKvitto WHERE
          ttKvitto.Kvitto_Seq = tt_Hode.iKvitto_Seq NO-ERROR.
      IF AVAILABLE ttKvitto THEN
          DELETE ttKvitto.
      DELETE tt_Hode.
  END.
  */

  ASSIGN
      piLoop = 0
      .

  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Konverterer kvittering." + 
               CHR(1) + "0").

  /* Går igjennom alle poster, kvittering for kvittering.       */
  /* MD-Postene er sortert på Act_Date, Act_Time og Receipt_No. */
  KVITTO:
  FOR EACH tt_Hode /*WHERE
      tt_Hode.iAnalyse = 1  Tar med allt salg */
      BREAK
      BY tt_Hode.Act_Date
      BY tt_Hode.Act_time
      BY tt_Hode.Receipt_No:

      ASSIGN
          piLoop  = piLoop + 1
          iAnalyse = 0
          .
      IF piLoop MODULO 100 = 0 THEN
      DO:
        RUN Telleverk IN h_Telleverk 
            ("Fil: " + string(Filer.FilId) + " " +
             Filer.FilNavn + " Oppretter bong " + STRING(piLoop) +
             " av " + STRING(iAntBonger) + " (FilId: " + 
             STRING(Filer.FilId) + ").")
            NO-ERROR.
      END.

      ASSIGN
          iRadNr   = 0
          rKvittoRadRecid = ?
          .
      KVITTOLINJER:
      FOR EACH tt_Rader WHERE 
          tt_Rader.Act_Date   = tt_Hode.Act_Date   AND
          tt_Rader.Act_Time   = tt_Hode.Act_Time   AND
          tt_Rader.Receipt_no = tt_Hode.Receipt_no
          BREAK
          BY tt_Rader.Act_Date
          BY tt_Rader.Act_Time
          BY tt_Rader.Receipt_no
          BY tt_Rader.iRow:

          /* Konverterer transaksjonskoden */
          KONVTRANSKODE:
          DO:
              ASSIGN
                pcPOS          = tt_Rader.POS
                piEntry        = LOOKUP(pcPOS,cPOSKoder)
                TT_Rader.TTId  = 0
                TT_Rader.TbId  = 1
                .
              IF piEntry = 0 THEN
                  ASSIGN
                  TT_Rader.TTId = 0
                  TT_Rader.TbId = 1
                  .
              ELSE
                  ASSIGN
                    TT_Rader.TTId = int(ENTRY(piEntry,cTTIdKoder))
                    TT_Rader.TbId = 1
                  NO-ERROR.
          END. /* KONVTRANSKODE */

          ASSIGN
              iRadNr = iRadNr + 1
              .
          /* Behandler linjene */
          CASE tt_Rader.POS:
              WHEN "H"   THEN RUN BongHode.
              WHEN "SLG" THEN
                         DO:
                             RUN VareSalg.
                         END.
              WHEN "CAS" THEN
                         DO:
                             RUN Kontant.
                             RUN Veksel.
                         END.
              WHEN "CRE" THEN 
                         DO:
                           RUN Kredit.
                           RUN Veksel.
                         END.
              WHEN "COU" THEN
                         DO:
                             RUN Kupong.
                             RUN Veksel.
                         END.
              WHEN "BAB" THEN
                         DO:
                             RUN Betalingskort.
                             RUN Veksel.
                         END.
              WHEN "CHE" THEN
                         DO:
                             RUN Sjekk.
                             RUN Veksel.
                         END.
              WHEN "REQ" THEN
                         DO:
                             RUN Rekvisisjon.
                             RUN Veksel.
                         END.
              WHEN "CAC" THEN
                         DO:
                             RUN KontantKort.
                             RUN Veksel.
                         END.
              OTHERWISE DO:
                /* Ukjente logid logges som gravenrede feil. */
                RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                               " - Ukjent LogId på Bong/Linje " + tt_Rader.Receipt_No + "/" + STRING(iRadNr + 1) + 
                               ". (" + tt_Rader.cType + ")." + 
                               CHR(1) + "3").
              END.
          END CASE.

          /* Posterer avrundng. */
          IF LAST-OF(tt_Rader.Receipt_no) AND ttKvitto.Avrunding <> 0 THEN
              RUN Avrunding.

          /* "Husker" siste behandlede post. */
          ASSIGN
              rKvittoRadRecid = RECID(tt_Rader)
              .
      END. /* KVITTOLINJER */

      DELETE tt_Hode.
  END. /* KVITTO */

  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Ferdig med opprettelse av kvittering." + 
               CHR(1) + "0").

  RUN Telleverk IN h_Telleverk (" ") NO-ERROR.

  /* Stempler posten som innlest. */
  IF AVAILABLE DataSett THEN
  DO TRANSACTION:
      FIND CURRENT DataSett EXCLUSIVE-LOCK.
      ASSIGN
          DataSett.AntallLinjer = DataSett.AntallLinjer + piAntISett
          DataSett.SettStatus   = (IF DataSett.SettNr > 1
                                    THEN 3 /* Ekstra  */
                                    ELSE 2 /* Mottatt */)
          DataSett.SettStatus   = (IF DataSett.ButikkNr <> 0
                                    THEN DataSett.SettStatus
                                    ELSE 9  /* Ikke koblet */)
          .
      FIND CURRENT Filer EXCLUSIVE-LOCK.
  END.

  /* Flagger ok på innlesning */
  IF CAN-FIND(FIRST ttKvitto) THEN
      ASSIGN
      bOkStatus     = TRUE
      .
  ELSE DO:
      RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                    STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                   " - Filen inneholdt ingen kvitteringer." + 
                   CHR(1) + "0").
      ASSIGN
          bOkStatus = TRUE
          .
  END.
  IF AVAILABLE DataSett THEN
      FIND CURRENT DataSett NO-LOCK.
  IF AVAILABLE Filer THEN
      FIND CURRENT Filer    NO-LOCK.

  RUN Telleverk IN h_Parent ("Kontrollerer at alle datasett er mottatt. Vent litt... ") NO-ERROR.
  RUN sjekkdatasett.p (INPUT lFilId, INPUT cDatoListe).
  RUN Telleverk IN h_Parent (" ") NO-ERROR.

  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Ferdig med innlesning av fil." + 
               CHR(1) + "0").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Kontant) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kontant Procedure 
PROCEDURE Kontant :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr     = ttKvitto.ButikkNr
       ttKvittoRad.GruppeNr     = ttKvitto.GruppeNr
       ttKvittoRad.KasseNr      = ttKvitto.KasseNr
       ttKvittoRad.TransDato    = ttKvitto.Dato
       ttKvittoRad.Tid          = tt_Rader.Act_Time
       ttKvittoRad.BongNr       = ttKvitto.BongNr

       ttKvittoRad.LinjeNr      = tt_Rader.iRow
       ttKvittoRad.OriginalData = tt_Rader.OriginalData
       ttKvittoRad.TTId         = tt_Rader.TTId
       ttKvittoRad.TBId         = tt_Rader.TBId
       ttKvittoRad.StrekKode    = ""
       
       ttKvittoRad.TEXT_1       = tt_Rader.PayType_Id /* Card name */
       ttKvittoRad.BongPris     = tt_Rader.Amount_Org
       ttKvittoRad.LinjeSum     = tt_Rader.Amount_Org
       /*ttKvittoRad.kpsxKvant  = DEC(ldPost.amount2) / 100*/
       ttKvittoRad.Antall       = 0
       .
   IF ttKvittoRad.LinjeSum < 0 THEN
       ASSIGN
       bPengerTilbake = TRUE
       .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KontantKort) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontantKort Procedure 
PROCEDURE KontantKort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr     = ttKvitto.ButikkNr
       ttKvittoRad.GruppeNr     = ttKvitto.GruppeNr
       ttKvittoRad.KasseNr      = ttKvitto.KasseNr
       ttKvittoRad.TransDato    = ttKvitto.Dato
       ttKvittoRad.Tid          = tt_Rader.Act_Time
       ttKvittoRad.BongNr       = ttKvitto.BongNr

       ttKvittoRad.LinjeNr      = tt_Rader.iRow
       ttKvittoRad.OriginalData = tt_Rader.OriginalData
       ttKvittoRad.TTId         = tt_Rader.TTId
       ttKvittoRad.TBId         = tt_Rader.TBId
       ttKvittoRad.StrekKode    = ""
       
       ttKvittoRad.TEXT_1       = tt_Rader.PayType_Id /* Card name */
       ttKvittoRad.BongPris     = tt_Rader.Amount_Org
       ttKvittoRad.LinjeSum     = tt_Rader.Amount_Org
       /*ttKvittoRad.kpsxKvant  = DEC(ldPost.amount2) / 100*/
       ttKvittoRad.Antall       = 0
       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Kredit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kredit Procedure 
PROCEDURE Kredit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr     = ttKvitto.ButikkNr
       ttKvittoRad.GruppeNr     = ttKvitto.GruppeNr
       ttKvittoRad.KasseNr      = ttKvitto.KasseNr
       ttKvittoRad.TransDato    = ttKvitto.Dato
       ttKvittoRad.Tid          = tt_Rader.Act_Time
       ttKvittoRad.BongNr       = ttKvitto.BongNr

       ttKvittoRad.LinjeNr      = tt_Rader.iRow
       ttKvittoRad.OriginalData = tt_Rader.OriginalData
       ttKvittoRad.TTId         = tt_Rader.TTId
       ttKvittoRad.TBId         = tt_Rader.TBId
       ttKvittoRad.StrekKode    = ""
       
       ttKvittoRad.TEXT_1       = tt_Rader.PayType_Id /* Card name */
       ttKvittoRad.BongPris     = tt_Rader.Amount_Org
       ttKvittoRad.LinjeSum     = tt_Rader.Amount_Org
       /*ttKvittoRad.kpsxKvant  = DEC(ldPost.amount2) / 100*/
       ttKvittoRad.Antall       = 0
       /* Kvittohode. */
       /*
       ttKvitto.ExpDate         = INT(ldPost.N3)
       ttKvitto.flBetalingskort = IF int(ldPost.N2) <= 3
                                    THEN TRUE
                                    ELSE FALSE
       ttKvitto.flBankKort      = IF int(ldPost.N2) <= 2
                                    THEN TRUE
                                    ELSE FALSE
       
       */
       ttKvitto.flKreditkort    = TRUE 
       .
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Kupong) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kupong Procedure 
PROCEDURE Kupong :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr     = ttKvitto.ButikkNr
       ttKvittoRad.GruppeNr     = ttKvitto.GruppeNr
       ttKvittoRad.KasseNr      = ttKvitto.KasseNr
       ttKvittoRad.TransDato    = ttKvitto.Dato
       ttKvittoRad.Tid          = tt_Rader.Act_Time
       ttKvittoRad.BongNr       = ttKvitto.BongNr

       ttKvittoRad.LinjeNr      = tt_Rader.iRow
       ttKvittoRad.OriginalData = tt_Rader.OriginalData
       ttKvittoRad.TTId         = tt_Rader.TTId
       ttKvittoRad.TBId         = tt_Rader.TBId
       ttKvittoRad.StrekKode    = ""
       
       ttKvittoRad.TEXT_1       = tt_Rader.PayType_Id /* Card name */
       ttKvittoRad.BongPris     = tt_Rader.Amount_Org
       ttKvittoRad.LinjeSum     = tt_Rader.Amount_Org
       ttKvittoRad.Antall       = 0
       .
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LagreBong) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreBong Procedure 
PROCEDURE LagreBong :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR prRowId      AS ROWID NO-UNDO.
DEF VAR plDataSettId AS DEC   NO-UNDO.
DEF VAR piSettNr     AS INT   NO-UNDO.
DEF VAR piAntBonger  AS INT   NO-UNDO.
DEF VAR piLoop       AS INT   NO-UNDO.
DEF VAR d31DecFgAr   AS DATE NO-UNDO.

ASSIGN
    iAntISett   = 0
    prRowId     = ?
    piAntBonger = 0
    bOkStatus   = FALSE
    .

RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
              STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
             " - Starter å lagre bongene i databasen." + 
             CHR(1) + "0").

/* Leser alle bonger i temp-file og lagrer dem i databasen. */
LAGRE-KVITTO:
FOR EACH ttKvitto 
    BREAK BY ButikkNr 
          BY GruppeNr 
          BY KasseNr 
          BY Dato 
          BY BongNr TRANSACTION:

    ASSIGN
        cError    = ""
        cFilError = "" /* Denne skal egentlig lagres i loggen. */
        .

    /* Oppretter datasett og BongHode. */
    IF FIRST-OF(ttKvitto.Dato) THEN
    DATASETT:
    DO:
      /* Ferdigstempler den vi hold på med. */
      IF prRowId <> ? THEN
      DO:
          FIND DataSett EXCLUSIVE-LOCK WHERE
              ROWID(Datasett) = prRowid.
          ASSIGN
            DataSett.AntallLinjer = DataSett.AntallLinjer + iAntISett
            DataSett.SettStatus   = (IF DataSett.SettNr > 1
                                      THEN 3 /* Ekstra  */
                                      ELSE 2 /* Mottatt */)
            DataSett.SettStatus   = (IF DataSett.ButikkNr <> 0
                                      THEN DataSett.SettStatus
                                      ELSE 9  /* Ikke koblet */)
            DataSett.Behandlet    = 3 /* Behandlet */
            iAntISett             = 0
            prRowId               = ?
            bOkStatus             = TRUE
            .
          /* Åpningsskjemahantering */
          IF cKontrolltabell = "1" THEN DO:
              FIND ApnSkjema WHERE ApnSkjema.ButikkNr = DataSett.ButikkNr AND
                                   ApnSkjema.Ar       = YEAR(DataSett.Dato) NO-ERROR.  
              IF AVAIL ApnSkjema THEN DO:
                  ASSIGN d31DecFgAr = DATE(12,31,ApnSkjema.Ar - 1)
                         ENTRY(DataSett.Dato - d31DecFgAr,ApnSkjema.OpenClosed) = "4" NO-ERROR.
              END.
          END.
          /* Åpnings.... SLUTT      */

          RELEASE DataSett.
      END.

      /* Finner neste DataSettId */
      FIND LAST DataSett NO-LOCK
          USE-INDEX DataSettId NO-ERROR.
      IF AVAILABLE DataSett THEN 
          plDataSettId = DataSett.DataSettId + 1.
      ELSE
          plDataSettId = 1.

      /* Finner neste SettNr */
      FIND LAST Datasett NO-LOCK WHERE
          Datasett.ButikkNr = ttKvitto.ButikkNr AND
          Datasett.GruppeNr = ttKvitto.GruppeNr AND
          Datasett.KasseNr  = ttKvitto.KasseNr  AND
          Datasett.Dato     = ttKvitto.Dato     AND
          DataSett.FilType  = Filer.Filtype /* Kvittering */
          USE-INDEX DataSett NO-ERROR.
      IF AVAILABLE DataSett THEN
          piSettNr = DataSett.SettNr + 1.
      ELSE 
          piSettNr = 1.

      /* Alle kvitteringer som kommer inn på samme dag skal kobles  */
      /* til det samme datasettet. Forutsetning er at settet ikke   */
      /* har behandlingsstatus > 1.                                 */
      IF AVAILABLE DataSett THEN
      DO:
        IF DataSett.SettNr <= 2 AND DataSett.SettStatus <= 2 THEN
            ASSIGN
              plDataSettId = DataSett.DataSettId
              piSettNr     = DataSett.SettNr
              .
        ELSE RELEASE DataSett. /* Ny post skal skapes. */
      END.

      IF NOT AVAILABLE DataSett THEN
      EVIG-LOOP:
      DO WHILE TRUE:
        CREATE DataSett.
        ASSIGN
            /*DataSett.DataSettId = plDataSettId - settes av trigger */
            DataSett.SettStatus = 8 /* Innlesning avbrutt */
            plDataSettId        = DataSett.DataSettId
            NO-ERROR.
        /* Ekstra sjekk på om datasett kan skapes. */
        IF ERROR-STATUS:ERROR THEN
        DO:
            /* Finner neste DataSettId */
            FIND LAST DataSett NO-LOCK
                USE-INDEX DataSettId NO-ERROR.
            IF AVAILABLE DataSett THEN 
                ASSIGN
                plDataSettId = DataSett.DataSettId + 1
                piSettNr     = 1
                .
            ELSE
                ASSIGN
                    plDataSEttId = 1
                    piSettNr     = 1.
            NEXT EVIG-LOOP.
        END.

        IF NOT CAN-DO(cDatoListe,STRING(ttKvitto.Dato)) THEN
            ASSIGN
              cDatoListe = cDatoListe + 
                           (IF cDatoListe = ""
                              THEN ""
                              ELSE ",") +
                            STRING(ttKvitto.Dato)
                            .
        /* Forlater evigheten */
        LEAVE EVIG-LOOP.
      END. /* EVIG-LOOP */
      ELSE  /* Bruker det vi fant. */
          FIND CURRENT DataSett EXCLUSIVE-LOCK.

      ASSIGN
        prRowId             = ROWID(DataSett)
        DataSett.ButikkNr   = ttKvitto.ButikkNr 
        DataSett.GruppeNr   = ttKvitto.GruppeNr
        DataSett.KasseNr    = ttKvitto.KasseNr
        DataSett.Dato       = ttKvitto.Dato
        DataSett.SettNr     = piSettNr
        DataSett.Tid        = ttKvitto.tid
        DataSett.FilId      = lFilId
        DataSett.FilType    = Filer.FilType /* Kvittering */
        .
    END. /* DATASETT */

    /* Oppretter bongHode. */
    IF NOT CAN-FIND(BongHode WHERE
                    BongHode.ButikkNr = ttKvitto.ButikkNr AND
                    BongHode.GruppeNr = ttKvitto.GruppeNr AND
                    BongHode.KasseNr  = ttKvitto.KasseNr  AND
                    BongHode.Dato     = ttKvitto.Dato     AND
                    BongHode.BongNr   = ttKvitto.BongNr) THEN
    OPPRETTBONG:
    DO:
        CREATE BongHode.
        BUFFER-COPY ttKvitto 
             EXCEPT Utskriftskopi 
                 TO BongHode
             ASSIGN BongHode.DataSettId = DataSett.DataSettId
                    BongHode.BongStatus = 5
             .
    END. /* OPPRETTBONG */
    ELSE 
        RELEASE BongHode.
    /* Legger opp radene */
    IF AVAILABLE BongHode THEN
    DO:
/*         RUN ValiderKasserer. */
        ASSIGN
            piLoop = 1
            .
        KVITTORAD:
        FOR EACH ttKvittoRad where
            ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq:
            CREATE BongLinje.
            BUFFER-COPY ttKvittoRad 
                     TO BongLinje
                 ASSIGN 
                     BongLinje.B_Id      = BongHode.B_Id
                     BongLinje.LinjeNr   = piLoop
                     BongLinje.GruppeNr  = BongHode.GruppeNr
                     BongLinje.Dato      = BongHode.Dato
                     BongLinje.TransDato = BongHode.Dato
                     BongLinje.TransTid  = BongHode.Tid
                     BongLinje.BongTekst = ttKvittoRad.Text_1
                     BongLinje.TTId      = ttKvittoRad.TTId
                     BongLinje.TBId      = ttKvittoRad.TBId
                     BongLinje.MvaGr     = ttKvittoRad.MvaKode                     
                     BongLinje.VVareKost = ttKvittoRad.BongPris
                     .
            ASSIGN
                piLoop = piLoop + 1
                .
            /* Hent varelinjeinfo */
            IF CAN-DO("001,002,003,004,005,006,007,008,009,010,011,012",STRING(BongLinje.TTId,"999")) THEN
            DO:
                RUN ValiderArtikkel.
                RUN HentVareLinjeInfo.
                RUN SettVareKost.
            END.
        END. /* KVITTORAD */

        /* Saldokontroll */
        RUN SjekkSaldo (INPUT BongHode.B_Id).
        /* KortValidering */
/*         IF BongHode.MedlemsKort <> "" then          */
/*             RUN ValiderKort (BongHode.MedlemsKort). */
/*         IF BongHode.KundeKort <> "" THEN            */
/*             RUN ValiderKort (BongHode.KundeKort).   */
        /* Legger på feilkoder */
        IF cError <> "" THEN
            ASSIGN
            BongHode.Logg = BongHode.Logg + 
                            (IF BongHode.Logg <> ""
                               THEN CHR(10)
                               ELSE "") + 
                            cError
            .
        /* Logger prisavvik */
        IF bLoggPrisAvvik THEN
            RUN PrisAvviksLogg.p (BongHode.ButikkNr,BongHode.b_id,1).
    END.

    /* Teller antall bonger. */
    ASSIGN
        iAntISett   = iAntISett   + 1
        piantBonger = piAntBonger + 1
        .
    IF iAntISett MODULO 50 = 0 OR 
       LAST(ttKvitto.ButikkNr) THEN
    DO:
      RUN Telleverk IN h_Telleverk 
          ("Fil: " + string(Filer.FilId) + " " + 
           Filer.FilNavn + " Lagrer kvittering " + STRING(piAntBonger) +
           " av " + STRING(iAntBonger) + " (FilId: " + 
           STRING(Filer.FilId) + 
           ").") NO-ERROR.
    END.

END. /* LAGRE-KVITTO */

RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
              STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
             " - Ferdig med å lagre kvitteringene." + 
             CHR(1) + "0").

/* Ferdigstempler den vi hold på med.        */
/* Ekstra her for å ta det siste datasettet. */
IF prRowId <> ? THEN
DO TRANSACTION:
    FIND DataSett EXCLUSIVE-LOCK WHERE
        ROWID(Datasett) = prRowid.
    ASSIGN
      DataSett.AntallLinjer = DataSett.AntallLinjer + iAntISett
      DataSett.SettStatus   = (IF DataSett.SettNr > 1
                                THEN 3 /* Ekstra  */
                                ELSE 2 /* Mottatt */)
      DataSett.SettStatus   = (IF DataSett.ButikkNr <> 0
                                THEN DataSett.SettStatus
                                ELSE 9  /* Ikke koblet */)
      DataSett.Behandlet    = 3 /* Behandlet */
      iAntISett             = 0
      prRowId               = ?
      bOkStatus             = TRUE
      .
    /* Åpningsskjemahantering */
    IF cKontrolltabell = "1" THEN DO:
        FIND ApnSkjema WHERE ApnSkjema.ButikkNr = DataSett.ButikkNr AND
                             ApnSkjema.Ar       = YEAR(DataSett.Dato) NO-ERROR.  
        IF AVAIL ApnSkjema THEN DO:
            ASSIGN d31DecFgAr = DATE(12,31,ApnSkjema.Ar - 1)
                   ENTRY(DataSett.Dato - d31DecFgAr,ApnSkjema.OpenClosed) = "4" NO-ERROR.
        END.
    END.
    /* Åpnings.... SLUTT      */
    RELEASE DataSett.
END. /* TRANSACTION */
  
RUN Telleverk IN h_Parent (" ") NO-ERROR.

ASSIGN
    iAntISett = piAntBonger
    .

RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
              STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
             " - Lagringsrutine ferdig." + 
             CHR(1) + "0").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OriginalData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OriginalData Procedure 
PROCEDURE OriginalData PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     Lagrer originaldata på bonglinjen.
  Parameters:  <none>
  Notes:       Det er IKKE plass til alle data på linjen. Endel felter på slutten tas ikke med.
------------------------------------------------------------------------------*/

    ASSIGN
        TT_Rader.OrigiNalData = 
        string(TT_Rader.cType) + CHR(1) +
        string(TT_Rader.iRow) + CHR(1) +           
        string(TT_Rader.RECEIPT_NO) + CHR(1) +     
        string(TT_Rader.ACT_NO) + CHR(1) +         
        (IF TT_Rader.ACT_DATE <> ?
           THEN string(TT_Rader.ACT_DATE)
           ELSE "?") + CHR(1) +       
        (IF TT_Rader.LogicDate <> ?
           THEN string(TT_Rader.LogicDate)
           ELSE "?") + CHR(1) +      
        string(TT_Rader.ACT_TIME) + CHR(1) +       
        string(TT_Rader.OUR_ID) + CHR(1) +         
        string(TT_Rader.OUR_NO) + CHR(1) +         
        string(TT_Rader.INDIVID_NO) + CHR(1) +     
        string(TT_Rader.INDIVID_ID) + CHR(1) +     
        string(TT_Rader.NAME) + CHR(1) +           
        string(TT_Rader.SALESP_ID) + CHR(1) +      
        string(TT_Rader.USER_ID) + CHR(1) +        
        string(TT_Rader.EME_NO) + CHR(1) +         
        string(TT_Rader.ACCOUNT_2) + CHR(1) +      
        string(TT_Rader.PROJECT_ID) + CHR(1) +     
        string(TT_Rader.NOTES) + CHR(1) +          
        (IF TT_Rader.INSERTED <> ?
           THEN string(TT_Rader.INSERTED)
           ELSE "?") + CHR(1) +       
        (IF TT_Rader.UPDATED <> ?
           THEN string(TT_Rader.UPDATED)
           ELSE "?") + CHR(1) +        
        string(TT_Rader.I_USER_NO) + CHR(1) +      
        string(TT_Rader.USER_NO) + CHR(1) +        
        string(TT_Rader.CAMP_NO) + CHR(1) +        
        string(TT_Rader.CMP_ACT_NO) + CHR(1) +     
        string(TT_Rader.IS_INTERN) + CHR(1) +      
        string(TT_Rader.IS_DONE) + CHR(1) +        
        string(TT_Rader.STOCKVALUE) + CHR(1) +     
        string(TT_Rader.FREIGHT) + CHR(1) +        
        string(TT_Rader.INV_FEE)
        .
    ASSIGN
        TT_Rader.OrigiNalData = TT_Rader.OrigiNalData + CHR(1) + 
        string(TT_Rader.VAT) + CHR(1) +            
        string(TT_Rader.ROWSUM) + CHR(1) +         
        string(TT_Rader.DISCOUNT) + CHR(1) +       
        string(TT_Rader.DISCOUNT_P) + CHR(1) +     
        string(TT_Rader.ROUND_OFF) + CHR(1) +      
        string(TT_Rader.TOTAL) + CHR(1) +          
        string(TT_Rader.CURR_RATE) + CHR(1) +      
        string(TT_Rader.CURRENCYID) + CHR(1) +     
        string(TT_Rader.IS_APPROVE) + CHR(1) +     
        string(TT_Rader.IS_SENT) + CHR(1) +        
        string(TT_Rader.BUYER_TYPE) + CHR(1) +     
        string(TT_Rader.BALANCE_NO) + CHR(1) +     
        string(TT_Rader.TillType) + CHR(1) +       
        string(TT_Rader.TillUnitId) + CHR(1) +     
        string(TT_Rader.S_CHANGE) + CHR(1) +       
        string(TT_Rader.PRINT_SAP) + CHR(1) +      
        string(TT_Rader.ORIGIN_NO) + CHR(1) +      
        string(TT_Rader.OFFLINE_NO) + CHR(1) +     
        string(TT_Rader.CUSTGRP_ID) + CHR(1) +     
        string(TT_Rader.SHOP_ID) + CHR(1) +        
        string(TT_Rader.CASHREG_NO) + CHR(1) +     
        string(TT_Rader.SIGNATURE) + CHR(1) +      
        string(TT_Rader.SUPP_PRICE) + CHR(1) +     
        string(TT_Rader.CARTYPEID) + CHR(1) +      
        string(TT_Rader.CREDCARDID) + CHR(1) +     
        string(TT_Rader.LEGETIMAT) + CHR(1) +      
        string(TT_Rader.THEIR_ID) + CHR(1) +       
        string(TT_Rader.EME_NAME) + CHR(1) +       
        string(TT_Rader.IS_STAT) + CHR(1) +        
        string(TT_Rader.IS_REPORT) + CHR(1) +      
        string(TT_Rader.IS_STAFF) + CHR(1) +       
        string(TT_Rader.IS_LOCSOLD) + CHR(1) +     
        string(TT_Rader.PRODUCT_NO) + CHR(1) /* +     
        string(TT_Rader.UNIT_ID) + CHR(1) +        
        string(TT_Rader.PriceTypeId) + CHR(1) +    
        string(TT_Rader.QUANTITY) + CHR(1) +       
        string(TT_Rader.STK_CONVF) + CHR(1) +      
        string(TT_Rader.L_SUPP_PRICE) + CHR(1) +   
        string(TT_Rader.UNIT_PRICE) + CHR(1) +     
        string(TT_Rader.AMOUNT) + CHR(1) +         
        string(TT_Rader.L_DISCOUNT_P) + CHR(1) +   
        string(TT_Rader.L_DISCOUNT) + CHR(1) +     
        string(TT_Rader.VAT_P) + CHR(1) +          
        string(TT_Rader.L_VAT) + CHR(1) +          
        string(TT_Rader.CONTRACTNO) + CHR(1) +     
        string(TT_Rader.GROUP_NO) + CHR(1) +       
        string(TT_Rader.ACCOUNT_4) + CHR(1) +      
        string(TT_Rader.ACCOUNT_5) + CHR(1) +      
        string(TT_Rader.IS_MAIN_ROW) + CHR(1) +    
        string(TT_Rader.IS_TEXT) + CHR(1) +        
        string(TT_Rader.IS_MANDISC) + CHR(1) +     
        string(TT_Rader.IS_DISC_P) + CHR(1) +      
        string(TT_Rader.IS_N_PRICE) + CHR(1) +     
        string(TT_Rader.PRODUCT_ID) + CHR(1) +     
        string(TT_Rader.DESCRIPT) + CHR(1) +       
        string(TT_Rader.PRODGR_ID) + CHR(1) +      
        string(TT_Rader.ORIGIN_ID) + CHR(1) +      
        string(TT_Rader.PROD_CLASS) + CHR(1) +     
        string(TT_Rader.DISC_CUST) + CHR(1) +      
        string(TT_Rader.DISC_EMPL) + CHR(1) +      
        string(TT_Rader.DISC_ACT) + CHR(1) +       
        string(TT_Rader.CRED_QTY) + CHR(1) +       
        string(TT_Rader.CUSTORD_NO) + CHR(1) +     
        string(TT_Rader.CORDROW_NO) + CHR(1) +     
        string(TT_Rader.REPORT_CODE) + CHR(1) +    
        string(TT_Rader.STAT_CODE) + CHR(1) +      
        string(TT_Rader.IS_PRESOLD) + CHR(1) +     
        string(TT_Rader.L_OUR_NO) + CHR(1) +       
        string(TT_Rader.UnitPriceWoAct) + CHR(1) + 
        string(TT_Rader.DiscTypeId) + CHR(1) +     
        string(TT_Rader.PAYTYPE_ID) + CHR(1) +     
        string(TT_Rader.PAYMENT_ID) + CHR(1) +     
        string(TT_Rader.RATE_OUT) + CHR(1) +       
        string(TT_Rader.P_AMOUNT) + CHR(1) +       
        string(TT_Rader.AMOUNT_ORG) + CHR(1) +     
        string(TT_Rader.PAIDAMOUNT) + CHR(1) +     
        string(TT_Rader.AMOUNT_RET)     
        */
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Rekvisisjon) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rekvisisjon Procedure 
PROCEDURE Rekvisisjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr     = ttKvitto.ButikkNr
       ttKvittoRad.GruppeNr     = ttKvitto.GruppeNr
       ttKvittoRad.KasseNr      = ttKvitto.KasseNr
       ttKvittoRad.TransDato    = ttKvitto.Dato
       ttKvittoRad.Tid          = tt_Rader.Act_Time
       ttKvittoRad.BongNr       = ttKvitto.BongNr

       ttKvittoRad.LinjeNr      = tt_Rader.iRow
       ttKvittoRad.OriginalData = tt_Rader.OriginalData
       ttKvittoRad.TTId         = tt_Rader.TTId
       ttKvittoRad.TBId         = tt_Rader.TBId
       ttKvittoRad.StrekKode    = ""
       
       ttKvittoRad.TEXT_1       = tt_Rader.PayType_Id 
       ttKvittoRad.BongPris     = tt_Rader.Amount_Org
       ttKvittoRad.LinjeSum     = tt_Rader.Amount_Org
       /*ttKvittoRad.kpsxKvant  = DEC(ldPost.amount2) / 100*/
       ttKvittoRad.Antall       = 0
       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SettVareKost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettVareKost Procedure 
PROCEDURE SettVareKost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:   Varekosten kommer fra kassen i feltet SUPP_PRICE.    
------------------------------------------------------------------------------*/
  DEF VAR pdVVAreKost AS DEC NO-UNDO.
      
/*   IF BongLinje.ArtikkelNr = "" THEN */
/*   DO:                               */
/*       ASSIGN                        */
/*           pdVVAreKost = 0           */
/*           .                         */
/*       RETURN.                       */
/*   END.                              */
  /* Henter varekost i butikken det overføres fra. */      
  /* Dette er pris eExMva.                         */
  IF BongLinje.ArtikkelNr <> "" THEN
      FIND Lager NO-LOCK WHERE
          Lager.ArtikkelNr = dec(BongLinje.ArtikkelNr) AND
          Lager.Butik      = BongLinje.Butik NO-ERROR.

  /* Renser bort dritt. */
  IF bongLinje.VVAreKost = ? THEN
      BongLinje.VVareKost = 0.

  /* Initierer varekost som kommer fra kassen */
  ASSIGN
      pdVVareKost = BongLinje.VVareKost
      .

  IF BongLinje.VVAreKost = 0 THEN
  DO:
      IF AVAILABLE Lager THEN
          pdVVarekost = Lager.VVareKost.
      ELSE 
          pdVVareKost = 0.
  END.

  /* Sjekker om varekost er satt.                                       */
  /* Er det ikke satt noen varekost, merkes transaksjonen med feilkode. */
  if pdVVareKost = 0 then /* or wBrutto% *** Skal også utføres for brutto% artikkler */
    DO:
      ASSIGN
          /* Omsetning eks. mva */
          pdVVareKost = (IF BongLinje.Antall >= 0
                          THEN (BongLinje.LinjeSum - 
                                (BongLinje.LinjeRab + BongLinje.SubtotalRab) - 
                                BongLinje.MvaKr)
                          ELSE (BongLinje.LinjeSum - 
                                (BongLinje.LinjeRab + BongLinje.SubtotalRab) - 
                                BongLinje.MvaKr) * -1)
          pdVVareKost = pdVVareKost / ABS(BongLinje.Antall)
          .
      /* Hvis ukjent artikkel, benyttes varegruppens kostnadsprosent. */
      /* Varekost settes inn pr. enhet.                               */
      IF NOT CAN-FIND(ArtBas WHERE
                      ArtBas.ArtikkelNr = DEC(BongLinje.ArtikkelNr)) THEN
      DO:
          FIND VarGr NO-LOCK WHERE
              VarGr.Vg = BongLinje.VareGr NO-ERROR.
          IF AVAILABLE VarGr THEN
          DO:
              IF VarGr.Kost_Proc > 0 THEN
                  ASSIGN
                  pdVVareKost = abs(ROUND((pdVVareKost * VarGr.Kost_Proc) / 100,2))
                  .
          END.
      END.
      ELSE if VALID-HANDLE(h_PrisKo) then
          RUN HentVareKost in h_PrisKo (INPUT  BongLinje.ArtikkelNr, 
                                        input  BongLinje.Butik, 
                                        INPUT  pdVVareKost, 
                                        output pdVVareKost).

    END.
  IF pdVVareKost = ? THEN
      pdVVareKost = 0.
  
  assign
      BongLinje.VVareKost = pdVVareKost
      . 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Sjekk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Sjekk Procedure 
PROCEDURE Sjekk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr     = ttKvitto.ButikkNr
       ttKvittoRad.GruppeNr     = ttKvitto.GruppeNr
       ttKvittoRad.KasseNr      = ttKvitto.KasseNr
       ttKvittoRad.TransDato    = ttKvitto.Dato
       ttKvittoRad.Tid          = tt_Rader.Act_Time
       ttKvittoRad.BongNr       = ttKvitto.BongNr

       ttKvittoRad.LinjeNr      = tt_Rader.iRow
       ttKvittoRad.OriginalData = tt_Rader.OriginalData
       ttKvittoRad.TTId         = tt_Rader.TTId
       ttKvittoRad.TBId         = tt_Rader.TBId
       ttKvittoRad.StrekKode    = ""
       
       ttKvittoRad.TEXT_1       = tt_Rader.PayType_Id 
       ttKvittoRad.BongPris     = tt_Rader.Amount_Org
       ttKvittoRad.LinjeSum     = tt_Rader.Amount_Org
       /*ttKvittoRad.kpsxKvant  = DEC(ldPost.amount2) / 100*/
       ttKvittoRad.Antall       = 0
       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SjekkSaldo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkSaldo Procedure 
PROCEDURE SjekkSaldo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plB_Id AS DEC NO-UNDO.

  DEF VAR plVareSum     AS DEC   NO-UNDO.
  DEF VAR plBetSum      AS DEC   NO-UNDO.

  DEF BUFFER bBongHode  FOR BongHode.
  DEF BUFFER bBongLinje FOR BongLinje.

  DO TRANSACTION:
      FIND bBongHode EXCLUSIVE-LOCK WHERE
          bBongHode.B_Id = plB_Id NO-ERROR.
      IF NOT AVAILABLE bBongHode THEN
          RETURN.

      /* Sjekker at bongens sum er = 0. */
      ASSIGN plVareSum = 0
             plBetSum  = 0.
      /* NB: Overføringer kontrolleres ikke. */
      FOR EACH bBongLinje NO-LOCK WHERE
          bBongLinje.B_Id = plB_Id:
        /* Sum varetranser (NB: 04 skal ikke telles med her.) */
        IF CAN-DO("001,003,010,012",string(bBongLinje.TTId,"999")) THEN
        DO:
            ASSIGN
              plVareSum = plVareSum + 
                          ((bBongLinje.LinjeSum - 
                            bBongLinje.LinjeRab -
                            bBongLinje.SubTotalRab) * (IF bBongLinje.Antall < 0
                                                                          THEN -1 
                                                                          ELSE 1))
              .
        END.

        /* Betalingstransaksjoner.                                */
        /* Subtotalrabatt er trukket fra fra før og skal ikke tas */
        /* med her.                                               */
        ELSE IF CAN-DO("050,051,052,054,055,056,058,065,066,070,071,072,073",string(bBongLinje.TTId,"999")) THEN
        DO:
            /* Varelinjer + deponering */
            IF CAN-DO("050,051,052,054,055,056,058,065,066,070,071,072",string(bBongLinje.TTId,"999")) THEN
            ASSIGN
              plBetSum = plBetSum + bBongLinje.LinjeSum
              .
            /* Betaling av deponering og veksel */
            ELSE IF CAN-DO("073",string(bBongLinje.TTId,"999")) THEN
            ASSIGN
              plBetSum = plBetSum - bBongLinje.LinjeSum
              .

        END.
      END.
      /* Logger hvis bongsummen ikke er lik 0. */
      IF (plVareSum - plBetSum <> 0) /*AND
         bLoggSaldoFeil = TRUE */ THEN
      DO:
          /* Vi ignorerer øresavrunding - dvs beløp mindre enn 0.49 øre. */
          IF ABSOLUTE(plVareSum - plBetSum) > 0.49 AND NUM-ENTRIES(cFilError,"|") < 15 THEN
          ASSIGN
          cFilError = cFilError + 
              (IF cFilError = ""
                 THEN ""
                 ELSE "|") + 
              " - Bongen's sum <> 0" + 
              " Diff: " + STRING(plVareSum,"->>>,>>>,>>9.99") + " - " + 
              STRING(plBetSum,"->>>,>>>,>>9.99") + " = " + 
              STRING(plVareSum - plBetSum,"->>>,>>>,>>9.99") + "." + 
              " (But/Kas/Dato/BongNr: " + 
              STRING(bBongHode.ButikkNr) + "/" + 
              STRING(bBongHode.KasseNr) + "/" + 
              STRING(bBongHode.Dato) + "/" + 
              STRING(bBongHode.BongNr) +  ")." + CHR(1) + "3"
          .
      END.

      /* Oppdaterer bonghode med beløp. */
      ASSIGN bBongHode.Belop = plVareSum.
  END. /* TRANSACTION */
  FIND CURRENT bBongHode NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TellOppLinjer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellOppLinjer Procedure 
PROCEDURE TellOppLinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      iTotAntLinjer = 0
      bOkStatus     = FALSE 
      .
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  repeat:
    IMPORT STREAM InnFil UNFORMATTED cLinje.
    ASSIGN
        iTotAntLinjer = iTotAntLinjer + 1
        .
    IF bOkStatus = FALSE THEN
        bOkStatus = TRUE.
  END.
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValiderArtikkel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderArtikkel Procedure 
PROCEDURE ValiderArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR plDec        AS DEC  NO-UNDO.
  DEF VAR cNyStrekKode AS CHAR NO-UNDO.
  
  ASSIGN
      plDec = DEC(BongLinje.Artikkelnr)
      NO-ERROR.

  IF NOT ERROR-STATUS:ERROR AND plDec > 0 THEN DO:
      /* Vi hämtar Impkonv för artikeln */
      FIND FIRST ImpKonv WHERE ImpKonv.EDB-System = "MegaDisk"    AND
                               ImpKonv.Tabell     = "MD711Art"       AND
                               ImpKonv.EksterntID = STRING(plDec) NO-LOCK NO-ERROR.
      IF AVAIL ImpKonv AND CAN-FIND(StrekKode WHERE StrekKode.Kode = ImpKonv.InterntID) THEN DO:
          ASSIGN cNyStrekkode = ImpKonv.InterntID.
      END.
  END.
  IF cNyStrekKode = "" THEN DO:
      FIND FIRST ImpKonv WHERE ImpKonv.EDB-System = "MegaDisk"    AND
                               ImpKonv.Tabell     = "VG711Slask"       AND
                               ImpKonv.EksterntID = STRING(BongLinje.VareGr) NO-LOCK NO-ERROR.
      IF AVAIL ImpKonv AND CAN-FIND(StrekKode WHERE StrekKode.Kode = ImpKonv.InterntID) THEN DO:
          ASSIGN cNyStrekkode = ImpKonv.InterntID.
      END.
  END.
  IF cNyStrekkode <> "" THEN DO:
      IF BongLinje.Strekkode <> cNyStrekKode THEN
          ASSIGN BongLinje.ForKonvertering = BongLinje.Strekkode
                 BongLinje.Strekkode       = cNyStrekKode.
  END.
  ELSE
      ASSIGN BongLinje.Strekkode = LEFT-TRIM(BongLinje.Strekkode,"0").
  RELEASE StrekKode.
  RELEASE ArtBas.
  IF BongLinje.Strekkode <> "" THEN
      FIND Strekkode NO-LOCK WHERE
          Strekkode.Kode = BongLinje.Strekkode NO-ERROR.
  IF AVAIL StrekKode THEN
      FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.

  /* Blank strekkode eller ukjent strekkode */
  IF AVAIL ArtBas THEN DO:
      FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
      FIND HuvGr OF ArtBas NO-LOCK NO-ERROR.
      ASSIGN BongLinje.ForKonvertering = (IF BongLinje.ForKonvertering = "" THEN BongLinje.StrekKode ELSE BongLinje.ForKonvertering) + CHR(1) +
             BongLinje.ArtikkelNr + CHR(1) + STRING(BongLinje.VareGr) + CHR(1) + STRING(BongLinje.HovedGr)
             BongLinje.ArtikkelNr     = TRIM(STRING(ArtBas.ArtikkelNr,">>>>>>>>>>>>9"))
             BongLinje.VareGr         = ArtBas.Vg
             BongLinje.LopeNr         = ArtBas.LopNr
             BongLinje.VareGruppeNavn = IF AVAIL VarGr THEN VarGr.VgBeskr ELSE BongLinje.VareGruppeNavn
             BongLinje.HovedGr        = ArtBas.Hg
             BongLinje.HovedGrBeskrivelse = IF AVAIL HuvGr THEN HuvGr.HgBeskr ELSE BongLinje.HovedGrBeskrivelse.
  END.
  ELSE DO:
      ASSIGN
          BongLinje.ForKonvertering = (IF BongLinje.ForKonvertering = "" THEN BongLinje.StrekKode ELSE BongLinje.ForKonvertering) + CHR(1) +
          BongLinje.ArtikkelNr
          BongLinje.StrekKode = "9999999999999"
          BongLinje.ArtikkelNr = ""
          BongLinje.LopeNr     = 0
          .
      RETURN.
  END. /* STREK-PLU */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValiderArtikkelORG) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderArtikkelORG Procedure 
PROCEDURE ValiderArtikkelORG :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR plDec AS DEC NO-UNDO.

  /* Det forekommer at her ligger alfanumeriske tegn */
  ASSIGN
      plDec = DEC(BongLinje.Strekkode)
      NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN 
  DO:
      /* Konverterer PLU koder. */
      IF DEC(BongLinje.Strekkode) <= 99999 THEN
          BongLinje.Strekkode = LEFT-TRIM(BongLinje.Strekkode,"0").

      FIND Strekkode NO-LOCK WHERE
          Strekkode.Kode = BongLinje.Strekkode NO-ERROR.
  END.

  /* Blank strekkode eller ukjent strekkode */
  IF BongLinje.Strekkode = "" OR NOT AVAILABLE Strekkode OR BongLinje.ArtikkelNr = "" THEN
  STREK-PLU:
  DO:
      /* Pressbyrån spesial for innlesning av MD data. */
      IF BongLinje.ArtikkelNr = "" THEN
      PLU-SJEKK:
      DO:
          /* Henter varegruppens PLU nummer */
          FIND FIRST ArtBas NO-LOCK where
                     ArtBas.Vg    = BongLinje.VareGr and
                     ArtBas.OPris = TRUE USE-INDEX PLU NO-ERROR.
          /* Henter diverse varegruppe */
          IF NOT AVAILABLE ArtBas THEN
              FIND FIRST ArtBas NO-LOCK where
                         ArtBas.Vg    = 1499 and
                         ArtBas.OPris = TRUE USE-INDEX PLU NO-ERROR.
          IF AVAILABLE ArtBas THEN
          DO:
              ASSIGN
                  BongLinje.ArtikkelNr = trim(STRING(ArtBas.ArtikkelNr))
/*                   cArtikkelNr          = trim(STRING(ArtBas.ArtikkelNr)) */
                  cError = cError + 
                           (IF cError = ""
                              THEN ""
                              ELSE chr(10)) + 
                           "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                           " Blank/ukjent artikkel strekkode/PLU nummer. Salget er postert på varegruppe " +
                           STRING(BongLinje.VareGr) +
                           "."
                  cFilError = cFilError + 
                           (IF cFilError = ""
                              THEN ""
                              ELSE "|") + 
                           " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                           "  Blank/ukjent artikkel strekkode/PLU nummer. Salget er postert på varegruppe . " +
                           "." + CHR(1) + "2"
                  BongHode.Gradering = 0
                  BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
                  .
              LEAVE STREK-PLU.
          END.
      END. /* PLU-SJEKK */

      ASSIGN
          BongLinje.ArtikkelNr = ""
          BongLinje.LopeNr     = 0
          cError = cError + 
                   (IF cError = ""
                      THEN ""
                      ELSE chr(10)) + 
                   "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                   " Ukjent strekkode/PLU nummer. " +
                   "."
          cFilError = cFilError + 
                   (IF cFilError = ""
                      THEN ""
                      ELSE "|") + 
                   " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                   " Ukjent strekkode/PLU nummer. " +
                   "." + CHR(1) + "2"
          BongHode.Gradering = IF BongHode.Gradering < 3 THEN 3 ELSE BongHode.Gradering
          BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
          .
      RETURN.
  END. /* STREK-PLU */
  
  IF AVAILABLE Strekkode THEN
      FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
  ELSE FIND ArtBas WHERE
      ArtBas.ArtikkelNr = dec(BongLinje.ArtikkelNr) NO-ERROR.

  /* Kontrollerer varegruppen */
  IF AVAILABLE ArtBas THEN
  DO:
      IF BongLinje.VareGr <> ArtBas.Vg THEN
      DO:
          ASSIGN  
            cError = cError + 
                     (IF cError = ""
                        THEN ""
                        ELSE chr(10)) + 
                     "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     " Varegruppe er byttet på artikkel (ArtikkelNr: " + STRING(BongLinje.ArtikkelNr) + ") " +
                     " Fra VG: " + STRING(BongLinje.VareGr) + " til VG: " + STRING(ArtBas.Vg) +
                     "."
            cFilError = cFilError + 
                     (IF cFilError = ""
                        THEN ""
                        ELSE "|") + 
                     " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     " Varegruppe er byttet på artikkel (ArtikkelNr: " + STRING(BongLinje.ArtikkelNr) + ") " +
                     " Fra VG: " + STRING(BongLinje.VareGr) + " til VG: " + STRING(ArtBas.Vg) +
                     "." + CHR(1) + "2"
            BongHode.Gradering = IF BongHode.Gradering < 1 THEN 1 ELSE BongHode.Gradering
            BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
            .
      END.
  END.
  /* Ukjent artikkel */
  ELSE 
  UKJENT-ARTIKKEL:
  DO:
      ASSIGN  
        cError = cError + 
                 (IF cError = ""
                    THEN ""
                    ELSE chr(10)) + 
                 "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                 " Ukjent artikkel på transaksjonen (ArtikkelNr: " + STRING(BongLinje.ArtikkelNr) + ")" + 
                 "."
        cFilError = cFilError + 
                 (IF cFilError = ""
                    THEN ""
                    ELSE "|") + 
                 " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                 " Ukjent artikkel på transaksjonen (ArtikkelNr: " + STRING(BongLinje.ArtikkelNr) + ")" + 
                 "." + CHR(1) + "2"
        BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
        BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
        .
  END. /* UKJENT-ARTIKKEL */

  ASSIGN
      BongLinje.ArtikkelNr = (IF AVAILABLE ArtBas
                                THEN trim(string(ArtBas.ArtikkelNr,">>>>>>>>>>>>9"))
                                ELSE trim(string(BongLinje.ArtikkelNr,">>>>>>>>>>>>9")))
      BongLinje.VareGr     = (IF AVAILABLE ArtBas
                               THEN ArtBas.Vg
                               ELSE BongLinje.VareGr)
      BongLinje.LopeNr     = (IF AVAILABLE ArtBas
                               THEN ArtBas.LopNr
                               ELSE ?)
      .

  /* Kontrollerer gyldig varegruppe */
  FIND VarGr NO-LOCK WHERE
      VarGr.Vg = BongLinje.VareGr NO-ERROR.

  IF AVAILABLE VarGr THEN
      BongLinje.VareGruppeNavn = VarGr.VgBeskr.
  ELSE DO:
      ASSIGN  
        BongLinje.VareGruppeNavn = "** Ukjent varegruppe **"
        cError = cError + 
                 (IF cError = ""
                    THEN ""
                    ELSE chr(10)) + 
                 "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                 " Ukjent varegruppe på transaksjonen " + STRING(BongLinje.VareGr) + 
                 "."
        cFilError = cFilError + 
                 (IF cFilError = ""
                    THEN ""
                    ELSE "|") + 
                 " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                 " Ukjent varegruppe på transaksjonen " + STRING(BongLinje.VareGr) + 
                 "." + CHR(1) + "2"
        BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
        BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
        .
  END.

  IF AVAILABLE VarGr THEN
  DO:
      FIND HuvGr NO-LOCK WHERE
          HuvGr.Hg = VarGr.Hg NO-ERROR.
      IF NOT AVAILABLE HuvGr THEN
          ASSIGN  
            cError = cError + 
                     (IF cError = ""
                        THEN ""
                        ELSE chr(10)) + 
                     "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     " Ukjent hovedgruppe på den varegruppe som er på varelinjen: " + string(BongLinje.VareGr) + "/" + STRING(VarGr.Hg) +
                     "."
            cFilError = cFilError + 
                     (IF cFilError = ""
                        THEN ""
                        ELSE "|") + 
                     " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     "  Ukjent hovedgruppe på den varegruppe som er på varelinjen: : " + string(BongLinje.VareGr) + "/" + STRING(VarGr.Hg) +  
                     "." + CHR(1) + "2"
            BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
            BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
            .
      ELSE
          ASSIGN
          BongLinje.HovedGr            = VarGr.Hg
          BongLinje.HovedGrBeskrivelse = HuvGr.HgBeskr
          .
  END.

  /* Henter størrelsen hvis det er en størrelseskode <> 0 */
  IF AVAILABLE Strekkode THEN
  IF Strekkode.StrKode <> 0 THEN
  DO:
      FIND StrKonv NO-LOCK WHERE
          StrKonv.StrKode = StrekKode.StrKode NO-ERROR.
      IF AVAILABLE StrKonv THEN
          BongLinje.Storrelse = StrKonv.Storl.
      ELSE DO:
          ASSIGN  
            cError = cError + 
                     (IF cError = ""
                        THEN ""
                        ELSE chr(10)) + 
                     "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     " Ukjent størrelse på bonglinjen: " + string(BongLinje.Storrelse) +
                     "."
            cFilError = cFilError + 
                     (IF cFilError = ""
                        THEN ""
                        ELSE "|") + 
                     " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
              " Ukjent størrelse på bonglinjen: " + string(BongLinje.Storrelse) +
                     "." + CHR(1) + "2"
            BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
            BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
            BongLinje.Storrelse = ""

            .
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValiderKasserer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderKasserer Procedure 
PROCEDURE ValiderKasserer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Gyldig kasserernr */
  IF NOT CAN-FIND(Forsalj WHERE
                  Forsalj.ForsNr = int(BongHode.KassererNr)) THEN
    ASSIGN  
      cError = cError + 
               (IF cError = ""
                  THEN ""
                  ELSE chr(10)) + 
               "** BongNr: " + string(BongHode.BongNr) + ": " +
               " Ukjent kasserer på transaksjonen " + STRING(BongHode.KassererNr) + 
               "." 
      cFilError = cFilError + 
               (IF cFilError = ""
                  THEN ""
                  ELSE "|") + 
               " - BongNr: " + string(BongHode.BongNr) + ": " +
               " Ukjent kasserer på transaksjonen " + STRING(BongHode.KassererNr) + 
               "." + CHR(1) + "2"
      BongHode.KassererNavn = "*Ukjent*"
      BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
      .
  /* Setter kassererinfo. */
  ELSE DO:
      FIND Forsalj NO-LOCK WHERE
          Forsalj.ForsNr = int(BongHode.KassererNr).
      ASSIGN
          BongHode.KassererNavn  = Forsalj.FoNamn
          .
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValiderKort) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderKort Procedure 
PROCEDURE ValiderKort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcKort     AS CHAR NO-UNDO.

  DEF VAR piKortType AS INT NO-UNDO.

  ASSIGN
      piKortType = 0
      .
        
  /* Påfører KortType, Medlemsnummer og kundenummer. */
  RUN sjekkmedlem.p (INPUT  pcKort,
                     INPUT  BongHode.ButikkNr,
                     INPUT  BongHode.KasseNr,
                     INPUT  BongHode.Dato,
                     OUTPUT piKortType,
                     OUTPUT BongHode.MedlemsNr,
                     OUTPUT BongHode.MedlemNavn,
                     OUTPUT BongHode.KundeNr,
                     OUTPUT BongHode.KundeNavn).

  ASSIGN
      BongHode.KortType = piKortType /* 1-Ingen, 2-Kunde, 3-Medlem */
      .
  IF piKortType = 0 THEN
      ASSIGN  
        cError = cError + 
                 (IF cError = ""
                    THEN ""
                    ELSE chr(10)) + 
                 "** BongNr: " + string(BongHode.BongNr) +
                 " Ukjent kort på transaksjonen " + pcKort + 
                 "." 
        cFilError = cFilError + 
                 (IF cFilError = ""
                    THEN ""
                    ELSE "|") + 
                 " - BongNr: " + string(BongHode.BongNr) + 
                 " Ukjent kort på transaksjonen " + pcKort + 
                 "." + CHR(1) + "2"
        BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
        .
        
  /* Validerer kundeinfo. */
  IF BongHode.KundeNr <> 0 THEN
  KUNDE:
  DO:
      /* Kontrollerer gyldig kundenummer. */                
      IF NOT CAN-FIND(Kunde WHERE
                      Kunde.KundeNr  = BongHode.KundeNr) THEN
        ASSIGN  
          cError = cError + 
                   (IF cError = ""
                      THEN ""
                      ELSE chr(10)) + 
                   "** BongNr: " + string(BongHode.BongNr) +
                   " Ukjent kontonummer på transaksjonen " + string(BongHode.KundeNr) + 
                   "."
          cFilError = cFilError + 
                   (IF cFilError = ""
                      THEN ""
                      ELSE "|") + 
                   " - BongNr: " + string(BongHode.BongNr) +
                   " Ukjent kontonummer på transaksjonen " + string(BongHode.KundeNr) + 
                   "." + CHR(1) + "2"
          BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
          .
  END. /* KUNDE */

  /* Validerer Medlemsinformajson. */
  IF BongHode.MedlemsNr <> 0 THEN
  MEDLEM:
  DO:
      /* Kontrollerer gyldig kundenummer. */                
      IF NOT CAN-FIND(Medlem WHERE
                      Medlem.MedlemsNr  = BongHode.MedlemsNr) THEN
        ASSIGN  
          cError = cError + 
                   (IF cError = ""
                      THEN ""
                      ELSE chr(10)) + 
                   "** BongNr: " + string(BongHode.BongNr)  +
                   " Ukjent medlemsnummer på transaksjonen " + STRING(BongHode.MedlemsNr) + 
                   "."
          cFilError = cFilError + 
                   (IF cFilError = ""
                      THEN ""
                      ELSE "|") + 
                   " - BongNr: " + string(BongHode.BongNr) +
                   " Ukjent medlemsnr på transaksjonen " + STRING(BongHode.MedlemsNr) + 
                   "." + CHR(1) + "2"
          BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
          .
  END. /* MEDLEM */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VareSalg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VareSalg Procedure 
PROCEDURE VareSalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR plDec AS DEC NO-UNDO.
    /*
    IF CAN-FIND(SlKort WHERE
            SlKort.KortId = tt_Rader.Product_Id) THEN
    */
/*     IF CAN-FIND(FIRST AnalyseArtikkel WHERE                               */
/*                 AnalyseArtikkel.ArtikkelNr = dec(tt_Rader.Product_Id) AND */
/*                 AnalyseArtikkel.Aktiv      = TRUE AND                     */
/*                 AnalyseArtikkel.StartDato <= tt_Rader.Act_Date AND        */
/*                 AnalyseArtikkel.SluttDato >= tt_Rader.Act_Date) THEN      */
/*     ASSIGN                                                                */
/*     iAnalyse = 1                                                          */
/*     .                                                                     */
   IF AVAILABLE ArtBas THEN
       RELEASE ArtBas.

   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr     = ttKvitto.ButikkNr
       ttKvittoRad.GruppeNr     = ttKvitto.GruppeNr
       ttKvittoRad.VareGr       = INT(tt_Rader.ProdGr_Id)
       ttKvittoRad.KasseNr      = ttKvitto.KasseNr
       ttKvittoRad.TransDato    = ttKvitto.Dato
       ttKvittoRad.Tid          = tt_Rader.Act_Time
       ttKvittoRad.BongNr       = ttKvitto.BongNr

       ttKvittoRad.ArtikkelNr   = TRIM(TT_Rader.PRODUCT_NO)
       ttKvittoRad.LopeNr       = iLopeNr
       ttKvittoRad.LinjeNr      = iRadNr
       ttKvittoRad.TTId         = IF tt_Rader.Quantity < 0
                                    THEN 10
                                    ELSE 1
       ttKvittoRad.TBId         = tt_Rader.TBId
       ttKvittoRad.TEXT_1       = Descript
       ttKvittoRad.StrekKode    = TRIM(tt_Rader.Product_Id)
       ttKvittoRad.Antall       = tt_Rader.Quantity 
       ttKvittoRad.LinjeSum     = abs(tt_Rader.Amount + (IF tt_Rader.L_Discount <> ?
                                                                 THEN tt_Rader.L_Discount
                                                                 ELSE 0))
       ttKvittoRad.BongPris    = ABS(tt_Rader.L_SUPP_PRICE)

/*        ttKvittoRad.BongPris    = ABS(tt_Rader.L_SUPP_PRICE / tt_Rader.Quantity) */
       ttKvittoRad.BongPris    = IF ttKvittoRad.BongPris = ?
                                    THEN 0
                                    ELSE ttKvittoRad.BongPris 
       ttKvittoRad.MvaKr       = ABS(tt_Rader.L_Vat)
       ttKvittoRad.Mva%        = ABS(tt_Rader.Vat_P)
       ttKvittoRad.LinjeRab    = ABS(tt_Rader.L_Discount)
       ttKvittoRad.OriginalData = tt_Rader.OriginalData
       /* Setter flagg i bonghodet. */
       ttKvitto.flSlKort        = iAnalyse
       iAnalyse                  = 0
       .   
    /* Konvertering av EAN og varegruppe. */
    KONVERT:
    DO:
        /* Lagrer originalverdier før konvertering */
        IF ttKvittoRad.ForKonvertering = "" THEN
            ASSIGN
            ttKvittoRad.ForKonvertering = FILL(CHR(1),20)
            .
        ASSIGN
            entry(1,ttKvittoRad.ForKonvertering,CHR(1)) = "VarGr=" + STRING(ttKvittoRad.VareGr)
            entry(2,ttKvittoRad.ForKonvertering,CHR(1)) = "Strekkode=" + STRING(ttKvittoRad.Strekkode)
            .

        /* EAN/PLU */
        FIND ImpKonv WHERE ImpKonv.EDB-System = "MegaDisk"  AND
                           ImpKonv.Tabell     = "Strekkode" AND
                           ImpKonv.EksterntID = TRIM(ttKvittoRad.StrekKode) NO-LOCK NO-ERROR.
        IF AVAILABLE ImpKonv THEN
            ttKvittoRad.StrekKode = ImpKonv.InterntId.
        /* Varegruppe */
        FIND ImpKonv WHERE ImpKonv.EDB-System = "MegaDisk" AND
                           ImpKonv.Tabell     = "VarGr" AND
                           ImpKonv.EksterntID = TRIM(string(ttKvittoRad.VareGr)) NO-LOCK NO-ERROR.
        IF AVAILABLE ImpKonv THEN
            ttKvittoRad.VareGr = int(ImpKonv.InterntId).
    END. /* KONVERT */

    FIND StrekKode NO-LOCK WHERE
         StrekKode.Kode = ttKvittoRad.StrekKode NO-ERROR.
    IF NOT AVAILABLE Strekkod THEN
    SJEKKSTREKKODE:
    DO:
        ASSIGN
            plDec = DEC(ttKvittoRad.StrekKode)
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            LEAVE SJEKKSTREKKODE.

        /* Er det en EAN 8? Er det det, sjekkes med og uten nullutfylling. */
        IF LENGTH(STRING(DEC(ttKvittoRad.StrekKode))) = 8 THEN
        DO:
            /* Tetster med 8 siffer. */
            FIND Strekkode NO-LOCK WHERE
                Strekkode.Kode = STRING(DEC(ttKvittoRad.StrekKode)) NO-ERROR.
            IF AVAILABLE Strekkode THEN
                ttKvittoRad.StrekKode = Strekkode.Kode.
            /* Tester med 13 siffer */
            ELSE DO:
                FIND Strekkode NO-LOCK WHERE
                    Strekkode.Kode = "00000" + STRING(DEC(ttKvittoRad.StrekKode)) NO-ERROR.
                IF AVAILABLE Strekkode THEN
                    ttKvittoRad.StrekKode = Strekkode.Kode.
            END.
        END.
        /* Sjekker uten nullutfylling */
        ELSE DO:
            FIND Strekkode NO-LOCK WHERE
                Strekkode.Kode = LEFT-TRIM(ttKvittoRad.StrekKode,"0") NO-ERROR.
            IF AVAILABLE STrekkode THEN
                ttKvittoRad.StrekKode = Strekkode.Kode.
        END.
    END. /* SJEKKSTREKKODE */
    IF AVAIL StrekKode THEN
        FIND ArtBas NO-LOCK WHERE ArtBas.Artikkelnr = StrekKode.ArtikkelNr NO-ERROR.
    IF NOT AVAIL ArtBas THEN
        FIND ArtBas NO-LOCK WHERE
            ArtBas.ArtikkelNr = dec(ttKvittoRad.ArtikkelNr) NO-ERROR.
    IF AVAILABLE ArtBas THEN
        ASSIGN
        ttKvittoRad.ArtikkelNr = STRING(ArtBas.ArtikkelNr)
        ttKvittoRad.VareGr = ArtBas.Vg
        ttKvittoRad.LopeNr = ArtBas.LopNr
        .
    ELSE
        ASSIGN ttKvittoRad.ArtikkelNr = "".

    /* Varegruppetekst */
    FIND VarGr NO-LOCK WHERE 
        VarGr.Vg = ttKvittoRad.VareGr NO-ERROR.
    IF AVAILABLE VarGr THEN
    DO:
        ASSIGN
            ttKvittoRad.VareGruppeNavn = VarGr.VgBeskr
            .
        FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
        IF AVAILABLE HuvGr THEN
            ASSIGN
            ttKvittoRad.HovedGr            = HuvGr.Hg
            ttKvittoRad.HovedGrBeskrivelse = HuvGr.HgBeskr
            .
        ELSE
            ttKvittoRad.HovedGrBeskrivelse = "** Ukjent hovedgruppe *".
    END.
    ELSE
        ttKvittoRad.VareGruppeNavn = "** Ukjent varegruppe **".

    /* Spesiell håndtering av utbetalinger av premier hos Pressbyrån. */
    IF can-do(cVinst,string(ttKvittoRad.VareGr)) THEN
        ASSIGN
        ttKvittoRad.LinjeSum = ttKvittoRad.LinjeSum
        ttKvittoRad.BongPris = ttKvittoRad.BongPris
        ttKVittoRad.TTId     = 62
        ttKvittoRad.Antall   = 0
        .
    
    /* Oppdaterer Mva gruppenavn */
    FIND FIRST Moms NO-LOCK WHERE
        Moms.MomsProc = ttKvittoRad.Mva% NO-ERROR.
    IF AVAILABLE Moms THEN
        ASSIGN
        ttKvittoRad.MvaKod        = Moms.MomsKod
        ttKvittoRad.MvaGruppeNavn = Moms.Beskrivelse
        .
    ELSE
        ttKvittoRad.MvaGruppeNavn = "** Ukjent mva gruppe **".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Veksel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Veksel Procedure 
PROCEDURE Veksel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Ved negativt kontantbeløp, kommer det også et beløp i 
               penger tilbake. I disse tilfellene skal det ikke 
               skapes en vekseltransaksjon.
------------------------------------------------------------------------------*/
   IF tt_Rader.Amount_Ret <> 0 AND bPengerTilbake = FALSE THEN
   DO:
       CREATE ttKvittorad.
       ASSIGN
           ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
           ttKvittoRad.ButikkNr     = ttKvitto.ButikkNr
           ttKvittoRad.GruppeNr     = ttKvitto.GruppeNr
           ttKvittoRad.KasseNr      = ttKvitto.KasseNr
           ttKvittoRad.TransDato    = ttKvitto.Dato
           ttKvittoRad.Tid          = tt_Rader.Act_Time
           ttKvittoRad.BongNr       = ttKvitto.BongNr

           ttKvittoRad.LinjeNr      = tt_Rader.iRow + 10 /* Sikre at linjenummer ikke smeller */
           ttKvittoRad.OriginalData = tt_Rader.OriginalData
           ttKvittoRad.TTId         = 70
           ttKvittoRad.TBId         = 1
           ttKvittoRad.StrekKode    = ""

           ttKvittoRad.TEXT_1       = tt_Rader.PayType_Id /* Card name */
           ttKvittoRad.BongPris     = tt_Rader.Amount_Ret * -1
           ttKvittoRad.LinjeSum     = tt_Rader.Amount_Ret * -1
           /*ttKvittoRad.kpsxKvant  = DEC(ldPost.amount2) / 100*/
           ttKvittoRad.Antall       = 0
           .
   END.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

