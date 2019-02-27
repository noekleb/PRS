&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        :  xinndtleljournalpos.p
    Purpose     :  Innlesning av kvitteringsfil fra Nucleus/Wayne kasse.

    Syntax      :

    Description :  Kvitteringsnummerne fra Wayne benytter samme serie fra 
                   alle kassene. Det leses derfor bare inn en kasse pr. butikk.

    Author(s)   :  Tom Nøkleby
    Created     :  6/12-05
    Notes       :
    
    Mapping av artikkelnummer. 
    Artikkelnummerne som ligger i disse gruppene, skal ha endret sitt artikkelnr.
    Regel for mapping er: 9000 + GruppeNr (to siffer).
    Nytt artikkelnummer bilr da på 6 siffer.
    01 = Bensin  **             1. Drivmedel
    02 = Diesel   **        1. Drivmedel
    70 = Bensin 95 oktan        1. Drivmedel
    71 = Bensin 96 oktan        1. Drivmedel
    72 = Bensin 98 oktan        1. Drivmedel
    73 = Diesel             1. Drivmedel
    74 = Biomil (diesel)        1. Drivmedel
    75 = Bensin                 1. Drivmedel
    76 = Bensin             1. Drivmedel
    77 = Bensin             1. Drivmedel
    78 = Etanol             1. Drivmedel
    79 = Diesel                 1. Drivmedel
    80 = Diesel Lsk         1. Drivmedel
    81 = D10                1. Drivmedel
    82 = Motorgas               1. Drivmedel

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF INPUT  PARAMETER h_Telleverk AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR cError          AS CHAR NO-UNDO.
DEF VAR piLoop1         AS INT  NO-UNDO.
DEF VAR cLinje          AS CHAR NO-UNDO.
DEF VAR cFilNavn        AS CHAR NO-UNDO.
DEF VAR cBongFil        AS CHAR NO-UNDO.

DEF VAR h_dproclib      AS HANDLE NO-UNDO.

/* Konvertering av transaksjonskoder */
DEF VAR cBONGLst        AS CHAR NO-UNDO.
DEF VAR cTTIDLst        AS CHAR NO-UNDO.
DEF VAR cPOSLst         AS CHAR NO-UNDO.
DEF VAR cPOS            AS CHAR NO-UNDO.
DEF VAR cTTID           AS CHAR NO-UNDO.

/* Felt for å holde info fra record Receipt Type */
DEF VAR iOperatorId        AS INT  NO-UNDO.
DEF VAR iTerminalNo        AS INT  NO-UNDO.
DEF VAR iOperatorShiftNo   AS INT  NO-UNDO.
DEF VAR iReceiptNumber     AS INT  NO-UNDO.
DEF VAR lReceiptTotAmaount AS DEC  NO-UNDO.
DEF VAR dReceiptDateTime   AS CHAR NO-UNDO.

DEF VAR pcBongLinje AS CHAR NO-UNDO.
DEF VAR pcPrefix    AS CHAR NO-UNDO.
DEF VAR pcRecord    AS CHAR NO-UNDO.
DEF VAR pcBeskr     AS CHAR NO-UNDO.
DEF VAR cStrekKode  AS CHAR NO-UNDO.
DEF VAR cOrgStrekkode AS CHAR NO-UNDO.
DEF VAR cTekst      AS CHAR NO-UNDO.
DEF VAR piVarGr     AS INT  NO-UNDO.
DEF VAR plEnhPris   AS DEC  FORMAT "->>>>>>>>9.99" NO-UNDO.
DEF VAR plQuantity  AS DEC  NO-UNDO.
DEF VAR plAmount    AS DEC  FORMAT "->>>>>>>>9.99" NO-UNDO.
DEF VAR piMvaKode   AS INT  NO-UNDO.
DEF VAR plMva%      AS DEC  FORMAT "->>9.99"       NO-UNDO.
DEF VAR plMvaKr     AS DEC  FORMAT "->>>>>>>>9.99" NO-UNDO.
DEF VAR plDiscount  AS DEC  FORMAT "->>>>>>>>9.99" NO-UNDO.
DEF VAR plSubTotal  AS DEC  FORMAT "->>>>>>>>9.99" NO-UNDO.
DEF VAR pbDropp     AS LOG  NO-UNDO.
DEF VAR bMakulert   AS LOG  NO-UNDO.
DEF VAR piCardMode  AS INT  NO-UNDO.

DEF VAR piAntBonger AS INT NO-UNDO.
DEF VAR plPrisprsalgsenhet AS DECI NO-UNDO.

DEF VAR lArtikkelNr     AS DEC  FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEF VAR lEAN            AS DEC  FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEF VAR iButikkNr       AS INT  NO-UNDO.
DEF VAR iGruppeNr       AS INT  NO-UNDO.
DEF VAR iKasseNr        AS INT  NO-UNDO.
DEF VAR iKassererNr     AS INT  NO-UNDO.
DEF VAR cInnKvittering  AS CHAR NO-UNDO.
DEF VAR iTotAntLinjer   AS INT  NO-UNDO.
DEF VAR cDatoListe      AS CHAR NO-UNDO.
DEF VAR cKontrolltabell AS CHARACTER  NO-UNDO. /* MottaksKontroll av vilken data vi skall testa mot */
DEF VAR iSequenceNbr    AS INT  NO-UNDO.
DEF VAR cReceiptType    AS CHAR NO-UNDO.
DEF VAR cKonvArt        AS CHAR NO-UNDO.
DEF VAR lSkiftHantering AS LOG NO-UNDO.

DEF STREAM InnFil.
DEF STREAM Bong.

DEF BUFFER bufButiker FOR Butiker.

DEF TEMP-TABLE tmpFilLinjer LIKE FilLinjer
    FIELD SequenceNbr   AS INT 
    FIELD ClTyNbr       AS INT FORMAT 999999
    FIELD ButikkNr      AS INT
    FIELD KasseNr       AS INT
    FIELD Dato          AS DATE
    FIELD Timer         AS INT
    FIELD Minutter      AS INT
    FIELD Sekunder      AS INT
    FIELD BongNr        AS INT
    FIELD BongLinje     AS INT
    FIELD KassererNr    AS INT
    FIELD Makulert      AS LOG
    FIELD RabattKr      AS DEC
    FIELD AvrundingKr   AS DEC
    FIELD PengerTilbake AS DEC
    FIELD Ignorer       AS LOG
    FIELD ArtikkelNr    AS DEC
    FIELD EAN           AS DEC
    FIELD Amount        AS DEC FORMAT "->>>,>>>,>>9.999"
    FIELD Antall        AS DEC FORMAT "->>>,>>>,>>9.999"
    FIELD MvaKr         AS DEC FORMAT "->>>,>>>,>>9.999"
    FIELD MvaKlasse     AS INT
    FIELD SkiftNr       AS DEC FORMAT "->>>>>>>>9"
    FIELD SkiftID       AS DEC FORMAT "->>>>>>>>>>>>9"
    INDEX Sequence IS UNIQUE SequenceNbr
    INDEX LinjeNr  IS UNIQUE LinjeNr
    INDEX Bong IS UNIQUE PRIMARY ButikkNr KasseNr Dato BongNr BongLinje
    INDEX MvaKlasse ButikkNr KasseNr Dato BongNr MvaKlasse
    .
DEF TEMP-TABLE tmpBongHode
    FIELD ButikkNr    AS INT FORMAT ">>>>>9"
    FIELD KasseNr     AS INT FORMAT ">>9"
    FIELD Dato        AS DATE
    FIELD BongNr      AS INT FORMAT "->>>>>>>9"
    FIELD TotVreMvaKr AS DEC FORMAT "->>>,>>>,>>9.999"
    FIELD TotLinMvaKr AS DEC FORMAT "->>>,>>>,>>9.999"
    FIELD BongRecid   AS RECID
    INDEX BongHode IS UNIQUE PRIMARY ButikkNr KasseNr Dato BongNr
    .

DEF TEMP-TABLE tmpMakBongHode
    FIELD ButikkNr    AS INT FORMAT ">>>>>9"
    FIELD KasseNr     AS INT FORMAT ">>9"
    FIELD Dato        AS DATE
    FIELD BongNr      AS INT FORMAT "->>>>>>>9"
    FIELD BongRecid   AS RECID
    INDEX BongHode IS UNIQUE PRIMARY ButikkNr KasseNr Dato BongNr
    .

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

ASSIGN
    cBONGLst  = "0102,1202,1302,1402,1502" 
    cPOSLst   = ""
    cTTIDLst  = ""
    cKonvArt  = "1,2,70,71,72,73,74,75,76,77,78,79,80,81,82"
    .

RUN NyFilLogg IN h_Parent (INPUT lFilId, STRING(TODAY) + " " + 
          STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
         " - xinnipeljournalpos.p startet.").

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
    cFilNavn = Filer.Katalog + "~\" + Filer.FilNavn.
   
/* Leser første linjen i filen. */
INPUT STREAM InnFil FROM VALUE(Filer.Katalog + "~\" + Filer.FilNavn) NO-ECHO.
  IMPORT STREAM InnFil UNFORMATTED cLinje.
INPUT STREAM InnFil CLOSE.

RUN koblekasse.p (INPUT lFilId,
                  INPUT h_Parent,
                  INPUT 1,
                  INPUT Filer.FilType,
                  INPUT cLinje,
                  OUTPUT iButikkNr,
                  OUTPUT iGruppeNr,
                  OUTPUT iKasseNr
                 ).
IF (iButikkNr = 0 AND iGruppeNr = 0 AND iKasseNr = 0) THEN
    RETURN "** Kobling av kasse misslykkes.".
{syspara.i 1 1 25 cKontrolltabell}
IF NOT CAN-DO("1,2",cKontrolltabell) THEN
    ASSIGN cKontrolltabell = "1".
FIND bufButiker NO-LOCK WHERE
    bufButiker.Butik = iButikkNr NO-ERROR.
IF AVAIL bufButiker THEN DO:
    ASSIGN lSkiftHantering = bufButiker.EODRapporter.
END.

RUN TellOppLinjer.

IF NOT VALID-HANDLE(h_dproclib) THEN
    RUN dproclib PERSISTENT SET h_dproclib.

RUN InnLesFil.    /* El-Journal. */

IF VALID-HANDLE(h_dproclib) THEN
    DELETE PROCEDURE h_dproclib.

RETURN "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Article) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Article Procedure 
PROCEDURE Article :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF CAN-DO("010301,120301,130301,140301,150301",string(tmpFilLinjer.ClTyNbr,"999999")) THEN
      pcBeskr = "Ukjent drivstoff artikkel".
  ELSE
      pcBeskr = "Ukjent artikkel".

  IF AVAILABLE ArtBas THEN
      RELEASE ArtBas.

  /* Trekker ut informasjon fra strengen */
  ASSIGN 
      piVarGr       = INT(SUBSTRING(tmpFilLinjer.Tekst,79,3))
      lArtikkelNr   = dec(SUBSTRING(tmpFilLinjer.Tekst,53,13))
      lEAN          = dec(SUBSTRING(tmpFilLinjer.Tekst,66,13))
      plEnhPris     = dec(trim(SUBSTRING(tmpFilLinjer.Tekst,82,8),"+")) / 100
      plQuantity    = dec(trim(SUBSTRING(tmpFilLinjer.Tekst,90,8),"+")) / 100
      plAmount      = dec(trim(SUBSTRING(tmpFilLinjer.Tekst,98,10),"+")) / 100
      piMvaKode     = INT(SUBSTRING(tmpFilLinjer.Tekst,108,2))
      plMvaKr       = IF tmpFilLinjer.MvaKr <> 0
                        THEN tmpFilLinjer.MvaKr
                        ELSE dec(trim(SUBSTRING(tmpFilLinjer.Tekst,110,10),"+")) / 100
      plDiscount    = dec(trim(SUBSTRING(tmpFilLinjer.Tekst,120,10),"+")) / 100 /* Linjerabatt */
      plDiscount    = plDiscount + tmpFilLinjer.RabattKr /* Legger på eventuell kampanjerabatt */
      /* Tar vare på strekkode eller artikkelnr. */
      cOrgStrekkode = IF LEFT-TRIM(TRIM(SUBSTRING(tmpFilLinjer.Tekst,66,13)),"0") <> ""
                       THEN "EAN=" + LEFT-trim(TRIM(SUBSTRING(tmpFilLinjer.Tekst,66,13)),"0")
                      ELSE IF LEFT-TRIM(TRIM(SUBSTRING(tmpFilLinjer.Tekst,53,13)),"0") <> "" THEN
                          "Vre=" + LEFT-TRIM(TRIM(SUBSTRING(tmpFilLinjer.Tekst,53,13)),"0")
                      ELSE "".
      pcRecord      = ";;;;;;;;;;;;;;;;". /* 17 felt */



  ASSIGN plPrisprsalgsenhet = dec(trim(SUBSTRING(tmpFilLinjer.Tekst,82,8),"+")) / 100 NO-ERROR.
  IF plPrisprsalgsenhet = ? THEN
      ASSIGN plPrisprsalgsenhet = 0.
 
  /* Mvakoder > 9 skal legges på 9. */
  IF piMvaKode > 9 THEN 
      piMvaKode = 9.

  /* Konverterer artikkelNr for drivstoffartikler.                           */
  /* Drivstoffartikler skal ha artikkelnr = 9000 + varegruppenr - to siffer. */
/*   IF tmpFilLinjer.BongNr     = 2977 OR                                    */
/*       tmpFilLinjer.BongNr    = 2997 THEN                                  */
/*   MESSAGE "BONGNR " tmpFilLinjer.BongNr "EAN " lEAN SKIP "VARGR " piVarGr */
/*       SKIP                                                                */
/*       "plEnhPris  "  plEnhPris SKIP                                       */
/*       "plQuantity "  plQuantity SKIP                                      */
/*       "plAmount   "  plAmount   SKIP                                      */
/*       "ARTNR      " lArtikkelNr                                           */
/*                                                                           */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                              */
  IF CAN-DO(cKonvArt,STRING(piVarGr)) THEN
      lEAN = dec("9000" + string(piVarGr,"99")).
  ELSE IF (lEAN > 900000 AND lEAN < 900200) OR (lArtikkelNr > 900000 AND lArtikkelNr < 900200) THEN DO:
      ASSIGN lEAN = 0
             lArtikkelNr = 0.
  END.

  /* Beregner Mva% */
  ASSIGN
      plMva% = ROUND(((abs(plAmount) / (abs(plAmount) - abs(plMvaKr))) - 1) * 100,0)
      plMva% = IF plMva% = ? THEN 0 ELSE plMva%
      .
  
  /* Henter artikkel på grunnlag av strekkoden */
  IF lEAN > 0 THEN
  DO:
      ASSIGN   
          cStrekkode = STRING(lEAN)
          .
      /* Här skall vi hantera 7388 samt 20... koder */
      IF (cStrekkode BEGINS "7388" OR cStrekkode BEGINS "20") AND LENGTH(cStrekkode) = 13 THEN DO:
          IF cStrekkode BEGINS "7388" THEN
              ASSIGN cStrekkode = SUBSTR(cStrekKode,1,8) + "00000".
          ELSE DO:
              ASSIGN cStrekKode = SUBSTR(cStrekKode,1,8) + "00000"
                     cStrekkode = DYNAMIC-FUNCTION('fixChkEAN':U,INPUT SUBSTR(cStrekkode,1,12)).
          END.
      END.

      /* Sjekker rått. */
      find Strekkode no-lock WHERE Strekkode.Kode = cStrekkode no-error.
      IF NOT AVAILABLE Strekkode THEN
      DO:
          ASSIGN
              cTekst     = STRING(lEAN)
              cStrekKode = IF LENGTH(cTekst) < 6 
                             THEN STRING(cTekst) 
                           ELSE IF LENGTH(cTekst) = 6 
                              THEN DYNAMIC-FUNCTION('fixChkEAN':U IN h_dproclib, 
                                       INPUT DYNAMIC-FUNCTION('EkspanderUPC':U IN h_dproclib, INPUT STRING(cTekst)))
                           ELSE STRING(cTekst,"9999999999999").
          /* Sjekksifferkontroll */
          IF (LENGTH(cStrekkode) = 13 OR 
              LENGTH(cStrekkode) = 8) THEN
          ASSIGN cStrekkode = DYNAMIC-FUNCTION('EANprefixKonv':U IN h_dproclib, INPUT cStrekkode).

          /* Sjekker med nullutfylling. */
          find Strekkode no-lock WHERE Strekkode.Kode = cStrekkode no-error.
      END.
      /* Sjekker uten nullutfylling. */
      IF NOT AVAILABLE Strekkode THEN
      DO:
          find Strekkode no-lock WHERE Strekkode.Kode = LEFT-TRIM(cStrekkode,"0") no-error.
          IF AVAILABLE Strekkode THEN
              cStrekkode = LEFT-TRIM(cStrekkode,"0").         
      END.
      IF AVAILABLE Strekkode THEN
          FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
  END.

  /* EAN kode ikke angitt, eller ukjent. Da skal koden hentes på artikkelnummer */
  IF lEAN = 0 OR NOT AVAILABLE ArtBas THEN
  DO:
      FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
      IF AVAILABLE ArtBas THEN
          FIND FIRST Strekkode OF ArtBas NO-ERROR.
      /* PLU nummer. Disse har 0 i strekkode fra Wayne */
/*       ELSE IF lArtikkelNr < 1000000 THEN                      */
/*           FIND Strekkode NO-LOCK WHERE                        */
/*                Strekkode.Kode = STRING(lArtikkelNr) NO-ERROR. */
      IF AVAILABLE Strekkode THEN 
          ASSIGN
              lEAN       = DEC(Strekkode.Kode)
              cStrekkode = Strekkode.Kode
              .
      ELSE /* Ukjent strekkode. Da setter vi artikkelnummeret inn i strekkodefeltet */
          assign
              cStrekkode = IF lEAN <> 0 THEN STRING(lEAN) ELSE string(lArtikkelNr)
              pcBeskr    = (IF lEAN <> 0 THEN "**" + pcBeskr + " - " + "Art.nr istedenfor EAN" ELSE pcBeskr)                        
              .
      IF AVAILABLE Strekkode THEN
          FIND ArtBas OF Strekkode NO-ERROR.
  END.
  /* Er varen ukjent - skal den legges på samleartikkel. */
  IF NOT AVAILABLE ArtBas THEN
  POSTER-PA-SAMLEARTIKKEL:
  DO:
      IF piVarGr > 99 OR NOT CAN-FIND(HuvGr WHERE HuvGr.Hg = piVarGr) THEN
          ASSIGN piVarGr = 24.
      IF entry(1,cOrgStrekkode,"=") = "Vre" THEN
          lEAN = DEC("9000" + string(piVarGr,"99")). /* PLU vare */
      ELSE 
          lEAN = DEC("9001" + string(piVarGr,"99")). /* Scanning vare */
      ASSIGN   
          cStrekkode = STRING(lEAN)
          .
      /* Sjekker rått. */
      find Strekkode no-lock WHERE Strekkode.Kode = cStrekkode no-error.
      IF NOT AVAILABLE Strekkode THEN
      DO:
          ASSIGN
              cTekst     = STRING(lEAN)
              cStrekKode = IF LENGTH(cTekst) < 6 
                             THEN STRING(cTekst) 
                           ELSE IF LENGTH(cTekst) = 6 
                              THEN DYNAMIC-FUNCTION('fixChkEAN':U IN h_dproclib, 
                                       INPUT DYNAMIC-FUNCTION('EkspanderUPC':U IN h_dproclib, INPUT STRING(cTekst)))
                           ELSE STRING(cTekst,"9999999999999").
          /* Sjekksifferkontroll */
          IF (LENGTH(cStrekkode) = 13 OR 
              LENGTH(cStrekkode) = 8) THEN
          ASSIGN cStrekkode = DYNAMIC-FUNCTION('EANprefixKonv':U IN h_dproclib, INPUT cStrekkode).

          /* Sjekker med nullutfylling. */
          find Strekkode no-lock WHERE Strekkode.Kode = cStrekkode no-error.
      END.
      /* Sjekker uten nullutfylling. */
      IF NOT AVAILABLE Strekkode THEN
      DO:
          find Strekkode no-lock WHERE Strekkode.Kode = LEFT-TRIM(cStrekkode,"0") no-error.
          IF AVAILABLE Strekkode THEN
              cStrekkode = LEFT-TRIM(cStrekkode,"0").         
      END.
      IF AVAILABLE Strekkode THEN
          FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
      /* Logg strekkoden på samleartikkelen. Plu skal ikke logges. */
      IF AVAILABLE ArtBas AND NUM-ENTRIES(cOrgStrekkode,"=") = 2 and
          entry(1,cOrgStrekkode,"=") = "EAN" THEN
      LOGG-STREKKODE:
      DO:
          CREATE Strekkode.
          ASSIGN
              Strekkode.ArtikkelNr = ArtBas.ArtikkelNr
              Strekkode.Kode       = ENTRY(2,cOrgStrekkode,"=")
              Strekkode.StrKode    = 1
              Strekkode.KodeType   = IF ENTRY(1,cOrgStrekkode,"=") = "EAN"
                                       THEN 1
                                       ELSE 0
              Strekkode.VareId     = ArtBas.ArtikkelNr
              Strekkode.IKasse     = FALSE
              NO-ERROR.
          IF ERROR-STATUS:ERROR AND AVAILABLE Strekkode THEN
              DELETE STrekkode.
          IF AVAILABLE Strekkode THEN
              RELEASE Strekkode.
      END. /* LOGG-STREKKODE */
  END. /* POSTER-PA-SAMLEARTIKKEL */
  /* FEIL - Ukjent artikkel */
  IF lEAN = 0 THEN
  DO:
      RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                      STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                     " - Ukjent artikkel på linje " + STRING(iAntLinjer + 1) + ". ArtNr/EAN: " + string(lArtikkelNr) + "/" + string(lEAN) + "." + 
                     CHR(1) + "2").
  END.

  /* Plukker frisk info fra artikkelen */
  IF AVAILABLE ArtBas THEN
  DO:
      FIND ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
          ArtPris.ProfilNr   = bufButiker.ProfilNr NO-ERROR.
      IF NOT AVAILABLE ArtPris THEN
          FIND FIRST ArtPris WHERE
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
      ASSIGN
      /*piVarGr   = ArtBas.Vg*/
      plEnhPris = IF AVAILABLE ArtPris THEN ArtPris.Varekost[1] ELSE 0
      pcBeskr   = trim(ArtBas.Beskr)
      .
  END.
  ELSE
      ASSIGN
          plEnhPris = 0
          .

  /* Kommer salget fra en seddelautomat skal solgt beløp alltid avrundes til */
  /* nærmeste hele krone alltid nedover.                                     */
  IF substring(tmpFilLinjer.Tekst,6,2) = "13" THEN
      plAmount = TRUNCATE(plAmount,0).

  /* PANT                                                                  */
  /* Er antall positivt, men beløp negativt, er det PANT.                  */
  /* RETUR legges ut med både antall og beløp negativt på varelinjen.      */
  IF plAmount < 0 AND plQuantity > 0 THEN
  BETALING-MED-PANT:
  DO:
      ASSIGN
          pcBeskr    = "PANT" + STRING(plAmount)
          plAmount   = plAmount * -1
          plQuantity = plQuantity * -1
          .
  END. /* BETALING-MED-PANT */


  ENTRY(1,pcRecord,";") = IF tmpFilLinjer.Makulert THEN "1003" ELSE "3". /* Varesalg eller makulering */
  ENTRY(2,pcRecord,";") = trim(cStrekkode). /* EAN koden */
  ENTRY(3,pcRecord,";") = string(piVarGr). /* Varegruppen */
  ENTRY(4,pcRecord,";") = '"' + pcBeskr + '"'. /* Varetekst */
  ENTRY(5,pcRecord,";") = trim(replace(string(plQuantity,"->>>>>>>>9.999"),",",".")). /* Antall/mengde */
  ENTRY(6,pcRecord,";") = trim(replace(string(plAmount,"->>>>>>>>9.99"),",",".")). /* Beløp */
  ENTRY(7,pcRecord,";") = trim(replace(string(plEnhPris,"->>>>>>>>9.99"),",",".")). /* Kostpris */
  ENTRY(8,pcRecord,";") = trim(replace(string(plMva%,"->>>9.99"),",",".")). /* Mva% */
  ENTRY(9,pcRecord,";") = trim(replace(string(plMvaKr,"->>>>>>>>9.99"),",",".")). /* MvaKr */
  ENTRY(10,pcRecord,";") = "0". /* Feilkode */
  ENTRY(11,pcRecord,";") = "0". /* Tiltakskode */
  ENTRY(12,pcRecord,";") = IF plDiscount = 0 THEN "0" ELSE "9". /* Salgstype - Linjerabatt*/
  ENTRY(13,pcRecord,";") = trim(replace(string(plDiscount,"->>>>>>>>9.99"),",",".")). /* Avslag */
  ENTRY(14,pcRecord,";") = "0". /* Salgstype */
  ENTRY(15,pcRecord,";") = "0". /* Avslag */
  ENTRY(16,pcRecord,";") = "0". /* Salgstype */
  ENTRY(17,pcRecord,";") = "0". /* Avslag */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BongEksport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BongEksport Procedure 
PROCEDURE BongEksport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcIgnore AS CHAR NO-UNDO.
  DEF VAR piTime   AS INT  NO-UNDO.
  DEF VAR lSkiftSkrevet AS LOG NO-UNDO.
  ASSIGN
      piAntBonger = 0
      pcIgnore    = "010601,010204," +               /* VAT og Slutt bong POS                  */
                    "020202," +                      /* Fylling                                */
                    "120601,120204," +               /* VAT og Slutt bong kortautomat          */
                    "130601,130204," +               /* VAT og Slutt bong seddelautomat        */
                    "140601,140204," +               /* VAT og Slutt bong manuelt              */
                    "150601,150204," +               /* VAT og Slutt bong Non POS              */
                    "010304,120304,150304," +        /* Kampanjerabatt recordene skal ikke ut. */
                    "010507,120507,140507," +        /* Avrunding                              */
                    "010508,120508,140508"           /* Loyalitetskort/medlemskort             */
      .
/*   OUTPUT TO "CLIPBOARD".   */
/*   FOR EACH tmpFilLinjer:   */
/*       EXPORT tmpFilLinjer. */
/*   END.                     */
/*   OUTPUT CLOSE.            */
  LESERLINJER:
  FOR EACH tmpFilLinjer WHERE
      tmpFilLinjer.BongNr >= 0 AND
      tmpFilLinjer.BongNr <= 999999
      BREAK BY tmpFilLinjer.ButikkNr
            BY tmpFilLinjer.KasseNr
            BY tmpFilLinjer.Dato
            BY tmpFilLinjer.BongNr
            BY tmpFilLinjer.BongLinje:
    ASSIGN
        cLinje = tmpFilLinjer.Tekst
        .
    /* Setter filnavn og åpner stream */
    IF FIRST-OF(tmpFilLinjer.BongNr) THEN
    DO:
        ASSIGN lSkiftSkrevet = FALSE
               plPrisprsalgsenhet = 0.
        ASSIGN
            plSubTotal  = 0
            /*  pbdropp avaktiverats */
/*             pbDropp     = IF can-do("010203,120203,140203",string(tmpFilLinjer.ClTyNbr,"999999")) */
/*                             THEN TRUE                                                             */
/*                             ELSE FALSE                                                            */
            pbDropp     = IF can-do("",string(tmpFilLinjer.ClTyNbr,"999999"))
                            THEN TRUE
                            ELSE FALSE
            piAntBonger = piAntBonger + 1
            cBongFil = Filer.Katalog + "~\journal." + 
                       STRING(tmpFilLinjer.KasseNr,"999") + "." /*+ 
                       STRING(tmpFilLinjer.BongNr,"9999") + "."*/ + 
                       string(YEAR(tmpFilLinjer.Dato),"9999") + "-" + 
                       string(MONTH(tmpFilLinjer.Dato),"99") + "-" + 
                       string(DAY(tmpFilLinjer.Dato),"99") + "." /* + 
                       STRING(TIME) + "." */ + 
                       STRING(iButikkNr)
            piTime     = (tmpFilLinjer.Timer * 60 * 60) +        
                         (tmpFilLinjer.Minutter * 60) +     
                          tmpFilLinjer.Sekunder    
            pcPrefix = STRING(iButikkNr) + ";" + 
                       STRING(tmpFilLinjer.KasseNr) + ";" +
                       string(tmpFilLinjer.Dato) + ";" + 
                       STRING(piTime) + ";" + 
                       string(tmpFilLinjer.BongNr) + ";" +
                       STRING(tmpFilLinjer.KassererNr) + ";"
            .
/* IF tmpFilLinjer.Dato = ? THEN          */
/* MESSAGE                                */
/*     tmpFilLinjer.Tekst SKIP            */
/*     tmpFilLinjer.KasseNr SKIP          */
/*     tmpFilLinjer.BongNr                */
/*     YEAR(tmpFilLinjer.Dato)            */
/*     MONTH(tmpFilLinjer.Dato)           */
/*     DAY(tmpFilLinjer.Dato)             */
/*     TIME SKIP                          */
/*     iButikkNr                          */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
        OUTPUT STREAM Bong TO VALUE(cBongFil) APPEND.
    END.


    /* Rydder gamle record scoop */
    IF AVAILABLE ArtBas THEN release ArtBas.
    IF AVAILABLE Strekkode THEN RELEASE Strekkode.

    /* BongHode */
    /* 01XXXX - Cash register record */
    /* 12XXXX - Card terminal record */
    IF CAN-DO("010201,010202,010203,010205,010207," + /* Bonger fra kasse                     */
              "120201,120203,120204,120207," +        /* Bonger fra Selvbetjent kortautomat   */
              "130201,130202," +                      /* Bonger fra Selvbetjent seddelautomat */
              "140201,120202,140203," +               /* Bonger fra Selvbetjent Manuelt styrt */
              "150201"                                /* Bonger fra Non POS                   */
              ,string(tmpFilLinjer.ClTyNbr,"999999")) THEN
        RUN BongHode.
    /* Varesalg - Drivstoffartikkel og vanlig artikkel. */
    ELSE IF CAN-DO("010301,010302,010303,010305," + /* Varesalg   Kasse       */
                   "010401,010402,010403,010405," + /* Makulering Kasse       */
                   "120301,120303," +               /* Varesalg   Selvbetjent */ 
                   "130301," +                      /* Varesalg   Seddelautomat */
                   "140301,140302," +               /* Varesalg   Manuelt     */ 
                   "150301"                         /* Varesalg   Non POS     */ 
                   ,string(tmpFilLinjer.ClTyNbr,"999999")) THEN
        RUN Article.
    /* Betaling - Kontant og kortbetaling. */
    ELSE IF CAN-DO("010501,010502,010503,010504,010505,010506,010509," + /* Betaling POS            */
                   "120502," +                                    /* Betaling kortautomat    */
                   "130501," +                                    /* Betaling Seddelautomat  */
                   "140501,150502," +                             /* Betaling kortautomat    */
                   "150501"                                       /* Betaling Non POS        */
                   ,string(tmpFilLinjer.ClTyNbr,"999999")) THEN
        RUN Tendering.    
    /* Koder som skal ignoreres. */
    ELSE IF CAN-DO(pcIgnore,string(tmpFilLinjer.ClTyNbr,"999999")) THEN. /* Gjør ingenting */
    /* Ukjent kode i bongen */
    ELSE pcBongLinje = "** Ukjent kode:" + tmpFilLinjer.Tekst.
    /* Seetter sammen recorden */
    ASSIGN pcBongLinje = pcPrefix + pcRecord.
    
    IF NOT lSkiftSkrevet THEN DO:
        PUT STREAM Bong UNFORMATTED 
        pcPrefix + "113;1000;" + string(tmpFilLinjer.BongNr) + ";" + string(tmpFilLinjer.SkiftNr) + ";" + string(tmpFilLinjer.SkiftId) + ";"
        SKIP.
        ASSIGN lSkiftSkrevet = TRUE.
    END.
    IF plPrisprsalgsenhet <> 0 THEN DO:
        PUT STREAM Bong UNFORMATTED 
        pcPrefix + "113;1001;" + string(plPrisprsalgsenhet) + ";" + ";;"
        SKIP.
        ASSIGN plPrisprsalgsenhet = 0.
    END.

    /* Record som ignoreres, skal heller ikke ut. */
    IF NOT CAN-DO(pcIgnore,string(tmpFilLinjer.ClTyNbr,"999999")) AND pcRecord <> "" THEN 
    DO:
        IF tmpFilLinjer.Ignorer = FALSE THEN
        DO:
            /* Legger ut den originale strekkoden i størrelseskode feltet. */
            /* Denne vil ved innlesning bli lagret i BongLinje.Originaldata. */
            IF cOrgStrekkode <> "" THEN
                PUT STREAM Bong UNFORMATTED 
                pcPrefix + "106;" + cOrgStrekkode 
                SKIP.

            /* Legger ut linjen. */
            PUT STREAM Bong UNFORMATTED pcBongLinje SKIP.
        END.
        ASSIGN
            pcBongLinje   = ""
            pcRecord      = ""
            cOrgStrekkode = ""
            .
    END.

    /* Ferdig */
    ASSIGN pcBongLinje = "".

    /* Lukker stream */
    IF LAST-OF(tmpFilLinjer.BongNr) THEN
    DO:
/*         PUT STREAM Bong UNFORMATTED                                                                   */
/*         pcPrefix + "113;99;" + string(tmpFilLinjer.BongNr) + ";" + string(tmpFilLinjer.SkiftNr) + ";" */
/*         SKIP.                                                                                         */
        OUTPUT STREAM Bong CLOSE.
    END.

  END. /* LESLINJER */
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
  /* Trekker ut informasjon fra strengen */
  ASSIGN 
      plSubTotal = dec(trim(SUBSTRING(tmpFilLinjer.Tekst,53,10),"+")) / 100 /* Totalbeløp bong */
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InnLesFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InnLesFil Procedure 
PROCEDURE InnLesFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
  iButikkNr 
  iGruppeNr 
  iKasseNr  
  cFilNavn  
  lFilId    
  iAntLinjer
------------------------------------------------------------------------------*/
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
  DEF VAR pcButKasLst  AS CHAR  NO-UNDO.
  DEF VAR pc2Tekst     AS CHAR  NO-UNDO.
  DEF VAR piBongLinje  AS INT   NO-UNDO.
  DEF VAR piBongNr     AS INT   NO-UNDO.
  DEF VAR piSkiftNr    AS INT   NO-UNDO.
  
  DEF VAR d31Dec2002   AS DATE NO-UNDO.
  DEF VAR dSkiftnrTmp  AS DECI DECIMALS 0 NO-UNDO.

  DEFINE VARIABLE cOldSekvens AS CHARACTER  NO-UNDO.

  DEF VAR prBongRecid AS RECID NO-UNDO.

  DEF BUFFER bSkift FOR Skift.

  ASSIGN
      iantLinjer  = 0
      pcSokMaske  = ""
      pcOSokMaske = ""
      cDatoListe  = ""
      prRowId     = ?
      pcOLinje    = ""
      piBongNr    = ?
      cOldSekvens = "00000"
      d31Dec2002  = DATE(12,31,2002).

  /* Tømmer Mva loggen */
  FOR EACH tmpBongHode:
      DELETE tmpBongHode.
  END.
  /* Tømmer Makulert loggen */
  FOR EACH tmpMakBongHode:
      DELETE tmpMakBongHode.
  END.
  /* Leser inn linjer i temp-file for sortering. */
  FOR EACH tmpFilLinjer: 
      DELETE tmpFilLinjer.
  END.

  RUN Telleverk IN h_Parent ("Leser og konverterer data. Vent litt... ") NO-ERROR.

  piLinjeNr = 1.
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESINNBUFFER:
  REPEAT:
    ASSIGN
        pcLinje = ""
        .

    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.
    IF TRIM(pcLinje) = "" THEN
        NEXT LESINNBUFFER.
    IF SUBSTRING(pcLinje,1,5) = cOldSekvens THEN
        NEXT LESINNBUFFER.
    ASSIGN cOldSekvens = SUBSTRING(pcLinje,1,5).
    IF CAN-FIND(tmpfillinjer WHERE tmpFilLinjer.SequenceNbr = INT(cOldSekvens)) THEN
        NEXT LESINNBUFFER.
    /* Plukker ut transaksjonskoden */
    ASSIGN cPOS = SUBSTRING(pcLinje,6,6).

    /* Hopper over allt som ikke skal iinn */
    /* 01 - Kasse                          */
    /* 12 - Kortautomat   - Selvbetjent    */
    /* 13 - Seddelautomat - Selvbetjent    */
    /* 14 - Kortautomat manuelt styrt.     */
    IF NOT can-do("01,12,13,14,20",substring(cPOS,1,2)) THEN
        NEXT LESINNBUFFER.

    /* Inntill videre skipper vi også disse transaksjonene */
    /* Operatør shift records.                             */
/*     IF CAN-DO("0101,1401",substring(cPOS,1,4)) THEN */
    IF CAN-DO("1401",substring(cPOS,1,4)) THEN
        NEXT LESINNBUFFER.
    
    /* Detekterer bonghoder - Kasserecords.                                         */
    /* Det skal ikke legges opp en fillinje for disse. Kun ekstraheres informasjon. */
    /* Informasjonen herfra benyttes til neste bonghode detekteres.                 */
    IF CAN-DO(cBONGLst,substring(cPOS,1,4)) THEN
    DO:
        ASSIGN
            iOperatorId        = int(SUBSTRING(pcLinje,39,2))
            iTerminalNo        = int(SUBSTRING(pcLinje,41,2))
            iOperatorShiftNo   = int(SUBSTRING(pcLinje,43,6))
            iReceiptNumber     = int(SUBSTRING(pcLinje,49,4))
            iReceiptNumber     = IF iReceiptNumber = 0 THEN int(SUBSTRING(pcLinje,1,5)) ELSE iReceiptNumber
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                            STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                           " - Feil i linje - Kvitteringshode-Id/TNo/ShNo/RecNo: " + STRING(iAntLinjer + 1) + ". " + pcLinje + "." + 
                           CHR(1) + "3").
        ASSIGN
            lReceiptTotAmaount = DEC(trim(SUBSTRING(pcLinje,53,10),"+"))
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                            STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                           " - Feil i linje - Kvitteringshode-TotAmaount: " + STRING(iAntLinjer + 1) + ". " + pcLinje + "." + 
                           CHR(1) + "3").
IF can-do("010204,120204,130204,140204,150204",cPOS) AND NOT AVAIL tmpBongHode THEN
    NEXT.
        ASSIGN
            dReceiptDateTime   = IF can-do("010204,120204,130204,140204,150204",cPOS)
                                    THEN (SUBSTRING(pcLinje,12,10))
                                    ELSE (SUBSTRING(pcLinje,12,10))
         /* koden nedan ger olika datum på linjerna när fsg startat före midnatt och avslutats efter midnatt */
         /* den nya koden hämtar data från ett riktigare ställe. varför 10 när vi bara använder 6? */
/*                                   THEN (SUBSTRING(pcLinje,53,10)) */
/*                                   ELSE (SUBSTRING(pcLinje,63,10)) */
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                            STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                           " - Feil i linje - Kvitteringshode-DateTime: " + STRING(iAntLinjer + 1) + ". " + pcLinje + "." + 
                           CHR(1) + "3").
        ASSIGN
            pdDato             = DATE(
                                      int(SUBSTRING(dReceiptDateTime,3,2)),
                                      int(SUBSTRING(dReceiptDateTime,5,2)),
                                      int("20" + SUBSTRING(dReceiptDateTime,1,2))
                                     )
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                            STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                           " - Feil i linje - Kvitteringshode-Dato: " + STRING(iAntLinjer + 1) + ". " + pcLinje + "." + 
                           CHR(1) + "3").
        END.

        /* Konverterer kassenummer til egen nummerserie pr. kassetype. */
        RUN SjekkKasseNr (INPUT SUBSTRING(pcLinje,6,4), 
                          INPUT iButikkNr,
                          INPUT-OUTPUT iTerminalNo).
        /* OpprettMvaLogg. */
        IF NOT CAN-FIND(tmpBongHode WHERE
                        tmpBongHode.ButikkNr = iButikkNr     AND
                        tmpBongHode.KasseNr  = iTerminalNo   AND
                        tmpBongHode.Dato     = pdDato        AND
                        tmpBongHode.BongNr   = iReceiptNumber
                        ) THEN
        DO:
            CREATE tmpBongHode.
            ASSIGN
                tmpBongHode.ButikkNr  = iButikkNr
                tmpBongHode.KasseNr   = iTerminalNo     
                tmpBongHode.Dato      = pdDato
                tmpBongHode.BongNr    = iReceiptNumber     
                tmpBongHode.BongRecid = recid(tmpBongHode)
                prBongRecid           = recid(tmpBongHode).
                .

            IF iOperatorShiftNo = 0 AND lSkiftHantering = TRUE THEN DO:
                FIND LAST bSkift NO-LOCK WHERE bSkift.ButikkNr     = iButikkNr AND
                                               bSkift.Skiftnr      > 1000000 USE-INDEX Skift NO-ERROR.
                IF AVAILABLE bSkift AND bSkift.Bokforingsid = 0 THEN
                    ASSIGN iOperatorShiftNo = bSkift.SkiftNr.
                ELSE DO:
                    IF AVAIL bSkift THEN
                        ASSIGN dSkiftnrTmp = bSkift.Skiftnr.
                    ASSIGN iOperatorShiftNo = 1000000 + (pdDato - d31Dec2002).
/*                     FIND LAST bSkift NO-LOCK WHERE bSkift.ButikkNr    = iButikkNr AND               */
/*                                                    bSkift.Bokforingsid <> 0         AND             */
/*                                                    bSkift.Skiftnr      = iOperatorShiftNo NO-ERROR. */
                    IF  iOperatorShiftNo = dSkiftnrTmp THEN
                        iOperatorShiftNo = dSkiftnrTmp + 1.
                END.
            END.
        END.
/*
MESSAGE 
    "iOperatorId       "  iOperatorId       SKIP
    "iTerminalNo       "  iTerminalNo       SKIP
    "iOperatorShiftNo  "  iOperatorShiftNo  SKIP
    "iReceiptNumber    "  iReceiptNumber    SKIP
    "lReceiptTotAmaount"  lReceiptTotAmaount SKIP
    "dReceiptDateTime  "  dReceiptDateTime   SKIP
    "pdDato            "  pdDato            
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/    
        /*NEXT LESINNBUFFER.*/
    END.

    /* Oppretter bokføringsdag. */
    IF lSkiftHantering = TRUE AND CAN-DO("010106",substring(cPOS,1,6)) THEN
    DO:
         ASSIGN piSkiftNr = int(SUBSTRING(pcLinje,43,6)).
         FIND bSkift WHERE bSkift.butikknr = iButikkNr AND
                          bskift.terminert = FALSE    AND
                          bskift.skiftnr   = piSkiftNr NO-ERROR.
         IF AVAIL bSkift THEN
             ASSIGN bSkift.Terminert = TRUE.
         RELEASE bSkift.
    END.
    IF lSkiftHantering = TRUE AND CAN-DO("200102",substring(cPOS,1,6)) THEN DO:
      IF pdDato <> ? THEN DO:
        FIND Bokforingsdag NO-LOCK WHERE
            Bokforingsdag.ButikkNr = iButikkNr AND
            Bokforingsdag.Dato     = pdDato NO-ERROR.
        IF NOT AVAILABLE Bokforingsdag THEN DO:
          CREATE Bokforingsdag.
          ASSIGN
              Bokforingsdag.ButikkNr = iButikkNr
              Bokforingsdag.Dato     = pdDato
              Bokforingsdag.GruppeNr = 1
              piSkiftNr              = int(SUBSTRING(pcLinje,46,6))
              .
          /* Setter bokføringsdag på alle tilhørende skift. */
          FOR EACH Skift WHERE
              Skift.BokforingsId = 0 AND
              Skift.ButikkNr     = ibutikkNr AND 
              skift.terminert    = TRUE:
              ASSIGN Skift.BokforingsId = BokforingsDag.BokforingsId.
          END.
          FOR EACH Skift WHERE Skift.BokforingsId = 0 AND
                               Skift.ButikkNr     = ibutikkNr AND
                               Skift.SkiftNr      > 1000000 AND
                               Skift.terminert    = FALSE:
              ASSIGN Skift.BokforingsId = BokforingsDag.BokforingsId
                     skift.terminert    = TRUE.
          END.
        END.
        ELSE DO:
            FOR EACH Skift WHERE
                Skift.BokforingsId = 0 AND
                Skift.ButikkNr     = ibutikkNr AND 
                skift.terminert    = TRUE:
                ASSIGN Skift.BokforingsId = BokforingsDag.BokforingsId.
            END.
            FOR EACH Skift WHERE Skift.BokforingsId = 0 AND
                                 Skift.ButikkNr     = ibutikkNr AND
                                 Skift.SkiftNr      > 1000000 AND
                                 Skift.terminert    = FALSE:
                ASSIGN Skift.BokforingsId = BokforingsDag.BokforingsId
                       skift.terminert    = TRUE.
            END.
        END.
      END.
      NEXT LESINNBUFFER.
    END.

    IF piBongLinje = 0 THEN
        piBongLinje = 1.
IF NOT AVAIL tmpBongHode THEN
    NEXT LESINNBUFFER.
    /* Nuller bonglinjeteller */
    IF piBongNr <> iReceiptNumber THEN
    DO:
        ASSIGN
            piBongLinje  = 1
            piBongNr     = iReceiptNumber
            iKasseNr     = iTerminalNo
            iKassererNr  = iOperatorId
            cReceiptType = SUBSTRING(pcLinje,8,4) /* For å detektere penger tilbake */
            /* Bong er makulert - Makulering kan bare gjøres i kassen. */
            bMakulert    = IF can-do("010205",SUBSTRING(pcLinje,6,6)) THEN TRUE ELSE FALSE                
            prBongRecid  = 0
            .
        /* Oppretter SkiftRecord. */
/*         IF NOT CAN-FIND(Skift WHERE                             */
/*                         Skift.ButikkNr = iButikkNr and          */
/*                         Skift.Aar      = year(pdDato) AND       */
/*                         Skift.SkiftNr  = iOperatorShiftNo) THEN */
        IF lSkiftHantering = TRUE THEN DO:
            FIND Skift WHERE Skift.ButikkNr  = iButikkNr AND
                             Skift.terminert = FALSE     AND
                             Skift.SkiftNr   = iOperatorShiftNo NO-LOCK NO-ERROR.
            IF NOT AVAIL Skift THEN DO:
                CREATE Skift.
                ASSIGN
                    Skift.ButikkNr       = iButikkNr 
                    Skift.Aar            = year(pdDato) 
                    Skift.SkiftNr        = iOperatorShiftNo
                    Skift.KassererNr     = iKassererNr
                    Skift.Dato           = pdDato NO-ERROR .
            END.
        END.
    END.
    CREATE tmpFilLinjer.
    ASSIGN
        tmpFilLinjer.SequenceNbr = int(SUBSTRING(pcLinje,1,5))
        tmpFilLinjer.FilId       = lFilId
        tmpFilLinjer.LinjeNr     = piLinjeNr
        tmpFilLinjer.Tekst       = pcLinje 
        tmpFilLinjer.ButikkNr    = iButikkNr
        tmpFilLinjer.KasseNr     = iTerminalNo
        tmpFilLinjer.Dato        = pdDato
        tmpFilLinjer.Timer       = int(SUBSTRING(pcLinje,18,2))
        tmpFilLinjer.Minutter    = int(SUBSTRING(pcLinje,20,2))
        tmpFilLinjer.Sekunder    = int(SUBSTRING(pcLinje,22,2))
        tmpFilLinjer.BongNr      = piBongNr
        tmpFilLinjer.BongLinje   = piBongLinje
        tmpFilLinjer.KassererNr  = IF iOperatorId = 0 THEN 1 ELSE iOperatorId
        tmpFilLinjer.ClTyNbr     = int(cPOS)
        tmpFilLinjer.SkiftNr     = iOperatorShiftNo
        tmpFilLinjer.SkiftId     = IF AVAIL skift THEN skift.skiftid ELSE iOperatorShiftNo
        tmpFilLinjer.Makulert    = bMakulert
        piLinjeNr                = piLinjeNr + 1
        piBongLinje              = piBongLinje + 1
        .
    /* Rabatt - Promotion. */
    IF can-do("010304,120304,150304",string(tmpFilLinjer.ClTyNbr,"999999")) THEN
        RUN PromotionRad.
    /* Avrunding. */
    ELSE IF can-do("010507,120507,140507",string(tmpFilLinjer.ClTyNbr,"999999")) THEN
        RUN RoundOff.
    /* Detekterer penger tilbake. Beløp skal påføres foregående trans. */ 
    /*   - Bong med vanlig salg.                                       */
    /*   - Betalingsmiddel kontant                                     */
    /*   - Kontantbeløp skal være negativt.                            */
    /*   - Og selvfølgelig - Cahs Back for Bank.                       */
    ELSE IF (cReceiptType = "0201" OR cReceiptType = "0203" ) AND 
        can-do("010501,130501,140501,010509",string(tmpFilLinjer.ClTyNbr,"999999")) AND
        (dec(trim(SUBSTRING(tmpFilLinjer.Tekst,53,10),"+")) / 100) < 0 THEN
        RUN MoneyBack.
    /* Flagger MvaKlasser på varelinjene og logger total mva fra varelinjene på bongen. */
    /* Flagger også bonger hvor det skal sjekkes for makulerte rader.                   */
    IF CAN-DO("010301,010302,010303,010305,010401,010402,010403,010405,120301,130301,140301,140302,150301",string(tmpFilLinjer.ClTyNbr,"999999")) THEN
    MVA-KLASS:
    DO:
        /* Mvaklasser */
        ASSIGN
        tmpFilLinjer.MvaKlasse  = int(substring(tmpFilLinjer.Tekst,108,2))
        tmpfilLinjer.Amount     = dec(trim(SUBSTRING(tmpFilLinjer.Tekst,98,10),"+")) / 100
        tmpfilLinjer.Antall     = dec(trim(SUBSTRING(tmpFilLinjer.Tekst,90,8),"+")) / 100
        tmpfilLinjer.ArtikkelNr = dec(SUBSTRING(tmpFilLinjer.Tekst,53,13))
        tmpfilLinjer.EAN        = dec(SUBSTRING(tmpFilLinjer.Tekst,66,13))
        tmpfilLinjer.MvaKr      = dec(trim(SUBSTRING(tmpFilLinjer.Tekst,110,10),"+")) / 100
        tmpBongHode.TotVreMvaKr = tmpBongHode.TotVreMvaKr + dec(trim(SUBSTRING(tmpFilLinjer.Tekst,110,10),"+")) / 100
        .
        /* Flagg makulerte rader */
        IF tmpFilLinjer.Amount < 0 AND bMakulert = FALSE THEN
        DO:
            IF NOT CAN-FIND(tmpMakBongHode WHERE
                            tmpMakBongHode.ButikkNr = tmpFilLinjer.ButikkNr AND
                            tmpMakBongHode.KasseNr  = tmpFilLinjer.KasseNr  AND
                            tmpMakBongHode.Dato     = tmpFilLinjer.Dato     AND
                            tmpMakBongHode.BongNr   = tmpFilLinjer.BongNr
                            ) THEN
            DO:
                CREATE tmpMakBongHode.
                ASSIGN
                    tmpMakBongHode.ButikkNr  = tmpFilLinjer.ButikkNr
                    tmpMakBongHode.KasseNr   = tmpFilLinjer.KasseNr   
                    tmpMakBongHode.Dato      = tmpFilLinjer.Dato    
                    tmpMakBongHode.BongNr    = tmpFilLinjer.BongNr       
                    .
            END.
        END.
    END. /* MVA-KLASS */
    /* Logger total mva for mvalinjene på bongen. */
    IF CAN-DO("010601,120601,130601,140601,150601",string(tmpFilLinjer.ClTyNbr,"999999")) THEN
        ASSIGN
        tmpFilLinjer.MvaKlasse  = int(substring(tmpFilLinjer.Tekst,53,2))
        tmpfilLinjer.MvaKr      = dec(trim(SUBSTRING(tmpFilLinjer.Tekst,55,10),"+")) / 100
        tmpBongHode.TotLinMvaKr = tmpBongHode.TotLinMvaKr + dec(trim(SUBSTRING(tmpFilLinjer.Tekst,55,10),"+")) / 100
        .

  END. /* LESINNBUFFER */
  INPUT STREAM InnFil CLOSE.

  /* Sjekk av MVA.                                               */
  /* Sjekker sum mva på bonglinjene mot sum mva på mvarecordene. */
  RUN justerMvaVarelinje.

  /* Sjekk for makulerte rader. */
  /* Leser alle rader som er negative og ser om det finnes en lik rad tidligere på bongen. */
  /* gjør det det, skal begge radene makuleres.                                            */
  RUN sjekkMakulerRad.

  /* Eksport av bonger */
  RUN BongEksport.

  /* Stempler posten som innlest. */
  DO TRANSACTION:
      FIND CURRENT Filer EXCLUSIVE-LOCK.
      ASSIGN
          Filer.Innlest = TRUE
          Filer.InnlestDato = TODAY
          Filer.InnlestKl   = TIME
          Filer.InnlestAv   = USERID("SkoTex")
          . 
      FIND CURRENT Filer NO-LOCK.
  END.

  /* Nullstiller telleverket */
  RUN Telleverk IN h_Telleverk (" ") NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-justerMvaVarelinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE justerMvaVarelinje Procedure 
PROCEDURE justerMvaVarelinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR plMvaSum AS DEC FORMAT ">>>,>>>,>>9,999" NO-UNDO.

  DEF BUFFER btmpFilLinjer FOR tmpFilLinjer.

  /* Finner vi diff på mva på bongen, skal diffen psoteres på den varelinje som */
  /* har størst salgsverdi for mvaklassen.                                      */
  FOR EACH tmpBongHode WHERE
      (tmpBongHode.TotVreMvaKr - tmpBongHode.TotLinMvaKr) <> 0:
      
      /* Leser MVA recordene på bongen og sjekker disse mot varelinjene */
      FOR EACH tmpFilLinjer WHERE
          tmpFilLinjer.ButikkNr  = tmpBongHode.ButikkNr AND
          tmpFilLinjer.KasseNr   = tmpBongHode.KasseNr  AND
          tmpFilLinjer.Dato      = tmpBongHode.Dato     AND
          tmpFilLinjer.BongNr    = tmpBongHode.BongNr   AND
          tmpFilLinjer.MvaKlasse > 0 AND
          CAN-DO("010601,120601,130601,140601,150601",string(tmpFilLinjer.ClTyNbr,"999999")):

          /* Leser varelinjene pr. MvaKlasse. */
          LINJEKORR:
          FOR EACH btmpFilLinjer WHERE
              btmpFilLinjer.ButikkNr  = tmpFilLinjer.ButikkNr  AND
              btmpFilLinjer.KasseNr   = tmpFilLinjer.KasseNr   AND
              btmpFilLinjer.Dato      = tmpFilLinjer.Dato      AND
              btmpFilLinjer.BongNr    = tmpFilLinjer.BongNr    AND
              btmpFilLinjer.MvaKlasse = tmpFilLinjer.MvaKlasse AND
              CAN-DO("010301,010302,010303,010305,010401,010402,010403,010405,120301,130301,140301,140302,150301",string(btmpFilLinjer.ClTyNbr,"999999"))
              BREAK BY btmpFilLinjer.ButikkNr
                    BY btmpFilLinjer.KasseNr 
                    BY btmpFilLinjer.Dato    
                    BY btmpFilLinjer.BongNr  
                    BY btmpFilLinjer.MvaKlasse
                    BY btmpFilLinjer.Amount:

              ASSIGN
                  plMvaSum = plMvasum + btmpFilLinjer.MvaKr
                  .

              /* Er det diff, skal denne posteres på den største linjen i mva klassen */
              IF LAST-OF(btmpFilLinjer.MvaKlasse) THEN
              DO:
                  /* Sum mvalinje <> SumMva fra varelinjene. */
                  IF plMvaSum <> tmpFilLinjer.MvaKr THEN
                  DO:
                      btmpFilLinjer.MvaKr = btmpFilLinjer.MvaKr - (plMvaSum - tmpFilLinjer.MvaKr).
                  END.
                  plMvaSum = 0.
              END.

          END.


      END. /* LINJEKORR */




  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MoneyBack) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoneyBack Procedure 
PROCEDURE MoneyBack :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR plPengerTilbakeKr AS DEC NO-UNDO.

  DEF BUFFER buftmpFilLinjer FOR tmpFilLinjer.

  assign
      iSequenceNbr      = tmpFilLinjer.LinjeNr - 1
      plPengerTilbakeKr = dec(trim(SUBSTRING(tmpFilLinjer.Tekst,53,10),"+")) / 100
      .
  /* Henter foregående record og makulerer den. */
  FIND buftmpFilLinjer WHERE
      buftmpFilLinjer.LinjeNr = iSequenceNbr NO-ERROR.
  IF AVAILABLE buftmpFilLinjer AND SUBstring(STRING(buftmpFilLinjer.ClTyNbr,"999999"),3,2) = "05" THEN
      ASSIGN
      buftmpFilLinjer.PengerTilbake = abs(plPengerTilbakeKr)
      tmpFilLinjer.Ignorer          = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PromotionRad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PromotionRad Procedure 
PROCEDURE PromotionRad :
/*------------------------------------------------------------------------------
  Purpose:     Det er gitt rabatt på en vare. Ikke varelinjerabatt, men
               kampanjerabatt. Denne kommer på en egen linje.
               
  Parameters:  <none>
  Notes:       Automatiske rabatter - kampanjer, skal ikke komme som rabatt.
------------------------------------------------------------------------------*/
  DEF VAR plRabattKr AS DEC NO-UNDO.

  DEF BUFFER buftmpFilLinjer FOR tmpFilLinjer.


/*   assign                                                                       */
/*       iSequenceNbr = tmpFilLinjer.LinjeNr - 1                                  */
/*       plRabattKr   = dec(trim(SUBSTRING(tmpFilLinjer.Tekst,100,10),"+")) / 100 */
/*       .                                                                        */
/*   /* Henter foregående record og makulerer den. */                             */
/*   FIND buftmpFilLinjer WHERE                                                   */
/*       buftmpFilLinjer.LinjeNr = iSequenceNbr NO-ERROR.                         */
/*   IF AVAILABLE buftmpFilLinjer THEN                                            */
/*       buftmpFilLinjer.RabattKr = plRabattKr.                                   */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RoundOff) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RoundOff Procedure 
PROCEDURE RoundOff :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR plAvrundingKr AS DEC FORMAT "->>>>>>>9.99" NO-UNDO.
  DEF VAR piLinjeNr     AS INT NO-UNDO.

  DEF BUFFER buftmpFilLinjer FOR tmpFilLinjer.

  assign
      piLinjeNr            = tmpFilLinjer.LinjeNr - 1
      plAvrundingKr        = dec(trim(SUBSTRING(tmpFilLinjer.Tekst,53,10),"+")) / 100
      tmpFilLinjer.Ignorer = TRUE
      .
  /* Henter foregående record og makulerer den. */
  FIND buftmpFilLinjer WHERE
      buftmpFilLinjer.LinjeNr = piLinjeNr NO-ERROR.
  /* Tendering linjen forran, er en "Penger tilbake". Må derfor gå en trans til. */
  IF AVAILABLE buftmpFilLinjer THEN
  DO:
      IF buftmpFilLinjer.Ignorer = TRUE THEN
          FIND buftmpFilLinjer WHERE
              buftmpFilLinjer.LinjeNr = piLinjeNr - 1 NO-ERROR.
      IF AVAILABLE buftmpFilLinjer THEN
          buftmpFilLinjer.AvrundingKr = (plAvrundingKr) * -1.
  END.
  ELSE DO:
/*       MESSAGE "Feil i RoundOff" SKIP         */
/*           tmpFilLinjer.Tekst                 */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  END.

  /*
  assign
      iSequenceNbr         = tmpFilLinjer.SequenceNbr - 1
      plAvrundingKr        = dec(trim(SUBSTRING(tmpFilLinjer.Tekst,53,10),"+")) / 100
      tmpFilLinjer.Ignorer = TRUE
      .
  /* Henter foregående record og makulerer den. */
  FIND buftmpFilLinjer WHERE
      buftmpFilLinjer.SequenceNbr = iSequenceNbr NO-ERROR.
  /* Tendering linjen forran, er en "Penger tilbake". Må derfor gå en trans til. */
  IF AVAILABLE buftmpFilLinjer THEN
  DO:
      IF buftmpFilLinjer.Ignorer = TRUE THEN
          FIND buftmpFilLinjer WHERE
              buftmpFilLinjer.SequenceNbr = iSequenceNbr - 1 NO-ERROR.
      IF AVAILABLE buftmpFilLinjer THEN
          buftmpFilLinjer.AvrundingKr = (plAvrundingKr) * -1.
  END.
  ELSE DO:
      MESSAGE "Feil i RoundOff" SKIP
          tmpFilLinjer.Tekst
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SjekkKasseNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkKasseNr Procedure 
PROCEDURE SjekkKasseNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       RUN SjekkKasseNr (INPUT-OUTPUT iTerminalNr).
------------------------------------------------------------------------------*/
DEF INPUT        PARAMETER pcRecType    AS CHAR NO-UNDO.
DEF INPUT        PARAMETER piButikkNr   AS INT  NO-UNDO.
DEF INPUT-OUTPUT PARAMETER piTerminalNr AS INT  NO-UNDO.

DEF BUFFER bKasse FOR Kasse.

CASE pcRecType:
    WHEN "0102" THEN piTerminalNr = piTerminalNr +  0.
    WHEN "1202" THEN piTerminalNr = piTerminalNr + 10.
    WHEN "1302" THEN piTerminalNr = piTerminalNr + 40.
    WHEN "1402" THEN piTerminalNr = piTerminalNr + 50.
    WHEN "1502" THEN piTerminalNr = piTerminalNr + 60.
END CASE.

/* Oppretter kassen hvis den mangler. */
IF NOT CAN-FIND(Kasse WHERE
                Kasse.ButikkNr = pibutikkNr AND
                Kasse.GruppeNr = 1  AND
                Kasse.KasseNr  = piTerminalNr) THEN
  DO FOR bKasse TRANSACTION:
    /* Kasse 1 skal ALLTID være lagt opp på alle butikker. */
    FIND Kasse NO-LOCK WHERE
        Kasse.ButikkNr = piButikkNr AND
        Kasse.Gruppe   = 1 AND
        Kasse.KasseNr  = 1 NO-ERROR.
    IF AVAILABLE Kasse THEN
    DO:
        CREATE bKasse.
        BUFFER-COPY Kasse TO bKasse
            ASSIGN
            bKasse.KasseNr      = piTerminalNr
            bKasse.Navn         = "Kasse " + string(piTerminalNr) + " - Butikk " + string(piButikkNr)
            bKasse.ElJournal[2] = STRING(pibutikkNr)
            bKasse.ElJournalId  = STRING(pibutikkNr) + ";" + string(piTerminalNr) + ";"
            .
    END.

    IF AVAILABLE bKasse THEN
        RELEASE bKasse.
  END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sjekkMakulerRad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sjekkMakulerRad Procedure 
PROCEDURE sjekkMakulerRad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER btmpFilLinjer FOR tmpFilLinjer.

  FOR EACH tmpMakBongHode:
      /* Leser makulerte rader på bongen*/
      FOR EACH tmpFilLinjer WHERE
          tmpFilLinjer.ButikkNr  = tmpMakBongHode.ButikkNr  AND
          tmpFilLinjer.KasseNr   = tmpMakBongHode.KasseNr   AND
          tmpFilLinjer.Dato      = tmpMakBongHode.Dato      AND
          tmpFilLinjer.BongNr    = tmpMakBongHode.BongNr    AND
          tmpFilLinjer.Amount    < 0                        AND
          tmpFilLinjer.Makulert  = FALSE                    AND
          CAN-DO("010301,010302,010303,010305,010401,010402,010403,010405,120301,130301,140301,140302,150301",string(tmpFilLinjer.ClTyNbr,"999999")):

          /* Finner første motsvarende rad */
          FIND FIRST btmpFilLinjer WHERE
              btmpFilLinjer.ButikkNr   = tmpMakBongHode.ButikkNr  AND
              btmpFilLinjer.KasseNr    = tmpMakBongHode.KasseNr   AND
              btmpFilLinjer.Dato       = tmpMakBongHode.Dato      AND
              btmpFilLinjer.BongNr     = tmpMakBongHode.BongNr    AND
              btmpFilLinjer.Amount     = tmpFilLinjer.Amount * -1 AND
              btmpFilLinjer.Antall     = tmpFilLinjer.Antall * -1 AND
              btmpfilLinjer.ArtikkelNr = tmpfilLinjer.ArtikkelNr  AND
              btmpfilLinjer.EAN        = tmpfilLinjer.EAN         AND
              btmpFilLinjer.Makulert   = FALSE                    AND
              CAN-DO("010301,010302,010303,010305,010401,010402,010403,010405,120301,130301,140301,140302,150301",string(btmpFilLinjer.ClTyNbr,"999999"))
              NO-ERROR.

          /* Finner vi den, makulerer vi begge radene. */
          IF AVAILABLE btmpFillinjer THEN
              ASSIGN
              btmpFillinjer.Makulert = TRUE
              tmpFilLinjer.Makulert  = TRUE
              .
      END.

  END.
       
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
      .
  INPUT STREAM InnFil FROM VALUE(Filer.Katalog + "~\" + Filer.FilNavn) NO-ECHO.
  repeat:
    IMPORT STREAM InnFil UNFORMATTED cLinje.
    ASSIGN
        iTotAntLinjer = iTotAntLinjer + 1
        .
  END.
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Tendering) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tendering Procedure 
PROCEDURE Tendering :
/*------------------------------------------------------------------------------
  Purpose:     Utlegg av betalingslinje.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piValKod   AS INT NO-UNDO.
  DEF VAR plValProc  AS DEC FORMAT "->>>>9.999"    NO-UNDO.
  DEF VAR plPaid     AS DEC FORMAT "->>>>>>>>9.99" NO-UNDO.
  DEF VAR plRound    AS DEC FORMAT "->>>>>>>>9.99" NO-UNDO.
  DEF VAR piCardtype AS INT NO-UNDO.
  DEF VAR pcRecordPlus AS CHAR NO-UNDO.

  /* Trekker ut informasjon fra strengen */
  ASSIGN 
      plAmount    = dec(trim(SUBSTRING(tmpFilLinjer.Tekst,53,10),"+")) / 100 /* Betalt beløp */
      plPaid      = dec(trim(SUBSTRING(tmpFilLinjer.Tekst,63,10),"+")) / 100  /* Benyttet beløp */
      plRound     =  tmpFilLinjer.AvrundingKr
      piCardtype  = int(TRIM(SUBSTRING(tmpFilLinjer.Tekst,73,2),"0"))
      piCardMode  = 0 /* 0-Debetkort, 1-Kreditkort */
      pcRecord    = ";;;;;" /* 6 felt */
      pcRecordPlus = "".
  IF NOT tmpFilLinjer.Makulert THEN DO:
      CASE STRING(tmpFilLinjer.ClTyNbr,"999999"):
          WHEN "010503" THEN /* Other credit --> KREDIT */
              ASSIGN
              . /* Her kan man ta vare på type, autorisasjonsnr og utløpsdato. */
          OTHERWISE  
              ASSIGN
              piValKod    = INT(SUBSTRING(tmpFilLinjer.Tekst,73,2))
              plValProc   = dec(trim(SUBSTRING(tmpFilLinjer.Tekst,75,10),"+")) / 1000.
      END CASE.
  END.

  CASE string(tmpFilLinjer.ClTyNbr,"999999"):
      WHEN "010501" THEN DO:
          ENTRY(1,pcRecord,";") = IF tmpFilLinjer.Makulert THEN "1055" ELSE "55". 
      END.
      WHEN "010502" THEN DO:
          ENTRY(1,pcRecord,";") = IF tmpFilLinjer.Makulert THEN "1059" ELSE "59". 
          /* 0-Debetkort, 1-Kreditkort */
          /*IF SUBSTRING(tmpFilLinjer.Tekst,122,1) = "1" THEN*/
              pcRecord = pcRecord + ";" + 
                         SUBSTRING(tmpFilLinjer.Tekst,122,1).
      END.
      WHEN "010503" THEN DO:
          /* Manuell registrering av kreditkort */
          IF SUBSTRING(tmpFilLinjer.Tekst,73,2) = "01" THEN DO:
              ENTRY(1,pcRecord,";") = IF tmpFilLinjer.Makulert THEN "1060" ELSE "60". 
              pcRecordPlus = ";PREEMMSLIP;01" NO-ERROR.
          END.
          /* Bonus sjekk brukt som betaling */
          ELSE DO:
              pcRecordPlus = ";PREEMGAVE;" + SUBSTRING(tmpFilLinjer.Tekst,73,2) NO-ERROR.
              IF SUBSTRING(tmpFilLinjer.Tekst,73,2) = "02" THEN
                  ENTRY(1,pcRecord,";") = IF tmpFilLinjer.Makulert THEN "1057" ELSE "57". 
              /* Gavekort */
              ELSE IF SUBSTRING(tmpFilLinjer.Tekst,73,2) = "03" THEN
                  ENTRY(1,pcRecord,";") = IF tmpFilLinjer.Makulert THEN "1057" ELSE "57".            
              ELSE 
                  ENTRY(1,pcRecord,";") = IF tmpFilLinjer.Makulert THEN "1057" ELSE "57".            
          END.
      END.
      WHEN "010504" THEN ENTRY(1,pcRecord,";") = IF tmpFilLinjer.Makulert THEN "1058" ELSE "58". /* Sjekk brukes for Drive-Off */
      WHEN "010505" THEN DO:
          pcRecord = ";". /* Dropp og inn/utbetalings recorder skal bare ha 2 felt */
          IF SUBSTRING(tmpFilLinjer.Tekst,73,1) = "0" THEN DO: /* Innbetal */
              /* här tar vi hand om utbetalingstype */
              ENTRY(1,pcRecord,";") = IF tmpFilLinjer.Makulert THEN "1023" ELSE "23". 
              pcRecordPlus = ";INTYPE;" + SUBSTRING(tmpFilLinjer.Tekst,74,2) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN
                  pcRecordPlus = "".
          END.
          ELSE IF SUBSTRING(tmpFilLinjer.Tekst,73,1) = "1" THEN DO: /* Utbetaling */
              ENTRY(1,pcRecord,";") = IF tmpFilLinjer.Makulert THEN "1022" ELSE "22". 
              /* här tar vi hand om utbetalingstype */
              pcRecordPlus = ";UTTYPE;" + SUBSTRING(tmpFilLinjer.Tekst,74,2) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN
                  pcRecordPlus = "".
          END.
          ELSE IF SUBSTRING(tmpFilLinjer.Tekst,73,1) = "2" THEN  /* Dropp */
              ENTRY(1,pcRecord,";") = IF tmpFilLinjer.Makulert THEN "1052" ELSE "52". 
      END.
      WHEN "010506" THEN ENTRY(1,pcRecord,";") = IF tmpFilLinjer.Makulert THEN "1060" ELSE "60". /* Sjekk brukes for Drive-Off */
      WHEN "120502" THEN DO:
          ENTRY(1,pcRecord,";") = IF tmpFilLinjer.Makulert THEN "1059" ELSE "59". 
          /* 0-Debetkort, 1-Kreditkort */
          /*IF SUBSTRING(tmpFilLinjer.Tekst,122,1) = "1" THEN*/
              pcRecord = pcRecord + ";" + 
                         SUBSTRING(tmpFilLinjer.Tekst,122,1).
      END.
      WHEN "130501" THEN ENTRY(1,pcRecord,";") = IF tmpFilLinjer.Makulert THEN "1055" ELSE "55". 
      WHEN "140501" THEN ENTRY(1,pcRecord,";") = IF tmpFilLinjer.Makulert THEN "1055" ELSE "55". 
      WHEN "140502" THEN DO:
          ENTRY(1,pcRecord,";") = IF tmpFilLinjer.Makulert THEN "1059" ELSE "59". 
          /* 0-Debetkort, 1-Kreditkort */
          /*IF SUBSTRING(tmpFilLinjer.Tekst,122,1) = "1" THEN*/
              pcRecord = pcRecord + ";" + 
                         SUBSTRING(tmpFilLinjer.Tekst,122,1).
      END.
      WHEN "150501" THEN ENTRY(1,pcRecord,";") = IF tmpFilLinjer.Makulert THEN "1055" ELSE "55". 
  END CASE.
  
  /* Legger verdiene på plass i feltene. */
  IF ENTRY(1,pcRecord,";") = "55" AND plSubTotal = 0 AND plAmount < 0 THEN
      ENTRY(2,pcRecord,";") = trim(replace(string(plAmount,"->>>>>>>>9.99"),",",".")). /* Hantering vid utbetalning */
  ELSE
      ENTRY(2,pcRecord,";") = trim(replace(string(plSubTotal,"->>>>>>>>9.99"),",",".")). /* Subtotal */
  IF NUM-ENTRIES(pcRecord,";") > 2 THEN
  DO:
      ENTRY(3,pcRecord,";") = trim(replace(string(plAmount,"->>>>>>>>9.99"),",",".")). /* Innbetalt */
      ENTRY(4,pcRecord,";") = trim(replace(string(tmpFilLinjer.PengerTilbake,"->>>>>>>>9.99"),",",".")). /* Penger tilbake */
      ENTRY(5,pcRecord,";") = trim(replace(string(plRound,"->>>>>>>>9.99"),",",".")). /* Avrunding */
      ENTRY(6,pcRecord,";") = string(piCardtype).
  END.
  IF pcRecordPlus <> "" THEN
      pcRecord = pcRecord + pcRecordPlus.
  IF pbDropp = FALSE THEN 
      /* Trekker ned saldo. */
      plSubTotal  = plSubTotal - plPaid.
  ELSE
      /* Ved dropp skal saldo bare endre fortegn */
      plSubTotal = plSubTotal * -1.
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

