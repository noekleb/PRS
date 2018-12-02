&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        :  xinndteljournalpos.p
    Purpose     :  Innlesning av kvitteringsfil fra StorePoint kasse.

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
    Created     :  17/9-03
    Notes       :
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
DEF VAR cTekst          AS CHAR NO-UNDO.
DEF VAR iLinjeNr        AS INT  NO-UNDO.
DEF VAR iAntBonger      AS INT  NO-UNDO.

DEF VAR iButikkNr       AS INT  NO-UNDO.
DEF VAR iGruppeNr       AS INT  NO-UNDO.
DEF VAR iKasseNr        AS INT  NO-UNDO.
DEF VAR cInnKvittering  AS CHAR NO-UNDO.
DEF VAR iTotAntLinjer   AS INT  NO-UNDO.
DEF VAR cDatoListe      AS CHAR NO-UNDO.
DEF VAR cPOSKoder       AS CHAR NO-UNDO.
DEF VAR cTTIdKoder      AS CHAR NO-UNDO.
DEF VAR bLoggSaldoFeil  AS LOG  NO-UNDO.
DEF VAR cFilError       AS CHAR NO-UNDO.
DEF VAR iCl             AS INT  NO-UNDO.
DEF VAR cKontrolltabell AS CHARACTER  NO-UNDO. /* MottaksKontroll av vilken data vi skall testa mot */

DEF STREAM InnFil.

DEF TEMP-TABLE tmpFilLinjer NO-UNDO LIKE FilLinjer
    FIELD ButikkNr   AS INT
    FIELD PosId      AS INT
    FIELD Dato       AS DATE
    FIELD TillNum    AS INT
    FIELD BongLinje  AS INT
    FIELD PosBatchId AS INT
    FIELD Tid        AS INT 
    FIELD RecordType AS INT
    FIELD TTId       AS INT
    FIELD TBId       AS INT
    .
DEF TEMP-TABLE tmpKasseBatch NO-UNDO
    FIELD ButikkNr AS INT
    FIELD Dato     AS DATE
    FIELD KasseNr  AS INT /* POS Id       */
    FIELD BongNr   AS INT /* Till num     */
    FIELD BatchNr  AS INT /* POS Batch Id */
    FIELD RecType  AS CHAR 
    .

DEF TEMP-TABLE tmpBongHode  NO-UNDO LIKE BongHode.
DEF TEMP-TABLE tmpBongLinje NO-UNDO LIKE BongLinje
    FIELD cPOS AS CHAR.

DEF BUFFER btmpBongHode FOR tmpBongHode.

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
         HEIGHT             = 22.1
         WIDTH              = 68.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* ASSIGN                                                                                              */
/*   cPOSKoder      = "170100,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,170400,170403," +                        */
/*                    "xxx,170500,170699,xxx,xxx,xxx,xxx,xxx,xxx,xxx," +                               */
/*                    "xxx,xxx,290100,290508,xxx,xxx,xxx," +                                           */
/*                    "290501,290502,290503,290504,290505,290506," +                                   */
/*                    "160403,160100,160400,160699,160700,160800,270000,290111,160701,161100," +       */
/*                    "xxx,290500,xxx,xxx,290206,xxx,xxx,xxx,290507,xxx,xxx,290400,xxx,xxx,xxx,xxx," + */
/*                    "xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx," +                                 */
/*                    "xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx" +  */
/*                    "xxx,xxx,xxx,xxx,xxxx"                                                           */
/*                                                                                                     */
/*   cTTIdKoder     = "001,002,003,004,005,006,007,008,009,010,010," +                                 */
/*                    "011,012,012,013,014,015,016,017,018,019," +                                     */
/*                    "022,023,050,050,058,052,053," +                                                 */
/*                    "056,056,056,056,056,056," +                                                     */
/*                    "100,100,100,100,100,100,151,078,203,059," +                                     */
/*                    "054,055,056,057,058,059,060,061,062,063,064,065,066,067,068,069," +             */
/*                    "070,071,072,073,080,081,082,096,097,098,099," +                                 */
/*                    "100,101,102,103,104,105,106,107,108,120,121,122,123,130,131,140,141,142,143" +  */
/*                    "200,201,202,203,146"                                                            */
/*   .                                                                                                 */
ASSIGN           
  cPOSKoder      = "170100,xxx,xxx,xxx,xxx,xxx,170300,xxx,xxx,170400,170403," + 
                   "xxx,170500,170699,xxx,xxx,xxx,xxx,xxx,xxx,xxx," + 
                   "xxx,xxx,290100,290508,xxx,xxx,xxx," + 
                   "290501,290502,290503,290504,290505,290506," + 
/*                    "160403,160100,160300,160400,160699,xxx,160800,270000,290111,160701,161100," + */
                   "160403,160100,160300,160400,160699,161400,160800,290111,160701,161100," + 
                   "xxx,290500,xxx,xxx,290206,290207,290208,xxx,xxx,xxx,290507,xxx,xxx,290400,xxx,xxx,xxx,xxx," + 
                   "xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx," +
                   "xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx" +
                   "xxx,xxx,xxx,160700,160701,xxxx"

  cTTIdKoder     = "001,002,003,004,005,006,007,008,009,010,010," + 
                   "011,012,012,013,014,015,016,017,018,019," + 
                   "022,023,050,050,058,052,053," + 
                   "056,056,056,056,056,056," + 
                   "100,100,100,100,100,100,100,078,203,059," +
                   "054,055,056,057,058,058,058,059,060,061,062,063,064,065,066,067,068,069," + 
                   "070,071,072,073,080,081,082,096,097,098,099," +
                   "100,101,102,103,104,105,106,107,108,120,121,122,123,130,131,140,141,142,143" +
                   "200,201,202,203,203,146"
  .

/* Sentrallager */
{syspara.i 5 1 1 iCl INT}
/* Logge saldofeil i bong */
{syspara.i 1 1 110 cTekst}
IF cTekst = "1" THEN
    bLoggSaldoFeil = TRUE.
ELSE
    bLoggSaldoFeil = FALSE.
{syspara.i 1 1 25 cKontrolltabell}
IF NOT CAN-DO("1,2",cKontrolltabell) THEN
    ASSIGN cKontrolltabell = "1".

RUN NyFilLogg IN h_Parent (INPUT lFilId, STRING(TODAY) + " " + 
          STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
         " - xinndteljournalpos.p startet.").

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

RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
              STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
             " - Teller opp linjer." + 
             CHR(1) + "0").
RUN TellOppLinjer.

RUN InnLesFil.    /* El-Journal import.              */

RUN OpprettBong.  /* Oppretter bongene i tmp tabell. */

RUN LagreBong.    /* Skriver bongene til databasen.  */

DO TRANSACTION:
    FIND Filer EXCLUSIVE-LOCK WHERE
        Filer.FilId = lFilId NO-ERROR.
    IF AVAILABLE Filer THEN
        ASSIGN
        Filer.Oppdatert     = TRUE
        Filer.OppdatertDato = TODAY
        Filer.OppdatertKl   = TIME
        Filer.OppdatertAv   = USERID("SkoTex")
        .
END.
IF AVAILABLE Filer THEN
  FIND CURRENT filer NO-LOCK NO-ERROR.

RETURN "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BongHodeLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BongHodeLinje Procedure 
PROCEDURE BongHodeLinje :
/*------------------------------------------------------------------------------
  Purpose:     Fyller på informasjonen på bonghodelinjen.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
BONGHODELINJE:
DO:
    ASSIGN
        tmpBongLinje.Strekkode = string(tmpBongHode.KassererNr)
        tmpBongLinje.BongTekst = tmpBongHode.KassererNavn
        .
END. /* BONGHODELINJE */


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
  DEF VAR piTillNum    AS INT   NO-UNDO.
  DEF VAR piRecordType AS INT   NO-UNDO.
  DEF VAR pcBongNr     AS CHAR  NO-UNDO.
  DEF VAR piPosId      AS INT   NO-UNDO.
  DEF VAR piButikkNr   AS INT   NO-UNDO.
  DEF VAR piBatchNr    AS INT   NO-UNDO.
  DEF VAR d31DecFgAr AS DATE NO-UNDO.

  ASSIGN
      iantLinjer  = 0
      pcSokMaske  = ""
      pcOSokMaske = ""
      cDatoListe  = ""
      prRowId     = ?
      pcOLinje    = ""
      .

  /* Leser inn linjer i temp-file for sortering. */
  FOR EACH tmpFilLinjer: 
      DELETE tmpFilLinjer.
  END.
  FOR EACH tmpKasseBatch:
      DELETE tmpKasseBatch.
  END.
  FOR EACH tmpBongHode:
      DELETE tmpBongHode.
  END.
  FOR EACH tmpBongLinje:
      DELETE tmpBongLinje.
  END.

  STATUS DEFAULT "Bygger bonglinste.".

  /* Oppretter tmpKasseBatch.                                       */
  /* Dette er nødvendig for å sikre at alle bonghodene leses først. */
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  KASSEBATCH:
  REPEAT:
      /* Leser linje fra filen */
      IMPORT STREAM InnFil UNFORMATTED pcLinje.

      /* Skipper linjer som ikke er interessante */
      IF NOT CAN-DO("16",trim(trim(trim(ENTRY(4,pcLinje),"+"),"-"),"0")) THEN
          NEXT KASSEBATCH.
      ASSIGN
          piButikkNr = int(LEFT-TRIM(trim(trim(ENTRY(1,pcLinje),"+"),"-"),"0"))
          piPosId    = INT(LEFT-TRIM(trim(trim(ENTRY(5,pcLinje),"+"),"-"),"0"))
          piTillNum  = INT(LEFT-TRIM(trim(trim(ENTRY(6,pcLinje),"+"),"-"),"0"))
          piBatchNr  = INT(LEFT-TRIM(trim(trim(ENTRY(7,pcLinje),"+"),"-"),"0"))
/*           pcDato     = trim(trim(trim(ENTRY(3,pcLinje),"+"),"-"),"0") */
          pcDato     = ENTRY(3,pcLinje)
          pdDato     = date(int(ENTRY(1,pcDato,"/")),
                            int(ENTRY(2,pcDato,"/")),
                            int(ENTRY(3,pcDato,"/")))
          .
      IF NOT CAN-FIND(tmpKasseBatch WHERE
                      tmpKasseBatch.ButikkNr = piButikkNr AND
                      tmpKasseBatch.Dato     = pdDato     AND
                      tmpKasseBatch.BongNr   = piTillNum  AND
                      tmpKasseBatch.BatchNr  = piBatchNr) THEN
      DO:
          CREATE tmpKasseBatch.
          ASSIGN
              tmpKasseBatch.ButikkNr   = piButikkNr 
              tmpKasseBatch.Dato       = pdDato     
              tmpKasseBatch.KasseNr    = piPosId
              tmpKasseBatch.BongNr     = piTillNum
              tmpKasseBatch.BatchNr    = piBatchNr
              .
      END.

  END. /* KASSEBATCH */
  INPUT STREAM InnFil CLOSE.

  /* Åpner filen og leser neste runde. */
  piLinjeNr = 0.
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESINNBUFFER:
  REPEAT:
    ASSIGN
        piLinjeNr = piLinjeNr + 1
        pcLinje   = ""
        pcTekst   = ""
        .
    IF piLinjeNr MODULO 100 = 0 THEN
        STATUS DEFAULT "Leser linje " + STRING(piLinjeNr) + " av " +
        string(iTotAntLinjer) + ".".

    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.

    /* Setter RecordType, butikknr og dato*/
    ASSIGN
        piRecordType = INT(trim(trim(trim(ENTRY(4,pcLinje),"+"),"-"),"0"))
/*         pcDato       = trim(trim(trim(ENTRY(3,pcLinje),"+"),"-"),"0") */
        pcDato       = ENTRY(3,pcLinje)
        pdDato       = date(int(ENTRY(1,pcDato,"/")),
                            int(ENTRY(2,pcDato,"/")),
                            int(ENTRY(3,pcDato,"/")))
        piButikkNr   = int(LEFT-TRIM(trim(trim(ENTRY(1,pcLinje),"+"),"-"),"0"))
        .
    /* Skipper linjer som ikke er interessange */
/*     IF NOT CAN-DO("16,17,27,29",string(piRecordType)) THEN */
    IF NOT CAN-DO("16,17,29",string(piRecordType)) THEN
        NEXT LESINNBUFFER.
    IF pirecordtype = 16 AND ENTRY(14,pcLinje) = "+000000116" THEN
        NEXT LESINNBUFFER.
    IF pirecordtype = 17 AND ENTRY(11,pcLinje) = "+000000116" THEN
        NEXT LESINNBUFFER.

    /* Bongteksten i recordtype 17 kan inneholde space. Pos 183 - 202. */
    IF piRecordType = 17 AND INDEX(SUBSTRING(pcLinje,183,20),",") <> 0 THEN
    DO:
        pcTekst = SUBSTRING(pcLinje,183,20).
        DO WHILE INDEX(pcTekst,",") <> 0:
          OVERLAY (pcTekst, INDEX(pcTekst,",")) = ".".
        END.
        OVERLAY (pcLinje, 183, 20) = pcTekst.
    END.

    /* Kobler record til bong. */
    IF can-do("16,17",string(piRecordType)) THEN 
    DO: 
        ASSIGN
            piPosId    = INT(LEFT-TRIM(trim(trim(ENTRY(5,pcLinje),"+"),"-"),"0"))
            piTillNum  = INT(LEFT-TRIM(trim(trim(ENTRY(6,pcLinje),"+"),"-"),"0"))
            piBatchNr  = INT(LEFT-TRIM(trim(trim(ENTRY(7,pcLinje),"+"),"-"),"0"))
            .
        FIND FIRST tmpKasseBatch WHERE
             tmpKasseBatch.ButikkNr = piButikkNr AND
             tmpKasseBatch.Dato     = pdDato     AND
             tmpKasseBatch.BongNr   = piTillNum AND
             tmpKasseBatch.BatchNr  = piBatchNr NO-ERROR.
        /*
        IF AVAILABLE tmpKasseBatch THEN
            piPosId = tmpKasseBatch.KasseNr.
        */
    END.
    /* Kobler record til bong. */
/*     ELSE IF can-do("27",string(piRecordType)) THEN                                */
/*     DO:                                                                           */
/*         ASSIGN                                                                    */
/*             piTillNum  = INT(LEFT-TRIM(trim(trim(ENTRY(6,pcLinje),"+"),"-"),"0")) */
/*             piBatchNr  = INT(LEFT-TRIM(trim(trim(ENTRY(7,pcLinje),"+"),"-"),"0")) */
/*             piPosId    = piBatchNr /* 0*/                                         */
/*             .                                                                     */
/*         FIND FIRST tmpKasseBatch WHERE                                            */
/*              tmpKasseBatch.ButikkNr = piButikkNr AND                              */
/*              tmpKasseBatch.Dato     = pdDato     AND                              */
/*              tmpKasseBatch.BongNr   = piTillNum AND                               */
/*              tmpKasseBatch.BatchNr  = piBatchNr NO-ERROR.                         */
/*         /*                                                                        */
/*         IF AVAILABLE tmpKasseBatch THEN                                           */
/*             piPosId = tmpKasseBatch.KasseNr.                                      */
/*         */                                                                        */
/*     END.                                                                          */
    /* Kobler record til bong. */
    ELSE IF can-do("29",string(piRecordType)) THEN 
    DO: 
        ASSIGN
            piTillNum  = INT(LEFT-TRIM(trim(trim(ENTRY(6,pcLinje),"+"),"-"),"0"))
            piBatchNr  = INT(LEFT-TRIM(trim(trim(ENTRY(5,pcLinje),"+"),"-"),"0"))
            piPosId    = piBatchNr /* 0 */
            .
        FIND FIRST tmpKasseBatch WHERE
             tmpKasseBatch.ButikkNr = piButikkNr AND
             tmpKasseBatch.Dato     = pdDato     AND
             tmpKasseBatch.BongNr   = piTillNum AND
             tmpKasseBatch.BatchNr  = piBatchNr NO-ERROR.
        IF AVAILABLE tmpKasseBatch THEN
            piPosId = tmpKasseBatch.KasseNr.

    END.

    CREATE tmpFilLinjer.
    ASSIGN
        tmpFilLinjer.FilId      = lFilId
        tmpFilLinjer.LinjeNr    = piLinjeNr
        tmpFilLinjer.StorTekst  = pcLinje
        tmpFilLinjer.ButikkNr   = piButikkNr
        tmpFilLinjer.PosId      = piPosId
        tmpFilLinjer.Dato       = pdDato
        tmpFilLinjer.TillNum    = piTillNum
        tmpFilLinjer.BongLinje  = piLinjeNr
        tmpFilLinjer.RecordType = piRecordType
        .
  END. /* LESINNBUFFER */
  INPUT STREAM InnFil CLOSE.

/* OUTPUT TO VALUE("Log.txt"). */
/* FOR EACH TmpKasseBatch:     */
/*     EXPORT DELIMITER ";"    */
/*         tmpKasseBatch.      */
/* END.                        */
/* OUTPUT CLOSE.               */

  FIND LAST FilLinjer OF Filer NO-LOCK NO-ERROR.
  IF AVAILABLE FilLinjer THEN
      piLinjeNr = FilLinjer.LinjeNr + 1.
  ELSE
      piLinjeNr = 1.

  LESERLINJER:
  FOR EACH tmpFilLinjer /* WHERE tmpFilLinjer.RecordType = 16 OR tmpFilLinjer.RecordType = 17 */
      BREAK BY tmpFilLinjer.ButikkNr
            BY tmpFilLinjer.PosId
            BY tmpFilLinjer.Dato
            BY tmpFilLinjer.TillNum
            BY tmpFilLinjer.BongLinje:
    ASSIGN
        pcLinje = tmpFilLinjer.StorTekst
        .

/*     OUTPUT TO VALUE("Gurre.txt") APPEND. */
/*     EXPORT DELIMITER ";"                 */
/*         tmpFilLinjer.ButikkNr            */
/*         tmpFilLinjer.KasseNr             */
/*         tmpFilLinjer.Dato                */
/*         tmpFilLinjer.BongNr              */
/*         tmpFilLinjer.BongLinje           */
/*         .                                */
/*     OUTPUT CLOSE.                        */

    /* Blanke linjer skippes */
    IF pcLinje = "" THEN
        NEXT LESERLINJER.

/*     /* Setter søkemaske og brytpunkt. */                        */
/*     ASSIGN                                                      */
/*         pcSokMaske  = ENTRY(1,pcLinje,";") + ";" + /* Butikk */ */
/*                       ENTRY(2,pcLinje,";") + ";" + /* Kasse  */ */
/*                       ENTRY(3,pcLinje,";") + ";"   /* Dato   */ */
/*         pcOLinje    = pcLinje                                   */
/*         pcDato      = ENTRY(3,pcLinje,";")                      */
/*         .                                                       */

    /* Oppretter datasett */
    /* IF pcSokMaske <> pcOSokMaske THEN */
    IF FIRST-OF(tmpFilLinjer.Dato) THEN
        OPPRETTDATASETT:
        DO:
          /* Ferdigstempler den vi hold på med. */
          IF prRowId <> ? THEN
          DO:
              FIND DataSett EXCLUSIVE-LOCK WHERE
                  ROWID(Datasett) = prRowid.
              ASSIGN
                DataSett.AntallLinjer = DataSett.AntallLinjer + piAntISett
                DataSett.SettStatus   = (IF DataSett.SettNr > 1
                                          THEN 3 /* Ekstra  */
                                          ELSE 2 /* Mottatt */)
                DataSett.SettStatus   = (IF DataSett.ButikkNr <> 0
                                          THEN DataSett.SettStatus
                                          ELSE 9  /* Ikke koblet */)
                piAntISett            = 0
                prRowId               = ?
                pcOSokMaske           = pcSokMaske
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
/*           ELSE                              */
/*               IF pcOSokMaske = "" THEN      */
/*                   pcOSokMaske = pcSokMaske. */

/*           RUN koblekasse.p (INPUT  lFilId,        */
/*                             INPUT  h_Parent,      */
/*                             INPUT  iAntLinjer,    */
/*                             INPUT  Filer.FilType, */
/*                             INPUT  pcLinje,       */
/*                             OUTPUT iButikkNr,     */
/*                             OUTPUT iGruppeNr,     */
/*                             OUTPUT iKasseNr       */
/*                            ).                     */
          ASSIGN
              iButikkNr  = tmpFilLinjer.ButikkNr
              iGruppeNr  = 1
              iKasseNr   = tmpFilLinjer.PosId
              pdDato     = tmpFilLinjer.Dato
              .
          /* Kobling misslykkes. */
          IF (iButikkNr = 0 AND
              iGruppeNr = 0 AND
              iKasseNr  = 0) THEN
            pbKoblet  = FALSE.
          ELSE
            pbKoblet = TRUE.

          /* Finner neste DataSettId */
          /*
          FIND LAST DataSett NO-LOCK
              USE-INDEX DataSettId NO-ERROR.
          IF AVAILABLE DataSett THEN 
              plDataSettId = DataSett.DataSettId + 1.
          ELSE
              plDataSettId = 1.
          */

          /* Finner neste SettNr */
          FIND LAST Datasett NO-LOCK WHERE
              Datasett.ButikkNr = iButikkNr AND
              Datasett.GruppeNr = iGruppeNr AND
              Datasett.KasseNr  = iKasseNr  AND
              Datasett.Dato     = pdDato    AND
              DataSett.FilType  = 1 /* EL-Journal */
              USE-INDEX DataSett NO-ERROR.
          IF AVAILABLE DataSett THEN
              piSettNr = DataSett.SettNr + 1.
          ELSE 
              piSettNr = 1.

          RELEASE DataSett. /* Ny post skal skapes. */

          IF NOT AVAILABLE DataSett THEN
          DO:
            CREATE DataSett.
            ASSIGN
                /*DataSett.DataSettId = plDataSettId - Settes av trigger */
                DataSett.SettStatus = 8 /* Innlesning avbrutt    */
                DataSett.pfFlagg    = 4 /* Disse skal ikke over. */
                .
            IF NOT CAN-DO(cDatoListe,STRING(pdDato)) THEN
                ASSIGN
                  cDatoListe = cDatoListe + 
                               (IF cDatoListe = ""
                                  THEN ""
                                  ELSE ",") +
                                STRING(pdDato)
                                .
          END.
          ELSE  /* Bruker det vi fant. */
              FIND CURRENT DataSett EXCLUSIVE-LOCK.

          ASSIGN
            prRowId             = ROWID(DataSett)
            DataSett.ButikkNr   = iButikkNr 
            DataSett.GruppeNr   = iGruppeNr
            DataSett.KasseNr    = iKasseNr
            DataSett.Dato       = pdDato
            DataSett.SettNr     = piSettNr
            DataSett.Tid        = 0
            DataSett.FilId      = lFilId
            DataSett.FilType    = 1 /* EL-Journal */
            .
    END. /* OPPRETTDATASETT */

/*     /* Posterer linjen */                                    */
/*     CREATE FilLinjer.                                        */
/*     ASSIGN                                                   */
/*         FilLinjer.FilId      = lFilId                        */
/*         FilLinjer.LinjeNr    = piLinjeNr                     */
/*         FilLinjer.StorTekst  = pcLinje                       */
/*         FilLinjer.Datasett   = IF pcLinje MATCHES pcSokMaske */
/*                                  THEN TRUE                   */
/*                                  ELSE FALSE                  */
/*         FilLinjer.DataSettId = plDataSettId                  */
/*         iAntLinjer           = iAntLinjer + 1                */
/*         piLinjeNr            = piLinjeNr  + 1                */
/*         piAntISett           = piAntISett + 1                */
/*         .                                                    */

    /* Kobler fillinjen til datasettet. */
    ASSIGN
        tmpFilLinjer.DataSettId = DataSett.DataSettId
        .

    IF iAntLinjer MODULO 25 = 0 THEN
    DO:
      RUN Telleverk IN h_Telleverk 
          ("Fil: " + string(Filer.FilId) + " " + Filer.Filnavn +  
           " Leser " + STRING(iAntLinjer) +
           " av " + STRING(iTotAntLinjer) + ".") NO-ERROR.
    END.

  END. /* LESERLINJER */

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
      ASSIGN
          Filer.Innlest = TRUE
          Filer.InnlestDato = TODAY
          Filer.InnlestKl   = TIME
          Filer.InnlestAv   = USERID("SkoTex")
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
  END.
  IF AVAILABLE DataSett THEN
      FIND CURRENT DataSett NO-LOCK.
  IF AVAILABLE Filer THEN
      FIND CURRENT Filer    NO-LOCK.

  RUN Telleverk IN h_Parent ("Kontrollerer at alle datasett er mottatt. Vent litt... ") NO-ERROR.
  RUN sjekkdatasett.p (INPUT lFilId, INPUT cDatoListe).
  RUN Telleverk IN h_Parent (" ") NO-ERROR.

  STATUS DEFAULT " ".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KortSjekk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KortSjekk Procedure 
PROCEDURE KortSjekk :
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

&IF DEFINED(EXCLUDE-LagreBong) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreBong Procedure 
PROCEDURE LagreBong :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piAntall   AS INT NO-UNDO.
DEF VAR piAntISett AS INT NO-UNDO.

DEF BUFFER bBongHode FOR BongHode.

/* StrongScoop for bBongHode */
DO FOR bBongHode:
  LAGREBONG:
  FOR EACH tmpBongHode
      BREAK BY tmpBongHode.DataSettId
            BY tmpBongHode.B_Id TRANSACTION:
  
      ASSIGN
          cFilError  = ""
          cError     = ""
          piAntall   = piAntall + 1
          piAntISett = piAntISett + 1
          .
      IF piAntall MODULO 100 = 0 THEN
      STATUS DEFAULT "Lagrer bong " + STRING(piAntall) + ".".

      /* Dublettkontroll. */
      IF CAN-FIND(BongHode WHERE
                  BongHode.ButikkNr = tmpBongHode.ButikkNr  AND
                  BongHode.GruppeNr = tmpBongHode.GruppeNr  AND
                  BongHode.KasseNr  = tmpBongHode.KasseNr   AND
                  BongHode.Dato     = tmpBongHode.Dato      AND
                  BongHode.BongNr   = tmpBongHode.BongNr) THEN 
          NEXT LAGREBONG.
  
      FIND LAST bBongHode NO-LOCK USE-INDEX B_Id NO-ERROR.
  
      CREATE BongHode.
      BUFFER-COPY tmpBongHode 
          EXCEPT B_Id 
          TO BongHode
          ASSIGN
          /*
          BongHode.B_Id       = IF AVAILABLE bBongHode
                                  THEN bBongHode.B_Id + 1
                                  ELSE 1
          */
          BongHode.BongStatus = 2
          BongHode.pfFlagg    = 4 /* Disse skal ikke over. */
          .
      /* Skriver bongen i databasen */
      LINJEKOPIERING:
      FOR EACH tmpBongLinje where
          tmpBongLinje.B_Id = tmpBongHode.B_Id
          BREAK BY tmpBongLinje.B_Id
                BY tmpBongLinje.LinjeNr:

          CREATE BongLinje.
          BUFFER-COPY tmpBongLinje EXCEPT tmpBongLinje.B_Id
              TO BongLinje
              ASSIGN
              BongLinje.B_Id = BongHode.B_Id
              .
      END. /* LINJEKOPIERING */

      /* Gjør sumkontroll på bongen */
      RUN SjekkSaldo.
      /* Sjekker annen info på bongen */
      RUN SjekkBong.

      /* Setter status på bongen */
      IF AVAILABLE BongHode THEN
        ASSIGN
          BongHode.Konvertert    = TRUE
          BongHode.BongStatus    = IF BongHode.BongStatus < 5 /* Oppdatert */
                                   THEN 5
                                   ELSE BongHode.BongStatus
          .
        IF NUM-ENTRIES(BongHode.Logg,chr(10)) < 12 THEN
            ASSIGN
            BongHode.Logg     = BongHode.Logg + 
                                   (IF BongHode.Logg = ""
                                      THEN ""
                                      ELSE chr(10)) + 
                                   (IF cError = ""
                                     THEN "Ingen feil funnet ved konvertering."
                                    ELSE
                                     "** Feil som er funnet ved konvertering." + CHR(10) + 
                                     cError) /* Chr(10) separerte meldinger. */
          .

      /* Logger feil som er funnet på bongen i loggen. */
      IF cFilError <> "" THEN
      DO piLoop1 = 1 TO NUM-ENTRIES(cFilError,"|"):

        PUBLISH "NyFilLogg" (INPUT lFilId, INPUT cFilError).

      END.

      /* Ferdigstempler den vi hold på med.        */
      /* Ekstra her for å ta det siste datasettet. */
      IF LAST-OF(tmpBongHode.DataSettId) THEN
      DO:
          FIND DataSett EXCLUSIVE-LOCK WHERE
              DataSett.DataSettId = tmpBongHode.DataSettId.
          ASSIGN
            DataSett.AntallLinjer = DataSett.AntallLinjer + piAntISett
            DataSett.SettStatus   = (IF DataSett.SettNr > 1
                                      THEN 3 /* Ekstra  */
                                      ELSE 2 /* Mottatt */)
            DataSett.SettStatus   = (IF DataSett.ButikkNr <> 0
                                      THEN DataSett.SettStatus
                                      ELSE 9  /* Ikke koblet */)
            DataSett.Behandlet    = 3 /* Behandlet */
            piAntISett            = 0
            piAntISett            = 0
            .
          RELEASE DataSett.
      END. /* TRANSACTION */
  END. /* LAGREBONG */
END.
           
STATUS DEFAULT " ".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettBong) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettBong Procedure 
PROCEDURE OpprettBong :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR prRowId      AS ROWID NO-UNDO.
DEF VAR plDataSettId AS DEC   NO-UNDO.
DEF VAR piSettNr     AS INT   NO-UNDO.
DEF VAR piLoop       AS INT   NO-UNDO.
DEF VAR pcLinje      AS CHAR  NO-UNDO.
DEF VAR pbOkStatus   AS LOG.
DEF VAR piAntISett   AS INT   NO-UNDO.
DEF VAR pcTekst      AS CHAR  NO-UNDO.
DEF VAR pcPOS        AS CHAR  NO-UNDO.
DEF VAR piEntry      AS INT   NO-UNDO.
DEF VAR piInt        AS INT   NO-UNDO.

RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
              STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
             " - Starter å lagre bongene i databasen." + 
             CHR(1) + "0").

ASSIGN
    pbOkStatus = FALSE
    iAntBonger = 0
    .

/* Leser alle bonger i temp-file og lagrer dem i databasen. */
OPPRETT-BONG:
FOR EACH tmpFilLinjer
    BREAK BY tmpFilLinjer.DataSettId
          BY tmpFilLinjer.ButikkNr
          BY tmpFilLinjer.PosId
          BY tmpFilLinjer.Dato
          BY tmpFilLinjer.TillNum
          BY tmpFilLinjer.BongLinje:
    /* Feil på linjen ? */
    FEIL:
    DO:
        /* Logger hvis bongsummen ikke er lik 0. */
        IF tmpFilLinjer.ButikkNr = 0 OR
           tmpFilLinjer.PosId    = 0 OR
           tmpFilLinjer.TillNum  = 0 THEN
        DO:
            ASSIGN
            cFilError = cFilError + 
                (IF cFilError = ""
                   THEN ""
                   ELSE "|") + 
                " - Feil på linje - " + 
                STRING(tmpFilLinjer.LinjeNr) + " " + 
                "But/Kasse/Bong: " + 
                string(tmpFilLinjer.ButikkNr) + "/" +
                string(tmpFilLinjer.PosId) + "/" +   
                string(tmpFilLinjer.TillNum) + "/" + 
                "." + 
                CHR(1) + "3"
            .
            PUBLISH "NyFilLogg" (INPUT lFilId, INPUT cFilError).
            cFilError = "".
        END.
    END. /* FEIL */

    /* Henter datasettet */
    IF FIRST-OF(tmpFilLinjer.DataSettId) THEN
    DO:
        FIND DataSett NO-LOCK WHERE
            DataSett.DataSettId = tmpFilLinjer.DataSettId NO-ERROR.
        ASSIGN
            iAntBonger = 0
            .
    END.

    ASSIGN
        piInt = piInt + 1
        .
    IF piInt MODULO 100 = 0 THEN
    STATUS DEFAULT "Oppretter bong. Linje " + STRING(piInt) + ".".

    /* Konverterer transaksjonskoden */
    KONVTRANSKODE:
    DO:
        ASSIGN
          pcPOS              = IF tmpFilLinjer.RecordType = 16 /* Bonghode */
                                 THEN string(tmpFillinjer.RecordType,"99") + 
                                      string(int(trim(trim(trim(ENTRY(12,tmpFilLinjer.StorTekst),"+"),"-"),"0")),"99") +  /* Status code    */
                                      string(int(trim(trim(trim(ENTRY(14,tmpFilLinjer.StorTekst),"+"),"-"),"0")),"99")   /* Substatus code */
                               ELSE IF tmpFilLinjer.RecordType = 17 /* Varelinje */
                                 THEN string(tmpFillinjer.RecordType,"99") + 
                                      string(int(trim(trim(trim(ENTRY(9,tmpFilLinjer.StorTekst),"+"),"-"),"0")),"99") +  /* Status code    */
                                      string(int(trim(trim(trim(ENTRY(11,tmpFilLinjer.StorTekst),"+"),"-"),"0")),"99")   /* Substatus code */
                               ELSE IF tmpFilLinjer.RecordType = 27 /* Kupong */ 
                                 THEN string(tmpFillinjer.RecordType,"99") + "0000"
                               ELSE IF tmpFilLinjer.RecordType = 29 /* Betalingstransaksjon */
                                 THEN string(tmpFillinjer.RecordType,"99") + 
                                      string(int(trim(trim(trim(ENTRY(8,tmpFilLinjer.StorTekst),"+"),"-"),"0")),"99") + 
                                      string(int(trim(trim(trim(ENTRY(26,tmpFilLinjer.StorTekst),"+"),"-"),"0")),"99")
                               ELSE "000000"
          piEntry            = LOOKUP(pcPOS,cPOSKoder)
          tmpFilLinjer.TTId  = 0
          tmpFilLinjer.TbId  = 1
          .
        IF piEntry = 0 THEN
            ASSIGN
            tmpFilLinjer.TTId = 0
            tmpFilLinjer.TbId = 1
            
            .
        ELSE
            ASSIGN
              tmpFilLinjer.TTId = int(ENTRY(piEntry,cTTIdKoder))
              tmpFilLinjer.TbId = 1
            NO-ERROR.
    END. /* KONVTRANSKODE */

    /* Setter bonglinjenummer */
    ASSIGN
        iLinjeNr = iLinjeNr + 1
        .
    /* Opprettelse av bonghode. */

    IF tmpFilLinjer.RecordType = 16 /*tmpFilLinjer.TTId = 100*/ THEN
        RUN Trans100. /* BongHode */

    /* Oppretter/henter bonglinje.          */
    /* Skaper bonglinjen. Men ikke noe mer. */
    RUN OpprettHentBongLinje.

    IF AVAILABLE tmpBongLinje THEN
    DO:
        /* Setter på ekstern transkode. */
        ASSIGN
            tmpBongLinje.cPOS = pcPOS
            .

        /* Fyller på informasjon på BongHodeLinjen */
        IF tmpFilLinjer.TTId = 100 THEN
            RUN BongHodeLinje.

        /* Varesalg, retur og makulering. */
        IF can-do("001,010,007,012",string(tmpFilLinjer.TTId,"999")) THEN
            RUN Trans001.

        /* Betalingslinjer */
        IF can-do("050,055,056,058,062,065,078",string(tmpFilLinjer.TTId,"999")) THEN
            RUN Trans050.

        /* Dropp */
        IF can-do("059",string(tmpFilLinjer.TTId,"999")) THEN
            RUN Trans059.
    END.
    
    /* Oppdaterer datasettet */
    IF LAST-OF(tmpFilLinjer.DataSettId) THEN
    DO TRANSACTION:
        FIND CURRENT Datasett EXCLUSIVE-LOCK.
        ASSIGN
            DataSett.AntallLinjer = iAntBonger
            .
        FIND CURRENT Datasett NO-LOCK.
    END.
END. /* OPPRETT-BONG */

RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
              STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
             " - Ferdig å lagre bongene i databasen." + 
             CHR(1) + "0").

RUN Telleverk IN h_Parent (" ") NO-ERROR.
STATUS DEFAULT " ".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettHentBongLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettHentBongLinje Procedure 
PROCEDURE OpprettHentBongLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT AVAILABLE tmpBongHode THEN
    RETURN.

FIND tmpBongLinje WHERE
     tmpBongLinje.ButikkNr = tmpBongHode.ButikkNr AND
     tmpBongLinje.GruppeNr = tmpBongHode.GruppeNr AND
     tmpBongLinje.KasseNr  = tmpBongHode.KasseNr  AND
     tmpBongLinje.Dato     = tmpBongHode.Dato     AND
     tmpBongLinje.BongNr   = tmpBongHode.BongNr   AND
     tmpBongLinje.LinjeNr  = iLinjeNr NO-ERROR.
IF NOT AVAILABLE tmpBongLinje THEN
LINJEOPPSTANDELSE:
DO:
  CREATE tmpBongLinje.
  ASSIGN
      tmpBongLinje.B_Id         = tmpBongHode.B_Id
      tmpBongLinje.ButikkNr     = tmpBongHode.ButikkNr 
      tmpBongLinje.GruppeNr     = tmpBongHode.GruppeNr 
      tmpBongLinje.KasseNr      = tmpBongHode.KasseNr  
      tmpBongLinje.Dato         = tmpBongHode.Dato     
      tmpBongLinje.BongNr       = tmpBongHode.BongNr   
      tmpBongLinje.LinjeNr      = iLinjeNr
      tmpBongLinje.TransDato    = tmpBongHode.Dato
      tmpBongLinje.TransTid     = tmpBongHode.Tid
/*       tmpBongLinje.Originaldata = tmpFilLinjer.StorTekst */
      tmpBongLinje.TTId         = tmpFillinjer.TTId
      tmpBongLinje.TBId         = tmpFillinjer.TBId
      .

END. /* LINJEOPPSTANDELSE */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SjekkBong) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkBong Procedure 
PROCEDURE SjekkBong :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Generell kontroll av bongen. */
  BONGLINJE:
  FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
      BongLinje.B_Id = BongHode.B_Id AND
      BongLinje.Makulert = FALSE:

    /* Kun varesalgstranser sjekkes */
    IF NOT CAN-DO("001,002,003,004,005,006,007,008,009,010,011",STRING(BongLinje.TTID,"999")) THEN
        NEXT BONGLINJE.

    /* Gyldig transaksjonskode. */
    IF BongLinje.TTid = 0 THEN
    DO:
        ASSIGN
        cFilError = cFilError + 
            (IF cFilError = ""
               THEN ""
               ELSE "|") + 
            " - Ukjent transaksjonstype." + 
            " (But/Kas/Dato/BongNr: " + 
            STRING(BongHode.ButikkNr) + "/" + 
            STRING(BongHode.KasseNr) + "/" + 
            STRING(BongHode.Dato) + "/" + 
            STRING(BongHode.BongNr) +  ")." + CHR(1) + "3"
        .
    END.

    FIND Strekkode NO-LOCK WHERE
        Strekkode.Kode = BongLinje.Strekkode NO-ERROR.

    /* Blank strekkode eller ukjent strekkode */
    IF BongLinje.Strekkode = "" OR NOT AVAILABLE Strekkode THEN
    DO:
        IF Bonglinje.Makulert = FALSE THEN
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
    END.

    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.

    /* Kontrollerer varegruppen */
    IF AVAILABLE ArtBas THEN
    DO:
        IF BongLinje.VareGr <> ArtBas.Vg AND BongLinje.VareGr > 0 THEN
        DO:
            IF Bonglinje.Makulert = FALSE THEN
            ASSIGN  
              cError = cError + 
                       (IF cError = ""
                          THEN ""
                          ELSE chr(10)) + 
                       "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                       " Varegruppe er byttet på artikkel (ArtikkelNr: " + STRING(Strekkode.ArtikkelNr) + ") " +
                       " Fra VG: " + STRING(BongLinje.VareGr) + " til VG: " + STRING(ArtBas.Vg) +
                       "."
              cFilError = cFilError + 
                       (IF cFilError = ""
                          THEN ""
                          ELSE "|") + 
                       " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                       " Varegruppe er byttet på artikkel (ArtikkelNr: " + STRING(Strekkode.ArtikkelNr) + ") " +
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
        IF Bonglinje.Makulert = FALSE THEN
        ASSIGN  
          cError = cError + 
                   (IF cError = ""
                      THEN ""
                      ELSE chr(10)) + 
                   "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                   " Ukjent artikkel på transaksjonen (ArtikkelNr: " + STRING(Strekkode.ArtikkelNr) + ")" + 
                   "."
          cFilError = cFilError + 
                   (IF cFilError = ""
                      THEN ""
                      ELSE "|") + 
                   " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                   " Ukjent artikkel på transaksjonen (ArtikkelNr: " + STRING(Strekkode.ArtikkelNr) + ")" + 
                   "." + CHR(1) + "2"
          BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
          BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
          .
    END. /* UKJENT-ARTIKKEL */

    ASSIGN
        BongLinje.ArtikkelNr = (IF AVAILABLE ArtBas
                                  THEN string(ArtBas.ArtikkelNr,">>>>>>>>>>>>9")
                                  ELSE string(Strekkode.ArtikkelNr,">>>>>>>>>>>>9"))
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
        IF Bonglinje.Makulert = FALSE THEN
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
        IF NOT AVAILABLE HuvGr AND 
           Bonglinje.Makulert = FALSE THEN
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
    /* vi gör detta istället */
    IF Strekkode.StrKode = 0 THEN
        ASSIGN BongLinje.Storrelse = " 1".

/*     IF Strekkode.StrKode <> 0 THEN                                                                                   */
/*     DO:                                                                                                              */
/*         FIND StrKonv NO-LOCK WHERE                                                                                   */
/*             StrKonv.StrKode = StrekKode.StrKode NO-ERROR.                                                            */
/*         IF AVAILABLE StrKonv THEN                                                                                    */
/*             BongLinje.Storrelse = StrKonv.Storl.                                                                     */
/*         ELSE DO:                                                                                                     */
/*             IF Bonglinje.Makulert = FALSE THEN                                                                       */
/*             ASSIGN                                                                                                   */
/*               cError = cError +                                                                                      */
/*                        (IF cError = ""                                                                               */
/*                           THEN ""                                                                                    */
/*                           ELSE chr(10)) +                                                                            */
/*                        "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +  */
/*                        " Ukjent størrelse på bonglinjen: " + string(BongLinje.Storrelse) +                           */
/*                        "."                                                                                           */
/*               cFilError = cFilError +                                                                                */
/*                        (IF cFilError = ""                                                                            */
/*                           THEN ""                                                                                    */
/*                           ELSE "|") +                                                                                */
/*                        " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +  */
/*                 " Ukjent størrelse på bonglinjen: " + string(BongLinje.Storrelse) +                                  */
/*                        "." + CHR(1) + "2"                                                                            */
/*               BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering                          */
/*               BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering                              */
/*               BongLinje.Storrelse = ""                                                                               */
/*                                                                                                                      */
/*               .                                                                                                      */
/*         END.                                                                                                         */
/*     END.                                                                                                             */
/*     /* Endrer til størrelse " 1". */                                                                                 */
/*     ELSE DO:                                                                                                         */
/*         ASSIGN                                                                                                       */
/*             BongLinje.Storrelse = " 1"                                                                               */
/*             .                                                                                                        */
/*     END.                                                                                                             */

    /* Gyldig kasserernr */
    /* Meningslöst */
/*     IF NOT CAN-FIND(Forsalj WHERE                                                     */
/*                     Forsalj.ForsNr = int(BongHode.KassererNr)) THEN                   */
/*       ASSIGN                                                                          */
/*         cError = cError +                                                             */
/*                  (IF cError = ""                                                      */
/*                     THEN ""                                                           */
/*                     ELSE chr(10)) +                                                   */
/*                  "** BongNr: " + string(BongHode.BongNr) + ": " +                     */
/*                  " Ukjent kasserer på transaksjonen " + STRING(BongHode.KassererNr) + */
/*                  "."                                                                  */
/*         cFilError = cFilError +                                                       */
/*                  (IF cFilError = ""                                                   */
/*                     THEN ""                                                           */
/*                     ELSE "|") +                                                       */
/*                  " - BongNr: " + string(BongHode.BongNr) + ": " +                     */
/*                  " Ukjent kasserer på transaksjonen " + STRING(BongHode.KassererNr) + */
/*                  "." + CHR(1) + "2"                                                   */
/*         BongHode.KassererNavn = "*Ukjent*"                                            */
/*         BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering */
/*         .                                                                             */
/*     /* Setter kassererinfo. */                                                        */
/*     ELSE DO:                                                                          */
/*         FIND Forsalj NO-LOCK WHERE                                                    */
/*             Forsalj.ForsNr = int(BongHode.KassererNr).                                */
/*         ASSIGN                                                                        */
/*             BongHode.KassererNavn  = Forsalj.FoNamn                                   */
/*             .                                                                         */
/*     END.                                                                              */
  END. /* BONGSJEKK */

  /* KortValidering */
/* Vi har ingen medlem eller kunde */
/*   IF BongHode.MedlemsKort <> "" then        */
/*       RUN KortSjekk (BongHode.MedlemsKort). */
/*   IF BongHode.KundeKort <> "" THEN          */
/*       RUN KortSjekk (BongHode.KundeKort).   */

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
  DEF VAR plVareSum     AS DEC   NO-UNDO.
  DEF VAR plBetSum      AS DEC   NO-UNDO.
  DEF VAR plInnbetaling AS DEC   NO-UNDO.

  /* Sjekker at bongens sum er = 0. */
  ASSIGN plVareSum = 0
         plBetSum  = 0.
  /* NB: Overføringer kontrolleres ikke. */
  FOR EACH BongLinje NO-LOCK WHERE
      BongLinje.B_Id = BongHode.B_Id AND
      BongLinje.Makulert = FALSE:

    /* Sum varetranser */
    /* TN 20/11-02 Deponering skal behandles som salg av vare. */
    /* Dropp skal summeres som vare */
    /* GAvekort ut skal behandles som vare i sumering */
    IF CAN-DO("001,002,003,004,005,006,007,008,009,010,011,012,072,134",string(BongLinje.TTId,"999")) THEN
    DO:
        IF can-do("134",string(BongLinje.TTID,"999")) THEN /* Solgt GAVEKORT */
          ASSIGN
          plVareSum = plVareSum + (BongLinje.LinjeSum)
          .
        ELSE
        ASSIGN
          plVareSum = plVareSum + 
                      ((BongLinje.LinjeSum - 
                        BongLinje.LinjeRab -
                        BongLinje.SubTotalRab) * (IF BongLinje.Antall < 0
                                                                      THEN -1 
                                                                      ELSE 1))
          .
    END.

    /* Betalingstransaksjoner.                                */
    /* Subtotalrabatt er trukket fra fra før og skal ikke tas */
    /* med her.                                               */
    /* Dropp kontrolleres ikke. Kontant er der = 0.           */
    ELSE IF CAN-DO("050,051,052,053,054,055,056,057,058,061,062,064,065,066,067,069,070,071,073,078,079,089",string(BongLinje.TTId,"999")) THEN
    DO:
        /* Betaling av deponering, og veksel */
        /* Innbetaling på konto.            */
        IF CAN-DO("061,073,089",string(BongLinje.TTId,"999")) THEN
            ASSIGN
            plBetSum      = plBetSum - BongLinje.LinjeSum
            /* Spesiell håndtering av innbetalinger. */
            plInnbetaling = plInnbetaling +
                            (IF CAN-DO("061",STRING(BongLinje.TTId,"999"))
                               THEN BongLinje.LinjeSum
                               ELSE 0)
            .
        /* Betalingstranser. */
        ELSE
            ASSIGN
            plBetSum = plBetSum + BongLinje.LinjeSum
            .
        
    END.
  END.
  /* Logger hvis bongsummen ikke er lik 0. */
  IF (plVareSum - plBetSum <> 0) AND
     bLoggSaldoFeil = TRUE THEN
  DO:
      ASSIGN
      cError    = cError +
          (IF cFilError = ""
             THEN ""
             ELSE chr(10)) + 
          " - Bongen's sum <> 0" + 
          " Diff: " + STRING(plVareSum,"->>>,>>>,>>9.99") + " - " + 
          STRING(plBetSum,"->>>,>>>,>>9.99") + " = " + 
          STRING(plVareSum - plBetSum,"->>>,>>>,>>9.99") + "."
      cFilError = cFilError + 
          (IF cFilError = ""
             THEN ""
             ELSE "|") + 
          " - Bongen's sum <> 0" + 
          " Diff: " + STRING(plVareSum,"->>>,>>>,>>9.99") + " - " + 
          STRING(plBetSum,"->>>,>>>,>>9.99") + " = " + 
          STRING(plVareSum - plBetSum,"->>>,>>>,>>9.99") + "." + 
          " (But/Kas/Dato/BongNr: " + 
          STRING(BongHode.ButikkNr) + "/" + 
          STRING(BongHode.KasseNr) + "/" + 
          STRING(BongHode.Dato) + "/" + 
          STRING(BongHode.BongNr) +  ")." + CHR(1) + "3"
      .
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

&IF DEFINED(EXCLUDE-Trans001) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Trans001 Procedure 
PROCEDURE Trans001 PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     Ordinært varesalg.
  Parameters:  <none>
  Notes:       På returer må beløpsfeltene korrigeres for negative verdier.
               Beløpene er alltid positive. Det er kun antall som har fortegn.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iKampanjeId AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dKampanjeRab AS DECIMAL    NO-UNDO.
BONGLINJEINFO:
DO:
    ASSIGN iKampanjeId = INT(ENTRY(25,tmpFilLinjer.StorTekst)) NO-ERROR. /* 1 = viktvara */
    IF ERROR-STATUS:ERROR THEN
        ASSIGN iKampanjeId = 0.
    ASSIGN        
        tmpBongLinje.Strekkode      = trim(ENTRY(16,tmpFilLinjer.StorTekst))
        tmpBongLinje.BongTekst      = ENTRY(17,tmpFilLinjer.StorTekst)
        tmpBongLinje.PrisPrSalgsenhet = DECI(ENTRY(18,tmpFilLinjer.StorTekst)) / 1000
        tmpBongLinje.Antall         = (DECI(ENTRY(19,tmpFilLinjer.StorTekst)) / 1000) * (DECI(ENTRY(21,tmpFilLinjer.StorTekst)) / 1000)
        tmpBongLinje.LinjeSum       = ABS(
                                          round((DEC(ENTRY(20,tmpFilLinjer.StorTekst)) / 1000 + 
                                          DEC(ENTRY(23,tmpFilLinjer.StorTekst)) / 1000),2) + 
                                          round(DEC(ENTRY(22,tmpFilLinjer.StorTekst)) / 1000,2))
        tmpBongLinje.BongPris       = tmpBongLinje.LinjeSum
        tmpBongLinje.LinjeRab       = ABS(round(DEC(ENTRY(22,tmpFilLinjer.StorTekst)) / 1000,2))
        dKampanjeRab                = IF iKampanjeId > 0 THEN ABS(round(DEC(ENTRY(24,tmpFilLinjer.StorTekst)) / 1000,2)) - 
                                                ABS(round(DEC(ENTRY(23,tmpFilLinjer.StorTekst)) / 1000,2)) - tmpBongLinje.LinjeRab ELSE 0
        tmpBongLinje.LinjeRab       = tmpBongLinje.LinjeRab + dKampanjeRab
        tmpBongLinje.LinjeSum       = tmpBongLinje.LinjeSum + dKampanjeRab
        tmpBongLinje.Kampanjeid     = iKampanjeId
        tmpBongLinje.MvaKr          = ABS(round(DEC(ENTRY(23,tmpFilLinjer.StorTekst)) / 1000,2))
        /* Mva% overstyres av varegruppen lenger ned. */
        tmpBongLinje.Mva%           = ROUND((tmpBongLinje.MvaKr * 100) / (tmpBongLinje.LinjeSum - tmpBongLinje.LinjeRab),2)
        tmpBongLinje.Mva%           = IF tmpBongLinje.Mva% = ? THEN 0 ELSE tmpBongLinje.Mva%
        tmpBongLinje.VVareKost      = ABS(round(DEC(ENTRY(36,tmpFilLinjer.StorTekst)) / 1000,2))
        tmpBongLinje.Storrelse      = " 1"
        tmpBongHode.flRabatt        = IF tmpBongLinje.LinjeRab <> 0 THEN TRUE ELSE tmpBongHode.flRabatt
        .
    /* Konverterer PLU koder. */
    IF DEC(tmpBongLinje.Strekkode) <= 99999 THEN
        tmpBongLinje.Strekkode = LEFT-TRIM(tmpBongLinje.Strekkode,"0").

/*     IF CAN-DO("28671,28765,28808",STRING(tmpBongLinje.bongnr)) THEN DO:                                             */
/*         OUTPUT TO "C:\tmp\16125.txt" APPEND.                                                                        */
/*         PUT UNFORMATTED tmpBongLinje.Antall " - " ENTRY(19,tmpFilLinjer.StorTekst) "-" tmpFilLinjer.Stortekst SKIP. */
/*         OUTPUT CLOSE.                                                                                               */
/*     END.                                                                                                            */
    /* Rader som har inkurans */
    IF tmpBongLinje.TTId = 7 THEN
        ASSIGN
        tmpBongLinje.Antall   = 0
        .
    /* Stempler makulerte rader. */
    IF tmpBongLinje.TTId = 12 THEN
        ASSIGN
        tmpBongLinje.Makulert = TRUE
        tmpBongLinje.Antall   = 0
        .
    /* Overstyrer momsen. */
    /*
    FIND VarGr NO-LOCK WHERE
        Vargr.Vg = BongLinje.VareGr NO-ERROR.
    IF AVAILABLE VarGr THEN
        FIND Moms OF VarGr NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Moms then
    */
        FIND FIRST Moms NO-LOCK WHERE
        Moms.MomsProc >= tmpBongLinje.Mva% NO-ERROR.
    IF AVAILABLE Moms THEN
        ASSIGN
        /*tmpBongLinje.Mva%  = Moms.MomsProc*/
        tmpBongLinje.MvaGr = Moms.MomsKod
        .

    FIND Strekkode NO-LOCK WHERE
        Strekkode.Kode = tmpBongLinje.Strekkode NO-ERROR. /* tmp fanns inte */
    IF AVAILABLE Strekkode THEN
        FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
    IF AVAILABLE ArtBas THEN
    DO:
        FIND VarGr  OF ArtBas NO-LOCK NO-ERROR.
        FIND HuvGr  OF ArtBas NO-LOCK NO-ERROR.
        FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
        ASSIGN
            tmpBongLinje.ArtikkelNr = STRING(ArtBas.ArtikkelNr)
            tmpBongLinje.VareGr     = ArtBas.Vg
            tmpBongLinje.VareGruppeNavn = IF AVAILABLE VarGr
                                            THEN VarGr.VgBeskr
                                            ELSE ""
            tmpBongLinje.HovedGrBeskrivelse = IF AVAILABLE HuvGr
                                                THEN HuvGr.HgBeskr
                                                ELSE ""
            tmpBongLinje.HovedGr    = ArtBas.Hg
            tmpBongLinje.Storrelse  = " 1"
            tmpBongLinje.LevNr      = ArtBas.LevNr
            tmpBongLinje.LevNavn    = IF AVAILABLE LevBas
                                        THEN LevBas.LevNamn
                                        ELSE ""
            .
    END.

END. /* BONGLINJEINFO */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Trans050) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Trans050 Procedure 
PROCEDURE Trans050 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

BONGLINJEBETALING:
DO:
    ASSIGN
        tmpBongLinje.LinjeSum       = round((DEC(ENTRY(15,tmpFilLinjer.StorTekst)) / 1000) + (DEC(ENTRY(23,tmpFilLinjer.StorTekst)) / 1000),2)
        tmpBongLinje.BongPris       = tmpBongLinje.LinjeSum
        tmpBongLinje.Antall         = 0
        tmpBongHode.flBetalingskort = IF tmpBongLinje.TTID = 51 THEN TRUE ELSE tmpBongHode.flBetalingskort
        tmpBongHode.flBetalingskort = IF tmpBongLinje.TTID = 58 THEN TRUE ELSE tmpBongHode.flBetalingskort
        tmpBongHode.flBetalingskort = IF tmpBongLinje.TTID = 52 THEN TRUE ELSE tmpBongHode.flBetalingskort
        tmpBongHode.flBankkort      = IF tmpBongLinje.TTID = 58 THEN TRUE ELSE tmpBongHode.flBankkort     
        tmpBongHode.flKreditkort    = IF tmpBongLinje.TTID = 52 THEN TRUE ELSE tmpBongHode.flKreditkort   
        tmpBongHode.flGavekort      = IF tmpBongLinje.TTID = 53 THEN TRUE ELSE tmpBongHode.flGavekort     
        tmpBongHode.flSjekk         = IF tmpBongLinje.TTID = 54 THEN TRUE ELSE tmpBongHode.flSjekk        
        tmpBongHode.flRekvisisasjon = IF tmpBongLinje.TTID = 55 THEN TRUE ELSE tmpBongHode.flRekvisisasjon  
        tmpBongHode.flKupong1       = IF tmpBongLinje.TTID = 56 THEN TRUE ELSE tmpBongHode.flKupong1
        tmpBongLinje.TTID           = IF tmpBongLinje.TTID = 50 AND tmpBongLinje.LinjeSu < 0 THEN 70 ELSE tmpBongLinje.TTID
        /*
        tmpBongLinje.Strekkode      = trim(ENTRY(16,tmpFilLinjer.StorTekst))
        tmpBongLinje.BongTekst      = ENTRY(17,tmpFilLinjer.StorTekst)
        tmpBongLinje.LinjeRab       = round(DEC(ENTRY(22,tmpFilLinjer.StorTekst)) / 1000,2)
        tmpBongLinje.MvaKr          = round(DEC(ENTRY(23,tmpFilLinjer.StorTekst)) / 1000,2)
        tmpBongLinje.Mva%           = ROUND((tmpBongLinje.MvaKr * 100) / tmpBongLinje.LinjeSum,0)
        tmpBongLinje.Storrelse      = " 1"
        */
        .
    /* Skrever tekst på VINSTER som benyttes til betaling. */
    CASE int(tmpBongLinje.cPOS):
        WHEN 290501 THEN tmpBongLinje.BongTekst = "Kuponginnlösen".
        WHEN 290502 THEN tmpBongLinje.BongTekst = "Rikskupong".
        WHEN 290503 THEN tmpBongLinje.BongTekst = "Lottvinst".
        WHEN 290504 THEN tmpBongLinje.BongTekst = "Tipsvinst".
        WHEN 290505 THEN tmpBongLinje.BongTekst = "Bingolotto".
        WHEN 290506 THEN tmpBongLinje.BongTekst = "ATG".
    END CASE.

END. /* BONGLINJEBETALING */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Trans055) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Trans055 Procedure 
PROCEDURE Trans055 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

BONGLINJEBETALING:
DO:
    ASSIGN
        tmpBongLinje.LinjeSum       = round((DEC(ENTRY(15,tmpFilLinjer.StorTekst)) / 1000) + (DEC(ENTRY(23,tmpFilLinjer.StorTekst)) / 1000),2)
        tmpBongLinje.BongPris       = tmpBongLinje.LinjeSum
        tmpBongLinje.Antall         = 0
        .
END. /* BONGLINJEBETALING */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Trans059) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Trans059 Procedure 
PROCEDURE Trans059 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

BONGLINJEBETALING:
DO:
    ASSIGN
/*         tmpBongLinje.LinjeSum       = round((DEC(ENTRY(23,tmpFilLinjer.StorTekst)) / 1000) + (DEC(ENTRY(23,tmpFilLinjer.StorTekst)) / 1000),2) */
        tmpBongLinje.LinjeSum       = ABS(round((DEC(ENTRY(23,tmpFilLinjer.StorTekst)) / 1000),2))
        tmpBongLinje.BongPris       = tmpBongLinje.LinjeSum
        tmpBongLinje.Antall         = 0
        tmpBongLinje.BongTekst      = "Dropp"
        /*
        tmpBongLinje.Strekkode      = trim(ENTRY(16,tmpFilLinjer.StorTekst))
        tmpBongLinje.BongTekst      = ENTRY(17,tmpFilLinjer.StorTekst)
        tmpBongLinje.LinjeRab       = round(DEC(ENTRY(22,tmpFilLinjer.StorTekst)) / 1000,2)
        tmpBongLinje.MvaKr          = round(DEC(ENTRY(23,tmpFilLinjer.StorTekst)) / 1000,2)
        tmpBongLinje.Mva%           = ROUND((tmpBongLinje.MvaKr * 100) / tmpBongLinje.LinjeSum,0)
        tmpBongLinje.Storrelse      = " 1"
        */
        .
END. /* BONGLINJEBETALING */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Trans065) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Trans065 Procedure 
PROCEDURE Trans065 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

BONGLINJEBETALING:
DO:
    ASSIGN
        tmpBongLinje.LinjeSum       = round((DEC(ENTRY(15,tmpFilLinjer.StorTekst)) / 1000) + (DEC(ENTRY(23,tmpFilLinjer.StorTekst)) / 1000),2)
        tmpBongLinje.BongPris       = tmpBongLinje.LinjeSum
        tmpBongLinje.Antall         = 0
        .
END. /* BONGLINJEBETALING */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Trans100) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Trans100 Procedure 
PROCEDURE Trans100 :
/*------------------------------------------------------------------------------
  Purpose:     Opprettelse av bonghode og bonghodelinje.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR pcTekst AS CHAR NO-UNDO.
        
BONGHODE:
DO:
    /* Sjekker om bongen finnes? */
    FIND tmpBongHode WHERE
         tmpBongHode.ButikkNr = tmpFilLinjer.ButikkNr AND
         tmpBongHode.GruppeNr = 1                     AND
         tmpBongHode.KasseNr  = tmpFilLinjer.PosId    AND
         tmpBongHode.Dato     = tmpFilLinjer.Dato     AND
         tmpBongHode.BongNr   = tmpFilLinjer.TillNum NO-ERROR.
    IF NOT AVAILABLE tmpBongHode THEN
    BONGOPPSTANDELSE:
    DO:
      FIND LAST btmpBongHode NO-LOCK USE-INDEX B_Id NO-ERROR.
      CREATE tmpBongHode.
      ASSIGN /* Setter indeksfeltene */
          tmpBongHode.ButikkNr   = tmpFilLinjer.ButikkNr 
          tmpBongHode.GruppeNr   = 1
          tmpBongHode.KasseNr    = tmpFilLinjer.PosId  
          tmpBongHode.Dato       = tmpFilLinjer.Dato     
          tmpBongHode.BongNr     = tmpFilLinjer.TillNum
          tmpBongHode.B_Id       = IF AVAILABLE btmpBongHode 
                                     THEN btmpBongHode.B_Id + 1
                                     ELSE 1
          tmpBongHode.DataSettId = tmpFilLinjer.DataSettId
          tmpBongHode.BongStatus = 1 /* Under klargjøring */
          tmpBongHode.pfFlagg    = 4
          iLinjeNr               = 0
          iAntBonger             = iAntBonger + 1
          .
    END. /* BONGOPPSTANDELSE */
END. /* BONGHODE */

BONGHODEINFO:
DO:
    ASSIGN
        tmpBongHode.KassererNr      = INT(trim(trim(ENTRY(16,tmpFilLinjer.StorTekst),"+"),"-"))
        tmpBongHode.SelgerNr        = 0
        pcTekst                     = trim(trim(ENTRY(11,tmpFilLinjer.StorTekst),"+"),"-")
        tmpBongHode.Belop           = round(
                                              (dec(trim(trim(ENTRY(23,tmpFilLinjer.StorTekst),"+"),"-")) / 1000) + 
                                              (dec(trim(trim(ENTRY(22,tmpFilLinjer.StorTekst),"+"),"-")) / 1000)
                                            ,2)
        tmpBongHode.KortType        = 1 /* (1-Ingen, 2-Kunde, 3-Medlem) */
        tmpBongHode.KundeKort       = trim(trim(ENTRY(24,tmpFilLinjer.StorTekst),"+"),"-")
        tmpBongHode.flBetalingskort = IF tmpBongHode.KundeKort <> ""
                                        THEN TRUE
                                        ELSE tmpBongHode.flBetalingskort
        tmpBongHode.flBankkort      = tmpBongHode.flBetalingskort 
        tmpBongHode.flKreditkort    = tmpBongHode.flBetalingskort 
        .
    /* Setter tid. */
    IF NUM-ENTRIES(pcTekst,":") = 2 THEN
        tmpBongHode.Tid = (int(ENTRY(1,pcTekst,":")) * 3600) + (int(ENTRY(2,pcTekst,":")) * 60).
    /* Kobler til kasserer */
    FIND ButikkForsalj NO-LOCK WHERE
        ButikkForsalj.Butik      = tmpBongHode.ButikkNr AND
        ButikkForsalj.KassererId = int(tmpBongHode.KassererNr) NO-ERROR.
    IF AVAILABLE ButikkForsalj THEN
    DO:
        tmpBongHode.KassererNr = ButikkForsalj.ForsNr.
        FIND Forsalj NO-LOCK where
            Forsalj.ForsNr = ButikkForsalj.ForsNr NO-ERROR.
        IF AVAILABLE Forsalj THEN
            tmpBongHode.KassererNavn = Forsalj.FoNamn.

    END.
    /* Kobler til kunde */
    FIND FIRST KundeKort NO-LOCK WHERE
        KundeKort.KortNr = tmpBongHode.KundeKort NO-ERROR.
    IF AVAILABLE KundeKort THEN
    DO:
        FIND Kunde NO-LOCK WHERE
            Kunde.KundeNr = KundeKort.KundeNr NO-ERROR.
        ASSIGN
            tmpBongHode.KundeNr   = KundeKort.KundeNr
            tmpBongHode.KundeNavn = IF AVAILABLE Kunde
                                      THEN Kunde.Navn
                                      ELSE ""
            .
    END.

/*
{1}.Konvertert      = {2}.Konvertert
{1}.KortType        = {2}.KortType
{1}.flBetalingskort = {2}.flBetalingskort
{1}.flBankkort      = {2}.flBankkort
{1}.flKreditkort    = {2}.flKreditkort
{1}.flGavekort      = {2}.flGavekort
{1}.flSjekk         = {2}.flSjekk
{1}.flRekvisisasjon = {2}.flRekvisisasjon.
{1}.flKupong1       = {2}.flKupong1
{1}.flSlKort        = {2}.flSlKort
{1}.flRabatt        = {2}.flRabatt
{1}.Systemkort      = {2}.Systemkort
{1}.flSystemkort    = {2}.flSystemkort
*/
END. /* BONGHODEINFO */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

