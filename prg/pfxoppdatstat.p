&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : pfxoppdatstat.p
    Purpose     : Integrasjon ProfitBase
                  Oppdatering av statistikkakkumulatorer som benyttes av 
                  ProfitBase.

    Syntax      :

    Description : Rutinen tar alltid alle registre. Startes automatisk 
                  etter import av VPI fra MegaDisc på HK.

    Author(s)   : Tom Nøkleby
    Created     : 18/3-03
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF VAR cRutiner AS CHAR NO-UNDO.

DEF BUFFER bufBongHode FOR BongHode.
DEF BUFFER bufDatasett FOR DataSett.

DEF VAR iAntDatasett AS INT  NO-UNDO.
DEF VAR iAntBonger   AS INT  NO-UNDO.
DEF VAR cPrefix      AS CHAR NO-UNDO.

DEF TEMP-TABLE tmpDataSett LIKE DataSett.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{syspara.i 2 6 1 cPrefix}

RUN stControll.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-stControll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stControll Procedure 
PROCEDURE stControll :
/*------------------------------------------------------------------------------
  Purpose:     Kontrollrutine som styrer de andre funksjonen i dette programmet.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop1      AS INT  NO-UNDO.

/*
DEF VAR cNoTrans    AS CHAR NO-UNDO.
DEF VAR iAntall     AS INT  NO-UNDO.
/* Liste over rutiner som skal kjøres. */
ASSIGN
  cNoTrans = "stDaySales,"
  .
/* Starter oppdateringsrutiner */
OPPDATER-NOTRANS:
DO iLoop1 = 1 TO NUM-ENTRIES(cNoTrans):
  ASSIGN
    iAntall = 0
    .
  PUBLISH 'PBR' (string(today) + " " + string(time,"HH:MM:SS") + " " +
                 "PRB Starter rutine " + ENTRY(iLoop1,cNoTrans) + ","). 
  RUN VALUE(ENTRY(iLoop1,cNoTrans)).
  PUBLISH 'PBR' (string(today) + " " + string(time,"HH:MM:SS") + " " +
                 "PRB antall bonger: " + string(iAntall) + "."). 
END. /* OPPDATER-NOTRANS */
*/

PUBLISH 'PBR' (string(today) + " " + string(time,"HH:MM:SS") + " " +
               "PRB Starter oppdatering av statistikk..."). 

/* Leser alle datasett som ikke er oppdatert eller delhvis oppdaterte. */
/* og starter deretter behandling av ett og  et datasett. Bongene i    */
/* datasettet leses og behandles ferdige en for en. Bongen flagges som */
/* overført når den er klar.                                           */
BYGG-TMP:
DO piLoop1 = 2 TO 1 BY -1: /* Delhvis opp. først deretter nye */
  PUBLISH 'PBR' (string(today) + " " + string(time,"HH:MM:SS") + " " +
                 "Logger datasett som skal behandles..."). 
  DATASETT:
  FOR EACH DataSett NO-LOCK WHERE
    DataSett.pfFlagg = piLoop1 AND
    DataSett.SettStatus > 1 /* Kun datasett som det er kommet data på */
    BREAK BY DataSett.pfFlagg
          BY DataSett.ButikkNr
          BY DataSett.GruppeNr
          BY DataSett.KasseNr
          BY DataSett.Dato
          BY DataSett.SettNr:
      /* Logger datasettet */
      CREATE tmpDataSett.
      BUFFER-COPY DataSett TO tmpDataSett.
      RELEASE tmpDataSett.
  END.
END. /* BYGG-TMP */

LOOP1:
FOR EACH tmpDataSett /* Delhvis opp. først deretter nye */
    BREAK BY tmpDataSett.pfFlagg
          BY tmpDataSett.ButikkNr
          BY tmpDataSett.GruppeNr
          BY tmpDataSett.KasseNr
          BY tmpDataSett.Dato
          BY tmpDataSett.SettNr:
  PUBLISH 'PBR' (string(today) + " " + string(time,"HH:MM:SS") + " " +
                 "Behandler datasett..."). 
  FIND DataSett NO-LOCK WHERE
      DataSett.DataSettId = tmpDataSett.DataSettId NO-ERROR.
  IF NOT AVAILABLE DataSett /* ??? */ THEN
      NEXT LOOP1.

  DATASETT:
  DO: 
    ASSIGN
      iAntDataSett = iAntDataSett + 1
      .

    /* Flagger settet som under oppdatering. */
    IF DataSett.pfFlagg < 2 THEN
    DO TRANSACTION:
      FIND bufDataSett EXCLUSIVE-LOCK WHERE
        recid(bufDataSett) = RECID(DataSett) NO-ERROR.
      ASSIGN
        bufDataSett.pfFlagg = 2 /* Under oppdatering */
        .
      RELEASE bufDataSett.
    END. /* TRANSACTION */

    /* Leser bongene i DataSettet */
    BONGHODE:
    FOR EACH BongHode OF DataSett EXCLUSIVE-LOCK WHERE
      BongHode.pfFlagg < 3:
      ASSIGN
        iAntBonger = iAntBonger + 1
        .
      RUN stDaySales.  /* Artikkelomsetning      */
      RUN stDaySh.     /* Varegruppesalg time    */
      RUN stStatRev.   /* Statistisk informasjon */
      RUN stTenderRev. /* Omsetning på media     */
      /* BongHode oppdatert */
      ASSIGN
        BongHode.pfFlagg = 3
        .
    END. /* BongHode */

    /* Flagger settet som oppdatert. */
    STEMPLE-OPPDAT:
    DO TRANSACTION:
      IF CAN-FIND(FIRST Bonghode OF DataSett
                  WHERE BongHode.pfFlagg < 3) THEN
        LEAVE STEMPLE-OPPDAT.
      FIND bufDataSett EXCLUSIVE-LOCK WHERE
        recid(bufDataSett) = RECID(DataSett) NO-ERROR.
      ASSIGN
        bufDataSett.pfFlagg = 3 /* Ferdig oppdatert */
        .
      RELEASE bufDataSett.
    END. /* STEMPLE-OPPDAT TRANSACTION */

  END. /* DATASETT */

  IF piLoop1 = 2 THEN
  DO:
    PUBLISH 'PBR' (string(today) + " " + string(time,"HH:MM:SS") +
                   " PRB Delhvis oppdaterte datasett behandlet: " + string(iAntDatasett) + 
                   " Antall bonger: " + STRING(iAntBonger) + "."). 
    ASSIGN
      iAntDataSett = 0
      iAntBonger   = 0
      .
  END.
  ELSE DO:
    PUBLISH 'PBR' (string(today) + " " + string(time,"HH:MM:SS") +
                   " PRB Antall nye datasett behandlet: " + string(iAntDatasett) + 
                   " Antall bonger: " + STRING(iAntBonger) + "."). 
    ASSIGN
      iAntDataSett = 0
      iAntBonger   = 0
      .
  END.
END. /* LOOP1 */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-stDaySales) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stDaySales Procedure 
PROCEDURE stDaySales :
/*------------------------------------------------------------------------------
  Purpose:     Oppdaterer Artikkelomsetning.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.
  DEF VAR cKode   AS CHAR NO-UNDO.

  ASSIGN 
    pcTekst = "001,003,006,010,011"
    .
                                    
  BONGLINJE:
  FOR EACH BongLinje NO-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.Makulert = FALSE AND
    CAN-DO(pcTekst,STRING(BongLinje.TTId,"999")):

    ASSIGN
        cKode = BongLinje.Strekkode
        .

    FIND Strekkode NO-LOCK WHERE
      Strekkode.Kode = cKode NO-ERROR.
    IF AVAILABLE Strekkode THEN
      FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
    IF AVAILABLE ArtBas THEN
      FIND LevBas OF ArtBas NO-LOCK NO-ERROR.

    /* Salg på ukjente artikler skal posteres på varegruppe. */
    IF NOT AVAILABLE ArtBas OR NOT AVAILABLE Strekkode THEN
    KONVERTERING:
    DO:
        IF AVAILABLE Strekkode THEN
            RELEASE Strekkode.
        FIND ArtBas NO-LOCK WHERE
             ArtBas.ArtikkelNr = dec(cPrefix + string(BongLinje.VareGr,"999999"))
            NO-ERROR.
        IF AVAILABLE ArtBas THEN
            FIND FIRST Strekkode OF ArtBas NO-ERROR.
        IF AVAILABLE Strekkode THEN
            cKode = Strekkode.Kode.
    END. /* KONVERTERING */

    FIND pfDaySales EXCLUSIVE-LOCK WHERE
      pfDaySales.Store_No = BongLinje.ButikkNr AND
      pfDaySales.Plu_Code = cKode AND
      pfDaySales.DATE     = BongLinje.TransDato
      NO-ERROR.
    IF NOT AVAILABLE pfDaySales THEN
    DO:
      CREATE pfDaySales.
      ASSIGN
        pfDaySales.Store_No = BongLinje.ButikkNr 
        pfDaySales.Plu_Code = cKode
        pfDaySales.DATE     = BongLinje.TransDato
        .
    END.

    ASSIGN
      pfDaySales.Sales     = pfDaySales.Sales     + ((BongLinje.LinjeSum /*- BongLinje.MvaK*/ - (BongLinje.LinjeRab + BongLinje.SubTotalRab)) * 
                                                     (IF BongLinje.Antall < 0
                                                         THEN -1
                                                         ELSE 1))
      pfDaySales.Qty       = pfDaySales.Qty       + BongLinje.Antall
      pfDaySales.Disc      = pfDaySales.Disc      + ((BongLinje.LinjeRab + BongLinje.SubTotalRab) * 
                                                     (IF BongLinje.Antall < 0
                                                         THEN -1
                                                         ELSE 1))
      pfDaySales.Vat       = pfDaySales.Vat       + (BongLinje.MvaKr * (IF BongLinje.Antall < 0
                                                                         THEN -1
                                                                         ELSE 1))
      pfDaySales.SalesCost = pfDaySales.SalesCost + (BongLinje.VVareKost * BongLinje.Antall)
      pfDaySales.VendorId  = IF AVAILABLE LevBas
                               THEN LevBas.LevNr
                               ELSE BongLinje.LevNr
      .

  END. /* BONGLINJE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-stDaySh) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stDaySh Procedure 
PROCEDURE stDaySh :
/*------------------------------------------------------------------------------
  Purpose:     Oppdaterer Hovedgrupper time.
  Parameters:  <none>
  Notes:       10/4-03 TN
               Oppdatering gjøres på HK nivå. Ref. E-Mail fra Nerland.
------------------------------------------------------------------------------*/
  DEF VAR pcTekst      AS CHAR NO-UNDO.
  DEF VAR piAvdelingNr AS INT  NO-UNDO.

  ASSIGN 
    pcTekst = "001,003,006,010,011"
    .       
                                    
  BONGLINJE:
  FOR EACH BongLinje NO-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.Makulert = FALSE AND
    CAN-DO(pcTekst,STRING(BongLinje.TTId,"999")):

    /* Kobler mot avdeling */
    FIND HuvGr NO-LOCK WHERE
        HuvGr.Hg = BongLinje.HovedGr NO-ERROR.
    IF AVAILABLE HuvGr THEN
        piAvdelingNr = HuvGr.AvdelingNr.
    ELSE 
        NEXT BONGLINJE.

    FIND pfDaySales_HourExt EXCLUSIVE-LOCK WHERE
      pfDaySales_HourExt.DepartmentId = piAvdelingNr AND
      pfDaySales_HourExt.Store_No     = BongLinje.ButikkNr AND
      pfDaySales_HourExt.DATE         = BongLinje.TransDato AND
      pfDaySales_HourExt.Tid          = int(entry(1,STRING(BongLinje.TransTid,"HH:MM:SS"),":"))
      NO-ERROR.
    IF NOT AVAILABLE pfDaySales_HourExt THEN
    DO:
      CREATE pfDaySales_HourExt.
      ASSIGN
        pfDaySales_HourExt.DepartmentId = piAvdelingNr 
        pfDaySales_HourExt.Store_No     = BongLinje.ButikkNr 
        pfDaySales_HourExt.DATE         = BongLinje.TransDato 
        pfDaySales_HourExt.RetailDate   = BongLinje.TransDato 
        pfDaySales_HourExt.Tid          = int(entry(1,STRING(BongLinje.TransTid,"HH:MM:SS"),":"))
        .
    END.

/*     FIND Strekkode NO-LOCK WHERE                     */
/*       Strekkode.Kode = BongLinje.Strekkode NO-ERROR. */
/*     IF AVAILABLE Strekkode THEN                      */
/*       FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.     */
/*     IF AVAILABLE ArtBas THEN                         */
/*       FIND LevBas OF ArtBas NO-LOCK NO-ERROR.        */

    ASSIGN
      pfDaySales_HourExt.Omsetning = pfDaySales_HourExt.omsetning + 
                                     ((BongLinje.LinjeSum /*- BongLinje.MvaK*/ - (BongLinje.LinjeRab + BongLinje.SubTotalRab)) * 
                                                     (IF BongLinje.Antall < 0
                                                         THEN -1
                                                         ELSE 1))
      pfDaySales_HourExt.Qty       = pfDaySales_HourExt.Qty + 
                                     BongLinje.Antall
      pfDaySales_HourExt.Disc      = pfDaySales_HourExt.Disc +
                                     ((BongLinje.LinjeRab + BongLinje.SubTotalRab) * 
                                                     (IF BongLinje.Antall < 0
                                                         THEN -1
                                                         ELSE 1))
      pfDaySales_HourExt.Vat       = pfDaySales_HourExt.Vat + 
                                     (BongLinje.MvaKr * (IF BongLinje.Antall < 0
                                                            THEN -1
                                                            ELSE 1))
      .

  END. /* BONGLINJE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-stStatRev) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stStatRev Procedure 
PROCEDURE stStatRev :
/*------------------------------------------------------------------------------
  Purpose:     Oppdaterer Statistiske data.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.
  DEF VAR pInt    AS INT  NO-UNDO.

  ASSIGN 
    pInt    = 1
    pcTekst = "001,003,006,010,011"
    .
                                    
  DO TRANSACTION:
      FIND pfStatisticsRevenue EXCLUSIVE-LOCK WHERE
        pfStatisticsRevenue.Store_No = BongHode.ButikkNr AND
        pfStatisticsRevenue.DATE     = BongHode.Dato
        NO-ERROR.
      IF NOT AVAILABLE pfStatisticsRevenue THEN
      DO:
        CREATE pfStatisticsRevenue.
        ASSIGN
          pfStatisticsRevenue.Store_No = BongHode.ButikkNr 
          pfStatisticsRevenue.DATE     = BongHode.Dato
          .
      END.
      BONGLINJE:
      FOR EACH BongLinje NO-LOCK WHERE
        BongLinje.B_Id = BongHode.B_Id AND
        BongLinje.Makulert = FALSE AND
        CAN-DO(pcTekst,STRING(BongLinje.TTId,"999")):

        ASSIGN
          pfStatisticsRevenue.Item_Count        = pfStatisticsRevenue.Item_Count        + BongLinje.Antall
          pfStatisticsRevenue.Refund_Count      = pfStatisticsRevenue.Refund_Count      + (IF BongLinje.TTId = 10
                                                                                            THEN abs(BongLinje.Antall)
                                                                                            ELSE 0)
          pfStatisticsRevenue.Refund_Amount = pfStatisticsRevenue.Refund_Amount + (IF BongLinje.TTId = 10
                                                                                            THEN abs(BongLinje.LinjeSum /*- BongLinje.MvaKr*/ - (BongLinje.LinjeRab + BongLinje.SubTotalRab))
                                                                                            ELSE 0)
          .

      END. /* BONGLINJE */

      /* Teller skuffeåpninger */
      ASSIGN pcTekst = "203".
      NO-SALE:
      FOR EACH BongLinje NO-LOCK WHERE
        BongLinje.B_Id = BongHode.B_Id AND
        BongLinje.Makulert = FALSE AND
        CAN-DO(pcTekst,STRING(BongLinje.TTId,"999")):

        ASSIGN
            pfStatisticsRevenue.NoSale_Count = pfStatisticsRevenue.NoSale_Count + 1
            .

      END. /* NO-SALE */

      /* Teller kunder og korreksjoner av bonglinjer. */
      ASSIGN pcTekst = "91,94,95,101,111,121,131,141,151,160,161,164,165,167,169,171,92,93,152,153,162,163".                                  
      KORREKSJONER:
      FOR EACH BongLinje NO-LOCK WHERE
        BongLinje.B_Id = BongHode.B_Id:

          ASSIGN
          pfStatisticsRevenue.Customers_Count   = pfStatisticsRevenue.Customers_Count + pInt
          pInt                                  = 0 /* Teller bare bongen en gang. */
          .
          /* Teller korreksjoner for HuginSveda bonger. Litt FyFy. */
          IF NUM-ENTRIES(BongLinje.Originaldata,CHR(1)) >= 2 THEN
          DO:
              IF CAN-DO(pcTekst,trim(entry(2,BongLinje.Originaldata,CHR(1)))) THEN
                  ASSIGN
                      pfStatisticsRevenue.Neg_Ticket_Count  = 
                             pfStatisticsRevenue.Neg_Ticket_Count  + (IF BongLinje.Antall < 0
                                                                        THEN abs(BongLinje.Antall)
                                                                        ELSE 0)
                      pfStatisticsRevenue.Neg_Ticket_Amount = 
                             pfStatisticsRevenue.Neg_Ticket_Amount + (IF BongLinje.Antall < 0
                                                                     THEN abs(BongLinje.LinjeSum /*- BongLinje.MvaKr*/ - (BongLinje.LinjeRab + BongLinje.SubTotalRab))
                                                                     ELSE 0)
              .
          END.
      END. /* KORREKSJONER */
  END. /* TRANS */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-stTenderRev) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stTenderRev Procedure 
PROCEDURE stTenderRev :
/*------------------------------------------------------------------------------
  Purpose:     Oppdaterer omsetning på media.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTTIdTekst AS CHAR NO-UNDO.
  DEF VAR pcPOSTekst  AS CHAR NO-UNDO.
  DEF VAR pcPos       AS CHAR NO-UNDO.
  DEF VAR pcTTId      AS CHAR NO-UNDO.
  DEF VAR piEntry     AS INT  NO-UNDO.

  ASSIGN
    pcTTIdTekst = "050,051,052,053,054,055,056,057,058,059,061,062,064,065,066,067,068,069,070,071,072,073,078,079"
/*     pcPOSTekst  = "050,051,052,053,054,055,056,057,058,059,061,062,064,065,066,067,068,069,070,071,072" */
/*     pcPOSTekst  = "xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx" */
    .

  BONGLINJE:
  FOR EACH BongLinje NO-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.Makulert = FALSE AND
    CAN-DO(pcTTIdTekst,STRING(BongLinje.TTId,"999")):

/*     IF LOOKUP(pcTTId,pcTTIdTekst) = 0 THEN   */
/*     DO:                                      */
/*       /* LOGG! LOGG FEIL HER */              */
/*       NEXT BONGLINJE.                        */
/*     END.                                     */
/*                                              */
/*     /* Konverterer transaksjonskoden */      */
/*     ASSIGN                                   */
/*       pcTTID  = STRING(BongLinje.TTID,"999") */
/*       piEntry = LOOKUP(pcTTId,pcTTIdTekst)   */
/*       pcPOS   = ENTRY(piEntry,pcPOSTekst)    */
/*       .                                      */
/*     IF pcPOS = "xxx" THEN                    */
/*     DO:                                      */
/*       /* LOGG! LOGG FEIL HER */              */
/*       NEXT BONGLINJE.                        */
/*     END.                                     */
    ASSIGN
      pcTTId = STRING(BongLinje.TTId,"999")
      pcPOS  = pcTTId
      .

    IF NOT CAN-DO("070",STRING(BongLinje.TTId,"999")) THEN
    ALLT-ANNET:
    DO:
        FIND pfTenderRevenue EXCLUSIVE-LOCK WHERE
          pfTenderRevenue.Store_No = BongLinje.ButikkNr AND
          pfTenderRevenue.DATE     = BongLinje.TransDato AND
          pfTenderRevenue.Media_No = int(pcPOS)
          NO-ERROR.
        IF NOT AVAILABLE pfTenderRevenue THEN
        DO:
          CREATE pfTenderRevenue.
          ASSIGN
            pfTenderRevenue.Store_No = BongLinje.ButikkNr 
            pfTenderRevenue.DATE     = BongLinje.TransDato 
            pfTenderRevenue.Media_No = int(pcPOS)
            .
        END.

        ASSIGN
          pfTenderRevenue.Sale_Amount = pfTenderRevenue.Sale_Amount + BongLinje.LinjeSum
          .
    END. /* ALLT-ANNET */

    /* Korrigerer kontant med veksel */
    IF CAN-DO("070",STRING(BongLinje.TTId,"999")) THEN
    VEKSEL:
    DO:
        FIND pfTenderRevenue EXCLUSIVE-LOCK WHERE
          pfTenderRevenue.Store_No = BongLinje.ButikkNr AND
          pfTenderRevenue.DATE     = BongLinje.TransDato AND
          pfTenderRevenue.Media_No = 50
          NO-ERROR.
        IF NOT AVAILABLE pfTenderRevenue THEN
        DO:
          CREATE pfTenderRevenue.
          ASSIGN
            pfTenderRevenue.Store_No = BongLinje.ButikkNr 
            pfTenderRevenue.DATE     = BongLinje.TransDato 
            pfTenderRevenue.Media_No = 50
            .
        END.
        ASSIGN
          pfTenderRevenue.Sale_Amount = pfTenderRevenue.Sale_Amount + BongLinje.LinjeSum
          .
    END. /* VEKSEL */
  END. /* BONGLINJE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

