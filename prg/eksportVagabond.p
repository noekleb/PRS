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

/*&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          */
/*  DEFINE VARIABLE dFraDato  AS DATE     INIT "01/01/2012" NO-UNDO.  /* MM/DD/YY */
  DEFINE VARIABLE dTilDato  AS DATE     INIT "06/10/2013" NO-UNDO.  /* MM/DD/YY */*/
DEFINE VARIABLE cButikkNr AS CHARACTER NO-UNDO.
/*&ELSE
  DEFINE INPUT PARAMETER dFraDato  AS DATE    NO-UNDO.
  DEFINE INPUT PARAMETER dTilDato  AS DATE    NO-UNDO.
  DEFINE INPUT PARAMETER cButikkNr AS CHARACTER NO-UNDO.
&ENDIF*/

DEFINE VARIABLE cFilNavn   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKopi      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKatalog   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDato      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAnt       AS INTEGER   NO-UNDO.
DEFINE VARIABLE pcLevNr    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iButikkNr  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iParaButik AS INTEGER   NO-UNDO.
DEFINE VARIABLE pcKundeNr  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTransNr2  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTransNr3  AS INTEGER   NO-UNDO.
DEFINE VARIABLE pcPara2    AS CHARACTER NO-UNDO.

DEFINE STREAM Ut.
DEFINE STREAM Kopi.

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
         HEIGHT             = 14.29
         WIDTH              = 42.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEFINE VARIABLE iLoop     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCount    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lNoButik  AS LOGICAL   NO-UNDO.

{syspara.i 1 8 6 cButikkNr} 
  IF cButikkNr = "" THEN
    QUIT.

{syspara.i 1 8 7 pcLevNr} /* Leverantörsnummer. */
  IF pcLevNr = "" THEN
      QUIT.
{syspara.i 1 8 1 cKatalog}
IF cKatalog = "" THEN
    cKatalog = "c:\home\lindbak\sendes".
/*MESSAGE dFradato SKIP dTildato SKIP cbutikknr
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

/*MESSAGE NUM-ENTRIES(cButikkNr,";")
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

IF NUM-ENTRIES(cButikkNr,";") < 1 THEN
   QUIT.

DO iCount = 1 TO NUM-ENTRIES(cButikkNr,";"):
  ASSIGN iButikkNr = INT(ENTRY(iCount,cButikkNr,";")).
  ASSIGN iParaButik = 1000 + iButikkNr.
/*  MESSAGE "iParaButik " iParaButik
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

  ASSIGN pcKundeNr = "".
  {syspara.i 1 8 iParaButik pcKundeNr}
  IF pcKundeNr = "" THEN 
    ASSIGN lNoButik = TRUE.
  ELSE
    ASSIGN lNoButik = FALSE.

  IF lNoButik = FALSE THEN
    DO:
      FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId = 1 AND 
        SysPara.SysGr  = 8 AND 
        SysPara.ParaNr = iParaButik NO-ERROR.
      IF AVAILABLE SysPara THEN
      DO:
        ASSIGN iLoop = INT(ENTRY(1,SysPara.Parameter2,";"))
               iTransNr3 = INT(ENTRY(2,SysPara.Parameter2,";")).
        ASSIGN iLoop = iLoop + 1.
        ASSIGN SysPara.Parameter2 = STRING(iLoop,"9999999") + ";" + STRING(iTransNr3).
        RELEASE SysPara.
/*        MESSAGE "iLoop " iLoop SKIP "iTransNr3 " iTransNr3
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
      END.
    END.
  IF lNoButik = FALSE THEN
  DO:
    {syspara.i 1 8 2 cTekst}
    IF cTekst = "" THEN cTekst = "P".
    cFilNavn = cTekst.
    cFilNavn = cFilNavn + pcKundeNr.

    cFilNavn = cFilNavn + STRING(iLoop,"9999999") + ".". 

    cKopi = cFilNavn.
    cKopi = cKopi + "csv".
    {syspara.i 1 8 5 cTekst}
    IF cTekst = "" THEN cTekst = "VB".
    cFilNavn = cFilNavn + cTekst.

    ASSIGN iAnt = 0.

/*    MESSAGE "iParaButik " iParaButik SKIP "lNoButik: " lNoButik
      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    RUN EksportBong.

/*    MESSAGE "iTransNr2 " iTransNr2 SKIP iParaButik 
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

/*    MESSAGE "iAnt " iAnt
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    IF iAnt = 0 THEN
    DO:
      OS-DELETE VALUE(cKatalog + "\" + cFilNavn).
      FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId = 1 AND 
        SysPara.SysGr  = 8 AND 
        SysPara.ParaNr = iParaButik NO-ERROR.
      IF AVAILABLE SysPara THEN
        DO:
          ASSIGN iLoop = INT(ENTRY(1,SysPara.Parameter2,";"))
                 iTransNr3 = INT(ENTRY(2,SysPara.Parameter2,";")).
          ASSIGN iLoop = iLoop - 1.
          ASSIGN SysPara.Parameter2 = STRING(iLoop,"9999999") + ";" + STRING(iTransNr3).
          RELEASE SysPara.
        END.
      END.
    ELSE
    DO:
      FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId = 1 AND 
        SysPara.SysGr  = 8 AND 
        SysPara.ParaNr = iParaButik NO-ERROR.
      IF AVAILABLE SysPara THEN
      DO:
        ASSIGN iLoop = INT(ENTRY(1,SysPara.Parameter2,";")).
        ASSIGN SysPara.Parameter2 = STRING(iLoop,"9999999") + ";" + STRING(iTransNr2).
        RELEASE SysPara.
      END.
    END.

/*      MESSAGE "Skickar fil "
          VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    IF SEARCH("cmd\vagabondftp.cmd") <> ? AND SEARCH(cKatalog + "\" + cFilNavn) <> ? THEN
        OS-COMMAND VALUE("cmd\vagabondftp.cmd" + " " + cFilNavn).
  END.
END.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-EksportBong) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportBong Procedure 
PROCEDURE EksportBong :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:     StartPos  Bredd    Typ   Obligatorisk
             -------------------------------------
Kundnr            1     20      Text      Ja 
Verifikation     21     20      Text      Ja
Radnr            41     10      Heltal    Ja
KassaNr          51     10      Text      Nej 
Transaktionstyp  61      2      Heltal    Ja
Bokf.mall        63     10      Text      Nej
Varugrupp        73     10      Text      Nej
Nr               83     20      Text      Ja
Artikelnr       103     20      Text      Ja
Storlek         123      4      Text      Ja
Orsakskod       127      4      Text      Nej
Kundens art.nr  131     15      Text      Ja
Antal           151     10      Decimal   Ja
Pris Inkl. moms 156     12      Decimal   Nej
Rabatt %        168     12      Decimal   Ja
Rabatt Kr+moms  180     12      Decimal   Ja
Belopp Kr+moms  192     12      Decimal   Ja
Varukost        204     12      Decimal   Ja
Datum           216      8      Datum     Ja
Tid             224      8      Tid       Ja
Leverantör      232     20      Text      Nej
Barcode         252     20      Text      Nej
T1              272     20      Text      Nej
T2              292     20      Text      Nej
D1 Momssats     312     12      Decimal   Ja
D2              324     12      Decimal   Nej
V1              336      2      Heltal    Nej
V2              338      2      Heltal    Nej
Flagga          340      1      Text      Ja
      
------------------------------------------------------------------------------*/
  DEFINE VARIABLE plVVarekost AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE plOrdPris   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE pcLevKod1   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcLevKod2   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcPOS       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcMva%      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iAnt2       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lLevTraff   AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE iCount      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iLevNr      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iTransNr    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iAntal      AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cDatum      AS CHARACTER   NO-UNDO.
  
  OUTPUT STREAM Ut   TO VALUE(cKatalog + "\" + cFilNavn).
  /*OUTPUT STREAM Kopi TO VALUE(cKatalog + "\" + cKopi).*/

  /* TEST */
/*   ASSIGN                    */
/*       dFraDato = 01/01/2001 */
/*       dTilDato = 12/31/2005 */
/*       .                     */

/*  MESSAGE "pcKundeNr: " pcKundeNr
      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
  TRANSLOGGEKSPORT:
  FOR EACH Translogg NO-LOCK WHERE 
      TransLogg.Butik      = iButikkNr AND
      TransLogg.TransNr > iTransNr3:
/*      TransLogg.Dato      >= dFraDAto AND 
      TransLogg.Dato      <= dTilDato:*/
      FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = Translogg.ArtikkelNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
          NEXT TRANSLOGGEKSPORT.

      ASSIGN lLevTraff = FALSE.

      DO iCount = 1 TO NUM-ENTRIES(pcLevNr,";"):
        ASSIGN iLevNr = INT(ENTRY(iCount,pcLevNr,";")).
        IF ArtBas.LevNr = iLevNr THEN
        DO:
           ASSIGN lLevTraff = TRUE.
           LEAVE.
        END.
      END.

      IF lLevTraff = FALSE THEN
        NEXT TRANSLOGGEKSPORT.
      
/*      IF artbas.levnr <> 1030 AND
         artbas.levnr <> 2030 AND
         artbas.levnr <> 3030 AND
         artbas.levnr <> 4030 THEN
          NEXT TRANSLOGGEKSPORT.  */

      IF AVAILABLE ArtBas THEN
          FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
      IF AVAILABLE ArtBas THEN
          FIND Lager NO-LOCK WHERE
          Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
          Lager.Butik      = TransLogg.Butik NO-ERROR.
      FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.

      DEFINE VARIABLE plPris        AS DECIMAL NO-UNDO.
      DEFINE VARIABLE plRabKr       AS DECIMAL NO-UNDO.
      DEFINE VARIABLE plRab%        AS DECIMAL NO-UNDO.
      DEFINE VARIABLE T_Antall      AS INTEGER NO-UNDO.
      DEFINE VARIABLE T_Korrtecken  AS INTEGER NO-UNDO.
      DEFINE VARIABLE T_Korrtecken2 AS INTEGER NO-UNDO.

      ASSIGN T_Antall = Translogg.Antall.
      
      CASE TransLogg.TTId:
          WHEN  1 THEN DO:
              pcPOS = "03". /* Varesalg */
              T_Antall = T_Antall * -1. /* Varesalg rapporteras som negativt */
          END.
          WHEN  2 THEN pcPOS = IF T_Antall >= 0 THEN "05" ELSE "06". /* Brekasje */
          WHEN  3 THEN DO:
              pcPOS = IF T_Antall >= 0 THEN "03" ELSE "04". /* Kundereklamasjon */
              T_Antall = T_Antall * -1. /* Lagerreklamation rapporteras som negativt */
          END.
          WHEN  4 THEN DO:
              pcPOS = "06". /* Lagerreklamasjon */
              T_Antall = T_Antall * -1. /* Lagerreklamation rapporteras som negativt */
          END.
          WHEN  5 THEN pcPOS = IF T_Antall >= 0 THEN "01" ELSE "02". /* Varekjøp */
          WHEN  6 THEN pcPOS = IF T_Antall >= 0 THEN "05" ELSE "06". /* Overføring */
          WHEN  7 THEN pcPOS = IF T_Antall >= 0 THEN "05" ELSE "06". /* Lagerjustering */
          WHEN  8 THEN pcPOS = IF T_Antall >= 0 THEN "05" ELSE "06". /* Nedskrivning */
          WHEN  9 THEN pcPOS = IF T_Antall >= 0 THEN "05" ELSE "06". /* Svinn */
          WHEN 10 THEN DO: 
              pcPOS = "04". /* Gjenkjøp */
              T_Antall = T_Antall * -1. /* Gjenköp rapporteras som positivt */
          END.
          WHEN 11 THEN pcPOS = IF T_Antall >= 0 THEN "05" ELSE "06". /* Internt forbruk */
      END CASE.
      
      /* Korrigerar negativt tecke framför vissa transtyper */
      IF pcPOS = "01" OR pcPOS = "03" OR pcPOS = "04" THEN 
              T_Korrtecken = -1.
          ELSE 
              T_Korrtecken = 1.
      IF pcPOS = "03" OR pcPOS = "04" THEN 
              T_Korrtecken2 = -1.
          ELSE 
              T_Korrtecken2 = 1.

      /* Prøver vvarekost fra TransLogg */
      ASSIGN
          pcLevKod1   = SUBSTRING(TRIM(ArtBas.LevKod),1,11) + (IF ArtBas.LevKod = "" THEN "<Blank>" ELSE "") + "-" + TRIM(TransLogg.Storl)
          pcLevKod2   = SUBSTRING(TRIM(ArtBas.LevKod),1,11) + (IF ArtBas.LevKod = "" THEN "<Blank>" ELSE "")
          plOrdPris   = (IF AVAILABLE ArtPris THEN 
                         ArtPris.Pris[1] ELSE 0)
/*                         ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1] ELSE 0)*/

          plPris      = TransLogg.Pris - TransLogg.RabKr
          plRabKr     = (IF AVAILABLE ArtPris THEN (plOrdPris - plPris) ELSE TransLogg.RabKr)
/*          plRab%      = (Translogg.RabKr / TransLogg.Pris) * 100*/
          plRab%      = (IF AVAILABLE ArtPris THEN ((plRabKr / plOrdPris) * 100) ELSE ((plRabKr / TransLogg.Pris) * 100))
          plRab%      = IF plRab% = ? THEN 0 ELSE plRab%
          plVVarekost = TransLogg.VVareKost.
      /* Prøver VVArekost fra Lager. */
      IF plVVareKost  = 0 OR 
          plVVareKost = ? THEN
          plVVareKost = (IF AVAILABLE Lager THEN Lager.VVAreKost ELSE 0).
      /* Prøver VVArekost fra kalkylen */
      IF plVVareKost  = 0 OR 
          plVVareKost = ? THEN
          plVVareKost = (IF AVAILABLE ArtPris THEN 
                         ArtPris.VareKost[IF ArtPris.Tilbud THEN 2 ELSE 1] ELSE 0).
       ASSIGN plVVareKost = plVVareKost * T_Antall.
       IF plVVAreKost < 0 THEN
         ASSIGN plVVareKost = plVVArekost * -1.
       ASSIGN iTransNr = TransLogg.BongId.
       IF iTransNr = 0 THEN
          ASSIGN iTransNr = Translogg.TransNr.
       iTransNr = INT(STRING(translogg.butik) + STRING(translogg.kassanr) + STRING(iTransNr)).
       ASSIGN iANtal = T_Antall.
       IF TransLogg.TTId = 3 AND pcPos = "04" THEN
         ASSIGN iAntal = 0.
       ELSE IF pcPos = "06" THEN
         ASSIGN iAntal = 0.

       ASSIGN cDatum = STRING(YEAR(Translogg.Dato),"9999") + "-" +
                       STRING(MONTH(Translogg.Dato),"99") + "-" +
                       STRING(DAY(Translogg.Dato),"99").
/*       MESSAGE "1 " cDatum
           VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
       ASSIGN cDatum = SUBSTRING(cDatum,3,10).
/*       MESSAGE "2 " cDatum
           VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

      PUT STREAM Ut UNFORMATTED
          /*  1 */ pcKundeNr + FILL(" ",20 - LENGTH(pcKundeNr))
          /*  2 */ STRING(iTransNr) + FILL(" ",20 - LENGTH(STRING(iTransNr)))               
          /*  3 */ STRING(TransLogg.BongLinjeNr) + FILL(" ",10 - LENGTH(STRING(TransLogg.BongLinjeNr)))
          /*  4 */ STRING(TransLogg.KassaNr) + FILL(" ",10 - LENGTH(STRING(TransLogg.KassaNr))) 
          /*  5 */ pcPOS
          /*  6 */ FILL(" ",10) /* Bokf.mal */  
          /*  7 */ FILL(" ",10) /* Varugrupp */               
          /*  8 */ SUBSTRING(pcLevKod1,1,20) + FILL(" ",20 - LENGTH(pcLevKod1))
          /*  9 */ SUBSTRING(pcLevKod2,1,20) + FILL(" ",20 - LENGTH(pcLevKod2)) 
          /* 10 */ TRIM(Translogg.Storl) + FILL(" ",4 - LENGTH(TRIM(Translogg.Storl)))
          /* 11 */ "    "/* Orsakskod */               
          /* 12 */ STRING(Translogg.ArtikkelNr) + FILL(" ",15 - LENGTH(STRING(Translogg.ArtikkelNr)))   /* 20 -> 15 */
          /* 13 */ FILL(" ",10 - LENGTH(STRING((iAntal)))) + STRING((iAntal))                           /* 5 -> 10 */
          /* 14 */ FILL(" ",12 - LENGTH(STRING(plOrdPris,"->>>>>>>9.99"))) + STRING(plOrdPris,"->>>>>>>9.99") /* Pris inkl. mva. */
          /* 15 */ FILL(" ",12 - LENGTH(STRING(plRab%,"->>>>>>>9.99"))) + STRING(plRab%,"->>>>>>>9.99")
          /* 16 */ FILL(" ",12 - LENGTH(STRING(plRabKr * T_Antall,"->>>>>>>9.99"))) + STRING(plRabKr * T_Antall,"->>>>>>>9.99")
          /* 17 */ FILL(" ",12 - LENGTH(STRING(((TransLogg.Pris - TransLogg.RabKr) * T_Antall * T_Korrtecken),"->>>>>>>9.99"))) + STRING(((TransLogg.Pris - TransLogg.RabKr) * T_Antall * T_Korrtecken),"->>>>>>>9.99") /* Linjesum inkl. mva. */ 
          /* 18 */ FILL(" ",12 - LENGTH(STRING(plVVareKost,"->>>>>>>9.99"))) + STRING(plVVareKost,"->>>>>>>9.99")             
          /* 19 */ cDatum  /* STRING(Translogg.Dato,"99-99-99") */
          /* 20 */ STRING(TransLogg.Tid,"HH:MM:SS")
          /* 21 */ FILL(" ",20) /*IF AVAILABLE LevBas THEN (SUBSTRING(LevBas.LevNamn,1,20) + FILL(" ",20 - LENGTH(SUBSTRING(LevBas.LevNamn,1,20)))) ELSE FILL(" ",20)*/
          /* 22 */ FILL(" ",20) /*TransLogg.Kode + fill(" ",20 - LENGTH(substring(TransLogg.Kode,1,20))) */           
          /* 23 */ FILL(" ",20) /*T1*/
          /* 24 */ FILL(" ",20) /*T2*/
          /* 25 */ FILL(" ",12 - LENGTH(STRING(TransLogg.Mva%,"->>>>>>>9.99"))) + STRING(TransLogg.Mva%,"->>>>>>>9.99") /*D1*/ 
          /* 26 */ FILL(" ",12) /*D2*/
          /* 27 */ "  " /*V1*/
          /* 28 */ "  " /*V2*/
          /* 29 */ "Y" /*Flagga*/              
          SKIP.
/*          /*  2 */ STRING(Translogg.TransNr) + FILL(" ",20 - LENGTH(STRING(Translogg.TransNr)))               */
/*          /* 13 */ FILL(" ",5 - LENGTH(STRING((T_Antall)))) + STRING((T_Antall))*/
/*          /* 14 */ FILL(" ",12) /* Ord.Pris inkl. mva. */*/
/*          /* 16 */ FILL(" ",12 - LENGTH(STRING(TransLogg.RabKr * T_Antall,"->>>>>>>9.99"))) + STRING(TransLogg.RabKr * T_Antall,"->>>>>>>9.99")*/
/*          /* 18 */ FILL(" ",12 - LENGTH(STRING(plVVareKost * T_Antall * T_Korrtecken,"->>>>>>>9.99"))) + STRING(plVVareKost * T_Antall * T_Korrtecken,"->>>>>>>9.99")             */

          ASSIGN iAnt = iAnt + 1.
          ASSIGN iAnt2 = iAnt2 + 1.
          IF TransLogg.TransNr > 0 THEN
            ASSIGN iTransNr2 = TransLogg.TransNr.

/*          IF iAnt2 = 100 THEN
          DO:
              MESSAGE "iAnt-a " iAnt
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              ASSIGN iAnt2 = 0.
          END.*/

      /*
      PUT STREAM Kopi UNFORMATTED   
          /*  1 */ pcKundeNr ";"
          /*  2 */ Translogg.BongId ";"
          /*  3 */ TransLogg.BongLinjeNr ";"
          /*  4 */ TransLogg.KassaNr ";"
          /*  5 */ TransLogg.TTId ";"
          /*  6 */ "" /* Bokf.mal */  ";"
          /*  7 */ "" /* Varugrupp */ ";"
          /*  8 */ (IF ArtBas.LevKod = "" THEN "<Blank>" ELSE ArtBas.LevKod) + "-" + trim(TransLogg.Storl) ";"
          /*  9 */ (IF ArtBas.LevKod = "" THEN "<Blank>" ELSE ArtBas.LevKod) ";"
          /* 10 */ trim(Translogg.Storl) ";"
          /* 11 */ ArtBas.Beskr /* Orsakskod */ ";"
          /* 12 */ Translogg.ArtikkelNr ";"
          /* 13 */ Translogg.Antall ";"
          /* 14 */ (TransLogg.Pris - TransLogg.RabKr) /* Pris inkl. mva. */ ";"
          /* 15 */ (Translogg.VVarekost / (TransLogg.Pris - TransLogg.RabKr - TransLogg.Mva)) ";"
          /* 16 */ TransLogg.RabKr ";"
          /* 17 */ (TransLogg.Pris - TransLogg.RabKr) * Translogg.Antall /* Linjesum inkl. mva. */ ";"
          /* 18 */ Translogg.VVareKost ";"
          /* 19 */ Translogg.DAto ";"
          /* 20 */ String(TransLogg.Tid,"HH:MM:SS") ";"
          /* 21 */ IF AVAILABLE LevBas THEN LEvBas.LevNamn ELSE "" ";"
          /* 22 */ TransLogg.Kode ";"
          /* 23 */ ";" /*T1*/
          /* 24 */ ";" /*T1*/
          /* 25 */ ";" /*T1*/
          /* 26 */ ";" /*T1*/
          /* 27 */ ";" /*T1*/
          /* 28 */ ";" /*T1*/
          /* 29 */ "Y" /*T1*/
          SKIP.
          */

  END. /* TRANSLOGGEKSPORT */

  /*OUTPUT STREAM Kopi CLOSE.*/
  OUTPUT STREAM Ut CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

