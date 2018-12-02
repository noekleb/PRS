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
DEFINE VARIABLE cFillopnr AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dLagerDato AS DATE        NO-UNDO.
DEFINE VARIABLE iLagerTid  AS INTEGER     NO-UNDO.
DEFINE VARIABLE cKundNrHosBG AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dStartdatum AS DATE        NO-UNDO.
DEFINE VARIABLE cFullFilnamn AS CHARACTER   NO-UNDO.
DEFINE STREAM Ut.
DEFINE STREAM Kopi.

DEFINE TEMP-TABLE tt_but NO-UNDO
    FIELD butik       AS INTE
    FIELD fglasttrans AS INTE
    FIELD lasttrans   AS INTE
    FIELD paranr      AS INTE
    FIELD filid       AS CHAR
    FIELD kundnr      AS CHAR
    INDEX butik IS PRIMARY UNIQUE butik.

DEFINE TEMP-TABLE tt_brunn NO-UNDO                                        /*  Obligatoriskt                                    */
    FIELD Butik                 AS INTE /* CHAR */                             /*  20 Ja   Kundnr hos Brunngård "2014"     */
    FIELD Verifikation          AS CHAR /* CHAR */                             /*  20 Ja   Senaste transaktionsnummer      */
    FIELD Radnr                 AS CHAR /* INT  */ INIT "1         "           /*  10 Ja   Alltid 1.                       */
    FIELD KassaNr               AS CHAR /* CHAR */ INIT "          "           /*  10 Nej  Blank                           */
    FIELD ttid                  AS INTE /* INT  */                             /*   2 Ja    Transaktionstyp                */
    FIELD Bokfmall              AS CHAR /* CHAR */ INIT "          "           /*  10 Nej                                  */
    FIELD Varugrupp             AS CHAR /* CHAR */ INIT "          "           /*  10 Nej  Varugrupp                       */
    FIELD Nr                    AS CHAR /* CHAR */                             /*  20 Ja   Brunngårds artikel nr + ev. sto */
    FIELD Artikelnr             AS CHAR /* CHAR */                             /*  20 Ja   Brunngårds artikel nr   (1594-0 */
    FIELD Storlek               AS CHAR /* CHAR */                             /*   4 Ja   Storlek (37)                    */
    FIELD Orsakskod             AS CHAR /* CHAR */ INIT "    "                 /*   4 Nej  Blank                           */
    FIELD Kundensartikelnr      AS DECI /* CHAR */                             /*  20 Ja   PRS artikelnr                   */
    FIELD Antal                 AS DECI /* DECI */                             /*   5 Ja   Netto antal lagerrörelser under */
    FIELD OrdinarieprisInklmoms AS CHAR /* CHAR */ INIT "        0,00"         /*  12 Nej                                  */
    FIELD Rabatt%               AS CHAR /* DECI */ INIT "        0,00"         /*  12 Nej  Alltid 0.                       */
    FIELD RabattInklmoms        AS CHAR /* DECI */ INIT "        0,00"         /*  12 Nej  Alltid 0.                       */
    FIELD BeloppInklmoms        AS CHAR /* DECI */ INIT "        0,00"         /*  12 Nej  Alltid 0.                       */
    FIELD VarukostExklmoms      AS CHAR /* DECI */ INIT "        0,00"         /*  12 Nej  Alltid 0.                       */
    FIELD Datum                 AS DATE /* DATE */                             /*   8 Ja   Datum för transaktion           */
    FIELD Tid                   AS INTE /* Tid  */                             /*   8 Ja   Klockslag för senaste transakti */
    FIELD Leverantor            AS CHAR /* CHAR */ INIT "                    " /*  20 Nej                                  */
    FIELD Barcode               AS CHAR /* CHAR */ INIT "                    " /*  20 Nej  Streckkod                       */
    FIELD T1                    AS CHAR /* CHAR */ INIT "                    " /*  20 Nej  Framtida bruk                   */
    FIELD T2                    AS CHAR /* CHAR */ INIT "                    " /*  20 Nej  Framtida bruk                   */
    FIELD D1                    AS CHAR /* DECI */ INIT "       25,00"         /*  12 Ja   Momssats dvs 25% Ex.25          */
    FIELD D2                    AS CHAR /* DECI */ INIT "            "         /*  12 Nej  Framtida bruk                   */
    FIELD V1                    AS CHAR /* INT  */ INIT "  "                   /*   2 Nej  Framtida bruk                   */
    FIELD V2                    AS CHAR /* INT  */ INIT "  "                   /*   2 Nej  Framtida bruk                   */
    FIELD Flagga                AS CHAR /* CHAR */ INIT "Y"                    /*   1 Ja   Skall alltid vara ett "Y"       */
/*     FIELD Lagerantal            AS DECI /* DECI */                          /*  12 Ja   Utgående lagersaldo, nytt fält! */ */
    INDEX buttidkundartstr IS PRIMARY UNIQUE butik ttid Kundensartikelnr Storlek
    INDEX Kundensartikelnr butik Kundensartikelnr.
    .

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
         HEIGHT             = 14.1
         WIDTH              = 42.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEFINE VARIABLE iLoop     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCount    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lNoButik  AS LOGICAL   NO-UNDO.

dStartdatum = DATE(10,27,2014) - 1. /* om den int körts tidigare så skall vi hitta första transen större än dato */
dLagerDato  = DATE(TODAY).
iLagerTid   = TIME.

{syspara.i 1 9 3 cKundNrHosBG} 

{syspara.i 1 9 6 cButikkNr} 
  IF cButikkNr = "" THEN
    QUIT.

{syspara.i 1 9 7 pcLevNr} /* Leverantörsnummer. */
  IF pcLevNr = "" THEN
      QUIT.
  pcLevNr = REPLACE(pcLevNr,";",",").
  {syspara.i 1 9 8 cFillopnr} /* löpnr exportfil. */

cFillopnr = TRIM(STRING(INT(cFillopnr) + 1)).
{syspara.i 1 9 1 cKatalog}
IF cKatalog = "" THEN
    cKatalog = "c:\home\lindbak\sendes".

/*MESSAGE dFradato SKIP dTildato SKIP cbutikknr
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

/*MESSAGE NUM-ENTRIES(cButikkNr,";")
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

IF NUM-ENTRIES(cButikkNr,";") < 1 THEN
   QUIT.

{syspara.i 1 9 2 cTekst}

IF cTekst = "" THEN cTekst = "B".
    cFilNavn = cTekst.
    cFilNavn = cFilNavn + cKundNrHosBG + "00".

    cFilNavn = cFilNavn + FILL("0",7 - LENGTH(cFillopnr)) + cFillopnr + ".". 

    cKopi = cFilNavn.
    cKopi = cKopi + "csv".
    {syspara.i 1 9 5 cTekst}
    IF cTekst = "" THEN cTekst = "txt".
    cFilNavn = cFilNavn + cTekst.


RUN SkapaButikPoster.
RUN ByggData.
RUN ByggLager.
cFullFilnamn = cKatalog + "\" + cFilNavn.
RUN Exportera.

/* sätt tillbaka syspara */

FIND SysPara EXCLUSIVE-LOCK WHERE
     SysPara.SysHId = 1 AND 
     SysPara.SysGr  = 9 AND 
     SysPara.ParaNr = 8 NO-ERROR.
IF AVAILABLE SysPara THEN DO:
    ASSIGN SysPara.Parameter1 = cFillopnr.
    RELEASE SysPara.
END.

FOR EACH tt_but:
    FIND SysPara EXCLUSIVE-LOCK WHERE
         SysPara.SysHId = 1 AND 
         SysPara.SysGr  = 9 AND 
         SysPara.ParaNr = tt_but.paranr NO-ERROR.
    IF NOT AVAIL SysPara THEN DO:
        CREATE Syspara.
        ASSIGN SysPara.SysHId = 1
               SysPara.SysGr  = 9
               SysPara.ParaNr = tt_but.paranr
               SysPara.Parameter1 = STRING(tt_but.paranr).

    END.
    SysPara.Parameter2 = IF lasttrans = 0 THEN STRING(fglasttrans) ELSE STRING(lasttrans).
END.

/*      MESSAGE "Skickar fil "
          VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
IF SEARCH("cmd\brunngardftp.cmd") <> ? AND SEARCH(cFullFilnamn) <> ? THEN
    OS-COMMAND VALUE("cmd\brunngardftp.cmd" + " " + cFilNavn).

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggData Procedure 
PROCEDURE ByggData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   FOR EACH tt_but:
       FOR EACH translogg WHERE translogg.butik = tt_but.butik AND
                                translogg.transnr > tt_but.fglasttrans NO-LOCK:
           tt_but.lasttrans = translogg.transnr.
           IF NOT CAN-DO(pcLevNr,STRING(translogg.levnr)) THEN
               NEXT.
           DO:
               FIND artbas WHERE artbas.artikkelnr = translogg.artikkelnr NO-LOCK NO-ERROR.
               IF AVAIL artbas THEN DO:
                   FIND tt_brunn WHERE tt_brunn.butik = tt_but.butik and
                                       tt_brunn.ttid  = translogg.ttid AND
                                       tt_brunn.Kundensartikelnr = translogg.artikkelnr and
                                       tt_brunn.Storlek = TransLogg.Storl NO-ERROR.
                   IF NOT AVAIL tt_brunn THEN DO:
                       CREATE tt_brunn.
                       ASSIGN tt_brunn.Butik            = tt_but.butik
                              tt_brunn.ttid             = translogg.ttid
                              tt_brunn.Storlek          = TransLogg.Storl
                              tt_brunn.Kundensartikelnr = artbas.artikkelnr
                              tt_brunn.Nr               = SUBSTR(artbas.levkod,1,20 - LENGTH(TRIM(TransLogg.Storl)) - 1) + "-" + TRIM(TransLogg.Storl)
                              tt_brunn.Nr               = IF LENGTH(tt_brunn.Nr) < 20 THEN tt_brunn.Nr + FILL(" ",20 - LENGTH(tt_brunn.Nr)) ELSE tt_brunn.Nr
                              tt_brunn.Artikelnr        = TRIM(SUBSTR(artbas.levkod,1,20))
                              tt_brunn.Artikelnr        = IF LENGTH(tt_brunn.Artikelnr) < 20 THEN tt_brunn.Artikelnr + FILL(" ",20 - LENGTH(tt_brunn.Artikelnr)) ELSE tt_brunn.Artikelnr
                              tt_brunn.Verifikation = STRING(translogg.transnr)
                              tt_brunn.Verifikation = tt_brunn.Verifikation + FILL(" ",20 - LENGTH(tt_brunn.Verifikation))
                              tt_brunn.Antal        = translogg.antall
                              tt_brunn.Datum        = translogg.dato
                              tt_brunn.Tid          = translogg.tid
                              tt_brunn.Barcode      = translogg.kode + FILL(" ",20 - LENGTH(translogg.kode)).
                                  .
/*                        FIND artlag WHERE artlag.artikkelnr = artbas.artikkelnr AND                                                                                                           */
/*                                          artlag.storl      = translogg.storl   AND                                                                                                           */
/*                                          artlag.butik      = tt_but.butik NO-LOCK NO-ERROR.                                                                                                  */
/*                        IF AVAIL artlag AND artlag.lagant > 0 THEN DO:                                                                                                                        */
/*                            CREATE tt_brunn.                                                                                                                                                  */
/*                            ASSIGN tt_brunn.Butik            = tt_but.butik                                                                                                                   */
/*                                   tt_brunn.ttid             = 99                                                                                                                             */
/*                                   tt_brunn.Storlek          = TransLogg.Storl                                                                                                                */
/*                                   tt_brunn.Kundensartikelnr = artbas.artikkelnr                                                                                                              */
/*                                   tt_brunn.Nr               = SUBSTR(artbas.levkod,1,20 - LENGTH(TRIM(TransLogg.Storl)) - 1) + "-" + TRIM(TransLogg.Storl)                                   */
/*                                   tt_brunn.Artikelnr        = TRIM(SUBSTR(artbas.levkod,1,20))                                                                                               */
/*                                   tt_brunn.Artikelnr        = IF LENGTH(tt_brunn.Artikelnr) < 20 THEN tt_brunn.Artikelnr + FILL(" ",20 - LENGTH(tt_brunn.Artikelnr)) ELSE tt_brunn.Artikelnr */
/*                                   tt_brunn.Verifikation = "0"                                                                                                                                */
/*                                   tt_brunn.Antal        = artlag.lagant                                                                                                                      */
/*                                   tt_brunn.Datum        = dLagerDato                                                                                                                         */
/*                                   tt_brunn.Tid          = iLagerTid.                                                                                                                         */
/*                                         .                                                                                                                                                    */
/*                        END.                                                                                                                                                                  */
                   END.
                   ELSE
                       ASSIGN tt_brunn.Verifikation = STRING(translogg.transnr)
                              tt_brunn.Verifikation = tt_brunn.Verifikation + FILL(" ",20 - LENGTH(tt_brunn.Verifikation))
                              tt_brunn.Antal        = tt_brunn.Antal + translogg.antall
                              tt_brunn.Datum        = translogg.dato
                              tt_brunn.Tid          = translogg.tid.
               END.
           END.
       END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggDataOrg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggDataOrg Procedure 
PROCEDURE ByggDataOrg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   FOR EACH tt_but:
       FOR EACH translogg WHERE translogg.butik = tt_but.butik AND
                                translogg.transnr > tt_but.fglasttrans NO-LOCK:
           tt_but.lasttrans = translogg.transnr.
           IF NOT CAN-DO(pcLevNr,STRING(translogg.levnr)) THEN
               NEXT.
           IF CAN-DO("1,3,4,10",STRING(translogg.ttid)) THEN DO:
               FIND artbas WHERE artbas.artikkelnr = translogg.artikkelnr NO-LOCK NO-ERROR.
               IF AVAIL artbas THEN DO:
                   FIND tt_brunn WHERE tt_brunn.butik = tt_but.butik and
                                       tt_brunn.ttid  = translogg.ttid AND
                                       tt_brunn.Kundensartikelnr = translogg.artikkelnr and
                                       tt_brunn.Storlek = TransLogg.Storl NO-ERROR.
                   IF NOT AVAIL tt_brunn THEN DO:
                       CREATE tt_brunn.
                       ASSIGN tt_brunn.Butik            = tt_but.butik
                              tt_brunn.ttid             = translogg.ttid
                              tt_brunn.Storlek          = TransLogg.Storl
                              tt_brunn.Kundensartikelnr = artbas.artikkelnr
                              tt_brunn.Nr               = SUBSTR(artbas.levkod,1,20 - LENGTH(TRIM(TransLogg.Storl)) - 1) + "-" + TRIM(TransLogg.Storl)
                              tt_brunn.Nr               = IF LENGTH(tt_brunn.Nr) < 20 THEN tt_brunn.Nr + FILL(" ",20 - LENGTH(tt_brunn.Nr)) ELSE tt_brunn.Nr
                              tt_brunn.Artikelnr        = TRIM(SUBSTR(artbas.levkod,1,20))
                              tt_brunn.Artikelnr        = IF LENGTH(tt_brunn.Artikelnr) < 20 THEN tt_brunn.Artikelnr + FILL(" ",20 - LENGTH(tt_brunn.Artikelnr)) ELSE tt_brunn.Artikelnr
                              tt_brunn.Verifikation = STRING(translogg.transnr)
                              tt_brunn.Verifikation = tt_brunn.Verifikation + FILL(" ",20 - LENGTH(tt_brunn.Verifikation))
                              tt_brunn.Antal        = translogg.antall
                              tt_brunn.Datum        = translogg.dato
                              tt_brunn.Tid          = translogg.tid
                              tt_brunn.Barcode      = translogg.kode + FILL(" ",20 - LENGTH(translogg.kode)).
                                  .
/*                        FIND artlag WHERE artlag.artikkelnr = artbas.artikkelnr AND                                                                                                           */
/*                                          artlag.storl      = translogg.storl   AND                                                                                                           */
/*                                          artlag.butik      = tt_but.butik NO-LOCK NO-ERROR.                                                                                                  */
/*                        IF AVAIL artlag AND artlag.lagant > 0 THEN DO:                                                                                                                        */
/*                            CREATE tt_brunn.                                                                                                                                                  */
/*                            ASSIGN tt_brunn.Butik            = tt_but.butik                                                                                                                   */
/*                                   tt_brunn.ttid             = 99                                                                                                                             */
/*                                   tt_brunn.Storlek          = TransLogg.Storl                                                                                                                */
/*                                   tt_brunn.Kundensartikelnr = artbas.artikkelnr                                                                                                              */
/*                                   tt_brunn.Nr               = SUBSTR(artbas.levkod,1,20 - LENGTH(TRIM(TransLogg.Storl)) - 1) + "-" + TRIM(TransLogg.Storl)                                   */
/*                                   tt_brunn.Artikelnr        = TRIM(SUBSTR(artbas.levkod,1,20))                                                                                               */
/*                                   tt_brunn.Artikelnr        = IF LENGTH(tt_brunn.Artikelnr) < 20 THEN tt_brunn.Artikelnr + FILL(" ",20 - LENGTH(tt_brunn.Artikelnr)) ELSE tt_brunn.Artikelnr */
/*                                   tt_brunn.Verifikation = "0"                                                                                                                                */
/*                                   tt_brunn.Antal        = artlag.lagant                                                                                                                      */
/*                                   tt_brunn.Datum        = dLagerDato                                                                                                                         */
/*                                   tt_brunn.Tid          = iLagerTid.                                                                                                                         */
/*                                         .                                                                                                                                                    */
/*                        END.                                                                                                                                                                  */
                   END.
                   ELSE
                       ASSIGN tt_brunn.Verifikation = STRING(translogg.transnr)
                              tt_brunn.Verifikation = tt_brunn.Verifikation + FILL(" ",20 - LENGTH(tt_brunn.Verifikation))
                              tt_brunn.Antal        = tt_brunn.Antal + translogg.antall
                              tt_brunn.Datum        = translogg.dato
                              tt_brunn.Tid          = translogg.tid.
               END.
           END.
       END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggLager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggLager Procedure 
PROCEDURE ByggLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
    DO ii = 1 TO NUM-ENTRIES(pcLevNr):
        FOR EACH artbas WHERE artbas.levnr = INT(ENTRY(ii,pcLevNr)) NO-LOCK.
            IF artbas.lopnr = ? THEN
                NEXT.
            FOR EACH tt_but:
                FOR EACH artlag WHERE artlag.butik = tt_but.butik AND
                                      artlag.artikkelnr = artbas.artikkelnr NO-LOCK:
/*                                       artlag.vg    = artbas.vg    AND      */
/*                                       artlag.lopnr = artbas.lopnr NO-LOCK. */
                    IF artlag.lagant > 0 THEN DO:
                        FIND FIRST strekkode WHERE strekkode.artikkelnr = artlag.artikkelnr AND 
                                                   strekkode.strkode    = artlag.strkode    AND
                                                   strekkode.HovedNr    = TRUE              AND 
                                               NOT strekkode.kode BEGINS "02" NO-LOCK NO-ERROR.
                        IF NOT AVAIL strekkode THEN DO:
                            FIND FIRST strekkode WHERE strekkode.artikkelnr = artlag.artikkelnr AND 
                                                       strekkode.strkode    = artlag.strkode    AND
                                                   NOT strekkode.kode BEGINS "02" NO-LOCK NO-ERROR.
                            IF NOT AVAIL strekkode THEN DO:
                                FIND FIRST strekkode WHERE strekkode.artikkelnr = artlag.artikkelnr AND 
                                                       strekkode.strkode    = artlag.strkode NO-LOCK NO-ERROR.
                            END.
                        END.
/*                         FIND FIRST strekkode WHERE strekkode.artikkelnr = artlag.artikkelnr AND                */
/*                                                    strekkode.strkode    = artlag.strkode    AND                */
/*                                                NOT strekkode.kode BEGINS "02" NO-LOCK NO-ERROR.                */
/*                         IF NOT AVAIL strekkode THEN DO:                                                        */
/*                             FIND FIRST strekkode WHERE strekkode.artikkelnr = artlag.artikkelnr AND            */
/*                                                        strekkode.strkode    = artlag.strkode NO-LOCK NO-ERROR. */
/*                         END.                                                                                   */
                        CREATE tt_brunn.
                        ASSIGN tt_brunn.Butik            = tt_but.butik
                               tt_brunn.ttid             = 99
                               tt_brunn.Storlek          = artlag.Storl
                               tt_brunn.Kundensartikelnr = artbas.artikkelnr
                               tt_brunn.Nr               = SUBSTR(artbas.levkod,1,20 - LENGTH(TRIM(artlag.Storl)) - 1) + "-" + TRIM(artlag.Storl)
                               tt_brunn.Nr               = IF LENGTH(tt_brunn.Nr) < 20 THEN tt_brunn.Nr + FILL(" ",20 - LENGTH(tt_brunn.Nr)) ELSE tt_brunn.Nr
                               tt_brunn.Artikelnr        = TRIM(SUBSTR(artbas.levkod,1,20))
                               tt_brunn.Artikelnr        = IF LENGTH(tt_brunn.Artikelnr) < 20 THEN tt_brunn.Artikelnr + FILL(" ",20 - LENGTH(tt_brunn.Artikelnr)) ELSE tt_brunn.Artikelnr
                               tt_brunn.Verifikation = "0"
                               tt_brunn.Antal        = artlag.lagant
                               tt_brunn.Datum        = dLagerDato
                               tt_brunn.Tid          = iLagerTid
                               tt_brunn.Barcode      = IF AVAIL strekkode THEN strekkode.kode + FILL(" ",20 - LENGTH(strekkode.kode)) ELSE FILL(" ",20).

                    END.
                END.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksportBongVaga) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportBongVaga Procedure 
PROCEDURE EksportBongVaga :
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

&IF DEFINED(EXCLUDE-Exportera) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Exportera Procedure 
PROCEDURE Exportera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cDato  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTTid  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iAntal AS INTEGER     NO-UNDO.
DEFINE VARIABLE cEXPttid AS CHAR      NO-UNDO.        /* Transtyp ändras och läggs i detta fältet för export */
OUTPUT STREAM Ut   TO VALUE(cFullFilnamn).
    FOR EACH tt_brunn:
        iAntal = tt_brunn.antal.
        CASE tt_brunn.TTId:
            WHEN  1 THEN DO:
                cEXPttid = "03". /* Varesalg */
                iAntal = iAntal * -1. /* Varesalg rapporteras som negativt */
            END.
            WHEN  2 THEN cEXPttid = IF iAntal >= 0 THEN "05" ELSE "06". /* Brekasje */
            WHEN  3 THEN DO:
                cEXPttid = IF iAntal >= 0 THEN "03" ELSE "04". /* Kundereklamasjon */
                iAntal = iAntal * -1. /* Lagerreklamation rapporteras som negativt */
            END.
            WHEN  4 THEN DO:
                cEXPttid = "06". /* Lagerreklamasjon */
                iAntal = iAntal * -1. /* Lagerreklamation rapporteras som negativt */
            END.
            WHEN  5 THEN cEXPttid = IF iAntal >= 0 THEN "01" ELSE "02". /* Varekjøp */
            WHEN  6 THEN cEXPttid = IF iAntal >= 0 THEN "05" ELSE "06". /* Overføring */
            WHEN  7 THEN cEXPttid = IF iAntal >= 0 THEN "05" ELSE "06". /* Lagerjustering */
            WHEN  8 THEN cEXPttid = IF iAntal >= 0 THEN "05" ELSE "06". /* Nedskrivning */
            WHEN  9 THEN cEXPttid = IF iAntal >= 0 THEN "05" ELSE "06". /* Svinn */
            WHEN 10 THEN DO: 
                cEXPttid = "04". /* Gjenkjøp */
                iAntal = iAntal * -1. /* Gjenköp rapporteras som positivt */
            END.
            WHEN 11 THEN cEXPttid = IF iAntal >= 0 THEN "05" ELSE "06". /* Internt forbruk */
            WHEN 99 THEN cEXPttid = "99".
        END CASE.


        cDato = SUBSTR(STRING(YEAR(Datum),"9999"),3,2) + "-" + STRING(MONTH(Datum),"99") + "-" + STRING(DAY(Datum),"99").
        PUT STREAM Ut UNFORMATTED
        STRING(tt_brunn.butik) + FILL(" ",20 - LENGTH(STRING(tt_brunn.butik)))    /*  20 Ja   Kundnr hos Brunngård "2014"     */
        Verifikation + FILL(" ", 20 - LENGTH(Verifikation))    /*  20 Ja   Senaste transaktionsnummer      */
        Radnr                                                  /*  10 Ja   Alltid 1.                       */
        KassaNr                                                /*  10 Nej  Blank                           */
/*         STRING(ttid,"99")                                      /*   2 Ja    Transaktionstyp                */ */
        cEXPttid                                               /*   2 Ja    Transaktionstyp                */
        Bokfmall                                               /*  10 Nej                                  */
        Varugrupp                                              /*  10 Nej  Varugrupp                       */
        Nr                                                     /*  20 Ja   Brunngårds artikel nr + ev. sto */
        Artikelnr                                              /*  20 Ja   Brunngårds artikel nr   (1594-0 */
        STRING(Storlek,"x(4)")                                 /*   4 Ja   Storlek (37)                    */
        Orsakskod                                              /*   4 Nej  Blank                           */
        string(Kundensartikelnr) + FILL(" ",20 - LENGTH(STRING(Kundensartikelnr))) /*  20 Ja   PRS artikelnr                   */
        FILL(" ",5 - LENGTH(STRING(iAntal))) + STRING(iAntal)    /*   5 Ja   Netto antal lagerrörelser under */
        OrdinarieprisInklmoms                                  /*  12 Nej                                  */
        Rabatt%                                                /*  12 Nej  Alltid 0.                       */
        RabattInklmoms                                         /*  12 Nej  Alltid 0.                       */
        BeloppInklmoms                                         /*  12 Nej  Alltid 0.                       */
        VarukostExklmoms                                       /*  12 Nej  Alltid 0.                       */
        cDato                                                  /*   8 Ja   Datum för transaktion           */
        STRING(Tid,"HH:MM:SS")                                 /*   8 Ja   Klockslag för senaste transakti */
        Leverantor                                             /*  20 Nej                                  */
        Barcode                                                /*  20 Nej  Streckkod                       */
        T1                                                     /*  20 Nej  Framtida bruk                   */
        T2                                                     /*  20 Nej  Framtida bruk                   */
        D1                                                     /*  12 Ja   Momssats dvs 25% Ex.25          */
        D2                                                     /*  12 Nej  Framtida bruk                   */
        V1                                                     /*   2 Nej  Framtida bruk                   */
        V2                                                     /*   2 Nej  Framtida bruk                   */
        Flagga                                                 /*   1 Ja   Skall alltid vara ett "Y"       */
            SKIP.
    END.
OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExporteraOrg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExporteraOrg Procedure 
PROCEDURE ExporteraOrg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cDato  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTTid  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iAntal AS INTEGER     NO-UNDO.
OUTPUT STREAM Ut   TO VALUE(cFullFilnamn).
    FOR EACH tt_brunn:
        cDato = SUBSTR(STRING(YEAR(Datum),"9999"),3,2) + "-" + STRING(MONTH(Datum),"99") + "-" + STRING(DAY(Datum),"99").
        PUT STREAM Ut UNFORMATTED
        STRING(tt_brunn.butik) + FILL(" ",20 - LENGTH(STRING(tt_brunn.butik)))    /*  20 Ja   Kundnr hos Brunngård "2014"     */
        Verifikation + FILL(" ", 20 - LENGTH(Verifikation))    /*  20 Ja   Senaste transaktionsnummer      */
        Radnr                                                  /*  10 Ja   Alltid 1.                       */
        KassaNr                                                /*  10 Nej  Blank                           */
        STRING(ttid,"99")                                      /*   2 Ja    Transaktionstyp                */
        Bokfmall                                               /*  10 Nej                                  */
        Varugrupp                                              /*  10 Nej  Varugrupp                       */
        Nr                                                     /*  20 Ja   Brunngårds artikel nr + ev. sto */
        Artikelnr                                              /*  20 Ja   Brunngårds artikel nr   (1594-0 */
        STRING(Storlek,"x(4)")                                 /*   4 Ja   Storlek (37)                    */
        Orsakskod                                              /*   4 Nej  Blank                           */
        string(Kundensartikelnr) + FILL(" ",20 - LENGTH(STRING(Kundensartikelnr))) /*  20 Ja   PRS artikelnr                   */
        FILL(" ",5 - LENGTH(STRING(Antal))) + STRING(Antal)    /*   5 Ja   Netto antal lagerrörelser under */
        OrdinarieprisInklmoms                                  /*  12 Nej                                  */
        Rabatt%                                                /*  12 Nej  Alltid 0.                       */
        RabattInklmoms                                         /*  12 Nej  Alltid 0.                       */
        BeloppInklmoms                                         /*  12 Nej  Alltid 0.                       */
        VarukostExklmoms                                       /*  12 Nej  Alltid 0.                       */
        cDato                                                  /*   8 Ja   Datum för transaktion           */
        STRING(Tid,"HH:MM:SS")                                 /*   8 Ja   Klockslag för senaste transakti */
        Leverantor                                             /*  20 Nej                                  */
        Barcode                                                /*  20 Nej  Streckkod                       */
        T1                                                     /*  20 Nej  Framtida bruk                   */
        T2                                                     /*  20 Nej  Framtida bruk                   */
        D1                                                     /*  12 Ja   Momssats dvs 25% Ex.25          */
        D2                                                     /*  12 Nej  Framtida bruk                   */
        V1                                                     /*   2 Nej  Framtida bruk                   */
        V2                                                     /*   2 Nej  Framtida bruk                   */
        Flagga                                                 /*   1 Ja   Skall alltid vara ett "Y"       */
            SKIP.
    END.
OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaButikposter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaButikposter Procedure 
PROCEDURE SkapaButikposter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DEFINE VARIABLE pcKundNr AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iFGlasttrans AS INTEGER     NO-UNDO.
DO ii = 1 TO NUM-ENTRIES(cButikkNr,";"):
    CREATE tt_but.
    ASSIGN tt_but.butik  = INT(ENTRY(ii,cButikkNr,";"))
           tt_but.paranr = 1000 + tt_but.butik.
END.

FOR EACH tt_but:
    {syspara.i 1 9 tt_but.paranr pcKundeNr}
    tt_but.kundnr = IF pcKundeNr = "" THEN STRING(tt_but.paranr) ELSE pcKundNr.
      FIND SysPara EXCLUSIVE-LOCK WHERE SysPara.SysHId = 1 AND
                                        SysPara.SysGr  = 9 AND
                                        SysPara.ParaNr = tt_but.paranr NO-ERROR.
      IF AVAILABLE SysPara THEN DO:
        ASSIGN iFGlasttrans = INT(SysPara.Parameter2) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            ASSIGN iFGlasttrans = 0.
        ASSIGN tt_but.fglasttrans = iFGlasttrans.
      END.
      IF tt_but.fglasttrans = 0 THEN DO:
          FIND FIRST translogg WHERE translogg.butik = tt_but.butik AND
                                     translogg.dato > dStartdatum NO-LOCK NO-ERROR.
          IF AVAIL translogg THEN
              tt_but.fglasttrans = translogg.transnr - 1.
      END.
END.

/* DEFINE TEMP-TABLE tt_but NO-UNDO                                                       */
/*     FIELD fglasttrans AS INTE                                                          */
/*     FIELD lasttrans   AS INTE                                                          */
/*     FIELD paranr      AS CHAR                                                          */
/*     FIELD filid       AS CHAR                                                          */
/*     FIELD kundnr      AS CHAR                                                          */
/*     INDEX butikknr IS PRIMARY UNIQUE butikknr.                                         */
/*                                                                                        */
/*   IF lNoButik = FALSE THEN                                                             */
/*     DO:                                                                                */
/*       FIND SysPara EXCLUSIVE-LOCK WHERE                                                */
/*         SysPara.SysHId = 1 AND                                                         */
/*         SysPara.SysGr  = 9 AND                                                         */
/*         SysPara.ParaNr = iParaButik NO-ERROR.                                          */
/*       IF AVAILABLE SysPara THEN                                                        */
/*       DO:                                                                              */
/*         ASSIGN iLoop = INT(ENTRY(1,SysPara.Parameter2,";"))                            */
/*                iTransNr3 = INT(ENTRY(2,SysPara.Parameter2,";")).                       */
/*         ASSIGN iLoop = iLoop + 1.                                                      */
/*         ASSIGN SysPara.Parameter2 = STRING(iLoop,"9999999") + ";" + STRING(iTransNr3). */
/*         RELEASE SysPara.                                                               */
/* /*        MESSAGE "iLoop " iLoop SKIP "iTransNr3 " iTransNr3                           */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.*/                                       */
/*       END.                                                                             */
/*     END.                                                                               */
/*   IF lNoButik = FALSE THEN                                                             */
/*   DO:                                                                                  */
/*     {syspara.i 1 9 2 cTekst}                                                           */
/*     IF cTekst = "" THEN cTekst = "B".                                                  */
/*     cFilNavn = cTekst.                                                                 */
/*     cFilNavn = cFilNavn + pcKundeNr.                                                   */
/*                                                                                        */
/*     cFilNavn = cFilNavn + STRING(iLoop,"9999999") + ".".                               */
/*                                                                                        */
/*     cKopi = cFilNavn.                                                                  */
/*     cKopi = cKopi + "csv".                                                             */
/*     {syspara.i 1 9 5 cTekst}                                                           */
/*     IF cTekst = "" THEN cTekst = "txt".                                                */
/*     cFilNavn = cFilNavn + cTekst.                                                      */
/*                                                                                        */
/*                                                                                        */





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

