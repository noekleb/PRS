&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
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

DEFINE INPUT  PARAMETER iButikknr    AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER cTyp         AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER dKordre_Id   AS DECIMAL     NO-UNDO.
DEFINE INPUT  PARAMETER cSelger      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cIdNr        AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER lcHode       AS LONGCHAR    NO-UNDO.
DEFINE OUTPUT PARAMETER cUtskrift    AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER lOK          AS LOGICAL     NO-UNDO.

DEFINE VARIABLE cSingleHW AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDoubleW  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDoubleH  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDoubleHW AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBold     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRight    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCenter   AS CHARACTER   NO-UNDO.

ASSIGN cSingleHW = CHR(27) + "|1C" /* dessa sätts till blankt om printer inte stöder */
       cDoubleW  = CHR(27) + "|2C"
       cDoubleH  = CHR(27) + "|3C"
       cDoubleHW = CHR(27) + "|4C"
       cBold     = CHR(27) + "|bC"
       cRight    = CHR(27) + "|cA"
       cCenter   = CHR(27) + "|rA".

DEFINE TEMP-TABLE ttKOrderList NO-UNDO
    FIELD KOrdre_Id   AS DECI DECIMALS 0
    FIELD EkstOrdreNr AS CHAR
    FIELD Namn        AS CHAR
    FIELD Plockad     AS LOG
    INDEX KI IS PRIMARY UNIQUE KOrdre_Id.


/* DEFINE TEMP-TABLE ttKorderLinje NO-UNDO */
/*     FIELD KOrdre_Id AS DECI             */
/*     FIELD Kode AS CHAR                  */
/*     INDEX KI IS PRIMARY KOrdre_Id.      */

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

CASE cTyp:
    WHEN "LIST" THEN DO:
        RUN GetList.
        IF CAN-FIND(FIRST ttKOrderList) THEN DO:
            TEMP-TABLE ttKOrderList:WRITE-JSON("longchar", lcHode, FALSE).
            EMPTY TEMP-TABLE ttKOrderList.
        END.
    END.
    WHEN "DELIVER" THEN DO:
        RUN Leverera.
    END.
    WHEN "MAKULERA" THEN DO:
        RUN Makulera.
    END.

END CASE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-bku) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bku Procedure 
PROCEDURE bku :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE cStr AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dBrutto AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dRabatt AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dMoms   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dNetto  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cVarenr AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cVaretekst AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFarge AS CHARACTER   NO-UNDO.
FIND kordrehode WHERE kordre_id = dKordre_Id NO-LOCK.
/* DH=dubbel höjd  S2=Skip2*/
cUtskrift = 
              ";" + cDoubleH + "           LEVERERAD WEBORDER" + CHR(1) + 
              ";" + cDoubleH + "              ORDERÖVERSIKT" + CHR(1) + 
              ";" + "Order: " + STRING(Kordrehode.kordre_id) + "/" + KOrdreHode.EkstOrdreNr  + CHR(1) +
              ";" + "       " + STRING(KOrdreHode.RegistrertDato)  + CHR(1) +
               ";" + KOrdreHode.SendingsNr   + CHR(1) +
               "S2;" + CHR(1) +
                ";" + KOrdreHode.Navn   + CHR(1) +
                ";" + KOrdreHode.LevAdresse1  + CHR(1) +
                ";" + KOrdreHode.LevPostNr  + " " + KOrdreHode.LevPostSted   + CHR(1) +
                ";" + REPLACE(Kordrehode.MobilTlf," ","")  + CHR(1) +
                ";" + TRIM(Kordrehode.ePostAdresse)  + CHR(1) +
                "S2;" + CHR(1).

FOR EACH KOrdreLinje OF KOrdrehode NO-LOCK:
   IF KOrdreLinje.antall = 0 OR KOrdreLinje.Varenr MATCHES "*BETALT*" THEN
       NEXT.
   IF KOrdreLinje.Varetekst MATCHES "*FRAKT*" THEN
       NEXT.
   ELSE DO:
       FIND artbas WHERE artbas.artikkelnr = DECI(KOrdreLinje.VareNr) NO-LOCK NO-ERROR.
       IF AVAIL artbas THEN DO:
           FIND farg WHERE farg.farg = artbas.farg NO-LOCK NO-ERROR.
           IF AVAIL farg THEN
               cFarge = STRING(farg.farbeskr).
           cVarenr = STRING(artbas.vg) + "/" + IF artbas.lopnr <> ? THEN STRING(artbas.lopnr) ELSE "".
           IF artbas.vmid > 0 THEN DO:
               FIND varemerke OF artbas NO-LOCK NO-ERROR.
               IF AVAIL varemerke AND TRIM(Varemerke.Beskrivelse) <> "" THEN
                   cVaretekst = TRIM(Varemerke.Beskrivelse).
           END.
       END.
       ELSE DO:
           cVarenr = KOrdreLinje.VareNr.
       END.

       ASSIGN dBrutto = dBrutto + KOrdreLinje.antall * KOrdreLinje.bruttopris
              dRabatt = dRabatt + (KOrdreLinje.Linjerabatt * -1)
              dMoms   = dMoms   + KOrdreLinje.MVaKr 
              dNetto  = dNetto  + KOrdreLinje.nettolinjesum.
/*        cFarge = KOrdreLinje.LevFargKod. */
       cVaretekst = KOrdreLinje.Varetekst.
       cStr = "Str. " + KOrdreLinje.Storl + "  " + STRING(KOrdreLinje.nettolinjesum,"->>,>>9.99").
       cStr = FILL(" ", 40 - LENGTH(cStr)) + cStr.
       cUtskrift = cUtskrift + "; " + STRING(cVarenr,"x(11)") + STRING(cFarge,"x(10)") + STRING(cVaretekst,"x(19)") + CHR(1) +
       ";" + cStr + CHR(1) +
       "S2;" + CHR(1).
   END.
END.
cUtskrift = cUtskrift + 
                ";                  Brutto      " + STRING(dBrutto,"->>,>>9.99") + CHR(1) +
                ";                  Rabatt      " + STRING(dRabatt,"->>,>>9.99") + CHR(1) +
                ";                  Summa       " + STRING(dNetto,"->>,>>9.99") + CHR(1) +
                ";                  Varav moms  " + STRING(dMoms,"->>,>>9.99") + CHR(1) +
                "S2;".
cUtskrift = cUtskrift + CHR(2) + ";" + cDoubleH + "          ****BUTIKENS EX****" + CHR(1) + cUtskrift + CHR(1) +
            "S2;" + CHR(1) +
            "S2;" + CHR(1) +
            ";Datum  ______________" + CHR(1) +
            "S2;" + CHR(1) +
            ";________________________________________" + CHR(1) +
            ";            Kundens signatur" + CHR(1) +
            "S2;" + CHR(1).
/* OUTPUT TO "C:\tmp\cetc.txt".        */
/*     PUT UNFORMATTED cUtskrift SKIP. */
/* OUTPUT CLOSE.                       */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixaUtskrift) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixaUtskrift Procedure 
PROCEDURE FixaUtskrift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE cStr AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dBrutto AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dRabatt AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dMoms   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dNetto  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cVarenr AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cVaretekst AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFarge AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTid AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMtxt AS CHARACTER   NO-UNDO.
FIND kordrehode WHERE kordre_id = dKordre_Id NO-LOCK.

FIND kunde OF kordrehode NO-LOCK NO-ERROR.
IF AVAIL kunde THEN DO:
    FIND medlem OF kunde NO-LOCK NO-ERROR.
    IF AVAIL medlem AND Medlem.MKlubbId = 3 THEN
        cMtxt = "Medlem i Johanssons Kundklubb" + CHR(1).
END.

/* DH=dubbel höjd  S2=Skip2*/
cUtskrift =  (IF cTyp = "MAKULERA" THEN 
              cDoubleH + "       ****   ORDER MAKULERAD   ****" + CHR(1) + CHR(1) + 
              cDoubleH + "       ****   ORDER MAKULERAD   ****" + CHR(1) + 
              "S1" + CHR(1)
              ELSE "") +                    
              "" + cDoubleH + "             LEVERANSKVITTO" + CHR(1) + 
              "" + cDoubleH + "                WEBORDER" + CHR(1) + 
              "S2" + CHR(1) +
              "" + " Order     : " + STRING(Kordrehode.kordre_id) + "/" + KOrdreHode.EkstOrdreNr  + CHR(1) +
              "" + " Orderdatum: " + STRING(KOrdreHode.RegistrertDato)  + CHR(1) +
               "" + " " + KOrdreHode.SendingsNr   + CHR(1) +
               "S2" + CHR(1) +
                " " + KOrdreHode.Navn   + CHR(1) +
                " " + "ID: " + cIdNr + CHR(1) + cMtxt + 
                " " + KOrdreHode.LevAdresse1  + CHR(1) +
                " " + KOrdreHode.LevPostNr  + " " + KOrdreHode.LevPostSted   + CHR(1) +
                " " + REPLACE(Kordrehode.MobilTlf," ","")  + CHR(1) +
                " " + TRIM(Kordrehode.ePostAdresse)  + CHR(1) +
                "S1" + CHR(1) +
                " Säljare: " + cSelger  + CHR(1) +
                "S2" + CHR(1).
FIND FIRST KOrdrelinje OF KOrdreHode WHERE KOrdreLinje.VareNr MATCHES "*BETALT*"  NO-LOCK NO-ERROR.
IF AVAIL KOrdreLinje THEN DO:
    cUtskrift = cUtskrift + " " + KOrdreLinje.Varetekst + CHR(1) +
               "S2" + CHR(1).
END.
FOR EACH KOrdreLinje OF KOrdrehode NO-LOCK:
   IF KOrdreLinje.antall = 0 OR KOrdreLinje.Varenr MATCHES "*BETALT*" THEN
       NEXT.
   IF KOrdreLinje.Varetekst MATCHES "*FRAKT*" THEN
       NEXT.
   ELSE DO:
       FIND artbas WHERE artbas.artikkelnr = DECI(KOrdreLinje.VareNr) NO-LOCK NO-ERROR.
       IF AVAIL artbas THEN DO:
           FIND farg WHERE farg.farg = artbas.farg NO-LOCK NO-ERROR.
           IF AVAIL farg THEN
               cFarge = STRING(farg.farbeskr).
           cVarenr = STRING(artbas.vg) + "/" + IF artbas.lopnr <> ? THEN STRING(artbas.lopnr) ELSE "".
           IF artbas.vmid > 0 THEN DO:
               FIND varemerke OF artbas NO-LOCK NO-ERROR.
               IF AVAIL varemerke AND TRIM(Varemerke.Beskrivelse) <> "" THEN
                   cVaretekst = TRIM(Varemerke.Beskrivelse).
           END.
       END.
       ELSE DO:
           cVarenr = KOrdreLinje.VareNr.
       END.

       ASSIGN dBrutto = dBrutto + KOrdreLinje.antall * KOrdreLinje.bruttopris
              dRabatt = dRabatt + (KOrdreLinje.Linjerabatt * -1)
              dMoms   = dMoms   + KOrdreLinje.MVaKr 
              dNetto  = dNetto  + KOrdreLinje.nettolinjesum.
/*        cFarge = KOrdreLinje.LevFargKod. */
       cVaretekst = KOrdreLinje.Varetekst.
       cStr = "Str. " + KOrdreLinje.Storl + "  " + STRING(KOrdreLinje.nettolinjesum,"->>,>>9.99").
       cStr = FILL(" ", 40 - LENGTH(cStr)) + cStr.
       cUtskrift = cUtskrift + " " + STRING(cVarenr,"x(11)") + STRING(cFarge,"x(10)") + STRING(cVaretekst,"x(19)") + CHR(1) +
       "" + cStr + CHR(1) +
       "S2" + CHR(1).
   END.
END.
cUtskrift = cUtskrift + 
                "                  Brutto      " + STRING(dBrutto,"->>,>>9.99") + CHR(1) +
                "                  Rabatt      " + STRING(dRabatt,"->>,>>9.99") + CHR(1) +
                "                  Summa       " + STRING(dNetto,"->>,>>9.99") + CHR(1) +
                "                  Varav moms  " + STRING(dMoms,"->>,>>9.99") + CHR(1) +
                "S2" + CHR(1).
IF cTyp = "DELIVER" THEN DO:
    FIND FIRST bonghode WHERE bonghode.kordre_id = KOrdreHode.Kordre_Id NO-LOCK NO-ERROR.
    cTid = SUBSTR(STRING(TIME,"HH:MM:SS"),1,5).
    cUtskrift = cUtskrift + 
/*                 (IF AVAIL bonghode THEN                           */
/*                 "SK;" + STRING(BongHode.b_id) + CHR(1) ELSE "") + */

                " But:" + STRING(iButikknr) + " Kv:" + STRING(bonghode.bongnr) + " Utlämn.: " + STRING(TODAY) + " " + cTid + CHR(1) +
                "SK;" + STRING(KOrdreHode.Kordre_Id) + CHR(1) +
                " Spara kvittot för ev retur."  + CHR(1) +
                " Öppet köp 14 dagar från"  + CHR(1) +
                " mottagningsdatum i valfri" + CHR(1) +
                " Johanssons Skor butik." + CHR(1) +
                " För mer information se" + CHR(1) + 
                " www.johanssons.se" + CHR(1) +
                CHR(2) + "" + cDoubleH + "          ****BUTIKENS EX****" + CHR(1) + cUtskrift + CHR(1) +
                "" + cDoubleH + "           BEKRÄFTAT MOTTAGET" + CHR(1) + 
                "S2" + CHR(1) +
                "Datum  ______________" + CHR(1) +
                "S2" + CHR(1) +
                "________________________________________" + CHR(1) +
                "            Kundens signatur" + CHR(1) +
                " " + "ID: " + cIdNr + CHR(1) + 
                "S2" + CHR(1).
END.
ELSE
    cUtskrift = cUtskrift + 
        "" + cDoubleH + "       ****   ORDER MAKULERAD   ****" + CHR(1) + CHR(1) + 
        "" + cDoubleH + "       ****   ORDER MAKULERAD   ****" + CHR(1) + 
        "S2" + CHR(1).

/* OUTPUT TO "C:\tmp\cetc.txt".        */
/*     PUT UNFORMATTED cUtskrift SKIP. */
/* OUTPUT CLOSE.                       */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetList Procedure 
PROCEDURE GetList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* DEFINE INPUT  PARAMETER iButikknr    AS INTEGER     NO-UNDO. */
/* DEFINE OUTPUT PARAMETER lcHode       AS LONGCHAR    NO-UNDO. */
DEFINE VARIABLE lPlockat AS LOG INIT TRUE  NO-UNDO.
FOR EACH KOrdreHode WHERE KOrdreHode.Levstatus = "30" AND KOrdreHode.butik = iButikknr NO-LOCK.
/* FIND kordrehode WHERE kordrehode.kordre_id = 1190000040 NO-LOCK. */
/* DO:                                                              */
    lPlockat = TRUE.
    CREATE ttKOrderList.
    ASSIGN ttKOrderList.KOrdre_Id    = KOrdreHode.KOrdre_Id  
           ttKOrderList.EkstOrdreNr  = KOrdreHode.EkstOrdreNr
           ttKOrderList.Namn         = KOrdreHode.Navn.
    FOR EACH KOrdreLinje OF KOrdreHode WHERE KOrdreLinje.plukkbutikk = iButikknr NO-LOCK.
        IF KOrdreLinje.plukkbutikk = 0 OR KOrdreLinje.antall = 0 THEN
            NEXT.
        IF KOrdreLinje.plockstatus <> 2 THEN DO:
            lPlockat = FALSE.
            LEAVE.
        END.
    END.
    ttKOrderList.plockad = lPlockat.
/*     cUtskrift = STRING(lPlockat,"Jäkligt bra/Åt helvete"). */
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Leverera) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Leverera Procedure 
PROCEDURE Leverera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ocReturn AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE plDec AS DECIMAL     NO-UNDO.
/* IF dKordre_id = 1190000040 THEN DO: */
/*     RUN FixaUtskrift.               */
/*     lOK = TRUE.                     */
/* END.                                */
    FOR EACH KOrdreLinje WHERE KOrdreLinje.KOrdre_id = dKordre_Id EXCLUSIVE-LOCK:
/*       plDec = 0.                                                */
/*       ASSIGN plDec = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.      */
/*       IF ERROR-STATUS:ERROR THEN                                */
/*         ERROR-STATUS:ERROR = FALSE.                             */
/*       IF plDec > 0 AND CAN-FIND(ArtBas WHERE                    */
/*                                 ArtBas.ArtikkelNr = plDec) THEN */
        KOrdreLinje.Leveringsdato  = TODAY.
    END.
    RELEASE KOrdreLinje.
    RUN kordre_kontant.p (STRING(dKordre_Id),?,"",OUTPUT ocReturn,OUTPUT lOK).
    IF lOK THEN
        RUN FixaUtskrift.
    ELSE
        cUtskrift = ocReturn.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Makulera) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Makulera Procedure 
PROCEDURE Makulera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND KOrdreHode WHERE KOrdreHode.Kordre_Id = dKordre_id EXCLUSIVE NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        cUtskrift = "Fel vid makulering".
        RETURN.
    END.
    RUN opprett_overforingsordre.p(dKordre_Id,TRUE).
    FOR EACH kordrelinje WHERE kordrelinje.kordre_id = dKordre_id AND KOrdrelinje.plockstatus > 0 USE-INDEX FaktLinje:
        KOrdrelinje.plockstatus = 0.
    END.
    ASSIGN KOrdreHode.Levstatus = "60".
    FIND CURRENT KOrdreHode NO-LOCK.
    RUN FixaUtskrift.
    lOk = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

