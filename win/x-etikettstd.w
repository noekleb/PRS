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
DEFINE INPUT  PARAMETER wDefault      AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER ipcPrinter    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iplTermklient AS LOGICAL    NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR wCl          AS INT  NO-UNDO.
DEF VAR wSeqNr       AS INT  NO-UNDO.
DEF VAR wLayout      AS CHAR NO-UNDO.
DEF VAR wBatch       AS CHAR NO-UNDO.
DEF VAR wEtikett_Fil AS CHAR NO-UNDO.
DEF VAR wFirma       AS CHAR NO-UNDO.
DEF VAR Linje        AS CHAR EXTENT 50.
DEF VAR BLinje       AS CHAR EXTENT 50.
DEF VAR TStorlek     AS CHAR FORMAT "x(4)".
DEF VAR Teller       AS INT  NO-UNDO.

/* Diverse flagg. */
DEF VAR wLayout_Flagg  AS LOG  NO-UNDO.
DEF VAR i              AS INT  NO-UNDO.
DEF VAR y              AS INT  NO-UNDO.
DEF VAR wwBatch        AS CHAR NO-UNDO.
DEF VAR wValgtSkriver  AS INT  NO-UNDO.
DEF VAR wParaListe     AS CHAR NO-UNDO.
DEF VAR wKommando      AS CHAR NO-UNDO.

DEFINE VARIABLE cEtikettSkriverPara2 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cWinPrinterName      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lTermKlient          AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cOrgSysPr            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lPrinterPreDef       AS LOGICAL    NO-UNDO.
{etikettlogg.i}
{xprint.i}
{runlib.i}

DEF BUFFER clButiker FOR Butiker.

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
         HEIGHT             = 21.24
         WIDTH              = 67.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
  DEF VAR wP700 AS CHAR NO-UNDO.
  DEF VAR cNya  AS CHAR NO-UNDO.


  cNya = "S4M,TIME_2_11,Sport1-1,Meto-Normal,METO-mn-4,GANT_Pris_under,METO_Pris_under,BV_Normalpris_PU,BV_PU,BV_Piggy_Back_PU,ECE4_Pris_under,Medlem_Meto,Medlem_ECE4" +
         ",HH_Ark_33,HH_Ark_332,TIME_3_5,TIME_Plakat,TIME_11_2".

  /* !!!!! TEST !!!!! */
  IF ipcPrinter <> "" THEN
      ASSIGN cWinPrinterName = ipcPrinter
             lTermKLient     = iplTermklient
             lPrinterPreDef  = TRUE.
     
  
  ASSIGN
    wValgtSkriver = wDefault.
  /* Henter layout. */
/*   IF valid-handle(wLibHandle) THEN                       */
/*       RUN GetEtikettLayout IN wLibHandle (OUTPUT wP700). */
/*   IF wP700 = "?" OR                                      */
/*      wP700 = "" THEN                                     */
  {syspar2.i 5 20 wValgtSkriver wP700}  

  /* Sentrallager */
  {syspara.i 5 1 1 wCl INT}
  FIND clbutiker NO-LOCK WHERE
    clButiker.Butik = wCl NO-ERROR.
  IF NOT AVAILABLE clButiker THEN
    DO:
      MESSAGE "Sentrallager er ikke lagt opp!"
        VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY.
    END.
    
  /* Her skal hentes profilnr fra den butikken det skrives ut etiketter fra lager på. */  
    FIND FIRST EtikettLogg NO-ERROR.
    IF AVAIL Etikettlogg THEN DO:
        FIND butiker WHERE butiker.butik = EtikettLogg.Butik NO-LOCK NO-ERROR.
    END.
    IF NOT AVAIL butiker THEN
        FIND butiker NO-LOCK WHERE
            Butiker.Butik = wCl NO-ERROR.

  {syspara.i 1  1 100 wFirma}
  {syspara.i 5 20 wDefault wEtikett_Fil}
  IF wEtikett_Fil = "" THEN
    DO:
      MESSAGE "Ugyldig parameteroppsett." SKIP
              "Etikettfilnavn på skriver " wDefault " er blank."
              VIEW-AS ALERT-BOX ERROR TITLE "Utskriftsfeil".
      RETURN NO-APPLY.
    END.
  {syspar2.i 5 21 wValgtSkriver cEtikettSkriverPara2}
  IF NOT lPrinterPreDef THEN DO:
      DO Teller = 1 TO NUM-ENTRIES(cEtikettSkriverPara2,";"):
          IF ENTRY(Teller,cEtikettSkriverPara2,";") BEGINS "PRINTERNAME" THEN DO:
              ASSIGN cWinPrinterName = ENTRY(Teller,cEtikettSkriverPara2,";").
              LEAVE.
          END.
      END.
      IF NUM-ENTRIES(cWinPrinterName,"=") = 2 THEN DO:
          ASSIGN cWinPrinterName = ENTRY(2,cWinPrinterName,"=").
      END.
      ELSE 
          ASSIGN cWinPrinterName = "".
    
      IF SESSION:PARAMETER <> "" AND CAN-DO(SESSION:PARAMETER,"TERMKLIENT") THEN
          ASSIGN lTermKlient = TRUE.
  END.
  IF NOT CAN-DO(cNya,wP700) THEN 
DO:
    RUN SlettINFO.
    RUN FixStorl.
END.
  
  CASE wP700:
    WHEN "RFIDStd" THEN
      DO:
        RUN RFIDStd.
      END.
    WHEN "P700" THEN
      DO:
        RUN UtP700.
      END.
    WHEN "P700-2" THEN
      DO:
        RUN UtP700-2.
      END.
    WHEN "MONARK-1" THEN /* Svan Skodon AB */
      DO:
        RUN Init-MONARK-1.
        RUN SendLayout.
        RUN Utskrift-MONRK-1.
        RUN SendFil.
      END.
    WHEN "MONARK-2" THEN /* RK Mode AB (Rolans) */
      DO:
        RUN Init-MONARK-2.
        RUN SendLayout.
        RUN Utskrift-MONRK-2.
        RUN SendFil.
      END.
    WHEN "M-Euro" THEN /* Euroskor */
      DO:
        RUN Init-Euro.
        RUN SendLayout.
        RUN Utskrift.
        RUN SendFil.
      END.
    WHEN "M-Skoteket" THEN /* skoteket */
      DO:
        RUN Init-Skoteket.
        RUN SendLayout.
        RUN Utskrift.
        RUN SendFil.
      END.
    WHEN "M-Blank" THEN /* Euroskor */
      DO:
        RUN Init-euroblank.
        RUN SendLayout.
        RUN Utskrift.
        RUN SendFil.
      END.
    WHEN "Sport1-1" THEN DO:
        RUN FixEAN. /* Finner strekkode till EtikettLogg där storleken är angitt */
        RUN Utskrift-Sport1-1.
    END.
    WHEN "SKO-1" THEN DO:
        RUN FixEAN. /* Finner strekkode till EtikettLogg där storleken är angitt */
        RUN Utskrift-SKO-1.
    END.
    WHEN "HH_Ark_33" THEN DO:
        RUN FixEAN. /* Finner strekkode till EtikettLogg där storleken är angitt */
        RUN Utskrift_HH_Ark_33.
    END.
    WHEN "HH_Ark_332" THEN DO:
        RUN FixEAN. /* Finner strekkode till EtikettLogg där storleken är angitt */
        RUN Utskrift_HH_Ark_332.
    END.
    WHEN "METO-mn-4" OR WHEN "METO_Pris_under" OR WHEN "GANT_Pris_under" OR WHEN "BV_Normalpris_PU" OR WHEN "BV_PU" OR WHEN "BV_Piggy_Back_PU" 
                     OR WHEN "Meto-Normal" THEN DO:
        RUN FixEAN. /* Finner strekkode till EtikettLogg där storleken är angitt */
        IF NOT lPrinterPreDef THEN DO:
          RUN getWinPrinterName.
          IF RETURN-VALUE = "AVBRYT" THEN
              RETURN.
        END.
        RUN VALUE(wP700).
    END.
      WHEN "ECE4_Pris_under" THEN
      DO:
        RUN FixEAN. /* Finner strekkode till EtikettLogg där storleken är angitt */
        IF NOT lPrinterPreDef THEN DO:
          RUN getWinPrinterName.
          IF RETURN-VALUE = "AVBRYT" THEN
              RETURN.
        END.
        RUN Utskrift-ECE4_Pris_under.
      END.
      WHEN "Medlem_Meto" THEN
      DO:
        RUN Medlem_Meto.
      END.
      WHEN "Medlem_ECE4" THEN
      DO:
        RUN Medlem_ECE4.
      END.
      WHEN "TIME_2_11" THEN
      DO:
        RUN Etikett_TIME_2_11.p.
      END.
      WHEN "TIME_3_5" THEN
      DO:
        RUN Etikett_TIME_3_5.p.
      END.
      WHEN "TIME_Plakat" THEN
      DO:
        RUN Etikett_TIME_Plakat.p.
      END.
      WHEN "TIME_11_2" THEN
      DO:
        RUN Etikett_TIME_11_2.p.
      END.
      WHEN "S4M" OR WHEN "S4MA" OR WHEN "DM3_L" THEN
      DO:
          RUN FixEAN. /* Finner strekkode till EtikettLogg där storleken är angitt */
          RUN FixStorl.
          IF NOT lPrinterPreDef THEN DO:
            RUN getWinPrinterName.
            IF RETURN-VALUE = "AVBRYT" THEN
                RETURN.
          END.
        RUN Etikett_S4M.p  (wP700,cWinPrinterName,lTermKLient).
      END.
      WHEN "MARKPOINT_0" OR WHEN "MARKPOINT_1" OR WHEN "MARKPOINT_2" OR WHEN "MARKPOINT_L" OR WHEN "MARKPOINT_LD" THEN
      DO:
/*           RUN FixEAN. /* Finner strekkode till EtikettLogg där storleken är angitt */ */
          IF NOT lPrinterPreDef THEN DO:
            RUN getWinPrinterName.
            IF RETURN-VALUE = "AVBRYT" THEN
                RETURN.
          END.
        RUN Etikett_Markpoint.p (wP700,cWinPrinterName,lTermKLient).
      END.

    OTHERWISE 
      DO:
        RUN Initiering.
        RUN SendLayout.
        RUN Utskrift.
        RUN SendFil.
      END.
  END CASE.
  IF cOrgSysPr <> "" THEN
    ASSIGN SESSION:PRINTER-NAME = cOrgSysPr.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BV_Normalpris_PU) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BV_Normalpris_PU Procedure 
PROCEDURE BV_Normalpris_PU :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dePris     AS DECIMAL                        NO-UNDO.
  DEFINE VARIABLE iKr        AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren      AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iRowEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iRow       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cRow       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCol       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cCol       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iColEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iEtikettnr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount     AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iCount2    AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE dPris2     AS DECIMAL  DECIMALS 2  NO-UNDO.
  DEFINE VARIABLE wOK        AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFormat AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cColKronor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColOren   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cInfoEtiTxt AS CHARACTER EXTENT 4 NO-UNDO.
  DEFINE VARIABLE cPrisKr   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrisOren AS CHARACTER  NO-UNDO.
  ASSIGN cColKronor = "200,200,200,185,160"
         cColOren   = "240,275,310,330,340".
  /* används för att kunna veta startetikett: Ant = startetikett */
  FOR EACH EtikettLogg WHERE EtikettLogg.SeqNr = 0 AND
                             EtikettLogg.Storl = "STARTETIKETT":
      DELETE EtikettLogg.
  END.
/*   IF SESSION:PARAMETER <> "" THEN DO iCount = 1 TO NUM-ENTRIES(SESSION:PARAMETER): */
/*       IF ENTRY(iCount,SESSION:PARAMETER) BEGINS "EPRINTER" AND                     */
/*          NUM-ENTRIES(ENTRY(iCount,SESSION:PARAMETER),"=") = 2 THEN DO:             */
/*          ASSIGN cWinPrinterName = ENTRY(2,ENTRY(iCount,SESSION:PARAMETER),"=").    */
/*          LEAVE.                                                                    */
/*       END.                                                                         */
/*   END.                                                                             */
/*   IF cWinPrinterName = "" THEN DO:                                                 */
/*       SYSTEM-DIALOG PRINTER-SETUP UPDATE wOK.                                      */
/*       IF NOT wOK THEN                                                              */
/*           RETURN NO-APPLY.                                                         */
/*   END.                                                                             */
/*   ELSE DO:                                                                         */
/*       ASSIGN cOrgSysPr = SESSION:PRINTER-NAME.                                     */
/*   END.                                                                             */
  /* hämta printernamn om cWinPrinterName = "" */
  IF cWinPrinterName = "" THEN DO:
      IF lTermKlient = TRUE THEN
          OUTPUT TO VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
      ELSE
          OUTPUT TO PRINTER VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
/*       OUTPUT TO PRINTER CONVERT TARGET "IBM850". */
  END.
  ELSE DO:
      IF lTermKlient = TRUE THEN
          OUTPUT TO VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      ELSE DO:
          ASSIGN SESSION:PRINTER-NAME = cWinPrinterName.
          OUTPUT TO PRINTER VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      END.
  END.
/*   ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "M0400" + CHR(2) + "O0000". /* m=metric */ */
  ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "M0600" + CHR(2) + "O0000". /* m=metric */
  PUT CONTROL cFormat.

  FOR EACH EtikettLogg WHERE EtikettLogg.Storl <> "":
      IF NOT EtikettLogg.Storl = "INFO" THEN DO:
          FIND Strekkode WHERE Strekkode.kode = EtikettLogg.Storl NO-LOCK NO-ERROR.
          FIND ArtBas OF StrekKode NO-LOCK.
          FIND Farg OF Artbas NO-LOCK NO-ERROR.

          FIND ArtPris OF Artbas NO-LOCK WHERE 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
            FIND ArtPris OF Artbas NO-LOCK WHERE 
              ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.

          FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
          /* Barneverden skall ha kalkylens pris */
          ASSIGN iKr   = TRUNC(ArtPris.Pris[1],0)
                 iOren = (ArtPris.Pris[1] - TRUNC(ArtPris.Pris[1],0)) * 100.
/*           ASSIGN iKr   = TRUNC(EtikettLogg.Pris,0)                             */
/*                  iOren = (EtikettLogg.Pris - TRUNC(EtikettLogg.Pris,0)) * 100. */
      END.
      IF EtikettLogg.Storl = "INFO" THEN DO:
          ASSIGN cInfoEtiTxt = "".
          DO iCount = 1 TO NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)):
              IF iCount = 5 THEN
                  LEAVE.
              ASSIGN cInfoEtiTxt[iCount] = ENTRY(iCount,Etikettlogg.Bongtekst,CHR(1)).
          END.
          PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "R0000" CHR(13) "D11" CHR(13)
                             "121100802100020" cInfoEtiTxt[1] CHR(13)
                             "121100801500020" cInfoEtiTxt[2] CHR(13)
                             "121100801000020" cInfoEtiTxt[3] CHR(13)
                             "121100800150020" cInfoEtiTxt[4] CHR(13)
                                                          "E" CHR(13).
      END.
      ELSE IF StrekKode.KodeType = 0 THEN
          PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
                             "191100202000020" ArtBas.Bongtekst CHR(13)
                             "1a0000000650080" TRIM(StrekKode.Kode) CHR(13)
                             "191100300150100" "PLU: " TRIM(StrekKode.Kode) CHR(13)
                          SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
      ELSE DO:
          /*  NYTT Vi har ett Etikettlogg.pris2 som visar ordinarie pris */
          IF NOT ArtBas.OPris THEN
              /* Barneverden vill inte ha tilbudspris ut på etikett */
              ASSIGN dPris2 = 0.
/*               ASSIGN dPris2 = IF Etikettlogg.pris2 > 0 THEN Etikettlogg.Pris2 ELSE  */
/*                               IF ArtPris.Tilbud = TRUE THEN ArtPris.Pris[1] ELSE 0. */
          ELSE 
              ASSIGN dPris2 = 0.
          IF ArtBas.OPris THEN
              ASSIGN cPrisKr   = ""
                     cPrisOren = "".
          ELSE
              ASSIGN cPrisKr   = SUBSTITUTE("191100600200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) + TRIM(STRING(iKr,">>>>9")) + CHR(13)
                     cPrisOren = SUBSTITUTE("191100400400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) + TRIM(STRING(iOren,"99")) + CHR(13).
          PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
             "191100202100020" ArtBas.Bongtekst CHR(13)
             "191100201800020" (IF ArtBas.StrTypeId > 2 AND AVAIL StrKonv THEN TRIM(StrKonv.Storl) + " " ELSE "") 
                         /*(IF AVAIL Farg AND Farg.FarBeskr <> "" THEN Farg.FarBeskr ELSE ArtBas.LevFargKod) CHR(13)*/
                         (IF ArtBas.LevFargKod <> '' THEN ArtBas.LevFargKod ELSE IF AVAIL Farg THEN Farg.FarBeskr ELSE '' ) CHR(13)
             "1F2206500900065" TRIM(StrekKode.Kode) CHR(13)
/*                  "491100300900050" STRING(ArtBas.Vg) CHR(13) */
          /*  NYTT  */
          (IF dPris2 > 0 THEN 
               "191100200300020" + "(" + TRIM(STRING(dPris2,">>>>9.99")) + ")" + CHR(13) ELSE "")
                 cPrisKr  
                 cPrisOren
/*                           SUBSTITUTE("191100600200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) TRIM(STRING(iKr,">>>>9")) CHR(13) */
/*                           SUBSTITUTE("191100400400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) TRIM(STRING(iOren,"99")) CHR(13)    */
                 (IF TRIM(ArtBas.LevKod) <> "" THEN
                            "121100200000020   L-NR:" + TRIM(ArtBas.LevKod) + CHR(13) ELSE "")
                          SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
      END.
  END.
  PUT UNFORMATTED CHR(2) "F".
  OUTPUT CLOSE.
/*   IF cOrgSysPr <> "" THEN                      */
/*       ASSIGN SESSION:PRINTER-NAME = cOrgSysPr. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BV_Piggy_Back_PU) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BV_Piggy_Back_PU Procedure 
PROCEDURE BV_Piggy_Back_PU :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dePris     AS DECIMAL                        NO-UNDO.
  DEFINE VARIABLE iKr        AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren      AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iRowEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iRow       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cRow       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCol       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cCol       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iColEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iEtikettnr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount     AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iCount2    AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE dPris2     AS DECIMAL  DECIMALS 2  NO-UNDO.
  DEFINE VARIABLE wOK        AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFormat AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cColKronor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColOren   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColKronor_2 AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColOren_2   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cInfoEtiTxt AS CHARACTER EXTENT 4 NO-UNDO.
  DEFINE VARIABLE cPrisKr   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrisOren AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrisKr_2   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrisOren_2 AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iParaNr   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEtiParNr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTime     AS INTEGER    NO-UNDO.
  ASSIGN cColKronor = "200,200,200,185,160"
         cColOren   = "240,275,310,330,340"
         cColKronor_2 = "600,600,600,585,560"
         cColOren_2   = "640,675,710,730,740"
      .
  /* används för att kunna veta startetikett: Ant = startetikett */
  IF CAN-FIND(FIRST EtikettLogg WHERE Etikettlogg.individ > 0) THEN DO:
      MESSAGE "Etikett for individ finnes, PiggyBack kann ikke brukes."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
  /* Här skall vi se vilken sys */
  FIND Bruker WHERE Bruker.BrukerID = USERID("skotex") NO-LOCK NO-ERROR.
  IF Bruker.Butikknr > 0 AND CAN-FIND(Butiker WHERE Butiker.Butik = Bruker.ButikkNr) THEN 
       ASSIGN iParaNr = Bruker.Butikknr.
   ELSE
       ASSIGN iParaNr = wCL.

  FIND SysPara WHERE SysPara.SysHId = 5 AND
                     SysPara.SysGr  = 23 AND
                     SysPara.ParaNr = iParaNr EXCLUSIVE NO-ERROR NO-WAIT.
  IF NOT LOCKED SysPara AND NOT AVAIL SysPara THEN DO:
      CREATE SysPara.
      ASSIGN SysPara.SysHId = 5 
             SysPara.SysGr  = 23 
             SysPara.ParaNr = iParaNr
             SysPara.Beskrivelse = "Butikkparameter parnummer"
             SysPara.Parameter1  = "0".
      FIND CURRENT Syspara EXCLUSIVE NO-ERROR NO-WAIT.
  END.
  ASSIGN iTime = TIME.
  IF LOCKED SysPara THEN REPEAT:
      FIND SysPara WHERE SysPara.SysHId = 5 AND
                         SysPara.SysGr  = 23 AND
                         SysPara.ParaNr = iParaNr EXCLUSIVE NO-ERROR NO-WAIT.
      IF AVAIL SysPara THEN
          LEAVE.
      IF TIME - iTime > 10 THEN 
          LEAVE.
  END.
  IF AVAIL SysPara THEN
      ASSIGN iEtiParNr = INT(SysPara.Parameter1).
  ELSE
      ASSIGN iEtiParNr = INT(SUBSTR(STRING(TIME,"99999"),3)).
  FOR EACH EtikettLogg WHERE EtikettLogg.SeqNr = 0 AND
                             EtikettLogg.Storl = "STARTETIKETT":
      DELETE EtikettLogg.
  END.
/*   IF SESSION:PARAMETER <> "" THEN DO iCount = 1 TO NUM-ENTRIES(SESSION:PARAMETER): */
/*       IF ENTRY(iCount,SESSION:PARAMETER) BEGINS "EPRINTER" AND                     */
/*          NUM-ENTRIES(ENTRY(iCount,SESSION:PARAMETER),"=") = 2 THEN DO:             */
/*          ASSIGN cWinPrinterName = ENTRY(2,ENTRY(iCount,SESSION:PARAMETER),"=").    */
/*          LEAVE.                                                                    */
/*       END.                                                                         */
/*   END.                                                                             */
/*   IF cWinPrinterName = "" THEN DO:                                                 */
/*       SYSTEM-DIALOG PRINTER-SETUP UPDATE wOK.                                      */
/*       IF NOT wOK THEN                                                              */
/*           RETURN NO-APPLY.                                                         */
/*   END.                                                                             */
/*   ELSE DO:                                                                         */
/*       ASSIGN cOrgSysPr = SESSION:PRINTER-NAME.                                     */
/*   END.                                                                             */
  /* hämta printernamn om cWinPrinterName = "" */
  IF cWinPrinterName = "" THEN DO:
      IF lTermKlient = TRUE THEN
          OUTPUT TO VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
      ELSE
          OUTPUT TO PRINTER VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
/*       OUTPUT TO PRINTER CONVERT TARGET "IBM850". */
  END.
  ELSE DO:
      IF lTermKlient = TRUE THEN
          OUTPUT TO VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      ELSE DO:
          ASSIGN SESSION:PRINTER-NAME = cWinPrinterName.
          OUTPUT TO PRINTER VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      END.
  END.
/*   ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "M0400" + CHR(2) + "O0000". /* m=metric */ */
  ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "M1200" + CHR(2) + "O0000". /* m=metric */
  PUT CONTROL cFormat.

  FOR EACH EtikettLogg WHERE EtikettLogg.Storl <> "":
      IF NOT EtikettLogg.Storl = "INFO" THEN DO:
          FIND Strekkode WHERE Strekkode.kode = EtikettLogg.Storl NO-LOCK NO-ERROR.
          FIND ArtBas OF StrekKode NO-LOCK.
          FIND Farg OF Artbas NO-LOCK NO-ERROR.

          FIND ArtPris OF Artbas NO-LOCK WHERE 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
            FIND ArtPris OF Artbas NO-LOCK WHERE 
              ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.

          FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
          /* Barneverden skall ha kalkylens pris */
          ASSIGN iKr   = TRUNC(ArtPris.Pris[1],0)
                 iOren = (ArtPris.Pris[1] - TRUNC(ArtPris.Pris[1],0)) * 100.
/*           ASSIGN iKr   = TRUNC(EtikettLogg.Pris,0)                             */
/*                  iOren = (EtikettLogg.Pris - TRUNC(EtikettLogg.Pris,0)) * 100. */
      END.
      IF EtikettLogg.Storl = "INFO" THEN DO:
          ASSIGN cInfoEtiTxt = "".
          DO iCount = 1 TO NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)):
              IF iCount = 5 THEN
                  LEAVE.
              ASSIGN cInfoEtiTxt[iCount] = ENTRY(iCount,Etikettlogg.Bongtekst,CHR(1)).
          END.
          PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "R0000" CHR(13) "D11" CHR(13)
                             "121100802100020" cInfoEtiTxt[1] CHR(13)
                             "121100801500020" cInfoEtiTxt[2] CHR(13)
                             "121100801000020" cInfoEtiTxt[3] CHR(13)
                             "121100800150020" cInfoEtiTxt[4] CHR(13)
                                                          "E" CHR(13).
      END.
      ELSE IF StrekKode.KodeType = 0 THEN
      PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
                         "191100202000020" ArtBas.Bongtekst CHR(13)
                         "1a0000000650080" TRIM(StrekKode.Kode) CHR(13)
                         "191100300150100" "PLU: " TRIM(StrekKode.Kode) CHR(13)
                      SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
      ELSE DO iCount = 1 TO EtikettLogg.Ant:
          /*  NYTT Vi har ett Etikettlogg.pris2 som visar ordinarie pris */
          ASSIGN iEtiParNr = iEtiParNr + 1.
          IF NOT ArtBas.OPris THEN
          /* Barneverden vill inte ha tilbudspris ut på etikett */
              ASSIGN dPris2 = 0.
/*               ASSIGN dPris2 = IF Etikettlogg.pris2 > 0 THEN Etikettlogg.Pris2 ELSE  */
/*                               IF ArtPris.Tilbud = TRUE THEN ArtPris.Pris[1] ELSE 0. */
          ELSE 
              ASSIGN dPris2 = 0.
          /* För att inte få in ordinarie pris vid inleverans när varan står på tilbud */
              IF ArtBas.OPris THEN
                  ASSIGN cPrisKr   = ""
                         cPrisOren = "".
              ELSE
                  ASSIGN cPrisKr   = SUBSTITUTE("191100600200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) + TRIM(STRING(iKr,">>>>9")) + CHR(13)
                         cPrisOren = SUBSTITUTE("191100400400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) + TRIM(STRING(iOren,"99")) + CHR(13)
                         cPrisKr_2   = SUBSTITUTE("191100600200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor_2)) + TRIM(STRING(iKr,">>>>9")) + CHR(13)
                         cPrisOren_2 = SUBSTITUTE("191100400400&1",ENTRY(LENGTH(STRING(iKr)),cColOren_2)) + TRIM(STRING(iOren,"99")) + CHR(13)
                      .
          PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
                /* 1 */ "191100202100020" ArtBas.Bongtekst CHR(13) 
                /* 2 */ "191100202100420" ArtBas.Bongtekst CHR(13)
                /* 1 */ "191100201800020" (IF ArtBas.StrTypeId > 2 AND AVAIL StrKonv THEN TRIM(StrKonv.Storl) + " " ELSE "") 
                                /*(IF AVAIL Farg AND Farg.FarBeskr <> "" THEN Farg.FarBeskr ELSE ArtBas.LevFargKod) CHR(13)*/
                                (IF ArtBas.LevFargKod <> '' THEN ArtBas.LevFargKod ELSE IF AVAIL Farg THEN Farg.FarBeskr ELSE '' ) CHR(13)
                    /* 2 */ "191100201800420" (IF ArtBas.StrTypeId > 2 AND AVAIL StrKonv THEN TRIM(StrKonv.Storl) + " " ELSE "") 
                                /*(IF AVAIL Farg AND Farg.FarBeskr <> "" THEN Farg.FarBeskr ELSE ArtBas.LevFargKod) CHR(13)*/
                                (IF ArtBas.LevFargKod <> '' THEN ArtBas.LevFargKod ELSE IF AVAIL Farg THEN Farg.FarBeskr ELSE '' ) CHR(13)
                    /* 1 */ "1F2206500900065" TRIM(StrekKode.Kode) CHR(13)
                    /* 2 */ "1F2206500900465" TRIM(StrekKode.Kode) CHR(13)
/*       /* 1 */ "491100300900050" STRING(ArtBas.Vg) CHR(13) */
/*       /* 2 */ "491100300900450" STRING(ArtBas.Vg) CHR(13) */

              /*  NYTT  */
          (IF dPris2 > 0 THEN 
               "191100200300020" + "(" + TRIM(STRING(dPris2,">>>>9.99")) + ")" + CHR(13) ELSE "")
                    /* 1 */    cPrisKr  
                 cPrisOren
                    /* 2 */    cPrisKr_2
                 cPrisOren_2
                    /*                           SUBSTITUTE("191100600200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) TRIM(STRING(iKr,">>>>9")) CHR(13) */
                    /*                           SUBSTITUTE("191100400400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) TRIM(STRING(iOren,"99")) CHR(13)    */
                    /* 1 */ ("491100200150050" + STRING(iParaNr) + "-" + (IF NOT AVAIL SysPara THEN "R" ELSE "") + STRING(iEtiParNr) + CHR(13))
                    /* 2 */ ("491100200150450" + STRING(iParaNr) + "-" + (IF NOT AVAIL SysPara THEN "R" ELSE "") + STRING(iEtiParNr) + CHR(13))
                    /* 1 */      (IF TRIM(ArtBas.LevKod) <> "" THEN
                   "121100200000020   L-NR:" + TRIM(ArtBas.LevKod) + CHR(13) ELSE "")
                    /* 2 */      (IF TRIM(ArtBas.LevKod) <> "" THEN
                   "121100200000420   L-NR:" + TRIM(ArtBas.LevKod) + CHR(13) ELSE "")
                          SUBSTITUTE("Q&1",STRING(1,"9999")) CHR(13) "E" CHR(13)
              .
      END.
  END.
  PUT UNFORMATTED CHR(2) "F".
  OUTPUT CLOSE.
/*   IF cOrgSysPr <> "" THEN                      */
/*       ASSIGN SESSION:PRINTER-NAME = cOrgSysPr. */
  IF AVAIL SysPara THEN DO:
      ASSIGN SysPara.Parameter1  = STRING(iEtiParNr).
      RELEASE SysPara.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BV_PU) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BV_PU Procedure 
PROCEDURE BV_PU :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dePris     AS DECIMAL                        NO-UNDO.
  DEFINE VARIABLE iKr        AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren      AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iRowEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iRow       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cRow       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCol       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cCol       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iColEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iEtikettnr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount     AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iCount2    AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE dPris2     AS DECIMAL  DECIMALS 2  NO-UNDO.
  DEFINE VARIABLE wOK        AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFormat AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cColKronor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColOren   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cInfoEtiTxt AS CHARACTER EXTENT 4 NO-UNDO.
  DEFINE VARIABLE cPrisKr   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrisOren AS CHARACTER  NO-UNDO.
  ASSIGN cColKronor = "200,200,200,185,160"
         cColOren   = "240,275,310,330,340".
  /* används för att kunna veta startetikett: Ant = startetikett */
  FOR EACH EtikettLogg WHERE EtikettLogg.SeqNr = 0 AND
                             EtikettLogg.Storl = "STARTETIKETT":
      DELETE EtikettLogg.
  END.
/*   IF SESSION:PARAMETER <> "" THEN DO iCount = 1 TO NUM-ENTRIES(SESSION:PARAMETER): */
/*       IF ENTRY(iCount,SESSION:PARAMETER) BEGINS "EPRINTER" AND                     */
/*          NUM-ENTRIES(ENTRY(iCount,SESSION:PARAMETER),"=") = 2 THEN DO:             */
/*          ASSIGN cWinPrinterName = ENTRY(2,ENTRY(iCount,SESSION:PARAMETER),"=").    */
/*          LEAVE.                                                                    */
/*       END.                                                                         */
/*   END.                                                                             */
/*   IF cWinPrinterName = "" THEN DO:                                                 */
/*       SYSTEM-DIALOG PRINTER-SETUP UPDATE wOK.                                      */
/*       IF NOT wOK THEN                                                              */
/*           RETURN NO-APPLY.                                                         */
/*   END.                                                                             */
/*   ELSE DO:                                                                         */
/*       ASSIGN cOrgSysPr = SESSION:PRINTER-NAME.                                     */
/*   END.                                                                             */
  /* hämta printernamn om cWinPrinterName = "" */
  IF cWinPrinterName = "" THEN DO:
      IF lTermKlient = TRUE THEN
          OUTPUT TO VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
      ELSE
          OUTPUT TO PRINTER VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
/*       OUTPUT TO PRINTER CONVERT TARGET "IBM850". */
  END.
  ELSE DO:
      IF lTermKlient = TRUE THEN
          OUTPUT TO VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      ELSE DO:
          ASSIGN SESSION:PRINTER-NAME = cWinPrinterName.
          OUTPUT TO PRINTER VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      END.
  END.
/*   ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "M0400" + CHR(2) + "O0000". /* m=metric */ */
  ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "M0600" + CHR(2) + "O0000". /* m=metric */
  PUT CONTROL cFormat.

  FOR EACH EtikettLogg WHERE EtikettLogg.Storl <> "":
      IF NOT EtikettLogg.Storl = "INFO" THEN DO:
          FIND Strekkode WHERE Strekkode.kode = EtikettLogg.Storl NO-LOCK NO-ERROR.
          FIND ArtBas OF StrekKode NO-LOCK.
          FIND Farg OF Artbas NO-LOCK NO-ERROR.

          FIND ArtPris OF Artbas NO-LOCK WHERE 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
            FIND ArtPris OF Artbas NO-LOCK WHERE 
              ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.

          FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
          ASSIGN iKr   = TRUNC(EtikettLogg.Pris,0)
                 iOren = (EtikettLogg.Pris - TRUNC(EtikettLogg.Pris,0)) * 100.
      END.
      IF EtikettLogg.Storl = "INFO" THEN DO:
          ASSIGN cInfoEtiTxt = "".
          DO iCount = 1 TO NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)):
              IF iCount = 5 THEN
                  LEAVE.
              ASSIGN cInfoEtiTxt[iCount] = ENTRY(iCount,Etikettlogg.Bongtekst,CHR(1)).
          END.
          PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "R0000" CHR(13) "D11" CHR(13)
                             "121100802100020" cInfoEtiTxt[1] CHR(13)
                             "121100801500020" cInfoEtiTxt[2] CHR(13)
                             "121100801000020" cInfoEtiTxt[3] CHR(13)
                             "121100800150020" cInfoEtiTxt[4] CHR(13)
                                                          "E" CHR(13).
      END.
      ELSE IF StrekKode.KodeType = 0 THEN
      PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
                         "191100202000020" ArtBas.Bongtekst CHR(13)
                         "1a0000000650080" TRIM(StrekKode.Kode) CHR(13)
                         "191100300150100" "PLU: " TRIM(StrekKode.Kode) CHR(13)
                      SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
      ELSE DO:
          /*  NYTT Vi har ett Etikettlogg.pris2 som visar ordinarie pris */
          IF NOT ArtBas.OPris THEN
              ASSIGN dPris2 = IF Etikettlogg.pris2 > 0 THEN Etikettlogg.Pris2 ELSE
                              IF ArtPris.Tilbud = TRUE THEN ArtPris.Pris[1] ELSE 0.
          ELSE 
              ASSIGN dPris2 = 0.
          /* För att inte få in ordinarie pris vid inleverans när varan står på tilbud */
          IF EtikettLogg.Pris = dPris2 THEN
             ASSIGN dPris2 = 0.
          IF ArtBas.OPris THEN
              ASSIGN cPrisKr   = ""
                     cPrisOren = "".
          ELSE
              ASSIGN cPrisKr   = SUBSTITUTE("191100600200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) + TRIM(STRING(iKr,">>>>9")) + CHR(13)
                     cPrisOren = SUBSTITUTE("191100400400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) + TRIM(STRING(iOren,"99")) + CHR(13).
         PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
             "191100202100020" ArtBas.Bongtekst CHR(13)
             "191100201800020" (IF ArtBas.StrTypeId > 2 AND AVAIL StrKonv THEN TRIM(StrKonv.Storl) + " " ELSE "") 
                         /*(IF AVAIL Farg AND Farg.FarBeskr <> "" THEN Farg.FarBeskr ELSE ArtBas.LevFargKod) CHR(13)*/
                         (IF ArtBas.LevFargKod <> '' THEN ArtBas.LevFargKod ELSE IF AVAIL Farg THEN Farg.FarBeskr ELSE '' ) CHR(13)
             "1F2206500900065" TRIM(StrekKode.Kode) CHR(13)
/*              "491100300900050" STRING(ArtBas.Vg) CHR(13) */
              /*  NYTT  */
          (IF dPris2 > 0 THEN 
               "191100200300020" + "(" + TRIM(STRING(dPris2,">>>>9.99")) + ")" + CHR(13) ELSE "")
                 cPrisKr  
                 cPrisOren
/*                           SUBSTITUTE("191100600200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) TRIM(STRING(iKr,">>>>9")) CHR(13) */
/*                           SUBSTITUTE("191100400400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) TRIM(STRING(iOren,"99")) CHR(13)    */
             (IF TRIM(ArtBas.LevKod) <> "" THEN
                        "121100200000020   L-NR:" + TRIM(ArtBas.LevKod) + CHR(13) ELSE "")
                      SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
      END.
  END.
  PUT UNFORMATTED CHR(2) "F".
  OUTPUT CLOSE.
/*   IF cOrgSysPr <> "" THEN                      */
/*       ASSIGN SESSION:PRINTER-NAME = cOrgSysPr. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Conv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Conv Procedure 
PROCEDURE Conv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT-OUTPUT PARAMETER navn AS CHAR NO-UNDO.

SORRY:
REPEAT ON ERROR UNDO, RETRY:
    IF INDEX(navn,"~{") <> 0 THEN
    navn = SUBSTRING(navn,1,INDEX(navn,"~{") - 1)
             + chr(126)
             + "132"
             + substring(navn,INDEX(navn,"~{") + 1,LENGTH(navn)).
    IF INDEX(navn,"~|") <> 0 THEN
    navn = SUBSTRING(navn,1,INDEX(navn,"~|") - 1)
             + chr(126)
             + "148"
             + substring(navn,INDEX(navn,"~|") + 1,LENGTH(navn)).
    IF INDEX(navn,"~}") <> 0 THEN
    navn = SUBSTRING(navn,1,INDEX(navn,"~}") - 1)
             + chr(126)
             + "134"
             + substring(navn,INDEX(navn,"~}") + 1,LENGTH(navn)).
    IF INDEX(navn,"~[") <> 0 THEN
    navn = SUBSTRING(navn,1,INDEX(navn,"~[") - 1)
             + chr(126)
             + "142"
             + substring(navn,INDEX(navn,"~[") + 1,LENGTH(navn)).
    IF INDEX(navn,"~\") <> 0 THEN
    navn = SUBSTRING(navn,1,INDEX(navn,"~\") - 1)
             + chr(126)
             + "153"
             + substring(navn,INDEX(navn,"~\") + 1,LENGTH(navn)).
    IF INDEX(navn,"~]") <> 0 THEN
    navn = SUBSTRING(navn,1,INDEX(navn,"~]") - 1)
             + chr(126)
             + "143"
             + substring(navn,INDEX(navn,"~]") + 1,LENGTH(navn)).
    IF INDEX(navn,"~{") <> 0 OR
       INDEX(navn,"~|") <> 0 OR
       INDEX(navn,"~}") <> 0 OR
       INDEX(navn,"~[") <> 0 OR
       INDEX(navn,"~\") <> 0 OR
       INDEX(navn,"~]") <> 0 THEN
    DO:
        NEXT SORRY.
    END.
    ELSE LEAVE SORRY.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixEAN Procedure 
PROCEDURE FixEAN :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStorl AS CHARACTER  NO-UNDO.
    FOR EACH EtikettLogg WHERE EtikettLogg.SeqNr > 0 AND
                                LENGTH(EtikettLogg.Storl) < 12 AND EtikettLogg.Storl <> "INFO":
/*                                LENGTH(EtikettLogg.Storl) < 13: */
        FIND ArtBas WHERE ArtBas.Vg    = EtikettLogg.Vg    AND
                          ArtBas.Lopnr = EtikettLogg.Lopnr NO-LOCK NO-ERROR.
        IF ArtBas.Opris = TRUE AND ArtBas.Vg = INT(EtikettLogg.Storl) THEN
            cStorl = EtikettLogg.Storl.
        ELSE DO:
            ASSIGN cStorl = "".

            FOR EACH StrekKode OF ArtBas NO-LOCK WHERE CAN-FIND(StrKonv OF StrekKode WHERE 
                                 StrKonv.Storl = EtikettLogg.Storl).
                ASSIGN cStorl = IF cStorl = "" THEN StrekKode.Kode ELSE IF
                              NOT cStorl BEGINS "02" THEN cStorl ELSE StrekKode.Kode.

    /*             ASSIGN cStorl = IF cStorl BEGINS "02" THEN cStorl ELSE StrekKode.Kode. */
            END.
        END.
        ASSIGN EtikettLogg.Storl = cStorl.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixStorl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixStorl Procedure 
PROCEDURE FixStorl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH EtikettLogg WHERE LENGTH(EtikettLogg.Storl) > 4:
        FIND StrekKode WHERE StrekKode.Kode = EtikettLogg.Storl NO-LOCK NO-ERROR.
        IF AVAIL StrekKode THEN DO:
            FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
            IF AVAIL StrKOnv THEN
                EtikettLogg.Storl = StrKonv.Storl.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GANT_Pris_under) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GANT_Pris_under Procedure 
PROCEDURE GANT_Pris_under :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dePris     AS DECIMAL                        NO-UNDO.
  DEFINE VARIABLE iKr        AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren      AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iRowEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iRow       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cRow       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCol       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cCol       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iColEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iEtikettnr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount     AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iCount2    AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE dPris2     AS DECIMAL  DECIMALS 2  NO-UNDO.
  DEFINE VARIABLE wOK        AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFormat AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cColKronor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColOren   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cInfoEtiTxt AS CHARACTER EXTENT 4 NO-UNDO.
  DEFINE VARIABLE cPrisKr   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrisOren AS CHARACTER  NO-UNDO.
  ASSIGN cColKronor = "200,200,200,185,160"
         cColOren   = "240,275,310,330,340".
  /* används för att kunna veta startetikett: Ant = startetikett */
  FOR EACH EtikettLogg WHERE EtikettLogg.SeqNr = 0 AND
                             EtikettLogg.Storl = "STARTETIKETT":
      DELETE EtikettLogg.
  END.
  /* hämta printernamn om cWinPrinterName = "" */
  IF cWinPrinterName = "" THEN DO:
      IF lTermKlient = TRUE THEN
          OUTPUT TO VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
      ELSE
          OUTPUT TO PRINTER VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
  END.
  ELSE DO:
      IF lTermKlient = TRUE THEN DO:
          OUTPUT TO VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      END.
      ELSE DO:
          ASSIGN SESSION:PRINTER-NAME = cWinPrinterName.          
          OUTPUT TO PRINTER VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      END.
  END.
/*   ASSIGN cFormat = CHR(2) + "KcLW150" + CHR(13). /* TEST */ */
/*   ASSIGN cFormat = CHR(2) + "cLW150m" + CHR(2) + "M0600" + CHR(2) + "O0000". /* m=metric */ */
/*   ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "KcLW400" + CHR(2) + "M0800" + CHR(2) + "O0000". /* m=metric */ */
/*   PUT CONTROL cFormat.                      */
/*   cFormat = CHR(2) + "LW400" + CHR(13). /* CR.*/.    */
/*   PUT CONTROL cFormat.                      */

  FOR EACH EtikettLogg WHERE EtikettLogg.Storl <> "" BY Etikettlogg.seqnr:
      ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "KcLW400" + CHR(2) + "M0800" + CHR(2) + "O0000". /* m=metric */
      PUT CONTROL cFormat.
      cFormat = CHR(2) + "LW400" + CHR(13). /* CR.*/. 
      PUT CONTROL cFormat.
      IF NOT EtikettLogg.Storl = "INFO" THEN DO:
          FIND Strekkode WHERE Strekkode.kode = EtikettLogg.Storl NO-LOCK NO-ERROR.
          FIND ArtBas OF StrekKode NO-LOCK.
          FIND Farg OF Artbas NO-LOCK NO-ERROR.
/*           FIND ArtPris OF Artbas NO-LOCK WHERE                */
/*             ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.     */
/*           IF NOT AVAILABLE ArtPris THEN                       */
/*             FIND ArtPris OF Artbas NO-LOCK WHERE              */
/*               ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR. */

          FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
          ASSIGN iKr   = TRUNC(EtikettLogg.Pris,0)
                 iOren = (EtikettLogg.Pris - TRUNC(EtikettLogg.Pris,0)) * 100.
      END.
      IF EtikettLogg.Storl = "INFO" THEN DO:
          IF etikettlogg.butik <> 0 THEN DO:
              FIND butiker WHERE butiker.butik = etikettlogg.butik NO-LOCK NO-ERROR.
              IF NOT AVAIL butiker THEN
                  FIND butiker WHERE butiker.butik = wCL NO-LOCK NO-ERROR.
          END.
          ASSIGN cInfoEtiTxt = "".
          DO iCount = 1 TO NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)):
              IF iCount = 5 THEN
                  LEAVE.
              ASSIGN cInfoEtiTxt[iCount] = ENTRY(iCount,Etikettlogg.Bongtekst,CHR(1)).
          END.
          PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "R0000" CHR(13) "D11" CHR(13)
                             "121100802100020" cInfoEtiTxt[1] CHR(13)
                             "121100801500020" cInfoEtiTxt[2] CHR(13)
                             "121100801000020" cInfoEtiTxt[3] CHR(13)
                             "121100800150020" cInfoEtiTxt[4] CHR(13)
                                                          "E" CHR(13).
      END.
      ELSE IF StrekKode.KodeType = 0 THEN
      PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
                         "191100202000020" ArtBas.Bongtekst CHR(13)
                         "1a0000000650080" TRIM(StrekKode.Kode) CHR(13)
                         "191100300150100" "PLU: " TRIM(StrekKode.Kode) CHR(13)
                      SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
      ELSE DO:
          /*  NYTT Vi har ett Etikettlogg.pris2 som visar ordinarie pris */
          IF NOT ArtBas.OPris THEN
              ASSIGN dPris2 = IF Etikettlogg.pris2 > 0 THEN Etikettlogg.Pris2 ELSE 0.
/*                               IF ArtPris.Tilbud = TRUE THEN ArtPris.Pris[1] ELSE 0. */
          ELSE 
              ASSIGN dPris2 = 0.
          /* För att inte få in ordinarie pris vid inleverans när varan står på tilbud */
          IF EtikettLogg.Pris = dPris2 THEN
             ASSIGN dPris2 = 0.
          IF ArtBas.OPris THEN
              ASSIGN cPrisKr   = ""
                     cPrisOren = "".
          ELSE
              ASSIGN cPrisKr   = SUBSTITUTE("191100600200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) + TRIM(STRING(iKr,">>>>9")) + CHR(13)
                     cPrisOren = SUBSTITUTE("191100400400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) + TRIM(STRING(iOren,"99")) + CHR(13).
         PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
             "191100202100020" ArtBas.Bongtekst CHR(13)
             "191100201800020" (IF ArtBas.StrTypeId > 2 AND AVAIL StrKonv THEN TRIM(StrKonv.Storl) + " " ELSE "") 
                         /*(IF AVAIL Farg AND Farg.FarBeskr <> "" THEN Farg.FarBeskr ELSE ArtBas.LevFargKod) CHR(13)*/
                         (IF ArtBas.LevFargKod <> '' THEN ArtBas.LevFargKod ELSE IF AVAIL Farg THEN Farg.FarBeskr ELSE '' ) CHR(13)
             "1F2206500900065" TRIM(StrekKode.Kode) CHR(13)
/*              "491100300900050" STRING(ArtBas.Vg) CHR(13) */
              /*  NYTT  */
          (IF dPris2 > 0 THEN 
               "191100300300020" + "(" + TRIM(STRING(dPris2,">>>>9.99")) + ")" + CHR(13) ELSE "")
                 cPrisKr  
                 cPrisOren
/*                           SUBSTITUTE("191100600200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) TRIM(STRING(iKr,">>>>9")) CHR(13) */
/*                           SUBSTITUTE("191100400400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) TRIM(STRING(iOren,"99")) CHR(13)    */
             (IF TRIM(ArtBas.LevKod) <> "" THEN
                    "121100200000020   L-NR:" + TRIM(ArtBas.LevKod) + CHR(13) ELSE "")
             SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
/*                  (IF Etikettlogg.individ > 0 THEN                                                           */
/*                             "121100200000020    ID:" + TRIM(STRING(Etikettlogg.Individ)) + CHR(13) ELSE "") */
/*                           SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).             */
      END.
  END.
  PUT UNFORMATTED CHR(2) "F".
  OUTPUT CLOSE.
/*   IF cOrgSysPr <> "" THEN                      */
/*       ASSIGN SESSION:PRINTER-NAME = cOrgSysPr. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWinPrinterName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getWinPrinterName Procedure 
PROCEDURE getWinPrinterName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount AS INTEGER          NO-UNDO.
  DEFINE VARIABLE cValdPrinter AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cClientname AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAvailPrinters AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEntry         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lOK AS LOGICAL    NO-UNDO.
  ASSIGN cClientname = OS-GETENV("CLIENTNAME").
  IF cClientName <> ? OR SESSION:PARAMETER <> "" THEN DO iCount = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
      IF ENTRY(iCount,SESSION:PARAMETER) BEGINS "EPRINTER" AND
         NUM-ENTRIES(ENTRY(iCount,SESSION:PARAMETER),"=") = 2 THEN DO:
         ASSIGN cWinPrinterName = ENTRY(2,ENTRY(iCount,SESSION:PARAMETER),"=").
         LEAVE.
      END.
      ELSE IF /* cClientname <> ? OR */ ENTRY(iCount,SESSION:PARAMETER) = "SESSIONPR" THEN DO:
/*           ASSIGN cClientname = OS-GETENV("CLIENTNAME"). */
          IF cClientname <> ? THEN DO:
              ASSIGN cAvailPrinters = SESSION:GET-PRINTERS().
              DO iCount = 1 TO NUM-ENTRIES(cAvailPrinters).
                  cEntry = ENTRY(iCount,cAvailPrinters).
                  IF cEntry BEGINS "Meto" AND cEntry MATCHES "*" + cClientName + "*" THEN DO:
                      ASSIGN cWinPrinterName = cEntry.
                      LEAVE.
                  END.
              END.
          END.
          LEAVE.
      END.
  END.
  IF NUM-ENTRIES(cWinPrinterName,"&") > 1 THEN DO:
      RUN d-VelgGenerellCombo.w ("Velg etikettprinter",REPLACE(REPLACE(cWinPrinterName,"|",","),"&",","),INPUT-OUTPUT cValdPrinter).
      IF cValdPrinter = "" THEN
          RETURN "AVBRYT".
      ASSIGN cWinPrinterName = cValdPrinter.
  END.
  IF cWinPrinterName = "" THEN DO:
      SYSTEM-DIALOG PRINTER-SETUP UPDATE lOK.
      IF NOT lOK THEN
          RETURN "AVBRYT".
  END.
  ELSE DO:
      ASSIGN cOrgSysPr = SESSION:PRINTER-NAME.
  END.
  RETURN "".   /* Function return value. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Init-Euro) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init-Euro Procedure 
PROCEDURE Init-Euro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF wFirma <> "" THEN
  DO:
    ASSIGN
      wFirma = SUBSTRING(wFirma,1,17)
      wFirma = FILL(" ",17 - length(wFirma)).

    /* Koverterer |{}\[] for Monarch. */
    DO i = 2 TO 3:
             IF INDEX(wFirma,"~{") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
        ELSE IF INDEX(wFirma,"~|") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
        ELSE IF INDEX(wFirma,"~}") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
        ELSE IF INDEX(wFirma,"~[") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
        ELSE IF INDEX(wFirma,"~\") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
        ELSE IF INDEX(wFirma,"~]") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
    END.

    linje[2]  =   "C,445,5,0,1,1,1,B,C,0,0,"""
                + wFirma
                + """|" + chr(10).

    linje[3]  =   "C,215,5,0,1,1,1,B,C,0,0,"""
                + wFirma
                + """|" + chr(10).
END.
ELSE DO:
    ASSIGN
    linje[2]  = "C,445,5,0,1,1,1,W,C,0,0,""                 ""|" + chr(10)
    linje[3]  = "C,215,5,0,1,1,1,W,C,0,0,""                 ""|" + chr(10).
END.

/* Inget namn skrivs ut!! 980616 */
ASSIGN
  linje[2]  = "C,200,40,0,3,1,1,B,L,0,0,""EUROSKOR""|" + chr(10)
  linje[3]  = "C,430,40,0,3,1,1,B,L,0,0,""EUROSKOR""|" + chr(10).

/* Definerer format */
ASSIGN
linje[1]  = "F,1,A,R,G,456,288,""EURO""|" + chr(10)
linje[4]  = "C,145,50,0,1,1,1,B,L,0,0,""   ""|" + chr(10)
linje[5]  = "C,380,50,0,1,1,1,B,L,0,0,""   ""|" + chr(10)
linje[6]  = "C,30,155,5,2,2,1,B,L,0,0,""STL:""|" + chr(10)
linje[7]  = "C,260,155,5,2,2,1,B,L,0,0,""STL:""|" + chr(10)

linje[8]  = "T,1,4,F,400,27,0,1,1,1,B,R,0,1|" + chr(10)
          + "R,60,I,1,1,4|" + chr(10)
linje[9]  = "T,2,4,F,170,27,0,1,1,1,B,R,0,1|" + chr(10)
          + "R,60,I,1,1,4|" + chr(10)
linje[10]  = "T,3,9,V,375,11,0,3,1,1,B,R,0,0|" + chr(10)
linje[11]  = "T,4,9,V,145,11,0,3,1,1,B,R,0,0|" + chr(10)
linje[12]  = "B,5,12,V,304,43,3,12,64,8,C,0|" + chr(10)
linje[13]  = "B,6,12,V,74,43,3,12,64,8,C,0|" + chr(10)
linje[14] = "T,7,9,V,252,5,0,1,2,1,B,C,0,0|" + chr(10)
linje[15] = "T,8,9,V,26,5,0,1,2,1,B,C,0,0|" + chr(10)
linje[16] = "T,9,4,V,252,198,0,1,2,1,B,C,0,0|" + chr(10)
linje[17] = "T,10,4,V,26,198,0,1,2,1,B,C,0,0|" + chr(10).

/* Definerer batch default verdier. */
ASSIGN
blinje[1]  = "B,1,N,1|" + chr(10)
blinje[2]  = "1,""0001""|" + chr(10)
blinje[3]  = "2,""0001""|" + chr(10)
blinje[4]  = "3,""123456.00""|" + chr(10)
blinje[5]  = "4,""123456.00""|" + chr(10)
blinje[6]  = "5,""002023400340""|" + chr(10)
blinje[7]  = "6,""002023400340""|" + chr(10)
blinje[8]  = "7,""2/234-34""|" + chr(10)
blinje[9]  = "8,""2/234-34""|" + chr(10).

/* Init av variabler. */
INIT:
DO:
    /* Etikettens layout */
    wlayout = "~{".
    DO i = 1 TO 17:
        wlayout = wlayout + linje[i].
    END.
    wlayout = wlayout + "~}".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Init-Euroblank) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init-Euroblank Procedure 
PROCEDURE Init-Euroblank :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF wFirma <> "" THEN
  DO:
    ASSIGN
      wFirma = SUBSTRING(wFirma,1,17)
      wFirma = FILL(" ",17 - length(wFirma)).

    /* Koverterer |{}\[] for Monarch. */
    DO i = 2 TO 3:
             IF INDEX(wFirma,"~{") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
        ELSE IF INDEX(wFirma,"~|") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
        ELSE IF INDEX(wFirma,"~}") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
        ELSE IF INDEX(wFirma,"~[") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
        ELSE IF INDEX(wFirma,"~\") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
        ELSE IF INDEX(wFirma,"~]") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
    END.

    linje[2]  =   "C,445,5,0,1,1,1,B,C,0,0,"""
                + wFirma
                + """|" + chr(10).

    linje[3]  =   "C,215,5,0,1,1,1,B,C,0,0,"""
                + wFirma
                + """|" + chr(10).
END.
ELSE DO:
    ASSIGN
    linje[2]  = "C,445,5,0,1,1,1,W,C,0,0,""                 ""|" + chr(10)
    linje[3]  = "C,215,5,0,1,1,1,W,C,0,0,""                 ""|" + chr(10).
END.

/* Inget namn skrivs ut!! 980616 */
ASSIGN
  linje[2]  = "C,200,40,0,3,1,1,B,L,0,0,""        ""|" + chr(10)
  linje[3]  = "C,430,40,0,3,1,1,B,L,0,0,""        ""|" + chr(10).

/* Definerer format */
ASSIGN
linje[1]  = "F,1,A,R,G,456,288,""EURO""|" + chr(10)
linje[4]  = "C,145,50,0,1,1,1,B,L,0,0,""   ""|" + chr(10)
linje[5]  = "C,380,50,0,1,1,1,B,L,0,0,""   ""|" + chr(10)
linje[6]  = "C,30,155,5,2,2,1,B,L,0,0,""STL:""|" + chr(10)
linje[7]  = "C,260,155,5,2,2,1,B,L,0,0,""STL:""|" + chr(10)

linje[8]  = "T,1,4,F,400,27,0,1,1,1,B,R,0,1|" + chr(10)
          + "R,60,I,1,1,4|" + chr(10)
linje[9]  = "T,2,4,F,170,27,0,1,1,1,B,R,0,1|" + chr(10)
          + "R,60,I,1,1,4|" + chr(10)
linje[10]  = "T,3,9,V,375,11,0,3,1,1,B,R,0,0|" + chr(10)
linje[11]  = "T,4,9,V,145,11,0,3,1,1,B,R,0,0|" + chr(10)
linje[12]  = "B,5,12,V,304,43,3,12,64,8,C,0|" + chr(10)
linje[13]  = "B,6,12,V,74,43,3,12,64,8,C,0|" + chr(10)
linje[14] = "T,7,9,V,252,5,0,1,2,1,B,C,0,0|" + chr(10)
linje[15] = "T,8,9,V,26,5,0,1,2,1,B,C,0,0|" + chr(10)
linje[16] = "T,9,4,V,252,198,0,1,2,1,B,C,0,0|" + chr(10)
linje[17] = "T,10,4,V,26,198,0,1,2,1,B,C,0,0|" + chr(10).

/* Definerer batch default verdier. */
ASSIGN
blinje[1]  = "B,1,N,1|" + chr(10)
blinje[2]  = "1,""0001""|" + chr(10)
blinje[3]  = "2,""0001""|" + chr(10)
blinje[4]  = "3,""123456.00""|" + chr(10)
blinje[5]  = "4,""123456.00""|" + chr(10)
blinje[6]  = "5,""002023400340""|" + chr(10)
blinje[7]  = "6,""002023400340""|" + chr(10)
blinje[8]  = "7,""2/234-34""|" + chr(10)
blinje[9]  = "8,""2/234-34""|" + chr(10).

/* Init av variabler. */
INIT:
DO:
    /* Etikettens layout */
    wlayout = "~{".
    DO i = 1 TO 17:
        wlayout = wlayout + linje[i].
    END.
    wlayout = wlayout + "~}".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Init-MONARK-1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init-MONARK-1 Procedure 
PROCEDURE Init-MONARK-1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN
linje[1]  = "F,1,A,R,M,850,450,""SVAN""|"

linje[2]  = "T,1,7,V,100,480,0,2,4,2,B,R,0,2|"
linje[3]  = "T,2,7,V,340,480,0,2,4,2,B,R,0,2|"

linje[4]  = "T,3,14,V,165,350,0,1,2,1,B,R,0,2|"
linje[5]  = "T,4,4,V,225,125,0,2,3,2,B,R,0,2|"
linje[6]  = "T,5,14,V,410,350,0,1,2,1,B,R,0,2|"
linje[7]  = "T,6,4,V,470,125,0,2,3,2,B,R,0,2|"

linje[8] = "B,7,12,V,500,35,3,12,90,5,C,0|"
linje[9] = "B,8,12,V,260,35,3,12,90,5,C,0|"
linje[10] = "B,9,12,V,15,35,3,12,90,5,C,0|"

linje[11] = "T,10,4,V,615,350,0,2,4,3,B,C,0,3|"

linje[12] = "T,11,5,V,840,10,0,1,5,2,B,R,0,3|"
linje[13] = "T,12,5,V,840,165,0,1,5,2,B,R,0,3|"
linje[14] = "T,13,5,V,840,310,0,1,5,2,B,R,0,3|"

linje[15] = "C,225,240,0,2,2,2,B,L,0,2,""stlk:""|"
linje[16] = "C,470,240,0,2,2,2,B,L,0,2,""stlk:""|".

/* Definerer batch default verdier. */
ASSIGN
blinje[1]  = "B,1,N,1|"
blinje[2]  = "1,""0001""|"
blinje[3]  = "2,""0001""|"
blinje[4]  = "3,""123456.00""|"
blinje[5]  = "4,""123456.00""|"
blinje[6]  = "5,""002023400340""|"
blinje[7]  = "6,""002023400340""|"
blinje[8]  = "7,""2/234-34""|"
blinje[9]  = "8,""2/234-34""|".

/* Init av variabler. */
INIT:
DO:
    /* Etikettens layout */
    wLayout = "~{".
    DO i = 1 TO 16:
        wLayout = wLayout + linje[i].
    END.
    wLayout = wLayout + "~}".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Init-MONARK-2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init-MONARK-2 Procedure 
PROCEDURE Init-MONARK-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN
linje[1]  = "F,1,A,R,G,256,288,""JF""|"
linje[2]  = "C,175,6,0,1,1,2,B,C,0,0,""ROLANS""|"
linje[3]  = "T,1,8,V,10,38,0,1,1,1,B,R,0,1|"
linje[4]  = "T,2,10,V,125,101,0,1,2,1,B,R,0,0|"
linje[5]  = "B,3,12,V,54,55,3,12,64,0,C,0|"
linje[6] =  "T,4,17,V,0,16,0,1,2,1,B,C,0,0|".

/* Definerer batch default verdier. */
ASSIGN
blinje[1]  = "B,1,N,1|"
blinje[2]  = "1,""0599PNHH""|"
blinje[3]  = "2,""123456.00""|"
blinje[4]  = "3,""002023400340""|"
blinje[5]  = "4,""2/234-34""|".

/* Init av variabler. */
INIT:
DO:
    /* Etikettens layout */
    wLayout = "~{".
    DO i = 1 TO 6:
        wLayout = wLayout + linje[i].
    END.
    wLayout = wLayout + "~}".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Init-Skoteket) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init-Skoteket Procedure 
PROCEDURE Init-Skoteket :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF wFirma <> "" THEN
  DO:
    ASSIGN
      wFirma = SUBSTRING(wFirma,1,17)
      wFirma = FILL(" ",17 - length(wFirma)).

    /* Koverterer |{}\[] for Monarch. */
    DO i = 2 TO 3:
             IF INDEX(wFirma,"~{") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
        ELSE IF INDEX(wFirma,"~|") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
        ELSE IF INDEX(wFirma,"~}") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
        ELSE IF INDEX(wFirma,"~[") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
        ELSE IF INDEX(wFirma,"~\") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
        ELSE IF INDEX(wFirma,"~]") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
    END.

    linje[2]  =   "C,445,5,0,1,1,1,B,C,0,0,"""
                + wFirma
                + """|" + chr(10).

    linje[3]  =   "C,215,5,0,1,1,1,B,C,0,0,"""
                + wFirma
                + """|" + chr(10).
END.
ELSE DO:
    ASSIGN
    linje[2]  = "C,445,5,0,1,1,1,W,C,0,0,""                 ""|" + chr(10)
    linje[3]  = "C,215,5,0,1,1,1,W,C,0,0,""                 ""|" + chr(10).
END.

/* Inget namn skrivs ut!! 980616 */
ASSIGN
  linje[2]  = "C,200,20,0,1,2,2,B,L,0,0,""skoteket""|" + chr(10)
  linje[3]  = "C,430,20,0,1,2,2,B,L,0,0,""skoteket""|" + chr(10).

/* Definerer format */
ASSIGN
linje[1]  = "F,1,A,R,G,456,288,""SKOTEK""|" + chr(10)
linje[4]  = "C,145,50,0,1,1,1,B,L,0,0,""   ""|" + chr(10)
linje[5]  = "C,380,50,0,1,1,1,B,L,0,0,""   ""|" + chr(10)
linje[6]  = "C,30,155,5,2,2,1,B,L,0,0,""STL:""|" + chr(10)
linje[7]  = "C,260,155,5,2,2,1,B,L,0,0,""STL:""|" + chr(10)

linje[8]  = "T,1,4,F,300,27,0,1,1,1,B,R,0,1|" + chr(10)
          + "R,60,I,1,1,4|" + chr(10)
linje[9]  = "T,2,4,F,80,27,0,1,1,1,B,R,0,1|" + chr(10)
          + "R,60,I,1,1,4|" + chr(10)
linje[10]  = "T,3,9,V,375,1,0,1,2,2,B,R,0,0|" + chr(10)
linje[11]  = "T,4,9,V,145,1,0,1,2,2,B,R,0,0|" + chr(10)
linje[12]  = "B,5,12,V,304,43,3,12,64,8,C,0|" + chr(10)
linje[13]  = "B,6,12,V,74,43,3,12,64,8,C,0|" + chr(10)
linje[14] = "T,7,9,V,252,5,0,1,2,1,B,C,0,0|" + chr(10)
linje[15] = "T,8,9,V,26,5,0,1,2,1,B,C,0,0|" + chr(10)
linje[16] = "T,9,4,V,252,198,0,1,2,1,B,C,0,0|" + chr(10)
linje[17] = "T,10,4,V,26,198,0,1,2,1,B,C,0,0|" + chr(10).

/* Definerer batch default verdier. */
ASSIGN
blinje[1]  = "B,1,N,1|" + chr(10)
blinje[2]  = "1,""0001""|" + chr(10)
blinje[3]  = "2,""0001""|" + chr(10)
blinje[4]  = "3,""123456.00""|" + chr(10)
blinje[5]  = "4,""123456.00""|" + chr(10)
blinje[6]  = "5,""002023400340""|" + chr(10)
blinje[7]  = "6,""002023400340""|" + chr(10)
blinje[8]  = "7,""2/234-34""|" + chr(10)
blinje[9]  = "8,""2/234-34""|" + chr(10).

/* Init av variabler. */
INIT:
DO:
    /* Etikettens layout */
    wlayout = "~{".
    DO i = 1 TO 17:
        wlayout = wlayout + linje[i].
    END.
    wlayout = wlayout + "~}".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Initiering) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initiering Procedure 
PROCEDURE Initiering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF wFirma <> "" THEN
  DO:
    ASSIGN
      wFirma = SUBSTRING(wFirma,1,17)
      wFirma = FILL(" ",17 - length(wFirma)).

    /* Koverterer |{}\[] for Monarch. */
    DO i = 2 TO 3:
             IF INDEX(wFirma,"~{") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
        ELSE IF INDEX(wFirma,"~|") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
        ELSE IF INDEX(wFirma,"~}") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
        ELSE IF INDEX(wFirma,"~[") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
        ELSE IF INDEX(wFirma,"~\") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
        ELSE IF INDEX(wFirma,"~]") <> 0 THEN RUN conv (INPUT-OUTPUT wFirma).
    END.

    linje[2]  =   "C,445,5,0,1,1,1,B,C,0,0,"""
                + wFirma
                + """|" + chr(10).

    linje[3]  =   "C,215,5,0,1,1,1,B,C,0,0,"""
                + wFirma
                + """|" + chr(10).
END.
ELSE DO:
    ASSIGN
    linje[2]  = "C,445,5,0,1,1,1,W,C,0,0,""                 ""|" + chr(10)
    linje[3]  = "C,215,5,0,1,1,1,W,C,0,0,""                 ""|" + chr(10).
END.

/* Inget namn skrivs ut!! 980616 */
ASSIGN
  linje[2]  = "C,445,5,0,1,1,1,B,C,0,0,""                 ""|" + chr(10)
  linje[3]  = "C,215,5,0,1,1,1,B,C,0,0,""                 ""|" + chr(10).

/* Definerer format */
ASSIGN
linje[1]  = "F,1,A,R,G,456,288,""JF1""|" + chr(10)
linje[4]  = "C,165,70,0,1,1,1,B,L,0,0,""KR:""|" + chr(10)
linje[5]  = "C,400,70,0,1,1,1,B,L,0,0,""KR:""|" + chr(10)
linje[6]  = "C,30,155,5,2,2,1,B,L,0,0,""STL:""|" + chr(10)
linje[7]  = "C,260,155,5,2,2,1,B,L,0,0,""STL:""|" + chr(10)

linje[8]  = "T,1,4,F,400,27,0,1,1,1,B,R,0,1|" + chr(10)
          + "R,60,I,1,1,4|" + chr(10)
linje[9]  = "T,2,4,F,170,27,0,1,1,1,B,R,0,1|" + chr(10)
          + "R,60,I,1,1,4|" + chr(10)
linje[10]  = "T,3,10,V,395,98,0,1,2,1,B,R,0,0|" + chr(10)
linje[11]  = "T,4,10,V,165,98,0,1,2,1,B,R,0,0|" + chr(10)
linje[12]  = "B,5,12,V,304,43,3,12,84,8,C,0|" + chr(10)
linje[13]  = "B,6,12,V,74,43,3,12,84,8,C,0|" + chr(10)
linje[14] = "T,7,9,V,252,5,0,1,2,1,B,C,0,0|" + chr(10)
linje[15] = "T,8,9,V,26,5,0,1,2,1,B,C,0,0|" + chr(10)
linje[16] = "T,9,4,V,252,198,0,1,2,1,B,C,0,0|" + chr(10)
linje[17] = "T,10,4,V,26,198,0,1,2,1,B,C,0,0|" + chr(10).

/* Definerer batch default verdier. */
ASSIGN
blinje[1]  = "B,1,N,1|" + chr(10)
blinje[2]  = "1,""0001""|" + chr(10)
blinje[3]  = "2,""0001""|" + chr(10)
blinje[4]  = "3,""123456.00""|" + chr(10)
blinje[5]  = "4,""123456.00""|" + chr(10)
blinje[6]  = "5,""002023400340""|" + chr(10)
blinje[7]  = "6,""002023400340""|" + chr(10)
blinje[8]  = "7,""2/234-34""|" + chr(10)
blinje[9]  = "8,""2/234-34""|" + chr(10).

/* Init av variabler. */
INIT:
DO:
    /* Etikettens layout */
    wlayout = "~{".
    DO i = 1 TO 17:
        wlayout = wlayout + linje[i].
    END.
    wlayout = wlayout + "~}".
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Medlem_ECE4) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Medlem_ECE4 Procedure 
PROCEDURE Medlem_ECE4 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount     AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE wOK        AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFormat AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cColKronor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColOren   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cOrgSysPr  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cInfoEtiTxt AS CHARACTER EXTENT 4 NO-UNDO.
  DEFINE VARIABLE cInneHaver AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iAntFill   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cButnamn   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cMedlemtxt AS CHARACTER  NO-UNDO.

  /* används för att kunna veta startetikett: Ant = startetikett */
  IF SESSION:PARAMETER <> "" THEN DO iCount = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
      IF ENTRY(iCount,SESSION:PARAMETER) BEGINS "EPRINTER" AND 
         NUM-ENTRIES(ENTRY(iCount,SESSION:PARAMETER),"=") = 2 THEN DO:
         ASSIGN cWinPrinterName = ENTRY(2,ENTRY(iCount,SESSION:PARAMETER),"=").
         LEAVE.
      END.
  END.
  IF cWinPrinterName = "" THEN DO:
      SYSTEM-DIALOG PRINTER-SETUP UPDATE wOK.
      IF NOT wOK THEN
          RETURN NO-APPLY.
  END.
  ELSE DO:
      ASSIGN cOrgSysPr = SESSION:PRINTER-NAME.
  END.
  /* hämta printernamn om cWinPrinterName = "" */
  IF cWinPrinterName = "" THEN
      OUTPUT TO PRINTER VALUE(SESSION:PRINTER-NAME) /* CONVERT TARGET "IBM850" */ .
/*       OUTPUT TO PRINTER CONVERT TARGET "IBM850". */
  ELSE DO:
      ASSIGN SESSION:PRINTER-NAME = cWinPrinterName.
/*       OUTPUT TO PRINTER. */
      OUTPUT TO PRINTER VALUE(cWinPrinterName) /* CONVERT TARGET "IBM850" */.
  END.
  PUT UNFORMATTED
      'SYSVAR(18)=0' SKIP
      'INPUT OFF' SKIP
      'PRINT KEY ON' SKIP
      'SETUP "SERVICE,MEDIA SIZE,LENGTH,211"' SKIP
      'SETUP "SERVICE,MEDIA SIZE,XSTART,0"' SKIP
      'SETUP "SERVICE,MEDIA SIZE,WIDTH,832"' SKIP
      'SETUP "SERVICE,MEDIA TYPE,LABEL (w GAPS)"' SKIP
      'SETUP "DETECTION,FEEDADJ,STARTADJ,-152"' SKIP
      'SETUP "DETECTION,FEEDADJ,STOPADJ,0"' SKIP
      'SETUP "SERVICE,PERFORMANCE,NORMAL"' SKIP
      'SETUP "SERVICE,PRINT_DEFS,NEW_SUPPLIES,GQ90"' SKIP
      'SETUP "CONTRAST,5"' SKIP
      'LTS& OFF' SKIP.

  FOR EACH EtikettLogg:
      ASSIGN cInneHaver = TRIM(SUBSTR(ENTRY(2,Etikettlogg.bongtekst,"|"),1,25))
             cButnamn   = TRIM(SUBSTR(ENTRY(3,Etikettlogg.bongtekst,"|"),1,25))
             cMedlemtxt = TRIM(ENTRY(1,Etikettlogg.bongtekst,"|"))
             iAntFill   = IF LENGTH(cButnamn) < 25 THEN (24 - LENGTH(cButnamn)) ELSE 0
             cButnamn   = FILL(" ",iAntFill)  + cButnamn
             iAntFill   = IF LENGTH(cInneHaver) < 25 THEN (24 - LENGTH(cInneHaver)) ELSE 0
             cInneHaver = FILL(" ",iAntFill)  + cInneHaver
             cMedlemtxt = FILL(" ",20 - LENGTH(cMedlemtxt)- 4) + cMedlemtxt.
  PUT UNFORMATTED
     'INPUT OFF' SKIP
     'FORMAT INPUT CHR$(2),CHR$(3),CHR$(13)' SKIP
     'KILL "LBLSOFT.LAY"' SKIP
     'LAYOUT INPUT "LBLSOFT.LAY"' SKIP
     'NASC -2:AN 7' SKIP
     'MAG 1,1' SKIP
     SUBSTITUTE('DIR 1:PP 24,201:FT "Swiss 721 BT":FS 8:FL 0:NI:PT "&1"',cInnehaver) SKIP
     SUBSTITUTE('BF "Monospace 821 BT",7:BF ON:PP 93,160:BT "EAN13":BM 2:BH 56:PB "&1"',SUBSTR(TRIM(Etikettlogg.storl),1,12)) SKIP
     SUBSTITUTE('DIR 1:PP 24,75:FT "Swiss 721 BT":FS 11:FL 0:NI:PT "&1"',cMedlemtxt) SKIP
     SUBSTITUTE('DIR 1:PP 24,42:FT "Swiss 721 BT":FS 8:FL 0:NI:PT "&1"',cButnamn) SKIP
     SUBSTITUTE('DIR 4:PP 28,50:FS 10:NI:PT "&1"',TRIM(ENTRY(4,Etikettlogg.bongtekst,"|"))) SKIP
     'LAYOUT END' SKIP
     'LAYOUT RUN "LBLSOFT.LAY"' SKIP
     'INPUT ON' SKIP
     SUBSTITUTE('PF &1',IF EtikettLogg.Ant > 1 THEN STRING(EtikettLogg.Ant) ELSE "") SKIP.
  END.
  PUT UNFORMATTED 'FORMFEED' SKIP 'PRINT KEY OFF' SKIP.
  OUTPUT CLOSE.
  IF cOrgSysPr <> "" THEN
      ASSIGN SESSION:PRINTER-NAME = cOrgSysPr.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Medlem_Meto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Medlem_Meto Procedure 
PROCEDURE Medlem_Meto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount     AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE wOK        AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFormat AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cColKronor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColOren   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cOrgSysPr  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cInfoEtiTxt AS CHARACTER EXTENT 4 NO-UNDO.
  DEFINE VARIABLE cInneHaver AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iAntFill   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cButnamn   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cValdPrinter AS CHARACTER  NO-UNDO.

  /* används för att kunna veta startetikett: Ant = startetikett */
  IF SESSION:PARAMETER <> "" THEN DO iCount = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
      IF ENTRY(iCount,SESSION:PARAMETER) BEGINS "EPRINTER" AND 
         NUM-ENTRIES(ENTRY(iCount,SESSION:PARAMETER),"=") = 2 THEN DO:
         ASSIGN cWinPrinterName = ENTRY(2,ENTRY(iCount,SESSION:PARAMETER),"=").
         LEAVE.
      END.
  END.
  IF cWinPrinterName = "" THEN DO:
      SYSTEM-DIALOG PRINTER-SETUP UPDATE wOK.
      IF NOT wOK THEN
          RETURN NO-APPLY.
  END.
  ELSE DO:
      ASSIGN cOrgSysPr = SESSION:PRINTER-NAME.
  END.
  IF NUM-ENTRIES(cWinPrinterName,"&") = 2 THEN DO:
      RUN d-VelgGenerellCombo.w ("Velg etikettprinter",REPLACE(REPLACE(cWinPrinterName,"|",","),"&",","),INPUT-OUTPUT cValdPrinter).
      IF cValdPrinter = "" THEN
          RETURN.
      ASSIGN cWinPrinterName = cValdPrinter.
  END.
  /* hämta printernamn om cWinPrinterName = "" */
  IF cWinPrinterName = "" THEN DO:
      IF lTermKlient = TRUE THEN
          OUTPUT TO VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
      ELSE
          OUTPUT TO PRINTER VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
/*       OUTPUT TO PRINTER CONVERT TARGET "IBM850". */
  END.
  ELSE DO:
      IF lTermKlient = TRUE THEN DO:
/*           IF NUM-ENTRIES(cWinPrinterName,"-") = 1 THEN                                                                                 */
/*               cValdPrinter = cWinPrinterName.                                                                                          */
/*           ELSE                                                                                                                         */
/*               RUN d-VelgGenerellCombo.w ("Velg etikettprinter",REPLACE(REPLACE(cWinPrinterName,"|",","),"-",","),INPUT-OUTPUT cValdPrinter). */
/*           IF cValdPrinter = "" THEN                                                                                                    */
/*               RETURN.                                                                                                                  */
          OUTPUT TO VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      END.
      ELSE DO:
          ASSIGN SESSION:PRINTER-NAME = cWinPrinterName.
          OUTPUT TO PRINTER VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      END.
  END.
/*   ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "M0400" + CHR(2) + "O0000". /* m=metric */ */
  ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "KcLW400" + CHR(2) + "M0600" + CHR(2) + "O0000". /* m=metric */
  PUT CONTROL cFormat.
  cFormat = CHR(2) + "LW400" + CHR(2) + CHR(13).
  PUT CONTROL cFormat.

  FOR EACH EtikettLogg:
      ASSIGN cInneHaver = TRIM(SUBSTR(ENTRY(2,Etikettlogg.bongtekst,"|"),1,25))
             cButnamn   = TRIM(SUBSTR(ENTRY(3,Etikettlogg.bongtekst,"|"),1,25))
             iAntFill   = IF LENGTH(cButnamn) < 25 THEN (24 - LENGTH(cButnamn)) / 2 ELSE 0
             cButnamn   = FILL(" ",iAntFill)  + cButnamn
             iAntFill   = IF LENGTH(cInneHaver) < 25 THEN (24 - LENGTH(cInneHaver)) / 2 ELSE 0
             cInneHaver = FILL(" ",iAntFill)  + cInneHaver.
  PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
/*                       "121100002200020" ArtBas.Bongtekst CHR(13) */
                  "121100802100020" cInneHaver /* cButnamn */ CHR(13)
                  "1F2208000900065" Etikettlogg.storl CHR(13)
                  "191100400350105 " ENTRY(1,Etikettlogg.bongtekst,"|") CHR(13)
                  "121100800150020" cButnamn CHR(13)
                  "491100300450050" ENTRY(4,Etikettlogg.bongtekst,"|") CHR(13)
                  SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
  END.
  PUT UNFORMATTED CHR(2) "F".
  OUTPUT CLOSE.
  IF cOrgSysPr <> "" THEN
      ASSIGN SESSION:PRINTER-NAME = cOrgSysPr.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Meto-4724) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Meto-4724 Procedure 
PROCEDURE Meto-4724 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dePris     AS DECIMAL                        NO-UNDO.
  DEFINE VARIABLE iKr        AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren      AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iRowEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iRow       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cRow       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCol       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cCol       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iColEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iEtikettnr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount     AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iCount2    AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE dPris2     AS DECIMAL  DECIMALS 2  NO-UNDO.
  DEFINE VARIABLE wOK        AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFormat AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cColKronor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColOren   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cInfoEtiTxt AS CHARACTER EXTENT 4 NO-UNDO.
  DEFINE VARIABLE cPrisKr   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrisOren AS CHARACTER  NO-UNDO.
  ASSIGN cColKronor = "200,200,200,185,160"
         cColOren   = "240,275,310,330,340".
  /* används för att kunna veta startetikett: Ant = startetikett */
  FOR EACH EtikettLogg WHERE EtikettLogg.SeqNr = 0 AND
                             EtikettLogg.Storl = "STARTETIKETT":
      DELETE EtikettLogg.
  END.
/*   IF SESSION:PARAMETER <> "" THEN DO iCount = 1 TO NUM-ENTRIES(SESSION:PARAMETER):                                             */
/*       IF ENTRY(iCount,SESSION:PARAMETER) BEGINS "EPRINTER" AND                                                                 */
/*          NUM-ENTRIES(ENTRY(iCount,SESSION:PARAMETER),"=") = 2 THEN DO:                                                         */
/*          ASSIGN cWinPrinterName = ENTRY(2,ENTRY(iCount,SESSION:PARAMETER),"=").                                                */
/*          LEAVE.                                                                                                                */
/*       END.                                                                                                                     */
/*   END.                                                                                                                         */
/*   IF NUM-ENTRIES(cWinPrinterName,"&") = 2 THEN DO:                                                                             */
/*       RUN d-VelgGenerellCombo.w ("Velg etikettprinter",REPLACE(REPLACE(cWinPrinterName,"|",","),"&",","),INPUT-OUTPUT cValdPrinter). */
/*       IF cValdPrinter = "" THEN                                                                                                */
/*           RETURN.                                                                                                              */
/*       ASSIGN cWinPrinterName = cValdPrinter.                                                                                   */
/*   END.                                                                                                                         */
/*   IF cWinPrinterName = "" THEN DO:                                                                                             */
/*       SYSTEM-DIALOG PRINTER-SETUP UPDATE wOK.                                                                                  */
/*       IF NOT wOK THEN                                                                                                          */
/*           RETURN NO-APPLY.                                                                                                     */
/*   END.                                                                                                                         */
/*   ELSE DO:                                                                                                                     */
/*       ASSIGN cOrgSysPr = SESSION:PRINTER-NAME.                                                                                 */
/*   END.                                                                                                                         */
  /* hämta printernamn om cWinPrinterName = "" */
  IF cWinPrinterName = "" THEN DO:
      IF lTermKlient = TRUE THEN
          OUTPUT TO VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
      ELSE
          OUTPUT TO PRINTER VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
  END.
  ELSE DO:
      IF lTermKlient = TRUE THEN DO:
          OUTPUT TO VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      END.
      ELSE DO:
          ASSIGN SESSION:PRINTER-NAME = cWinPrinterName.
          OUTPUT TO PRINTER VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      END.
  END.
/*   ASSIGN cFormat = CHR(2) + "KcLW150" + CHR(13). /* TEST */ */
/*   ASSIGN cFormat = CHR(2) + "cLW150m" + CHR(2) + "M0600" + CHR(2) + "O0000". /* m=metric */ */
  ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "KcLW400" + chr(2) + "M0600" + CHR(2) + "O0000". /* m=metric */
  PUT CONTROL cFormat.
  cFormat = CHR(2) + "LW400" + CHR(13). /* CR.*/. 
  PUT CONTROL cFormat.

  FOR EACH EtikettLogg WHERE EtikettLogg.Storl <> "":
      IF NOT EtikettLogg.Storl = "INFO" THEN DO:
          FIND Strekkode WHERE Strekkode.kode = EtikettLogg.Storl NO-LOCK NO-ERROR.
          FIND ArtBas OF StrekKode NO-LOCK.
          FIND Farg OF Artbas NO-LOCK NO-ERROR.

          FIND ArtPris OF Artbas NO-LOCK WHERE 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
            FIND ArtPris OF Artbas NO-LOCK WHERE 
              ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.

          FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
          ASSIGN iKr   = TRUNC(EtikettLogg.Pris,0)
                 iOren = (EtikettLogg.Pris - TRUNC(EtikettLogg.Pris,0)) * 100.
      END.
      IF EtikettLogg.Storl = "INFO" THEN DO:
          ASSIGN cInfoEtiTxt = "".
          DO iCount = 1 TO NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)):
              IF iCount = 5 THEN
                  LEAVE.
              ASSIGN cInfoEtiTxt[iCount] = ENTRY(iCount,Etikettlogg.Bongtekst,CHR(1)).
          END.
          PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "R0000" CHR(13) "D11" CHR(13)
                             "121100802100020" cInfoEtiTxt[1] CHR(13)
                             "121100801500020" cInfoEtiTxt[2] CHR(13)
                             "121100801000020" cInfoEtiTxt[3] CHR(13)
                             "121100800150020" cInfoEtiTxt[4] CHR(13)
                                                          "E" CHR(13).
      END.
      ELSE IF StrekKode.KodeType = 0 THEN
      PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
                         "191100202000020" ArtBas.Bongtekst CHR(13)
                         "1a0000000650080" TRIM(StrekKode.Kode) CHR(13)
                         "191100300150100" "PLU: " TRIM(StrekKode.Kode) CHR(13)
                      SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
      ELSE DO:
          /*  NYTT Vi har ett Etikettlogg.pris2 som visar ordinarie pris */
          IF NOT ArtBas.OPris THEN
              ASSIGN dPris2 = IF Etikettlogg.pris2 > 0 THEN Etikettlogg.Pris2 ELSE
                              IF ArtPris.Tilbud = TRUE THEN ArtPris.Pris[1] ELSE 0.
          ELSE 
              ASSIGN dPris2 = 0.
          /* För att inte få in ordinarie pris vid inleverans när varan står på tilbud */
          IF EtikettLogg.Pris = dPris2 THEN
             ASSIGN dPris2 = 0.
          IF ArtBas.OPris THEN
              ASSIGN cPrisKr   = ""
                     cPrisOren = "".
          ELSE
              ASSIGN cPrisKr   = SUBSTITUTE("191100600200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) + TRIM(STRING(iKr,">>>>9")) + CHR(13)
                     cPrisOren = SUBSTITUTE("191100400400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) + TRIM(STRING(iOren,"99")) + CHR(13).
         PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
             "191100202100020" ArtBas.Bongtekst CHR(13)
             "191100201800020" (IF ArtBas.StrTypeId > 2 AND AVAIL StrKonv THEN TRIM(StrKonv.Storl) + " " ELSE "") 
                         /*(IF AVAIL Farg AND Farg.FarBeskr <> "" THEN Farg.FarBeskr ELSE ArtBas.LevFargKod) CHR(13)*/
                         (IF ArtBas.LevFargKod <> '' THEN ArtBas.LevFargKod ELSE IF AVAIL Farg THEN Farg.FarBeskr ELSE '' ) CHR(13)
             "1F2206500900065" TRIM(StrekKode.Kode) CHR(13)
             "491100300900050" STRING(ArtBas.Vg) CHR(13)
              /*  NYTT  */
          (IF dPris2 > 0 THEN 
               "191100200300020" + "(" + TRIM(STRING(dPris2,">>>>9.99")) + ")" + CHR(13) ELSE "")
                 cPrisKr  
                 cPrisOren
/*                           SUBSTITUTE("191100600200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) TRIM(STRING(iKr,">>>>9")) CHR(13) */
/*                           SUBSTITUTE("191100400400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) TRIM(STRING(iOren,"99")) CHR(13)    */
                 (IF Etikettlogg.individ > 0 THEN
                            "121100200000020    ID:" + TRIM(STRING(Etikettlogg.Individ)) + CHR(13) ELSE "")
                          SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
      END.
  END.
  PUT UNFORMATTED CHR(2) "F".
  OUTPUT CLOSE.
/*   IF cOrgSysPr <> "" THEN                      */
/*       ASSIGN SESSION:PRINTER-NAME = cOrgSysPr. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-METO-mn-4) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE METO-mn-4 Procedure 
PROCEDURE METO-mn-4 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dePris     AS DECIMAL                        NO-UNDO.
  DEFINE VARIABLE iKr        AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren      AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iRowEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iRow       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cRow       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCol       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cCol       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iColEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iEtikettnr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount     AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iCount2    AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE dPris2     AS DECIMAL DECIMALS 2   NO-UNDO.
  DEFINE VARIABLE wOK        AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFormat AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cColKronor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColOren   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cInfoEtiTxt AS CHARACTER EXTENT 4 NO-UNDO.
  DEFINE VARIABLE cPrisKr   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrisOren AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cValdPrinter AS CHARACTER  NO-UNDO.
  ASSIGN cColKronor = "200,200,200,185,160"
         cColOren   = "240,275,310,330,340".
  /* används för att kunna veta startetikett: Ant = startetikett */
  FOR EACH EtikettLogg WHERE EtikettLogg.SeqNr = 0 AND
                             EtikettLogg.Storl = "STARTETIKETT":
      DELETE EtikettLogg.
  END.
/*   IF SESSION:PARAMETER <> "" THEN DO iCount = 1 TO NUM-ENTRIES(SESSION:PARAMETER):                                             */
/*       IF ENTRY(iCount,SESSION:PARAMETER) BEGINS "EPRINTER" AND                                                                 */
/*          NUM-ENTRIES(ENTRY(iCount,SESSION:PARAMETER),"=") = 2 THEN DO:                                                         */
/*          ASSIGN cWinPrinterName = ENTRY(2,ENTRY(iCount,SESSION:PARAMETER),"=").                                                */
/*          LEAVE.                                                                                                                */
/*       END.                                                                                                                     */
/*   END.                                                                                                                         */
/*   IF NUM-ENTRIES(cWinPrinterName,"&") > 1 THEN DO:                                                                             */
/*       RUN d-VelgGenerellCombo.w ("Velg etikettprinter",REPLACE(REPLACE(cWinPrinterName,"|",","),"&",","),INPUT-OUTPUT cValdPrinter). */
/*       IF cValdPrinter = "" THEN                                                                                                */
/*           RETURN.                                                                                                              */
/*       MESSAGE cValdPrinter                                                                                                     */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                                   */
/*       RETURN.                                                                                                                  */
/*       ASSIGN cWinPrinterName = cValdPrinter.                                                                                   */
/*   END.                                                                                                                         */
/*   IF cWinPrinterName = "" THEN DO:                                                                                             */
/*       SYSTEM-DIALOG PRINTER-SETUP UPDATE wOK.                                                                                  */
/*       IF NOT wOK THEN                                                                                                          */
/*           RETURN NO-APPLY.                                                                                                     */
/*   END.                                                                                                                         */
/*   ELSE DO:                                                                                                                     */
/*       ASSIGN cOrgSysPr = SESSION:PRINTER-NAME.                                                                                 */
/*   END.                                                                                                                         */
  /* hämta printernamn om cWinPrinterName = "" */

  IF cWinPrinterName = "" THEN DO:
      IF lTermKlient = TRUE THEN
          OUTPUT TO VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
      ELSE
          OUTPUT TO PRINTER VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
  END.
  ELSE DO:
      IF lTermKlient = TRUE THEN DO:
          OUTPUT TO VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      END.
      ELSE DO:
          ASSIGN SESSION:PRINTER-NAME = cWinPrinterName.
          OUTPUT TO PRINTER VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      END.
  END.
/*   ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "M0400" + CHR(2) + "O0000". /* m=metric */ */
  
/*   ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "KcLW400" + CHR(2) + "M0600" + CHR(2) + "O0000". /* m=metric */ */
/*   PUT CONTROL cFormat.                                                                                     */
/*   cFormat = CHR(2) + "LW400" + CHR(13). /* CR.*/.                                                          */
/* /*   cFormat = CHR(2) + "LW400" + CHR(13). /* CR.*/. */                                                    */
/*   PUT CONTROL cFormat.                                                                                     */

/*   ASSIGN cFormat = CHR(2) + "KcLW150" + CHR(13). /* TEST */ */
/*   PUT CONTROL cFormat. /* TEST */                           */
  
  FOR EACH EtikettLogg WHERE EtikettLogg.Storl <> "":
      ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "KcLW400" + chr(2) + "M0800" + CHR(2) + "O0000". /* m=metric */
      PUT CONTROL cFormat.
      cFormat = CHR(2) + "LW400" + CHR(13). /* CR.*/.
      PUT CONTROL cFormat.
      IF NOT EtikettLogg.Storl = "INFO" THEN DO:
          FIND Strekkode WHERE Strekkode.kode = EtikettLogg.Storl NO-LOCK NO-ERROR.
          FIND ArtBas OF StrekKode NO-LOCK NO-ERROR.
          FIND Farg OF Artbas NO-LOCK NO-ERROR.

          FIND ArtPris OF Artbas NO-LOCK WHERE 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
            FIND ArtPris OF Artbas NO-LOCK WHERE 
              ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.

          FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
          ASSIGN iKr   = TRUNC(EtikettLogg.Pris,0)
                 iOren = (EtikettLogg.Pris - TRUNC(EtikettLogg.Pris,0)) * 100.
      END.
      IF EtikettLogg.Storl = "INFO" THEN DO:
          ASSIGN cInfoEtiTxt = "".
          DO iCount = 1 TO NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)):
              IF iCount = 5 THEN
                  LEAVE.
              ASSIGN cInfoEtiTxt[iCount] = ENTRY(iCount,Etikettlogg.Bongtekst,CHR(1)).
          END.
          PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
                             "121100802100020" cInfoEtiTxt[1] CHR(13)
                             "121100801500020" cInfoEtiTxt[2] CHR(13)
                             "121100801000020" cInfoEtiTxt[3] CHR(13)
                             "121100800150020" cInfoEtiTxt[4] CHR(13)
                                                          "E" CHR(13).
      END.
      ELSE IF StrekKode.KodeType = 0 THEN
          PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
                             "191100202000020" ArtBas.Bongtekst CHR(13)
                             "1a0000000650080" TRIM(StrekKode.Kode) CHR(13)
                             "191100300150100" "PLU: " TRIM(StrekKode.Kode) CHR(13)
                          SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
      ELSE DO:
          /*  NYTT Vi har ett Etikettlogg.pris2 som visar ordinarie pris */
          IF NOT ArtBas.OPris THEN
              ASSIGN dPris2 = IF Etikettlogg.pris2 > 0 THEN Etikettlogg.Pris2 ELSE
                              IF ArtPris.Tilbud = TRUE THEN ArtPris.Pris[1] ELSE 0.
          ELSE 
              ASSIGN dPris2 = 0.
          /* För att inte få in ordinarie pris vid inleverans när varan står på tilbud */
          IF EtikettLogg.Pris = dPris2 THEN
             ASSIGN dPris2 = 0.
          IF ArtBas.OPris THEN
              ASSIGN cPrisKr   = ""
                     cPrisOren = "".
          ELSE
              ASSIGN cPrisKr   = SUBSTITUTE("191100601200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) + TRIM(STRING(iKr,">>>>9")) + CHR(13)
                     cPrisOren = SUBSTITUTE("191100401400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) + TRIM(STRING(iOren,"99")) + CHR(13).
          PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
                          "191100202100020" ArtBas.Bongtekst CHR(13)
                          "191100201800020" (IF ArtBas.StrTypeId > 2 AND AVAIL StrKonv THEN TRIM(StrKonv.Storl) + " " ELSE "") 
                                            /*(IF AVAIL Farg AND Farg.FarBeskr <> "" THEN Farg.FarBeskr ELSE ArtBas.LevFargKod) CHR(13)*/
                                            (IF ArtBas.LevFargKod <> '' THEN ArtBas.LevFargKod ELSE IF AVAIL Farg THEN Farg.FarBeskr ELSE '' ) CHR(13)
    /*                       "121100801400010" "Veil Pris" CHR(13) */
              (IF dPris2 > 0 THEN 
                    "191100201300020" + "(" + TRIM(STRING(dPris2,">>>>9.99")) + ")" + CHR(13) ELSE "")
                  cPrisKr
                  cPrisOren
/*                           SUBSTITUTE("191100601200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) TRIM(STRING(iKr,">>>>9")) CHR(13) */
/*                           SUBSTITUTE("191100401400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) TRIM(STRING(iOren,"99")) CHR(13)    */
                                     "1F2206500450065" StrekKode.Kode CHR(13)
                                     "491100300450050" STRING(ArtBas.Vg) CHR(13)
    /*                       "1F2209000050065" StrekKode.Kode CHR(13) */
              (IF Etikettlogg.individ > 0 THEN
                         "121100200000020    ID:" + TRIM(STRING(Etikettlogg.Individ)) + CHR(13) ELSE "")
                          SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
      END.
  END.
  PUT UNFORMATTED CHR(2) "F".
  OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Meto-Normal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Meto-Normal Procedure 
PROCEDURE Meto-Normal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:             <STX>KcLW150 CR
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dePris     AS DECIMAL                        NO-UNDO.
  DEFINE VARIABLE iKr        AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren      AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iRowEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iRow       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cRow       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCol       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cCol       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iColEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iEtikettnr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount     AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iCount2    AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE dPris2     AS DECIMAL  DECIMALS 2  NO-UNDO.
  DEFINE VARIABLE wOK        AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFormat AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cColKronor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColOren   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cInfoEtiTxt AS CHARACTER EXTENT 4 NO-UNDO.
  DEFINE VARIABLE cPrisKr   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrisOren AS CHARACTER  NO-UNDO.
  ASSIGN cColKronor = "200,200,200,185,160"
         cColOren   = "240,275,310,330,340".

  /* används för att kunna veta startetikett: Ant = startetikett */
  FOR EACH EtikettLogg WHERE EtikettLogg.SeqNr = 0 AND
                             EtikettLogg.Storl = "STARTETIKETT":
      DELETE EtikettLogg.
  END.
  /* hämta printernamn om cWinPrinterName = "" */
  IF cWinPrinterName = "" THEN DO:
      IF lTermKlient = TRUE THEN
          OUTPUT TO VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
      ELSE
          OUTPUT TO PRINTER VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
  END.
  ELSE DO:

      IF lTermKlient = TRUE THEN DO:
          OUTPUT TO VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      END.
      ELSE DO:
          ASSIGN SESSION:PRINTER-NAME = cWinPrinterName.
          OUTPUT TO PRINTER VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      END.
  END.
/*   ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "KcLW400" + CHR(2) + "M0600" + CHR(2) + "O0000". /* m=metric */ */
/*   PUT CONTROL cFormat.                                                                                     */
/* /*   cFormat = CHR(2) + "KcLW400" + CHR(13). /* CR.*/. */                                                  */
/*   cFormat = CHR(2) + "LW400" + CHR(13). /* + "DK20" + CHR(13). /* CR.*/. */                                */
/*   PUT CONTROL cFormat.                                                                                     */
  FOR EACH EtikettLogg WHERE EtikettLogg.Storl <> "":
      ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "KcLW400" + chr(2) + "M0800" + CHR(2) + "O0000". /* m=metric */
      PUT CONTROL cFormat.
      cFormat = CHR(2) + "LW400" + CHR(13). /* CR.*/.
      PUT CONTROL cFormat.
      IF NOT EtikettLogg.Storl = "INFO" THEN DO:
          FIND Strekkode WHERE Strekkode.kode = EtikettLogg.Storl NO-LOCK NO-ERROR.
          FIND ArtBas OF StrekKode NO-LOCK.
          FIND Farg OF Artbas NO-LOCK NO-ERROR.

          FIND ArtPris OF Artbas NO-LOCK WHERE 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
            FIND ArtPris OF Artbas NO-LOCK WHERE 
              ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.

          FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
          /* Kalkylens normalpris */
          ASSIGN iKr   = TRUNC(ArtPris.Pris[1],0)
                 iOren = (ArtPris.Pris[1] - TRUNC(ArtPris.Pris[1],0)) * 100.
/*           ASSIGN iKr   = TRUNC(EtikettLogg.Pris,0)                             */
/*                  iOren = (EtikettLogg.Pris - TRUNC(EtikettLogg.Pris,0)) * 100. */
      END.
      IF EtikettLogg.Storl = "INFO" THEN DO:
          ASSIGN cInfoEtiTxt = "".
          DO iCount = 1 TO NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)):
              IF iCount = 5 THEN
                  LEAVE.
              ASSIGN cInfoEtiTxt[iCount] = ENTRY(iCount,Etikettlogg.Bongtekst,CHR(1)).
          END.
          PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "R0000" CHR(13) "D11" CHR(13)
                             "121100802100020" cInfoEtiTxt[1] CHR(13)
                             "121100801500020" cInfoEtiTxt[2] CHR(13)
                             "121100801000020" cInfoEtiTxt[3] CHR(13)
                             "121100800150020" cInfoEtiTxt[4] CHR(13)
                                                          "E" CHR(13).
      END.
      ELSE IF StrekKode.KodeType = 0 THEN
      PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
                         "191100202000020" ArtBas.Bongtekst CHR(13)
                         "1a0000000650080" TRIM(StrekKode.Kode) CHR(13)
                         "191100300150100" "PLU: " TRIM(StrekKode.Kode) CHR(13)
                      SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
      ELSE DO:
          /*  NYTT Vi har ett Etikettlogg.pris2 som visar ordinarie pris */
          IF NOT ArtBas.OPris THEN
              ASSIGN dPris2 = 0.
          ELSE 
              ASSIGN dPris2 = 0.
          /* För att inte få in ordinarie pris vid inleverans när varan står på tilbud */
          IF EtikettLogg.Pris = dPris2 THEN
             ASSIGN dPris2 = 0.
          IF ArtBas.OPris THEN
              ASSIGN cPrisKr   = ""
                     cPrisOren = "".
          ELSE
              ASSIGN cPrisKr   = SUBSTITUTE("191100600200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) + TRIM(STRING(iKr,">>>>9")) + CHR(13)
                     cPrisOren = SUBSTITUTE("191100400400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) + TRIM(STRING(iOren,"99")) + CHR(13).
         PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
             "191100202100020" ArtBas.Bongtekst CHR(13)
             "191100201800020" (IF ArtBas.StrTypeId > 2 AND AVAIL StrKonv THEN TRIM(StrKonv.Storl) + " " ELSE "") 
                         /*(IF AVAIL Farg AND Farg.FarBeskr <> "" THEN Farg.FarBeskr ELSE ArtBas.LevFargKod) CHR(13)*/
                         (IF ArtBas.LevFargKod <> '' THEN ArtBas.LevFargKod ELSE IF AVAIL Farg THEN Farg.FarBeskr ELSE '' ) CHR(13)
             "1F2206500900065" TRIM(StrekKode.Kode) CHR(13)
/*              "491100300500050" STRING(ArtBas.Vg) CHR(13) */
              /*  NYTT  */
          (IF dPris2 > 0 THEN 
               "191100200300020" + "(" + TRIM(STRING(dPris2,">>>>9.99")) + ")" + CHR(13) ELSE "")
                 cPrisKr  
                 cPrisOren
/*                           SUBSTITUTE("191100600200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) TRIM(STRING(iKr,">>>>9")) CHR(13) */
/*                           SUBSTITUTE("191100400400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) TRIM(STRING(iOren,"99")) CHR(13)    */
             ("121100200000020  " + TRIM(ArtBas.LevKod) + CHR(13))
/*                  (IF Etikettlogg.individ > 0 THEN                                                           */
/*                             "121100200000020    ID:" + TRIM(STRING(Etikettlogg.Individ)) + CHR(13) ELSE "") */
                          SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
      END.
  END.
  PUT UNFORMATTED CHR(2) "F".
  OUTPUT CLOSE.
/*   IF cOrgSysPr <> "" THEN                      */
/*       ASSIGN SESSION:PRINTER-NAME = cOrgSysPr. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-METO_Pris_under) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE METO_Pris_under Procedure 
PROCEDURE METO_Pris_under :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dePris     AS DECIMAL                        NO-UNDO.
  DEFINE VARIABLE iKr        AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren      AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iRowEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iRow       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cRow       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCol       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cCol       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iColEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iEtikettnr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount     AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iCount2    AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE dPris2     AS DECIMAL  DECIMALS 2  NO-UNDO.
  DEFINE VARIABLE wOK        AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFormat AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cColKronor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColOren   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cInfoEtiTxt AS CHARACTER EXTENT 4 NO-UNDO.
  DEFINE VARIABLE cPrisKr   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrisOren AS CHARACTER  NO-UNDO.
  ASSIGN cColKronor = "200,200,200,185,160"
         cColOren   = "240,275,310,330,340".
  /* används för att kunna veta startetikett: Ant = startetikett */
  FOR EACH EtikettLogg WHERE EtikettLogg.SeqNr = 0 AND
                             EtikettLogg.Storl = "STARTETIKETT":
      DELETE EtikettLogg.
  END.
/*   OUTPUT TO "CLIPBOARD".  */
/*   FOR EACH etikettlogg.   */
/*       EXPORT etikettlogg. */
/*   END.                    */
/*   OUTPUT CLOSE.           */
  
  /* hämta printernamn om cWinPrinterName = "" */
  IF cWinPrinterName = "" THEN DO:
      IF lTermKlient = TRUE THEN
          OUTPUT TO VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
      ELSE
          OUTPUT TO PRINTER VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
  END.
  ELSE DO:
      IF lTermKlient = TRUE THEN DO:
          OUTPUT TO VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      END.
      ELSE DO:
          ASSIGN SESSION:PRINTER-NAME = cWinPrinterName.
          OUTPUT TO PRINTER VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      END.
  END.
/*   ASSIGN cFormat = CHR(2) + "KcLW150" + CHR(13). /* TEST */ */
/*   ASSIGN cFormat = CHR(2) + "cLW150m" + CHR(2) + "M0600" + CHR(2) + "O0000". /* m=metric */ */
/*   ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "KcLW400" + chr(2) + "M0800" + CHR(2) + "O0000". /* m=metric */ */
/*   PUT CONTROL cFormat.                                                                                     */
/*   cFormat = CHR(2) + "LW400" + CHR(13). /* CR.*/.                                                          */
/*   PUT CONTROL cFormat.                                                                                     */

  FOR EACH EtikettLogg WHERE EtikettLogg.Storl <> "" BY Etikettlogg.seqnr:
      ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "KcLW400" + chr(2) + "M0800" + CHR(2) + "O0000". /* m=metric */
      PUT CONTROL cFormat.
      cFormat = CHR(2) + "LW400" + CHR(13). /* CR.*/.
      PUT CONTROL cFormat.
      IF NOT EtikettLogg.Storl = "INFO" THEN DO:
          FIND Strekkode WHERE Strekkode.kode = EtikettLogg.Storl NO-LOCK NO-ERROR.
          FIND ArtBas OF StrekKode NO-LOCK.
          FIND Farg OF Artbas NO-LOCK NO-ERROR.
          FIND ArtPris OF Artbas NO-LOCK WHERE 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
            FIND ArtPris OF Artbas NO-LOCK WHERE 
              ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.

          FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
          ASSIGN iKr   = TRUNC(EtikettLogg.Pris,0)
                 iOren = (EtikettLogg.Pris - TRUNC(EtikettLogg.Pris,0)) * 100.
      END.
      IF EtikettLogg.Storl = "INFO" THEN DO:
          IF etikettlogg.butik <> 0 THEN DO:
              FIND butiker WHERE butiker.butik = etikettlogg.butik NO-LOCK NO-ERROR.
              IF NOT AVAIL butiker THEN
                  FIND butiker WHERE butiker.butik = wCL NO-LOCK NO-ERROR.
          END.
          ASSIGN cInfoEtiTxt = "".
          DO iCount = 1 TO NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)):
              IF iCount = 5 THEN
                  LEAVE.
              ASSIGN cInfoEtiTxt[iCount] = ENTRY(iCount,Etikettlogg.Bongtekst,CHR(1)).
          END.
          PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "R0000" CHR(13) "D11" CHR(13)
                             "121100802100020" cInfoEtiTxt[1] CHR(13)
                             "121100801500020" cInfoEtiTxt[2] CHR(13)
                             "121100801000020" cInfoEtiTxt[3] CHR(13)
                             "121100800150020" cInfoEtiTxt[4] CHR(13)
                                                          "E" CHR(13).
      END.
      ELSE IF StrekKode.KodeType = 0 THEN
      PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
                         "191100202000020" ArtBas.Bongtekst CHR(13)
                         "1a0000000650080" TRIM(StrekKode.Kode) CHR(13)
                         "191100300150100" "PLU: " TRIM(StrekKode.Kode) CHR(13)
                      SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
      ELSE DO:
          /*  NYTT Vi har ett Etikettlogg.pris2 som visar ordinarie pris */
          IF NOT ArtBas.OPris THEN
              ASSIGN dPris2 = IF Etikettlogg.pris2 > 0 THEN Etikettlogg.Pris2 ELSE
                              IF ArtPris.Tilbud = TRUE THEN ArtPris.Pris[1] ELSE 0.
          ELSE 
              ASSIGN dPris2 = 0.
          /* För att inte få in ordinarie pris vid inleverans när varan står på tilbud */
          IF EtikettLogg.Pris = dPris2 THEN
             ASSIGN dPris2 = 0.
          IF ArtBas.OPris THEN
              ASSIGN cPrisKr   = ""
                     cPrisOren = "".
          ELSE
              ASSIGN cPrisKr   = SUBSTITUTE("191100600200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) + TRIM(STRING(iKr,">>>>9")) + CHR(13)
                     cPrisOren = SUBSTITUTE("191100400400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) + TRIM(STRING(iOren,"99")) + CHR(13).
         PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
             "191100202100020" ArtBas.Bongtekst CHR(13)
             "191100201800020" (IF ArtBas.StrTypeId > 2 AND AVAIL StrKonv THEN TRIM(StrKonv.Storl) + " " ELSE "") 
                         /*(IF AVAIL Farg AND Farg.FarBeskr <> "" THEN Farg.FarBeskr ELSE ArtBas.LevFargKod) CHR(13)*/
                         (IF ArtBas.LevFargKod <> '' THEN ArtBas.LevFargKod ELSE IF AVAIL Farg THEN Farg.FarBeskr ELSE '' ) CHR(13)
             "1F2206500900065" TRIM(StrekKode.Kode) CHR(13)
             "491100300900050" STRING(ArtBas.Vg) CHR(13)
              /*  NYTT  */
          (IF dPris2 > 0 THEN 
               "191100200300020" + "(" + TRIM(STRING(dPris2,">>>>9.99")) + ")" + CHR(13) ELSE "")
                 cPrisKr  
                 cPrisOren
/*                           SUBSTITUTE("191100600200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) TRIM(STRING(iKr,">>>>9")) CHR(13) */
/*                           SUBSTITUTE("191100400400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) TRIM(STRING(iOren,"99")) CHR(13)    */
             ("121100200000020  " + TRIM(ArtBas.LevKod) + CHR(13))
/*                  (IF Etikettlogg.individ > 0 THEN                                                           */
/*                             "121100200000020    ID:" + TRIM(STRING(Etikettlogg.Individ)) + CHR(13) ELSE "") */
                          SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
      END.
  END.
  PUT UNFORMATTED CHR(2) "F".
  OUTPUT CLOSE.
/*   IF cOrgSysPr <> "" THEN                      */
/*       ASSIGN SESSION:PRINTER-NAME = cOrgSysPr. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RFIDStd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RFIDStd Procedure
PROCEDURE RFIDStd:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    RUN cls\RFIDEtikett\runRFIDEtikettStd.p (wValgtSkriver).

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-SendFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendFil Procedure 
PROCEDURE SendFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wPrinted   AS LOG  NO-UNDO.

  
  /* Henter utskriftskommanden. */
  {syspara.i 5 21 wValgtSkriver wKommando}
  {syspar2.i 5 21 wValgtSkriver wParaListe}

  /* Kommandosyntaks          */
  /* ftp -v -s:kom\pgm\dm1.ftp */
  /*    
  message wValgtSkriver skip
          num-entries(wParaListe,";") skip
          wKommando 
          entry(1,wParaListe,";") 
          entry(2,wParaListe,";") skip
          wEtikett_Fil
          view-as alert-box.
  */
  
  ASSIGN wParaListe = REPLACE(wParaListe,"AVAIL;","").
  /* Sender fil til valgt enhet. */
  IF wKommando = "" THEN /* Standard windows skriver */
    DO:
      wPrinted = FALSE.
      
      /* Sender fil til utskrift via windows driver. */
      RUN adecomm/_osprint.p ( INPUT  CURRENT-WINDOW,
                               INPUT  ENTRY(1,wParaListe,";"),
                               INPUT  1,
                               INPUT  1,
                               INPUT  0,
                               INPUT  0,
                               OUTPUT wPrinted ).
                                   
      /* Tar bort fil etter vellykket utskrift */
      IF wPrinted = TRUE THEN
        OS-DELETE value(ENTRY(1,wParaListe,";")).
    END.
  ELSE /* Spesialoppsatt skriver. */
  CASE NUM-ENTRIES(wParaListe,";"):
    WHEN 1 THEN 
      OS-COMMAND SILENT VALUE(wKommando) 
                        VALUE(ENTRY(1,wParaListe,";")).      
    WHEN 2 THEN 
      OS-COMMAND SILENT VALUE(wKommando) 
                        VALUE(ENTRY(1,wParaListe,";"))
                        VALUE(ENTRY(2,wParaListe,";")).
    WHEN 3 THEN 
      OS-COMMAND SILENT VALUE(wKommando)
                        VALUE(ENTRY(1,wParaListe,";"))
                        VALUE(ENTRY(2,wParaListe,";"))
                        VALUE(ENTRY(3,wParaListe,";")).
    WHEN 4 THEN 
      OS-COMMAND SILENT VALUE(wKommando) 
                        VALUE(ENTRY(1,wParaListe,";"))
                        VALUE(ENTRY(2,wParaListe,";"))
                        VALUE(ENTRY(3,wParaListe,";"))
                        VALUE(ENTRY(4,wParaListe,";")).
    OTHERWISE 
      MESSAGE "Ugyldigparameteroppsett:" wParaListe
              VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
  END CASE.
  
  /* Nullstiller etikettlisten */
  FOR EACH EtikettLogg:
    DELETE EtikettLogg.
  END.

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendLayout) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendLayout Procedure 
PROCEDURE SendLayout :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Legger ut layout */
  OUTPUT to value(wEtikett_Fil) no-echo append.
    PUT CONTROL wLayout.
  OUTPUT close.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettINFO) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettINFO Procedure 
PROCEDURE SlettINFO :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH EtikettLogg WHERE EtikettLogg.Storl = "INFO":
        DELETE EtikettLogg.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UtP700) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtP700 Procedure 
PROCEDURE UtP700 :
/*------------------------------------------------------------------------------
  Purpose:     Sender etiketter til P700 skriveren.
  Parameters:  <none>
  Notes:       Denne krever spesielt oppsett av lpt port og comport.
------------------------------------------------------------------------------*/
DEF VAR wLoop AS INT NO-UNDO.
DEF VAR cant             AS CH FORMAT "X(3)".
DEF VAR tvg AS INT FORMAT "ZZZ". /*LABEL "VG".*/
DEF VAR tlopnr AS INT FORMAT "ZZZZ". /*LABEL "L\\PNR". */
DEF VAR tstorlek AS DEC FORMAT "99.9". /*LABEL "STORLEK". */
DEF VAR ttext AS CH FORMAT "X(10)". /*LABEL "FRI TEXT".*/
DEF VAR tpris AS DE FORMAT "-ZZZZ9.99". /*LABEL "PRIS". */
DEF VAR tantal AS INT FORMAT "ZZZZZ". /*LABEL "ANTAL".*/
DEF VAR loop AS INT FORMAT "ZZZZ9".
DEF VAR x AS INT FORMAT "Z9".
DEF VAR tbutik AS INT FORMAT "ZZ". /*LABEL "BUTIK".*/
DEF VAR pgm AS CH FORMAT "X(4)".
DEF VAR rubrik AS CH FORMAT "X(23)".
DEF VAR tid AS CH FORMAT "X(5)".
DEF VAR text3 AS ch FORMAT "999999999999". /*"999999999999"*/
DEF VAR text4 AS CH FORMAT "X(10)" .
DEF VAR text1 AS CH FORMAT "X(7)".
DEF VAR text2 AS CH FORMAT "X(2)".
DEF VAR text5 AS ch FORMAT "x(14)".
DEF VAR esca AS CH FORMAT "X(6)".
DEF VAR escf AS CH FORMAT "X(2)".
DEF VAR esck AS CH FORMAT "X(2)".
DEF VAR cr AS CH FORMAT "X".
DEF VAR koll AS INT FORMAT "99999".
DEF VAR eot AS CH FORMAT "X".
DEF VAR ant AS INT FORMAT "999".
DEF VAR etx AS CH FORMAT "X".
DEF VAR rad AS CH FORMAT "X(150)".
DEF VAR inrad AS CH FORMAT "X(20)".
DEF VAR skriv AS CH FORMAT "X(12)".
DEF VAR wComPort AS CHAR NO-UNDO.
DEF VAR wCommand AS CHAR NO-UNDO.

DEF VAR wEtiStreng AS CHAR NO-UNDO.
DEF VAR wStorlek   AS CHAR NO-UNDO.

/* Variabler for generering av sjekksiffer (EAN-13) */
DEF VAR e1      AS INT  FORMAT "9".
DEF VAR e2      AS INT  FORMAT "9".
DEF VAR e3      AS INT  FORMAT "9".
DEF VAR e4      AS INT  FORMAT "9".
DEF VAR e5      AS INT  FORMAT "9".
DEF VAR e6      AS INT  FORMAT "9".
DEF VAR e7      AS INT  FORMAT "9".
DEF VAR e8      AS INT  FORMAT "9".
DEF VAR e9      AS INT  FORMAT "9".
DEF VAR e10     AS INT  FORMAT "9".
DEF VAR e11     AS INT  FORMAT "9".
DEF VAR e12     AS INT  FORMAT "9".
DEF VAR e13     AS CHAR FORMAT "x".
DEF VAR s1      AS INT  FORMAT "99".
DEF VAR s2      AS INT  FORMAT "99".
DEF VAR s3      AS INT  FORMAT "999".
DEF VAR m1      AS CHAR FORMAT "xxx".

/* Kontrollvariabler til skriver. */
  ASSIGN
    escf     = CHR(27) + "F" + "1"  /* Alltid labelnummer = 1  */
    esca     = CHR(2) + CHR(27) + "A"
    esck     = CHR(27) + "K"
    cr       = CHR(13)
    eot      = CHR(4)
    etx      = CHR(3).
    
  {syspar2.i 5 21 wValgtSkriver wComPort}
  wComPort = ENTRY(2,wComPort,";").
  {syspara.i 5 21 wValgtSkriver wCommand}
  
  /*Ut med alle etikettene i filen. */
  FOR EACH EtikettLogg
     BREAK /* by EtikettLogg.butik */
           BY EtikettLogg.vg
           BY EtikettLogg.lopnr
           BY EtikettLogg.Storl:
           
     /* Henter artikkelinformasjonen */
     FIND ArtBas NO-LOCK WHERE
       ArtBas.Vg = EtikettLogg.Vg AND
       ArtBas.LopNr = EtikettLogg.LopNr NO-ERROR.
     IF NOT AVAILABLE ArtBas THEN
       DO:
         MESSAGE "Ukjent artikkel " Etikettlogg.Vg etikettlogg.LopNr VIEW-AS ALERT-BOX.
         NEXT.
       END.
           
        /* Konverterer st|rrelsen.                   */
        /* Nb: Nullstiller alfanumeriske st|rrelser. */
        wstorlek = "".
        DO i = 1 TO LENGTH(EtikettLogg.Storl):

            IF SUBSTRING(EtikettLogg.Storl,i,1) = " " THEN
              wStorlek = wStorlek + "0". /* Space */

            ELSE IF SUBSTRING(EtikettLogg.Storl,i,1) = "." THEN
              NEXT. /* Punktum strippes bort. */

            ELSE IF (SUBSTRING(EtikettLogg.Storl,i,1) < "0" OR
                     SUBSTRING(EtikettLogg.Storl,i,1) > "9") THEN
              wStorlek = wStorlek + "0". /* Alfanumeriske st|rrelser. */

            ELSE
              wStorlek = wStorlek + substring(EtikettLogg.Storl,i,1). /* Tar vare p} verdien */
        END.

        /*
        " 3.5"  "035"
        " 3"    "03"
        "35"    "35"
        "35.5"  "355"
        */

        ASSIGN /* Trimmer bort ledende space og gjør lengden = 4. */
          wStorlek = TRIM(wStorlek)
          wStorlek = (IF LENGTH(wStorlek) = 2
                        THEN wStorlek + "0"
                        ELSE wStorlek)
          wStorlek = /*"0" + */ wStorlek.


        ASSIGN
          ant   = EtikettLogg.Ant
          text1 = SUBSTRING(STRING(EtikettLogg.Pris,"ZZZZ9.99"),1,5)
          text2 = SUBSTRING(STRING(EtikettLogg.Pris,"ZZZZ9.99"),7,2)
          Text5 = CAPS(SUBSTRING(ArtBas.LevKod,1,14))
         Text3 =  "0"
               + string(EtikettLogg.vg,"99")
               + string(EtikettLogg.lopnr,"9999")
               + "0"
               + substring(STRING(wStorlek,"99.9"),1,2)
               + substring(STRING(wStorlek,"99.9"),4,4)
               + "0".
        /********************  Genererer sjekksiffer  *********************/

       ASSIGN
        e12  = INTEGER(SUBSTRING(text3,12,1))
        e10  = INTEGER(SUBSTRING(text3,10,1))
        e8   = INTEGER(SUBSTRING(text3,8,1))
        e6   = INTEGER(SUBSTRING(text3,6,1))
        e4   = INTEGER(SUBSTRING(text3,4,1))
        e2   = INTEGER(SUBSTRING(text3,2,1)).

       ASSIGN
        s1   = (e2 + e4 + e6 + e8 + e10 + e12) * 3.

       ASSIGN
        e11  = INTEGER(SUBSTRING(text3,11,1))
        e9   = INTEGER(SUBSTRING(text3,9,1))
        e7   = INTEGER(SUBSTRING(text3,7,1))
        e5   = INTEGER(SUBSTRING(text3,5,1))
        e3   = INTEGER(SUBSTRING(text3,3,1))
        e1   = INTEGER(SUBSTRING(text3,1,1)).

       ASSIGN
        s2   = e1 + e3 + e5 + e7 + e9 + e11.

       ASSIGN
        s3   = s1 + s2
        m1   = STRING(s3,"999").
        
        IF INTEGER(SUBSTRING(m1,3,1)) = 0 THEN 
          e13 = "0".
       ELSE  e13  = STRING(10 - integer(SUBSTRING(m1,3,1)),"9").

        text3 = text3 + trim(e13).
        /*******************  Sjekksiffer ferdig  *************************/

        text4 = STRING(EtikettLogg.vg) + 
                "/" + 
                STRING(EtikettLogg.lopnr) + 
                " " + 
                string(EtikettLogg.Storl).

        ASSIGN
         cant = STRING(ant,"999")
          wEtiStreng = Text1 + cr + /* Kroner     */
                      Text2 + cr + /* Øre        */
                      Text3 + cr + /* EAN-13     */
                      Text4 + cr + /* Varegruppe */
                      Text5 + cr + /* Lev.ArtNr. */
                      Text1 + cr +
                      Text2 + cr +
                      Text3 + cr +
                      Text4 + cr +
                      Text5 + cr.
        DO wLoop = 1 TO 20:
          wEtiStreng = wEtiStreng + cr.
        END.
        wEtiStreng = EscA +
                     EscF +
                     wEtiStreng + 
                     EscK + 
                     cAnt + 
                     EtX +
                     string(LENGTH(wEtiStreng) + 12,"99999") + 
                     EoT.

       OUTPUT to VALUE(wEtikett_Fil).
         PUT CONTROL
            wEtiStreng.
       OUTPUT CLOSE.

       DOS SILENT VALUE(wCommand) VALUE(wEtikett_Fil) VALUE(wComPort).
       
       /* Gir skriveren en pustepause for † hinde overflow i buffer. */
       PAUSE ant + ((ant / 100) * 20) NO-MESSAGE.
       
       IF SEARCH(wEtikett_Fil) <> ? THEN
         OS-DELETE value(wEtikett_Fil).
         
       /* Døden */
       DELETE EtikettLogg.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UtP700-2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtP700-2 Procedure 
PROCEDURE UtP700-2 :
/*------------------------------------------------------------------------------
  Purpose:     Sender etiketter til P700 skriveren.
  Parameters:  <none>
  Notes:       Denne krever spesielt oppsett av lpt port og comport.
------------------------------------------------------------------------------*/
DEF VAR wLoop AS INT NO-UNDO.
DEF VAR cant             AS CH FORMAT "X(3)".
DEF VAR tvg AS INT FORMAT "ZZZ". /*LABEL "VG".*/
DEF VAR tlopnr AS INT FORMAT "ZZZZ". /*LABEL "L\\PNR". */
DEF VAR tstorlek AS DEC FORMAT "99.9". /*LABEL "STORLEK". */
DEF VAR ttext AS CH FORMAT "X(10)". /*LABEL "FRI TEXT".*/
DEF VAR tpris AS DE FORMAT "-ZZZZ9.99". /*LABEL "PRIS". */
DEF VAR tantal AS INT FORMAT "ZZZZZ". /*LABEL "ANTAL".*/
DEF VAR loop AS INT FORMAT "ZZZZ9".
DEF VAR x AS INT FORMAT "Z9".
DEF VAR tbutik AS INT FORMAT "ZZ". /*LABEL "BUTIK".*/
DEF VAR pgm AS CH FORMAT "X(4)".
DEF VAR rubrik AS CH FORMAT "X(23)".
DEF VAR tid AS CH FORMAT "X(5)".
DEF VAR text3 AS ch FORMAT "999999999999". /*"999999999999"*/
DEF VAR text4 AS CH FORMAT "X(10)" .
DEF VAR text1 AS CH FORMAT "X(7)".
DEF VAR text2 AS CH FORMAT "X(2)".
DEF VAR text5 AS ch FORMAT "x(14)".
DEF VAR text6 AS ch FORMAT "x(14)".
DEF VAR esca AS CH FORMAT "X(6)".
DEF VAR escf AS CH FORMAT "X(2)".
DEF VAR esck AS CH FORMAT "X(2)".
DEF VAR cr AS CH FORMAT "X".
DEF VAR koll AS INT FORMAT "99999".
DEF VAR eot AS CH FORMAT "X".
DEF VAR ant AS INT FORMAT "999".
DEF VAR etx AS CH FORMAT "X".
DEF VAR rad AS CH FORMAT "X(150)".
DEF VAR inrad AS CH FORMAT "X(20)".
DEF VAR skriv AS CH FORMAT "X(12)".
DEF VAR wComPort AS CHAR NO-UNDO.
DEF VAR wCommand AS CHAR NO-UNDO.

DEF VAR wEtiStreng AS CHAR NO-UNDO.
DEF VAR wStorlek   AS CHAR NO-UNDO.

/* Variabler for generering av sjekksiffer (EAN-13) */
DEF VAR e1      AS INT  FORMAT "9".
DEF VAR e2      AS INT  FORMAT "9".
DEF VAR e3      AS INT  FORMAT "9".
DEF VAR e4      AS INT  FORMAT "9".
DEF VAR e5      AS INT  FORMAT "9".
DEF VAR e6      AS INT  FORMAT "9".
DEF VAR e7      AS INT  FORMAT "9".
DEF VAR e8      AS INT  FORMAT "9".
DEF VAR e9      AS INT  FORMAT "9".
DEF VAR e10     AS INT  FORMAT "9".
DEF VAR e11     AS INT  FORMAT "9".
DEF VAR e12     AS INT  FORMAT "9".
DEF VAR e13     AS CHAR FORMAT "x".
DEF VAR s1      AS INT  FORMAT "99".
DEF VAR s2      AS INT  FORMAT "99".
DEF VAR s3      AS INT  FORMAT "999".
DEF VAR m1      AS CHAR FORMAT "xxx".

/* Kontrollvariabler til skriver. */
  ASSIGN
    escf     = CHR(27) + "F" + "1"  /* Alltid labelnummer = 1  */
    esca     = CHR(2) + CHR(27) + "A"
    esck     = CHR(27) + "K"
    cr       = CHR(13)
    eot      = CHR(4)
    etx      = CHR(3).
    
  {syspar2.i 5 21 wValgtSkriver wComPort}
  wComPort = ENTRY(2,wComPort,";").
  {syspara.i 5 21 wValgtSkriver wCommand}
  
  /*Ut med alle etikettene i filen. */
  FOR EACH EtikettLogg
     BREAK BY EtikettLogg.butik
           BY EtikettLogg.vg
           BY EtikettLogg.lopnr
           BY EtikettLogg.Storl:
           
     /* Henter artikkelinformasjonen */
     FIND ArtBas NO-LOCK WHERE
       ArtBas.Vg = EtikettLogg.Vg AND
       ArtBas.LopNr = EtikettLogg.LopNr NO-ERROR.
     IF NOT AVAILABLE ArtBas THEN
       DO:
         MESSAGE "Ukjent artikkel " Etikettlogg.Vg etikettlogg.LopNr VIEW-AS ALERT-BOX.
         NEXT.
       END.
           
        /* Konverterer st|rrelsen.                   */
        /* Nb: Nullstiller alfanumeriske st|rrelser. */
        wstorlek = "".
        DO i = 1 TO LENGTH(EtikettLogg.Storl):

            IF SUBSTRING(EtikettLogg.Storl,i,1) = " " THEN
              wStorlek = wStorlek + "0". /* Space */

            ELSE IF SUBSTRING(EtikettLogg.Storl,i,1) = "." THEN
              NEXT. /* Punktum strippes bort. */

            ELSE IF (SUBSTRING(EtikettLogg.Storl,i,1) < "0" OR
                     SUBSTRING(EtikettLogg.Storl,i,1) > "9") THEN
              wStorlek = wStorlek + "0". /* Alfanumeriske st|rrelser. */

            ELSE
              wStorlek = wStorlek + substring(EtikettLogg.Storl,i,1). /* Tar vare p} verdien */
        END.

        /*
        " 3.5"  "035"
        " 3"    "03"
        "35"    "35"
        "35.5"  "355"
        */

        ASSIGN /* Trimmer bort ledende space og gjør lengden = 4. */
          wStorlek = TRIM(wStorlek)
          wStorlek = (IF LENGTH(wStorlek) = 2
                        THEN wStorlek + "0"
                        ELSE wStorlek)
          wStorlek = /*"0" + */ wStorlek.

        ASSIGN
          ant   = EtikettLogg.Ant
          text1 = SUBSTRING(STRING(EtikettLogg.Pris,"ZZZ9.99"),1,4)
          text2 = SUBSTRING(STRING(EtikettLogg.Pris,"ZZZ9.99"),6,2)
          Text3 = STRING(EtikettLogg.vg,"999")
                  + string(EtikettLogg.lopnr,"9999")
                  + "00"
                  + substring(STRING(wStorlek,"99.9"),1,2)
                  + substring(STRING(wStorlek,"99.9"),4,4).
          Text6 = SUBSTRING(STRING(wstorlek,"99.9"),1,2) 
                  + "." 
                  + substring(STRING(wstorlek,"99.9"),4,4).
        /********************  Genererer sjekksiffer  *********************/

       ASSIGN
        e12  = INTEGER(SUBSTRING(text3,12,1))
        e10  = INTEGER(SUBSTRING(text3,10,1))
        e8   = INTEGER(SUBSTRING(text3,8,1))
        e6   = INTEGER(SUBSTRING(text3,6,1))
        e4   = INTEGER(SUBSTRING(text3,4,1))
        e2   = INTEGER(SUBSTRING(text3,2,1)).

       ASSIGN
        s1   = (e2 + e4 + e6 + e8 + e10 + e12) * 3.

       ASSIGN
        e11  = INTEGER(SUBSTRING(text3,11,1))
        e9   = INTEGER(SUBSTRING(text3,9,1))
        e7   = INTEGER(SUBSTRING(text3,7,1))
        e5   = INTEGER(SUBSTRING(text3,5,1))
        e3   = INTEGER(SUBSTRING(text3,3,1))
        e1   = INTEGER(SUBSTRING(text3,1,1)).

       ASSIGN
        s2   = e1 + e3 + e5 + e7 + e9 + e11.

       ASSIGN
        s3   = s1 + s2
        m1   = STRING(s3,"999").
        
        IF INTEGER(SUBSTRING(m1,3,1)) = 0 THEN 
          e13 = "0".
       ELSE  e13  = STRING(10 - integer(SUBSTRING(m1,3,1)),"9").

        text3 = text3 + trim(e13).
        /*******************  Sjekksiffer ferdig  *************************/

      ASSIGN
        Text4 = STRING(EtikettLogg.vg,"999") + 
                " " + 
                STRING(EtikettLogg.lopnr,"9999")                 
        Text5 = CAPS(SUBSTRING(artbas.beskr,1,14)).                

        ASSIGN
         cant = STRING(ant,"999")
          wEtiStreng = Text1 + cr + 
                       Text2 + cr + 
                       Text3 + cr + 
                       Text4 + cr + 
                       Text1 + cr +
                       Text2 + cr +
                       Text3 + cr +
                       Text4 + cr +
                       Text5 + cr +
                       Text5 + cr +
                       Text5 + cr +
                       Text5 + cr +
                       Text6 + cr +
                       Text6 + cr.
                       
        DO wLoop = 1 TO 16:
          wEtiStreng = wEtiStreng + cr.
        END.
        wEtiStreng = EscA +
                     EscF +
                     wEtiStreng + 
                     EscK + 
                     cAnt + 
                     EtX +
                     string(LENGTH(wEtiStreng) + 12,"99999") + 
                     EoT.

       OUTPUT to VALUE(wEtikett_Fil).
         PUT CONTROL
            wEtiStreng.
       OUTPUT CLOSE.

       DOS SILENT VALUE(wCommand) VALUE(wEtikett_Fil) VALUE(wComPort).
       
       /* Gir skriveren en pustepause for † hinde overflow i buffer. */
       PAUSE ant + ((ant / 100) * 20) NO-MESSAGE.
       
       IF SEARCH(wEtikett_Fil) <> ? THEN
         OS-DELETE value(wEtikett_Fil).
         
       /* Døden */
       DELETE EtikettLogg.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Utskrift) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift Procedure 
PROCEDURE Utskrift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cTeller AS CHARACTER  NO-UNDO.
  /*Ut med alle etikettene i filen. */
  FOR EACH EtikettLogg
     BREAK BY EtikettLogg.butik
           BY EtikettLogg.vg
           BY EtikettLogg.lopnr
           BY EtikettLogg.Storl:

        /* Konverterer st|rrelsen.                   */
        /* Nb: Nullstiller alfanumeriske st|rrelser. */
        tstorlek = "".
        DO i = 1 TO LENGTH(EtikettLogg.Storl):

            IF SUBSTRING(EtikettLogg.Storl,i,1) = " " THEN
              TStorlek = TStorlek + "0". /* Space */

            ELSE IF SUBSTRING(EtikettLogg.Storl,i,1) = "." THEN
              NEXT. /* Punktum strippes bort. */

            ELSE IF (SUBSTRING(EtikettLogg.Storl,i,1) < "0" OR
                     SUBSTRING(EtikettLogg.Storl,i,1) > "9") THEN
              TStorlek = TStorlek + "0". /* Alfanumeriske st|rrelser. */

            ELSE
              TStorlek = TStorlek + substring(EtikettLogg.Storl,i,1). /* Tar vare p} verdien */
        END.

        /*
        " 3.5"  "035"
        " 3"    "03"
        "35"    "35"
        "35.5"  "355"
        */

        ASSIGN /* Trimmer bort ledende space og gjør lengden = 4. */
          TStorlek = TRIM(TStorlek)
          TStorlek = (IF LENGTH(TStorlek) = 2
                        THEN TStorlek + "0"
                        ELSE TStorlek)
          TStorlek = "0" + TStorlek.

        /* Initierer telleverk */
        IF SEARCH("teller") <> ? THEN
        DO:
            INPUT from teller no-echo.
            IMPORT UNFORMATTED cTeller.
            ASSIGN teller = INT(cTeller).
/*             set teller. */
            INPUT close.
            IF (teller = 0 OR teller > 9999) THEN teller = 1.
        END.
        ELSE teller = 1.

        blinje[1]  = "B,1,N," + string(EtikettLogg.Ant) + "|" + chr(10).
        blinje[2]  = "1," + """"
                          + string((teller),"9999")
                          + """"
                          + "|" + chr(10).
        blinje[3]  = "2," + """"
                          + string((teller),"9999")
                          + """"
                          + "|" + chr(10).
        blinje[4]  = "3," + """"
                          + string(EtikettLogg.pris,"zzzz9.99")
                          + """"
                          + "|" + chr(10).
        blinje[5]  = "4," + """"
                          + string(EtikettLogg.pris,"zzzz9.99")
                          + """"
                          + "|" + chr(10).
        blinje[6]  = "5," + """"
                          + string(EtikettLogg.vg,"999")
                          + string(EtikettLogg.lopnr,"9999")
                          + "0"
                          + tstorlek
                          + """"
                          + "|" + chr(10).
        blinje[7]  = "6," + """"
                          + string(EtikettLogg.vg,"999")
                          + string(EtikettLogg.lopnr,"9999")
                          + "0"
                          + tstorlek
                          + """"
                          + "|" + chr(10).
        blinje[8]  = "7," + """"
                          + string(EtikettLogg.vg)
                          + "-"
                          + string(EtikettLogg.lopnr)
                          + """"
                          + "|" + chr(10).
        blinje[9]  = "8," + """"
                          + string(EtikettLogg.vg)
                          + "-"
                          + string(EtikettLogg.lopnr)
                          + """"
                          + "|" + chr(10).
        blinje[10]  = "9," + """"
                          + EtikettLogg.storl
                          + """"
                          + "|" + chr(10).
        blinje[11]  = "10," + """"
                          + EtikettLogg.storl
                          + """"
                          + "|" + chr(10).
                          

        /* Ut med dr.... */
        DO:
            /* wBatch informasjon. */
            wBatch = "~{".
            DO y = 1 TO 11:
                wBatch = wBatch + blinje[y].
            END.
            wBatch = wBatch + "~}".            

            /* ]pner stream */
            OUTPUT to value(wEtikett_Fil) no-echo append.

            /* Skriver etikett. */
            PUT CONTROL wBatch.

            /* Lukker stream */
            OUTPUT close.

        END.

        /* Resetter teller */
        DO:
            teller = teller + EtikettLogg.Ant.
            OUTPUT to teller no-echo.
            EXPORT teller.
            OUTPUT close.
        END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Utskrift-ECE4_Pris_Under) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift-ECE4_Pris_Under Procedure 
PROCEDURE Utskrift-ECE4_Pris_Under :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Intermec EasyCoder E4      
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dePris     AS DECIMAL                        NO-UNDO.
  DEFINE VARIABLE iKr        AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren      AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iRowEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iRow       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cRow       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCol       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cCol       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iColEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iEtikettnr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount     AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iCount2    AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE dPris2     AS DECIMAL  DECIMALS 2  NO-UNDO.
  DEFINE VARIABLE wOK        AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFormat AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cColKronor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColOren   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cOrgSysPr  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cInfoEtiTxt AS CHARACTER EXTENT 4 NO-UNDO.
  DEFINE VARIABLE cEtifil     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cStr1       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cStr2       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cStr3       AS CHARACTER  NO-UNDO.
  ASSIGN cColKronor = "200,200,200,185,160"
         cColOren   = "240,275,310,330,340".
  /* används för att kunna veta startetikett: Ant = startetikett */
  FOR EACH EtikettLogg WHERE EtikettLogg.SeqNr = 0 AND
                             EtikettLogg.Storl = "STARTETIKETT":
      DELETE EtikettLogg.
  END.
/*   IF SESSION:PARAMETER <> "" THEN DO iCount = 1 TO NUM-ENTRIES(SESSION:PARAMETER): */
/*       IF ENTRY(iCount,SESSION:PARAMETER) BEGINS "EPRINTER" AND                     */
/*          NUM-ENTRIES(ENTRY(iCount,SESSION:PARAMETER),"=") = 2 THEN DO:             */
/*          ASSIGN cWinPrinterName = ENTRY(2,ENTRY(iCount,SESSION:PARAMETER),"=").    */
/*          LEAVE.                                                                    */
/*       END.                                                                         */
/*   END.                                                                             */
/*   IF cWinPrinterName = "" THEN DO:                                                 */
/*       SYSTEM-DIALOG PRINTER-SETUP UPDATE wOK.                                      */
/*       IF NOT wOK THEN                                                              */
/*           RETURN NO-APPLY.                                                         */
/*   END.                                                                             */
/*   ELSE DO:                                                                         */
/*       ASSIGN cOrgSysPr = SESSION:PRINTER-NAME.                                     */
/*   END.                                                                             */
/*   /* hämta printernamn om cWinPrinterName = "" */                                  */
/*   IF cWinPrinterName = "" THEN                                                     */
/*       OUTPUT TO PRINTER VALUE(SESSION:PRINTER-NAME) /* CONVERT TARGET "IBM850" */. */
/* /*       OUTPUT TO PRINTER. */                                                     */
/*   ELSE DO:                                                                         */
/*       ASSIGN SESSION:PRINTER-NAME = cWinPrinterName.                               */
/* /*       OUTPUT TO PRINTER. */                                                     */
/*       OUTPUT TO PRINTER VALUE(cWinPrinterName) /* CONVERT TARGET "IBM850" */ .     */
/*   END.                                                                             */
/*   ASSIGN cEtifil = SESSION:TEMP-DIR + "ECELBL.txt". */
/*   OUTPUT TO VALUE(cEtifil).                         */
/*   OUTPUT TO PRINTER "ECE4TextOnly" CONVERT TARGET "IBM850". */
    IF cWinPrinterName = "" THEN DO:
        IF lTermKlient = TRUE THEN
            OUTPUT TO VALUE(SESSION:PRINTER-NAME).
        ELSE
            OUTPUT TO PRINTER VALUE(SESSION:PRINTER-NAME).
    END.
    ELSE DO:
        IF lTermKlient = TRUE THEN DO:
            OUTPUT TO VALUE(cWinPrinterName).
        END.
        ELSE DO:
            ASSIGN SESSION:PRINTER-NAME = cWinPrinterName.
            OUTPUT TO PRINTER VALUE(cWinPrinterName).
        END.
    END.

  PUT UNFORMATTED
      'SYSVAR(18)=0' SKIP
      'INPUT OFF' SKIP
      'PRINT KEY ON' SKIP
      'SETUP "SERVICE,MEDIA SIZE,LENGTH,211"' SKIP
      'SETUP "SERVICE,MEDIA SIZE,XSTART,0"' SKIP
      'SETUP "SERVICE,MEDIA SIZE,WIDTH,832"' SKIP
      'SETUP "SERVICE,MEDIA TYPE,LABEL (w GAPS)"' SKIP
      'SETUP "DETECTION,FEEDADJ,STARTADJ,-152"' SKIP
      'SETUP "DETECTION,FEEDADJ,STOPADJ,0"' SKIP
      'SETUP "SERVICE,PERFORMANCE,NORMAL"' SKIP
      'SETUP "SERVICE,PRINT_DEFS,NEW_SUPPLIES,GQ90"' SKIP
      'SETUP "CONTRAST,5"' SKIP
      'LTS& OFF' SKIP.
/*                                                                                                                                */
  FOR EACH EtikettLogg WHERE EtikettLogg.Storl <> "":
      IF NOT EtikettLogg.Storl = "INFO" THEN DO:
          FIND Strekkode WHERE Strekkode.kode = EtikettLogg.Storl NO-LOCK NO-ERROR.
          FIND ArtBas OF StrekKode NO-LOCK.
          FIND Farg OF Artbas NO-LOCK NO-ERROR.

          FIND ArtPris OF Artbas NO-LOCK WHERE 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
            FIND ArtPris OF Artbas NO-LOCK WHERE 
              ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.

          FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
          ASSIGN iKr   = TRUNC(EtikettLogg.Pris,0)
                 iOren = (EtikettLogg.Pris - TRUNC(EtikettLogg.Pris,0)) * 100.
      END.
      IF EtikettLogg.Storl = "INFO" THEN DO:
          ASSIGN cInfoEtiTxt = "".
          DO iCount = 1 TO NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)):
              IF iCount = 5 THEN
                  LEAVE.
/*               ASSIGN cInfoEtiTxt[iCount] = ENTRY(iCount,Etikettlogg.Bongtekst,CHR(1)). */
              /* fix om vi har 'bubbelfnutt' i txt */
              ASSIGN cInfoEtiTxt[iCount] = REPLACE(ENTRY(iCount,Etikettlogg.Bongtekst,CHR(1)),'"','').
          END.
          PUT UNFORMATTED
              'INPUT OFF' SKIP
              'FORMAT INPUT CHR$(2),CHR$(3),CHR$(13)' SKIP
              'KILL "LBLSOFT.LAY"' SKIP
              'LAYOUT INPUT "LBLSOFT.LAY"' SKIP
              'NASC -2:AN 7' SKIP
              'MAG 1,1' SKIP
              'DIR 1:PP 27,193:FT "Swiss 721 BT":FS 8:FL 0:NI' SKIP
              SUBSTITUTE('PT "&1"',cInfoEtiTxt[1]) SKIP
              SUBSTITUTE('PP 27,143:NI:PT "&1"',cInfoEtiTxt[2]) SKIP
              SUBSTITUTE('PP 27,93:NI:PT "&1"',cInfoEtiTxt[3]) SKIP
              SUBSTITUTE('PP 27,43:NI:PT "&1"',cInfoEtiTxt[4]) SKIP
              'LAYOUT END' SKIP
              'LAYOUT RUN "LBLSOFT.LAY"' SKIP
              'INPUT ON' SKIP
              'PF' SKIP.
              
      END.
      ELSE IF StrekKode.KodeType = 0 THEN
          PUT UNFORMATTED 
              'INPUT OFF' SKIP
              'FORMAT INPUT CHR$(2),CHR$(3),CHR$(13)' SKIP
              'KILL "LBLSOFT.LAY"' SKIP
              'LAYOUT INPUT "LBLSOFT.LAY"' SKIP
              'NASC -2:AN 7' SKIP
              SUBSTITUTE('BF "Monospace 821 BT",7:PP 75,154:DIR 1:BT "CODE39":BM 1:BR 5,2:BH 71:PB "&1"',Strekkode.Kode) SKIP
              'MAG 1,1' SKIP
              SUBSTITUTE('PP 99,75:FT "Swiss 721 BT":FS 10:FL 0:NI:PT "PLU: &1"',StrekKode.Kode) SKIP
/*               SUBSTITUTE('PP 27,192:FS 8:NI:PT "&1"',ArtBas.Bongtekst) SKIP */
              /* bubbelfnuttfix */
              SUBSTITUTE('PP 27,192:FS 8:NI:PT "&1"',REPLACE(ArtBas.Bongtekst,'"','')) SKIP
              'LAYOUT END' SKIP
              'LAYOUT RUN "LBLSOFT.LAY"' SKIP
              'INPUT ON' SKIP
              SUBSTITUTE('PF &1',IF EtikettLogg.Ant > 1 THEN STRING(EtikettLogg.Ant) ELSE "") SKIP.
      ELSE DO:
          /*  NYTT Vi har ett Etikettlogg.pris2 som visar ordinarie pris */
          IF ArtBas.OPris THEN
              ASSIGN dPris2 = 0.
          ELSE
              ASSIGN dPris2 = IF Etikettlogg.pris2 > 0 THEN Etikettlogg.Pris2 ELSE
                              IF ArtPris.Tilbud = TRUE THEN ArtPris.Pris[1] ELSE 0.
          /* För att inte få in ordinarie pris vid inleverans när varan står på tilbud */
          IF EtikettLogg.Pris = dPris2 THEN
             ASSIGN dPris2 = 0.
          ASSIGN cStr1 = (IF ArtBas.StrTypeId > 2 AND AVAIL StrKonv THEN TRIM(StrKonv.Storl) + " " ELSE "") +
/*                  (IF AVAIL Farg THEN Farg.FarBeskr ELSE ""). */
                 /* dubbelfnuttfix */
                 (IF AVAIL Farg THEN REPLACE(Farg.FarBeskr,'"','') ELSE "").
                 cStr2 = IF iKr > 9999 THEN
                             'DIR 1:PP 132,68:FS 16:NI:PT "&1"'
                    ELSE IF iKr > 999 THEN
                             'DIR 1:PP 145,68:FS 16:NI:PT "&1"'
                    ELSE IF iKr > 99 THEN
                        'DIR 1:PP 158,68:FS 16:NI:PT "&1"'
                    ELSE IF iKr > 9 THEN
                        'DIR 1:PP 171,68:FS 16:NI:PT "&1"'
                    ELSE
                        'DIR 1:PP 184,68:FS 16:NI:PT "&1"'.
                 cStr2 = IF ArtBas.OPris THEN "" ELSE cStr2.
                 cStr3 = IF Etikettlogg.individ > 0 THEN SUBSTITUTE('PP 100,28:FS 8:NI:PT "&1"',"ID:" + TRIM(STRING(Etikettlogg.Individ))) + CHR(13) ELSE "".
          PUT UNFORMATTED
             'INPUT OFF' SKIP
             'FORMAT INPUT CHR$(2),CHR$(3),CHR$(13)' SKIP
             'KILL "LBLSOFT.LAY"' SKIP
             'LAYOUT INPUT "LBLSOFT.LAY"' SKIP
             'NASC -2:AN 7' SKIP
             'MAG 1,1' SKIP
/*              SUBSTITUTE('DIR 1:PP 24,201:FT "Swiss 721 BT":FS 8:FL 0:NI:PT "&1"',ArtBas.Bongtekst) SKIP */
             /* dubbelfnuttfix */
             SUBSTITUTE('DIR 1:PP 24,201:FT "Swiss 721 BT":FS 8:FL 0:NI:PT "&1"',REPLACE(ArtBas.Bongtekst,'"','')) SKIP
             SUBSTITUTE('BF "Monospace 821 BT",7:BF ON:PP 93,146:BT "EAN13":BM 2:BH 56:PB "&1"',SUBSTR(TRIM(StrekKode.Kode),1,12)) SKIP
             SUBSTITUTE('PP 24,175:NI:PT "&1"',cStr1) SKIP
             SUBSTITUTE('DIR 4:PP 28,81:FS 10:NI:PT "&1"',STRING(ArtBas.Vg)) SKIP
             SUBSTITUTE(cStr2,STRING(iKr,">>>>9")) SKIP
             SUBSTITUTE('PP 258,65:FS 11:NI:PT "&1"',IF ArtBas.OPris THEN "" ELSE STRING(iOren,"99")) SKIP 
             cStr3
             (IF dPris2 > 0 THEN SUBSTITUTE('PP 24,55:FS 8:NI:PT "~(&1~)"',STRING(dPris2,">>>>9.99")) ELSE "") SKIP
             'LAYOUT END' SKIP
             'LAYOUT RUN "LBLSOFT.LAY"' SKIP
             'INPUT ON' SKIP
             SUBSTITUTE('PF &1',IF EtikettLogg.Ant > 1 THEN STRING(EtikettLogg.Ant) ELSE "") SKIP.
      END.
  END.
  PUT UNFORMATTED 'FORMFEED' SKIP 'PRINT KEY OFF' SKIP.
  OUTPUT CLOSE.
/*   OS-COMMAND SILENT VALUE("TYPE " + cEtifil + " > LPT1"). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Utskrift-METO-mn-4Org) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift-METO-mn-4Org Procedure 
PROCEDURE Utskrift-METO-mn-4Org :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dePris     AS DECIMAL                        NO-UNDO.
  DEFINE VARIABLE iKr        AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren      AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iRowEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iRow       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cRow       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCol       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cCol       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iColEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iEtikettnr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount     AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iCount2    AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE dPris2     AS DECIMAL DECIMALS 2   NO-UNDO.
  DEFINE VARIABLE wOK        AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFormat AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cColKronor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColOren   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cOrgSysPr  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cInfoEtiTxt AS CHARACTER EXTENT 4 NO-UNDO.
  DEFINE VARIABLE cPrisKr   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrisOren AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cValdPrinter AS CHARACTER  NO-UNDO.
  ASSIGN cColKronor = "200,200,200,185,160"
         cColOren   = "240,275,310,330,340".
  /* används för att kunna veta startetikett: Ant = startetikett */
  FOR EACH EtikettLogg WHERE EtikettLogg.SeqNr = 0 AND
                             EtikettLogg.Storl = "STARTETIKETT":
      DELETE EtikettLogg.
  END.
  IF SESSION:PARAMETER <> "" THEN DO iCount = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
      IF ENTRY(iCount,SESSION:PARAMETER) BEGINS "EPRINTER" AND
         NUM-ENTRIES(ENTRY(iCount,SESSION:PARAMETER),"=") = 2 THEN DO:
         ASSIGN cWinPrinterName = ENTRY(2,ENTRY(iCount,SESSION:PARAMETER),"=").
         LEAVE.
      END.
  END.
  IF NUM-ENTRIES(cWinPrinterName,"&") > 1 THEN DO:
      RUN d-VelgGenerellCombo.w ("Velg etikettprinter",REPLACE(REPLACE(cWinPrinterName,"|",","),"&",","),INPUT-OUTPUT cValdPrinter).
      IF cValdPrinter = "" THEN
          RETURN.
      ASSIGN cWinPrinterName = cValdPrinter.
      RETURN.
  END.
  IF cWinPrinterName = "" THEN DO:
      SYSTEM-DIALOG PRINTER-SETUP UPDATE wOK.
      IF NOT wOK THEN
          RETURN NO-APPLY.
  END.
  ELSE DO:
      ASSIGN cOrgSysPr = SESSION:PRINTER-NAME.
  END.
  /* hämta printernamn om cWinPrinterName = "" */
  IF cWinPrinterName = "" THEN DO:
      IF lTermKlient = TRUE THEN
          OUTPUT TO VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
      ELSE
          OUTPUT TO PRINTER VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
  END.
  ELSE DO:
      IF lTermKlient = TRUE THEN DO:
          OUTPUT TO VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      END.
      ELSE DO:
          ASSIGN SESSION:PRINTER-NAME = cWinPrinterName.
          OUTPUT TO PRINTER VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      END.
  END.
/*   ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "M0400" + CHR(2) + "O0000". /* m=metric */ */
  ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "M0600" + CHR(2) + "O0000". /* m=metric */
  PUT CONTROL cFormat.
  cFormat = CHR(2) + "LW400" + CHR(13). /* CR.*/. 
  PUT CONTROL cFormat.

  FOR EACH EtikettLogg WHERE EtikettLogg.Storl <> "":
      IF NOT EtikettLogg.Storl = "INFO" THEN DO:
          FIND Strekkode WHERE Strekkode.kode = EtikettLogg.Storl NO-LOCK NO-ERROR.
          FIND ArtBas OF StrekKode NO-LOCK NO-ERROR.
          FIND Farg OF Artbas NO-LOCK NO-ERROR.

          FIND ArtPris OF Artbas NO-LOCK WHERE 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
            FIND ArtPris OF Artbas NO-LOCK WHERE 
              ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.

          FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
          ASSIGN iKr   = TRUNC(EtikettLogg.Pris,0)
                 iOren = (EtikettLogg.Pris - TRUNC(EtikettLogg.Pris,0)) * 100.
      END.
      IF EtikettLogg.Storl = "INFO" THEN DO:
          ASSIGN cInfoEtiTxt = "".
          DO iCount = 1 TO NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)):
              IF iCount = 5 THEN
                  LEAVE.
              ASSIGN cInfoEtiTxt[iCount] = ENTRY(iCount,Etikettlogg.Bongtekst,CHR(1)).
          END.
          PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
                             "121100802100020" cInfoEtiTxt[1] CHR(13)
                             "121100801500020" cInfoEtiTxt[2] CHR(13)
                             "121100801000020" cInfoEtiTxt[3] CHR(13)
                             "121100800150020" cInfoEtiTxt[4] CHR(13)
                                                          "E" CHR(13).
      END.
      ELSE IF StrekKode.KodeType = 0 THEN
          PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
                             "191100202000020" ArtBas.Bongtekst CHR(13)
                             "1a0000000650080" TRIM(StrekKode.Kode) CHR(13)
                             "191100300150100" "PLU: " TRIM(StrekKode.Kode) CHR(13)
                          SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
      ELSE DO:
          /*  NYTT Vi har ett Etikettlogg.pris2 som visar ordinarie pris */
          IF NOT ArtBas.OPris THEN
              ASSIGN dPris2 = IF Etikettlogg.pris2 > 0 THEN Etikettlogg.Pris2 ELSE
                              IF ArtPris.Tilbud = TRUE THEN ArtPris.Pris[1] ELSE 0.
          ELSE 
              ASSIGN dPris2 = 0.
          /* För att inte få in ordinarie pris vid inleverans när varan står på tilbud */
          IF EtikettLogg.Pris = dPris2 THEN
             ASSIGN dPris2 = 0.
          IF ArtBas.OPris THEN
              ASSIGN cPrisKr   = ""
                     cPrisOren = "".
          ELSE
              ASSIGN cPrisKr   = SUBSTITUTE("191100601200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) + TRIM(STRING(iKr,">>>>9")) + CHR(13)
                     cPrisOren = SUBSTITUTE("191100401400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) + TRIM(STRING(iOren,"99")) + CHR(13).
          PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
                          "191100202100020" ArtBas.Bongtekst CHR(13)
                          "191100201800020" (IF ArtBas.StrTypeId > 2 AND AVAIL StrKonv THEN TRIM(StrKonv.Storl) + " " ELSE "") 
                                            /*(IF AVAIL Farg AND Farg.FarBeskr <> "" THEN Farg.FarBeskr ELSE ArtBas.LevFargKod) CHR(13)*/
                                            (IF ArtBas.LevFargKod <> '' THEN ArtBas.LevFargKod ELSE IF AVAIL Farg THEN Farg.FarBeskr ELSE '' ) CHR(13)
    /*                       "121100801400010" "Veil Pris" CHR(13) */
              (IF dPris2 > 0 THEN 
                    "191100201300020" + "(" + TRIM(STRING(dPris2,">>>>9.99")) + ")" + CHR(13) ELSE "")
                  cPrisKr
                  cPrisOren
/*                           SUBSTITUTE("191100601200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) TRIM(STRING(iKr,">>>>9")) CHR(13) */
/*                           SUBSTITUTE("191100401400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) TRIM(STRING(iOren,"99")) CHR(13)    */
                                     "1F2206500450065" StrekKode.Kode CHR(13)
                                     "491100300450050" STRING(ArtBas.Vg) CHR(13)
    /*                       "1F2209000050065" StrekKode.Kode CHR(13) */
              (IF Etikettlogg.individ > 0 THEN
                         "121100200000020    ID:" + TRIM(STRING(Etikettlogg.Individ)) + CHR(13) ELSE "")
                          SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
      END.
  END.
  PUT UNFORMATTED CHR(2) "F".
  OUTPUT CLOSE.
  IF cOrgSysPr <> "" THEN
      ASSIGN SESSION:PRINTER-NAME = cOrgSysPr.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Utskrift-METO_Pris_under) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift-METO_Pris_under Procedure 
PROCEDURE Utskrift-METO_Pris_under :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dePris     AS DECIMAL                        NO-UNDO.
  DEFINE VARIABLE iKr        AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren      AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iRowEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iRow       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cRow       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCol       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cCol       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iColEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iEtikettnr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount     AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iCount2    AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE dPris2     AS DECIMAL  DECIMALS 2  NO-UNDO.
  DEFINE VARIABLE wOK        AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFormat AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cColKronor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColOren   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cOrgSysPr  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cInfoEtiTxt AS CHARACTER EXTENT 4 NO-UNDO.
  DEFINE VARIABLE cPrisKr   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrisOren AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cValdPrinter AS CHARACTER  NO-UNDO.
  ASSIGN cColKronor = "200,200,200,185,160"
         cColOren   = "240,275,310,330,340".
  /* används för att kunna veta startetikett: Ant = startetikett */
  FOR EACH EtikettLogg WHERE EtikettLogg.SeqNr = 0 AND
                             EtikettLogg.Storl = "STARTETIKETT":
      DELETE EtikettLogg.
  END.
  IF SESSION:PARAMETER <> "" THEN DO iCount = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
      IF ENTRY(iCount,SESSION:PARAMETER) BEGINS "EPRINTER" AND 
         NUM-ENTRIES(ENTRY(iCount,SESSION:PARAMETER),"=") = 2 THEN DO:
         ASSIGN cWinPrinterName = ENTRY(2,ENTRY(iCount,SESSION:PARAMETER),"=").
         LEAVE.
      END.
  END.
   IF NUM-ENTRIES(cWinPrinterName,"&") = 2 THEN DO:
      RUN d-VelgGenerellCombo.w ("Velg etikettprinter",REPLACE(REPLACE(cWinPrinterName,"|",","),"&",","),INPUT-OUTPUT cValdPrinter).
      IF cValdPrinter = "" THEN
          RETURN.
      ASSIGN cWinPrinterName = cValdPrinter.
  END.
  IF cWinPrinterName = "" THEN DO:
      SYSTEM-DIALOG PRINTER-SETUP UPDATE wOK.
      IF NOT wOK THEN
          RETURN NO-APPLY.
  END.
  ELSE DO:
      ASSIGN cOrgSysPr = SESSION:PRINTER-NAME.
  END.
  /* hämta printernamn om cWinPrinterName = "" */
  IF cWinPrinterName = "" THEN DO:
      IF lTermKlient = TRUE THEN
          OUTPUT TO VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
      ELSE
          OUTPUT TO PRINTER VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850".
  END.
  ELSE DO:
      IF lTermKlient = TRUE THEN DO:
          OUTPUT TO VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      END.
      ELSE DO:
          ASSIGN SESSION:PRINTER-NAME = cWinPrinterName.
          OUTPUT TO PRINTER VALUE(cWinPrinterName) CONVERT TARGET "IBM850".
      END.
  END.
  ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "M0600" + CHR(2) + "O0000". /* m=metric */
  PUT CONTROL cFormat.
  cFormat = CHR(2) + "LW1200" + CHR(13). /* CR.*/. 
/*   cFormat = CHR(2) + "KcLW400" + CHR(13). /* CR.*/. */
  PUT CONTROL cFormat.

  FOR EACH EtikettLogg WHERE EtikettLogg.Storl <> "" BY Etikettlogg.seqnr:
      IF NOT EtikettLogg.Storl = "INFO" THEN DO:
          FIND Strekkode WHERE Strekkode.kode = EtikettLogg.Storl NO-LOCK NO-ERROR.
          FIND ArtBas OF StrekKode NO-LOCK.
          FIND Farg OF Artbas NO-LOCK NO-ERROR.

          FIND ArtPris OF Artbas NO-LOCK WHERE 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
            FIND ArtPris OF Artbas NO-LOCK WHERE 
              ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.

          FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
          ASSIGN iKr   = TRUNC(EtikettLogg.Pris,0)
                 iOren = (EtikettLogg.Pris - TRUNC(EtikettLogg.Pris,0)) * 100.
      END.
      IF EtikettLogg.Storl = "INFO" THEN DO:
          ASSIGN cInfoEtiTxt = "".
          DO iCount = 1 TO NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)):
              IF iCount = 5 THEN
                  LEAVE.
              ASSIGN cInfoEtiTxt[iCount] = ENTRY(iCount,Etikettlogg.Bongtekst,CHR(1)).
          END.
          PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "R0000" CHR(13) "D11" CHR(13)
                             "121100802100020" cInfoEtiTxt[1] CHR(13)
                             "121100801500020" cInfoEtiTxt[2] CHR(13)
                             "121100801000020" cInfoEtiTxt[3] CHR(13)
                             "121100800150020" cInfoEtiTxt[4] CHR(13)
                                                          "E" CHR(13).
      END.
      ELSE IF StrekKode.KodeType = 0 THEN
      PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
                         "191100202000020" ArtBas.Bongtekst CHR(13)
                         "1a0000000650080" TRIM(StrekKode.Kode) CHR(13)
                         "191100300150100" "PLU: " TRIM(StrekKode.Kode) CHR(13)
                      SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
      ELSE DO:
          /*  NYTT Vi har ett Etikettlogg.pris2 som visar ordinarie pris */
          IF NOT ArtBas.OPris THEN
              ASSIGN dPris2 = IF Etikettlogg.pris2 > 0 THEN Etikettlogg.Pris2 ELSE
                              IF ArtPris.Tilbud = TRUE THEN ArtPris.Pris[1] ELSE 0.
          ELSE 
              ASSIGN dPris2 = 0.
          /* För att inte få in ordinarie pris vid inleverans när varan står på tilbud */
          IF EtikettLogg.Pris = dPris2 THEN
             ASSIGN dPris2 = 0.
          IF ArtBas.OPris THEN
              ASSIGN cPrisKr   = ""
                     cPrisOren = "".
          ELSE
              ASSIGN cPrisKr   = SUBSTITUTE("191100600200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) + TRIM(STRING(iKr,">>>>9")) + CHR(13)
                     cPrisOren = SUBSTITUTE("191100400400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) + TRIM(STRING(iOren,"99")) + CHR(13).
         PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
             "191100202100020" ArtBas.Bongtekst CHR(13)
             "191100201800020" (IF ArtBas.StrTypeId > 2 AND AVAIL StrKonv THEN TRIM(StrKonv.Storl) + " " ELSE "") 
                         /*(IF AVAIL Farg AND Farg.FarBeskr <> "" THEN Farg.FarBeskr ELSE ArtBas.LevFargKod) CHR(13)*/
                         (IF ArtBas.LevFargKod <> '' THEN ArtBas.LevFargKod ELSE IF AVAIL Farg THEN Farg.FarBeskr ELSE '' ) CHR(13)
             "1F2206500900065" TRIM(StrekKode.Kode) CHR(13)
             "491100300900050" STRING(ArtBas.Vg) CHR(13)
              /*  NYTT  */
          (IF dPris2 > 0 THEN 
               "191100200300020" + "(" + TRIM(STRING(dPris2,">>>>9.99")) + ")" + CHR(13) ELSE "")
                 cPrisKr  
                 cPrisOren
/*                           SUBSTITUTE("191100600200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) TRIM(STRING(iKr,">>>>9")) CHR(13) */
/*                           SUBSTITUTE("191100400400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) TRIM(STRING(iOren,"99")) CHR(13)    */
                 (IF Etikettlogg.individ > 0 THEN
                            "121100200000020    ID:" + TRIM(STRING(Etikettlogg.Individ)) + CHR(13) ELSE "")
                          SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
      END.
  END.
  PUT UNFORMATTED CHR(2) "F".
  OUTPUT CLOSE.
  IF cOrgSysPr <> "" THEN
      ASSIGN SESSION:PRINTER-NAME = cOrgSysPr.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Utskrift-METO_Pris_underx) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift-METO_Pris_underx Procedure 
PROCEDURE Utskrift-METO_Pris_underx :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dePris     AS DECIMAL                        NO-UNDO.
  DEFINE VARIABLE iKr        AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren      AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iRowEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iRow       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cRow       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCol       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cCol       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iColEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iEtikettnr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount     AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iCount2    AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE dPris2     AS DECIMAL DECIMALS 2   NO-UNDO.
  DEFINE VARIABLE wOK        AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFormat AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cColKronor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColOren   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cOrgSysPr  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cInfoEtiTxt AS CHARACTER EXTENT 4 NO-UNDO.
  ASSIGN cColKronor = "200,200,200,185,160"
         cColOren   = "240,275,310,330,340".
  /* används för att kunna veta startetikett: Ant = startetikett */
  FOR EACH EtikettLogg WHERE EtikettLogg.SeqNr = 0 AND
                             EtikettLogg.Storl = "STARTETIKETT":
      DELETE EtikettLogg.
  END.
  IF SESSION:PARAMETER <> "" THEN DO iCount = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
      IF ENTRY(iCount,SESSION:PARAMETER) BEGINS "EPRINTER" AND 
         NUM-ENTRIES(ENTRY(iCount,SESSION:PARAMETER),"=") = 2 THEN DO:
         ASSIGN cWinPrinterName = ENTRY(2,ENTRY(iCount,SESSION:PARAMETER),"=").
         LEAVE.
      END.
  END.
  IF cWinPrinterName = "" THEN DO:
      SYSTEM-DIALOG PRINTER-SETUP UPDATE wOK.
      IF NOT wOK THEN
          RETURN NO-APPLY.
  END.
  ELSE DO:
      ASSIGN cOrgSysPr = SESSION:PRINTER-NAME.
  END.

  /* hämta printernamn om cWinPrinterName = "" */
  IF cWinPrinterName = "" THEN
/*       OUTPUT TO PRINTER VALUE(SESSION:PRINTER-NAME) CONVERT TARGET "IBM850". */
      OUTPUT TO PRINTER CONVERT TARGET "IBM850".
  ELSE DO:
      ASSIGN SESSION:PRINTER-NAME = cWinPrinterName.
      OUTPUT TO PRINTER CONVERT TARGET "IBM850".
/*       OUTPUT TO PRINTER VALUE(cWinPrinterName) CONVERT TARGET "IBM850". */
  END.
/*   ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "M0400" + CHR(2) + "O0000". /* m=metric */ */
  ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "M0600" + CHR(2) + "O0000". /* m=metric */
  PUT CONTROL cFormat.
  cFormat = CHR(2) + "LW400" + CHR(13). /* CR.*/. 
  PUT CONTROL cFormat.

  FOR EACH EtikettLogg WHERE EtikettLogg.Storl <> "":
      IF NOT EtikettLogg.Storl = "INFO" THEN DO:
          FIND Strekkode WHERE Strekkode.kode = EtikettLogg.Storl NO-LOCK NO-ERROR.
          FIND ArtBas OF StrekKode NO-LOCK NO-ERROR.
          FIND Farg OF Artbas NO-LOCK NO-ERROR.

          FIND ArtPris OF Artbas NO-LOCK WHERE 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
            FIND ArtPris OF Artbas NO-LOCK WHERE 
              ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.

          FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
          ASSIGN iKr   = TRUNC(EtikettLogg.Pris,0)
                 iOren = (EtikettLogg.Pris - TRUNC(EtikettLogg.Pris,0)) * 100.
      END.
      IF EtikettLogg.Storl = "INFO" THEN DO:
          ASSIGN cInfoEtiTxt = "".
          DO iCount = 1 TO NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)):
              IF iCount = 5 THEN
                  LEAVE.
              ASSIGN cInfoEtiTxt[iCount] = ENTRY(iCount,Etikettlogg.Bongtekst,CHR(1)).
          END.
          PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
                             "121100802100020" cInfoEtiTxt[1] CHR(13)
                             "121100801500020" cInfoEtiTxt[2] CHR(13)
                             "121100801000020" cInfoEtiTxt[3] CHR(13)
                             "121100800150020" cInfoEtiTxt[4] CHR(13)
                                                          "E" CHR(13).
      END.
      ELSE IF StrekKode.KodeType = 0 THEN
      PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
                         "191100202000020" ArtBas.Bongtekst CHR(13)
                         "1a0000000650080" TRIM(StrekKode.Kode) CHR(13)
                         "191100300150100" "PLU: " TRIM(StrekKode.Kode) CHR(13)
                      SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
      ELSE DO:
          /*  NYTT Vi har ett Etikettlogg.pris2 som visar ordinarie pris */
          ASSIGN dPris2 = IF Etikettlogg.pris2 > 0 THEN Etikettlogg.Pris2 ELSE
                          IF ArtPris.Tilbud = TRUE THEN ArtPris.Pris[1] ELSE 0.
/*
          PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13) "O0000" CHR(13)
                          "191100202100020" ArtBas.Bongtekst CHR(13)
                          "191100201800020" (IF ArtBas.StrTypeId > 2 AND AVAIL StrKonv THEN StrKonv.Storl + " " ELSE "") 
                                            (IF AVAIL Farg THEN Farg.FarBeskr ELSE "") CHR(13)
    /*                       "121100801400010" "Veil Pris" CHR(13) */
                                     "1F2209000700065" StrekKode.Kode CHR(13)
                                     "491100300700050" STRING(ArtBas.Vg) CHR(13)
    /*                       "1F2209000050065" StrekKode.Kode CHR(13) */
              (IF dPris2 > 0 THEN 
                    "191100201300020" + "(" + STRING(dPris2,">>>>9.99") + ")" + CHR(13) ELSE "")
                          SUBSTITUTE("191100600050&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) TRIM(STRING(iKr,">>>>9")) CHR(13)
                          SUBSTITUTE("191100400250&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) TRIM(STRING(iOren,"99")) CHR(13)
                          SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
                          */
      PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
                      "121100002200020" ArtBas.Bongtekst CHR(13)
                      "121100801900020" (IF ArtBas.StrTypeId > 2 AND AVAIL StrKonv THEN StrKonv.Storl + " " ELSE "") 
                                        /*(IF AVAIL Farg THEN Farg.FarBeskr ELSE "") CHR(13)*/
                                        (IF ArtBas.LevFargKod <> '' THEN ArtBas.LevFargKod ELSE IF AVAIL Farg THEN Farg.FarBeskr ELSE '' ) CHR(13)
/*                       "1F2209000050065" StrekKode.Kode CHR(13)                                                             */
/*                       "121100801400010" "Veil Pris" CHR(13)                                                                */
/*                       SUBSTITUTE("191100601200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) TRIM(STRING(iKr,">>>>9")) CHR(13) */
/*                       SUBSTITUTE("191100401400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) TRIM(STRING(iOren,"99")) CHR(13)    */
                      "1F2208000800065" StrekKode.Kode CHR(13)
/*                       "121100800050020" "Veil Pris" CHR(13) */
/*              (IF EtikettLogg.Pris <> ArtPris.Pris[1] THEN */
                 (IF ArtPris.Tilbud = TRUE THEN 
                  "191100200100020" + "(" + STRING(ArtPris.Pris[1],">>>9.99") + ")" + CHR(13) ELSE "")
                      SUBSTITUTE("121100400000&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) TRIM(STRING(iKr,">>>>9")) CHR(13)
                      SUBSTITUTE("191100200200&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) TRIM(STRING(iOren,"99")) CHR(13)
                      SUBSTITUTE("Q&1",STRING(EtikettLogg.Ant,"9999")) CHR(13) "E" CHR(13).
      END.
  END.
  PUT UNFORMATTED CHR(2) "F".
  OUTPUT CLOSE.
  IF cOrgSysPr <> "" THEN
      ASSIGN SESSION:PRINTER-NAME = cOrgSysPr.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Utskrift-MONRK-1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift-MONRK-1 Procedure 
PROCEDURE Utskrift-MONRK-1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR svantxt AS CHAR NO-UNDO.


  /*Ut med alle etikettene i filen. */
  FOR EACH EtikettLogg
     BREAK BY EtikettLogg.butik
           BY EtikettLogg.vg
           BY EtikettLogg.lopnr
           BY EtikettLogg.Storl:
           
        /* Henter artikkelinformasjonen. */
        FIND ArtBas NO-LOCK WHERE
          ArtBas.Vg = EtikettLogg.Vg AND
          ArtBas.LopNr = EtikettLogg.LopNr NO-ERROR.
        IF AVAILABLE ArtBas THEN
          /* NB NB Endret format fra gammel etikett */
          /* NB NB NB Ikke rör gammal layout!! G.J. Vet om att format är fel...*/
          svantxt = STRING(ArtBas.farg,"99") +
                     string(artbas.matkod,"9") +
                    string(artbas.klack,"9").
        ELSE
          svantxt = "".   
           
        /* Konverterer st|rrelsen.                   */
        /* Nb: Nullstiller alfanumeriske st|rrelser. */
        tstorlek = "".
        DO i = 1 TO LENGTH(EtikettLogg.Storl):

            IF SUBSTRING(EtikettLogg.Storl,i,1) = " " THEN
              TStorlek = TStorlek + "0". /* Space */

            ELSE IF SUBSTRING(EtikettLogg.Storl,i,1) = "." THEN
              NEXT. /* Punktum strippes bort. */

            ELSE IF (SUBSTRING(EtikettLogg.Storl,i,1) < "0" OR
                     SUBSTRING(EtikettLogg.Storl,i,1) > "9") THEN
              TStorlek = TStorlek + "0". /* Alfanumeriske st|rrelser. */

            ELSE
              TStorlek = TStorlek + substring(EtikettLogg.Storl,i,1). /* Tar vare p} verdien */
        END.

        /*
        " 3.5"  "035"
        " 3"    "03"
        "35"    "35"
        "35.5"  "355"
        */

        ASSIGN /* Trimmer bort ledende space og gjør lengden = 4. */
          TStorlek = TRIM(TStorlek)
          TStorlek = (IF LENGTH(TStorlek) = 2
                        THEN TStorlek + "0"
                        ELSE TStorlek)
          TStorlek = "0" + TStorlek.

        /* Initierer telleverk */
        IF SEARCH("teller") <> ? THEN
        DO:
            INPUT from teller no-echo.
            SET teller.
            INPUT close.
            IF Teller > 9999 THEN
            DO:
                ASSIGN
                    Teller = 1.
                OUTPUT TO Teller NO-ECHO.
                EXPORT Teller.
                OUTPUT CLOSE.
            END.
            IF (teller = 0 OR teller > 9999) THEN teller = 1.
        END.
        ELSE teller = 1.

        /* Tillordner verdier til variablene */
        blinje[1]  = "B,1,N," + string(EtikettLogg.Ant) + "|" + chr(10).
        blinje[2]  = "1," + """"
                          + string((EtikettLogg.pris),"zzz9")
                          + ".00"
                          + """"
                          + "|" + chr(10).
        blinje[3]  = "2," + """"
                          + string((EtikettLogg.pris),"zzz9")
                          + ".00"
                          + """"
                          + "|" + chr(10).
        blinje[4]  = "3," + """"
                          + string(EtikettLogg.vg)
                          + "-"
                          + svantxt
                          + "-"
                          + string(EtikettLogg.lopnr)
                          + """"
                          + "|" + chr(10).
        blinje[5]  = "4," + """"
                          + EtikettLogg.storl
                          + """"
                          + "|" + chr(10).
        blinje[6]  = "5," + """"
                          + string(EtikettLogg.vg)
                          + "-"
                          + svantxt
                          + "-"
                          + string(EtikettLogg.lopnr)
                          + """"
                          + "|" + chr(10).
        blinje[7]  = "6," + """"
                          + EtikettLogg.storl
                          + """"
                          + "|" + chr(10).
        blinje[8]  = "7," + """"
                          + string(EtikettLogg.vg,"999")
                          + string(EtikettLogg.lopnr,"9999")
                          + "0"
                          + tstorlek
                          + """"
                          + "|" + chr(10).
        blinje[9]  = "8," + """"
                          + string(EtikettLogg.vg,"999")
                          + string(EtikettLogg.lopnr,"9999")
                          + "0"
                          + tstorlek
                          + """"
                          + "|" + chr(10).
        blinje[10]  = "9," + """"
                          + string(EtikettLogg.vg,"999")
                          + string(EtikettLogg.lopnr,"9999")
                          + "0"
                          + tstorlek
                          + """"
                          + "|" + chr(10).
        blinje[11]  = "10," + """"
                          + EtikettLogg.storl
                          + """"
                          + "|" + chr(10).
        blinje[12]  = "11," + """"
                          + string(EtikettLogg.lopnr)
                          + """"
                          + "|" + chr(10).
        blinje[13]  = "12," + """"
                          + svantxt
                          + """"
                          + "|" + chr(10).
        blinje[14]  = "13," + """"
                          + string(EtikettLogg.vg)
                          + """"
                          + "|" + chr(10).
                          

        /* Ut med dr.... */
        DO:
            /* wBatch informasjon. */
            wBatch = "~{".
            DO y = 1 TO 14:
                wBatch = wBatch + blinje[y].
            END.
            wBatch = wBatch + "~}".            

            /* ]pner stream */
            OUTPUT to value(wEtikett_Fil) no-echo append.

            /* Skriver etikett. */
            PUT CONTROL wBatch.

            /* Lukker stream */
            OUTPUT close.

        END.

        /* Resetter teller */
        DO:
            teller = teller + EtikettLogg.Ant.
            OUTPUT to teller no-echo.
            EXPORT teller.
            OUTPUT close.
        END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Utskrift-MONRK-2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift-MONRK-2 Procedure 
PROCEDURE Utskrift-MONRK-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR rolans  AS CHAR NO-UNDO.
  DEF VAR trolans AS CHAR NO-UNDO.
  DEF VAR i       AS INT  NO-UNDO.
  DEF VAR j       AS INT  NO-UNDO.
  DEF VAR tod     AS CHAR NO-UNDO.
  DEF VAR siffra  AS INT  NO-UNDO.

  /*Ut med alle etikettene i filen. */
  FOR EACH EtikettLogg
     BREAK BY EtikettLogg.butik
           BY EtikettLogg.vg
           BY EtikettLogg.lopnr
           BY EtikettLogg.Storl:
           
        /* Henter artikkelinformasjonen. */
        FIND ArtBas NO-LOCK WHERE
          ArtBas.Vg = EtikettLogg.Vg AND
          ArtBas.LopNr = EtikettLogg.LopNr NO-ERROR.
        IF AVAILABLE ARtBas THEN
          DO:          
            /* Special Rolans */
            trolans = "".
            rolans  = "".
            FIND Butiker NO-LOCK WHERE
              Butiker.Butik = EtikettLogg.Butik NO-ERROR.
            IF AVAILABLE Butiker THEN
              FIND ArtPris NO-LOCK WHERE
                ArtPris.ArtikkelNr = ArtBAs.ArtikkelNr AND
                ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
            ELSE    
            /* för direktutskrift används ej butiknr, därför första artpris */
              FIND FIRST ArtPris NO-LOCK WHERE
                ArtPris.ArtikkelNr = ArtBAs.ArtikkelNr NO-ERROR.

            IF AVAILABLE ArtPris THEN
              DO:
              /* inkjopspris kodas om enl. pathfindermodell */
                trolans = SUBSTRING(STRING(ArtPris.Innkjopspris[IF ArtPris.Tilbud THEN 2 ELSE 1],">>>>9.99"),1,5).
                i = LENGTH(trolans).
                j = 1.
                DO WHILE i >= j :
                  IF SUBSTRING(trolans,j,1) <> " " THEN
                    DO:
                      siffra = INTEGER(SUBSTRING(trolans,j,1)).
                      IF siffra = 0 THEN siffra = 10.
                      rolans = rolans +
                      string(SUBSTRING("PATHFINDER",siffra,1)).
                    END.
                    j = j + 1.
                  END.
              END.

               tod = STRING(TODAY).
               /* Lägger på Datum mmyy i klartext obs, datumformat dmy*/
               rolans = STRING(SUBSTRING(tod,4,2)) +
                       string(SUBSTRING(tod,1,2)) +
                      rolans.
           END.


        /* Konverterer st|rrelsen.                   */
        /* Nb: Nullstiller alfanumeriske st|rrelser. */
        tstorlek = "".
        DO i = 1 TO LENGTH(EtikettLogg.Storl):

            IF SUBSTRING(EtikettLogg.Storl,i,1) = " " THEN
              TStorlek = TStorlek + "0". /* Space */

            ELSE IF SUBSTRING(EtikettLogg.Storl,i,1) = "." THEN
              NEXT. /* Punktum strippes bort. */

            ELSE IF (SUBSTRING(EtikettLogg.Storl,i,1) < "0" OR
                     SUBSTRING(EtikettLogg.Storl,i,1) > "9") THEN
              TStorlek = TStorlek + "0". /* Alfanumeriske st|rrelser. */

            ELSE
              TStorlek = TStorlek + substring(EtikettLogg.Storl,i,1). /* Tar vare p} verdien */
        END.

        /*
        " 3.5"  "035"
        " 3"    "03"
        "35"    "35"
        "35.5"  "355"
        */

        ASSIGN /* Trimmer bort ledende space og gjør lengden = 4. */
          TStorlek = TRIM(TStorlek)
          TStorlek = (IF LENGTH(TStorlek) = 2
                        THEN TStorlek + "0"
                        ELSE TStorlek)
          TStorlek = "0" + TStorlek.

        /* Initierer telleverk */
        IF SEARCH("teller") <> ? THEN
        DO:
            INPUT from teller no-echo.
            SET teller.
            INPUT close.
            IF (teller = 0 OR teller > 9999) THEN teller = 1.
        END.
        ELSE teller = 1.

        /* Tillordner verdier til variablene */
        blinje[1]  = "B,1,N," + string(EtikettLogg.Ant) + "|" + chr(10).
        blinje[2]  = "1," + """"
                          + rolans
                          + """"
                          + "|" + chr(10).
        blinje[3]  = "2," + """"
                          + string(EtikettLogg.pris)
                          + ".00"
                          + """"
                          + "|" + chr(10).
        blinje[4]  = "3," + """"
                          + string(EtikettLogg.Vg,"999")
                          + string(EtikettLogg.LopNr,"9999")
                          + "0"
                          + tstorlek
                          + """"
                          + "|" + chr(10).
        blinje[5]  = "4," + """"
                          + string(EtikettLogg.Vg)
                          + "/"
                          + string(EtikettLogg.LopNr)
                          + " "
                          + EtikettLogg.Storl
                          + """"
                          + "|" + chr(10).

        /* Ut med dr.... */
        DO:
            /* wBatch informasjon. */
            wBatch = "~{".
            DO y = 1 TO 5:
                wBatch = wBatch + blinje[y].
            END.
            wBatch = wBatch + "~}".            

            /* ]pner stream */
            OUTPUT to value(wEtikett_Fil) no-echo append.

            /* Skriver etikett. */
            PUT CONTROL wBatch.

            /* Lukker stream */
            OUTPUT close.

        END.

        /* Resetter teller */
        DO:
            teller = teller + EtikettLogg.Ant.
            OUTPUT to teller no-echo.
            EXPORT teller.
            OUTPUT close.
        END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Utskrift-SKO-1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift-SKO-1 Procedure 
PROCEDURE Utskrift-SKO-1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dePris     AS DECIMAL                        NO-UNDO.
  DEFINE VARIABLE iKr        AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren      AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iKr2       AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren2     AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iRowEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iRow       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cRow       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCol       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cCol       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iColEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iEtikettnr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount     AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iCount2    AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE dPris2     AS DECIMAL   DECIMALS 2 NO-UNDO.
  DEFINE VARIABLE cInfoEtiTxt AS CHARACTER EXTENT 4 NO-UNDO.
  DEF VAR pcRappFil      AS CHAR NO-UNDO.
/* MESSAGE "here"                             */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.     */
/* FOR EACH ETIKETTLOGG:                      */
/*     MESSAGE storl                          */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/* END.                                       */
  ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "BarCode" + ".xpr".
  /* används för att kunna veta startetikett: Ant = startetikett */
  FIND FIRST EtikettLogg WHERE EtikettLogg.SeqNr = 0 AND 
             EtikettLogg.Storl  = "STARTETIKETT" NO-ERROR.
  ASSIGN cCol = "7,77,147"
         cRow = "15,51,87,123,159,196,232,268"
         iEtikettnr = IF AVAIL EtikettLogg THEN EtikettLogg.Ant ELSE 1
         iRowEntry = IF iEtikettnr < 4  THEN 1 ELSE IF iEtikettnr < 7 THEN 2 ELSE
                     IF iEtikettnr < 10 THEN 3 ELSE IF iEtikettnr < 13 THEN 4 ELSE
                     IF iEtikettnr < 16 THEN 5 ELSE IF iEtikettnr < 19 THEN 6 ELSE
                     IF iEtikettnr < 22 THEN 6 ELSE 8
         iRow      = INT(ENTRY(iRowEntry,cRow))
         iColEntry = IF iEtikettnr MOD 3 = 0 THEN 3 ELSE iEtikettnr MOD 3.
         iCol = INT(ENTRY(iColEntry,cCol)).
  FOR EACH EtikettLogg WHERE EtikettLogg.SeqNr = 0 AND
                             EtikettLogg.Storl = "STARTETIKETT":
      DELETE EtikettLogg.
  END.
/*   output to "BarCode.xpr" PAGED page-size 100. */
  OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE 100.
/* Olika utput mellan Xprint - PDF */
  PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
/*   FILE-INFO:File-NAME = "4GL.bmp". */
/*   Run PrinterAdmin. */
                       
  PUT CONTROL "<TRANSPARENT=true><UNITS=MM><LINECOLOR=BLUE><PREVIEW><FGCOLOR=BLACK>".
  FOR EACH EtikettLogg WHERE EtikettLogg.Storl <> "":
      IF NOT EtikettLogg.Storl = "INFO" THEN DO:
          FIND Strekkode WHERE Strekkode.kode = EtikettLogg.Storl NO-LOCK NO-ERROR.
          FIND ArtBas OF StrekKode NO-LOCK.
          FIND Farg OF Artbas NO-LOCK NO-ERROR.

          FIND ArtPris OF Artbas NO-LOCK WHERE 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
            FIND ArtPris OF Artbas NO-LOCK WHERE 
              ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.

          FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
          ASSIGN iKr    = TRUNC(EtikettLogg.Pris,0)
                 iOren  = (EtikettLogg.Pris - TRUNC(EtikettLogg.Pris,0)) * 100
                 dPris2 = IF Etikettlogg.pris2 > 0 THEN Etikettlogg.Pris2 ELSE
                          IF ArtPris.Tilbud = TRUE THEN ArtPris.Pris[1] ELSE 0
                 dPris2 = IF EtikettLogg.Pris = dPris2 THEN 0 ELSE dPris2
                 iKr2   = IF dPris2 > 0 THEN TRUNC(dPris2,0) ELSE 0
                 iOren2 = IF dPris2 > 0 THEN (dPris2 - TRUNC(dPris2,0)) * 100 ELSE 0.
      END.
      ELSE DO:
/*           NEXT. */
          ASSIGN cInfoEtiTxt = "".
          DO iCount = 1 TO NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)):
              IF iCount = 5 THEN
                  LEAVE.
              ASSIGN cInfoEtiTxt[iCount] = ENTRY(iCount,Etikettlogg.Bongtekst,CHR(1)).
          END.
/*           PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13) */
/*                              "121100802100020" cInfoEtiTxt[1] CHR(13)      */
/*                              "121100801500020" cInfoEtiTxt[2] CHR(13)      */
/*                              "121100801000020" cInfoEtiTxt[3] CHR(13)      */
/*                              "121100800150020" cInfoEtiTxt[4] CHR(13)      */
/*                                                           "E" CHR(13).     */
/*           PUT UNFORMATTED                                                                                 */
/*             "<AT=" STRING(iRow) "," STRING(iCol) "><FTimes New Roman><P12><B>" cInfoEtiTxt[1] "</B>" SKIP */
/*             "<AT=" STRING(iRow + 4) "," STRING(iCol) ">" cInfoEtiTxt[2] SKIP                              */
/*             "<AT=" STRING(iRow + 8) "," STRING(iCol) ">" cInfoEtiTxt[3] SKIP                              */
/*             "<AT=" STRING(iRow + 12) "," STRING(iCol) ">" cInfoEtiTxt[4].                                 */
      END.



            
/*         If cust-num mod 2 = 0 then         */
/*             put control "<FGCOLOR=GREEN>". */
/*         else                               */
  DO iCount = 1 TO EtikettLogg.Ant:
      IF EtikettLogg.Storl = "INFO" THEN DO:
          PUT UNFORMATTED
            "<AT=" STRING(iRow) "," STRING(iCol) "><FTimes New Roman><P12><B>" cInfoEtiTxt[1] "</B>" SKIP
            "<AT=" STRING(iRow + 4) "," STRING(iCol) ">" cInfoEtiTxt[2] SKIP
            "<AT=" STRING(iRow + 8) "," STRING(iCol) ">" cInfoEtiTxt[3] SKIP
            "<AT=" STRING(iRow + 12) "," STRING(iCol) ">" cInfoEtiTxt[4].
      END.
      ELSE IF Strekkode.KodeType = 0 THEN
          PUT UNFORMATTED
            "<AT=" STRING(iRow) "," STRING(iCol) "><FTimes New Roman><P10><B>" ArtBas.Bongtekst "</B>" SKIP
            "<AT=" STRING(iRow + 8) "," STRING(iCol) "><#1><AT=+10,+35>"
              "<BARCODE#1,TYPE=39,CHECKSUM=none,VALUE=" StrekKode.Kode ">"
            "<R+0.5><FTimes New Roman><P12><AT=," STRING(iCol + 2) ">" "PLU: " + StrekKode.Kode
                .
      ELSE
  PUT UNFORMATTED
    "<AT=" STRING(iRow) "," STRING(iCol) "><FTimes New Roman><P10><B>" ArtBas.Bongtekst "</B>" SKIP
    "<AT=," STRING(iCol) "><P8>" Artbas.Vg "/" Artbas.lopnr " " (IF AVAIL StrKonv THEN StrKonv.Storl + " " ELSE "") 
      /*(IF AVAIL Farg AND Farg.FarBeskr <> "" THEN Farg.FarBeskr ELSE ArtBas.LevFargKod) SKIP*/
      (IF ArtBas.LevFargKod <> '' THEN ArtBas.LevFargKod ELSE IF AVAIL Farg THEN Farg.FarBeskr ELSE '' ) SKIP
    "<AT=" STRING(iRow + 8) "," STRING(iCol) "><#1><AT=+10,+35>"
      "<BARCODE#1,TYPE=" (IF LENGTH(StrekKode.Kode) = 12 THEN "UPC_A" ELSE "EAN13") ",CHECKSUM=none,VALUE=" StrekKode.Kode ">"
/*     "<BARCODE#1,TYPE=EAN13,CHECKSUM=none,SHOW=CODE,VALUE=" StrekKode.Kode ">" */
    "<FArial Narrow><P28><AT=" STRING(iRow + 8) ",>" STRING(iKr) "<P18><AT=" STRING(iRow + 8) ",>" IF LENGTH(STRING(iOren)) = 1 THEN "0" + STRING(iOren) ELSE STRING(iOren) SKIP
    "<R+1.4><FTimes New Roman><P12><AT=," STRING(iCol + 2) ">" StrekKode.Kode
                        /* ord.pris */ (IF iKr2 > 0 THEN "<FArial Narrow><P14><C+2>" + STRING(iKr2) + "<P12>" + STRING(iOren2,"99") ELSE "").
        .
    IF iRowEntry = 8 AND iColEntry = 3 THEN DO:
        PAGE.
        ASSIGN iRowEntry = 1
               iColEntry = 1.
    END.
    ELSE
        ASSIGN iRowEntry = IF iColEntry < 3 THEN iRowEntry ELSE iRowEntry + 1
               iColEntry = IF iColEntry = 3 THEN 1 ELSE iColEntry + 1.
    ASSIGN iRow = INT(ENTRY(iRowEntry,cRow))
           iCol = INT(ENTRY(iColEntry,cCol)).
/*     IF iRowEntry = 8 AND iColEntry = 3 THEN                               */
/*         PAGE.                                                             */
/*     ASSIGN iRowEntry = IF iColEntry < 3 THEN iRowEntry ELSE iRowEntry + 1 */
/*            iRowEntry = IF iRowEntry = 9 THEN 1 ELSE iRowEntry             */
/*            iRow      = INT(ENTRY(iRowEntry,cRow))                         */
/*            iColEntry = IF iColEntry = 3 THEN 1 ELSE iColEntry + 1         */
/*            iCol      = INT(ENTRY(iColEntry,cCol)).                        */
  END.
  END.
  OUTPUT TO TERMINAL.
/*   FILE-INFO:File-NAME = "BarCode.xpr".      */
/*   Run printFile( FILE-INFO:FULL-PATHNAME ). */
  /* Klargjør rapportfilnavnet */
  ASSIGN FILE-INFO:FILE-NAME = pcRappFil.
    
  /* Sender filen til visning og utskrift. */
 RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). 
   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
       OS-DELETE VALUE(FILE-INFO:FILE-NAME).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Utskrift-Sport1-1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift-Sport1-1 Procedure 
PROCEDURE Utskrift-Sport1-1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dePris     AS DECIMAL                        NO-UNDO.
  DEFINE VARIABLE iKr        AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren      AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iKr2       AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren2     AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iRowEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iRow       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cRow       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCol       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cCol       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iColEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iEtikettnr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount     AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iCount2    AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE dPris2     AS DECIMAL   DECIMALS 2 NO-UNDO.
  DEFINE VARIABLE cInfoEtiTxt AS CHARACTER EXTENT 4 NO-UNDO.
  DEF VAR pcRappFil      AS CHAR NO-UNDO.
/* MESSAGE "here"                             */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.     */
/* FOR EACH ETIKETTLOGG:                      */
/*     MESSAGE storl                          */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/* END.                                       */
  ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "BarCode" + ".xpr".
  /* används för att kunna veta startetikett: Ant = startetikett */
  FIND FIRST EtikettLogg WHERE EtikettLogg.SeqNr = 0 AND 
             EtikettLogg.Storl  = "STARTETIKETT" NO-ERROR.
  ASSIGN cCol = "7,77,147"
         cRow = "15,51,87,123,159,196,232,268"
         iEtikettnr = IF AVAIL EtikettLogg THEN EtikettLogg.Ant ELSE 1
         iRowEntry = IF iEtikettnr < 4  THEN 1 ELSE IF iEtikettnr < 7 THEN 2 ELSE
                     IF iEtikettnr < 10 THEN 3 ELSE IF iEtikettnr < 13 THEN 4 ELSE
                     IF iEtikettnr < 16 THEN 5 ELSE IF iEtikettnr < 19 THEN 6 ELSE
                     IF iEtikettnr < 22 THEN 6 ELSE 8
         iRow      = INT(ENTRY(iRowEntry,cRow))
         iColEntry = IF iEtikettnr MOD 3 = 0 THEN 3 ELSE iEtikettnr MOD 3.
         iCol = INT(ENTRY(iColEntry,cCol)).
  FOR EACH EtikettLogg WHERE EtikettLogg.SeqNr = 0 AND
                             EtikettLogg.Storl = "STARTETIKETT":
      DELETE EtikettLogg.
  END.
/*   output to "BarCode.xpr" PAGED page-size 100. */
  OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE 100.
/* Olika utput mellan Xprint - PDF */
  PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
/*   FILE-INFO:File-NAME = "4GL.bmp". */
/*   Run PrinterAdmin. */
                       
  PUT CONTROL "<TRANSPARENT=true><UNITS=MM><LINECOLOR=BLUE><PREVIEW><FGCOLOR=BLACK>".
  FOR EACH EtikettLogg WHERE EtikettLogg.Storl <> "":
      IF NOT EtikettLogg.Storl = "INFO" THEN DO:
          FIND Strekkode WHERE Strekkode.kode = EtikettLogg.Storl NO-LOCK NO-ERROR.
          FIND ArtBas OF StrekKode NO-LOCK.
          FIND Farg OF Artbas NO-LOCK NO-ERROR.

          FIND ArtPris OF Artbas NO-LOCK WHERE 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
            FIND ArtPris OF Artbas NO-LOCK WHERE 
              ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.

          FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
          ASSIGN iKr    = TRUNC(EtikettLogg.Pris,0)
                 iOren  = (EtikettLogg.Pris - TRUNC(EtikettLogg.Pris,0)) * 100
                 dPris2 = IF Etikettlogg.pris2 > 0 THEN Etikettlogg.Pris2 ELSE
                          IF ArtPris.Tilbud = TRUE THEN ArtPris.Pris[1] ELSE 0
                 dPris2 = IF EtikettLogg.Pris = dPris2 THEN 0 ELSE dPris2
                 iKr2   = IF dPris2 > 0 THEN TRUNC(dPris2,0) ELSE 0
                 iOren2 = IF dPris2 > 0 THEN (dPris2 - TRUNC(dPris2,0)) * 100 ELSE 0.
      END.
      ELSE DO:
/*           NEXT. */
          ASSIGN cInfoEtiTxt = "".
          DO iCount = 1 TO NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)):
              IF iCount = 5 THEN
                  LEAVE.
              ASSIGN cInfoEtiTxt[iCount] = ENTRY(iCount,Etikettlogg.Bongtekst,CHR(1)).
          END.
/*           PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13) */
/*                              "121100802100020" cInfoEtiTxt[1] CHR(13)      */
/*                              "121100801500020" cInfoEtiTxt[2] CHR(13)      */
/*                              "121100801000020" cInfoEtiTxt[3] CHR(13)      */
/*                              "121100800150020" cInfoEtiTxt[4] CHR(13)      */
/*                                                           "E" CHR(13).     */
/*           PUT UNFORMATTED                                                                                 */
/*             "<AT=" STRING(iRow) "," STRING(iCol) "><FTimes New Roman><P12><B>" cInfoEtiTxt[1] "</B>" SKIP */
/*             "<AT=" STRING(iRow + 4) "," STRING(iCol) ">" cInfoEtiTxt[2] SKIP                              */
/*             "<AT=" STRING(iRow + 8) "," STRING(iCol) ">" cInfoEtiTxt[3] SKIP                              */
/*             "<AT=" STRING(iRow + 12) "," STRING(iCol) ">" cInfoEtiTxt[4].                                 */
      END.



            
/*         If cust-num mod 2 = 0 then         */
/*             put control "<FGCOLOR=GREEN>". */
/*         else                               */
  DO iCount = 1 TO EtikettLogg.Ant:
      IF EtikettLogg.Storl = "INFO" THEN DO:
          PUT UNFORMATTED
            "<AT=" STRING(iRow) "," STRING(iCol) "><FTimes New Roman><P12><B>" cInfoEtiTxt[1] "</B>" SKIP
            "<AT=" STRING(iRow + 4) "," STRING(iCol) ">" cInfoEtiTxt[2] SKIP
            "<AT=" STRING(iRow + 8) "," STRING(iCol) ">" cInfoEtiTxt[3] SKIP
            "<AT=" STRING(iRow + 12) "," STRING(iCol) ">" cInfoEtiTxt[4].
      END.
      ELSE IF Strekkode.KodeType = 0 THEN
          PUT UNFORMATTED
            "<AT=" STRING(iRow) "," STRING(iCol) "><FTimes New Roman><P10><B>" ArtBas.Bongtekst "</B>" SKIP
            "<AT=" STRING(iRow + 8) "," STRING(iCol) "><#1><AT=+10,+35>"
              "<BARCODE#1,TYPE=39,CHECKSUM=none,VALUE=" StrekKode.Kode ">"
            "<R+0.5><FTimes New Roman><P12><AT=," STRING(iCol + 2) ">" "PLU: " + StrekKode.Kode
                .
      ELSE
  PUT UNFORMATTED
    "<AT=" STRING(iRow) "," STRING(iCol) "><FTimes New Roman><P10><B>" ArtBas.Bongtekst "</B>" SKIP
    "<AT=," STRING(iCol) "><P8>" (IF AVAIL StrKonv THEN StrKonv.Storl + " " ELSE "") 
      /*(IF AVAIL Farg AND Farg.FarBeskr <> "" THEN Farg.FarBeskr ELSE ArtBas.LevFargKod) SKIP*/
      (IF ArtBas.LevFargKod <> '' THEN ArtBas.LevFargKod ELSE IF AVAIL Farg THEN Farg.FarBeskr ELSE '' ) SKIP 
    "<AT=" STRING(iRow + 8) "," STRING(iCol) "><#1><AT=+10,+35>"
      "<BARCODE#1,TYPE=" (IF LENGTH(StrekKode.Kode) = 12 THEN "UPC_A" ELSE "EAN13") ",CHECKSUM=none,VALUE=" StrekKode.Kode ">"
/*     "<BARCODE#1,TYPE=EAN13,CHECKSUM=none,SHOW=CODE,VALUE=" StrekKode.Kode ">" */
    "<FArial Narrow><P28><AT=" STRING(iRow + 8) ",>" STRING(iKr) "<P18><AT=" STRING(iRow + 8) ",>" IF LENGTH(STRING(iOren)) = 1 THEN "0" + STRING(iOren) ELSE STRING(iOren) SKIP
    "<R+1.4><FTimes New Roman><P12><AT=," STRING(iCol + 2) ">" StrekKode.Kode
                        /* ord.pris */ (IF iKr2 > 0 THEN "<FArial Narrow><P14><C+2>" + STRING(iKr2) + "<P12>" + STRING(iOren2,"99") ELSE "").
        .
    IF iRowEntry = 8 AND iColEntry = 3 THEN DO:
        PAGE.
        ASSIGN iRowEntry = 1
               iColEntry = 1.
    END.
    ELSE
        ASSIGN iRowEntry = IF iColEntry < 3 THEN iRowEntry ELSE iRowEntry + 1
               iColEntry = IF iColEntry = 3 THEN 1 ELSE iColEntry + 1.
    ASSIGN iRow = INT(ENTRY(iRowEntry,cRow))
           iCol = INT(ENTRY(iColEntry,cCol)).
/*     IF iRowEntry = 8 AND iColEntry = 3 THEN                               */
/*         PAGE.                                                             */
/*     ASSIGN iRowEntry = IF iColEntry < 3 THEN iRowEntry ELSE iRowEntry + 1 */
/*            iRowEntry = IF iRowEntry = 9 THEN 1 ELSE iRowEntry             */
/*            iRow      = INT(ENTRY(iRowEntry,cRow))                         */
/*            iColEntry = IF iColEntry = 3 THEN 1 ELSE iColEntry + 1         */
/*            iCol      = INT(ENTRY(iColEntry,cCol)).                        */
  END.
  END.
  OUTPUT TO TERMINAL.
/*   FILE-INFO:File-NAME = "BarCode.xpr".      */
/*   Run printFile( FILE-INFO:FULL-PATHNAME ). */
  /* Klargjør rapportfilnavnet */
  ASSIGN FILE-INFO:FILE-NAME = pcRappFil.
    
  /* Sender filen til visning og utskrift. */
 RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). 
   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
       OS-DELETE VALUE(FILE-INFO:FILE-NAME).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Utskrift_HH_Ark_33) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift_HH_Ark_33 Procedure 
PROCEDURE Utskrift_HH_Ark_33 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dePris     AS DECIMAL                        NO-UNDO.
  DEFINE VARIABLE iKr        AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren      AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iKr2       AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren2     AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iRowEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iRow       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cRow       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCol       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cCol       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iColEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iEtikettnr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount     AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iCount2    AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE dPris2     AS DECIMAL   DECIMALS 2 NO-UNDO.
  DEFINE VARIABLE cKrCol     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cInfoEtiTxt AS CHARACTER EXTENT 4 NO-UNDO.
  DEF VAR pcRappFil      AS CHAR NO-UNDO.
/* MESSAGE "here"                             */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.     */
/* FOR EACH ETIKETTLOGG:                      */
/*     MESSAGE storl                          */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/* END.                                       */
  ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "BarCode" + ".xpr".
  /* används för att kunna veta startetikett: Ant = startetikett */
  FIND FIRST EtikettLogg WHERE EtikettLogg.SeqNr = 0 AND 
             EtikettLogg.Storl  = "STARTETIKETT" NO-ERROR.
  ASSIGN cKrCol = "18,45,72"
         cCol   = "7,77,147"
         cRow   = "15,40,65,90,115,140,165,190,215,240,265"
         iEtikettnr = IF AVAIL EtikettLogg THEN EtikettLogg.Ant ELSE 1
         iRowEntry = IF iEtikettnr < 4  THEN 1 ELSE IF iEtikettnr < 7 THEN 2 ELSE
                     IF iEtikettnr < 10 THEN 3 ELSE IF iEtikettnr < 13 THEN 4 ELSE
                     IF iEtikettnr < 16 THEN 5 ELSE IF iEtikettnr < 19 THEN 6 ELSE
                     IF iEtikettnr < 22 THEN 7 ELSE IF iEtikettnr < 25 THEN 8 ELSE
                     IF iEtikettnr < 28 THEN 9 ELSE IF iEtikettnr < 31 THEN 10 ELSE 11
         iRow      = INT(ENTRY(iRowEntry,cRow))
         iColEntry = IF iEtikettnr MOD 3 = 0 THEN 3 ELSE iEtikettnr MOD 3.
         iCol = INT(ENTRY(iColEntry,cCol)).
  FOR EACH EtikettLogg WHERE EtikettLogg.SeqNr = 0 AND
                             EtikettLogg.Storl = "STARTETIKETT":
      DELETE EtikettLogg.
  END.
/*   output to "BarCode.xpr" PAGED page-size 100. */
  OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE 100.
/* Olika utput mellan Xprint - PDF */
  PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
/*   FILE-INFO:File-NAME = "4GL.bmp". */
/*   Run PrinterAdmin. */
                       
  PUT CONTROL "<TRANSPARENT=true><UNITS=MM><LINECOLOR=BLUE><PREVIEW><FGCOLOR=BLACK>".
  FOR EACH EtikettLogg WHERE EtikettLogg.Storl <> "":
      IF NOT EtikettLogg.Storl = "INFO" THEN DO:
          FIND Strekkode WHERE Strekkode.kode = EtikettLogg.Storl NO-LOCK NO-ERROR.
          FIND ArtBas OF StrekKode NO-LOCK.
          FIND Levbas OF artbas NO-LOCK NO-ERROR.
          FIND Farg OF Artbas NO-LOCK NO-ERROR.

          FIND ArtPris OF Artbas NO-LOCK WHERE 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
            FIND ArtPris OF Artbas NO-LOCK WHERE 
              ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.

          FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
          ASSIGN iKr    = TRUNC(EtikettLogg.Pris,0)
                 iOren  = (EtikettLogg.Pris - TRUNC(EtikettLogg.Pris,0)) * 100
                 dPris2 = IF Etikettlogg.pris2 > 0 THEN Etikettlogg.Pris2 ELSE
                          IF ArtPris.Tilbud = TRUE THEN ArtPris.Pris[1] ELSE 0
                 dPris2 = IF EtikettLogg.Pris = dPris2 THEN 0 ELSE dPris2
                 iKr2   = IF dPris2 > 0 THEN TRUNC(dPris2,0) ELSE 0
                 iOren2 = IF dPris2 > 0 THEN (dPris2 - TRUNC(dPris2,0)) * 100 ELSE 0.
      END.
      ELSE DO:
/*           NEXT. */
          ASSIGN cInfoEtiTxt = "".
          DO iCount = 1 TO NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)):
              IF iCount = 5 THEN
                  LEAVE.
              ASSIGN cInfoEtiTxt[iCount] = ENTRY(iCount,Etikettlogg.Bongtekst,CHR(1)).
          END.
/*           PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13) */
/*                              "121100802100020" cInfoEtiTxt[1] CHR(13)      */
/*                              "121100801500020" cInfoEtiTxt[2] CHR(13)      */
/*                              "121100801000020" cInfoEtiTxt[3] CHR(13)      */
/*                              "121100800150020" cInfoEtiTxt[4] CHR(13)      */
/*                                                           "E" CHR(13).     */
/*           PUT UNFORMATTED                                                                                 */
/*             "<AT=" STRING(iRow) "," STRING(iCol) "><FArial><P12><B>" cInfoEtiTxt[1] "</B>" SKIP */
/*             "<AT=" STRING(iRow + 4) "," STRING(iCol) ">" cInfoEtiTxt[2] SKIP                              */
/*             "<AT=" STRING(iRow + 8) "," STRING(iCol) ">" cInfoEtiTxt[3] SKIP                              */
/*             "<AT=" STRING(iRow + 12) "," STRING(iCol) ">" cInfoEtiTxt[4].                                 */
      END.



            
/*         If cust-num mod 2 = 0 then         */
/*             put control "<FGCOLOR=GREEN>". */
/*         else                               */
  DO iCount = 1 TO EtikettLogg.Ant:
      IF EtikettLogg.Storl = "INFO" THEN DO:
          PUT UNFORMATTED
            "<AT=" STRING(iRow) "," STRING(iCol) "><FArial><P12><B>" cInfoEtiTxt[1] "</B>" SKIP
            "<AT=" STRING(iRow + 4) "," STRING(iCol) ">" cInfoEtiTxt[2] SKIP
            "<AT=" STRING(iRow + 8) "," STRING(iCol) ">" cInfoEtiTxt[3] SKIP
            "<AT=" STRING(iRow + 12) "," STRING(iCol) ">" cInfoEtiTxt[4].
      END.
      ELSE IF Strekkode.KodeType = 0 THEN
          PUT UNFORMATTED
            "<AT=" STRING(iRow) "," STRING(iCol) "><FArial><P10><B>" ArtBas.Bongtekst "</B>" SKIP
            "<AT=" STRING(iRow + 8) "," STRING(iCol) "><#1><AT=+10,+35>"
              "<BARCODE#1,TYPE=39,CHECKSUM=none,VALUE=" StrekKode.Kode ">"
            "<R+0.5><FArial><P12><AT=," STRING(iCol + 2) ">" "PLU: " + StrekKode.Kode
                .
      ELSE
          PUT UNFORMATTED
            "<AT=" STRING(iRow) "," STRING(iCol) "><FArial><P8>" TRIM(ArtBas.Levkod) + " " + IF AVAIL Levbas THEN SUBSTR(Levbas.levnamn,1,15) ELSE ""
            "<AT=" STRING(iRow + 4) "," STRING(iCol) "><FArial><P10><B>" ArtBas.Beskr "</B>" SKIP
/*               (IF AVAIL Farg AND Farg.FarBeskr <> "" THEN Farg.FarBeskr ELSE ArtBas.LevFargKod) SKIP */
            "<AT=" STRING(iRow + 15) "," STRING(iCol) "><#1><AT=+4,+25>"
              "<BARCODE#1,TYPE=" (IF LENGTH(StrekKode.Kode) = 12 THEN "UPC_A" ELSE "EAN13") ",CHECKSUM=none,VALUE=" StrekKode.Kode ">"
                
/*              (IF iKr2 > 0 THEN "<AT=" + STRING(iRow + 15) + ",+25><FArial Narrow><P12>" + STRING(iKr2) + "<P12>" + "," + STRING(iOren2,"99") ELSE "") */


            "<FArial Narrow><P26><AT=" STRING(iRow + 11) "><C" ENTRY(iColEntry,cKrCol)  "><RIGHT=C+7><B><FGCOLOR=RED>" STRING(EtikettLogg.Pris,">>>>9.99") "</B><FGCOLOR=BLACK>" SKIP
/*             "<FArial Narrow><P28><AT=" STRING(iRow + 8) "," ENTRY(iColEntry,cKrCol)  "><RIGHT=C+6><B><FGCOLOR=RED>" STRING(EtikettLogg.Pris,">>>9.99") "</B><FGCOLOR=BLACK>" SKIP */
            "<FArial><P10><AT=" STRING(iRow + 9) "," STRING(iCol) ">" StrekKode.Kode
        /* ord.pris */ 
                (IF iKr2 > 0 THEN "<FArial Narrow><P12><R-.3><C+3>" + STRING(iKr2) + "<P12>" + "," + STRING(iOren2,"99") ELSE "").
    IF iRowEntry = 11 AND iColEntry = 3 THEN DO:
        PAGE.
        ASSIGN iRowEntry = 1
               iColEntry = 1.
    END.
    ELSE
        ASSIGN iRowEntry = IF iColEntry < 3 THEN iRowEntry ELSE iRowEntry + 1
               iColEntry = IF iColEntry = 3 THEN 1 ELSE iColEntry + 1.
    ASSIGN iRow = INT(ENTRY(iRowEntry,cRow))
           iCol = INT(ENTRY(iColEntry,cCol)).
/*     IF iRowEntry = 8 AND iColEntry = 3 THEN                               */
/*         PAGE.                                                             */
/*     ASSIGN iRowEntry = IF iColEntry < 3 THEN iRowEntry ELSE iRowEntry + 1 */
/*            iRowEntry = IF iRowEntry = 9 THEN 1 ELSE iRowEntry             */
/*            iRow      = INT(ENTRY(iRowEntry,cRow))                         */
/*            iColEntry = IF iColEntry = 3 THEN 1 ELSE iColEntry + 1         */
/*            iCol      = INT(ENTRY(iColEntry,cCol)).                        */
  END.
  END.
  OUTPUT TO TERMINAL.
/*   FILE-INFO:File-NAME = "BarCode.xpr".      */
/*   Run printFile( FILE-INFO:FULL-PATHNAME ). */
  /* Klargjør rapportfilnavnet */
  ASSIGN FILE-INFO:FILE-NAME = pcRappFil.
    
  /* Sender filen til visning og utskrift. */
 RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). 
   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
       OS-DELETE VALUE(FILE-INFO:FILE-NAME).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Utskrift_HH_Ark_332) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift_HH_Ark_332 Procedure 
PROCEDURE Utskrift_HH_Ark_332 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dePris     AS DECIMAL                        NO-UNDO.
  DEFINE VARIABLE iKr        AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren      AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iKr2       AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren2     AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iRowEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iRow       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cRow       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCol       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cCol       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iColEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iEtikettnr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount     AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iCount2    AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE dPris2     AS DECIMAL   DECIMALS 2 NO-UNDO.
  DEFINE VARIABLE cKrCol     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cInfoEtiTxt AS CHARACTER EXTENT 4 NO-UNDO.
  DEF VAR pcRappFil      AS CHAR NO-UNDO.
/* MESSAGE "here"                             */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.     */
/* FOR EACH ETIKETTLOGG:                      */
/*     MESSAGE storl                          */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/* END.                                       */
  ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "BarCode" + ".xpr".
  /* används för att kunna veta startetikett: Ant = startetikett */
  FIND FIRST EtikettLogg WHERE EtikettLogg.SeqNr = 0 AND 
             EtikettLogg.Storl  = "STARTETIKETT" NO-ERROR.
  ASSIGN cKrCol = "23,44,65"
         cCol   = "27,81,135"
         cRow   = "11,37,62,87,112,138,163,189,214,240,265"
/*       cRow   = "13,39,64,89,114,140,165,191,216,242,267" */
         iEtikettnr = IF AVAIL EtikettLogg THEN EtikettLogg.Ant ELSE 1
         iRowEntry = IF iEtikettnr < 4  THEN 1 ELSE IF iEtikettnr < 7 THEN 2 ELSE
                     IF iEtikettnr < 10 THEN 3 ELSE IF iEtikettnr < 13 THEN 4 ELSE
                     IF iEtikettnr < 16 THEN 5 ELSE IF iEtikettnr < 19 THEN 6 ELSE
                     IF iEtikettnr < 22 THEN 7 ELSE IF iEtikettnr < 25 THEN 8 ELSE
                     IF iEtikettnr < 28 THEN 9 ELSE IF iEtikettnr < 31 THEN 10 ELSE 11
         iRow      = INT(ENTRY(iRowEntry,cRow))
         iColEntry = IF iEtikettnr MOD 3 = 0 THEN 3 ELSE iEtikettnr MOD 3.
         iCol = INT(ENTRY(iColEntry,cCol)).
  FOR EACH EtikettLogg WHERE EtikettLogg.SeqNr = 0 AND
                             EtikettLogg.Storl = "STARTETIKETT":
      DELETE EtikettLogg.
  END.
/*   output to "BarCode.xpr" PAGED page-size 100. */
  OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE 100.
/* Olika utput mellan Xprint - PDF */
  PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
/*   FILE-INFO:File-NAME = "4GL.bmp". */
/*   Run PrinterAdmin. */
                       
  PUT CONTROL "<TRANSPARENT=true><UNITS=MM><LINECOLOR=BLUE><PREVIEW><FGCOLOR=BLACK>".
  FOR EACH EtikettLogg WHERE EtikettLogg.Storl <> "":
      IF NOT EtikettLogg.Storl = "INFO" THEN DO:
          FIND Strekkode WHERE Strekkode.kode = EtikettLogg.Storl NO-LOCK NO-ERROR.
          FIND ArtBas OF StrekKode NO-LOCK.
          FIND Levbas OF artbas NO-LOCK NO-ERROR.
          FIND Farg OF Artbas NO-LOCK NO-ERROR.

          FIND ArtPris OF Artbas NO-LOCK WHERE 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
            FIND ArtPris OF Artbas NO-LOCK WHERE 
              ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.

          FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
          ASSIGN iKr    = TRUNC(EtikettLogg.Pris,0)
                 iOren  = (EtikettLogg.Pris - TRUNC(EtikettLogg.Pris,0)) * 100
                 dPris2 = IF Etikettlogg.pris2 > 0 THEN Etikettlogg.Pris2 ELSE
                          IF ArtPris.Tilbud = TRUE THEN ArtPris.Pris[1] ELSE 0
                 dPris2 = IF EtikettLogg.Pris = dPris2 THEN 0 ELSE dPris2
                 iKr2   = IF dPris2 > 0 THEN TRUNC(dPris2,0) ELSE 0
                 iOren2 = IF dPris2 > 0 THEN (dPris2 - TRUNC(dPris2,0)) * 100 ELSE 0.
      END.
      ELSE DO:
/*           NEXT. */
          ASSIGN cInfoEtiTxt = "".
          DO iCount = 1 TO NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)):
              IF iCount = 5 THEN
                  LEAVE.
              ASSIGN cInfoEtiTxt[iCount] = ENTRY(iCount,Etikettlogg.Bongtekst,CHR(1)).
          END.
/*           PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13) */
/*                              "121100802100020" cInfoEtiTxt[1] CHR(13)      */
/*                              "121100801500020" cInfoEtiTxt[2] CHR(13)      */
/*                              "121100801000020" cInfoEtiTxt[3] CHR(13)      */
/*                              "121100800150020" cInfoEtiTxt[4] CHR(13)      */
/*                                                           "E" CHR(13).     */
/*           PUT UNFORMATTED                                                                                 */
/*             "<AT=" STRING(iRow) "," STRING(iCol) "><FArial><P12><B>" cInfoEtiTxt[1] "</B>" SKIP */
/*             "<AT=" STRING(iRow + 4) "," STRING(iCol) ">" cInfoEtiTxt[2] SKIP                              */
/*             "<AT=" STRING(iRow + 8) "," STRING(iCol) ">" cInfoEtiTxt[3] SKIP                              */
/*             "<AT=" STRING(iRow + 12) "," STRING(iCol) ">" cInfoEtiTxt[4].                                 */
      END.



            
/*         If cust-num mod 2 = 0 then         */
/*             put control "<FGCOLOR=GREEN>". */
/*         else                               */
  DO iCount = 1 TO EtikettLogg.Ant:
      IF EtikettLogg.Storl = "INFO" THEN DO:
          PUT UNFORMATTED
            "<AT=" STRING(iRow) "," STRING(iCol) "><FArial><P12><B>" cInfoEtiTxt[1] "</B>" SKIP
            "<AT=" STRING(iRow + 4) "," STRING(iCol) ">" cInfoEtiTxt[2] SKIP
            "<AT=" STRING(iRow + 8) "," STRING(iCol) ">" cInfoEtiTxt[3] SKIP
            "<AT=" STRING(iRow + 12) "," STRING(iCol) ">" cInfoEtiTxt[4].
      END.
      ELSE IF Strekkode.KodeType = 0 THEN
          PUT UNFORMATTED
            "<AT=" STRING(iRow) "," STRING(iCol) "><FArial><P10><B>" ArtBas.Bongtekst "</B>" SKIP
            "<AT=" STRING(iRow + 8) "," STRING(iCol) "><#1><AT=+10,+35>"
              "<BARCODE#1,TYPE=39,CHECKSUM=none,VALUE=" StrekKode.Kode ">"
            "<R+0.5><FArial><P12><AT=," STRING(iCol + 2) ">" "PLU: " + StrekKode.Kode
                .
      ELSE
          PUT UNFORMATTED
            "<AT=" STRING(iRow) "," STRING(iCol) "><FArial><P8>" TRIM(ArtBas.Levkod) + " " + IF AVAIL Levbas THEN SUBSTR(Levbas.levnamn,1,18) ELSE ""
            "<AT=" STRING(iRow + 4) "," STRING(iCol) "><FArial><P9><B>" SUBSTR(ArtBas.Beskr,1,25) "</B>" SKIP
/*               (IF AVAIL Farg AND Farg.FarBeskr <> "" THEN Farg.FarBeskr ELSE ArtBas.LevFargKod) SKIP */
            "<AT=" STRING(iRow + 15) "," STRING(iCol) "><#1><AT=+4,+25>"
              "<BARCODE#1,TYPE=" (IF LENGTH(StrekKode.Kode) = 12 THEN "UPC_A" ELSE "EAN13") ",CHECKSUM=none,VALUE=" StrekKode.Kode ">"
                
/*              (IF iKr2 > 0 THEN "<AT=" + STRING(iRow + 15) + ",+25><FArial Narrow><P12>" + STRING(iKr2) + "<P12>" + "," + STRING(iOren2,"99") ELSE "") */


            "<FArial Narrow><P20><AT=" STRING(iRow + 13) "><C" ENTRY(iColEntry,cKrCol)  "><RIGHT=C+7><B><FGCOLOR=RED>" STRING(EtikettLogg.Pris,">>>>9.99") "</B><FGCOLOR=BLACK>" SKIP
/*             "<FArial Narrow><P28><AT=" STRING(iRow + 8) "," ENTRY(iColEntry,cKrCol)  "><RIGHT=C+6><B><FGCOLOR=RED>" STRING(EtikettLogg.Pris,">>>9.99") "</B><FGCOLOR=BLACK>" SKIP */
            "<FArial><P10><AT=" STRING(iRow + 9) "," STRING(iCol) ">" StrekKode.Kode.
        /* inget ord.pris */ 
/*          (IF iKr2 > 0 THEN "<FArial Narrow><P12><R-.3><C+3>" + STRING(iKr2) + "<P12>" + "," + STRING(iOren2,"99") ELSE ""). */
    IF iRowEntry = 11 AND iColEntry = 3 THEN DO:
        PAGE.
        ASSIGN iRowEntry = 1
               iColEntry = 1.
    END.
    ELSE
        ASSIGN iRowEntry = IF iColEntry < 3 THEN iRowEntry ELSE iRowEntry + 1
               iColEntry = IF iColEntry = 3 THEN 1 ELSE iColEntry + 1.
    ASSIGN iRow = INT(ENTRY(iRowEntry,cRow))
           iCol = INT(ENTRY(iColEntry,cCol)).
/*     IF iRowEntry = 8 AND iColEntry = 3 THEN                               */
/*         PAGE.                                                             */
/*     ASSIGN iRowEntry = IF iColEntry < 3 THEN iRowEntry ELSE iRowEntry + 1 */
/*            iRowEntry = IF iRowEntry = 9 THEN 1 ELSE iRowEntry             */
/*            iRow      = INT(ENTRY(iRowEntry,cRow))                         */
/*            iColEntry = IF iColEntry = 3 THEN 1 ELSE iColEntry + 1         */
/*            iCol      = INT(ENTRY(iColEntry,cCol)).                        */
  END.
  END.
  OUTPUT TO TERMINAL.
/*   FILE-INFO:File-NAME = "BarCode.xpr".      */
/*   Run printFile( FILE-INFO:FULL-PATHNAME ). */
  /* Klargjør rapportfilnavnet */
  ASSIGN FILE-INFO:FILE-NAME = pcRappFil.
    
  /* Sender filen til visning og utskrift. */
 RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). 
   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
       OS-DELETE VALUE(FILE-INFO:FILE-NAME).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

