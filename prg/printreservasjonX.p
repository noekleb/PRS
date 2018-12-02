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
/* DEFINE VAR ipBuntNr LIKE ovBunt.BuntNr  NO-UNDO. */

DEFINE INPUT  PARAMETER ipBuntNr LIKE ovBunt.BuntNr  NO-UNDO.

DEFINE VARIABLE cButikNavnFra AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE cButikNavnTil AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE cRubrik       AS CHARACTER FORMAT "X(16)" NO-UNDO.
DEFINE VARIABLE cBeskr        AS CHARACTER FORMAT "X(25)" NO-UNDO.
DEFINE VARIABLE iColVg        AS INTEGER INIT 6  NO-UNDO.
DEFINE VARIABLE iColLopNr     AS INTEGER INIT 11 NO-UNDO.
DEFINE VARIABLE iColBeskr     AS INTEGER INIT 16 NO-UNDO.
DEFINE VARIABLE iColAntall    AS INTEGER INIT 42 NO-UNDO.
DEFINE VARIABLE iColStorl     AS INTEGER INIT 48 NO-UNDO.
DEFINE VARIABLE iColTilStorl  AS INTEGER INIT 53 NO-UNDO.
DEFINE VARIABLE iColVkost     AS INTEGER INIT 58 NO-UNDO.
DEFINE VARIABLE iColSum       AS INTEGER INIT 67 NO-UNDO.
DEFINE VARIABLE iColMerknad   AS INTEGER INIT 77 NO-UNDO.
DEFINE VARIABLE iStrekrad     AS INTEGER  FORMAT ">9"  NO-UNDO.
DEFINE VARIABLE iRad          AS INTEGER    NO-UNDO.
DEFINE VARIABLE cSideRubrikker  AS CHARACTER  INIT "PLUKKLISTE,MOTTAKSLISTE,FØLGESEDDEL,STØRRELSESENDRING,AVDELINGSREGNSKAP" NO-UNDO.
DEFINE VARIABLE cMerknadLbl     AS CHARACTER INIT "Kommentar:" NO-UNDO.
DEFINE VARIABLE cMerknad        AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE cButikkInfo     AS CHARACTER  INIT "Fra butikk:,Til butikk:" NO-UNDO.
DEFINE VARIABLE cListeRubrikker AS CHARACTER  INIT "Vg,Løpnr,Varetekst,Antall,Str,Til str,Varekost,Sum,Kommentar" NO-UNDO.
DEFINE VARIABLE cSumLinje       AS CHARACTER  INIT "Sum:" NO-UNDO.
DEFINE VARIABLE cKvittering     AS CHARACTER  INIT "Mottatt:,Butikk:" NO-UNDO.
DEFINE VARIABLE cBeskrRub       AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE cOppdaterttxt   AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE cBuntNr         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAvdelingTxt    AS CHARACTER FORMAT "X(13)" EXTENT 10 INIT ["Avd1","Avd2","Avd3","Avd4","Avd5","Avd6","Avd7","Avd8","Avd9","Sum"] NO-UNDO.
DEFINE FRAME PageHeader
   HEADER
      "<OLANDSCAPE><ALIGN=BASE><FArial><R3><B><C6><P14>" TODAY "<C+3>Buntnr:<C+1>" cBuntNr "<C+10><P24>" cRubrik
      "<P12></B><C110><P10>" PAGE-NUMBER FORMAT ">>" SKIP
      "<R5><C6><B>" ENTRY(1,CButikkInfo) FORMAT "X(15)" "<C15>" cButikNavnFra "<C50><RIGHT=C+20>" cOppdaterttxt "</B>"  SKIP
      "<R6><C6><B>" ENTRY(2,CButikkInfo) FORMAT "X(15)" "<C15>" cButikNavnTil "</B>" SKIP
      "<R7><C6><B>" cMerknadLbl FORMAT "X(15)" "<C15>" cMerknad "</B>" SKIP
      "<R8><C6><FROM><R8><C112><LINE>" SKIP
      "<R" STRING(iStrekrad) "><C" STRING(iColVg,">9")       ">" ENTRY(1,cListeRubrikker)
                              "<C" STRING(iColLopNr,">9")    ">" ENTRY(2,cListeRubrikker)
                              "<C" STRING(iColBeskr,">9")    ">" cBeskrRub /* ENTRY(3,cListeRubrikker) */
                              "<C" STRING(iColAntall,"->9")   ">" ENTRY(4,cListeRubrikker)
                              "<C" STRING(iColStorl,">9")    ">" ENTRY(5,cListeRubrikker)
                              "<C" STRING(iColTilStorl,">9") ">" ENTRY(6,cListeRubrikker)
                              "<C" STRING(iColVkost,"->9")    "><RIGHT=C+8>" ENTRY(7,cListeRubrikker)
                              "<C" STRING(iColSum,"->9")      "><RIGHT=C+9>" ENTRY(8,cListeRubrikker)
                              "<C" STRING(iColMerknad,">9")  ">" ENTRY(9,cListeRubrikker) FORMAT "X(12)"
      "<R9><C6><FROM><R9><C112><LINE>" SKIP
      WITH PAGE-TOP STREAM-IO WIDTH 255.

DEFINE FRAME Kvittering
   HEADER
      "<ALIGN=BASE><FArial>"
      "<R" STRING(iRad) "><C20><B>" ENTRY(1,cKvittering) "</B>" SKIP
      "<R" STRING(iRad + 1) "><C20><FROM><R" STRING(iRad + 1) "><C45><LINE>" SKIP

      "<R" STRING(iRad + 1) "><C20><B>" ENTRY(2,cKvittering) "<C26>" cButikNavnTil "</B>" SKIP
      WITH STREAM-IO WIDTH 255.

DEFINE FRAME SumHeader
   HEADER
      "<OLANDSCAPE><ALIGN=BASE><FArial><R3><B><C6><P14>" TODAY "<C+3>Buntnr:<C+1>" cBuntNr "<C+10><P24>" cRubrik
      "<P12></B><C110><P10>" PAGE-NUMBER FORMAT ">>" SKIP
      "<B><C50><RIGHT=C+20>" cOppdaterttxt "</B>" SKIP
      "<R7><C6><B>" cMerknadLbl FORMAT "X(15)" "<C15>" cMerknad "</B>" SKIP
      "<R8><C6><FROM><R8><C112><LINE>" SKIP
      "<R" STRING(iStrekrad) "><C6>" "Fra/Til"
                              "<C14>" cAvdelingTxt[1]
                              "<C24>" cAvdelingTxt[2]
                              "<C34>" cAvdelingTxt[3]
                              "<C44>" cAvdelingTxt[4]
                              "<C54>" cAvdelingTxt[5]
                              "<C64>" cAvdelingTxt[6]
                              "<C74>" cAvdelingTxt[7]
                              "<C84>" cAvdelingTxt[8]
                              "<C94>" cAvdelingTxt[9]
                              "<C104>" cAvdelingTxt[10] SKIP
      "<R9><C6><FROM><R9><C112><LINE>" SKIP
      WITH PAGE-TOP STREAM-IO WIDTH 255.

DEFINE TEMP-TABLE ttOvAvdSum
    FIELD ButikkNrFra LIKE OvBuffer.ButikkNrFra
    FIELD ButikkNrTil LIKE OvBuffer.ButikkNrTil
    FIELD AvdelingSum AS DECI DECIMALS 2 FORMAT "->>>>,>>9.99" EXTENT 10
    INDEX FraTil ButikkNrFra DESC ButikkNrTil DESC.

{xPrint.i}
{runlib.i}

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
{sww.i}
FIND ovBunt WHERE ovBunt.BuntNr = ipBuntNr NO-LOCK NO-ERROR.
IF NOT AVAIL ovBunt THEN DO:
    MESSAGE "Överföring saknas"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
END.
ELSE IF NOT CAN-FIND(FIRST ovBuffer OF ovBunt) THEN DO:
    MESSAGE "Inga artiklar finns registrerade på denna överföring."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
END.
/* RUN InitLabels för språkhantering */ 
ASSIGN cBuntNr = STRING(ipBuntNr).
RUN InitLabels.
RUN SkrivRapport.
{swn.i}
RETURN RETURN-VALUE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-InitLabels) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitLabels Procedure 
PROCEDURE InitLabels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount   AS INTEGER    NO-UNDO.
  FOR EACH SysPara WHERE SysPara.SysHId = 6 AND SysPara.SysGr = 1101 NO-LOCK:
      CASE SysPara.ParaNr:
          WHEN 1 THEN DO:
              IF SysPara.Parameter1 <> "" THEN DO:
                  DO iCount = 1 TO NUM-ENTRIES(SysPara.Parameter1):
                      ASSIGN ENTRY(iCount,cSideRubrikker) = ENTRY(iCount,SysPara.Parameter1).
                  END.
/*                   IF NUM-ENTRIES(SysPara.Parameter1) = 3 THEN     */
/*                       ASSIGN cSideRubrikker = SysPara.Parameter1. */
              END.
          END.
          WHEN 2 THEN DO:
              IF NUM-ENTRIES(SysPara.Parameter1) = 2 THEN
                  ASSIGN cButikkInfo = SysPara.Parameter1.
          END.
          WHEN 3 THEN DO:
              IF NUM-ENTRIES(SysPara.Parameter1) = 9 THEN
                  ASSIGN cListeRubrikker = SysPara.Parameter1.
          END.
          WHEN 4 THEN DO:
              IF NUM-ENTRIES(SysPara.Parameter1) = 1 THEN
                  ASSIGN cSumLinje = SysPara.Parameter1.
          END.
          WHEN 5 THEN DO:
              IF NUM-ENTRIES(SysPara.Parameter1) = 2 THEN
                  ASSIGN cKvittering = SysPara.Parameter1.
          END.
      END CASE.
      ASSIGN cBeskrRub = ENTRY(3,cListeRubrikker).
      DO iCount = 1 TO 9:
          FIND Avdeling WHERE Avdeling.Avdelingnr = iCount NO-LOCK NO-ERROR.
          IF AVAIL Avdeling THEN
             ASSIGN cAvdelingTxt[iCount] = TRIM(CAPS(SUBSTRING(Avdeling.AvdelingNavn, 1, 1) ) +
                                           LC(SUBSTRING(Avdeling.AvdelingNavn, 2,12) )).
      END.
  END.
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
  DEF VAR pcRappFil           AS CHAR               NO-UNDO.
  DEFINE VARIABLE dVarekost   LIKE Lager.VVarekost  NO-UNDO.
  DEFINE VARIABLE dSum        LIKE Lager.VVarekost  NO-UNDO.
  DEFINE VARIABLE dTotSum     LIKE Lager.VVarekost  NO-UNDO.
  DEFINE VARIABLE iProfilNr   LIKE Butiker.Profilnr NO-UNDO.
  DEFINE VARIABLE iAvdelingNr AS INTEGER            NO-UNDO.
  DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.

  DEFINE BUFFER bufovBuffer FOR ovBuffer.
  ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "ovBunt_" + STRING(ipBuntNr) + ".xpr"
         cOppdaterttxt = IF OvBunt.DatoOppdatert = ? THEN "IKKE OPPDATERT" ELSE "".

  /*   IF VALID-HANDLE(wLibHandle) THEN                                          */
  /*      RUN GetTempFileName IN wLibHandle ("ovBunt", "xpr", OUTPUT pcRappFil). */
  /* Åpner stream til skriverfil. */
  OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(48).
  PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
  ASSIGN cMerknad = ovBunt.Merknad.
  PUT CONTROL '<PREVIEW=ZoomToWidth>'.
  FOR EACH ovBuffer OF ovBunt NO-LOCK WHERE ovBuffer.ButikkNrFra <> ovBuffer.ButikkNrTil BREAK /* BY ovBuffer.BuntNr */
                                    BY ovBuffer.ButikkNrFra 
                                    BY ovBuffer.ButikkNrTil:
    IF FIRST-OF(ovBuffer.ButikkNrTil) THEN DO:
        FIND Butiker WHERE Butiker.Butik = ovBuffer.ButikkNrFra NO-LOCK NO-ERROR.
        ASSIGN cButikNavnFra = IF AVAIL Butiker THEN Butiker.ButNamn ELSE "?????????"
               iProfilNr = IF AVAIL Butiker THEN Butiker.Profilnr ELSE 1.
        FIND Butiker WHERE Butiker.Butik = ovBuffer.ButikkNrTil NO-LOCK NO-ERROR.
        ASSIGN cButikNavnTil = IF AVAIL Butiker THEN Butiker.ButNamn ELSE "?????????".
        FIND ttOvAvdSum WHERE ttOvAvdSum.ButikkNrFra = OvBuffer.ButikkNrFra AND
                              ttOvAvdSum.ButikkNrTil = OvBuffer.ButikkNrTil NO-ERROR.
        IF NOT AVAIL ttOvAvdSum THEN DO:
            CREATE ttOvAvdSum.
            ASSIGN ttOvAvdSum.ButikkNrFra = OvBuffer.ButikkNrFra
                   ttOvAvdSum.ButikkNrTil = OvBuffer.ButikkNrTil.
        END.

        IF NOT FIRST(ovBuffer.ButikkNrFra) THEN DO:
            OUTPUT TO VALUE(pcRappFil) APPEND PAGED PAGE-SIZE VALUE(48). /* iPageSize */
        END.
        ASSIGN cRubrik = ENTRY(1,cSideRubrikker).
        VIEW FRAME PageHeader.
        ASSIGN iRad      = 10
               iStrekrad = iRad - 2.
        FOR EACH bufovBuffer OF ovBunt NO-LOCK WHERE bufovBuffer.ButikkNrFra = ovBuffer.ButikkNrFra AND
                                             bufovBuffer.ButikkNrTil = ovBuffer.ButikkNrTil BY
                                             bufovBuffer.Vg BY bufovBuffer.LopNr BY bufovBuffer.Storl.
            IF LINE-COUNTER >= 41 THEN DO:
                PAGE.
                ASSIGN iRad      = 10
                       iStrekrad = iRad - 2.
                VIEW FRAME PageHeader.
            END.
            FIND ArtBas WHERE Artbas.Vg = bufovBuffer.Vg AND ArtBas.LopNr = bufovBuffer.LopNr NO-LOCK NO-ERROR.
            IF AVAIL ArtBas THEN DO:
                FIND HuvGr OF ArtBas NO-LOCK NO-ERROR.
                ASSIGN iAvdelingNr = IF AVAIL HuvGr AND HuvGr.AvdelingNr > 0 AND HuvGr.AvdelingNr < 9 THEN HuvGr.AvdelingNr ELSE 9.
/*                 FIND Lager WHERE Lager.Butik = ovBuffer.ButikkNrFra AND Lager.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK NO-ERROR.   */
/*                 IF AVAIL Lager THEN                                                                                              */
/*                     ASSIGN dVarekost = Lager.VVarekost.                                                                          */
/*                 ELSE DO:                                                                                                         */
/*                     FIND ArtPris WHERE Artpris.Profilnr = iProfilNr AND Artpris.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK NO-ERROR. */
/*                     IF NOT AVAIL Artpris THEN                                                                                    */
/*                         FIND FIRST ArtPris WHERE Artpris.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK NO-ERROR.                        */
/*                     ASSIGN dVarekost = IF AVAIL ArtPris THEN ROUND(ArtPris.Varekost[IF ArtPris.Tilbud THEN 2 ELSE 1],2) ELSE 0.  */
/*                 END.                                                                                                             */
               FIND Farg OF ArtBas NO-LOCK NO-ERROR.
            END.
/*             ELSE                      */
/*                 ASSIGN dVarekost = 0. */
            ASSIGN dVarekost = bufovBuffer.Varekost
                   dSum      = bufovBuffer.Antall * dVarekost
                   dTotSum   = dTotSum + dSum
                   cBeskr    = IF AVAIL ArtBas THEN SUBSTR(ArtBas.Beskr,1,30) ELSE "".
               ACCUMULATE bufovBuffer.Antall (TOTAL).
               
             PUT UNFORMATTED "<FCourier NEW><B><R" STRING(iRad) 
                                   "><C" STRING(iColVg)       ">" String(bufovBuffer.Vg,"zzzzz9")
                                    "<C" STRING(iColLopNr)    ">" STRING(bufovBuffer.LopNr,"zzz9") 
                                    "<C" STRING(iColBeskr)    ">" cBeskr
                                    "<C" STRING(iColAntall)   "><RIGHT=C+4>" STRING(bufovBuffer.Antall,"-zz9")
                                    "<C" STRING(iColStorl)    "><RIGHT=C+4>" bufovBuffer.Storl
                                    "<C" STRING(iColTilStorl) "><RIGHT=C+4>" " "
                                    "<C" STRING(iColVkost)    "><RIGHT=C+8>" STRING(dVarekost,">>,>>9.99")
                                    "<C" STRING(iColSum)      "><RIGHT=C+9>" STRING(dSum,"->>>,>>9.99")
             IF TRIM(bufovBuffer.Storl) <> TRIM(bufovBuffer.Storl) THEN bufovBuffer.TilStorl ELSE "" "</B>"
                                    "<C" STRING(iColMerknad)  ">" (IF AVAILABLE FArg THEN Farg.FarBeskr + " " ELSE "") + bufovBuffer.Merknad SKIP.
                    ASSIGN iRad = iRad + 1.
             ASSIGN ttOvAvdSum.AvdelingSum[iAvdelingNr] = ttOvAvdSum.AvdelingSum[iAvdelingNr] + dSum.
        END.
        PUT UNFORMATTED "<R" STRING(iRad + 1) "><C" STRING(iColBeskr + 15) "><FROM><R" STRING(iRad + 1) "><C" 
            STRING(iColMerknad - 1) "><LINE>" SKIP
            "<B><R" STRING(iRad + 1) "><C" STRING(iColBeskr) "><RIGHT=C+25>" cSumLinje
            "<C" STRING(iColAntall) "><RIGHT=C+4>" STRING((ACCUM TOTAL bufovBuffer.Antall),"-zz9") "<C" STRING(iColSum) "><RIGHT=C+9>" STRING(dTotSum,"->>>,>>9.99").
        PAGE.
/*         RUN skrivPlocklista(ovBuffer.ButikkNrFra,ovBuffer.ButikkNrTil). */
        ASSIGN cRubrik = ENTRY(2,cSideRubrikker).
        VIEW FRAME PageHeader.
        ASSIGN iRad      = 10
               iStrekrad = iRad - 2
               dTotSum   = 0.
        FOR EACH bufovBuffer OF ovBunt NO-LOCK WHERE bufovBuffer.ButikkNrFra = ovBuffer.ButikkNrFra AND
                                             bufovBuffer.ButikkNrTil = ovBuffer.ButikkNrTil BY
                                             bufovBuffer.Vg BY bufovBuffer.LopNr BY bufovBuffer.Storl:
            IF LINE-COUNTER >= 41 THEN DO:
                PAGE.
                ASSIGN iRad      = 10
                       iStrekrad = iRad - 2.
                VIEW FRAME PageHeader.
            END.
            FIND ArtBas WHERE Artbas.Vg = bufovBuffer.Vg AND ArtBas.LopNr = bufovBuffer.LopNr NO-LOCK NO-ERROR.
/*             IF AVAIL ArtBas THEN DO:                                                                                             */
/*                 FIND Lager WHERE Lager.Butik = ovBuffer.ButikkNrFra AND Lager.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK NO-ERROR.   */
/*                 IF AVAIL Lager THEN                                                                                              */
/*                     ASSIGN dVarekost = Lager.VVarekost.                                                                          */
/*                 ELSE DO:                                                                                                         */
/*                     FIND ArtPris WHERE Artpris.Profilnr = iProfilNr AND Artpris.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK NO-ERROR. */
/*                     IF NOT AVAIL Artpris THEN                                                                                    */
/*                         FIND FIRST ArtPris WHERE Artpris.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK NO-ERROR.                        */
/*                     ASSIGN dVarekost = IF AVAIL ArtPris THEN ROUND(ArtPris.Varekost[IF ArtPris.Tilbud THEN 2 ELSE 1],2) ELSE 0.  */
/*                 END.                                                                                                             */
/*             END.                                                                                                                 */
/*             ELSE                                                                                                                 */
/*                 ASSIGN dVarekost = 0.                                                                                            */
            IF AVAILABLE ArtBAs THEN FIND Farg OF ArtBas NO-LOCK NO-ERROR.
            ASSIGN dVarekost = bufovBuffer.Varekost
                   dSum    = bufovBuffer.Antall * dVarekost
                   dTotSum = dTotSum + dSum
                   cBeskr  = IF AVAIL ArtBas THEN ArtBas.Beskr ELSE "".
             ACCUMULATE bufovBuffer.Antall (TOTAL).
             PUT UNFORMATTED "<FCourier NEW><B><R" STRING(iRad) 
                                   "><C" STRING(iColVg)       ">" String(bufovBuffer.Vg,"zzzzz9")
                                    "<C" STRING(iColLopNr)    ">" STRING(bufovBuffer.LopNr,"zzz9") 
                                    "<C" STRING(iColBeskr)    ">" cBeskr
                                    "<C" STRING(iColAntall)   "><RIGHT=C+4>" STRING(bufovBuffer.Antall,"-zz9")
                                    "<C" STRING(iColStorl)    "><RIGHT=C+4>" bufovBuffer.Storl
                                    "<C" STRING(iColTilStorl) "><RIGHT=C+4>" " "
                                    "<C" STRING(iColVkost)    "><RIGHT=C+8>" STRING(dVarekost,">>,>>9.99")
                                    "<C" STRING(iColSum)      "><RIGHT=C+9>" STRING(dSum,"->>>,>>9.99")
             IF TRIM(bufovBuffer.Storl) <> TRIM(bufovBuffer.Storl) THEN bufovBuffer.TilStorl ELSE "" "</B>"
                                    "<C" STRING(iColMerknad)  ">" (IF AVAILABLE FArg THEN Farg.FarBeskr + " " ELSE "") + bufovBuffer.Merknad SKIP.
                    ASSIGN iRad = iRad + 1.
        END.
        PUT UNFORMATTED "<R" STRING(iRad + 1) "><C" STRING(iColBeskr + 15) "><FROM><R" STRING(iRad + 1) "><C" 
            STRING(iColMerknad - 1) "><LINE>" SKIP
            "<B><R" STRING(iRad + 1) "><C" STRING(iColBeskr) "><RIGHT=C+25>" cSumLinje
            "<C" STRING(iColAntall) "><RIGHT=C+4>" STRING((ACCUM TOTAL bufovBuffer.Antall),"-zz9") "<C" STRING(iColSum) "><RIGHT=C+9>" STRING(dTotSum,"->>>,>>9.99").
        PAGE.
/*         RUN skrivMottaksbilag(ovBuffer.ButikkNrFra,ovBuffer.ButikkNrTil). */
        ASSIGN cRubrik = ENTRY(3,cSideRubrikker).
        VIEW FRAME PageHeader.
        ASSIGN iRad      = 10
               iStrekrad = iRad - 2
               dTotSum = 0.
        FOR EACH bufovBuffer OF ovBunt NO-LOCK WHERE bufovBuffer.ButikkNrFra = ovBuffer.ButikkNrFra AND
                                             bufovBuffer.ButikkNrTil = ovBuffer.ButikkNrTil BY
                                             bufovBuffer.Vg BY bufovBuffer.LopNr BY bufovBuffer.Storl:
            IF LINE-COUNTER >= 41 THEN DO:
                PAGE.
                ASSIGN iRad      = 10
                       iStrekrad = iRad - 2.
                VIEW FRAME PageHeader.
            END.
            FIND ArtBas WHERE Artbas.Vg = bufovBuffer.Vg AND ArtBas.LopNr = bufovBuffer.LopNr NO-LOCK NO-ERROR.
            IF AVAILABLE ArtBas THEN FIND farg OF ArtBas NO-LOCK NO-ERROR.
            ASSIGN cBeskr = IF AVAIL ArtBas THEN ArtBas.Beskr ELSE "".
            ACCUMULATE bufovBuffer.Antall (TOTAL).
            PUT UNFORMATTED "<FCourier NEW><B><R" STRING(iRad) 
                                  "><C" STRING(iColVg)       ">" String(bufovBuffer.Vg,"zzzzz9")
                                   "<C" STRING(iColLopNr)    ">" STRING(bufovBuffer.LopNr,"zzz9") 
                                   "<C" STRING(iColBeskr)    ">" cBeskr
                                   "<C" STRING(iColAntall)   "><RIGHT=C+4>" STRING(bufovBuffer.Antall,"-zz9")
                                   "<C" STRING(iColStorl)    "><RIGHT=C+4>" bufovBuffer.Storl 
                                   "<C" STRING(iColTilStorl) ">" 
                                   "<C" STRING(iColVkost)    ">"
                                   "<C" STRING(iColSum)      ">"
           IF TRIM(bufovBuffer.Storl) <> TRIM(bufovBuffer.Storl) THEN bufovBuffer.TilStorl ELSE "" "</B>"
                                   "<C" STRING(iColMerknad)  ">" (IF AVAILABLE FArg THEN Farg.FarBeskr + " " ELSE "") + bufovBuffer.Merknad SKIP.
                    ASSIGN iRad = iRad + 1.
        END.
        PUT UNFORMATTED "<R" STRING(iRad + 1) "><C" STRING(iColBeskr + 15) "><FROM><R" STRING(iRad + 1) "><C" 
            STRING(iColAntall + 4) "><LINE>" SKIP
            "<B><R" STRING(iRad + 1) "><C" STRING(iColBeskr) "><RIGHT=C+25>" cSumLinje
                     "<C" STRING(iColAntall) "><RIGHT=C+4>" STRING((ACCUM TOTAL bufovBuffer.Antall),"-zz9").
        iRad = iRad + 4.
        VIEW FRAME Kvittering.
        OUTPUT CLOSE.
/*         RUN skrivMottaksbilag(ovBuffer.ButikkNrFra,ovBuffer.ButikkNrTil). */
    END.
  END.
  OUTPUT CLOSE.
  IF CAN-FIND(FIRST ovBuffer OF ovBunt WHERE ovBuffer.ButikkNrFra = ovBuffer.ButikkNrTil) THEN DO:
/*       ASSIGN cRubrik = "STORLEKSÄNDRING". */
      ASSIGN cRubrik = ENTRY(4,cSideRubrikker).
      OUTPUT TO VALUE(pcRappFil) APPEND PAGED PAGE-SIZE VALUE(48). /* iPageSize */
      FOR EACH ovBuffer OF ovBunt NO-LOCK WHERE ovBuffer.ButikkNrFra = ovBuffer.ButikkNrTil BREAK
                                  BY ovBuffer.ButikkNrFra:
          IF FIRST-OF(ovBuffer.ButikkNrFra) THEN DO:
              FIND Butiker WHERE Butiker.Butik = ovBuffer.ButikkNrFra NO-LOCK NO-ERROR.
              ASSIGN cButikNavnFra = IF AVAIL Butiker THEN Butiker.ButNamn ELSE "?????????"
                     cButikNavnTil = cButikNavnFra.
              IF NOT FIRST(ovBuffer.ButikkNrFra) THEN DO:
                  OUTPUT TO VALUE(pcRappFil) APPEND PAGED PAGE-SIZE VALUE(48). /* iPageSize */
              END.
              VIEW FRAME PageHeader.
              ASSIGN iRad      = 10
                     iStrekrad = iRad - 2.
              FOR EACH bufovBuffer OF ovBunt NO-LOCK WHERE bufovBuffer.ButikkNrFra = ovBuffer.ButikkNrFra AND
                                                   bufovBuffer.ButikkNrTil = ovBuffer.ButikkNrTil BY
                                                   bufovBuffer.Vg BY bufovBuffer.LopNr BY bufovBuffer.Storl 
                                                   BY bufovBuffer.TilStorl.
                  IF LINE-COUNTER >= 41 THEN DO:
                      PAGE.
                      ASSIGN iRad      = 10
                             iStrekrad = iRad - 2.
                      VIEW FRAME PageHeader.
                  END.
                  FIND ArtBas WHERE Artbas.Vg = bufovBuffer.Vg AND ArtBas.LopNr = bufovBuffer.LopNr NO-LOCK NO-ERROR.
                  IF AVAILABLE ArtBas THEN FIND farg OF ArtBas NO-LOCK NO-ERROR.
                  ASSIGN cBeskr = IF AVAIL ArtBas THEN ArtBas.Beskr ELSE "".
                   ACCUMULATE bufovBuffer.Antall (TOTAL).
                  PUT UNFORMATTED "<FCourier NEW><B><R" STRING(iRad) 
                                   "><C" STRING(iColVg)       ">" String(bufovBuffer.Vg,"zzzzz9")
                                   "<C" STRING(iColLopNr)    ">" STRING(bufovBuffer.LopNr,"zzz9") 
                                   "<C" STRING(iColBeskr)    ">" cBeskr
                                   "<C" STRING(iColAntall)   "><RIGHT=C+4>" STRING(bufovBuffer.Antall,"-zz9")
                                   "<C" STRING(iColStorl)    "><RIGHT=C+4>"  bufovBuffer.Storl 
                                   "<C" STRING(iColTilStorl) "><RIGHT=C+4>"  bufovBuffer.TilStorl "</B>"
                                   "<C" STRING(iColMerknad)  ">" (IF AVAILABLE FArg THEN Farg.FarBeskr + " " ELSE "") + bufovBuffer.Merknad SKIP.
                             
                          ASSIGN iRad = iRad + 1.
              END.
              PUT UNFORMATTED "<R" STRING(iRad + 1) "><C" STRING(iColBeskr + 15) "><FROM><R" STRING(iRad + 1) "><C" 
                  STRING(iColAntall + 4) "><LINE>" SKIP
                  "<B><R" STRING(iRad + 1) "><C" STRING(iColBeskr) "><RIGHT=C+25>" cSumLinje
                           "<C" STRING(iColAntall) "><RIGHT=C+4>" STRING((ACCUM TOTAL bufovBuffer.Antall),"-zz9").
              PAGE.
              OUTPUT CLOSE.
      /*         RUN skrivMottaksbilag(ovBuffer.ButikkNrFra,ovBuffer.ButikkNrTil). */
          END.
      END.
  END.
  IF CAN-FIND(FIRST ttOvAvdSum) THEN DO:
      OUTPUT TO VALUE(pcRappFil) APPEND PAGED PAGE-SIZE VALUE(48). /* iPageSize */
      ASSIGN cRubrik = ENTRY(5,cSideRubrikker).
      PAGE.
      VIEW FRAME SumHeader.

      FOR EACH ttOvAvdSum BY ttOvAvdSum.ButikkNrFra BY ttOvAvdSum.ButikkNrTil.
          DO iCount = 1 TO 9:
              ASSIGN ttOvAvdSum.AvdelingSum[10] = ttOvAvdSum.AvdelingSum[10] + 
                                                  ttOvAvdSum.AvdelingSum[iCount].
/*    FÖR TEST    ttOvAvdSum.AvdelingSum[iCount] = ttOvAvdSum.AvdelingSum[iCount] + 1000000 */
          END.
/*           PUT UNFORMATTED "<FCourier NEW><B><R10>" */
          PUT UNFORMATTED "<B><R+1>" 
            "<C6>"  ttOvAvdSum.ButikkNrFra "/"
             "<C9>" ttOvAvdSum.ButikkNrTil
             "<C13><RIGHT=C+9>" ttOvAvdSum.AvdelingSum[1] FORMAT "->>>>,>>9.99"
             "<C23><RIGHT=C+9>" ttOvAvdSum.AvdelingSum[2] FORMAT "->>>>,>>9.99"
             "<C33><RIGHT=C+9>" ttOvAvdSum.AvdelingSum[3] FORMAT "->>>>,>>9.99"
             "<C43><RIGHT=C+9>" ttOvAvdSum.AvdelingSum[4] FORMAT "->>>>,>>9.99"
             "<C53><RIGHT=C+9>" ttOvAvdSum.AvdelingSum[5] FORMAT "->>>>,>>9.99"
             "<C63><RIGHT=C+9>" ttOvAvdSum.AvdelingSum[6] FORMAT "->>>>,>>9.99"
             "<C73><RIGHT=C+9>" ttOvAvdSum.AvdelingSum[7] FORMAT "->>>>,>>9.99"
             "<C83><RIGHT=C+9>" ttOvAvdSum.AvdelingSum[8] FORMAT "->>>>,>>9.99"
             "<C93><RIGHT=C+9>" ttOvAvdSum.AvdelingSum[9] FORMAT "->>>>,>>9.99"
             "<C103><RIGHT=C+9>" ttOvAvdSum.AvdelingSum[10] FORMAT "->>>>>,>>9.99" SKIP.

       END.
       OUTPUT CLOSE.
  END.
  OUTPUT TO TERMINAL.

  /* Klargjør rapportfilnavnet */
  ASSIGN FILE-INFO:File-NAME = pcRappFil.
    
  /* Sender filen til visning og utskrift. */
   RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). 
   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
       OS-DELETE VALUE(FILE-INFO:FILE-NAME).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

