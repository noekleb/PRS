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
/* Parameters Definitions ---                                           */
  DEFINE INPUT  PARAMETER cListeType         AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cReklamasjonsListe AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE         iCl                AS INTEGER    NO-UNDO.
  DEFINE VARIABLE         cLogo              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE         cTitel             AS CHARACTER FORMAT "x(25)" NO-UNDO.
  DEFINE VARIABLE         cClInfo            AS CHARACTER  FORMAT "x(70)" NO-UNDO.
  DEFINE BUFFER clButiker FOR Butiker.
  DEFINE FRAME PageHeader
     HEADER
        "<ALIGN=BASE><FArial><R4><P20><B><C1><CENTER=C80>" cTitel "<P12></B><C75><P10>" PAGE-NUMBER FORMAT ">>" SKIP
        "<R5><C6><FROM><R5><C78><LINE>" SKIP
        WITH PAGE-TOP STREAM-IO WIDTH 255.

{xPrint.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getButNavn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getButNavn Procedure 
FUNCTION getButNavn RETURNS CHARACTER
  ( INPUT iButik AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFeil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFeil Procedure 
FUNCTION getFeil RETURNS CHARACTER
  ( INPUT iFeilKode AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPostNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPostNr Procedure 
FUNCTION getPostNr RETURNS CHARACTER
  ( INPUT cPostNr AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSysPara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSysPara Procedure 
FUNCTION getSysPara RETURNS CHARACTER
  ( INPUT iSysHId AS INTEGER, INPUT iSysGr AS INTEGER, INPUT iParaNr AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTilTak) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTilTak Procedure 
FUNCTION getTilTak RETURNS CHARACTER
  ( INPUT iBetalesAv AS INTEGER )  FORWARD.

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
{syspara.i 5 1 1 iCl INT}
FIND Butiker WHERE Butiker.Butik = iCl NO-LOCK NO-ERROR.
IF AVAIL Butiker THEN
    ASSIGN cClInfo = TRIM(Butiker.ButNamn) + "<R+1><C6>" + TRIM(Butiker.BuAdr) + " " + Butiker.BuPonr +
      getPostNr(Butiker.BuPonr) + " Tel: " + Butiker.BuTel.

{syspara.i 5 4 30 cLogo}
IF cLogo = "" THEN
    cLogo = "icon\orderlogo.bmp".
IF cReklamasjonsListe <> "" THEN
    RUN SkrivUt IN THIS-PROCEDURE.
ELSE RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-PageFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PageFooter Procedure 
PROCEDURE PageFooter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  PUT UNFORMATTED
      "<R66><C6><FROM><R66><C78><LINE>"
      "<R67><C6>" cClInfo SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivDato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivDato Procedure 
PROCEDURE SkrivDato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    PUT UNFORMATTED "<R4><C6>" STRING(TODAY) SKIP.
    IF SEARCH(cLogo) <> ? THEN DO:
        ASSIGN FILE-INFO:File-NAME = cLogo.
        PUT UNFORMATTED
            "<TRANSPARENT=false><R2,5><C55><#3><R4,9><C75><IMAGE#3="
            FILE-INFO:FULL-PATHNAME + ">".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivKundeInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivKundeInfo Procedure 
PROCEDURE SkrivKundeInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      PUT UNFORMATTED "<FArial><R12><C9>" ReklamasjonsLogg.KundeNavn    SKIP
                      "<FArial><R13><C9>" ReklamasjonsLogg.KundeAdresse SKIP
                      "<FArial><R14><C9>" ReklamasjonsLogg.PostNr getPostNr(ReklamasjonsLogg.PostNr)    SKIP.
        
/*             ReklamasjonsLogg.KundeNr */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivLevInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivLevInfo Procedure 
PROCEDURE SkrivLevInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND LevBas OF ReklamasjonsLogg NO-LOCK NO-ERROR.
  IF NOT AVAIL LevBas THEN
      RETURN.
  IF cListeType = "1" OR cListeType = "2" THEN
    PUT UNFORMATTED "<R12><C9>" LevBas.levnamn    SKIP
                    "<R13><C9>" LevBas.levadr SKIP
                    "<R14><C9>" LevBas.levponr getPostNr(LevBas.levponr) SKIP
                    "<R15><C9>" LevBas.levland SKIP.
  ELSE IF cListeType = "3" THEN DO:
      PUT UNFORMATTED "<R8><C9><B><U>Leverandørinfo</U></B><C45><B><U>Kontaktinfo</U></B>"
          "<R10><C9>Navn:<C20>"      LevBas.levnamn   "<C45>"   LevBas.levkon SKIP
          "<R11><C9>Adresse:<C20>"   LevBas.levadr    "<C45>"   LevBas.koadr  SKIP       
          "<R12><C9>Postadr:<C20>"  LevBas.levponr  getPostNr(LevBas.levponr)   SKIP
                                                   "<C45>"   LevBas.koponr getPostNr(LevBas.koponr)  SKIP
          "<R13><C9>Telefon:<C20>"  LevBas.levtel  "<C45>"     LevBas.kotel         SKIP
          "<R14><C9>Telefax:<C20>"  LevBas.telefax "<C45>"     LevBas.kotelefax     SKIP
          "<R15><C9>Mobil:<C20>"    LevBas.telex   "<C45>"     LevBas.kotelex       SKIP
          "<R16><C9>Email:<C20>"    LevBas.E_MailLev "<C45>"   LevBas.E_MailKontakt  SKIP
          "<R17><C9>Land:<C20>"     LevBas.levland   "<C45>"   LevBas.koland.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivLinjeInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivLinjeInfo Procedure 
PROCEDURE SkrivLinjeInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iRad AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iToggle AS INTEGER    NO-UNDO.
    ASSIGN iRad = 28
           iToggle = 1.
    FOR EACH ReklamasjonsLinje OF ReklamasjonsLogg NO-LOCK BY ReklamasjonsLinje.ArtikkelNr BY ReklamasjonsLinje.Storl:
        IF iToggle = 3 THEN DO:
            ASSIGN iToggle = 0
                   iRad    = 7.
            RUN PageFooter.
            PAGE.
            VIEW FRAME PageHeader.
            RUN SkrivDato.
        END.
        PUT UNFORMATTED "<R" STRING(iRad + 1) "><C9><B><U>Reklamert</U><C40><U>Solgt</U><C60><U>Verdi</U></B>"
                        "<R" STRING(iRad + 2) "><C9>Butikk: <C20>" getButNavn(ReklamasjonsLinje.Butik) 
                             "<C40>" getButNavn(ReklamasjonsLinje.SolgtIButikk) 
                             "<C60>Verdi: <C68><RIGHT=C+10>" STRING(ReklamasjonsLinje.ReklamVerdi,">>>,>>9.99") SKIP
                        "<R" STRING(iRad + 3) "><C9>Dato: <C20>" ReklamasjonsLinje.Dato "<C40>" ReklamasjonsLinje.SolgtDato
                             "<C60>Utgifter: <C68><RIGHT=C+10>" STRING(ReklamasjonsLinje.ReklamUtgifter,">>>,>>9.99") SKIP
                        "<R" STRING(iRad + 4) "><C9>Bongnr: <C20>" ReklamasjonsLinje.BongId "<C40>" ReklamasjonsLinje.SolgtBongId
                              "<C60>Total: <C68><RIGHT=C+10>" STRING(ReklamasjonsLinje.ReklamTotal,">>>,>>9.99") SKIP.
        ASSIGN iRad = iRad + 5.
        PUT UNFORMATTED "<R" STRING(iRad) "><C9><B><U>Bonginfo</U></B>"
                        "<R" STRING(iRad + 1) "><C9>Artikkel: <C20>" ReklamasjonsLinje.ArtikkelNr " (" ReklamasjonsLinje.Vg "/" ReklamasjonsLinje.LopNr ")"
                             "<C60>Antall: <C68><RIGHT=C+10>" ReklamasjonsLinje.Antall SKIP
                        "<R" STRING(iRad + 2) "><C9>Levkode: <C20>"   ReklamasjonsLinje.LevKod
                                               "<C60>Pris: <C68><RIGHT=C+10>"    STRING(ReklamasjonsLinje.Pris,">>>,>>9.99") SKIP
                        "<R" STRING(iRad + 3) "><C9>Varetekst: <C20>" ReklamasjonsLinje.Varetekst
                                               "<C60>Mva: <C68><RIGHT=C+10>"       STRING(ReklamasjonsLinje.Mva,">>>,>>9.99") SKIP
                        "<R" STRING(iRad + 4) "><C9>Størrelse: <C20>" TRIM(ReklamasjonsLinje.Storl)
                                              "<C60>Subrab: <C68><RIGHT=C+10>"     STRING(ReklamasjonsLinje.SubtotalRab,">>>,>>9.99") SKIP
                        "<R" STRING(iRad + 5) "><C60>Rabatt: <C68><RIGHT=C+10>"    STRING(ReklamasjonsLinje.RabKr,">>>,>>9.99") SKIP
                        "<R" STRING(iRad + 6) "><C60>Utbetalt: <C68><RIGHT=C+10>"   STRING(ReklamasjonsLinje.AkseptertVerdi,">>>,>>9.99") SKIP
            "<R" STRING(iRad + 7) "><C9><B>Feilkode: " ReklamasjonsLinje.FeilKode  " </B>" getFeil(ReklamasjonsLinje.FeilKode) SKIP
            "<R" STRING(iRad + 8) "><C9><FROM><R" STRING(iRad + 8) "><C78><LINE>" SKIP
            .
        ASSIGN iRad    = iRad + 9
               iToggle = iToggle + 1.

/* /*                 ReklamasjonsLinje.LopNr */                     */
/*                 X                                                 */
/*                 X                                                 */
/*                 X                ReklamasjonsLinje.KassaNr        */
/*                 X                ReklamasjonsLinje.BongId         */
/*                 X                ReklamasjonsLinje.ForsNr         */
/*                 X                ReklamasjonsLinje.SelgerNr       */
/*                                                                   */
/*                 X                                                 */
/*                 X                                                 */
/*                 X                ReklamasjonsLinje.SolgtForsNr    */
/*                 X                ReklamasjonsLinje.SolgtBongId    */
/*                                                                   */
/*                 X                                                 */
/*                 X                                                 */
/*                 X                                                 */
/*                                                                   */
/*                 X                       */
/*                 X                ReklamasjonsLinje.FeilNotat      */
/*                                                                   */
/*                 X                     */
/*                 X                             */
/*                 X                          */
/*                 X                         */
/*                 X                      */
/*                 X                          */
/*                                                                   */
/*                 X                         */
/*                 X                           */
/*                 X                            */
/*                 X                    */
/*                 X                 */
/*                 X                 */
/*                                                                   */
/* /*                 ReklamasjonsLinje.BongLinjeNr */               */
/* /*                 ReklamasjonsLinje.BrukerID */                  */
/* /*                 ReklamasjonsLinje.EDato */                     */
/* /*                 ReklamasjonsLinje.ETid  */                     */
/* /*                 ReklamasjonsLinje.LinjeNr */                   */
/* /*                 ReklamasjonsLinje.RegistrertAv   */            */
/* /*                 ReklamasjonsLinje.RegistrertDato */            */
/* /*                 ReklamasjonsLinje.RegistrertTid  */            */
/* /*                 ReklamasjonsLinje.ReklamasjonsNr */            */
/* /*                 ReklamasjonsLinje.SeqNr */                     */
/*                                                                   */
/* /*                 ReklamasjonsLinje.Tid       */                 */
/* /*                 ReklamasjonsLinje.TransNr   */                 */
/* /*                 ReklamasjonsLinje.VVarekost */                 */
    END.
    RUN PageFooter.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivLoggInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivLoggInfo Procedure 
PROCEDURE SkrivLoggInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  IF cListeType = "1" OR cListeType = "2" THEN DO:
      PUT UNFORMATTED "<R20><C9><FROM><R20><C78><LINE>" SKIP
                      "<R20><C9><B>Behandling kunde</B>" SKIP
                      "<R21><C9><B>Avgjørelse: </B><R22><C9>" getSysPara(15,2,ReklamasjonsLogg.AkseptertKunde)  SKIP
                      "<R23><C9><B>Besluttet: </B><R24><C9>" ReklamasjonsLogg.AkseptertAv " "
                        IF ReklamasjonsLogg.AkseptertDato <> ? THEN STRING(ReklamasjonsLogg.AkseptertDato) ELSE ""   SKIP
                      "<R27><C9><FROM><R27><C78><LINE>" SKIP.
      PUT UNFORMATTED "<R21><C35><#1><R27><C78><FRAME#1><USE#1>" ReklamasjonsLogg.AkseptertNotat  "</USE>"  SKIP.
  END.
  ELSE DO:
      PUT UNFORMATTED "<R19><C9><FROM><R19><C78><LINE>" SKIP
                      "<R19><C9><B>Behandling leverandør</B>" SKIP
                      "<R21><C9><B>Betales av: </B>"
                      "<R22><C9>" getTilTak(ReklamasjonsLogg.BetalesAv)  SKIP
                      "<R23><C9><B>Beløp: </B><C20><RIGHT=C+10>" STRING(ReklamasjonsLogg.ReklamVerdi,">>>,>>9.99")  SKIP
                      "<R24><C9><B>Utgifter: </B><C20><RIGHT=C+10>" STRING(ReklamasjonsLogg.ReklamUtgifter,">>>,>>9.99")  SKIP
                      "<R25><C9><B>Total: </B><C20><RIGHT=C+10>" STRING(ReklamasjonsLogg.ReklamTotal,">>>,>>9.99")  SKIP
                      "<R26><C9><B>Besluttet: </B><C20>" ReklamasjonsLogg.BetalesBruker " " 
                                  IF ReklamasjonsLogg.BetalesDato <> ? THEN STRING(ReklamasjonsLogg.BetalesDato)  ELSE "" SKIP
                      "<R27><C9><FROM><R27><C78><LINE>" SKIP.
      PUT UNFORMATTED "<R21><C35><#1><R27><C78><FRAME#1><USE#1>" ReklamasjonsLogg.BetalesNotat  "</USE>"  SKIP.
  END.

/*         ReklamasjonsLogg.AkseptertBesluttet */
/*         ReklamasjonsLogg.AkseptertVerdi     */
/*         ReklamasjonsLogg.BetalesAv          */
/*         ReklamasjonsLogg.BetalesAvgjort     */
/*         ReklamasjonsLogg.BetalesBruker      */
/*         ReklamasjonsLogg.BetalesDato        */
/*         ReklamasjonsLogg.BetalesNotat       */
/*         ReklamasjonsLogg.BrukerID           */
/*         ReklamasjonsLogg.EDato              */
/*         ReklamasjonsLogg.ETid               */
/*         ReklamasjonsLogg.KundeAdresse       */
/*         ReklamasjonsLogg.KundeE-Mail        */
/*         ReklamasjonsLogg.KundeMobil         */
/*         ReklamasjonsLogg.KundeNavn          */
/*         ReklamasjonsLogg.KundeNr            */
/*         ReklamasjonsLogg.KundeTelefon       */
/*         ReklamasjonsLogg.LevNr              */
/*         ReklamasjonsLogg.PostNr             */
/*         ReklamasjonsLogg.RegistrertAv       */
/*         ReklamasjonsLogg.RegistrertDato     */
/*         ReklamasjonsLogg.RegistrertTid      */
/*         ReklamasjonsLogg.ReklamasjonsNr     */
/*         ReklamasjonsLogg.ReklamStatus       */
/*         ReklamasjonsLogg.ReklamTotal        */
/*         ReklamasjonsLogg.ReklamUtgifter     */
/*         ReklamasjonsLogg.ReklamVerdi        */
/*         ReklamasjonsLogg.SluttfortAv        */
/*         ReklamasjonsLogg.SluttfortBesluttet */
/*         ReklamasjonsLogg.SluttfortDato      */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivRekStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivRekStatus Procedure 
PROCEDURE SkrivRekStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    PUT UNFORMATTED
          "<R6><C9><B>Reklamasjon nr:</B> " ReklamasjonsLogg.ReklamasjonsNr " <B>Dato:</B> " ReklamasjonsLogg.RegistrertDato 
          " <B>Status:</B> " getSysPara(15,2,ReklamasjonsLogg.ReklamStatus) 
          IF ReklamasjonsLogg.SluttfortBesluttet THEN " <B>Besluttet:</B> " + ReklamasjonsLogg.SluttfortAv + " " +
              STRING(ReklamasjonsLogg.SluttfortDato) ELSE "" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivUt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivUt Procedure 
PROCEDURE SkrivUt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount  AS INTEGER    NO-UNDO.
    DEFINE VARIABLE pcRappFil AS CHARACTER  NO-UNDO.
    ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "SErek.xpr".
    /* tillfällig tömning av fil */
    OUTPUT TO VALUE(pcRappFil).
    OUTPUT CLOSE.
    DO iCount = 1 TO NUM-ENTRIES(cReklamasjonsListe):
        FIND ReklamasjonsLogg WHERE 
             ReklamasjonsLogg.ReklamasjonsNr = INT(ENTRY(iCount,cReklamasjonsListe))
             NO-LOCK NO-ERROR.
        IF AVAIL ReklamasjonsLogg THEN
            FIND Levbas OF ReklamasjonsLogg NO-LOCK NO-ERROR.
        IF NOT AVAIL LevBas THEN
            NEXT.

        /* Varje reklamation skall starta från sidonummer 1 */
        OUTPUT TO VALUE(pcRappFil) APPEND PAGED PAGE-SIZE VALUE(80).
        PUT CONTROL '<PREVIEW=ZoomToWidth>'.
        CASE cListeType:
            WHEN "1" THEN DO:
                ASSIGN cTitel = "Kundereklamasjon".
                VIEW FRAME PageHeader.
                RUN SkrivDato.
                RUN SkrivRekStatus.
                RUN SkrivKundeInfo.
                RUN SkrivLoggInfo.
                RUN SkrivLinjeInfo.
            END.
            WHEN "2" THEN DO:
                ASSIGN cTitel = "Leverandørreklamasjon".
                VIEW FRAME PageHeader.
                RUN SkrivDato.
                RUN SkrivRekStatus.
                RUN SkrivLevInfo.
                RUN SkrivLoggInfo.
                RUN SkrivLinjeInfo.
            END.
            WHEN "3" THEN DO:
                ASSIGN cTitel = "Reklamasjonsliste".
                VIEW FRAME PageHeader.
                RUN SkrivDato.
                RUN SkrivRekStatus.
                RUN SkrivLevInfo.
                RUN SkrivLoggInfo.
                RUN SkrivLinjeInfo.
            END.
        END CASE.
        /* Skriv Levinfo           */
        /* Skriv ReklamatsjonsLogg */
/*         ReklamasjonsLogg.AkseptertAv        */
/*         ReklamasjonsLogg.AkseptertBesluttet */
/*         ReklamasjonsLogg.AkseptertDato      */
/*         ReklamasjonsLogg.AkseptertKunde     */
/*         ReklamasjonsLogg.AkseptertNotat     */
/*         ReklamasjonsLogg.AkseptertVerdi     */
/*         ReklamasjonsLogg.BetalesAv          */
/*         ReklamasjonsLogg.BetalesAvgjort     */
/*         ReklamasjonsLogg.BetalesBruker      */
/*         ReklamasjonsLogg.BetalesDato        */
/*         ReklamasjonsLogg.BetalesNotat       */
/*         ReklamasjonsLogg.BrukerID           */
/*         ReklamasjonsLogg.EDato              */
/*         ReklamasjonsLogg.ETid               */
/*         ReklamasjonsLogg.KundeAdresse       */
/*         ReklamasjonsLogg.KundeE-Mail        */
/*         ReklamasjonsLogg.KundeMobil         */
/*         ReklamasjonsLogg.KundeNavn          */
/*         ReklamasjonsLogg.KundeNr            */
/*         ReklamasjonsLogg.KundeTelefon       */
/*         ReklamasjonsLogg.LevNr              */
/*         ReklamasjonsLogg.PostNr             */
/*         ReklamasjonsLogg.RegistrertAv       */
/*         ReklamasjonsLogg.RegistrertDato     */
/*         ReklamasjonsLogg.RegistrertTid      */
/*         ReklamasjonsLogg.ReklamasjonsNr     */
/*         ReklamasjonsLogg.ReklamStatus       */
/*         ReklamasjonsLogg.ReklamTotal        */
/*         ReklamasjonsLogg.ReklamUtgifter     */
/*         ReklamasjonsLogg.ReklamVerdi        */
/*         ReklamasjonsLogg.SluttfortAv        */
/*         ReklamasjonsLogg.SluttfortBesluttet */
/*         ReklamasjonsLogg.SluttfortDato      */

/*         FOR EACH ReklamasjonsLinje OF ReklamasjonsLogg NO-LOCK BY ReklamasjonsLinje.ArtikkelNr BY ReklamasjonsLinje.Storl: */
/*             ReklamasjonsLinje.AkseptertVerdi                                                                               */
/* /*                 ReklamasjonsLinje.LopNr */                                                                              */
/*                 ReklamasjonsLinje.ArtikkelNr                                                                               */
/*                 ReklamasjonsLinje.Vg                                                                                       */
/*                 ReklamasjonsLinje.LopNr                                                                                    */
/*                 ReklamasjonsLinje.Storl                                                                                    */
/*                 ReklamasjonsLinje.Antall                                                                                   */
/* /*                 ReklamasjonsLinje.BongId      */                                                                        */
/* /*                 ReklamasjonsLinje.BongLinjeNr */                                                                        */
/* /*                 ReklamasjonsLinje.BrukerID */                                                                           */
/*                 ReklamasjonsLinje.Butik                                                                                    */
/*                 ReklamasjonsLinje.Dato                                                                                     */
/* /*                 ReklamasjonsLinje.EDato */                                                                              */
/* /*                 ReklamasjonsLinje.ETid  */                                                                              */
/*                 ReklamasjonsLinje.FeilKode                                                                                 */
/*                 ReklamasjonsLinje.FeilNotat                                                                                */
/*                 ReklamasjonsLinje.ForsNr                                                                                   */
/*                 ReklamasjonsLinje.KassaNr                                                                                  */
/*                 ReklamasjonsLinje.LevKod                                                                                   */
/* /*                 ReklamasjonsLinje.LinjeNr */                                                                            */
/*                 ReklamasjonsLinje.Mva                                                                                      */
/*                 ReklamasjonsLinje.Pris                                                                                     */
/*                 ReklamasjonsLinje.RabKr                                                                                    */
/* /*                 ReklamasjonsLinje.RegistrertAv   */                                                                     */
/* /*                 ReklamasjonsLinje.RegistrertDato */                                                                     */
/* /*                 ReklamasjonsLinje.RegistrertTid  */                                                                     */
/* /*                 ReklamasjonsLinje.ReklamasjonsNr */                                                                     */
/*                 ReklamasjonsLinje.ReklamTotal                                                                              */
/*                 ReklamasjonsLinje.ReklamUtgifter                                                                           */
/*                 ReklamasjonsLinje.ReklamVerdi                                                                              */
/*                 ReklamasjonsLinje.SelgerNr                                                                                 */
/* /*                 ReklamasjonsLinje.SeqNr */                                                                              */
/*                 ReklamasjonsLinje.SolgtBongId                                                                              */
/*                 ReklamasjonsLinje.SolgtDato                                                                                */
/*                 ReklamasjonsLinje.SolgtForsNr                                                                              */
/*                 ReklamasjonsLinje.SolgtIButikk                                                                             */
/*                 ReklamasjonsLinje.SubtotalRab                                                                              */
/*                 ReklamasjonsLinje.Tid                                                                                      */
/*                 ReklamasjonsLinje.TransNr                                                                                  */
/*                 ReklamasjonsLinje.Varetekst                                                                                */
/*                 ReklamasjonsLinje.VVarekost                                                                                */
        OUTPUT CLOSE.
        END.
/*         OUTPUT TO TERMINAL. */

/* Klargjør rapportfilnavnet */
ASSIGN FILE-INFO:File-NAME = pcRappFil.

/* Sender filen til visning og utskrift. */
/*  RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). */
 RUN VisXprint.p (pcRappFil).    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getButNavn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getButNavn Procedure 
FUNCTION getButNavn RETURNS CHARACTER
  ( INPUT iButik AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND Butiker WHERE Butiker.Butik = iButik NO-LOCK NO-ERROR.
  RETURN IF AVAIL Butiker THEN Butiker.ButNamn ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFeil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFeil Procedure 
FUNCTION getFeil RETURNS CHARACTER
  ( INPUT iFeilKode AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND Feilkode WHERE Feilkode.FeilKode = iFeilKode NO-LOCK NO-ERROR.
  RETURN IF AVAIL FeilKode THEN Feilkode.Beskrivelse ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPostNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPostNr Procedure 
FUNCTION getPostNr RETURNS CHARACTER
  ( INPUT cPostNr AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cBeskrivelse AS CHARACTER  NO-UNDO.
  IF TRIM(cPostNr) <> "" THEN
      FIND Post WHERE Post.PostNr = cPostNr NO-LOCK NO-ERROR.
  IF AVAIL Post THEN DO:
      ASSIGN cBeskrivelse =  Post.Beskrivelse.
      RELEASE Post.
  END.
  RETURN IF cBeskrivelse <> "" THEN " " + cBeskrivelse ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSysPara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSysPara Procedure 
FUNCTION getSysPara RETURNS CHARACTER
  ( INPUT iSysHId AS INTEGER, INPUT iSysGr AS INTEGER, INPUT iParaNr AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND SysPara WHERE SysPara.SysHId = iSysHId AND
                     SysPara.SysGr  = iSysGr  AND
                     SysPara.ParaNr = ReklamasjonsLogg.AkseptertKunde NO-LOCK NO-ERROR.
  RETURN IF AVAIL SysPara THEN SysPara.Parameter1 ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTilTak) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTilTak Procedure 
FUNCTION getTilTak RETURNS CHARACTER
  ( INPUT iBetalesAv AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND Kravkode WHERE Kravkode.KravKode = iBetalesAv NO-LOCK NO-ERROR.
  RETURN IF AVAIL KravKode THEN KravKode.Beskrivelse ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

