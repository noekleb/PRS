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
DEFINE VARIABLE cTyper AS CHARACTER   NO-UNDO. /* håller på rapporttyper */

DEFINE VARIABLE cSendEmail   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMailhub   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDoAUTH    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAuthType  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUser      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPassword  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEmailCC   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEMailTo  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEMailFra AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEmne    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cVedlegg AS CHARACTER   NO-UNDO.
DEFINE VARIABLE E-Meddelande AS CHARACTER NO-UNDO.
DEFINE VARIABLE lMailOK    AS LOG         NO-UNDO.
DEFINE VARIABLE crlf       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMessage   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFiles AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE TT_Mailto NO-UNDO
    FIELD butik AS INTE
    FIELD rapport AS CHAR
    FIELD email AS CHAR
    FIELD cc AS CHAR
    INDEX butik IS PRIMARY butik.

DEFINE TEMP-TABLE tt_analyser NO-UNDO
    FIELD analyseid AS INTEGER
    INDEX anal IS PRIMARY analyseid.

DEFINE TEMP-TABLE tt_butiker NO-UNDO
    FIELD analyseid AS INTEGER
    FIELD butik     AS INTE
    FIELD dato      AS DATE
    FIELD stengd    AS LOG
    INDEX butik IS PRIMARY analyseid butik.

DEFINE TEMP-TABLE tt_kampanjer NO-UNDO
    FIELD analyseid  AS INTEGER
    FIELD kampid     AS DECIMAL
    FIELD kamptilbid AS INTEGER
    FIELD radnr      AS INTE
    INDEX ankamp     IS PRIMARY analyseid kampid kamptilbid.

DEFINE TEMP-TABLE tt_artiklar NO-UNDO
    FIELD analyseid  AS INTEGER
    FIELD artikkelnr AS DECIMAL
    FIELD radnr      AS INTEGER
    INDEX anart IS PRIMARY analyseid artikkelnr.

DEFINE TEMP-TABLE tt_vg NO-UNDO
    FIELD analyseid  AS INTEGER
    FIELD vg AS DECIMAL
    FIELD radnr      AS INTEGER
    INDEX anvg IS PRIMARY analyseid vg.

DEFINE TEMP-TABLE tt_tilbud NO-UNDO
    FIELD analyseid            AS INTEGER
    FIELD kampid               AS DECIMAL
    FIELD kamptilbid           AS INTEGER
    FIELD ProdFamId            AS DECIMAL
    FIELD KampTilbArtMinAntall AS DECI
    FIELD Solgtantall          AS DECI
    FIELD radnr      AS INTEGER
    INDEX kamp IS PRIMARY analyseid kampid kamptilbid.

DEFINE TEMP-TABLE tt_tilbudart NO-UNDO
    FIELD ProdFamId            AS DECIMAL
    FIELD Artikkelnr           AS DECI
    INDEX art IS PRIMARY UNIQUE ProdFamId Artikkelnr.

DEFINE TEMP-TABLE tt_utskrivet NO-UNDO
    FIELD butik     AS INTEGER
    FIELD analyseid AS INTEGER
    INDEX buart IS PRIMARY UNIQUE butik analyseid.

DEFINE TEMP-TABLE tt_totbutresult NO-UNDO LIKE preemanalyseresult.
DEFINE TEMP-TABLE tt_totallabutresult NO-UNDO LIKE preemanalyseresult.

{ pdf_inc.i "NOT SUPER"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getTyper) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTyper Procedure 
FUNCTION getTyper RETURNS CHARACTER
  ( INPUT ipanalyseid AS INTEGER )  FORWARD.

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

/* lägg alla aktiva analyser i temp-table kö */

RUN ByggTT_analys.

/* förbered email */
RUN EmailPrepare.

/* Bygg en temp-table med alla butiker */
RUN AnalyseLoop.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-AnalyseLoop) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AnalyseLoop Procedure 
PROCEDURE AnalyseLoop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH tt_analyser:
        IF CAN-FIND(FIRST tt_butiker WHERE tt_butiker.Analyseid = tt_analyser.analyseid) THEN DO:
            cTyper = getTyper(tt_analyser.AnalyseId).
            RUN ByggTTArtKamp.
            RUN Rapport.
        END.
    END.
    /* här kör vi rapporter */
    FOR EACH tt_analyser:
        IF NOT CAN-FIND(FIRST tt_butiker WHERE tt_butiker.analyseid = tt_analyser.analyseid) THEN
            RUN ByggTT_butikerTMP.
        RUN RapportPDF.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTTArtKamp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTTArtKamp Procedure 
PROCEDURE ByggTTArtKamp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* DEFINE TEMP-TABLE tt_kampanjer NO-UNDO                       */
/*     FIELD analyseid  AS INTEGER                              */
/*     FIELD kampid     AS DECIMAL                              */
/*     FIELD kamptilbid AS INTEGER                              */
/*     FIELD radnr      AS INTE                                 */
/*     INDEX ankamp     IS PRIMARY analyseid kampid kamptilbid. */
/*                                                              */
/* DEFINE TEMP-TABLE tt_artiklar NO-UNDO                        */
/*     FIELD analyseid  AS INTEGER                              */
/*     FIELD artikkelnr AS DECIMAL                              */
/*     FIELD radnr      AS INTEGER                              */
/*     INDEX anart IS PRIMARY analyseid artikkelnr.             */
/* DEFINE TEMP-TABLE tt_vg NO-UNDO         */
/*     FIELD analyseid  AS INTEGER         */
/*     FIELD vg AS DECIMAL                 */
/*     FIELD radnr      AS INTEGER         */
/*     INDEX anvg IS PRIMARY analyseid vg. */
    EMPTY TEMP-TABLE tt_vg.
    EMPTY TEMP-TABLE tt_kampanjer.
    EMPTY TEMP-TABLE tt_artiklar.
    EMPTY TEMP-TABLE tt_tilbud.
    EMPTY TEMP-TABLE tt_tilbudart.

    FOR EACH preemanalyserad WHERE preemanalyserad.AnalyseId = tt_analyser.analyseid AND
                                   preemanalyserad.typ       = 4 NO-LOCK.
        FOR EACH preemanalysembr WHERE preemanalysembr.AnalyseId = tt_analyser.analyseid AND
                                       preemanalysembr.radnr     = preemanalyserad.radnr NO-LOCK.
            IF preemanalysembr.artikkelnr = 0 THEN
                NEXT.
            CREATE tt_artiklar.
            ASSIGN tt_artiklar.analyseid  = tt_analyser.analyseid
                   tt_artiklar.artikkelnr = preemanalysembr.artikkelnr
                   tt_artiklar.radnr      = preemanalysembr.radnr.
            END.
    END.
    FOR EACH preemanalyserad WHERE preemanalyserad.AnalyseId = tt_analyser.analyseid AND
                                   preemanalyserad.typ       = 5 NO-LOCK.
        FOR EACH preemanalysembr WHERE preemanalysembr.AnalyseId = tt_analyser.analyseid AND
                                       preemanalysembr.radnr     = preemanalyserad.radnr NO-LOCK.
            IF preemanalysembr.kampid = 0 OR preemanalysembr.kamptilbid = 0 THEN
                NEXT.
            FIND kampanjetilbud WHERE kampanjetilbud.kampid = preemanalysembr.kampid AND
                                      kampanjetilbud.kamptilbid = preemanalysembr.kamptilbid NO-LOCK NO-ERROR.
            IF NOT AVAIL kampanjetilbud THEN
                NEXT.
            CREATE tt_kampanjer.
            ASSIGN tt_kampanjer.analyseid  = tt_analyser.analyseid
                   tt_kampanjer.kampid     = preemanalysembr.kampid
                   tt_kampanjer.kamptilbid = preemanalysembr.kamptilbid
                   tt_kampanjer.radnr      = preemanalysembr.radnr.
            FOR EACH KampanjeTilbArtikkel OF kampanjetilbud NO-LOCK:
                CREATE tt_tilbud.
                ASSIGN tt_tilbud.analyseid            = tt_analyser.analyseid
                       tt_tilbud.kampid               = preemanalysembr.kampid
                       tt_tilbud.kamptilbid           = preemanalysembr.kamptilbid
                       tt_tilbud.ProdFamId            = KampanjeTilbArtikkel.ProdFamId
                       tt_tilbud.KampTilbArtMinAntall = KampanjeTilbArtikkel.KampTilbArtMinAntall
                       tt_tilbud.radnr                = preemanalysembr.radnr.

                IF KampanjeTilbArtikkel.KampTilbArtId <> 0 THEN DO:
                    CREATE tt_tilbudart.
                    ASSIGN tt_tilbudart.prodfamid  = KampanjeTilbArtikkel.ProdFamId
                           tt_tilbudart.artikkelnr = KampanjeTilbArtikkel.KampTilbArtId.
                END.
                ELSE DO:
                    FOR EACH ProduktFamMedlem WHERE ProduktFamMedlem.Prodfamid = KampanjeTilbArtikkel.ProdFamId NO-LOCK:
                        FIND tt_tilbudart WHERE tt_tilbudart.prodfamid  = KampanjeTilbArtikkel.ProdFamId    AND
                                                tt_tilbudart.artikkelnr = ProduktFamMedlem.ProdFamArtikkelNr NO-ERROR.
                        IF NOT AVAIL tt_tilbudart THEN DO:
                            CREATE tt_tilbudart.
                            ASSIGN tt_tilbudart.prodfamid  = KampanjeTilbArtikkel.ProdFamId
                                   tt_tilbudart.artikkelnr = ProduktFamMedlem.ProdFamArtikkelNr.
                        END.
                    END.
                END.
            END.
        END.
    END.
    FOR EACH preemanalyserad WHERE preemanalyserad.AnalyseId = tt_analyser.analyseid AND
                                   preemanalyserad.typ       = 6 NO-LOCK.
        FOR EACH preemanalysembr WHERE preemanalysembr.AnalyseId = tt_analyser.analyseid AND
                                       preemanalysembr.radnr     = preemanalyserad.radnr NO-LOCK.
            IF preemanalysembr.artikkelnr = 0 THEN
                NEXT.
            CREATE tt_vg.
            ASSIGN tt_vg.analyseid  = tt_analyser.analyseid
                   tt_vg.vg = preemanalysembr.artikkelnr
                   tt_vg.radnr      = preemanalysembr.radnr.
            END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTT_analys) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTT_analys Procedure 
PROCEDURE ByggTT_analys :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dDato AS DATE    NO-UNDO.
    DEFINE VARIABLE d31decFgAr AS DATE        NO-UNDO.
    DEFINE VARIABLE iDayNum    AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cData AS CHARACTER   NO-UNDO.
    FOR EACH preemanalyse WHERE preemanalyse.aktiv = TRUE NO-LOCK.
        IF preemanalyse.startdato >= TODAY THEN
            NEXT.
        IF TODAY - preemanalyse.SluttDato > 1 THEN
            NEXT.
        ASSIGN d31decFgar = DATE(12,31,YEAR(preemanalyse.startdato) - 1).

        FIND butikkteam WHERE butikkteam.BrGrpNr = 1 and butikkteam.TeamTypeId = 2
             AND butikkteam.teamnr = preemanalyse.teamnr NO-LOCK NO-ERROR.
        IF NOT AVAIL butikkteam THEN
            NEXT.
        IF NOT CAN-FIND(FIRST ButikkKobling OF butikkteam) THEN
            NEXT.
        CREATE tt_analyser.
        ASSIGN tt_analyser.analyseid = preemanalyse.analyseid.
        FOR EACH ButikkKobling OF butikkteam NO-LOCK:
            FIND ApnSkjema WHERE ApnSkjema.ButikkNr = butikkkobling.butik AND ApnSkjema.Ar = YEAR(preemanalyse.startdato) NO-LOCK NO-ERROR.
            IF NOT AVAIL ApnSkjema THEN
                NEXT.
            DO dDato = preemanalyse.startdato TO preemanalyse.sluttdato:
                IF dDato = TODAY THEN
                    LEAVE.
                IF can-find(FIRST preemanalyseresult WHERE preemanalyseresult.AnalyseId = preemanalyse.analyseid AND
                                                           preemanalyseresult.butikknr  = butikkkobling.butik    AND
                                                           preemanalyseresult.dato      = dDato) THEN
                    NEXT.
                /* Har butiken godkänd dag */
                iDayNum    = dDato - d31decFgar.
                cData      = ENTRY(iDayNum,ApnSkjema.OpenClosed) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    LEAVE.
                IF cData = "4" OR cData = "0" THEN DO:
                    CREATE tt_butiker.
                    ASSIGN tt_butiker.AnalyseId = preemanalyse.analyseid
                           tt_butiker.butik     = butikkkobling.butik
                           tt_butiker.dato      = dDato
                           tt_butiker.stengd    = cData = "0".
                END.
                ELSE
                    LEAVE.
            END.
        END.
    END.

END PROCEDURE.

/* 

DEFINE TEMP-TABLE tt_butiker NO-UNDO
    FIELD analyseid AS INTEGER
    FIELD butik AS INTE
    FIELD firstday AS DATE
    FIELD lastday  AS DATE
    INDEX butik IS PRIMARY analyseid butik.
 
 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTT_butikerTMP) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTT_butikerTMP Procedure 
PROCEDURE ByggTT_butikerTMP :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH preemanalyse WHERE preemanalyse.analyseId = tt_analyser.analyseid NO-LOCK.
        FIND butikkteam WHERE butikkteam.BrGrpNr = 1 and butikkteam.TeamTypeId = 2
             AND butikkteam.teamnr = preemanalyse.teamnr NO-LOCK NO-ERROR.
        IF NOT AVAIL butikkteam THEN
            NEXT.
        FOR EACH ButikkKobling OF butikkteam NO-LOCK.
                    CREATE tt_butiker.
                    ASSIGN tt_butiker.AnalyseId = preemanalyse.analyseid
                           tt_butiker.butik     = butikkkobling.butik.
/*                            tt_butiker.dato      = dDato        */
/*                            tt_butiker.stengd    = cData = "0". */
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EmailPrepare) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EmailPrepare Procedure 
PROCEDURE EmailPrepare :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {syspara.i 210 256 5 cSendEmail}
  IF cSendEmail = "J" THEN DO:
      {syspara.i 50 50 1 cMailhub }
      {syspara.i 50 50 2 cDoAUTH  }
      {syspara.i 50 50 3 cAuthType}
      {syspara.i 50 50 4 cUser    }
      {syspara.i 50 50 5 cPassword}
      {syspara.i 210 256 10 cEMailTo}
      {syspar2.i 210 256 10 cEmailCC}
      {syspara.i 210 256 20 cEmailFra}
      {syspara.i 210 256 30 cEmne}
  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PageHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PageHeader Procedure 
PROCEDURE PageHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND preemanalyse WHERE preemanalyse.analyseid = tt_analyser.analyseid NO-LOCK NO-ERROR.
IF AVAIL preemanalyse THEN DO:
    FIND butiker WHERE butiker.butik = tt_butiker.butik NO-LOCK NO-ERROR.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",14).
    RUN pdf_text_at IN h_PDFinc ("Spdf","Station " + STRING(tt_butiker.butik) + " " + (IF AVAIL butiker THEN butiker.butnamn ELSE ""),10).
    RUN pdf_skip    IN h_PDFinc ("Spdf").
    RUN pdf_text_at IN h_PDFinc ("Spdf","Säljmål " + preemanalyse.Navn,10).
    RUN pdf_skip    IN h_PDFinc ("Spdf").
    RUN pdf_skip    IN h_PDFinc ("Spdf").

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Rapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rapport Procedure 
PROCEDURE Rapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iAntKunder   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE lSalg        AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE iKoeff       AS INTEGER     NO-UNDO.
    DEFINE VARIABLE dSalgssumTmp AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dSalgssum    AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dDrivmSum    AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dDrivmVol    AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE iAnt         AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cSkipHG      AS CHARACTER   NO-UNDO.
/*     cSkipHG = "1,4,5,6,7,10,19,22,23,25,29,30,31,32,34,35,47,50,53,54,55,56,57,58,59,60,61,63,64,65,66,67,68,69". */
    cSkipHG = "1,2,4,5,6,7,10,19,22,23,24,25,29,30,31,32,34,35,36,37,44,47,49,50,51,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69".
    FOR EACH tt_butiker WHERE tt_butiker.analyseid = tt_analyser.analyseid BY dato BY butik:
        IF tt_butiker.stengd = TRUE THEN DO:
            CREATE preemanalyseresult.
            ASSIGN preemanalyseresult.AnalyseId = tt_analyser.analyseid
                   preemanalyseresult.butikknr  = tt_butiker.butik
                   preemanalyseresult.Dato      = tt_butiker.dato
                   preemanalyseresult.radnr     = 1.
        END.
        ELSE DO:
            ASSIGN iAntKunder = 0
                   dSalgssum  = 0
                   dDrivmSum  = 0
                   dDrivmVol  = 0.
            FOR EACH kasse WHERE kasse.butikknr = tt_butiker.butik NO-LOCK:
                FOR EACH bonghode WHERE bonghode.butikknr = kasse.butikknr AND
                                        bonghode.gruppenr = kasse.gruppenr AND
                                        bonghode.kassenr  = kasse.kassenr  AND
                                        bonghode.dato     = tt_butiker.dato AND
                                        bonghode.makulert <> 2 NO-LOCK:
                    ASSIGN lSalg = FALSE.
                    BLINJELOOP:
                    FOR EACH bonglinje WHERE bonglinje.b_id = bonghode.b_id AND bonglinje.makulert = FALSE NO-LOCK:
                        IF NOT CAN-DO("1,3,10",STRING(bonglinje.ttid)) OR bonglinje.antall = 0 THEN
                            NEXT.
                        FIND HuvGr WHERE HuvGr.Hg = Bonglinje.HovedGr NO-LOCK NO-ERROR.
                        ASSIGN lSalg = TRUE.
                        ASSIGN iKoeff         = IF BongLinje.Antall > 0 THEN 1 ELSE -1
/*                                dSalgssumTmp = BongLinje.LinjeSum - BongLinje.Linjerab - BongLinje.SubtotalRab. */
                               dSalgssumTmp = (BongLinje.LinjeSum - BongLinje.Linjerab - BongLinje.SubtotalRab - BongLinje.MvaKr) * iKoeff.
                        IF lSalg = TRUE AND AVAIL HuvGr AND HuvGr.avdelingnr <> 1 THEN DO:
                            /* Totfsg */
                            IF NOT CAN-DO(cSkipHG,STRING(Bonglinje.HovedGr)) THEN
                                ASSIGN dSalgssum = dSalgssum + dSalgssumTmp.
                        END.
                        /* Är det drivmedel med */
                        IF CAN-DO(cTyper,"3") THEN DO:
                            IF AVAIL HuvGr AND HuvGr.avdelingnr = 1 THEN DO:
                                ASSIGN dDrivmSum = dDrivmSum + dSalgssumTmp
                                       dDrivmVol = dDrivmVol + bonglinje.antall.
                            END.
                        END.
                        /* Är det kampanjeuppföljning */
                        IF CAN-DO(cTyper,"5") THEN DO:
                            IF bonglinje.kampid <> 0 AND bonglinje.kamptilbid <> 0 THEN DO:
                                FIND tt_kampanjer WHERE tt_kampanjer.kampid     = bonglinje.kampid AND
                                                        tt_kampanjer.kamptilbid = bonglinje.kamptilbid NO-ERROR.
                                IF AVAIL tt_kampanjer THEN DO:
                                    FOR EACH tt_tilbud WHERE tt_tilbud.analyseid  = tt_analyser.analyseid AND
                                                             tt_tilbud.kampid     = bonglinje.kampid AND
                                                             tt_tilbud.kamptilbid = bonglinje.kamptilbid:
                                        IF CAN-FIND(tt_tilbudart WHERE tt_tilbudart.prodfamid = tt_tilbud.prodfamid AND
                                                                       tt_tilbudart.artikkelnr = DECI(bonglinje.artikkelnr)) THEN DO:
                                            ASSIGN tt_tilbud.solgtantall = tt_tilbud.solgtantall + bonglinje.antall.
                                            /* om vi fått träff i kampanj så läser vi nästa */
/*  Preem vill dubbellogga i.e om i kampanj även om artikel  NEXT BLINJELOOP. */
                                        END.
                                    END.
                                END.
                            END.
                        END.
                        /* Är det vguppföljning */
                        IF CAN-DO(cTyper,"6") THEN DO:
                            IF CAN-FIND(FIRST tt_vg WHERE tt_vg.analyseid  = tt_analyser.analyseid AND
                                                                tt_vg.vg = DECI(bonglinje.varegr)) THEN DO:
                                FIND FIRST tt_vg WHERE tt_vg.analyseid  = tt_analyser.analyseid AND
                                                       tt_vg.vg         = DECI(bonglinje.varegr).
                                FIND preemanalyseresult WHERE preemanalyseresult.AnalyseId = tt_analyser.analyseid AND
                                                              preemanalyseresult.butikknr  = tt_butiker.butik      AND
                                                              preemanalyseresult.Dato      = tt_butiker.dato       AND
                                                              preemanalyseresult.radnr     = tt_vg.radnr NO-ERROR.
                                IF NOT AVAIL preemanalyseresult THEN DO:
                                    CREATE preemanalyseresult.
                                    ASSIGN preemanalyseresult.AnalyseId = tt_analyser.analyseid
                                           preemanalyseresult.butikknr  = tt_butiker.butik
                                           preemanalyseresult.Dato      = tt_butiker.dato
                                           preemanalyseresult.radnr     = tt_vg.radnr.
                                END.
                                ASSIGN preemanalyseresult.antall = preemanalyseresult.antall + bonglinje.antall
                                       preemanalyseresult.summa  = preemanalyseresult.summa  + dSalgssumTmp.
/*  Preem vill dubbellogga i.e om i vg även om artikel  NEXT BLINJELOOP. */
                            END.
                        END.
                        /* Är det artikeluppföljning */
                        IF CAN-DO(cTyper,"4") THEN DO:
                            IF CAN-FIND(FIRST tt_artiklar WHERE tt_artiklar.analyseid  = tt_analyser.analyseid AND
                                                                tt_artiklar.artikkelnr = DECI(bonglinje.artikkelnr)) THEN DO:
                                FIND FIRST tt_artiklar WHERE tt_artiklar.analyseid  = tt_analyser.analyseid AND
                                                                tt_artiklar.artikkelnr = DECI(bonglinje.artikkelnr).
                                FIND preemanalyseresult WHERE preemanalyseresult.AnalyseId = tt_analyser.analyseid AND
                                                              preemanalyseresult.butikknr  = tt_butiker.butik      AND
                                                              preemanalyseresult.Dato      = tt_butiker.dato       AND
                                                              preemanalyseresult.radnr     = tt_artiklar.radnr NO-ERROR.
                                IF NOT AVAIL preemanalyseresult THEN DO:
                                    CREATE preemanalyseresult.
                                    ASSIGN preemanalyseresult.AnalyseId = tt_analyser.analyseid
                                           preemanalyseresult.butikknr  = tt_butiker.butik
                                           preemanalyseresult.Dato      = tt_butiker.dato
                                           preemanalyseresult.radnr     = tt_artiklar.radnr.
                                END.
                                ASSIGN preemanalyseresult.antall = preemanalyseresult.antall + bonglinje.antall
                                       preemanalyseresult.summa  = preemanalyseresult.summa  + dSalgssumTmp.
                            END.
                        END.
                    END.
                    IF lSalg = TRUE THEN
                        ASSIGN iAntKunder = iAntKunder + 1.
                    /* här kollar vi om vi fått tillslag på tilbud */
                    FOR EACH tt_tilbud BREAK BY tt_tilbud.kampid BY tt_tilbud.kamptilbId:
                        IF FIRST-OF(tt_tilbud.kamptilbid) AND tt_tilbud.Solgtantall > 0 THEN DO:
                            FIND preemanalyseresult WHERE preemanalyseresult.AnalyseId = tt_analyser.analyseid AND
                                                          preemanalyseresult.butikknr  = tt_butiker.butik      AND
                                                          preemanalyseresult.Dato      = tt_butiker.dato       AND
                                                          preemanalyseresult.radnr     = tt_tilbud.radnr NO-ERROR.
                            IF NOT AVAIL preemanalyseresult THEN DO:
                                CREATE preemanalyseresult.
                                ASSIGN preemanalyseresult.AnalyseId = tt_analyser.analyseid
                                       preemanalyseresult.butikknr  = tt_butiker.butik
                                       preemanalyseresult.Dato      = tt_butiker.dato
                                       preemanalyseresult.radnr     = tt_tilbud.radnr.
                            END.
                            ASSIGN preemanalyseresult.antall = preemanalyseresult.antall + TRUNCATE(tt_tilbud.Solgtantall / KampTilbArtMinAntall,0).
                        END.
                        tt_tilbud.Solgtantall = 0.
                    END.
                END.
            END.
            IF CAN-DO(cTyper,"1") THEN DO:
                FIND preemanalyserad WHERE preemanalyserad.analyseid = tt_analyser.analyseid AND
                                           preemanalyserad.typ       = 1 NO-LOCK NO-ERROR.
                IF AVAIL preemanalyserad THEN DO:
                    CREATE preemanalyseresult.
                    ASSIGN preemanalyseresult.AnalyseId = tt_analyser.analyseid
                           preemanalyseresult.butikknr  = tt_butiker.butik
                           preemanalyseresult.Dato      = tt_butiker.dato
                           preemanalyseresult.radnr     = preemanalyserad.radnr
                           preemanalyseresult.antall    = iAntkunder.
                END.
            END.
            IF CAN-DO(cTyper,"2") THEN DO:
                FIND preemanalyserad WHERE preemanalyserad.analyseid = tt_analyser.analyseid AND
                                           preemanalyserad.typ       = 2 NO-LOCK NO-ERROR.
                IF AVAIL preemanalyserad THEN DO:
                    CREATE preemanalyseresult.
                    ASSIGN preemanalyseresult.AnalyseId = tt_analyser.analyseid
                           preemanalyseresult.butikknr  = tt_butiker.butik
                           preemanalyseresult.Dato      = tt_butiker.dato
                           preemanalyseresult.radnr     = preemanalyserad.radnr
                           preemanalyseresult.summa     = dSalgssum.
/*                            preemanalyseresult.summa     = ROUND(dSalgssum / iAntkunder,2). */
                END.
            END.
            IF CAN-DO(cTyper,"3") THEN DO:
                FIND preemanalyserad WHERE preemanalyserad.analyseid = tt_analyser.analyseid AND
                                           preemanalyserad.typ       = 3 NO-LOCK NO-ERROR.
                IF AVAIL preemanalyserad THEN DO:
                    CREATE preemanalyseresult.
                    ASSIGN preemanalyseresult.AnalyseId = tt_analyser.analyseid
                           preemanalyseresult.butikknr  = tt_butiker.butik
                           preemanalyseresult.Dato      = tt_butiker.dato
                           preemanalyseresult.radnr     = preemanalyserad.radnr
                           preemanalyseresult.antall    = dDrivmVol
                           preemanalyseresult.summa     = dDrivmSum.
/*                            preemanalyseresult.antall    = ROUND(dDrivmVol / iAntkunder,3)  */
/*                            preemanalyseresult.summa     = ROUND(dDrivmSum / iAntkunder,2). */
                END.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RapportPDF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportPDF Procedure 
PROCEDURE RapportPDF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iAntfelt   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cRubriker  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iAntKunder AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iAntall    AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iAntDagar  AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cEntryTmp  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEntryRapp AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE ii         AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iCols      AS INTEGER  EXTENT 14   NO-UNDO.
    DEFINE VARIABLE pcRappfil  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iToRight AS INTEGER  EXTENT 14   NO-UNDO.
    DEFINE VARIABLE dOrgWidth AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dRiktigWidth AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE cReplaceTxt AS CHARACTER   NO-UNDO.
    ASSIGN iCols[1] = 10.
    FIND LAST preemanalyserad WHERE preemanalyserad.analyseid = tt_analyser.analyseid NO-LOCK.
    iAntfelt = preemanalyserad.radnr.
    cEntryTmp = FILL("0,",iAntFelt) + "0".
    cEntryTmp = REPLACE(cEntryTmp,",",CHR(9)).
    cRubriker = cEntryTmp.
    ENTRY(1,cRubriker,CHR(9)) = "Datum".

    FOR EACH preemanalyserad WHERE preemanalyserad.analyseid = tt_analyser.analyseid NO-LOCK.
        ENTRY(preemanalyserad.radn + 1,cRubriker,CHR(9)) =  preemanalyserad.beskr.
    END.
/*     OUTPUT TO "CLIPBOARD". */
    ASSIGN cVedlegg = "".
    FOR EACH tt_butiker WHERE tt_butiker.analyseid = tt_analyser.analyseid:
        IF CAN-FIND(tt_utskrivet WHERE tt_utskrivet.butik     = tt_butiker.butik AND
                                       tt_utskrivet.analyseid = tt_butiker.analyseid) THEN
            NEXT.
        CREATE tt_utskrivet.
        ASSIGN tt_utskrivet.butik     = tt_butiker.butik
               tt_utskrivet.analyseid = tt_butiker.analyseid.
      DO:
        ASSIGN pcRappFil = SESSION:TEMP-DIR + "Saljmal" + "_" + STRING(tt_butiker.butik) + ".pdf"
               cVedlegg = cVedlegg + (IF cVedlegg <> "" THEN "," ELSE "") + pcRappfil
               cFiles = cFiles + (IF cFiles <> "" THEN "," ELSE "") + ENTRY(NUM-ENTRIES(pcRappfil,"\"),pcRappfil,"\").
        RUN pdf_new IN h_PDFinc ("Spdf",pcRappFil).
        RUN pdf_set_BottomMargin IN h_PDFinc ("Spdf", 60).
        RUN pdf_set_PaperType IN h_PDFinc ("Spdf","A4").
          /*   RUN LoadFonts. */
        RUN pdf_set_Orientation IN h_PDFinc ("Spdf","Landscape").
        RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",12).
        RUN pdf_new_page IN h_PDFinc ("Spdf").
        RUN PageHeader.
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
        DO ii = 1 TO NUM-ENTRIES(cRubriker,CHR(9)):
            IF ii = 1 THEN
                RUN pdf_text_at IN h_PDFinc ("Spdf",ENTRY(ii,cRubriker,CHR(9)) + "  ",iCols[ii]).
            ELSE DO:
                RUN pdf_text IN h_PDFinc ("Spdf","  " + ENTRY(ii,cRubriker,CHR(9))).
                iToRight[ii] = pdf_TextX("Spdf").
                dOrgWidth = pdf_text_width ("Spdf",ENTRY(ii,cRubriker,CHR(9))).
                cReplaceTxt = ENTRY(ii,cRubriker,CHR(9)).
                cReplaceTxt = REPLACE(cReplaceTxt,CHR(229),"a").
                cReplaceTxt = REPLACE(cReplaceTxt,CHR(228),"a").
                cReplaceTxt = REPLACE(cReplaceTxt,CHR(246),"o").
                cReplaceTxt = REPLACE(cReplaceTxt,CHR(197),"A").
                cReplaceTxt = REPLACE(cReplaceTxt,CHR(196),"A").
                cReplaceTxt = REPLACE(cReplaceTxt,CHR(214),"Ö").
                dRiktigWidth = pdf_text_width ("Spdf",cReplaceTxt).
                IF dOrgWidth < dRiktigWidth THEN DO:
                    iToRight[ii] = iToRight[ii] + (dRiktigWidth - dOrgWidth).
                END.
            END.
        END.
        RUN pdf_skip    IN h_PDFinc ("Spdf").
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
/*         MESSAGE pdf_PageWidth("Spdf") SKIP iToRight[1] SKIP iToRight[2] SKIP iToRight[3] SKIP iToRight[4] SKIP iToRight[5] */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                             */
        EMPTY TEMP-TABLE tt_totbutresult.
/*         PUT UNFORMATTED tt_butiker.butik SKIP. */
/*         PUT UNFORMATTED cRubriker SKIP.        */
        iAntDagar = 0.
        FOR EACH preemanalyseresult WHERE preemanalyseresult.AnalyseId = tt_butiker.analyseid and
                                          preemanalyseresult.butikknr  = tt_butiker.butik NO-LOCK BREAK BY preemanalyseresult.dato BY preemanalyseresult.radnr:
            IF FIRST-OF(preemanalyseresult.dato) THEN DO:
                cEntryRapp = cEntryTmp.
                ENTRY(1,cEntryRapp,CHR(9)) = STRING(preemanalyseresult.dato).
            END.
            IF FIRST-OF(preemanalyseresult.dato) THEN
                iAntDagar = iAntDagar + 1.
            FIND FIRST tt_totbutresult WHERE tt_totbutresult.analyseid = preemanalyseresult.AnalyseId AND
                                             tt_totbutresult.butikknr  = preemanalyseresult.butikknr AND
                                             tt_totbutresult.radnr     = preemanalyseresult.radnr NO-ERROR.
            IF NOT AVAIL tt_totbutresult THEN DO:
                CREATE tt_totbutresult.
                ASSIGN tt_totbutresult.analyseid = preemanalyseresult.AnalyseId
                       tt_totbutresult.butik     = preemanalyseresult.butikknr 
                       tt_totbutresult.radnr     = preemanalyseresult.radnr.
            END.
            ASSIGN tt_totbutresult.antall = tt_totbutresult.antall + preemanalyseresult.antall
                   tt_totbutresult.summa  = tt_totbutresult.summa  + preemanalyseresult.summa.
            CASE preemanalyseresult.radnr:
                WHEN 1 THEN DO:
                    iAntkunder = preemanalyseresult.antall.
                    ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(iAntkunder).
                END.
                WHEN 2 THEN
                    ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(preemanalyseresult.summa / iAntkunder,2),"->>9.99").
                WHEN 3 THEN
                    ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(preemanalyseresult.antall,2),"->>>,>>9.99").
/*                     ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(preemanalyseresult.antall / iAntkunder,2),"->>9.99"). */
                WHEN 4 THEN DO:
                    iAntall = preemanalyseresult.antall.
                    ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(iAntall).
                END.
                WHEN 5 THEN DO:
                    iAntall = preemanalyseresult.antall.
                    ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(iAntall).
                END.
                WHEN 6 THEN DO:
                    iAntall = preemanalyseresult.antall.
                    ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(iAntall).
                END.
                WHEN 7 THEN DO:
                    iAntall = preemanalyseresult.antall.
                    ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(iAntall).
                END.
                WHEN 8 THEN DO:
                    iAntall = preemanalyseresult.antall.
                    ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(iAntall).
                END.
                WHEN 9 THEN DO:
                    iAntall = preemanalyseresult.antall.
                    ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(iAntall).
                END.
                WHEN 10 THEN DO:
                    iAntall = preemanalyseresult.antall.
                    ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(iAntall).
                END.
            END CASE.
            IF LAST-OF(preemanalyseresult.dato) THEN DO:
              DO ii = 1 TO NUM-ENTRIES(cEntryRapp,CHR(9)):
                  IF ii = 1 AND iCols[ii] <> 0 THEN
                      RUN pdf_text_at IN h_PDFinc ("Spdf",ENTRY(ii,cEntryRapp,CHR(9)),iCols[ii]).
                  ELSE IF ii = 1 THEN
                      RUN pdf_text_to IN h_PDFinc ("Spdf",ENTRY(ii,cEntryRapp,CHR(9)),iToRight[ii]).
                  RUN pdf_text_xy  IN h_PDFinc ("Spdf",ENTRY(ii,cEntryRapp,CHR(9)),iToRight[ii] - pdf_text_width ("Spdf",ENTRY(ii,cEntryRapp,CHR(9))),pdf_TextY("Spdf")).
              END.
              RUN pdf_skip    IN h_PDFinc ("Spdf").
/*               PUT UNFORMATTED cEntryRapp SKIP. */
            END.
        END.
        cEntryRapp = cEntryTmp.
        ENTRY(1,cEntryRapp,CHR(9)) = "Medeltal".
        FOR EACH tt_totbutresult WHERE tt_totbutresult.analyseid = tt_butiker.analyseid AND
                                       tt_totbutresult.butikknr  = tt_butiker.butik.
            CASE tt_totbutresult.radnr:
                WHEN 1 THEN DO:
                    iAntkunder = tt_totbutresult.antall / iAntDagar.
                    ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(iAntkunder).
                    iAntkunder = tt_totbutresult.antall.
                END.
                WHEN 2 THEN
                    ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(tt_totbutresult.summa / iAntkunder,2),"->>9.99").
                WHEN 3 THEN
                    ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(tt_totbutresult.antall / iAntdagar,2),"->>>,>>9.99").
/*                     ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(tt_totbutresult.antall / iAntkunder,2),"->>9.99"). */
                WHEN 4 THEN DO:
                    ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(tt_totbutresult.antall / iAntDagar,1),"->>9.9").
                END.
                WHEN 5 THEN DO:
                    ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(tt_totbutresult.antall / iAntDagar,1),"->>9.9").
                END.
                WHEN 6 THEN DO:
                    ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(tt_totbutresult.antall / iAntDagar,1),"->>9.9").
                END.
                WHEN 7 THEN DO:
                    ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(tt_totbutresult.antall / iAntDagar,1),"->>9.9").
                END.
                WHEN 8 THEN DO:
                    ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(tt_totbutresult.antall / iAntDagar,1),"->>9.9").
                END.
                WHEN 9 THEN DO:
                    ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(tt_totbutresult.antall / iAntDagar,1),"->>9.9").
                END.
                WHEN 10 THEN DO:
                    ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(tt_totbutresult.antall / iAntDagar,1),"->>9.9").
                END.
            END CASE.
        END.
        RUN pdf_skip    IN h_PDFinc ("Spdf").
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
        DO ii = 1 TO NUM-ENTRIES(cEntryRapp,CHR(9)):
            IF ii = 1 AND iCols[ii] <> 0 THEN
                RUN pdf_text_at IN h_PDFinc ("Spdf",ENTRY(ii,cEntryRapp,CHR(9)),iCols[ii]).
            ELSE IF ii = 1 THEN
                RUN pdf_text_to IN h_PDFinc ("Spdf",ENTRY(ii,cEntryRapp,CHR(9)),iToRight[ii]).
            RUN pdf_text_xy  IN h_PDFinc ("Spdf",ENTRY(ii,cEntryRapp,CHR(9)),iToRight[ii] - pdf_text_width ("Spdf",ENTRY(ii,cEntryRapp,CHR(9))),pdf_TextY("Spdf")).
        END.
        RUN pdf_skip    IN h_PDFinc ("Spdf").
/*               PUT UNFORMATTED cEntryRapp SKIP. */
        RUN pdf_close IN h_PDFinc ("Spdf").

      END.
    END.
    IF cVedlegg <> "" AND cSendEmail = "J" THEN
        RUN SendEmail IN THIS-PROCEDURE.
/*     OUTPUT CLOSE. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RapportPDFOrg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportPDFOrg Procedure 
PROCEDURE RapportPDFOrg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iAntfelt   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cRubriker  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iAntKunder AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iAntall    AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iAntDagar  AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cEntryTmp  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEntryRapp AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE ii         AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iCols      AS INTEGER  EXTENT 14   NO-UNDO.
    DEFINE VARIABLE pcRappfil  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iToRight AS INTEGER  EXTENT 14   NO-UNDO.
    ASSIGN iCols[1] = 10.
    FIND LAST preemanalyserad WHERE preemanalyserad.analyseid = tt_analyser.analyseid NO-LOCK.
    iAntfelt = preemanalyserad.radnr.
    cEntryTmp = FILL("0,",iAntFelt) + "0".
    cEntryTmp = REPLACE(cEntryTmp,",",CHR(9)).
    cRubriker = cEntryTmp.
    ENTRY(1,cRubriker,CHR(9)) = "Datum".

    FOR EACH preemanalyserad WHERE preemanalyserad.analyseid = tt_analyser.analyseid NO-LOCK.
        ENTRY(preemanalyserad.radn + 1,cRubriker,CHR(9)) =  preemanalyserad.beskr.
    END.
/*     OUTPUT TO "CLIPBOARD". */

    FOR EACH tt_butiker WHERE tt_butiker.analyseid = tt_analyser.analyseid BREAK BY tt_butiker.butik:
      IF FIRST-OF(tt_butiker.butik) THEN DO:

        ASSIGN pcRappFil = SESSION:TEMP-DIR + "Saljmal" + "_" + STRING(tt_butiker.butik) + ".pdf".
        RUN pdf_new IN h_PDFinc ("Spdf",pcRappFil).
        RUN pdf_set_BottomMargin IN h_PDFinc ("Spdf", 60).
        RUN pdf_set_PaperType IN h_PDFinc ("Spdf","A4").
          /*   RUN LoadFonts. */
        RUN pdf_set_Orientation IN h_PDFinc ("Spdf","Landscape").
        RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
        RUN pdf_new_page IN h_PDFinc ("Spdf").
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
        DO ii = 1 TO NUM-ENTRIES(cRubriker,CHR(9)):
            IF ii = 1 THEN
                RUN pdf_text_at IN h_PDFinc ("Spdf",ENTRY(ii,cRubriker,CHR(9)) + "  ",iCols[ii]).
            ELSE DO:
                RUN pdf_text IN h_PDFinc ("Spdf","  " + ENTRY(ii,cRubriker,CHR(9))).
                iToRight[ii] = pdf_TextX("Spdf").
            END.
        END.
        RUN pdf_skip    IN h_PDFinc ("Spdf").
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
/*         MESSAGE pdf_PageWidth("Spdf") SKIP iToRight[1] SKIP iToRight[2] SKIP iToRight[3] SKIP iToRight[4] SKIP iToRight[5] */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                             */
        EMPTY TEMP-TABLE tt_totbutresult.
/*         PUT UNFORMATTED tt_butiker.butik SKIP. */
/*         PUT UNFORMATTED cRubriker SKIP.        */
        iAntDagar = 0.
        FOR EACH preemanalyseresult WHERE preemanalyseresult.AnalyseId = tt_butiker.analyseid and
                                          preemanalyseresult.butikknr  = tt_butiker.butik NO-LOCK BREAK BY preemanalyseresult.dato BY preemanalyseresult.radnr:
            IF FIRST-OF(preemanalyseresult.dato) THEN DO:
                cEntryRapp = cEntryTmp.
                ENTRY(1,cEntryRapp,CHR(9)) = STRING(preemanalyseresult.dato).
            END.
            IF FIRST-OF(preemanalyseresult.dato) THEN
                iAntDagar = iAntDagar + 1.
            FIND FIRST tt_totbutresult WHERE tt_totbutresult.analyseid = preemanalyseresult.AnalyseId AND
                                             tt_totbutresult.butikknr  = preemanalyseresult.butikknr AND
                                             tt_totbutresult.radnr     = preemanalyseresult.radnr NO-ERROR.
            IF NOT AVAIL tt_totbutresult THEN DO:
                CREATE tt_totbutresult.
                ASSIGN tt_totbutresult.analyseid = preemanalyseresult.AnalyseId
                       tt_totbutresult.butik     = preemanalyseresult.butikknr 
                       tt_totbutresult.radnr     = preemanalyseresult.radnr.
            END.
            ASSIGN tt_totbutresult.antall = tt_totbutresult.antall + preemanalyseresult.antall
                   tt_totbutresult.summa  = tt_totbutresult.summa  + preemanalyseresult.summa.
            CASE preemanalyseresult.radnr:
                WHEN 1 THEN DO:
                    iAntkunder = preemanalyseresult.antall.
                    ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(iAntkunder).
                END.
                WHEN 2 THEN
                    ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(preemanalyseresult.summa / iAntkunder,2),">>9.99").
                WHEN 3 THEN
                    ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(preemanalyseresult.antall / iAntkunder,2),">>9.99").
                WHEN 4 THEN DO:
                    iAntall = preemanalyseresult.antall.
                    ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(iAntall).
                END.
                WHEN 5 THEN DO:
                    iAntall = preemanalyseresult.antall.
                    ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(iAntall).
                END.
                WHEN 6 THEN DO:
                    iAntall = preemanalyseresult.antall.
                    ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(iAntall).
                END.
                WHEN 7 THEN DO:
                    iAntall = preemanalyseresult.antall.
                    ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(iAntall).
                END.
                WHEN 8 THEN DO:
                    iAntall = preemanalyseresult.antall.
                    ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(iAntall).
                END.
                WHEN 9 THEN DO:
                    iAntall = preemanalyseresult.antall.
                    ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(iAntall).
                END.
                WHEN 10 THEN DO:
                    iAntall = preemanalyseresult.antall.
                    ENTRY(preemanalyseresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(iAntall).
                END.
            END CASE.
            IF LAST-OF(preemanalyseresult.dato) THEN DO:
              DO ii = 1 TO NUM-ENTRIES(cEntryRapp,CHR(9)):
                  IF iCols[ii] <> 0 THEN
                      RUN pdf_text_at IN h_PDFinc ("Spdf",ENTRY(ii,cEntryRapp,CHR(9)),iCols[ii]).
                  ELSE
                      RUN pdf_text_to IN h_PDFinc ("Spdf",ENTRY(ii,cEntryRapp,CHR(9)),iToRight[ii]).
/*                       RUN pdf_text_xy ("Spdf",ENTRY(ii,cEntryRapp,CHR(9)),iToRight[ii] - pdf_text_width ("Spdf",ENTRY(ii,cEntryRapp,CHR(9))),pdf_TextY("Spdf")). */
              END.
              RUN pdf_skip    IN h_PDFinc ("Spdf").
/*               PUT UNFORMATTED cEntryRapp SKIP. */
            END.
        END.
        cEntryRapp = cEntryTmp.
        ENTRY(1,cEntryRapp,CHR(9)) = "Genomsnitt".
        FOR EACH tt_totbutresult WHERE tt_totbutresult.analyseid = tt_butiker.analyseid AND
                                       tt_totbutresult.butikknr  = tt_butiker.butik.
            CASE tt_totbutresult.radnr:
                WHEN 1 THEN DO:
                    iAntkunder = tt_totbutresult.antall / iAntDagar.
                    ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(iAntkunder).
                    iAntkunder = tt_totbutresult.antall.
                END.
                WHEN 2 THEN
                    ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(tt_totbutresult.summa / iAntkunder,2)).
                WHEN 3 THEN
                    ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(tt_totbutresult.antall / iAntkunder,2)).
                WHEN 4 THEN DO:
                    ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(tt_totbutresult.antall / iAntDagar,1)).
                END.
                WHEN 5 THEN DO:
                    ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(tt_totbutresult.antall / iAntDagar,1)).
                END.
                WHEN 6 THEN DO:
                    ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(tt_totbutresult.antall / iAntDagar,1)).
                END.
                WHEN 7 THEN DO:
                    ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(tt_totbutresult.antall / iAntDagar,1)).
                END.
                WHEN 8 THEN DO:
                    ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(tt_totbutresult.antall / iAntDagar,1)).
                END.
                WHEN 9 THEN DO:
                    ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(tt_totbutresult.antall / iAntDagar,1)).
                END.
                WHEN 10 THEN DO:
                    ENTRY(tt_totbutresult.radnr + 1,cEntryRapp,CHR(9)) = STRING(ROUND(tt_totbutresult.antall / iAntDagar,1)).
                END.
            END CASE.
        END.
        DO ii = 1 TO NUM-ENTRIES(cEntryRapp,CHR(9)):
            IF iCols[ii] <> 0 THEN
                RUN pdf_text_at IN h_PDFinc ("Spdf",ENTRY(ii,cEntryRapp,CHR(9)),iCols[ii]).
            ELSE
                RUN pdf_text_to IN h_PDFinc ("Spdf",ENTRY(ii,cEntryRapp,CHR(9)),iToRight[ii]).
        END.
        RUN pdf_skip    IN h_PDFinc ("Spdf").
/*               PUT UNFORMATTED cEntryRapp SKIP. */
        RUN pdf_close IN h_PDFinc ("Spdf").

      END.
    END.
/*     OUTPUT CLOSE. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendEmail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendEmail Procedure 
PROCEDURE SendEmail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      RUN prssmtpmailv5_7a.p (
        /*mailhub    */   cMailhub,
        /*EmailTo    */   cEMailTo,
        /*EmailFrom  */   cEMailFra,
        /*EmailCC    */   cEmailCC,
        /*Attachments*/   cFiles,
        /*LocalFiles */   cVedlegg,
        /*Subject    */   cEmne,
        /*Body       */   E-Meddelande,
        /*MIMEHeader */   "CharSet=iso8859-1",
        /*BodyType   */   "",
        /*Importance */   0,
        /*L_DoAUTH   */   FALSE,
        /*C_AuthType */   cAuthType,
        /*C_User     */   cUser,
        /*C_Password */   cPassword,
        /*oSuccessful*/  OUTPUT lMailOK,
        /*vMessage   */  OUTPUT cMessage) NO-ERROR.
      OUTPUT TO VALUE(SESSION:TEMP-DIR + "preemanalyse_log.txt").
      PUT UNFORMATTED STRING(TODAY) " " STRING(lMailOK,"OK/FEL") (IF lMailOK = FALSE THEN cMessage ELSE "") SKIP.
      OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getTyper) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTyper Procedure 
FUNCTION getTyper RETURNS CHARACTER
  ( INPUT ipanalyseid AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cList AS CHARACTER   NO-UNDO.
  FOR EACH preemanalyserad NO-LOCK WHERE preemanalyserad.AnalyseId = ipanalyseid BY preemanalyserad.typ:
      IF NOT CAN-DO(cList,STRING(preemanalyserad.typ)) THEN
          ASSIGN cList = cList + (IF cList <> "" THEN "," ELSE "") + STRING(preemanalyserad.typ).
  END.
  RETURN cList.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

