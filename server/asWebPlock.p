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
DEFINE INPUT  PARAMETER cRowId       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER iConfReject  AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER lPlockklinje AS LOGICAL     NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE-HANDLE hTable.

DEFINE VARIABLE hBuffer AS HANDLE      NO-UNDO.
DEFINE BUFFER bufKordreLinje FOR kordrelinje.

DEFINE VARIABLE cNettButLst    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLagerListe    AS CHARACTER NO-UNDO.
DEF VAR         cMailEnablat   AS CHAR      NO-UNDO.
/* Angivelse av primærlagre for nettbutikk. Det er de lagerne som skal sjekkes først. */
{syspara.i 150 1 3 cNettButLst}
/* Liste over lagre som nettbutikken kan plukke fra. Disse lagrene + primærbutikk, er de tilgjengelige lagrene */
{syspara.i 150 1 4 cLagerListe}
/* om vi inte kunnat tilldela ny plockbutik. skall vi då sända mail */
{syspar2.i 50 50 27 cMailEnablat}
DEFINE TEMP-TABLE tmpArtLag LIKE ArtLag
    FIELD totsolgtArt AS DECI.

DEFINE TEMP-TABLE tt_prio NO-UNDO
    FIELD prio AS INTE
    FIELD butik AS INTE
    INDEX prio IS PRIMARY UNIQUE prio.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getAntArt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAntArt Procedure 
FUNCTION getAntArt RETURNS INTEGER
  ( INPUT K_Id AS DECI )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPlockbutik) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPlockbutik Procedure 
FUNCTION getPlockbutik RETURNS INTEGER
  ( INPUT dArtikkelnr AS DECI, INPUT cStorl AS CHAR )  FORWARD.

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

CASE cTyp:
    WHEN "FINNS" THEN
        lPlockklinje = CAN-FIND(FIRST KOrdrelinje WHERE 
                                   KOrdreLinje.PlukkButikk = iButikknr AND
                                   KOrdreLinje.plockstatus = 1).
    WHEN "GETDATA" THEN DO:
        RUN GetKOrdrelinje.
    END.
    WHEN "CONFIRM" THEN DO: /* det kan vara även reject */
        RUN UpdateKOrdrelinje.
    END.
END CASE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggtmpLager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggtmpLager Procedure 
PROCEDURE ByggtmpLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cRejectListe AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lDec AS DECIMAL     NO-UNDO.
    EMPTY TEMP-TABLE tmpArtLag.
    DO:
        ASSIGN lDec = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN.
        FOR EACH ArtLag NO-LOCK WHERE ArtLag.ArtikkelNr  = DECI(KOrdreLinje.VareNr) AND 
                                      TRIM(ArtLag.storl) = TRIM(KOrdreLinje.Storl)     AND
                                      ArtLag.Lagant      > 0:
          IF CAN-DO(cRejectListe,STRING(ArtLag.butik)) THEN
              NEXT.
          IF STRING(ArtLag.butik) = TRIM(cNettButLst) OR CAN-DO(cLagerListe,STRING(ArtLag.butik)) THEN DO:
              FIND lager WHERE lager.artikkelnr = artlag.artikkelnr AND lager.butik = artlag.butik NO-LOCK NO-ERROR.
              CREATE tmpArtLag.
              BUFFER-COPY ArtLag TO tmpArtLag.
              tmpArtlag.storl = TRIM(tmpArtlag.storl).
              IF AVAIL lager THEN
                  tmpArtlag.totsolgtArt = lager.antsolgt.
              RELEASE tmpArtLag.
          END.
        END.
/*         cNettButLst cLagerListe */
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetKOrdrelinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetKOrdrelinje Procedure 
PROCEDURE GetKOrdrelinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Vi måste kontrollera för säkerhets skull att ordern inte är levererad */
    DEFINE VARIABLE cVgLop AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dB_id  AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE iAnt   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cPlusTxt AS CHARACTER   NO-UNDO.
    DEFINE BUFFER bufButiker FOR butiker.
    lPlockklinje = CAN-FIND(FIRST KOrdrelinje WHERE KOrdrelinje.plukkbutikk = iButikkNr AND 
                                  KOrdrelinje.plockstatus = 1).
    IF lPlockklinje THEN DO:
        lPlockklinje = FALSE.
        hBuffer = hTable:DEFAULT-BUFFER-HANDLE.
        FOR EACH KOrdrelinje WHERE KOrdreLinje.PlukkButikk = iButikknr AND KOrdreLinje.plockstatus = 1 NO-LOCK.
            cPlusTxt = "".
            IF KOrdreLinje.Varetekst BEGINS "FRAKT" THEN
                NEXT.
            FIND KOrdrehode OF KOrdrelinje NO-LOCK NO-ERROR.
            IF AVAIL KOrdreHode THEN DO:
                IF KordreHode.Levstatus <> "30" THEN
                    NEXT.
                FIND butiker WHERE butiker.butik = kordrehode.butikknr NO-LOCK NO-ERROR.
                dB_id = DECI(KOrdreHode.EkstOrdreNr) NO-ERROR.
                IF ERROR-STATUS:ERROR OR dB_id = ? OR dB_id = 0 THEN
                    dB_id = KOrdreLinje.KOrdre_Id.
                IF KOrdreHode.butik <> 0 THEN DO:
                    FIND bufButiker WHERE bufButiker.butik = KOrdreHode.butik NO-LOCK NO-ERROR.
                    iAnt = getAntArt(KOrdrehode.kordre_id).
                    cPlusTxt = "|" + STRING(KOrdreHode.butik) + (IF AVAIL bufButiker THEN bufButiker.butnamn ELSE "Okänd") + "|" + STRING(iAnt).
                END.
            END.
            ELSE DO:
                dB_id = KOrdreLinje.KOrdre_Id.
            END.
            FIND artbas WHERE artbas.artikkelnr = DECI(KOrdrelinje.varenr) NO-LOCK NO-ERROR.
            IF AVAIL artbas THEN DO:
                lPlockklinje = TRUE.
/*                 cVgLop = IF lSkomodus THEN STRING(artbas.vg) + "/" + STRING(artbas.lopnr) ELSE KOrdrelinje.varenr. */
                cVgLop = STRING(artbas.vg) + "/" + STRING(artbas.lopnr).
                hBuffer:BUFFER-CREATE().
                hBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE = 1. 
                hBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE = cVgLop.
                hBuffer:BUFFER-FIELD("Levkod"):BUFFER-VALUE = ArtBas.levkod.
                hBuffer:BUFFER-FIELD("Storrelse"):BUFFER-VALUE = KOrdreLinje.Storl.
                hBuffer:BUFFER-FIELD("Bongtekst"):BUFFER-VALUE = Artbas.Bongtekst.
                hBuffer:BUFFER-FIELD("Originaldata"):BUFFER-VALUE = STRING(ROWID(KOrdreLinje)).
                hBuffer:BUFFER-FIELD("B_id"):BUFFER-VALUE = dB_Id.
                hBuffer:BUFFER-FIELD("Linjenr"):BUFFER-VALUE = KOrdreLinje.KOrdreLinjeNr.
                hBuffer:BUFFER-FIELD("RefTekst"):BUFFER-VALUE = (IF AVAIL butiker THEN STRING(butiker.butik) + " " + butiker.butnamn ELSE "") + cPlusTxt.
/*                 OUTPUT TO "c:\tmp\_wp.txt" APPEND. */
/*                 PUT UNFORMATTED cVgLop SKIP.       */
/*                 OUTPUT CLOSE.                      */
                
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UpdateKOrdrelinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateKOrdrelinje Procedure 
PROCEDURE UpdateKOrdrelinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Vid manuellt byte av plockbutik så kommer det 9 i iConfReject
               och då skall vi inte göra ett automatiskt byte
------------------------------------------------------------------------------*/
    ON WRITE OF KOrdrelinje OVERRIDE DO: END.
    DEFINE VARIABLE cRejectListe AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iNyplockbutik AS INTEGER     NO-UNDO.
    DEFINE VARIABLE dKOrdre_Id AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE cMailKOrdre_Id AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cVgLop AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cWebshop AS CHARACTER   NO-UNDO.
    FIND KOrdrelinje WHERE ROWID(KOrdrelinje) = TO-ROWID(cRowId) NO-ERROR.
    lPlockklinje = FALSE.
    IF AVAIL KOrdrelinje AND KOrdrelinje.plockstatus = 1 THEN DO TRANSACTION:
        lPlockklinje = TRUE.
        /* om iConfReject = 2 så skall plock godkännas. */
        IF iConfReject = 2 THEN DO:
            ASSIGN KOrdrelinje.plockstatus = iConfReject
                   KOrdrelinje.plockdatetime = NOW.
        END.
        ELSE IF iConfReject = 9 THEN DO:
            /* Ny plockbutik sätts i den manuella hanteringen */
            ASSIGN KOrdrelinje.plockstatus = iConfReject
                   KOrdrelinje.plockdatetime = NOW.
            RUN reverser_overforing_kordrelinje.p (TO-ROWID(cRowId)) NO-ERROR.
        END.
        ELSE IF iConfReject > 2 THEN DO:
             RUN reverser_overforing_kordrelinje.p (TO-ROWID(cRowId)) NO-ERROR.
             cRejectListe = STRING(KOrdreLinje.PlukkButikk).
             CREATE KOrdreLinjeRejectPlock.
             ASSIGN KOrdreLinjeRejectPlock.kordre_id     = KOrdrelinje.kordre_id
                    KOrdreLinjeRejectPlock.KOrdreLinjeNr = KOrdrelinje.KOrdreLinjeNr
                    KOrdreLinjeRejectPlock.PlukkButikk   = KOrdrelinje.PlukkButikk
                    KOrdreLinjeRejectPlock.Storl         = TRIM(KOrdrelinje.Storl)
                    KOrdreLinjeRejectPlock.plockstatus   = iConfReject
                    KOrdreLinjeRejectPlock.plockdatetime = NOW.

             FOR EACH KOrdreLinjeRejectPlock WHERE
                KOrdreLinjeRejectPlock.kordre_id     = KOrdrelinje.kordre_id AND
                KOrdreLinjeRejectPlock.KOrdreLinjeNr = KOrdrelinje.KOrdreLinjeNr NO-LOCK.
                IF NOT CAN-DO(cRejectListe,STRING(KOrdreLinjeRejectPlock.PlukkButikk)) THEN
                    cRejectListe = cRejectListe + "," + STRING(KOrdreLinjeRejectPlock.PlukkButikk).
             END.
             /* här skall vi hämta ev ny plockbutik */
            /* se om det finns butiker som rejectat */
            /* exkludera dom + den butik som står som plockbutik */
            /* se om det finns lager på någon kvarvarande butik  */
             RUN ByggtmpLager (cRejectListe).
             iNyplockbutik = getPlockbutik(DECI(KordreLinje.Varenr),TRIM(KordreLinje.storl)).
             IF iNyPlockbutik = 0 THEN DO:
                 ASSIGN KOrdrelinje.plockstatus = iConfReject
                        KOrdrelinje.plockdatetime = NOW
                        dKOrdre_Id = KOrdrelinje.KOrdre_Id.
                 FIND artbas WHERE artbas.artikkelnr = DECI(kordrelinje.varenr) NO-LOCK NO-ERROR.
                 IF AVAIL artbas THEN
                     cVgLop = " Artikel: " + STRING(artbas.vg) + "/" + STRING(artbas.lopnr).
             END.
            /* om vi hittar någon med lager så skall inte plcokstaus ändras, men först skall vi reversera */
            /* Ny plockbutik sätts i sett_plukkbutikk_kundeordre_JF  */
            /* RUN sett_plukkbutikk_kundeordreJF.p ("",cKOLinjeRowId,iTst). */


            RELEASE KOrdrelinje.
        END.
    END.
    IF iNyplockbutik > 0 THEN DO:
        RUN sett_plukkbutikk_kundeordreJF.p ("",cRowId,iNyplockbutik). /* kör överföring */
    END.
    ELSE IF dKOrdre_Id > 0 AND cMailEnablat = "1" THEN DO:
        IF NOT AVAIL KOrdreHode THEN
            FIND KOrdreHode WHERE KOrdreHode.KOrdre_Id = dKOrdre_Id NO-LOCK NO-ERROR.
        IF AVAIL KOrdreHode THEN DO:
            cMailKOrdre_Id = TRIM(KOrdreHode.EkstOrdreNr).
            cWebshop = "Webshop: " + STRING(KOrdreHode.ButikkNr).
            IF cMailKOrdre_Id = "" THEN
                cMailKOrdre_Id = STRING(dKOrdre_Id).
        END.
        ELSE
            cMailKOrdre_Id = STRING(dKOrdre_Id).
        RUN sendmail_tsl.p ("PLOCKBUTIK","Plockbutik ej tilldelad","", cWebshop + " Kundorder: " + cMailKOrdre_Id + cVgLop,"","") NO-ERROR.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getAntArt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAntArt Procedure 
FUNCTION getAntArt RETURNS INTEGER
  ( INPUT K_Id AS DECI ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iAnt AS INTEGER     NO-UNDO.
  DEFINE VARIABLE dTst AS DECIMAL     NO-UNDO.
  FOR EACH bufkordrelinje WHERE bufkordrelinje.kordre_id = K_Id AND bufkordrelinje.antall > 0 NO-LOCK:
      dTst = DECI(bufKOrdreLinje.VareNr) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
          NEXT.
      iAnt = iAnt + kordrelinje.antall.
  END.
  RETURN iAnt.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPlockbutik) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPlockbutik Procedure 
FUNCTION getPlockbutik RETURNS INTEGER
  ( INPUT dArtikkelnr AS DECI, INPUT cStorl AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE iPrimaryButik AS INTEGER     NO-UNDO.
DEFINE VARIABLE iReturButik AS INTEGER     NO-UNDO.
DEFINE VARIABLE dHigh       AS DECI NO-UNDO.
DEFINE VARIABLE dMinstSolgt AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iPrio AS INTEGER     NO-UNDO.
iPrimaryButik = INT(cNettButLst).

FIND tmpArtlag WHERE tmpArtlag.artikkelnr = dArtikkelnr AND
                     tmpArtlag.butik      = iPrimaryButik AND
                     tmpArtlag.storl      = cStorl NO-ERROR.
IF AVAIL tmpArtlag THEN DO:
    tmpArtlag.lagant = tmpArtlag.lagant - 1.
    iReturButik = tmpArtLag.butik.
    IF tmpArtlag.lagant = 0 THEN     
        DELETE tmpArtlag.
    RETURN iReturButik. /* eller kanske  */
END.
/* här skall det prioriteras bland andra butiker */

FOR EACH tmpArtlag WHERE tmpArtlag.artikkelnr = dArtikkelnr AND tmpArtlag.storl = cStorl BY tmpArtLag.lagant descending:
    dHigh = tmpArtLag.lagant. /* högsta värdet kommer först */
    LEAVE.
END.

IF dHigh > 0 THEN DO:
    EMPTY TEMP-TABLE tt_prio.
    
    FOR EACH tmpArtlag WHERE tmpArtlag.artikkelnr = dArtikkelnr AND tmpArtlag.storl = cStorl AND tmpArtLag.Lagant = dHigh BY tmpArtLag.totsolgtArt:
        dMinstSolgt = tmpArtLag.totsolgtArt.
        LEAVE.
    END.
    FOR EACH tmpArtlag WHERE tmpArtlag.artikkelnr = dArtikkelnr AND tmpArtlag.storl = cStorl AND tmpArtLag.Lagant = dHigh AND tmpArtLag.totsolgtArt = dMinstSolgt:
        iPrio = LOOKUP(STRING(tmpArtlag.butik),cLagerListe).
        IF iPrio > 0 THEN DO:
            CREATE tt_prio.
            ASSIGN tt_prio.prio  = iPrio
                   tt_prio.butik = INT(ENTRY(iPrio,cLagerListe)).
        END.
    END.
    
/*     FOR EACH tt_prio:                                */
/*         EXPORT tt_prio.                              */
/* /*         MESSAGE "prio" Prio SKIP               */ */
/* /*                 "butik" tt_prio.butik          */ */
/* /*             VIEW-AS ALERT-BOX INFO BUTTONS OK. */ */
/*     END.                                             */
    FIND FIRST tt_prio NO-ERROR.
    IF NOT AVAIL tt_prio THEN
        RETURN iReturButik.
    FIND tmpArtlag WHERE tmpArtlag.artikkelnr = dArtikkelnr   AND
                         tmpArtlag.butik      = tt_prio.butik AND
                         tmpArtlag.storl      = cStorl        NO-ERROR. 
    IF AVAIL tmpArtlag THEN DO:
        tmpArtlag.lagant = tmpArtlag.lagant - 1.
        iReturButik = tmpArtLag.butik.
        IF tmpArtlag.lagant = 0 THEN
            DELETE tmpArtlag.
        RETURN iReturButik. /* eller kanske  */
    END.
END.
  

RETURN 0.   /* Function return value. */

/* DEFINE TEMP-TABLE tt_prio NO-UNDO      */
/*     FIELD prio AS INTE                 */
/*     FIELD butik AS INTE                */
/*     INDEX prio IS PRIMARY UNIQUE prio. */

 /* cLagerListe */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

