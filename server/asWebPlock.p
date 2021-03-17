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
DEFINE VARIABLE lFerdigPlockad AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cNettButLst    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLagerListe    AS CHARACTER NO-UNDO.
DEF VAR         cMailEnablat   AS CHAR      NO-UNDO.
DEFINE VARIABLE dKOrdre_Id AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cMsgTxt AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTmp AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTmp1 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTmp2 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTmp3 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DEFINE VARIABLE lRejectFinns AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iAntRaderBest AS INTEGER     NO-UNDO.
DEFINE BUFFER bufSyspara FOR SysPara.
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
         HEIGHT             = 14.43
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

CASE cTyp:
    WHEN "FINNS" THEN DO:
        RUN GetFinns.
/*                                                                            */
/*         lPlockklinje = CAN-FIND(FIRST KOrdrelinje WHERE                    */
/*                                    KOrdreLinje.PlukkButikk = iButikknr AND */
/*                                    KOrdreLinje.plockstatus = 1).           */
    END.
    WHEN "GETDATA" THEN DO:
        RUN GetKOrdrelinje.
    END.
    WHEN "CONFIRM" THEN DO: /* det kan vara även reject */
        RUN UpdateKOrdrelinje(OUTPUT dKOrdre_Id).
        IF dKOrdre_Id > 0 THEN
            FIND KOrdreHode WHERE KOrdreHode.KOrdre_Id = dKOrdre_Id NO-LOCK NO-ERROR.
        IF AVAIL KordreHode AND KordreHode.butik > 0 THEN DO:
            FIND syspara WHERE SysPara.SysHId = 150 AND
                               SysPara.SysGr  =  17 AND
                               SysPara.ParaNr =   1 NO-LOCK NO-ERROR. /* Reservationsweb */
/*             IF AVAIL syspara AND syspara.parameter1 = "1" THEN DO: */
            DO: /* För närvarande så gäller nedanstående även för C&C JF, alltså behövs inte parametern tas hänsyn till KordreHode.butik > 0 */
                lFerdigPlockad = TRUE.
/*                 OUTPUT TO c:\tmp\plock.txt APPEND. */
                FOR EACH kordrelinje OF kordrehode WHERE kordrelinje.plukkbutikk = kordrehode.butik NO-LOCK:
/*                     EXPORT kordrelinje.kordre_id kordrelinje.plukkbutikk kordrelinje.plockstatus. */
                    iAntRaderBest = iAntRaderBest + 1.
                    IF lFerdigPlockad = TRUE THEN
                        lFerdigPlockad = kordrelinje.plockstatus = 2.
                    IF NOT lFerdigPlockad AND syspara.parameter1 = "1" THEN /* Jarmeus */
                        LEAVE.
                    IF kordrelinje.plockstatus > 2 THEN
                        lRejectFinns = TRUE.
                END.
/*                 PUT UNFORMATTED lFerdigPlockad "/" syspara.parameter1 "/" lRejectFinns "/" iAntRaderBest SKIP. */
                IF NOT lFerdigPlockad AND syspara.parameter1 = "0" AND lRejectFinns = TRUE AND iAntRaderBest = 1 THEN DO: /* JF en rad och den är rejectad */
                    FIND CURRENT KordreHode EXCLUSIVE.
                    ASSIGN KordreHode.Levstatus = "60". /* Detta gäller JF.  */
                    FIND CURRENT KordreHode NO-LOCK.
                    FIND butiker WHERE butiker.butik = KOrdreHode.butik NO-LOCK.
                    FIND bufSysPara WHERE bufSysPara.SysHId = 150 AND
                                          bufSysPara.SysGr  =  17 AND
                                          bufSysPara.ParaNr =   5 NO-LOCK NO-ERROR.
                    cTmp1 = bufSysPara.Parameter1.
                    cTmp1 = REPLACE(cTmp1,"BUTNAMN", CAPS(butiker.butnamn)).
                    cTmp1 = REPLACE(cTmp1,"ORDERNO", KOrdreHode.EkstOrdreNr).
                    cTmp2 = bufSysPara.Parameter2. /* Här är det lagt in \n i texten, därför hela entryt */
                    cTmp2 = REPLACE(cTmp2,"BUTNAMN", CAPS(butiker.butnamn)).
                    cTmp2 = REPLACE(cTmp2,"NEWLINE","\n").
                    RUN SMSpickupPoint.p(REPLACE(KOrdreHode.telefon,"+",""),cTmp1,cTmp2,"") NO-ERROR.
                END.
/*                 OUTPUT CLOSE. */
                IF lFerdigplockad THEN DO:
                    FIND CURRENT KordreHode EXCLUSIVE.
                    IF AVAIL syspara AND syspara.parameter1 = "1" THEN
                         ASSIGN KordreHode.Levstatus = "45". /* Detta gäller Jarmeus. För C&C behåller vi för närvarande "30" */
                    FIND CURRENT KordreHode NO-LOCK.
                    FIND syspara WHERE SysPara.SysHId = 150 AND
                                       SysPara.SysGr  =  17 AND
                                       SysPara.ParaNr =   4 NO-LOCK NO-ERROR.
                    IF AVAIL syspara AND syspara.parameter1 = "1" AND KOrdreHode.telefon <> "" THEN DO:
                        DEFINE VARIABLE cDatum AS CHARACTER   NO-UNDO.
                        DEFINE VARIABLE cStr AS CHARACTER   NO-UNDO.
                        DEFINE VARIABLE cDateFormat AS CHARACTER   NO-UNDO.
/*                         cDateFormat = SESSION:DATE-FORMAT. */
/*                         SESSION:DATE-FORMAT = "ymd".       */
                        cStr = ENTRY(NUM-ENTRIES(syspara.parameter2," "),syspara.parameter2," ") NO-ERROR.
                        IF cStr BEGINS "DATUM" AND NUM-ENTRIES(cStr,"+") = 2 THEN
                            cDatum = STRING(TODAY + INT(ENTRY(2,cStr,"+"))).
                        ELSE
                            ASSIGN cStr = " "
                                   cDatum = " ".
/*                         SESSION:DATE-FORMAT = cDateFormat. */
                        IF KordreHode.Levstatus = "45" THEN
                            RUN SMSpickupPoint.p(REPLACE(KOrdreHode.telefon,"+",""),"Order: " + KOrdreHode.EkstOrdreNr,REPLACE(syspara.parameter2,cStr,cDatum) + ". " + KOrdreHode.SendingsNr,"") NO-ERROR.
                        ELSE DO: /* Johanssons */
                            FIND butiker WHERE butiker.butik = KOrdreHode.butik NO-LOCK.
                            cTmp = syspara.parameter2.
                            cTmp1 = REPLACE(ENTRY(1,cTmp,"|"),"BUTNAMN", CAPS(butiker.butnamn)).
                            cTmp2 = ENTRY(2,cTmp,"|").
                            cStr = ENTRY(NUM-ENTRIES(cTmp2," "),cTmp2," ").
                            IF cStr BEGINS "DATUM" AND NUM-ENTRIES(cStr,"+") = 2 THEN
                                cDatum = STRING(TODAY + INT(ENTRY(2,cStr,"+"))).
                            ELSE
                                ASSIGN cStr = " "
                                       cDatum = " ".
                            cTmp2 = REPLACE(cTmp2,"ORDERNO", KOrdreHode.EkstOrdreNr).
                            cTmp2 = REPLACE(cTmp2,cStr,cDatum).
                            cTmp3 = REPLACE(ENTRY(3,cTmp,"|"),"BUTNAMN", butiker.butnamn).
                            RUN SMSpickupPoint.p(REPLACE(KOrdreHode.telefon,"+",""),cTmp1,cTmp2 + "\n" + cTmp3,"") NO-ERROR.
                        END.
                    END.
                    RELEASE KordreHode.
                END.
            END.
        END.
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

&IF DEFINED(EXCLUDE-GetFinns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetFinns Procedure 
PROCEDURE GetFinns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iVanlig AS INTEGER     NO-UNDO.
DEFINE VARIABLE ieXpress AS INTEGER     NO-UNDO.
    lPlockklinje = CAN-FIND(FIRST KOrdrelinje WHERE                   
                               KOrdreLinje.PlukkButikk = iButikknr AND
                               KOrdreLinje.plockstatus = 1).          
    IF lPlockklinje THEN DO:
        lPlockklinje  = false.
        FOR EACH KOrdrelinje WHERE KOrdreLinje.PlukkButikk = iButikknr AND
                                   KOrdreLinje.plockstatus = 1 NO-LOCK:
            FIND kordrehode OF kordrelinje NO-LOCK NO-ERROR.
            IF kordrehode.levstatus = "60" THEN
                NEXT.
            IF AVAIL kordrehode AND kordrehode.butik = iButikknr THEN
                ieXpress = 2.
            ELSE IF AVAIL kordrehode THEN
                iVanlig = 1.
        END.
        if iVanlig + ieXpress > 0 then
            lPlockklinje = true.
        
        IF iVanlig + ieXpress > 1 THEN DO: /* Bara om vi har express skall detta skapa post */
            /* Om vi har express så skall vi kolla att syspara inte är reservationsweb */
            FIND syspara WHERE SysPara.SysHId = 150 AND
                               SysPara.SysGr  =  17 AND
                               SysPara.ParaNr =   1 NO-LOCK NO-ERROR.
            IF AVAIL SysPara AND SysPara.parameter1 = "0" THEN DO:
                hBuffer = hTable:DEFAULT-BUFFER-HANDLE.
                hBuffer:BUFFER-CREATE().
                hBuffer:BUFFER-FIELD("typ"):BUFFER-VALUE = iVanlig + ieXpress. 
            END.
        END.
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

    FIND syspara WHERE SysPara.SysHId = 150 AND
                       SysPara.SysGr  =  17 AND
                       SysPara.ParaNr =   1 NO-LOCK NO-ERROR. /* Reservationsweb */
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
                    cPlusTxt = "|" + STRING(KOrdreHode.butik) + " " + (IF AVAIL bufButiker THEN bufButiker.butnamn ELSE "Okänd") + "|" + STRING(iAnt).
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
                IF hBuffer:BUFFER-FIELD("KundeNavn"):AVAILABLE THEN
                    hBuffer:BUFFER-FIELD("KundeNavn"):BUFFER-VALUE = IF KOrdreHode.butik > 0 THEN KOrdreHode.Navn + CHR(1) + KOrdreHode.MobilTlf + CHR(1) + KOrdreHode.KundeMerknad ELSE "".
                hBuffer:BUFFER-FIELD("RefTekst"):BUFFER-VALUE = (IF AVAIL butiker THEN STRING(butiker.butik) + " " + butiker.butnamn ELSE "") + cPlusTxt.
                IF KOrdreHode.butik > 0 AND hBuffer:BUFFER-FIELD("NotatKodeTekst"):AVAILABLE AND AVAIL SysPara AND SysPara.Parameter1 = "0" THEN
                    hBuffer:BUFFER-FIELD("NotatKodeTekst"):BUFFER-VALUE = "EXPRESS".
/*                 OUTPUT TO "c:\tmp\_wp.txt" APPEND. */
/*                 PUT UNFORMATTED cPlusTxt SKIP.     */
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
    DEFINE OUTPUT PARAMETER dKOrdre_Id AS DECI NO-UNDO.
    DEFINE VARIABLE cRejectListe AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iNyplockbutik AS INTEGER     NO-UNDO.
/*     DEFINE VARIABLE dKOrdre_Id AS DECIMAL     NO-UNDO. */
    DEFINE VARIABLE cMailKOrdre_Id AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cVgLop AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cWebshop AS CHARACTER   NO-UNDO.

    FIND KOrdrelinje WHERE ROWID(KOrdrelinje) = TO-ROWID(cRowId) NO-ERROR.
    FIND KOrdreHode OF KOrdreLinje NO-LOCK NO-ERROR.
    lPlockklinje = FALSE.
    IF AVAIL KOrdrelinje AND KOrdrelinje.plockstatus = 1 AND AVAIL KOrdreHode THEN DO TRANSACTION:
        lPlockklinje = TRUE.
        /* om iConfReject = 2 så skall plock godkännas. */
        IF iConfReject = 2 THEN DO:
            ASSIGN KOrdrelinje.plockstatus = iConfReject
                   KOrdrelinje.plockdatetime = NOW.
            dKOrdre_Id = KOrdrelinje.KOrdre_Id.
            RETURN.
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
             IF KOrdreHode.Butik = 0 THEN DO  /*vid c&c ingen ny butik */ : 
                 RUN ByggtmpLager (cRejectListe).
                 iNyplockbutik = getPlockbutik(DECI(KordreLinje.Varenr),TRIM(KordreLinje.storl)).
             END.
             IF iNyPlockbutik = 0 THEN DO:
                 ASSIGN KOrdrelinje.plockstatus = iConfReject
                        KOrdrelinje.plockdatetime = NOW
                        dKOrdre_Id = KOrdrelinje.KOrdre_Id.
                 FIND CURRENT kordrelinje NO-LOCK.
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
/*         dKOrdre_Id = 0. */
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
/*   FOR EACH bufkordrelinje WHERE bufkordrelinje.kordre_id = K_Id AND bufkordrelinje.antall > 0 NO-LOCK: */
  FOR EACH bufkordrelinje WHERE bufkordrelinje.kordre_id = K_Id AND bufkordrelinje.plukkbutikk = KOrdreHode.butik NO-LOCK:
      dTst = DECI(bufKOrdreLinje.VareNr) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
          NEXT.
      iAnt = iAnt + bufkordrelinje.antall.
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

