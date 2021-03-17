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

DEFINE INPUT  PARAMETER iButikkNr  AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER cTyp       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cStorl     AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER iFlerastrTyp  AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER lEgetLager    AS LOGICAL     NO-UNDO.
DEFINE INPUT-OUTPUT  PARAMETER cHg        AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT  PARAMETER cStorlekar AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT  PARAMETER cMaterial  AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT  PARAMETER cFarg      AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT  PARAMETER lcVg       AS LONGCHAR   NO-UNDO.
DEFINE OUTPUT PARAMETER lcLagerdata       AS LONGCHAR     NO-UNDO.
DEFINE VARIABLE cEnabled_HG AS CHARACTER   NO-UNDO.


/*
cTyp "INIT" - ger Huvudgrupper Storlekar Material Färg
     "VG"   - ger VG registret till den HG som skikcats in
     "LAGER1" - Visa lager egen butik - iButikknr
     "LAGER2" - Visa lager egen + andras butik - iButikknr
     
*/

DEFINE BUFFER bufStrtstr FOR strtstr.

DEFINE TEMP-TABLE tt_eusok NO-UNDO
    FIELD vg         AS INTE
    FIELD lopnr      AS INTE
    FIELD artikkelnr AS INTE
    FIELD eustorl    AS CHAR
    FIELD storl      AS CHAR
    FIELD lagant     AS INTE
    FIELD lagant2    AS INTE
    FIELD strtypeid  AS INTE
    FIELD bildnamn   AS CHAR

    INDEX art IS PRIMARY artikkelnr
    INDEX vgl  vg lopnr.

DEFINE TEMP-TABLE tt_storl NO-UNDO
    FIELD strtypeid AS INTE
    FIELD seq       AS INTE
    FIELD eustorl   AS CHAR
    FIELD storl     AS CHAR
    INDEX ss IS PRIMARY UNIQUE strtypeid seq
    INDEX s2 strtypeid eustorl.

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


cEnabled_HG = "1,2,3,4". /* Görs om till syspara */
CASE cTyp:
    WHEN "INIT" THEN DO:
        RUN FillHG.
        RUN FillStorl.
        RUN FillMatr.
        RUN FillFarg.
    END.
    WHEN "VG" THEN DO:
        RUN FillVg.
    END.
    WHEN "LAGER" THEN DO:
        RUN ByggStorlekar.
        RUN Letaartiklar.
        RUN ByggLager.

/*         MESSAGE "iButikkNr   " iButikkNr      SKIP */
/*                 "cTyp        " cTyp           SKIP */
/*                 "cStorl      " cStorl         SKIP */
/*                 "iFlerastrTyp" iFlerastrTyp   SKIP */
/*                 "lEgetLager  " lEgetLager     SKIP */
/*                 "cHg         " cHg            SKIP */
/*                 "cStorlekar  " cStorlekar     SKIP */
/*                 "cMaterial   " cMaterial      SKIP */
/*                 "cFarg       " cFarg          SKIP */
/*                 "lcVg        " STRING(lcVg)        */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*         IF CAN-FIND(FIRST tt_eusok) THEN */
            TEMP-TABLE tt_eusok:WRITE-JSON("longchar",lcLagerdata,TRUE).
    END.
END CASE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggLager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggLager Procedure 
PROCEDURE ByggLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH tt_eusok:
        IF lEgetLager THEN DO:
            FIND artlag WHERE artlag.artikkelnr = tt_eusok.artikkelnr AND
                              artlag.storl      = tt_eusok.storl      AND
                              artlag.butik      = iButikkNr            NO-LOCK NO-ERROR.
            IF AVAIL artlag AND artlag.lagant > 0 THEN
                tt_eusok.lagant = artlag.lagant.
            ELSE
                DELETE tt_eusok.
        END.
        ELSE DO:
            FOR EACH artlag WHERE artlag.artikkelnr = tt_eusok.artikkelnr AND
                                  artlag.storl      = tt_eusok.storl      NO-LOCK.
                IF artlag.lagant > 0 THEN DO:
                    ASSIGN tt_eusok.lagant  = tt_eusok.lagant  + (IF artlag.butik  = iButikkNr THEN artlag.lagant ELSE 0)
                           tt_eusok.lagant2 = tt_eusok.lagant2 + (IF artlag.butik <> iButikkNr THEN artlag.lagant ELSE 0).
                END.
            END. 
            IF tt_eusok.lagant = 0 AND tt_eusok.lagant2 = 0 THEN
                DELETE tt_eusok.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggStorlekar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggStorlekar Procedure 
PROCEDURE ByggStorlekar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
/* bufStrtstr
"Exakt":U, 1,
"'2-' (Minus)":U, 2,
"'2+' (Plus)":U, 3,
"'+/-' (Både och)":U, 4 
 */
   FOR EACH strtstr WHERE strtstr.eustorl = cStorlekar NO-LOCK:
        IF CAN-FIND(FIRST tt_storl WHERE tt_storl.strtypeid = strtstr.strtypeid AND
                                         tt_storl.eustorl   = cStorlekar) THEN
            NEXT.
        PUT UNFORMATTED strtstr.strtypeid " " strtstr.sostorl " " cStorlekar SKIP.
        CREATE tt_storl.
        ASSIGN tt_storl.strtypeid = strtstr.strtypeid
               tt_storl.seq       = IF iFlerastrTyp = 1 THEN 1 ELSE IF iFlerastrTyp = 2 THEN 3 ELSE IF iFlerastrTyp = 3 THEN 1 ELSE 2
               tt_storl.eustorl   = TRIM(strtstr.eustorl)
               tt_storl.storl     = strtstr.sostorl.
        ii = ii + 1.
        IF iFlerastrTyp <> 1 THEN DO:
            FIND bufstrtstr WHERE ROWID(bufstrtstr) = ROWID(strtstr) NO-LOCK.
            IF iFlerastrTyp = 2 OR iFlerastrTyp = 4 THEN DO:
                FIND PREV bufstrtstr NO-LOCK NO-ERROR.
                IF AVAIL bufstrtstr THEN DO:
                    IF bufstrtstr.strtypeid = strtstr.strtypeid THEN DO:
                        CREATE tt_storl.
                        ASSIGN tt_storl.strtypeid = bufstrtstr.strtypeid
                               tt_storl.seq       = IF iFlerastrTyp = 2 THEN 2 ELSE 1
                               tt_storl.eustorl   = TRIM(bufstrtstr.eustorl)
                               tt_storl.storl     = bufstrtstr.sostorl.
                        ii = ii + 1.
                    END.
                    IF iFlerastrTyp = 2 THEN DO:
                        FIND PREV bufstrtstr NO-LOCK NO-ERROR.
                        IF AVAIL bufstrtstr THEN DO:
                            IF bufstrtstr.strtypeid = strtstr.strtypeid THEN DO:
                                CREATE tt_storl.
                                ASSIGN tt_storl.strtypeid = bufstrtstr.strtypeid
                                       tt_storl.seq       = 1
                                       tt_storl.eustorl   = TRIM(bufstrtstr.eustorl)
                                       tt_storl.storl     = bufstrtstr.sostorl.
                                ii = ii + 1.
                            END.
                        END.
                    END.
                END.
                IF iFlerastrTyp = 4 THEN DO:
                    FIND bufstrtstr WHERE ROWID(bufstrtstr) = ROWID(strtstr) NO-LOCK.
                    FIND NEXT bufstrtstr NO-LOCK NO-ERROR.
                    IF AVAIL bufstrtstr THEN DO:
                        IF bufstrtstr.strtypeid = strtstr.strtypeid THEN DO:
                            CREATE tt_storl.
                            ASSIGN tt_storl.strtypeid = bufstrtstr.strtypeid
                                   tt_storl.seq       = 3
                                   tt_storl.eustorl   = TRIM(bufstrtstr.eustorl)
                                   tt_storl.storl     = bufstrtstr.sostorl.
                            ii = ii + 1.
                        END.
                    END.
                END.
            END.
            ELSE IF iFlerastrTyp = 3 THEN DO:
                FIND NEXT bufstrtstr NO-LOCK NO-ERROR.
                IF AVAIL bufstrtstr THEN DO:
                    IF bufstrtstr.strtypeid = strtstr.strtypeid THEN DO:
                        CREATE tt_storl.
                        ASSIGN tt_storl.strtypeid = bufstrtstr.strtypeid
                               tt_storl.seq       = 2
                               tt_storl.eustorl   = TRIM(bufstrtstr.eustorl)
                               tt_storl.storl     = bufstrtstr.sostorl.
                        ii = ii + 1.
                    END.
                    FIND NEXT bufstrtstr NO-LOCK NO-ERROR.
                    IF AVAIL bufstrtstr THEN DO:
                        IF bufstrtstr.strtypeid = strtstr.strtypeid THEN DO:
                            CREATE tt_storl.
                            ASSIGN tt_storl.strtypeid = bufstrtstr.strtypeid
                                   tt_storl.seq       = 3
                                   tt_storl.eustorl   = TRIM(bufstrtstr.eustorl)
                                   tt_storl.storl     = bufstrtstr.sostorl.
                            ii = ii + 1.
                        END.
                    END.
                END.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillFarg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillFarg Procedure 
PROCEDURE FillFarg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* fyll som list-item-pairs */
    cFarg = "Alla,0".
    FOR EACH farg NO-LOCK:
        cFarg = cFarg + "," + farg.farbeskr + "," + STRING(farg.farg).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillHG) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillHG Procedure 
PROCEDURE FillHG :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* fyll som list-item-pairs */
    cHg = "Välj,0".
    FOR EACH huvgr NO-LOCK:
        IF NOT CAN-DO(cEnabled_HG,STRING(huvgr.hg)) THEN
            NEXT.
        cHg = cHg + "," + huvgr.hgbeskr + "," + STRING(huvgr.hg).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillMatr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillMatr Procedure 
PROCEDURE FillMatr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* fyll som list-item-pairs */
    cMaterial = "Alla,0".
    FOR EACH Material NO-LOCK:
        cMaterial = cMaterial + "," + Material.MatBeskr + "," + STRING(Material.Matkod).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillStorl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillStorl Procedure 
PROCEDURE FillStorl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* fyll som list-item-pairs */
    cStorlekar = "".
    FOR EACH EUskor NO-LOCK:
        cStorlekar = cStorlekar + "," + EUskor.eustorl.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillVg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillVg Procedure 
PROCEDURE FillVg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* fyll som list-item-pairs */
    lcVg = "Välj,0".
    FOR EACH Vargr WHERE Vargr.hg = INT(cHg) NO-LOCK:
        lcVg = lcVg + "," + REPLACE(Vargr.VgBeskr,",","") + "," + STRING(Vargr.Vg).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Letaartiklar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Letaartiklar Procedure 
PROCEDURE Letaartiklar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DEFINE VARIABLE iVg AS INT   NO-UNDO.
DO ii = 1 TO NUM-ENTRIES(lcVg):
    iVg = INT(ENTRY(ii,lcVg)).
    FOR EACH artbas WHERE artbas.vg = iVg NO-LOCK:
        IF cMaterial <> "0" AND ArtBas.MatKod <> INT(cMaterial) THEN
            NEXT.
        IF cFarg <> "0" AND ArtBas.Farg <> INT(cFarg) THEN
            NEXT.
        FIND FIRST tt_storl WHERE tt_storl.strtypeid = artbas.strtypeid NO-ERROR.
        IF NOT AVAIL tt_storl THEN
            NEXT.
        IF lEgetLager THEN DO:
            IF NOT CAN-FIND(lager WHERE lager.butik      = iButikkNr AND
                                        lager.artikkelnr = artbas.artikkelnr AND
                                        lager.lagant > 0) THEN
                NEXT.
        END.
        ELSE DO:
            IF NOT CAN-FIND(FIRST lager WHERE lager.artikkelnr = artbas.artikkelnr AND
                                        lager.lagant > 0) THEN
                NEXT.
        END.
        FIND Bilderegister OF ArtBas NO-LOCK NO-ERROR.

        FOR EACH tt_storl WHERE tt_storl.strtypeid = artbas.strtypeid:
            CREATE tt_eusok.
            ASSIGN tt_eusok.vg         = artbas.vg
                   tt_eusok.lopnr      = artbas.lopnr
                   tt_eusok.artikkelnr = artbas.artikkelnr
                   tt_eusok.eustorl    = tt_storl.eustorl
                   tt_eusok.storl      = tt_storl.storl
                   tt_eusok.strtypeid  = artbas.strtypeid
                .  tt_eusok.bildnamn   = IF AVAIL Bilderegister AND Bilderegister.bildnr > 0 THEN "mini" + bilderegister.filnavn ELSE "".
            ii = ii + 1.
        END.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

