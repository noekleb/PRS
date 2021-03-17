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
DEFINE INPUT  PARAMETER cDatatyp   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER iHg        AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER cParam     AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER cHGCombodata AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER cVGCombodata AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER cFGCombodata AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER lOK        AS LOGICAL     NO-UNDO.
DEFINE INPUT-OUTPUT  PARAMETER TABLE-HANDLE hTable.
DEFINE VARIABLE hBuffer AS HANDLE      NO-UNDO.

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

/*   cDirection */
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
CASE cDatatyp:
    WHEN "HG" THEN DO:
        cHGCombodata = ",0".
        FOR EACH huvgr WHERE huvgr.hg > 0 NO-LOCK:
            cHGCombodata = cHGCombodata + "," + huvgr.hgbeskr + "," + STRING(huvgr.hg).
            lOK = TRUE.
        END.
        cFGCombodata = "ALLA,999999".
        FOR EACH farg NO-LOCK:
            cFGCombodata = cFGCombodata + "," + (IF TRIM(farg.farbeskr) = "" THEN "Ej angiven" ELSE farg.farbeskr) + "," + STRING(farg.farg).
            lOK = TRUE.
        END.
    END.
    WHEN "VG" THEN DO:
        cVGCombodata = ",0".
        FOR EACH vargr WHERE vargr.hg = iHg AND Vargr.vg > 0 NO-LOCK:
            ii = ii + 1.
            cVGCombodata = cVGCombodata + "," + STRING(vargr.vg) + " " + vargr.vgbeskr + "," + STRING(vargr.vg).
            lOK = TRUE.
/*             IF ii = 40 THEN */
/*                 LEAVE.      */
        END.
    END.
    WHEN "ARTBAS" THEN DO:
        FIND butiker WHERE butiker.butik = iButikkNr NO-LOCK.
        RUN FillTT.
    END.
END CASE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-FillTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillTT Procedure 
PROCEDURE FillTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iVg AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iFarg AS INTEGER     NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
    DEFINE VARIABLE dPris AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dOrdpris AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE cOptText AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iLagant AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iAndrasLagant AS INTEGER     NO-UNDO.
    IF NUM-ENTRIES(cParam) <> 2 THEN
        RETURN.
    iVg = INT(ENTRY(1,cParam)).
    iFarg = INT(ENTRY(2,cParam)).
    hBuffer = hTable:DEFAULT-BUFFER-HANDLE.
    IF iFarg = 999999 THEN DO:
        FOR EACH artbas WHERE vg = iVg NO-LOCK:
            FIND Bilderegister OF ArtBas NO-LOCK NO-ERROR.
            hBuffer:BUFFER-CREATE().
            hBuffer:BUFFER-COPY(BUFFER artbas:HANDLE).
            hBuffer:BUFFER-FIELD("rrowid"):BUFFER-VALUE = ROWID(artbas). 
            RUN getPrisSumLager(OUTPUT dPris,OUTPUT dOrdpris,OUTPUT iLagant,OUTPUT iAndrasLagant,OUTPUT cOpttext).
            hBuffer:BUFFER-FIELD("pris"):BUFFER-VALUE = dPris.
            hBuffer:BUFFER-FIELD("ordpris"):BUFFER-VALUE = dOrdpris.
            hBuffer:BUFFER-FIELD("lagant"):BUFFER-VALUE = iLagant.
            hBuffer:BUFFER-FIELD("andraslagant"):BUFFER-VALUE = iAndrasLagant.
            hBuffer:BUFFER-FIELD("opttext"):BUFFER-VALUE = cOpttext.
            IF AVAIL Bilderegister THEN 
                hBuffer:BUFFER-FIELD("bildfil"):BUFFER-VALUE = "mini" + bilderegister.filnavn.


            

            /*     bildfil */

        END.
    END.
    ELSE DO:
        FOR EACH artbas WHERE vg = iVg AND artbas.farg = iFarg NO-LOCK:
            FIND Bilderegister OF ArtBas NO-LOCK NO-ERROR.
            hBuffer:BUFFER-CREATE().
            hBuffer:BUFFER-COPY(BUFFER artbas:HANDLE).
            hBuffer:BUFFER-FIELD("rrowid"):BUFFER-VALUE = ROWID(artbas). 
            RUN getPris(OUTPUT dPris,OUTPUT dOrdpris,OUTPUT cOpttext).
            hBuffer:BUFFER-FIELD("pris"):BUFFER-VALUE = dPris.
            hBuffer:BUFFER-FIELD("ordpris"):BUFFER-VALUE = dOrdpris.
            hBuffer:BUFFER-FIELD("opttext"):BUFFER-VALUE = cOpttext.
            IF AVAIL Bilderegister THEN 
                hBuffer:BUFFER-FIELD("bildfil"):BUFFER-VALUE = "mini" + bilderegister.filnavn.
        END.
    END.
/*     OUTPUT TO "C:\tmp\asart.txt".               */
/*     PUT UNFORMATTED cParam "-" ivg "-" ii SKIP. */
/*     OUTPUT CLOSE.                               */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPrisSumLager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPrisSumLager Procedure 
PROCEDURE getPrisSumLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER dPris AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER dOrdPris AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER iLagant AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER iAndrasLagant AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER cMelding AS CHARACTER   NO-UNDO.
    FIND artpris WHERE artpris.artikkelnr = artbas.artikkelnr AND
                       artpris.profilnr   = butiker.profilnr NO-LOCK NO-ERROR.
    IF NOT AVAIL artpris THEN
        FIND artpris WHERE artpris.artikkelnr = artbas.artikkelnr AND
                           artpris.profilnr   = 1 NO-LOCK NO-ERROR.
    IF NOT AVAIL artpris THEN DO:
        RETURN.
    END.
    dPris = IF artpris.tilbud THEN artpris.pris[2] ELSE artpris.pris[1].
    dOrdpris = artpris.pris[1].
    cMelding = IF artpris.tilbud THEN "Kampanj slut: " + string(ArtPris.TilbudTilDato) ELSE "".
    FOR EACH lager WHERE lager.artikkelnr = artbas.artikkelnr NO-LOCK.
        IF lager.butik = iButikkNr THEN
            iLagant = iLagant + lager.lagant.
        ELSE
            iAndrasLagant = iAndrasLagant + lager.lagant.
    END.
    IF AVAIL lager THEN
        iLagant = lager.lagant.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

