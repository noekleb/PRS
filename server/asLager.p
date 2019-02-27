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

DEFINE INPUT         PARAMETER iButikkNr   AS INTEGER     NO-UNDO.
DEFINE INPUT         PARAMETER cSprak      AS CHARACTER   NO-UNDO.
DEFINE INPUT         PARAMETER cDirection  AS CHARACTER   NO-UNDO.
DEFINE INPUT         PARAMETER cEan        AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT  PARAMETER iVg         AS INTEGER     NO-UNDO.
DEFINE INPUT-OUTPUT  PARAMETER iLopnr      AS INTEGER     NO-UNDO.
DEFINE INPUT-OUTPUT  PARAMETER rArtBas     AS ROWID       NO-UNDO.
DEFINE OUTPUT        PARAMETER cBeskr      AS CHARACTER   NO-UNDO.
DEFINE OUTPUT        PARAMETER dVk         AS DECIMAL     NO-UNDO.
DEFINE OUTPUT        PARAMETER dPris       AS DECIMAL     NO-UNDO.
DEFINE OUTPUT        PARAMETER dNormalPris AS DECIMAL     NO-UNDO.
DEFINE OUTPUT        PARAMETER cBildfil    AS CHARACTER   NO-UNDO.
DEFINE OUTPUT        PARAMETER lOK         AS LOGICAL     NO-UNDO.
DEFINE OUTPUT        PARAMETER cMelding    AS CHARACTER   NO-UNDO.
DEFINE OUTPUT        PARAMETER cLager      AS CHARACTER   NO-UNDO.

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

FIND butiker WHERE butiker.butik = iButikkNr NO-LOCK NO-ERROR.
IF NOT AVAIL butiker THEN DO:
    lOK = FALSE.
    cMelding = STRING(cSprak = "NO","Butikk mangler/Butik saknas").
    RETURN.
END.
IF rArtBas <> ? THEN DO:
    FIND artbas WHERE ROWID(artbas) = rArtBas NO-LOCK NO-ERROR.
    IF cDirection = "CURRENT" THEN DO: /* om vi utgår från browser på klient */
        IF AVAIL artbas THEN DO:
            RUN PosLager.p("A" + CHR(2) + STRING(artbas.artikkelnr) + CHR(2) + STRING(iButikkNr),OUTPUT cLager).
            lOK = TRUE.
        END.
        RETURN.
    END.
    ELSE IF cDirection = "NEXT" THEN DO: /*  */
        FIND NEXT artbas USE-INDEX vglopnr NO-LOCK NO-ERROR.
        IF NOT AVAIL Artbas THEN DO:
            lOK = FALSE.
            cMelding = STRING(cSprak = "NO","Artikkel mangler/Artikel saknas").
            RETURN.
        END.
    END.
    ELSE IF cDirection = "PREV" THEN DO: /*  */
        FIND PREV artbas USE-INDEX vglopnr NO-LOCK NO-ERROR.
        IF NOT AVAIL Artbas THEN DO:
            lOK = FALSE.
            cMelding = STRING(cSprak = "NO","Artikkel mangler/Artikel saknas").
            RETURN.
        END.
    END.
END.
ELSE DO:
    FIND Artbas WHERE artbas.vg = iVg AND lopnr = iLopnr NO-LOCK NO-ERROR.
    IF NOT AVAIL Artbas THEN DO:
        lOK = FALSE.
        cMelding = STRING(cSprak = "NO","Artikkel mangler/Artikel saknas").
        RETURN.
    END.
END.
IF AVAIL artbas THEN
    RUN getdata.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-getdata) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getdata Procedure 
PROCEDURE getdata :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    rArtBas = ROWID(Artbas).
    cBeskr = Artbas.bongtekst.
    iVg = artbas.vg.
    iLopnr = artbas.lopnr.
    FIND artpris WHERE artpris.artikkelnr = artbas.artikkelnr AND
                       artpris.profilnr   = butiker.profilnr NO-LOCK NO-ERROR.
    IF NOT AVAIL artpris THEN
        FIND artpris WHERE artpris.artikkelnr = artbas.artikkelnr AND
                           artpris.profilnr   = 1 NO-LOCK NO-ERROR.
    IF NOT AVAIL artpris THEN DO:
        lOK = FALSE.
        cMelding = STRING(cSprak = "NO","Pris mangler/Artikel saknas").
        RETURN.
    END.
    dPris = IF artpris.tilbud THEN artpris.pris[2] ELSE artpris.pris[1].
    dNormalpris = artpris.pris[1].
    lOK = TRUE.
    cMelding = IF artpris.tilbud THEN string(cSprak = "NO", "Tilbud slutt: /Kampanj slut: ") + string(ArtPris.TilbudTilDato) ELSE "".
    RUN PosLager.p("A" + CHR(2) + STRING(artbas.artikkelnr) + CHR(2) + STRING(iButikkNr),OUTPUT cLager).
    FIND Bilderegister OF ArtBas NO-LOCK NO-ERROR.
    IF AVAIL Bilderegister THEN 
        cBildFil = "mini" + bilderegister.filnavn.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

