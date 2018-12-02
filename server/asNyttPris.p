&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : asNyttPris 
    Purpose     : Behandlar prisändringsrequest från kassan.

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER iButikkNr     AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER dArtikkelnr   AS DECIMAL     NO-UNDO.
DEFINE INPUT  PARAMETER cSprak        AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER dOnsketPris   AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER dNyprisPRIS   AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER dMvaKr AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER dDbKr  AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER dDB%   AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER cMelding      AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER lOK           AS LOGICAL     NO-UNDO.

DEFINE VARIABLE iCL AS INTEGER NO-UNDO.

DEFINE BUFFER clButiker FOR Butiker.
DEFINE BUFFER bufArtPris FOR ArtPris.

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

{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE 
  clButiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE clbutiker THEN 
DO:
    RUN bibl_logg.p ('AppServerPOS', 'asNyttPris.p Butikk: ' + STRING(iButikkNr) + ' - Sentrallager ikke definert')).
    cMelding = STRING(cSprak = "NO","Sentrallager ikke definert/Centralalger inte definerad").
    lOK = FALSE.
    RETURN.
END.

FIND Butiker WHERE butiker.butik = iButikkNr NO-LOCK NO-ERROR.
IF NOT AVAIL butiker THEN DO:
    RUN bibl_logg.p ('AppServerPOS', 'asNyttPris.p Butikk: ' + STRING(iButikkNr) + '  - Finner ikke butikk ' + STRING(iButikkNr))).
    cMelding = STRING(cSprak = "NO","Finner ikke butikk/Butiken finns inte").
    lOK = FALSE.
    RETURN.
END.

IF Butiker.ProfilNr = clButiker.ProfilNr THEN 
DO:
    RUN bibl_logg.p ('AppServerPOS', 'asNyttPris.p Butikk: ' + STRING(iButikkNr) + '  - Butikken mangler egen prisprofil')).
    cMelding = STRING(cSprak = "NO","Butikken mangler egen prisprofil/Butiken har ingen egen prisprofil").
    lOK = FALSE.
    RETURN.
END.

FIND artbas WHERE artbas.artikkelnr = dArtikkelnr NO-LOCK NO-ERROR.
IF NOT AVAIL artbas THEN DO:
    RUN bibl_logg.p ('AppServerPOS', 'asNyttPris.p Butikk: ' + STRING(iButikkNr) + '  - Ukjent artikkel på HK ' + STRING(dArtikkelnr)).
    cMelding = STRING(cSprak = "NO","Ukjent artikkel på HK " + STRING(dArtikkelnr) + "/HK har inte artikel " + STRING(dArtikkelnr)).
    lOK = FALSE.
    RETURN.
END.

RUN EndrePris IN THIS-PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-EndrePris) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndrePris Procedure 
PROCEDURE EndrePris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* Henter butikkens lokale pris. */
   FIND ArtPris OF ArtBas NO-LOCK WHERE
      ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
   IF NOT AVAILABLE ArtPris THEN 
     DO TRANSACTION:
       FIND ArtPris OF ArtBas NO-LOCK WHERE
          ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.
       CREATE bufArtPris.
       BUFFER-COPY ArtPris 
       EXCEPT ProfilNr
       TO bufArtPris
       ASSIGN
           bufArtPris.ProfilNr = Butiker.ProfilNr.
       RELEASE bufArtPris.
       FIND ArtPris OF ArtBas NO-LOCK WHERE
          ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
     END.   
   IF NOT AVAILABLE ArtPris THEN 
   DO:
     cMelding = STRING(cSprak = "NO","Feil ved opprettelse av lokal pris /Feil ved opprettelse av lokal pris ").
     lOK = FALSE.
     RETURN.
   END.
   ELSE IF ArtPris.Tilbud THEN 
   DO:
     cMelding = STRING(cSprak = "NO","Artikkel er på tilbud. /Artikel er på tilbud ").
     lOK = FALSE.
     RETURN.
   END.
   
   DO TRANSACTION:
     FIND CURRENT ArtPris EXCLUSIVE-LOCK.
     ASSIGN
        ArtPris.Pris[1]  = dOnsketPris
        ArtPris.MvaKr[1] = ArtPris.Pris[1] - (ArtPris.Pris[1] / (1 + (ArtPris.Mva%[1] / 100)))
        ArtPris.DBKr[1]  = ArtPris.Pris[1] - ArtPris.MvaKr[1] - ArtPris.VareKost[1]
        ArtPris.DB%[1]   = (ArtPris.DBKr[1] / (ArtPris.Pris[1] - ArtPris.MvaKr[1])) * 100
        ArtPris.DB%[1]   = IF ArtPris.DB%[1] = ? THEN 0 ELSE ArtPris.DB%[1]
        dNyprisPRIS      = ArtPris.Pris[1]
        dMvaKr           = ArtPris.MvaKr[1]
        dDbKr            = ArtPris.DBKr[1]
        dDB%             = ArtPris.DB%[1] 
        cMelding         = STRING(cSprak = "NO","Ny pris OK. /Ny pris OK ")
        lOK              = TRUE
        ArtPris.ETid     = TIME /* Tvinger trigger til å slå til */
        .
     RELEASE ArtPris.
   END.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

