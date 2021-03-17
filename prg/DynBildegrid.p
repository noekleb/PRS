&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE tBild NO-UNDO LIKE Bild.



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
DEFINE INPUT  PARAMETER hTTArt      AS HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER cQRY        AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cArtikkelNr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButNamn        AS CHARACTER  FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE cColLabelString AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrintString    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTitle          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKundenavn      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPolygon        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCL             AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCLProfilnr     AS INTEGER    NO-UNDO.
DEFINE VARIABLE cButikkListe    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lNullposter     AS LOGICAL    NO-UNDO.
DEFINE VARIABLE iSortering      AS INTEGER    NO-UNDO.

DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR hQuery  AS HANDLE NO-UNDO.
DEF VAR wChild  AS HANDLE NO-UNDO.

DEF BUFFER clButiker FOR Butiker.

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
   Temp-Tables and Buffers:
      TABLE: tBild T "NEW SHARED" NO-UNDO Temp-DB Bild
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 13.52
         WIDTH              = 59.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEF VAR wMenuAction AS CHAR NO-UNDO.
DEF VAR wMenu       AS CHAR NO-UNDO.
def var wFilNavn    as char no-undo.
def var wAntall     as int no-undo.
def var wRGB        as char no-undo.
def var wSort       as int no-undo.
           
{syspara.i 1 1 100 cKundenavn}
{syspara.i 1 1 101 cPolygon}
{syspara.i 5 1 1 iCl INT}
ASSIGN 
    cTitle = "Bildegrid"
    wSort  = 1
    wMenuAction = "INIT"
    wAntall = 0
    .
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCl.

for each tBild: delete tBild. end.
RUN ByggTmpListe.

run artikkelgridmeny.p (wMenuAction, "", output wMenu).   

IF VALID-HANDLE(wChild) THEN
    APPLY "CLOSE" TO wChild.

IF NOT VALID-HANDLE(wChild) THEN  
  RUN w-bildegrid.w PERSISTENT 
      SET wChild (THIS-PROCEDURE:HANDLE,wMenu,2,wAntall).

RETURN NO-APPLY.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggTmpListe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpListe Procedure 
PROCEDURE ByggTmpListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hTTArtBuf AS HANDLE  NO-UNDO.
  DEFINE        VARIABLE  hBufferField AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cStorlArray AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iEntry AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLagAnt    LIKE LAger.LagAnt  NO-UNDO.
  DEFINE VARIABLE dVVarekost AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cVisEntry AS CHARACTER  NO-UNDO.

  hTTArtBuf = hTTArt:DEFAULT-BUFFER-HANDLE.
  CREATE QUERY  hQuery.
  hQuery:SET-BUFFERS(hTTArtBuf).

  IF hTTArtBuf:NAME = "TTArt" THEN
    hQuery:QUERY-PREPARE("FOR EACH TTArt").
  ELSE 
    hQuery:QUERY-PREPARE(cQRY).
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
      hBufferField = hTTArtBuf:BUFFER-FIELD("ArtikkelNr").
      FIND ArtBas WHERE ArtBas.ArtikkelNr = hBufferField:BUFFER-VALUE() NO-LOCK NO-ERROR.
      IF AVAIL Artbas AND NOT (ArtBas.Lager = FALSE OR ArtBas.OPris = TRUE) THEN DO:
          DO:
              FIND Artpris OF ArtBas WHERE ArtPris.ProfilNr = iCLProfilNr NO-LOCK NO-ERROR.
              IF NOT AVAIL Artpris THEN
                  FIND FIRST Artpris OF ArtBas NO-LOCK NO-ERROR.
              /* Henter bilde. Finnes ikke bilde, benyttes blankt bilde. */
              FIND BildeRegister OF ArtBas NO-LOCK NO-ERROR.
              if VALID-HANDLE(wLibHandle) then
                RUN HentBildePeker in wLibHandle
                                   (input BildeRegister.BildNr,
                                    INPUT 1,
                                     (if available Bilderegister
                                      then BildeRegister.Filnavn
                                      else ""),
                                    OUTPUT wFilNavn).

              /* Oppretter og initierer record. */
              create tBild.    
              assign
                wAntall          = wAntall + 1
                tBild.CellNr     = wAntall - 1 /*ListeLinje.CellNr  */
                tBild.ArtikkelNr = ArtBas.ArtikkelNr      
                tBild.BildTxt    = string(ArtBas.Vg) + "/" +
                                     (if ArtBas.LopNr <> ?
                                       then string(ArtBas.LopNr)
                                       else "?") + " " + 
                                   string(ArtBas.LevNr) + "/" + 
                                   string(ArtBas.LevKod)                          
                tBild.LevNr      = ArtBas.LevNr
                tBild.LevArtNr   = ArtBas.LevKod
                tBild.LevTid     = ""
                tBild.BestNr     = 0
                tBild.OrdreNr    = 0
                tBild.VgLopNr    = string(ArtBas.Vg) + "/" + 
                                     (if ArtBas.LopNr <> ? 
                                         then string(ArtBas.LopNr)
                                         else "")
                tBild.LopNr      = ArtBas.LopNr
                tBild.ValutaPris = if available ArtPris
                                     then ArtPris.ValPris[if ArtPris.Tilbud then 2 else 1]
                                     ELSE 0
                tBild.InnkjPris  = if available ArtPris
                                     then ArtPris.InnkjopsPris[if ArtPris.Tilbud then 2 else 1]
                                     ELSE 0
                tBild.VareKost   = if available ArtPris
                                     then ArtPris.VareKost[if ArtPris.Tilbud then 2 else 1]
                                     ELSE 0
                tBild.Bild       = wFilNavn
                .
              RUN TekstLinje IN THIS-PROCEDURE (1) NO-ERROR.
          END.
      END.
      hQuery:GET-NEXT().
  END.
  DELETE OBJECT hQuery.
  hTTArtBuf:BUFFER-RELEASE().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MenyValg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenyValg Procedure 
PROCEDURE MenyValg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER wRowId      AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER wMenuAction AS CHAR  NO-UNDO.    
    def var wTekst as char.
    
    run kollgridmeny.p (wMenuAction, "", output wTekst).
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = tBild.ArtikkelNr NO-ERROR.
    IF AVAILABLE ArtBAs THEN
    TEKSTRAD:
    DO:
      RUN TekstLinje (1).
      RUN TekstLinje (2).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TekstLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TekstLinje Procedure 
PROCEDURE TekstLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:    
  
  Forutsetter at tBild og ArtBas record er tilgjengelig.
     
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piLinje AS INT NO-UNDO.

  IF NOT AVAILABLE ArtBas THEN
      RETURN.

  IF piLinje = 1 THEN
  DO:
    ASSIGN
      tBild.TekstRad1  = string(ArtBas.Vg) + "/" +
                           (if ArtBas.LopNr <> ?
                             then string(ArtBas.LopNr)
                             else "?") + " " + 
                         string(ArtBas.LevNr) + "/" + 
                         string(ArtBas.LevKod).
  END.

  IF piLinje = 2 THEN
  DO:
    /* Henter prisinformasjon */
    FIND ArtPris NO-LOCK WHERE
        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
        ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN
        FIND FIRST ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.

    FIND Farg NO-LOCK WHERE
        Farg.Farg = ArtBas.Farg NO-ERROR.
    ASSIGN
      tBild.TekstRad2 = (IF AVAILABLE ArtPris
                           THEN string(int(ArtPris.InnkjopsPris[IF ArtPris.Tilbud THEN 2 ELSE 1]))
                           ELSE "0") + " / " +
                         (IF NOT AVAILABLE Farg
                            THEN ""
                          ELSE IF Farg.KFarge <> ""
                            THEN Farg.KFarge
                          ELSE
                            Farg.FarBeskr).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

